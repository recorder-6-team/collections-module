/*===========================================================================*\
	  Grant permissions for search term table
\*===========================================================================*/
IF OBJECT_ID('dbo.Search_Term') IS NOT NULL
BEGIN
	PRINT 'Setting up security on table Search_Term'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
    	GRANT SELECT ON dbo.Search_Term TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
	BEGIN
		GRANT SELECT ON dbo.Search_Term TO [R2k_Administrator]
		GRANT UPDATE ON dbo.Search_Term TO [R2k_Administrator]
		GRANT INSERT ON dbo.Search_Term TO [R2k_Administrator]
		GRANT DELETE ON dbo.Search_Term TO [R2k_Administrator]
	END
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
	BEGIN
		GRANT SELECT ON dbo.Search_Term TO R2k_FullEdit
		GRANT UPDATE ON dbo.Search_Term TO R2k_FullEdit
		GRANT INSERT ON dbo.Search_Term TO R2k_FullEdit
		GRANT DELETE ON dbo.Search_Term TO R2k_FullEdit
	END
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT SELECT ON dbo.Search_Term TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT SELECT ON dbo.Search_Term TO [R2k_RecordCardsOnly]
END

/*===========================================================================*\
  Drop usp_Concept_UpdatePublishedTerm - no longer required
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].usp_Concept_UpdatePublishedTerm')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].usp_Concept_UpdatePublishedTerm

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptSimple_Insert]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptSimple_Insert]
GO

/*===========================================================================*\
  Description:	Inserts a new concept and term (if necessary).

  Parameters:	@Key OUTPUT,
		@TermKey 
		@ConceptGroupKey 
		@TermVersionKey 
		@ListPreferred 
		@IsCurrent
		@Preferred 
		@ConceptRankKey 
		@NameTypeConceptKey 
		@MeaningKey 
		@AuthorCopy 
		@SortCode 
		@ListCode
		@SessionID 
		@SystemSuppliedData 

  Created:	December 2003

  Last revision information:
    $Revision: 2 $
    $Date: 26/08/11 17:21 $
    $Author: Jamesbichard $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_ConceptSimple_Insert]
	@Key char(16) OUTPUT,
	@TermKey char(16),
	@ConceptGroupKey char(16),
	@AuthorAndDate varchar(100) = NULL,
	@Attributes varchar(100) = NULL,
	@ListPreferred bit = NULL,
	@IsCurrent bit = NULL,
	@Preferred bit = NULL,
	@ConceptRankKey char(16) = NULL,
	@NameTypeConceptKey char(16) = NULL,
	@MeaningKey char(16) = NULL,
	@AuthorCopy varchar(100) = NULL,
	@SortCode int = NULL,
	@ListCode varchar(50) = NULL,
	@PublishedTerm nvarchar(450),
	@AutomaticPublishedTerm bit = 1,
	@TermGeneratorKey char(16),
	@SessionID char(16),
	@SystemSuppliedData bit = NULL
AS
	SET NOCOUNT ON
	SET ANSI_NULLS ON
	SET ANSI_PADDING ON
	SET ANSI_WARNINGS ON
	SET CONCAT_NULL_YIELDS_NULL ON
	SET QUOTED_IDENTIFIER ON
	SET NO_BROWSETABLE OFF
	SET XACT_ABORT ON

	BEGIN TRANSACTION
		DECLARE @NewMeaningKey char(16),
				@TermVersionKey char(16)
		
		SET @ListPreferred = IsNull(@ListPreferred, 1)
	
		/*-------------------------------------------------------------*\
		  If we don't have a meaning key, create one.
		\*-------------------------------------------------------------*/
		IF @MeaningKey IS NULL
		BEGIN
			EXEC spNextKey 'Meaning', @NewMeaningKey OUTPUT--, @SiteID
			IF @@ERROR <> 0 GOTO RollbackAndExit
			
			INSERT INTO Meaning (
				Meaning_Key
			) VALUES (
				@NewMeaningKey
			)
			IF @@Error <> 0 GOTO RollbackAndExit
		END
		ELSE
			SET @NewMeaningKey = @MeaningKey

		/*-------------------------------------------------------------*\
			Create new term version
		\*-------------------------------------------------------------*/

		EXEC usp_TermVersion_Insert
			@Key = @TermVersionKey OUTPUT,
			@ConceptKey = '',
			@TermKey = @TermKey,
			@VersionLabel = @Attributes,
			@AuthorAndDate = @AuthorAndDate,
			@SessionID = @SessionID,
			@SystemSuppliedData = @SystemSuppliedData
		IF @@Error <> 0 GOTO RollbackAndExit		
	
		/*-------------------------------------------------------------*\
		  Create new Concept.
		\*-------------------------------------------------------------*/
		EXEC spNextKey 'Concept', @Key OUTPUT
		IF @@ERROR <> 0 GOTO RollbackAndExit

		DECLARE @Plaintext VARCHAR(100)

		SELECT @Plaintext = Plaintext
		FROM Term
		WHERE Term_Key = @TermKey
		
		INSERT INTO Concept (
			Concept_Key, Term_Key, Concept_Group_Key, List_Preferred, 
			Is_Current, Preferred, Concept_Rank_Key, Name_Type_Concept_Key, 
			Meaning_Key, Author_Copy, Sort_Code, List_Code,
			Published_Term, Automatic_Published_Term, Term_Generator_Key,
			Entered_Session_ID, System_Supplied_Data, Term_Version_Key
		) VALUES (
			@Key, @TermKey, @ConceptGroupKey, @ListPreferred, 
			IsNull(@IsCurrent, 1), IsNull(@Preferred, 0), @ConceptRankKey,
			@NameTypeConceptKey, @NewMeaningKey, @AuthorCopy, @SortCode,
			@ListCode, ISNULL(@PublishedTerm, @Plaintext), @AutomaticPublishedTerm,
			@TermGeneratorKey, @SessionID, 0, @TermVersionKey
		)
		IF @@Error <> 0 GOTO RollbackAndExit
	
		/*-------------------------------------------------------------*\
		  Create Concept_Lineage.
		\*-------------------------------------------------------------*/
		EXECUTE		usp_ConceptLineage_NewConcept	@Key
		IF @@ERROR <> 0 GOTO RollbackAndExit
		
		/*----------------------------------------------------------------------------*\
		  If @Preferred = 1, then make sure the updated concept is the 
		  only Preferred synonym with the same language key and name type concept key.
		\*----------------------------------------------------------------------------*/
		IF @Preferred = 1 
			UPDATE		CSynonyms
			SET		Preferred = 0
			FROM 		Concept AS CSource
			INNER JOIN	Term 	AS TSource 	ON TSource.Term_Key = CSource.Term_Key
			INNER JOIN	Concept AS CSynonyms 	ON CSynonyms.Meaning_Key = CSource.Meaning_Key
								AND CSynonyms.Name_Type_Concept_Key = CSource.Name_Type_Concept_Key
								AND CSynonyms.Concept_Key <> CSource.Concept_Key
			INNER JOIN	Term 	AS TSynonyms 	ON TSynonyms.Term_Key = CSynonyms.Term_Key
								AND TSynonyms.Language_Key = TSource.Language_Key
			WHERE		CSource.Concept_Key = @Key

		IF @@Error <> 0 GOTO RollbackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit:
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_ConceptSimple_Insert failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptSimple_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptSimple_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
			GRANT EXECUTE ON dbo.usp_ConceptSimple_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptSimple_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptSimple_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptSimple_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptSimple_Insert TO [Dev - JNCC SQL]
END
GO
/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptSimple_Update') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_ConceptSimple_Update]
GO

/*===========================================================================*\
  Description:	Updates a record in the concept table.

  Parameters:	@Key
		@TermKey 
		@ConceptGroupKey 
		@Preferred
		@ConceptRankKey 
		@NameTypeConceptKey
		@SortCode 
		@ListCode 
		@SessionID 
		@RecordsAffected 
		@Timestamp 

  Created:	December 2003

  Last revision information:
    $Revision: 2 $
    $Date: 26/08/11 17:21 $
    $Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptSimple_Update]
	@Key char(16),
	@TermKey char(16),
	@AuthorAndDate varchar(100) = NULL,
	@Attributes varchar(100) = NULL,
	@NewTermVersionNeeded bit = 0,
	@ConceptGroupKey char(16),
	@ListPreferred bit = NULL,
	@Preferred bit,
	@ConceptRankKey char(16),
	@NameTypeConceptKey char(16) = NULL,
	@SortCode int,
	@ListCode varchar(50),
	@PublishedTerm nvarchar(450) = NULL,
	@AutomaticPublishedTerm bit,
	@TermGeneratorKey char(16),
	@SessionID char(16),
	@RecordsAffected int OUTPUT,
	@Timestamp timestamp
AS
SET NOCOUNT OFF
	
	BEGIN TRANSACTION
		-- Check record has not been updated by another user
		IF EXISTS( SELECT * 
					FROM Concept
					WHERE Concept_Key = @Key and @timestamp = Timestamp)
		BEGIN
			-- VI 13430 - CCN178 - TSEQUAL and stored procs
			IF @RecordsAffected = 0 AND EXISTS (
				SELECT Concept_Key FROM Concept WHERE Concept_Key = @Key
			)
			BEGIN
				RAISERROR('Record updated by another user', 16, 1)
			END
		END

		/*--------------------------------------------------*\
			Update/create new term version
		\*--------------------------------------------------*/
		DECLARE @TermVersionKey CHAR(16)

		SELECT @TermVersionKey = Term_Version_Key
		FROM Concept
		WHERE Concept_Key = @Key

		IF @TermVersionKey IS NULL OR @NewTermVersionNeeded = 1
		BEGIN
			EXEC usp_TermVersion_Insert
				@Key = @TermVersionKey OUTPUT,
				@ConceptKey = @Key,
				@VersionLabel = @Attributes,
				@AuthorAndDate = @AuthorAndDate,
				@SessionID = @SessionID
			IF @@Error <> 0 GOTO RollbackAndExit
		END
		ELSE
		BEGIN
			EXEC usp_TermVersion_Update
				@Key = @TermVersionKey,
				@ConceptKey = @Key,
				@VersionLabel = @Attributes,
				@AuthorAndDate = @AuthorAndDate,
				@SessionID = @SessionID	

			IF @@Error <> 0 GOTO RollbackAndExit	

			-- Term version may have changed, so make sure we have the right key
			SELECT @TermVersionKey = Term_Version_Key
			FROM Concept
			WHERE Concept_Key = @Key 	
		END		
		
		/*-------------------*\
		  Update the Concept.
		\*-------------------*/
		DECLARE @old_concept_group_key CHAR(16),
			@old_list_preferred BIT,
			@OldConceptRankKey char(16),
			@error INT

		DECLARE @Plaintext VARCHAR(100)

		SELECT @Plaintext = Plaintext
		FROM Term
		WHERE Term_Key = @TermKey
		
		UPDATE	Concept
		SET 	@old_concept_group_key = Concept_Group_Key,
			@old_list_preferred = List_Preferred,
			@OldConceptRankKey = Concept_Rank_Key,
			List_Preferred = IsNull(@ListPreferred, List_Preferred),
			Concept_Group_Key = @ConceptGroupKey,
			Term_Key = @TermKey,
			Term_Version_Key = @TermVersionKey,
			Concept_Rank_Key = @ConceptRankKey,
			Preferred = @Preferred,
			Name_Type_Concept_Key = @NameTypeConceptKey,
			Sort_Code = @SortCode,
			List_Code = @ListCode,
			Published_Term = ISNULL(@PublishedTerm, @Plaintext),
			Automatic_Published_Term = @AutomaticPublishedTerm,
			Term_Generator_Key = @TermGeneratorKey,
			Changed_Session_ID = @SessionID			
		WHERE	Concept_Key = @Key

		SELECT	@error = @@ERROR,
			@RecordsAffected = @@ROWCOUNT

		IF @error <> 0 GOTO RollbackAndExit		

		/*----------------------------------------------------------------------------*\
		  Make corresponding changes in Concept_Lineage
		\*----------------------------------------------------------------------------*/
		EXECUTE	usp_ConceptLineage_ConceptUpdated	@Key,
								@old_concept_group_key,
								@old_list_preferred
		IF @@ERROR <> 0 GOTO RollbackAndExit
		
		/*----------------------------------------------------------------------------*\
		  If @Preferred = 1, then make sure the updated concept is the 
		  only Preferred synonym with the same language key and name type concept key.
		\*----------------------------------------------------------------------------*/
		IF @Preferred = 1 
			UPDATE		CSynonyms
			SET		Preferred = 0
			FROM 		Concept AS CSource
			INNER JOIN	Term 	AS TSource 	ON TSource.Term_Key = CSource.Term_Key
			INNER JOIN	Concept AS CSynonyms 	ON CSynonyms.Meaning_Key = CSource.Meaning_Key
								AND CSynonyms.Name_Type_Concept_Key = CSource.Name_Type_Concept_Key
								AND CSynonyms.Concept_Key <> CSource.Concept_Key
			INNER JOIN	Term 	AS TSynonyms 	ON TSynonyms.Term_Key = CSynonyms.Term_Key
								AND TSynonyms.Language_Key = TSource.Language_Key
			WHERE		CSource.Concept_Key = @Key

		IF @@Error <> 0 GOTO RollbackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit:
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_ConceptSimple_Update failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptSimple_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptSimple_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptSimple_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptSimple_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptSimple_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptSimple_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptSimple_Update TO [Dev - JNCC SQL]
END
GO
If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Concepts_Select_ForSearch]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Concepts_Select_ForSearch]
GO

/*===========================================================================*\
  Description: Search proc for concepts where Term_Version_Key is not null.

  Parameters:	@SearchText

  Created:	December 2003

  Last revision information:
    $Revision: 2 $
    $Date: 26/08/11 17:21 $
    $Author: Jamesbichard $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_Concepts_Select_ForSearch] 
	@SearchText varchar(100)
AS

SET NOCOUNT ON

	SELECT DISTINCT
		CT.Concept_Key AS Item_Key,
		CT.Item_Name + ' - ' + CG.Item_Name AS DisplayTerm,
		CT.Item_Name + ' - ' + CG.Item_Name COLLATE SQL_Latin1_General_CP1_CI_AS AS SearchTerm
	FROM
		VW_ConceptTerm CT
	INNER JOIN Concept_Group CG ON CG.Concept_Group_Key=CT.Concept_Group_Key
	LEFT JOIN Search_Term ST ON ST.Concept_Key = CT.Concept_Key
	WHERE ST.Plaintext LIKE @SearchText + '%'	
	ORDER BY DisplayTerm
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concepts_Select_ForSearch') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Concepts_Select_ForSearch'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Concepts_Select_ForSearch TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concepts_Select_ForSearch TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concepts_Select_ForSearch TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Concepts_Select_ForSearch TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Concepts_Select_ForSearch TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Concepts_Select_ForSearch TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Concept_Delete]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Concept_Delete]
GO

/*===========================================================================*\
  Description:	Delete a record from the Concept tables. Also deletes records
		from other tables where necessary.  If @DeleteUnlinkedSynonyms
		is 1, then removes any non-list preferred concepts from the 
		same concept group.

  Parameters:	@Key		Concept key.
				@Timestamp

  Created:	December 2003

  Last revision information:
    $Revision: 2 $
    $Date: 26/08/11 17:21 $
    $Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Concept_Delete]
	@Key char(16),
	@Timestamp timestamp = NULL,
	@SyncTaxonDict bit = 0,
	@DeleteUnlinkedSynonyms bit = 0
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	DECLARE @MeaningKey char(16),
			@TermKey char(16),
			@TermVersionKey char(16),
			@ConceptsSharingMeaningKeyCount int,
			@ConceptsSharingTermKeyCount int,
			@ConceptsSharingTermVersionKeyCount int,
			@OriginalTimestamp timestamp,
			@error				INT,
			@RecordsAffected	INT

	-- Store the Meaning, Term and Term Version keys because the concept record
	-- needs to be deleted before these other records can be, due to referential
	-- integrity.
	SELECT	@MeaningKey = Meaning_Key,
			@TermKey = Term_Key,
			@TermVersionKey = Term_Version_Key,
			@OriginalTimestamp = [Timestamp]
	FROM 	Concept
	WHERE	Concept_Key = @Key

	-- Count the number of concepts that use this meaning key.
	SELECT 		@ConceptsSharingMeaningKeyCount = Count(C2.Concept_Key)
	FROM		Concept AS C1
	INNER JOIN	Concept AS C2 ON C2.Meaning_Key = C1.Meaning_Key
	WHERE		C1.Concept_Key = @Key

	-- Count the number of concepts that use the same term key as the concept we want to delete.
	SELECT 		@ConceptsSharingTermKeyCount = Count(DISTINCT C2.Concept_Key)
	FROM		Concept AS C1
	INNER JOIN	Concept AS C2 ON C2.Term_Key = C1.Term_Key
	WHERE		C1.Concept_Key = @Key

	-- Count the number of concepts that use the same term version key as the concept we want to delete.
	SELECT 		@ConceptsSharingTermVersionKeyCount = Count(DISTINCT C2.Concept_Key)
	FROM		Concept AS C1
	INNER JOIN	Concept AS C2 ON C2.Term_Version_Key = C1.Term_Version_Key
	WHERE		C1.Concept_Key = @Key


	BEGIN TRANSACTION
		/*============================================================*\
		  See if the user wants any associated taxon dictionary
		  records be deleted with the concept.
		\*============================================================*/
		IF @SyncTaxonDict = 1 
		BEGIN
			/*--------------------------------------------------------*\
			  Check that the Taxon_List_Item table exists before
			  attempting any of this deletion. In the future, the 
			  Thesaurus module could be installed without the Taxon
			  tables, so would go wrong if we tried to delete from
			  non-existant tables.			
			\*--------------------------------------------------------*/
			IF EXISTS (SELECT *
		   			FROM   SysObjects 
					WHERE  Id = Object_Id(N'[dbo].[Taxon_List_Item]')
					AND 	  Type = 'U')
			BEGIN
				-- Get the Taxon List Item Key for the current Concept
				DECLARE @TaxonListItemKey char(16)
	
				SELECT 	@TaxonListItemKey = Taxon_List_Item_Key
				FROM	Taxon_Dictionary_Concept_Mapping
				WHERE	Concept_Key = @Key

				/*--------------------------------------------------------*\
				  Delete the records related to the Taxon_List_Item table
				\*--------------------------------------------------------*/
				DELETE 	Taxon_Dictionary_Concept_Mapping
				WHERE	Taxon_List_Item_Key = @TaxonListItemKey
				AND	Concept_Key = @Key

				IF @@Error <> 0 GOTO RollbackAndExit

				DELETE	Taxon_Common_Name
				WHERE	Taxon_List_Item_Key = @TaxonListItemKey

				IF @@Error <> 0 GOTO RollbackAndExit

				DELETE	Index_Taxon_Synonym
				WHERE	Taxon_List_Item_Key = @TaxonListItemKey
				OR	Synonym_List_Item_Key = @TaxonListItemKey

				IF @@Error <> 0 GOTO RollbackAndExit

				DELETE	Export_Filter_Taxon
				WHERE	Taxon_List_Item_Key = @TaxonListItemKey

				IF @@Error <> 0 GOTO RollbackAndExit

				DELETE	Index_Taxon_Group
				WHERE	Taxon_List_Item_Key = @TaxonListItemKey
				OR	Contained_List_Item_Key = @TaxonListItemKey

				IF @@Error <> 0 GOTO RollbackAndExit

				DELETE	Taxon_Designation
				WHERE	Taxon_List_Item_Key = @TaxonListItemKey

				IF @@Error <> 0 GOTO RollbackAndExit

				DELETE	Taxon_User_Name
				WHERE	Taxon_List_Item_Key = @TaxonListItemKey

				IF @@Error <> 0 GOTO RollbackAndExit

				DELETE	Index_Taxon_Name
				WHERE	Taxon_List_Item_Key = @TaxonListItemKey

				IF @@Error <> 0 GOTO RollbackAndExit

				DELETE	Nameserver
				WHERE	Recommended_Taxon_List_Item_Key = @TaxonListItemKey

				IF @@Error <> 0 GOTO RollbackAndExit

				-- If one Concept shares the Term, attempt to delete the equivalent Taxon.
				IF @ConceptsSharingTermKeyCount = 1
				BEGIN
					DECLARE @TaxonKey char(16)

					-- Get the key of the equivalent Taxon
					SELECT 	@TaxonKey = Taxon_Key
					FROM	Taxon_Dictionary_Term_Mapping
					WHERE	Term_Key = @TermKey

							-- Only delete if there are no Taxon_Version records using the Taxon
					IF NOT EXISTS(SELECT 	*
									FROM 	Taxon_Version
									WHERE	Taxon_Key = @TaxonKey)
					BEGIN
						DELETE SF
						FROM Source_File SF
						INNER JOIN Taxon_Sources TS ON TS.Source_Key=SF.Source_Key
						WHERE TS.Taxon_Key=@TaxonKey
		
						DELETE Taxon_Sources
						WHERE Taxon_Key=@TaxonKey
					
						DELETE	Taxon
						WHERE	Taxon_Key = @TaxonKey
					END
				END

				/*-----------------------------------------------------------------*\
				  It is possible that this delete will fail. e.g. If the TLI record
				  is referred to in the Taxon_Determination table, or a row in 
				  the TLI table has its Parent set to the record we are attempting
				  to delete. This will cause it to go to the RollbackAndExit method,
				  where the user can be asked if they want to replace the concept
				  with another (4.2.17.18). Before deleting the TLI records, we
				  need to remove the Taxon_Dictionary_Meaning_Mapping records.
				\*-----------------------------------------------------------------*/ 
				DELETE	Taxon_Dictionary_Meaning_Mapping
				WHERE	Preferred_Name = @TaxonListItemKey

				IF @@Error <> 0 GOTO RollbackAndExit

				DELETE 	Taxon_List_Item
				WHERE	Taxon_List_Item_Key = @TaxonListItemKey

				IF @@Error <> 0 GOTO RollbackAndExit
			END
		END			

		/*====================================*\
		  Delete the synonyms that are no longer
		  required.
		\*====================================*/
		IF @DeleteUnlinkedSynonyms=1
		BEGIN
			DECLARE @SynConceptKey CHAR(16)
			
			DECLARE csr CURSOR FOR
				SELECT CSyn.Concept_Key
				FROM Concept C
				INNER JOIN Concept CSyn 
					ON CSyn.Meaning_Key=C.Meaning_Key
					AND CSyn.Concept_Group_Key=C.Concept_Group_Key
					AND CSyn.List_Preferred=0
					AND C.Concept_Key=@Key
			
			OPEN csr
			WHILE (1=1)
			BEGIN
				FETCH NEXT FROM csr INTO @SynConceptKey

				IF @@FETCH_STATUS <> 0
					BREAK

				-- Recurse to remove synonym concepts
				EXEC usp_Concept_Delete @SynConceptKey
			END
			CLOSE csr
			DEALLOCATE csr
		END
	
		/*====================================*\
		  Delete the records.
		\*====================================*/
		--Delete the concept's search terms
		DELETE	Search_Term
		WHERE	Concept_Key = @Key
			
		IF @@Error <> 0 GOTO RollbackAndExit

		-- If this is a descriptor parameter, delete related collection unit data.
		DELETE FROM Collection_Unit_Data
		WHERE Parameter_Concept_Key = @Key

		IF @@Error <> 0 GOTO RollbackAndExit

		-- Delete the Concept_History record.
		DELETE	Concept_History
		WHERE	Concept_Key = @Key

		IF @@Error <> 0 GOTO RollbackAndExit

		/*-------------------------------------------------------*\
		  Delete the relation records which refer to the concept.
		\*-------------------------------------------------------*/
		DELETE	Concept_Relation
		WHERE	To_Concept_Key = @Key
		OR	From_Concept_Key = @Key

		IF @@Error <> 0 GOTO RollbackAndExit

		DELETE	Meaning_Relation
		WHERE	To_Concept_Key = @Key
		OR	From_Concept_Key = @Key

		IF @@Error <> 0 GOTO RollbackAndExit

		DELETE	Term_Version_Relation
		WHERE	To_Concept_Key = @Key
		OR	From_Concept_Key = @Key

		IF @@Error <> 0 GOTO RollbackAndExit

		/*-------------------------------------------------------*\
		  Delete the Enquiry_Concept records because otherwise
		  the deletion will fail because it says other records
		  link to the Concept. Enquiries cannot be viewed in the
		  Thesaurus Editor it would appear at a casual glance
		  that nothing is actually linked to the concept. 
		  So best to just delete the Enquiry_Concept join records.
		\*-------------------------------------------------------*/
		IF EXISTS (SELECT * FROM dbo.sysobjects WHERE id = object_id(N'[dbo].[Enquiry_Concept]') 
					AND OBJECTPROPERTY(id, N'IsUserTable') = 1)
			DELETE	Enquiry_Concept
			WHERE	Concept_Key = @Key

		IF @@Error <> 0 GOTO RollbackAndExit

		-- Delete the Concept_Lineage records.
		IF EXISTS (SELECT 1 FROM Concept WHERE Concept_Key = @Key)
		BEGIN
			EXECUTE		usp_ConceptLineage_DeleteConcept	@Key
			IF @@ERROR <> 0 GOTO RollbackAndExit
		END

		/*-------------------------------------------------------*\
			Delete the concept's designation records (and related)
		\*-------------------------------------------------------*/
		IF EXISTS (SELECT * FROM dbo.sysobjects WHERE id = object_id(N'[dbo].[Taxon_Dictionary_Concept_Designation_Mapping]') 
					AND OBJECTPROPERTY(id, N'IsUserTable') = 1)
			DELETE DM
			FROM Taxon_Dictionary_Concept_Designation_Mapping DM
			INNER JOIN Concept_Designation CD ON CD.Concept_Designation_Key=DM.Concept_Designation_Key
			WHERE CD.Concept_Key=@Key

		IF @@Error <> 0 GOTO RollbackAndExit		

		IF EXISTS (SELECT * FROM dbo.sysobjects WHERE id = object_id(N'[dbo].[Source_Join]') 
					AND OBJECTPROPERTY(id, N'IsUserTable') = 1)
		BEGIN
			--Delete the source files
			DELETE SF
			FROM Source_File SF
			INNER JOIN Source_Join SJ
					ON SJ.Table_Name='Concept_Designation'
			INNER JOIN Concept_Designation CD
					ON CD.Concept_Designation_Key=SJ.Record_Key
					AND CD.Concept_Key=@Key
	
			IF @@Error <> 0 GOTO RollbackAndExit
		
			--Now delete the source joins
			DELETE SJ
			FROM Source_Join SJ
			INNER JOIN Concept_Designation CD
					ON CD.Concept_Designation_Key=SJ.Record_Key
					AND CD.Concept_Key=@Key
			WHERE SJ.Table_Name='Concept_Designation'

			IF @@Error <> 0 GOTO RollbackAndExit

			--Delete the source files for the main concept
			DELETE SF
			FROM Source_File SF
			INNER JOIN Source_Join SJ
					ON SJ.Table_Name='Concept' AND SJ.Record_Key=@Key

			IF @@Error <> 0 GOTO RollbackAndExit

			--Now delete the source joins
			DELETE SJ
			FROM Source_Join SJ
			WHERE SJ.Table_Name='Concept' AND SJ.Record_Key=@Key

			IF @@Error <> 0 GOTO RollbackAndExit
		END

		DELETE 
		FROM Concept_Designation
		WHERE Concept_Key=@Key

		/*-------------------------------------------------------*\
			 Delete the Concept record. Have to check timestamp passed into the proc
			 against the timestamp the Concept had before any of its related records
			 were deleted. This is because deleting the records above may cause
			 triggers to be fired. Deleting the record in Concept_History will fire
			 a trigger that updates the current Concept, causing its timestamp to 
			 change.
		\*-------------------------------------------------------*/

		DELETE	Concept
		WHERE	Concept_Key = @Key
		AND		(@Timestamp = @OriginalTimestamp OR @Timestamp IS NULL)

		-- VI 13430 - CCN178 - TSEQUAL and stored procs
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Concept WHERE Concept_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

		IF @RecordsAffected = 0 AND EXISTS (
			SELECT Concept_Key FROM Concept WHERE Concept_Key = @Key
		)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
		END

		-- Delete the Meaning record if only one Concept uses that Meaning key. Also
		-- delete any Homonym_Pair records with this meaning key.
		IF @ConceptsSharingMeaningKeyCount = 1 
		BEGIN
			DELETE	Homonym_Pair
			WHERE	Meaning_Key_1 = @MeaningKey
			OR		Meaning_Key_2 = @MeaningKey

			DELETE	Taxon_Dictionary_Meaning_Mapping
			WHERE	Meaning_Key = @MeaningKey

			DELETE 	Meaning
			WHERE	Meaning_Key = @MeaningKey
		END

		IF @@Error <> 0 GOTO RollbackAndExit

		-- Delete the Term Version record if only one Concept uses that Term Version key.
		IF @ConceptsSharingTermVersionKeyCount = 1
			DELETE	Term_Version
			WHERE	Term_Version_Key = @TermVersionKey

		IF @@Error <> 0 GOTO RollbackAndExit

		-- Delete the Term record if only one Concept uses that Term key.
		IF @ConceptsSharingTermKeyCount = 1
			IF NOT EXISTS(SELECT * FROM Term_Version WHERE Term_Key = @TermKey)	
				DELETE	Term
				WHERE	Term_Key = @TermKey

		IF @@Error <> 0 GOTO RollbackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
	RAISERROR ('usp_Concept_Delete failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Concept_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concept_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concept_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Concept_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Concept_Delete TO [Dev - JNCC SQL]
END
GO
/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Concept_Insert]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Concept_Insert]
GO

/*===========================================================================*\
  Description:	Inserts a new concept and term if necessary.

  Parameters:	@Key	
		@ConceptGroupKey
		@TermName
		@PlainText 
		@LanguageKey 
		@SessionID 
		@NameTypeConceptKey 
		@IsSystem bit
		@PublishedTerm
		@AutomaticPublishedTerm
		@TermVersionKey

  Created:	August 2003

  Last revision information:
    $Revision: 2 $
    $Date: 26/08/11 17:21 $
    $Author: Jamesbichard $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_Concept_Insert]
	@Key char(16) OUTPUT,
	@ConceptGroupKey char(16),
	@TermName nvarchar(100),
	@PlainText nvarchar(100) = NULL,
	@LanguageKey varchar(4) = NULL,
	@SessionID char(16),
	@NameTypeConceptKey char(16) = NULL,
	@IsSystem bit = NULL,
	@PublishedTerm NVARCHAR(450) = NULL,
	@AutomaticPublishedTerm BIT = NULL,
	@TermVersionKey CHAR(16) = NULL
AS
	SET NOCOUNT ON
	SET ANSI_NULLS ON
	SET ANSI_PADDING ON
	SET ANSI_WARNINGS ON
	SET CONCAT_NULL_YIELDS_NULL ON
	SET QUOTED_IDENTIFIER ON
	SET NO_BROWSETABLE OFF
	SET XACT_ABORT ON

	BEGIN TRANSACTION

	DECLARE @NewTermKey char(16), 
		@NewConceptHistoryKey char(16),
		@SiteID char(8),
		@ConceptGroupVersionKey char(16),
		@SystemSuppliedData bit,
		@NewMeaningKey char(16)

	IF @AutomaticPublishedTerm IS NULL
		SET @AutomaticPublishedTerm = 1

	IF @IsSystem = 1
	BEGIN
		SET @SiteID = 'SYSTEM00'
		SET @SystemSuppliedData = 1
	END
	ELSE
	BEGIN
		SELECT @SiteID = Data FROM Setting WHERE [Name] = 'SiteID'
		SET @SystemSuppliedData = 0
	END

	IF NOT EXISTS(SELECT * FROM Concept_Group WHERE Concept_Group_Key = @ConceptGroupKey)
	BEGIN
		RAISERROR('Invalid CONCEPT_GROUP_KEY, Concept Group does not exist', 16, 1)
		GOTO RollbackAndExit
	END
	ELSE
	BEGIN
		-- if no NameTypeConcept Key, use default one.
		IF @NameTypeConceptKey IS NULL SET @NameTypeConceptKey = 'SYSTEM00000000AN'

		-- if plaintext is null, set it equal to TermName.
		IF @PlainText IS NULL SET @PlainText = @TermName
		
		IF @PublishedTerm IS NULL SET @PublishedTerm = @TermName
		
		-- If Language not specified, get the one with Priority of 1.
		IF @LanguageKey IS NULL
			SELECT	@LanguageKey = Language_Key
			FROM	Language
			WHERE	Priority = 1

		/*-------------------------------------------------------------*\
		  Find out if new Term is required and create if needed.
		\*-------------------------------------------------------------*/
		SELECT 	@NewTermKey = Term_Key 
		FROM 	Term 
		WHERE 	Plaintext = dbo.ufn_RemoveHtmlMarkup(@TermName)
		AND 	Language_Key = @LanguageKey

		IF @NewTermKey IS NULL
		BEGIN
			EXEC spNextKey 'Term', @NewTermKey OUTPUT, @SiteID
			IF @@Error <> 0 GOTO RollbackAndExit
			INSERT INTO Term (
				Term_Key, Language_Key, Plaintext, Entered_Session_ID, 
				System_Supplied_Data
			) VALUES (
				@NewTermKey, @LanguageKey, @PlainText, @SessionID, @SystemSuppliedData
			)
			IF @@Error <> 0 GOTO RollbackAndExit
		END
		
		/*-------------------------------------------------------------*\
		  Create a Term_Version if an existing key wasn't supplied...
		\*-------------------------------------------------------------*/
		IF @TermVersionKey IS NULL
		BEGIN
			EXECUTE spNextKey 'Term_Version', @TermVersionKey OUTPUT
			
			IF @@Error <> 0 GOTO RollbackAndExit
			
			INSERT INTO	dbo.Term_Version
						(
							Term_Version_Key,
							Term_Key,
							Version_Label,
							Author_And_Date,
							Entered_Session_ID,
							Changed_Session_ID,
							Custodian
						)
			VALUES		(
							@TermVersionKey,
							@NewTermKey,
							NULL,
							NULL,
							@SessionID,
							NULL,
							NULL
						)
		END
		
		/*-------------------------------------------------------------*\
		  Create new Concept and Meaning.
		\*-------------------------------------------------------------*/
		EXEC spNextKey 'Concept', @Key OUTPUT, @SiteID
		IF @@Error <> 0 GOTO RollbackAndExit

		EXEC spNextKey 'Meaning', @NewMeaningKey OUTPUT, @SiteID
		IF @@Error <> 0 GOTO RollbackAndExit

		INSERT INTO [dbo].[Meaning] (Meaning_Key) VALUES (@NewMeaningKey)
		IF @@Error <> 0 GOTO RollbackAndExit

		-- Now insert the Concept.
		INSERT INTO Concept (
			Concept_Key, Term_Key, Term_Version_Key, Concept_Group_Key, List_Preferred, Preferred, Is_Current, 
			Name_Type_Concept_Key, Meaning_Key, Entered_Session_ID, System_Supplied_Data,
			Published_Term, Automatic_Published_Term
		) VALUES (
			@Key, @NewTermKey, @TermVersionKey, @ConceptGroupKey, 1, 1, 1, 
			@NameTypeConceptKey, @NewMeaningKey, @SessionID, @SystemSuppliedData,
			@PublishedTerm, @AutomaticPublishedTerm
		)
		IF @@Error <> 0 GOTO RollbackAndExit

		/*-------------------------------------------------------------*\
		  Create new Concept_History.
		\*-------------------------------------------------------------*/
		EXEC spNextKey 'Concept_History', @NewConceptHistoryKey OUTPUT, @SiteID
		IF @@Error <> 0 GOTO RollbackAndExit
		INSERT INTO Concept_History (
			Concept_History_Key, Concept_Key, Concept_Group_Version_From,
			Entered_Session_ID, System_Supplied_Data
		)
			SELECT TOP 1 	@NewConceptHistoryKey, @Key, Concept_Group_Version_Key ,
					@SessionID, @SystemSuppliedData
			FROM 		Concept_Group_Version
			WHERE 		Concept_Group_Key = @ConceptGroupKey
			ORDER BY 	From_Vague_Date_Start DESC
		IF @@Error <> 0 GOTO RollbackAndExit

		/*-------------------------------------------------------------*\
		  Create Concept_Lineage
		\*-------------------------------------------------------------*/
		EXECUTE		usp_ConceptLineage_NewConcept	@Key
		IF @@ERROR <> 0 GOTO RollbackAndExit
	END

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_Concept_Insert failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Concept_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Concept_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concept_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concept_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Concept_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Concept_Insert TO [Dev - JNCC SQL]
END
GO
/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
       FROM   SysObjects 
       WHERE  Id = Object_Id(N'[dbo].[usp_Concept_Select_ForConceptGroupSearch]')
       AND    Type = 'P')
    DROP PROCEDURE [dbo].[usp_Concept_Select_ForConceptGroupSearch]
GO

/*===========================================================================*\
  Description:  Retrieves a list of concepts that match a search string, in a 
                                specified concept group.

  Parameters:   @ConceptGroup - key of the concept group
                            @SearchText - search text

  Created:  August 2003

  Last revision information:
    $Revision: 2 $
    $Date: 26/08/11 17:21 $
    $Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Concept_Select_ForConceptGroupSearch]
	@SearchKey char(16),
	@SearchText varchar(100),
	@SearchSize int = NULL
AS

SET ANSI_NULLS ON
SET ANSI_PADDING ON
SET ANSI_WARNINGS ON
SET ARITHABORT ON
SET CONCAT_NULL_YIELDS_NULL ON
SET QUOTED_IDENTIFIER ON
SET NO_BROWSETABLE OFF

    SET @SearchSize =   ISNULL(@SearchSize, 0)

    SET ROWCOUNT @SearchSize

	SELECT DISTINCT CT.Concept_Key as Item_Key,
	  CT.Item_Name AS DisplayTerm,
	  CT.Item_Name AS SearchTerm,
	  CT.Author_copy,
	  CT.Concept_Rank_Key
	FROM VW_ConceptTerm CT
	LEFT JOIN Search_Term ST on ST.Concept_Key = CT.Concept_Key
	WHERE CT.Concept_Group_Key = @SearchKey
	AND (ST.Plaintext like @SearchText + '%' 
	OR CT.Author_Copy like @SearchText + '%')
	AND CT.Is_Current = 1
	ORDER BY SearchTerm, Author_Copy

    SET ROWCOUNT 0    
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_Select_ForConceptGroupSearch') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_Concept_Select_ForConceptGroupSearch'
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        GRANT EXECUTE ON dbo.usp_Concept_Select_ForConceptGroupSearch TO [R2k_AddOnly]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
        GRANT EXECUTE ON dbo.usp_Concept_Select_ForConceptGroupSearch TO [R2k_Administrator]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
        GRANT EXECUTE ON dbo.usp_Concept_Select_ForConceptGroupSearch TO [R2k_FullEdit]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
        GRANT EXECUTE ON dbo.usp_Concept_Select_ForConceptGroupSearch TO [R2k_ReadOnly]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
        GRANT EXECUTE ON dbo.usp_Concept_Select_ForConceptGroupSearch TO [R2k_RecordCardsOnly]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_Concept_Select_ForConceptGroupSearch TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Concept_Select_ForConceptGroupVersionSearch]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Concept_Select_ForConceptGroupVersionSearch]
GO

/*===========================================================================*\
  Description:	Retrieves a list of concepts that match a search string, in a 
 								specified concept group version.

  Parameters:	@SearchKey - key of the concept group version
							@SearchText - search text

  Created:	August 2003

  Last revision information:
    $Revision: 2 $
    $Date: 26/08/11 17:21 $
    $Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Concept_Select_ForConceptGroupVersionSearch]
	@SearchKey char(16),
  @SearchText varchar(100)
AS

SET ANSI_NULLS ON
SET ANSI_PADDING ON
SET ANSI_WARNINGS ON
SET ARITHABORT ON
SET CONCAT_NULL_YIELDS_NULL ON
SET QUOTED_IDENTIFIER ON
SET NO_BROWSETABLE OFF


SELECT DISTINCT CT.Concept_Key as Item_Key, 
  CT.Item_Name AS DisplayTerm,
  CT.Item_Name AS SearchTerm, 
  CT.Author_copy,
  CT.Concept_Rank_Key
FROM VW_ConceptTerm CT
  INNER JOIN Concept_Group_Version CGV on CGV.Concept_Group_Key=CT.Concept_Group_Key
      AND CGV.Concept_Group_Version_Key=@SearchKey
  LEFT JOIN Search_Term ST on ST.Concept_Key = CT.Concept_Key
  LEFT JOIN Concept_History CH on CH.Concept_Key=CT.Concept_Key
  LEFT JOIN Concept_Group_Version CGV1 ON CGV1.Concept_Group_Version_Key=CH.Concept_Group_Version_From
  LEFT JOIN Concept_Group_Version CGV2 ON CGV2.Concept_Group_Version_Key=CH.Concept_Group_Version_To
WHERE (ST.Plaintext like @SearchText + '%'
  OR CT.Author_Copy like @SearchText + '%')
  AND (CGV1.Concept_Group_Version_Key IS NULL OR CGV1.Sequence<=CGV.Sequence)
  AND (CGV2.Concept_Group_Version_Key IS NULL OR CGV2.Sequence>=CGV.Sequence)
ORDER BY CT.SearchTerm, CT.Author_Copy
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_Select_ForConceptGroupVersionSearch') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Concept_Select_ForConceptGroupVersionSearch'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Concept_Select_ForConceptGroupVersionSearch TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForConceptGroupVersionSearch TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForConceptGroupVersionSearch TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForConceptGroupVersionSearch TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForConceptGroupVersionSearch TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Concept_Select_ForConceptGroupVersionSearch TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Concept_Select_ForKeyListAndGroup]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Concept_Select_ForKeyListAndGroup]
GO
    
/*===========================================================================*\
  Description:	Returns all concept keys relating to the group and in a list of keys.

  Parameters:	@Concept_Group_Key	Key of the Concept group whose members we're retrieving
		@Key#			Keys to match

  Created:	August 2003

  Last revision information:
    $Revision: 2 $
    $Date: 26/08/11 17:21 $
    $Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Concept_Select_ForKeyListAndGroup]
	@Concept_Group_Key1 as Char(16),
	@Concept_Group_Key2 as Char(16) = NULL,
	@Key1 as Char(16),
	@Key2 as Char(16) = NULL,
	@Key3 as Char(16) = NULL,
	@Key4 as Char(16) = NULL,
	@Key5 as Char(16) = NULL,
	@Key6 as Char(16) = NULL,
	@Key7 as Char(16) = NULL,
	@Key8 as Char(16) = NULL,
	@Key9 as Char(16) = NULL,
	@Key10 as Char(16) = NULL
AS
	SET NOCOUNT ON

	SELECT		C.Concept_Key, C.Published_Term as Plaintext
	FROM		Concept C  
	WHERE 		(C.Concept_Group_Key = @Concept_Group_Key1
	OR		 C.Concept_Group_Key = @Concept_Group_Key2)
	AND		C.List_Preferred = 1
	AND		C.Is_Current = 1
	AND		Concept_Key IN (@Key1, @Key2, @Key3, @Key4, @Key5, @Key6, @Key7, @Key8,@Key9, @Key10)
	ORDER BY	C.Sort_Code, C.Published_Term
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_Select_ForKeyListAndGroup') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Concept_Select_ForKeyListAndGroup'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Concept_Select_ForKeyListAndGroup TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForKeyListAndGroup TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForKeyListAndGroup TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForKeyListAndGroup TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForKeyListAndGroup TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Concept_Select_ForKeyListAndGroup TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
       FROM   SysObjects 
       WHERE  Id = Object_Id(N'[dbo].[usp_Concept_Select_ForSubjectAreaSearch]')
       AND    Type = 'P')
    DROP PROCEDURE [dbo].[usp_Concept_Select_ForSubjectAreaSearch]
GO

/*===========================================================================*\
  Description:  Retrieves a list of concepts that match a search string, in a 
                                specified subject area.

  Parameters:   @SearchKey - key of the subject area
                            @SearchText - search text

  Created:  August 2003

  Last revision information:
    $Revision: 2 $
    $Date: 26/08/11 17:21 $
    $Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Concept_Select_ForSubjectAreaSearch]
	@SearchKey char(16),
	@SearchText varchar(100),
	@SearchSize int = NULL
AS

SET ANSI_NULLS ON
SET ANSI_PADDING ON
SET ANSI_WARNINGS ON
SET ARITHABORT ON
SET CONCAT_NULL_YIELDS_NULL ON
SET QUOTED_IDENTIFIER ON
SET NO_BROWSETABLE OFF

    SET @SearchSize =   ISNULL(@SearchSize, 0)

    SET ROWCOUNT @SearchSize

	SELECT CT.Concept_Key as Item_Key,
	  CT.Item_Name AS DisplayTerm,
	  CT.Item_Name AS SearchTerm,
	  CT.Author_copy,
	  CT.Concept_Rank_Key
	FROM VW_ConceptTerm CT
	INNER JOIN Concept_Group CG ON CT.Concept_Group_Key = CG.Concept_Group_Key
	INNER JOIN Local_Domain LD ON CG.Local_Domain_Key = LD.Local_Domain_Key
	INNER JOIN Domain D ON LD.Domain_Key = D.Domain_Key
	LEFT JOIN Search_Term ST ON ST.Concept_Key = CT.Concept_Key
	WHERE D.Subject_Area_Key = @SearchKey
	AND (ST.Plaintext like @SearchText + '%'
	OR CT.Author_Copy like @SearchText + '%')
	AND CT.Is_Current = 1
	ORDER BY CT.SearchTerm, CT.Author_Copy

    SET ROWCOUNT 0    
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_Select_ForSubjectAreaSearch') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_Concept_Select_ForSubjectAreaSearch'
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        GRANT EXECUTE ON dbo.usp_Concept_Select_ForSubjectAreaSearch TO [R2k_AddOnly]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
        GRANT EXECUTE ON dbo.usp_Concept_Select_ForSubjectAreaSearch TO [R2k_Administrator]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
        GRANT EXECUTE ON dbo.usp_Concept_Select_ForSubjectAreaSearch TO [R2k_FullEdit]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
        GRANT EXECUTE ON dbo.usp_Concept_Select_ForSubjectAreaSearch TO [R2k_ReadOnly]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
        GRANT EXECUTE ON dbo.usp_Concept_Select_ForSubjectAreaSearch TO [R2k_RecordCardsOnly]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_Concept_Select_ForSubjectAreaSearch TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].usp_Concept_UpdateAutomaticPublishedTerm')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].usp_Concept_UpdateAutomaticPublishedTerm
GO

/*===========================================================================*\
  Description:	Returns all concepts which belong to the domain/local domain/
				concept group specified, or descend from the concept specified

  Parameters:	

  Created:	August 2011

  Last revision information:
    $Revision: 2 $
    $Date: 26/08/11 17:21 $
    $Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].usp_Concept_UpdateAutomaticPublishedTerm
	@ConceptKey char(16),
	@AutomaticPublishedTerm bit,
	@RecordsAffected int = 1 OUTPUT
AS
	SET NOCOUNT OFF
	
	BEGIN TRANSACTION
		DECLARE @Error INT

		UPDATE Concept
		SET Automatic_Published_Term = @AutomaticPublishedTerm
		WHERE Concept_Key = @ConceptKey

		SELECT @RecordsAffected = @@ROWCOUNT, @Error = @@Error

		IF @Error <> 0 GOTO RollbackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollbackAndExit:
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_UpdateAutomaticPublishedTerm') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Concept_UpdateAutomaticPublishedTerm'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Concept_UpdateAutomaticPublishedTerm TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concept_UpdateAutomaticPublishedTerm TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concept_UpdateAutomaticPublishedTerm TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Concept_UpdateAutomaticPublishedTerm TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Concept_UpdateAutomaticPublishedTerm TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Concept_UpdateAutomaticPublishedTerm TO [Dev - JNCC SQL]
END

GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_DeterminationName_Get]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_DeterminationName_Get]
GO

/*===========================================================================*\
  Description:	Gets the determination name given a determination key for a
		life science or earth science determination.

  Parameters:	@Key
		@IsLifeScience
		@Caption 	OUTPUT	

  Created:	November 2003

  Last revision information:
    $Revision: 2 $
    $Date: 26/08/11 17:21 $
    $Author: Jamesbichard $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_DeterminationName_Get] 
@Key CHAR(16),
@IsLifeScience bit,
@Caption VARCHAR(100) OUTPUT

AS
	SET NOCOUNT ON
	SET ANSI_NULLS ON
	SET ANSI_PADDING ON
	SET ANSI_WARNINGS ON
	SET ARITHABORT ON
	SET CONCAT_NULL_YIELDS_NULL ON
	SET QUOTED_IDENTIFIER ON
	SET NO_BROWSETABLE OFF

	IF @IsLifeScience = 1 
		SELECT		@Caption = Preferred_Name
		FROM		Index_Taxon_Name
		WHERE 		Taxon_List_Item_Key = @Key
	ELSE
		SELECT		@Caption = Item_Name
		FROM 		VW_ConceptTerm CT
		WHERE 		Concept_Key = @Key
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_DeterminationName_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_DeterminationName_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_DeterminationName_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_DeterminationName_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_DeterminationName_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_DeterminationName_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_DeterminationName_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_DeterminationName_Get TO [Dev - JNCC SQL]
END

GO
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_DeterminationsEarthSciences_Select_ForSearch') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_DeterminationsEarthSciences_Select_ForSearch]
GO

/*===========================================================================*\
  Description:	Returns Concept_Key and DisplayTerm when search characters are 
		entered.

  Parameters:	@SearchText
		@UserDomainMask

  Created:	October 2003

  Last revision information:
	$Revision: 2 $
	$Date: 26/08/11 17:21 $
	$Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_DeterminationsEarthSciences_Select_ForSearch] 
	@SearchText VARCHAR(100),
	@UserDomainMask INT
AS

	SET NOCOUNT ON

	SELECT 	DISTINCT
			VW.Concept_Key AS Item_Key,
			VW.Item_Name + ' - ' + CG.Item_Name AS DisplayTerm,
			VW.Item_Name + ' - ' + CG.Item_Name AS SearchTerm, 
			CG.Item_Name,
			VW.List_Preferred

	FROM		VW_ConceptTerm AS VW 
	INNER JOIN 	Concept_Group CG ON CG.Concept_Group_Key = VW.Concept_Group_Key
	INNER JOIN 	Local_Domain LD ON LD.Local_Domain_Key = CG.Local_Domain_Key
	INNER JOIN 	Domain D ON D.Domain_Key = LD.Domain_Key
			AND ((D.Domain_Mask & @UserDomainMask > 0) OR (D.Domain_Mask = 0))
			AND D.Has_Occurrences = 1
	-- Join to find out which concepts are mapped to taxa
	LEFT JOIN	Taxon_Dictionary_Concept_Mapping TDCM ON TDCM.Concept_Key = VW.Concept_Key
	LEFT JOIN	Search_Term ST ON ST.Concept_Key = VW.Concept_Key
	WHERE 		ST.PlainText LIKE @SearchText + '%'
	-- And filter out all concepts that are mapped to any taxon.
	AND		TDCM.Concept_Key IS NULL
	
	ORDER BY DisplayTerm
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_DeterminationsEarthSciences_Select_ForSearch') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_DeterminationsEarthSciences_Select_ForSearch'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_DeterminationsEarthSciences_Select_ForSearch TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_DeterminationsEarthSciences_Select_ForSearch TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_DeterminationsEarthSciences_Select_ForSearch TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_DeterminationsEarthSciences_Select_ForSearch TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_DeterminationsEarthSciences_Select_ForSearch TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_DeterminationsEarthSciences_Select_ForSearch TO [Dev - JNCC SQL]
END
GO
If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Determinations_Select_ForSearch]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Determinations_Select_ForSearch]
GO

CREATE PROCEDURE [dbo].[usp_Determinations_Select_ForSearch] 
@SearchText VARCHAR(100)

AS
--
--  DESCRIPTION
--  Returns Concept_Key and DisplayTerm when search characters are entered.
--
--	PARAMETERS
--	NAME			DESCRIPTION
--	@SearchText 		Search text used to find collections.
--
--  AUTHOR:			Anthony Simpson, Dorset Software
--  CREATED:			2003-10-20

SET NOCOUNT ON

	SELECT DISTINCT D.Concept_Key AS Item_Key, VW.Item_Name AS DisplayTerm, VW.Item_Name AS SearchTerm
	FROM		Determination AS D
	LEFT JOIN	Search_Term ST ON ST.Concept_Key = D.Concept_Key
	INNER JOIN	VW_ConceptTerm AS VW ON VW.Concept_Key = D.Concept_Key
	AND 		ST.Plaintext LIKE @SearchText + '%'
	
	UNION
	
	SELECT 		TD.Taxon_List_Item_Key AS Item_Key, ITN.Preferred_Name AS DisplayTerm, ITN.Preferred_Name AS SearchTerm
	FROM		Taxon_Determination AS TD
	INNER JOIN	Index_Taxon_Name AS ITN ON ITN.Taxon_List_Item_Key = TD.Taxon_List_Item_Key
	AND 		ITN.Preferred_Name LIKE @SearchText + '%'

	ORDER BY DisplayTerm
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Determinations_Select_ForSearch') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Determinations_Select_ForSearch'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Determinations_Select_ForSearch TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Determinations_Select_ForSearch TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Determinations_Select_ForSearch TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Determinations_Select_ForSearch TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Determinations_Select_ForSearch TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Determinations_Select_ForSearch TO [Dev - JNCC SQL]
END

GO
/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Determination_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Determination_Select]
GO

/*===========================================================================*\
  Description:	Returns data for the Determination General frame.

  Parameters:	@Key		Determination key
		@IsLifeScience 	If we want Life Sciences information, then
				we need to go to the Taxon tables, and hence,
				@IsLifeScience will be set to 1. If we want Earth
				Sciences information, we go to the Determination
				table and @IsLifeScience will be set to 0.

  Created:	October 2003

  Last revision information:
    $Revision: 2 $
    $Date: 26/08/11 17:21 $
    $Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Determination_Select]
	@Key char(16),
	@IsLifeScience bit,
	@IsSpecimenUnit bit = NULL
AS

SET NOCOUNT ON

	IF @IsLifeScience = 1 
		IF (@IsSpecimenUnit = NULL) OR (@IsSpecimenUnit = 0)	
			SELECT 		D.Taxon_List_Item_Key AS DetKey, 
					ITN.Preferred_Name AS Term,
					DM.Domain_Mask,
					D.Determination_Type_Key,
					D.Taxon_Occurrence_Key AS Occurrence_Key,
					D.Specimen_Collection_Unit_Key,
					DT.Short_Name AS Type,
					CTStat.Concept_Key AS Status_Concept_Key,
					CTStat.PlainText AS Status_Term,
					D.Confidence,
					D.Determiner AS Determiner_Name_Key,
					dbo.ufn_GetFormattedName(D.Determiner) AS Determiner,
					DR.Determiner_Role_Key,
					DR.Short_Name AS Determiner_Role,
					D.Vague_Date_Start,
					D.Vague_Date_End,
					D.Vague_Date_Type,
					D.Used_Specimen,
					D.Preferred,
					D.Method,
					D.Comment AS Notes,
					D.Inferred_Determiner,
					D.[Timestamp],
					D.Include_In_Label		
			FROM 		Taxon_Determination D
			INNER JOIN 	Index_Taxon_Name ITN ON ITN.Taxon_List_Item_Key = D.Taxon_List_Item_Key
			LEFT JOIN 	vw_ConceptTerm CTStat ON CTStat.Concept_Key = D.Nomenclatural_Status_Concept_Key
			INNER JOIN 	Determination_Type DT ON DT.Determination_Type_Key = D.Determination_Type_Key
			INNER JOIN 	Determiner_Role DR ON DR.Determiner_Role_Key = D.Determiner_Role_Key 
			LEFT JOIN 	Taxon_Dictionary_Concept_Mapping TDCM ON TDCM.Taxon_List_ITem_Key = D.Taxon_List_Item_Key
			LEFT JOIN 	Concept C ON C.Concept_Key = TDCM.Concept_Key
			LEFT JOIN 	Concept_Group CG ON CG.Concept_Group_Key = C.Concept_Group_Key
			LEFT JOIN 	Local_Domain LD ON LD.Local_Domain_Key = CG.Local_Domain_Key
			LEFT JOIN 	Domain DM ON DM.Domain_Key = LD.Domain_Key
			WHERE 		D.Taxon_Determination_Key = @Key
		ELSE
			SELECT 			
					SU.Collection_Unit_Key,		
					D.Taxon_List_Item_Key AS DetKey, 
					ITN.Preferred_Name AS Term,
					DM.Domain_Mask,
					D.Determination_Type_Key,
					D.Taxon_Occurrence_Key AS Occurrence_Key,
					D.Specimen_Collection_Unit_Key,
					DT.Short_Name AS Type,
					CTStat.Concept_Key AS Status_Concept_Key,
					CTStat.PlainText AS Status_Term,
					D.Confidence,
					D.Determiner AS Determiner_Name_Key,
					dbo.ufn_GetFormattedName(D.Determiner) AS Determiner,
					DR.Determiner_Role_Key,
					DR.Short_Name AS Determiner_Role,
					D.Vague_Date_Start,
					D.Vague_Date_End,
					D.Vague_Date_Type,
					D.Used_Specimen,
					CASE WHEN SU.Collection_Unit_Key IS NULL THEN 0
								ELSE 1
					END AS Preferred,
	
					D.Method,
					D.Comment AS Notes,
					D.Inferred_Determiner,
					D.[Timestamp],
					D.Include_In_Label			
			FROM 		Taxon_Determination D
			INNER JOIN 	Index_Taxon_Name ITN ON ITN.Taxon_List_Item_Key = D.Taxon_List_Item_Key
			LEFT JOIN 	vw_ConceptTerm CTStat ON CTStat.Concept_Key = D.Nomenclatural_Status_Concept_Key
			INNER JOIN 	Determination_Type DT ON DT.Determination_Type_Key = D.Determination_Type_Key
			INNER JOIN 	Determiner_Role DR ON DR.Determiner_Role_Key = D.Determiner_Role_Key 
			LEFT JOIN 	Taxon_Dictionary_Concept_Mapping TDCM ON TDCM.Taxon_List_ITem_Key = D.Taxon_List_Item_Key
			LEFT JOIN 	Concept C ON C.Concept_Key = TDCM.Concept_Key
			LEFT JOIN 	Concept_Group CG ON CG.Concept_Group_Key = C.Concept_Group_Key
			LEFT JOIN 	Local_Domain LD ON LD.Local_Domain_Key = CG.Local_Domain_Key
			LEFT JOIN 	Domain DM ON DM.Domain_Key = LD.Domain_Key
			LEFT JOIN	Specimen_Unit AS SU ON SU.Collection_Unit_Key = D.Specimen_Collection_Unit_Key
							AND SU.Preferred_Taxon_Determination_Key = @Key
			WHERE 		D.Taxon_Determination_Key = @Key
	ELSE
		IF (@IsSpecimenUnit = NULL) OR (@IsSpecimenUnit = 0)	
			SELECT 		CT.Concept_Key AS DetKey,
					CT.Item_Name AS Term,
					DM.Domain_Mask,
					D.Determination_Type_Key,
					D.Occurrence_Key,
					D.Specimen_Collection_Unit_Key,
					DT.Short_Name AS Type,
					CTStat.Concept_Key AS Status_Concept_Key,
					CTStat.PlainText AS Status_Term,
					D.Confidence,
					D.Determiner_Name_Key,
					dbo.ufn_GetFormattedName(D.Determiner_Name_Key ) AS Determiner,
					DR.Determiner_Role_Key,
					DR.Short_Name AS Determiner_Role,
					D.Vague_Date_Start,
					D.Vague_Date_End,
					D.Vague_Date_Type,
					D.Used_Specimen,
					D.Preferred,
					D.Method,
					D.Notes,
					D.Inferred_Determiner,
					D.[Timestamp],
					D.Include_In_Label
			FROM 		Determination D
			INNER JOIN 	vw_ConceptTerm CT ON CT.Concept_Key = D.Concept_Key
			LEFT JOIN 	vw_ConceptTerm CTStat ON CTStat.Concept_Key = D.Nomenclatural_Status_Concept_Key
			INNER JOIN 	Determination_Type DT ON DT.Determination_Type_Key = D.Determination_Type_Key
			INNER JOIN 	Determiner_Role DR ON DR.Determiner_Role_Key = D.Determiner_Role_Key 
			INNER JOIN 	Concept C ON C.Concept_Key = D.Concept_Key
			INNER JOIN 	Concept_Group CG ON CG.Concept_Group_Key = C.Concept_Group_Key
			INNER JOIN 	Local_Domain LD ON LD.Local_Domain_Key = CG.Local_Domain_Key
			INNER JOIN 	Domain DM ON DM.Domain_Key = LD.Domain_Key
			WHERE 		D.Determination_Key = @Key
		ELSE
			SELECT 		
					SU.Collection_Unit_Key,
					CT.Concept_Key AS DetKey,
					CT.Item_Name AS Term,
					DM.Domain_Mask,
					D.Determination_Type_Key,
					D.Occurrence_Key,
					D.Specimen_Collection_Unit_Key,
					DT.Short_Name AS Type,
					CTStat.Concept_Key AS Status_Concept_Key,
					CTStat.PlainText AS Status_Term,
					D.Confidence,
					D.Determiner_Name_Key,
					dbo.ufn_GetFormattedName(D.Determiner_Name_Key ) AS Determiner,
					DR.Determiner_Role_Key,
					DR.Short_Name AS Determiner_Role,
					D.Vague_Date_Start,
					D.Vague_Date_End,
					D.Vague_Date_Type,
					D.Used_Specimen,
					CASE WHEN SU.Collection_Unit_Key IS NULL THEN 0
									ELSE 1
					END AS Preferred,
					D.Method,
					D.Notes,
					D.Inferred_Determiner,
					D.[Timestamp],
					D.Include_In_Label			
			FROM 		Determination D
			INNER JOIN 	vw_ConceptTerm CT ON CT.Concept_Key = D.Concept_Key
			LEFT JOIN 	vw_ConceptTerm CTStat ON CTStat.Concept_Key = D.Nomenclatural_Status_Concept_Key
			INNER JOIN 	Determination_Type DT ON DT.Determination_Type_Key = D.Determination_Type_Key
			INNER JOIN 	Determiner_Role DR ON DR.Determiner_Role_Key = D.Determiner_Role_Key 
			INNER JOIN 	Concept C ON C.Concept_Key = D.Concept_Key
			INNER JOIN 	Concept_Group CG ON CG.Concept_Group_Key = C.Concept_Group_Key
			INNER JOIN 	Local_Domain LD ON LD.Local_Domain_Key = CG.Local_Domain_Key
			INNER JOIN 	Domain DM ON DM.Domain_Key = LD.Domain_Key
			LEFT JOIN	Specimen_Unit AS SU ON SU.Collection_Unit_Key = D.Specimen_Collection_Unit_Key
							AND SU.Preferred_Determination_Key = @Key
			WHERE 		D.Determination_Key = @Key
SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Determination_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Determination_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Determination_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Determination_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Determination_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Determination_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Determination_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Determination_Select TO [Dev - JNCC SQL]
END

GO

IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_DuplicateTerms_Merge]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
	DROP PROCEDURE [dbo].[usp_DuplicateTerms_Merge]
GO

/*===========================================================================*\
  Description:	
	Reassign records linked to @OldTermKey to @NewTermKey before deleting
	the @OldTermKey.

  Parameters:	
	@NewTermKey	Specify the key of the term to keep.
	@OldTermKey	Specify the key of the term to delete.

  Created:	
	January 2006

  Last revision information:
    $Revision: 2 $
    $Date: 26/08/11 17:21 $
    $Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_DuplicateTerms_Merge]
	@NewTermKey	CHAR(16),
	@OldTermKey	CHAR(16)
AS
	IF @NewTermKey <> @OldTermKey
	BEGIN
		SET NOCOUNT ON 

		-- Change over 
		UPDATE	Taxon_Dictionary_Term_Mapping
		SET	Term_Key = @NewTermKey
		WHERE	Term_Key = @OldTermKey

		UPDATE	Concept
		SET	Term_Key = @NewTermKey
		WHERE	Term_Key = @OldTermKey	

		/*========================================================*\
			Update term version records
		\*========================================================*/
		DECLARE @CurrentKey CHAR(16), @DuplicateKey CHAR(16), @UpdateTermKey BIT
		
		-- Update/delete term versions that use the old term key
		WHILE 1 = 1
		BEGIN			
			-- Get the first term version which uses the old term key
			SELECT @CurrentKey = Term_Version_Key
			FROM Term_Version
			WHERE Term_Key = @OldTermKey

			-- If there are no more term versions using the old key, there is nothing to do
			IF @@RowCount = 0 BREAK

			-- Flags whether the current term version should be updated or deleted
			SET @UpdateTermKey = 0

			-- Check if we have already got a term version with the new term key which has
			-- the same authority and version label as the current term version. 
			SELECT @DuplicateKey = tvduplicate.Term_Version_Key
			FROM Term_Version tvduplicate
			INNER JOIN Term_Version tvcurrent 
				ON ISNULL(tvduplicate.Version_Label, '') = ISNULL(tvcurrent.Version_Label, '')
				AND	ISNULL(tvduplicate.Author_And_Date, '') = ISNULL(tvcurrent.Author_And_Date, '')
			WHERE tvcurrent.Term_Version_Key = @CurrentKey AND tvduplicate.Term_Key = @NewTermKey

			IF @@RowCount > 0
			BEGIN
				-- If we do, only keep the term version if it is already mapped to a taxon
				-- version (VI 24145). Otherwise remove the term version
				IF EXISTS (
					SELECT * FROM Taxon_Dictionary_Term_Version_Mapping
					WHERE Term_Version_Key = @CurrentKey)
				BEGIN
					SET @UpdateTermKey = 1
				END
			END
			ELSE
			BEGIN
				-- If the term version will not duplicate an existing term version when updated,
				-- update it.
				SET @UpdateTermKey = 1
			END
			
			-- Update term version to new term key/delete term version as required. In either
			-- case, the current term version will not be considered in the next iteration of the
			-- while loop since it will either no longer have the old term key or it will have
			-- been deleted.
			IF @UpdateTermKey = 1
			BEGIN
				UPDATE Term_Version
				SET Term_Key = @NewTermKey
				WHERE Term_Version_Key = @CurrentKey
			END
			ELSE
			BEGIN
				UPDATE Concept	
				SET Term_Version_Key = @DuplicateKey
				WHERE Term_Version_Key = @CurrentKey

				UPDATE Thesaurus_Fact	
				SET Term_Version_Key = @DuplicateKey
				WHERE Term_Version_Key = @CurrentKey

				DELETE FROM Term_Version
				WHERE Term_Version_Key = @CurrentKey
			END		
		END

		-- And finally remove the unnecessary leftover term.
		DELETE	Term
		WHERE	Term_Key = @OldTermKey

		SET NOCOUNT ON
	END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_DuplicateTerms_Merge') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_DuplicateTerms_Merge'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_DuplicateTerms_Merge TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_DuplicateTerms_Merge TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_DuplicateTerms_Merge TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_DuplicateTerms_Merge TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_DuplicateTerms_Merge TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_DuplicateTerms_Merge TO [Dev - JNCC SQL]
END

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Specimens_Select_ForSearch]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Specimens_Select_ForSearch]
GO

/*===========================================================================*\
  Description:	Returns Collection_Unit_Key and DisplayTerm when search characters 
		are entered. The Specimen_Unit table does not have a Display_Caption 
		or Search_Caption field, so the caption must be constructed through 
		joins to other tables.

  Parameters:	@UserDomainMask		User's Domain Mask restricting which records may be returned.
		@SessionID 		User's SessionID.
		@SearchText 		Search text used to find collections.

  Created:	September 2003

  Last revision information:
    $Revision: 2 $
    $Date: 26/08/11 17:21 $
    $Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Specimens_Select_ForSearch] 
	@UserDomainMask int,
	@SessionID char(16),
	@ShowCommonNames BIT,
	@SearchText varchar(100)
AS

SET NOCOUNT ON


--Use temp table to build output, so silly data errors such as duplicate accessions
-- don't duplicate in the list
DECLARE @SpecimensSearch TABLE
(
	[Item_Key] [char] (16)				COLLATE database_default NULL,
	[DisplayTerm] [nvarchar] (150)		COLLATE database_default NULL,
	[SearchTerm] [nvarchar] (150)		COLLATE database_default NULL,
	[Life_Sciences] [bit] NULL
)

--Find all specimens with an earth sciences determination match
INSERT INTO 
	@SpecimensSearch (Item_Key, Life_Sciences, SearchTerm, DisplayTerm) 
SELECT DISTINCT 
	SU.Collection_Unit_Key COLLATE database_default, 
	0,
	CSearch.Published_Term COLLATE database_default AS SearchTerm,
	CSearch.Published_Term COLLATE database_default AS DisplayTerm	
FROM SPECIMEN_UNIT SU
INNER JOIN Collection_Unit CU ON SU.Collection_Unit_Key = CU.Collection_Unit_Key 
 	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
	OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
LEFT JOIN VW_SpecimenDetsEarth SDE ON SU.Collection_Unit_Key = SDE.Collection_Unit_Key
LEFT JOIN Concept C ON SDE.Concept_Key = C.Concept_Key
LEFT JOIN Concept CSearch ON CSearch.Meaning_Key=C.Meaning_Key
LEFT JOIN Search_Term ST ON ST.Concept_Key = CSearch.Concept_Key
WHERE ST.Plaintext LIKE @SearchText + '%' AND SU.Life_Sciences=0

--Find all specimens with a life sciences determination match
INSERT INTO 
	@SpecimensSearch (Item_Key, Life_Sciences, SearchTerm, DisplayTerm) 
SELECT DISTINCT 
	SU.Collection_Unit_Key COLLATE database_default, 
	1,
	ITN.Actual_Name	COLLATE database_default AS SearchTerm,
	CASE ITN.Actual_Name_Italic	
		WHEN 1 THEN '<i>' + ITN.Actual_Name + '</i>' 
		ELSE ITN.Actual_Name			
	END COLLATE database_default AS DisplayTerm	
FROM SPECIMEN_UNIT SU
INNER JOIN Collection_Unit CU ON SU.Collection_Unit_Key = CU.Collection_Unit_Key 
 	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
	OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
LEFT JOIN VW_SpecimenDetsLife SDL ON SU.Collection_Unit_Key = SDL.Collection_Unit_Key
LEFT JOIN Index_Taxon_Synonym ITS ON ITS.Taxon_List_Item_Key=SDL.Taxon_List_Item_Key
LEFT JOIN INDEX_TAXON_NAME ITN	ON ITS.Synonym_List_Item_Key = ITN.Taxon_List_Item_Key
WHERE ITN.Actual_Name LIKE @SearchText + '%' AND SU.Life_Sciences = 1

-- Update the number in case there are 2 registrations for a specimen, so we don't duplicate
-- the rows in the output results.
UPDATE @SpecimensSearch
SET 
		SearchTerm = SearchTerm + ' - ' + CUN.Number,
		DisplayTerm = DisplayTerm + ' - ' + CUN.Number
FROM @SpecimensSearch SU
INNER JOIN Collection_Unit_Number CUN ON SU.Item_key = CUN.Collection_Unit_Key 	AND CUN.Preferred = 1

-- Select table and sort appropriately
SELECT * from @SpecimensSearch
ORDER BY SearchTerm

GO




/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Specimens_Select_ForSearch') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_Specimens_Select_ForSearch'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
       	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearch TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearch TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearch TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearch TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearch TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
       	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearch TO [Dev - JNCC SQL]
END
GO
/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_TermVersion_Insert]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_TermVersion_Insert]
GO

/*===========================================================================*\
  Description:	Inserts a record into the Term_Version table

  Parameters:	@Key (Term_Version_Key)	OUTPUT
		@ConceptKey
		@VersionLabel
		@AuthorAndDate
		@SessionID
		@SyncTaxonDict
		@SystemSuppliedData
		
  Created:	December 2003

  Last revision information:
    $Revision: 2 $
    $Date: 26/08/11 17:21 $
    $Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_TermVersion_Insert]
	@Key char(16) OUTPUT,
	@ConceptKey char(16),
	@TermKey char(16) = null,
	@VersionLabel varchar(100),
	@AuthorAndDate varchar(100),
	@SessionID char(16),
	@SyncTaxonDict bit = 0,
	@SystemSuppliedData bit = NULL
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION

		IF @TermKey IS NULL
		BEGIN
			SELECT 	@TermKey = Term_Key
			FROM	Concept
			WHERE	Concept_Key = @ConceptKey
		END

		SELECT @Key = NULL

		-- Use existing term version if possible
		SELECT @Key = Term_Version_Key 
		FROM Term_Version
		WHERE (Version_Label = @VersionLabel OR (Version_Label IS NULL AND @VersionLabel IS NULL))
		AND (Author_And_Date = @AuthorAndDate OR (Author_And_Date IS NULL AND @AuthorAndDate IS NULL))
		AND Term_Key = @TermKey

		IF @Key IS NULL
		BEGIN
			EXECUTE spNextKey 'Term_Version', @Key OUTPUT
			
			INSERT INTO Term_Version (
				Term_Version_Key,
				Term_Key,
				Version_Label,
				Author_And_Date,
				Entered_Session_ID,
				System_Supplied_Data			
			) VALUES (
				@Key, 	
				@TermKey,
				@VersionLabel,
				@AuthorAndDate,
				@SessionID,
				IsNull(@SystemSuppliedData, 0)
			)
			IF @@Error <> 0 GOTO RollbackAndExit
		END

		/*-------------------------------------------------------------*\
		  Update Concept to point to new version
		\*-------------------------------------------------------------*/
		UPDATE Concept
		SET Term_Version_Key=@Key
		WHERE Concept_Key=@ConceptKey

		IF @@Error <> 0 GOTO RollbackAndExit

		/*-------------------------------------------------------------*\
			Keep mapping for version, so that deletion synching can work
		\*-------------------------------------------------------------*/
		IF (@SyncTaxonDict = 1) AND EXISTS (SELECT *	FROM	SysObjects 
						WHERE	Id = Object_Id(N'[dbo].[Taxon_Version]') AND Type = 'U')
		BEGIN
			DECLARE @TaxonVersionKey CHAR(16)
			SELECT @TaxonVersionKey=TLI.Taxon_Version_Key
			FROM Concept C
			INNER JOIN Taxon_Dictionary_Concept_Mapping CM ON CM.Concept_Key=C.Concept_Key
			INNER JOIN Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key=CM.Taxon_List_Item_Key

			IF NOT @TaxonVersionKey IS NULL
			BEGIN

				IF EXISTS(SELECT 1 FROM Taxon_Dictionary_Term_Version_Mapping WHERE Taxon_Version_Key=@TaxonVersionKey)
					UPDATE Taxon_Dictionary_Term_Version_Mapping 
					SET Term_Version_Key=@Key
					WHERE Taxon_Version_Key=@TaxonVersionKey
				ELSE
				INSERT INTO Taxon_Dictionary_Term_Version_Mapping VALUES(@TaxonVersionKey, @Key, NULL)

				IF @@Error <> 0 GOTO RollbackAndExit

			END

			IF @@Error <> 0 GOTO RollbackAndExit

		END

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TermVersion_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_TermVersion_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_TermVersion_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_TermVersion_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_TermVersion_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_TermVersion_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_TermVersion_Insert TO [Dev - JNCC SQL]
END
GO
