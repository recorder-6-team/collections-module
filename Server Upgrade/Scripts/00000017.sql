SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptGroup_Delete]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptGroup_Delete]
GO

/*===========================================================================*\
  Description:	Deletes a Concept_Group record.

  Parameters:	@Key	Concept_Group_key
		@Timestamp

  Created:	December 2003

  Last revision information:
    $Revision: 2 $
    $Date: 2/02/09 16:44 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptGroup_Delete]
	@Key char(16),
	@Timestamp timestamp = null,
	@SyncTaxonDict bit = 0
AS

SET NOCOUNT ON

	BEGIN TRANSACTION
		/*============================================================*\
		  See if the user wants any associated taxon dictionary
		  records be deleted with the Concept Group.
		\*============================================================*/
		IF @SyncTaxonDict = 1 
		BEGIN
			/*--------------------------------------------------------*\
			  Check that the Taxon_List table exists before
			  attempting any of this deletion. 
			\*--------------------------------------------------------*/
			IF EXISTS (SELECT *
		   			FROM   SysObjects 
					WHERE  Id = Object_Id(N'[dbo].[Taxon_List]')
					AND 	  Type = 'U')
			BEGIN
				DECLARE @TaxonListKey char(16)

				SELECT 	@TaxonListKey = Taxon_List_Key
				FROM	Taxon_Dictionary_Concept_Group_Mapping
				WHERE	Concept_Group_Key = @Key

				IF @@Error <> 0 GOTO RollbackAndExit

				DELETE	Taxon_List_Version
				WHERE	@TaxonListKey = Taxon_List_Key

				IF @@Error <> 0 GOTO RollbackAndExit

				DELETE	Taxon_Dictionary_Concept_Group_Mapping
				WHERE		Concept_Group_Key = @Key

				IF @@Error <> 0 GOTO RollbackAndExit

				DELETE	Taxon_List
				WHERE	Taxon_List_Key = @TaxonListKey

				IF @@Error <> 0 GOTO RollbackAndExit	
			END
		END
		ELSE
			DELETE	Taxon_Dictionary_Concept_Group_Mapping
			WHERE		Concept_Group_Key = @Key

		DELETE SF
		FROM Source_File SF
		INNER JOIN Source_Join SJ ON SJ.Source_Key=SF.Source_Key
		WHERE SJ.Table_Name='Concept_Group'
		AND SJ.Record_Key=@Key

		DELETE Source_Join
		WHERE Table_Name='Concept_Group'
		AND Record_Key=@Key
	
		DELETE 
		FROM 		Concept_Group
		WHERE		Concept_Group_Key = @Key
		AND			((@Timestamp = Timestamp) OR (@Timestamp IS NULL))

		IF @@Error <> 0 GOTO RollbackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptGroup_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptGroup_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptGroup_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptGroup_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptGroup_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptGroup_Delete TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptGroup_ImportTaxonList]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_ConceptGroup_ImportTaxonList]
GO

/*===========================================================================*\
  Description:	Import a taxon list into the specified concept group.

  Parameters:   @job_id					Job identifier
				@taxon_list_key			Taxon list key
				@concept_group_key		Concept group key

  Created:		Nov 2003

  Last revision information:
	$Revision: 2 $
	$Date: 2/02/09 16:44 $
	$Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptGroup_ImportTaxonList]
	@job_id				INT,
	@taxon_list_key		CHAR(16),
	@concept_group_key	CHAR(16)
AS
	SET NOCOUNT ON
	SET ARITHABORT ON

	DECLARE		@existing_group_key		CHAR(16)

	SELECT		@existing_group_key						=	Concept_Group_Key
	FROM		Taxon_Dictionary_Concept_Group_Mapping
	WHERE		Taxon_List_Key							=	@taxon_list_key

	IF @@ROWCOUNT = 0
	BEGIN
		BEGIN TRANSACTION

		/* record mapping */
		INSERT		Taxon_Dictionary_Concept_Group_Mapping (
					Taxon_List_Key,
					Concept_Group_Key)
		VALUES		(@taxon_list_key,
					@concept_group_key)

		IF @@ERROR <> 0 GOTO fail

		COMMIT TRANSACTION
	END
	ELSE IF @existing_group_key <> @concept_group_key
	BEGIN
		RAISERROR (
			'Taxon list has previously been imported into a different group',
			16,
			1)
		RETURN
	END

	/* Calculate size of job */
	DECLARE		@record_count					INT

	DECLARE		@items	TABLE (
				Taxon_List_Item_Key			CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS,
				Taxon_List_Version_Key		CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS,
				Taxon_Rank_Key				CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS,
				Taxon_Version_Key			CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS,
				List_Preferred_Key			CHAR(16))

	INSERT		@items
	SELECT    	tli.TAXON_LIST_ITEM_KEY,
				tli.TAXON_LIST_VERSION_KEY,
				tli.TAXON_RANK_KEY,
				tli.TAXON_VERSION_KEY,
				CASE WHEN tli.TAXON_LIST_ITEM_KEY = tli.PREFERRED_NAME
					THEN tli.TAXON_LIST_ITEM_KEY
					ELSE NULL
				END
	FROM        TAXON_LIST_VERSION			AS	tlv
	INNER JOIN	TAXON_LIST_ITEM				AS	tli
	ON			tli.TAXON_LIST_VERSION_KEY	=	tlv.TAXON_LIST_VERSION_KEY
	WHERE		tlv.TAXON_LIST_KEY			=	@taxon_list_key

	SELECT		@record_count				=	2 * COUNT(DISTINCT tli.Taxon_List_Item_Key)
												+ COUNT(DISTINCT tli.Taxon_List_Version_Key)
												+ COUNT(DISTINCT tli.Taxon_Rank_Key)
												+ COUNT(DISTINCT tli.Taxon_Version_Key)
												+ COUNT(DISTINCT tv.TAXON_KEY)
												+ COUNT(DISTINCT ts.SOURCE_LINK_KEY)
												+ COUNT(DISTINCT tx.TAXON_NAME_TYPE_KEY)
												+ COUNT(DISTINCT td.TAXON_DESIGNATION_KEY)
												+ COUNT(DISTINCT td.TAXON_DESIGNATION_TYPE_KEY)
												+ COUNT(DISTINCT tf.TAXON_FACT_KEY)
												+ COUNT(DISTINCT tli.List_Preferred_Key)
	FROM        @items						AS	tli
	INNER JOIN	TAXON_VERSION				AS	tv
	ON			tv.TAXON_VERSION_KEY		=	tli.Taxon_Version_Key
	INNER JOIN	TAXON						AS	tx
	ON			tx.TAXON_KEY				=	tv.TAXON_KEY
	LEFT JOIN	TAXON_SOURCES				AS	ts
	ON			ts.TAXON_KEY				=	tx.TAXON_KEY
	LEFT JOIN	TAXON_DESIGNATION			AS	td
	ON			td.TAXON_LIST_ITEM_KEY		=	tli.Taxon_List_Item_Key
	LEFT JOIN	TAXON_FACT					AS	tf
	ON			tf.TAXON_VERSION_KEY		=	tli.Taxon_Version_Key
	WHERE ts.SOURCE_LINK_KEY IS NOT NULL

	EXECUTE		usp_Import_Export_Job_Configure		@job_id,
													@concept_group_key,
													@record_count
	IF @@ERROR <> 0 RETURN

	/* import versions */
	EXECUTE		usp_ConceptGroupVersion_ImportTaxonList		@job_id
	IF @@ERROR <> 0 RETURN

	/* import terms */
	EXECUTE		usp_Term_ImportTaxonList	@job_id
	IF @@ERROR <> 0 RETURN

	/* import term versions */
	EXECUTE		usp_TermVersion_ImportTaxonList		@job_id
	IF @@ERROR <> 0 RETURN

	/* import concept ranks */
	EXECUTE		usp_ConceptRank_ImportTaxonList     @job_id
	IF @@ERROR <> 0 RETURN

	/* import name type concepts */
	EXECUTE		usp_Concept_ImportTaxonNameTypes    @job_id
	IF @@ERROR <> 0 RETURN

	/* import concepts */
	EXECUTE		usp_Concept_ImportTaxonList		@job_id
	IF @@ERROR <> 0 RETURN

	/* import concept relationships */
	EXECUTE		usp_ConceptRelation_ImportTaxonList		@job_id
	IF @@ERROR <> 0 RETURN

	/* (re-)create concept lineage */
	EXECUTE     usp_ConceptLineage_GenerateForGroup		@job_id
	IF @@ERROR <> 0 RETURN

	/* import term/source relationships */
	EXECUTE		usp_SourceJoin_ImportTaxonSources	@job_id
	IF @@ERROR <> 0 RETURN

	/* import designation types */
	EXECUTE		usp_Concept_ImportTaxonDesignationTypes		@job_id
	IF @@ERROR <> 0 RETURN

	/* import concept designations */
	EXECUTE		usp_ConceptDesignation_ImportTaxonList		@job_id
	IF @@ERROR <> 0 RETURN

	/* import thesaurus facts */
	EXECUTE		usp_ThesaurusFact_ImportTaxonList	@job_id
	IF @@ERROR <> 0 RETURN

	RETURN

fail:
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_ConceptGroup_ImportTaxonList failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptGroup_ImportTaxonList') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptGroup_ImportTaxonList'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptGroup_ImportTaxonList TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptGroup_ImportTaxonList TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptGroup_ImportTaxonList TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptRanks_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptRanks_Select]
GO

/*===========================================================================*\
  Description: 	Returns the concept ranks available for the concept's
		current domain.		

  Parameters:	@ConceptGroupKey

  Created:	December 2003

  Last revision information:
    $Revision: 2 $
    $Date: 2/02/09 16:44 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptRanks_Select]
	@ConceptGroupKey char(16)
AS

	SELECT
			CR.Concept_Rank_Key AS Item_Key,
			CR.Item_Name 
	FROM 		Concept_Rank AS CR
	INNER JOIN	Domain AS D ON D.Domain_Key = CR.Domain_Key
	INNER JOIN	Local_Domain AS LD ON LD.Domain_Key = D.Domain_Key
	INNER JOIN	Concept_Group AS CG ON CG.Local_Domain_Key = LD.Local_Domain_Key
	WHERE		CG.Concept_Group_Key = @ConceptGroupKey
	ORDER BY 	CR.Sort_Order, CR.Concept_Rank_Key


GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptRanks_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptRanks_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptRanks_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptRanks_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptRanks_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ConceptRanks_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptRanks_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptRanks_Select TO [Dev - JNCC SQL]
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
		from other tables where necessary.

  Parameters:	@Key		Concept key.
				@Timestamp

  Created:	December 2003

  Last revision information:
    $Revision: 2 $
    $Date: 2/02/09 16:44 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Concept_Delete]
	@Key char(16),
	@Timestamp timestamp = NULL,
	@SyncTaxonDict bit = 0
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	DECLARE @MeaningKey char(16),
			@TermKey char(16),
			@TermVersionKey char(16),
			@ConceptsSharingMeaningKeyCount int,
			@ConceptsSharingTermKeyCount int,
			@ConceptsSharingTermVersionKeyCount int,
			@OriginalTimestamp timestamp

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
		  Delete the records.
		\*====================================*/
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
		DELETE DM
		FROM Taxon_Dictionary_Concept_Designation_Mapping DM
		INNER JOIN Concept_Designation CD ON CD.Concept_Designation_Key=DM.Concept_Designation_Key
		WHERE CD.Concept_Key=@Key

		IF @@Error <> 0 GOTO RollbackAndExit		

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

		--Delete the source files
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

		DELETE	Concept
		WHERE	Concept_Key = @Key
		AND		((@Timestamp = @OriginalTimestamp) OR (@Timestamp IS NULL))

		IF @@Error <> 0 GOTO RollbackAndExit

		-- Delete the Meaning record if only one Concept uses that Meaning key.
		IF @ConceptsSharingMeaningKeyCount = 1 
			DELETE 	Meaning
			WHERE	Meaning_Key = @MeaningKey

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
	   WHERE  Id = Object_Id(N'[dbo].[usp_Concept_ImportTaxonDesignationTypes]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_Concept_ImportTaxonDesignationTypes]
GO

/*===========================================================================*\
  Description:	Import concepts corresponding to the taxon designation types
				used in the specified taxon list.

  Parameters:	@job_id					Job identifier

  Created:		Dec 2003

  Last revision information:
	$Revision: 2 $
	$Date: 2/02/09 16:44 $
	$Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Concept_ImportTaxonDesignationTypes]
	@job_id					INT
AS
	SET NOCOUNT ON
	SET ANSI_NULLS ON

	DECLARE     @taxon_list_key					CHAR(16),
				@taxon_designation_type_key		CHAR(16),
				@item_name						NVARCHAR(300),
				@ins_user_key					CHAR(16),
				@ins_date						DATETIME,
				@ins_session_id					CHAR(16),
				@upd_user_key					CHAR(16),
				@upd_date						DATETIME,
				@upd_session_id					CHAR(16),
				@system							BIT,
				@concept_designation_type_key	CHAR(16),
				@term_key						CHAR(16),
				@meaning_key					CHAR(16),
				@concept_history_key			CHAR(16)

	/* determine parameters of job */
	SELECT		@taxon_list_key							=	m.Taxon_List_Key
	FROM		Import_Export_Job						AS	j
	INNER JOIN	Taxon_Dictionary_Concept_Group_Mapping	AS	m
	ON			m.Concept_Group_Key						=	j.Concept_Group_Key
	WHERE		j.Import_Export_Job_ID					=	@job_id

	IF @@ROWCOUNT = 0
	BEGIN
		RAISERROR ('Job does not exist or has not been configured', 16, 1)
		RETURN
	END

	EXECUTE		usp_Import_Export_Job_UpdateStatus	@job_id,
													'Importing designation types'
	IF @@ERROR <> 0 RETURN

	DECLARE		types	CURSOR LOCAL FAST_FORWARD FOR
	SELECT DISTINCT
				tdt.TAXON_DESIGNATION_TYPE_KEY,
				ISNULL(tdt.LONG_NAME,
					   tdt.SHORT_NAME),
				tdt.ENTERED_BY,
				tdt.ENTRY_DATE,
				tdt.CHANGED_BY,
				tdt.CHANGED_DATE,
				tdt.SYSTEM_SUPPLIED_DATA
	FROM		TAXON_LIST_VERSION				AS	tlv
	INNER JOIN	TAXON_LIST_ITEM					AS	tli
	ON			tli.TAXON_LIST_VERSION_KEY		=	tlv.TAXON_LIST_VERSION_KEY
	INNER JOIN	TAXON_DESIGNATION				AS	td
	ON			td.TAXON_LIST_ITEM_KEY			=	tli.TAXON_LIST_ITEM_KEY
	INNER JOIN	TAXON_DESIGNATION_TYPE			AS	tdt
	ON			tdt.TAXON_DESIGNATION_TYPE_KEY	=	td.TAXON_DESIGNATION_TYPE_KEY
	WHERE		tlv.TAXON_LIST_KEY				=	@taxon_list_key

	OPEN		types

	WHILE 1 = 1
	BEGIN
		FETCH		types
		INTO		@taxon_designation_type_key,
					@item_name,
					@ins_user_key,
					@ins_date,
					@upd_user_key,
					@upd_date,
					@system

		IF @@FETCH_STATUS <> 0 BREAK

		BEGIN TRANSACTION

		/* obtain session identifiers */
		EXECUTE		usp_Session_ForDate		@ins_user_key,
											@ins_date,
											@ins_session_id		OUTPUT
		IF @@ERROR <> 0 GOTO fail_from_cursor

		IF @upd_user_key IS NULL
		BEGIN
			SET			@upd_session_id		=	NULL
		END
		ELSE
		BEGIN
			EXECUTE		usp_Session_ForDate		@upd_user_key,
												@upd_date,
												@upd_session_id		OUTPUT
			IF @@ERROR <> 0 GOTO fail_from_cursor
		END

		SELECT		@concept_designation_type_key				=	tdm.Concept_Designation_Type_Key,
					@term_key									=	c.Term_Key
		FROM		Taxon_Dictionary_Designation_Type_Mapping	AS	tdm
		INNER JOIN	Concept										AS	c
		ON			c.Concept_Key								=	tdm.Concept_Designation_Type_Key
		WHERE		tdm.Taxon_Designation_Type_Key				=	@taxon_designation_type_key

		IF @@ROWCOUNT > 0
		BEGIN
			/* designation type has previously been imported */
			IF NOT EXISTS (	SELECT		1
							FROM		Term
							WHERE		Term_Key		=	@term_key
							AND			Language_Key	=	'en'
							AND			Item_Name		=	@item_name )
			BEGIN
				/* term has changed */
				IF EXISTS (	SELECT		1
							FROM		Concept
							WHERE		Term_Key			=	@term_key
							AND			Concept_Group_Key	<>	'SYSTEM000000000T' )
				BEGIN
					/* term is linked outside this concept group; create
					 * a new term instead of updating the existing one */
					EXECUTE		spNextKey	'Term',
											@term_key	OUTPUT
					IF @@ERROR <> 0 GOTO fail_from_cursor

					INSERT		Term (
								Term_Key,
								Language_Key,
								Item_Name,
								Plaintext,
								Entered_Session_ID,
								Changed_Session_ID,
								System_Supplied_Data)
					VALUES		(@term_key,
								'en',
								@item_name,
								@item_name,
								@ins_session_id,
								@upd_session_id,
								@system)

					IF @@ERROR <> 0 GOTO fail_from_cursor
				END
				ELSE
				BEGIN
					/* term only linked within this concept group */
					DECLARE		@cur_term_key		CHAR(16)

					SELECT		@cur_term_key	=	Term_Key
					FROM		Term
					WHERE		Language_Key	=	'en'
					AND			Item_Name		=	@item_name

					IF @@ROWCOUNT = 0
					BEGIN
						/* term can simply be updated */
						UPDATE		Term
						SET			Language_Key	=	'en',
									Item_Name		=	@item_name
						WHERE		Term_Key		=	@term_key

						IF @@ERROR <> 0 GOTO fail_from_cursor
					END
					ELSE
					BEGIN
						/* term cannot be updated; there is an existing
						 * term with the same name which we will link to
						 * instead */
						DELETE		Term
						WHERE		Term_Key			=	@term_key

						IF @@ERROR <> 0 GOTO fail_from_cursor

						SET			@term_key			=	@cur_term_key
					END
				END
			END

			UPDATE		Concept
			SET			Term_Key				=	@term_key,
						Entered_Session_ID		=	@ins_session_id,
						Changed_Session_ID		=	@upd_session_id,
						System_Supplied_Data	=	@system
			WHERE		Concept_Key				=	@concept_designation_type_key

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END
		ELSE
		BEGIN
			/* find/create term */
			SELECT		@term_key		=	Term_Key
			FROM		Term
			WHERE		Language_Key	=	'en'
			AND			Item_Name		=	@item_name

			IF @@ROWCOUNT = 0
			BEGIN
				EXECUTE		spNextKey	'Term',
										@term_key	OUTPUT
				IF @@ERROR <> 0 GOTO fail_from_cursor

				INSERT		Term (
							Term_Key,
							Language_Key,
							Item_Name,
							Plaintext,
							Entered_Session_ID,
							Changed_Session_ID,
							System_Supplied_Data)
				VALUES		(@term_key,
							'en',
							@item_name,
							@item_name,
							@ins_session_id,
							@upd_session_id,
							@system)

				IF @@ERROR <> 0 GOTO fail_from_cursor
			END

			/* create Meaning */
			EXECUTE		spNextKey	'Meaning',
									@meaning_key	OUTPUT
			IF @@ERROR <> 0 GOTO fail_from_cursor

			INSERT		Meaning (
						Meaning_Key)
			VALUES		(@meaning_key)

			IF @@ERROR <> 0 GOTO fail_from_cursor

			/* create Concept */
			EXECUTE		spNextKey	'Concept',
									@concept_designation_type_key	OUTPUT
			IF @@ERROR <> 0 GOTO fail_from_cursor

			INSERT		Concept (
						Concept_Key,
						Term_Key,
						Concept_Group_Key,
						List_Preferred,
						Is_Current,
						Preferred,
						Name_Type_Concept_Key,
						Meaning_Key,
						Entered_Session_ID,
						Changed_Session_ID,
						System_Supplied_Data)
			VALUES 		(@concept_designation_type_key,
						@term_key,
						'SYSTEM000000000T', /* "Concept Designation Types" group */
						1,
						1,
						1,
						'SYSTEM0000000000', /* "Formal" -- meaningless, but
												we need a value here */
						@meaning_key,
						@ins_session_id,
						@upd_session_id,
						@system)

			IF @@ERROR <> 0 GOTO fail_from_cursor

			/* create concept history */
			EXECUTE		spNextKey	'Concept_History',
									@concept_history_key	OUTPUT
			IF @@ERROR <> 0 GOTO fail_from_cursor

			INSERT		Concept_History (
						Concept_History_Key,
						Concept_Key,
						Concept_Group_Version_From,
						Entered_Session_ID,
						Changed_Session_ID,
						System_Supplied_Data)
			VALUES		(@concept_history_key,
						@concept_designation_type_key,
						'SYSTEM000000000T', /* "Concept Designation Types" version */
						@ins_session_id,
						@upd_session_id,
						@system)

			IF @@ERROR <> 0 GOTO fail_from_cursor

			/* record taxon designation type mapping */
			INSERT		Taxon_Dictionary_Designation_Type_Mapping (
						Taxon_Designation_Type_Key,
						Concept_Designation_Type_Key)
			VALUES		(@taxon_designation_type_key,
						@concept_designation_type_key)

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END

		EXECUTE		usp_Import_Export_Job_RecordProcessed	@job_id
		IF @@ERROR <> 0 GOTO fail_from_cursor

		COMMIT TRANSACTION
	END

	CLOSE		types
	DEALLOCATE	types
	RETURN

fail_from_cursor:
	CLOSE		types
	DEALLOCATE	types

fail:
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_Concept_ImportTaxonDesignationTypes failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_ImportTaxonDesignationTypes') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Concept_ImportTaxonDesignationTypes'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concept_ImportTaxonDesignationTypes TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concept_ImportTaxonDesignationTypes TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Concept_ImportTaxonDesignationTypes TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_Concept_ImportTaxonList]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_Concept_ImportTaxonList]
GO

/*===========================================================================*\
  Description:	Import concepts corresponding to the contents of a taxon list.

  Parameters:	@job_id					Job identifier

  Created:		Nov 2003

  Last revision information:
	$Revision: 2 $
	$Date: 2/02/09 16:44 $
	$Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Concept_ImportTaxonList]
	@job_id				INT
AS
	SET NOCOUNT ON
	SET ANSI_NULLS ON

	DECLARE     @taxon_list_key				CHAR(16),
				@concept_group_key			CHAR(16),
				@taxon_list_item_key		CHAR(16),
				@taxon_version_key			CHAR(16),
				@term_key					CHAR(16),
				@term_version_key			CHAR(16),
				@list_preferred				BIT,
				@is_current					BIT,
				@is_preferred				BIT,
				@taxon_rank_key				CHAR(16),
				@rank_uses_italics			BIT,
				@concept_rank_key			CHAR(16),
				@name_type_concept_key		CHAR(16),
				@sort_code					INT,
				@ins_user_key				CHAR(16),
				@ins_date					SMALLDATETIME,
				@ins_session_id				CHAR(16),
				@upd_user_key				CHAR(16),
				@upd_date					SMALLDATETIME,
				@upd_session_id				CHAR(16),
				@system						BIT,
				@preferred_name				CHAR(16),
				@taxon_list_version_from	CHAR(16),
				@taxon_list_version_to		CHAR(16),	
				@concept_group_version_from	CHAR(16),
				@concept_group_version_to	CHAR(16),
				@meaning_key				CHAR(16),
				@concept_key				CHAR(16)

	/* determine parameters of job */
	SELECT		@taxon_list_key							=	m.Taxon_List_Key,
				@concept_group_key						=	m.Concept_Group_Key
	FROM		Import_Export_Job						AS	j
	INNER JOIN	Taxon_Dictionary_Concept_Group_Mapping	AS	m
	ON			m.Concept_Group_Key						=	j.Concept_Group_Key
	WHERE		j.Import_Export_Job_ID					=	@job_id

	IF @@ROWCOUNT = 0
	BEGIN
		RAISERROR ('Job does not exist or has not been configured', 16, 1)
		RETURN
	END

	EXECUTE		usp_Import_Export_Job_UpdateStatus	@job_id,
													'Importing concepts'
	IF @@ERROR <> 0 RETURN

	/* remove current lineage data */
	DELETE		l
	FROM		Concept					AS	c
	INNER JOIN	Concept_Lineage			AS	l
	ON			l.Concept_Key			=	c.Concept_Key
	WHERE		c.Concept_Group_Key		=	@concept_group_key

	IF @@ERROR <> 0 RETURN

	DECLARE		@items	TABLE (
				Taxon_List_Item_Key	CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS,
				Rank_Uses_Italic	BIT)

	INSERT		@items
	SELECT      tli.TAXON_LIST_ITEM_KEY,
				tr.LIST_FONT_ITALIC
	FROM        TAXON_LIST_VERSION				AS	tlv
	INNER JOIN	TAXON_LIST_ITEM					AS	tli
	ON			tli.TAXON_LIST_VERSION_KEY		=	tlv.TAXON_LIST_VERSION_KEY
	INNER JOIN	TAXON_RANK						AS	tr
	ON			tr.TAXON_RANK_KEY				=	tli.TAXON_RANK_KEY
	WHERE		tlv.TAXON_LIST_KEY				=	@taxon_list_key

	DECLARE		items		CURSOR FAST_FORWARD LOCAL FOR
	SELECT		tli.TAXON_LIST_ITEM_KEY,
				tli.TAXON_VERSION_KEY,
				CASE WHEN tli.TAXON_LIST_ITEM_KEY = tli.PREFERRED_NAME
					THEN 1	/* list preferred */
					ELSE 0
				END,
				CASE WHEN tli.TAXON_LIST_VERSION_TO IS NULL
					THEN 1	/* current */
					ELSE 0
				END,
				tli.TAXON_RANK_KEY,
				itm.Rank_Uses_Italic,
				tli.SORT_CODE,
				tli.ENTERED_BY,
				tli.ENTRY_DATE,
				tli.CHANGED_BY,
				tli.CHANGED_DATE,
				tli.SYSTEM_SUPPLIED_DATA,
				tli.PREFERRED_NAME,
				tli.TAXON_LIST_VERSION_KEY,
				tli.TAXON_LIST_VERSION_TO
	FROM		@items							AS	itm
	INNER JOIN	TAXON_LIST_ITEM					AS	tli
	ON			tli.TAXON_LIST_ITEM_KEY			=	itm.TAXON_LIST_ITEM_KEY

	OPEN        items

	WHILE 1 = 1
	BEGIN
		FETCH		items
		INTO		@taxon_list_item_key,
					@taxon_version_key,
					@list_preferred,
					@is_current,
					@taxon_rank_key,
					@rank_uses_italics,
					@sort_code,
					@ins_user_key,
					@ins_date,
					@upd_user_key,
					@upd_date,
					@system,
					@preferred_name,
					@taxon_list_version_from,
					@taxon_list_version_to

		IF @@FETCH_STATUS <> 0 BREAK

		BEGIN TRANSACTION

		/* perform mappings */
		SELECT		@term_version_key						=	Term_Version_Key
		FROM		Taxon_Dictionary_Term_Version_Mapping
		WHERE		Taxon_Version_Key						=	@taxon_version_key

		IF @@ROWCOUNT = 0 GOTO skip_item

		SELECT		@term_key								=	tm.Term_Key,
					@name_type_concept_key					=	ntm.Thesaurus_Name_Type_Key
		FROM		TAXON_VERSION							AS	tv
		INNER JOIN	TAXON									AS	tx
		ON			tx.TAXON_KEY							=	tv.TAXON_KEY
		INNER JOIN	Taxon_Dictionary_Term_Mapping			AS	tm
		ON			tm.Taxon_Key							=	tx.TAXON_KEY
		AND			tm.Italic_Font							=	CASE WHEN tx.Language = 'La'
																	 AND @rank_uses_italics = 1
																	THEN 1
																	ELSE 0
																END
		INNER JOIN	Taxon_Dictionary_Name_Type_Mapping		AS	ntm
		ON			ntm.Taxon_Name_Type_Key					=	tx.TAXON_NAME_TYPE_KEY
		WHERE		tv.TAXON_VERSION_KEY					=	@taxon_version_key

		IF @@ROWCOUNT = 0 GOTO skip_item

		SELECT		@concept_rank_key						=	Concept_Rank_Key
		FROM		Taxon_Dictionary_Concept_Rank_Mapping
		WHERE		Taxon_Rank_Key							=	@taxon_rank_key

		IF @@ROWCOUNT = 0 GOTO skip_item

		IF @list_preferred = 1
			SET			@is_preferred		=	1
		ELSE
		BEGIN
			SELECT		@is_preferred 		=	CASE WHEN TAXON_VERSION_KEY = @taxon_version_key
													THEN 1
													ELSE 0
												END
			FROM		TAXON_COMMON_NAME
			WHERE		TAXON_LIST_ITEM_KEY	=	@taxon_list_item_key
		END

		SELECT      @concept_group_version_from						=	Concept_Group_Version_Key
		FROM		Taxon_Dictionary_Concept_Group_Version_Mapping
		WHERE		Taxon_List_Version_Key							=   @taxon_list_version_from

		IF @@ROWCOUNT = 0 GOTO skip_item

		SELECT		@concept_group_version_to						=	Concept_Group_Version_Key
		FROM		Taxon_Dictionary_Concept_Group_Version_Mapping
		WHERE		Taxon_List_Version_Key							=	@taxon_list_version_to

		IF @@ROWCOUNT = 0
		BEGIN
			SET			@concept_group_version_to	=	NULL
		END

		/* obtain meaning key */
		SELECT		@meaning_key						=	Meaning_Key
		FROM        Taxon_Dictionary_Meaning_Mapping
		WHERE		Preferred_Name						=	@preferred_name

		IF @@ROWCOUNT = 0
		BEGIN
			/* look for meaning assigned to synonyms of @preferred_name from
			 * some other taxon list */
			SELECT		@meaning_key						=	tdm.Meaning_Key
			FROM		INDEX_TAXON_SYNONYM					AS	its
			INNER JOIN	Taxon_Dictionary_Meaning_Mapping	AS	tdm
			ON			tdm.Preferred_Name					=	its.SYNONYM_LIST_ITEM_KEY
			WHERE		its.TAXON_LIST_ITEM_KEY				=	@preferred_name

			IF @@ROWCOUNT = 0
			BEGIN
				/* create new meaning */
				EXECUTE		spNextKey	'Meaning',
										@meaning_key	OUTPUT
				IF @@ERROR <> 0 GOTO fail_from_cursor

				INSERT		Meaning (
							Meaning_Key)
				VALUES		(@meaning_key)

				IF @@ERROR <> 0 GOTO fail_from_cursor
			END

			INSERT		Taxon_Dictionary_Meaning_Mapping (
						Preferred_Name,
						Meaning_Key)
			VALUES		(@preferred_name,
						@meaning_key)

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END

		IF @meaning_key IS NOT NULL
				/* meaning not explicitly mapped to null,
				 * so we can import item */
		BEGIN
			/* obtain session identifiers */
			EXECUTE		usp_Session_ForDate		@ins_user_key,
												@ins_date,
												@ins_session_id		OUTPUT
			IF @@ERROR <> 0 GOTO fail_from_cursor

			IF @upd_user_key IS NULL
			BEGIN
				SET			@upd_session_id		=	NULL
			END
			ELSE
			BEGIN
				EXECUTE		usp_Session_ForDate		@upd_user_key,
													@upd_date,
													@upd_session_id		OUTPUT
				IF @@ERROR <> 0 GOTO fail_from_cursor
			END

			SELECT      @concept_key						=	Concept_Key
			FROM		Taxon_Dictionary_Concept_Mapping
			WHERE		Taxon_List_Item_Key					=	@taxon_list_item_key

			IF @@ROWCOUNT > 0
			BEGIN
				DECLARE		@old_group_key			CHAR(16),
							@was_list_preferred		BIT
							
				/* update concept */
				UPDATE		Concept
				SET         @old_group_key				=	Concept_Group_Key,
							@was_list_preferred			=	List_Preferred,
							Term_Key					=	@term_key,
							Concept_Group_Key			=	@concept_group_key,
							Term_Version_Key			=	@term_version_key,
							List_Preferred				=	@list_preferred,
							Is_Current					=	@is_current,
							Preferred					=	@is_preferred,
							Concept_Rank_Key			=	@concept_rank_key,
							Name_Type_Concept_Key		=   @name_type_concept_key,
							Meaning_Key					=	@meaning_key,
							Sort_Code					=	@sort_code,
							Entered_Session_ID			=	@ins_session_id,
							Changed_Session_ID			=	@upd_session_id,
							System_Supplied_Data		=	@system
				WHERE		Concept_Key					=	@concept_key

				IF @@ERROR <> 0 GOTO fail_from_cursor

				/* re-create concept history */
				DELETE		Concept_History
				WHERE		Concept_Key					=	@concept_key

				IF @@ERROR <> 0 GOTO fail_from_cursor

				EXECUTE		usp_ConceptHistory_Insert_Imported	@concept_key,
																@concept_group_version_from,
																@concept_group_version_to
				IF @@ERROR <> 0 GOTO fail_from_cursor
			END
			ELSE
			BEGIN
				/* create concept */
				EXECUTE		spNextKey	'Concept',
										@concept_key	OUTPUT
				IF @@ERROR <> 0 GOTO fail_from_cursor

				INSERT		Concept
							(Concept_Key,
							Term_Key,
							Concept_Group_Key,
							Term_Version_Key,
							List_Preferred,
							Is_Current,
							Preferred,
							Concept_Rank_Key,
							Name_Type_Concept_Key,
							Meaning_Key,
							Sort_Code,
							Entered_Session_ID,
							Changed_Session_ID,
							System_Supplied_Data)
				VALUES		(@concept_key,
							@term_key,
							@concept_group_key,
							@term_version_key,
							@list_preferred,
							@is_current,
							@is_preferred,
							@concept_rank_key,
							@name_type_concept_key,
							@meaning_key,
							@sort_code,
							@ins_session_id,
							@upd_session_id,
							@system)

				IF @@ERROR <> 0 GOTO fail_from_cursor

				/* create concept history */
				EXECUTE		usp_ConceptHistory_Insert_Imported	@concept_key,
																@concept_group_version_from,
																@concept_group_version_to
				IF @@ERROR <> 0 GOTO fail_from_cursor

				/* record mapping */
				INSERT		Taxon_Dictionary_Concept_Mapping
							(Taxon_List_Item_Key,
							Concept_Key)
				VALUES		(@taxon_list_item_key,
							@concept_key)

				IF @@ERROR <> 0 GOTO fail_from_cursor
			END
		END
		
skip_item:
		EXECUTE		usp_Import_Export_Job_RecordProcessed	@job_id
		IF @@ERROR <> 0 GOTO fail_from_cursor

		COMMIT TRANSACTION
	END

	CLOSE		items
	DEALLOCATE	items
	RETURN

fail_from_cursor:
	CLOSE		items
	DEALLOCATE	items

fail:
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_Concept_ImportTaxonList failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_ImportTaxonList') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Concept_ImportTaxonList'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concept_ImportTaxonList TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concept_ImportTaxonList TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Concept_ImportTaxonList TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_Concept_ImportTaxonNameTypes]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_Concept_ImportTaxonNameTypes]
GO

/*===========================================================================*\
  Description:	Import concepts corresponding to the taxon name types used in
				the specified taxon list.

  Parameters:   @job_id					Job identifier

  Created:		Nov 2003

  Last revision information:
	$Revision: 2 $
	$Date: 2/02/09 16:44 $
	$Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Concept_ImportTaxonNameTypes]
	@job_id					INT
AS
	SET NOCOUNT ON
	SET ANSI_NULLS ON

	DECLARE		@taxon_list_key				CHAR(16),
				@taxon_name_type_key		CHAR(16),
				@item_name					VARCHAR(100),
				@author_and_date			VARCHAR(100),
				@ins_user_key				CHAR(16),
				@ins_date					SMALLDATETIME,
				@ins_session_id				CHAR(16),
				@system						BIT,
				@thesaurus_name_type_key	CHAR(16),
				@system_mapping				BIT,
				@term_key					CHAR(16),
				@term_version_key			CHAR(16),
				@meaning_key				CHAR(16),
				@concept_history_key		CHAR(16)

	/* determine parameters of job */
	SELECT		@taxon_list_key							=	m.Taxon_List_Key
	FROM		Import_Export_Job						AS	j
	INNER JOIN	Taxon_Dictionary_Concept_Group_Mapping	AS	m
	ON			m.Concept_Group_Key						=	j.Concept_Group_Key
	WHERE		j.Import_Export_Job_ID					=	@job_id

	IF @@ROWCOUNT = 0
	BEGIN
		RAISERROR ('Job does not exist or has not been configured', 16, 1)
		RETURN
	END

	EXECUTE		usp_Import_Export_Job_UpdateStatus	@job_id,
													'Importing name types'
	IF @@ERROR <> 0 RETURN

	DECLARE     @versions   TABLE ( Taxon_Version_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS)

	INSERT      @versions
	SELECT      tli.TAXON_VERSION_KEY
	FROM		TAXON_LIST_VERSION				AS	tlv
	INNER JOIN	TAXON_LIST_ITEM					AS	tli
	ON			tli.TAXON_LIST_VERSION_KEY		=	tlv.TAXON_LIST_VERSION_KEY
	WHERE		tlv.TAXON_LIST_KEY				=	@taxon_list_key

	DECLARE		name_types	CURSOR LOCAL FAST_FORWARD FOR
	SELECT DISTINCT
				tnt.TAXON_NAME_TYPE_KEY,
				tnt.SHORT_NAME,
				tnt.AUTHORITY,
				tnt.ENTERED_BY,
				tnt.ENTRY_DATE,
				tnt.SYSTEM_SUPPLIED_DATA
	FROM        @versions                       AS  v0
	INNER JOIN	TAXON_VERSION					AS	tv
	ON			tv.TAXON_VERSION_KEY			=	v0.TAXON_VERSION_KEY
	INNER JOIN	TAXON							AS	tx
	ON			tx.TAXON_KEY					=	tv.TAXON_KEY
	INNER JOIN	TAXON_NAME_TYPE					AS	tnt
	ON			tnt.TAXON_NAME_TYPE_KEY			=	tx.TAXON_NAME_TYPE_KEY

	OPEN        name_types

	WHILE 1 = 1
	BEGIN
		FETCH		name_types
		INTO		@taxon_name_type_key,
					@item_name,
					@author_and_date,
					@ins_user_key,
					@ins_date,
					@system
					
		IF @@FETCH_STATUS <> 0 BREAK

		BEGIN TRANSACTION

		SELECT		@thesaurus_name_type_key			=   Thesaurus_Name_Type_Key,
					@system_mapping						=	System_Supplied_Data
		FROM		Taxon_Dictionary_Name_Type_Mapping
		WHERE       Taxon_Name_Type_Key					=	@taxon_name_type_key

		IF @@ROWCOUNT > 0
		BEGIN
			/* name type has previously been imported */
			IF @system_mapping = 0
			BEGIN
				SELECT		@term_key			=	Term_Key,
							@term_version_key	=	Term_Version_Key
				FROM		Concept
				WHERE		Concept_Key			=	@thesaurus_name_type_key

				IF NOT EXISTS (	SELECT		1
								FROM		Term
								WHERE		Term_Key		=	@term_key
								AND			Language_Key	=	'en'
								AND			Item_Name		=	@item_name )
				BEGIN
					/* term has changed */
					IF EXISTS (	SELECT		1
								FROM		Concept
								WHERE		Term_Key			=	@term_key
								AND			Concept_Group_Key	<>	'SYSTEM000000000M' )
					BEGIN
						/* term is linked outside this concept group; create
						 * a new term instead of updating the existing one */
						EXECUTE		spNextKey	'Term',
												@term_key	OUTPUT
						IF @@ERROR <> 0 GOTO fail_from_cursor

						INSERT		Term (
									Term_Key,
									Language_Key,
									Item_Name,
									Plaintext,
									Entered_Session_ID,
									System_Supplied_Data)
						VALUES		(@term_key,
									'en',
									@item_name,
									@item_name,
									@ins_session_id,
									@system)

						IF @@ERROR <> 0 GOTO fail_from_cursor

						EXECUTE		spNextKey		'Term_Version',
													@term_version_key	OUTPUT
						IF @@ERROR <> 0 GOTO fail_from_cursor

						INSERT		Term_Version (
									Term_Version_Key,
									Term_Key,
									Author_And_Date,
									Entered_Session_ID,
									System_Supplied_Data)
						VALUES		(@term_version_key,
									@term_key,
									@author_and_date,
									@ins_session_id,
									@system)

						IF @@ERROR <> 0 GOTO fail_from_cursor									

						UPDATE		Concept
						SET			Term_Key		=	@term_key
						WHERE		Concept_Key		=	@thesaurus_name_type_key

						IF @@ERROR <> 0 GOTO fail_from_cursor
					END
					ELSE
					BEGIN
						/* term only linked within this concept group */
						DECLARE		@cur_term_key		CHAR(16)

						SELECT		@cur_term_key	=	Term_Key
						FROM		Term
						WHERE		Language_Key	=	'en'
						AND			Item_Name		=	@item_name

						IF @@ROWCOUNT = 0
						BEGIN
							/* term can simply be updated */
							UPDATE		Term
							SET			Language_Key	=	'en',
										Item_Name		=	@item_name
							WHERE		Term_Key		=	@term_key

							IF @@ERROR <> 0 GOTO fail_from_cursor

							UPDATE		Term_Version
							SET			Author_And_Date		=	@author_and_date
							WHERE		Term_Version_Key	=	@term_version_key

							IF @@ERROR <> 0 GOTO fail_from_cursor
						END
						ELSE
						BEGIN
							/* term cannot be updated; there is an existing
							 * term with the same name which we will link to
							 * instead */
							EXECUTE		spNextKey	'Term_Version',
													@term_version_key	OUTPUT
							IF @@ERROR <> 0 GOTO fail_from_cursor

							INSERT		Term_Version (
										Term_Version_Key,
										Term_Key,
										Author_And_Date,
										Entered_Session_ID,
										System_Supplied_Data)
							VALUES		(@term_version_key,
										@cur_term_key,
										@author_and_date,
										@ins_session_id,
										@system)

							IF @@error <> 0 GOTO fail_from_cursor

							UPDATE		Concept
							SET			Term_Key			=	@cur_term_key,
										Term_Version_Key	=	@term_version_key
							WHERE		Term_Key			=	@term_key

							IF @@ERROR <> 0 GOTO fail_from_cursor

							DELETE		Term
							WHERE		Term_Key			=	@term_key

							IF @@ERROR <> 0 GOTO fail_from_cursor
						END
					END
				END
			END
		END
		ELSE
		BEGIN
			/* obtain session identifier */
			EXECUTE		usp_Session_ForDate		@ins_user_key,
												@ins_date,
												@ins_session_id		OUTPUT
			IF @@ERROR <> 0 GOTO fail_from_cursor

			/* find/create term */
			SELECT		@term_key		=	Term_Key
			FROM		Term
			WHERE		Language_Key	=	'en'
			AND			Item_Name		=	@item_name

			IF @@ROWCOUNT = 0
			BEGIN
				EXECUTE		spNextKey	'Term',
										@term_key	OUTPUT
				IF @@ERROR <> 0 GOTO fail_from_cursor

				INSERT		Term (
							Term_Key,
							Language_Key,
							Item_Name,
							Plaintext,
							Entered_Session_ID,
							System_Supplied_Data)
				VALUES		(@term_key,
							'en',
							@item_name,
							@item_name,
							@ins_session_id,
							@system)

				IF @@ERROR <> 0 GOTO fail_from_cursor
			END

			EXECUTE		spNextKey	'Term_Version',
									@term_version_key	OUTPUT
			IF @@ERROR <> 0 GOTO fail_from_cursor

			INSERT		Term_Version (
						Term_Version_Key,
						Term_Key,
						Author_And_Date,
						Entered_Session_ID,
						System_Supplied_Data)
			VALUES		(@term_version_key,
						@term_key,
						@author_and_date,
						@ins_session_id,
						@system)

			IF @@ERROR <> 0 GOTO fail_from_cursor

			/* create Meaning */
			EXECUTE		spNextKey	'Meaning',
									@meaning_key	OUTPUT
			IF @@ERROR <> 0 GOTO fail_from_cursor

			INSERT		Meaning (
						Meaning_Key)
			VALUES		(@meaning_key)

			IF @@ERROR <> 0 GOTO fail_from_cursor

			/* create Concept */
			EXECUTE		spNextKey	'Concept',
									@thesaurus_name_type_key	OUTPUT
			IF @@ERROR <> 0 GOTO fail_from_cursor

			INSERT		Concept (
						Concept_Key,
						Term_Key,
						Concept_Group_Key,
						Term_Version_Key,
						List_Preferred,
						Is_Current,
						Preferred,
						Name_Type_Concept_Key,
						Meaning_Key,
						Entered_Session_ID,
						System_Supplied_Data)
			VALUES 		(@thesaurus_name_type_key,
						@term_key,
						'SYSTEM000000000M', /* "Thesaurus Name Types" group */
						@term_version_key,
						1,
						1,
						1,
						'SYSTEM0000000000', /* "Formal" -- meaningless, but
												we need a value here */
						@meaning_key,
						@ins_session_id,
						@system)

			IF @@ERROR <> 0 GOTO fail_from_cursor

			/* create concept history */
			EXECUTE		spNextKey	'Concept_History',
									@concept_history_key	OUTPUT
			IF @@ERROR <> 0 GOTO fail_from_cursor

			INSERT		Concept_History (
						Concept_History_Key,
						Concept_Key,
						Concept_Group_Version_From,
						Entered_Session_ID,
						System_Supplied_Data)
			VALUES		(@concept_history_key,
						@thesaurus_name_type_key,
						'SYSTEM000000000M', /* "Thesaurus Name Types" version */
						@ins_session_id,
						@system)

			IF @@ERROR <> 0 GOTO fail_from_cursor

			/* record taxon name type mapping */
			INSERT		Taxon_Dictionary_Name_Type_Mapping (
						Taxon_Name_Type_Key,
						Thesaurus_Name_Type_Key,
						System_Supplied_Data)
			VALUES		(@taxon_name_type_key,
						@thesaurus_name_type_key,
						0)

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END

		EXECUTE		usp_Import_Export_Job_RecordProcessed	@job_id
		IF @@ERROR <> 0 GOTO fail_from_cursor

		COMMIT TRANSACTION
	END

	CLOSE		name_types
	DEALLOCATE	name_types
	RETURN

fail_from_cursor:
	CLOSE		name_types
	DEALLOCATE	name_types

fail:
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_Concept_ImportTaxonNameTypes failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_ImportTaxonNameTypes') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Concept_ImportTaxonNameTypes'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concept_ImportTaxonNameTypes TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concept_ImportTaxonNameTypes TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Concept_ImportTaxonNameTypes TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_Concept_InsertSpreadsheetRow]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_Concept_InsertSpreadsheetRow]
GO

/*===========================================================================*\
  Description:	Insert a Concept record, plus related records as required,
				based on data extracted from a spreadsheet.

  Parameters:   @SessionID				Session key
				@concept_group_key		Concept group key
				@author					Name of author
				@child_of				Parent concept key
				@citation_date			Citation date
                @rank                   Rank (abbreviation or name)
				@fact_title				Name of fact
				@fact_type				Name of fact type
				@fact_description		Fact data
				@list_code				Concept list code
				@name_type				Name of name type
				@sort_code				Concept sort code
				@synonym_of				Synonym concept key
				@language				Name of term language
				@language_key			Term language key
				@term_name				Term name
				@concept_key			[on exit] New concept key

  Created:		Jan 2004

  Last revision information:
	$Revision: 2 $
	$Date: 2/02/09 16:44 $
	$Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Concept_InsertSpreadsheetRow]
	@SessionID			CHAR(16),
	@concept_group_key	CHAR(16),
	@author				VARCHAR(100),
	@child_of			CHAR(16),
	@citation_date		VARCHAR(100),
    @rank               VARCHAR(100),
	@fact_title			VARCHAR(100),
	@fact_type			NVARCHAR(300),
	@fact_description	TEXT,
	@list_code			VARCHAR(50),
	@name_type			NVARCHAR(300),
	@sort_code			INT,
	@synonym_of			CHAR(16),
	@language			VARCHAR(50),
	@language_key		VARCHAR(4),
	@term_name			NVARCHAR(300),
	@concept_key		CHAR(16)	OUTPUT
AS
	SET NOCOUNT ON
	SET ARITHABORT ON

	/* check parameters */
	IF @term_name IS NULL
	BEGIN
		RAISERROR ('Term name must be specified.', 16, 1)
		RETURN
	END

	IF NOT @language_key IS NULL
	BEGIN
		IF NOT EXISTS (	SELECT		1
						FROM		Language
						WHERE		Language_Key	=	@language_key )
		BEGIN
			RAISERROR ('Specified language does not exist.', 16, 1)
			RETURN
		END
	END
	ELSE
	BEGIN
		IF @language IS NULL
		BEGIN
			RAISERROR ('Language or Language Key must be specified.', 16, 1)
			RETURN
		END

		SELECT		@language_key	=	Language_Key
		FROM		Language
		WHERE		Item_Name		=	@language

		IF @@ROWCOUNT = 0
		BEGIN
			RAISERROR ('Specified language is not recognised.', 16, 1)
			RETURN
		END
	END

	DECLARE		@term_key			CHAR(16),
				@term_version_key	CHAR(16),
				@name_type_key		CHAR(16),
				@meaning_key		CHAR(16),
                @domain_key         CHAR(16),
                @rank_key           CHAR(16)

	BEGIN TRANSACTION

	/* work out the term */
	SELECT		@term_key		=	Term_Key
	FROM		Term
	WHERE		Language_Key	=	@language_key
	AND			Item_Name		=	@term_name

	IF @@ROWCOUNT = 0
	BEGIN
		EXECUTE		spNextKey	'Term',
								@term_key	OUTPUT
		IF @@ERROR <> 0 GOTO fail

		INSERT		Term (
					Term_Key,
					Language_Key,
					Item_Name,
					Plaintext,
					Entered_Session_ID)
		VALUES		(@term_key,
					@language_key,
					@term_name,
					dbo.ufn_RemoveHtmlMarkup(@term_name),
					@SessionID)
		IF @@ERROR <> 0 GOTO fail
	END

	/* create term version */
	IF @author IS NOT NULL OR @citation_date IS NOT NULL
	BEGIN
		EXECUTE		spNextKey	'Term_Version',
								@term_version_key	OUTPUT
		IF @@ERROR <> 0 GOTO fail

		INSERT		Term_Version (
					Term_Version_Key,
					Term_Key,
					Author_And_Date,
					Entered_Session_ID)
		VALUES		(@term_version_key,
					@term_key,
					ISNULL(@author + ' ', '') + ISNULL(@citation_date, ''),
					@SessionID)
		IF @@ERROR <> 0 GOTO fail
	END

	/* work out the meaning */
	IF @synonym_of IS NOT NULL
	BEGIN
		SELECT		@meaning_key	=	Meaning_Key
		FROM		Concept
		WHERE		Concept_Key		=	@synonym_of

		IF @@ROWCOUNT = 0
		BEGIN
			RAISERROR ('Synonym does not exist.', 16, 1)
			GOTO fail
		END
	END
	ELSE
	BEGIN
		EXECUTE		spNextKey	'Meaning',
								@meaning_key	OUTPUT
		IF @@ERROR <> 0 GOTO fail

		INSERT		Meaning (
					Meaning_Key)
		VALUES		(@meaning_key)

		IF @@ERROR <> 0 GOTO fail
	END

	/* work out name type */
	IF @name_type IS NULL
	BEGIN
		SET			@name_type_key	=	'SYSTEM00000000AN' /* 'Unknown' */
	END
	ELSE
	BEGIN
		SELECT		@name_type_key		=	c.Concept_Key
		FROM		Concept				AS	c
		INNER JOIN	Term				AS	t
		ON			t.Term_Key			=	c.Term_Key
		WHERE		c.Concept_Group_Key	=	'SYSTEM000000000M'
		AND			t.Language_Key		=	'en'
		AND			t.Item_Name			=	@name_type

		IF @@ROWCOUNT = 0
		BEGIN
			EXECUTE		usp_Concept_Insert	@name_type_key	OUTPUT,
											'SYSTEM000000000M',
											@name_type,
											@name_type,
											'en',
											@SessionID,
											'SYSTEM00000000AN'
			IF @@ERROR <> 0 GOTO fail
		END
	END

    /* work out rank */
    IF @rank IS NOT NULL
    BEGIN
        EXECUTE     usp_ConceptGroup_GetDomain  @concept_group_key,
                                                @domain_key         OUTPUT
        IF @@ERROR <> 0 GOTO fail

        SELECT      @rank_key       =   Concept_Rank_Key
        FROM        Concept_Rank
        WHERE       Domain_Key      =   @domain_key
        AND         Abbreviation    =   @rank

        IF @rank_key IS NULL
        BEGIN
            SELECT      @rank_key       =   Concept_Rank_Key
            FROM        Concept_Rank
            WHERE       Domain_Key      =   @domain_key
            AND         Item_Name       =   @rank
        END

        IF @rank_key IS NULL
        BEGIN
            EXECUTE     spNextKey   'Concept_Rank',
                                    @rank_key       OUTPUT
            IF @@ERROR <> 0 GOTO fail

            INSERT      Concept_Rank (
                        Concept_Rank_Key,
                        Domain_Key,
                        Item_Name,
                        Abbreviation,
                        Color_R,
                        Color_G,
                        Color_B,
                        Entered_Session_ID)
            VALUES      (@rank_key,
                        @domain_key,
                        @rank,
                        @rank,
                        0,
                        0,
                        0,
                        @SessionID)
            IF @@ERROR <> 0 GOTO fail
        END
    END

	/* create concept */
	EXECUTE		spNextKey	'Concept',
							@concept_key	OUTPUT
	IF @@ERROR <> 0 GOTO fail

	INSERT		Concept (
				Concept_Key,
				Term_Key,
				Concept_Group_Key,
				Term_Version_Key,
				List_Preferred,
				Preferred,
                Concept_Rank_Key,
				Name_Type_Concept_Key,
				Meaning_Key,
				Sort_Code,
				List_Code,
				Entered_Session_ID)
	SELECT		@concept_key,
				@term_key,
				@concept_group_key,
				@term_version_key,
				CASE WHEN @synonym_of IS NULL THEN 1 ELSE 0 END,
				CASE
					WHEN @synonym_of IS NULL THEN 1
					WHEN EXISTS (	SELECT		1
									FROM		Concept			AS	c
									INNER JOIN	Term			AS	t
									ON			t.Term_Key		=	c.Term_Key
									WHERE		c.Meaning_Key	=	@meaning_key
									AND			t.Language_Key	=	@language_key)
						THEN 0
					ELSE 1
				END,
                @rank_key,
				@name_type_key,
				@meaning_key,
				@sort_code,
				@list_code,
				@SessionID
	IF @@ERROR <> 0 GOTO fail

	/* update lineage */
	EXECUTE		usp_ConceptLineage_NewConcept	@concept_key
	IF @@ERROR <> 0 GOTO fail

	/* create parent-child relationship */
	IF @child_of IS NOT NULL
	BEGIN
		DECLARE		@relation_key		CHAR(16),
					@relation_type_key	CHAR(16)

		SELECT		@relation_type_key	=	Hierarchy_Relation_Type_Key
		FROM		Concept_Group
		WHERE		Concept_Group_Key	=	@concept_group_key

		EXECUTE		usp_ConceptRelation_Insert	@relation_key	OUTPUT,
												@child_of,
												@concept_key,
												@relation_type_key,
												@SessionID = @SessionID
		IF @@ERROR <> 0 GOTO fail
	END

	/* create fact */
	IF @fact_description IS NOT NULL
	BEGIN
		DECLARE		@fact_type_key		CHAR(16),
					@fact_key			CHAR(16)

		IF @fact_type IS NULL
		BEGIN
			SET			@fact_type_key	=	'SYSTEM00000002NO' /* HTML */
		END
		ELSE
		BEGIN
			SELECT		@fact_type_key		=	c.Concept_Key
			FROM		Concept				AS	c
			INNER JOIN	Term				AS	t
			ON			t.Term_Key			=	c.Term_Key
			WHERE		c.Concept_Group_Key	=	'SYSTEM000000000L'
			AND			t.Language_Key		=	'en'
			AND			t.Item_Name			=	@fact_type

			IF @@ROWCOUNT = 0
			BEGIN
				EXECUTE		usp_Concept_Insert	@fact_type_key	OUTPUT,
												'SYSTEM000000000L',
												@fact_type,
												@fact_type,
												'en',
												@SessionID,
												'SYSTEM00000000AN'
				IF @@ERROR <> 0 GOTO fail
			END
		END

		EXECUTE		spNextKey	'Thesaurus_Fact',
								@fact_key			OUTPUT
		INSERT		Thesaurus_Fact (
					Thesaurus_Fact_Key,
					Item_Name,
					Data,
					Meaning_Key,
					Language_Key,
					Fact_Vague_Date_Type,
					Fact_Type_Concept_Key,
					Entered_Session_ID,
					System_Supplied_Data)
		VALUES		(@fact_key,
					ISNULL(@fact_title, 'Fact'),
					@fact_description,
					@meaning_key,
					'en',
					'U',
					@fact_type_key,
					@SessionID,
					0)
		IF @@ERROR <> 0 GOTO fail
	END

	COMMIT TRANSACTION
	RETURN

fail:
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_Concept_InsertSpreadsheetRow failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_InsertSpreadsheetRow') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Concept_InsertSpreadsheetRow'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concept_InsertSpreadsheetRow TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concept_InsertSpreadsheetRow TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Concept_InsertSpreadsheetRow TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConservationJob_Update_DomainMask]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConservationJob_Update_DomainMask]
GO

/*===========================================================================*\
  Description:	Update the Domain Mask field of all Conservation Jobs related
		to either a collection Unit item or a conservatio task.
		If no values are provided, all domain masks will be recalculated.
		That could be useful to "repair" the masks inConservation_Job.

		Use @CollectionUnitKey when item was updated in Collection_Unit.

		Use @ConservationTaskKey when called from trigger on 
		Collection_Unit_Task table. Especially important for DELETE trigger.

  Parameters:	@CollectionUnitKey
		@ConservationTaskKey

  Created:	September 2003

  Last revision information:
    $Revision: 2 $
    $Date: 2/02/09 16:44 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConservationJob_Update_DomainMask]
	@CollectionUnitKey char(16) = NULL,
	@ConservationTaskKey char(16) = NULL
AS
	DECLARE	@JobKey char(16),
		@NewMask int

	/*-------------------------------------------------------------*\
	  Find all related jobs first.
	\*-------------------------------------------------------------*/
	IF @CollectionUnitKey IS NOT NULL
		-- All jobs related to a Collection Unit item
		DECLARE curJobs CURSOR LOCAL FAST_FORWARD FOR
			SELECT		DISTINCT CJ.Conservation_Job_Key
			FROM		Collection_Unit CU
			INNER JOIN 	Collection_Unit_Task CUT ON CUT.Collection_Unit_Key = CU.Collection_Unit_Key
			INNER JOIN	Conservation_Task CT ON CT.Conservation_Task_Key = CUT.Conservation_Task_Key
			INNER JOIN	Conservation_Job CJ ON CJ.Conservation_Job_Key = CT.Conservation_Job_Key
			WHERE		CU.Collection_Unit_Key = @CollectionUnitKey
	ELSE 
	IF @ConservationTaskKey IS NOT NULL
		-- All jobs related to a Conservation Task. 
		DECLARE curJobs CURSOR LOCAL FAST_FORWARD FOR
			SELECT		DISTINCT CJ.Conservation_Job_Key
			FROM		Conservation_Task CT 
			INNER JOIN	Conservation_Job CJ ON CJ.Conservation_Job_Key = CT.Conservation_Job_Key
			WHERE		CT.Conservation_Task_Key = @ConservationTaskKey
	ELSE
		-- All jobs related to records in Collection_Unit_Task
		DECLARE curJobs CURSOR LOCAL FAST_FORWARD FOR
			SELECT		DISTINCT CJ.Conservation_Job_Key
			FROM		Collection_Unit CU
			INNER JOIN 	Collection_Unit_Task CUT ON CUT.Collection_Unit_Key = CU.Collection_Unit_Key
			INNER JOIN	Conservation_Task CT ON CT.Conservation_Task_Key = CUT.Conservation_Task_Key
			INNER JOIN	Conservation_Job CJ ON CJ.Conservation_Job_Key = CT.Conservation_Job_Key
	
	
	/*-------------------------------------------------------------*\
	  Got all jobs. Now update the domain masks.
	\*-------------------------------------------------------------*/
	OPEN curJobs
	FETCH NEXT FROM curJobs INTO @JobKey
	WHILE @@Fetch_Status = 0
	BEGIN
		-- Recalculate the mask. Need to select first, or we get following error:
		-- "An aggregate may not appear in the set list of an UPDATE statement."
		SELECT		@NewMask = Sum(DISTINCT CU.Domain_Mask)
		FROM		Collection_Unit CU
		INNER JOIN 	Collection_Unit_Task CUT ON CUT.Collection_Unit_Key = CU.Collection_Unit_Key
		INNER JOIN	Conservation_Task CT ON CT.Conservation_Task_Key = CUT.Conservation_Task_Key
		INNER JOIN	Conservation_Job CJ ON CJ.Conservation_Job_Key = CT.Conservation_Job_Key
		WHERE		CJ.Conservation_Job_Key = @JobKey
	
		-- So do update afterwards
		UPDATE	Conservation_Job
		SET	Domain_Mask = IsNull(@NewMask, 0)
		WHERE	Conservation_Job_Key = @JobKey
	
		-- Move on
		FETCH NEXT FROM curJobs INTO @JobKey
	END

	-- Cleanup
	CLOSE curJobs
	DEALLOCATE curJobs
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConservationJob_Update_DomainMask') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConservationJob_Update_DomainMask'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConservationJob_Update_DomainMask TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConservationJob_Update_DomainMask TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConservationJob_Update_DomainMask TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConservationJob_Update_DomainMask TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConservationJob_Update_DomainMask TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_FieldData_Insert]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_FieldData_Insert]
GO

/*===========================================================================*\
  Description:	Inserts a record into the Specimen_Field_Data table, and
		updates records in others. This stored proc. differs from 
		usp_SpecimenFieldData insert because this updates records in 
		the Sample and Survey_Event tables.

  Parameters:	@Key OUTPUT
		@SurveyEventKey 
	        @SurveyKey 
	        @LocationKey 
		@SampleKey 
	        @SpatialRefQualifier 
	        @SpatialRef 
	        @GatheringMethod 
	        @VagueDateStart 
	        @VagueDateEnd 
	        @VagueDateType
		@CollectionUnitKey 
		@OccurrenceKey 
		@TaxonOccurrenceKey 
		@InferredSurvey 
		@InferredLocation 
		@InferredSpatialRef 
		@InferredSampleType 
		@InferredDate 
		@InferredCollectors 
		@GatheringEvent
		@SessionID 

  Created:	January 2004

  Last revision information:
    $Revision: 2 $
    $Date: 2/02/09 16:44 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_FieldData_Insert]
	@Key char(16) OUTPUT,
	@SurveyEventKey char(16) = NULL,
	@SurveyKey char(16),
	@LocationKey char(16),
	@SampleKey char(16),
	@SpatialRefQualifier varchar(20),
	@SpatialRef varchar(40),
	@Lat float,
	@Long float,
        @GatheringMethod char(16),
        @VagueDateStart int,
        @VagueDateEnd int,
        @VagueDateType varchar(2) = NULL,
	@CollectionUnitKey char(16),
	@OccurrenceKey char(16),
	@TaxonOccurrenceKey char(16),
	@InferredSurvey tinyint,
	@InferredLocation tinyint,
	@InferredSpatialRef tinyint,
	@InferredSampleType tinyint,
	@InferredDate tinyint,
	@InferredCollectors tinyint,
	@GatheringEvent bit,
	@SessionID char(16)
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF
	SET ANSI_NULLS ON

	BEGIN TRANSACTION
		
		UPDATE 	[Sample]
		SET 	Location_Key = @LocationKey,
			Vague_Date_Start = @VagueDateStart, 
			Vague_Date_End = @VagueDateEnd, 
			Vague_Date_Type = IsNull(@VagueDateType, 'U'),
			Spatial_Ref_Qualifier = @SpatialRefQualifier,
			Spatial_Ref = @SpatialRef,
			Sample_Type_Key = @GatheringMethod,
			Lat = @Lat,
			Long = @Long
		WHERE	Sample_Key = @SampleKey

		IF @@Error <> 0 GOTO RollbackAndExit

		-- Update the Survey Event table so that is points to the
		-- correct Survey record.

		IF @SurveyEventKey = NULL 
			SET @SurveyEventKey = (SELECT	Survey_Event_Key
						FROM	[Sample]
						WHERE	Sample_Key = @SampleKey)
		UPDATE	Survey_Event
		SET	Survey_Key = @SurveyKey
		WHERE	Survey_Event_Key = @SurveyEventKey

		IF @@Error <> 0 GOTO RollbackAndExit

		EXECUTE	spNextKey 'Specimen_Field_Data', @Key OUTPUT

		INSERT INTO Specimen_Field_Data (
			Specimen_Field_Data_Key, 
			Collection_Unit_Key, 
			Occurrence_Key, 
			Taxon_Occurrence_Key,
			Inferred_Survey, 
			Inferred_Location, 
			Inferred_Spatial_Ref, 
			Inferred_Sample_Type,
			Inferred_Date, 
			Inferred_Collectors, 
			Gathering_Event, 
			Entered_Session_ID
		) VALUES (
			@Key, 
			@CollectionUnitKey, 
			@OccurrenceKey, 
			@TaxonOccurrenceKey,
			@InferredSurvey, 
			@InferredLocation, 
			@InferredSpatialRef,
			@InferredSampleType, 
			@InferredDate, 
			@InferredCollectors,
			@GatheringEvent, 
			@SessionID
		)		
		IF @@Error <> 0 GOTO RollbackAndExit
	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_FieldData_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_FieldData_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_FieldData_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_FieldData_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_FieldData_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_FieldData_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_FieldData_Insert TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_MeasurementQualifier_Select_ForType') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_MeasurementQualifier_Select_ForType]
GO

/*===========================================================================*\
  Description: Returns a list of Measurement Qualifiers for the specified type

  Parameters:  @Type

  Created:     November 2004

  Last revision information:
    $Revision: 2 $
    $Date: 2/02/09 16:44 $
    $Author: Pauldavies $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_MeasurementQualifier_Select_ForType]
	@Type CHAR(16)
AS
	SELECT	Measurement_Qualifier_Key AS Item_Key,
		Short_Name
	FROM 	Measurement_Qualifier
	WHERE 	Measurement_Type_Key = @Type
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_MeasurementQualifier_Select_ForType') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_MeasurementQualifier_Select_ForType'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_MeasurementQualifier_Select_ForType TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_MeasurementQualifier_Select_ForType TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_MeasurementQualifier_Select_ForType TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_MeasurementQualifier_Select_ForType TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_MeasurementQualifier_Select_ForType TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_MeasurementQualifier_Select_ForType TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_MeasurementType_Select') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_MeasurementType_Select]
GO

/*===========================================================================*\
  Description: Returns a list of Measurement Types

  Parameters:  @DataTable

  Created:     November 2004

  Last revision information:
    $Revision: 2 $
    $Date: 2/02/09 16:44 $
    $Author: Pauldavies $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_MeasurementType_Select]
	@DataTable VARCHAR(30)
AS
	SELECT		MT.Measurement_Type_Key AS Item_Key,
			MT.Short_Name
	FROM		Measurement_Type MT
	LEFT JOIN	Measurement_Type_Context MTC ON MTC.Measurement_Type_Key = MT.Measurement_Type_Key
	LEFT JOIN	Measurement_Context MC ON MC.Measurement_Context_Key = MTC.Measurement_Context_Key
	WHERE		MC.Data_Table = @DataTable
	OR		MC.Data_Table IS NULL
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_MeasurementType_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_MeasurementType_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_MeasurementType_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_MeasurementType_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_MeasurementType_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_MeasurementType_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_MeasurementType_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_MeasurementType_Select TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_MeasurementUnit_Select_ForType') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_MeasurementUnit_Select_ForType]
GO

/*===========================================================================*\
  Description: Returns a list of Measurement Units for the specified type

  Parameters:  @Type

  Created:     November 2004

  Last revision information:
    $Revision: 2 $
    $Date: 2/02/09 16:44 $
    $Author: Pauldavies $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_MeasurementUnit_Select_ForType]
	@Type CHAR(16)
AS
	SELECT	Measurement_Unit_Key AS Item_Key,
		Short_Name
	FROM 	Measurement_Unit
	WHERE 	Measurement_Type_Key = @Type
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_MeasurementUnit_Select_ForType') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_MeasurementUnit_Select_ForType'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_MeasurementUnit_Select_ForType TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_MeasurementUnit_Select_ForType TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_MeasurementUnit_Select_ForType TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_MeasurementUnit_Select_ForType TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_MeasurementUnit_Select_ForType TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_MeasurementUnit_Select_ForType TO [Dev - JNCC SQL]
END
GO

IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_QETemplateField_Insert') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_QETemplateField_Insert]
GO
    
/*===========================================================================*\
  Description:	Inserts a record in QE_Template_Field table

  Parameters:	Table's fields.

  Created:	August 2003

  Last revision information:
    $Revision: 2 $
    $Date: 2/02/09 16:44 $
    $Author: Pauldavies $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_QETemplateField_Insert]
	@Key 				CHAR(16) OUTPUT,
	@QETemplateKey 			CHAR(16),
	@QEFieldKey 			CHAR(16) = NULL,
	@GeneralTab 			BIT,
	@SpecimenTab 			BIT,
	@ItemName 			VARCHAR(100),
	@DefaultValue 			VARCHAR(200),
	@DefaultDisplay 		VARCHAR(200),
	@SessionID 			VARCHAR(16),
	@IsMeasurement 			BIT,
	@MeasurementAppliesTo 		VARCHAR(50) = NULL,
	@MeasurementMethodConceptKey 	CHAR(16) = NULL,
	@MeasurementDuration 		VARCHAR(50) = NULL,
	@MeasurementAccuracy 		VARCHAR(50) = NULL,
	@MeasurementParameterConceptKey CHAR(16) = NULL,
	@MeasurementunitConceptKey 	CHAR(16) = NULL,
	@MeasurementIsSpecimen 		BIT = NULL,
	@TaxonMeasurementQualifierKey	CHAR(16) = NULL,
	@TaxonMeasurementUnitKey	CHAR(16) = NULL,
	@MeasurementIsTaxonData		BIT = NULL,
	@Sequence 			INT = 0
AS

SET NOCOUNT OFF

	EXEC spNextKey 'QE_Template_Field', @Key OUTPUT

	INSERT INTO QE_Template_Field (
		QE_Template_Field_Key, QE_Template_Key, QE_Field_Key, General_Tab, Specimen_Tab,
		Item_Name, Default_Value, Default_Display, Entered_Session_ID, System_Supplied_Data, Is_Measurement,
		Measurement_Applies_To, Measurement_Method_Concept_Key, Measurement_Duration, Measurement_Accuracy,
		Measurement_Parameter_Concept_Key, Measurement_Unit_Concept_Key, Measurement_Is_Specimen,
		Measurement_Is_TaxonData, Taxon_Measurement_Qualifier_Key, Taxon_Measurement_Unit_Key, [Sequence]
	) VALUES (
		@Key, @QETemplateKey, @QEFieldKey, @GeneralTab, @SpecimenTab,
		@ItemName, @DefaultValue, @DefaultDisplay, @SessionID, 0, @IsMeasurement,
		@MeasurementAppliesTo, @MeasurementMethodConceptKey, @MeasurementDuration, @MeasurementAccuracy,
		@MeasurementParameterConceptKey, @MeasurementUnitConceptKey, @MeasurementIsSpecimen, 
		@MeasurementIsTaxonData, @TaxonMeasurementQualifierKey, @TaxonMeasurementUnitKey, @Sequence
	)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_QETemplateField_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_QETemplateField_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_QETemplateField_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_QETemplateField_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_QETemplateField_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_QETemplateField_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_QETemplateField_Insert TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_QETemplateField_Select_ForTemplate') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_QETemplateField_Select_ForTemplate]
GO

/*===========================================================================*\
  Description:	Selects the fields for a template

  Parameters:	@QETemplateKey - QE_Template_Key
		@TemplateType - see template type field description

  Created:	Jan 2004

  Last revision information:
    $Revision: 2 $
    $Date: 2/02/09 16:44 $
    $Author: Pauldavies $

\*===========================================================================*/    
CREATE PROCEDURE [dbo].[usp_QETemplateField_Select_ForTemplate]
	@QETemplateKey 	char(16),
	@TemplateType 	tinyint
AS

SET NOCOUNT ON

	SELECT  	F.Item_Name AS 'Field', 
			General_Tab,
		 	Specimen_Tab,
			TF.Item_Name AS 'Alternative_Name', 
			Default_Value, 
			F.Default_Size,
			TF.QE_Template_Field_Key, 
			TF.Timestamp,
			Data_Type, 
			F.QE_Field_Key,
			F.Field_Lookup_Key,
			Default_Display,
			Is_Measurement,
			NULL AS Measurement_Applies_To,			-- So we get columns named
			NULL AS Measurement_Method_Concept_Key,
			NULL AS Measurement_Duration,
			NULL AS Measurement_Accuracy,
			NULL AS Measurement_Parameter_Concept_Key,
			NULL AS Measurement_Unit_Concept_Key,
			NULL AS Measurement_Is_Specimen,
			NULL AS Measurement_Is_TaxonData,
			NULL AS Taxon_Measurement_Qualifier_Key,
			NULL AS Taxon_Measurement_Unit_Key,
			Field_Name,
			Table_Name
		
	FROM  		QE_Field F 
	LEFT JOIN 	QE_Template_Field TF ON F.QE_Field_Key = TF.QE_Field_Key AND @QETemplateKey = TF.QE_Template_Key AND Is_Measurement = 0
	WHERE 		(Template_Type & @TemplateType) <> 0

	UNION -- Measurements for Thesaurus determinations

	SELECT 	CT.Plaintext + ' (' + D.Item_Name + ')' COLLATE SQL_Latin1_General_CP1_CI_AS, 
		General_Tab,
		Specimen_Tab, 
		F.Item_Name, 
		Default_Value,
		20, 
		QE_Template_Field_Key, 
		F.Timestamp,
		0, 
		NULL,
		'',
		Default_Display,
	 	Is_Measurement,
		Measurement_Applies_To,
		Measurement_Method_Concept_Key,
		Measurement_Duration,
		Measurement_Accuracy,
		Measurement_Parameter_Concept_Key,
		Measurement_Unit_Concept_Key,
		Measurement_Is_Specimen,
		Measurement_Is_TaxonData,
		Taxon_Measurement_Qualifier_Key,
		Taxon_Measurement_Unit_Key,
		NULL,
		NULL
	FROM	QE_Template_Field F
	JOIN	vw_ConceptTerm CT ON Concept_Key = F.Measurement_Parameter_Concept_Key
	JOIN 	Concept_Group CG ON CT.Concept_Group_Key = CG.Concept_Group_Key
	JOIN 	Local_Domain LD ON LD.Local_Domain_Key = CG.Local_Domain_Key
	JOIN 	Domain D ON D.Domain_Key = LD.Domain_Key AND (D.Has_Occurrences = 1 OR D.Domain_Key = 'SYSTEM00000000')
	WHERE 	Is_Measurement = 1 AND QE_Template_Key = @QETemplateKey AND Measurement_Is_TaxonData = 0

	UNION -- Measurements for Taxon  determinations

	SELECT 	MT.Short_Name + ' of ' + MQ.Short_Name + ' (' + MU.Short_Name + ')' COLLATE SQL_Latin1_General_CP1_CI_AS, 
		General_Tab,
		Specimen_Tab, 
		F.Item_Name, 
		Default_Value,
		20, 
		QE_Template_Field_Key, 
		F.Timestamp,
		0, 
		NULL,
		'',
		Default_Display,
	 	Is_Measurement,
		Measurement_Applies_To,
		NULL,	-- Irrelevant for Taxon
		NULL,	-- Irrelevant for Taxon
		Measurement_Accuracy,
		Measurement_Parameter_Concept_Key,
		Measurement_Unit_Concept_Key,
		Measurement_Is_Specimen,
		Measurement_Is_TaxonData,
		Taxon_Measurement_Qualifier_Key,
		Taxon_Measurement_Unit_Key,
		NULL,
		NULL
	FROM	QE_Template_Field F
	JOIN	Measurement_Qualifier MQ ON MQ.Measurement_Qualifier_Key = Taxon_Measurement_Qualifier_Key
	JOIN	Measurement_Unit MU ON MU.Measurement_Unit_Key = Taxon_Measurement_Unit_Key 
	JOIN	Measurement_Type MT ON MT.Measurement_Type_Key = MU.Measurement_Type_Key
	WHERE 	Is_Measurement = 1 AND QE_Template_Key = @QETemplateKey AND Measurement_Is_Specimen = 0 AND Measurement_Is_TaxonData = 1

	ORDER BY 1
GO 

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_QETemplateField_Select_ForTemplate') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_QETemplateField_Select_ForTemplate'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_QETemplateField_Select_ForTemplate TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_QETemplateField_Select_ForTemplate TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_QETemplateField_Select_ForTemplate TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_QETemplateField_Select_ForTemplate TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_QETemplateField_Select_ForTemplate TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_QETemplateField_Select_ForTemplate TO [Dev - JNCC SQL]
END
GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_ReportBlockOrders_Select]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_ReportBlockOrders_Select]
GO

CREATE PROCEDURE [dbo].[usp_ReportBlockOrders_Select] 
@ReportBlockKey CHAR(16)

AS

--  DESCRIPTION
--  Returns the allowed ordering that can be applied to a Report Block
--
--  PARAMETERS
--	NAME				DESCRIPTION
--	@ReportBlockKey		Report Block which ordering is to be found
--
--
--  AUTHOR:     		Ben Collier, Dorset Software
--  CREATED:    		2004-05-01
--
SET NOCOUNT ON

SELECT Report_Block_Order_Key, Item_Name, Order_Clause_SQL
FROM
	Report_Block_Order
WHERE Report_Block_Key = @ReportBlockKey
ORDER BY Item_Name

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ReportBlockOrders_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ReportBlockOrders_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ReportBlockOrders_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ReportBlockOrders_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ReportBlockOrders_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ReportBlockOrders_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ReportBlockOrders_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ReportBlockOrders_Select TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TaxonDetermination_Insert') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_TaxonDetermination_Insert]
GO

/*===========================================================================*\
  Description:	Inserts a record in the Taxon Determination table.
		Ensures the Domain mask of the specimen is also updated.

  Parameters:	@Key 
		@DeterminedItemKey
		@OccurrenceKey  
		@SpecimenCollectionUnitKey  
		@DeterminationTypeKey  
		@NomenclaturalStatusConceptKey 
		@Confidence
		@DeterminerNameKey 
		@InferredDeterminer 
		@DeterminerRoleKey 
		@VagueDateStart
		@VagueDateEnd 
		@VagueDateType 
		@UsedSpecimen 
		@Preferred
		@Method
		@Notes 
		@SessionID
		@IsForSpecimen		Indicates whether to update preferred 
					flag in Specimen_Unit or Determination.

  Created:	July 2003

  Last revision information:
    $Revision: 2 $
    $Date: 2/02/09 16:44 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_TaxonDetermination_Insert]
	@Key char(16) output, 
	@DeterminedItemKey char(16), 
	@OccurrenceKey char(16), 
	@SpecimenCollectionUnitKey char(16), 
	@DeterminationTypeKey char(16), 
	@NomenclaturalStatusConceptKey char(16),
	@Confidence tinyint,
	@DeterminerNameKey char(16), 
	@InferredDeterminer tinyint,
	@DeterminerRoleKey char(16), 
	@VagueDateStart int, 
	@VagueDateEnd int, 
	@VagueDateType varchar(2),
	@UsedSpecimen bit,
	@Preferred bit,
	@Method text,
	@Notes text,
	@SessionID char(16),
	@IsForSpecimen bit
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF
	SET XACT_ABORT ON
	SET ANSI_NULLS ON

	--Wrap everything in a transaction
	BEGIN TRANSACTION

		-- Get the user name key, as Taxon_Determination table doesn't have SessionID fields.
		DECLARE	@EnteredBy char(16),
			@PreferredForSpecimen bit

		SELECT	@EnteredBy = User_Name_Key FROM Session WHERE Session_ID = @SessionID
		SET	@PreferredForSpecimen = 0

		-- Get a new key first.
		EXECUTE spNextKey 'Taxon_Determination', @Key OUTPUT

		/*---------------------------------------------------------------------------------*\
		  Ensure only one preferred determination per occurrence.
		\*---------------------------------------------------------------------------------*/
		IF @IsForSpecimen = 1
		BEGIN
			-- Either Preferred passed in as true, or not preferred but not set in 
			-- Speciment Unit either.
			IF @Preferred = 1
			OR (@Preferred = 0 AND EXISTS ( SELECT * FROM Specimen_Unit 
							WHERE Collection_Unit_Key = @SpecimenCollectionUnitKey 
							AND Preferred_Taxon_Determination_Key IS NULL))
				SET @PreferredForSpecimen = 1

			-- Not used for Taxon_Occurrence, unless not already one present
			IF EXISTS(SELECT 1 FROM Taxon_Determination WHERE Taxon_Occurrence_Key=@OccurrenceKey AND Preferred=1)
				SET @Preferred = 0
		END ELSE BEGIN
			-- If new determination not preferred, but there isn't one already, change the flag.
			IF @Preferred = 0 
			AND NOT EXISTS(SELECT * FROM Taxon_Determination WHERE Taxon_Occurrence_Key = @OccurrenceKey AND Preferred = 1)
				SET @Preferred = 1
			ELSE
			-- If new determination is preferred, make sure previous preferred is turned off.
			IF @Preferred = 1
			BEGIN
				UPDATE	Taxon_Determination
				SET	Preferred = 0
				WHERE	Taxon_Occurrence_Key = @OccurrenceKey
				AND	Preferred = 1

				IF @@Error <> 0 GOTO RollbackAndExit
			END
		END

		/*---------------------------------------------------------------------------------*\
		  Do table insert.
		\*---------------------------------------------------------------------------------*/
		INSERT INTO Taxon_Determination (
			Taxon_Determination_Key, Taxon_List_Item_Key, Taxon_Occurrence_Key, 
			Vague_Date_Start, Vague_Date_End, Vague_Date_Type, Comment, Preferred, Determiner, 
			Determination_Type_Key, Determiner_Role_Key, Entered_By, Entry_Date, 
			Specimen_Collection_Unit_Key, Nomenclatural_Status_Concept_Key,
			Confidence, Used_Specimen, Method, Inferred_Determiner
		) VALUES (
			@Key, @DeterminedItemKey, @OccurrenceKey, 
			@VagueDateStart, @VagueDateEnd, @VagueDateType, @Notes, @Preferred, @DeterminerNameKey,
			@DeterminationTypeKey, @DeterminerRoleKey, @EnteredBy, GetDate(), 
			@SpecimenCollectionUnitKey, @NomenclaturalStatusConceptKey, 
			@Confidence, @UsedSpecimen, @Method, @InferredDeterminer
		)

		IF @@Error <> 0 GOTO RollbackAndExit

		IF @IsForSpecimen = 0 AND @Preferred = 1
		BEGIN
			-- Update validation flag of Taxon_Occurrence
			DECLARE	@ValidationLevel int
			SELECT	@ValidationLevel = Validation_Level
			FROM	Taxon_List_Item TLI 
			JOIN	Taxon_Version TV ON TV.Taxon_Version_Key = TLI.Taxon_Version_Key
			WHERE	TLI.Taxon_List_Item_Key = @DeterminedItemKey

			DECLARE	@CompetencyLevel int
			SELECT	@CompetencyLevel = DR.Validation_Competency
			FROM	Taxon_Determination TD 
			JOIN	Determiner_Role DR ON DR.Determiner_Role_Key = TD.Determiner_Role_Key
			WHERE	TD.Taxon_Determination_Key = @Key

			UPDATE	Taxon_Occurrence
			SET	Verified = 
					CASE 
						WHEN @ValidationLevel IS NULL THEN 0
						WHEN @ValidationLevel <= @CompetencyLevel THEN 2
						ELSE 1
					END
			WHERE	Taxon_Occurrence_Key = @OccurrenceKey
		END ELSE
		IF @PreferredForSpecimen = 1
		BEGIN
			UPDATE	Specimen_Unit
			SET	Preferred_Taxon_Determination_Key = @Key
			WHERE	Collection_Unit_Key = @SpecimenCollectionUnitKey

			IF @@Error <> 0 GOTO RollbackAndExit
		END

		/*-------------------------------------------------------------*\
		  Switch bit of new mask ON in Collection_Unit.
		\*-------------------------------------------------------------*/
		IF @IsForSpecimen = 1 AND @PreferredForSpecimen = 1
		BEGIN
			DECLARE @ConceptKey char(16),
				@ConceptMask int

			-- Get the right concept before getting the mask
			SELECT	@ConceptKey = Concept_Key
			FROM	Taxon_Dictionary_Concept_Mapping
			WHERE	Taxon_List_Item_Key = @DeterminedItemKey

			-- Retrieve the mask of the new concept.
			EXECUTE	usp_Get_Concept_Domain_Mask @ConceptKey, @ConceptMask OUTPUT
			-- And switch appropriate bit ON in Collection_Unit
			EXECUTE	usp_CollectionUnit_Update_DomainMask @SpecimenCollectionUnitKey, @ConceptMask, 1

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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TaxonDetermination_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_TaxonDetermination_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_TaxonDetermination_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_TaxonDetermination_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_TaxonDetermination_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_TaxonDetermination_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_TaxonDetermination_Insert TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_TaxonList_ImportConceptGroup]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_TaxonList_ImportConceptGroup]
GO

/*===========================================================================*\
  Description:	Import a concept group as a taxon list.

  Parameters:   @job_id					Job identifier
				@taxon_list_key			Taxon list key
				@concept_group_key		Concept group key

  Created:		Dec 2003

  Last revision information:
	$Revision: 2 $
	$Date: 2/02/09 16:44 $
	$Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_TaxonList_ImportConceptGroup]
	@job_id				INT,
	@taxon_list_key		CHAR(16),
	@concept_group_key	CHAR(16),
	@SessionID	CHAR(16)
AS
	SET NOCOUNT ON

	DECLARE		@existing_list_key		CHAR(16)

	SELECT		@existing_list_key						=	Taxon_List_Key
	FROM		Taxon_Dictionary_Concept_Group_Mapping
	WHERE		Concept_Group_Key						=	@concept_group_key

	IF @@ROWCOUNT = 0
	BEGIN
		/* record mapping */
		INSERT		Taxon_Dictionary_Concept_Group_Mapping (
					Taxon_List_Key,
					Concept_Group_Key)
		VALUES		(@taxon_list_key,
					@concept_group_key)

		IF @@ERROR <> 0 RETURN
	END
	ELSE IF @existing_list_key <> @taxon_list_key
	BEGIN
		RAISERROR (
			'Concept group has previously been imported into a different taxon list',
			16,
			1)
		RETURN
	END

	/* Calculate size of job */
	DECLARE		@record_count			INT

	SELECT		@record_count			=	COUNT(*)
	FROM		Concept_Group_Version
	WHERE		Concept_Group_Key		=	@concept_group_key

	SELECT		@record_count							=	@record_count * 3
															+ COUNT(DISTINCT c.Name_Type_Concept_Key)
															+ COUNT(DISTINCT c.Term_Key)
															+ COUNT(DISTINCT c.Term_Version_Key)
															+ COUNT(DISTINCT j.Source_Join_Key)
															+ COUNT(DISTINCT c.Concept_Rank_Key)
															+ COUNT(DISTINCT c.Concept_Key)
															+ COUNT(DISTINCT d.Designation_Type_Concept_Key)
															+ COUNT(DISTINCT d.Concept_Designation_Key)
															+ COUNT(DISTINCT f.Thesaurus_Fact_Key
																	+ vm.Term_Version_Key)
	FROM		Concept									AS	c
	LEFT JOIN	Source_Join								AS	j
	ON			j.Record_Key							=	c.Term_Key
	AND			j.Table_Name							=	'Term'
	LEFT JOIN	Concept_Designation						AS	d
	ON			d.Concept_Key							=	c.Concept_Key
	LEFT JOIN	Thesaurus_Fact							AS	f
	ON			f.Meaning_Key							=	c.Meaning_Key
	LEFT JOIN	Taxon_Dictionary_Term_Version_Mapping	AS	vm
	ON			vm.Term_Version_Key						=	c.Term_Version_Key
	WHERE		c.Concept_Group_Key						=	@concept_group_key
	AND j.Source_Join_Key IS NOT NULL

	EXECUTE		usp_Import_Export_Job_Configure		@job_id,
													@concept_group_key,
													@record_count
	IF @@ERROR <> 0 RETURN
	
	/* import versions */
	EXECUTE		usp_TaxonListVersion_ImportConceptGroup		@job_id
	IF @@ERROR <> 0 RETURN

	/* import name types */
	EXECUTE		usp_TaxonNameType_ImportConceptGroup	@job_id
	IF @@ERROR <> 0 RETURN

	/* import taxa */
	EXECUTE		usp_Taxon_ImportConceptGroup	@job_id
	IF @@ERROR <> 0 RETURN

	/* import taxon versions */
	EXECUTE		usp_TaxonVersion_ImportConceptGroup		@job_id, @SessionID
	IF @@ERROR <> 0 RETURN

	/* import taxon/source relationships */
	EXECUTE		usp_TaxonSources_ImportConceptGroup		@job_id
	IF @@ERROR <> 0 RETURN

	/* import taxon ranks */
	EXECUTE		usp_TaxonRank_ImportConceptGroup	@job_id
	IF @@ERROR <> 0 RETURN

	/* import taxon list items */
	EXECUTE		usp_TaxonListItem_ImportConceptGroup	@job_id
	IF @@ERROR <> 0 RETURN

	/* import hierarchical relationships */
	EXECUTE		usp_TaxonListItem_ImportRelationships	@job_id
	IF @@ERROR <> 0 RETURN

	/* import designation types */
	EXECUTE		usp_TaxonDesignationType_ImportConceptGroup		@job_id
	IF @@ERROR <> 0 RETURN

	/* import designations */
	EXECUTE		usp_TaxonDesignation_ImportConceptGroup		@job_id
	IF @@ERROR <> 0 RETURN

	/* import facts */
	EXECUTE		usp_TaxonFact_ImportConceptGroup	@job_id
	IF @@ERROR <> 0 RETURN 

	EXECUTE		usp_Import_Export_Job_UpdateStatus	@job_id,
													'Updating Taxon Names Index...'
	/* Discard Index_Taxon_Name records for the concept group */
	DELETE ITN
	FROM Index_Taxon_Name ITN
	INNER JOIN Taxon_Dictionary_Concept_Mapping TDM ON TDM.Taxon_List_Item_Key=ITN.Taxon_List_Item_Key
	INNER JOIN Concept C1 ON C1.Concept_Key=TDM.Concept_Key
			AND C1.Concept_Group_Key=@Concept_Group_Key
	
	/* Rebuild Index_Taxon_Name for the concept group */
	INSERT INTO Index_Taxon_Name (Taxon_List_Item_Key, Taxon_List_Version_Key,
	 Actual_Name, Actual_Name_Italic, Common_Name, Common_Name_Italic, 
	  Preferred_Name, Preferred_Name_Italic, Abbreviation, Authority, System_Supplied_Data )
	SELECT TLI.Taxon_List_Item_Key, TLI.Taxon_List_Version_Key, 
	  T.Item_Name, CASE WHEN TR3.List_Font_Italic = 1 AND T.Language = 'La' THEN 1 ELSE 0 END, 
	  T2.Item_Name, CASE WHEN TR3.List_Font_Italic = 1 AND T2.Language = 'La' THEN 1 ELSE 0 END, 
	  T3.Item_Name, CASE WHEN TR3.List_Font_Italic = 1 AND T3.Language = 'La' THEN 1 ELSE 0 END, 
	  T.Abbreviation, T.Authority, 1 
	FROM ((((((((Taxon_List_Item AS TLI 
	INNER JOIN Taxon_Dictionary_Concept_Mapping TDM ON TDM.Taxon_List_Item_Key=TLI.Taxon_List_Item_Key
	INNER JOIN Concept C1 ON C1.Concept_Key=TDM.Concept_Key
			AND C1.Concept_Group_Key=@Concept_Group_Key
	LEFT JOIN Taxon_version AS TV ON TV.Taxon_Version_Key = TLI.Taxon_Version_Key) 
	LEFT JOIN Taxon AS T ON T.Taxon_Key = TV.Taxon_Key) 
	LEFT JOIN Taxon_Common_Name AS TCN ON TCN.Taxon_List_Item_Key = TLI.Taxon_List_Item_Key) 
	LEFT JOIN Taxon_Version AS TV2 ON TV2.Taxon_Version_Key = TCN.Taxon_Version_Key) 
	LEFT JOIN Taxon AS T2 ON T2.Taxon_Key = TV2.Taxon_Key) 
	LEFT JOIN Taxon_List_Item AS TLI3 ON TLI3.Taxon_List_Item_Key = TLI.Preferred_Name) 
	LEFT JOIN Taxon_Rank AS TR3 ON TR3.Taxon_Rank_Key = TLI3.Taxon_Rank_Key) 
	LEFT JOIN Taxon_Version AS TV3 ON TV3.Taxon_Version_Key = TLI3.Taxon_Version_Key) 
	LEFT JOIN Taxon AS T3 ON T3.Taxon_Key = TV3.Taxon_Key 
	WHERE TLI.Taxon_List_Version_To IS NULL

	UPDATE Import_Export_Job
	SET Records_Processed = Records_Processed + @@ROWCOUNT
	WHERE Import_Export_Job_ID = @job_id
	
	EXECUTE		usp_Import_Export_Job_UpdateStatus	@job_id,
													'Exporting Taxon Common Names...'	

	/* Create a local table containing the taxon common name data */
	DECLARE @TaxonCommonName TABLE (
		Taxon_List_Item_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS PRIMARY KEY,
		Taxon_Version_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS
	)

	INSERT INTO @TaxonCommonName
	SELECT DISTINCT TDM1.Taxon_List_Item_Key, TLI.Taxon_Version_Key
	FROM Taxon_Dictionary_Concept_Mapping TDM1 
	INNER JOIN Concept C1 ON C1.Concept_Key=TDM1.Concept_Key
			AND C1.Concept_Group_Key=@Concept_Group_Key
	LEFT JOIN (
			Concept C2 
			INNER JOIN Term T ON T.Term_Key=C2.Term_Key
			INNER JOIN Language L ON L.Language_Key=T.Language_Key AND L.Priority=1
		) ON C2.Meaning_Key=C1.Meaning_Key
			AND C2.Preferred=1
			AND C2.Name_Type_Concept_Key='SYSTEM000000000L'
	LEFT JOIN Concept C3 ON C3.Meaning_Key=C1.Meaning_Key
		AND C3.List_Preferred=1
		AND C3.Concept_Group_Key=C1.Concept_Group_Key
	INNER JOIN Taxon_Dictionary_Concept_Mapping TDM2 ON TDM2.Concept_Key=ISNULL(C2.Concept_Key, C3.Concept_Key)
	INNER JOIN Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key=TDM2.Taxon_List_Item_Key

	UPDATE Import_Export_Job
	SET Records_Processed = Records_Processed + @@ROWCOUNT
	WHERE Import_Export_Job_ID = @job_id

	/* Update existing taxon common name records that are out of date */
	UPDATE TCN
	SET Taxon_Version_Key=TCNTmp.Taxon_Version_Key
	FROM @TaxonCommonName TCNTmp
	INNER JOIN Taxon_Common_Name TCN ON TCN.Taxon_List_Item_Key=TCNTmp.Taxon_List_Item_Key
	WHERE TCN.Taxon_Version_Key=TCNTmp.Taxon_Version_Key
		
	/* Insert any new required taxon common name records */
	INSERT INTO Taxon_Common_Name
	SELECT DISTINCT TCNTmp.Taxon_List_Item_Key, TCNTmp.Taxon_Version_Key
	FROM @TaxonCommonName TCNTmp
	LEFT JOIN Taxon_Common_Name TCN ON TCN.Taxon_List_Item_Key=TCNTmp.Taxon_List_Item_Key
	WHERE TCN.Taxon_List_Item_Key IS NULL

	UPDATE Import_Export_Job
	SET Records_Processed = Records_Processed + @@ROWCOUNT
	WHERE Import_Export_Job_ID = @job_id


GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TaxonList_ImportConceptGroup') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_TaxonList_ImportConceptGroup'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_TaxonList_ImportConceptGroup TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_TaxonList_ImportConceptGroup TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_TaxonList_ImportConceptGroup TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TaxonOccurrenceData_Insert') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_TaxonOccurrenceData_Insert]
GO

/*===========================================================================*\
  Description:	Insert a record in Taxon_Occurrence_Data.

  Parameters:	

  Created:	November 2004

  Last revision information:
    $Revision: 2 $
    $Date: 2/02/09 16:44 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_TaxonOccurrenceData_Insert]
	@TaxonOccurrenceKey 	CHAR(16),
	@QualifierKey 		CHAR(16),
	@Accuracy		VARCHAR(10),
	@UnitKey		CHAR(16),
	@Data			VARCHAR(20),
	@EnteredBy		CHAR(16),
	@Key			CHAR(16) OUTPUT
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF
	
	/*-------------------------------------------------------------*\
	  Get a new key.
	\*-------------------------------------------------------------*/
	EXECUTE spNextKey 'Taxon_Occurrence_Data', @Key OUTPUT
	
	INSERT INTO Taxon_Occurrence_Data(
		Taxon_Occurrence_Data_Key, Taxon_Occurrence_Key, Data, Accuracy,
		Measurement_Qualifier_Key, Measurement_Unit_Key, Entered_By
	) VALUES (
		@Key, @TaxonOccurrenceKey, @Data, @Accuracy,
		@QualifierKey, @UnitKey, @EnteredBy
	)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TaxonOccurrenceData_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_TaxonOccurrenceData_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_TaxonOccurrenceData_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_TaxonOccurrenceData_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_TaxonOccurrenceData_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_TaxonOccurrenceData_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_TaxonOccurrenceData_Insert TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_TaxonOccurrence_TaxonDetermination_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_TaxonOccurrence_TaxonDetermination_Update]
GO

/*===========================================================================*\
  Description:	

  Parameters:	@TaxonOccurrenceKey	-Taxon Occurrence Key
		@SpecimenUnitKey	-Specimen Unit Key

  Created:	Feb 2004

  Last revision information:
    $Revision: 2 $
    $Date: 2/02/09 16:44 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_TaxonOccurrence_TaxonDetermination_Update]
	@TaxonOccurrenceKey char(16),
	@SpecimenUnitKey char(16)
AS

	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF
	SET ANSI_NULLS ON

	BEGIN TRANSACTION	
		/*----------------------------------------------------------------------------*\
		  A new Taxon_Determination record needs to be created and associated with the
		  new Taxon_Occurrence record. This will be a duplicate record of the 
		  Taxon_Determination record who's key is in the Preferred_Taxon_Determination
		  field of the Specimen_Unit table.
		\*----------------------------------------------------------------------------*/

		DECLARE @NewTaxonDeterminationKey char(16)

		EXECUTE spNextKey 'Taxon_Determination', @NewTaxonDeterminationKey OUTPUT

		INSERT INTO Taxon_Determination (
			Taxon_Determination_Key, Taxon_List_Item_Key, Taxon_Occurrence_Key, Vague_Date_Start,
			Vague_Date_End, Vague_Date_Type, Comment, Preferred, Determiner, Determination_Type_Key,
			Determiner_Role_Key, Entered_By, Entry_Date, Changed_By, Changed_Date, Source_Key,
			Custodian, Specimen_Collection_Unit_Key, Nomenclatural_Status_Concept_Key, Confidence,
			Used_Specimen, Method, Inferred_Determiner
		)
		SELECT 	@NewTaxonDeterminationKey, Taxon_List_Item_Key, @TaxonOccurrenceKey, Vague_Date_Start,
			Vague_Date_End, Vague_Date_Type, Comment, 1, Determiner, Determination_Type_Key,
			Determiner_Role_Key, Entered_By, Entry_Date, Changed_By, Changed_Date, Source_Key,
			Custodian, Specimen_Collection_Unit_Key, Nomenclatural_Status_Concept_Key, Confidence,
			Used_Specimen, Method, Inferred_Determiner
		FROM		Taxon_Determination AS TD
		INNER JOIN 	Specimen_Unit AS SU ON SU.Preferred_Taxon_Determination_Key = TD.Taxon_Determination_Key
		WHERE		SU.Collection_Unit_Key = @SpecimenUnitKey

		IF @@Error <> 0 GOTO RollBackAndExit

		-- Update Validation flag in Taxon Occurrence
		DECLARE	@ValidationLevel int
		SELECT	@ValidationLevel = Validation_Level
		FROM	Taxon_Determination TD 
		JOIN	Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key = TD.Taxon_List_Item_Key
		JOIN	Taxon_Version TV ON TV.Taxon_Version_Key = TLI.Taxon_Version_Key
		WHERE	TD.Taxon_Determination_Key = @NewTaxonDeterminationKey

		DECLARE	@CompetencyLevel int
		SELECT	@CompetencyLevel = DR.Validation_Competency
		FROM	Taxon_Determination TD 
		JOIN	Determiner_Role DR ON DR.Determiner_Role_Key = TD.Determiner_Role_Key
		WHERE	TD.Taxon_Determination_Key = @NewTaxonDeterminationKey

		UPDATE	Taxon_Occurrence
		SET	Verified = 
				CASE 
					WHEN @ValidationLevel IS NULL THEN 0
					WHEN @ValidationLevel <= @CompetencyLevel THEN 2
					ELSE 1
				END
		WHERE	Taxon_Occurrence_Key = @TaxonOccurrenceKey

		IF @@Error <> 0 GOTO RollBackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TaxonOccurrence_TaxonDetermination_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_TaxonOccurrence_TaxonDetermination_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_TaxonOccurrence_TaxonDetermination_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_TaxonOccurrence_TaxonDetermination_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_TaxonOccurrence_TaxonDetermination_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_TaxonOccurrence_TaxonDetermination_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_TaxonOccurrence_TaxonDetermination_Update TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_TaxonRank_ImportConceptGroup]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_TaxonRank_ImportConceptGroup]
GO

/*===========================================================================*\
  Description:	Import taxon ranks corresponding to concept ranks from the
				specified concept group.

  Parameters:	@job_id					Job identifier

  Created:		Dec 2003

  Last revision information:
	$Revision: 2 $
	$Date: 2/02/09 16:44 $
	$Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_TaxonRank_ImportConceptGroup]
	@job_id				INT
AS
	SET NOCOUNT ON

	DECLARE     @concept_group_key		CHAR(16),
				@concept_rank_key		CHAR(16),
				@sequence				SMALLINT,
				@short_name				VARCHAR(20),
				@long_name				VARCHAR(100),
				@entered_by				CHAR(16),
				@entry_date				SMALLDATETIME,
				@changed_by				CHAR(16),
				@changed_date			SMALLDATETIME,
				@system					BIT,
				@taxon_rank_key			CHAR(16),
				@DoUpdate				BIT

	/* determine parameters of job */
	SELECT      @concept_group_key			=	j.Concept_Group_Key
	FROM		Import_Export_Job			AS	j
	WHERE		j.Import_Export_Job_ID		=	@job_id

	IF @@ROWCOUNT = 0
	BEGIN
		RAISERROR ('Job does not exist or has not been configured', 16, 1)
		GOTO fail
	END

	EXECUTE		usp_Import_Export_Job_UpdateStatus	@job_id,
													'Exporting concept ranks'
	IF @@ERROR <> 0 GOTO fail

	DECLARE		ranks	CURSOR LOCAL FAST_FORWARD FOR
	SELECT DISTINCT
				cr.Concept_Rank_Key,
				cr.Sort_Order,
				cr.Abbreviation,
				cr.Item_Name,
				es.User_Name_Key,
				CONVERT(SMALLDATETIME,
						CONVERT(VARCHAR, es.Date_Time_Start, 112)),
				cs.User_Name_Key,
				CONVERT(SMALLDATETIME,
						CONVERT(VARCHAR, cs.Date_Time_Start, 112)),
				cr.System_Supplied_Data
	FROM		Concept				   			AS	c
	INNER JOIN	Concept_Rank					AS	cr
	ON			cr.Concept_Rank_Key				=	c.Concept_Rank_Key
	INNER JOIN	Session							AS	es
	ON			es.Session_ID					=	cr.Entered_Session_ID
	LEFT JOIN	Session							AS	cs
	ON			cs.Session_ID					=	cr.Changed_Session_ID
	WHERE		c.Concept_Group_Key				=	@concept_group_key

	OPEN		ranks

	WHILE 1 = 1
	BEGIN
		FETCH		ranks
		INTO		@concept_rank_key,
					@sequence,
					@short_name,
					@long_name,
					@entered_by,
					@entry_date,
					@changed_by,
					@changed_date,
					@system

		IF @@FETCH_STATUS <> 0 BREAK

		BEGIN TRANSACTION

		SELECT		@taxon_rank_key							=	Taxon_Rank_Key
		FROM		Taxon_Dictionary_Concept_Rank_Mapping
		WHERE		Concept_Rank_Key						=	@concept_rank_key

		IF @@ROWCOUNT = 0
		BEGIN
			/* No existing mapping, but search for a suitable taxon rank to map to */
			SELECT		@taxon_rank_key							=	Taxon_Rank_Key
			FROM		Taxon_Rank
			WHERE		Long_Name=@Long_Name
			AND 		Sequence=@sequence
			
			IF @@ROWCOUNT = 0
				SET @DoUpdate=0
			ELSE
				SET @DoUpdate=1
		END
		ELSE
			SET @DoUpdate=1

		IF @DoUpdate=1
		BEGIN
			/* update taxon rank */
		   UPDATE		TAXON_RANK
		   SET			SEQUENCE				=	@sequence,
						SHORT_NAME				=	@short_name,
						LONG_NAME				=	@long_name,
						ENTERED_BY				=	@entered_by,
						ENTRY_DATE				=	@entry_date,
						CHANGED_BY				=	@changed_by,
						CHANGED_DATE			=	@changed_date,
						SYSTEM_SUPPLIED_DATA	=	@system
		   WHERE		TAXON_RANK_KEY			=	@taxon_rank_key

		   IF @@ERROR <> 0 GOTO fail_from_cursor
		END
		ELSE
		BEGIN
			/* create taxon rank */
			EXECUTE		spNextKey		'TAXON_RANK',
										@taxon_rank_key		OUTPUT
			IF @@ERROR <> 0 GOTO fail_from_cursor

			INSERT		TAXON_RANK (
						TAXON_RANK_KEY,
						SEQUENCE,
						SHORT_NAME,
						LONG_NAME,
						LIST_FONT_ITALIC,
						DISPLAY_IN_DETAILS,
						ENTERED_BY,
						ENTRY_DATE,
						CHANGED_BY,
						CHANGED_DATE,
						SYSTEM_SUPPLIED_DATA)
			VALUES		(@taxon_rank_key,
						@sequence,
						@short_name,
						@long_name,
						0,
						0,
						@entered_by,
						@entry_date,
						@changed_by,
						@changed_date,
						@system)

			IF @@ERROR <> 0 GOTO fail_from_cursor

			/* record mapping */
			INSERT		Taxon_Dictionary_Concept_Rank_Mapping (
						Taxon_Rank_Key,
						Concept_Rank_Key)
			VALUES		(@taxon_rank_key,
						@concept_rank_key)

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END

		EXECUTE		usp_Import_Export_Job_RecordProcessed	@job_id
		IF @@ERROR <> 0 GOTO fail

		COMMIT TRANSACTION
	END

	CLOSE		ranks
	DEALLOCATE	ranks
	RETURN

fail_from_cursor:
	CLOSE		ranks
	DEALLOCATE	ranks

fail:
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_TaxonRank_ImportConceptGroup failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TaxonRank_ImportConceptGroup') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_TaxonRank_ImportConceptGroup'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_TaxonRank_ImportConceptGroup TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_TaxonRank_ImportConceptGroup TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_TaxonRank_ImportConceptGroup TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_Term_ImportTaxonList]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_Term_ImportTaxonList]
GO

/*===========================================================================*\
  Description:	Import terms corresponding to items in a taxon list.

  Parameters:   @job_id					Job identifier

  Created:		Nov 2003

  Last revision information:
	$Revision: 2 $
	$Date: 2/02/09 16:44 $
	$Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Term_ImportTaxonList]
	@job_id				INT
AS
	SET NOCOUNT ON
	SET ANSI_NULLS ON

	DECLARE     @concept_group_key	CHAR(16),
				@taxon_list_key		CHAR(16),
				@taxon_key			CHAR(16),
				@term_key			CHAR(16),
				@item_name      	VARCHAR(60),
				@language			VARCHAR(2),
				@ins_user_key		CHAR(16),
				@ins_date			SMALLDATETIME,
				@ins_session_id		CHAR(16),
				@upd_user_key		CHAR(16),
				@upd_date			SMALLDATETIME,
				@upd_session_id		CHAR(16),
				@system				BIT,
				@italic				BIT,
				@plaintext			NVARCHAR(300),
				@create_term		BIT

	/* determine parameters of job */
	SELECT		@concept_group_key						=	m.Concept_Group_Key,
				@taxon_list_key							=	m.Taxon_List_Key
	FROM		Import_Export_Job						AS	j
	INNER JOIN	Taxon_Dictionary_Concept_Group_Mapping	AS	m
	ON			m.Concept_Group_Key						=	j.Concept_Group_Key
	WHERE		j.Import_Export_Job_ID					=	@job_id

	IF @@ROWCOUNT = 0
	BEGIN
		RAISERROR ('Job does not exist or has not been configured', 16, 1)
		RETURN
	END

	EXECUTE		usp_Import_Export_Job_UpdateStatus	@job_id,
													'Importing terms'
	IF @@ERROR <> 0 RETURN

	DECLARE		@versions	TABLE (
				Taxon_Version_Key	CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS,
				List_Font_Italic	BIT)

	INSERT		@versions
    SELECT      tli.TAXON_VERSION_KEY,
                tr.List_Font_Italic
	FROM        TAXON_LIST_VERSION				AS	tlv
	INNER JOIN	TAXON_LIST_ITEM					AS	tli
	ON			tli.TAXON_LIST_VERSION_KEY		=	tlv.TAXON_LIST_VERSION_KEY
	INNER JOIN	TAXON_RANK						AS	tr
	ON			tr.TAXON_RANK_KEY				=	tli.TAXON_RANK_KEY
	WHERE		tlv.TAXON_LIST_KEY				=	@taxon_list_key

	DECLARE		terms	CURSOR LOCAL FAST_FORWARD FOR
	SELECT DISTINCT
				t.TAXON_KEY,
				t.ITEM_NAME,
				t.LANGUAGE,
				t.ENTERED_BY,
				t.ENTRY_DATE,
				t.CHANGED_BY,
				t.CHANGED_DATE,
				t.SYSTEM_SUPPLIED_DATA,
				CASE WHEN t.LANGUAGE = 'La'
					 AND v0.LIST_FONT_ITALIC = 1
					THEN 1
					ELSE 0
				END
	FROM		@versions							AS	v0
	INNER JOIN	TAXON_VERSION						AS	tv
	ON			tv.TAXON_VERSION_KEY				=	v0.TAXON_VERSION_KEY
	INNER JOIN	TAXON								AS	t
	ON			t.TAXON_KEY							=	tv.TAXON_KEY

	OPEN		terms

	WHILE 1 = 1
	BEGIN
		FETCH		terms
		INTO		@taxon_key,
					@plaintext,
					@language,
					@ins_user_key,
					@ins_date,
					@upd_user_key,
					@upd_date,
					@system,
					@italic

		IF @@FETCH_STATUS <> 0 BREAK

		SET			@item_name		=	CASE WHEN @italic = 0
											THEN @plaintext
											ELSE '<i>' + @plaintext + '</i>'
										END

		BEGIN TRANSACTION										

		/* obtain session identifiers */
		EXECUTE		usp_Session_ForDate		@ins_user_key,
											@ins_date,
											@ins_session_id		OUTPUT
		IF @@ERROR <> 0 GOTO fail_from_cursor

		IF @upd_user_key IS NULL
		BEGIN
			SET			@upd_session_id		=	NULL
		END
		ELSE
		BEGIN
			EXECUTE		usp_Session_ForDate		@upd_user_key,
												@upd_date,
												@upd_session_id		OUTPUT
			IF @@ERROR <> 0 GOTO fail_from_cursor
		END

		/* check for existing mapping */
		SELECT		@term_key						=	Term_Key
		FROM		Taxon_Dictionary_Term_Mapping
		WHERE		Taxon_Key						=	@taxon_key
		AND			Italic_Font						=	@italic

		SELECT		@create_term	=	CASE WHEN @@ROWCOUNT = 0
											THEN 1
											ELSE 0
										END

		IF @create_term = 0
		BEGIN
			IF NOT EXISTS (	SELECT		1
							FROM		Term
							WHERE		Term_Key				=	@term_key
							AND			Language_Key			=	@language
							AND			Item_Name				=	@item_name )
			BEGIN
				/* term has been modified */
				IF EXISTS (	SELECT		1
							FROM		Concept
							WHERE		Term_Key			=	@term_key
							AND			Concept_Group_Key	<>	@concept_group_key )
				BEGIN
					/* term is linked outside this concept group */
					SET			@create_term	=	1
				END
				ELSE
				BEGIN
					/* term linked only within this concept group */
					DECLARE		@new_term_key	CHAR(16)

					SELECT		@new_term_key	=	Term_Key
					FROM		Term
					WHERE		Language_Key	=	@language
					AND			Item_Name		=	@item_name
					AND			Term_Key		<>	@term_key

					IF @@ROWCOUNT = 0
					BEGIN
						/* update the current term */
						UPDATE		Term
						SET         Language_Key		=	@language,
									Item_Name			=	@item_name,
									Plaintext			=	@plaintext,
									Changed_Session_ID	=	@upd_session_id
						WHERE		Term_Key			=	@term_key

						IF @@ERROR <> 0 GOTO fail_from_cursor
					END
					ELSE
					BEGIN
						/* remove current term */
						UPDATE		Concept
						SET			Term_Key			=	@new_term_key,
									Term_Version_Key	=	NULL
						WHERE		Term_Key			=	@term_key

						IF @@ERROR <> 0 GOTO fail_from_cursor

						DELETE		Term_Version
						WHERE		Term_Key		=	@term_key

						IF @@ERROR <> 0 GOTO fail_from_cursor
						
						DELETE		Term
						WHERE		Term_Key		=	@term_key

						IF @@ERROR <> 0 GOTO fail_from_cursor

						/* link to the existing term that already
						 * has the new details */
						INSERT		Taxon_Dictionary_Term_Mapping (
									Taxon_Key,
									Italic_Font,
									Term_Key)
						VALUES		(@taxon_key,
									@italic,
									@new_term_key)

						IF @@ERROR <> 0 GOTO fail_from_cursor
					END
				END
			END /* term has been modified */
		END /* if @create_term = 0 */

		IF @create_term = 1
		BEGIN
			/* check for existing term that could be used */
			SELECT		@term_key		=	Term_Key
			FROM		Term
			WHERE		Language_Key	=	@language
			AND			Item_Name		=	@item_name

			IF @@ROWCOUNT > 0
			BEGIN
				/* map taxon onto the existing term */
				INSERT		Taxon_Dictionary_Term_Mapping (
							Taxon_Key,
							Italic_Font,
							Term_Key)
				VALUES		(@taxon_key,
							@italic,
							@term_key)

				IF @@ERROR <> 0 GOTO fail_from_cursor
			END
			ELSE
			BEGIN
				/* create term */
				EXECUTE		spNextKey	'Term',
										@term_key	OUTPUT
				IF @@ERROR <> 0 GOTO fail_from_cursor

				INSERT		Term (
							Term_Key,
							Language_Key,
							Item_Name,
							Plaintext,
							Entered_Session_ID,
							Changed_Session_ID,
							System_Supplied_Data)
				VALUES		(@term_key,
							@language,
							@item_name,
							@plaintext,
							@ins_session_id,
							@upd_session_id,
							@system)

				IF @@ERROR <> 0 GOTO fail_from_cursor

				/* record mapping */
				INSERT		Taxon_Dictionary_Term_Mapping
							(Taxon_Key,
							Italic_Font,
							Term_Key)
				VALUES		(@taxon_key,
							@italic,
							@term_key)

				IF @@ERROR <> 0 GOTO fail_from_cursor
			END
		END

		EXECUTE		usp_Import_Export_Job_RecordProcessed	@job_id
		IF @@ERROR <> 0 GOTO fail_from_cursor

		COMMIT TRANSACTION
	END

	CLOSE		terms
	DEALLOCATE	terms
	RETURN

fail_from_cursor:
	CLOSE		terms
	DEALLOCATE	terms

fail:
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_Term_ImportTaxonList failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Term_ImportTaxonList') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Term_ImportTaxonList'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Term_ImportTaxonList TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Term_ImportTaxonList TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Term_ImportTaxonList TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ThesaurusFact_Delete]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ThesaurusFact_Delete]
GO

/*===========================================================================*\
  Description:	Delete a record from the Thesaurus_Fact table.

  Parameters:	@Key		Thesaurus Fact key
		@Timestamp

  Created:	December 2003

  Last revision information:
    $Revision: 2 $
    $Date: 2/02/09 16:44 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ThesaurusFact_Delete]
	@Key char(16),
	@Timestamp timestamp = NULL,
	@SyncTaxonDict bit = 0
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION

		/*============================================================*\
		  See if the user wants any associated taxon dictionary
		  records be deleted with the concept.
		\*============================================================*/
		IF @SyncTaxonDict = 1 
		BEGIN
			/*--------------------------------------------------------*\
			  Check that the Taxon_Fact table exists before
			  attempting any of this deletion. 		
			\*--------------------------------------------------------*/
			IF EXISTS (SELECT *
		   			FROM   SysObjects 
					WHERE  Id = Object_Id(N'[dbo].[Taxon_Fact]')
					AND 	  Type = 'U')
			BEGIN
				DECLARE	@TaxonFactKey char(16)

				SELECT 	@TaxonFactKey = Taxon_Fact_Key
				FROM 	Taxon_Dictionary_Thesaurus_Fact_Mapping
				WHERE	Thesaurus_Fact_Key = @Key

				IF @@Error <> 0 GOTO RollbackAndExit

				DELETE	Taxon_Dictionary_Thesaurus_Fact_Mapping
				WHERE	Thesaurus_Fact_Key = @Key

				IF @@Error <> 0 GOTO RollbackAndExit

				DELETE	Taxon_Fact
				WHERE	Taxon_Fact_Key = @TaxonFactKey

				IF @@Error <> 0 GOTO RollbackAndExit

			END
		END	
		ELSE BEGIN
			DELETE	Taxon_Dictionary_Thesaurus_Fact_Mapping
			WHERE	Thesaurus_Fact_Key = @Key

			IF @@Error <> 0 GOTO RollbackAndExit
		END

		DELETE	Thesaurus_Fact
		WHERE	Thesaurus_Fact_Key = @Key
		AND		((@Timestamp = Timestamp) OR (@Timestamp IS NULL))

		IF @@Error <> 0 GOTO RollbackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ThesaurusFact_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ThesaurusFact_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ThesaurusFact_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ThesaurusFact_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ThesaurusFact_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ThesaurusFact_Delete TO [Dev - JNCC SQL]
END
GO

