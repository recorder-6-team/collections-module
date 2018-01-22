SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptRelation_Delete]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptRelation_Delete]
GO

/*===========================================================================*\
  Description:	Deletes a record from the ConceptRelation table.

  Parameters:	@Key	Concept_Relation_Key
		@Timestamp

  Created:	December 2003

  Last revision information: 
    $Revision: 1 $
    $Date: 7/12/07 15:34 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptRelation_Delete]
	@Key char(16),
	@Timestamp timestamp
AS

SET NOCOUNT ON

	/*-------------------------------------------------------------*\
	  Wrap everything in one transaction.
	\*-------------------------------------------------------------*/
	BEGIN TRANSACTION

		/*-------------------------------------------------------------*\
		  Remove Concept_Lineage records.
		\*-------------------------------------------------------------*/
		DECLARE		@from_concept_key	CHAR(16),
					@to_concept_key		CHAR(16),
					@relation_type_key	CHAR(16),
					@error				INT,
					@RecordsAffected	INT

		SELECT		@from_concept_key		=	From_Concept_Key,
					@to_concept_key			=	To_Concept_Key,
					@relation_type_key		=	Thesaurus_Relation_Type_Key
		FROM		Concept_Relation
		WHERE		Concept_Relation_Key	=	@Key

		IF @@ROWCOUNT > 0
		BEGIN
			EXECUTE		usp_ConceptLineage_RelationDeleted	@from_concept_key,
															@to_concept_key,
															@relation_type_key
			IF @@ERROR <> 0 GOTO RollbackAndExit
		END

		/*-------------------------------------------------------------*\
		  Update in Concept_Relation.
		\*-------------------------------------------------------------*/
		DELETE 	Concept_Relation
		WHERE	Concept_Relation_Key = @Key	
		AND	@Timestamp = Timestamp
		
		-- VI 13430 - CCN178 - TSEQUAL and stored procs
		SELECT	@error = @@ERROR,
			@RecordsAffected = @@ROWCOUNT

		IF @error <> 0 GOTO RollbackAndExit

		IF @RecordsAffected = 0 AND EXISTS (
			SELECT Concept_Relation_Key FROM Concept_Relation WHERE Concept_Relation_Key = @Key
		)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
		END

	/*-------------------------------------------------------------*\
	  All went well, so commit.
	\*-------------------------------------------------------------*/
	COMMIT TRANSACTION

RETURN 0

RollBackAndExit: 
	/*-------------------------------------------------------------*\
	  Cancel any changes, or left-overs might mess up some tables.
	\*-------------------------------------------------------------*/
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_ConceptRelation_Delete failed', 16, 1)
GO	

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptRelation_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptRelation_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptRelation_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptRelation_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptRelation_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptRelation_Delete TO [Dev - JNCC SQL]
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
    $Revision: 1 $
    $Date: 7/12/07 15:34 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptSimple_Update]
	@Key char(16),
	@TermKey char(16),
	@ConceptGroupKey char(16),
	@ListPreferred bit = NULL,
	@Preferred bit,
	@ConceptRankKey char(16),
	@NameTypeConceptKey char(16) = NULL,
	@SortCode int,
	@ListCode varchar(50),
	@SessionID char(16),
	@RecordsAffected int OUTPUT,
	@Timestamp timestamp
AS
SET NOCOUNT OFF
	
	BEGIN TRANSACTION
		/*-------------------*\
		  Update the Concept.
		\*-------------------*/
		DECLARE @old_concept_group_key CHAR(16),
			@old_list_preferred BIT,
			@OldConceptRankKey char(16),
			@error INT
		
		UPDATE	Concept
		SET 	@old_concept_group_key = Concept_Group_Key,
			@old_list_preferred = List_Preferred,
			@OldConceptRankKey = Concept_Rank_Key,
			List_Preferred = IsNull(@ListPreferred, List_Preferred),
			Concept_Group_Key = @ConceptGroupKey,
			Term_Key = @TermKey,
			Concept_Rank_Key = @ConceptRankKey,
			Preferred = @Preferred,
			Name_Type_Concept_Key = @NameTypeConceptKey,
			Sort_Code = @SortCode,
			List_Code = @ListCode,
			Changed_Session_ID = @SessionID			
		WHERE	Concept_Key = @Key
		AND	@Timestamp = Timestamp

		SELECT	@error = @@ERROR,
			@RecordsAffected = @@ROWCOUNT

		IF @error <> 0 GOTO RollbackAndExit

		-- VI 13430 - CCN178 - TSEQUAL and stored procs
		IF @RecordsAffected = 0 AND EXISTS (
			SELECT Concept_Key FROM Concept WHERE Concept_Key = @Key
		)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
		END

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
    $Revision: 1 $
    $Date: 7/12/07 15:34 $
    $Author: Johnvanbreda $

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
		SELECT	@error = @@ERROR,
			@RecordsAffected = @@ROWCOUNT

		IF @error <> 0 GOTO RollbackAndExit

		IF @RecordsAffected = 0 AND EXISTS (
			SELECT Concept_Key FROM Concept WHERE Concept_Key = @Key
		)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
		END

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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_QETemplateField_Select_ForTemplate') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_QETemplateField_Select_ForTemplate]
GO

/*===========================================================================*\
  Description:	Selects the fields for a template

  Parameters:	@QETemplateKey - QE_Template_Key
		@TemplateType - see template type field description

  Created:	Jan 2004

  Last revision information:
    $Revision: 1 $
    $Date: 7/12/07 15:34 $
    $Author: Johnvanbreda $

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
			Is_Custom,
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
			Table_Name,
			isnull(TF.Sequence, (select count(*) from QE_Template_Field)),
			NULL AS Number_Type_Concept_Key,
			NULL AS Number_Preferred,
			Hidden,
			Locked,
			NULL AS Metadata_Type_Key 
		
	FROM  		QE_Field F 
	LEFT JOIN 	QE_Template_Field TF ON F.QE_Field_Key = TF.QE_Field_Key AND @QETemplateKey = TF.QE_Template_Key AND Is_Custom = 0
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
	 	Is_Custom,
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
		NULL,
		isnull(F.Sequence, (select count(*) from QE_Template_Field)),
		NULL AS Number_Type_Concept_Key,
		NULL AS Number_Preferred,
		Hidden,
		Locked,
		NULL AS Metadata_Type_Key

	FROM	QE_Template_Field F
	JOIN	vw_ConceptTerm CT ON Concept_Key = F.Measurement_Parameter_Concept_Key
	JOIN 	Concept_Group CG ON CT.Concept_Group_Key = CG.Concept_Group_Key
	JOIN 	Local_Domain LD ON LD.Local_Domain_Key = CG.Local_Domain_Key
	JOIN 	Domain D ON D.Domain_Key = LD.Domain_Key AND (D.Has_Occurrences = 1 OR D.Domain_Key = 'SYSTEM00000000')
	WHERE 	Is_Custom = 1 AND QE_Template_Key = @QETemplateKey AND Measurement_Is_TaxonData = 0

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
	 	Is_Custom,
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
		NULL,
		isnull(F.Sequence, (select count(*) from QE_Template_Field)),
		NULL AS Number_Type_Concept_Key,
		NULL AS Number_Preferred,
		Hidden,
		Locked,
		NULL AS Metadata_Type_Key

	FROM	QE_Template_Field F
	JOIN	Measurement_Qualifier MQ ON MQ.Measurement_Qualifier_Key = Taxon_Measurement_Qualifier_Key
	JOIN	Measurement_Unit MU ON MU.Measurement_Unit_Key = Taxon_Measurement_Unit_Key 
	JOIN	Measurement_Type MT ON MT.Measurement_Type_Key = MU.Measurement_Type_Key
	WHERE 	Is_Custom = 1 AND QE_Template_Key = @QETemplateKey AND Measurement_Is_Specimen = 0 AND Measurement_Is_TaxonData = 1

	UNION --Numbers

	SELECT 	CT.Plaintext COLLATE SQL_Latin1_General_CP1_CI_AS, 
		General_Tab,
		Specimen_Tab, 
		F.Item_Name, 
		Default_Value,
		20, 
		QE_Template_Field_Key, 
		F.Timestamp,
		CASE F.Number_Type_Concept_Key
			WHEN 'SYSTEM0000000001' THEN 16 ELSE 0 END, 
		NULL,
		'',
		Default_Display,
	 	Is_Custom,
		NULL,
		NULL,
		NULL,
		NULL,
		NULL,
		NULL,
		NULL,
		NULL,
		NULL,
		NULL,
		NULL,
		NULL,
		isnull(F.Sequence, (select count(*) from QE_Template_Field)),
		F.Number_Type_Concept_Key,
		F.Number_Preferred,
		Hidden,
		Locked,
		NULL AS Metadata_Type_Key

	FROM	QE_Template_Field F
	JOIN	vw_ConceptTerm CT ON Concept_Key = F.Number_Type_Concept_Key
	WHERE 	Is_Custom = 2 AND QE_Template_Key = @QETemplateKey

	UNION --Metadata

	SELECT 	MT.Table_Name + ' ' + MT.Item_Name COLLATE SQL_Latin1_General_CP1_CI_AS, 
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
	 	Is_Custom,
		NULL,
		NULL,
		NULL,
		NULL,
		NULL,
		NULL,
		NULL,
		NULL,
		NULL,
		NULL,
		NULL,
		NULL,
		isnull(F.Sequence, (select count(*) from QE_Template_Field)),
		NULL AS Number_Type_Concept_Key,
		NULL AS Number_Preferred,
		Hidden,
		Locked,
		F.Metadata_Type_Key

	FROM	QE_Template_Field F
	JOIN	MetaData_Type MT ON MT.MetaData_Type_Key = F.Metadata_Type_Key
	WHERE 	Is_Custom = 3 AND QE_Template_Key = @QETemplateKey

	ORDER BY 26
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

