SET QUOTED_IDENTIFIER ON
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
    $Date: 2/02/09 16:49 $
    $Author: Pauldavies $

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

--test
/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Concept_Select_ForTopLevel]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Concept_Select_ForTopLevel]
GO

/*===========================================================================*\
  Description: Returns a list of concepts that are the top level for the 
    					 supplied concept group.

  Parameters:	@ConceptGroupKey
		@HierarchyRelationTypeKey - relationship type used to populate
						hierarchy.

  Created:	August 2003

  Last revision information:
    $Revision: 2 $
    $Date: 2/02/09 16:49 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Concept_Select_ForTopLevel]
	@ConceptGroupKey char(16),
  	@HierarchyRelationTypeKey char(16)
AS

SET ANSI_NULLS ON
SET ANSI_PADDING ON
SET ANSI_WARNINGS ON
SET ARITHABORT ON
SET CONCAT_NULL_YIELDS_NULL ON
SET QUOTED_IDENTIFIER ON
SET NO_BROWSETABLE OFF

	SELECT DISTINCT CT.Concept_Key, 
			CT.Item_Name, 
	  		CASE WHEN CR2.Concept_Relation_Key IS NULL THEN 0 ELSE 1 END AS HasChildren,
	  		CT.Concept_Rank_Key, 
			CT.Sort_Code,
			CT.PlainText  -- Required by the ORDER BY

	FROM 		VW_ConceptTerm CT
	LEFT JOIN 	Concept_Relation CR1 ON CR1.To_Concept_Key=CT.Concept_Key
	      				    AND CR1.Thesaurus_Relation_Type_Key=@HierarchyRelationTypeKey
			-- This is used to get the children. Join into Concept table to get only
			-- List_Preferred and Is_Current because these are the only one that will be 
			-- visible when expanded.
	LEFT JOIN 	(Concept_Relation CR2 
				INNER JOIN Concept AS C2 ON C2.Concept_Key = CR2.To_Concept_Key
							AND C2.List_Preferred = 1
							AND C2.Is_Current = 1) 
			ON CR2.From_Concept_Key=CT.Concept_Key
	       		AND CR2.Thesaurus_Relation_Type_Key=@HierarchyRelationTypeKey 
	WHERE 		CT.List_Preferred = 1
	AND 		CT.Is_Current = 1
	AND 		CT.Concept_Group_Key = @ConceptGroupKey
	AND 		CR1.From_Concept_Key IS NULL   -- i.e. No parents, therefore top level concept. 

	ORDER BY 	CT.Sort_Code, CT.PlainText  -- Use PlainText too, so list is alpha sorted when no Sort Codes.

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_Select_ForTopLevel') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Concept_Select_ForTopLevel'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Concept_Select_ForTopLevel TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForTopLevel TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForTopLevel TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForTopLevel TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForTopLevel TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Concept_Select_ForTopLevel TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_DepartmentName_Get]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_DepartmentName_Get]
GO

/*===========================================================================*\
  Description:	Returns the value of Item_Name or acronym from the 
	Organisation_Department table.

  Parameters:	@Key	Collection unit key
		@GetAcronym - if 1 then the acronym is returned if available
		@Name	OUTPUT

  Created:	September 2003

  Last revision information:
    $Revision: 2 $
    $Date: 2/02/09 16:49 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_DepartmentName_Get]
	@NameKey char(16),
	@GetAcronym bit=0,
	@FormattedName varchar(100) OUTPUT
AS

	SELECT	@FormattedName = CASE WHEN (@GetAcronym IS NOT NULL) AND (@GetAcronym = 1) 
				THEN IsNull(OD.Acronym, OD.Item_Name)
				ELSE OD.Item_Name
				END
	FROM	Organisation_Department AS OD
	WHERE	OD.Organisation_Department_Key = @NameKey
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_DepartmentName_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_DepartmentName_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_DepartmentName_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_DepartmentName_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_DepartmentName_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_DepartmentName_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_DepartmentName_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_DepartmentName_Get TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Languages_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Languages_Select]
GO

/*===========================================================================*\
  Description:	Returns all the Languages.

  Parameters:

  Created:	Setember 2003

  Last revision information:
    $Revision: 2 $
    $Date: 2/02/09 16:49 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Languages_Select]

AS

SET NOCOUNT ON

	SELECT		Lower(L.Language_Key) AS Language_Key,
			L.Item_Name,
			L.Priority

	FROM		Language AS L

	ORDER BY	ISNULL(Priority, 32767), L.Item_Name

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Languages_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Languages_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Languages_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Languages_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Languages_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Languages_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Languages_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Languages_Select TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ListSynonyms_Select_ForConcept]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ListSynonyms_Select_ForConcept]
GO

/*===========================================================================*\
  Description:	Returns List Synonyms

  Parameters:	@Key	Concept_Key

  Created:	December 2003

  Last revision information:
    $Revision: 2 $
    $Date: 2/02/09 16:49 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ListSynonyms_Select_ForConcept]
	@Key char(16)
AS

SET NOCOUNT ON

	/*=============================*\
	  Get the list synonyms.
	\*=============================*/
	SELECT 		CListSynonyms.Concept_Key AS Item_Key,
			IsNull(T.Item_Name + ' ' + TV.Author_And_Date, T.Item_Name) AS Item_Name,
			T.Language_Key,
			L.Item_Name AS Language
	FROM 		Concept AS CSource
	INNER JOIN	Concept AS CListSynonyms 	ON CListSynonyms.Meaning_Key = CSource.Meaning_Key 
							AND CListSynonyms.Concept_Group_Key = CSource.Concept_Group_Key
							AND CListSynonyms.Concept_Key <> @Key
	INNER JOIN	Term AS T 			ON T.Term_Key = CListSynonyms.Term_Key
	INNER JOIN 	Language AS L			ON L.Language_Key=T.Language_Key
	LEFT JOIN	Term_Version AS TV 		ON TV.Term_Version_Key = CListSynonyms.Term_Version_Key
	WHERE 		CSource.Concept_key = @Key

	ORDER BY 	Item_Name

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ListSynonyms_Select_ForConcept') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ListSynonyms_Select_ForConcept'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ListSynonyms_Select_ForConcept TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ListSynonyms_Select_ForConcept TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ListSynonyms_Select_ForConcept TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ListSynonyms_Select_ForConcept TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ListSynonyms_Select_ForConcept TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ListSynonyms_Select_ForConcept TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_MovementOfOwnership_Insert]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_MovementOfOwnership_Insert]
GO
/*===========================================================================*\
  Description: 	Inserts a new record into Movement_Of_Ownership
  Parameters:	@Key
		@MovementDirectionKey 
		@VagueDateStart
		@VagueDateEnd
		@VagueDateType
		@Notes
		@Completed
		@SessionID

  Created:	September 2003

  Last revision information:
    $Revision: 2 $
    $Date: 2/02/09 16:49 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_MovementOfOwnership_Insert]
	@Key char(16) OUTPUT,
	@ParentKey char(16),
	@MovementDirectionKey char(16)=null,
	@ContactNameKey char(16),
	@VagueDateStart int,
	@VagueDateEnd int,
	@VagueDateType varchar(2),
	@Notes text,
	@Completed bit,
	@SessionID char(16)	
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF


	/*-------------------------------------------------------------*\
	  Get the MovementDirectionKey if not supplied
	\*-------------------------------------------------------------*/
	IF @MovementDirectionKey IS NULL
		SELECT	@MovementDirectionKey = Movement_Direction_Key
		FROM 	Movement_Direction
		WHERE 	Movement_Key = @ParentKey
	
	EXECUTE spNextKey 'Movement_Of_Ownership', @Key OUTPUT

	BEGIN TRANSACTION

		INSERT INTO Movement_Of_Ownership (
			Movement_Of_Ownership_Key,
			Movement_Direction_Key,
			Vague_Date_Start,
			Vague_Date_End,
			Vague_Date_Type,
			Notes,
			Completed,
			Entered_Session_ID
		) VALUES (
			@Key,
			@MovementDirectionKey,
			@VagueDateStart,
			@VagueDateEnd,
			@VagueDateType,
			@Notes,
			@Completed,
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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_MovementOfOwnership_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_MovementOfOwnership_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_MovementOfOwnership_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_MovementOfOwnership_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_MovementOfOwnership_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_MovementOfOwnership_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_MovementOfOwnership_Insert TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Movement_Insert]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Movement_Insert]
GO

/*===========================================================================*\
  Description:	Inserts a record into the Movement table

  Parameters:	@Key
		@MovementType
		@OtherPartyNameKey
		@StaffResponsibleNameKey
		@DepartmentKey - if null, obtains department for staff responsible
		@ContactNameKey
		@VagueDateStart
		@VagueDateEnd
		@VagueDateType
		@Number
		@Notes
		@SessionID

  Created:	September 2003

  Last revision information:
    $Revision: 2 $
    $Date: 2/02/09 16:49 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Movement_Insert]
	@Key char(16) OUTPUT,
	@MovementType tinyint,
	@WithAcquisition bit, 
	@OtherPartyNameKey char(16),
	@StaffResponsibleNameKey char(16),
	@DepartmentKey char(16)=null,
	@ContactNameKey char(16),
	@VagueDateStart int, 
	@VagueDateEnd int, 
	@VagueDateType varchar(2),
	@Number varchar(30),
	@Notes text,
	@SessionID char(16) 
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	/*-------------------------------------------------------------*\
	  Get a new key.
	\*-------------------------------------------------------------*/
	EXECUTE spNextKey 'Movement', @Key OUTPUT

	DECLARE @MovementDirectionKey char(16),
		@MovementOfOwnershipKey char(16),
		@MovementOfMaterialKey char(16),
		@HoldingOrg char(16)

	SELECT @HoldingOrg = (SELECT Data FROM Setting WHERE Name = 'HoldingOrg')

	IF @DepartmentKey IS NULL
		SELECT 	@DepartmentKey = Organisation_Department_Key
		FROM	Individual
		WHERE	Name_Key = @StaffResponsibleNameKey

	BEGIN TRANSACTION

		/*-------------------------------------------------------------*\
		  Insert in Movement table.
		\*-------------------------------------------------------------*/
		INSERT INTO Movement (
			Movement_Key, Movement_Type, Other_Party_Name_Key, 
			Staff_Responsible_Name_Key, Contact_Name_Key, Exp_Vague_Date_Start, 
			Exp_Vague_Date_End, Exp_Vague_Date_Type, Number, 
			Notes, Entered_Session_ID
			
		) VALUES (
			@Key, @MovementType, @OtherPartyNameKey, 
			@StaffResponsibleNameKey, @ContactNameKey, @VagueDateStart, 
			@VagueDateEnd, @VagueDateType, @Number, 
			@Notes, @SessionID
		)
		IF @@Error <> 0 GOTO RollbackAndExit

		/*----------------------------------------*\
		   Accession / Accession with Acquisition
		\*----------------------------------------*/
		IF (@MovementType = 0)
		BEGIN
			-- Insert a record into the Movement_Direction table
			EXECUTE spNextKey 'Movement_Direction', @MovementDirectionKey OUTPUT
			INSERT INTO Movement_Direction (
				Movement_Direction_Key, Movement_Key,
				Receiver_Name_Key, Outbound, Entered_Session_ID
			) VALUES (
				@MovementDirectionKey, @Key,
				@HoldingOrg, 0, @SessionID
			)	

			IF @@Error <> 0 GOTO RollbackAndExit

			EXECUTE spNextKey 'Movement_Of_Ownership', @MovementOfOwnershipKey OUTPUT		
			INSERT INTO Movement_Of_Ownership (
				Movement_Of_Ownership_Key, Movement_Direction_Key, Contact_Name_Key,
				Vague_Date_Start, Vague_Date_End, Vague_Date_Type, Notes,
				Entered_Session_ID
			) VALUES (
				@MovementOfOwnershipKey, @MovementDirectionKey, @ContactNameKey,
				@VagueDateStart, @VagueDateEnd, @VagueDateType, @Notes,
				@SessionID
			)
			IF @@Error <> 0 GOTO RollbackAndExit
			
			/*----------------------------------------------------------------*\
		 	   Insert record into the Movement_Of_Material table if it is an
			   Accession with Acquisition.
			\*----------------------------------------------------------------*/			
			IF (@WithAcquisition = 1)
			BEGIN
				EXECUTE spNextKey 'Movement_Of_Material', @MovementOfMaterialKey OUTPUT
				INSERT INTO Movement_Of_Material (
					Movement_Of_Material_Key, Movement_Direction_Key, Contact_Name_Key,
					Vague_Date_Start, Vague_Date_End, Vague_Date_Type, Entered_Session_ID, Receiver_Name_Key,
					Receiver_Organisation_Department_Key
				) VALUES (
					@MovementOfMaterialKey, @MovementDirectionKey, @ContactNameKey, 
					@VagueDateStart, @VagueDateEnd, @VagueDateType, @SessionID, @StaffResponsibleNameKey,
					@DepartmentKey
				)	
				IF @@Error <> 0 GOTO RollbackAndExit
			END
		END ELSE
		/*----------------*\
		  Exchange
		\*----------------*/
		IF @MovementType = 1
		BEGIN
			/*-------------------------------------------------------------*\
		 	   Inserting records into the tables for the inbound movement.
			\*-------------------------------------------------------------*/
			EXECUTE spNextKey 'Movement_Direction', @MovementDirectionKey OUTPUT
			INSERT INTO Movement_Direction (
				Movement_Direction_Key, Movement_Key,
				Receiver_Name_Key, Outbound, Entered_Session_ID
			) VALUES (
				@MovementDirectionKey, @Key,
				@HoldingOrg, 0, @SessionID
			)	
			IF @@Error <> 0 GOTO RollbackAndExit

			EXECUTE spNextKey 'Movement_Of_Ownership', @MovementOfOwnershipKey OUTPUT		
			INSERT INTO Movement_Of_Ownership (
				Movement_Of_Ownership_Key, Movement_Direction_Key, Contact_Name_Key,
				Vague_Date_Start, Vague_Date_End, Vague_Date_Type, Notes,
				Entered_Session_ID
			) VALUES (
				@MovementOfOwnershipKey, @MovementDirectionKey, @ContactNameKey,
				@VagueDateStart, @VagueDateEnd, @VagueDateType, @Notes,
				@SessionID
			)
			IF @@Error <> 0 GOTO RollbackAndExit

			EXECUTE spNextKey 'Movement_Of_Material', @MovementOfMaterialKey OUTPUT
			INSERT INTO Movement_Of_Material (
				Movement_Of_Material_Key, Movement_Direction_Key, Contact_Name_Key,
				Vague_Date_Start, Vague_Date_End, Vague_Date_Type, Entered_Session_ID, Receiver_Name_Key,
				Receiver_Organisation_Department_Key
			) VALUES (
				@MovementOfMaterialKey, @MovementDirectionKey, @ContactNameKey,
				@VagueDateStart, @VagueDateEnd, @VagueDateType, @SessionID, @StaffResponsibleNameKey,
				@DepartmentKey
			)	
			IF @@Error <> 0 GOTO RollbackAndExit

			/*-------------------------------------------------------------*\
		 	   Inserting records into the tables for the outbound movement.
			\*-------------------------------------------------------------*/
			EXECUTE spNextKey 'Movement_Direction', @MovementDirectionKey OUTPUT
			INSERT INTO Movement_Direction (
				Movement_Direction_Key, Movement_Key,
				Receiver_Name_Key, Outbound, Entered_Session_ID
			) VALUES (
				@MovementDirectionKey, @Key,
				@OtherPartyNameKey, 1, @SessionID
			)		
			IF @@Error <> 0 GOTO RollbackAndExit
			
			EXECUTE spNextKey 'Movement_Of_Ownership', @MovementOfOwnershipKey OUTPUT		
			INSERT INTO Movement_Of_Ownership (
				Movement_Of_Ownership_Key, Movement_Direction_Key, Contact_Name_Key,
				Vague_Date_Start, Vague_Date_End, Vague_Date_Type, Notes,
				Entered_Session_ID
			) VALUES (
				@MovementOfOwnershipKey, @MovementDirectionKey, @ContactNameKey,
				@VagueDateStart, @VagueDateEnd, @VagueDateType, @Notes,
				@SessionID
			)
			IF @@Error <> 0 GOTO RollbackAndExit

			EXECUTE spNextKey 'Movement_Of_Material', @MovementOfMaterialKey OUTPUT
			INSERT INTO Movement_Of_Material (
				Movement_Of_Material_Key, Movement_Direction_Key, Contact_Name_Key,
				Vague_Date_Start, Vague_Date_End, Vague_Date_Type, Entered_Session_ID, Receiver_Name_Key,
				Receiver_Organisation_Department_Key
			) VALUES (
				@MovementOfMaterialKey, @MovementDirectionKey, @ContactNameKey,
				@VagueDateStart, @VagueDateEnd, @VagueDateType, @SessionID, @OtherPartyNameKey,
				@DepartmentKey
			)	
			IF @@Error <> 0 GOTO RollbackAndExit
		END ELSE
		/*------------*\
		  Loan In 
		\*------------*/
		IF @MovementType = 2
		BEGIN
			/*-------------------------------------------------------------*\
		 	   Inserting records into the tables for the inbound movement.
			\*-------------------------------------------------------------*/
			EXECUTE spNextKey 'Movement_Direction', @MovementDirectionKey OUTPUT
			INSERT INTO Movement_Direction (
				Movement_Direction_Key, Movement_Key,
				Receiver_Name_Key, Outbound, Entered_Session_ID
			) VALUES (
				@MovementDirectionKey, @Key,
				@HoldingOrg, 0, @SessionID
			)	
			IF @@Error <> 0 GOTO RollbackAndExit

			EXECUTE spNextKey 'Movement_Of_Material', @MovementOfMaterialKey OUTPUT
			INSERT INTO Movement_Of_Material (
				Movement_Of_Material_Key, Movement_Direction_Key, Contact_Name_Key,
				Vague_Date_Start, Vague_Date_End, Vague_Date_Type, Entered_Session_ID, Receiver_Name_Key,
				Receiver_Organisation_Department_Key
			) VALUES (
				@MovementOfMaterialKey, @MovementDirectionKey, @ContactNameKey,
				@VagueDateStart, @VagueDateEnd, @VagueDateType, @SessionID, @StaffResponsibleNameKey,
				@DepartmentKey
			)	
			IF @@Error <> 0 GOTO RollbackAndExit

			/*----------------------------------------------------------------------*\
		 	  A 'loan in' movement means that the linked items are owned elsewhere.
			  This information is not currently stored anywhere, so create a 
			  Movement_Of_Ownership record so we know the items are owned elsewhere.
			\*----------------------------------------------------------------------*/
			EXECUTE spNextKey 'Movement_Of_Ownership', @MovementOfOwnershipKey OUTPUT		
			INSERT INTO Movement_Of_Ownership (
				Movement_Of_Ownership_Key, Movement_Direction_Key, Contact_Name_Key,
				Vague_Date_Start, Vague_Date_End, Vague_Date_Type, Notes,
				Entered_Session_ID
			) VALUES (
				@MovementOfOwnershipKey, @MovementDirectionKey, @ContactNameKey,
				@VagueDateStart, @VagueDateEnd, @VagueDateType, @Notes,
				@SessionID
			)
			IF @@Error <> 0 GOTO RollbackAndExit

			/*-------------------------------------------------------------*\
		 	   Inserting records into the tables for the outbound movement.
			\*-------------------------------------------------------------*/
			EXECUTE spNextKey 'Movement_Direction', @MovementDirectionKey OUTPUT
			INSERT INTO Movement_Direction (
				Movement_Direction_Key, Movement_Key,
				Receiver_Name_Key, Outbound, Entered_Session_ID
			) VALUES (
				@MovementDirectionKey, @Key,
				@OtherPartyNameKey, 1, @SessionID
			)	
			IF @@Error <> 0 GOTO RollbackAndExit	
			
			EXECUTE spNextKey 'Movement_Of_Material', @MovementOfMaterialKey OUTPUT
			INSERT INTO Movement_Of_Material (
				Movement_Of_Material_Key, Movement_Direction_Key, Contact_Name_Key,
				Vague_Date_Start, Vague_Date_End, Vague_Date_Type, Entered_Session_ID, Receiver_Name_Key,
				Receiver_Organisation_Department_Key
			) VALUES (
				@MovementOfMaterialKey, @MovementDirectionKey, @ContactNameKey,
				@VagueDateStart, @VagueDateEnd, @VagueDateType, @SessionID, @OtherPartyNameKey,
				@DepartmentKey
			)	


		END ELSE
		/*----------*\
		  Loan out
		\*----------*/
		IF @MovementType = 3
		BEGIN
			/*-------------------------------------------------------------*\
		 	   Inserting records into the tables for the outbound movement.
			\*-------------------------------------------------------------*/
			EXECUTE spNextKey 'Movement_Direction', @MovementDirectionKey OUTPUT
			INSERT INTO Movement_Direction (
				Movement_Direction_Key, Movement_Key,
				Receiver_Name_Key, Outbound, Entered_Session_ID
			) VALUES (
				@MovementDirectionKey, @Key,
				@HoldingOrg, 1, @SessionID
			)	
			IF @@Error <> 0 GOTO RollbackAndExit

			EXECUTE spNextKey 'Movement_Of_Material', @MovementOfMaterialKey OUTPUT
			INSERT INTO Movement_Of_Material (
				Movement_Of_Material_Key, Movement_Direction_Key, Contact_Name_Key,
				Vague_Date_Start, Vague_Date_End, Vague_Date_Type, Entered_Session_ID, Receiver_Name_Key,
				Receiver_Organisation_Department_Key
			) VALUES (
				@MovementOfMaterialKey, @MovementDirectionKey, @ContactNameKey,
				@VagueDateStart, @VagueDateEnd, @VagueDateType, @SessionID, @OtherPartyNameKey,
				@DepartmentKey
			)	
			IF @@Error <> 0 GOTO RollbackAndExit

			/*-------------------------------------------------------------*\
		 	   Inserting records into the tables for the inbound movement.
			\*-------------------------------------------------------------*/
			EXECUTE spNextKey 'Movement_Direction', @MovementDirectionKey OUTPUT
			INSERT INTO Movement_Direction (
				Movement_Direction_Key, Movement_Key,
				Receiver_Name_Key, Outbound, Entered_Session_ID
			) VALUES (
				@MovementDirectionKey, @Key,
				@OtherPartyNameKey, 0, @SessionID
			)	
			IF @@Error <> 0 GOTO RollbackAndExit	
			
			EXECUTE spNextKey 'Movement_Of_Material', @MovementOfMaterialKey OUTPUT
			INSERT INTO Movement_Of_Material (
				Movement_Of_Material_Key, Movement_Direction_Key, Contact_Name_Key,
				Vague_Date_Start, Vague_Date_End, Vague_Date_Type, Entered_Session_ID, Receiver_Name_Key,
				Receiver_Organisation_Department_Key
			) VALUES (
				@MovementOfMaterialKey, @MovementDirectionKey, @ContactNameKey,
				@VagueDateStart, @VagueDateEnd, @VagueDateType, @SessionID, @StaffResponsibleNameKey,
				@DepartmentKey
			)	
			IF @@Error <> 0 GOTO RollbackAndExit
		END ELSE
		/*-----------------*\
		  Destroyed / Lost
		\*-----------------*/
		IF @MovementType = 4 OR @MovementType = 7
		BEGIN
			EXECUTE spNextKey 'Movement_Direction', @MovementDirectionKey OUTPUT
			INSERT INTO Movement_Direction (
				Movement_Direction_Key, Movement_Key,
				Receiver_Name_Key, Outbound, Entered_Session_ID
			) VALUES (
				@MovementDirectionKey, @Key,
				NULL, 1, @SessionID
			)	
			IF @@Error <> 0 GOTO RollbackAndExit

			EXECUTE spNextKey 'Movement_Of_Material', @MovementOfMaterialKey OUTPUT
			INSERT INTO Movement_Of_Material (
				Movement_Of_Material_Key, Movement_Direction_Key, 
				Vague_Date_Start, Vague_Date_End, Vague_Date_Type, Entered_Session_ID,
				Receiver_Organisation_Department_Key 
			) VALUES (
				@MovementOfMaterialKey, @MovementDirectionKey, 
				@VagueDateStart, @VagueDateEnd, @VagueDateType, @SessionID,
				@DepartmentKey 
			)
			IF @@Error <> 0 GOTO RollbackAndExit
		END ELSE
		/*-----------*\
		  Disposed
		\*-----------*/
		IF @MovementType = 5
		BEGIN
			EXECUTE spNextKey 'Movement_Direction', @MovementDirectionKey OUTPUT
			INSERT INTO Movement_Direction (
				Movement_Direction_Key, Movement_Key,
				Receiver_Name_Key, Outbound, Entered_Session_ID
			) VALUES (
				@MovementDirectionKey, @Key,
				NULL, 1, @SessionID
			)	
			IF @@Error <> 0 GOTO RollbackAndExit

			EXECUTE spNextKey 'Movement_Of_Ownership', @MovementOfOwnershipKey OUTPUT		
			INSERT INTO Movement_Of_Ownership (
				Movement_Of_Ownership_Key, Movement_Direction_Key, Contact_Name_Key,
				Vague_Date_Start, Vague_Date_End, Vague_Date_Type, Notes,
				Entered_Session_ID
			) VALUES (
				@MovementOfOwnershipKey, @MovementDirectionKey, @ContactNameKey,
				@VagueDateStart, @VagueDateEnd, @VagueDateType, @Notes,
				@SessionID
			)
			IF @@Error <> 0 GOTO RollbackAndExit

			EXECUTE spNextKey 'Movement_Of_Material', @MovementOfMaterialKey OUTPUT
			INSERT INTO Movement_Of_Material (
				Movement_Of_Material_Key, Movement_Direction_Key, Contact_Name_Key,
				Vague_Date_Start, Vague_Date_End, Vague_Date_Type, Entered_Session_ID, Receiver_Name_Key,
				Receiver_Organisation_Department_Key
			) VALUES (
				@MovementOfMaterialKey, @MovementDirectionKey, @ContactNameKey,
				@VagueDateStart, @VagueDateEnd, @VagueDateType, @SessionID, @OtherPartyNameKey,
				@DepartmentKey
			)
			IF @@Error <> 0 GOTO RollbackAndExit
		END ELSE
		/*-------------------*\
		  Internal transfer
		\*-------------------*/
		IF @MovementType = 6
		BEGIN
			EXECUTE spNextKey 'Movement_Direction', @MovementDirectionKey OUTPUT
			INSERT INTO Movement_Direction (
				Movement_Direction_Key, Movement_Key,
				Receiver_Name_Key, Outbound, Entered_Session_ID
			) VALUES (
				@MovementDirectionKey, @Key,
				@HoldingOrg, 0, @SessionID
			)	
			IF @@Error <> 0 GOTO RollbackAndExit

			EXECUTE spNextKey 'Movement_Of_Material', @MovementOfMaterialKey OUTPUT
			INSERT INTO Movement_Of_Material (
				Movement_Of_Material_Key, Movement_Direction_Key, Contact_Name_Key,
				Vague_Date_Start, Vague_Date_End, Vague_Date_Type, Entered_Session_ID, Receiver_Name_Key,
				Receiver_Organisation_Department_Key
			) VALUES (
				@MovementOfMaterialKey, @MovementDirectionKey, @ContactNameKey,
				@VagueDateStart, @VagueDateEnd, @VagueDateType, @SessionID, @StaffResponsibleNameKey,
				@DepartmentKey	
			)
			IF @@Error <> 0 GOTO RollbackAndExit
		END ELSE
		/*-------*\
		  Sold
		\*-------*/
		IF @MovementType = 8
		BEGIN
			EXECUTE spNextKey 'Movement_Direction', @MovementDirectionKey OUTPUT
			INSERT INTO Movement_Direction (
				Movement_Direction_Key, Movement_Key,
				Receiver_Name_Key, Outbound, Entered_Session_ID
			) VALUES (
				@MovementDirectionKey, @Key,
				@OtherPartyNameKey, 1, @SessionID
			)	
			IF @@Error <> 0 GOTO RollbackAndExit

			EXECUTE spNextKey 'Movement_Of_Ownership', @MovementOfOwnershipKey OUTPUT		
			INSERT INTO Movement_Of_Ownership (
				Movement_Of_Ownership_Key, Movement_Direction_Key, Contact_Name_Key,
				Vague_Date_Start, Vague_Date_End, Vague_Date_Type, Notes,
				Entered_Session_ID
			) VALUES (
				@MovementOfOwnershipKey, @MovementDirectionKey, @ContactNameKey,
				@VagueDateStart, @VagueDateEnd, @VagueDateType, @Notes,
				@SessionID
			)
			IF @@Error <> 0 GOTO RollbackAndExit

			EXECUTE spNextKey 'Movement_Of_Material', @MovementOfMaterialKey OUTPUT
			INSERT INTO Movement_Of_Material (
				Movement_Of_Material_Key, Movement_Direction_Key, Contact_Name_Key,
				Vague_Date_Start, Vague_Date_End, Vague_Date_Type, Entered_Session_ID, Receiver_Name_Key,
				Receiver_Organisation_Department_Key
			) VALUES (
				@MovementOfMaterialKey, @MovementDirectionKey, @ContactNameKey,
				@VagueDateStart, @VagueDateEnd, @VagueDateType, @SessionID, @OtherPartyNameKey,
				@DepartmentKey
			)
			IF @@Error <> 0 GOTO RollbackAndExit
		END ELSE
		/*----------------*\
		  Hosted Material
		\*----------------*/
		IF @MovementType = 9
		BEGIN
			EXECUTE spNextKey 'Movement_Direction', @MovementDirectionKey OUTPUT
			INSERT INTO Movement_Direction (
				Movement_Direction_Key, Movement_Key,
				Receiver_Name_Key, Outbound, Entered_Session_ID
			) VALUES (
				@MovementDirectionKey, @Key,
				@OtherPartyNameKey, 1, @SessionID
			)	
			IF @@Error <> 0 GOTO RollbackAndExit

			EXECUTE spNextKey 'Movement_Of_Ownership', @MovementOfOwnershipKey OUTPUT		
			INSERT INTO Movement_Of_Ownership (
				Movement_Of_Ownership_Key, Movement_Direction_Key, Contact_Name_Key,
				Vague_Date_Start, Vague_Date_End, Vague_Date_Type, Notes,
				Entered_Session_ID
			) VALUES (
				@MovementOfOwnershipKey, @MovementDirectionKey, @ContactNameKey,
				@VagueDateStart, @VagueDateEnd, @VagueDateType, @Notes,
				@SessionID
			)
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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Movement_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Movement_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Movement_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Movement_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Movement_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Movement_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Movement_Insert TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_QESession_SearchAndReplace]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_QESession_SearchAndReplace]
GO

/*===========================================================================*\
  Description:	Searches and replaces a string in a given template field for
	a quick entry session.  Returns a list of data item keys that were 
	updated

  Parameters:	@QESessionKey
			@TemplateFieldKey
			@SearchText
			@ReplaceText			

  Created:	Jan 2006

  Last revision information:
    $Revision: 2 $
    $Date: 2/02/09 16:49 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_QESession_SearchAndReplace]
@QESessionKey CHAR(16),
@TemplateFieldKey CHAR(16),
@SearchText VARCHAR(100),
@ReplaceText VARCHAR(100)
AS
	-- Return a results set that lists the items that are updated
	SELECT QE_Data_Item_Key
	FROM QE_Data_Item DI
	INNER JOIN QE_Data_Row DR
		ON DR.QE_Data_Row_Key=DI.QE_Data_Row_Key
		AND DR.QE_Session_Key=@QESessionKey
		AND DR.Processed=0
	WHERE DI.QE_Template_Field_Key=@TemplateFieldKey
	AND DI.Data_Display LIKE '%' + @SearchText + '%'

	-- Now perform the udpate
	UPDATE DI
	SET 	DI.Data_Display=REPLACE(DI.Data_Display, @SearchText, @ReplaceText),
		DI.Data_Value=REPLACE(DI.Data_Value, @SearchText, @ReplaceText)
	FROM QE_Data_Item DI
	INNER JOIN QE_Data_Row DR
		ON DR.QE_Data_Row_Key=DI.QE_Data_Row_Key
		AND DR.QE_Session_Key=@QESessionKey
		AND DR.Processed=0
	WHERE DI.QE_Template_Field_Key=@TemplateFieldKey
	AND DI.Data_Display LIKE '%' + @SearchText + '%'

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_QESession_SearchAndReplace') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_QESession_SearchAndReplace'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_QESession_SearchAndReplace TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_QESession_SearchAndReplace TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_QESession_SearchAndReplace TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_QESession_SearchAndReplace TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_QESession_SearchAndReplace TO [Dev - JNCC SQL]
END

GO

