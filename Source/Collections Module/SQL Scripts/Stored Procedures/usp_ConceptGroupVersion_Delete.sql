/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptGroupVersion_Delete]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptGroupVersion_Delete]
GO

/*===========================================================================*\
  Description:	Deletes a Concept_Group_Version record.

  Parameters:	@Key	Concept_Group_Version key
		@Timestamp,
		@RecordsAffected

  Created:	November 2003

  Last revision information:
    $Revision: 9 $
    $Date: 2/02/09 17:46 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptGroupVersion_Delete]
	@Key char(16),
	@Timestamp timestamp = NULL,
	@SyncTaxonDict bit = 0,
	@RecordsAffected int = 1 OUTPUT
AS

SET NOCOUNT ON

	BEGIN TRANSACTION
		/*============================================================*\
		  See if the user wants any associated taxon dictionary
		  records be deleted with the Concept Group Version.
		\*============================================================*/
		IF @SyncTaxonDict = 1 
		BEGIN
			/*--------------------------------------------------------*\
			  Check that the Taxon_List_Version table exists before
			  attempting any of this deletion.
			\*--------------------------------------------------------*/
			IF EXISTS (SELECT *
		   			FROM   SysObjects 
					WHERE  Id = Object_Id(N'[dbo].[Taxon_List_Version]')
					AND 	  Type = 'U')
			BEGIN
				DECLARE		@Taxon_List_Version_Key char(16)

				SELECT		@Taxon_List_Version_Key = Taxon_List_Version_Key
				FROM		Taxon_Dictionary_Concept_Group_Version_Mapping
				WHERE		Concept_Group_Version_Key = @Key

				DELETE		Taxon_Dictionary_Concept_Group_Version_Mapping
				WHERE		Concept_Group_Version_Key = @Key

				/*-----------------------------------------------------------------*\
				  It is possible that this delete will fail. e.g. If the TLV record
				  is referred to in the Index_Taxon_Name or Taxon_List_Item tables, 
				  This will cause it to go to the RollbackAndExit method,
				  where the user can be asked if they want to replace the concept
				  with another (4.2.17.18)
				\*-----------------------------------------------------------------*/ 
				DELETE 		Taxon_List_Version
				WHERE		Taxon_List_Version_Key = @Taxon_List_Version_Key

				IF @@Error <> 0 GOTO RollbackAndExit
			END
		END

		DECLARE 
			@ConceptGroupKey CHAR(16),
			@Error INT

		SELECT @ConceptGroupKey=Concept_Group_Key
		FROM 	Concept_Group_Version
		WHERE	Concept_Group_Version_Key = @Key

		DELETE 
		FROM 	Concept_Group_Version
		WHERE	Concept_Group_Version_Key = @Key
		AND		([Timestamp] = @Timestamp	OR (@Timestamp IS NULL))

		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Concept_Group_Version WHERE Concept_Group_Version_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

		IF @RecordsAffected>0 
		BEGIN
			DECLARE @CurrentConceptKey CHAR(16)
			-- Update all the concepts to check if they are current
			DECLARE curConcepts CURSOR LOCAL FAST_FORWARD FOR
				SELECT	C.Concept_Key
				FROM	Concept AS C
				WHERE	Concept_Group_Key = @ConceptGroupKey
		
			OPEN curConcepts
		
			FETCH NEXT
			FROM	curConcepts
			INTO	@CurrentConceptKey
		
			WHILE @@Fetch_Status = 0
			BEGIN
				EXEC usp_Concept_UpdateIsCurrent @CurrentConceptKey, @ConceptGroupKey
				IF @@Error <> 0 GOTO RollbackAndExit
		
				FETCH NEXT
				FROM	curConcepts
				INTO	@CurrentConceptKey
			END
			CLOSE	curConcepts
			DEALLOCATE curConcepts
		END
	
	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptGroupVersion_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptGroupVersion_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptGroupVersion_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptGroupVersion_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptGroupVersion_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptGroupVersion_Delete TO [Dev - JNCC SQL]
END
GO