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
    $Revision: 10 $
    $Date: 25/05/11 9:24 $
    $Author: Jamesbichard $

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

		DELETE Concept_Group_Quality_Check
		WHERE Concept_Group_Key = @Key
	
		DELETE 
		FROM 		Concept_Group
		WHERE		Concept_Group_Key = @Key
		AND			([Timestamp] = @Timestamp	OR (@Timestamp IS NULL))

		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Concept_Group WHERE Concept_Group_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

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