/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Synonym_Delete]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Synonym_Delete]
GO

/*===========================================================================*\
  Description:	'Delete' a Synonym. This doesn't actually delete the concept
		that is the selected synonym, but gives the concept a new
		meaning key, so it is no longer a synonym.

  Parameters:	@Key
		@RecordsAffected  OUTPUT

  Created:	March 2004

  Last revision information:
    $Revision: 3 $
    $Date: 25/05/11 9:20 $
    $Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Synonym_Delete]
	@Key char(16),
	@Timestamp timestamp,
	@RecordsAffected int OUTPUT
AS
SET NOCOUNT OFF
	
	BEGIN TRANSACTION
		DECLARE 
			@OldMeaningKey char(16),
			@NewMeaningKey char(16),
			@HomonymKey char(16),
			@Error int

		SELECT @OldMeaningKey = Meaning_Key
		FROM Concept
		WHERE Concept_Key = @Key

		/*-------------------------------------------------------------*\
		  Create a new Meaning Key
		\*-------------------------------------------------------------*/
		EXEC spNextKey 'Meaning', @NewMeaningKey OUTPUT
		IF @@ERROR <> 0 GOTO RollbackAndExit

		INSERT INTO Meaning (
			Meaning_Key
		) VALUES (
			@NewMeaningKey
		)
		IF @@Error <> 0 GOTO RollbackAndExit

		/*--------------------------------------------------------------*\
			Copy over existing homonym pairs for new meaning key
		\*--------------------------------------------------------------*/
		EXEC dbo.usp_HomonymPair_CopyPairs @OldMeaningKey, @NewMeaningKey
		

		/*-------------------------------------------------------------*\
		  Update the concept record
		\*-------------------------------------------------------------*/
		UPDATE	Concept
		SET 	Meaning_Key = @NewMeaningKey,	
			List_Preferred = 1
		WHERE	Concept_Key = @Key
		AND	[Timestamp] = @Timestamp

		/*-------------------------------------------------------------*\
		  Get the number of records affected and see if any errors.
		\*-------------------------------------------------------------*/	
		SELECT	@RecordsAffected = @@RowCount,
			@Error = @@Error
		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Concept WHERE Concept_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit:
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_Synonym_Delete failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Synonym_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Synonym_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Synonym_Delete TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Synonym_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Synonym_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Synonym_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Synonym_Delete TO [Dev - JNCC SQL]
END
GO