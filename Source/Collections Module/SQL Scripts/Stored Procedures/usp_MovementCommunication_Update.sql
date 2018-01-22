/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_MovementCommunication_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_MovementCommunication_Update]
GO

/*===========================================================================*\
  Description:	Updates a record in the Movement_Communication table.

  Parameters:	@Key
		@MovementKey
		@SenderNameKey
		@ReceiverNameKey
		@CommunicationTypeConceptKey
		@VagueDateStart
		@VagueDateEnd
		@VagueDateType
		@Content
		@FileRef
		@SessionID,
		@Timestamp

  Created:	September 2003

  Last revision information:
    $Revision: 5 $
    $Date: 3/02/09 9:47 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_MovementCommunication_Update]
	@Key char(16) OUTPUT,
	@MovementKey char(16),
	@SenderNameKey char(16),
	@ReceiverNameKey char(16),
	@CommunicationTypeConceptKey char(16),
	@VagueDateStart int, 
	@VagueDateEnd int, 
	@VagueDateType varchar(2),
	@Content text,
	@FileRef varchar(20),
	@SessionID char(16), 
	@Timestamp timestamp
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION
		
		UPDATE 	Movement_Communication
		SET 	Movement_Key = @MovementKey,
			Sender_Name_Key = @SenderNameKey,
			Receiver_Name_Key = @ReceiverNameKey,
			Communication_Type_Concept_Key = @CommunicationTypeConceptKey,
			Vague_Date_Start = @VagueDateStart,
			Vague_Date_End = @VagueDateEnd,
			Vague_Date_Type = @VagueDateType,
			Content = @Content,
			File_Ref = @FileRef,
			Changed_Session_ID = @SessionID

		WHERE	Movement_Communication_Key = @Key
		AND		[Timestamp] = @Timestamp

		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Movement_Communication WHERE Movement_Communication_Key = @Key)
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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_MovementCommunication_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_MovementCommunication_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_MovementCommunication_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_MovementCommunication_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_MovementCommunication_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_MovementCommunication_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_MovementCommunication_Update TO [Dev - JNCC SQL]
END
GO