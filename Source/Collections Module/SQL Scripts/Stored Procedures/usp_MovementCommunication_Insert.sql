/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_MovementCommunication_Insert]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_MovementCommunication_Insert]
GO

/*===========================================================================*\
  Description:	Inserts a record into the Movement_Communication table

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
		@SessionID

  Created:	September 2003

  Last revision information:
    $Revision: 3 $
    $Date: 8/12/03 11:15 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_MovementCommunication_Insert]
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
	@SessionID char(16) 
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	EXECUTE spNextKey 'Movement_Communication', @Key OUTPUT

	BEGIN TRANSACTION

		/*-------------------------------------------------------------*\
		  Insert in Movement.
		\*-------------------------------------------------------------*/
		INSERT INTO Movement_Communication (
			Movement_Communication_Key, Movement_Key, Sender_Name_Key, 
			Receiver_Name_Key, Communication_Type_Concept_Key, Vague_Date_Start, 
			Vague_Date_End, Vague_Date_Type, Content, File_Ref, 
			Entered_Session_ID
			
		) VALUES (
			@Key, @MovementKey, @SenderNameKey, 
			@ReceiverNameKey, @CommunicationTypeConceptKey, @VagueDateStart, 
			@VagueDateEnd, @VagueDateType, @Content, @FileRef, 
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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_MovementCommunication_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_MovementCommunication_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_MovementCommunication_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_MovementCommunication_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_MovementCommunication_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_MovementCommunication_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_MovementCommunication_Insert TO [Dev - JNCC SQL]
END
GO
			