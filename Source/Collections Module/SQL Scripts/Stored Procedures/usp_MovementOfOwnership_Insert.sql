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
    $Revision: 6 $
    $Date: 5/01/06 9:49 $
    $Author: Johnvanbreda $

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