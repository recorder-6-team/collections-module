/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_MovementOfOwnership_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_MovementOfOwnership_Update]
GO
/*===========================================================================*\
  Description: 	Updates a record in the Movement_Of_Ownership table.
  Parameters:	@Key
		@MovementDirectionKey
		@ContactNameKey
		@VagueDateStart
		@VagueDateEnd
		@VagueDateType
		@Notes
		@Completed
		@SessionID
		@Timestamp 

  Created:	October 2003

  Last revision information:
    $Revision: 6 $
    $Date: 3/02/09 9:52 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_MovementOfOwnership_Update]
	@Key char(16) OUTPUT,
	@ParentKey char(16),
	@ContactNameKey char(16),
	@VagueDateStart int,
	@VagueDateEnd int,
	@VagueDateType varchar(2),
	@Notes text,
	@Completed bit,
	@SessionID char(16),
	@Outbound bit,
	@Timestamp timestamp
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF
	
	DECLARE @MovementDirectionKey varchar(16)

	/*-------------------------------------------------------------*\
	  Need to get the MovementDirectionKey.
	\*-------------------------------------------------------------*/
	SELECT	@MovementDirectionKey = Movement_Direction_Key
	FROM	Movement_Direction AS MD
	WHERE 	MD.Movement_Key = @ParentKey
	AND 	MD.Outbound = @Outbound

	BEGIN TRANSACTION

		UPDATE 	Movement_Of_Ownership
		SET 	Movement_Direction_Key = @MovementDirectionKey,
			Contact_Name_Key = @ContactNameKey,
			Vague_Date_Start = @VagueDateStart,
			Vague_Date_End = @VagueDateEnd,
			Vague_Date_Type = @VagueDateType,
			Notes = @Notes,
			Completed = @Completed,
			Changed_Session_ID = @SessionID
		WHERE	Movement_Of_Ownership_Key = @Key 
		AND		[Timestamp] = @Timestamp

		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Movement_Of_Ownership WHERE Movement_Of_Ownership_Key = @Key)
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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_MovementOfOwnership_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_MovementOfOwnership_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_MovementOfOwnership_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_MovementOfOwnership_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_MovementOfOwnership_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_MovementOfOwnership_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_MovementOfOwnership_Update TO [Dev - JNCC SQL]
END
GO