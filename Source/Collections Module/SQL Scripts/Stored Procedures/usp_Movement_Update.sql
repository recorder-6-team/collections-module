/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Movement_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Movement_Update]
GO

/*===========================================================================*\
  Description:	Updates a record in the Movement table.

  Parameters:	@Key
		@MovementType
		@OtherPartyNameKey
		@StaffResponsibleNameKey
		@ContactNameKey
		@ExpVagueDateStart
		@ExpVagueDateEnd
		@ExpVagueDateType
		@LoanVagueDateStart
		@LoanVagueDateEnd
		@LoanComplete
		@ReturnVagueDateStart
		@ReturnVagueDateEnd
		@ReturnVagueDateType
		@ReturnComplete
		@Number
		@Notes
		@SessionID
		@Timestamp 

  Created:	September 2003

  Last revision information:
    $Revision: 9 $
    $Date: 8/04/14 11:37 $
    $Author: Brynhorsfieldschonhut $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Movement_Update]
	@Key char(16) OUTPUT,
	@MovementType tinyint, 
	@StaffResponsibleNameKey char(16),
	@OtherPartyNameKey char(16),
	@ContactNameKey char(16),
	@VagueDateStart int, 
	@VagueDateEnd int, 
	@VagueDateType varchar(2),
	@LoanVagueDateStart int = null,
	@LoanVagueDateEnd int = null,
	@LoanVagueDateType varchar(2) = null,
	@LoanComplete bit = null,
	@ReturnVagueDateStart int = null,
	@ReturnVagueDateEnd int = null,
	@ReturnVagueDateType varchar(2) = null,
	@ReturnComplete bit = null,
	@Number varchar(30),
	@Notes text,
	@SessionID char(16), 
	@Timestamp timestamp,
	@RecordsAffected int OUTPUT

AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	DECLARE @Error int
	DECLARE @InboundMovementKey char(16), @OutboundMovementKey char(16)

	SET @InboundMovementKey = (SELECT MOM.Movement_Of_Material_Key
								FROM Movement_Of_Material MOM
								INNER JOIN Movement_Direction MD
								ON MOM.Movement_Direction_Key = MD.Movement_Direction_Key
								WHERE MD.Movement_Key = @Key AND MD.OutBound = 0)
	SET @OutboundMovementKey = (SELECT MOM.Movement_Of_Material_Key
								FROM Movement_Of_Material MOM
								INNER JOIN Movement_Direction MD
								ON MOM.Movement_Direction_Key = MD.Movement_Direction_Key
								WHERE MD.Movement_Key = @Key AND MD.OutBound = 1)
 

	BEGIN TRANSACTION
		
		UPDATE 	Movement
		SET 	Movement_Type = @MovementType, 
			Staff_Responsible_Name_Key = @StaffResponsibleNameKey,			
			Other_Party_Name_Key = @OtherPartyNameKey,
			Contact_Name_Key = @ContactNameKey,
			Exp_Vague_Date_Start = @VagueDateStart, 
			Exp_Vague_Date_End = @VagueDateEnd, 
			Exp_Vague_Date_Type = @VagueDateType,
			Number = @Number, 
			Notes = @Notes, 
			Changed_Session_ID = @SessionID

		WHERE	Movement_Key = @Key
		AND		[Timestamp] = @Timestamp

		SELECT	@RecordsAffected = @@RowCount,
			@Error = @@Error

		IF @Error <> 0 GOTO RollbackAndExit

		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Movement WHERE Movement_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

		IF @MovementType = 2
		BEGIN
			-- Loan In
			IF @LoanVagueDateStart IS NOT NULL AND @LoanVagueDateEnd IS NOT NULL AND @LoanVagueDateType IS NOT NULL AND @LoanComplete IS NOT NULL
			BEGIN
				UPDATE Movement_Of_Material
				SET Vague_Date_Start = @LoanVagueDateStart,
					Vague_Date_End = @LoanVagueDateEnd,
					Vague_Date_Type = @LoanVagueDateType,
					Completed = @LoanComplete,
					Changed_Session_ID = @SessionID
				WHERE Movement_Of_Material_Key = @InboundMovementKey

				SELECT	@RecordsAffected = @RecordsAffected + @@RowCount,
					 @Error = @@Error

				IF @Error <> 0 GOTO RollbackAndExit

				IF @RecordsAffected = 0 AND EXISTS(SELECT 1 
											FROM Movement_Of_Material
										 WHERE Movement_Of_Material_Key = @InboundMovementKey)
				BEGIN
					RAISERROR('Record updated by another user', 16, 1)
					GOTO RollbackAndExit
				END
			END
			ELSE IF @LoanComplete IS NOT NULL
			BEGIN
				UPDATE Movement_Of_Material
				SET Completed = @LoanComplete,
					Changed_Session_ID = @SessionID
				WHERE Movement_Of_Material_Key = @InboundMovementKey

				SELECT	@RecordsAffected = @RecordsAffected + @@RowCount,
					 @Error = @@Error

				IF @Error <> 0 GOTO RollbackAndExit

				IF @RecordsAffected = 0 AND EXISTS(SELECT 1 
											FROM Movement_Of_Material
										 WHERE Movement_Of_Material_Key = @OutboundMovementKey)
				BEGIN
					RAISERROR('Record updated by another user', 16, 1)
					GOTO RollbackAndExit
				END	
			END	

			IF @ReturnVagueDateStart IS NOT NULL AND @ReturnVagueDateEnd IS NOT NULL AND @ReturnVagueDateType IS NOT NULL AND @ReturnComplete IS NOT NULL
			BEGIN
				UPDATE Movement_Of_Material
				SET Vague_Date_Start = @ReturnVagueDateStart,
					Vague_Date_End = @ReturnVagueDateEnd,
					Vague_Date_Type = @ReturnVagueDateType,
					Completed = @ReturnComplete,
					Changed_Session_ID = @SessionID
				WHERE Movement_Of_Material_Key = @OutboundMovementKey

				SELECT	@RecordsAffected = @RecordsAffected + @@RowCount,
					 @Error = @@Error

				IF @Error <> 0 GOTO RollbackAndExit

				IF @RecordsAffected = 0 AND EXISTS(SELECT 1 
											FROM Movement_Of_Material
										 WHERE Movement_Of_Material_Key = @OutboundMovementKey)
				BEGIN
					RAISERROR('Record updated by another user', 16, 1)
					GOTO RollbackAndExit
				END
			END
			ELSE IF @ReturnComplete IS NOT NULL
			BEGIN
				UPDATE Movement_Of_Material
				SET Completed = @ReturnComplete,
					Changed_Session_ID = @SessionID
				WHERE Movement_Of_Material_Key = @OutboundMovementKey

				SELECT	@RecordsAffected = @RecordsAffected + @@RowCount,
					 @Error = @@Error

				IF @Error <> 0 GOTO RollbackAndExit

				IF @RecordsAffected = 0 AND EXISTS(SELECT 1 
											FROM Movement_Of_Material
										 WHERE Movement_Of_Material_Key = @OutboundMovementKey)
				BEGIN
					RAISERROR('Record updated by another user', 16, 1)
					GOTO RollbackAndExit
				END				
			END			
		END

		IF @MovementType = 3
		BEGIN
			-- Loan Out
			IF @ReturnVagueDateStart IS NOT NULL AND @ReturnVagueDateEnd IS NOT NULL AND @ReturnVagueDateType IS NOT NULL AND @ReturnComplete IS NOT NULL
			BEGIN
				UPDATE Movement_Of_Material
				SET Vague_Date_Start = @ReturnVagueDateStart,
					Vague_Date_End = @ReturnVagueDateEnd,
					Vague_Date_Type = @ReturnVagueDateType,
					Completed = @ReturnComplete,
					Changed_Session_ID = @SessionID
				WHERE Movement_Of_Material_Key = @InboundMovementKey

				SELECT	@RecordsAffected = @RecordsAffected + @@RowCount,
					 @Error = @@Error

				IF @Error <> 0 GOTO RollbackAndExit

				IF @RecordsAffected = 0 AND EXISTS(SELECT 1 
											FROM Movement_Of_Material
										 WHERE Movement_Of_Material_Key = @InboundMovementKey)
				BEGIN
					RAISERROR('Record updated by another user', 16, 1)
					GOTO RollbackAndExit
				END
			END
			ELSE IF @ReturnComplete IS NOT NULL
			BEGIN
				UPDATE Movement_Of_Material
				SET Completed = @ReturnComplete,
					Changed_Session_ID = @SessionID
				WHERE Movement_Of_Material_Key = @InboundMovementKey

				SELECT	@RecordsAffected = @RecordsAffected + @@RowCount,
					 @Error = @@Error

				IF @Error <> 0 GOTO RollbackAndExit

				IF @RecordsAffected = 0 AND EXISTS(SELECT 1 
											FROM Movement_Of_Material
										 WHERE Movement_Of_Material_Key = @OutboundMovementKey)
				BEGIN
					RAISERROR('Record updated by another user', 16, 1)
					GOTO RollbackAndExit
				END				
			END	

			IF @LoanVagueDateStart IS NOT NULL AND @LoanVagueDateEnd IS NOT NULL AND @LoanVagueDateType IS NOT NULL AND @LoanComplete IS NOT NULL
			BEGIN
				UPDATE Movement_Of_Material
				SET Vague_Date_Start = @LoanVagueDateStart,
					Vague_Date_End = @LoanVagueDateEnd,
					Vague_Date_Type = @LoanVagueDateType,
					Completed = @LoanComplete,
					Changed_Session_ID = @SessionID
				WHERE Movement_Of_Material_Key = @OutboundMovementKey

				SELECT	@RecordsAffected = @RecordsAffected + @@RowCount,
					 @Error = @@Error

				IF @Error <> 0 GOTO RollbackAndExit

				IF @RecordsAffected = 0 AND EXISTS(SELECT 1 
											FROM Movement_Of_Material
										 WHERE Movement_Of_Material_Key = @OutboundMovementKey)
				BEGIN
					RAISERROR('Record updated by another user', 16, 1)
					GOTO RollbackAndExit
				END	
			END
			ELSE IF @LoanComplete IS NOT NULL
			BEGIN
				UPDATE Movement_Of_Material
				SET Completed = @LoanComplete,
					Changed_Session_ID = @SessionID
				WHERE Movement_Of_Material_Key = @OutboundMovementKey

				SELECT	@RecordsAffected = @RecordsAffected + @@RowCount,
					 @Error = @@Error

				IF @Error <> 0 GOTO RollbackAndExit

				IF @RecordsAffected = 0 AND EXISTS(SELECT 1 
											FROM Movement_Of_Material
										 WHERE Movement_Of_Material_Key = @OutboundMovementKey)
				BEGIN
					RAISERROR('Record updated by another user', 16, 1)
					GOTO RollbackAndExit
				END	
			END	
		END

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Movement_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Movement_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Movement_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Movement_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Movement_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Movement_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Movement_Update TO [Dev - JNCC SQL]
END
GO