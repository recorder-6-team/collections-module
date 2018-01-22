If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_QESession_Delete]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_QESession_Delete]
GO
    
/*===========================================================================*\
  Description:	

  Parameters:	

  Created:	August 2003

  Last revision information:
    $Revision: 7 $
    $Date: 5/07/16 9:28 $
    $Author: Christopherknight $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_QESession_Delete]
  @QESessionKey as int,
  @Timestamp as timestamp,
  @SessionID as char(16)
AS

	SET NOCOUNT ON

	SET Xact_abort ON
	BEGIN TRAN

		DELETE FROM QE_Data_Item 
		WHERE QE_Data_Row_key IN 
		(SELECT QE_Data_Row_Key FROM QE_Data_Row 
		WHERE QE_Session_Key = @QESessionKey)

		IF @@Error <> 0 GOTO RollbackAndExit 

		DELETE FROM QE_Data_Row
		WHERE QE_Session_Key = @QESessionKey
	
		IF @@Error <> 0 GOTO RollbackAndExit 

		DELETE FROM QE_Session
		WHERE QE_Session_Key = @QESessionKey
			AND ([Timestamp] = @Timestamp
				OR Changed_Session_ID = @SessionID
				OR (Changed_Session_ID IS NULL
					AND Entered_Session_ID = @SessionID))

		DECLARE @Error int
		DECLARE @RecordsAffected int

		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM QE_Session WHERE QE_Session_Key = @QESessionKey)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

	COMMIT TRAN
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_QESession_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_QESession_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_QESession_Delete TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_QESession_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_QESession_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_QESession_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_QESession_Delete TO [Dev - JNCC SQL]
END

GO
