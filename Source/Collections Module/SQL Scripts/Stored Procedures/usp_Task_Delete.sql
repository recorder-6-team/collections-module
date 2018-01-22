/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Task_Delete]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Task_Delete]
GO

/*===========================================================================*\
  Description:	Delete a record from the Conservation_Task table.

  Parameters:	@Key
		@Timestamp

  Created:	December 2003

  Last revision information:
    $Revision: 7 $
    $Date: 3/02/09 11:00 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Task_Delete]
	@Key char(16),
	@Timestamp timestamp
AS
	SET NOCOUNT OFF

	BEGIN TRANSACTION

		-- Delete all links between Collection units and this task.
		DELETE	Collection_Unit_Task
		WHERE	Conservation_Task_Key = @Key

		-- Delete all sources
		EXEC usp_References_Delete_ForSources 'Conservation_Task', @Key

		-- Delete record from Conservation_Task table.
		DELETE	Conservation_Task
		WHERE	Conservation_Task_Key = @Key
		AND		[Timestamp] = @Timestamp

		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount
	
		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Conservation_Task WHERE Conservation_Task_Key = @Key)
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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Task_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Task_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Task_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Task_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Task_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Task_Delete TO [Dev - JNCC SQL]
END
GO