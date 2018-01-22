/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Task_Delete_ForJob]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Task_Delete_ForJob]
GO

/*===========================================================================*\
  Description:	Remove the link between a task and a job.

  Parameters:	@Key

  Created:	April 2004

  Last revision information:
    $Revision: 1 $
    $Date: 14/04/04 11:20 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Task_Delete_ForJob]
	@Key char(16)
AS
	SET NOCOUNT OFF

	BEGIN TRANSACTION

		UPDATE	Conservation_Task
		SET 	Conservation_Job_Key = NULL
		WHERE	Conservation_Task_Key = @Key
	
		IF @@Error <> 0 GOTO RollbackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Task_Delete_ForJob') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Task_Delete_ForJob'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Task_Delete_ForJob TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Task_Delete_ForJob TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Task_Delete_ForJob TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Task_Delete_ForJob TO [Dev - JNCC SQL]
END
GO