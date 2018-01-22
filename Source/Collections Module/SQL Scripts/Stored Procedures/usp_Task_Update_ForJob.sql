/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Task_Update_ForJob]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Task_Update_ForJob]
GO

/*===========================================================================*\
  Description:	Updates the record in the Conservation_Task table with the 
		key of the parent Conservation_Job_Key.

  Parameters:	@ParentKey 	The key of the parent Job node.
		@ChildKey	The key of the added (child) Task node .

  Created:	September 2003

  Last revision information:
    $Revision: 4 $
    $Date: 8/04/04 18:16 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Task_Update_ForJob] 
	@ParentKey CHAR(16),
	@ChildKey CHAR(16),
	@JoinKey char(16) OUTPUT
AS
	SET NOCOUNT ON

	-- Join key is Conservation_Task_Key, same as Child Key in this case.
	SET @JoinKey = @ChildKey

	BEGIN TRANSACTION

		UPDATE	Conservation_Task
		SET	Conservation_Job_Key = @ParentKey
		WHERE	Conservation_Task_Key = @ChildKey
	
		IF @@Error <> 0 GOTO RollbackAndExit
	
	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Task_Update_ForJob') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Task_Update_ForJob'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Task_Update_ForJob TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Task_Update_ForJob TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Task_Update_ForJob TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Task_Update_ForJob TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Task_Update_ForJob TO [Dev - JNCC SQL]
END
GO