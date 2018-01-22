/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Job_Update_ForTask]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Job_Update_ForTask]
GO

/*===========================================================================*\
  Description:	Updates the record in the Conservation_Task table with the 
		key of the child Conservation_Job_Key.

  Parameters:	@ParentKey 	The key of the parent Task node.
		@ChildKey	The key of the added (child) Job node.

  Created:	September 2003

  Last revision information:
    $Revision: 4 $
    $Date: 8/04/04 18:15 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Job_Update_ForTask] 
	@ParentKey char(16),
	@ChildKey char(16),
	@JoinKey char(16) OUTPUT
AS

	SET NOCOUNT OFF

	SET @JoinKey = @ParentKey

	BEGIN TRANSACTION

		UPDATE	Conservation_Task
		SET	Conservation_Job_Key = @ChildKey
		WHERE	Conservation_Task_Key = @ParentKey
	
		IF @@Error <> 0 GOTO RollbackAndExit
	
	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Job_Update_ForTask') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Job_Update_ForTask'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Job_Update_ForTask TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Job_Update_ForTask TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Job_Update_ForTask TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Job_Update_ForTask TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Job_Update_ForTask TO [Dev - JNCC SQL]
END
GO