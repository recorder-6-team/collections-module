/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Task_Update_ForConditionCheck]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Task_Update_ForConditionCheck]
GO

/*===========================================================================*\
  Description:	Updates the record in the Conservation_Task table with the 
		key of the parent Conservation_Check_Key.

  Parameters:	@ParentKey 	The key of the parent ConservationCheck node.
		@ChildKey	The key of the added (child) Task node. 

  Created:	September 2003

  Last revision information:
    $Revision: 6 $
    $Date: 8/04/04 18:16 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Task_Update_ForConditionCheck] 
	@ParentKey CHAR(16),
	@ChildKey CHAR(16),
	@SessionID CHAR(16),
	@JoinKey char(16) OUTPUT
AS
	SET NOCOUNT ON

	-- Join key is Conservation_Task_Key, same as Child Key in this case.
	SET @JoinKey = @ChildKey

	DECLARE @TaskKey char(16)

	BEGIN TRANSACTION

		UPDATE	Conservation_Task
		SET	Conservation_Check_Key = @ParentKey
		WHERE	Conservation_Task_Key = @ChildKey
	
		IF @@Error <> 0 GOTO RollbackAndExit

		/*---------------------------------------------------------------------------------*\
		  If the Conservation Check has just one task linked to it, we will want to duplicate 
		  records from the Collection_Unit_Check table into the Collection_Unit_Table.
		  Here we are going to find out how many Tasks are linked to the Conservation_Check.
		  If it is just one, then we have stored this task key in @TaskKey and can run
		  usp_CollectionUnitTask_Insert_ForTaskLinkCondCheck which will duplicate the 
		  records for us.
		\*---------------------------------------------------------------------------------*/

		SELECT 	@TaskKey = Conservation_Task_Key
		FROM	Conservation_Task
		WHERE	Conservation_Check_Key = @ParentKey

		IF @@RowCount = 1
			EXEC usp_CollectionUnitTask_Insert_ForTaskLinkCondCheck @TaskKey, @ParentKey, @SessionID

		IF @@Error <> 0 GOTO RollbackAndExit
	
	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Task_Update_ForConditionCheck') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Task_Update_ForConditionCheck'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Task_Update_ForConditionCheck TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Task_Update_ForConditionCheck TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Task_Update_ForConditionCheck TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Task_Update_ForConditionCheck TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Task_Update_ForConditionCheck TO [Dev - JNCC SQL]
END
GO