/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_CollectionUnit_Update_ForConditionCheck]')
	   AND    Type = 'P')
    DROP PROCEDURE [dbo].[usp_CollectionUnit_Update_ForConditionCheck]
GO

/*===========================================================================*\
  Description:	Inserts a record into the Conservation_Unit_Check join table 
		so that there is a relationship between the Condition Check 
		and Collection Unit tables.

  Parameters:	@ParentKey 	The key of the top level (parent) Condition Check node.
		@ChildKey	The key of the added (child) Collection Unit node. 
		@SessionID
		@JoinKey	Key of new record in Collection_Unit_Check

  Created:	September 2003

  Last revision information:
    $Revision: 6 $
    $Date: 8/04/04 18:15 $
    $Author: Ericsalmon $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_CollectionUnit_Update_ForConditionCheck] 
	@ParentKey char(16),
	@ChildKey char(16),
	@SessionID char(16),
	@JoinKey char(16) OUTPUT
AS

	SET NOCOUNT ON

	DECLARE @TaskKey char(16)
	EXECUTE spNextKey 'Collection_Unit_Check', @JoinKey OUTPUT

	BEGIN TRANSACTION

		INSERT INTO Collection_Unit_Check (
			Collection_Unit_Check_Key, Conservation_Check_Key, Collection_Unit_Key, Entered_Session_ID
		) VALUES (
			@JoinKey, @ParentKey, @ChildKey, @SessionID
		)

		IF @@Error <> 0 GOTO RollbackAndExit

		/*---------------------------------------------------------------------------------*\
		  If the Conservation Check has one task linked to it, we will want to duplicate 
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
		BEGIN
			EXEC usp_CollectionUnitTask_Insert_ForTaskLinkCondCheck @TaskKey, @ParentKey, @SessionID
		END

		IF @@Error <> 0 GOTO RollbackAndExit
	
	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_CollectionUnit_Update_ForConditionCheck') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_CollectionUnit_Update_ForConditionCheck'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_CollectionUnit_Update_ForConditionCheck TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_CollectionUnit_Update_ForConditionCheck TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_CollectionUnit_Update_ForConditionCheck TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_CollectionUnit_Update_ForConditionCheck TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_CollectionUnit_Update_ForConditionCheck TO [Dev - JNCC SQL]
END
GO