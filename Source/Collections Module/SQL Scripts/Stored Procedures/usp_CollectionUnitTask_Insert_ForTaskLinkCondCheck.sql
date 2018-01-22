/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_CollectionUnitTask_Insert_ForTaskLinkCondCheck]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_CollectionUnitTask_Insert_ForTaskLinkCondCheck]
GO
/*===========================================================================*\
  Description:	When a Task is being linked to a Condition Check, all of the
		Condition Check's Collection Units need to be linked to the
		Task. This is done by duplicating the records from the
		Collection_Unit_Check table to the Collection_Unit_Task table
		that aren't already there.

  Parameters:	@ConservationTaskKey 
		@ConservationCheckKey

  Created:	March 2004

  Last revision information:
    $Revision: 1 $
    $Date: 15/03/04 17:20 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_CollectionUnitTask_Insert_ForTaskLinkCondCheck]
	@ConservationTaskKey char(16),
	@ConservationCheckKey char(16),
	@SessionID char(16)	
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	DECLARE	@CollectionUnitKey char(16),
		@CollectionUnitTaskKey char(16)

	DECLARE curCollectionUnitCheck CURSOR LOCAL STATIC FOR
		SELECT 		T1.Collection_Unit_Key
		FROM		Collection_Unit_Check AS T1
		LEFT JOIN	Collection_Unit_Task AS T2 ON T1.Collection_Unit_Key = T2.Collection_Unit_Key
							   AND T2.Conservation_Task_Key = @ConservationTaskKey
		WHERE		T1.Conservation_Check_Key = @ConservationCheckKey
		AND		T2.Collection_Unit_Key IS NULL
	
	/*-------------------------------------------------------------*\
	  Wrap everything in one transaction.
	\*-------------------------------------------------------------*/
	BEGIN TRANSACTION

		OPEN curCollectionUnitCheck

		FETCH NEXT
		FROM	curCollectionUnitCheck
		INTO	@CollectionUnitKey

		WHILE @@Fetch_Status = 0
		BEGIN
			EXECUTE spNextKey 'Collection_Unit_Task', @CollectionUnitTaskKey OUTPUT
	
			INSERT INTO Collection_Unit_Task
				(Collection_Unit_Task_Key,
				Conservation_Task_Key,
				Collection_Unit_Key,
				Entered_Session_ID)
			VALUES
				(@CollectionUnitTaskKey,
				@ConservationTaskKey,
				@CollectionUnitKey,
				@SessionID)
		
			IF @@Error <> 0 GOTO RollbackAndExit

			FETCH NEXT
			FROM	curCollectionUnitCheck
			INTO	@CollectionUnitKey
		END

		CLOSE curCollectionUnitCheck
		DEALLOCATE curCollectionUnitCheck
	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit:
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_CollectionUnitTask_Insert_ForTaskLinkCondCheck') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_CollectionUnitTask_Insert_ForTaskLinkCondCheck'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitTask_Insert_ForTaskLinkCondCheck TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_CollectionUnitTask_Insert_ForTaskLinkCondCheck TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_CollectionUnitTask_Insert_ForTaskLinkCondCheck TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_CollectionUnitTask_Insert_ForTaskLinkCondCheck TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitTask_Insert_ForTaskLinkCondCheck TO [Dev - JNCC SQL]
END

GO