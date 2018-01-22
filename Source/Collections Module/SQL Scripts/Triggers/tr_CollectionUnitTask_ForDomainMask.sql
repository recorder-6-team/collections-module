/*===========================================================================*\
  Drop trigger before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[tr_CollectionUnitTask_ForDomainMask]') 
	   AND    Type = 'TR')
    DROP TRIGGER [dbo].[tr_CollectionUnitTask_ForDomainMask]
GO

/*===========================================================================*\
  Description:	Update Domain_Mask of all jobs linked to changed records in 
		Collection_Unit_Task.

  Type:		AFTER INSERT, UPDATE and DELETE

  Created:	September 2003

  Last revision information:
    $Revision: 2 $
    $Date: 21/01/04 9:04 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE TRIGGER [dbo].[tr_CollectionUnitTask_ForDomainMask]
ON [dbo].[Collection_Unit_Task]
AFTER INSERT, UPDATE, DELETE
AS 
	DECLARE	@TaskKey char(16)

	/*-------------------------------------------------------------*\
	  Only the task key is needed to get to the jobs needing 
	  updating. But we need to through all keys, hence the cursor.
	\*-------------------------------------------------------------*/
	DECLARE curTasks CURSOR LOCAL FAST_FORWARD FOR
		SELECT	DISTINCT Conservation_Task_Key
		FROM	Deleted
		WHERE	Conservation_Task_Key NOT IN (SELECT Conservation_Task_Key FROM Inserted)
		UNION
		SELECT	DISTINCT Conservation_Task_Key
		FROM 	Inserted	
	
	/*-------------------------------------------------------------*\
	  Go and do it now.
	\*-------------------------------------------------------------*/
	OPEN curTasks
	FETCH NEXT FROM curTasks INTO @TaskKey
	WHILE @@Fetch_Status = 0
	BEGIN
		EXECUTE	usp_ConservationJob_Update_DomainMask NULL, @TaskKey
	
		FETCH NEXT FROM curTasks INTO @TaskKey
	END
	
	-- Cleanup
	CLOSE curTasks
	DEALLOCATE curTasks
GO
