/*===========================================================================*\
  Drop trigger before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT Name 
	   FROM   SysObjects 
	   WHERE  Name = N'tr_MovementCollectionUnit_DeleteCleanup' 
	   AND 	  Type = 'TR')
    DROP TRIGGER [dbo].[tr_MovementCollectionUnit_DeleteCleanup]
GO

/*===========================================================================*\
  Description:	The purpose of this trigger is to keep the Movement_Of_Material_Exclusion 
		and Movement_Of_Ownership_Exclusion tables consistent with the 
		Movement_Collection_Unit table. If a record is removed from the 
		latter, the corresponding record in the former tables must be 
		removed (if there is one).

  Created:	July 2003

  Last revision information:
    $Revision: 2 $
    $Date: 21/01/04 9:04 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE TRIGGER [dbo].[tr_MovementCollectionUnit_DeleteCleanup] ON [dbo].[Movement_Collection_Unit] 
FOR DELETE 
AS

	DELETE FROM Movement_Of_Material_Exclusion
	WHERE Movement_Of_Material_Exclusion_Key IN
		(SELECT MME.Movement_Of_Material_Exclusion_Key
		FROM Movement_Of_Material_Exclusion MME
		INNER JOIN Movement_Of_Material MM ON MME.Movement_Of_Material_Key = MM.Movement_Of_Material_Key
		INNER JOIN Movement_Direction MD ON MM.Movement_Direction_Key = MD.Movement_Direction_Key
		INNER JOIN Deleted D ON MD.Movement_Direction_Key = D.Movement_Direction_Key
		WHERE D.Collection_Unit_Key=MME.Collection_Unit_Key)
	
	-- THis section keeps the Movement_Of_Ownership_Exclusion up to date
	DELETE FROM Movement_Of_Ownership_Exclusion
	WHERE Movement_Of_Ownership_Exclusion_Key IN
		(SELECT MME.Movement_Of_Ownership_Exclusion_Key
		FROM Movement_Of_Ownership_Exclusion MME
		INNER JOIN Movement_Of_Ownership MO ON MME.Movement_Of_Ownership_Key = MO.Movement_Of_Ownership_Key
		INNER JOIN Movement_Direction MD ON MO.Movement_Direction_Key = MD.Movement_Direction_Key
		INNER JOIN Deleted D ON MD.Movement_Direction_Key = D.Movement_Direction_Key
		WHERE D.Collection_Unit_Key=MME.Collection_Unit_Key)
	
GO