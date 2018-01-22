If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_ConditionChecks_Select_ForMovement]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_ConditionChecks_Select_ForMovement]
GO

/*===========================================================================*\
  Description:	Returns Condition Checks data to the CollectionsBrowser for a given Movement.

  Parameters:	
	@ParentKey 	When specified, only the records associated with the parent key are returned
	@SortOrderIndex	Index determining Sort Order

  Created:	August 2003

  Last revision information:
    $Revision: 7 $
    $Date: 3/08/11 15:07 $
    $Author: Simonlewis $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_ConditionChecks_Select_ForMovement] 
	@ParentKey CHAR(16),
	@SortOrderIndex TINYINT
AS

SET NOCOUNT ON

IF @SortOrderIndex = 0
	SELECT 		CC.Conservation_Check_Key AS Item_Key, CC.Display_Caption, MCC.Movement_Conservation_Check_Key AS Join_Key
	FROM 		Movement_Conservation_Check MCC
	INNER JOIN	Conservation_Check CC ON MCC.Conservation_Check_Key = CC.Conservation_Check_Key AND MCC.Movement_Key = @ParentKey
	INNER JOIN 	Concept C ON CC.Type_Concept_Key = C.Concept_Key
	INNER JOIN 	Term T ON C.Term_Key = T.Term_Key
	ORDER BY 	CC.Vague_Date_Start DESC, CC.Vague_Date_End DESC, T.Plaintext, CC.Ref_Number
ELSE 
IF @SortOrderIndex = 1
	SELECT 		CC.Conservation_Check_Key AS Item_Key, CC.Display_Caption, MCC.Movement_Conservation_Check_Key AS Join_Key
	FROM 		Movement_Conservation_Check MCC
	INNER JOIN	Conservation_Check CC ON MCC.Conservation_Check_Key = CC.Conservation_Check_Key AND MCC.Movement_Key = @ParentKey
	ORDER BY 	CC.Ref_Number
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConditionChecks_Select_ForMovement') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConditionChecks_Select_ForMovement'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConditionChecks_Select_ForMovement TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConditionChecks_Select_ForMovement TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConditionChecks_Select_ForMovement TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ConditionChecks_Select_ForMovement TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConditionChecks_Select_ForMovement TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConditionChecks_Select_ForMovement TO [Dev - JNCC SQL]
END

GO