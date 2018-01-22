If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Tasks_Select_ForConditionCheck]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Tasks_Select_ForConditionCheck]
GO

/*===========================================================================*\
  Description:	Returns Tasks associated with a specified Condition Check.

  Parameters:
	@ParentKey 	When specified, only the records associated with the parent key are returned
	@UserDomainMask	User's Domain Mask restricting which records may be returned
	@SessionID 	User's SessionID
	@SortOrderIndex	Index determining Sort Order

  Created:	August 2003

  Last revision information:
    $Revision: 8 $
    $Date: 8/04/04 18:16 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Tasks_Select_ForConditionCheck] 
	@UserDomainMask INT,
	@SessionID CHAR(16),
	@ParentKey CHAR(16),
	@SortOrderIndex TINYINT
AS

SET NOCOUNT ON

	SELECT 		CT.Conservation_Task_Key AS Item_Key, CT.Display_Caption, 
			CT.Conservation_Task_Key AS Join_Key  -- Conservation_Task is a "join table" in itself.

	FROM 		Conservation_Check CC
	INNER JOIN 	Conservation_Task CT
		ON CC.Conservation_Check_Key = CT.Conservation_Check_Key 
		AND CC.Conservation_Check_Key = @ParentKey
		AND ((CC.Domain_Mask & @UserDomainMask > 0) OR (CC.Entered_Session_ID = @SessionID)
			OR (CC.Changed_Session_ID = @SessionID) OR (CC.Domain_Mask = 0))
	
	ORDER BY 
		-- 0: CT.Set_Vague_Date_xxx, dbo.ufn_GetConservationStatus(Status)
		-- 1: dbo.ufn_GetConservationStatus(Status), CT.Set_Vague_Date_xxx
		CASE @SortOrderIndex WHEN 1 THEN dbo.ufn_GetConservationStatus(Status) ELSE NULL END,
		CT.Set_Vague_Date_Start DESC, CT.Set_Vague_Date_End DESC, CT.Set_Vague_Date_Type, 
		CASE @SortOrderIndex WHEN 0 THEN dbo.ufn_GetConservationStatus(Status) ELSE NULL END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Tasks_Select_ForConditionCheck') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Tasks_Select_ForConditionCheck'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Tasks_Select_ForConditionCheck TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Tasks_Select_ForConditionCheck TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Tasks_Select_ForConditionCheck TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Tasks_Select_ForConditionCheck TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Tasks_Select_ForConditionCheck TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Tasks_Select_ForConditionCheck TO [Dev - JNCC SQL]
END
GO