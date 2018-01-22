If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Jobs_Select_ForTask]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Jobs_Select_ForTask]
GO

/*===========================================================================*\
  Description:	Returns Jobs associated with a specified Task.
		The Join_Key is the Conservation_Task_Key because it is the join table,
		so when the link is removed between task and job, the Task table is
		the one that has to be updated.

  Parameters:
	@ParentKey 	When specified, only the records associated with the parent key are returned
	@UserDomainMask	User's Domain Mask restricting which records may be returned
	@SortOrderIndex	Index determining Sort Order
	@SessionID 	User's SessionID

  Created:	August 2003

  Last revision information:
    $Revision: 11 $
    $Date: 14/04/04 10:39 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Jobs_Select_ForTask] 
	@UserDomainMask INT,
	@SessionID CHAR(16),
	@SortOrderIndex TINYINT,
	@ParentKey CHAR(16)
AS

SET NOCOUNT ON

	SELECT 		CJ.Conservation_Job_Key AS Item_Key, CJ.Display_Caption,
			CT.Conservation_Task_Key AS Join_Key  -- Conservation_Task is the table to update when removing link

	FROM 		Conservation_Task CT
	INNER JOIN	Conservation_Job CJ
		ON CT.Conservation_Job_Key = CJ.Conservation_Job_Key 
		AND CT.Conservation_Task_Key = @ParentKey
		AND ((CJ.Domain_Mask & @UserDomainMask > 0) OR (CJ.Entered_Session_ID = @SessionID) 
				OR (CJ.Changed_Session_ID = @SessionID) OR (CJ.Domain_Mask = 0))
	ORDER BY 
		-- 0: CJ.Vague_Date_xxx, Item_Name
		-- 1: Item_Name
		CASE @SortOrderIndex WHEN 0 THEN CJ.From_Vague_Date_Start ELSE NULL END DESC, 
		CASE @SortOrderIndex WHEN 0 THEN CJ.From_Vague_Date_End ELSE NULL END DESC, 
		CASE @SortOrderIndex WHEN 0 THEN CJ.From_Vague_Date_Type ELSE NULL END, 
		Item_Name
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Jobs_Select_ForTask') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Jobs_Select_ForTask'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Jobs_Select_ForTask TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Jobs_Select_ForTask TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Jobs_Select_ForTask TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Jobs_Select_ForTask TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Jobs_Select_ForTask TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Jobs_Select_ForTask TO [Dev - JNCC SQL]
END
GO