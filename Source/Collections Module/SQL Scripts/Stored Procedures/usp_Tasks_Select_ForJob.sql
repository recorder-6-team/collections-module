/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Tasks_Select_ForJob]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Tasks_Select_ForJob]
GO

/*===========================================================================*\
  Description:	Returns the Tasks associated with a job.

  Parameters:	@UserDomainMask
		@SessionID 
		@SortOrderIndex 
		@ParentKey 

  Created:	February 2004

  Last revision information:
    $Revision: 2 $
    $Date: 8/04/04 18:16 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Tasks_Select_ForJob]
	@UserDomainMask INT,
	@SessionID CHAR(16),
	@SortOrderIndex TINYINT,
	@ParentKey CHAR(16)
AS

SET NOCOUNT ON

	SELECT 		CT.Conservation_Task_Key AS Item_Key, CT.Display_Caption, 
			CT.Conservation_Task_Key AS Join_Key  -- Conservation_Task is a "join table" in itself.

	FROM		Conservation_Task AS CT 
	INNER JOIN	Conservation_Check AS CC
	 	ON CC.Conservation_Check_Key = CT.Conservation_Check_Key
		AND CT.Conservation_Job_Key = @ParentKey
		AND ((CC.Domain_Mask & @UserDomainMask > 0) 
			OR (CC.Entered_Session_ID = @SessionID) 
			OR (CC.Changed_Session_ID = @SessionID) 
			OR (CC.Domain_Mask = 0))

	ORDER BY 
		-- 0: CT.Set_Vague_Date_xxx, CT.Display_Caption
		-- 1: CT.Display_Caption
		CASE @SortOrderIndex WHEN 0 THEN CT.Set_Vague_Date_Start ELSE NULL END DESC, 
		CASE @SortOrderIndex WHEN 0 THEN CT.Set_Vague_Date_End ELSE NULL END DESC, 
		CASE @SortOrderIndex WHEN 0 THEN CT.Set_Vague_Date_Type ELSE NULL END, 
		CT.Display_Caption

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Tasks_Select_ForJob') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Tasks_Select_ForJob'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Tasks_Select_ForJob TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Tasks_Select_ForJob TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Tasks_Select_ForJob TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Tasks_Select_ForJob TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Tasks_Select_ForJob TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Tasks_Select_ForJob TO [Dev - JNCC SQL]
END
GO