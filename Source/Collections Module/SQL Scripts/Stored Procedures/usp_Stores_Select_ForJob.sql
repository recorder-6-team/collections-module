If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Stores_Select_ForJob]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Stores_Select_ForJob]
GO

/*===========================================================================*\
  Description:	Returns Stores associated with a specified Job.

  Parameters:	
	@UserDomainMask	User's Domain Mask restricting which records may be returned
	@SessionID 	User's SessionID
	@SortOrderIndex	Index determining Sort Order
	@ParentKey 	Only the records associated with the parent key are returned

  Created:	August 2003

  Last revision information:
    $Revision: 7 $
    $Date: 23/09/04 17:17 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Stores_Select_ForJob] 
	@UserDomainMask INT,
	@SessionID CHAR(16),
	@SortOrderIndex TINYINT,
	@ParentKey CHAR(16)
AS
SET NOCOUNT ON

	SELECT 		S.Collection_Unit_Key AS Item_Key,  
			S.Item_Name + IsNull(' - ' + CU.Current_Location_Code, IsNull(' - ' + CU.Usual_Location_Code, '')) AS Item_Name, 
			Number, CUT.Collection_Unit_Task_Key AS Join_Key

	FROM 		Conservation_Job CJ
	INNER JOIN	Conservation_Task CT ON CJ.Conservation_Job_Key = CT.Conservation_Job_Key AND CJ.Conservation_Job_Key = @ParentKey
	INNER JOIN	Collection_Unit_Task CUT ON CT.Conservation_Task_Key = CUT.Conservation_Task_Key
	INNER JOIN	Store S ON CUT.Collection_Unit_Key = S.Collection_Unit_Key
	INNER JOIN    	Collection_Unit CU 
		ON S.Collection_Unit_Key = CU.Collection_Unit_Key
	       	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID)
			OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))

	LEFT JOIN 	Collection_Unit_Number CUN 
		ON S.Collection_Unit_Key = CUN.Collection_Unit_Key 
		AND CUN.Type_Concept_Key = 'SYSTEM0000000001'  --REGISTRATION NUMBER
		AND CUN.Preferred = 1

	ORDER BY 
		-- 0: S.Item_Name, Number 
		-- 1: Number, S.Item_Name
		-- 2: Current_Location_Code, S.Item_Name, CUN.Number 
		CASE @SortOrderIndex WHEN 1 THEN Number WHEN 2 THEN Current_Location_Code ELSE NULL END, 
		S.Item_Name,
		CASE @SortOrderIndex WHEN 1 THEN NULL ELSE Number END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Stores_Select_ForJob') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Stores_Select_ForJob'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Stores_Select_ForJob TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Stores_Select_ForJob TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Stores_Select_ForJob TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Stores_Select_ForJob TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Stores_Select_ForJob TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Stores_Select_ForJob TO [Dev - JNCC SQL]
END
GO