If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Stores_Select_ForEnquiry]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Stores_Select_ForEnquiry]
GO

/*===========================================================================*\
  Description:	Returns Stores for a specified Enquiry.

  Parameters:	
	@ParentKey 	When specified, only the records associated with the parent key are returned
	@UserDomainMask	User's Domain Mask restricting which records may be returned
	@SessionID 	User's SessionID
	@SortOrderIndex	Index determining Sort Order

  Created:	August 2003

  Last revision information:
    $Revision: 9 $
    $Date: 23/09/04 17:17 $
    $Author: Anthonysimpson $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_Stores_Select_ForEnquiry] 
	@UserDomainMask INT,
	@SessionID CHAR(16),
	@SortOrderIndex TINYINT,
	@ParentKey CHAR(16)
AS

SET NOCOUNT ON

	SELECT 		S.Collection_Unit_Key AS Item_Key,  
			S.Item_Name + IsNull(' - ' + CU.Current_Location_Code, IsNull(' - ' + CU.Usual_Location_Code, '')) AS Item_Name, 
			CUN.Number, CUE.Collection_Unit_Enquiry_Key AS Join_Key

	FROM 		Collection_Unit_Enquiry CUE
	INNER JOIN	Store S	ON CUE.Collection_Unit_Key = S.Collection_Unit_Key AND CUE.Enquiry_Key = @ParentKey
	INNER JOIN	Collection_Unit CU 
		ON S.Collection_Unit_Key = CU.Collection_Unit_Key
	        AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID)
			OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))

	LEFT JOIN 	Collection_Unit_Number CUN 
		ON S.Collection_Unit_Key = CUN.Collection_Unit_Key 
		AND CUN.Type_Concept_Key = 'SYSTEM0000000001'  --REGISTRATION NUMBER
		AND CUN.Preferred = 1

	ORDER BY 
		-- 0: S.Item_Name, CUN.Number 
		-- 1: CUN.Number, S.Item_Name
		-- 2: Current_Location_Code, S.Item_Name, CUN.Number 
		CASE @SortOrderIndex WHEN 1 THEN CUN.Number WHEN 2 THEN Current_Location_Code ELSE NULL END, 
		S.Item_Name,
		CASE @SortOrderIndex WHEN 1 THEN NULL ELSE CUN.Number END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Stores_Select_ForEnquiry') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Stores_Select_ForEnquiry'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Stores_Select_ForEnquiry TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Stores_Select_ForEnquiry TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Stores_Select_ForEnquiry TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Stores_Select_ForEnquiry TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Stores_Select_ForEnquiry TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Stores_Select_ForEnquiry TO [Dev - JNCC SQL]
END
GO