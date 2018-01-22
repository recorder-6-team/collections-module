If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_ConditionChecks_Select_ForCollectionUnit]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_ConditionChecks_Select_ForCollectionUnit]
GO

/*===========================================================================*\
  Description:	Returns Condition Checks for a specified Collection.

  Parameters:
	@ParentKey 	When specified, only the records associated with the parent key are returned
	@UserDomainMask	User's Domain Mask restricting which records may be returned
	@SessionID 	User's SessionID
	@SortOrderIndex	Index determining Sort Order

  Created:	August 2003

  Last revision information:
    $Revision: 11 $
    $Date: 3/08/11 15:04 $
    $Author: Simonlewis $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConditionChecks_Select_ForCollectionUnit] 
	@UserDomainMask INT,
	@SessionID CHAR(16),
	@ParentKey CHAR(16),
	@SortOrderIndex TINYINT
AS

SET NOCOUNT ON

	SELECT 		CUC.Conservation_Check_Key AS Item_Key, CC.Display_Caption,
			CUC.Collection_Unit_Check_Key AS Join_Key

	FROM 		Collection_Unit_Check CUC
	INNER JOIN 	Conservation_Check CC
		ON CUC.Conservation_Check_Key = CC.Conservation_Check_Key 
		AND CUC.Collection_Unit_Key = @ParentKey
		AND ((CC.Domain_Mask & @UserDomainMask > 0) OR (CC.Entered_Session_ID = @SessionID) 
			OR (CC.Changed_Session_ID = @SessionID) OR (CC.Domain_Mask = 0))

	INNER JOIN 	Concept C ON CC.Type_Concept_Key = C.Concept_Key
	INNER JOIN 	Term T ON C.Term_Key = T.Term_Key

	ORDER BY 
		-- 0: CC.Vague_Date_Start DESC, CC.Vague_Date_End DESC, Item_Name, CC.Ref_Number
		-- 1: CC.Ref_Number
		CASE @SortOrderIndex WHEN 0 THEN CC.Vague_Date_Start ELSE NULL END DESC, 
		CASE @SortOrderIndex WHEN 0 THEN CC.Vague_Date_End ELSE NULL END DESC, 
		CASE @SortOrderIndex WHEN 0 THEN T.Plaintext ELSE NULL END,
		CC.Ref_Number
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConditionChecks_Select_ForCollectionUnit') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConditionChecks_Select_ForCollectionUnit'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConditionChecks_Select_ForCollectionUnit TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConditionChecks_Select_ForCollectionUnit TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConditionChecks_Select_ForCollectionUnit TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ConditionChecks_Select_ForCollectionUnit TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConditionChecks_Select_ForCollectionUnit TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConditionChecks_Select_ForCollectionUnit TO [Dev - JNCC SQL]
END
GO