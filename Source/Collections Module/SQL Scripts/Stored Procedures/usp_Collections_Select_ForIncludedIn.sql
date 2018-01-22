If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Collections_Select_ForIncludedIn]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Collections_Select_ForIncludedIn]
GO

/*===========================================================================*\
  Description:	Returns Parent Collection for a specified Collection.

  Parameters:
	@ParentKey 	When specified, only the records associated with the parent key are returned
	@UserDomainMask	User's Domain Mask restricting which records may be returned
	@SessionID 	User's SessionID
	@SortOrderIndex	Index determining Sort Order

  Created:	August 2003

  Last revision information:
    $Revision: 8 $
    $Date: 15/07/04 16:02 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Collections_Select_ForIncludedIn] 
	@UserDomainMask INT,
	@SessionID CHAR(16),
	@SortOrderIndex TINYINT,
	@ParentKey CHAR(16)
AS

SET NOCOUNT ON

	SELECT 		CP.Collection_Unit_Key AS Item_Key, CP.Item_Name, M.Number,
			C.Collection_Unit_Key AS Join_Key

	FROM 		Collection C
	INNER JOIN 	Collection CP 
		ON C.Parent_Collection_Collection_Unit_Key = CP.Collection_Unit_Key 
		AND C.Collection_Unit_Key = @ParentKey

	INNER JOIN	Collection_Unit CU 
		ON CP.Collection_Unit_Key = CU.Collection_Unit_Key
		AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
			OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))

	LEFT JOIN	(Movement_Collection_Unit MCU
			INNER JOIN	Movement_Direction MD
				ON MCU.Movement_Direction_Key = MD.Movement_Direction_Key 
				AND (MD.Outbound = 0)

			INNER JOIN	Movement M 
				ON MD.Movement_Key = M.Movement_Key 
				AND (M.Movement_Type = 0 OR M.Movement_Type = 1)

			LEFT JOIN	(Concept CON
					INNER JOIN Term T ON CON.Term_Key = T.Term_Key
					)
				ON CON.Concept_Key = 'SYSTEM0000000006' --ACCESSION NUMBER
			)
		ON CU.Collection_Unit_Key = MCU.Collection_Unit_Key

	ORDER BY 
		-- 0: Collation_From_Vague_Date_Start DESC, Collation_From_Vague_Date_End DESC, C.Item_Name, Number
		-- 1: C.Item_Name, Number
		-- 2: Number
		CASE @SortOrderIndex WHEN 0 THEN CP.Collation_From_Vague_Date_Start ELSE NULL END DESC,
		CASE @SortOrderIndex WHEN 0 THEN CP.Collation_From_Vague_Date_End ELSE NULL END DESC,
		CASE WHEN @SortOrderIndex IN (0, 1) THEN CP.Item_Name ELSE NULL END,
		Number
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Collections_Select_ForIncludedIn') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Collections_Select_ForIncludedIn'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Collections_Select_ForIncludedIn TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Collections_Select_ForIncludedIn TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Collections_Select_ForIncludedIn TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Collections_Select_ForIncludedIn TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Collections_Select_ForIncludedIn TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Collections_Select_ForIncludedIn TO [Dev - JNCC SQL]
END
GO