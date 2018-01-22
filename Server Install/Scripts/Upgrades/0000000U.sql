SET ANSI_NULLS ON
SET QUOTED_IDENTIFIER ON
GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Collections_Select_ForConditionCheck]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Collections_Select_ForConditionCheck]
GO

/*===========================================================================*\
  Description:	Returns Collections associated with a specified Condition Check.

  Parameters:	
	@ParentKey 	When specified, only the records associated with the parent key are returned
	@SortOrderIndex	Index determining Sort Order
	@UserDomainMask	User's Domain Mask restricting which records may be returned
	@SessionID 	User's SessionID

  Created:	August 2003

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_Collections_Select_ForConditionCheck] 
	@UserDomainMask INT,
	@SessionID CHAR(16),
	@SortOrderIndex TINYINT,
	@ParentKey CHAR(16)
AS

SET NOCOUNT ON

	SELECT		C.Collection_Unit_Key AS Item_Key, C.Item_Name, M.Number, CUC.Collection_Unit_Check_Key AS Join_Key

	FROM 		(Conservation_Check CC
			INNER JOIN	Collection_Unit_Check CUC ON CC.Conservation_Check_Key = CUC.Conservation_Check_Key
			INNER JOIN 	Collection C ON CUC.Collection_Unit_Key = C.Collection_Unit_Key AND CUC.Conservation_Check_Key = @ParentKey
		    	INNER JOIN	Collection_Unit CU 
				ON C.Collection_Unit_Key = CU.Collection_Unit_Key
				AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
					OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
			)
	LEFT JOIN	(Movement_Collection_Unit MCU
			INNER JOIN	Movement_Direction MD ON MCU.Movement_Direction_Key = MD.Movement_Direction_Key AND (MD.Outbound = 0)
			INNER JOIN	Movement M ON MD.Movement_Key = M.Movement_Key AND (M.Movement_Type = 0 OR M.Movement_Type = 1)
			LEFT JOIN	(Concept CON
					INNER JOIN Term T ON CON.Term_Key = T.Term_Key
					)
			ON CON.Concept_Key = 'SYSTEM0000000006' -- ACCESSION NUMBER
			)
		ON CU.Collection_Unit_Key = MCU.Collection_Unit_Key

	ORDER BY 
		-- 0: Collation_From_Vague_Date_Start DESC, Collation_From_Vague_Date_End DESC, C.Item_Name, M.Number
		-- 1: C.Item_Name, M.Number
		-- 2: M.Number
		CASE @SortOrderIndex WHEN 0 THEN Collation_From_Vague_Date_Start ELSE NULL END DESC,
		CASE @SortOrderIndex WHEN 0 THEN Collation_From_Vague_Date_End ELSE NULL END DESC,
		CASE WHEN @SortOrderIndex IN (0, 1) THEN C.Item_Name ELSE NULL END,
		M.Number
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Collections_Select_ForConditionCheck') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Collections_Select_ForConditionCheck'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Collections_Select_ForConditionCheck TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Collections_Select_ForConditionCheck TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Collections_Select_ForConditionCheck TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Collections_Select_ForConditionCheck TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Collections_Select_ForConditionCheck TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Collections_Select_ForConditionCheck TO [Dev - JNCC SQL]
END
GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Collections_Select_ForEnquiry]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Collections_Select_ForEnquiry]
GO

/*===========================================================================*\
  Description:	Returns Collections for a specified Enquiry.

  Parameters:	
	@ParentKey 	When specified, only the records associated with the parent key are returned
	@SortOrderIndex	Index determining Sort Order
	@UserDomainMask	User's Domain Mask restricting which records may be returned
	@SessionID 	User's SessionID

  Created:	August 2003

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Collections_Select_ForEnquiry] 
	@UserDomainMask INT,
	@SessionID CHAR(16),
	@SortOrderIndex TINYINT,
	@ParentKey CHAR(16)
AS

SET NOCOUNT ON

	SELECT 		C.Collection_Unit_Key AS Item_Key, C.Item_Name, M.Number, CUE.Collection_Unit_Enquiry_Key AS Join_Key

	FROM 		(Collection_Unit_Enquiry CUE
			INNER JOIN 	Collection C ON CUE.Collection_Unit_Key = C.Collection_Unit_Key AND CUE.Enquiry_Key = @ParentKey
			INNER JOIN	Collection_Unit CU 
				ON C.Collection_Unit_Key = CU.Collection_Unit_Key
				AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
					OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
			)
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
		-- 0: Collation_From_Vague_Date_Start DESC, Collation_From_Vague_Date_End DESC, C.Item_Name, M.Number
		-- 1: C.Item_Name, M.Number
		-- 2: M.Number
		CASE @SortOrderIndex WHEN 0 THEN Collation_From_Vague_Date_Start ELSE NULL END DESC,
		CASE @SortOrderIndex WHEN 0 THEN Collation_From_Vague_Date_End ELSE NULL END DESC,
		CASE WHEN @SortOrderIndex IN (0, 1) THEN C.Item_Name ELSE NULL END,
		M.Number
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Collections_Select_ForEnquiry') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Collections_Select_ForEnquiry'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Collections_Select_ForEnquiry TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Collections_Select_ForEnquiry TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Collections_Select_ForEnquiry TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Collections_Select_ForEnquiry TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Collections_Select_ForEnquiry TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Collections_Select_ForEnquiry TO [Dev - JNCC SQL]
END
GO

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
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

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

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Collections_Select_ForIncludes]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Collections_Select_ForIncludes]
GO

/*===========================================================================*\
  Description:	Returns Child Collections for a specified Collection

  Parameters:	
	@ParentKey 	When specified, only the records associated with the parent key are returned
	@UserDomainMask	User's Domain Mask restricting which records may be returned
	@SessionID 	User's SessionID
	@SortOrderIndex	Index determining Sort Order

  Created:	August 2003

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Collections_Select_ForIncludes] 
	@UserDomainMask INT,
	@SessionID CHAR(16),
	@SortOrderIndex TINYINT,
	@ParentKey CHAR(16)
AS

SET NOCOUNT ON

	SELECT 		C.Collection_Unit_Key AS Item_Key, C.Item_Name, M.Number, C.Collection_Unit_Key AS Join_Key

	FROM 		Collection CP
	INNER JOIN 	Collection C
		ON CP.Collection_Unit_Key = C.Parent_Collection_Collection_Unit_Key 
		AND CP.Collection_Unit_Key = @ParentKey

	INNER JOIN	Collection_Unit CU 
		ON C.Collection_Unit_Key = CU.Collection_Unit_Key
		AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
			OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))

	LEFT JOIN	(Movement_Collection_Unit MCU
			INNER JOIN	Movement_Direction MD
				ON MCU.Movement_Direction_Key = MD.Movement_Direction_Key 
				AND (MD.Outbound = 0)

			INNER JOIN	Movement M ON MD.Movement_Key = M.Movement_Key AND M.Movement_Type IN (0, 1)
			LEFT JOIN	(Concept CON
					INNER JOIN Term T ON CON.Term_Key = T.Term_Key
					)
				ON CON.Concept_Key = 'SYSTEM0000000006' --ACCESSION NUMBER
			)
		ON CU.Collection_Unit_Key = MCU.Collection_Unit_Key

	ORDER BY 
		-- 0: Collation_From_Vague_Date_Start DESC, Collation_From_Vague_Date_End DESC, C.Item_Name, M.Number
		-- 1: C.Item_Name, M.Number
		-- 2: M.Number
		CASE @SortOrderIndex WHEN 0 THEN C.Collation_From_Vague_Date_Start ELSE NULL END DESC,
		CASE @SortOrderIndex WHEN 0 THEN C.Collation_From_Vague_Date_End ELSE NULL END DESC,
		CASE WHEN @SortOrderIndex IN (0, 1) THEN C.Item_Name ELSE NULL END,
		M.Number
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Collections_Select_ForIncludes') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Collections_Select_ForIncludes'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Collections_Select_ForIncludes TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Collections_Select_ForIncludes TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Collections_Select_ForIncludes TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Collections_Select_ForIncludes TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Collections_Select_ForIncludes TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Collections_Select_ForIncludes TO [Dev - JNCC SQL]
END
GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Collections_Select_ForJob]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Collections_Select_ForJob]
GO

/*===========================================================================*\
  Description:	Returns Collections associated with a specified Job.

  Parameters:
	@UserDomainMask	User's Domain Mask restricting which records may be returned
	@SessionID 	User's SessionID
	@SortOrderIndex	Index determining Sort Order
	@ParentKey 	Only the records associated with the parent key are returned

  Created:	August 2003

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Collections_Select_ForJob] 
	@UserDomainMask INT,
	@SessionID CHAR(16),
	@SortOrderIndex TINYINT,
	@ParentKey CHAR(16)
AS

SET NOCOUNT ON

	SELECT 		C.Collection_Unit_Key AS Item_Key, C.Item_Name, M.Number, CUT.Collection_Unit_Task_Key AS Join_Key

	FROM 		Conservation_Job CJ
	INNER JOIN	Conservation_Task CT ON CJ.Conservation_Job_Key = CT.Conservation_Job_Key AND CJ.Conservation_Job_Key = @ParentKey
	INNER JOIN	Collection_Unit_Task CUT ON CT.Conservation_Task_Key = CUT.Conservation_Task_Key
	INNER JOIN	Collection C ON CUT.Collection_Unit_Key = C.Collection_Unit_Key
	INNER JOIN	Collection_Unit CU 
		ON C.Collection_Unit_Key = CU.Collection_Unit_Key
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
		-- 0: Collation_From_Vague_Date_Start DESC, Collation_From_Vague_Date_End DESC, C.Item_Name, M.Number
		-- 1: C.Item_Name, M.Number
		-- 2: M.Number
		CASE @SortOrderIndex WHEN 0 THEN Collation_From_Vague_Date_Start ELSE NULL END DESC,
		CASE @SortOrderIndex WHEN 0 THEN Collation_From_Vague_Date_End ELSE NULL END DESC,
		CASE WHEN @SortOrderIndex IN (0, 1) THEN C.Item_Name ELSE NULL END,
		M.Number
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Collections_Select_ForJob') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Collections_Select_ForJob'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Collections_Select_ForJob TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Collections_Select_ForJob TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Collections_Select_ForJob TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Collections_Select_ForJob TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Collections_Select_ForJob TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Collections_Select_ForJob TO [Dev - JNCC SQL]
END
GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Collections_Select_ForLinkedOther]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Collections_Select_ForLinkedOther]
GO

/*===========================================================================*\
  Description:	Returns Related Collections for a specified Collection

  Parameters:	
	@ParentKey 	When specified, only the records associated with the parent key are returned
	@SortOrderIndex	Index determining Sort Order
	@UserDomainMask	User's Domain Mask restricting which records may be returned
	@SessionID 	User's SessionID

  Created:	August 2003

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Collections_Select_ForLinkedOther] 
	@UserDomainMask INT,
	@SessionID CHAR(16),
	@SortOrderIndex TINYINT,
	@ParentKey CHAR(16)
AS

SET NOCOUNT ON

IF @SortOrderIndex = 0
	SELECT DISTINCT --DISTINCT BECAUSE OF TWO MOVEMENT_DIRECTION records
		CUR.Collection_Unit_Relation_Key AS Item_Key,
		CUR.Collection_Unit_Relation_Key AS Join_Key,
		CASE WHEN C1.Collection_Unit_Key IS NOT NULL THEN C1.Collection_Unit_Key ELSE C2.Collection_Unit_Key END AS Hyperlink_Item_Key, 
		CASE WHEN C1.Collection_Unit_Key IS NOT NULL THEN C1.Collection_Unit_Key ELSE C2.Collection_Unit_Key END AS Drag_Drop_Item_Key, 
		CASE WHEN C1.Collection_Unit_Key IS NOT NULL THEN C1.Item_Name ELSE C2.Item_Name END AS Item_Name,
		CASE WHEN C1.Collection_Unit_Key IS NOT NULL THEN M1.Number ELSE M2.Number END AS Number,
		CASE WHEN C1.Collection_Unit_Key IS NOT NULL THEN C1.Collation_From_Vague_Date_Start ELSE C2.Collation_From_Vague_Date_Start END AS Collation_From_Vague_Date_Start,
		CASE WHEN C1.Collection_Unit_Key IS NOT NULL THEN C1.Collation_From_Vague_Date_End ELSE C2.Collation_From_Vague_Date_End END AS Collation_From_Vague_Date_End,
		CASE WHEN C1.Collection_Unit_Key IS NOT NULL THEN C1.Collation_From_Vague_Date_Type ELSE C2.Collation_From_Vague_Date_Type END AS Collation_From_Vague_Date_Type

	FROM 		Collection_Unit_Relation CUR
	INNER JOIN	Thesaurus_Relation_Type TRT ON CUR.Thesaurus_Relation_Type_Key = TRT.Thesaurus_Relation_Type_Key
	INNER JOIN	Semantic_Relation SR ON TRT.Semantic_Relation_Key = SR.Semantic_Relation_Key --AND SR.Unidirectional = 0
	LEFT JOIN 	(Collection C1
			INNER JOIN	Collection_Unit CU1
				ON C1.Collection_Unit_Key = CU1.Collection_Unit_Key
				AND ((CU1.Domain_Mask & @UserDomainMask > 0) OR (CU1.Entered_Session_ID = @SessionID) 
					OR (CU1.Changed_Session_ID = @SessionID) OR (CU1.Domain_Mask = 0))
			)
		ON CUR.To_Collection_Unit_Key = C1.Collection_Unit_Key AND CUR.From_Collection_Unit_Key = @ParentKey

	LEFT JOIN	(Movement_Collection_Unit MCU1
			INNER JOIN	Movement_Direction MD1
				ON MCU1.Movement_Direction_Key = MD1.Movement_Direction_Key 
				AND (MD1.Outbound = 0)

			INNER JOIN	Movement M1 ON MD1.Movement_Key = M1.Movement_Key AND M1.Movement_Type IN (0, 1)
			LEFT JOIN	(Concept CON1
					INNER JOIN Term T1 ON CON1.Term_Key = T1.Term_Key
					)
				ON CON1.Concept_Key = 'SYSTEM0000000006' --ACCESSION NUMBER
			)
		ON CU1.Collection_Unit_Key = MCU1.Collection_Unit_Key

	LEFT JOIN 	(Collection C2
			INNER JOIN 	Collection_Unit CU2
				ON C2.Collection_Unit_Key = CU2.Collection_Unit_Key
				AND ((CU2.Domain_Mask & @UserDomainMask > 0) OR (CU2.Entered_Session_ID = @SessionID) 
					OR (CU2.Changed_Session_ID = @SessionID) OR (CU2.Domain_Mask = 0))
			)
		ON CUR.From_Collection_Unit_Key = C2.Collection_Unit_Key AND CUR.To_Collection_Unit_Key = @ParentKey
		AND SR.Unidirectional = 0

	LEFT JOIN	(Movement_Collection_Unit MCU2
			INNER JOIN	Movement_Direction MD2
				ON MCU2.Movement_Direction_Key = MD2.Movement_Direction_Key 
				AND (MD2.Outbound = 0)

			INNER JOIN	Movement M2 ON MD2.Movement_Key = M2.Movement_Key AND M2.Movement_Type IN (0, 1)
			LEFT JOIN	(Concept CON2
					INNER JOIN Term T2 ON CON2.Term_Key = T2.Term_Key
					)
				ON CON2.Concept_Key = 'SYSTEM0000000006' --ACCESSION NUMBER
			)
		ON CU2.Collection_Unit_Key = MCU2.Collection_Unit_Key

	WHERE 	(C1.Collection_Unit_Key IS NOT NULL) 
	OR 	(C2.Collection_Unit_Key IS NOT NULL)

	ORDER BY Collation_From_Vague_Date_Start DESC, Collation_From_Vague_Date_End DESC, C.Item_Name, M.Number

ELSE IF @SortOrderIndex = 1
	SELECT DISTINCT --DISTINCT BECAUSE OF TWO MOVEMENT_DIRECTION records
		CUR.Collection_Unit_Relation_Key AS Item_Key,
		CUR.Collection_Unit_Relation_Key AS Join_Key,
		CASE WHEN C1.Collection_Unit_Key IS NOT NULL THEN C1.Collection_Unit_Key ELSE C2.Collection_Unit_Key END AS Hyperlink_Item_Key, 
		CASE WHEN C1.Collection_Unit_Key IS NOT NULL THEN C1.Collection_Unit_Key ELSE C2.Collection_Unit_Key END AS Drag_Drop_Item_Key, 
		CASE WHEN C1.Collection_Unit_Key IS NOT NULL THEN C1.Item_Name ELSE C2.Item_Name END AS Item_Name,
		CASE WHEN C1.Collection_Unit_Key IS NOT NULL THEN M1.Number ELSE M2.Number END AS Number,
		CASE WHEN C1.Collection_Unit_Key IS NOT NULL THEN C1.Collation_From_Vague_Date_Start ELSE C2.Collation_From_Vague_Date_Start END AS Collation_From_Vague_Date_Start,
		CASE WHEN C1.Collection_Unit_Key IS NOT NULL THEN C1.Collation_From_Vague_Date_End ELSE C2.Collation_From_Vague_Date_End END AS Collation_From_Vague_Date_End,
		CASE WHEN C1.Collection_Unit_Key IS NOT NULL THEN C1.Collation_From_Vague_Date_Type ELSE C2.Collation_From_Vague_Date_Type END AS Collation_From_Vague_Date_Type

	FROM 		Collection_Unit_Relation CUR
	INNER JOIN	Thesaurus_Relation_Type TRT ON CUR.Thesaurus_Relation_Type_Key = TRT.Thesaurus_Relation_Type_Key
	INNER JOIN	Semantic_Relation SR ON TRT.Semantic_Relation_Key = SR.Semantic_Relation_Key --AND SR.Unidirectional = 0
	LEFT JOIN 	(Collection C1
			INNER JOIN	Collection_Unit CU1
				ON C1.Collection_Unit_Key = CU1.Collection_Unit_Key
				AND ((CU1.Domain_Mask & @UserDomainMask > 0) OR (CU1.Entered_Session_ID = @SessionID) 
					OR (CU1.Changed_Session_ID = @SessionID) OR (CU1.Domain_Mask = 0))
			)
		ON CUR.To_Collection_Unit_Key = C1.Collection_Unit_Key AND CUR.From_Collection_Unit_Key = @ParentKey

	LEFT JOIN	(Movement_Collection_Unit MCU1
			INNER JOIN	Movement_Direction MD1
				ON MCU1.Movement_Direction_Key = MD1.Movement_Direction_Key 
				AND (MD1.Outbound = 0)

			INNER JOIN	Movement M1 ON MD1.Movement_Key = M1.Movement_Key AND M1.Movement_Type IN (0, 1)
			LEFT JOIN	(Concept CON1
					INNER JOIN Term T1 ON CON1.Term_Key = T1.Term_Key
					)
				ON CON1.Concept_Key = 'SYSTEM0000000006' --ACCESSION NUMBER
			)
		ON CU1.Collection_Unit_Key = MCU1.Collection_Unit_Key

	LEFT JOIN 	(Collection C2
			INNER JOIN 	Collection_Unit CU2
				ON C2.Collection_Unit_Key = CU2.Collection_Unit_Key
				AND ((CU2.Domain_Mask & @UserDomainMask > 0) OR (CU2.Entered_Session_ID = @SessionID) 
					OR (CU2.Changed_Session_ID = @SessionID) OR (CU2.Domain_Mask = 0))
			)
		ON CUR.From_Collection_Unit_Key = C2.Collection_Unit_Key AND CUR.To_Collection_Unit_Key = @ParentKey
		AND SR.Unidirectional = 0

	LEFT JOIN	(Movement_Collection_Unit MCU2
			INNER JOIN	Movement_Direction MD2
				ON MCU2.Movement_Direction_Key = MD2.Movement_Direction_Key 
				AND (MD2.Outbound = 0)

			INNER JOIN	Movement M2 ON MD2.Movement_Key = M2.Movement_Key AND M2.Movement_Type IN (0, 1)
			LEFT JOIN	(Concept CON2
					INNER JOIN Term T2 ON CON2.Term_Key = T2.Term_Key
					)
				ON CON2.Concept_Key = 'SYSTEM0000000006' --ACCESSION NUMBER
			)
		ON CU2.Collection_Unit_Key = MCU2.Collection_Unit_Key

	WHERE 	(C1.Collection_Unit_Key IS NOT NULL) 
	OR 	(C2.Collection_Unit_Key IS NOT NULL)

	ORDER BY C.Item_Name, M.Number


ELSE IF @SortOrderIndex = 2
	SELECT DISTINCT --DISTINCT BECAUSE OF TWO MOVEMENT_DIRECTION records
		CUR.Collection_Unit_Relation_Key AS Item_Key,
		CUR.Collection_Unit_Relation_Key AS Join_Key,
		CASE WHEN C1.Collection_Unit_Key IS NOT NULL THEN C1.Collection_Unit_Key ELSE C2.Collection_Unit_Key END AS Hyperlink_Item_Key, 
		CASE WHEN C1.Collection_Unit_Key IS NOT NULL THEN C1.Collection_Unit_Key ELSE C2.Collection_Unit_Key END AS Drag_Drop_Item_Key, 
		CASE WHEN C1.Collection_Unit_Key IS NOT NULL THEN C1.Item_Name ELSE C2.Item_Name END AS Item_Name,
		CASE WHEN C1.Collection_Unit_Key IS NOT NULL THEN M1.Number ELSE M2.Number END AS Number,
		CASE WHEN C1.Collection_Unit_Key IS NOT NULL THEN C1.Collation_From_Vague_Date_Start ELSE C2.Collation_From_Vague_Date_Start END AS Collation_From_Vague_Date_Start,
		CASE WHEN C1.Collection_Unit_Key IS NOT NULL THEN C1.Collation_From_Vague_Date_End ELSE C2.Collation_From_Vague_Date_End END AS Collation_From_Vague_Date_End,
		CASE WHEN C1.Collection_Unit_Key IS NOT NULL THEN C1.Collation_From_Vague_Date_Type ELSE C2.Collation_From_Vague_Date_Type END AS Collation_From_Vague_Date_Type

	FROM 		Collection_Unit_Relation CUR
	INNER JOIN	Thesaurus_Relation_Type TRT ON CUR.Thesaurus_Relation_Type_Key = TRT.Thesaurus_Relation_Type_Key
	INNER JOIN	Semantic_Relation SR ON TRT.Semantic_Relation_Key = SR.Semantic_Relation_Key --AND SR.Unidirectional = 0
	LEFT JOIN 	(Collection C1
			INNER JOIN	Collection_Unit CU1
				ON C1.Collection_Unit_Key = CU1.Collection_Unit_Key
				AND ((CU1.Domain_Mask & @UserDomainMask > 0) OR (CU1.Entered_Session_ID = @SessionID) 
					OR (CU1.Changed_Session_ID = @SessionID) OR (CU1.Domain_Mask = 0))
			)
		ON CUR.To_Collection_Unit_Key = C1.Collection_Unit_Key AND CUR.From_Collection_Unit_Key = @ParentKey

	LEFT JOIN	(Movement_Collection_Unit MCU1
			INNER JOIN	Movement_Direction MD1
				ON MCU1.Movement_Direction_Key = MD1.Movement_Direction_Key 
				AND (MD1.Outbound = 0)

			INNER JOIN	Movement M1 ON MD1.Movement_Key = M1.Movement_Key AND M1.Movement_Type IN (0, 1)
			LEFT JOIN	(Concept CON1
					INNER JOIN Term T1 ON CON1.Term_Key = T1.Term_Key
					)
				ON CON1.Concept_Key = 'SYSTEM0000000006' --ACCESSION NUMBER
			)
		ON CU1.Collection_Unit_Key = MCU1.Collection_Unit_Key

	LEFT JOIN 	(Collection C2
			INNER JOIN 	Collection_Unit CU2
				ON C2.Collection_Unit_Key = CU2.Collection_Unit_Key
				AND ((CU2.Domain_Mask & @UserDomainMask > 0) OR (CU2.Entered_Session_ID = @SessionID) 
					OR (CU2.Changed_Session_ID = @SessionID) OR (CU2.Domain_Mask = 0))
			)
		ON CUR.From_Collection_Unit_Key = C2.Collection_Unit_Key AND CUR.To_Collection_Unit_Key = @ParentKey
		AND SR.Unidirectional = 0

	LEFT JOIN	(Movement_Collection_Unit MCU2
			INNER JOIN	Movement_Direction MD2
				ON MCU2.Movement_Direction_Key = MD2.Movement_Direction_Key 
				AND (MD2.Outbound = 0)

			INNER JOIN	Movement M2 ON MD2.Movement_Key = M2.Movement_Key AND M2.Movement_Type IN (0, 1)
			LEFT JOIN	(Concept CON2
					INNER JOIN Term T2 ON CON2.Term_Key = T2.Term_Key
					)
				ON CON2.Concept_Key = 'SYSTEM0000000006' --ACCESSION NUMBER
			)
		ON CU2.Collection_Unit_Key = MCU2.Collection_Unit_Key

	WHERE 	(C1.Collection_Unit_Key IS NOT NULL) 
	OR 	(C2.Collection_Unit_Key IS NOT NULL)

	ORDER BY M.Number

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Collections_Select_ForLinkedOther') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Collections_Select_ForLinkedOther'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Collections_Select_ForLinkedOther TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Collections_Select_ForLinkedOther TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Collections_Select_ForLinkedOther TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Collections_Select_ForLinkedOther TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Collections_Select_ForLinkedOther TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Collections_Select_ForLinkedOther TO [Dev - JNCC SQL]
END
GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Collections_Select_ForMovement]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Collections_Select_ForMovement]
GO

/*===========================================================================*\
  Description:	Returns Collection Units for a specified movement.

  Parameters:	
	@ParentKey 	When specified, only the records associated with the parent key are returned
	@SortOrderIndex	Index determining Sort Order
	@UserDomainMask	User's Domain Mask restricting which records may be returned
	@SessionID 	User's SessionID

  Created:	August 2003

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Collections_Select_ForMovement] 
	@UserDomainMask INT,
	@SessionID CHAR(16),
	@SortOrderIndex TINYINT,
	@ParentKey CHAR(16)
AS

SET NOCOUNT ON

	SELECT 		C.Collection_Unit_Key AS Item_Key, C.Item_Name, M2.Number, MCU.Movement_Collection_Unit_Key AS Join_Key,
			Collation_From_Vague_Date_Start, Collation_From_Vague_Date_End 

	FROM		(Movement M
			INNER JOIN 	Movement_Direction MD ON M.Movement_Key = MD.Movement_Key AND (M.Movement_Key = @ParentKey)
			INNER JOIN 	Movement_Collection_Unit MCU ON MD.Movement_Direction_Key = MCU.Movement_Direction_Key
			INNER JOIN 	Collection C ON MCU.Collection_Unit_Key = C.Collection_Unit_Key
		    	INNER JOIN	Collection_Unit CU 
				ON C.Collection_Unit_Key = CU.Collection_Unit_Key
			    	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
					OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
			)
	LEFT JOIN	(Movement_Collection_Unit MCU2
			INNER JOIN	Movement_Direction MD2
				ON MCU2.Movement_Direction_Key = MD2.Movement_Direction_Key 
				AND (MD2.Outbound = 0)

			INNER JOIN	Movement M2
				ON MD2.Movement_Key = M2.Movement_Key 
				AND (M2.Movement_Type = 0 OR M2.Movement_Type = 1)

			LEFT JOIN	(Concept CON
					INNER JOIN Term T ON CON.Term_Key = T.Term_Key
					)
			ON CON.Concept_Key = 'SYSTEM0000000006' --ACCESSION NUMBER
			)
		ON CU.Collection_Unit_Key = MCU2.Collection_Unit_Key
	ORDER BY 
		-- 0: Collation_From_Vague_Date_Start DESC, Collation_From_Vague_Date_End DESC, C.Item_Name, M.Number
		-- 1: C.Item_Name, M.Number
		-- 2: M.Number
		CASE @SortOrderIndex WHEN 0 THEN Collation_From_Vague_Date_Start ELSE NULL END DESC,
		CASE @SortOrderIndex WHEN 0 THEN Collation_From_Vague_Date_End ELSE NULL END DESC,
		CASE WHEN @SortOrderIndex IN (0, 1) THEN C.Item_Name ELSE NULL END,
		M.Number
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Collections_Select_ForMovement') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Collections_Select_ForMovement'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Collections_Select_ForMovement TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Collections_Select_ForMovement TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Collections_Select_ForMovement TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Collections_Select_ForMovement TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Collections_Select_ForMovement TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Collections_Select_ForMovement TO [Dev - JNCC SQL]
END
GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Collections_Select_ForMovementIn]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Collections_Select_ForMovementIn]
GO

/*===========================================================================*\
  Description:	Returns Collection Units for a specified movement.

  Parameters:	
	@ParentKey 	When specified, only the records associated with the parent key are returned
	@SortOrderIndex	Index determining Sort Order
	@UserDomainMask	User's Domain Mask restricting which records may be returned
	@SessionID 	User's SessionID

  Created:	August 2003

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Collections_Select_ForMovementIn] 
	@UserDomainMask INT,
	@SessionID CHAR(16),
	@SortOrderIndex TINYINT,
	@ParentKey CHAR(16)
AS

SET NOCOUNT ON

	SELECT 		C.Collection_Unit_Key AS Item_Key, C.Item_Name, M2.Number, MCU.Movement_Collection_Unit_Key AS Join_Key,
			Collation_From_Vague_Date_Start, Collation_From_Vague_Date_End 

	FROM		(Movement M
			INNER JOIN 	Movement_Direction MD  
				ON M.Movement_Key = MD.Movement_Key 
				AND (M.Movement_Key = @ParentKey) 
				AND (MD.Outbound = 0)

			INNER JOIN 	Movement_Collection_Unit MCU ON MD.Movement_Direction_Key = MCU.Movement_Direction_Key
			INNER JOIN 	Collection C ON MCU.Collection_Unit_Key = C.Collection_Unit_Key
		    	INNER JOIN	Collection_Unit CU 
				ON C.Collection_Unit_Key = CU.Collection_Unit_Key
				AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
					OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
			)
	LEFT JOIN	(Movement_Collection_Unit MCU2
			INNER JOIN	Movement_Direction MD2 
				ON MCU2.Movement_Direction_Key = MD2.Movement_Direction_Key 
				AND (MD2.Outbound = 0)

			INNER JOIN	Movement M2 
				ON MD2.Movement_Key = M2.Movement_Key 
				AND (M2.Movement_Type = 0 OR M2.Movement_Type = 1)

			LEFT JOIN 	(Concept CON
					INNER JOIN Term T ON CON.Term_Key = T.Term_Key
					)
				ON CON.Concept_Key = 'SYSTEM0000000006' --ACCESSION NUMBER
			)
		ON CU.Collection_Unit_Key = MCU2.Collection_Unit_Key

	ORDER BY
		-- 0: Collation_From_Vague_Date_Start DESC, Collation_From_Vague_Date_End DESC, C.Item_Name, M.Number
		-- 1: C.Item_Name, M.Number
		-- 2: M.Number
		CASE @SortOrderIndex WHEN 0 THEN Collation_From_Vague_Date_Start ELSE NULL END DESC,
		CASE @SortOrderIndex WHEN 0 THEN Collation_From_Vague_Date_End ELSE NULL END DESC,
		CASE WHEN @SortOrderIndex IN (0, 1) THEN C.Item_Name ELSE NULL END,
		M.Number
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Collections_Select_ForMovementIn') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Collections_Select_ForMovementIn'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Collections_Select_ForMovementIn TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Collections_Select_ForMovementIn TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Collections_Select_ForMovementIn TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Collections_Select_ForMovementIn TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Collections_Select_ForMovementIn TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Collections_Select_ForMovementIn TO [Dev - JNCC SQL]
END
GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Collections_Select_ForMovementOut]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Collections_Select_ForMovementOut]
GO

/*===========================================================================*\
  Description:	Returns Collection Units for a specified movement.

  Parameters:	
	@ParentKey 	When specified, only the records associated with the parent key are returned
	@SortOrderIndex	Index determining Sort Order
	@UserDomainMask	User's Domain Mask restricting which records may be returned
	@SessionID 	User's SessionID

  Created:	August 2003

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Collections_Select_ForMovementOut] 
	@UserDomainMask INT,
	@SessionID CHAR(16),
	@SortOrderIndex TINYINT,
	@ParentKey CHAR(16)
AS

SET NOCOUNT ON

	SELECT 		C.Collection_Unit_Key AS Item_Key, C.Item_Name, M2.Number, MCU.Movement_Collection_Unit_Key AS Join_Key,
			Collation_From_Vague_Date_Start, Collation_From_Vague_Date_End 

	FROM		(Movement M
			INNER JOIN 	Movement_Direction MD  
				ON M.Movement_Key = MD.Movement_Key 
				AND (M.Movement_Key = @ParentKey) 
				AND (MD.Outbound = 1) 

			INNER JOIN 	Movement_Collection_Unit MCU ON MD.Movement_Direction_Key = MCU.Movement_Direction_Key
			INNER JOIN 	Collection C ON MCU.Collection_Unit_Key = C.Collection_Unit_Key
		    	INNER JOIN	Collection_Unit CU 
				ON C.Collection_Unit_Key = CU.Collection_Unit_Key
				AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
					OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
			)
	LEFT JOIN	(Movement_Collection_Unit MCU2
			INNER JOIN	Movement_Direction MD2 
				ON MCU2.Movement_Direction_Key = MD2.Movement_Direction_Key 
				AND (MD2.Outbound = 0)

			INNER JOIN	Movement M2 
				ON MD2.Movement_Key = M2.Movement_Key 
				AND (M2.Movement_Type = 0 OR M2.Movement_Type = 1)

			LEFT JOIN 	(Concept CON
					INNER JOIN Term T ON CON.Term_Key = T.Term_Key
					)
				ON CON.Concept_Key = 'SYSTEM0000000006' --ACCESSION NUMBER
			)
		ON CU.Collection_Unit_Key = MCU2.Collection_Unit_Key

	ORDER BY 
		-- 0: Collation_From_Vague_Date_Start DESC, Collation_From_Vague_Date_End DESC, C.Item_Name, M.Number
		-- 1: C.Item_Name, M.Number
		-- 2: M.Number
		CASE @SortOrderIndex WHEN 0 THEN Collation_From_Vague_Date_Start ELSE NULL END DESC,
		CASE @SortOrderIndex WHEN 0 THEN Collation_From_Vague_Date_End ELSE NULL END DESC,
		CASE WHEN @SortOrderIndex IN (0, 1) THEN C.Item_Name ELSE NULL END,
		M.Number
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Collections_Select_ForMovementOut') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Collections_Select_ForMovementOut'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Collections_Select_ForMovementOut TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Collections_Select_ForMovementOut TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Collections_Select_ForMovementOut TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Collections_Select_ForMovementOut TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Collections_Select_ForMovementOut TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Collections_Select_ForMovementOut TO [Dev - JNCC SQL]
END
GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Collections_Select_ForSearchByAccessionNumber]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Collections_Select_ForSearchByAccessionNumber]
GO

CREATE PROCEDURE [dbo].[usp_Collections_Select_ForSearchByAccessionNumber] 
@UserDomainMask INT,
@SessionID CHAR(16),
@SortOrderIndex TINYINT,
@SearchText VARCHAR(30)

AS

--  DESCRIPTION
--  Returns top level Collections based on the search parameter for Accession Number
--
--	PARAMETERS
--	NAME				DESCRIPTION
--	@UserDomainMask		User's Domain Mask restricting which records may be returned
--	@SessionID 			User's SessionID
--	@SortOrderIndex		Index determining Sort Order
--	@SearchText			Text to be used for search
--
--
--  AUTHOR:				Ben Collier, Dorset Software
--  CREATED:			2003-09-12
--
SET NOCOUNT ON

IF @SortOrderIndex = 0
	SELECT C.Collection_Unit_Key AS Item_Key, C.Item_Name, M.Number, M.Number AS Hint
	FROM 
		(COLLECTION C
		INNER JOIN
   		    COLLECTION_UNIT CU 
	   	ON C.Collection_Unit_Key = CU.Collection_Unit_Key
    	   	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
				OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
		)
		LEFT JOIN
			(MOVEMENT_COLLECTION_UNIT MCU
			INNER JOIN
				MOVEMENT_DIRECTION MD
			ON MCU.Movement_Direction_Key = MD.Movement_Direction_Key 
				AND (MD.Outbound = 0)
			INNER JOIN
				MOVEMENT M
			ON MD.Movement_Key = M.Movement_Key AND (M.Movement_Type = 0 OR M.Movement_Type = 1)
			LEFT JOIN
				(CONCEPT CON
				INNER JOIN 
					TERM T
				ON CON.Term_Key = T.Term_Key)
			ON CON.Concept_Key = 'SYSTEM0000000006') --ACCESSION NUMBER
		ON CU.Collection_Unit_Key = MCU.Collection_Unit_Key
	WHERE M.Number LIKE @SearchText + '%'
	ORDER BY Collation_From_Vague_Date_Start DESC, Collation_From_Vague_Date_End DESC, C.Item_Name, M.Number

ELSE IF @SortOrderIndex = 1
	SELECT C.Collection_Unit_Key AS Item_Key, C.Item_Name, M.Number, M.Number AS Hint
	FROM 
		(COLLECTION C
		INNER JOIN
   		    COLLECTION_UNIT CU 
	   	ON C.Collection_Unit_Key = CU.Collection_Unit_Key
    	   	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
				OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
		)
		LEFT JOIN
			(MOVEMENT_COLLECTION_UNIT MCU
			INNER JOIN
				MOVEMENT_DIRECTION MD
			ON MCU.Movement_Direction_Key = MD.Movement_Direction_Key 
				AND (MD.Outbound = 0)
			INNER JOIN
				MOVEMENT M
			ON MD.Movement_Key = M.Movement_Key AND (M.Movement_Type = 0 OR M.Movement_Type = 1)
			LEFT JOIN
				(CONCEPT CON
				INNER JOIN 
					TERM T
				ON CON.Term_Key = T.Term_Key)
			ON CON.Concept_Key = 'SYSTEM0000000006') --ACCESSION NUMBER
		ON CU.Collection_Unit_Key = MCU.Collection_Unit_Key
	WHERE M.Number LIKE @SearchText + '%'
	ORDER BY C.Item_Name, M.Number

ELSE IF @SortOrderIndex = 2
	SELECT C.Collection_Unit_Key AS Item_Key, C.Item_Name, M.Number, M.Number AS Hint
	FROM 
		(COLLECTION C
		INNER JOIN
   		    COLLECTION_UNIT CU 
	   	ON C.Collection_Unit_Key = CU.Collection_Unit_Key
    	   	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
				OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
		)
		LEFT JOIN
			(MOVEMENT_COLLECTION_UNIT MCU
			INNER JOIN
				MOVEMENT_DIRECTION MD
			ON MCU.Movement_Direction_Key = MD.Movement_Direction_Key 
				AND (MD.Outbound = 0)
			INNER JOIN
				MOVEMENT M
			ON MD.Movement_Key = M.Movement_Key AND (M.Movement_Type = 0 OR M.Movement_Type = 1)
			LEFT JOIN
				(CONCEPT CON
				INNER JOIN 
					TERM T
				ON CON.Term_Key = T.Term_Key)
			ON CON.Concept_Key = 'SYSTEM0000000006') --ACCESSION NUMBER
		ON CU.Collection_Unit_Key = MCU.Collection_Unit_Key
	WHERE M.Number LIKE @SearchText + '%'
	ORDER BY M.Number

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Collections_Select_ForSearchByAccessionNumber') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Collections_Select_ForSearchByAccessionNumber'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Collections_Select_ForSearchByAccessionNumber TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Collections_Select_ForSearchByAccessionNumber TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Collections_Select_ForSearchByAccessionNumber TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Collections_Select_ForSearchByAccessionNumber TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Collections_Select_ForSearchByAccessionNumber TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Collections_Select_ForSearchByAccessionNumber TO [Dev - JNCC SQL]
END

GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Collections_Select_ForSearchByAssembledBy]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Collections_Select_ForSearchByAssembledBy]
GO

CREATE PROCEDURE [dbo].[usp_Collections_Select_ForSearchByAssembledBy] 
@UserDomainMask INT,
@SessionID CHAR(16),
@SortOrderIndex TINYINT,
@SearchText VARCHAR(100)

AS

--  DESCRIPTION
--  Returns Collections based on the search parameter for Assembled By
--
--	PARAMETERS
--	NAME				DESCRIPTION
--	@UserDomainMask		User's Domain Mask restricting which records may be returned
--	@SessionID 			User's SessionID
--	@SortOrderIndex		Index determining Sort Order
--	@SearchText			Text to be used for search
--
--
--  AUTHOR:				Ben Collier, Dorset Software
--  CREATED:			2003-09-12
--
SET NOCOUNT ON

IF @SortOrderIndex = 0
	SELECT C.Collection_Unit_Key AS Item_Key, C.Item_Name, M.Number,

	CASE WHEN N.Organisation = 0 THEN 
			dbo.ufn_GetFormattedIndividualByParams(I.Title, I.Initials, I.Forename, I.Surname)
		ELSE 
			CASE WHEN O.Acronym IS NULL THEN 
				O.Full_Name
			ELSE 
				O.Acronym + ', ' + O.Full_Name
			END
		END AS Hint

	FROM 
		(COLLECTION C
		INNER JOIN
   		    COLLECTION_UNIT CU 
	   	ON C.Collection_Unit_Key = CU.Collection_Unit_Key
    	   	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
				OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
		INNER JOIN
			[NAME] N
		ON C.Assembler_Name_Key = N.Name_Key
		)
		LEFT JOIN
			(MOVEMENT_COLLECTION_UNIT MCU
			INNER JOIN
				MOVEMENT_DIRECTION MD
			ON MCU.Movement_Direction_Key = MD.Movement_Direction_Key 
				AND (MD.Outbound = 0)
			INNER JOIN
				MOVEMENT M
			ON MD.Movement_Key = M.Movement_Key AND (M.Movement_Type = 0 OR M.Movement_Type = 1)
			LEFT JOIN
				(CONCEPT CON
				INNER JOIN 
					TERM T
				ON CON.Term_Key = T.Term_Key)
			ON CON.Concept_Key = 'SYSTEM0000000006') --ACCESSION NUMBER
		ON CU.Collection_Unit_Key = MCU.Collection_Unit_Key
	
		LEFT JOIN
			INDIVIDUAL I
		ON N.Name_Key = I.NAME_KEY
			AND (dbo.ufn_GetFormattedIndividualByParams(I.Title, I.Initials, I.Forename, I.Surname) LIKE @SearchText + '%')
		LEFT JOIN
			ORGANISATION O
		ON N.Name_Key = O.NAME_KEY
			AND 
			(CASE WHEN O.Acronym IS NULL THEN 
				O.Full_Name
			ELSE 
				O.Acronym + ', ' + O.Full_Name
			END LIKE @SearchText + '%')
	WHERE (I.NAME_KEY IS NOT NULL) OR (O.NAME_KEY IS NOT NULL) 
	ORDER BY Collation_From_Vague_Date_Start DESC, Collation_From_Vague_Date_End DESC, C.Item_Name, M.Number

ELSE IF @SortOrderIndex = 1
	SELECT C.Collection_Unit_Key AS Item_Key, C.Item_Name, M.Number,

	CASE WHEN N.Organisation = 0 THEN 
			dbo.ufn_GetFormattedIndividualByParams(I.Title, I.Initials, I.Forename, I.Surname)
		ELSE 
			CASE WHEN O.Acronym IS NULL THEN 
				O.Full_Name
			ELSE 
				O.Acronym + ', ' + O.Full_Name
			END
		END AS Hint

	FROM 
		(COLLECTION C
		INNER JOIN
   		    COLLECTION_UNIT CU 
	   	ON C.Collection_Unit_Key = CU.Collection_Unit_Key
    	   	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
				OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
		INNER JOIN
			[NAME] N
		ON C.Assembler_Name_Key = N.Name_Key
		)
		LEFT JOIN
			(MOVEMENT_COLLECTION_UNIT MCU
			INNER JOIN
				MOVEMENT_DIRECTION MD
			ON MCU.Movement_Direction_Key = MD.Movement_Direction_Key 
				AND (MD.Outbound = 0)
			INNER JOIN
				MOVEMENT M
			ON MD.Movement_Key = M.Movement_Key AND (M.Movement_Type = 0 OR M.Movement_Type = 1)
			LEFT JOIN
				(CONCEPT CON
				INNER JOIN 
					TERM T
				ON CON.Term_Key = T.Term_Key)
			ON CON.Concept_Key = 'SYSTEM0000000006') --ACCESSION NUMBER
		ON CU.Collection_Unit_Key = MCU.Collection_Unit_Key
	
		LEFT JOIN
			INDIVIDUAL I
		ON N.Name_Key = I.NAME_KEY
			AND (dbo.ufn_GetFormattedIndividualByParams(I.Title, I.Initials, I.Forename, I.Surname) LIKE @SearchText + '%')
		LEFT JOIN
			ORGANISATION O
		ON N.Name_Key = O.NAME_KEY
			AND 
			(CASE WHEN O.Acronym IS NULL THEN 
				O.Full_Name
			ELSE 
				O.Acronym + ', ' + O.Full_Name
			END LIKE @SearchText + '%')
	WHERE (I.NAME_KEY IS NOT NULL) OR (O.NAME_KEY IS NOT NULL) 
	ORDER BY C.Item_Name, M.Number

ELSE IF @SortOrderIndex = 2
	SELECT C.Collection_Unit_Key AS Item_Key, C.Item_Name, M.Number,

	CASE WHEN N.Organisation = 0 THEN 
			dbo.ufn_GetFormattedIndividualByParams(I.Title, I.Initials, I.Forename, I.Surname)
		ELSE 
			CASE WHEN O.Acronym IS NULL THEN 
				O.Full_Name
			ELSE 
				O.Acronym + ', ' + O.Full_Name
			END
		END AS Hint

	FROM 
		(COLLECTION C
		INNER JOIN
   		    COLLECTION_UNIT CU 
	   	ON C.Collection_Unit_Key = CU.Collection_Unit_Key
    	   	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
				OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
		INNER JOIN
			[NAME] N
		ON C.Assembler_Name_Key = N.Name_Key
		)
		LEFT JOIN
			(MOVEMENT_COLLECTION_UNIT MCU
			INNER JOIN
				MOVEMENT_DIRECTION MD
			ON MCU.Movement_Direction_Key = MD.Movement_Direction_Key 
				AND (MD.Outbound = 0)
			INNER JOIN
				MOVEMENT M
			ON MD.Movement_Key = M.Movement_Key AND (M.Movement_Type = 0 OR M.Movement_Type = 1)
			LEFT JOIN
				(CONCEPT CON
				INNER JOIN 
					TERM T
				ON CON.Term_Key = T.Term_Key)
			ON CON.Concept_Key = 'SYSTEM0000000006') --ACCESSION NUMBER
		ON CU.Collection_Unit_Key = MCU.Collection_Unit_Key
	
		LEFT JOIN
			INDIVIDUAL I
		ON N.Name_Key = I.NAME_KEY
			AND (dbo.ufn_GetFormattedIndividualByParams(I.Title, I.Initials, I.Forename, I.Surname) LIKE @SearchText + '%')
		LEFT JOIN
			ORGANISATION O
		ON N.Name_Key = O.NAME_KEY
			AND 
			(CASE WHEN O.Acronym IS NULL THEN 
				O.Full_Name
			ELSE 
				O.Acronym + ', ' + O.Full_Name
			END LIKE @SearchText + '%')
	WHERE (I.NAME_KEY IS NOT NULL) OR (O.NAME_KEY IS NOT NULL) 
	ORDER BY M.Number


GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Collections_Select_ForSearchByAssembledBy') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Collections_Select_ForSearchByAssembledBy'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Collections_Select_ForSearchByAssembledBy TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Collections_Select_ForSearchByAssembledBy TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Collections_Select_ForSearchByAssembledBy TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Collections_Select_ForSearchByAssembledBy TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Collections_Select_ForSearchByAssembledBy TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Collections_Select_ForSearchByAssembledBy TO [Dev - JNCC SQL]
END

GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Collections_Select_ForSearchByDescription]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Collections_Select_ForSearchByDescription]
GO

CREATE PROCEDURE [dbo].[usp_Collections_Select_ForSearchByDescription] 
@UserDomainMask INT,
@SessionID CHAR(16),
@SortOrderIndex TINYINT,
@SearchText VARCHAR(100)

AS

--  DESCRIPTION
--  Returns Collections based on the search parameter for Description
--
--	PARAMETERS
--	NAME				DESCRIPTION
--	@UserDomainMask		User's Domain Mask restricting which records may be returned
--	@SessionID 			User's SessionID
--	@SortOrderIndex		Index determining Sort Order
--	@SearchText			Text to be used for search
--
--
--  AUTHOR:				Ben Collier, Dorset Software
--  CREATED:			2003-09-12
--
SET NOCOUNT ON

IF @SortOrderIndex = 0
	SELECT C.Collection_Unit_Key AS Item_Key, C.Item_Name, M.Number, MT.Text AS Hint
	FROM 
		(COLLECTION C
		INNER JOIN
   		    COLLECTION_UNIT CU 
	   	ON C.Collection_Unit_Key = CU.Collection_Unit_Key
    	   	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
				OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
		)
		LEFT JOIN
			(MOVEMENT_COLLECTION_UNIT MCU
			INNER JOIN
				MOVEMENT_DIRECTION MD
			ON MCU.Movement_Direction_Key = MD.Movement_Direction_Key 
				AND (MD.Outbound = 0)
			INNER JOIN
				MOVEMENT M
			ON MD.Movement_Key = M.Movement_Key AND (M.Movement_Type = 0 OR M.Movement_Type = 1)
			LEFT JOIN
				(CONCEPT CON
				INNER JOIN 
					TERM T
				ON CON.Term_Key = T.Term_Key)
			ON CON.Concept_Key = 'SYSTEM0000000006') --ACCESSION NUMBER
		ON CU.Collection_Unit_Key = MCU.Collection_Unit_Key

		INNER JOIN
			METADATA MT
		ON C.Collection_Unit_Key = MT.Record_Key
			AND MT.MetaData_Type_Key = 'SYSTEM0000000003'  --DESCRIPTION
			AND MT.Text LIKE @SearchText + '%'
	ORDER BY Collation_From_Vague_Date_Start DESC, Collation_From_Vague_Date_End DESC, C.Item_Name, M.Number

ELSE IF @SortOrderIndex = 1
	SELECT C.Collection_Unit_Key AS Item_Key, C.Item_Name, M.Number, MT.Text AS Hint
	FROM 
		(COLLECTION C
		INNER JOIN
   		    COLLECTION_UNIT CU 
	   	ON C.Collection_Unit_Key = CU.Collection_Unit_Key
    	   	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
				OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
		)
		LEFT JOIN
			(MOVEMENT_COLLECTION_UNIT MCU
			INNER JOIN
				MOVEMENT_DIRECTION MD
			ON MCU.Movement_Direction_Key = MD.Movement_Direction_Key 
				AND (MD.Outbound = 0)
			INNER JOIN
				MOVEMENT M
			ON MD.Movement_Key = M.Movement_Key AND (M.Movement_Type = 0 OR M.Movement_Type = 1)
			LEFT JOIN
				(CONCEPT CON
				INNER JOIN 
					TERM T
				ON CON.Term_Key = T.Term_Key)
			ON CON.Concept_Key = 'SYSTEM0000000006') --ACCESSION NUMBER
		ON CU.Collection_Unit_Key = MCU.Collection_Unit_Key

		INNER JOIN
			METADATA MT
		ON C.Collection_Unit_Key = MT.Record_Key
			AND MT.MetaData_Type_Key = 'SYSTEM0000000003'
	ORDER BY C.Item_Name, M.Number

ELSE IF @SortOrderIndex = 2
	SELECT C.Collection_Unit_Key AS Item_Key, C.Item_Name, M.Number, MT.Text AS Hint
	FROM 
		(COLLECTION C
		INNER JOIN
   		    COLLECTION_UNIT CU 
	   	ON C.Collection_Unit_Key = CU.Collection_Unit_Key
    	   	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
				OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
		)
		LEFT JOIN
			(MOVEMENT_COLLECTION_UNIT MCU
			INNER JOIN
				MOVEMENT_DIRECTION MD
			ON MCU.Movement_Direction_Key = MD.Movement_Direction_Key 
				AND (MD.Outbound = 0)
			INNER JOIN
				MOVEMENT M
			ON MD.Movement_Key = M.Movement_Key AND (M.Movement_Type = 0 OR M.Movement_Type = 1)
			LEFT JOIN
				(CONCEPT CON
				INNER JOIN 
					TERM T
				ON CON.Term_Key = T.Term_Key)
			ON CON.Concept_Key = 'SYSTEM0000000006') --ACCESSION NUMBER
		ON CU.Collection_Unit_Key = MCU.Collection_Unit_Key

		INNER JOIN
			METADATA MT
		ON C.Collection_Unit_Key = MT.Record_Key
			AND MT.MetaData_Type_Key = 'SYSTEM0000000003'
			AND MT.Text LIKE @SearchText + '%'
	ORDER BY M.Number

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Collections_Select_ForSearchByDescription') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Collections_Select_ForSearchByDescription'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Collections_Select_ForSearchByDescription TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Collections_Select_ForSearchByDescription TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Collections_Select_ForSearchByDescription TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Collections_Select_ForSearchByDescription TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Collections_Select_ForSearchByDescription TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Collections_Select_ForSearchByDescription TO [Dev - JNCC SQL]
END

GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Collections_Select_ForSearchByGeographicInformation]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Collections_Select_ForSearchByGeographicInformation]
GO

CREATE PROCEDURE [dbo].[usp_Collections_Select_ForSearchByGeographicInformation] 
@UserDomainMask INT,
@SessionID CHAR(16),
@SortOrderIndex TINYINT,
@SearchText VARCHAR(100)

AS

--  DESCRIPTION
--  Returns Collections based on the search parameter for Geographic Information
--
--	PARAMETERS
--	NAME				DESCRIPTION
--	@UserDomainMask		User's Domain Mask restricting which records may be returned
--	@SessionID 			User's SessionID
--	@SortOrderIndex		Index determining Sort Order
--	@SearchText			Text to be used for search
--
--
--  AUTHOR:				Ben Collier, Dorset Software
--  CREATED:			2003-09-12
--
SET NOCOUNT ON

IF @SortOrderIndex = 0
	SELECT C.Collection_Unit_Key AS Item_Key, C.Item_Name, M.Number, MT.Text AS Hint
	FROM 
		(COLLECTION C
		INNER JOIN
   		    COLLECTION_UNIT CU 
	   	ON C.Collection_Unit_Key = CU.Collection_Unit_Key
    	   	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
				OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
		)
		LEFT JOIN
			(MOVEMENT_COLLECTION_UNIT MCU
			INNER JOIN
				MOVEMENT_DIRECTION MD
			ON MCU.Movement_Direction_Key = MD.Movement_Direction_Key 
				AND (MD.Outbound = 0)
			INNER JOIN
				MOVEMENT M
			ON MD.Movement_Key = M.Movement_Key AND (M.Movement_Type = 0 OR M.Movement_Type = 1)
			LEFT JOIN
				(CONCEPT CON
				INNER JOIN 
					TERM T
				ON CON.Term_Key = T.Term_Key)
			ON CON.Concept_Key = 'SYSTEM0000000006') --ACCESSION NUMBER
		ON CU.Collection_Unit_Key = MCU.Collection_Unit_Key

		INNER JOIN
			METADATA MT
		ON C.Collection_Unit_Key = MT.Record_Key
			AND MT.MetaData_Type_Key = 'SYSTEM0000000004'
			AND MT.TEXT LIKE @SearchText + '%'
	ORDER BY Collation_From_Vague_Date_Start DESC, Collation_From_Vague_Date_End DESC, C.Item_Name, M.Number

ELSE IF @SortOrderIndex = 1
	SELECT C.Collection_Unit_Key AS Item_Key, C.Item_Name, M.Number, MT.Text AS Hint
	FROM 
		(COLLECTION C
		INNER JOIN
   		    COLLECTION_UNIT CU 
	   	ON C.Collection_Unit_Key = CU.Collection_Unit_Key
    	   	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
				OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
		)
		LEFT JOIN
			(MOVEMENT_COLLECTION_UNIT MCU
			INNER JOIN
				MOVEMENT_DIRECTION MD
			ON MCU.Movement_Direction_Key = MD.Movement_Direction_Key 
				AND (MD.Outbound = 0)
			INNER JOIN
				MOVEMENT M
			ON MD.Movement_Key = M.Movement_Key AND (M.Movement_Type = 0 OR M.Movement_Type = 1)
			LEFT JOIN
				(CONCEPT CON
				INNER JOIN 
					TERM T
				ON CON.Term_Key = T.Term_Key)
			ON CON.Concept_Key = 'SYSTEM0000000006') --ACCESSION NUMBER
		ON CU.Collection_Unit_Key = MCU.Collection_Unit_Key

		INNER JOIN
			METADATA MT
		ON C.Collection_Unit_Key = MT.Record_Key
			AND MT.MetaData_Type_Key = 'SYSTEM0000000004'
			AND MT.TEXT LIKE @SearchText + '%'
	ORDER BY C.Item_Name, M.Number

ELSE IF @SortOrderIndex = 2
	SELECT C.Collection_Unit_Key AS Item_Key, C.Item_Name, M.Number, MT.Text AS Hint
	FROM 
		(COLLECTION C
		INNER JOIN
   		    COLLECTION_UNIT CU 
	   	ON C.Collection_Unit_Key = CU.Collection_Unit_Key
    	   	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
				OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
		)
		LEFT JOIN
			(MOVEMENT_COLLECTION_UNIT MCU
			INNER JOIN
				MOVEMENT_DIRECTION MD
			ON MCU.Movement_Direction_Key = MD.Movement_Direction_Key 
				AND (MD.Outbound = 0)
			INNER JOIN
				MOVEMENT M
			ON MD.Movement_Key = M.Movement_Key AND (M.Movement_Type = 0 OR M.Movement_Type = 1)
			LEFT JOIN
				(CONCEPT CON
				INNER JOIN 
					TERM T
				ON CON.Term_Key = T.Term_Key)
			ON CON.Concept_Key = 'SYSTEM0000000006') --ACCESSION NUMBER
		ON CU.Collection_Unit_Key = MCU.Collection_Unit_Key

		INNER JOIN
			METADATA MT
		ON C.Collection_Unit_Key = MT.Record_Key
			AND MT.MetaData_Type_Key = 'SYSTEM0000000004'
			AND MT.TEXT LIKE @SearchText + '%'
	ORDER BY M.Number

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Collections_Select_ForSearchByGeographicInformation') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Collections_Select_ForSearchByGeographicInformation'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Collections_Select_ForSearchByGeographicInformation TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Collections_Select_ForSearchByGeographicInformation TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Collections_Select_ForSearchByGeographicInformation TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Collections_Select_ForSearchByGeographicInformation TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Collections_Select_ForSearchByGeographicInformation TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Collections_Select_ForSearchByGeographicInformation TO [Dev - JNCC SQL]
END

GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Collections_Select_ForSearchByName]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Collections_Select_ForSearchByName]
GO

CREATE PROCEDURE [dbo].[usp_Collections_Select_ForSearchByName] 
@UserDomainMask INT,
@SessionID CHAR(16),
@SortOrderIndex TINYINT,
@SearchText VARCHAR(150)

AS

--  DESCRIPTION
--  Returns Collections based on the search parameter for Collection Name
--
--	PARAMETERS
--	NAME				DESCRIPTION
--	@UserDomainMask		User's Domain Mask restricting which records may be returned
--	@SessionID 			User's SessionID
--	@SortOrderIndex		Index determining Sort Order
--	@SearchText			Text to be used for search
--
--
--  AUTHOR:				Ben Collier, Dorset Software
--  CREATED:			2003-09-12
--
SET NOCOUNT ON

IF @SortOrderIndex = 0
	SELECT C.Collection_Unit_Key AS Item_Key, C.Item_Name, M.Number, C.Item_Name AS Hint
	FROM 
		(COLLECTION C
		INNER JOIN
   		    COLLECTION_UNIT CU 
	   	ON C.Collection_Unit_Key = CU.Collection_Unit_Key
    	   	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
				OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
			AND (C.Item_Name LIKE @SearchText + '%')
		)
		LEFT JOIN
			(MOVEMENT_COLLECTION_UNIT MCU
			INNER JOIN
				MOVEMENT_DIRECTION MD
			ON MCU.Movement_Direction_Key = MD.Movement_Direction_Key 
				AND (MD.Outbound = 0)
			INNER JOIN
				MOVEMENT M
			ON MD.Movement_Key = M.Movement_Key AND (M.Movement_Type = 0 OR M.Movement_Type = 1)
			LEFT JOIN
				(CONCEPT CON
				INNER JOIN 
					TERM T
				ON CON.Term_Key = T.Term_Key)
			ON CON.Concept_Key = 'SYSTEM0000000006') --ACCESSION NUMBER
		ON CU.Collection_Unit_Key = MCU.Collection_Unit_Key
	ORDER BY Collation_From_Vague_Date_Start DESC, Collation_From_Vague_Date_End DESC, C.Item_Name, M.Number

ELSE IF @SortOrderIndex = 1
	SELECT C.Collection_Unit_Key AS Item_Key, C.Item_Name, M.Number, C.Item_Name AS Hint
	FROM 
		(COLLECTION C
		INNER JOIN
   		    COLLECTION_UNIT CU 
	   	ON C.Collection_Unit_Key = CU.Collection_Unit_Key
    	   	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
				OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
			AND (C.Item_Name LIKE @SearchText + '%')
		)
		LEFT JOIN
			(MOVEMENT_COLLECTION_UNIT MCU
			INNER JOIN
				MOVEMENT_DIRECTION MD
			ON MCU.Movement_Direction_Key = MD.Movement_Direction_Key 
				AND (MD.Outbound = 0)
			INNER JOIN
				MOVEMENT M
			ON MD.Movement_Key = M.Movement_Key AND (M.Movement_Type = 0 OR M.Movement_Type = 1)
			LEFT JOIN
				(CONCEPT CON
				INNER JOIN 
					TERM T
				ON CON.Term_Key = T.Term_Key)
			ON CON.Concept_Key = 'SYSTEM0000000006') --ACCESSION NUMBER
		ON CU.Collection_Unit_Key = MCU.Collection_Unit_Key
	ORDER BY C.Item_Name, M.Number

ELSE IF @SortOrderIndex = 2
	SELECT C.Collection_Unit_Key AS Item_Key, C.Item_Name, M.Number, C.Item_Name AS Hint
	FROM 
		(COLLECTION C
		INNER JOIN
   		    COLLECTION_UNIT CU 
	   	ON C.Collection_Unit_Key = CU.Collection_Unit_Key
    	   	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
				OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
			AND (C.Item_Name LIKE @SearchText + '%')
		)
		LEFT JOIN
			(MOVEMENT_COLLECTION_UNIT MCU
			INNER JOIN
				MOVEMENT_DIRECTION MD
			ON MCU.Movement_Direction_Key = MD.Movement_Direction_Key 
				AND (MD.Outbound = 0)
			INNER JOIN
				MOVEMENT M
			ON MD.Movement_Key = M.Movement_Key AND (M.Movement_Type = 0 OR M.Movement_Type = 1)
			LEFT JOIN
				(CONCEPT CON
				INNER JOIN 
					TERM T
				ON CON.Term_Key = T.Term_Key)
			ON CON.Concept_Key = 'SYSTEM0000000006') --ACCESSION NUMBER
		ON CU.Collection_Unit_Key = MCU.Collection_Unit_Key
	ORDER BY M.Number

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Collections_Select_ForSearchByName') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Collections_Select_ForSearchByName'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Collections_Select_ForSearchByName TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Collections_Select_ForSearchByName TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Collections_Select_ForSearchByName TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Collections_Select_ForSearchByName TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Collections_Select_ForSearchByName TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Collections_Select_ForSearchByName TO [Dev - JNCC SQL]
END

GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Collections_Select_ForSearchByTopic]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Collections_Select_ForSearchByTopic]
GO

CREATE PROCEDURE [dbo].[usp_Collections_Select_ForSearchByTopic] 
@UserDomainMask INT,
@SessionID CHAR(16),
@SortOrderIndex TINYINT,
@SearchText VARCHAR(200)

AS

--  DESCRIPTION
--  Returns Collections based on the search parameter for the Topic
--
--	PARAMETERS
--	NAME				DESCRIPTION
--	@UserDomainMask		User's Domain Mask restricting which records may be returned
--	@SessionID 			User's SessionID
--	@SortOrderIndex		Index determining Sort Order
--	@SearchText			Text to be used for search
--
--
--  AUTHOR:				Ben Collier, Dorset Software
--  CREATED:			2003-09-12
--
SET NOCOUNT ON

IF @SortOrderIndex = 0
	SELECT C.Collection_Unit_Key AS Item_Key, C.Item_Name, M.Number, C.Topic AS Hint
	FROM 
		(COLLECTION C
		INNER JOIN
   		    COLLECTION_UNIT CU 
	   	ON C.Collection_Unit_Key = CU.Collection_Unit_Key
    	   	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
				OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
			AND (C.Topic LIKE @SearchText + '%')
		)
		LEFT JOIN
			(MOVEMENT_COLLECTION_UNIT MCU
			INNER JOIN
				MOVEMENT_DIRECTION MD
			ON MCU.Movement_Direction_Key = MD.Movement_Direction_Key 
				AND (MD.Outbound = 0)
			INNER JOIN
				MOVEMENT M
			ON MD.Movement_Key = M.Movement_Key AND (M.Movement_Type = 0 OR M.Movement_Type = 1)
			LEFT JOIN
				(CONCEPT CON
				INNER JOIN 
					TERM T
				ON CON.Term_Key = T.Term_Key)
			ON CON.Concept_Key = 'SYSTEM0000000006') --ACCESSION NUMBER
		ON CU.Collection_Unit_Key = MCU.Collection_Unit_Key
	ORDER BY Collation_From_Vague_Date_Start DESC, Collation_From_Vague_Date_End DESC, C.Item_Name, M.Number

ELSE IF @SortOrderIndex = 1
	SELECT C.Collection_Unit_Key AS Item_Key, C.Item_Name, M.Number, C.Topic AS Hint
	FROM 
		(COLLECTION C
		INNER JOIN
   		    COLLECTION_UNIT CU 
	   	ON C.Collection_Unit_Key = CU.Collection_Unit_Key
    	   	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
				OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
			AND (C.Topic LIKE @SearchText + '%')
		)
		LEFT JOIN
			(MOVEMENT_COLLECTION_UNIT MCU
			INNER JOIN
				MOVEMENT_DIRECTION MD
			ON MCU.Movement_Direction_Key = MD.Movement_Direction_Key 
				AND (MD.Outbound = 0)
			INNER JOIN
				MOVEMENT M
			ON MD.Movement_Key = M.Movement_Key AND (M.Movement_Type = 0 OR M.Movement_Type = 1)
			LEFT JOIN
				(CONCEPT CON
				INNER JOIN 
					TERM T
				ON CON.Term_Key = T.Term_Key)
			ON CON.Concept_Key = 'SYSTEM0000000006') --ACCESSION NUMBER
		ON CU.Collection_Unit_Key = MCU.Collection_Unit_Key
	ORDER BY C.Item_Name, M.Number

ELSE IF @SortOrderIndex = 2
	SELECT C.Collection_Unit_Key AS Item_Key, C.Item_Name, M.Number, C.Topic AS Hint
	FROM 
		(COLLECTION C
		INNER JOIN
   		    COLLECTION_UNIT CU 
	   	ON C.Collection_Unit_Key = CU.Collection_Unit_Key
    	   	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
				OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
			AND (C.Topic LIKE @SearchText + '%')
		)
		LEFT JOIN
			(MOVEMENT_COLLECTION_UNIT MCU
			INNER JOIN
				MOVEMENT_DIRECTION MD
			ON MCU.Movement_Direction_Key = MD.Movement_Direction_Key 
				AND (MD.Outbound = 0)
			INNER JOIN
				MOVEMENT M
			ON MD.Movement_Key = M.Movement_Key AND (M.Movement_Type = 0 OR M.Movement_Type = 1)
			LEFT JOIN
				(CONCEPT CON
				INNER JOIN 
					TERM T
				ON CON.Term_Key = T.Term_Key)
			ON CON.Concept_Key = 'SYSTEM0000000006') --ACCESSION NUMBER
		ON CU.Collection_Unit_Key = MCU.Collection_Unit_Key
	ORDER BY M.Number

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Collections_Select_ForSearchByTopic') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Collections_Select_ForSearchByTopic'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Collections_Select_ForSearchByTopic TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Collections_Select_ForSearchByTopic TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Collections_Select_ForSearchByTopic TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Collections_Select_ForSearchByTopic TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Collections_Select_ForSearchByTopic TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Collections_Select_ForSearchByTopic TO [Dev - JNCC SQL]
END

GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Collections_Select_ForSpecimen]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Collections_Select_ForSpecimen]
GO

/*===========================================================================*\
  Description:	Returns Collections for a specified Specimen.

  Parameters:
	@ParentKey 	Only the records associated with the parent key are returned
	@SortOrderIndex	Index determining Sort Order
	@UserDomainMask	User's Domain Mask restricting which records may be returned
	@SessionID 	User's SessionID

  Created:	September 2003

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Collections_Select_ForSpecimen] 
	@UserDomainMask INT,
	@SessionID CHAR(16),
	@SortOrderIndex TINYINT,
	@ParentKey CHAR(16)
AS

SET NOCOUNT ON

	SELECT		C.Collection_Unit_Key AS Item_Key, C.Item_Name, M.Number, C.Collection_Unit_Key AS Join_Key

	FROM 		Specimen_Unit SU
	INNER JOIN	Collection C
		ON SU.Parent_Collection_Collection_Unit_Key = C.Collection_Unit_Key 
		AND SU.Collection_Unit_Key = @ParentKey

	INNER JOIN	Collection_Unit CU 
		ON C.Collection_Unit_Key = CU.Collection_Unit_Key
		AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
			OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))

	LEFT JOIN	(Movement_Collection_Unit MCU
			INNER JOIN	Movement_Direction MD
				ON MCU.Movement_Direction_Key = MD.Movement_Direction_Key 
				AND (MD.Outbound = 0)

			INNER JOIN	Movement M ON MD.Movement_Key = M.Movement_Key AND M.Movement_Type IN (0, 1)
			LEFT JOIN	(Concept CON
					INNER JOIN Term T ON CON.Term_Key = T.Term_Key
					)
				ON CON.Concept_Key = 'SYSTEM0000000006' --ACCESSION NUMBER
			)
		ON CU.Collection_Unit_Key = MCU.Collection_Unit_Key

	ORDER BY
		-- 0: Collation_From_Vague_Date_Start DESC, Collation_From_Vague_Date_End DESC, C.Item_Name, M.Number
		-- 1: C.Item_Name, M.Number
		-- 2: M.Number
		CASE @SortOrderIndex WHEN 0 THEN Collation_From_Vague_Date_Start ELSE NULL END DESC,
		CASE @SortOrderIndex WHEN 0 THEN Collation_From_Vague_Date_End ELSE NULL END DESC,
		CASE WHEN @SortOrderIndex IN (0, 1) THEN C.Item_Name ELSE NULL END,
		M.Number
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Collections_Select_ForSpecimen') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Collections_Select_ForSpecimen'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Collections_Select_ForSpecimen TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Collections_Select_ForSpecimen TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Collections_Select_ForSpecimen TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Collections_Select_ForSpecimen TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Collections_Select_ForSpecimen TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Collections_Select_ForSpecimen TO [Dev - JNCC SQL]
END
GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Collections_Select_ForStore]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Collections_Select_ForStore]
GO

/*===========================================================================*\
  Description:	Returns Collections for a specified Store.

  Parameters:	
	@ParentKey 	Only the records associated with the parent key are returned
	@SortOrderIndex	Index determining Sort Order
	@UserDomainMask	User's Domain Mask restricting which records may be returned
	@SessionID 	User's SessionID

  Created:	September 2003

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Collections_Select_ForStore] 
	@UserDomainMask INT,
	@SessionID CHAR(16),
	@SortOrderIndex TINYINT,
	@ParentKey CHAR(16)
AS

SET NOCOUNT ON

	DECLARE @Collection TABLE
	(
		[Collection_Unit_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
		[Item_Name] [varchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[Number] [varchar] (30) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[Collation_From_Vague_Date_Start] [int] NULL,
		[Collation_From_Vague_Date_End] [int] NULL
	)

	INSERT INTO @Collection (Collection_Unit_Key)
	SELECT DISTINCT C.Collection_Unit_Key
	FROM		Collection_Unit CU
	INNER JOIN 	Specimen_Unit SU 
		ON CU.Collection_Unit_Key = SU.Collection_Unit_Key
		AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
			OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
		AND CU.Current_Container_Collection_Unit_Key = @ParentKey

	INNER JOIN 	Collection C ON SU.Parent_Collection_Collection_Unit_Key = C.Collection_Unit_Key

	UPDATE	CList
	SET 	Item_Name = C.Item_Name,
		Number = M.Number,
		Collation_From_Vague_Date_Start = C.Collation_From_Vague_Date_Start,
		Collation_From_Vague_Date_End = C.Collation_From_Vague_Date_End

	FROM 		@Collection CList INNER JOIN Collection C ON CList.Collection_Unit_Key = C.Collection_Unit_Key
	LEFT JOIN	(Movement_Collection_Unit MCU
			INNER JOIN	Movement_Direction MD ON MCU.Movement_Direction_Key = MD.Movement_Direction_Key AND (MD.Outbound = 0)
			INNER JOIN	Movement M ON MD.Movement_Key = M.Movement_Key AND M.Movement_Type IN (0, 1)
			LEFT JOIN	(Concept CON
					INNER JOIN Term T ON CON.Term_Key = T.Term_Key
					)
				ON CON.Concept_Key = 'SYSTEM0000000006' --ACCESSION NUMBER
			)
		ON C.Collection_Unit_Key = MCU.Collection_Unit_Key

IF @SortOrderIndex = 0
	SELECT	Collection_Unit_Key AS Item_Key, Collection_Unit_Key AS Join_Key, Item_Name, Number
	FROM 	@Collection
	ORDER BY Collation_From_Vague_Date_Start DESC, Collation_From_Vague_Date_End DESC, Item_Name, Number
ELSE 
IF @SortOrderIndex = 1
	SELECT	Collection_Unit_Key AS Item_Key, Collection_Unit_Key AS Join_Key, Item_Name, Number
	FROM 	@Collection
	ORDER BY Item_Name, Number
ELSE 
IF @SortOrderIndex = 2
	SELECT	Collection_Unit_Key AS Item_Key, Collection_Unit_Key AS Join_Key, Item_Name, Number
	FROM 	@Collection
	ORDER BY Number
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Collections_Select_ForStore') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Collections_Select_ForStore'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Collections_Select_ForStore TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Collections_Select_ForStore TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Collections_Select_ForStore TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Collections_Select_ForStore TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Collections_Select_ForStore TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Collections_Select_ForStore TO [Dev - JNCC SQL]
END
GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Collections_Select_ForTopLevel]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Collections_Select_ForTopLevel]
GO

CREATE PROCEDURE [dbo].[usp_Collections_Select_ForTopLevel] 
@UserDomainMask INT,
@SessionID CHAR(16),
@SortOrderIndex TINYINT,
@Key CHAR(16) = NULL

AS

--  DESCRIPTION
--  Returns top level Collections data to the CollectionsBrowser
--
--	PARAMETERS
--	NAME				DESCRIPTION
--	@Key 				Optional Key. When specified, only the single top level record is returned with that key
--	@SortOrderIndex		Index determining Sort Order
--	@UserDomainMask		User's Domain Mask restricting which records may be returned
--	@SessionID 			User's SessionID
--
--
--  AUTHOR:				Ben Collier, Dorset Software
--  CREATED:			2003-08-12
--
SET NOCOUNT ON

-- Create  a table to hold the items we are looking for
DECLARE @Search TABLE (ItemKey CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS PRIMARY KEY)

IF @Key IS NOT NULL
		INSERT INTO @Search VALUES (@Key)
ELSE IF object_id('tempdb..#TempFilter') is not null
	INSERT INTO @Search SELECT DISTINCT ItemKey FROM #TempFilter
ELSE
	INSERT INTO @Search SELECT Collection_Unit_Key FROM Collection

IF @SortOrderIndex = 0
	SELECT C.Collection_Unit_Key AS Item_Key, C.Item_Name, M.Number
	FROM 
		(COLLECTION C
		INNER JOIN
  		    COLLECTION_UNIT CU 
  		ON C.Collection_Unit_Key = CU.Collection_Unit_Key
      		AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
				OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
		)
		LEFT JOIN
			(MOVEMENT_COLLECTION_UNIT MCU
			INNER JOIN
				MOVEMENT_DIRECTION MD
			ON MCU.Movement_Direction_Key = MD.Movement_Direction_Key 
				AND (MD.Outbound = 0)
			INNER JOIN
				MOVEMENT M
			ON MD.Movement_Key = M.Movement_Key AND (M.Movement_Type = 0 OR M.Movement_Type = 1)
			LEFT JOIN
				(CONCEPT CON
				INNER JOIN 
					TERM T
				ON CON.Term_Key = T.Term_Key)
			ON CON.Concept_Key = 'SYSTEM0000000006') --ACCESSION NUMBER
		ON CU.Collection_Unit_Key = MCU.Collection_Unit_Key
		INNER JOIN @Search S ON S.ItemKey=CU.Collection_Unit_Key
	ORDER BY Collation_From_Vague_Date_Start DESC, Collation_From_Vague_Date_End DESC, C.Item_Name, M.Number

ELSE IF @SortOrderIndex = 1
	SELECT C.Collection_Unit_Key AS Item_Key, C.Item_Name, M.Number
	FROM 
		(COLLECTION C
		INNER JOIN
  		    COLLECTION_UNIT CU 
  		ON C.Collection_Unit_Key = CU.Collection_Unit_Key
      		AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
				OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
		)
		LEFT JOIN
			(MOVEMENT_COLLECTION_UNIT MCU
			INNER JOIN
				MOVEMENT_DIRECTION MD
			ON MCU.Movement_Direction_Key = MD.Movement_Direction_Key 
				AND (MD.Outbound = 0)
			INNER JOIN
				MOVEMENT M
			ON MD.Movement_Key = M.Movement_Key AND (M.Movement_Type = 0 OR M.Movement_Type = 1)
			LEFT JOIN
				(CONCEPT CON
				INNER JOIN 
					TERM T
				ON CON.Term_Key = T.Term_Key)
			ON CON.Concept_Key = 'SYSTEM0000000006') --ACCESSION NUMBER
		ON CU.Collection_Unit_Key = MCU.Collection_Unit_Key
		INNER JOIN @Search S ON S.ItemKey=CU.Collection_Unit_Key
	ORDER BY C.Item_Name, M.Number

ELSE IF @SortOrderIndex = 2
	SELECT C.Collection_Unit_Key AS Item_Key, C.Item_Name, M.Number
	FROM 
		(COLLECTION C
		INNER JOIN
  		    COLLECTION_UNIT CU 
  		ON C.Collection_Unit_Key = CU.Collection_Unit_Key
      		AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
				OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
		)
		LEFT JOIN
			(MOVEMENT_COLLECTION_UNIT MCU
			INNER JOIN
				MOVEMENT_DIRECTION MD
			ON MCU.Movement_Direction_Key = MD.Movement_Direction_Key 
				AND (MD.Outbound = 0)
			INNER JOIN
				MOVEMENT M
			ON MD.Movement_Key = M.Movement_Key AND (M.Movement_Type = 0 OR M.Movement_Type = 1)
			LEFT JOIN
				(CONCEPT CON
				INNER JOIN 
					TERM T
				ON CON.Term_Key = T.Term_Key)
			ON CON.Concept_Key = 'SYSTEM0000000006') --ACCESSION NUMBER
		ON CU.Collection_Unit_Key = MCU.Collection_Unit_Key
		INNER JOIN @Search S ON S.ItemKey=CU.Collection_Unit_Key
	ORDER BY M.Number

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Collections_Select_ForTopLevel') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Collections_Select_ForTopLevel'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Collections_Select_ForTopLevel TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Collections_Select_ForTopLevel TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Collections_Select_ForTopLevel TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Collections_Select_ForTopLevel TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Collections_Select_ForTopLevel TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Collections_Select_ForTopLevel TO [Dev - JNCC SQL]
END

GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Collections_Select_ForValuation]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Collections_Select_ForValuation]
GO

/*===========================================================================*\
  Description:	Returns Collections data for a given Valuations.

  Parameters:
	@SortOrderIndex	Index determining Sort Order
	@ParentKey 	Only the records associated with the parent key are returned
	@UserID		Name_Key of current user

  Created:	August 2003

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Collections_Select_ForValuation] 
	@SortOrderIndex TINYINT,
	@ParentKey CHAR(16),
	@UserID CHAR(16)
AS

SET NOCOUNT ON

	SELECT 		C.Collection_Unit_Key AS Item_Key, C.Item_Name, M.Number, 
			Collation_From_Vague_Date_Start, Collation_From_Vague_Date_End,
			CUV.Collection_Unit_Valuation_Key AS Join_Key

	FROM 		Collection_Unit_Valuation CUV
	INNER JOIN	[User] U ON U.Name_Key = @UserID 
	INNER JOIN	Collection C ON CUV.Collection_Unit_Key = C.Collection_Unit_Key AND CUV.Valuation_Key = @ParentKey
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
		ON C.Collection_Unit_Key = MCU.Collection_Unit_Key

	WHERE U.Allow_Finance = 1

	ORDER BY 
		-- 0: Collation_From_Vague_Date_Start DESC, Collation_From_Vague_Date_End DESC, C.Item_Name, M.Number
		-- 1: C.Item_Name, M.Number
		-- 2: M.Number
		CASE @SortOrderIndex WHEN 0 THEN Collation_From_Vague_Date_Start ELSE NULL END DESC,
		CASE @SortOrderIndex WHEN 0 THEN Collation_From_Vague_Date_End ELSE NULL END DESC,
		CASE WHEN @SortOrderIndex IN (0, 1) THEN C.Item_Name ELSE NULL END,
		M.Number
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Collections_Select_ForValuation') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Collections_Select_ForValuation'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Collections_Select_ForValuation TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Collections_Select_ForValuation TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Collections_Select_ForValuation TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Collections_Select_ForValuation TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Collections_Select_ForValuation TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Collections_Select_ForValuation TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_CollectionUnitData_Insert]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_CollectionUnitData_Insert]
GO

/*===========================================================================*\
  Description:	Inserts a record into the Collection_Unit_Data table.
		The Collection_Unit_Data table hold descriptor and measurement
		information.

  Parameters:	@Key
		@CollectionUnitKey
		@AppliesTo
		@MethodConceptKey
		@Duration
		@Accuracy
		@ParameterConceptKey
		@UnitConceptKey
		@Value
		@UpperValue
		@IsDescriptor
		@SessionID

  Created:	September 2003

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_CollectionUnitData_Insert]
	-- Required by both Measurements and Descriptors updates.
	@Key char(16) OUTPUT,
	@IsDescriptor bit,
	@ParameterConceptKey char(16),
	@AppliesTo varchar(50),
	@Value varchar(50) = NULL,
	@CollectionUnitKey char(16),
	@SessionID char(16),
	-- Only required for the Measurements update.
	@UpperValue varchar(50) = NULL,
	@MethodConceptKey char(16) = NULL,
	@Duration varchar(50) = NULL,
	@Accuracy varchar(50) = NULL,
	@UnitConceptKey char(16) = NULL
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	/*-------------------------------------------------------------*\
	  Get a new key.
	\*-------------------------------------------------------------*/
	EXECUTE spNextKey 'Collection_Unit_Data', @Key OUTPUT

	BEGIN TRANSACTION
		/*----------------------------------------------------------------------------------*\
		  If we are inserting measurement data, more information needs to inserted than if 
		  we are inserting descriptor data. Hence, there are two different insert statements.
		\*----------------------------------------------------------------------------------*/
		IF @IsDescriptor = 1
			INSERT INTO Collection_Unit_Data (
				Collection_Unit_Data_Key, Collection_Unit_Key, Applies_To, 
				Parameter_Concept_Key, Lower_Value, Is_Descriptor, Entered_Session_ID
			) VALUES (
				@Key, @CollectionUnitKey, @AppliesTo, @ParameterConceptKey, 
				IsNull(@Value, ' '), @IsDescriptor, @SessionID
			)
		ELSE
			INSERT INTO Collection_Unit_Data (
				Collection_Unit_Data_Key, Collection_Unit_Key, Applies_To,
				Method_Concept_Key, Duration, Accuracy, Parameter_Concept_Key,
				Unit_Concept_Key, Lower_Value, Upper_Value, Is_Descriptor,
				Entered_Session_ID
			) VALUES (
				@Key, @CollectionUnitKey, @AppliesTo, @MethodConceptKey, @Duration,
				@Accuracy, @ParameterConceptKey, @UnitConceptKey, IsNull(@Value, ' '),
				@UpperValue, @IsDescriptor, @SessionID
			)
		IF @@Error <> 0 GOTO RollbackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_CollectionUnitData_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_CollectionUnitData_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitData_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_CollectionUnitData_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_CollectionUnitData_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_CollectionUnitData_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitData_Insert TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_CollectionUnitData_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_CollectionUnitData_Update]
GO

/*===========================================================================*\
  Description:	Updates a record in the Collection_Unit_Data table.
		The Collection_Unit_Data table hold descriptor and measurement
		information.

  Parameters:	@Key
		@CollectionUnitKey
		@AppliesTo
		@MethodConceptKey
		@Duration
		@Accuracy
		@ParameterConceptKey
		@UnitConceptKey
		@Value
		@UpperValue
		@IsDescriptor
		@SessionID
		@Timestamp

  Created:	September 2003

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_CollectionUnitData_Update]
	-- Required by both Measurements and Descriptors updates.
	@Key char(16),
	@IsDescriptor bit,
	@ParameterConceptKey char(16),
	@AppliesTo varchar(50),
	@Value varchar(50) = NULL,
	@SessionID char(16),
	@Timestamp timestamp,
	-- Only required for the Measurements update.
	@UpperValue varchar(50) = NULL,
	@CollectionUnitKey char(16) = NULL,
	@MethodConceptKey char(16) = NULL,
	@Duration varchar(50) = NULL,
	@Accuracy varchar(50) = NULL,
	@UnitConceptKey char(16) = NULL
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	/*----------------------------------------------------------------------------------*\
	  If we are updating measurement data, more information needs to changed than if
	  we are inserting descriptor data. Hence, there are two different update statements.
	\*----------------------------------------------------------------------------------*/
	BEGIN TRANSACTION

		IF @IsDescriptor = 1	
			-- Updating a descriptor.	
			UPDATE	Collection_Unit_Data
			SET	
				Applies_To = @AppliesTo,		
				Parameter_Concept_Key = @ParameterConceptKey,
				Lower_Value = IsNull(@Value, ' '),
				Is_Descriptor = @IsDescriptor,
				Changed_Session_ID = @SessionID
			WHERE	Collection_Unit_Data_Key = @Key
			AND	(@Timestamp = Timestamp)
		ELSE			
			-- Updating a measurement.
			UPDATE	Collection_Unit_Data
			SET	
				Applies_To = @AppliesTo,		
				Parameter_Concept_Key = @ParameterConceptKey,
				Lower_Value = IsNull(@Value, ' '),
				Is_Descriptor = @IsDescriptor,
				Changed_Session_ID = @SessionID,
				Collection_Unit_Key = @CollectionUnitKey,
				Method_Concept_Key = @MethodConceptKey,
				Duration = @Duration,
				Accuracy = @Accuracy,
				Unit_Concept_Key = @UnitConceptKey,
				Upper_Value = @UpperValue
			WHERE	Collection_Unit_Data_Key = @Key
			AND	(@Timestamp = Timestamp)

		IF @@Error <> 0 GOTO RollbackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_CollectionUnitData_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_CollectionUnitData_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitData_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_CollectionUnitData_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_CollectionUnitData_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_CollectionUnitData_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitData_Update TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_CollectionUnitName_DragDropKey_Get]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_CollectionUnitName_DragDropKey_Get]
GO

/*===========================================================================*\
  Description:	Given a Collection_Unit_Name_Key, this proc. will return
		the Name_Key associated with it.

  Parameters:	@Key		Collection_Unit_Name_Key	
		@DragDropKey	Output

  Created:	May 2004

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_CollectionUnitName_DragDropKey_Get]
	@Key char(16),
	@DragDropKey char(16) OUTPUT
AS
	SELECT 	@DragDropKey = Name_Key
	FROM	Collection_Unit_Name
	WHERE	Collection_Unit_Name_Key = @Key

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_CollectionUnitName_DragDropKey_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_CollectionUnitName_DragDropKey_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitName_DragDropKey_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_CollectionUnitName_DragDropKey_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_CollectionUnitName_DragDropKey_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_CollectionUnitName_DragDropKey_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_CollectionUnitName_DragDropKey_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitName_DragDropKey_Get TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_CollectionUnitRelation_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_CollectionUnitRelation_Select]
GO

/*===========================================================================*\
  Description:	Returns data for the Collection Unit Relationship screen.

  Parameters:	@Key	Collection key

  Created:	October 2003

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_CollectionUnitRelation_Select]
	@Key char(16),
	@ParentKey char(16)
AS

SET NOCOUNT ON

	SELECT TOP 1
	CASE CR.From_Collection_Unit_Key WHEN @ParentKey THEN -- Check which direction the relationship is in
		CR.To_Collection_Unit_Key
	ELSE
		CR.From_Collection_Unit_Key
	END AS Related_Unit_Key,
	CASE WHEN CRel.Collection_Unit_Key IS NULL THEN
		CASE WHEN SRel.Collection_Unit_Key IS NULL THEN
			CASE WHEN CTPreferred.PlainText IS NULL THEN 
				ITN.Preferred_Name
			ELSE	 
				+ CTPreferred.PlainText COLLATE SQL_Latin1_General_CP1_CI_AS
			END + 
			IsNull (' - ' + CN.Number, '')
		ELSE     
			IsNull (SRel.Item_Name + ' - ' + CN.Number, SRel.Item_Name)
		END		
	ELSE	
		IsNull (CRel.Item_Name + ' - ' + M.Number, CRel.Item_Name)
	END AS RelatedTo,
	CR.Thesaurus_Relation_Type_Key,
	CASE CR.From_Collection_Unit_Key WHEN @ParentKey THEN -- Check which direction the relationship is in
		TRT.Forward_Term
	ELSE 	
		TRT.Reverse_Term
	END AS RelationType,
	CR.Inferred_Type,
	CR.Vague_Date_Start,
	CR.Vague_Date_End,
	CR.Vague_Date_Type,
	CR.Author_Name_Key,
	dbo.ufn_GetFormattedName(CR.Author_Name_Key) AS AuthorName,
	CR.Comment,
	CR.Custodian,
	CR.[Timestamp]
	FROM Collection_Unit_Relation CR
	INNER JOIN Collection_Unit CURel ON (
			(CURel.Collection_Unit_Key=CR.To_Collection_Unit_Key AND CR.From_Collection_Unit_Key=@ParentKey) OR
			(CURel.Collection_Unit_Key=CR.From_Collection_Unit_Key AND CR.To_Collection_Unit_Key=@ParentKey))
	LEFT JOIN Collection CRel ON CRel.Collection_Unit_Key=CURel.Collection_Unit_Key
	LEFT JOIN (Movement_Collection_Unit MCU 
			INNER JOIN Movement_Direction MD ON MD.Movement_Direction_Key=MCU.Movement_Direction_Key
					AND MD.Outbound=0
			INNER JOIN Movement_Of_Ownership MO ON MO.Movement_Direction_Key = MD.Movement_Direction_Key
			INNER JOIN Movement_Of_Ownership_Exclusion MOE ON MOE.Movement_Of_Ownership_Key=MO.Movement_Of_Ownership_Key
				AND MOE.Movement_Of_Ownership_Exclusion_Key IS NULL
			INNER JOIN Movement M ON M.Movement_Key=MD.Movement_Key) ON MCU.Collection_Unit_Key=CRel.Collection_Unit_Key
	LEFT JOIN Store SRel ON SRel.Collection_Unit_Key=CURel.Collection_Unit_Key
	LEFT JOIN Specimen_Unit SURel ON SURel.Collection_Unit_Key=CURel.Collection_Unit_Key
	LEFT JOIN Determination D ON D.Determination_Key=SURel.Preferred_Determination_Key
	LEFT JOIN VW_ConceptTermPreferred CTPreferred ON CTPreferred.Concept_Key=D.Concept_Key
	LEFT JOIN Taxon_Determination TD ON TD.Taxon_Determination_Key=SURel.Preferred_Taxon_Determination_Key
	LEFT JOIN Index_Taxon_Name ITN ON ITN.Taxon_List_Item_Key=TD.Taxon_List_Item_Key
	LEFT JOIN Collection_Unit_Number CN ON CN.Collection_Unit_Key=CURel.Collection_Unit_Key
			AND CN.Preferred=1
	LEFT JOIN Concept C ON C.Concept_Key=CN.Type_Concept_Key
	 		AND C.Meaning_Key='SYSTEM0000000001'
	LEFT JOIN Thesaurus_Relation_Type TRT ON TRT.Thesaurus_Relation_Type_Key=CR.Thesaurus_Relation_Type_Key
	WHERE CR.Collection_Unit_Relation_Key=@Key
	AND MOE.Movement_Of_Ownership_Exclusion_Key IS NULL

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_CollectionUnitRelation_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_CollectionUnitRelation_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitRelation_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_CollectionUnitRelation_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_CollectionUnitRelation_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_CollectionUnitRelation_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_CollectionUnitRelation_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitRelation_Select TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_CollectionUnitStatus_Get') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_CollectionUnitStatus_Get]
GO

/*===========================================================================*\
  Description:	Returns the movement status for a collection unit record.

  Parameters:	@Key	Collection unit key
		@Status	OUTPUT

  Created:	August 2003

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_CollectionUnitStatus_Get]
	@Key char(16),
	@Status varchar(50) OUTPUT
AS
	/*-------------------------------------------------------------*\
	  Get a new key.
	\*-------------------------------------------------------------*/
	-- If unknown, set result and exit.
	IF NOT EXISTS(SELECT * FROM Movement_Collection_Unit WHERE Collection_Unit_Key = @Key)
	BEGIN
		SET @Status = 'Unknown'
		RETURN
	END

	/*-------------------------------------------------------------*\
	  Need to work out what it is.
	\*-------------------------------------------------------------*/
	-- Some variables.
	DECLARE	@HoldingOrganisationKey char(16),
		@OwnershipNameKey char(16),
		@OwnershipNameKeyIsHoldingOrg bit,
		@OwnershipMovementType int,
		@MaterialNameKey char(16),
		@MaterialNameKeyIsHoldingOrg bit,
		@MaterialMovementType int,
		@DepartmentName varchar(100)

	-- Get the organisation holding software install	
	SELECT	@HoldingOrganisationKey = Data FROM Setting WHERE [Name] = 'HoldingOrg'

	/*==============================================================================*\
	  Find out if the item is owned by the Holding Organisation or not.
	\*==============================================================================*/
	/*-------------------------------------------------------------*\
	  Get most recent movement of ownership.
	\*-------------------------------------------------------------*/
	SELECT	TOP 1	@OwnershipNameKey = IsNull(MD2.Receiver_Name_Key, MD.Receiver_Name_Key),
			@OwnershipMovementType = M.Movement_Type
	FROM		Movement_Collection_Unit MCU
	INNER JOIN 	Movement_Direction MD ON MD.Movement_Direction_Key = MCU.Movement_Direction_Key
	INNER JOIN 	Movement M ON M.Movement_Key = MD.Movement_Key
			-- If the Movement is a loan in, the name key stored in the Movement_Direction
			-- record linked to the Movement_Collection_Unit record will be the receiver.
			-- The actual owner will be stored in the outbound Movement_Direction record
			-- associated with the movement, so do a left join on this.
	LEFT JOIN	(Movement_Direction MD2 
				INNER JOIN Movement AS M2 ON M2.Movement_Key = MD2.Movement_key
								AND M2.Movement_Type = 2)
			ON MD2.Movement_Key = M.Movement_Key
			AND MD2.Outbound = 1
	INNER JOIN 	Movement_Of_Ownership MO ON MO.Movement_Direction_Key = MCU.Movement_Direction_Key
	LEFT JOIN 	Movement_Of_Ownership_Exclusion MOE ON MOE.Movement_Of_Ownership_Key = MO.Movement_Of_Ownership_Key

	WHERE		MCU.Collection_Unit_Key = @Key
	AND 		MOE.Movement_Of_Ownership_Key IS NULL

	ORDER BY	MO.Vague_Date_Start DESC

	/*-----------------------------------------------------------------------------*\
	  Now that we have the Name Key of the most recent Movement_Of_Onwership for 
	  this Collection_Unit, we want to see if this key refers to
	  the holding organisation, or if it refers to an individual who belongs
	  to the holding organisation
	\*-----------------------------------------------------------------------------*/
	IF 	@OwnershipNameKey = @HoldingOrganisationKey
		SET 	@OwnershipNameKeyIsHoldingOrg = 1
	ELSE IF EXISTS( SELECT 		I.Name_Key
			FROM		Individual AS I
			LEFT JOIN	Name_Relation AS NR1 ON NR1.Name_Key_1 = I.Name_Key
			LEFT JOIN	Name_Relation AS NR2 ON NR2.Name_Key_2 = I.Name_Key
			INNER JOIN	Organisation AS O ON (O.Name_Key = NR1.Name_Key_2
							  OR O.Name_Key = NR2.Name_Key_1)
			WHERE		I.Name_Key = @OwnershipNameKey
			AND 		O.Name_Key = @HoldingOrganisationKey)
		SET @OwnershipNameKeyIsHoldingOrg = 1
	-- If there is no @OwnershipNameKey assume that it belongs to the holding org.
	ELSE IF @OwnershipNameKey Is NULL
		SET @OwnershipNameKeyIsHoldingOrg = 1
	ELSE	SET @OwnershipNameKeyIsHoldingOrg = 0

	/*==============================================================================*\
	  Find out if the item is currently in the Holding Organisation or not.
	\*==============================================================================*/	
	/*-------------------------------------------------------------*\
	  Get most recent movement of material.
	\*-------------------------------------------------------------*/
	SELECT	TOP 1	@MaterialNameKey = MM.Receiver_Name_Key, 
			@MaterialMovementType = Movement_Type, 
			@DepartmentName = OD.Item_Name

	FROM		Movement_Collection_Unit MCU
	INNER JOIN 	Movement_Direction MD ON MD.Movement_Direction_Key = MCU.Movement_Direction_Key
	INNER JOIN 	Movement M ON M.Movement_Key = MD.Movement_Key
	INNER JOIN 	Movement_Of_Material MM ON MM.Movement_Direction_Key = MCU.Movement_Direction_Key
	LEFT JOIN 	Movement_Of_Material_Exclusion MME ON MME.Movement_Of_Material_Key = MM.Movement_Of_Material_Key
	LEFT JOIN	Organisation_Department OD ON OD.Organisation_Department_Key = MM.Receiver_Organisation_Department_Key

	WHERE		MCU.Collection_Unit_Key = @Key
	AND 		MME.Movement_Of_Material_Key IS NULL

	ORDER BY	MM.Vague_Date_Start DESC

	/*-----------------------------------------------------------------------------*\
	  Now that we have the Name Key for the most recent Movement of Material, we 
	  want to see if this key refers to the holding organisation, or if it refers 
	  to an individual who belongs to the holding organisation
	\*-----------------------------------------------------------------------------*/
	IF 	@MaterialNameKey = @HoldingOrganisationKey
		SET 	@MaterialNameKeyIsHoldingOrg = 1
	ELSE IF EXISTS( SELECT 		I.Name_Key
			FROM		Individual AS I
			LEFT JOIN	Name_Relation AS NR1 ON NR1.Name_Key_1 = I.Name_Key
			LEFT JOIN	Name_Relation AS NR2 ON NR2.Name_Key_2 = I.Name_Key
			INNER JOIN	Organisation AS O ON (O.Name_Key = NR1.Name_Key_2
							  OR O.Name_Key = NR2.Name_Key_1)
			WHERE		I.Name_Key = @MaterialNameKey
			AND 		O.Name_Key = @HoldingOrganisationKey)
		SET @MaterialNameKeyIsHoldingOrg = 1
	ELSE	SET @MaterialNameKeyIsHoldingOrg = 0

	/*==============================================================================*\
	  Now work out what to display.
	\*==============================================================================*/		
	-- Formatted additional info after status. No need to repeat all over the place.
	DECLARE @ByOwner varchar(200), @ToOwner varchar(200)
	SET @ByOwner = IsNull(NullIf(' by '+ dbo.ufn_GetFormattedName(@OwnershipNameKey), ' by '), '')
	SET @ToOwner = IsNull(NullIf(' to '+ dbo.ufn_GetFormattedName(@OwnershipNameKey), ' by '), '')

	/*---------------------------------------------------------------------------------------------*\
 	  If there is no Movement_Of_Material record associated with this movement, it will come here
	  (i.e. Hosted Materials and Accessions without an Acqusition).
	  It will also come here if there is a Movement_Of_Material record, but it is not going in to
	  anyone's possession.
	\*---------------------------------------------------------------------------------------------*/
	IF @MaterialNameKey IS NULL
		IF 	(@OwnershipMovementType = 0) SET @Status = 'On loan' + @ToOwner
		ELSE IF (@OwnershipMovementType = 9) SET @Status = 'Owned' + @ByOwner
		-- No-one has the stuff
		ELSE SET @Status = 	CASE @MaterialMovementType
						WHEN 4 THEN 'Destroyed' + @ByOwner
						WHEN 7 THEN 'Lost' + @ByOwner
					END
	ELSE	
	/*-----------------------*\
	  It's been disposed of.
	\*-----------------------*/
	IF @MaterialMovementType = 5 SET @Status = 'Disposed' + @ByOwner
	ELSE
	/*------------------------------------------------------------------------------*\
	  It's here and it's owned here. Movement Type 6 refers to an internal transfer.
	\*------------------------------------------------------------------------------*/
	IF ((@MaterialNameKeyIsHoldingOrg = 1) AND (@OwnershipNameKeyIsHoldingOrg = 1)) OR (@MaterialMovementType = 6)
		IF @DepartmentName IS NOT NULL 
			SET @Status = 'In ' + @DepartmentName + ' department.'
		ELSE BEGIN
			SELECT	@DepartmentName = Full_Name
			FROM	Organisation
			WHERE	Name_Key = @HoldingOrganisationKey

			SET @Status = 'In ' + @DepartmentName + ' organisation.' 
		END 
	ELSE
	/*------------------------------------------------*\
	  It is currently here, but not owned here.
	\*------------------------------------------------*/
	IF (@MaterialNameKeyIsHoldingOrg = 1) AND (@OwnershipNameKeyIsHoldingOrg = 0) 
		SET @Status = 'On loan from ' + dbo.ufn_GetFormattedName(@HoldingOrganisationKey)
	ELSE
	/*------------------------------------------------*\
	  Don't have it here, but owned here
	\*------------------------------------------------*/
	IF (@OwnershipNameKeyIsHoldingOrg = 1) AND (@MaterialNameKeyIsHoldingOrg = 0) 
	BEGIN
		SET @Status = 'On loan to ' + dbo.ufn_GetFormattedName(@MaterialNameKey)
		IF @DepartmentName IS NOT NULL
			SET @Status = @Status + ' ' + @DepartmentName
	END
	ELSE
	/*------------------------------------------------*\
	  Don't have it here and owned by someone else.
	\*------------------------------------------------*/
	IF (@OwnershipNameKeyIsHoldingOrg = 0) AND (@MaterialNameKeyIsHoldingOrg = 0) BEGIN 
		IF @MaterialMovementType = 8
			SET @Status = 'Sold' + @ToOwner
		ELSE
			SET @Status = 'Owned' + @ByOwner
	END

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_CollectionUnitStatus_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_CollectionUnitStatus_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitStatus_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_CollectionUnitStatus_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_CollectionUnitStatus_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_CollectionUnitStatus_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_CollectionUnitStatus_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitStatus_Get TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_CollectionUnit_OwnedByHoldingOrg_Get]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_CollectionUnit_OwnedByHoldingOrg_Get]
GO

/*===========================================================================*\
  Description:	See if a Collection Unit is currently owned by the Holding
		Organisation at a certain point in time

  Parameters:	@CollectionUnitKey  	
		@MovementKey			(Optional) Key will be provided if this
						proc is called because the user clicked
						'Link to existing'
		@NewMovementVagueDateEnd 	(Optional) Will be provided if this proc
						has been called from the Movement Frame.
		@OwnedByHoldingOrg		Output parameter than returns 1 if it does 
						belong to the Holding Organisation, 0 if it 
						doesn't.

  Created:	April 2004

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_CollectionUnit_OwnedByHoldingOrg_Get] 
	@CollectionUnitKey char(16),
	@MovementKey char(16) = NULL,
	@NewMovementVagueDateEnd int = NULL,
	@OwnedByHoldingOrg bit OUTPUT
AS

SET NOCOUNT ON
	DECLARE @OwnershipNameKey char(16),
		@HoldingOrganisationKey char(16),
		@MaterialNameKey char(16),
		@MovementDirectionReceiverNameKey char(16),
		@MaterialMovementKey char(16),
		@OwnershipMovementKey char(16),
		@CurrentMovementVagueDateEnd int

	/*=======================================================================*\
	  If the most recent movement for an item saw it stop being owned by the
	  holding organisation, you shouldn't be able to add any movements that
	  happen after that date. However, you should be allowed to add movements
	  that occur prior to the movement that saw it leave. 

	  This is slightly flawed, in that if the most recent movement was a sale,
	  you could add another sale before this movement. Hence, the item would
	  have been sold twice. Retrospective altering of the data is not going
	  to be implemented in this version of the Collections Browser.
	\*=======================================================================*/
	/*-----------------------------------------------------------------------*\
	  This proc is called from the ValidateNewNode methods that are called when
	  the user clicks 'link to existing' and also from the Movement
	  frame. 

	  In the former case, we will have a Movement Key and can retrieve
	  the data using that. In the latter, we may not have a Movement Key, but
	  we will have the date because it is a field on the frame. 
	\*-----------------------------------------------------------------------*/
	IF @NewMovementVagueDateEnd IS NULL
		SELECT	@NewMovementVagueDateEnd = Exp_Vague_Date_End
		FROM	Movement
		WHERE	Movement_Key = @MovementKey

	/*----------------------------------------------------------------------*\
	  Get the date of the most recent movement. We will want to compare this
	  with the previously selected dates later on.
	\*----------------------------------------------------------------------*/
	SELECT TOP 1	@CurrentMovementVagueDateEnd = M.Exp_Vague_Date_End
	FROM		Movement_Collection_Unit AS MCU
	INNER JOIN	Movement_Direction AS MD ON MD.Movement_Direction_Key = MCU.Movement_Direction_Key
	INNER JOIN	Movement AS M ON M.Movement_Key = MD.Movement_Key
	WHERE		MCU.Collection_Unit_Key = @CollectionUnitKey
	ORDER BY	M.Exp_Vague_Date_Start DESC

	/*-------------------------------------------------------------*\
	  Get the organisation holding software install.
	\*-------------------------------------------------------------*/
	SELECT	@HoldingOrganisationKey = Data FROM Setting WHERE [Name] = 'HoldingOrg'

	/*-------------------------------------------------------------*\
	  Get most recent movement of ownership.
	\*-------------------------------------------------------------*/
	SELECT	TOP 1	@OwnershipNameKey = IsNull(MD2.Receiver_Name_Key, MD.Receiver_Name_Key),
			@OwnershipMovementKey = M.Movement_Key
	FROM		Movement_Collection_Unit MCU
	INNER JOIN 	Movement_Direction MD ON MD.Movement_Direction_Key = MCU.Movement_Direction_Key
	INNER JOIN 	Movement M ON M.Movement_Key = MD.Movement_Key
			/*------------------------------------------------------------------------------*\
			  If the Movement is a loan in, the name key stored in the Movement_Direction
			  record linked to the Movement_Collection_Unit record will be the receiver.
			  The actual owner will be stored in the outbound Movement_Direction record
			  associated with the movement, so do a left join on this.
			\*------------------------------------------------------------------------------*/
	LEFT JOIN	(Movement_Direction MD2 
				INNER JOIN Movement AS M2 ON M2.Movement_Key = MD2.Movement_key
								AND M2.Movement_Type = 2)
			ON MD2.Movement_Key = M.Movement_Key
			AND MD2.Outbound = 1
	LEFT JOIN 	Movement_Of_Ownership MO ON MO.Movement_Direction_Key = MCU.Movement_Direction_Key
	LEFT JOIN 	Movement_Of_Ownership_Exclusion MOE ON MOE.Movement_Of_Ownership_Key = MO.Movement_Of_Ownership_Key

	WHERE		MCU.Collection_Unit_Key = @CollectionUnitKey
	AND 		MOE.Movement_Of_Ownership_Key IS NULL

	ORDER BY	MO.Vague_Date_Start DESC

	/*-------------------------------------------------------------*\
	  Get most recent movement of material.
	\*-------------------------------------------------------------*/
	SELECT	TOP 1	@MaterialNameKey = MM.Receiver_Name_Key,
			@MovementDirectionReceiverNameKey = MD.Receiver_Name_Key,
			@MaterialMovementKey = M.Movement_Key 
	FROM		Movement_Collection_Unit MCU
	INNER JOIN 	Movement_Direction MD ON MD.Movement_Direction_Key = MCU.Movement_Direction_Key
	INNER JOIN 	Movement M ON M.Movement_Key = MD.Movement_Key
	INNER JOIN 	Movement_Of_Material MM ON MM.Movement_Direction_Key = MCU.Movement_Direction_Key
	LEFT JOIN 	Movement_Of_Material_Exclusion MME ON MME.Movement_Of_Material_Key = MM.Movement_Of_Material_Key
	LEFT JOIN	Organisation_Department OD ON OD.Organisation_Department_Key = MM.Receiver_Organisation_Department_Key

	WHERE		MCU.Collection_Unit_Key = @CollectionUnitKey
	AND 		MME.Movement_Of_Material_Key IS NULL

	ORDER BY	MM.Vague_Date_Start DESC

	/*==================================================================================*\
	  Now decide whether the item belongs to the holding organisation at the given
	  point in time.
	\*==================================================================================*/

	/*------------------------------------------------------------------------------*\
	  If the new current Movement is the most recent movement, then you should be
	  allowed to edit the date of the Movement freely. We need to check the 
	  Movement_Key against the key obtained from both the MOO and MOM, in case
	  it is null for one of them.
	\*------------------------------------------------------------------------------*/
	IF (@OwnershipMovementKey = @MovementKey) OR (@MaterialMovementKey = @MovementKey)
		SET @OwnedByHoldingOrg = 1

	/*------------------------------------------------------------------------------*\
	  If @CurrentMovementVagueDateEnd = 0, this means that there isn't a movement
	  currently associated with the Collection_Unit, so safe to assume that
	  it is owned by holding organisation
	\*------------------------------------------------------------------------------*/
	ELSE IF @CurrentMovementVagueDateEnd = 0
		SET @OwnedByHoldingOrg = 1

	/*------------------------------------------------------------------------------*\
	  If the new Movement is after the current movement we have to check that
	  the item has not been moved out of the holding organisation
	\*------------------------------------------------------------------------------*/
	ELSE IF @NewMovementVagueDateEnd >= @CurrentMovementVagueDateEnd
	BEGIN
		/*------------------------------------------------------------------------------*\
		  If there is a Movement Direction record but the Receiver Name Key is Null,
		  it will be a disposed / destroyed / lost movement and hence not owned by
		  the holding org.
		\*------------------------------------------------------------------------------*/
		IF (@MovementDirectionReceiverNameKey IS NULL)
			SET	@OwnedByHoldingOrg = 0

		/*------------------------------------------------------------------------------*\
		  Owner name key is the holding organsiation
		\*------------------------------------------------------------------------------*/
		ELSE IF @OwnershipNameKey = @HoldingOrganisationKey
			SET 	@OwnedByHoldingOrg = 1

		/*------------------------------------------------------------------------------*\
		  See if the name key is an individual who belongs to the holding organisation.
		\*------------------------------------------------------------------------------*/
		ELSE IF EXISTS( SELECT 		I.Name_Key
				FROM		Individual AS I
				LEFT JOIN	Name_Relation AS NR1 ON NR1.Name_Key_1 = I.Name_Key
				LEFT JOIN	Name_Relation AS NR2 ON NR2.Name_Key_2 = I.Name_Key
				INNER JOIN	Organisation AS O ON (O.Name_Key = NR1.Name_Key_2
								  OR O.Name_Key = NR2.Name_Key_1)
				WHERE		I.Name_Key = @OwnershipNameKey
				AND 		O.Name_Key = @HoldingOrganisationKey)
			SET @OwnedByHoldingOrg = 1

		/*------------------------------------------------------------------------------*\
		  If there is no @OwnershipNameKey assume that it belongs to the holding org.
		\*------------------------------------------------------------------------------*/
		ELSE IF (@OwnershipNameKey Is NULL) 
			SET @OwnedByHoldingOrg = 1 

		/*------------------------------------------------------------------------------*\
		  Otherwise assume it doesn't belong to the holding organisation.
		\*------------------------------------------------------------------------------*/
		ELSE	SET @OwnedByHoldingOrg = 0
	END
	/*------------------------------------------------------------------------------*\
	  If the new movement is before the current movement then we can assume that
	  it is ok to add this movement
	\*------------------------------------------------------------------------------*/
	ELSE	
		SET @OwnedByHoldingOrg = 1
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_CollectionUnit_OwnedByHoldingOrg_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_CollectionUnit_OwnedByHoldingOrg_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_CollectionUnit_OwnedByHoldingOrg_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_CollectionUnit_OwnedByHoldingOrg_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_CollectionUnit_OwnedByHoldingOrg_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_CollectionUnit_OwnedByHoldingOrg_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_CollectionUnit_OwnedByHoldingOrg_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_CollectionUnit_OwnedByHoldingOrg_Get TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_CollectionUnit_Update_ForMovement]')
	   AND    Type = 'P')
    DROP PROCEDURE [dbo].[usp_CollectionUnit_Update_ForMovement]
GO

/*===========================================================================*\
  Description:	Inserts a record into the Movement_Collection_Unit join table 
		so that there is a relationship between the Movement and 
		Collection Unit tables.

  Parameters:	@ParentKey 	The key of the top level (parent) Movement node.
		@ChildKey	The key of the added (child) Collection Unit node. 
		@SessionID
		@JoinKey	Key for new record on join table.
		@IsAccessionOrExchange  Optional parameter
		@IsInbound	Optional parameter

  Created:	September 2003

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_CollectionUnit_Update_ForMovement] 
	@ParentKey char(16), --Movement_Key
	@ChildKey char(16), --Collection_Unit_Key
	@SessionID char(16),
	@IsAccessionOrExchange bit = NULL,
	@IsInbound bit = NULL,
	@JoinKey char(16) OUTPUT
AS
	SET NOCOUNT ON

	EXECUTE spNextKey 'Movement_Collection_Unit', @JoinKey OUTPUT

	DECLARE @DirKey CHAR(16)

	BEGIN TRANSACTION
		-- The Accessions and Exchanges folder node is only allowed to contain one node.
		-- As a result, any other accession or exchange movements associated with the 
		-- collection unit should be deleted before the new record is inserted.
		IF @IsAccessionOrExchange = 1
			DELETE		Movement_Collection_Unit
			FROM		Movement_Collection_Unit AS MCU
			INNER JOIN	Movement_Direction AS MD ON MD.Movement_Direction_Key = MCU.Movement_Direction_Key
			INNER JOIN	Movement AS M ON M.Movement_Key = MD.Movement_Key
							AND M.Movement_Type IN  (0,1)
			WHERE		MCU.Collection_Unit_Key = @ChildKey

		-- To link the new Movement node to the Collection Unit it was added in, 
		-- we need to add a record to the Movement_Collection_Unit table. This table
		-- requires a Movement_Direction_Key. Hence, we need to get the correct 
		-- Movement_Direction_Key for the the Movement we are linking.
		-- Also, if we are dealing with a loan in, we want the Movement_Collection_Unit
		-- record to contain the Movement_Direction key for the inbound movement.
	
		IF (@IsInbound IS NULL) OR (@IsInbound = 0)
			SELECT	@DirKey = Movement_Direction_Key
			FROM 	Movement_Direction
			WHERE 	Movement_Key = @ParentKey
			AND 	Outbound = 1
	
		IF @DirKey IS NULL
			SELECT	@DirKey = Movement_Direction_Key
			FROM	Movement_Direction
			WHERE	Movement_Key = @ParentKey
			AND	Outbound = 0

		-- Now that we have the Movement_Direction_Key, we can insert a record into
		-- the Movement_Collection_Unit table.
		INSERT INTO Movement_Collection_Unit (
			Movement_Collection_Unit_Key,
			Movement_Direction_Key,
			Collection_Unit_Key,
			Entered_Session_ID
		) VALUES (
			@JoinKey,
			@DirKey,
			@ChildKey,
			@SessionID
		)
	
		IF @@Error <> 0 GOTO RollbackAndExit

		DECLARE @First_Movement_Of_Material_Key CHAR(16)

		SELECT TOP 1 @First_Movement_Of_Material_Key = Movement_Of_Material_Key
		FROM 	Movement_Of_Material 
		WHERE 	Movement_Direction_Key = @DirKey
		ORDER BY [TimeStamp]

		--If this is not the first movement of material, exclude this collection_unit from it
		IF EXISTS(SELECT * FROM Movement_Of_Material 
				WHERE (Movement_Of_Material_Key <> @First_Movement_Of_Material_Key) 
				AND (Movement_Direction_Key = @DirKey))
		BEGIN
			DECLARE @Movement_Of_Material_Key CHAR(16)
			DECLARE @Movement_Of_Material_Exclusion_Key CHAR(16)
	
			DECLARE MOMEcsr CURSOR LOCAL FAST_FORWARD
			FOR 
			SELECT	Movement_Of_Material_Key
			FROM	Movement_Of_Material
			WHERE	Movement_Direction_Key = @DirKey
		
			OPEN MOMEcsr
			
			FETCH NEXT FROM MOMEcsr INTO @Movement_Of_Material_Key
			
			WHILE @@FETCH_STATUS = 0
			BEGIN
				EXECUTE spNextKey 'Movement_Of_Material_Exclusion', @Movement_Of_Material_Exclusion_Key OUTPUT				
				INSERT INTO Movement_Of_Material_Exclusion
					(Movement_Of_Material_Exclusion_Key,
					Movement_Of_Material_Key,
					Collection_Unit_Key,
					Entered_Session_ID)
				VALUES
					(@Movement_Of_Material_Exclusion_Key,
					@Movement_Of_Material_Key,
					@ChildKey,
					@SessionID)
	
				IF @@Error <> 0 GOTO RollbackAndExit
			
			
			FETCH NEXT FROM MOMEcsr INTO @Movement_Of_Material_Key
			END --End While
			
			CLOSE MOMEcsr
			DEALLOCATE MOMEcsr
		END

		IF @@Error <> 0 GOTO RollbackAndExit
	
	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_CollectionUnit_Update_ForMovement') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_CollectionUnit_Update_ForMovement'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_CollectionUnit_Update_ForMovement TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_CollectionUnit_Update_ForMovement TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_CollectionUnit_Update_ForMovement TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_CollectionUnit_Update_ForMovement TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_CollectionUnit_Update_ForMovement TO [Dev - JNCC SQL]
END
GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Collection_Contains_Get]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Collection_Contains_Get]
GO

CREATE PROCEDURE [dbo].[usp_Collection_Contains_Get] 
@ContainerCollectionKey CHAR(16),
@ContainedCollectionKey CHAR(16),
@Contains BIT OUTPUT
AS

--  DESCRIPTION
--  Checks to see if there is a hierarchical link between two specified collections
--
--  PARAMETERS
--  NAME					DESCRIPTION
--	@ContainerCollectionKey Container collection unit key
--	@ContainedCollectionKey Contained collection unit key
--  @Contains				Result
--
--
--  AUTHOR:     			Ben Collier, Dorset Software
--  CREATED:    			18/02/2004
--
SET NOCOUNT ON


DECLARE @ContainedCollectionContainers TABLE
(
	[Collection_Unit_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL
)


DECLARE @Parent_Collection_Unit_Key CHAR(16)

SET @Parent_Collection_Unit_Key = @ContainedCollectionKey
SET @Contains = 0

--Obtain successive parents
WHILE @Parent_Collection_Unit_Key IS NOT NULL
BEGIN
	SELECT @Parent_Collection_Unit_Key = C.Parent_Collection_Collection_Unit_Key
	FROM Collection C
	WHERE C.Collection_Unit_Key = @Parent_Collection_Unit_Key

	IF (@Parent_Collection_Unit_Key IS NULL) OR (@@RowCount = 0) --At top of storage hierarchy. No match found.
	BEGIN
		SET @Contains = 0
		BREAK
	END
	ELSE IF @Parent_Collection_Unit_Key = @ContainerCollectionKey --Match found.
	BEGIN
		SET @Contains = 1
		BREAK
	END
	ELSE IF EXISTS(SELECT * FROM @ContainedCollectionContainers WHERE Collection_Unit_Key = @Parent_Collection_Unit_Key)
	BEGIN --Recursive Store hierarchy found.
		SET @Contains = 1
		BREAK
	END
	ELSE --Log current Store
		INSERT INTO @ContainedCollectionContainers (Collection_Unit_Key) VALUES (@Parent_Collection_Unit_Key)

END


GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Collection_Contains_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Collection_Contains_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Collection_Contains_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Collection_Contains_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Collection_Contains_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Collection_Contains_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Collection_Contains_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Collection_Contains_Get TO [Dev - JNCC SQL]
END

GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Collection_Select_ForSearch]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Collection_Select_ForSearch]
GO

CREATE PROCEDURE [dbo].[usp_Collection_Select_ForSearch] 
@UserDomainMask INT,
@SessionID CHAR(16),
@SearchText VARCHAR(100)

AS

--  DESCRIPTION
--  Returns top level Collections data to the CollectionsBrowser
--
--	PARAMETERS
--	NAME				DESCRIPTION
--	@SearchText 		Search text used to find collections
--	@UserDomainMask		User's Domain Mask restricting which records may be returned
--	@SessionID 			User's SessionID
--
--
--  AUTHOR:				Ben Collier, Dorset Software
--  CREATED:			2003-09-08
--

SET NOCOUNT ON
SET NO_BROWSETABLE OFF

SELECT C.Collection_Unit_Key AS Item_Key, 
	CASE WHEN M.Number IS NULL THEN C.Item_Name ELSE C.Item_Name + ' - ' + M.Number END AS DisplayTerm,
	CASE WHEN M.Number IS NULL THEN C.Item_Name ELSE C.Item_Name + ' - ' + M.Number END AS SearchTerm

FROM 
	(COLLECTION C
	INNER JOIN
   	    COLLECTION_UNIT CU 
   	ON C.Collection_Unit_Key = CU.Collection_Unit_Key
       	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
			OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
	)
	LEFT JOIN
		(MOVEMENT_COLLECTION_UNIT MCU
		INNER JOIN
			MOVEMENT_DIRECTION MD
		ON MCU.Movement_Direction_Key = MD.Movement_Direction_Key 
			AND (MD.Outbound = 0)
		INNER JOIN
			MOVEMENT M
		ON MD.Movement_Key = M.Movement_Key AND (M.Movement_Type = 0 OR M.Movement_Type = 1)
		LEFT JOIN
			(CONCEPT CON
			INNER JOIN 
				TERM T
			ON CON.Term_Key = T.Term_Key)
		ON CON.Concept_Key = 'SYSTEM0000000006') --ACCESSION NUMBER
	ON CU.Collection_Unit_Key = MCU.Collection_Unit_Key
WHERE C.Item_Name LIKE @SearchText + '%'
ORDER BY DisplayTerm

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Collection_Select_ForSearch') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Collection_Select_ForSearch'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Collection_Select_ForSearch TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Collection_Select_ForSearch TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Collection_Select_ForSearch TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Collection_Select_ForSearch TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Collection_Select_ForSearch TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Collection_Select_ForSearch TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptDesignation_ImportTaxonList]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_ConceptDesignation_ImportTaxonList]
GO

/*===========================================================================*\
  Description:	Import concept designations corresponding to the taxon
				designations associated with the specified taxon list.

  Parameters:	@job_id					Job identifier

  Created:		Dec 2003

  Last revision information:
	$Revision: 7 $
	$Date: 6/02/09 10:41 $
	$Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptDesignation_ImportTaxonList]
	@job_id					INT
AS
	SET NOCOUNT ON

	DECLARE     @taxon_list_key					CHAR(16),
				@taxon_designation_key			CHAR(16),
				@concept_key					CHAR(16),
				@designation_type_concept_key	CHAR(16),
				@from_vague_date_start			INT,
				@to_vague_date_start			INT,
				@source_key						CHAR(16),
				@source_join_key				CHAR(16),
				@ins_user_key					CHAR(16),
				@ins_date						DATETIME,
				@ins_session_id					CHAR(16),
				@upd_user_key					CHAR(16),
				@upd_date						DATETIME,
				@upd_session_id					CHAR(16),
				@system							BIT,
				@concept_designation_key		CHAR(16)

	/* determine parameters of job */
	SELECT		@taxon_list_key							=	m.Taxon_List_Key
	FROM		Import_Export_Job						AS	j
	INNER JOIN	Taxon_Dictionary_Concept_Group_Mapping	AS	m
	ON			m.Concept_Group_Key						=	j.Concept_Group_Key
	WHERE		j.Import_Export_Job_ID					=	@job_id

	IF @@ROWCOUNT = 0
	BEGIN
		RAISERROR ('Job does not exist or has not been configured', 16, 1)
		RETURN
	END

	EXECUTE		usp_Import_Export_Job_UpdateStatus	@job_id,
													'Importing concept designations'
	IF @@ERROR <> 0 RETURN

	DECLARE		designations	CURSOR LOCAL FAST_FORWARD FOR
	SELECT		td.TAXON_DESIGNATION_KEY,
				tcm.Concept_Key,
				tdm.Concept_Designation_Type_Key,
				DATEDIFF(d, '18991230', td.DATE_FROM),
				DATEDIFF(d, '18991230', td.DATE_TO),
				td.SOURCE_KEY,
				td.ENTERED_BY,
				td.ENTRY_DATE,
				td.CHANGED_BY,
				td.CHANGED_DATE,
				td.SYSTEM_SUPPLIED_DATA
	FROM		TAXON_LIST_VERSION							AS	tlv
	INNER JOIN	TAXON_LIST_ITEM								AS	tli
	ON			tli.TAXON_LIST_VERSION_KEY					=	tlv.TAXON_LIST_VERSION_KEY
	INNER JOIN	Taxon_Dictionary_Concept_Mapping			AS	tcm
	ON			tcm.Taxon_List_Item_Key						=	tli.TAXON_LIST_ITEM_KEY
	INNER JOIN	TAXON_DESIGNATION							AS	td
	ON			td.TAXON_LIST_ITEM_KEY						=	tcm.Taxon_List_Item_Key
	INNER JOIN	Taxon_Dictionary_Designation_Type_Mapping	AS	tdm
	ON			tdm.Taxon_Designation_Type_Key				=	td.TAXON_DESIGNATION_TYPE_KEY
	WHERE		tlv.TAXON_LIST_KEY							=	@taxon_list_key

	OPEN		designations

	WHILE 1 = 1
	BEGIN
		FETCH		designations
		INTO		@taxon_designation_key,
					@concept_key,
					@designation_type_concept_key,
					@from_vague_date_start,
					@to_vague_date_start,
					@source_key,
					@ins_user_key,
					@ins_date,
					@upd_user_key,
					@upd_date,
					@system

		IF @@FETCH_STATUS <> 0 BREAK

		BEGIN TRANSACTION

		/* obtain session identifiers */
		EXECUTE		usp_Session_ForDate		@ins_user_key,
											@ins_date,
											@ins_session_id		OUTPUT
		IF @@ERROR <> 0 GOTO fail_from_cursor

		IF @upd_user_key IS NULL
		BEGIN
			SET			@upd_session_id		=	NULL
		END
		ELSE
		BEGIN
			EXECUTE		usp_Session_ForDate		@upd_user_key,
												@upd_date,
												@upd_session_id		OUTPUT
			IF @@ERROR <> 0 GOTO fail_from_cursor
		END

		SELECT		@concept_designation_key						=	Concept_Designation_Key,
					@source_join_key								=	Source_Join_Key
		FROM		Taxon_Dictionary_Concept_Designation_Mapping
		WHERE		Taxon_Designation_Key							=	@taxon_designation_key

		IF @@ROWCOUNT > 0
		BEGIN
			/* update concept designation */
			UPDATE		Concept_Designation
			SET			Concept_Key						=	@concept_key,
						Designation_Type_Concept_Key	=	@designation_type_concept_key,
						From_Vague_Date_Start			=	@from_vague_date_start,
						From_Vague_Date_End				=	@from_vague_date_start,
						From_Vague_Date_Type			=	CASE WHEN @from_vague_date_start IS NULL
																THEN 'U'
																ELSE 'D'
															END,
						To_Vague_Date_Start				=	@to_vague_date_start,
						To_Vague_Date_End				=	@to_vague_date_start,
						To_Vague_Date_Type				=	CASE WHEN @to_vague_date_start IS NULL
																THEN NULL
																ELSE 'D'
															END,
						Entered_Session_ID				=	@ins_session_id,
						Changed_Session_ID				=	@upd_session_id,
						System_Supplied_Data			=	@system
			WHERE		Concept_Designation_Key			=	@concept_designation_key

			IF @@ERROR <> 0 GOTO fail_from_cursor												
		END
		ELSE
		BEGIN
			/* create concept designation */
			EXECUTE		spNextKey	'Concept_Designation',
									@concept_designation_key	OUTPUT
			IF @@ERROR <> 0 GOTO fail_from_cursor

			SET			@source_join_key				=	NULL

			INSERT		Concept_Designation (
						Concept_Designation_Key,
						Concept_Key,
						Designation_Type_Concept_Key,
						From_Vague_Date_Start,
						From_Vague_Date_End,
						From_Vague_Date_Type,
						To_Vague_Date_Start,
						To_Vague_Date_End,
						To_Vague_Date_Type,
						Entered_Session_ID,
						Changed_Session_ID,
						System_Supplied_Data)
			SELECT		@concept_designation_key,
						@concept_key,
						@designation_type_concept_key,
						@from_vague_date_start,
						@from_vague_date_start,
						CASE WHEN @from_vague_date_start IS NULL
							THEN 'U'
							ELSE 'D'
						END,
						@to_vague_date_start,
						@to_vague_date_start,
						CASE WHEN @to_vague_date_start IS NULL
							THEN NULL
							ELSE 'D'
						END,
						@ins_session_id,
						@upd_session_id,
						@system

			IF @@ERROR <> 0 GOTO fail_from_cursor

			/* record mapping */
			INSERT		Taxon_Dictionary_Concept_Designation_Mapping (
						Taxon_Designation_Key,
						Concept_Designation_Key)
			VALUES		(@taxon_designation_key,
						@concept_designation_key)

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END

		/* make any changes required in Source_Join */
		IF @source_key IS NULL
		BEGIN
			UPDATE		Taxon_Dictionary_Concept_Designation_Mapping
			SET			Source_Join_Key									=	NULL
			WHERE		Taxon_Designation_Key							=	@taxon_designation_key

			IF @@ERROR <> 0 GOTO fail_from_cursor			
		END

		EXECUTE		usp_SourceJoin_RecordImported	@source_join_key	OUTPUT,
													'Concept_Designation',
													@concept_designation_key,
													@source_key,
													@ins_session_id,
													@system
		IF @@ERROR <> 0 GOTO fail_from_cursor

		IF @source_key IS NOT NULL
		BEGIN
			UPDATE		Taxon_Dictionary_Concept_Designation_Mapping
			SET			Source_Join_Key									=	@source_join_key
			WHERE		Taxon_Designation_Key							=	@taxon_designation_key

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END

		/* update progress counter */
		EXECUTE		usp_Import_Export_Job_RecordProcessed	@job_id
		IF @@ERROR <> 0 GOTO fail_from_cursor

		COMMIT TRANSACTION
	END

	CLOSE		designations
	DEALLOCATE	designations
	RETURN

fail_from_cursor:
	CLOSE		designations
	DEALLOCATE	designations

fail:
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_ConceptDesignation_ImportTaxonList failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptDesignation_ImportTaxonList') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptDesignation_ImportTaxonList'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptDesignation_ImportTaxonList TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptDesignation_ImportTaxonList TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptDesignation_ImportTaxonList TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptGroupVersion_ImportTaxonList]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_ConceptGroupVersion_ImportTaxonList]
GO

/*===========================================================================*\
  Description:	Import concept group versions corresponding to the versions
				of a taxon list.

  Parameters:   @job_id					Job identifier

  Created:		Nov 2003

  Last revision information:
	$Revision: 7 $
	$Date: 6/02/09 10:41 $
	$Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptGroupVersion_ImportTaxonList]
	@job_id				INT
AS
	SET NOCOUNT ON

	DECLARE		@concept_group_key					CHAR(16),
				@taxon_list_key						CHAR(16),
				@taxon_list_version_key				CHAR(16),
				@version							INT,
				@vd_start							INT,
				@vd_end								INT,
				@vd_type							VARCHAR(2),
				@source_key							CHAR(16),
				@source_join_key					CHAR(16),
				@ins_user_key						CHAR(16),
				@ins_date							SMALLDATETIME,
				@ins_session_id						CHAR(16),
				@upd_user_key						CHAR(16),
				@upd_date							SMALLDATETIME,
				@upd_session_id						CHAR(16),
				@system								BIT,
				@concept_group_version_key			CHAR(16),
				@prior_concept_group_version_key	CHAR(16)

	/* determine parameters of job */
	SELECT      @concept_group_key						=	m.Concept_Group_Key,
				@taxon_list_key							=	m.Taxon_List_Key
	FROM		Import_Export_Job						AS	j
	INNER JOIN	Taxon_Dictionary_Concept_Group_Mapping	AS	m
	ON			m.Concept_Group_Key						=	j.Concept_Group_Key
	WHERE		j.Import_Export_Job_ID					=	@job_id

	IF @@ROWCOUNT = 0
	BEGIN
		RAISERROR ('Job does not exist or has not been configured', 16, 1)
		GOTO fail
	END

	EXECUTE		usp_Import_Export_Job_UpdateStatus	@job_id,
													'Importing concept group versions'
	IF @@ERROR <> 0 GOTO fail

	DECLARE		versions	CURSOR LOCAL FOR
	SELECT		TAXON_LIST_VERSION_KEY,
				VERSION,
				VAGUE_DATE_START,
				VAGUE_DATE_END,
				VAGUE_DATE_TYPE,
				SOURCE_KEY,
				ENTERED_BY,
				ENTRY_DATE,
				CHANGED_BY,
				CHANGED_DATE,
				SYSTEM_SUPPLIED_DATA
	FROM		TAXON_LIST_VERSION
	WHERE		TAXON_LIST_KEY			=	@taxon_list_key
	ORDER BY	VERSION

	OPEN		versions

	WHILE 1 = 1
	BEGIN
		FETCH		versions
		INTO		@taxon_list_version_key,
					@version,
					@vd_start,
					@vd_end,
					@vd_type,
					@source_key,
					@ins_user_key,
					@ins_date,
					@upd_user_key,
					@upd_date,
					@system

		IF @@FETCH_STATUS <> 0 BREAK

		/* clean up any vague date weirdness */
		SET			@vd_type	=	ISNULL(@vd_type, 'U')

		IF @vd_type = 'U'
		BEGIN
			SELECT		@vd_start	=	NULL,
						@vd_end		=	NULL
		END

		/* obtain session identifiers */
		EXECUTE		usp_Session_ForDate		@ins_user_key,
											@ins_date,
											@ins_session_id		OUTPUT
		IF @@ERROR <> 0 GOTO fail_from_cursor

		IF @upd_user_key IS NULL
		BEGIN
			SET			@upd_session_id		=	NULL
		END
		ELSE
		BEGIN
			EXECUTE		usp_Session_ForDate		@upd_user_key,
												@upd_date,
												@upd_session_id		OUTPUT
			IF @@ERROR <> 0 GOTO fail_from_cursor
		END

		BEGIN TRANSACTION

		SELECT		@concept_group_version_key						=	Concept_Group_Version_Key,
					@source_join_key								=	Source_Join_Key
		FROM		Taxon_Dictionary_Concept_Group_Version_Mapping
		WHERE		Taxon_List_Version_Key							=   @taxon_list_version_key

		IF @@ROWCOUNT > 0
		BEGIN
			/* update concept group version */
			UPDATE		Concept_Group_Version
			SET			Sequence					=	@version,
						From_Vague_Date_Start		=	@vd_start,
						From_Vague_Date_End			=	@vd_end,
						From_Vague_Date_Type		=	@vd_type,
						Entered_Session_ID			=	@ins_session_id,
						Changed_Session_ID			=	@upd_session_id,
						System_Supplied_Data		=	@system
			WHERE		Concept_Group_Version_Key	=	@concept_group_version_key

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END
		ELSE
		BEGIN
			/* create concept group version */
			EXECUTE		spNextKey	'Concept_Group_Version',
									@concept_group_version_key	OUTPUT

			IF @@ERROR <> 0 GOTO fail_from_cursor

			SET			@source_join_key			=	NULL

			INSERT		Concept_Group_Version
						(Concept_Group_Version_Key,
						Concept_Group_Key,
						Version,
						Sequence,
						From_Vague_Date_Start,
						From_Vague_Date_End,
						From_Vague_Date_Type,
						Acq_Vague_Date_Type,
						Entered_Session_ID,
						Changed_Session_ID,
						System_Supplied_Data)
			SELECT		@concept_group_version_key,
						@concept_group_key,
						LTRIM(STR(@version)),
						@version,
						@vd_start,
						@vd_end,
						@vd_type,
						'U',
						@ins_session_id,
						@upd_session_id,
						@system

			IF @@ERROR <> 0 GOTO fail_from_cursor

			/* record mapping */
			INSERT		Taxon_Dictionary_Concept_Group_Version_Mapping (
						Taxon_List_Version_Key,
						Concept_Group_Version_Key)
			VALUES		(@taxon_list_version_key,
						@concept_group_version_key)

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END
		
		/* set end date of prior version */
		IF @prior_concept_group_version_key IS NOT NULL
		BEGIN
			UPDATE		Concept_Group_Version
			SET			To_Vague_Date_Start			=	@vd_start,
						To_Vague_Date_End			=	@vd_end,
						To_Vague_Date_Type			=	@vd_type
			WHERE		Concept_Group_Version_Key	=	@prior_concept_group_version_key

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END

		/* make any changes required in Source_Join */
		IF @source_key IS NULL
		BEGIN
			UPDATE		Taxon_Dictionary_Concept_Group_Version_Mapping
			SET			Source_Join_Key									=	NULL
			WHERE		Concept_Group_Version_Key						=	@concept_group_version_key

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END

		EXECUTE		usp_SourceJoin_RecordImported	@source_join_key		OUTPUT,
													'Concept_Group_Version',
													@concept_group_version_key,
													@source_key,
													@ins_session_id,
													@system
		IF @@ERROR <> 0 GOTO fail_from_cursor

		IF @source_key IS NOT NULL
		BEGIN
			UPDATE		Taxon_Dictionary_Concept_Group_Version_Mapping
			SET			Source_Join_Key									=	@source_join_key
			WHERE		Concept_Group_Version_Key						=	@concept_group_version_key

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END

		/* update progress counter */
		EXECUTE		usp_Import_Export_Job_RecordProcessed	@job_id
		IF @@ERROR <> 0 GOTO fail_from_cursor

		COMMIT TRANSACTION

		SET		@prior_concept_group_version_key = @concept_group_version_key
	END

	CLOSE		versions
	DEALLOCATE	versions
	RETURN

fail_from_cursor:
	CLOSE		versions
	DEALLOCATE	versions

fail:
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_ConceptGroupVersion_ImportTaxonList failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptGroupVersion_ImportTaxonList') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptGroupVersion_ImportTaxonList'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptGroupVersion_ImportTaxonList TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptGroupVersion_ImportTaxonList TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptGroupVersion_ImportTaxonList TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptGroupVersion_Select_ForDomain]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptGroupVersion_Select_ForDomain]
GO

/*===========================================================================*\
  Description:	Retrieves a list of all versions of concept groups for a supplied domain.
		Includes the version in the caption and the date information.

  Parameters:	@Domain - key of the domain

  Created:	August 2003

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptGroupVersion_Select_ForDomain]
	@Domain char(16)
AS

SELECT 		CGV.Concept_Group_Version_Key as Item_Key, 
		CG.Item_Name + ' - ' + CGV.Version as Item_Name, 
		CGV.From_Vague_Date_Start, 
		CGV.From_Vague_Date_End, 
		CGV.From_Vague_Date_Type,
		CGV.To_Vague_Date_Start, 
		CGV.To_Vague_Date_End, 
		CGV.To_Vague_Date_Type,
		IsNull(CG.Hierarchy_Relation_Type_Key, D.Default_Hierarchy_Relation_Type_Key) AS Hierarchy_Relation_Type_Key
FROM 		Local_Domain LD
INNER JOIN 	Concept_Group CG ON CG.Local_Domain_Key=LD.Local_Domain_Key
INNER JOIN 	Concept_Group_Version CGV on CGV.Concept_Group_Key=CG.Concept_Group_Key
INNER JOIN 	Domain AS D ON D.Domain_Key = LD.Domain_Key
WHERE 		LD.Domain_Key=@Domain
ORDER BY 	CGV.Sequence, CG.Item_Name

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptGroupVersion_Select_ForDomain') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptGroupVersion_Select_ForDomain'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptGroupVersion_Select_ForDomain TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptGroupVersion_Select_ForDomain TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptGroupVersion_Select_ForDomain TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ConceptGroupVersion_Select_ForDomain TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptGroupVersion_Select_ForDomain TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptGroupVersion_Select_ForDomain TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptGroup_GetLatestTimestamp]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_ConceptGroup_GetLatestTimestamp]
GO

/*===========================================================================*\
  Description:	Latest timestamp assigned to concepts in the specified group.

  Parameters:   @concept_group_key		Concept group key
				@timestamp				[on exit] Latest timestamp

  Created:		Jan 2004

  Last revision information:
	$Revision: 7 $
	$Date: 6/02/09 10:41 $
	$Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptGroup_GetLatestTimestamp]
	@concept_group_key	CHAR(16),
	@timestamp			TIMESTAMP	OUTPUT
AS
	SET NOCOUNT ON

	SELECT		@timestamp			=	MAX(Timestamp)
	FROM		Concept				AS	c
	WHERE		Concept_Group_Key	=	@concept_group_key
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptGroup_GetLatestTimestamp') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptGroup_GetLatestTimestamp'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptGroup_GetLatestTimestamp TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptGroup_GetLatestTimestamp TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptGroup_GetLatestTimestamp TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptGroup_GetTaxonList]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_ConceptGroup_GetTaxonList]
GO

/*===========================================================================*\
  Description:	Determine the taxon list (if any) associated with the
				specified concept group.

  Parameters:	@concept_group_key		Concept group key
				@taxon_list_key			[on exit] Taxon list key, or NULL

  Created:		Jan 2004

  Last revision information:
	$Revision: 7 $
	$Date: 6/02/09 10:41 $
	$Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptGroup_GetTaxonList]
	@concept_group_key			CHAR(16),
	@taxon_list_key				CHAR(16)	OUTPUT
AS
	SET NOCOUNT ON

	SELECT		@taxon_list_key							=	Taxon_List_Key
	FROM		Taxon_Dictionary_Concept_Group_Mapping
	WHERE		Concept_Group_Key						=	@concept_group_key

	IF @@ROWCOUNT = 0
	BEGIN
		SET			@taxon_list_key		=	NULL
	END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptGroup_GetTaxonList') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptGroup_GetTaxonList'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptGroup_GetTaxonList TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptGroup_GetTaxonList TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptGroup_GetTaxonList TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptGroup_ImportTaxonList]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_ConceptGroup_ImportTaxonList]
GO

/*===========================================================================*\
  Description:	Import a taxon list into the specified concept group.

  Parameters:   @job_id					Job identifier
				@taxon_list_key			Taxon list key
				@concept_group_key		Concept group key

  Created:		Nov 2003

  Last revision information:
	$Revision: 7 $
	$Date: 6/02/09 10:41 $
	$Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptGroup_ImportTaxonList]
	@job_id				INT,
	@taxon_list_key		CHAR(16),
	@concept_group_key	CHAR(16)
AS
	SET NOCOUNT ON
	SET ARITHABORT ON

	DECLARE		@existing_group_key		CHAR(16)

	SELECT		@existing_group_key						=	Concept_Group_Key
	FROM		Taxon_Dictionary_Concept_Group_Mapping
	WHERE		Taxon_List_Key							=	@taxon_list_key

	IF @@ROWCOUNT = 0
	BEGIN
		BEGIN TRANSACTION

		/* record mapping */
		INSERT		Taxon_Dictionary_Concept_Group_Mapping (
					Taxon_List_Key,
					Concept_Group_Key)
		VALUES		(@taxon_list_key,
					@concept_group_key)

		IF @@ERROR <> 0 GOTO fail

		COMMIT TRANSACTION
	END
	ELSE IF @existing_group_key <> @concept_group_key
	BEGIN
		RAISERROR (
			'Taxon list has previously been imported into a different group',
			16,
			1)
		RETURN
	END

	/* Calculate size of job */
	DECLARE		@record_count					INT

	DECLARE		@items	TABLE (
				Taxon_List_Item_Key			CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS,
				Taxon_List_Version_Key		CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS,
				Taxon_Rank_Key				CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS,
				Taxon_Version_Key			CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS,
				List_Preferred_Key			CHAR(16))

	INSERT		@items
	SELECT    	tli.TAXON_LIST_ITEM_KEY,
				tli.TAXON_LIST_VERSION_KEY,
				tli.TAXON_RANK_KEY,
				tli.TAXON_VERSION_KEY,
				CASE WHEN tli.TAXON_LIST_ITEM_KEY = tli.PREFERRED_NAME
					THEN tli.TAXON_LIST_ITEM_KEY
					ELSE NULL
				END
	FROM        TAXON_LIST_VERSION			AS	tlv
	INNER JOIN	TAXON_LIST_ITEM				AS	tli
	ON			tli.TAXON_LIST_VERSION_KEY	=	tlv.TAXON_LIST_VERSION_KEY
	WHERE		tlv.TAXON_LIST_KEY			=	@taxon_list_key

	SELECT		@record_count				=	2 * COUNT(DISTINCT tli.Taxon_List_Item_Key)
												+ COUNT(DISTINCT tli.Taxon_List_Version_Key)
												+ COUNT(DISTINCT tli.Taxon_Rank_Key)
												+ COUNT(DISTINCT tli.Taxon_Version_Key)
												+ COUNT(DISTINCT tv.TAXON_KEY)
												+ COUNT(DISTINCT ts.SOURCE_LINK_KEY)
												+ COUNT(DISTINCT tx.TAXON_NAME_TYPE_KEY)
												+ COUNT(DISTINCT td.TAXON_DESIGNATION_KEY)
												+ COUNT(DISTINCT td.TAXON_DESIGNATION_TYPE_KEY)
												+ COUNT(DISTINCT tf.TAXON_FACT_KEY)
												+ COUNT(DISTINCT tli.List_Preferred_Key)
	FROM        @items						AS	tli
	INNER JOIN	TAXON_VERSION				AS	tv
	ON			tv.TAXON_VERSION_KEY		=	tli.Taxon_Version_Key
	INNER JOIN	TAXON						AS	tx
	ON			tx.TAXON_KEY				=	tv.TAXON_KEY
	LEFT JOIN	TAXON_SOURCES				AS	ts
	ON			ts.TAXON_KEY				=	tx.TAXON_KEY
	LEFT JOIN	TAXON_DESIGNATION			AS	td
	ON			td.TAXON_LIST_ITEM_KEY		=	tli.Taxon_List_Item_Key
	LEFT JOIN	TAXON_FACT					AS	tf
	ON			tf.TAXON_VERSION_KEY		=	tli.Taxon_Version_Key

	EXECUTE		usp_Import_Export_Job_Configure		@job_id,
													@concept_group_key,
													@record_count
	IF @@ERROR <> 0 RETURN

	/* import versions */
	EXECUTE		usp_ConceptGroupVersion_ImportTaxonList		@job_id
	IF @@ERROR <> 0 RETURN

	/* import terms */
	EXECUTE		usp_Term_ImportTaxonList	@job_id
	IF @@ERROR <> 0 RETURN

	/* import term versions */
	EXECUTE		usp_TermVersion_ImportTaxonList		@job_id
	IF @@ERROR <> 0 RETURN

	/* import concept ranks */
	EXECUTE		usp_ConceptRank_ImportTaxonList     @job_id
	IF @@ERROR <> 0 RETURN

	/* import name type concepts */
	EXECUTE		usp_Concept_ImportTaxonNameTypes    @job_id
	IF @@ERROR <> 0 RETURN

	/* import concepts */
	EXECUTE		usp_Concept_ImportTaxonList		@job_id
	IF @@ERROR <> 0 RETURN

	/* import concept relationships */
	EXECUTE		usp_ConceptRelation_ImportTaxonList		@job_id
	IF @@ERROR <> 0 RETURN

	/* (re-)create concept lineage */
	EXECUTE     usp_ConceptLineage_GenerateForGroup		@job_id
	IF @@ERROR <> 0 RETURN

	/* import term/source relationships */
	EXECUTE		usp_SourceJoin_ImportTaxonSources	@job_id
	IF @@ERROR <> 0 RETURN

	/* import designation types */
	EXECUTE		usp_Concept_ImportTaxonDesignationTypes		@job_id
	IF @@ERROR <> 0 RETURN

	/* import concept designations */
	EXECUTE		usp_ConceptDesignation_ImportTaxonList		@job_id
	IF @@ERROR <> 0 RETURN

	/* import thesaurus facts */
	EXECUTE		usp_ThesaurusFact_ImportTaxonList	@job_id
	IF @@ERROR <> 0 RETURN

	RETURN

fail:
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_ConceptGroup_ImportTaxonList failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptGroup_ImportTaxonList') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptGroup_ImportTaxonList'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptGroup_ImportTaxonList TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptGroup_ImportTaxonList TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptGroup_ImportTaxonList TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptGroup_Select_ForDomain]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptGroup_Select_ForDomain]
GO

/*===========================================================================*\
  Description:	Retrieves a list of concept groups for a supplied domain.

  Parameters:	@Domain - key of the domain

  Created:	August 2003

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptGroup_Select_ForDomain]
	@Domain char(16)
AS

SELECT CG.Concept_Group_Key as Item_Key, CG.Item_Name, CG.Hierarchy_Relation_Type_Key
FROM Local_Domain LD
INNER JOIN Concept_Group CG ON CG.Local_Domain_Key=LD.Local_Domain_Key
WHERE LD.Domain_Key=@Domain
ORDER BY CG.Item_Name

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptGroup_Select_ForDomain') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptGroup_Select_ForDomain'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptGroup_Select_ForDomain TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptGroup_Select_ForDomain TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptGroup_Select_ForDomain TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ConceptGroup_Select_ForDomain TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptGroup_Select_ForDomain TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptGroup_Select_ForDomain TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptHistory_Insert_Imported]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_ConceptHistory_Insert_Imported]
GO

/*===========================================================================*\
  Description:	Add a concept history record corresponding to a concept
				imported from a taxon dictionary.

  Parameters:	@ConceptKey				Concept identifier
				@GroupVersionFrom		Initial concept group identifier
				@GroupVersionTo			Final concept group identifier (if any)

  Created:		Nov 2003

  Last revision information:
	$Revision: 7 $
	$Date: 6/02/09 10:41 $
	$Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptHistory_Insert_Imported]
	@ConceptKey			CHAR(16),
	@GroupVersionFrom	CHAR(16),
	@GroupVersionTo		CHAR(16)
AS
	SET NOCOUNT ON

	DECLARE		@concept_history_key	CHAR(16),
				@from_start				INT,
				@from_end				INT,
				@from_type				VARCHAR(2),
				@to_start				INT,
				@to_end					INT,
				@to_type				VARCHAR(2)

	SELECT		@from_start					=	From_Vague_Date_Start,
				@from_end					=	From_Vague_Date_End,
				@from_type					=	From_Vague_Date_Type
	FROM		Concept_Group_Version
	WHERE		Concept_Group_Version_Key	=   @GroupVersionFrom

	IF @GroupVersionTo IS NOT NULL
	BEGIN
		SELECT		@to_start					=	To_Vague_Date_Start,
					@to_end						=	To_Vague_Date_End,
					@to_type					=	To_Vague_Date_Type
		FROM		Concept_Group_Version
		WHERE		Concept_Group_Version_Key	=	@GroupVersionTo
	END

	/* write history record */
	EXECUTE		spNextKey	'Concept_History',
							@concept_history_key	OUTPUT
	IF @@ERROR <> 0 GOTO fail

	INSERT		Concept_History (
				Concept_History_Key,
				Concept_Key,
				Concept_Group_Version_From,
				Concept_Group_Version_To,
				From_Vague_Date_Start,
				From_Vague_Date_End,
				From_Vague_Date_Type,
				To_Vague_Date_Start,
				To_Vague_Date_End,
				To_Vague_Date_Type,
				Entered_Session_ID,
				Changed_Session_ID,
				System_Supplied_Data)
	SELECT      @concept_history_key,
				@ConceptKey,
				@GroupVersionFrom,
				@GroupVersionTo,
				@from_start,
				@from_end,
				@from_type,
				@to_start,
				@to_end,
				@to_type,
				c.Entered_Session_ID,
				c.Changed_Session_ID,
				c.System_Supplied_Data
	FROM		Concept							AS	c
	WHERE		c.Concept_Key					=	@ConceptKey				

	IF @@ERROR <> 0 GOTO fail
	RETURN

fail:
	RAISERROR ('usp_ConceptHistory_Insert_Imported failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptHistory_Insert_Imported') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptHistory_Insert_Imported'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptHistory_Insert_Imported TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptHistory_Insert_Imported TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptHistory_Insert_Imported TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptLineage_ConceptUpdated]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_ConceptLineage_ConceptUpdated]
GO

/*===========================================================================*\
  Description:	Make changes to lineage corresponding to a change in a
				concept.

  Parameters:	@concept_key			Concept key
				@old_concept_group_key	Original concept group key
				@old_list_preferred		Original "list preferred" flag

  Created:		Jan 2004

  Last revision information:
	$Revision: 7 $
	$Date: 6/02/09 10:41 $
	$Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptLineage_ConceptUpdated]
	@concept_key				CHAR(16),
	@old_concept_group_key      CHAR(16),
	@old_list_preferred			BIT
AS
	SET NOCOUNT ON

	DECLARE		@new_concept_group_key		CHAR(16),
				@new_list_preferred			BIT

	SELECT		@new_concept_group_key	=	Concept_Group_Key,
				@new_list_preferred		=	List_Preferred
	FROM		Concept
	WHERE		Concept_Key			   	=	@concept_key

	IF @@ROWCOUNT = 0
	BEGIN
		RAISERROR ('Concept does not exist', 16, 1)
		RETURN
	END

	BEGIN TRANSACTION

	IF @old_concept_group_key <> @new_concept_group_key
		OR (@old_list_preferred = 1 AND @new_list_preferred = 0)
	BEGIN
		/* remove lineage (if any) from original concept group */
		DELETE		dl
		FROM		Concept_Lineage		AS	cl
		INNER JOIN	Concept_Lineage		AS	dl
		ON			dl.Lineage			LIKE cl.Lineage + '\%'
		INNER JOIN	Concept				AS	d
		ON			d.Concept_Key		=	dl.Concept_Key
		WHERE		cl.Concept_Key		=	@concept_key
		AND			d.Concept_Group_Key	=	@old_concept_group_key

		IF @@ERROR <> 0 GOTO fail

		EXECUTE		usp_ConceptLineage_CreateForOrphans	@old_concept_group_key
		IF @@ERROR <> 0 GOTO fail

		DELETE		Concept_Lineage
		WHERE		Concept_Key			=	@concept_key

		IF @@ERROR <> 0 GOTO fail
	END

	IF  @new_list_preferred = 1
		AND (@old_list_preferred = 0
			OR @old_concept_group_key <> @new_concept_group_key)
	BEGIN
		/* create lineage (if required) in new concept group */
		EXECUTE		usp_ConceptLineage_CreateSubTree	@concept_key,
														NULL
		IF @@ERROR <> 0 GOTO fail
	END

	COMMIT TRANSACTION
	RETURN

fail:
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_ConceptLineage_ConceptUpdated failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptLineage_ConceptUpdated') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptLineage_ConceptUpdated'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptLineage_ConceptUpdated TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptLineage_ConceptUpdated TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptLineage_ConceptUpdated TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptLineage_CreateForOrphans]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_ConceptLineage_CreateForOrphans]
GO

/*===========================================================================*\
  Description:	Generate concept lineage for orphaned concepts in a specified
				concept group.

  Parameters:	@concept_group_key		Concept group key

  Created:		Jan 2004

  Last revision information:
	$Revision: 7 $
	$Date: 6/02/09 10:41 $
	$Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptLineage_CreateForOrphans]
	@concept_group_key	CHAR(16)
AS
	SET NOCOUNT ON

	DECLARE		@relation_type_key	CHAR(16),
				@concept_key		CHAR(16)

	/* determine whether the group has a hierarchical relation */
	SELECT		@relation_type_key	=	Hierarchy_Relation_Type_Key
	FROM		Concept_Group
	WHERE		Concept_Group_Key	=	@concept_group_key

	IF @@ROWCOUNT = 0
	BEGIN
		RAISERROR ('Concept group does not exist', 16, 1)
		GOTO fail
	END

	IF @relation_type_key IS NOT NULL
	BEGIN
		BEGIN TRANSACTION

		DECLARE		orphans					CURSOR LOCAL FAST_FORWARD FOR
		SELECT		c.Concept_Key
		FROM		Concept					AS	c
		WHERE		c.Concept_Group_Key		=	@concept_group_key
		AND			c.List_Preferred		=	1
		AND			NOT EXISTS (
						SELECT		1
						FROM		Concept_Lineage
						WHERE		Concept_Key			=	c.Concept_Key)
		AND			NOT EXISTS (
						SELECT		1
						FROM		Concept_Relation				AS	r
						INNER JOIN	Concept							AS	p
						ON			p.Concept_Key					=	r.From_Concept_Key
						WHERE		r.To_Concept_Key				=	c.Concept_Key
						AND			r.Thesaurus_Relation_Type_Key	=	@relation_type_key
						AND			p.Concept_Group_Key				=	@concept_group_key
						AND			p.List_Preferred				=	1)

		OPEN		orphans

		WHILE 1 = 1
		BEGIN
			FETCH		orphans
			INTO		@concept_key

			IF @@FETCH_STATUS <> 0 BREAK

			EXECUTE		usp_ConceptLineage_NewConcept	@concept_key
			IF @@ERROR <> 0 GOTO fail_from_cursor	
		END

		CLOSE		orphans

		COMMIT TRANSACTION
	END
	RETURN

fail_from_cursor:
	CLOSE		orphans

fail:
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_ConceptLineage_CreateForOrphans failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptLineage_CreateForOrphans') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptLineage_CreateForOrphans'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptLineage_CreateForOrphans TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptLineage_CreateForOrphans TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptLineage_CreateForOrphans TO [Dev - JNCC SQL]
END
GO


/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptLineage_CreateSubtree]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_ConceptLineage_CreateSubtree]
GO

/*===========================================================================*\
  Description:	Create lineage records beneath the given parent lineage for
				the specified concept and its descendants.

				If the concept is not list preferred or the group has no
				hierarchical relation then no lineage records are created. 

  Parameters:	@concept_key			Concept key
				@parent_lineage			Parent lineage
				@job_id					[optional] Import job identifier

  Created:		Dec 2003

  Last revision information:
	$Revision: 7 $
	$Date: 6/02/09 10:41 $
	$Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptLineage_CreateSubtree]
	@concept_key		CHAR(16),
	@parent_lineage		VARCHAR(900),
	@job_id				INT				=	NULL
AS
	SET NOCOUNT ON

	DECLARE     @list_preferred			BIT,
				@concept_group_key		CHAR(16),
				@relation_type_key		CHAR(16),
				@lineage				VARCHAR(900),
				@child_concept_key		CHAR(16)

	SELECT      @list_preferred			=	c.List_Preferred,
				@concept_group_key		=	c.Concept_Group_Key,
				@relation_type_key		=	g.Hierarchy_Relation_Type_Key
	FROM		Concept					AS	c
	INNER JOIN	Concept_Group			AS	g
	ON			g.Concept_Group_Key		=	c.Concept_Group_Key
	WHERE		c.Concept_Key			=	@concept_key

	IF @@ROWCOUNT = 0
	BEGIN
		RAISERROR ('Concept does not exist', 16, 1)
		RETURN
	END

	IF @list_preferred = 1 AND @relation_type_key IS NOT NULL
	BEGIN
		IF @job_id IS NULL BEGIN TRANSACTION

		DECLARE @pending_lineage TABLE	(
						Concept_Key		CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS PRIMARY KEY,
						Parent_Lineage  VARCHAR(900) COLLATE SQL_Latin1_General_CP1_CI_AS NULL)

		INSERT		@pending_lineage (
					Concept_Key,
					Parent_Lineage)
		VALUES		(@concept_key,
					@parent_lineage)

		WHILE @concept_key IS NOT NULL
		BEGIN
			IF @job_id IS NOT NULL BEGIN TRANSACTION

			/* create lineage for current concept */
			SET			@lineage		=	dbo.ufn_NextChildLineage(
													@parent_lineage,
													@concept_group_key)

			INSERT		Concept_Lineage (
						Concept_Key,
						Lineage_ID,
						Lineage)
			SELECT		@concept_key,
						ISNULL(MAX(Lineage_ID), 0) + 1,
						@lineage
			FROM		Concept_Lineage
			WHERE		Concept_Key			=	@concept_key

			IF @@ERROR <> 0 GOTO fail

			/* record lineage sequence number */
			IF @parent_lineage IS NULL
			BEGIN
				UPDATE		Concept_Group
				SET			Last_Sequence_Number	=	@lineage
				WHERE		Concept_Group_Key		=	@concept_group_key

				IF @@ERROR <> 0 GOTO fail
			END
			ELSE
			BEGIN
				UPDATE		Concept_Lineage
				SET			Last_Sequence_Number	=	dbo.ufn_LineageSequenceNumber(@lineage)
				FROM		Concept_Lineage			AS	l
				INNER JOIN	Concept					AS	c
				ON			c.Concept_Key			=	l.Concept_Key
				WHERE		l.Lineage				=	@parent_lineage
				AND			c.Concept_Group_Key		=	@concept_group_key

				IF @@ERROR <> 0 GOTO fail
			END

			/* remove current concept from pending list */
			DELETE		@pending_lineage
			WHERE		Concept_Key			=	@concept_key

			IF @@ERROR <> 0 GOTO fail

			/* add offspring of current concept to pending list */
			INSERT		@pending_lineage (
						Concept_Key,
						Parent_Lineage)
			SELECT		r.To_Concept_Key,
						@lineage
			FROM		Concept_Relation				AS	r
			INNER JOIN Concept AS c	ON c.Concept_Key	=	r.To_Concept_Key
			LEFT JOIN @pending_lineage pl ON pl.Concept_Key=C.Concept_Key
			WHERE       r.From_Concept_Key				=	@concept_key
			AND			r.Thesaurus_Relation_Type_Key	=	@relation_type_key
			AND			c.Concept_Group_Key				=	@concept_group_key
			AND			c.List_Preferred				=	1
			AND 		pl.Concept_Key IS NULL

			IF @@ERROR <> 0 GOTO fail

			/* select the next concept (if any) for processing */
			SET ROWCOUNT 1

			SELECT		@concept_key		=	Concept_Key,
						@parent_lineage		=	Parent_Lineage
			FROM		@pending_lineage

			IF @@ROWCOUNT = 0
				SET			@concept_key		=	NULL

			SET ROWCOUNT 0

			IF @job_id IS NOT NULL
			BEGIN
				EXECUTE		usp_Import_Export_Job_RecordProcessed	@job_id
				IF @@ERROR <> 0 GOTO fail

				COMMIT TRANSACTION
			END
		END  /* WHILE @concept_key IS NOT NULL */

		IF @job_id IS NULL COMMIT TRANSACTION
	END /* IF @list_preferred = 1 AND ... */
	RETURN

fail:
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_ConceptLineage_CreateSubtree failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptLineage_CreateSubtree') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptLineage_CreateSubtree'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptLineage_CreateSubtree TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptLineage_CreateSubtree TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptLineage_CreateSubtree TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptLineage_DeleteConcept]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_ConceptLineage_DeleteConcept]
GO

/*===========================================================================*\
  Description:	Remove lineage corresponding to a specified concept.

  Parameters:	@concept_key			Concept key

  Created:		Jan 2004

  Last revision information:
	$Revision: 7 $
	$Date: 6/02/09 10:41 $
	$Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptLineage_DeleteConcept]
	@concept_key				CHAR(16)
AS
	SET NOCOUNT ON

	DECLARE		@concept_group_key		CHAR(16)

	SELECT		@concept_group_key	=	Concept_Group_Key
	FROM		Concept
	WHERE		Concept_Key			=	@concept_key

	IF @@ROWCOUNT = 0
	BEGIN
		RAISERROR ('Concept does not exist', 16, 1)
		RETURN
	END

	BEGIN TRANSACTION

	/* delete lineage for descendants */
	DELETE		dl
	FROM		Concept_Lineage		AS	cl
	INNER JOIN	Concept_Lineage		AS	dl
	ON			dl.Lineage			LIKE cl.Lineage + '\%'
	INNER JOIN	Concept				AS	d
	ON			d.Concept_Key		=	dl.Concept_Key
	WHERE		cl.Concept_Key		=	@concept_key
	AND			d.Concept_Group_Key	=	@concept_group_key

	IF @@ERROR <> 0 GOTO fail

	/* create top-level lineage for newly orphaned concepts */
	EXECUTE		usp_ConceptLineage_CreateForOrphans		@concept_group_key
	IF @@ERROR <> 0 GOTO fail

	/* delete lineage for specified concept */
	DELETE		Concept_Lineage
	WHERE		Concept_Key			=	@concept_key

	IF @@ERROR <> 0 GOTO fail

	COMMIT TRANSACTION
	RETURN

fail:
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_ConceptLineage_DeleteConcept failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptLineage_DeleteConcept') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptLineage_DeleteConcept'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptLineage_DeleteConcept TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptLineage_DeleteConcept TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptLineage_DeleteConcept TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptLineage_DeleteSubtree]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_ConceptLineage_DeleteSubtree]
GO

/*===========================================================================*\
  Description:	Delete specified concept lineage and its descendants.

  Parameters:	@concept_group_key		Concept group key
				@lineage				Parent lineage

  Created:		Jan 2004

  Last revision information:
	$Revision: 7 $
	$Date: 6/02/09 10:41 $
	$Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptLineage_DeleteSubtree]
	@concept_group_key		CHAR(16),
	@lineage				VARCHAR(900)
AS
	SET NOCOUNT ON

	BEGIN TRANSACTION

	IF LEN(ISNULL(@lineage, '')) = 0
	BEGIN
		SET			@lineage				=	'%'

		UPDATE		Concept_Group
		SET			Last_Sequence_Number	=	NULL
		WHERE		Concept_Group_Key		=	@concept_group_key

		IF @@ERROR <> 0 GOTO fail
	END
	ELSE
	BEGIN
		/* delete specified lineage record */
		DELETE		Concept_Lineage
		FROM		Concept					AS	c
		INNER JOIN	Concept_Lineage			AS	l
		ON			l.Concept_Key			=	c.Concept_Key
		WHERE		c.Concept_Group_Key		=	@concept_group_key
		AND			l.Lineage				=	@lineage

		IF @@ERROR <> 0 GOTO fail

		SET			@lineage				=	@lineage + '\%'
	END

	/* delete descendants */
	DELETE		Concept_Lineage
	FROM		Concept					AS	c
	INNER JOIN	Concept_Lineage			AS	l
	ON			l.Concept_Key			=	c.Concept_Key
	WHERE		c.Concept_Group_Key		=	@concept_group_key
	AND			l.Lineage				LIKE @lineage

	IF @@ERROR <> 0 GOTO fail	

	COMMIT TRANSACTION
	RETURN

fail:
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_ConceptLineage_DeleteSubtree failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptLineage_DeleteSubtree') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptLineage_DeleteSubtree'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptLineage_DeleteSubtree TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptLineage_DeleteSubtree TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptLineage_DeleteSubtree TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptLineage_GenerateForGroup]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_ConceptLineage_GenerateForGroup]
GO

/*===========================================================================*\
  Description:	Generate (or re-generate) concept lineage for a specified
				concept group.

				Either @job_id or @concept_group_key must be supplied.

  Parameters:	@job_id					Job identifier
				@concept_group_key		Concept group key

  Created:		Dec 2003

  Last revision information:
	$Revision: 7 $
	$Date: 6/02/09 10:41 $
	$Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptLineage_GenerateForGroup]
	@job_id				INT			=	NULL,
	@concept_group_key	CHAR(16)	=	NULL
AS
	SET NOCOUNT ON

	DECLARE     @hierarchy_relation_type_key	CHAR(16)

	IF @job_id IS NULL
	BEGIN
		IF @concept_group_key IS NULL
		BEGIN
			RAISERROR ('Concept group or Job must be specified', 16, 1)
			RETURN
		END
	END
	ELSE
	BEGIN
		/* determine parameters of job */
		SELECT		@concept_group_key		=	Concept_Group_Key
		FROM		Import_Export_Job
		WHERE		Import_Export_Job_ID	=	@job_id

		IF @@ROWCOUNT = 0
		BEGIN
			RAISERROR ('Job does not exist or has not been configured', 16, 1)
			RETURN
		END

		EXECUTE		usp_Import_Export_Job_UpdateStatus	@job_id,
														'Generating lineage'
		IF @@ERROR <> 0 RETURN
	END

	/* determine whether the group has a hierarchical relation */
	SELECT		@hierarchy_relation_type_key	=	Hierarchy_Relation_Type_Key
	FROM		Concept_Group
	WHERE		Concept_Group_Key				=	@concept_group_key

	IF @@ROWCOUNT = 0
	BEGIN
		RAISERROR ('Concept group does not exist', 16, 1)
		RETURN
	END

	/* remove existing concept lineage records */
	EXECUTE		usp_ConceptLineage_DeleteSubtree	@concept_group_key,
													NULL
	IF @@ERROR <> 0 RETURN

	IF @hierarchy_relation_type_key IS NOT NULL
	BEGIN
		DECLARE		@concept_key	CHAR(16)

		/* create/update concept lineage */
		DECLARE		root_concepts			CURSOR LOCAL STATIC FOR
		SELECT		c.Concept_Key
		FROM		Concept					AS	c
		WHERE		c.Concept_Group_Key		=	@concept_group_key
		AND			c.List_Preferred		=	1
		AND			NOT EXISTS (
							SELECT		1
							FROM		Concept_Relation				AS	r
							INNER JOIN	Concept							AS	p
							ON			p.Concept_Key					=	r.From_Concept_Key
							WHERE		r.To_Concept_Key				=	c.Concept_Key
							AND			r.Thesaurus_Relation_Type_Key	=	@hierarchy_relation_type_key
							AND			p.Concept_Group_Key				=	@concept_group_key
							AND			p.List_Preferred				=	1)

		OPEN		root_concepts

		WHILE 1 = 1
		BEGIN
			FETCH		root_concepts
			INTO		@concept_key

			IF @@FETCH_STATUS <> 0 BREAK

			EXECUTE		usp_ConceptLineage_CreateSubtree	@concept_key,
															NULL,
															@job_id
			IF @@ERROR <> 0 GOTO fail_from_cursor
		END

		CLOSE		root_concepts
	END
	RETURN

fail_from_cursor:
	CLOSE		root_concepts

fail:
	RAISERROR ('usp_ConceptLineage_GenerateForGroup failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptLineage_GenerateForGroup') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptLineage_GenerateForGroup'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptLineage_GenerateForGroup TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptLineage_GenerateForGroup TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptLineage_GenerateForGroup TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptLineage_Get]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptLineage_Get]
GO

/*===========================================================================*\
  Description: Returns a list of concept lineages for the supplied concept

  Parameters:	@ConceptKey
							@IncludeSynonyms - if 1 then lineages for all synonyms are returned
							@UserDomainMask

  Created:	August 2003

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptLineage_Get]
	@ConceptKey VARCHAR(100),
  @IncludeSynonyms BIT,
  @UserDomainMask INT
AS

IF @IncludeSynonyms=1
	SELECT DISTINCT CSyn.Concept_Key, CL.Lineage_ID, CL.Lineage, CSyn.Concept_Rank_Key
  FROM Concept C
  INNER JOIN Concept CSyn on CSyn.Meaning_Key=C.Meaning_Key
  INNER JOIN Concept_Lineage CL on CL.Concept_Key=CSyn.Concept_Key
  INNER JOIN Concept_Group CG on CG.Concept_Group_Key=CSyn.Concept_Group_Key
  INNER JOIN Local_Domain LD on LD.Local_Domain_Key=CG.Local_Domain_Key
  INNER JOIN Domain D on D.Domain_Key=LD.Domain_Key
      AND ((D.Domain_Mask & @UserDomainMask > 0) OR D.Has_Occurrences=0)
	WHERE C.Concept_Key=@ConceptKey
	AND 	PATINDEX('%\%', CL.Lineage)<>0

ELSE
	SELECT C.Concept_Key, CL.Lineage_ID, Lineage, C.Concept_Rank_Key
  FROM Concept C
  INNER JOIN Concept_Lineage CL on CL.Concept_Key=C.Concept_Key
	WHERE C.Concept_Key=@ConceptKey
	AND 	PATINDEX('%\%', CL.Lineage)<>0

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptLineage_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptLineage_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptLineage_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptLineage_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptLineage_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ConceptLineage_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptLineage_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptLineage_Get TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptLineage_NewConcept]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_ConceptLineage_NewConcept]
GO

/*===========================================================================*\
  Description:	Create concept lineage record for a new concept, if required.

  Parameters:	@concept_key			Concept key

  Created:		Jan 2004

  Last revision information:
	$Revision: 7 $
	$Date: 6/02/09 10:41 $
	$Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptLineage_NewConcept]
	@concept_key		CHAR(16)
AS
	SET NOCOUNT ON

	EXECUTE		usp_ConceptLineage_CreateSubTree	@concept_key,
													NULL
	IF @@ERROR <> 0 GOTO fail
	RETURN

fail:
	RAISERROR ('usp_ConceptLineage_NewConcept failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptLineage_NewConcept') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptLineage_NewConcept'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptLineage_NewConcept TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptLineage_NewConcept TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptLineage_NewConcept TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptLineage_NewRelation]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_ConceptLineage_NewRelation]
GO

/*===========================================================================*\
  Description:	Create concept lineage records as required for a new concept
				relationship.  Called from usp_ConceptLineage_Insert.

  Parameters:	@concept_relation_key	Concept relation key

  Created:		Jan 2004

  Last revision information:
	$Revision: 7 $
	$Date: 6/02/09 10:41 $
	$Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptLineage_NewRelation]
	@concept_relation_key		CHAR(16)
AS
	SET NOCOUNT ON

	DECLARE		@parent_key				CHAR(16),
				@child_key				CHAR(16),
				@relation_type_key		CHAR(16),
				@concept_group_key		CHAR(16),
				@child_lineage			VARCHAR(900),
				@parent_lineage			VARCHAR(900)

	SELECT		@parent_key				=	From_Concept_Key,
				@child_key				=	To_Concept_Key,
				@relation_type_key		=	Thesaurus_Relation_Type_Key
	FROM		Concept_Relation
	WHERE		Concept_Relation_Key	=	@concept_relation_key

	IF @@ROWCOUNT = 0
	BEGIN
		RAISERROR ('Concept relationship does not exist', 16, 1)
		RETURN
	END

	/* do nothing unless relationship should appear in the lineage */
	IF dbo.ufn_ConceptRelationAffectsLineage(
				@parent_key,
				@child_key,
				@relation_type_key) = 1
	BEGIN
		BEGIN TRANSACTION

		/* if the child has no other parents, remove its existing lineage */
		SELECT		@concept_group_key	=	c.Concept_Group_Key,
					@child_lineage		=	l.Lineage
		FROM		Concept				AS	c
		INNER JOIN	Concept_Lineage		AS	l
		ON			l.Concept_Key		=	c.Concept_Key
		WHERE		c.Concept_Key		=	@child_key

		IF @@ROWCOUNT = 1 AND CHARINDEX('\', @child_lineage) = 0
		BEGIN
			EXECUTE		usp_ConceptLineage_DeleteSubTree	@concept_group_key,
															@child_lineage
			IF @@ERROR <> 0 GOTO fail
		END

		/* for each lineage record of the parent, create lineage records for
		 * the child and its descendants
		 */ 
		DECLARE		lineage				CURSOR LOCAL FAST_FORWARD FOR
		SELECT		Lineage
		FROM		Concept_Lineage
		WHERE		Concept_Key			=	@parent_key

		OPEN		lineage

		WHILE 1 = 1
		BEGIN
			FETCH		lineage
			INTO		@parent_lineage

			IF @@FETCH_STATUS <> 0 BREAK

			EXECUTE		usp_ConceptLineage_CreateSubtree	@child_key,
															@parent_lineage
			IF @@ERROR <> 0 GOTO fail_from_cursor
		END

		CLOSE 		lineage

		COMMIT TRANSACTION
	END
	RETURN

fail_from_cursor:
	CLOSE		lineage

fail:
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_ConceptLineage_NewRelation failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptLineage_NewRelation') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptLineage_NewRelation'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptLineage_NewRelation TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptLineage_NewRelation TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptLineage_NewRelation TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptLineage_RelationDeleted]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_ConceptLineage_RelationDeleted]
GO

/*===========================================================================*\
  Description:	Remove concept lineage associated with a concept relationship.

  Parameters:	@concept_relation_key	Concept relation key

  Created:		Jan 2004

  Last revision information:
	$Revision: 7 $
	$Date: 6/02/09 10:41 $
	$Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptLineage_RelationDeleted]
	@from_concept_key		CHAR(16),
	@to_concept_key			CHAR(16),
	@relation_type_key		CHAR(16)
AS
	SET NOCOUNT ON

	DECLARE		@concept_group_key		CHAR(16),
				@child_lineage			VARCHAR(900)

	/* do nothing unless relationship appears in the lineage */
	IF dbo.ufn_ConceptRelationAffectsLineage(
				@from_concept_key,
				@to_concept_key,
				@relation_type_key) = 1
	BEGIN
		BEGIN TRANSACTION

		SELECT		@concept_group_key	=	Concept_Group_Key
		FROM		Concept
		WHERE		Concept_Key			=	@from_concept_key

		IF @@ROWCOUNT = 0
		BEGIN
			RAISERROR ('Source concept does not exist', 16, 1)
			RETURN
		END

		DECLARE		lineage				CURSOR LOCAL FAST_FORWARD FOR
		SELECT		cl.Lineage
		FROM		Concept_Lineage		AS	pl
		INNER JOIN	Concept_Lineage		AS	cl
		ON			cl.Lineage			LIKE pl.Lineage + '\%'
		WHERE		pl.Concept_Key	   	=	@from_concept_key
		AND         cl.Concept_Key		=	@to_concept_key

		OPEN		lineage

		WHILE 1 = 1
		BEGIN
			FETCH		lineage
			INTO		@child_lineage

			IF @@FETCH_STATUS <> 0 BREAK

			EXECUTE		usp_ConceptLineage_DeleteSubTree	@concept_group_key,
															@child_lineage
			IF @@ERROR <> 0 GOTO fail_from_cursor
		END

		CLOSE		lineage

		/* if child concept is now an orphan, create top-level lineage */
		IF NOT EXISTS (	SELECT		1
						FROM		Concept_Lineage
						WHERE		Concept_Key			=	@to_concept_key )
		BEGIN
			EXECUTE		usp_ConceptLineage_CreateSubTree	@to_concept_key,
															NULL
			IF @@ERROR <> 0 GOTO fail
		END

		COMMIT TRANSACTION
	END
	RETURN

fail_from_cursor:
	CLOSE		lineage

fail:
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_ConceptLineage_RelationDeleted failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptLineage_RelationDeleted') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptLineage_RelationDeleted'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptLineage_RelationDeleted TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptLineage_RelationDeleted TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptLineage_RelationDeleted TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptLineage_UpdateRelation]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_ConceptLineage_UpdateRelation]
GO

/*===========================================================================*\
  Description:	Make changes to lineage corresponding to a change in a
				concept relationship.

  Parameters:	@concept_relation_key	Concept relation key
				@old_from_concept_key	Original source concept key
				@old_to_concept_key		Original destination concept key
				@old_type_key			Original relation type key

  Created:		Dec 2003

  Last revision information:
	$Revision: 7 $
	$Date: 6/02/09 10:41 $
	$Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptLineage_UpdateRelation]
	@concept_relation_key		CHAR(16),
	@old_from_concept_key		CHAR(16),
	@old_to_concept_key			CHAR(16),
	@old_type_key				CHAR(16)
AS
	SET NOCOUNT ON

	DECLARE		@new_type_key			CHAR(16),
				@new_from_concept_key	CHAR(16),
				@new_to_concept_key		CHAR(16)

	SELECT		@new_type_key			=	Thesaurus_Relation_Type_Key,
				@new_from_concept_key	=	From_Concept_Key,
				@new_to_concept_key		=	To_Concept_Key
	FROM		Concept_Relation
	WHERE		Concept_Relation_Key	=	@concept_relation_key

	IF @@ROWCOUNT = 0
	BEGIN
		RAISERROR ('Concept relationship does not exist', 16, 1)
		RETURN
	END

	IF @old_type_key <> @new_type_key
		OR @old_from_concept_key <> @new_from_concept_key
		OR @old_to_concept_key <> @new_to_concept_key
	BEGIN
		IF dbo.ufn_ConceptRelationAffectsLineage(
					@old_from_concept_key,
					@old_to_concept_key,
					@old_type_key) = 1
		BEGIN
			/* remove old lineage */
			EXECUTE		usp_ConceptLineage_RelationDeleted	@old_from_concept_key,
															@old_to_concept_key,
															@old_type_key
			IF @@ERROR <> 0 GOTO fail
		END

		IF dbo.ufn_ConceptRelationAffectsLineage(
					@new_from_concept_key,
					@new_to_concept_key,
					@new_type_key) = 1
		BEGIN
			/* create new lineage */
			EXECUTE		usp_ConceptLineage_NewRelation	@concept_relation_key
			IF @@ERROR <> 0 GOTO fail
		END
    END
	RETURN

fail:
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_ConceptLineage_UpdateRelation failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptLineage_UpdateRelation') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptLineage_UpdateRelation'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptLineage_UpdateRelation TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptLineage_UpdateRelation TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptLineage_UpdateRelation TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptRankColor_ForDetermination_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptRankColor_ForDetermination_Select]
GO

/*===========================================================================*\
  Description:	Selects the RGB color components for a Determination /
		Taxon_Determination key.

  Parameters:	@Key		(Taxon) Determination key
		@IsLifeScience

  Created:	April 2004

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptRankColor_ForDetermination_Select]
	@Key char(16),
	@IsLifeScience bit
AS

SET NOCOUNT ON

	IF @IsLifeScience = 1 
		SELECT 		IsNull(CR.Color_R, 0) AS Color_R,
				IsNull(CR.Color_G, 0) AS Color_G,
				IsNull(CR.Color_B, 0) AS Color_B
		FROM		Taxon_Determination AS TD
		LEFT JOIN	Concept AS C ON C.Concept_Key = TD.Nomenclatural_Status_Concept_Key 
		LEFT JOIN	Concept_Rank AS CR ON CR.Concept_Rank_Key = C.Concept_Rank_Key
		WHERE		TD.Taxon_Determination_Key = @Key
	ELSE
		SELECT 		IsNull(CR.Color_R, 0) AS Color_R,
				IsNull(CR.Color_G, 0) AS Color_G,
				IsNull(CR.Color_B, 0) AS Color_B
		FROM		Determination AS D
		LEFT JOIN	Concept AS C ON C.Concept_Key = D.Nomenclatural_Status_Concept_Key
		LEFT JOIN 	Concept_Rank CR ON CR.Concept_Rank_Key = C.Concept_Rank_Key
		WHERE		D.Determination_Key = @Key		

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptRankColor_ForDetermination_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptRankColor_ForDetermination_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptRankColor_ForDetermination_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptRankColor_ForDetermination_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptRankColor_ForDetermination_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ConceptRankColor_ForDetermination_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptRankColor_ForDetermination_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptRankColor_ForDetermination_Select TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptRanks_FixDomain]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptRanks_FixDomain]
GO

/*===========================================================================*\
  Description:	When pasting concepts into a concept group, ensure that the
		ranks are also copied

  Parameters:	@FixDomainKey - domain key which should be checked for fixing
							@SessionID
							@SystemSuppliedData - both used only if new ranks must be created.

  Created:	July 2004

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptRanks_FixDomain]
	@FixDomainKey CHAR(16),	
	@SessionID CHAR(16),
	@SystemSuppliedData bit
AS

BEGIN TRANSACTION

-- cursor variables
DECLARE 
	@ConceptKey CHAR(16),
	@ConceptRankKey CHAR(16),
	@DomainKey CHAR(16),
	@RankDomainKey CHAR(16),
	@RankName VARCHAR(100)

DECLARE @ExistingRankKey CHAR(16)
DECLARE @NewRankKey CHAR(16)

DECLARE csr CURSOR FOR
	SELECT distinct
		C.Concept_Key, 
		C.Concept_Rank_Key, 
		LD.Domain_Key, 
		CR.Domain_Key AS rank_domain_key,
		CR.Item_Name
	FROM Concept c
	INNER JOIN Concept_Rank cr ON cr.Concept_Rank_Key=c.Concept_Rank_Key
	INNER JOIN concept_group cg ON cg.Concept_Group_Key=c.Concept_Group_Key
	INNER JOIN Local_Domain ld ON ld.Local_Domain_Key=cg.Local_Domain_Key
	WHERE ld.Domain_Key<>cr.Domain_Key
	AND LD.Domain_Key=@FixDomainKey
OPEN csr

FETCH NEXT FROM csr 
INTO @ConceptKey, @ConceptRankKey, @DomainKey, @RankDomainKey, @RankName

WHILE @@FETCH_STATUS=0
BEGIN
	--Find if a suitable rank exists in the domain
	SELECT @ExistingRankKey=Concept_Rank_Key
	FROM Concept_Rank
	WHERE Domain_Key=@DomainKey
	AND Item_Name=@RankName
	IF @@Error<>0 GOTO Rollbackandexit

	IF @ExistingRankKey IS NOT NULL
		UPDATE Concept
		SET Concept_Rank_Key=@ExistingRankKey
		WHERE Concept_Key=@ConceptKey
	ELSE 
	BEGIN
		--Clone old rank into new domain
		EXECUTE spNextKey 'Concept_Rank', @NewRankKey OUTPUT
	IF @@Error<>0 GOTO Rollbackandexit
		
	INSERT INTO Concept_Rank (
				Concept_Rank_Key,
				Domain_Key,
				Item_Name, 
				Sort_Order, 
				Abbreviation, 
				Color_R, 
				Color_G, 
				Color_B, 
				Entered_Session_ID,
				System_Supplied_Data,
				Custodian)
			SELECT 
				@NewRankKey, 
				@DomainKey, 
				Item_Name, 
				Sort_Order, 
				Abbreviation, 
				Color_R, 
				Color_G, 
				Color_B, 
				@SessionID,
				@SystemSuppliedData,
				LEFT(@NewRankKey, 8)
			FROM Concept_Rank
			WHERE Concept_Rank_Key=@ConceptRankKey
		IF @@Error<>0 GOTO Rollbackandexit

		UPDATE Concept
		SET Concept_Rank_Key=@NewRankKey
		WHERE Concept_Key=@ConceptKey
		IF @@Error<>0 GOTO Rollbackandexit
	END
	
	FETCH NEXT FROM csr 
	INTO @ConceptKey, @ConceptRankKey, @DomainKey, @RankDomainKey, @RankName
END

CLOSE csr
DEALLOCATE csr
   


COMMIT TRANSACTION
RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptRanks_FixDomain') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptRanks_FixDomain'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptRanks_FixDomain TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptRanks_FixDomain TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptRanks_FixDomain TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptRanks_FixDomain TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptRanks_FixDomain TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptRank_ImportTaxonList]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_ConceptRank_ImportTaxonList]
GO

/*===========================================================================*\
  Description:	Import concept ranks corresponding to taxon ranks from the
				specified taxon list.

  Parameters:   @job_id					Job identifier

  Created:		Nov 2003

  Last revision information:
	$Revision: 7 $
	$Date: 6/02/09 10:41 $
	$Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptRank_ImportTaxonList]
	@job_id				INT
AS
	SET NOCOUNT ON

	DECLARE     @taxon_list_key		CHAR(16),
				@domain_key			CHAR(16),
				@taxon_rank_key		CHAR(16),
				@item_name			VARCHAR(100),
				@sort_order			INT,
				@abbreviation		VARCHAR(10),
				@ins_user_key		CHAR(16),
				@ins_date			SMALLDATETIME,
				@ins_session_id		CHAR(16),
				@upd_user_key		CHAR(16),
				@upd_date			SMALLDATETIME,
				@upd_session_id		CHAR(16),
				@system				BIT,
				@concept_rank_key	CHAR(16)

	/* determine parameters of job */
	SELECT		@taxon_list_key							=	m.Taxon_List_Key,
				@domain_key								=	ld.Domain_Key
	FROM		Import_Export_Job						AS	j
	INNER JOIN	Taxon_Dictionary_Concept_Group_Mapping	AS	m
	ON			m.Concept_Group_Key						=	j.Concept_Group_Key
	INNER JOIN	Concept_Group							AS	g
	ON			g.Concept_Group_Key						=	m.Concept_Group_Key
	INNER JOIN	Local_Domain							AS	ld
	ON			ld.Local_Domain_Key						=	g.Local_Domain_Key
	WHERE		j.Import_Export_Job_ID					=	@job_id

	IF @@ROWCOUNT = 0
	BEGIN
		RAISERROR ('Job does not exist or has not been configured', 16, 1)
		RETURN
	END

	EXECUTE		usp_Import_Export_Job_UpdateStatus	@job_id,
													'Importing concept ranks'
	IF @@ERROR <> 0 RETURN

	DECLARE		ranks	CURSOR LOCAL FAST_FORWARD FOR
	SELECT DISTINCT
				tr.TAXON_RANK_KEY,
				ISNULL(tr.LONG_NAME, tr.SHORT_NAME),
				tr.SEQUENCE,
				tr.SHORT_NAME,
				tr.ENTERED_BY,
				tr.ENTRY_DATE,
				tr.CHANGED_BY,
				tr.CHANGED_DATE,
				tr.SYSTEM_SUPPLIED_DATA
	FROM		TAXON_LIST_VERSION			AS	tlv
	INNER JOIN	TAXON_LIST_ITEM				AS	tli
	ON			tli.TAXON_LIST_VERSION_KEY	=	tlv.TAXON_LIST_VERSION_KEY
	INNER JOIN	TAXON_RANK					AS	tr
	ON			tr.TAXON_RANK_KEY			=	tli.TAXON_RANK_KEY
	WHERE		tlv.TAXON_LIST_KEY			=	@taxon_list_key

	OPEN		ranks

	WHILE 1 = 1
	BEGIN
		FETCH		ranks
		INTO        @taxon_rank_key,
					@item_name,
					@sort_order,
					@abbreviation,		/* TODO: may clip! */
					@ins_user_key,
					@ins_date,
					@upd_user_key,
					@upd_date,
					@system

		IF @@FETCH_STATUS <> 0 BREAK

		BEGIN TRANSACTION

		/* obtain session identifiers */
		EXECUTE		usp_Session_ForDate		@ins_user_key,
											@ins_date,
											@ins_session_id		OUTPUT
		IF @@ERROR <> 0 GOTO fail_from_cursor

		IF @upd_user_key IS NULL
		BEGIN
			SET			@upd_session_id		=	NULL
		END
		ELSE
		BEGIN
			EXECUTE		usp_Session_ForDate		@upd_user_key,
												@upd_date,
												@upd_session_id		OUTPUT
			IF @@ERROR <> 0 GOTO fail_from_cursor
		END

		SELECT		@concept_rank_key						=	Concept_Rank_Key
		FROM		Taxon_Dictionary_Concept_Rank_Mapping
		WHERE		Taxon_Rank_Key							=	@taxon_rank_key

		IF @@ROWCOUNT > 0
		BEGIN
			/* update concept rank */
			UPDATE		Concept_Rank
			SET			Domain_Key				=	@domain_key,
						Item_Name				=	@item_name,
						Sort_Order				=	@sort_order,
						Abbreviation			=	@abbreviation,
						Entered_Session_ID		=	@ins_session_id,
						Changed_Session_ID		=	@upd_session_id,
						System_Supplied_Data	=	@system
			WHERE		Concept_Rank_Key		=	@concept_rank_key

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END
		ELSE
		BEGIN
			/* create concept rank */
			EXECUTE		spNextKey	'Concept_Rank',
									@concept_rank_key		OUTPUT
			IF @@ERROR <> 0 GOTO fail_from_cursor

			INSERT		Concept_Rank (
						Concept_Rank_Key,
						Domain_Key,
						Item_Name,
						Sort_Order,
						Abbreviation,
						Color_R,
						Color_G,
						Color_B,
						Entered_Session_ID,
						Changed_Session_ID,
						System_Supplied_Data)
			VALUES		(@concept_rank_key,
						@domain_key,
						@item_name,
						@sort_order,
						@abbreviation,
						0,
						0,
						0,
						@ins_session_id,
						@upd_session_id,
						@system)

			IF @@ERROR <> 0 GOTO fail_from_cursor

			/* record mapping */
			INSERT		Taxon_Dictionary_Concept_Rank_Mapping (
						Taxon_Rank_Key,
						Concept_Rank_Key)
			VALUES		(@taxon_rank_key,
						@concept_rank_key)

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END

		EXECUTE		usp_Import_Export_Job_RecordProcessed	@job_id
		IF @@ERROR <> 0 GOTO fail_from_cursor

		COMMIT TRANSACTION
	END

	CLOSE		ranks
	DEALLOCATE	ranks
	RETURN

fail_from_cursor:
	CLOSE		ranks
	DEALLOCATE	ranks

fail:
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_ConceptRank_ImportTaxonList failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptRank_ImportTaxonList') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptRank_ImportTaxonList'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptRank_ImportTaxonList TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptRank_ImportTaxonList TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptRank_ImportTaxonList TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptRelation_ImportTaxonList]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_ConceptRelation_ImportTaxonList]
GO

/*===========================================================================*\
  Description:	Import relationships between concepts from the specified
				taxon list.

  Parameters:	@job_id					Job identifier

  Created:		Dec 2003

  Last revision information:
	$Revision: 7 $
	$Date: 6/02/09 10:41 $
	$Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptRelation_ImportTaxonList]
	@job_id				INT
AS
	SET NOCOUNT ON

	DECLARE     @concept_group_key				CHAR(16),
				@taxon_list_key					CHAR(16),
				@thesaurus_relation_type_key	CHAR(16),
				@parent_concept_key				CHAR(16),
				@child_concept_key				CHAR(16),
				@changed_session_id				CHAR(16),
				@system							BIT,
				@changed						BIT,
				@concept_relation_key			CHAR(16)

	/* determine parameters of job */
	SELECT      @concept_group_key						=	m.Concept_Group_Key,
				@taxon_list_key							=	m.Taxon_List_Key,
				@thesaurus_relation_type_key			=	g.Hierarchy_Relation_Type_Key
	FROM		Import_Export_Job						AS	j
	INNER JOIN	Taxon_Dictionary_Concept_Group_Mapping	AS	m
	ON			m.Concept_Group_Key						=	j.Concept_Group_Key
	INNER JOIN	Concept_Group							AS	g
	ON			g.Concept_Group_Key						=	m.Concept_Group_Key
	WHERE		j.Import_Export_Job_ID					=	@job_id

	IF @@ROWCOUNT = 0
	BEGIN
		RAISERROR ('Job does not exist or has not been configured', 16, 1)
		RETURN
	END

	EXECUTE		usp_Import_Export_Job_UpdateStatus	@job_id,
													'Importing concept relationships'
	IF @@ERROR <> 0 RETURN

	DECLARE		concepts	CURSOR LOCAL FAST_FORWARD FOR
	SELECT		p.Concept_Key,
				c.Concept_Key,
				ISNULL(c.Changed_Session_ID,
					   c.Entered_Session_ID),
				c.System_Supplied_Data
	FROM		TAXON_LIST_VERSION					AS	tlv
	INNER JOIN	TAXON_LIST_ITEM						AS	tli
	ON			tli.TAXON_LIST_VERSION_KEY			=	tlv.TAXON_LIST_VERSION_KEY
	INNER JOIN	Taxon_Dictionary_Concept_Mapping	AS	cm
	ON			cm.Taxon_List_Item_Key				=	tli.TAXON_LIST_ITEM_KEY
	INNER JOIN	Concept								AS	c
	ON			c.Concept_Key						=	cm.Concept_Key
	LEFT JOIN	Taxon_Dictionary_Concept_Mapping	AS	pm
	ON			pm.Taxon_List_Item_Key				=	tli.PARENT
	LEFT JOIN	Concept								AS	p
	ON			p.Concept_Key						=	pm.Concept_Key
	WHERE		tlv.TAXON_LIST_KEY					=	@taxon_list_key

	OPEN		concepts

	WHILE 1 = 1
	BEGIN
		FETCH		concepts
		INTO		@parent_concept_key,
					@child_concept_key,
					@changed_session_id,
					@system

		IF @@FETCH_STATUS <> 0 BREAK

		BEGIN TRANSACTION

		SELECT      @changed					=	CASE WHEN System_Supplied_Data = @system
														THEN 0
														ELSE 1
													END
		FROM		Concept_Relation
		WHERE		From_Concept_Key			=	@parent_concept_key
		AND			To_Concept_Key				=	@child_concept_key
		AND			Thesaurus_Relation_Type_Key	=   @thesaurus_relation_type_key

		IF @@ROWCOUNT > 0
		BEGIN
			IF @changed = 1
			BEGIN
				/* update existing relationship */
				UPDATE		Concept_Relation
				SET			Changed_Session_ID			=	@changed_session_id,
							System_Supplied_Data		=	@system
				WHERE		To_Concept_Key				=	@child_concept_key
				AND			Thesaurus_Relation_Type_Key	=	@thesaurus_relation_type_key

				IF @@ERROR <> 0 GOTO fail_from_cursor
			END
		END
		ELSE
		BEGIN
			/* remove any old relationships */
			DELETE		Concept_Relation
			WHERE		To_Concept_Key				=	@child_concept_key
			AND			Thesaurus_Relation_Type_Key	=	@thesaurus_relation_type_key

			IF @@ERROR <> 0 GOTO fail_from_cursor

			IF @parent_concept_key IS NOT NULL
			BEGIN
				/* create new relationship */
				EXECUTE     spNextKey	'Concept_Relation',
										@concept_relation_key	OUTPUT
				IF @@ERROR <> 0 GOTO fail_from_cursor

				INSERT		Concept_Relation (
							Concept_Relation_Key,
							From_Concept_Key,
							To_Concept_Key,
							Thesaurus_Relation_Type_Key,
							Entered_Session_ID,
							System_Supplied_Data)
				VALUES		(@concept_relation_key,
							@parent_concept_key,
							@child_concept_key,
							@thesaurus_relation_type_key,
							@changed_session_id,
							@system)

				IF @@ERROR <> 0 GOTO fail_from_cursor
			END
		END

		EXECUTE		usp_Import_Export_Job_RecordProcessed	@job_id
		IF @@ERROR <> 0 GOTO fail_from_cursor

		COMMIT TRANSACTION
	END

	CLOSE		concepts
	RETURN

fail_from_relations:
	CLOSE		relations
	
fail_from_cursor:
	CLOSE		concepts

fail:
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_ConceptRelation_ImportTaxonList failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptRelation_ImportTaxonList') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptRelation_ImportTaxonList'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptRelation_ImportTaxonList TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptRelation_ImportTaxonList TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptRelation_ImportTaxonList TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptToConceptRelations_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptToConceptRelations_Select]
GO

/*===========================================================================*\
  Description:	Returns the list of relationships of any type that exist 
			between a concept and a list of other concepts.  Both directions are scanned.

  Parameters:	@FromKey 		Concept_Key
							@ToKeys			Concept_Key\Concept_Key etc
							@DoAncestors - if 0, then inherited relationships not found.  If 1, 
							then only inherited relationships returned

  Created:	Dec 2003

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptToConceptRelations_Select]
	@FromKey CHAR(16),
	@ToKeys VARCHAR(1600),  -- max 100 links scanned
	@IncludeInherited BIT

AS

SET NOCOUNT ON

DECLARE @CharPos INTEGER

DECLARE @FromConceptKeys TABLE (
  Concept_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS PRIMARY KEY,
	Ancestor BIT,   -- is this a concept in the lineage above the selected concept?
	Concept_Group_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS
)

DECLARE @ToConceptKeys TABLE (
  Concept_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS,
	Leaf_Concept_Key CHAR(16),
	Concept_Group_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS,
	Ancestor BIT,   -- is this a concept in the lineage above the selected concept?
	PRIMARY KEY (Concept_Key, Leaf_Concept_Key)
)

/*===========================================================================*\	
  Create temp tables to store the concept keys we are scanning from and to.
	This includes those in the hierarchy, because relationships can be 
	inherited
\*===========================================================================*/

--Find the current concept groups to aid when filtering lineage
DECLARE @FromConceptGroupKey char(16)
SELECT @FromConceptGroupKey=Concept_Group_Key FROM Concept WHERE Concept_Key=@FromKey

--Store the from concept key, we'll add the ancestors later
INSERT INTO @FromConceptKeys (Concept_Key, Ancestor, Concept_Group_Key) 
VALUES (@FromKey, 0, @FromConceptGroupKey)

-- and retrieve the To keys by parsing the \ separated list
SET @CharPos=1

WHILE @CharPos<LEN(@ToKeys)
BEGIN
  IF SUBSTRING(@ToKeys, @CharPos, 1)='\'
		INSERT INTO @ToConceptKeys (Concept_Key, Leaf_Concept_Key, Concept_Group_Key, Ancestor)
		  SELECT C.Concept_Key, C.Concept_Key, C.Concept_Group_Key, 0
			FROM Concept C
			LEFT JOIN @ToConceptKeys T ON T.Concept_Key=C.Concept_Key
			WHERE C.Concept_Key=SUBSTRING(@ToKeys, @CharPos-16, 16)
			AND T.Concept_Key IS NULL
  SET @CharPos=@CharPos+1
END

-- read the last item which has no \ after it
INSERT INTO @ToConceptKeys (Concept_Key, Leaf_Concept_Key, Concept_Group_Key, Ancestor)
  SELECT C.Concept_Key, C.Concept_Key, C.Concept_Group_Key, 0
	FROM Concept C
	LEFT JOIN @ToConceptKeys T ON T.Concept_Key=C.Concept_Key
	WHERE C.Concept_Key=RIGHT(@ToKeys, 16)
			AND T.Concept_Key IS NULL

/*===========================================================================*\	
	Retrieve the list of lineage concept keys that we need to look at for 
	inherited relationships, for both the From and the To ends.
	Note that Concept_Group_Key is included and any concept group is matched, 
	then the data is filtered afer.  This seems much faster than filtering
	out the concept group key at the start.
\*===========================================================================*/
IF @IncludeInherited=1 
BEGIN
  INSERT INTO @FromConceptKeys (Concept_Key, Ancestor, Concept_Group_Key) 
		SELECT DISTINCT CL2.Concept_Key, 1, C.Concept_Group_Key
		FROM @FromConceptKeys F
		INNER JOIN Concept_Lineage CL1 ON CL1.Concept_Key=F.Concept_Key
		INNER JOIN Concept_Lineage CL2 ON CL2.Lineage = LEFT(CL1.Lineage, LEN(CL2.Lineage))
		INNER JOIN Concept C ON C.Concept_Key=CL2.Concept_Key
		LEFT JOIN @FromConceptKeys F2 ON F2.Concept_Key=CL2.Concept_Key
		WHERE F2.Concept_Key IS NULL 

	INSERT INTO @ToConceptKeys (Concept_Key, Leaf_Concept_Key, Concept_Group_Key, Ancestor)
		SELECT DISTINCT C.Concept_Key, T.Concept_Key, C.Concept_Group_Key, 1
		FROM @ToConceptKeys T
		INNER JOIN Concept_Lineage CL1 ON CL1.Concept_Key=T.Concept_Key
		INNER JOIN Concept_Lineage CL2 ON CL2.Lineage = LEFT(CL1.Lineage, LEN(CL2.Lineage))
		INNER JOIN Concept C ON C.Concept_Key=CL2.Concept_Key
		LEFT JOIN @ToConceptKeys T2 ON T2.Concept_Key=CL2.Concept_Key
				AND T2.Leaf_Concept_Key=T.Concept_Key Collate SQL_Latin1_General_CP1_CI_AS
		WHERE T2.Concept_Key IS NULL 

		DELETE FROM @FromConceptKeys WHERE Concept_Group_Key<>@FromConceptGroupKey
		DELETE FROM @ToConceptKeys WHERE Concept_Group_Key<>@FromConceptGroupKey

END


SELECT DISTINCT
		'Concept' AS Type, 
		'Forward' AS Direction,
		T.Leaf_Concept_Key AS To_Concept_Key,
		TRT.Thesaurus_Relation_Type_Key, 
		TRT.Item_Name, 
		TRT.Forward_Term, 
		TRT.Reverse_Term
FROM Concept_Relation CR
INNER JOIN Thesaurus_Relation_Type TRT ON TRT.Thesaurus_Relation_Type_Key=CR.Thesaurus_Relation_Type_Key
INNER JOIN @FromConceptKeys F ON From_Concept_Key = F.Concept_Key
INNER JOIN @ToConceptKeys T ON To_Concept_Key=T.Concept_Key
WHERE @IncludeInherited=0 OR (CR.Inherited=1 AND (F.Ancestor=1 OR T.Ancestor=1))
UNION
SELECT 
		'Concept' AS Type, 
		'Reverse' AS Direction,
		T.Leaf_Concept_Key AS To_Concept_Key,
		TRT.Thesaurus_Relation_Type_Key, 
		TRT.Item_Name, 
		TRT.Forward_Term, 
		TRT.Reverse_Term
FROM Concept_Relation CR
INNER JOIN Thesaurus_Relation_Type TRT ON TRT.Thesaurus_Relation_Type_Key=CR.Thesaurus_Relation_Type_Key
INNER JOIN @FromConceptKeys F ON To_Concept_Key = F.Concept_Key
INNER JOIN @ToConceptKeys T ON From_Concept_Key=T.Concept_Key
WHERE @IncludeInherited=0 OR (CR.Inherited=1 AND (F.Ancestor=1 OR T.Ancestor=1))
UNION
SELECT 
		'Meaning' AS Type, 
		'Forward' AS Direction,
		T.Leaf_Concept_Key AS To_Concept_Key,
		TRT.Thesaurus_Relation_Type_Key, 
		TRT.Item_Name, 
		TRT.Forward_Term, 
		TRT.Reverse_Term
FROM Meaning_Relation MR
INNER JOIN Thesaurus_Relation_Type TRT ON TRT.Thesaurus_Relation_Type_Key=MR.Thesaurus_Relation_Type_Key
INNER JOIN Concept CFrom ON CFrom.Meaning_Key=MR.From_Meaning_Key
INNER JOIN Concept CTo ON CTo.Meaning_Key=MR.To_Meaning_Key
INNER JOIN @FromConceptKeys F ON CFrom.Concept_Key = F.Concept_Key
INNER JOIN @ToConceptKeys T ON CTo.Concept_Key=T.Concept_Key
WHERE @IncludeInherited=0 OR (MR.Inherited=1 AND (F.Ancestor=1 OR T.Ancestor=1))
UNION
SELECT 
		'Meaning' AS Type, 
		'Reverse' AS Direction,
		T.Leaf_Concept_Key AS To_Concept_Key,
		TRT.Thesaurus_Relation_Type_Key, 
		TRT.Item_Name, 
		TRT.Forward_Term, 
		TRT.Reverse_Term
FROM Meaning_Relation MR
INNER JOIN Thesaurus_Relation_Type TRT ON TRT.Thesaurus_Relation_Type_Key=MR.Thesaurus_Relation_Type_Key
INNER JOIN Concept CFrom ON CFrom.Meaning_Key=MR.From_Meaning_Key
INNER JOIN Concept CTo ON CTo.Meaning_Key=MR.To_Meaning_Key
INNER JOIN @FromConceptKeys F ON CTo.Concept_Key=F.Concept_Key
INNER JOIN @ToConceptKeys T ON CFrom.Concept_Key=T.Concept_Key
WHERE @IncludeInherited=0 OR (MR.Inherited=1 AND (F.Ancestor=1 OR T.Ancestor=1))
UNION
SELECT 
		'TermVersion' AS Type, 
		'Forward' AS Direction,
		T.Leaf_Concept_Key AS To_Concept_Key,
		TRT.Thesaurus_Relation_Type_Key, 
		TRT.Item_Name, 
		TRT.Forward_Term, 
		TRT.Reverse_Term
FROM Term_Version_Relation TVR
INNER JOIN Thesaurus_Relation_Type TRT ON TRT.Thesaurus_Relation_Type_Key=TVR.Thesaurus_Relation_Type_Key
INNER JOIN Concept CFrom ON CFrom.Term_Version_Key=TVR.From_Term_Version_Key
INNER JOIN Concept CTo ON CTo.Term_Version_Key=TVR.To_Term_Version_Key
INNER JOIN @ToConceptKeys T ON T.Concept_Key=CTo.Concept_Key
WHERE CFrom.Concept_Key=@FromKey
AND @IncludeInherited=0
UNION
SELECT 
		'TermVersion' AS Type, 
		'Reverse' AS Direction,
		T.Leaf_Concept_Key AS To_Concept_Key,
		TRT.Thesaurus_Relation_Type_Key, 
		TRT.Item_Name, 
		TRT.Forward_Term, 
		TRT.Reverse_Term
FROM Term_Version_Relation TVR
INNER JOIN Thesaurus_Relation_Type TRT ON TRT.Thesaurus_Relation_Type_Key=TVR.Thesaurus_Relation_Type_Key
INNER JOIN Concept CFrom ON CFrom.Term_Version_Key=TVR.From_Term_Version_Key
INNER JOIN Concept CTo ON CTo.Term_Version_Key=TVR.To_Term_Version_Key
INNER JOIN @ToConceptKeys T ON T.Concept_Key=CFrom.Concept_Key
WHERE CTo.Concept_Key=@FromKey
AND @IncludeInherited=0

SET NOCOUNT OFF

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptToConceptRelations_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptToConceptRelations_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptToConceptRelations_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptToConceptRelations_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptToConceptRelations_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ConceptToConceptRelations_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptToConceptRelations_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptToConceptRelations_Select TO [Dev - JNCC SQL]
END

GO


 /*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Concept_Delete]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Concept_Delete]
GO

/*===========================================================================*\
  Description:	Delete a record from the Concept tables. Also deletes records
		from other tables where necessary.

  Parameters:	@Key		Concept key.
		@Timestamp

  Created:	December 2003

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Concept_Delete]
	@Key char(16),
	@Timestamp timestamp
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION
	
		DECLARE @MeaningKey char(16),
			@TermKey char(16),
			@TermVersionKey char(16),
			@ConceptsSharingMeaningKeyCount int,
			@ConceptsSharingTermKeyCount int,
			@ConceptsSharingTermVersionKeyCount int,
			@OriginalTimestamp timestamp

		-- Store the Meaning, Term and Term Version keys because the concept record
		-- needs to be deleted before these other records can be, due to referential
		-- integrity.
		SELECT	@MeaningKey = Meaning_Key,
			@TermKey = Term_Key,
			@TermVersionKey = Term_Version_Key,
			@OriginalTimestamp = [Timestamp]
		FROM 	Concept
		WHERE	Concept_Key = @Key

		-- Count the number of concepts that use this meaning key.
		SELECT 		@ConceptsSharingMeaningKeyCount = Count(C2.Concept_Key)
		FROM		Concept AS C1
		INNER JOIN	Concept AS C2 ON C2.Meaning_Key = C1.Meaning_Key
		WHERE		C1.Concept_Key = @Key

		-- Count the number of concepts that use the same term key as the concept we want to delete.
		SELECT 		@ConceptsSharingTermKeyCount = Count(DISTINCT C2.Concept_Key)
		FROM		Concept AS C1
		INNER JOIN	Concept AS C2 ON C2.Term_Key = C1.Term_Key
		WHERE		C1.Concept_Key = @Key

		-- Count the number of concepts that use the same term version key as the concept we want to delete.
		SELECT 		@ConceptsSharingTermVersionKeyCount = Count(DISTINCT C2.Concept_Key)
		FROM		Concept AS C1
		INNER JOIN	Concept AS C2 ON C2.Term_Version_Key = C1.Term_Version_Key
		WHERE		C1.Concept_Key = @Key

		/*====================================*\
		  Delete the records.
		\*====================================*/
		-- Delete the Concept_History record.
		DELETE	Concept_History
		WHERE	Concept_Key = @Key

		IF @@Error <> 0 GOTO RollbackAndExit

		/*-------------------------------------------------------*\
		  Delete the relation records which refer to the concept.
		\*-------------------------------------------------------*/
		DELETE	Concept_Relation
		WHERE	To_Concept_Key = @Key
		OR	From_Concept_Key = @Key

		IF @@Error <> 0 GOTO RollbackAndExit

		DELETE	Meaning_Relation
		WHERE	To_Concept_Key = @Key
		OR	From_Concept_Key = @Key

		IF @@Error <> 0 GOTO RollbackAndExit

		DELETE	Term_Version_Relation
		WHERE	To_Concept_Key = @Key
		OR	From_Concept_Key = @Key

		IF @@Error <> 0 GOTO RollbackAndExit

		/*-------------------------------------------------------*\
		  Delete the Enquiry_Concept records because otherwise
		  the deletion will fail because it says other records
		  link to the Concept. Enquiries cannot be viewed in the
		  Thesaurus Editor it would appear at a casual glance
		  that nothing is actually linked to the concept. 
		  So best to just delete the Enquiry_Concept join records.
		\*-------------------------------------------------------*/
		DELETE	Enquiry_Concept
		WHERE	Concept_Key = @Key

		IF @@Error <> 0 GOTO RollbackAndExit

		-- Delete the Concept_Lineage records.
		IF EXISTS (SELECT 1 FROM Concept WHERE Concept_Key = @Key)
		BEGIN
			EXECUTE		usp_ConceptLineage_DeleteConcept	@Key
			IF @@ERROR <> 0 GOTO RollbackAndExit
		END

		-- Delete the Concept record. Have to check timestamp passed into the proc
		-- against the timestamp the Concept had before any of its related records
		-- were deleted. This is because deleting the records above may cause
		-- triggers to be fired. Deleting the record in Concept_History will fire
		-- a trigger that updates the current Concept, causing its timestamp to 
		-- change.
		DELETE	Concept
		WHERE	Concept_Key = @Key
		AND	(@Timestamp = @OriginalTimestamp)

		IF @@Error <> 0 GOTO RollbackAndExit

		-- Delete the Meaning record if only one Concept uses that Meaning key.
		IF @ConceptsSharingMeaningKeyCount = 1 
			DELETE 	Meaning
			WHERE	Meaning_Key = @MeaningKey

		IF @@Error <> 0 GOTO RollbackAndExit

		-- Delete the Term Version record if only one Concept uses that Term Version key.
		IF @ConceptsSharingTermVersionKeyCount = 1
			DELETE	Term_Version
			WHERE	Term_Version_Key = @TermVersionKey

		IF @@Error <> 0 GOTO RollbackAndExit

		-- Delete the Term record if only one Concept uses that Term key.
		IF @ConceptsSharingTermKeyCount = 1
			DELETE	Term
			WHERE	Term_Key = @TermKey

		IF @@Error <> 0 GOTO RollbackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
	RAISERROR ('usp_Concept_Delete failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Concept_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concept_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concept_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Concept_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Concept_Delete TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_Concept_ImportTaxonDesignationTypes]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_Concept_ImportTaxonDesignationTypes]
GO

/*===========================================================================*\
  Description:	Import concepts corresponding to the taxon designation types
				used in the specified taxon list.

  Parameters:	@job_id					Job identifier

  Created:		Dec 2003

  Last revision information:
	$Revision: 7 $
	$Date: 6/02/09 10:41 $
	$Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Concept_ImportTaxonDesignationTypes]
	@job_id					INT
AS
	SET NOCOUNT ON

	DECLARE     @taxon_list_key					CHAR(16),
				@taxon_designation_type_key		CHAR(16),
				@item_name						NVARCHAR(300),
				@ins_user_key					CHAR(16),
				@ins_date						DATETIME,
				@ins_session_id					CHAR(16),
				@upd_user_key					CHAR(16),
				@upd_date						DATETIME,
				@upd_session_id					CHAR(16),
				@system							BIT,
				@concept_designation_type_key	CHAR(16),
				@term_key						CHAR(16),
				@meaning_key					CHAR(16),
				@concept_history_key			CHAR(16)

	/* determine parameters of job */
	SELECT		@taxon_list_key							=	m.Taxon_List_Key
	FROM		Import_Export_Job						AS	j
	INNER JOIN	Taxon_Dictionary_Concept_Group_Mapping	AS	m
	ON			m.Concept_Group_Key						=	j.Concept_Group_Key
	WHERE		j.Import_Export_Job_ID					=	@job_id

	IF @@ROWCOUNT = 0
	BEGIN
		RAISERROR ('Job does not exist or has not been configured', 16, 1)
		RETURN
	END

	EXECUTE		usp_Import_Export_Job_UpdateStatus	@job_id,
													'Importing designation types'
	IF @@ERROR <> 0 RETURN

	DECLARE		types	CURSOR LOCAL FAST_FORWARD FOR
	SELECT DISTINCT
				tdt.TAXON_DESIGNATION_TYPE_KEY,
				ISNULL(tdt.LONG_NAME,
					   tdt.SHORT_NAME),
				tdt.ENTERED_BY,
				tdt.ENTRY_DATE,
				tdt.CHANGED_BY,
				tdt.CHANGED_DATE,
				tdt.SYSTEM_SUPPLIED_DATA
	FROM		TAXON_LIST_VERSION				AS	tlv
	INNER JOIN	TAXON_LIST_ITEM					AS	tli
	ON			tli.TAXON_LIST_VERSION_KEY		=	tlv.TAXON_LIST_VERSION_KEY
	INNER JOIN	TAXON_DESIGNATION				AS	td
	ON			td.TAXON_LIST_ITEM_KEY			=	tli.TAXON_LIST_ITEM_KEY
	INNER JOIN	TAXON_DESIGNATION_TYPE			AS	tdt
	ON			tdt.TAXON_DESIGNATION_TYPE_KEY	=	td.TAXON_DESIGNATION_TYPE_KEY
	WHERE		tlv.TAXON_LIST_KEY				=	@taxon_list_key

	OPEN		types

	WHILE 1 = 1
	BEGIN
		FETCH		types
		INTO		@taxon_designation_type_key,
					@item_name,
					@ins_user_key,
					@ins_date,
					@upd_user_key,
					@upd_date,
					@system

		IF @@FETCH_STATUS <> 0 BREAK

		BEGIN TRANSACTION

		/* obtain session identifiers */
		EXECUTE		usp_Session_ForDate		@ins_user_key,
											@ins_date,
											@ins_session_id		OUTPUT
		IF @@ERROR <> 0 GOTO fail_from_cursor

		IF @upd_user_key IS NULL
		BEGIN
			SET			@upd_session_id		=	NULL
		END
		ELSE
		BEGIN
			EXECUTE		usp_Session_ForDate		@upd_user_key,
												@upd_date,
												@upd_session_id		OUTPUT
			IF @@ERROR <> 0 GOTO fail_from_cursor
		END

		SELECT		@concept_designation_type_key				=	tdm.Concept_Designation_Type_Key,
					@term_key									=	c.Term_Key
		FROM		Taxon_Dictionary_Designation_Type_Mapping	AS	tdm
		INNER JOIN	Concept										AS	c
		ON			c.Concept_Key								=	tdm.Concept_Designation_Type_Key
		WHERE		tdm.Taxon_Designation_Type_Key				=	@taxon_designation_type_key

		IF @@ROWCOUNT > 0
		BEGIN
			/* designation type has previously been imported */
			IF NOT EXISTS (	SELECT		1
							FROM		Term
							WHERE		Term_Key		=	@term_key
							AND			Language_Key	=	'en'
							AND			Item_Name		=	@item_name )
			BEGIN
				/* term has changed */
				IF EXISTS (	SELECT		1
							FROM		Concept
							WHERE		Term_Key			=	@term_key
							AND			Concept_Group_Key	<>	'SYSTEM000000000T' )
				BEGIN
					/* term is linked outside this concept group; create
					 * a new term instead of updating the existing one */
					EXECUTE		spNextKey	'Term',
											@term_key	OUTPUT
					IF @@ERROR <> 0 GOTO fail_from_cursor

					INSERT		Term (
								Term_Key,
								Language_Key,
								Item_Name,
								Plaintext,
								Entered_Session_ID,
								Changed_Session_ID,
								System_Supplied_Data)
					VALUES		(@term_key,
								'en',
								@item_name,
								@item_name,
								@ins_session_id,
								@upd_session_id,
								@system)

					IF @@ERROR <> 0 GOTO fail_from_cursor
				END
				ELSE
				BEGIN
					/* term only linked within this concept group */
					DECLARE		@cur_term_key		CHAR(16)

					SELECT		@cur_term_key	=	Term_Key
					FROM		Term
					WHERE		Language_Key	=	'en'
					AND			Item_Name		=	@item_name

					IF @@ROWCOUNT = 0
					BEGIN
						/* term can simply be updated */
						UPDATE		Term
						SET			Language_Key	=	'en',
									Item_Name		=	@item_name
						WHERE		Term_Key		=	@term_key

						IF @@ERROR <> 0 GOTO fail_from_cursor
					END
					ELSE
					BEGIN
						/* term cannot be updated; there is an existing
						 * term with the same name which we will link to
						 * instead */
						DELETE		Term
						WHERE		Term_Key			=	@term_key

						IF @@ERROR <> 0 GOTO fail_from_cursor

						SET			@term_key			=	@cur_term_key
					END
				END
			END

			UPDATE		Concept
			SET			Term_Key				=	@term_key,
						Entered_Session_ID		=	@ins_session_id,
						Changed_Session_ID		=	@upd_session_id,
						System_Supplied_Data	=	@system
			WHERE		Concept_Key				=	@concept_designation_type_key

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END
		ELSE
		BEGIN
			/* find/create term */
			SELECT		@term_key		=	Term_Key
			FROM		Term
			WHERE		Language_Key	=	'en'
			AND			Item_Name		=	@item_name

			IF @@ROWCOUNT = 0
			BEGIN
				EXECUTE		spNextKey	'Term',
										@term_key	OUTPUT
				IF @@ERROR <> 0 GOTO fail_from_cursor

				INSERT		Term (
							Term_Key,
							Language_Key,
							Item_Name,
							Plaintext,
							Entered_Session_ID,
							Changed_Session_ID,
							System_Supplied_Data)
				VALUES		(@term_key,
							'en',
							@item_name,
							@item_name,
							@ins_session_id,
							@upd_session_id,
							@system)

				IF @@ERROR <> 0 GOTO fail_from_cursor
			END

			/* create Meaning */
			EXECUTE		spNextKey	'Meaning',
									@meaning_key	OUTPUT
			IF @@ERROR <> 0 GOTO fail_from_cursor

			INSERT		Meaning (
						Meaning_Key)
			VALUES		(@meaning_key)

			IF @@ERROR <> 0 GOTO fail_from_cursor

			/* create Concept */
			EXECUTE		spNextKey	'Concept',
									@concept_designation_type_key	OUTPUT
			IF @@ERROR <> 0 GOTO fail_from_cursor

			INSERT		Concept (
						Concept_Key,
						Term_Key,
						Concept_Group_Key,
						List_Preferred,
						Is_Current,
						Preferred,
						Name_Type_Concept_Key,
						Meaning_Key,
						Entered_Session_ID,
						Changed_Session_ID,
						System_Supplied_Data)
			VALUES 		(@concept_designation_type_key,
						@term_key,
						'SYSTEM000000000T', /* "Concept Designation Types" group */
						1,
						1,
						1,
						'SYSTEM0000000000', /* "Formal" -- meaningless, but
												we need a value here */
						@meaning_key,
						@ins_session_id,
						@upd_session_id,
						@system)

			IF @@ERROR <> 0 GOTO fail_from_cursor

			/* create concept history */
			EXECUTE		spNextKey	'Concept_History',
									@concept_history_key	OUTPUT
			IF @@ERROR <> 0 GOTO fail_from_cursor

			INSERT		Concept_History (
						Concept_History_Key,
						Concept_Key,
						Concept_Group_Version_From,
						Entered_Session_ID,
						Changed_Session_ID,
						System_Supplied_Data)
			VALUES		(@concept_history_key,
						@concept_designation_type_key,
						'SYSTEM000000000T', /* "Concept Designation Types" version */
						@ins_session_id,
						@upd_session_id,
						@system)

			IF @@ERROR <> 0 GOTO fail_from_cursor

			/* record taxon designation type mapping */
			INSERT		Taxon_Dictionary_Designation_Type_Mapping (
						Taxon_Designation_Type_Key,
						Concept_Designation_Type_Key)
			VALUES		(@taxon_designation_type_key,
						@concept_designation_type_key)

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END

		EXECUTE		usp_Import_Export_Job_RecordProcessed	@job_id
		IF @@ERROR <> 0 GOTO fail_from_cursor

		COMMIT TRANSACTION
	END

	CLOSE		types
	DEALLOCATE	types
	RETURN

fail_from_cursor:
	CLOSE		types
	DEALLOCATE	types

fail:
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_Concept_ImportTaxonDesignationTypes failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_ImportTaxonDesignationTypes') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Concept_ImportTaxonDesignationTypes'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concept_ImportTaxonDesignationTypes TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concept_ImportTaxonDesignationTypes TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Concept_ImportTaxonDesignationTypes TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_Concept_ImportTaxonList]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_Concept_ImportTaxonList]
GO

/*===========================================================================*\
  Description:	Import concepts corresponding to the contents of a taxon list.

  Parameters:	@job_id					Job identifier

  Created:		Nov 2003

  Last revision information:
	$Revision: 7 $
	$Date: 6/02/09 10:41 $
	$Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Concept_ImportTaxonList]
	@job_id				INT
AS
	SET NOCOUNT ON

	DECLARE     @taxon_list_key				CHAR(16),
				@concept_group_key			CHAR(16),
				@taxon_list_item_key		CHAR(16),
				@taxon_version_key			CHAR(16),
				@term_key					CHAR(16),
				@term_version_key			CHAR(16),
				@list_preferred				BIT,
				@is_current					BIT,
				@is_preferred				BIT,
				@taxon_rank_key				CHAR(16),
				@rank_uses_italics			BIT,
				@concept_rank_key			CHAR(16),
				@name_type_concept_key		CHAR(16),
				@sort_code					INT,
				@ins_user_key				CHAR(16),
				@ins_date					SMALLDATETIME,
				@ins_session_id				CHAR(16),
				@upd_user_key				CHAR(16),
				@upd_date					SMALLDATETIME,
				@upd_session_id				CHAR(16),
				@system						BIT,
				@preferred_name				CHAR(16),
				@taxon_list_version_from	CHAR(16),
				@taxon_list_version_to		CHAR(16),	
				@concept_group_version_from	CHAR(16),
				@concept_group_version_to	CHAR(16),
				@meaning_key				CHAR(16),
				@concept_key				CHAR(16)

	/* determine parameters of job */
	SELECT		@taxon_list_key							=	m.Taxon_List_Key,
				@concept_group_key						=	m.Concept_Group_Key
	FROM		Import_Export_Job						AS	j
	INNER JOIN	Taxon_Dictionary_Concept_Group_Mapping	AS	m
	ON			m.Concept_Group_Key						=	j.Concept_Group_Key
	WHERE		j.Import_Export_Job_ID					=	@job_id

	IF @@ROWCOUNT = 0
	BEGIN
		RAISERROR ('Job does not exist or has not been configured', 16, 1)
		RETURN
	END

	EXECUTE		usp_Import_Export_Job_UpdateStatus	@job_id,
													'Importing concepts'
	IF @@ERROR <> 0 RETURN

	/* remove current lineage data */
	DELETE		l
	FROM		Concept					AS	c
	INNER JOIN	Concept_Lineage			AS	l
	ON			l.Concept_Key			=	c.Concept_Key
	WHERE		c.Concept_Group_Key		=	@concept_group_key

	IF @@ERROR <> 0 RETURN

	DECLARE		@items	TABLE (
				Taxon_List_Item_Key	CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS,
				Rank_Uses_Italic	BIT)

	INSERT		@items
	SELECT      tli.TAXON_LIST_ITEM_KEY,
				tr.LIST_FONT_ITALIC
	FROM        TAXON_LIST_VERSION				AS	tlv
	INNER JOIN	TAXON_LIST_ITEM					AS	tli
	ON			tli.TAXON_LIST_VERSION_KEY		=	tlv.TAXON_LIST_VERSION_KEY
	INNER JOIN	TAXON_RANK						AS	tr
	ON			tr.TAXON_RANK_KEY				=	tli.TAXON_RANK_KEY
	WHERE		tlv.TAXON_LIST_KEY				=	@taxon_list_key

	DECLARE		items		CURSOR FAST_FORWARD LOCAL FOR
	SELECT		tli.TAXON_LIST_ITEM_KEY,
				tli.TAXON_VERSION_KEY,
				CASE WHEN tli.TAXON_LIST_ITEM_KEY = tli.PREFERRED_NAME
					THEN 1	/* list preferred */
					ELSE 0
				END,
				CASE WHEN tli.TAXON_LIST_VERSION_TO IS NULL
					THEN 1	/* current */
					ELSE 0
				END,
				tli.TAXON_RANK_KEY,
				itm.Rank_Uses_Italic,
				tli.SORT_CODE,
				tli.ENTERED_BY,
				tli.ENTRY_DATE,
				tli.CHANGED_BY,
				tli.CHANGED_DATE,
				tli.SYSTEM_SUPPLIED_DATA,
				tli.PREFERRED_NAME,
				tli.TAXON_LIST_VERSION_KEY,
				tli.TAXON_LIST_VERSION_TO
	FROM		@items							AS	itm
	INNER JOIN	TAXON_LIST_ITEM					AS	tli
	ON			tli.TAXON_LIST_ITEM_KEY			=	itm.TAXON_LIST_ITEM_KEY

	OPEN        items

	WHILE 1 = 1
	BEGIN
		FETCH		items
		INTO		@taxon_list_item_key,
					@taxon_version_key,
					@list_preferred,
					@is_current,
					@taxon_rank_key,
					@rank_uses_italics,
					@sort_code,
					@ins_user_key,
					@ins_date,
					@upd_user_key,
					@upd_date,
					@system,
					@preferred_name,
					@taxon_list_version_from,
					@taxon_list_version_to

		IF @@FETCH_STATUS <> 0 BREAK

		BEGIN TRANSACTION

		/* perform mappings */
		SELECT		@term_version_key						=	Term_Version_Key
		FROM		Taxon_Dictionary_Term_Version_Mapping
		WHERE		Taxon_Version_Key						=	@taxon_version_key

		IF @@ROWCOUNT = 0 GOTO skip_item

		SELECT		@term_key								=	tm.Term_Key,
					@name_type_concept_key					=	ntm.Thesaurus_Name_Type_Key
		FROM		TAXON_VERSION							AS	tv
		INNER JOIN	TAXON									AS	tx
		ON			tx.TAXON_KEY							=	tv.TAXON_KEY
		INNER JOIN	Taxon_Dictionary_Term_Mapping			AS	tm
		ON			tm.Taxon_Key							=	tx.TAXON_KEY
		AND			tm.Italic_Font							=	CASE WHEN tx.Language = 'La'
																	 AND @rank_uses_italics = 1
																	THEN 1
																	ELSE 0
																END
		INNER JOIN	Taxon_Dictionary_Name_Type_Mapping		AS	ntm
		ON			ntm.Taxon_Name_Type_Key					=	tx.TAXON_NAME_TYPE_KEY
		WHERE		tv.TAXON_VERSION_KEY					=	@taxon_version_key

		IF @@ROWCOUNT = 0 GOTO skip_item

		SELECT		@concept_rank_key						=	Concept_Rank_Key
		FROM		Taxon_Dictionary_Concept_Rank_Mapping
		WHERE		Taxon_Rank_Key							=	@taxon_rank_key

		IF @@ROWCOUNT = 0 GOTO skip_item

		IF @list_preferred = 1
			SET			@is_preferred		=	1
		ELSE
		BEGIN
			SELECT		@is_preferred 		=	CASE WHEN TAXON_VERSION_KEY = @taxon_version_key
													THEN 1
													ELSE 0
												END
			FROM		TAXON_COMMON_NAME
			WHERE		TAXON_LIST_ITEM_KEY	=	@taxon_list_item_key
		END

		SELECT      @concept_group_version_from						=	Concept_Group_Version_Key
		FROM		Taxon_Dictionary_Concept_Group_Version_Mapping
		WHERE		Taxon_List_Version_Key							=   @taxon_list_version_from

		IF @@ROWCOUNT = 0 GOTO skip_item

		SELECT		@concept_group_version_to						=	Concept_Group_Version_Key
		FROM		Taxon_Dictionary_Concept_Group_Version_Mapping
		WHERE		Taxon_List_Version_Key							=	@taxon_list_version_to

		IF @@ROWCOUNT = 0
		BEGIN
			SET			@concept_group_version_to	=	NULL
		END

		/* obtain meaning key */
		SELECT		@meaning_key						=	Meaning_Key
		FROM        Taxon_Dictionary_Meaning_Mapping
		WHERE		Preferred_Name						=	@preferred_name

		IF @@ROWCOUNT = 0
		BEGIN
			/* look for meaning assigned to synonyms of @preferred_name from
			 * some other taxon list */
			SELECT		@meaning_key						=	tdm.Meaning_Key
			FROM		INDEX_TAXON_SYNONYM					AS	its
			INNER JOIN	Taxon_Dictionary_Meaning_Mapping	AS	tdm
			ON			tdm.Preferred_Name					=	its.SYNONYM_LIST_ITEM_KEY
			WHERE		its.TAXON_LIST_ITEM_KEY				=	@preferred_name

			IF @@ROWCOUNT = 0
			BEGIN
				/* create new meaning */
				EXECUTE		spNextKey	'Meaning',
										@meaning_key	OUTPUT
				IF @@ERROR <> 0 GOTO fail_from_cursor

				INSERT		Meaning (
							Meaning_Key)
				VALUES		(@meaning_key)

				IF @@ERROR <> 0 GOTO fail_from_cursor
			END

			INSERT		Taxon_Dictionary_Meaning_Mapping (
						Preferred_Name,
						Meaning_Key)
			VALUES		(@preferred_name,
						@meaning_key)

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END

		IF @meaning_key IS NOT NULL
				/* meaning not explicitly mapped to null,
				 * so we can import item */
		BEGIN
			/* obtain session identifiers */
			EXECUTE		usp_Session_ForDate		@ins_user_key,
												@ins_date,
												@ins_session_id		OUTPUT
			IF @@ERROR <> 0 GOTO fail_from_cursor

			IF @upd_user_key IS NULL
			BEGIN
				SET			@upd_session_id		=	NULL
			END
			ELSE
			BEGIN
				EXECUTE		usp_Session_ForDate		@upd_user_key,
													@upd_date,
													@upd_session_id		OUTPUT
				IF @@ERROR <> 0 GOTO fail_from_cursor
			END

			SELECT      @concept_key						=	Concept_Key
			FROM		Taxon_Dictionary_Concept_Mapping
			WHERE		Taxon_List_Item_Key					=	@taxon_list_item_key

			IF @@ROWCOUNT > 0
			BEGIN
				DECLARE		@old_group_key			CHAR(16),
							@was_list_preferred		BIT
							
				/* update concept */
				UPDATE		Concept
				SET         @old_group_key				=	Concept_Group_Key,
							@was_list_preferred			=	List_Preferred,
							Term_Key					=	@term_key,
							Concept_Group_Key			=	@concept_group_key,
							Term_Version_Key			=	@term_version_key,
							List_Preferred				=	@list_preferred,
							Is_Current					=	@is_current,
							Preferred					=	@is_preferred,
							Concept_Rank_Key			=	@concept_rank_key,
							Name_Type_Concept_Key		=   @name_type_concept_key,
							Meaning_Key					=	@meaning_key,
							Sort_Code					=	@sort_code,
							Entered_Session_ID			=	@ins_session_id,
							Changed_Session_ID			=	@upd_session_id,
							System_Supplied_Data		=	@system
				WHERE		Concept_Key					=	@concept_key

				IF @@ERROR <> 0 GOTO fail_from_cursor

				/* re-create concept history */
				DELETE		Concept_History
				WHERE		Concept_Key					=	@concept_key

				IF @@ERROR <> 0 GOTO fail_from_cursor

				EXECUTE		usp_ConceptHistory_Insert_Imported	@concept_key,
																@concept_group_version_from,
																@concept_group_version_to
				IF @@ERROR <> 0 GOTO fail_from_cursor
			END
			ELSE
			BEGIN
				/* create concept */
				EXECUTE		spNextKey	'Concept',
										@concept_key	OUTPUT
				IF @@ERROR <> 0 GOTO fail_from_cursor

				INSERT		Concept
							(Concept_Key,
							Term_Key,
							Concept_Group_Key,
							Term_Version_Key,
							List_Preferred,
							Is_Current,
							Preferred,
							Concept_Rank_Key,
							Name_Type_Concept_Key,
							Meaning_Key,
							Sort_Code,
							Entered_Session_ID,
							Changed_Session_ID,
							System_Supplied_Data)
				VALUES		(@concept_key,
							@term_key,
							@concept_group_key,
							@term_version_key,
							@list_preferred,
							@is_current,
							@is_preferred,
							@concept_rank_key,
							@name_type_concept_key,
							@meaning_key,
							@sort_code,
							@ins_session_id,
							@upd_session_id,
							@system)

				IF @@ERROR <> 0 GOTO fail_from_cursor

				/* create concept history */
				EXECUTE		usp_ConceptHistory_Insert_Imported	@concept_key,
																@concept_group_version_from,
																@concept_group_version_to
				IF @@ERROR <> 0 GOTO fail_from_cursor

				/* record mapping */
				INSERT		Taxon_Dictionary_Concept_Mapping
							(Taxon_List_Item_Key,
							Concept_Key)
				VALUES		(@taxon_list_item_key,
							@concept_key)

				IF @@ERROR <> 0 GOTO fail_from_cursor
			END
		END
		
skip_item:
		EXECUTE		usp_Import_Export_Job_RecordProcessed	@job_id
		IF @@ERROR <> 0 GOTO fail_from_cursor

		COMMIT TRANSACTION
	END

	CLOSE		items
	DEALLOCATE	items
	RETURN

fail_from_cursor:
	CLOSE		items
	DEALLOCATE	items

fail:
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_Concept_ImportTaxonList failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_ImportTaxonList') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Concept_ImportTaxonList'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concept_ImportTaxonList TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concept_ImportTaxonList TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Concept_ImportTaxonList TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_Concept_ImportTaxonNameTypes]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_Concept_ImportTaxonNameTypes]
GO

/*===========================================================================*\
  Description:	Import concepts corresponding to the taxon name types used in
				the specified taxon list.

  Parameters:   @job_id					Job identifier

  Created:		Nov 2003

  Last revision information:
	$Revision: 7 $
	$Date: 6/02/09 10:41 $
	$Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Concept_ImportTaxonNameTypes]
	@job_id					INT
AS
	SET NOCOUNT ON

	DECLARE		@taxon_list_key				CHAR(16),
				@taxon_name_type_key		CHAR(16),
				@item_name					VARCHAR(100),
				@author_and_date			VARCHAR(100),
				@ins_user_key				CHAR(16),
				@ins_date					SMALLDATETIME,
				@ins_session_id				CHAR(16),
				@system						BIT,
				@thesaurus_name_type_key	CHAR(16),
				@system_mapping				BIT,
				@term_key					CHAR(16),
				@term_version_key			CHAR(16),
				@meaning_key				CHAR(16),
				@concept_history_key		CHAR(16)

	/* determine parameters of job */
	SELECT		@taxon_list_key							=	m.Taxon_List_Key
	FROM		Import_Export_Job						AS	j
	INNER JOIN	Taxon_Dictionary_Concept_Group_Mapping	AS	m
	ON			m.Concept_Group_Key						=	j.Concept_Group_Key
	WHERE		j.Import_Export_Job_ID					=	@job_id

	IF @@ROWCOUNT = 0
	BEGIN
		RAISERROR ('Job does not exist or has not been configured', 16, 1)
		RETURN
	END

	EXECUTE		usp_Import_Export_Job_UpdateStatus	@job_id,
													'Importing name types'
	IF @@ERROR <> 0 RETURN

	DECLARE     @versions   TABLE ( Taxon_Version_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS)

	INSERT      @versions
	SELECT      tli.TAXON_VERSION_KEY
	FROM		TAXON_LIST_VERSION				AS	tlv
	INNER JOIN	TAXON_LIST_ITEM					AS	tli
	ON			tli.TAXON_LIST_VERSION_KEY		=	tlv.TAXON_LIST_VERSION_KEY
	WHERE		tlv.TAXON_LIST_KEY				=	@taxon_list_key

	DECLARE		name_types	CURSOR LOCAL FAST_FORWARD FOR
	SELECT DISTINCT
				tnt.TAXON_NAME_TYPE_KEY,
				tnt.SHORT_NAME,
				tnt.AUTHORITY,
				tnt.ENTERED_BY,
				tnt.ENTRY_DATE,
				tnt.SYSTEM_SUPPLIED_DATA
	FROM        @versions                       AS  v0
	INNER JOIN	TAXON_VERSION					AS	tv
	ON			tv.TAXON_VERSION_KEY			=	v0.TAXON_VERSION_KEY
	INNER JOIN	TAXON							AS	tx
	ON			tx.TAXON_KEY					=	tv.TAXON_KEY
	INNER JOIN	TAXON_NAME_TYPE					AS	tnt
	ON			tnt.TAXON_NAME_TYPE_KEY			=	tx.TAXON_NAME_TYPE_KEY

	OPEN        name_types

	WHILE 1 = 1
	BEGIN
		FETCH		name_types
		INTO		@taxon_name_type_key,
					@item_name,
					@author_and_date,
					@ins_user_key,
					@ins_date,
					@system
					
		IF @@FETCH_STATUS <> 0 BREAK

		BEGIN TRANSACTION

		SELECT		@thesaurus_name_type_key			=   Thesaurus_Name_Type_Key,
					@system_mapping						=	System_Supplied_Data
		FROM		Taxon_Dictionary_Name_Type_Mapping
		WHERE       Taxon_Name_Type_Key					=	@taxon_name_type_key

		IF @@ROWCOUNT > 0
		BEGIN
			/* name type has previously been imported */
			IF @system_mapping = 0
			BEGIN
				SELECT		@term_key			=	Term_Key,
							@term_version_key	=	Term_Version_Key
				FROM		Concept
				WHERE		Concept_Key			=	@thesaurus_name_type_key

				IF NOT EXISTS (	SELECT		1
								FROM		Term
								WHERE		Term_Key		=	@term_key
								AND			Language_Key	=	'en'
								AND			Item_Name		=	@item_name )
				BEGIN
					/* term has changed */
					IF EXISTS (	SELECT		1
								FROM		Concept
								WHERE		Term_Key			=	@term_key
								AND			Concept_Group_Key	<>	'SYSTEM000000000M' )
					BEGIN
						/* term is linked outside this concept group; create
						 * a new term instead of updating the existing one */
						EXECUTE		spNextKey	'Term',
												@term_key	OUTPUT
						IF @@ERROR <> 0 GOTO fail_from_cursor

						INSERT		Term (
									Term_Key,
									Language_Key,
									Item_Name,
									Plaintext,
									Entered_Session_ID,
									System_Supplied_Data)
						VALUES		(@term_key,
									'en',
									@item_name,
									@item_name,
									@ins_session_id,
									@system)

						IF @@ERROR <> 0 GOTO fail_from_cursor

						EXECUTE		spNextKey		'Term_Version',
													@term_version_key	OUTPUT
						IF @@ERROR <> 0 GOTO fail_from_cursor

						INSERT		Term_Version (
									Term_Version_Key,
									Term_Key,
									Author_And_Date,
									Entered_Session_ID,
									System_Supplied_Data)
						VALUES		(@term_version_key,
									@term_key,
									@author_and_date,
									@ins_session_id,
									@system)

						IF @@ERROR <> 0 GOTO fail_from_cursor									

						UPDATE		Concept
						SET			Term_Key		=	@term_key
						WHERE		Concept_Key		=	@thesaurus_name_type_key

						IF @@ERROR <> 0 GOTO fail_from_cursor
					END
					ELSE
					BEGIN
						/* term only linked within this concept group */
						DECLARE		@cur_term_key		CHAR(16)

						SELECT		@cur_term_key	=	Term_Key
						FROM		Term
						WHERE		Language_Key	=	'en'
						AND			Item_Name		=	@item_name

						IF @@ROWCOUNT = 0
						BEGIN
							/* term can simply be updated */
							UPDATE		Term
							SET			Language_Key	=	'en',
										Item_Name		=	@item_name
							WHERE		Term_Key		=	@term_key

							IF @@ERROR <> 0 GOTO fail_from_cursor

							UPDATE		Term_Version
							SET			Author_And_Date		=	@author_and_date
							WHERE		Term_Version_Key	=	@term_version_key

							IF @@ERROR <> 0 GOTO fail_from_cursor
						END
						ELSE
						BEGIN
							/* term cannot be updated; there is an existing
							 * term with the same name which we will link to
							 * instead */
							EXECUTE		spNextKey	'Term_Version',
													@term_version_key	OUTPUT
							IF @@ERROR <> 0 GOTO fail_from_cursor

							INSERT		Term_Version (
										Term_Version_Key,
										Term_Key,
										Author_And_Date,
										Entered_Session_ID,
										System_Supplied_Data)
							VALUES		(@term_version_key,
										@cur_term_key,
										@author_and_date,
										@ins_session_id,
										@system)

							IF @@error <> 0 GOTO fail_from_cursor

							UPDATE		Concept
							SET			Term_Key			=	@cur_term_key,
										Term_Version_Key	=	@term_version_key
							WHERE		Term_Key			=	@term_key

							IF @@ERROR <> 0 GOTO fail_from_cursor

							DELETE		Term
							WHERE		Term_Key			=	@term_key

							IF @@ERROR <> 0 GOTO fail_from_cursor
						END
					END
				END
			END
		END
		ELSE
		BEGIN
			/* obtain session identifier */
			EXECUTE		usp_Session_ForDate		@ins_user_key,
												@ins_date,
												@ins_session_id		OUTPUT
			IF @@ERROR <> 0 GOTO fail_from_cursor

			/* find/create term */
			SELECT		@term_key		=	Term_Key
			FROM		Term
			WHERE		Language_Key	=	'en'
			AND			Item_Name		=	@item_name

			IF @@ROWCOUNT = 0
			BEGIN
				EXECUTE		spNextKey	'Term',
										@term_key	OUTPUT
				IF @@ERROR <> 0 GOTO fail_from_cursor

				INSERT		Term (
							Term_Key,
							Language_Key,
							Item_Name,
							Plaintext,
							Entered_Session_ID,
							System_Supplied_Data)
				VALUES		(@term_key,
							'en',
							@item_name,
							@item_name,
							@ins_session_id,
							@system)

				IF @@ERROR <> 0 GOTO fail_from_cursor
			END

			EXECUTE		spNextKey	'Term_Version',
									@term_version_key	OUTPUT
			IF @@ERROR <> 0 GOTO fail_from_cursor

			INSERT		Term_Version (
						Term_Version_Key,
						Term_Key,
						Author_And_Date,
						Entered_Session_ID,
						System_Supplied_Data)
			VALUES		(@term_version_key,
						@term_key,
						@author_and_date,
						@ins_session_id,
						@system)

			IF @@ERROR <> 0 GOTO fail_from_cursor

			/* create Meaning */
			EXECUTE		spNextKey	'Meaning',
									@meaning_key	OUTPUT
			IF @@ERROR <> 0 GOTO fail_from_cursor

			INSERT		Meaning (
						Meaning_Key)
			VALUES		(@meaning_key)

			IF @@ERROR <> 0 GOTO fail_from_cursor

			/* create Concept */
			EXECUTE		spNextKey	'Concept',
									@thesaurus_name_type_key	OUTPUT
			IF @@ERROR <> 0 GOTO fail_from_cursor

			INSERT		Concept (
						Concept_Key,
						Term_Key,
						Concept_Group_Key,
						Term_Version_Key,
						List_Preferred,
						Is_Current,
						Preferred,
						Name_Type_Concept_Key,
						Meaning_Key,
						Entered_Session_ID,
						System_Supplied_Data)
			VALUES 		(@thesaurus_name_type_key,
						@term_key,
						'SYSTEM000000000M', /* "Thesaurus Name Types" group */
						@term_version_key,
						1,
						1,
						1,
						'SYSTEM0000000000', /* "Formal" -- meaningless, but
												we need a value here */
						@meaning_key,
						@ins_session_id,
						@system)

			IF @@ERROR <> 0 GOTO fail_from_cursor

			/* create concept history */
			EXECUTE		spNextKey	'Concept_History',
									@concept_history_key	OUTPUT
			IF @@ERROR <> 0 GOTO fail_from_cursor

			INSERT		Concept_History (
						Concept_History_Key,
						Concept_Key,
						Concept_Group_Version_From,
						Entered_Session_ID,
						System_Supplied_Data)
			VALUES		(@concept_history_key,
						@thesaurus_name_type_key,
						'SYSTEM000000000M', /* "Thesaurus Name Types" version */
						@ins_session_id,
						@system)

			IF @@ERROR <> 0 GOTO fail_from_cursor

			/* record taxon name type mapping */
			INSERT		Taxon_Dictionary_Name_Type_Mapping (
						Taxon_Name_Type_Key,
						Thesaurus_Name_Type_Key,
						System_Supplied_Data)
			VALUES		(@taxon_name_type_key,
						@thesaurus_name_type_key,
						0)

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END

		EXECUTE		usp_Import_Export_Job_RecordProcessed	@job_id
		IF @@ERROR <> 0 GOTO fail_from_cursor

		COMMIT TRANSACTION
	END

	CLOSE		name_types
	DEALLOCATE	name_types
	RETURN

fail_from_cursor:
	CLOSE		name_types
	DEALLOCATE	name_types

fail:
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_Concept_ImportTaxonNameTypes failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_ImportTaxonNameTypes') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Concept_ImportTaxonNameTypes'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concept_ImportTaxonNameTypes TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concept_ImportTaxonNameTypes TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Concept_ImportTaxonNameTypes TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_Concept_InsertSpreadsheetRow]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_Concept_InsertSpreadsheetRow]
GO

/*===========================================================================*\
  Description:	Insert a Concept record, plus related records as required,
				based on data extracted from a spreadsheet.

  Parameters:   @SessionID				Session key
				@concept_group_key		Concept group key
				@author					Name of author
				@child_of				Parent concept key
				@citation_date			Citation date
				@fact_title				Name of fact
				@fact_type				Name of fact type
				@fact_description		Fact data
				@list_code				Concept list code
				@name_type				Name of name type
				@sort_code				Concept sort code
				@synonym_of				Synonym concept key
				@language				Name of term language
				@language_key			Term language key
				@term_name				Term name
				@concept_key			[on exit] New concept key

  Created:		Jan 2004

  Last revision information:
	$Revision: 7 $
	$Date: 6/02/09 10:41 $
	$Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Concept_InsertSpreadsheetRow]
	@SessionID			CHAR(16),
	@concept_group_key	CHAR(16),
	@author				VARCHAR(100),
	@child_of			CHAR(16),
	@citation_date		VARCHAR(100),
	@fact_title			VARCHAR(100),
	@fact_type			NVARCHAR(300),
	@fact_description	TEXT,
	@list_code			VARCHAR(50),
	@name_type			NVARCHAR(300),
	@sort_code			INT,
	@synonym_of			CHAR(16),
	@language			VARCHAR(50),
	@language_key		VARCHAR(4),
	@term_name			NVARCHAR(300),
	@concept_key		CHAR(16)	OUTPUT
AS
	SET NOCOUNT ON

	/* check parameters */
	IF @term_name IS NULL
	BEGIN
		RAISERROR ('Term name must be specified.', 16, 1)
		RETURN
	END

	IF NOT @language_key IS NULL
	BEGIN
		IF NOT EXISTS (	SELECT		1
						FROM		Language
						WHERE		Language_Key	=	@language_key )
		BEGIN
			RAISERROR ('Specified language does not exist.', 16, 1)
			RETURN
		END
	END
	ELSE
	BEGIN
		IF @language IS NULL
		BEGIN
			RAISERROR ('Language or Language Key must be specified.', 16, 1)
			RETURN
		END

		SELECT		@language_key	=	Language_Key
		FROM		Language
		WHERE		Item_Name		=	@language

		IF @@ROWCOUNT = 0
		BEGIN
			RAISERROR ('Specified language is not recognised.', 16, 1)
			RETURN
		END
	END

	DECLARE		@term_key			CHAR(16),
				@term_version_key	CHAR(16),
				@name_type_key		CHAR(16),
				@meaning_key		CHAR(16)

	BEGIN TRANSACTION

	/* work out the term */
	SELECT		@term_key		=	Term_Key
	FROM		Term
	WHERE		Language_Key	=	@language_key
	AND			Item_Name		=	@term_name

	IF @@ROWCOUNT = 0
	BEGIN
		EXECUTE		spNextKey	'Term',
								@term_key	OUTPUT
		IF @@ERROR <> 0 GOTO fail

		INSERT		Term (
					Term_Key,
					Language_Key,
					Item_Name,
					Plaintext,
					Entered_Session_ID)
		VALUES		(@term_key,
					@language_key,
					@term_name,
					dbo.ufn_RemoveHtmlMarkup(@term_name),
					@SessionID)
		IF @@ERROR <> 0 GOTO fail
	END

	/* create term version */
	IF @author IS NOT NULL OR @citation_date IS NOT NULL
	BEGIN
		EXECUTE		spNextKey	'Term_Version',
								@term_version_key	OUTPUT
		IF @@ERROR <> 0 GOTO fail

		INSERT		Term_Version (
					Term_Version_Key,
					Term_Key,
					Author_And_Date,
					Entered_Session_ID)
		VALUES		(@term_version_key,
					@term_key,
					ISNULL(@author + ' ', '') + ISNULL(@citation_date, ''),
					@SessionID)
		IF @@ERROR <> 0 GOTO fail
	END

	/* work out the meaning */
	IF @synonym_of IS NOT NULL
	BEGIN
		SELECT		@meaning_key	=	Meaning_Key
		FROM		Concept
		WHERE		Concept_Key		=	@synonym_of

		IF @@ROWCOUNT = 0
		BEGIN
			RAISERROR ('Synonym does not exist.', 16, 1)
			GOTO fail
		END
	END
	ELSE
	BEGIN
		EXECUTE		spNextKey	'Meaning',
								@meaning_key	OUTPUT
		IF @@ERROR <> 0 GOTO fail

		INSERT		Meaning (
					Meaning_Key)
		VALUES		(@meaning_key)

		IF @@ERROR <> 0 GOTO fail
	END

	/* work out name type */
	IF @name_type IS NULL
	BEGIN
		SET			@name_type_key	=	'SYSTEM00000000AN' /* 'Unknown' */
	END
	ELSE
	BEGIN
		SELECT		@name_type_key		=	c.Concept_Key
		FROM		Concept				AS	c
		INNER JOIN	Term				AS	t
		ON			t.Term_Key			=	c.Term_Key
		WHERE		c.Concept_Group_Key	=	'SYSTEM000000000M'
		AND			t.Language_Key		=	'en'
		AND			t.Item_Name			=	@name_type

		IF @@ROWCOUNT = 0
		BEGIN
			EXECUTE		usp_Concept_Insert	@name_type_key	OUTPUT,
											'SYSTEM000000000M',
											@name_type,
											@name_type,
											'en',
											@SessionID,
											'SYSTEM00000000AN'
			IF @@ERROR <> 0 GOTO fail
		END
	END

	/* create concept */
	EXECUTE		spNextKey	'Concept',
							@concept_key	OUTPUT
	IF @@ERROR <> 0 GOTO fail

	INSERT		Concept (
				Concept_Key,
				Term_Key,
				Concept_Group_Key,
				Term_Version_Key,
				List_Preferred,
				Preferred,
				Name_Type_Concept_Key,
				Meaning_Key,
				Sort_Code,
				List_Code,
				Entered_Session_ID)
	SELECT		@concept_key,
				@term_key,
				@concept_group_key,
				@term_version_key,
				CASE WHEN @synonym_of IS NULL THEN 1 ELSE 0 END,
				CASE
					WHEN @synonym_of IS NULL THEN 1
					WHEN EXISTS (	SELECT		1
									FROM		Concept			AS	c
									INNER JOIN	Term			AS	t
									ON			t.Term_Key		=	c.Term_Key
									WHERE		c.Meaning_Key	=	@meaning_key
									AND			t.Language_Key	=	@language_key)
						THEN 0
					ELSE 1
				END,
				@name_type_key,
				@meaning_key,
				@sort_code,
				@list_code,
				@SessionID
	IF @@ERROR <> 0 GOTO fail

	/* update lineage */
	EXECUTE		usp_ConceptLineage_NewConcept	@concept_key
	IF @@ERROR <> 0 GOTO fail

	/* create parent-child relationship */
	IF @child_of IS NOT NULL
	BEGIN
		DECLARE		@relation_key		CHAR(16),
					@relation_type_key	CHAR(16)

		SELECT		@relation_type_key	=	Hierarchy_Relation_Type_Key
		FROM		Concept_Group
		WHERE		Concept_Group_Key	=	@concept_group_key

		EXECUTE		usp_ConceptRelation_Insert	@relation_key	OUTPUT,
												@child_of,
												@concept_key,
												@relation_type_key,
												@SessionID = @SessionID
		IF @@ERROR <> 0 GOTO fail
	END

	/* create fact */
	IF @fact_description IS NOT NULL
	BEGIN
		DECLARE		@fact_type_key		CHAR(16),
					@fact_key			CHAR(16)

		IF @fact_type IS NULL
		BEGIN
			SET			@fact_type_key	=	'SYSTEM00000002NO' /* HTML */
		END
		ELSE
		BEGIN
			SELECT		@fact_type_key		=	c.Concept_Key
			FROM		Concept				AS	c
			INNER JOIN	Term				AS	t
			ON			t.Term_Key			=	c.Term_Key
			WHERE		c.Concept_Group_Key	=	'SYSTEM000000000L'
			AND			t.Language_Key		=	'en'
			AND			t.Item_Name			=	@fact_type

			IF @@ROWCOUNT = 0
			BEGIN
				EXECUTE		usp_Concept_Insert	@fact_type_key	OUTPUT,
												'SYSTEM000000000L',
												@fact_type,
												@fact_type,
												'en',
												@SessionID,
												'SYSTEM00000000AN'
				IF @@ERROR <> 0 GOTO fail
			END
		END

		EXECUTE		spNextKey	'Thesaurus_Fact',
								@fact_key			OUTPUT
		INSERT		Thesaurus_Fact (
					Thesaurus_Fact_Key,
					Item_Name,
					Data,
					Meaning_Key,
					Language_Key,
					Fact_Vague_Date_Type,
					Fact_Type_Concept_Key,
					Entered_Session_ID,
					System_Supplied_Data)
		VALUES		(@fact_key,
					ISNULL(@fact_title, 'Fact'),
					@fact_description,
					@meaning_key,
					'en',
					'U',
					@fact_type_key,
					@SessionID,
					0)
		IF @@ERROR <> 0 GOTO fail
	END

	COMMIT TRANSACTION
	RETURN

fail:
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_Concept_InsertSpreadsheetRow failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_InsertSpreadsheetRow') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Concept_InsertSpreadsheetRow'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concept_InsertSpreadsheetRow TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concept_InsertSpreadsheetRow TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Concept_InsertSpreadsheetRow TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Concept_Paste]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Concept_Paste]
GO

/*===========================================================================*\
  Description:	Pastes a concept from one position to another

  Parameters:	

  Created:	Aug 2004

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Concept_Paste]
	@ConceptKey CHAR(16),
	@DestConceptGroupKey CHAR(16),
	@DestParentConceptKey CHAR(16),
	@IsCut BIT,
	@SessionID CHAR(16),
	@SystemSuppliedData BIT=0
AS

BEGIN TRANSACTION

/*-------------------------------------------------------------*\
	Prepare things for the operation
\*-------------------------------------------------------------*/

--Enforce a value in @SystemSuppliedData as the default value 
--doesn't seem to work every time
IF @SystemSuppliedData IS NULL
	SET @SystemSuppliedData=0

DECLARE @SrcConceptGroupKey CHAR(16)
DECLARE @Lineage VARCHAR(900)
DECLARE @OldRelationTypeKey CHAR(16)
DECLARE @NewRelationTypeKey CHAR(16)
DECLARE @Key CHAR(16)
DECLARE @DestConceptKey CHAR(16)


SELECT @SrcConceptGroupKey=Concept_Group_Key
FROM Concept
WHERE Concept_Key=@ConceptKey
IF @@Error <> 0 GOTO RollbackAndExit

--Find the source concept group's hierarchy relationship
SELECT @OldRelationTypeKey=CG.Hierarchy_Relation_Type_Key
FROM Concept C
INNER JOIN Concept_Group CG ON CG.Concept_Group_Key=C.Concept_Group_Key
WHERE C.Concept_Key=@ConceptKey
IF @@Error <> 0 GOTO RollbackAndExit

--Find the dest concept group's hierarchy relationship
IF @DestParentConceptKey IS NULL 
	SET @NewRelationTypeKey=@OldRelationTypeKey
ELSE
BEGIN
	SELECT @NewRelationTypeKey=CG.Hierarchy_Relation_Type_Key
	FROM Concept C
	INNER JOIN Concept_Group CG ON CG.Concept_Group_Key=C.Concept_Group_Key
	WHERE C.Concept_Key=@DestParentConceptKey
END

/*-------------------------------------------------------------*\
	Perform the cut or copy operation
\*-------------------------------------------------------------*/
IF @IsCut=1 
BEGIN
	SET @DestConceptKey=@ConceptKey

	--Prepare to delete subtree of lineage
	SELECT @Lineage=Lineage
	FROM Concept_Lineage
	WHERE Concept_Key=@ConceptKey
	IF @@Error <> 0 GOTO RollbackAndExit

	IF @DestParentConceptKey IS NULL
	BEGIN
		--Delete source's parent relationship(s)
		DECLARE @KeyToDel CHAR(16)
		DECLARE @Timestamp TIMESTAMP

		DECLARE csr CURSOR STATIC LOCAL FOR
			SELECT Concept_Relation_Key, Timestamp
			FROM Concept_Relation 
			WHERE To_Concept_Key=@ConceptKey
			AND Thesaurus_Relation_Type_Key=@OldRelationTypeKey

		OPEN csr
		
		WHILE 1=1
		BEGIN
			FETCH NEXT FROM csr INTO @KeyToDel, @Timestamp

			IF @@FETCH_STATUS<>0 
				BREAK
			
			EXEC usp_ConceptRelation_Delete @KeyToDel, @Timestamp
			IF @@Error <> 0 GOTO RollbackAndExit

		END
	END
	ELSE
	BEGIN
		--Update source's parent relationship to point to new parent key
		IF EXISTS(SELECT 1 FROM Concept_Relation 
					WHERE To_Concept_Key=@ConceptKey
					AND Thesaurus_Relation_Type_Key=@OldRelationTypeKey)
		BEGIN
			DECLARE @OldKey CHAR(16)
			SELECT @OldKey=From_Concept_Key, @Key=Concept_Relation_Key
			FROM Concept_Relation
			WHERE To_Concept_Key=@ConceptKey
				AND Thesaurus_Relation_Type_Key=@OldRelationTypeKey
			IF @@Error <> 0 GOTO RollbackAndExit

			UPDATE Concept_Relation 
			SET From_Concept_Key=@DestParentConceptKey,
				Changed_Session_ID=@SessionID,
				Thesaurus_Relation_Type_Key=@NewRelationTypeKey
			WHERE To_Concept_Key=@ConceptKey
				AND Thesaurus_Relation_Type_Key=@OldRelationTypeKey
			IF @@Error <> 0 GOTO RollbackAndExit

			EXECUTE		usp_ConceptLineage_UpdateRelation	
					@Key,
					@OldKey,
					@ConceptKey,
					@OldRelationTypeKey
			IF @@Error <> 0 GOTO RollbackAndExit
		END
		ELSE
		BEGIN
			EXECUTE spNextKey 'Concept_Relation', @Key OUTPUT	
		
			EXEC usp_ConceptRelation_Insert
				@Key,
				@DestParentConceptKey,
				@ConceptKey,
				@OldRelationTypeKey,
				NULL,
				NULL,
				NULL,
				@SessionID, 
				@SystemSuppliedData
			IF @@Error <> 0 GOTO RollbackAndExit
		END

	END

	IF @SrcConceptGroupKey<>@DestConceptGroupKey 
	BEGIN
		--Update concept group for source concepts to new group
		UPDATE CChild
		SET Concept_Group_Key = @DestConceptGroupKey
		FROM VW_ConceptChildren CC 
		INNER JOIN Concept CChild ON CChild.Concept_Key=CC.Child_Concept_Key
			AND CChild.Concept_Group_Key=@SrcConceptGroupKey
		WHERE CC.Parent_Concept_Key=@ConceptKey
		IF @@Error <> 0 GOTO RollbackAndExit
	END

	-- Actually delete the old lineage information	
	EXEC usp_ConceptLineage_DeleteSubtree @SrcConceptGroupKey, @Lineage
	IF @@Error <> 0 GOTO RollbackAndExit

END
ELSE
BEGIN
	--Whole branch being copied into a the concept group, so find all concepts and clone them
	DECLARE @ChildConceptKey CHAR(16)

	--Clone the source concepts, updating concept group key
	DECLARE csr CURSOR STATIC LOCAL FOR
		SELECT DISTINCT CChild.Concept_Key
		FROM VW_ConceptChildren CC 
		INNER JOIN Concept CChild ON CChild.Concept_Key=CC.Child_Concept_Key
			AND CChild.Concept_Group_Key=@SrcConceptGroupKey
		WHERE CC.Parent_Concept_Key=@ConceptKey

	--Create a local table to hold key mappings
	DECLARE @ConceptMapping TABLE (
		Src_Concept_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS PRIMARY KEY,
		Dest_Concept_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS
	)

	OPEN csr

	FETCH NEXT FROM csr INTO @ChildConceptKey

	WHILE @@FETCH_STATUS=0
	BEGIN
		EXECUTE spNextKey 'Concept', @Key OUTPUT
		IF @@Error <> 0 GOTO RollBackAndExit
		
		-- When cloning the actual selected concept, remember the new concept key
		IF @ChildConceptKey=@ConceptKey 
			SET @DestConceptKey=@Key
		-- Rememer mappings so we can update relationships later
		INSERT INTO @ConceptMapping VALUES (@ChildConceptKey, @Key)
		IF @@Error <> 0 GOTO RollBackAndExit

		-- Clone the concept
		INSERT INTO Concept (
				Concept_Key,
				Term_Key, 
				Concept_Group_Key,
				Term_Version_Key, 
				List_Preferred, 
				Is_Current, 
				Preferred, 
				Concept_Rank_Key,
				Name_Type_Concept_Key,
				Meaning_Key,
				Author_Copy,
				Sort_Code,
				List_Code,
				Entered_Session_ID,
				System_Supplied_Data,
				Custodian	)
			SELECT 
				@Key, 
				Term_Key, 
				@DestConceptGroupKey, 
				Term_Version_Key, 
				List_Preferred, 
				Is_Current, 
				Preferred, 
				Concept_Rank_Key,
				Name_Type_Concept_Key,
				Meaning_Key,
				Author_Copy,
				Sort_Code,
				List_Code,
				@SessionID,
				@SystemSuppliedData,
				LEFT(@Key, 8)
			FROM Concept 
			WHERE Concept_Key=@ChildConceptKey
		IF @@Error <> 0 GOTO RollBackAndExit

		FETCH NEXT FROM csr INTO @ChildConceptKey
	END

	CLOSE csr
	DEALLOCATE csr

	/*-------------------------------------------------------------*\
		Clone the hierarchical relationships within the copied branch
			of concepts
	\*-------------------------------------------------------------*/
	DECLARE @SrcKey CHAR(16), @DestKey CHAR(16)

	--Declare a temp table with same structure as concept relation that 
	--we can populate with dummy primary keys, then update later
	SELECT TOP 0 * INTO #TempRel FROM Concept_Relation
	IF @@Error <> 0 GOTO RollbackAndExit

	DECLARE cmap CURSOR STATIC LOCAL FOR
		--Note we are cloning parent relationships within the branch, so 
		--exclude the top node
		SELECT * FROM @ConceptMapping WHERE Dest_Concept_Key<>@DestConceptKey
	
	OPEN cmap
	
	FETCH NEXT FROM cmap INTO @SrcKey, @DestKey
	WHILE @@FETCH_STATUS=0
	BEGIN
		INSERT INTO #TempRel (
			Concept_Relation_Key,
			From_Concept_Key,
			To_Concept_Key,
			Thesaurus_Relation_Type_Key,
			Multiplicity,
			Inherited,
			Comment,
			Entered_Session_ID,
			System_Supplied_Data,
			Custodian
			)
		SELECT 
			CR.Concept_Relation_Key, -- Will be replaced later
			ISNULL(CM.Dest_Concept_Key, CR.From_Concept_Key),
			@DestKey,
			Thesaurus_Relation_Type_Key,
			Multiplicity,
			Inherited,
			Comment,
			@SessionID,
			@SystemSuppliedData,
			Left(@DestKey, 8)
		FROM Concept_Relation CR
		LEFT JOIN @ConceptMapping CM ON CM.Src_Concept_Key=CR.From_Concept_Key
		WHERE CR.To_Concept_Key=@SrcKey
		AND CR.Thesaurus_Relation_Type_Key=@OldRelationTypeKey
		IF @@Error <> 0 GOTO RollbackAndExit

		FETCH NEXT FROM cmap INTO @SrcKey, @DestKey
	END
	
	CLOSE cmap
	DEALLOCATE cmap 

	--Now we have a table of concept relationships to insert, but we must update the keys first
	DECLARE crel CURSOR LOCAL FOR
		SELECT Concept_Relation_Key FROM #TempRel
	
	OPEN crel
	
	FETCH NEXT FROM crel INTO @SrcKey
	
	WHILE @@FETCH_STATUS=0
	BEGIN
		EXECUTE spNextKey 'Concept_Relation', @DestKey OUTPUT
		IF @@Error <> 0 GOTO RollbackAndExit
		
		UPDATE #TempRel
		SET Concept_Relation_Key=@DestKey
		WHERE CURRENT OF crel
		IF @@Error <> 0 GOTO RollbackAndExit

		FETCH NEXT FROM crel INTO @SrcKey		
	END

	CLOSE crel
	DEALLOCATE crel

	--Copy the relationships into the concept relation table
	INSERT INTO Concept_Relation (
			Concept_Relation_Key,
			From_Concept_Key,
			To_Concept_Key,
			Thesaurus_Relation_Type_Key,
			Multiplicity,
			Inherited,
			Comment,
			Entered_Session_ID,
			System_Supplied_Data,
			Custodian
		)
		SELECT 
			Concept_Relation_Key,
			From_Concept_Key,
			To_Concept_Key,
			Thesaurus_Relation_Type_Key,
			Multiplicity,
			Inherited,
			Comment,
			Entered_Session_ID,
			System_Supplied_Data,
			Custodian 
		FROM #TempRel
	IF @@Error <> 0 GOTO RollbackAndExit

	DROP TABLE #TempRel

END

/*-------------------------------------------------------------*\
	Join the copied branch of concepts to the destination concept.
	This also fixes up the lineage.
\*-------------------------------------------------------------*/
IF (@DestParentConceptKey IS NOT NULL)
		AND ((@SrcConceptGroupKey<>@DestConceptGroupKey)
			 OR (@IsCut=0))
BEGIN
	EXECUTE spNextKey 'Concept_Relation', @Key OUTPUT	

	EXEC usp_ConceptRelation_Insert
		@Key,
		@DestParentConceptKey,
		@DestConceptKey,
		@OldRelationTypeKey,
		NULL,
		NULL,
		NULL,
		@SessionID, 
		@SystemSuppliedData
	IF @@Error <> 0 GOTO RollbackAndExit
END

/*-------------------------------------------------------------*\
  All went well, so commit.
\*-------------------------------------------------------------*/
COMMIT TRANSACTION

RETURN

RollBackAndExit: 
	/*-------------------------------------------------------------*\
	  Cancel any changes, or left-overs might mess up some tables.
	\*-------------------------------------------------------------*/
	IF @@TranCount > 0 ROLLBACK TRANSACTION

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_Paste') AND SysStat & 0xf = 4)
BEGIN
	PRINT 'Setting up security on procedure usp_Concept_Paste'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concept_Paste TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concept_Paste TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
		GRANT EXECUTE ON dbo.usp_Concept_Paste TO [Dev - JNCC SQL]
END

GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Concept_RecursionCheck_Get]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Concept_RecursionCheck_Get]
GO

CREATE PROCEDURE [dbo].[usp_Concept_RecursionCheck_Get] 
@PotentialChildKey CHAR(16),
@PotentialParentKey CHAR(16),
@RecursionExists BIT OUTPUT
AS

/*===========================================================================*\
  Description:  Checks that the user isn't trying to create a circular
        Concept_Relation.

  Parameters:   @PotentialChildKey - key of dragged node.
                @PotentialParentKey - key of target node.
                @RecursionExists - if cycle exists (i.e. a problem) return 1
                        else return 0 (i.e. OK)

  Created:  March 2004

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/

    SET NOCOUNT ON

    IF @PotentialChildKey = @PotentialParentKey
        SET         @RecursionExists    =   1
    ELSE
    BEGIN
        SELECT @RecursionExists = MAX(CASE WHEN LP.Lineage LIKE LC.Lineage + '\%' THEN 1 ELSE 0 END)
        FROM Concept_Lineage LC
        CROSS JOIN Concept_Lineage LP
        WHERE LC.Concept_Key=@PotentialChildKey
        AND LP.Concept_Key = @PotentialParentKey

        IF @RecursionExists IS NULL
            SET         @RecursionExists    =   0
    END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_RecursionCheck_Get') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_Concept_RecursionCheck_Get'
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
            GRANT EXECUTE ON dbo.usp_Concept_RecursionCheck_Get TO [R2k_AddOnly]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
        GRANT EXECUTE ON dbo.usp_Concept_RecursionCheck_Get TO [R2k_Administrator]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
        GRANT EXECUTE ON dbo.usp_Concept_RecursionCheck_Get TO [R2k_FullEdit]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
        GRANT EXECUTE ON dbo.usp_Concept_RecursionCheck_Get TO [R2k_ReadOnly]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
        GRANT EXECUTE ON dbo.usp_Concept_RecursionCheck_Get TO [R2k_RecordCardsOnly]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_Concept_RecursionCheck_Get TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
       FROM   SysObjects 
       WHERE  Id = Object_Id(N'[dbo].[usp_Concept_Select_ForConceptGroupSearch]')
       AND    Type = 'P')
    DROP PROCEDURE [dbo].[usp_Concept_Select_ForConceptGroupSearch]
GO

/*===========================================================================*\
  Description:  Retrieves a list of concepts that match a search string, in a 
                                specified concept group.

  Parameters:   @ConceptGroup - key of the concept group
                            @SearchText - search text

  Created:  August 2003

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Concept_Select_ForConceptGroupSearch]
    @SearchKey char(16),
  @SearchText varchar(100),
  @SearchSize int = NULL
AS

SET ANSI_NULLS ON
SET ANSI_PADDING ON
SET ANSI_WARNINGS ON
SET ARITHABORT ON
SET CONCAT_NULL_YIELDS_NULL ON
SET QUOTED_IDENTIFIER ON
SET NO_BROWSETABLE OFF


    SET         @SearchSize =   ISNULL(@SearchSize, 0)

    SET ROWCOUNT @SearchSize

SELECT Concept_Key as Item_Key,
  CASE WHEN Author_Copy IS NULL THEN
    Item_Name
  ELSE
    Item_Name + ' ' + Author_Copy
  END AS DisplayTerm,
  CASE WHEN Author_Copy IS NULL THEN
    Plaintext
  ELSE
    Plaintext + ' ' + Author_Copy COLLATE SQL_Latin1_General_CP1_CI_AI
  END AS SearchTerm,
  Author_copy,
  Concept_Rank_Key
FROM VW_ConceptTerm
WHERE Concept_Group_Key=@SearchKey
AND (Plaintext like @SearchText + '%'
OR Author_Copy like @SearchText + '%')
AND Is_Current=1
ORDER BY SearchTerm, Author_Copy

    SET ROWCOUNT 0    
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_Select_ForConceptGroupSearch') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_Concept_Select_ForConceptGroupSearch'
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
            GRANT EXECUTE ON dbo.usp_Concept_Select_ForConceptGroupSearch TO [R2k_AddOnly]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
        GRANT EXECUTE ON dbo.usp_Concept_Select_ForConceptGroupSearch TO [R2k_Administrator]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
        GRANT EXECUTE ON dbo.usp_Concept_Select_ForConceptGroupSearch TO [R2k_FullEdit]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
        GRANT EXECUTE ON dbo.usp_Concept_Select_ForConceptGroupSearch TO [R2k_ReadOnly]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
        GRANT EXECUTE ON dbo.usp_Concept_Select_ForConceptGroupSearch TO [R2k_RecordCardsOnly]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_Concept_Select_ForConceptGroupSearch TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Concept_Select_ForParent]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Concept_Select_ForParent]
GO

/*===========================================================================*\
  Description: Returns a list of concepts that are with the supplied parent	

  Parameters:	@ParentConceptKey
		@HierarchyRelationTypeKey - relationship type used to populate
						hierarchy.

  Created:	August 2003

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Concept_Select_ForParent]
	@ParentConceptKey varchar(100),
  	@HierarchyRelationTypeKey char(16)
AS

SELECT distinct C.Concept_Key, 
		T.Item_Name, 
		C.Sort_Code, 
  		CASE WHEN CR2.Concept_Relation_Key IS NULL THEN 0 
							   ELSE 1 
		END AS HasChildren,
  		C.Concept_Rank_Key
FROM 		Concept_Relation CR1
INNER JOIN 	Concept C ON C.Concept_Key = CR1.To_Concept_Key
INNER JOIN 	Term T ON T.Term_Key = C.Term_Key
LEFT JOIN 	(Concept_Relation CR2 
			INNER JOIN Concept AS C2 ON C2.Concept_Key = CR2.To_Concept_Key
						AND C2.List_Preferred = 1
						AND C2.Is_Current = 1)
		ON CR2.From_Concept_Key = C.Concept_Key
       		AND CR2.Thesaurus_Relation_Type_Key = @HierarchyRelationTypeKey
WHERE 		CR1.From_Concept_Key = @ParentConceptKey
AND 		CR1.Thesaurus_Relation_Type_Key = @HierarchyRelationTypeKey
AND 		C.List_Preferred = 1
AND 		C.Is_Current = 1
ORDER BY 	C.Sort_Code, 
		T.Item_Name

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_Select_ForParent') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Concept_Select_ForParent'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Concept_Select_ForParent TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForParent TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForParent TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForParent TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForParent TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Concept_Select_ForParent TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_Concept_Select_Imported]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_Concept_Select_Imported]
GO

/*===========================================================================*\
  Description:	List-preferred concepts from the specified group that have
				Timestamp later than the given value.

				Note that @session_id should *not* be named @SessionID so
				that dmGeneral does not automagically supply a value when
				we don't want it to.

  Parameters:   @concept_group_key		Concept group key
				@timestamp				Timestamp
				@session_id				[optional] If specified, restrict
										records to those inserted in that
										session

  Created:		Jan 2004

  Last revision information:
	$Revision: 7 $
	$Date: 6/02/09 10:41 $
	$Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Concept_Select_Imported]
	@concept_group_key	CHAR(16),
	@timestamp			TIMESTAMP,
	@session_id			CHAR(16)	=	NULL
AS
	SET NOCOUNT ON

	IF @session_id IS NULL
	BEGIN
		SELECT		c.Concept_Key,
					c.Timestamp,
					ISNULL(
						t.Item_Name + ' '  + c.Author_Copy,
						t.Item_Name)		AS	Item_Name
		FROM		Concept					AS	c
		INNER JOIN	Term					AS	t
		ON			t.Term_Key				=	c.Term_Key
		WHERE		c.Concept_Group_Key		=	@concept_group_key
		AND			c.Timestamp				>	ISNULL(@timestamp, 0)
		AND			c.List_Preferred		=	1
	END
	ELSE
	BEGIN
		SELECT		c.Concept_Key,
					c.Timestamp,
					ISNULL(
						t.Item_Name + ' '  + c.Author_Copy,
						t.Item_Name)		AS	Item_Name
		FROM		Concept					AS	c
		INNER JOIN	Term					AS	t
		ON			t.Term_Key				=	c.Term_Key
		WHERE		c.Concept_Group_Key		=	@concept_group_key
		AND			c.Entered_Session_ID	=	@session_id
		AND			c.Timestamp				>	ISNULL(@timestamp, 0)
		AND			c.List_Preferred		=	1
	END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_Select_Imported') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Concept_Select_Imported'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concept_Select_Imported TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concept_Select_Imported TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Concept_Select_Imported TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_Concept_Select_Synonyms]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_Concept_Select_Synonyms]
GO

/*===========================================================================*\
  Description:	Names of synonyms of the specified concept.

  Parameters:   @concept_key			Concept key

  Created:		Jan 2004

  Last revision information:
	$Revision: 7 $
	$Date: 6/02/09 10:41 $
	$Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Concept_Select_Synonyms]
	@concept_key		CHAR(16)
AS
	SET NOCOUNT ON

	SELECT DISTINCT
				t.Item_Name
	FROM		Concept					AS	c
	INNER JOIN	Concept					AS	s
	ON			s.Meaning_Key			=	c.Meaning_Key
	INNER JOIN	Term					AS	t
	ON			t.Term_Key				=	s.Term_Key
	WHERE		c.Concept_Key			=	@concept_key
	AND			s.Concept_Key			<>	@concept_key	
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_Select_Synonyms') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Concept_Select_Synonyms'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concept_Select_Synonyms TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concept_Select_Synonyms TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Concept_Select_Synonyms TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Department_Get_ForIndividual]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Department_Get_ForIndividual]
GO

/*===========================================================================*\
  Description:	Returns an department name for an individual record.

  Parameters:	@Key		Individual key
		@GetAcronym 	If this is set to 1, then the acronym of the 
				department will be returned. If the department
				does not have an acronym, then the department
				name will be returned instead.
		@Output		Department name (Output parameter)

  Created:	August 2003

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Department_Get_ForIndividual]
	@Key char(16),
	@GetAcronym bit = NULL,
	@Output varchar(100) output
AS

SET NOCOUNT ON

SELECT 		@Output = CASE WHEN (@GetAcronym IS NOT NULL) AND (@GetAcronym = 1) 
				THEN IsNull(OD.Acronym, OD.Item_Name)
				ELSE OD.Item_Name
			END 
FROM 		Organisation_Department OD
INNER JOIN 	Organisation O ON O.Name_Key = OD.Name_Key
INNER JOIN 	Individual I ON I.Organisation_Department_Key = OD.Organisation_Department_Key
WHERE 		I.Name_Key=@Key

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Department_Get_ForIndividual') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Department_Get_ForIndividual'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Department_Get_ForIndividual TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Department_Get_ForIndividual TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Department_Get_ForIndividual TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Department_Get_ForIndividual TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Department_Get_ForIndividual TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Department_Get_ForIndividual TO [Dev - JNCC SQL]
END

GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_DeterminationsLifeSciences_Select_ForSearch]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_DeterminationsLifeSciences_Select_ForSearch]
GO

CREATE PROCEDURE [dbo].[usp_DeterminationsLifeSciences_Select_ForSearch] 
@SearchText VARCHAR(100),
@UserDomainMask int,
@SearchSize int = NULL
AS
--
--  DESCRIPTION
--  Returns Concept_Key and DisplayTerm when search characters are entered.
--  Uses domain mask if the taxon list item is mapped
--
--	PARAMETERS
--	NAME			
--	@SearchText
--	@UserDomainMask 		
--
--  AUTHOR:			Anthony Simpson, Dorset Software
--  CREATED:			2003-10-20

SET NOCOUNT ON

    SET @SearchSize = ISNULL(@SearchSize, 0)

    SET ROWCOUNT @SearchSize

	SELECT
			ITN.Taxon_List_Item_Key AS Item_Key, 
			ITN.Preferred_Name + ' - ' + TL.Item_Name AS DisplayTerm, 
			ITN.Preferred_Name + ' - ' + TL.Item_Name AS SearchTerm
	FROM		Index_Taxon_Name AS ITN 
	INNER JOIN Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key = ITN.Taxon_List_Version_Key
	INNER JOIN Taxon_List TL ON TL.Taxon_List_Key=TLV.Taxon_List_Key
	LEFT JOIN	Taxon_Dictionary_Concept_Mapping TDCM ON TDCM.Taxon_List_Item_Key = ITN.Taxon_List_Item_Key
	LEFT JOIN	Concept C ON C.Concept_Key = TDCM.Concept_Key
	LEFT JOIN 	Concept_Group CG ON CG.Concept_Group_Key=C.Concept_Group_Key
	LEFT JOIN 	Local_Domain LD ON LD.Local_Domain_Key=CG.Local_Domain_Key
	LEFT JOIN 	Domain D ON D.Domain_Key=LD.Domain_Key
	WHERE 		ITN.Preferred_Name LIKE @SearchText + '%'
			AND ((((D.Domain_Mask & @UserDomainMask>0) OR (D.Domain_Mask = 0))
			AND D.Has_Occurrences = 1) OR D.Domain_Key IS NULL)

	ORDER BY DisplayTerm

    SET ROWCOUNT 0
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_DeterminationsLifeSciences_Select_ForSearch') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_DeterminationsLifeSciences_Select_ForSearch'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_DeterminationsLifeSciences_Select_ForSearch TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_DeterminationsLifeSciences_Select_ForSearch TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_DeterminationsLifeSciences_Select_ForSearch TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_DeterminationsLifeSciences_Select_ForSearch TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_DeterminationsLifeSciences_Select_ForSearch TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_DeterminationsLifeSciences_Select_ForSearch TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Determination_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Determination_Update]
GO

/*===========================================================================*\
  Description:	Updates a record in the Determination table.
		Ensures the Domain mask of the specimen is also updated.

  Parameters:	@Key 
		@DeterminedItemKey
		@OccurrenceKey 
		@SpecimenCollectionUnitKey  
		@DeterminationTypeKey  
		@NomenclaturalStatusConceptKey 
		@Confidence
		@DeterminerNameKey 
		@InferredDeterminer 
		@DeterminerRoleKey 
		@VagueDateStart
		@VagueDateEnd 
		@VagueDateType 
		@UsedSpecimen 
		@Preferred
		@Method
		@Notes 
		@SessionID 
		@Timestamp
		@IsForSpecimen		Indicates whether to update preferred 
					flag in Specimen_Unit or Determination.
		@RecordsAffected	OUTPUT Can't rely on correct value to come out,
					so use a parameter instead.

  Created:	July 2003

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Determination_Update]
	@Key char(16), 
	@DeterminedItemKey char(16), 
	@OccurrenceKey char(16), 
	@SpecimenCollectionUnitKey char(16), 
	@DeterminationTypeKey char(16), 
	@NomenclaturalStatusConceptKey char(16),
	@Confidence tinyint,
	@DeterminerNameKey char(16), 
	@InferredDeterminer tinyint,
	@DeterminerRoleKey char(16), 
	@VagueDateStart int, 
	@VagueDateEnd int, 
	@VagueDateType varchar(2),
	@UsedSpecimen bit,
	@Preferred bit,
	@Method text,
	@Notes text,
	@SessionID char(16),
	@Timestamp timestamp,
	@IsForSpecimen bit,
	@RecordsAffected int OUTPUT
AS

	DECLARE @SpecimenKey char(16),
		@CurrentConceptKey char(16),
		@CurrentConceptMask int,
		@CurrentPreferred bit,
		@ConceptMask int

	BEGIN TRANSACTION
		-- Retrieve the mask of the new concept.
		EXECUTE	usp_Get_Concept_Domain_Mask @DeterminedItemKey, @ConceptMask OUTPUT

		/*-------------------------------------------------------------*\
		  Determine if and where to update the preferred states.
		  And also if specimen mask needs to be update.

		  If @Preferred is 0, then we are updating non-preferred
		  determination and therefore, no need to reset the one that is
		  still preferred. This only ensures there is only 1 preferred
		  at any time for the specimen.

		  We only do this if the preferred determination has actually
		  changed determination. This is necessary to avoid a timestamp
		  error. For example - if @Preferred were 1, and there was no
		  checking to see if the preferred determination had changed,
		  the record's Preferred field and the record's Timestamp would
		  be altered. The main update would then fail because the
		  timestamp is different to the one passed into the procedure.
		\*-------------------------------------------------------------*/
		IF @IsForSpecimen = 1 AND @Preferred = 1 BEGIN
			DECLARE	@CurrentPrefDetKey char(16),
				@CurrentPrefTaxonDetKey char(16)

			-- Get existing preferred keys from Specimen_Unit table
			SELECT	@CurrentPrefDetKey = Preferred_Determination_Key,
				@CurrentPrefTaxonDetKey = Preferred_Taxon_Determination_Key
			FROM	Specimen_Unit
			WHERE	Collection_Unit_Key = @SpecimenCollectionUnitKey

			-- Changing to another preferred Determination
			IF @CurrentPrefDetKey <> @Key BEGIN
				-- Get existing concept's key
				SELECT	@CurrentConceptKey = Concept_Key
				FROM	Determination
				WHERE	Determination_Key = @Key

				-- We're having a new preferred, so replace the old one.
				UPDATE	Specimen_Unit
				SET	Preferred_Determination_Key = @Key
				WHERE	Collection_Unit_Key = @SpecimenCollectionUnitKey

				IF @@Error <> 0 GOTO RollbackAndExit

				-- Preferred state should NOT change in Determination, so get existing value to
				-- override parameter value.
				SELECT	@Preferred = Preferred
				FROM	Determination
				WHERE	Determination_Key = @Key
			END

			-- Get existing concept's mask
			EXECUTE	usp_Get_Concept_Domain_Mask @CurrentConceptKey, @CurrentConceptMask OUTPUT

			-- Different mask, so switch current one OFF in Collection_Unit
			IF @CurrentConceptMask <> @ConceptMask BEGIN
				EXECUTE	usp_CollectionUnit_Update_DomainMask @SpecimenCollectionUnitKey, @CurrentConceptMask, 0
				IF @@Error <> 0 GOTO RollbackAndExit
			END
		END ELSE 
		-- Dealing with Determination/Taxon_Determination tables only, for occurrences.
		-- Also means that Specimen_Collection_Unit_Key can be NULL, but that doesn't mean
		-- the value passed in is NULL though.
		IF @IsForSpecimen = 0 AND @Preferred = 1 BEGIN
			-- Not guaranteed there is an associated specimen key for the preferred determination	.
			SELECT	@CurrentConceptKey = Concept_Key,
				@SpecimenKey = Specimen_Collection_Unit_Key
			FROM	Determination
			WHERE	Determination_Key IN (
					SELECT	D.Determination_Key
					FROM	Determination D 
					WHERE	D.Occurrence_Key = @OccurrenceKey
				UNION
					SELECT	D.Determination_Key
					FROM	Specimen_Field_Data SFD 
					JOIN	Specimen_Unit SU ON SU.Collection_Unit_Key = SFD.Collection_Unit_Key
					JOIN	Determination D ON D.Specimen_Collection_Unit_Key = SU.Collection_Unit_Key
					WHERE	SFD.Occurrence_Key = @OccurrenceKey
				)
			AND	Preferred = 1

			-- We're having a new preferred, so switch the old one OFF
			-- But only if not changing preferred, or updating WILL mess up timestamp flag!
			UPDATE	Determination
			SET	Preferred = 0
			WHERE	Determination_Key IN (
					SELECT	D.Determination_Key
					FROM	Determination D 
					WHERE	D.Occurrence_Key = @OccurrenceKey
				UNION
					SELECT	D.Determination_Key
					FROM	Specimen_Field_Data SFD 
					JOIN	Specimen_Unit SU ON SU.Collection_Unit_Key = SFD.Collection_Unit_Key
					JOIN	Determination D ON D.Specimen_Collection_Unit_Key = SU.Collection_Unit_Key
					WHERE	SFD.Occurrence_Key = @OccurrenceKey
				)
			AND	Preferred = 1
			AND	Determination_Key <> @Key 

			IF @@Error <> 0 GOTO RollbackAndExit

			-- Get existing concept's mask.
			EXECUTE	usp_Get_Concept_Domain_Mask @CurrentConceptKey, @CurrentConceptMask OUTPUT

			-- New concept's mask different from current one. Refresh specimen mask is there is one.
			IF @SpecimenKey IS NOT NULL AND @CurrentConceptMask <> @ConceptMask BEGIN
				EXECUTE	usp_CollectionUnit_Update_DomainMask @SpecimenKey, @CurrentConceptMask, 0
				IF @@Error <> 0 GOTO RollbackAndExit
			END
		END

		/*-------------------------------------------------------------*\
		  Do the table update.
		\*-------------------------------------------------------------*/
		UPDATE	Determination
		SET	Concept_Key = @DeterminedItemKey,
			Occurrence_Key = @OccurrenceKey,
			Specimen_Collection_Unit_Key = @SpecimenCollectionUnitKey,
			Determination_Type_Key = @DeterminationTypeKey,
			Nomenclatural_Status_Concept_Key = @NomenclaturalStatusConceptKey,
			Confidence = @Confidence,
			Determiner_Name_Key = @DeterminerNameKey,
			Inferred_Determiner = @InferredDeterminer,
			Determiner_Role_Key = @DeterminerRoleKey,
			Vague_Date_Start = @VagueDateStart,
			Vague_Date_End = @VagueDateEnd,
			Vague_Date_Type = @VagueDateType,
			Used_Specimen = @UsedSpecimen,
			Preferred = @Preferred,
			Method = @Method,
			Notes = @Notes,
			Changed_Session_ID = @SessionID
		WHERE	Determination_Key = @Key
		AND	(@Timestamp = Timestamp)

		SET @RecordsAffected = @@RowCount

		IF @@Error <> 0 GOTO RollbackAndExit

		/*-------------------------------------------------------------*\
		  Switch bit of new mask ON in Collection_Unit.
		\*-------------------------------------------------------------*/
		IF @SpecimenCollectionUnitKey IS NOT NULL BEGIN
			EXECUTE	usp_CollectionUnit_Update_DomainMask @SpecimenCollectionUnitKey, @ConceptMask, 1
			IF @@Error <> 0 GOTO RollbackAndExit
		END

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Determination_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Determination_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Determination_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Determination_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Determination_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Determination_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Determination_Update TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_DomainConceptGroup_Name_Get]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_DomainConceptGroup_Name_Get]
GO

/*===========================================================================*\
  Description:	Returns the name of the Domain and Concept Group

  Parameters:	@Key	Concept group key
		@Name	Domain and Concept group name

  Created:	13th August 2004

  Last revision information:
	$Revision: 7 $
	$Date: 6/02/09 10:41 $
	$Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_DomainConceptGroup_Name_Get]
	@Key CHAR(16),
	@Name VARCHAR(201) OUTPUT
AS
	SET NOCOUNT ON

	DECLARE @DomainName varchar(100),
		@ConceptGroupName varchar(100)
	
	SELECT 		@ConceptGroupName = CG.Item_Name,
			@DomainName = D.Item_Name
	FROM 		Concept_Group AS CG
	INNER JOIN	Local_Domain AS LD ON LD.Local_Domain_Key = CG.Local_Domain_Key
	INNER JOIN	Domain AS D ON D.Domain_Key = LD.Domain_Key
	WHERE		Concept_Group_Key = @Key

	SET @Name = IsNull(@DomainName + '/', '---/') + IsNull(@ConceptGroupName, '---')
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_DomainConceptGroup_Name_Get') AND SysStat & 0xf = 4)
BEGIN
  PRINT 'Setting up security on procedure usp_DomainConceptGroup_Name_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_DomainConceptGroup_Name_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_DomainConceptGroup_Name_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_DomainConceptGroup_Name_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_DomainConceptGroup_Name_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_DomainConceptGroup_Name_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_DomainConceptGroup_Name_Get TO [Dev - JNCC SQL]
END
GO


/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_DomainConceptGroup_Select_ForCollectionUnit]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_DomainConceptGroup_Select_ForCollectionUnit]
GO

/*===========================================================================*\
  Description:	Returns concept group key and local domain key for the named concept
		group associated with the domain/local domain of the preferred
		determination for the given collection unit.
		System concept group is always returned for Collections and Stores,
		as there is no 100% sure way to identify a single domain for those.

  Parameters:	@Key		
		@DomainConceptGroupName
				Name of the domain concept group where new parameters
				added by users should go. If the concept group exists
				for the domain, its key will be returned, ready for use.

  Created:	November 2003

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_DomainConceptGroup_Select_ForCollectionUnit]
	@Key char(16),
	@DomainConceptGroupName varchar(100),
	@TopLevelNodeContext int
AS

SET NOCOUNT ON

	/*-------------------------------------------------------------*\
	  Use left join at the end to always get the Local_Domain_Key, 
	  even if @DomainConceptGroupName doesn't exist for the 
	  determination's domain.
	\*-------------------------------------------------------------*/
	IF @TopLevelNodeContext = 1	-- i.e. if NodeContext = ncSpecimen
		SELECT 		LD2.Local_Domain_Key, CG2.Concept_Group_Key
		FROM 		Specimen_Unit SU
		LEFT JOIN	Determination D ON D.Determination_Key = SU.Preferred_Determination_Key
		LEFT JOIN	(Taxon_Determination TD 
					INNER JOIN Taxon_Dictionary_Concept_Mapping AS TDCM ON TD.Taxon_List_Item_Key = TDCM.Taxon_List_Item_Key)
				ON TD.Taxon_Determination_Key = SU.Preferred_Taxon_Determination_Key
		INNER JOIN	Concept C ON (C.Concept_Key = D.Concept_Key) OR (C.Concept_Key = TDCM.Concept_Key)
		INNER JOIN	Concept_Group CG ON CG.Concept_Group_Key = C.Concept_Group_Key
		INNER JOIN	Local_Domain LD ON LD.Local_Domain_Key = CG.Local_Domain_Key
		INNER JOIN	Local_Domain LD2 ON LD2.Domain_Key = LD.Domain_Key
		LEFT JOIN	Concept_Group CG2 ON CG2.Local_Domain_Key = LD2.Local_Domain_Key
				AND CG2.Item_Name = @DomainConceptGroupName
		WHERE		SU.Collection_Unit_Key = @Key
	ELSE IF @DomainConceptGroupName = 'Measurement Parameters' 
		SELECT 		Local_Domain_Key, Concept_Group_Key
		FROM		Concept_Group
		WHERE		Concept_Group_Key = 'SYSTEM000000000E'
	ELSE IF @DomainConceptGroupName = 'Descriptor Parameters'
		SELECT 		Local_Domain_Key, Concept_Group_Key
		FROM		Concept_Group
		WHERE		Concept_Group_Key = 'SYSTEM000000000W'

SET NOCOUNT OFF 
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_DomainConceptGroup_Select_ForCollectionUnit') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_DomainConceptGroup_Select_ForCollectionUnit'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_DomainConceptGroup_Select_ForCollectionUnit TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_DomainConceptGroup_Select_ForCollectionUnit TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_DomainConceptGroup_Select_ForCollectionUnit TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_DomainConceptGroup_Select_ForCollectionUnit TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_DomainConceptGroup_Select_ForCollectionUnit TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_DomainConceptGroup_Select_ForCollectionUnit TO [Dev - JNCC SQL]
END
GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Domain_Mask_Get]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Domain_Mask_Get]
GO

/*===========================================================================*\
  Description:	Returns a given user's domain mask

  Parameters:
	@Name_Key	Name_Key of user to calculate domain mask for
	@UserDomainMask	User's Domain Mask. Returns 0 if User has no domains assigned.

  Created:	January 2004

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Domain_Mask_Get] 
	@Name_Key CHAR(16),
	@UserDomainMask INT OUTPUT
AS

	SET NOCOUNT ON
	-- Use DISTINCT in the SUM(), in case some domains have the same mask!
	SELECT 		@UserDomainMask = SUM(DISTINCT D.Domain_Mask)
	FROM 		User_Domain_Access UDA
	INNER JOIN	Domain D ON UDA.Domain_Key = D.Domain_Key AND D.Has_Occurrences = 1
	WHERE 		UDA.Allow_Browse = 1
	AND 		UDA.Name_Key = @Name_Key
	
	--Return a domain mask of 0 (no domains) if no records exist
	SET @UserDomainMask = ISNULL(@UserDomainMask, 0)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Domain_Mask_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Domain_Mask_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Domain_Mask_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Domain_Mask_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Domain_Mask_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Domain_Mask_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Domain_Mask_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Domain_Mask_Get TO [Dev - JNCC SQL]
END
GO

IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_FieldData_DragDropKey_Get') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_FieldData_DragDropKey_Get]
GO

/*===========================================================================*\
  Description:	Returns the key used for drag and drop given a Specimen_Field_Data_Key.

  Parameters:	@Key 

  Created:	January 2004

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_FieldData_DragDropKey_Get] 
	@Key CHAR(16),
	@DragDropKey CHAR(16) OUTPUT
AS
	SET NOCOUNT ON
	SELECT 		@DragDropKey = 
			CASE 
				WHEN O.Sample_Key IS NULL THEN XO.Sample_Key
				ELSE O.Sample_Key
			END
	FROM		Specimen_Field_Data SFD 
	LEFT JOIN 	Occurrence O ON SFD.Occurrence_Key = O.Occurrence_Key 
	LEFT JOIN 	Taxon_Occurrence XO ON SFD.Taxon_Occurrence_Key = XO.Taxon_Occurrence_Key
	WHERE		Specimen_Field_Data_Key = @Key
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_FieldData_DragDropKey_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_FieldData_DragDropKey_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_FieldData_DragDropKey_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_FieldData_DragDropKey_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_FieldData_DragDropKey_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_FieldData_DragDropKey_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_FieldData_DragDropKey_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_FieldData_DragDropKey_Get TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_FieldData_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_FieldData_Select]
GO

/*===========================================================================*\
  Description:	Returns data for the Field Data frame.

  Parameters:	@Key	Collection key

  Created:	October 2003

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_FieldData_Select]
	@Key char(16)
AS

SET NOCOUNT ON

	SELECT	SV.Survey_Key,
		SV.Item_Name + ' - ' + dbo.ufn_GetFormattedName(SV.Run_By) AS Survey_Name,
		SV.Run_By, 
		SFD.Inferred_Survey,
		S.Sample_Key,
		SE.Survey_Event_Key,
		S.Location_Key,
		LN.Item_Name AS Location_Name,
		CASE WHEN S.Location_Key IS NULL THEN S.Spatial_Ref_System ELSE L.Spatial_Ref_System END AS Spatial_Ref_System,
		SFD.Inferred_Location,
		S.Spatial_Ref_Qualifier,
		S.Spatial_Ref,
		S.Lat, 
		S.Long,
		SFD.Inferred_Spatial_Ref,
		S.Sample_Type_Key,
		ST.Short_Name AS Sample_Type,
		S.Vague_Date_Start,
		S.Vague_Date_End,
		S.Vague_Date_Type,
		SFD.Taxon_Occurrence_Key,
		SFD.Occurrence_Key,
		SFD.Inferred_Sample_Type,
		SFD.Inferred_Date,
		SFD.Inferred_Collectors,
		SFD.Gathering_Event,
	  	SFD.Custodian,
		SFD.[Timestamp]
	FROM Specimen_Field_Data SFD
	LEFT JOIN Taxon_Occurrence XO ON XO.Taxon_Occurrence_Key=SFD.Taxon_Occurrence_Key
	LEFT JOIN Occurrence O ON O.Occurrence_Key=SFD.Occurrence_Key
	INNER JOIN [Sample] S ON S.Sample_Key=XO.Sample_Key OR S.Sample_Key=O.Sample_Key
	INNER JOIN Survey_Event SE ON SE.Survey_Event_Key=S.Survey_Event_Key
	INNER JOIN Survey SV ON SV.Survey_Key=SE.Survey_Key
	LEFT JOIN Location AS L ON L.Location_Key = S.Location_Key
	LEFT JOIN Location_Name LN ON LN.Location_Key=S.Location_Key
				AND LN.Preferred = 1
	INNER JOIN Sample_Type ST ON ST.Sample_Type_Key=S.Sample_Type_Key
	WHERE SFD.Specimen_Field_Data_Key = @Key

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_FieldData_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_FieldData_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_FieldData_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_FieldData_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_FieldData_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_FieldData_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_FieldData_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_FieldData_Select TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_HoldingOrgName_Get]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_HoldingOrgName_Get]
GO

/*===========================================================================*\
  Description:	Get the HoldingOrg name.

  Created:	May 2004

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_HoldingOrgName_Get]
	@HoldingOrg varchar(60) OUTPUT 
AS
	SELECT 		@HoldingOrg = O.Full_Name
	FROM 		Setting AS S
	INNER JOIN	Organisation AS O ON O.Name_Key = S.Data
	WHERE 		[Name] = 'HoldingOrg'
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_HoldingOrgName_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_HoldingOrgName_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_HoldingOrgName_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_HoldingOrgName_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_HoldingOrgName_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_HoldingOrgName_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_HoldingOrgName_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_HoldingOrgName_Get TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_Import_Export_Job_Close]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_Import_Export_Job_Close]
GO

/*===========================================================================*\
  Description:	Close a taxon dictionary import/export process.

  Parameters:	@import_export_id		Job identifier

  Created:		Dec 2003

  Last revision information:
	$Revision: 7 $
	$Date: 6/02/09 10:41 $
	$Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Import_Export_Job_Close]
	@import_export_id		INT
AS
	SET NOCOUNT ON

	DELETE		Import_Export_Job
	WHERE		Import_Export_Job_ID	=	@import_export_id
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Import_Export_Job_Close') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Import_Export_Job_Close'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Import_Export_Job_Close TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Import_Export_Job_Close TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Import_Export_Job_Close TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_Import_Export_Job_Configure]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_Import_Export_Job_Configure]
GO

/*===========================================================================*\
  Description:	Set parameters of an import/export process.

  Parameters:	@import_export_id		Job identifier
				@concept_group_key		Concept group key
                @record_count			Total record count

  Created:		Dec 2003

  Last revision information:
	$Revision: 7 $
	$Date: 6/02/09 10:41 $
	$Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Import_Export_Job_Configure]
	@import_export_id		INT,
	@concept_group_key		CHAR(16),
	@record_count			INT
AS
	SET NOCOUNT ON

	UPDATE		Import_Export_Job
	SET			Total_Records			=	@record_count,
				Concept_Group_Key		=	@concept_group_key
	WHERE		Import_Export_Job_ID	=	@import_export_id
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Import_Export_Job_Configure') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Import_Export_Job_Configure'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Import_Export_Job_Configure TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Import_Export_Job_Configure TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Import_Export_Job_Configure TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_Import_Export_Job_GetProgress]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_Import_Export_Job_GetProgress]
GO

/*===========================================================================*\
  Description:	Read current state of the specified import/export job.

  Parameters:	@job_id					Job identifier
				@status_message			[on exit] Describes current status
				@total_records			[on exit] Number of records in job
				@records_processed		[on exit] Number of records processed

  Created:		Dec 2003

  Last revision information:
	$Revision: 7 $
	$Date: 6/02/09 10:41 $
	$Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Import_Export_Job_GetProgress]
	@import_export_id		INT,
	@status_message			VARCHAR(200)	OUTPUT,
	@total_records			INT				OUTPUT,
	@records_processed		INT				OUTPUT
AS
	SET NOCOUNT ON

	SELECT		@status_message			=	Status_Message,
				@total_records			=	Total_Records,
				@records_processed		=	Records_Processed
	FROM		Import_Export_Job
	WHERE		Import_Export_Job_ID	=	@import_export_id

	IF @@ROWCOUNT = 0 RAISERROR ('Job does not exist or has been closed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Import_Export_Job_GetProgress') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Import_Export_Job_GetProgress'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Import_Export_Job_GetProgress TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Import_Export_Job_GetProgress TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Import_Export_Job_GetProgress TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_Import_Export_Job_Prepare]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_Import_Export_Job_Prepare]
GO

/*===========================================================================*\
  Description:	Prepare to intiate a taxon dictionary import/export process.

  Parameters:	@import_export_id		On exit, a job identifier, to be
										passed to
											usp_ConceptGroup_ImportTaxonList
										or
											usp_TaxonList_ImportConceptGroup.

										Can be used to look up current
										progress of the job in the table

											Import_Export_Progress.

  Created:		Dec 2003

  Last revision information:
	$Revision: 7 $
	$Date: 6/02/09 10:41 $
	$Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Import_Export_Job_Prepare]
	@import_export_id		INT		OUTPUT
AS
	SET NOCOUNT ON

	INSERT		Import_Export_Job (
				Status_Message,
				Total_Records,
				Records_Processed)
	VALUES		('Initialising',
				0,
				0)

	IF @@ERROR <> 0 RETURN

    SET			@import_export_id	=	SCOPE_IDENTITY()
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Import_Export_Job_Prepare') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Import_Export_Job_Prepare'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Import_Export_Job_Prepare TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Import_Export_Job_Prepare TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Import_Export_Job_Prepare TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_Import_Export_Job_RecordProcessed]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_Import_Export_Job_RecordProcessed]
GO

/*===========================================================================*\
  Description:	Increment current record counter of an import/export process.

  Parameters:	@import_export_id		Job identifier

  Created:		Dec 2003

  Last revision information:
	$Revision: 7 $
	$Date: 6/02/09 10:41 $
	$Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Import_Export_Job_RecordProcessed]
	@import_export_id		INT
AS
	SET NOCOUNT ON

	UPDATE		Import_Export_Job
	SET			Records_Processed		=	Records_Processed + 1
	WHERE		Import_Export_Job_ID	=	@import_export_id
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Import_Export_Job_RecordProcessed') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Import_Export_Job_RecordProcessed'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Import_Export_Job_RecordProcessed TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Import_Export_Job_RecordProcessed TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Import_Export_Job_RecordProcessed TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_Import_Export_Job_UpdateStatus]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_Import_Export_Job_UpdateStatus]
GO

/*===========================================================================*\
  Description:	Update current status of an import/export process.

  Parameters:	@import_export_id		Job identifier
				@status_message			New status message

  Created:		Dec 2003

  Last revision information:
	$Revision: 7 $
	$Date: 6/02/09 10:41 $
	$Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Import_Export_Job_UpdateStatus]
	@import_export_id		INT,
	@status_message			VARCHAR(200)
AS
	SET NOCOUNT ON

	UPDATE		Import_Export_Job
	SET			Status_Message			=	@status_message
	WHERE		Import_Export_Job_ID	=	@import_export_id
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Import_Export_Job_UpdateStatus') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Import_Export_Job_UpdateStatus'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Import_Export_Job_UpdateStatus TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Import_Export_Job_UpdateStatus TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Import_Export_Job_UpdateStatus TO [Dev - JNCC SQL]
END
GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_InscriptionsAndLabels_Select_ForSpecimen]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_InscriptionsAndLabels_Select_ForSpecimen]
GO

CREATE PROCEDURE [dbo].[usp_InscriptionsAndLabels_Select_ForSpecimen] 
@ParentKey CHAR(16)

AS

--  DESCRIPTION
--  Returns Inscriptions and Labels for a specified Specimen
--
--  PARAMETERS
--  NAME				DESCRIPTION
--	@ParentKey 			Only the records associated with the parent key are returned
--
--
--  AUTHOR:     		Ben Collier, Dorset Software
--  CREATED:    		2003-08-27
--
SET NOCOUNT ON
SELECT 
	Specimen_Label_Key AS Item_Key, 
	Specimen_Label_Key AS Join_Key,
	Inscription AS Item_Name
FROM 
	SPECIMEN_LABEL
WHERE Collection_Unit_Key = @ParentKey

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_InscriptionsAndLabels_Select_ForSpecimen') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_InscriptionsAndLabels_Select_ForSpecimen'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_InscriptionsAndLabels_Select_ForSpecimen TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_InscriptionsAndLabels_Select_ForSpecimen TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_InscriptionsAndLabels_Select_ForSpecimen TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_InscriptionsAndLabels_Select_ForSpecimen TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_InscriptionsAndLabels_Select_ForSpecimen TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_InscriptionsAndLabels_Select_ForSpecimen TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
       FROM   SysObjects 
       WHERE  Id = Object_Id(N'[dbo].[usp_InternalReferences_Select_ForSearch]')
       AND    Type = 'P')
    DROP PROCEDURE [dbo].[usp_InternalReferences_Select_ForSearch]
GO

/*===========================================================================*\
  Description:  Search procedure for Internal References

  Parameters:   @SearchText 

  Created:  November 2003

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_InternalReferences_Select_ForSearch]
    @SearchText VARCHAR(100)
AS
    SET NOCOUNT ON

    SELECT      r.Source_Key                                AS  Item_Key,
                a.Author
                + ' - '
                + dbo.ufn_GetDateFromVagueDate(
                    r.Year_Vague_Date_Start,
                    r.Year_Vague_Date_End,
                    r.Year_Vague_Date_Type)
                + ', '
                + dbo.ufn_RtfToPlainText(r.Full_Reference)  AS  DisplayTerm,
                a.Author
                + ' - '
                + dbo.ufn_GetDateFromVagueDate(
                    r.Year_Vague_Date_Start,
                    r.Year_Vague_Date_End,
                    r.Year_Vague_Date_Type)
                + ', '
                + dbo.ufn_RtfToPlainText(r.Full_Reference)  AS  SearchTerm
    FROM        Reference                                   AS  r
    INNER JOIN  VW_Reference_Authors                        AS  a
    ON          a.Source_Key                                =   r.Source_Key
    WHERE       a.Author                                    LIKE @SearchText + '%'
    OR          dbo.ufn_RtfToPlainText(r.Title)             LIKE @SearchText + '%'
    OR          dbo.ufn_RtfToPlainText(r.Full_Reference)    LIKE @SearchText + '%'
    ORDER BY    a.Author,
                r.Year_Vague_Date_Start,
                dbo.ufn_RtfToPlainText(r.Full_Reference)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_InternalReferences_Select_ForSearch') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_InternalReferences_Select_ForSearch'
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
            GRANT EXECUTE ON dbo.usp_InternalReferences_Select_ForSearch TO [R2k_AddOnly]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
        GRANT EXECUTE ON dbo.usp_InternalReferences_Select_ForSearch TO [R2k_Administrator]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
        GRANT EXECUTE ON dbo.usp_InternalReferences_Select_ForSearch TO [R2k_FullEdit]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
        GRANT EXECUTE ON dbo.usp_InternalReferences_Select_ForSearch TO [R2k_ReadOnly]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
        GRANT EXECUTE ON dbo.usp_InternalReferences_Select_ForSearch TO [R2k_RecordCardsOnly]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_InternalReferences_Select_ForSearch TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ListReportAndBlock_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ListReportAndBlock_Select]
GO

/*===========================================================================*\
  Description:	Returns information required from a List_Report and the 
		associated Report_Block record.

  Parameters:	@Key	List_Report key

  Created:	August 2003

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ListReportAndBlock_Select]
	@Key char(16)
AS

SET NOCOUNT ON

SELECT
	LR.Item_Name AS Report_Title,
	RB.Header_File,
	RB.Row_File,
	RB.Footer_File,
	LR.Population_SQL,
	RB.Report_Block_Key
FROM List_Report LR
INNER JOIN Report_Block RB ON RB.Report_Block_Key=LR.Report_Block_Key
WHERE LR.List_Report_Key=@Key

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ListReportAndBlock_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ListReportAndBlock_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ListReportAndBlock_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ListReportAndBlock_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ListReportAndBlock_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ListReportAndBlock_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ListReportAndBlock_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ListReportAndBlock_Select TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_LocationFeatureData_Delete') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_LocationFeatureData_Delete]
GO

/*===========================================================================*\
  Description:	Deletes a record in the Location Feature Data table

  Parameters:	@Key
		@Timestamp

  Created:	August 2004

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_LocationFeatureData_Delete]
	@Key char(16),
	@Timestamp timestamp
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION

		DELETE	Location_Feature_Data
		WHERE	Location_Feature_Data_Key = @Key
		AND	(@Timestamp = Timestamp)

		IF @@Error <> 0 GOTO RollbackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_LocationFeatureData_Delete') AND SysStat & 0xf = 4)
BEGIN
	PRINT 'Setting up security on procedure usp_LocationFeatureData_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_LocationFeatureData_Delete TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_LocationFeatureData_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_LocationFeatureData_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_LocationFeatureData_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
		GRANT EXECUTE ON dbo.usp_LocationFeatureData_Delete TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_LocationFeatureData_Insert') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_LocationFeatureData_Insert]
GO

/*===========================================================================*\
  Description: Inserts a record into Location_Feature_Data.

  Parameters:  Fields of Location_Feature_Data

  Created:     August 2004

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_LocationFeatureData_Insert]
	-- Required by both Measurements and Descriptors updates.
	@Key char(16) OUTPUT,
	@LocationFeatureKey char(16) = NULL,
       	@AppliesTo varchar(50),
       	@ParameterConceptKey char(16),
	@Value varchar(50),	-- Used for Descriptors and as Lower_Value for Measurements
	@IsDescriptor bit,
	@SessionID char(16),
	-- Only required for the Measurements update.
	@UpperValue varchar(50) = NULL,
	@MethodConceptKey char(16) = NULL,
	@Duration varchar(50) = NULL,
	@Accuracy varchar(50) = NULL,
	@UnitConceptKey char(16) = NULL
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION

		EXECUTE spNextKey 'Location_Feature_Data', @Key OUTPUT

		/*----------------------------------------------------------------------------------*\
		  If we are inserting measurement data, more information needs to inserted than if 
		  we are inserting descriptor data. Hence, there are two different insert statements.
		\*----------------------------------------------------------------------------------*/
		IF @IsDescriptor = 1
			INSERT INTO Location_Feature_Data (
				Location_Feature_Data_Key, Location_Feature_Key, Applies_To, Parameter_Concept_Key, 
				Lower_Value, Is_Descriptor, Entered_Session_ID
			) VALUES (
				@Key, @LocationFeatureKey, @AppliesTo, @ParameterConceptKey, IsNull(@Value, ' '), 
				1, @SessionID
			)
		ELSE
			INSERT INTO Location_Feature_Data (
				Location_Feature_Data_Key, Location_Feature_Key, Applies_To, Method_Concept_Key, Duration,
				Accuracy, Parameter_Concept_Key, Unit_Concept_Key, Lower_Value, Upper_Value,
				Is_Descriptor, Entered_Session_ID
			) VALUES (
				@Key, @LocationFeatureKey, @AppliesTo, @MethodConceptKey, @Duration,
				@Accuracy, @ParameterConceptKey, @UnitConceptKey, IsNull(@Value, ' '), @UpperValue,
				0, @SessionID
			)

		IF @@Error <> 0 GOTO RollbackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_LocationFeatureData_Insert') AND SysStat & 0xf = 4)
BEGIN
	PRINT 'Setting up security on procedure usp_LocationFeatureData_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_LocationFeatureData_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_LocationFeatureData_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_LocationFeatureData_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_LocationFeatureData_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
		GRANT EXECUTE ON dbo.usp_LocationFeatureData_Insert TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_LocationFeatureData_Select') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_LocationFeatureData_Select]
GO

/*===========================================================================*\
  Description:	Returns a measurement or descriptor record from
		Location_Feature_Data table.

  Parameters:	@Key		Occurrence Data key
		@IsDescriptor	Flag to indicate whether Measurements or Descriptors
				are requested.

  Created:	August 2004

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_LocationFeatureData_Select]
	@Key char(16),
	@IsDescriptor bit
AS

SET NOCOUNT ON

	-- Set of options for better use of vw_ConceptTerm.
	SET ANSI_NULLS ON
	SET ANSI_PADDING ON
	SET ANSI_WARNINGS ON
	SET CONCAT_NULL_YIELDS_NULL ON
	SET QUOTED_IDENTIFIER ON
	SET NO_BROWSETABLE OFF

	IF @IsDescriptor = 1
		-- Descriptor tab page wants to match on the master key, to get all records.
		SELECT 	  	LFD.Location_Feature_Data_Key AS Item_Key,
				LFD.Applies_To,
				LFD.Method_Concept_Key,
				CTM.Plaintext AS Method_Term,
				LFD.Duration,
				LFD.Accuracy,
				LFD.Parameter_Concept_Key,
				CTP.Plaintext AS Parameter_Term,
				LFD.Unit_Concept_Key,
				CTU.Plaintext AS Unit_Term,
				LFD.Lower_Value AS Value,
				LFD.Lower_Value, 
				LFD.Upper_Value,			
				LFD.Custodian,
				LFD.[Timestamp],
				S.Date_Time_Start
		FROM 		Location_Feature_Data LFD
		INNER JOIN 	vw_ConceptTerm CTP ON CTP.Concept_Key = LFD.Parameter_Concept_Key
		LEFT JOIN 	vw_ConceptTerm CTM ON CTM.Concept_Key = LFD.Method_Concept_Key
		LEFT JOIN 	vw_ConceptTerm CTU ON CTU.Concept_Key = LFD.Unit_Concept_Key
		LEFT JOIN 	Session S ON LFD.Entered_Session_ID = S.Session_ID
		WHERE 		LFD.Location_Feature_Key = @Key
		AND		LFD.Is_Descriptor = @IsDescriptor
	ELSE
		-- Measurements wants to match on detail key, to get a single record.
		SELECT 	  	LFD.Location_Feature_Data_Key AS Item_Key,
				LFD.Applies_To,
				LFD.Method_Concept_Key,
				CTM.Plaintext AS Method_Term,
				LFD.Duration,
				LFD.Accuracy,
				LFD.Parameter_Concept_Key,
				CTP.Plaintext AS Parameter_Term,
				LFD.Unit_Concept_Key,
				CTU.Plaintext AS Unit_Term,
				LFD.Lower_Value AS Value,
				LFD.Lower_Value, 
				LFD.Upper_Value,			
				LFD.Custodian,
				LFD.[Timestamp],
				S.Date_Time_Start
		FROM 		Location_Feature_Data LFD
		INNER JOIN 	vw_ConceptTerm CTP ON CTP.Concept_Key = LFD.Parameter_Concept_Key
		LEFT JOIN 	vw_ConceptTerm CTM ON CTM.Concept_Key = LFD.Method_Concept_Key
		LEFT JOIN 	vw_ConceptTerm CTU ON CTU.Concept_Key = LFD.Unit_Concept_Key
		LEFT JOIN 	Session S ON LFD.Entered_Session_ID = S.Session_ID
		WHERE 		LFD.Location_Feature_Data_Key = @Key
		AND		LFD.Is_Descriptor = @IsDescriptor

SET NOCOUNT OFF 
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_LocationFeatureData_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_LocationFeatureData_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_LocationFeatureData_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_LocationFeatureData_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_LocationFeatureData_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_LocationFeatureData_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_LocationFeatureData_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_LocationFeatureData_Select TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_LocationFeatureData_Update') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_LocationFeatureData_Update]
GO

/*===========================================================================*\
  Description:	Updates a record in the Location Feature Data table.
		The LocationFeature_Data table hold descriptor and measurement
		information.

  Parameters:  Fields of Location_Feature_Data

  Created:     August 2004

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_LocationFeatureData_Update]
	-- Required by both Measurements and Descriptors updates.
	@Key char(16),
	@LocationFeatureKey char(16) = NULL,
	@IsDescriptor bit,
	@ParameterConceptKey char(16),
	@AppliesTo varchar(50),
	@Value varchar(50),	-- Used for Descriptors and as Lower_Value for Measurements
	@SessionID char(16),
	@Timestamp timestamp,
	-- Only required for the Measurements update.
	@UpperValue varchar(50) = NULL,
	@MethodConceptKey char(16) = NULL,
	@Duration varchar(50) = NULL,
	@Accuracy varchar(50) = NULL,
	@UnitConceptKey char(16) = NULL
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	/*----------------------------------------------------------------------------------*\
	  If we are updating measurement data, more information needs to changed than if
	  we are inserting descriptor data. Hence, there are two different update statements.
	\*----------------------------------------------------------------------------------*/
	BEGIN TRANSACTION

		IF @IsDescriptor = 1	
			-- Updating a descriptor.
			UPDATE	Location_Feature_Data
			SET	Applies_To = @AppliesTo,
				Parameter_Concept_Key = @ParameterConceptKey,
				Lower_Value = IsNull(@Value, ' '),
				Is_Descriptor = 1,
				Changed_Session_ID = @SessionID
			WHERE	Location_Feature_Data_Key = @Key
			AND	(@Timestamp = Timestamp)
	
		ELSE		
			-- Updating a measurement.
			UPDATE	Location_Feature_Data
			SET	Applies_To = @AppliesTo,
				Parameter_Concept_Key = @ParameterConceptKey,
				Lower_Value = IsNull(@Value, ' '),
				Is_Descriptor = 0,
				Changed_Session_ID = @SessionID,
				Location_Feature_Key = @LocationFeatureKey,
				Method_Concept_Key = @MethodConceptKey,
				Duration = @Duration,
				Accuracy = @Accuracy,
				Unit_Concept_Key = @UnitConceptKey,
				Upper_Value = @UpperValue
			WHERE	Location_Feature_Data_Key = @Key
			AND	(@Timestamp = Timestamp)

		IF @@Error <> 0 GOTO RollbackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_LocationFeatureData_Update') AND SysStat & 0xf = 4)
BEGIN
	PRINT 'Setting up security on procedure usp_LocationFeatureData_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_LocationFeatureData_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_LocationFeatureData_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_LocationFeatureData_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_LocationFeatureData_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
		GRANT EXECUTE ON dbo.usp_LocationFeatureData_Update TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Macros_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Macros_Select]
GO

/*===========================================================================*\
  Description:	Returns the list of macros

  Created:	April 2004

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Macros_Select]

AS
	SELECT Number_Type, Macro_ID_Generation, Macro, ID_Macro, ID_Seed
	FROM Macro

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Macros_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Macros_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Macros_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Macros_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Macros_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Macros_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Macros_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Macros_Select TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Macro_Get]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Macro_Get]
GO

/*===========================================================================*\
  Description:	Returns the next available macro value of a particular type

  Parameters: @NumberType (Registration, Accession, etc)
		@Dept - department acronym to use in macro
		@Macro OUTPUT

  Created:	April 2004

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Macro_Get]
	@NumberType VARCHAR(20),
	@Dept VARCHAR(100),
	@Macro VARCHAR(200) OUTPUT

AS

DECLARE @MacroIDGeneration BIT
DECLARE @IDMacro VARCHAR(100)
DECLARE @LastID INT
DECLARE @IDSeed INT

SELECT 
		@Macro=Macro,
		@MacroIDGeneration = Macro_ID_Generation,
		@IDMacro = ID_Macro,
		@LastID = Last_Global_ID,
		@IDSeed = ID_Seed
FROM Macro
WHERE Number_Type = @NumberType

IF @Macro IS NULL 
	SET @Macro=''
ELSE
BEGIN

	SET @Macro = REPLACE(@Macro, '<#YEAR>', CONVERT(VARCHAR(4), YEAR(GETDATE())))
	SET @Macro = REPLACE(@Macro, '<#MONTH>', CONVERT(VARCHAR(4), MONTH(GETDATE())))
	SET @Macro = REPLACE(@Macro, '<#DAY>', CONVERT(VARCHAR(4), DAY(GETDATE())))
	SET @Macro = REPLACE(@Macro, '<#DEPT>', @Dept)
	
	IF (@MacroIDGeneration=1) AND (@IDMacro IS NOT NULL)
	BEGIN
	  -- ID Number is unique for the sub macro
		SET @IDMacro = REPLACE(@IDMacro, '<#YEAR>', CONVERT(VARCHAR(4), YEAR(GETDATE())))
		SET @IDMacro = REPLACE(@IDMacro, '<#MONTH>', CONVERT(VARCHAR(4), MONTH(GETDATE())))
		SET @IDMacro = REPLACE(@IDMacro, '<#DAY>', CONVERT(VARCHAR(4), DAY(GETDATE())))
		SET @IDMacro = REPLACE(@IDMacro, '<#DEPT>', @Dept)
	
		IF NOT EXISTS(SELECT 1 FROM Macro_Generated_ID WHERE Number_Type=@NumberType AND Macro_Output=@IDMacro)
		BEGIN
			INSERT INTO Macro_Generated_ID VALUES (@NumberType, @IDMacro, 1)
			SET @LastID=@IDSeed
		END
		ELSE BEGIN
			SELECT @LastID=Last_Macro_ID FROM Macro_Generated_ID WHERE Number_Type=@NumberType AND Macro_Output=@IDMacro
			WHILE 1=1
			BEGIN
				SET @LastID = @LastID + 1
			  UPDATE Macro_Generated_ID
					SET Last_Macro_ID=@LastID
				WHERE Number_Type=@NumberType
					AND Macro_Output=@IDMacro
					AND Last_Macro_ID=@LastID-1
				IF @@ROWCOUNT>0 
					BREAK
			END
		END		
	END
	ELSE BEGIN
	  -- ID Number is globally unique
	
		-- Increment the number in a guaranteed unique way
		WHILE 1=1
		BEGIN
			SET @LastID = @LastID + 1
		  UPDATE Macro
				SET Last_Global_ID=@LastID
			WHERE Number_Type=@NumberType
				AND Last_Global_ID=@LastID-1
			IF @@ROWCOUNT>0 
				BREAK
		END
	
	END
	
	-- Finally, update the IDs
	SET @Macro = REPLACE(@Macro, '<#ID>', CONVERT(VARCHAR(10), @LastID))
END

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Macro_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Macro_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Macro_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Macro_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Macro_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Macro_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Macro_Get TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Macro_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Macro_Update]
GO

/*===========================================================================*\
  Description:	Returns the list of macros

  Created:	April 2004

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Macro_Update]
	@NumberType VARCHAR(20),
	@Macro VARCHAR(200),
	@MacroIDGeneration BIT,
	@IDMacro VARCHAR(100),
	@IDSeed INT
AS
	UPDATE Macro
	SET Macro = @Macro,
		 Macro_ID_Generation = @MacroIDGeneration,
		 ID_Macro=@IDMacro,
		 ID_Seed=@IDSeed
	WHERE Number_Type=@NumberType

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Macro_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Macro_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Macro_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Macro_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Macro_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Macro_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Macro_Update TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_MemberOfHoldingOrg_Get]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_MemberOfHoldingOrg_Get]
GO

/*===========================================================================*\
  Description:	Takes a Name key and returns 1 if the person is a 
		member of the Holding Organisation and 0 if they aren't.
		If the Name key belongs to an Organisation, then 1 is also
		returned.

  Parameters:	@Key 	Individual_Key
		@IsMember bit (output)

  Created:	March 2004

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_MemberOfHoldingOrg_Get]
	@Key char(16),
	@IsMember bit OUTPUT 
AS
	DECLARE	@HoldingOrgKey char(16)

	/*-----------------------------------------*\
	  Get the Holding Organisation key.
	\*-----------------------------------------*/
	SELECT 	@HoldingOrgKey = Data
	FROM 	Setting
	WHERE	[Name] = 'HoldingOrg'


	/*--------------------------------------------------------------------------------------------*\
	  See if the Individual is a member of the Holding Organisation or actually is the HoldingOrg.
	\*--------------------------------------------------------------------------------------------*/
	SELECT 		@IsMember = CASE WHEN Count(*) = 0 THEN 0 ELSE 1 END
	FROM		Individual AS I
	LEFT JOIN	Name_Relation AS NR1 ON NR1.Name_Key_1 = I.Name_Key
						AND NR1.Name_Key_2 = @HoldingOrgKey
	LEFT JOIN	Name_Relation AS NR2 ON NR2.Name_Key_1 = @HoldingOrgKey
						AND NR2.Name_Key_2 = I.Name_Key
	INNER JOIN	Organisation AS O ON ((O.Name_Key = NR1.Name_Key_2) OR (O.Name_Key = NR2.Name_Key_1))
	WHERE		I.Name_Key = @Key
	OR		O.Name_Key = @Key   
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_MemberOfHoldingOrg_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_MemberOfHoldingOrg_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_MemberOfHoldingOrg_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_MemberOfHoldingOrg_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_MemberOfHoldingOrg_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_MemberOfHoldingOrg_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_MemberOfHoldingOrg_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_MemberOfHoldingOrg_Get TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_MovementDirectionInboundMaterialKey_Get]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_MovementDirectionInboundMaterialKey_Get]
GO
/*===========================================================================*\
  Description: Gets a MovementKey	
  Parameters:	

  Created:	September 2003

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_MovementDirectionInboundMaterialKey_Get]
	@Key char(16) OUTPUT,
	@MovementNumber varchar(30),
	@SessionID CHAR(16)
	
AS
	DECLARE @MovementKey CHAR(16)

	SELECT @Key = Movement_Direction_Key, @MovementKey=Movement.Movement_Key
	FROM Movement 
	INNER JOIN Movement_Direction 
	ON Movement.Movement_Key = Movement_Direction.Movement_Key 
	WHERE 
		Number = @MovementNumber
		AND Outbound = 0
		AND Movement.Movement_Type IN (0,1)

	-- If we are obtaining an accession number to use for a new specimen, 
  -- but it does not have a movement of material, then we must add one
	IF NOT (@Key IS NULL)
	BEGIN
		IF NOT EXISTS(SELECT 1 FROM Movement_Of_Material MOM WHERE Movement_Direction_Key=@Key)
		BEGIN
			DECLARE @MovementOfMaterialKey CHAR(16)
			EXECUTE spNextKey 'Movement_Of_Material', @MovementOfMaterialKey OUTPUT

			INSERT INTO Movement_Of_Material (
							Movement_Of_Material_Key, Movement_Direction_Key, Contact_Name_Key,
							Vague_Date_Start, Vague_Date_End, Vague_Date_Type, Entered_Session_ID, Receiver_Name_Key,
							Receiver_Organisation_Department_Key
						)
				SELECT @MovementOfMaterialKey, @Key, Contact_Name_Key, 
							Exp_Vague_Date_Start, Exp_Vague_Date_End, Exp_Vague_Date_Type, @SessionID, Staff_Responsible_Name_Key,
							I.Organisation_Department_Key
				FROM Movement 
				INNER JOIN Individual I ON I.Name_Key=Staff_Responsible_Name_Key
				WHERE Movement_Key=@MovementKey 
		END
	END
GO


/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_MovementDirectionInboundMaterialKey_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_MovementDirectionInboundMaterialKey_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_MovementDirectionInboundMaterialKey_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_MovementDirectionInboundMaterialKey_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_MovementDirectionInboundMaterialKey_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_MovementDirectionInboundMaterialKey_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_MovementDirectionInboundMaterialKey_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_MovementDirectionInboundMaterialKey_Get TO [Dev - JNCC SQL]
END

GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Movements_Select_ForTopLevel]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Movements_Select_ForTopLevel]
GO

CREATE PROCEDURE [dbo].[usp_Movements_Select_ForTopLevel] 
@UserDomainMask BIGINT,
@SessionID CHAR(16),
@Key CHAR(16) = NULL,
@MovementGroupType TINYINT,
@SortOrderIndex TINYINT

AS

--  DESCRIPTION
--  Returns Movements data to the top level of the CollectionsBrowser
--
--  PARAMETERS
--	NAME				DESCRIPTION
--	@Key 				Optional Key. When specified, only the single top level record is returned with that key
--	@UserDomainMask		User's Domain Mask restricting which records may be returned
--	@SessionID 			User's SessionID
--	@SortOrderIndex		Index determining Sort Order
--
--
--  AUTHOR:     		Ben Collier, Dorset Software
--  CREATED:    		2003-08-15
--
SET NOCOUNT ON

DECLARE @MovementTypeLow INT, @MovementTypeHigh INT

-- Create  a table to hold the items we are looking for
DECLARE @Search TABLE (ItemKey CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS PRIMARY KEY)

IF @MovementGroupType=0 BEGIN -- AccessionsAndExchanges
	SET @MovementTypeLow=0
	SET @MovementTypeHigh=1
END
ELSE IF @MovementGroupType=1 BEGIN -- Loans
	SET @MovementTypeLow=2
	SET @MovementTypeHigh=3
END	
ELSE IF @MovementGroupType=2 BEGIN -- Other movements
	SET @MovementTypeLow=4
	SET @MovementTypeHigh=9
END


IF @Key IS NOT NULL
		INSERT INTO @Search VALUES (@Key)
ELSE IF object_id('tempdb..#TempFilter') is not null
BEGIN
	-- This is for selecting Exchanges to show. It is only done for @MovementGroupType 0.
	IF @MovementGroupType = 0
	BEGIN
		-- Select all items
		INSERT INTO @Search 
			SELECT DISTINCT ItemKey 
			FROM #TempFilter 
			INNER JOIN 	Movement M ON M.Movement_Key=#TempFilter.ItemKey
			INNER JOIN 	MOVEMENT_DIRECTION MD1 ON M.Movement_Key = MD1.Movement_Key AND MD1.Outbound=1
			LEFT JOIN 	MOVEMENT_COLLECTION_UNIT MCU1 ON MD1.Movement_Direction_Key = MCU1.Movement_Direction_Key
			LEFT JOIN 	COLLECTION_UNIT CU1 ON MCU1.Collection_Unit_Key = CU1.Collection_Unit_Key
			INNER JOIN 	MOVEMENT_DIRECTION MD2 ON M.Movement_Key = MD2.Movement_Key AND MD2.Outbound=0
			LEFT JOIN 	MOVEMENT_COLLECTION_UNIT MCU2 ON MD2.Movement_Direction_Key = MCU2.Movement_Direction_Key
			LEFT JOIN 	COLLECTION_UNIT CU2 ON MCU2.Collection_Unit_Key = CU2.Collection_Unit_Key
			WHERE 		((CU1.Domain_Mask IS NULL AND CU2.Domain_Mask IS NULL) OR 
					(CU1.Domain_Mask & @UserDomainMask > 0) OR (CU1.Domain_Mask = 0) OR
					(CU2.Domain_Mask & @UserDomainMask > 0) OR (CU2.Domain_Mask = 0))
			AND		M.Movement_type = 1
	END 

	-- Use the temporary filter table to provide list of keys. All movements except Exchanges.
	INSERT INTO @Search 
		SELECT DISTINCT ItemKey 
		FROM #TempFilter 
		INNER JOIN Movement M ON M.Movement_Key=#TempFilter.ItemKey AND M.Movement_Type <> 1
		INNER JOIN MOVEMENT_DIRECTION MD ON M.Movement_Key = MD.Movement_Key 
			-- If Loan Out we are only interested in the outbound movement because 
			-- it is by following this Movement_Direction that we will get to the Domain_Mask
			-- that we need to compare with the @UserDomainMask.
			AND ((Movement_Type = 3 AND MD.Outbound = 1) 
				-- If it is a Loan In, only interested in inbound movement
				OR (Movement_Type = 2 AND MD.Outbound = 0) 
				-- Otherwise.
				OR NOT Movement_Type IN (1,2,3) )
		LEFT JOIN MOVEMENT_COLLECTION_UNIT MCU ON MD.Movement_Direction_Key = MCU.Movement_Direction_Key
		LEFT JOIN COLLECTION_UNIT CU ON MCU.Collection_Unit_Key = CU.Collection_Unit_Key
		WHERE (Movement_Type BETWEEN @MovementTypeLow AND @MovementTypeHigh)
			AND ((CU.Domain_Mask IS NULL) OR (CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Domain_Mask = 0)
			OR (M.Entered_Session_ID = @SessionID) OR (M.Changed_Session_ID = @SessionID))
END	
ELSE BEGIN
	-- This is for selecting Exchanges to show. It is only done for @MovementGroupType 0.
	IF @MovementGroupType = 0
	BEGIN
		-- Select all items
		INSERT INTO @Search 
			SELECT DISTINCT M.Movement_Key
			FROM 		Movement M
			INNER JOIN 	MOVEMENT_DIRECTION MD1 ON M.Movement_Key = MD1.Movement_Key AND MD1.Outbound=1
			LEFT JOIN 	MOVEMENT_COLLECTION_UNIT MCU1 ON MD1.Movement_Direction_Key = MCU1.Movement_Direction_Key
			LEFT JOIN 	COLLECTION_UNIT CU1 ON MCU1.Collection_Unit_Key = CU1.Collection_Unit_Key
			INNER JOIN 	MOVEMENT_DIRECTION MD2 ON M.Movement_Key = MD2.Movement_Key AND MD2.Outbound=0
			LEFT JOIN 	MOVEMENT_COLLECTION_UNIT MCU2 ON MD2.Movement_Direction_Key = MCU2.Movement_Direction_Key
			LEFT JOIN 	COLLECTION_UNIT CU2 ON MCU2.Collection_Unit_Key = CU2.Collection_Unit_Key
			WHERE 		((CU1.Domain_Mask IS NULL AND CU2.Domain_Mask IS NULL) OR 
					(CU1.Domain_Mask & @UserDomainMask > 0) OR (CU1.Domain_Mask = 0) OR
					(CU2.Domain_Mask & @UserDomainMask > 0) OR (CU2.Domain_Mask = 0))
			AND		M.Movement_type = 1
	END

	-- Select all items except Exchanges.
	INSERT INTO @Search 
		SELECT DISTINCT M.Movement_Key 
		FROM Movement M
		INNER JOIN MOVEMENT_DIRECTION MD ON M.Movement_Key = MD.Movement_Key 
			-- If Loan Out we are only interested in the outbound movement because 
			-- it is by following this Movement_Direction that we will get to the Domain_Mask
			-- that we need to compare with the @UserDomainMask.
			AND ((Movement_Type = 3 AND MD.Outbound = 1) 
				-- If it is a Loan In, only interested in inbound movement
				OR (Movement_Type = 2 AND MD.Outbound = 0) 
				-- Otherwise.
				OR NOT Movement_Type IN (1,2,3) )
		LEFT JOIN MOVEMENT_COLLECTION_UNIT MCU ON MD.Movement_Direction_Key = MCU.Movement_Direction_Key
		LEFT JOIN COLLECTION_UNIT CU ON MCU.Collection_Unit_Key = CU.Collection_Unit_Key
		WHERE (Movement_Type BETWEEN @MovementTypeLow AND @MovementTypeHigh) 
			AND ((CU.Domain_Mask IS NULL) OR (CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Domain_Mask = 0)
			OR (M.Entered_Session_ID = @SessionID) OR (M.Changed_Session_ID = @SessionID))
END

IF @SortOrderIndex = 0
BEGIN
	SELECT DISTINCT M.Movement_Key AS Item_Key, M.Movement_Type, Number, M.Display_Caption,
		 Exp_Vague_Date_Start, Exp_Vague_Date_End --DISTINCT removes duplicate records due to two direction types
	FROM MOVEMENT M
	INNER JOIN @Search S ON S.ItemKey=M.Movement_Key
	ORDER BY Exp_Vague_Date_Start DESC, Exp_Vague_Date_End DESC, M.Movement_Type, Number
END
ELSE IF @SortOrderIndex = 1
BEGIN
	SELECT DISTINCT M.Movement_Key AS Item_Key, M.Movement_Type, Number, M.Display_Caption,
		 Exp_Vague_Date_Start, Exp_Vague_Date_End --DISTINCT removes duplicate records due to two direction types
	FROM MOVEMENT M
	INNER JOIN @Search S ON S.ItemKey=M.Movement_Key
	ORDER BY Number
END
ELSE IF @SortOrderIndex = 2
BEGIN
	SELECT DISTINCT M.Movement_Key AS Item_Key, M.Movement_Type, Number, M.Display_Caption,
		 Exp_Vague_Date_Start, Exp_Vague_Date_End --DISTINCT removes duplicate records due to two direction types
	FROM MOVEMENT M
	INNER JOIN @Search S ON S.ItemKey=M.Movement_Key
	ORDER BY M.Movement_Type, M.Number
END

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Movements_Select_ForTopLevel') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Movements_Select_ForTopLevel'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Movements_Select_ForTopLevel TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Movements_Select_ForTopLevel TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Movements_Select_ForTopLevel TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Movements_Select_ForTopLevel TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Movements_Select_ForTopLevel TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Movements_Select_ForTopLevel TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Movement_Insert]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Movement_Insert]
GO

/*===========================================================================*\
  Description:	Inserts a record into the Movement table

  Parameters:	@Key
		@MovementType
		@OtherPartyNameKey
		@StaffResponsibleNameKey
		@ContactNameKey
		@VagueDateStart
		@VagueDateEnd
		@VagueDateType
		@Number
		@Notes
		@SessionID

  Created:	September 2003

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Movement_Insert]
	@Key char(16) OUTPUT,
	@MovementType tinyint,
	@WithAcquisition bit, 
	@OtherPartyNameKey char(16),
	@StaffResponsibleNameKey char(16),
	@ContactNameKey char(16),
	@VagueDateStart int, 
	@VagueDateEnd int, 
	@VagueDateType varchar(2),
	@Number varchar(30),
	@Notes text,
	@SessionID char(16) 
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	/*-------------------------------------------------------------*\
	  Get a new key.
	\*-------------------------------------------------------------*/
	EXECUTE spNextKey 'Movement', @Key OUTPUT

	DECLARE @MovementDirectionKey char(16),
		@MovementOfOwnershipKey char(16),
		@MovementOfMaterialKey char(16),
		@HoldingOrg char(16),
		@ReceiverOrganisationDepartmentKey char(16)

	SELECT @HoldingOrg = (SELECT Data FROM Setting WHERE Name = 'HoldingOrg')

	SELECT 	@ReceiverOrganisationDepartmentKey = Organisation_Department_Key
	FROM	Individual
	WHERE	Name_Key = IsNull(@OtherPartyNameKey, @StaffResponsibleNameKey)

	BEGIN TRANSACTION

		/*-------------------------------------------------------------*\
		  Insert in Movement table.
		\*-------------------------------------------------------------*/
		INSERT INTO Movement (
			Movement_Key, Movement_Type, Other_Party_Name_Key, 
			Staff_Responsible_Name_Key, Contact_Name_Key, Exp_Vague_Date_Start, 
			Exp_Vague_Date_End, Exp_Vague_Date_Type, Number, 
			Notes, Entered_Session_ID
			
		) VALUES (
			@Key, @MovementType, @OtherPartyNameKey, 
			@StaffResponsibleNameKey, @ContactNameKey, @VagueDateStart, 
			@VagueDateEnd, @VagueDateType, @Number, 
			@Notes, @SessionID
		)
		IF @@Error <> 0 GOTO RollbackAndExit

		/*----------------------------------------*\
		   Accession / Accession with Acquisition
		\*----------------------------------------*/
		IF (@MovementType = 0)
		BEGIN
			-- Insert a record into the Movement_Direction table
			EXECUTE spNextKey 'Movement_Direction', @MovementDirectionKey OUTPUT
			INSERT INTO Movement_Direction (
				Movement_Direction_Key, Movement_Key,
				Receiver_Name_Key, Outbound, Entered_Session_ID
			) VALUES (
				@MovementDirectionKey, @Key,
				@HoldingOrg, 0, @SessionID
			)	

			IF @@Error <> 0 GOTO RollbackAndExit

			EXECUTE spNextKey 'Movement_Of_Ownership', @MovementOfOwnershipKey OUTPUT		
			INSERT INTO Movement_Of_Ownership (
				Movement_Of_Ownership_Key, Movement_Direction_Key, Contact_Name_Key,
				Vague_Date_Start, Vague_Date_End, Vague_Date_Type, Notes,
				Entered_Session_ID
			) VALUES (
				@MovementOfOwnershipKey, @MovementDirectionKey, @ContactNameKey,
				@VagueDateStart, @VagueDateEnd, @VagueDateType, @Notes,
				@SessionID
			)
			IF @@Error <> 0 GOTO RollbackAndExit
			
			/*----------------------------------------------------------------*\
		 	   Insert record into the Movement_Of_Material table if it is an
			   Accession with Acquisition.
			\*----------------------------------------------------------------*/			
			IF (@WithAcquisition = 1)
			BEGIN
				EXECUTE spNextKey 'Movement_Of_Material', @MovementOfMaterialKey OUTPUT
				INSERT INTO Movement_Of_Material (
					Movement_Of_Material_Key, Movement_Direction_Key, Contact_Name_Key,
					Vague_Date_Start, Vague_Date_End, Vague_Date_Type, Entered_Session_ID, Receiver_Name_Key,
					Receiver_Organisation_Department_Key
				) VALUES (
					@MovementOfMaterialKey, @MovementDirectionKey, @ContactNameKey, 
					@VagueDateStart, @VagueDateEnd, @VagueDateType, @SessionID, @StaffResponsibleNameKey,
					@ReceiverOrganisationDepartmentKey
				)	
				IF @@Error <> 0 GOTO RollbackAndExit
			END
		END ELSE
		/*----------------*\
		  Exchange
		\*----------------*/
		IF @MovementType = 1
		BEGIN
			/*-------------------------------------------------------------*\
		 	   Inserting records into the tables for the inbound movement.
			\*-------------------------------------------------------------*/
			EXECUTE spNextKey 'Movement_Direction', @MovementDirectionKey OUTPUT
			INSERT INTO Movement_Direction (
				Movement_Direction_Key, Movement_Key,
				Receiver_Name_Key, Outbound, Entered_Session_ID
			) VALUES (
				@MovementDirectionKey, @Key,
				@HoldingOrg, 0, @SessionID
			)	
			IF @@Error <> 0 GOTO RollbackAndExit

			EXECUTE spNextKey 'Movement_Of_Ownership', @MovementOfOwnershipKey OUTPUT		
			INSERT INTO Movement_Of_Ownership (
				Movement_Of_Ownership_Key, Movement_Direction_Key, Contact_Name_Key,
				Vague_Date_Start, Vague_Date_End, Vague_Date_Type, Notes,
				Entered_Session_ID
			) VALUES (
				@MovementOfOwnershipKey, @MovementDirectionKey, @ContactNameKey,
				@VagueDateStart, @VagueDateEnd, @VagueDateType, @Notes,
				@SessionID
			)
			IF @@Error <> 0 GOTO RollbackAndExit

			EXECUTE spNextKey 'Movement_Of_Material', @MovementOfMaterialKey OUTPUT
			INSERT INTO Movement_Of_Material (
				Movement_Of_Material_Key, Movement_Direction_Key, Contact_Name_Key,
				Vague_Date_Start, Vague_Date_End, Vague_Date_Type, Entered_Session_ID, Receiver_Name_Key,
				Receiver_Organisation_Department_Key
			) VALUES (
				@MovementOfMaterialKey, @MovementDirectionKey, @ContactNameKey,
				@VagueDateStart, @VagueDateEnd, @VagueDateType, @SessionID, @StaffResponsibleNameKey,
				@ReceiverOrganisationDepartmentKey
			)	
			IF @@Error <> 0 GOTO RollbackAndExit

			/*-------------------------------------------------------------*\
		 	   Inserting records into the tables for the outbound movement.
			\*-------------------------------------------------------------*/
			EXECUTE spNextKey 'Movement_Direction', @MovementDirectionKey OUTPUT
			INSERT INTO Movement_Direction (
				Movement_Direction_Key, Movement_Key,
				Receiver_Name_Key, Outbound, Entered_Session_ID
			) VALUES (
				@MovementDirectionKey, @Key,
				@OtherPartyNameKey, 1, @SessionID
			)		
			IF @@Error <> 0 GOTO RollbackAndExit
			
			EXECUTE spNextKey 'Movement_Of_Ownership', @MovementOfOwnershipKey OUTPUT		
			INSERT INTO Movement_Of_Ownership (
				Movement_Of_Ownership_Key, Movement_Direction_Key, Contact_Name_Key,
				Vague_Date_Start, Vague_Date_End, Vague_Date_Type, Notes,
				Entered_Session_ID
			) VALUES (
				@MovementOfOwnershipKey, @MovementDirectionKey, @ContactNameKey,
				@VagueDateStart, @VagueDateEnd, @VagueDateType, @Notes,
				@SessionID
			)
			IF @@Error <> 0 GOTO RollbackAndExit

			EXECUTE spNextKey 'Movement_Of_Material', @MovementOfMaterialKey OUTPUT
			INSERT INTO Movement_Of_Material (
				Movement_Of_Material_Key, Movement_Direction_Key, Contact_Name_Key,
				Vague_Date_Start, Vague_Date_End, Vague_Date_Type, Entered_Session_ID, Receiver_Name_Key,
				Receiver_Organisation_Department_Key
			) VALUES (
				@MovementOfMaterialKey, @MovementDirectionKey, @ContactNameKey,
				@VagueDateStart, @VagueDateEnd, @VagueDateType, @SessionID, @OtherPartyNameKey,
				@ReceiverOrganisationDepartmentKey
			)	
			IF @@Error <> 0 GOTO RollbackAndExit
		END ELSE
		/*------------*\
		  Loan In 
		\*------------*/
		IF @MovementType = 2
		BEGIN
			/*-------------------------------------------------------------*\
		 	   Inserting records into the tables for the inbound movement.
			\*-------------------------------------------------------------*/
			EXECUTE spNextKey 'Movement_Direction', @MovementDirectionKey OUTPUT
			INSERT INTO Movement_Direction (
				Movement_Direction_Key, Movement_Key,
				Receiver_Name_Key, Outbound, Entered_Session_ID
			) VALUES (
				@MovementDirectionKey, @Key,
				@HoldingOrg, 0, @SessionID
			)	
			IF @@Error <> 0 GOTO RollbackAndExit

			EXECUTE spNextKey 'Movement_Of_Material', @MovementOfMaterialKey OUTPUT
			INSERT INTO Movement_Of_Material (
				Movement_Of_Material_Key, Movement_Direction_Key, Contact_Name_Key,
				Vague_Date_Start, Vague_Date_End, Vague_Date_Type, Entered_Session_ID, Receiver_Name_Key,
				Receiver_Organisation_Department_Key
			) VALUES (
				@MovementOfMaterialKey, @MovementDirectionKey, @ContactNameKey,
				@VagueDateStart, @VagueDateEnd, @VagueDateType, @SessionID, @StaffResponsibleNameKey,
				@ReceiverOrganisationDepartmentKey
			)	
			IF @@Error <> 0 GOTO RollbackAndExit

			/*----------------------------------------------------------------------*\
		 	  A 'loan in' movement means that the linked items are owned elsewhere.
			  This information is not currently stored anywhere, so create a 
			  Movement_Of_Ownership record so we know the items are owned elsewhere.
			\*----------------------------------------------------------------------*/
			EXECUTE spNextKey 'Movement_Of_Ownership', @MovementOfOwnershipKey OUTPUT		
			INSERT INTO Movement_Of_Ownership (
				Movement_Of_Ownership_Key, Movement_Direction_Key, Contact_Name_Key,
				Vague_Date_Start, Vague_Date_End, Vague_Date_Type, Notes,
				Entered_Session_ID
			) VALUES (
				@MovementOfOwnershipKey, @MovementDirectionKey, @ContactNameKey,
				@VagueDateStart, @VagueDateEnd, @VagueDateType, @Notes,
				@SessionID
			)
			IF @@Error <> 0 GOTO RollbackAndExit

			/*-------------------------------------------------------------*\
		 	   Inserting records into the tables for the outbound movement.
			\*-------------------------------------------------------------*/
			EXECUTE spNextKey 'Movement_Direction', @MovementDirectionKey OUTPUT
			INSERT INTO Movement_Direction (
				Movement_Direction_Key, Movement_Key,
				Receiver_Name_Key, Outbound, Entered_Session_ID
			) VALUES (
				@MovementDirectionKey, @Key,
				@OtherPartyNameKey, 1, @SessionID
			)	
			IF @@Error <> 0 GOTO RollbackAndExit	
			
			EXECUTE spNextKey 'Movement_Of_Material', @MovementOfMaterialKey OUTPUT
			INSERT INTO Movement_Of_Material (
				Movement_Of_Material_Key, Movement_Direction_Key, Contact_Name_Key,
				Vague_Date_Start, Vague_Date_End, Vague_Date_Type, Entered_Session_ID, Receiver_Name_Key,
				Receiver_Organisation_Department_Key
			) VALUES (
				@MovementOfMaterialKey, @MovementDirectionKey, @ContactNameKey,
				@VagueDateStart, @VagueDateEnd, @VagueDateType, @SessionID, @OtherPartyNameKey,
				@ReceiverOrganisationDepartmentKey
			)	


		END ELSE
		/*----------*\
		  Loan out
		\*----------*/
		IF @MovementType = 3
		BEGIN
			/*-------------------------------------------------------------*\
		 	   Inserting records into the tables for the outbound movement.
			\*-------------------------------------------------------------*/
			EXECUTE spNextKey 'Movement_Direction', @MovementDirectionKey OUTPUT
			INSERT INTO Movement_Direction (
				Movement_Direction_Key, Movement_Key,
				Receiver_Name_Key, Outbound, Entered_Session_ID
			) VALUES (
				@MovementDirectionKey, @Key,
				@HoldingOrg, 1, @SessionID
			)	
			IF @@Error <> 0 GOTO RollbackAndExit

			EXECUTE spNextKey 'Movement_Of_Material', @MovementOfMaterialKey OUTPUT
			INSERT INTO Movement_Of_Material (
				Movement_Of_Material_Key, Movement_Direction_Key, Contact_Name_Key,
				Vague_Date_Start, Vague_Date_End, Vague_Date_Type, Entered_Session_ID, Receiver_Name_Key,
				Receiver_Organisation_Department_Key
			) VALUES (
				@MovementOfMaterialKey, @MovementDirectionKey, @ContactNameKey,
				@VagueDateStart, @VagueDateEnd, @VagueDateType, @SessionID, @OtherPartyNameKey,
				@ReceiverOrganisationDepartmentKey
			)	
			IF @@Error <> 0 GOTO RollbackAndExit

			/*-------------------------------------------------------------*\
		 	   Inserting records into the tables for the inbound movement.
			\*-------------------------------------------------------------*/
			EXECUTE spNextKey 'Movement_Direction', @MovementDirectionKey OUTPUT
			INSERT INTO Movement_Direction (
				Movement_Direction_Key, Movement_Key,
				Receiver_Name_Key, Outbound, Entered_Session_ID
			) VALUES (
				@MovementDirectionKey, @Key,
				@OtherPartyNameKey, 0, @SessionID
			)	
			IF @@Error <> 0 GOTO RollbackAndExit	
			
			EXECUTE spNextKey 'Movement_Of_Material', @MovementOfMaterialKey OUTPUT
			INSERT INTO Movement_Of_Material (
				Movement_Of_Material_Key, Movement_Direction_Key, Contact_Name_Key,
				Vague_Date_Start, Vague_Date_End, Vague_Date_Type, Entered_Session_ID, Receiver_Name_Key,
				Receiver_Organisation_Department_Key
			) VALUES (
				@MovementOfMaterialKey, @MovementDirectionKey, @ContactNameKey,
				@VagueDateStart, @VagueDateEnd, @VagueDateType, @SessionID, @StaffResponsibleNameKey,
				@ReceiverOrganisationDepartmentKey
			)	
			IF @@Error <> 0 GOTO RollbackAndExit
		END ELSE
		/*-----------------*\
		  Destroyed / Lost
		\*-----------------*/
		IF @MovementType = 4 OR @MovementType = 7
		BEGIN
			EXECUTE spNextKey 'Movement_Direction', @MovementDirectionKey OUTPUT
			INSERT INTO Movement_Direction (
				Movement_Direction_Key, Movement_Key,
				Receiver_Name_Key, Outbound, Entered_Session_ID
			) VALUES (
				@MovementDirectionKey, @Key,
				NULL, 1, @SessionID
			)	
			IF @@Error <> 0 GOTO RollbackAndExit

			EXECUTE spNextKey 'Movement_Of_Material', @MovementOfMaterialKey OUTPUT
			INSERT INTO Movement_Of_Material (
				Movement_Of_Material_Key, Movement_Direction_Key, 
				Vague_Date_Start, Vague_Date_End, Vague_Date_Type, Entered_Session_ID,
				Receiver_Organisation_Department_Key 
			) VALUES (
				@MovementOfMaterialKey, @MovementDirectionKey, 
				@VagueDateStart, @VagueDateEnd, @VagueDateType, @SessionID,
				@ReceiverOrganisationDepartmentKey 
			)
			IF @@Error <> 0 GOTO RollbackAndExit
		END ELSE
		/*-----------*\
		  Disposed
		\*-----------*/
		IF @MovementType = 5
		BEGIN
			EXECUTE spNextKey 'Movement_Direction', @MovementDirectionKey OUTPUT
			INSERT INTO Movement_Direction (
				Movement_Direction_Key, Movement_Key,
				Receiver_Name_Key, Outbound, Entered_Session_ID
			) VALUES (
				@MovementDirectionKey, @Key,
				NULL, 1, @SessionID
			)	
			IF @@Error <> 0 GOTO RollbackAndExit

			EXECUTE spNextKey 'Movement_Of_Ownership', @MovementOfOwnershipKey OUTPUT		
			INSERT INTO Movement_Of_Ownership (
				Movement_Of_Ownership_Key, Movement_Direction_Key, Contact_Name_Key,
				Vague_Date_Start, Vague_Date_End, Vague_Date_Type, Notes,
				Entered_Session_ID
			) VALUES (
				@MovementOfOwnershipKey, @MovementDirectionKey, @ContactNameKey,
				@VagueDateStart, @VagueDateEnd, @VagueDateType, @Notes,
				@SessionID
			)
			IF @@Error <> 0 GOTO RollbackAndExit

			EXECUTE spNextKey 'Movement_Of_Material', @MovementOfMaterialKey OUTPUT
			INSERT INTO Movement_Of_Material (
				Movement_Of_Material_Key, Movement_Direction_Key, Contact_Name_Key,
				Vague_Date_Start, Vague_Date_End, Vague_Date_Type, Entered_Session_ID, Receiver_Name_Key,
				Receiver_Organisation_Department_Key
			) VALUES (
				@MovementOfMaterialKey, @MovementDirectionKey, @ContactNameKey,
				@VagueDateStart, @VagueDateEnd, @VagueDateType, @SessionID, @OtherPartyNameKey,
				@ReceiverOrganisationDepartmentKey
			)
			IF @@Error <> 0 GOTO RollbackAndExit
		END ELSE
		/*-------------------*\
		  Internal transfer
		\*-------------------*/
		IF @MovementType = 6
		BEGIN
			EXECUTE spNextKey 'Movement_Direction', @MovementDirectionKey OUTPUT
			INSERT INTO Movement_Direction (
				Movement_Direction_Key, Movement_Key,
				Receiver_Name_Key, Outbound, Entered_Session_ID
			) VALUES (
				@MovementDirectionKey, @Key,
				@HoldingOrg, 0, @SessionID
			)	
			IF @@Error <> 0 GOTO RollbackAndExit

			EXECUTE spNextKey 'Movement_Of_Material', @MovementOfMaterialKey OUTPUT
			INSERT INTO Movement_Of_Material (
				Movement_Of_Material_Key, Movement_Direction_Key, Contact_Name_Key,
				Vague_Date_Start, Vague_Date_End, Vague_Date_Type, Entered_Session_ID, Receiver_Name_Key,
				Receiver_Organisation_Department_Key
			) VALUES (
				@MovementOfMaterialKey, @MovementDirectionKey, @ContactNameKey,
				@VagueDateStart, @VagueDateEnd, @VagueDateType, @SessionID, @StaffResponsibleNameKey,
				@ReceiverOrganisationDepartmentKey	
			)
			IF @@Error <> 0 GOTO RollbackAndExit
		END ELSE
		/*-------*\
		  Sold
		\*-------*/
		IF @MovementType = 8
		BEGIN
			EXECUTE spNextKey 'Movement_Direction', @MovementDirectionKey OUTPUT
			INSERT INTO Movement_Direction (
				Movement_Direction_Key, Movement_Key,
				Receiver_Name_Key, Outbound, Entered_Session_ID
			) VALUES (
				@MovementDirectionKey, @Key,
				@OtherPartyNameKey, 1, @SessionID
			)	
			IF @@Error <> 0 GOTO RollbackAndExit

			EXECUTE spNextKey 'Movement_Of_Ownership', @MovementOfOwnershipKey OUTPUT		
			INSERT INTO Movement_Of_Ownership (
				Movement_Of_Ownership_Key, Movement_Direction_Key, Contact_Name_Key,
				Vague_Date_Start, Vague_Date_End, Vague_Date_Type, Notes,
				Entered_Session_ID
			) VALUES (
				@MovementOfOwnershipKey, @MovementDirectionKey, @ContactNameKey,
				@VagueDateStart, @VagueDateEnd, @VagueDateType, @Notes,
				@SessionID
			)
			IF @@Error <> 0 GOTO RollbackAndExit

			EXECUTE spNextKey 'Movement_Of_Material', @MovementOfMaterialKey OUTPUT
			INSERT INTO Movement_Of_Material (
				Movement_Of_Material_Key, Movement_Direction_Key, Contact_Name_Key,
				Vague_Date_Start, Vague_Date_End, Vague_Date_Type, Entered_Session_ID, Receiver_Name_Key,
				Receiver_Organisation_Department_Key
			) VALUES (
				@MovementOfMaterialKey, @MovementDirectionKey, @ContactNameKey,
				@VagueDateStart, @VagueDateEnd, @VagueDateType, @SessionID, @OtherPartyNameKey,
				@ReceiverOrganisationDepartmentKey
			)
			IF @@Error <> 0 GOTO RollbackAndExit
		END ELSE
		/*----------------*\
		  Hosted Material
		\*----------------*/
		IF @MovementType = 9
		BEGIN
			EXECUTE spNextKey 'Movement_Direction', @MovementDirectionKey OUTPUT
			INSERT INTO Movement_Direction (
				Movement_Direction_Key, Movement_Key,
				Receiver_Name_Key, Outbound, Entered_Session_ID
			) VALUES (
				@MovementDirectionKey, @Key,
				@OtherPartyNameKey, 1, @SessionID
			)	
			IF @@Error <> 0 GOTO RollbackAndExit

			EXECUTE spNextKey 'Movement_Of_Ownership', @MovementOfOwnershipKey OUTPUT		
			INSERT INTO Movement_Of_Ownership (
				Movement_Of_Ownership_Key, Movement_Direction_Key, Contact_Name_Key,
				Vague_Date_Start, Vague_Date_End, Vague_Date_Type, Notes,
				Entered_Session_ID
			) VALUES (
				@MovementOfOwnershipKey, @MovementDirectionKey, @ContactNameKey,
				@VagueDateStart, @VagueDateEnd, @VagueDateType, @Notes,
				@SessionID
			)
			IF @@Error <> 0 GOTO RollbackAndExit
		END

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Movement_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Movement_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Movement_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Movement_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Movement_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Movement_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Movement_Insert TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_NumberHistoryReadOnly_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_NumberHistoryReadOnly_Select]
GO

/*===========================================================================*\
  Description:	Returns fields required for the NumberHistoryReadOnly frame.

  Parameters:	@Key	Accession Key

  Created:	Setember 2003

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_NumberHistoryReadOnly_Select]
	@Key char(16)
AS

SET NOCOUNT ON

	SELECT		dbo.ufn_GetFormattedName(M.Other_Party_Name_Key) AS OtherPartyName,
			dbo.ufn_GetFormattedName(M.Staff_Responsible_Name_Key) AS StaffResponsible,
			M.Number,
			MOO.Vague_Date_Start,
			MOO.Vague_Date_End,
			MOO.Vague_Date_Type,
			OD.Item_Name
	FROM		Movement AS M

	INNER JOIN	Movement_Direction AS MD ON M.Movement_Key = MD.Movement_Key
	LEFT JOIN	Movement_Of_Ownership AS MOO ON MD.Movement_Direction_Key = MOO.Movement_Direction_Key
	LEFT JOIN	Movement_Of_Material AS MOM ON MD.Movement_Direction_key = MOM.Movement_Direction_Key
	LEFT JOIN	Organisation_Department AS OD ON MOM.Receiver_Organisation_Department_Key = OD.Organisation_Department_Key

	WHERE		M.Movement_Key = @Key AND MD.Outbound = 0
		
SET NOCOUNT OFF

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_NumberHistoryReadOnly_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_NumberHistoryReadOnly_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_NumberHistoryReadOnly_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_NumberHistoryReadOnly_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_NumberHistoryReadOnly_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_NumberHistoryReadOnly_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_NumberHistoryReadOnly_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_NumberHistoryReadOnly_Select TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_OccurrenceData_Insert') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_OccurrenceData_Insert]
GO

/*===========================================================================*\
  Description: Inserts a record into Occurrence_Data.

  Parameters:  Fields of Occurrence_Data

  Created:     September 2003

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_OccurrenceData_Insert]
	-- Required by both Measurements and Descriptors updates.
	@Key char(16) OUTPUT,
       	@AppliesTo varchar(50),
       	@ParameterConceptKey char(16),
	@Value varchar(50),	-- Used for Descriptors and as Lower_Value for Measurements
	@IsDescriptor bit,
	@SessionID char(16),
	-- Only required for the Measurements update.
	@UpperValue varchar(50) = NULL,
	@OccurrenceKey char(16) = NULL,
	@MethodConceptKey char(16) = NULL,
	@Duration varchar(50) = NULL,
	@Accuracy varchar(50) = NULL,
	@UnitConceptKey char(16) = NULL
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION

		EXECUTE spNextKey 'Occurrence_Data', @Key OUTPUT

		/*----------------------------------------------------------------------------------*\
		  If we are inserting measurement data, more information needs to inserted than if 
		  we are inserting descriptor data. Hence, there are two different insert statements.
		\*----------------------------------------------------------------------------------*/
		IF @IsDescriptor = 1
			INSERT INTO Occurrence_Data (
				Occurrence_Data_Key, Occurrence_Key, Applies_To, Parameter_Concept_Key, 
				Lower_Value, Is_Descriptor, Entered_Session_ID
			) VALUES (
				@Key, @OccurrenceKey, @AppliesTo, @ParameterConceptKey, IsNull(@Value, ' '), 
				1, @SessionID
			)
		ELSE
			INSERT INTO Occurrence_Data (
				Occurrence_Data_Key, Occurrence_Key, Applies_To, Method_Concept_Key, Duration,
				Accuracy, Parameter_Concept_Key, Unit_Concept_Key, Lower_Value, Upper_Value,
				Is_Descriptor, Entered_Session_ID
			) VALUES (
				@Key, @OccurrenceKey, @AppliesTo, @MethodConceptKey, @Duration,
				@Accuracy, @ParameterConceptKey, @UnitConceptKey, IsNull(@Value, ' '), @UpperValue,
				0, @SessionID
			)

		IF @@Error <> 0 GOTO RollbackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_OccurrenceData_Insert') AND SysStat & 0xf = 4)
BEGIN
	PRINT 'Setting up security on procedure usp_OccurrenceData_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_OccurrenceData_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_OccurrenceData_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_OccurrenceData_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_OccurrenceData_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
		GRANT EXECUTE ON dbo.usp_OccurrenceData_Insert TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_OccurrenceData_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_OccurrenceData_Select]
GO

/*===========================================================================*\
  Description:	Returns a measurement or descriptor record from
		Occurrence_Data table.

  Parameters:	@Key		Occurrence Data key
		@IsDescriptor	Flag to indicate whether Measurements or Descriptors
				are requested.

  Created:	October 2003

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_OccurrenceData_Select]
	@Key char(16),
	@IsDescriptor bit
AS

SET NOCOUNT ON

	-- Set of options for better use of vw_ConceptTerm.
	SET ANSI_NULLS ON
	SET ANSI_PADDING ON
	SET ANSI_WARNINGS ON
	SET CONCAT_NULL_YIELDS_NULL ON
	SET QUOTED_IDENTIFIER ON
	SET NO_BROWSETABLE OFF

	IF @IsDescriptor = 1
		-- Descriptor tab page wants to match on the master key, to get all records.
		SELECT 	  	OD.Occurrence_Data_Key AS Item_Key,
				OD.Applies_To,
				OD.Method_Concept_Key,
				CTM.Plaintext AS Method_Term,
				OD.Duration,
				OD.Accuracy,
				OD.Parameter_Concept_Key,
				CTP.Plaintext AS Parameter_Term,
				OD.Unit_Concept_Key,
				CTU.Plaintext AS Unit_Term,
				OD.Lower_Value AS Value,
				OD.Lower_Value, 
				OD.Upper_Value,			
				OD.Custodian,
				OD.[Timestamp],
				S.Date_Time_Start
		FROM 		Occurrence_Data OD
		INNER JOIN 	vw_ConceptTerm CTP ON CTP.Concept_Key = OD.Parameter_Concept_Key
		LEFT JOIN 	vw_ConceptTerm CTM ON CTM.Concept_Key = OD.Method_Concept_Key
		LEFT JOIN 	vw_ConceptTerm CTU ON CTU.Concept_Key = OD.Unit_Concept_Key
		LEFT JOIN 	Session S ON OD.Entered_Session_ID = S.Session_ID
		WHERE 		OD.Occurrence_Key = @Key
		AND		OD.Is_Descriptor = @IsDescriptor
	ELSE
		-- Measurements wants to match on detail key, to get a single record.
		SELECT 	  	OD.Occurrence_Data_Key AS Item_Key,
				OD.Applies_To,
				OD.Method_Concept_Key,
				CTM.Plaintext AS Method_Term,
				OD.Duration,
				OD.Accuracy,
				OD.Parameter_Concept_Key,
				CTP.Plaintext AS Parameter_Term,
				OD.Unit_Concept_Key,
				CTU.Plaintext AS Unit_Term,
				OD.Lower_Value AS Value,
				OD.Lower_Value, 
				OD.Upper_Value,			
				OD.Custodian,
				OD.[Timestamp],
				S.Date_Time_Start
		FROM 		Occurrence_Data OD
		INNER JOIN 	vw_ConceptTerm CTP ON CTP.Concept_Key = OD.Parameter_Concept_Key
		LEFT JOIN 	vw_ConceptTerm CTM ON CTM.Concept_Key = OD.Method_Concept_Key
		LEFT JOIN 	vw_ConceptTerm CTU ON CTU.Concept_Key = OD.Unit_Concept_Key
		LEFT JOIN 	Session S ON OD.Entered_Session_ID = S.Session_ID
		WHERE 		OD.Occurrence_Data_Key = @Key
		AND		OD.Is_Descriptor = @IsDescriptor

SET NOCOUNT OFF 
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_OccurrenceData_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_OccurrenceData_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_OccurrenceData_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_OccurrenceData_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_OccurrenceData_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_OccurrenceData_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_OccurrenceData_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_OccurrenceData_Select TO [Dev - JNCC SQL]
END

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_OccurrenceData_Update') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_OccurrenceData_Update]
GO

/*===========================================================================*\
  Description:	Updates a record in the Occurrence_Data table.
		The Occurrence_Data table hold descriptor and measurement
		information.

  Parameters:	@Key
		@CollectionUnitKey
		@AppliesTo
		@MethodConceptKey
		@Duration
		@Accuracy
		@ParameterConceptKey
		@UnitConceptKey
		@Value
		@UpperValue
		@IsDescriptor
		@SessionID
		@Timestamp

  Created:	November 2003

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_OccurrenceData_Update]
	-- Required by both Measurements and Descriptors updates.
	@Key char(16),
	@IsDescriptor bit,
	@ParameterConceptKey char(16),
	@AppliesTo varchar(50),
	@Value varchar(50),	-- Used for Descriptors and as Lower_Value for Measurements
	@SessionID char(16),
	@Timestamp timestamp,

	-- Only required for the Measurements update.
	@UpperValue varchar(50) = NULL,
	@OccurrenceKey char(16) = NULL,
	@MethodConceptKey char(16) = NULL,
	@Duration varchar(50) = NULL,
	@Accuracy varchar(50) = NULL,
	@UnitConceptKey char(16) = NULL

AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	/*----------------------------------------------------------------------------------*\
	  If we are updating measurement data, more information needs to changed than if
	  we are inserting descriptor data. Hence, there are two different update statements.
	\*----------------------------------------------------------------------------------*/
	BEGIN TRANSACTION

		IF @IsDescriptor = 1	
			-- Updating a descriptor.
			UPDATE	Occurrence_Data
			SET	Applies_To = @AppliesTo,
				Parameter_Concept_Key = @ParameterConceptKey,
				Lower_Value = IsNull(@Value, ' '),
				Is_Descriptor = @IsDescriptor,
				Changed_Session_ID = @SessionID
			WHERE	Occurrence_Data_Key = @Key
			AND	(@Timestamp = Timestamp)
	
		ELSE		
			-- Updating a measurement.
			UPDATE	Occurrence_Data
			SET	Applies_To = @AppliesTo,
				Parameter_Concept_Key = @ParameterConceptKey,
				Lower_Value = IsNull(@Value, ' '),
				Is_Descriptor = @IsDescriptor,
				Changed_Session_ID = @SessionID,
				Occurrence_Key = @OccurrenceKey,
				Method_Concept_Key = @MethodConceptKey,
				Duration = @Duration,
				Accuracy = @Accuracy,
				Unit_Concept_Key = @UnitConceptKey,
				Upper_Value = @UpperValue
			WHERE	Occurrence_Data_Key = @Key
			AND	(@Timestamp = Timestamp)

		IF @@Error <> 0 GOTO RollbackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_OccurrenceData_Update') AND SysStat & 0xf = 4)
BEGIN
	PRINT 'Setting up security on procedure usp_OccurrenceData_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_OccurrenceData_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_OccurrenceData_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_OccurrenceData_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_OccurrenceData_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
		GRANT EXECUTE ON dbo.usp_OccurrenceData_Update TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_OccurrenceRelations_Select_ForOccurrence]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_OccurrenceRelations_Select_ForOccurrence]
GO

/*===========================================================================*\
  Description:	Returns Occurrence Relation records for a given Occurrence.

  Parameters:	@Key	Occurrence Key

  Created:	November 2003

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_OccurrenceRelations_Select_ForOccurrence]
	@Key char(16)
AS
	
SET NOCOUNT ON

	SELECT 	  	R.Occurrence_Relation_Key AS Item_Key,
			R.To_Occurrence_Key AS Related_Occurrence_Key,
			CT.Item_Name AS Related_Occurrence_Name,
			TRT.Thesaurus_Relation_Type_Key,
			TRT.Item_Name AS Thesaurus_Relation_Type_Name,
			R.Comment,
			R.Custodian,
			R.[Timestamp]
	FROM		Occurrence_Relation R
	INNER JOIN	Thesaurus_Relation_Type TRT ON TRT.Thesaurus_Relation_Type_Key = R.Thesaurus_Relation_Type_Key
	LEFT JOIN	Determination D ON R.To_Occurrence_Key = D.Occurrence_Key AND D.Preferred = 1
	LEFT JOIN	vw_ConceptTermPreferred CT ON D.Concept_Key = CT.Concept_Key
	WHERE		R.From_Occurrence_Key = @Key

SET NOCOUNT OFF 
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_OccurrenceRelations_Select_ForOccurrence') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_OccurrenceRelations_Select_ForOccurrence'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_OccurrenceRelations_Select_ForOccurrence TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_OccurrenceRelations_Select_ForOccurrence TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_OccurrenceRelations_Select_ForOccurrence TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_OccurrenceRelations_Select_ForOccurrence TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_OccurrenceRelations_Select_ForOccurrence TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_OccurrenceRelations_Select_ForOccurrence TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_OccurrencesInEvent_Count_Get]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_OccurrencesInEvent_Count_Get]
GO

/*===========================================================================*\
  Description:	Returns the number of Occurrences / Taxon Occurrences
		associated with a Survey Event

  Parameters:	@Key  	Sample Key
		@Count OUTPUT

  Created:	August 2004

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_OccurrencesInEvent_Count_Get] 
@Key char(16),
@Count int OUTPUT

AS

SET NOCOUNT ON

	SELECT 		@Count = Count(*)
	FROM		Occurrence AS O
	INNER JOIN	[Sample] AS S ON S.Sample_Key = O.Sample_Key
	WHERE		S.Survey_Event_Key = @Key
	
	SELECT 		@Count = @Count + Count(*)
	FROM		Taxon_Occurrence AS XO
	INNER JOIN	[Sample] AS S ON S.Sample_Key = XO.Sample_Key
	WHERE		S.Survey_Event_Key = @Key

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_OccurrencesInEvent_Count_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_OccurrencesInEvent_Count_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_OccurrencesInEvent_Count_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_OccurrencesInEvent_Count_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_OccurrencesInEvent_Count_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_OccurrencesInEvent_Count_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_OccurrencesInEvent_Count_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_OccurrencesInEvent_Count_Get TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Occurrences_Select_ForSample]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Occurrences_Select_ForSample]
GO

/*===========================================================================*\
  Description:	Returns occurrence records for a specified sample key.

  Parameters:	@SampleKey

  Created:	October 2003

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Occurrences_Select_ForSample]
	@SampleKey char(16)
AS

SET NOCOUNT ON
	
	DECLARE @Occurrences TABLE (
		Item_Key char(16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
		Item_Name varchar(50) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		Checked bit,
		Confidential bit
	)

	INSERT INTO	@Occurrences (Item_Key, Checked, Confidential) 
	SELECT DISTINCT	Occurrence_Key, Checked, Confidential
	FROM		Occurrence
	WHERE		Sample_Key = @SampleKey

	UPDATE		@Occurrences
	SET		Item_Name =
				CASE	WHEN DO.Concept_Key IS NOT NULL THEN CTPO.Item_Name
					WHEN DSU.Concept_Key IS NOT NULL THEN CTPSU.Item_Name
				END
	FROM		@Occurrences O
	LEFT JOIN	Determination DO ON DO.Occurrence_Key = O.Item_Key AND DO.Preferred = 1
	LEFT JOIN	vw_ConceptTermPreferred CTPO ON CTPO.Concept_Key = DO.Concept_Key 

	LEFT JOIN	Specimen_Field_Data SFD ON SFD.Occurrence_Key = O.Item_Key
	LEFT JOIN	Specimen_Unit SU ON SU.Collection_Unit_Key = SFD.Collection_Unit_Key
	LEFT JOIN	Determination DSU ON DSU.Specimen_Collection_Unit_Key = SU.Collection_Unit_Key AND DSU.Preferred = 1
	LEFT JOIN	vw_ConceptTermPreferred CTPSU ON CTPSU.Concept_Key = DSU.Concept_Key 

	SELECT * FROM @Occurrences

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Occurrences_Select_ForSample') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Occurrences_Select_ForSample'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Occurrences_Select_ForSample TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Occurrences_Select_ForSample TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Occurrences_Select_ForSample TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Occurrences_Select_ForSample TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Occurrences_Select_ForSample TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Occurrences_Select_ForSample TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Occurrences_Select_ForSearch]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Occurrences_Select_ForSearch]
GO

/*===========================================================================*\
  Description:	Returns a list of Occurrences

  Parameters:	@SearchText

  Created:	November 2003

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Occurrences_Select_ForSearch]
	@SearchText varchar(150)
AS

SET NOCOUNT ON

	SELECT DISTINCT O.Occurrence_Key AS [Item_Key],
			CT.Item_Name + ' - ' +
			dbo.ufn_GetDateFromVagueDate(S.Vague_Date_Start, S.Vague_Date_End, S.Vague_Date_Type) +
			' - ' + 
			CASE 
				WHEN LN.Item_Name IS NULL THEN
					CASE WHEN S.Spatial_Ref IS NULL THEN '' ELSE S.Spatial_Ref END
				ELSE LN.Item_Name + ' (' + 
					CASE 
						WHEN S.Spatial_Ref IS NULL THEN L.Spatial_Ref
						ELSE S.Spatial_Ref END +
					')'
			END
			AS SearchTerm,
			CT.Item_Name + ' - ' +
			dbo.ufn_GetDateFromVagueDate(S.Vague_Date_Start, S.Vague_Date_End, S.Vague_Date_Type) +
			' - ' + 
			CASE 
				WHEN LN.Item_Name IS NULL THEN
					CASE WHEN S.Spatial_Ref IS NULL THEN '' ELSE S.Spatial_Ref END
				ELSE LN.Item_Name + ' (' + 
					CASE 
						WHEN S.Spatial_Ref IS NULL THEN L.Spatial_Ref
						ELSE S.Spatial_Ref END +
					')'
			END
			AS DisplayTerm
	FROM		Occurrence O
	INNER JOIN	Determination D ON O.Occurrence_Key = D.Occurrence_Key 
	INNER JOIN	vw_ConceptTermPreferred CT ON D.Concept_Key = CT.Concept_Key
	INNER JOIN 	Sample S ON S.Sample_Key = O.Sample_Key
	LEFT JOIN	Location L ON L.Location_Key = S.Location_Key 
	LEFT JOIN	Location_Name LN ON LN.Location_Key = L.Location_Key AND LN.Preferred = 1
	WHERE		CT.PlainText LIKE @SearchText + '%'
	ORDER BY	SearchTerm

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Occurrences_Select_ForSearch') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Occurrences_Select_ForSearch'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Occurrences_Select_ForSearch TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Occurrences_Select_ForSearch TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Occurrences_Select_ForSearch TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Occurrences_Select_ForSearch TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Occurrences_Select_ForSearch TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Occurrences_Select_ForSearch TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Occurrence_Update_ForSample') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_Occurrence_Update_ForSample]
GO

/*===========================================================================*\
  Description:	Updates the Sample Key of an occurrence.

  Parameters:	@Key
		@SampleKey

  Created:	August 2004

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Occurrence_Update_ForSample]
	@Key char(16),
	@SampleKey char(16)
AS

	BEGIN TRANSACTION

		UPDATE	Occurrence
		SET	Sample_Key = @SampleKey
		WHERE	Occurrence_Key = @Key

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Occurrence_Update_ForSample') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Occurrence_Update_ForSample'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Occurrence_Update_ForSample TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Occurrence_Update_ForSample TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Occurrence_Update_ForSample TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Occurrence_Update_ForSample TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Occurrence_Update_ForSample TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
       FROM   SysObjects
       WHERE  Id = Object_Id(N'[dbo].[usp_PotentialSynonyms_Select_ForImportedConcept]')
       AND    Type = 'P')
    DROP PROCEDURE [dbo].[usp_PotentialSynonyms_Select_ForImportedConcept]
GO

/*===========================================================================*\
  Description:  List-preferred potential synonyms of the specified concept.

  Parameters:   @concept_key            Concept key

  Created:      Jan 2004

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_PotentialSynonyms_Select_ForImportedConcept]
    @concept_key        CHAR(16)
AS
    SET NOCOUNT ON

    DECLARE     @src_meaning_key    CHAR(16),
                @src_group_key      CHAR(16)

    SELECT      @src_meaning_key    =   Meaning_Key,
                @src_group_key      =   Concept_Group_Key
    FROM        Concept
    WHERE       Concept_Key         =   @concept_key

    /* work out all current synonyms of the concept */
    DECLARE     @current_synonyms   TABLE (
        Language_Key        CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS,
        Plaintext           NVARCHAR(300) COLLATE SQL_Latin1_General_CP1_CI_AI
        PRIMARY KEY (Language_Key, Plaintext))

    INSERT      @current_synonyms (
                Language_Key,
                Plaintext)
    SELECT DISTINCT
                t.Language_Key,
                t.Plaintext
    FROM        Concept             AS  c
    INNER JOIN  Term                AS  t
    ON          t.Term_Key          =   c.Term_Key
    WHERE       c.Meaning_Key       =   @src_meaning_key

    IF @@ERROR <> 0 RETURN        

    /* work out all list-preferred potential synonyms */
    DECLARE     @potential  TABLE (
            Concept_Key         CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS PRIMARY KEY,
            Concept_Group_Key   CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS,
            Meaning_Key         CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS,
            Term_Key            CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS,
            Author_Copy         VARCHAR(100) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
            Different_Group     BIT,
            Session_Start       DATETIME)

    INSERT      @potential (
                Concept_Key,
                Concept_Group_Key,
                Meaning_Key,
                Term_Key,
                Author_Copy,
                Different_Group,
                Session_Start)
    SELECT DISTINCT
                psyn.Concept_Key,
                psyn.Concept_Group_Key,
                psyn.Meaning_Key,
                psyn.Term_Key,
                psyn.Author_Copy,
                CASE psyn.Concept_Group_Key
                    WHEN @src_group_key THEN 0
                    ELSE 1
                END,
                s.Date_Time_Start
    FROM        @current_synonyms   AS  curr
    INNER JOIN  Term                AS  tpot WITH (INDEX (IX_Plaintext))
    ON          tpot.Plaintext      =   curr.Plaintext
    AND         tpot.Language_Key   =   curr.Language_Key
    INNER JOIN  Concept             AS  pot
    ON          pot.Term_Key        =   tpot.Term_Key
    INNER JOIN  Concept             AS  psyn
    ON          psyn.Meaning_Key    =   pot.Meaning_Key
    AND         psyn.List_Preferred =   1
    INNER JOIN  Session             AS  s
    ON          s.Session_ID        =   psyn.Entered_Session_ID
    WHERE       pot.Meaning_Key     <>  @src_meaning_key  /* not currently a synonym */

    IF @@ERROR <> 0 RETURN

    /* select most recently entered list-preferred concept for each
     * potential synonym */
    SELECT      p.Concept_Key       AS  Item_Key,
                ISNULL(
                    t.Item_Name + ' ' + p.Author_Copy,
                    t.Item_Name)    AS  Item_Name,
                g.Item_Name         AS  Group_Name
    FROM        @potential          AS  p
    INNER JOIN  Term                AS  t
    ON          t.Term_Key          =   p.Term_Key
    INNER JOIN  Concept_Group       AS  g
    ON          g.Concept_Group_Key =   p.Concept_Group_Key
    WHERE       p.Session_Start     =   (   SELECT      TOP 1 Session_Start
                                            FROM        @potential      AS  p2
                                            WHERE       p2.Meaning_Key  =   p.Meaning_Key
                                            ORDER BY    p2.Different_Group,
                                                        p2.Session_Start DESC)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_PotentialSynonyms_Select_ForImportedConcept') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_PotentialSynonyms_Select_ForImportedConcept'
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
            GRANT EXECUTE ON dbo.usp_PotentialSynonyms_Select_ForImportedConcept TO [R2k_AddOnly]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
        GRANT EXECUTE ON dbo.usp_PotentialSynonyms_Select_ForImportedConcept TO [R2k_Administrator]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
        GRANT EXECUTE ON dbo.usp_PotentialSynonyms_Select_ForImportedConcept TO [R2k_FullEdit]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
        GRANT EXECUTE ON dbo.usp_PotentialSynonyms_Select_ForImportedConcept TO [R2k_ReadOnly]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
        GRANT EXECUTE ON dbo.usp_PotentialSynonyms_Select_ForImportedConcept TO [R2k_RecordCardsOnly]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_PotentialSynonyms_Select_ForImportedConcept TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_PreferredDeterminationName_Get_ForOccurrence') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_PreferredDeterminationName_Get_ForOccurrence]
GO

/*===========================================================================*\
  Description:	Returns the displayname of the preferred determination for 
		the given occurrence. 

  Parameters:	@OccurrenceKey
		@ItemName	OUTPUT

  Created:	October 2003

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_PreferredDeterminationName_Get_ForOccurrence]
	@OccurrenceKey char(16),
	@ItemName varchar(150) OUTPUT
AS

SET NOCOUNT ON

	SET @ItemName = NULL

	SELECT	@ItemName = CT.Item_Name
	FROM	Determination D
	JOIN	vw_ConceptTermPreferred CT ON D.Concept_Key = CT.Concept_Key
	WHERE	D.Occurrence_Key = @OccurrenceKey
	AND	D.Preferred = 1

	-- If it's not direct through Determination, go through Specimen_Field_Data instead
	IF @ItemName IS NULL
		SELECT	@ItemName = CT.Item_Name
		FROM	Specimen_Field_Data SFD 
		JOIN	Specimen_Unit SU ON SU.Collection_Unit_Key = SFD.Collection_Unit_Key
		JOIN	Determination DSU ON DSU.Specimen_Collection_Unit_Key = SU.Collection_Unit_Key AND DSU.Preferred = 1
		JOIN	vw_ConceptTermPreferred CT ON CT.Concept_Key = DSU.Concept_Key 
		WHERE	SFD.Occurrence_Key = @OccurrenceKey

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_PreferredDeterminationName_Get_ForOccurrence') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_PreferredDeterminationName_Get_ForOccurrence'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_PreferredDeterminationName_Get_ForOccurrence TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_PreferredDeterminationName_Get_ForOccurrence TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_PreferredDeterminationName_Get_ForOccurrence TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_PreferredDeterminationName_Get_ForOccurrence TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_PreferredDeterminationName_Get_ForOccurrence TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_PreferredDeterminationName_Get_ForOccurrence TO [Dev - JNCC SQL]
END

GO


If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Processes_Select_ForSearch]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Processes_Select_ForSearch]
GO

/*===========================================================================*\
  Description:	Searches for Processes from one concept group.

  Parameters:	@SearchText
		@SearchKey   Concept_Group_Key

  Created:	November 2003

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_Processes_Select_ForSearch] 
@SearchText varchar(100),
@SearchKey char(16)

AS
	SET NOCOUNT ON
	SET ANSI_NULLS ON
	SET ANSI_PADDING ON
	SET ANSI_WARNINGS ON
	SET ARITHABORT ON
	SET CONCAT_NULL_YIELDS_NULL ON
	SET QUOTED_IDENTIFIER ON
	SET NO_BROWSETABLE OFF

	SELECT 		Concept_Key AS Item_Key, 
			Item_Name AS DisplayTerm, 
			Item_Name AS SearchTerm
			
	FROM 		VW_ConceptTerm
	WHERE 		Concept_Group_Key = @SearchKey
	AND 		Item_Name LIKE @SearchText + '%'
	ORDER BY 	Item_Name
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Processes_Select_ForSearch') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Processes_Select_ForSearch'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Processes_Select_ForSearch TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Processes_Select_ForSearch TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Processes_Select_ForSearch TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Processes_Select_ForSearch TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Processes_Select_ForSearch TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Processes_Select_ForSearch TO [Dev - JNCC SQL]
END

GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_QEDataItem_Insert]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_QEDataItem_Insert]
GO
    
/*===========================================================================*\
  Description:	

  Parameters:	

  Created:	August 2003

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_QEDataItem_Insert]
  @QEDataRowKey as int,
  @QETemplateFieldKey as char(16),
  @DataValue as varchar(200),
  @DataDisplay as Varchar(200),
  @SessionID as char(16)
 AS

SET NOCOUNT ON

IF NOT EXISTS(SELECT 1 FROM QE_Data_Item 
		WHERE QE_Data_Row_Key=@QEDataRowKey
		AND QE_Template_Field_Key=@QETemplateFieldKey)
BEGIN
	INSERT INTO QE_Data_Item (QE_Data_Row_Key, QE_Template_Field_Key,
			Data_Value, Data_Display, Entered_Session_ID)
		VALUES(@QEDataRowKey, @QETemplateFieldKey, @DataValue, @DataDisplay, @SessionID)

	SELECT QE_Data_Item_Key, Timestamp 
	FROM QE_Data_Item 
	WHERE	QE_Data_Item_Key = Scope_Identity()
END
ELSE 
BEGIN
  -- Item already exists so just update it
	DECLARE @QEDataItemKey CHAR(16)

  SELECT @QEDataItemKey=QE_Data_Item_Key
	FROM QE_Data_Item
	WHERE QE_Data_Row_Key=@QEDataRowKey
		AND QE_Template_Field_Key=@QETemplateFieldKey
	
	UPDATE QE_Data_Item
		SET Data_Value = @DataValue,
		Data_Display = @DataDisplay,
		Changed_Session_ID = @SessionID
	WHERE	 QE_Data_Item_Key = @QEDataItemKey
	
	SELECT QE_Data_Item_Key, Timestamp 
	FROM QE_Data_Item 
	WHERE	QE_Data_Item_Key = @QEDataItemKey
END


go

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_QEDataItem_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_QEDataItem_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_QEDataItem_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_QEDataItem_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_QEDataItem_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_QEDataItem_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_QEDataItem_Insert TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_QETemplateField_Select_ForTemplate') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_QETemplateField_Select_ForTemplate]
GO

/*===========================================================================*\
  Description:	Selects the fields for a template

  Parameters:	@QETemplateKey - QE_Template_Key
		@TemplateType - see template type field description

  Created:	Jan 2004

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/    
CREATE PROCEDURE [dbo].[usp_QETemplateField_Select_ForTemplate]
	@QETemplateKey as char(16),
	@TemplateType as tinyint
AS

SET NOCOUNT ON

	SELECT  	F.Item_Name AS 'Field', 
			General_Tab,
		 	Specimen_Tab,
			TF.Item_Name AS 'Alternative_Name', 
			Default_Value, 
			F.Default_Size,
			TF.QE_Template_Field_Key, 
			TF.Timestamp,
			Data_Type, 
			F.QE_Field_Key,
			F.Field_Lookup_Key,
			Default_Display,
			Is_Measurement,
			NULL AS Measurement_Applies_To,			-- So we get columns named
			NULL AS Measurement_Method_Concept_Key,
			NULL AS Measurement_Duration,
			NULL AS Measurement_Accuracy,
			NULL AS Measurement_Parameter_Concept_Key,
			NULL AS Measurement_Unit_Concept_Key,
			NULL AS Measurement_Is_Specimen,
			Field_Name,
			Table_Name
		
	FROM  		QE_Field F 
	LEFT JOIN 	QE_Template_Field TF ON F.QE_Field_Key = TF.QE_Field_Key AND @QETemplateKey = TF.QE_Template_Key AND Is_Measurement = 0
	WHERE 		(Template_Type & @TemplateType) <> 0

	UNION -- Measurements now

	SELECT 	CT.Plaintext + ' (' + D.Item_Name + ')' COLLATE SQL_Latin1_General_CP1_CI_AS, 
		General_Tab,
		Specimen_Tab, 
		F.Item_Name, 
		Default_Value,
		20, 
		QE_Template_Field_Key, 
		F.Timestamp,
		0, 
		NULL,
		'',
		Default_Display,
	 	Is_Measurement,
		Measurement_Applies_To,
		Measurement_Method_Concept_Key,
		Measurement_Duration,
		Measurement_Accuracy,
		Measurement_Parameter_Concept_Key,
		Measurement_Unit_Concept_Key,
		Measurement_Is_Specimen,
		NULL,
		NULL
	FROM	QE_Template_Field F
	JOIN	vw_ConceptTerm CT ON Concept_Key = F.Measurement_Parameter_Concept_Key
	JOIN 	Concept_Group CG ON CT.Concept_Group_Key = CG.Concept_Group_Key
	JOIN 	Local_Domain LD ON LD.Local_Domain_Key = CG.Local_Domain_Key
	JOIN 	Domain D ON D.Domain_Key = LD.Domain_Key AND (D.Has_Occurrences = 1 OR D.Domain_Key = 'SYSTEM00000000')
	WHERE 	Is_Measurement = 1 AND QE_Template_Key = @QETemplateKey

	ORDER BY F.Table_Name
GO 

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_QETemplateField_Select_ForTemplate') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_QETemplateField_Select_ForTemplate'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_QETemplateField_Select_ForTemplate TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_QETemplateField_Select_ForTemplate TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_QETemplateField_Select_ForTemplate TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_QETemplateField_Select_ForTemplate TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_QETemplateField_Select_ForTemplate TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_QETemplateField_Select_ForTemplate TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_References_Delete_ForSources]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_References_Delete_ForSources]
GO

/*===========================================================================*\
  Description:	Deletes all records from the Source, Source_File and Source_Join
		tables given the Table_Name and Record_Key of the master record.

  Parameters:	@TableName
		@RecordKey

  Created:	December 2003

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_References_Delete_ForSources]
	@TableName varchar(50),
	@RecordKey char(16)
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION
		/*-------------------------------------------------------------*\
		  Use temp table to store the keys of record to delete. Mind the 
		  collation!
		\*-------------------------------------------------------------*/
		DECLARE @Keys TABLE (
			Source_Join_Key char(16) COLLATE SQL_Latin1_General_CP1_CI_AS, 
			Source_Key char(16) COLLATE SQL_Latin1_General_CP1_CI_AS
		)

		INSERT @Keys
			SELECT 	Source_Join_Key, Source_Key
			FROM	Source_Join
			WHERE	Table_Name = @TableName
			AND	Record_Key = @RecordKey

		/*-------------------------------------------------------------*\
		  Delete from source tables. Order is important because of
		  referential integrity.
		\*-------------------------------------------------------------*/
		-- Source_Join first, external AND internal references
		DELETE 	Source_Join
		WHERE	Source_Join_Key IN (SELECT Source_Join_Key FROM @Keys)

		IF @@Error <> 0 GOTO RollbackAndExit

		-- Source_File, for external references
		DELETE	Source_File
		WHERE	Source_Key IN (SELECT Source_Key FROM @Keys)

		IF @@Error <> 0 GOTO RollbackAndExit

		-- Source table last
		DELETE	Source
		WHERE	Source_Key IN (SELECT Source_Key FROM @Keys)
		AND	Internal = 0

		IF @@Error <> 0 GOTO RollbackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_References_Delete_ForSources') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_References_Delete_ForSources'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_References_Delete_ForSources TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_References_Delete_ForSources TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_References_Delete_ForSources TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_References_Delete_ForSources TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ReportBlocks_Select_ForSection]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ReportBlocks_Select_ForSection]
GO

/*===========================================================================*\
  Description:	Returns a list of the report block in a details report section.

  Parameters:	@Key - report section key

  Created:	Aug 2004

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ReportBlocks_Select_ForSection]
@Key CHAR(16)
AS

SET NOCOUNT ON

SELECT 
		RB.Report_Block_Key,
		RB.Title,
		RB.Header_File,
		RB.Row_File,
		RB.Footer_File,
		RBIS.Population_SQL,
		RBIS.Population_SQL_Record_Count
FROM Report_Block_In_Section RBIS
INNER JOIN Report_Block RB
		ON RB.Report_Block_Key=RBIS.Report_Block_Key
WHERE RBIS.Report_Section_Key=@Key
ORDER BY RBIS.[Sequence]

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ReportBlocks_Select_ForSection') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ReportBlocks_Select_ForSection'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ReportBlocks_Select_ForSection TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ReportBlocks_Select_ForSection TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ReportBlocks_Select_ForSection TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ReportBlocks_Select_ForSection TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ReportBlocks_Select_ForSection TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ReportBlocks_Select_ForSection TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ReportSections_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ReportSections_Select]
GO

/*===========================================================================*\
  Description:	Returns a list of the report sections in a details report,
			and the SQL used to populate the section repeats.

  Parameters:	@Key - details report key

  Created:	Aug 2004

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ReportSections_Select]
@Key CHAR(16)
AS

SET NOCOUNT ON

SELECT Report_Section_Key, Section_List_SQL, Item_Name_Macro
FROM Report_Section
WHERE Details_Report_Key=@Key
ORDER BY [Sequence]

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ReportSections_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ReportSections_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ReportSections_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ReportSections_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ReportSections_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ReportSections_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ReportSections_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ReportSections_Select TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SampleKey_Get') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_SampleKey_Get]
GO
/*===========================================================================*\
  Description:	
  Parameters:	

  Created:	July 2003

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SampleKey_Get]
	@Key char(16) OUTPUT,
	@VagueDateStart int,
	@VagueDateEnd int,
	@VagueDateType varchar(2),
	@SpatialRef varchar(40),
	@SpatialRefSystem varchar(4),
	@Lat float,
	@Long float,
	@SpatialRefQualifier varchar(20),
	@SampleTypeKey char(16),
	@LocationKey char(16),
	@SurveyKey char(16),
	@LocationName varchar(100)
AS
	/*----------------------------------------------------------------------------*\
	  Match exact list of Field Collectors if #TempFieldCollectors has been 
	  created by Collections Browser.
	\*----------------------------------------------------------------------------*/
	IF object_id('tempdb..#TempFieldCollectors') IS NOT NULL
	BEGIN
		/*-----------------------------------------------------------------*\
		  Create a table variable to store the Survey_Event match keys.
		\*-----------------------------------------------------------------*/
		DECLARE @tableSurveyEventMatches TABLE (
			Survey_Event_Key char(16) COLLATE SQL_Latin1_General_CP1_CI_AS
		)
	
		DECLARE @CurrentFieldCollector char(16)
	
		/*------------------------------------------------------------------*\
		  Create the cursor.
		\*------------------------------------------------------------------*/
		DECLARE curFieldCollectors CURSOR LOCAL FAST_FORWARD FOR
			SELECT 	Name_Key
			FROM	#TempFieldCollectors
	
		OPEN	curFieldCollectors

		/*-------------------------------------------------------*\
		  Give @CurrentFieldCollector its first value.
		\*-------------------------------------------------------*/	
		FETCH NEXT
		FROM	curFieldCollectors
		INTO	@CurrentFieldCollector

		/*-------------------------------------------------------*\
		  Start looping through the field collectors.
		\*-------------------------------------------------------*/		
		WHILE @@Fetch_Status = 0
		BEGIN
			/*------------------------------------------------------------------------*\
			  If the there are no records in @tableSurveyEventMatches, insert all of
			  the Survey_Event records for the first Field Collector in the table.
			\*------------------------------------------------------------------------*/
			IF NOT EXISTS (SELECT * FROM @tableSurveyEventMatches)
			BEGIN
				INSERT INTO @tableSurveyEventMatches (
					Survey_Event_Key
				) 
				SELECT 	Survey_Event_Key
				FROM	Survey_Event_Recorder
				WHERE	Name_Key = @CurrentFieldCollector
			END
			ELSE
			/*------------------------------------------------------------------------*\
			  As there are records in @tableSurveyEventMatches, we can now start
			  removing records that aren't matched.
			\*------------------------------------------------------------------------*/ 
			BEGIN
				-- Delete non matches
				DELETE		@tableSurveyEventMatches
				FROM		@tableSurveyEventMatches AS SEM
				LEFT JOIN	Survey_Event_Recorder AS SER ON SER.Survey_Event_Key = SEM.Survey_Event_Key
									AND SER.Name_Key = @CurrentFieldCollector
				WHERE		SER.Survey_Event_Key IS NULL
			END

			/*---------------------------------------------------------------------*\
			  Get next field collector and put value into @CurrentFieldCollector.
			\*---------------------------------------------------------------------*/		
			FETCH NEXT
			FROM	curFieldCollectors
			INTO	@CurrentFieldCollector
		END
	
		CLOSE curFieldCollectors
		DEALLOCATE curFieldCollectors

		/*---------------------------------------------------------------------*\
		  Now match on Spatial Ref, Sample Type, Date and Sample Recorders.
		\*---------------------------------------------------------------------*/	
		SELECT 	@Key = Sample_Key
		FROM 	[Sample] AS S
		JOIN 	Survey_Event AS SE ON SE.Survey_Event_Key = S.Survey_Event_Key
		JOIN 	@tableSurveyEventMatches AS SEM ON SEM.Survey_Event_Key = S.Survey_Event_Key
		WHERE	SE.Survey_Key = @SurveyKey
		AND	S.Sample_Type_Key = @SampleTypeKey
		AND	((S.Vague_Date_Start = @VagueDateStart AND S.Vague_Date_End = @VagueDateEnd AND S.Vague_Date_Type = @VagueDateType)
			OR 
			(S.Vague_Date_Type = 'U' AND (@VagueDateType='U' OR @VagueDateType IS NULL)))
		AND	IsNull(S.Location_Key, '') = IsNull(@LocationKey, '')
		AND	IsNull(S.Spatial_Ref, '') = IsNull(@SpatialRef, '')
		AND	IsNull(S.Spatial_Ref_System, '') = IsNull(@SpatialRefSystem, '')
		AND	IsNull(S.Location_Name, '') = IsNull(@LocationName, '')
	END
	ELSE
		/*---------------------------------------------------------------------*\
		  This matches on Spatial Ref, Sample Type and Date. I left this here  
		  in case it is required somewhere else and #TempFieldCollectors hasn't 
		  been created by the application.
		\*---------------------------------------------------------------------*/	
		SELECT 	@Key = Sample_Key
		FROM 	[Sample] AS S
		JOIN 	Survey_Event AS SE ON SE.Survey_Event_Key = S.Survey_Event_Key
		WHERE	SE.Survey_Key = @SurveyKey
		AND	S.Sample_Type_Key = @SampleTypeKey
		AND	((S.Vague_Date_Start = @VagueDateStart AND S.Vague_Date_End = @VagueDateEnd AND S.Vague_Date_Type = @VagueDateType)
			OR 
			(S.Vague_Date_Type = 'U' AND (@VagueDateType='U' OR @VagueDateType IS NULL)))
		AND	IsNull(S.Location_Key, '') = IsNull(@LocationKey, '')
		AND	IsNull(S.Spatial_Ref, '') = IsNull(@SpatialRef, '')
		AND	IsNull(S.Spatial_Ref_System, '') = IsNull(@SpatialRefSystem, '')
		AND	IsNull(S.Location_Name, '') = IsNull(@LocationName, '')
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SampleKey_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SampleKey_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SampleKey_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SampleKey_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SampleKey_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_SampleKey_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SampleKey_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SampleKey_Get TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_SampleRecorder_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_SampleRecorder_Update]
GO
/*===========================================================================*\
  Description:	Update a record in the Sample Recorder join table.
  Parameters:	@SampleKey char(16),
		@SERecorderKey char(16)

  Created:	August 2004

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SampleRecorder_Update]
	@SampleKey char(16),
	@SERecorderKey char(16)
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION

		UPDATE 	Sample_Recorder
		SET	Sample_Key = @SampleKey
		WHERE	SE_Recorder_Key = @SERecorderKey

		IF @@Error <> 0 GOTO RollBackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit:
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SampleRecorder_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SampleRecorder_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SampleRecorder_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SampleRecorder_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SampleRecorder_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SampleRecorder_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SampleRecorder_Update TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Sample_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Sample_Update]
GO

/*===========================================================================*\
  Description:	Updates a record into the Sample table

  Parameters:	@Key
		@ConservationCheckKey 
		@ConservationJobKey
		@SetVagueDateStart
		@SetVagueDateEnd 
		@SetVagueDateType
		@Status
		@TypeConceptKey
		@Priority
		@Duration
		@DurationUnitConceptKey 
		@IdentifierNameKey
		@TaskAction
		@Comment
		@SessionID
		@Timestamp 

  Created:	August 2004

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Sample_Update]
	@Key char(16),
	@VagueDateStart int,
	@VagueDateEnd int,
	@VagueDateType varchar(2) = NULL,
	@SpatialRef varchar(40),
	@SpatialRefSystem varchar(4),
	@Lat float,
	@Long float,
	@SpatialRefQualifier varchar(20),
	@SampleTypeKey char(16) = NULL,
	@LocationKey char(16),
	@SurveyEventKey char(16),
	@Comment text,
	@ChangedBy char(16),
	@LocationName varchar(100)
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION
		
		UPDATE 	[Sample]
		SET 	Vague_Date_Start = @VagueDateStart, 
			Vague_Date_End = @VagueDateEnd, 
			Vague_Date_Type = @VagueDateType,
			Spatial_Ref = @SpatialRef, 
			Spatial_Ref_System = @SpatialRefSystem, 
			Spatial_Ref_Qualifier = @SpatialRefQualifier,
			Lat = @Lat, 
			Long = @Long, 
			Location_Key = @LocationKey,
			Sample_Type_Key = @SampleTypeKey,
			Survey_Event_Key= @SurveyEventKey,
			Comment = @Comment,
			Location_Name = @LocationName,
			Changed_By = @ChangedBy,
			Changed_Date = GetDate()
		WHERE	Sample_Key = @Key

		IF @@Error <> 0 GOTO RollbackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Sample_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Sample_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Sample_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Sample_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Sample_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Sample_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Sample_Update TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Session_Close]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Session_Close]
GO

/*===========================================================================*\
  Description: Records the time that a session was closed

  Parameters:	@SessionID

  Created:	Nov 2003

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Session_Close]
	@SessionID char(16)
AS

UPDATE Session SET Date_Time_End=GetDate() WHERE Session_ID=@SessionID

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Session_Close') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Session_Close'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Session_Close TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Session_Close TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Session_Close TO [R2k_FullEdit]
	-- ReadOnly users have permission to run this stored proc because they need to shut
	-- the Session for their SessionID.
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Session_Close TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Session_Close TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Session_Close TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_Session_ForDate]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_Session_ForDate]
GO

/*===========================================================================*\
  Description:	Obtain a session identifier for specified user and date.

				Used when converting ENTERED_BY, ENTRY_DATE etc. from
				dictionaries to avoid generating many duplicate sessions.

  Parameters:	@UserKey				User key
				@Date					Date
				@SessionID				Session identifier (on exit)

  Created:		Nov 2003

  Last revision information:
	$Revision: 7 $
	$Date: 6/02/09 10:41 $
	$Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Session_ForDate]
	@UserKey			CHAR(16),
	@Date				DATETIME,
	@SessionID			CHAR(16)	OUTPUT
AS
	SET NOCOUNT ON

	/* remove any time component */
	SELECT		@Date				=	CONVERT(VARCHAR, @Date, 113)

	SELECT		@SessionID			=	Session_ID
	FROM		Session
	WHERE		User_Name_Key		=	@UserKey
	AND			Date_Time_Start		=	@Date
	AND			Date_Time_End		=	@Date

	IF @@ROWCOUNT = 0
	BEGIN
		EXECUTE		spNextKey	'Session',
								@SessionID	OUTPUT
		IF @@ERROR <> 0 GOTO fail

		INSERT		Session
					(Session_ID,
					User_Name_Key,
					Date_Time_Start,
					Date_Time_End)
		VALUES		(@SessionID,
					@UserKey,
					@Date,
					@Date)

		IF @@ERROR <> 0 GOTO fail
	END

	RETURN

fail:
	RAISERROR ('usp_Session_ForDate failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Session_ForDate') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Session_ForDate'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Session_ForDate TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Session_ForDate TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Session_ForDate TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_SourceJoin_ImportTaxonSources]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_SourceJoin_ImportTaxonSources]
GO

/*===========================================================================*\
  Description:	Import term/source relationships corresponding to the
  				taxon/source relationships in a taxon list.

  Parameters:   @job_id					Job identifier

  Created:		Jan 2004

  Last revision information:
	$Revision: 7 $
	$Date: 6/02/09 10:41 $
	$Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SourceJoin_ImportTaxonSources]
	@job_id				INT
AS
	SET NOCOUNT ON

	DECLARE		@concept_group_key		CHAR(16),
				@source_link_key		CHAR(16),
				@term_key				CHAR(16),
				@source_key				CHAR(16),
				@original				BIT,
				@system_supplied_data	BIT,
				@source_join_key		CHAR(16)

	/* determine parameters of job */
	SELECT		@concept_group_key		=	Concept_Group_Key
	FROM		Import_Export_Job
	WHERE		Import_Export_Job_ID	=	@job_id

	IF @@ROWCOUNT = 0
	BEGIN
		RAISERROR ('Job does not exist or has not been configured', 16, 1)
		RETURN
	END

	EXECUTE		usp_Import_Export_Job_UpdateStatus	@job_id,
													'Importing term/source relationships'
	IF @@ERROR <> 0 RETURN

	DECLARE		sources		CURSOR LOCAL FAST_FORWARD FOR
	SELECT		ts.SOURCE_LINK_KEY,
				tm.Term_Key,
				ts.SOURCE_KEY,
				ts.ORIGINAL,
				tx.System_Supplied_Data
	FROM		Concept							AS	c
	INNER JOIN	Taxon_Dictionary_Term_Mapping	AS	tm
	ON			tm.Term_Key						=	c.Term_Key
	INNER JOIN	TAXON_SOURCES					AS	ts
	ON			ts.TAXON_KEY					=	tm.Taxon_Key
	INNER JOIN	TAXON							AS	tx
	ON			tx.TAXON_KEY					=	ts.TAXON_KEY
	WHERE		c.Concept_Group_Key				=	@concept_group_key

	OPEN		sources

	WHILE 1 = 1
	BEGIN
		FETCH		sources
		INTO		@source_link_key,
					@term_key,
					@source_key,
					@original,
					@system_supplied_data

		IF @@FETCH_STATUS <> 0 BREAK

		BEGIN TRANSACTION

		SELECT		@source_join_key						=	NULL

		SELECT		@source_join_key						=	Source_Join_Key
		FROM		Taxon_Dictionary_Term_Sources_Mapping
		WHERE		Source_Link_Key							=	@source_link_key

		IF @@ROWCOUNT = 0
		BEGIN
			SELECT		@source_join_key	=	Source_Join_Key
			FROM		Source_Join
			WHERE		Record_Key			=	@term_key
			AND			Table_Name			=	'Term'
			AND			Source_Key			=	@source_key
		END

		IF @source_join_key IS NOT NULL
		BEGIN
			/* update existing source join */
			UPDATE		Source_Join
			SET			Record_Key				=	@term_key,
						Source_Key				=	@source_key,
						Original				=	@original,
						System_Supplied_Data	=	@system_supplied_data
			WHERE		Source_Join_Key			=	@source_join_key

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END
		ELSE
		BEGIN
			/* create new source join */
			EXECUTE		spNextKey	'Source_Join',
									@source_join_key	OUTPUT
			IF @@ERROR <> 0 GOTO fail_from_cursor

			INSERT		Source_Join (
						Source_Join_Key,
						Table_Name,
						Record_Key,
						Source_Key,
						Original,
						Entered_Session_ID,
						System_Supplied_Data)
			VALUES 		(@source_join_key,
						'Term',
						@term_key,
						@source_key,
						@original,
						'SYSTEM0000000000',
						@system_supplied_data)

			IF @@ERROR <> 0 GOTO fail_from_cursor

			/* record mapping */
			INSERT		Taxon_Dictionary_Term_Sources_Mapping (
						Source_Link_Key,
						Source_Join_Key)
			VALUES		(@source_link_key,
						@source_join_key)

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END

		/* update progress counter */
		EXECUTE		usp_Import_Export_Job_RecordProcessed	@job_id
		IF @@ERROR <> 0 GOTO fail_from_cursor
		
		COMMIT TRANSACTION
	END

	CLOSE		sources
	DEALLOCATE	sources
	RETURN

fail_from_cursor:
	CLOSE		sources
	DEALLOCATE	sources

fail:
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_SourceJoin_ImportTaxonSources failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SourceJoin_ImportTaxonSources') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SourceJoin_ImportTaxonSources'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SourceJoin_ImportTaxonSources TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SourceJoin_ImportTaxonSources TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SourceJoin_ImportTaxonSources TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_SourceJoin_RecordImported]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_SourceJoin_RecordImported]
GO

/*===========================================================================*\
  Description:	Make any changes required to Source_Join following import of
				a record from the taxon dictionary.

  Parameters:   @source_join_key		Source join key, or NULL if there is
										none.  Current value passed as input
										is updated on exit.
				@table_name				Table into which record was imported
				@record_key				Key of imported record
				@source_key				Source key (NULL if record should not
										be associated with a source)
				@entered_session_id		Session in which join was created (if
										applicable)
				@system_supplied_data	Is the join system supplied data?

  Created:		Jan 2004

  Last revision information:
	$Revision: 7 $
	$Date: 6/02/09 10:41 $
	$Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SourceJoin_RecordImported]
	@source_join_key		CHAR(16)		OUTPUT,
	@table_name				VARCHAR(50),
	@record_key				CHAR(16),
	@source_key				CHAR(16),
	@entered_session_id		CHAR(16),
	@system_supplied_data	BIT
AS
	SET NOCOUNT ON

	/* check whether source exists (current database contains random junk
	 * in TAXON_LIST_VERSION.SOURCE_KEY)
	 */
	IF @source_key IS NOT NULL
	BEGIN
		IF NOT EXISTS (	SELECT		1
						FROM		SOURCE
						WHERE		Source_Key			=	@source_key)
		BEGIN
			SET			@source_key			=	NULL
		END
	END

	IF @source_key IS NULL
	BEGIN
		/* delete current source join, if any */
		IF @source_join_key IS NOT NULL
		BEGIN
			DELETE		Source_Join
			WHERE		Source_Join_Key		=	@source_join_key

			IF @@ERROR <> 0 GOTO fail

			SET			@source_join_key	=	NULL
		END
	END
	ELSE IF @source_join_key IS NOT NULL
	BEGIN
		/* update current source join */
		UPDATE		Source_Join
		SET         Source_Key				=	@source_key,
					Entered_Session_ID		=	@entered_session_id,
					System_Supplied_Data	=	@system_supplied_data
		WHERE		Source_Join_Key			=	@source_join_key

		IF @@ERROR <> 0 GOTO fail
	END
	ELSE
	BEGIN
		SELECT		@source_join_key    =	Source_Join_Key
		FROM		Source_Join
		WHERE		Table_Name			=	@table_name
		AND			Record_Key			=	@record_key
		AND			Source_Key			=	@source_key

		IF @@ROWCOUNT = 0
		BEGIN
			/* create new source join */
			EXECUTE		spNextKey	'Source_Join',
									@source_join_key	OUTPUT
			IF @@ERROR <> 0 GOTO fail

			INSERT		Source_Join (
						Source_Join_Key,
						Table_Name,
						Record_Key,
						Source_Key,
						Entered_Session_ID,
						System_Supplied_Data)
			VALUES		(@source_join_key,
						@table_name,
						@record_key,
						@source_key,
						@entered_session_id,
						@system_supplied_data)

			IF @@ERROR <> 0 GOTO fail
		END
	END
	RETURN

fail:
	RAISERROR ('usp_SourceJoin_RecordImported failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SourceJoin_RecordImported') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SourceJoin_RecordImported'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SourceJoin_RecordImported TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SourceJoin_RecordImported TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SourceJoin_RecordImported TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_SpecimenFieldData_Select_ForOccurrence]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_SpecimenFieldData_Select_ForOccurrence]
GO

/*===========================================================================*\
  Description:	Returns a list of specimens linked to an occurrence.

  Parameters:	@OccurrenceKey

  Created:	November 2003

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SpecimenFieldData_Select_ForOccurrence] 
	@OccurrenceKey char(16),
	@IsTaxonOccurrence bit
AS

SET NOCOUNT ON

	SELECT		SFD.Specimen_Field_Data_Key AS Item_Key,
			SFD.Collection_Unit_Key AS Specimen_Key,
		    	CASE 
				WHEN SU.Life_Sciences = 0 THEN CT.Item_Name COLLATE SQL_Latin1_General_CP1_CI_AS
				ELSE 
					CASE ITN.Actual_Name_Italic 
						WHEN 1 THEN '<i>' + ITN.Actual_Name + '</i>'
						ELSE ITN.Actual_Name 
					END
			END +
			CASE 
				WHEN CUN.Number IS NOT NULL THEN + ' - ' + CUN.Number 
				ELSE + '' 
			END AS Display_Text,
			SFD.Timestamp,
			SFD.Custodian

	FROM		Specimen_Field_Data SFD
	INNER JOIN	Specimen_Unit SU ON SU.Collection_Unit_Key = SFD.Collection_Unit_Key
	LEFT JOIN	(Collection_Unit_Number CUN 
				INNER JOIN	Concept C ON C.Concept_Key = CUN.Type_Concept_Key 
						AND C.Meaning_Key = 'SYSTEM0000000001')  -- Registration Number
			ON CUN.Collection_Unit_Key = SFD.Collection_Unit_Key 
			AND CUN.Preferred = 1
	LEFT JOIN	Determination D 
				INNER JOIN	vw_ConceptTerm CT ON CT.Concept_Key = D.Concept_Key
			ON D.Determination_Key = SU.Preferred_Determination_Key
	LEFT JOIN 	(Taxon_Determination TD
				INNER JOIN	Index_Taxon_Name ITN ON TD.Taxon_List_Item_Key = ITN.Taxon_List_Item_Key)
			ON SU.Preferred_Taxon_Determination_Key = TD.Taxon_Determination_Key

	WHERE		(@IsTaxonOccurrence = 0 AND SFD.Occurrence_Key = @OccurrenceKey)
	OR		(@IsTaxonOccurrence = 1 AND SFD.Taxon_Occurrence_Key = @OccurrenceKey)

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SpecimenFieldData_Select_ForOccurrence') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SpecimenFieldData_Select_ForOccurrence'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SpecimenFieldData_Select_ForOccurrence TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SpecimenFieldData_Select_ForOccurrence TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SpecimenFieldData_Select_ForOccurrence TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_SpecimenFieldData_Select_ForOccurrence TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SpecimenFieldData_Select_ForOccurrence TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SpecimenFieldData_Select_ForOccurrence TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_SpecimenLabel_Insert]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_SpecimenLabel_Insert]
GO
/*===========================================================================*\
  Description:	
  Parameters:	

  Created:	July 2003

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SpecimenLabel_Insert]
	@Key char(16) OUTPUT,
	@CollectionUnitKey char(16),
	@IsInscription bit,
	@Position varchar(100),
	@Inscription nText,
	@Translated text,
	@LanguageConceptKey varchar(4),
	@InferredAuthor tinyint,
	@Comments text,
	@AuthorNameKey char(16) = NULL,
	@ConfidenceConceptKey char(16),
	@SessionID char(16)
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF
	
	EXECUTE spNextKey 'Specimen_Label', @Key OUTPUT

	BEGIN TRANSACTION	
		INSERT INTO Specimen_Label (
			Specimen_Label_Key,
			Collection_Unit_Key,
			Is_Inscription,
			Label_Position,
			Inscription,
			Translated,
			Translated_Language_Key,
			Inferred_Author,
			Comments,
			Author_Name_Key,
			Confidence_Concept_Key,
			Entered_Session_ID
		) VALUES (
			@Key,
			@CollectionUnitKey,
			@IsInscription,
			@Position,
			@Inscription,
			@Translated,
			@LanguageConceptKey,
			IsNull(@InferredAuthor, 0),
			@Comments,
			@AuthorNameKey,
			@ConfidenceConceptKey,
			@SessionID
		)

		IF @@Error <> 0 GOTO RollBackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit:
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SpecimenLabel_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SpecimenLabel_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SpecimenLabel_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SpecimenLabel_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SpecimenLabel_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SpecimenLabel_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SpecimenLabel_Insert TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_SpecimenNameAndRegistration_Get]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_SpecimenNameAndRegistration_Get]
GO

/*===========================================================================*\
  Description:	Returns the formatted name of a specimen, including registration 
		number if available.

  Parameters:	@Key
		@Name	OUTPUT 

  Created:	November 2003

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SpecimenNameAndRegistration_Get] 
	@Key char(16),
	@Caption varchar(150) OUTPUT
AS
SET NOCOUNT ON

	SELECT		@Caption = 
			CASE 
				WHEN SU.Life_Sciences = 0 THEN CT.Item_Name 
				ELSE 
					CASE ITN.Actual_Name_Italic 
						WHEN 1 THEN '<i>' + ITN.Actual_Name + '</i>'
						ELSE ITN.Actual_Name 
					END
			END +
			CASE 
				WHEN CUN.Number IS NOT NULL THEN + ' - ' + CUN.Number 
				ELSE + '' 
			END

	FROM		Specimen_Unit SU
	LEFT JOIN	(Determination D 
				INNER JOIN	vw_ConceptTerm CT ON CT.Concept_Key = D.Concept_Key)
			ON D.Determination_Key = SU.Preferred_Determination_Key
	LEFT JOIN 	(Taxon_Determination TD
				INNER JOIN	Index_Taxon_Name ITN ON TD.Taxon_List_Item_Key = ITN.Taxon_List_Item_Key)
			ON SU.Preferred_Taxon_Determination_Key = TD.Taxon_Determination_Key
	LEFT JOIN	(Collection_Unit_Number CUN 
			INNER JOIN	Concept C ON C.Concept_Key = CUN.Type_Concept_Key 
					AND C.Meaning_Key = 'SYSTEM0000000001')  -- Registration Number.
			ON CUN.Collection_Unit_Key = SU.Collection_Unit_Key

	WHERE		SU.Collection_Unit_Key = @Key
	AND		(CUN.Number IS NULL OR CUN.Preferred = 1)

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SpecimenNameAndRegistration_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SpecimenNameAndRegistration_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SpecimenNameAndRegistration_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SpecimenNameAndRegistration_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SpecimenNameAndRegistration_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_SpecimenNameAndRegistration_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SpecimenNameAndRegistration_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SpecimenNameAndRegistration_Get TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_SpecimensCollected_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_SpecimensCollected_Select]
GO

/*===========================================================================*\
  Description:	Returns the list of all preferred determinations and the
		registration number for the specimens linked to the currently
		selected collection. Returns the list of all determinations for 
		specimen top level node.

  Parameters:	@Key			Key of the event recorder person
		@CollectionUnitKey	Specimen or Collection Unit key
		@KeyIsSpecimen		Bit saying whether the key is a specimen

  Created:	October 2003

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SpecimensCollected_Select]
	@Key char(16),
	@CollectionUnitKey char(16),
	@UserDomainMask int,
	@SessionID char(16),
	@ShowCommonNames bit,
	@KeyIsSpecimen bit
AS

SET NOCOUNT ON

IF @KeyIsSpecimen = 1
	SELECT DISTINCT
			SU.Collection_Unit_Key AS Item_Key,
			CASE WHEN SU.Life_Sciences = 0
			THEN CTP.PlainText COLLATE SQL_Latin1_General_CP1_CI_AS
			ELSE dbo.ufn_GetFormattedTaxonNameByParams(
								Preferred_Name,
								0,
								Common_Name,
								0,
								NULL,
								@ShowCommonNames)
			END + CASE WHEN CUN.Number IS NULL THEN '' ELSE ' - ' + CUN.Number END AS Item_Name
	FROM		Individual AS I
	INNER JOIN	Survey_Event_Recorder AS SER ON SER.Name_Key = I.Name_Key
	INNER JOIN	Sample_Recorder AS SR ON SR.SE_Recorder_Key = SER.SE_Recorder_Key
	INNER JOIN	[Sample] AS S ON S.Sample_Key = SR.Sample_Key
	LEFT JOIN	Occurrence AS O ON O.Sample_Key = S.Sample_Key
	LEFT JOIN	Taxon_Occurrence AS XO ON XO.Sample_Key = S.Sample_Key
	INNER JOIN	Specimen_Field_Data AS SFD ON (SFD.Occurrence_Key = O.Occurrence_Key
							OR SFD.Taxon_Occurrence_Key = XO.Taxon_Occurrence_Key)
						AND SFD.Gathering_Event = 1
	INNER JOIN	Specimen_Unit AS SU ON SU.Collection_Unit_Key = SFD.Collection_Unit_Key
					AND SU.Collection_Unit_Key = @CollectionUnitKey
	INNER JOIN	Collection_Unit AS CU ON CU.Collection_Unit_Key = SU.Collection_Unit_Key 
					AND ((CU.Domain_Mask & @UserDomainMask > 0) 
						OR (CU.Entered_Session_ID = @SessionID)
						OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
					
	LEFT JOIN	(Determination AS D
				INNER JOIN	VW_ConceptTermPreferred AS CTP ON CTP.Concept_Key = D.Concept_Key)
			ON D.Determination_Key = SU.Preferred_Determination_Key
	LEFT JOIN 	(Taxon_Determination AS TD 
				INNER JOIN 	Index_Taxon_Name ITN ON ITN.Taxon_List_Item_Key = TD.Taxon_List_Item_Key)
			ON TD.Taxon_Determination_Key = SU.Preferred_Taxon_Determination_Key  
	LEFT JOIN 	Collection_Unit_Number CUN ON SU.Collection_Unit_key = CUN.Collection_Unit_Key 
					 AND CUN.Preferred = 1 AND CUN.Type_Concept_Key = 'SYSTEM0000000001' --REGISTRATION NUMBER
	
	WHERE		I.Name_Key = @Key
	ORDER BY	Item_Name
ELSE
	SELECT DISTINCT
			SU.Collection_Unit_Key AS Item_Key,
			CASE WHEN SU.Life_Sciences = 0
			THEN CTP.PlainText COLLATE SQL_Latin1_General_CP1_CI_AS
			ELSE dbo.ufn_GetFormattedTaxonNameByParams(
								Preferred_Name,
								0,
								Common_Name,
								0,
								NULL,
								@ShowCommonNames)
			END + CASE WHEN CUN.Number IS NULL THEN '' ELSE ' - ' + CUN.Number END AS Item_Name
	FROM		Individual AS I
	INNER JOIN	Survey_Event_Recorder AS SER ON SER.Name_Key = I.Name_Key
	INNER JOIN	Sample_Recorder AS SR ON SR.SE_Recorder_Key = SER.SE_Recorder_Key
	INNER JOIN	[Sample] AS S ON S.Sample_Key = SR.Sample_Key
	LEFT JOIN	Occurrence AS O ON O.Sample_Key = S.Sample_Key
	LEFT JOIN	Taxon_Occurrence AS XO ON XO.Sample_Key = S.Sample_Key
	INNER JOIN	Specimen_Field_Data AS SFD ON (SFD.Occurrence_Key = O.Occurrence_Key
							OR SFD.Taxon_Occurrence_Key = XO.Taxon_Occurrence_Key)
						AND SFD.Gathering_Event = 1
	INNER JOIN	Specimen_Unit AS SU ON SU.Collection_Unit_Key = SFD.Collection_Unit_Key
					AND SU.Parent_Collection_Collection_Unit_Key = @CollectionUnitKey
	INNER JOIN	Collection_Unit AS CU ON CU.Collection_Unit_Key = SU.Collection_Unit_Key 
					AND ((CU.Domain_Mask & @UserDomainMask > 0) 
						OR (CU.Entered_Session_ID = @SessionID)
						OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
	LEFT JOIN	(Determination AS D
				INNER JOIN	VW_ConceptTermPreferred AS CTP ON CTP.Concept_Key = D.Concept_Key)
			ON D.Determination_Key = SU.Preferred_Determination_Key
	LEFT JOIN 	(Taxon_Determination AS TD 
				INNER JOIN 	Index_Taxon_Name ITN ON ITN.Taxon_List_Item_Key = TD.Taxon_List_Item_Key)
			ON TD.Taxon_Determination_Key = SU.Preferred_Taxon_Determination_Key  
	LEFT JOIN 	Collection_Unit_Number CUN ON SU.Collection_Unit_key = CUN.Collection_Unit_Key 
					 AND CUN.Preferred = 1 AND CUN.Type_Concept_Key = 'SYSTEM0000000001' --REGISTRATION NUMBER
	
	WHERE		I.Name_Key = @Key
	ORDER BY	Item_Name
SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SpecimensCollected_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SpecimensCollected_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SpecimensCollected_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SpecimensCollected_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SpecimensCollected_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_SpecimensCollected_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SpecimensCollected_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SpecimensCollected_Select TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_SpecimensDetermined_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_SpecimensDetermined_Select]
GO

/*===========================================================================*\
  Description:	Returns the list of all preferred determinations and the
		registration number for the specimens linked to the currently
		selected collection. Returns the list of all determinations for
		specimen top level node.

  Parameters:	@Key			Key of the determiner.
		@CollectionUnitKey	Key of the collection unit
		@KeyIsSpecimen		Bit saying whether the key is a specimen

  Created:	October 2003

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SpecimensDetermined_Select]
	@Key char(16),
	@CollectionUnitKey char(16),
	@UserDomainMask int,
	@SessionID char(16),
	@ShowCommonNames bit,
	@KeyIsSpecimen bit
AS

SET NOCOUNT ON

IF @KeyIsSpecimen = 1
BEGIN
	SELECT DISTINCT
			SU.Collection_Unit_Key AS Item_Key, 
			CASE SU.Life_Sciences 
				WHEN 0 THEN 
					CTPref.PlainText COLLATE SQL_Latin1_General_CP1_CI_AS
				ELSE dbo.ufn_GetFormattedTaxonNameByParams(
								ITN.Preferred_Name,
								0,
								ITN.Common_Name,
								0,
								NULL,
								@ShowCommonNames)
			END + 
			CASE 
				WHEN CUN.Number IS NULL THEN '' 
				ELSE ' - ' + CUN.Number 
			END AS Item_Name
	FROM 		Specimen_Unit AS SU
	INNER JOIN	Collection_Unit AS CU ON CU.Collection_Unit_Key = SU.Collection_Unit_Key	
					AND ((CU.Domain_Mask & @UserDomainMask > 0) 
						OR (CU.Entered_Session_ID = @SessionID) 
						OR (CU.Changed_Session_ID = @SessionID) 
						OR (CU.Domain_Mask = 0))
	LEFT JOIN	Taxon_Determination AS TD ON TD.Taxon_Determination_Key = SU.Preferred_Taxon_Determination_Key
	LEFT JOIN	Determination AS D ON D.Determination_Key = SU.Preferred_Determination_Key
	INNER JOIN	Individual AS I ON (I.Name_Key = TD.Determiner
						OR I.Name_Key = D.Determiner_Name_Key)
					AND I.Name_Key = @Key
	LEFT JOIN 	VW_ConceptTermPreferred CTPref ON CTPref.Concept_Key=D.Concept_Key
	LEFT JOIN 	Index_Taxon_Name ITN ON ITN.Taxon_List_Item_Key=TD.Taxon_List_Item_Key
	LEFT JOIN 	Collection_Unit_Number CUN ON SU.Collection_Unit_key = CUN.Collection_Unit_Key 
			 			AND CUN.Preferred = 1 
						AND CUN.Type_Concept_Key = 'SYSTEM0000000001' --REGISTRATION NUMBER
	WHERE 		SU.Collection_unit_key = @CollectionUnitKey
	ORDER BY 	Item_Name
END
ELSE
BEGIN
	SELECT DISTINCT
			SU.Collection_Unit_Key AS Item_Key, 
			CASE SU.Life_Sciences 
				WHEN 0 THEN 
					CTPref.PlainText COLLATE SQL_Latin1_General_CP1_CI_AS
				ELSE dbo.ufn_GetFormattedTaxonNameByParams(
								ITN.Preferred_Name,
								0,
								ITN.Common_Name,
								0,
								NULL,
								@ShowCommonNames)
			END + 
			CASE 
				WHEN CUN.Number IS NULL THEN '' 
				ELSE ' - ' + CUN.Number 
			END AS Item_Name
	FROM 		Specimen_Unit AS SU
	INNER JOIN	Collection_Unit AS CU ON CU.Collection_Unit_Key = SU.Collection_Unit_Key	
					AND ((CU.Domain_Mask & @UserDomainMask > 0) 
						OR (CU.Entered_Session_ID = @SessionID) 
						OR (CU.Changed_Session_ID = @SessionID) 
						OR (CU.Domain_Mask = 0))
	LEFT JOIN	Taxon_Determination AS TD ON TD.Taxon_Determination_Key = SU.Preferred_Taxon_Determination_Key
	LEFT JOIN	Determination AS D ON D.Determination_Key = SU.Preferred_Determination_Key
	INNER JOIN	Individual AS I ON (I.Name_Key = TD.Determiner
						OR I.Name_Key = D.Determiner_Name_Key)
					AND I.Name_Key = @Key
	LEFT JOIN 	VW_ConceptTermPreferred CTPref ON CTPref.Concept_Key=D.Concept_Key
	LEFT JOIN 	Index_Taxon_Name ITN ON ITN.Taxon_List_Item_Key=TD.Taxon_List_Item_Key
	LEFT JOIN 	Collection_Unit_Number CUN ON SU.Collection_Unit_key = CUN.Collection_Unit_Key 
			 			AND CUN.Preferred = 1 
						AND CUN.Type_Concept_Key = 'SYSTEM0000000001' --REGISTRATION NUMBER
	WHERE 		SU.Parent_Collection_Collection_unit_key = @CollectionUnitKey
	ORDER BY 	Item_Name
END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SpecimensDetermined_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SpecimensDetermined_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SpecimensDetermined_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SpecimensDetermined_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SpecimensDetermined_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_SpecimensDetermined_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SpecimensDetermined_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SpecimensDetermined_Select TO [Dev - JNCC SQL]
END

GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Specimens_Select_ForCollection]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Specimens_Select_ForCollection]
GO

/*===========================================================================*\
  Description:	Returns Specimens data to the CollectionsBrowser for a given Collection.

  Parameters:
	@ParentKey 		When specified, only the records associated with the parent key are returned
	@UserDomainMask		User's Domain Mask restricting which records may be returned
	@SessionID 		User's SessionID
	@ShowCommonNames	Specifies whether or not Common Names should be shown
	@SortOrderIndex		Index determining Sort Order

  Created:	August 2003

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Specimens_Select_ForCollection] 
	@UserDomainMask INT,
	@SessionID CHAR(16),
	@ParentKey CHAR(16),
	@ShowCommonNames BIT,
	@SortOrderIndex TINYINT
AS

SET NOCOUNT ON
-- Set of options for better use of vw_ConceptTerm.
SET ANSI_NULLS ON
SET ANSI_PADDING ON
SET ANSI_WARNINGS ON
SET ARITHABORT ON
SET CONCAT_NULL_YIELDS_NULL ON
SET QUOTED_IDENTIFIER ON
SET NO_BROWSETABLE OFF

	-- Use temp table to build output, so silly data errors such as duplicate accessions
	-- don't duplicate in the list
	DECLARE @SpecimensSearch TABLE
	(
		[Item_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[Join_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[Det_Item_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[Det_Item_Name] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[Item_Name] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[Life_Sciences] [bit] NULL,
		[Number] [varchar] (30) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[Hint] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL 
	)

	INSERT INTO	@SpecimensSearch (Item_Key, Join_Key, Life_Sciences) 
	SELECT DISTINCT SU.Collection_Unit_Key, SU.Collection_Unit_Key, SU.Life_Sciences
	FROM 		Specimen_Unit SU
	INNER JOIN 	Collection_Unit CU 
		ON SU.Collection_Unit_Key = CU.Collection_Unit_Key
		AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
			OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
	WHERE 	SU.Parent_Collection_Collection_Unit_Key = @ParentKey


	UPDATE @SpecimensSearch
	SET	Det_Item_Key = 
			CASE WHEN SU.Life_Sciences = 0 THEN CPref.Concept_Key ELSE TD.Taxon_List_Item_Key END,
		Item_Name = 
			CASE 	WHEN SU.Life_Sciences = 0 THEN TPref.Item_Name 
				ELSE dbo.ufn_GetFormattedTaxonNameByParams(Preferred_Name, Preferred_Name_Italic,
					Common_Name, Common_Name_Italic, NULL, @ShowCommonNames)
			END,
		Number = CUN.Number,
		Det_Item_Name =
			CASE WHEN SU.Life_Sciences = 0 THEN TDet.Plaintext ELSE ITN.Actual_Name END

	FROM 		@SpecimensSearch SU
	LEFT JOIN 	Collection_Unit_Number CUN ON SU.Item_key = CUN.Collection_Unit_Key AND CUN.Type_Concept_Key = 'SYSTEM0000000001'
	INNER JOIN 	Specimen_Unit SUnit ON SUnit.Collection_Unit_Key = SU.Item_Key
	LEFT JOIN 	Determination D ON D.Determination_Key = SUnit.Preferred_Determination_Key
	LEFT JOIN 	Concept C ON C.Concept_Key = D.Concept_Key
	LEFT JOIN 	Term TDet ON TDet.Term_Key = C.Term_Key
	LEFT JOIN 	Concept CPref ON CPref.Meaning_Key = C.Meaning_Key AND CPref.List_Preferred = 1 AND CPref.Concept_Group_Key = C.Concept_Group_Key
	LEFT JOIN 	Term TPref ON TPref.Term_Key = CPref.Term_Key
	LEFT JOIN 	Taxon_Determination TD ON SU.Item_Key = TD.Specimen_Collection_Unit_Key
	LEFT JOIN 	Index_Taxon_Name ITN ON ITN.Taxon_List_Item_Key = TD.Taxon_List_Item_Key


-- Select table and sort appropriately
IF @SortOrderIndex = 0
	SELECT * FROM @SpecimensSearch
	ORDER BY Item_Name, Number
ELSE 
IF @SortOrderIndex = 1
	SELECT * FROM @SpecimensSearch
	ORDER BY Number, Item_Name

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Specimens_Select_ForCollection') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Specimens_Select_ForCollection'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForCollection TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForCollection TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForCollection TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForCollection TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForCollection TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForCollection TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Specimens_Select_ForSearch]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Specimens_Select_ForSearch]
GO

/*===========================================================================*\
  Description:	Returns Collection_Unit_Key and DisplayTerm when search characters 
		are entered. The Specimen_Unit table does not have a Display_Caption 
		or Search_Caption field, so the caption must be constructed through 
		joins to other tables.

  Parameters:	@UserDomainMask		User's Domain Mask restricting which records may be returned.
		@SessionID 		User's SessionID.
		@SearchText 		Search text used to find collections.

  Created:	September 2003

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Specimens_Select_ForSearch] 
	@UserDomainMask int,
	@SessionID char(16),
	@ShowCommonNames BIT,
	@SearchText varchar(100)
AS

SET NOCOUNT ON


--Use temp table to build output, so silly data errors such as duplicate accessions
-- don't duplicate in the list
DECLARE @SpecimensSearch TABLE
(
	[Item_Key] [char] (16) COLLATE database_default NULL,
	[Det_Item_Key] [char] (16) COLLATE database_default NULL,
	[DisplayTerm] [nvarchar] (150) COLLATE database_default NULL,
	[SearchTerm] [nvarchar] (150) COLLATE database_default NULL,
	[Life_Sciences] [bit] NULL
)

--Find all specimens with a determination match
INSERT INTO 
	@SpecimensSearch (Item_Key, Life_Sciences, SearchTerm, DisplayTerm, Det_Item_Key) 
SELECT DISTINCT 
	SU.Collection_Unit_Key				COLLATE database_default, 
	SU.Life_Sciences,
	CASE Su.Life_Sciences 
		WHEN 0 THEN TSearch.Plaintext	COLLATE database_default
		ELSE ITN.Actual_Name			COLLATE database_default
	END AS SearchTerm,
	CASE Su.Life_Sciences 
		WHEN 0 THEN TSearch.Item_Name	COLLATE database_default 
		ELSE CASE ITN.Actual_Name_Italic
			WHEN 1 THEN '<i>' + ITN.Actual_Name + '</i>' COLLATE database_default
			ELSE ITN.Actual_Name		COLLATE database_default
		END
	END AS DisplayTerm,
	CASE Su.Life_Sciences 
		WHEN 0 THEN C.Concept_Key		COLLATE database_default
		ELSE ITN.Taxon_List_Item_Key	COLLATE database_default
	END AS Det_Item_Key
	
FROM SPECIMEN_UNIT SU
INNER JOIN Collection_Unit CU ON SU.Collection_Unit_Key = CU.Collection_Unit_Key 
 	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
	OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
LEFT JOIN VW_SpecimenDetsEarth SDE ON SU.Collection_Unit_Key = SDE.Collection_Unit_Key
LEFT JOIN Concept C ON SDE.Concept_Key = C.Concept_Key
LEFT JOIN Concept CSearch ON CSearch.Meaning_Key=C.Meaning_Key
LEFT JOIN Term TSearch ON TSearch.Term_Key=CSearch.Term_Key
LEFT JOIN VW_SpecimenDetsLife SDL ON SU.Collection_Unit_Key = SDL.Collection_Unit_Key
LEFT JOIN Index_Taxon_Synonym ITS ON ITS.Taxon_List_Item_Key=SDL.Taxon_List_Item_Key
LEFT JOIN INDEX_TAXON_NAME ITN	ON ITS.Synonym_List_Item_Key = ITN.Taxon_List_Item_Key
WHERE 
	(TSearch.Plaintext LIKE @SearchText + '%' AND SU.Life_Sciences=0) 
	OR 
	(ITN.Actual_Name LIKE @SearchText + '%' AND SU.Life_Sciences=1)

-- Update the number in case there are 2 registrations for a specimen, so we don't duplicate
-- the rows in the output results.
UPDATE @SpecimensSearch
SET 
		SearchTerm = SearchTerm + ' - ' + CUN.Number,
		DisplayTerm = DisplayTerm + ' - ' + CUN.Number
FROM @SpecimensSearch SU
INNER JOIN Collection_Unit_Number CUN ON SU.Item_key = CUN.Collection_Unit_Key 
		AND CUN.Type_Concept_Key = 'SYSTEM0000000001' AND CUN.Preferred=1
INNER JOIN Concept C ON C.Concept_Key=CUN.Type_Concept_Key
	AND C.Meaning_Key='SYSTEM0000000001'

-- Select table and sort appropriately
SELECT * from @SpecimensSearch
ORDER BY SearchTerm

GO




/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Specimens_Select_ForSearch') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Specimens_Select_ForSearch'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearch TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearch TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearch TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearch TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearch TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearch TO [Dev - JNCC SQL]
END
GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Specimens_Select_ForSearchByAnyDetermination]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Specimens_Select_ForSearchByAnyDetermination]
GO

CREATE PROCEDURE [dbo].[usp_Specimens_Select_ForSearchByAnyDetermination] 
@UserDomainMask INT,
@SessionID CHAR(16),
@ShowCommonNames BIT,
@SearchText VARCHAR(150),
@SortOrderIndex TINYINT

AS

--  DESCRIPTION
--  Returns Specimens based on the any determination
--
--  PARAMETERS
--  NAME				DESCRIPTION
--	@UserDomainMask		User's Domain Mask restricting which records may be returned
--	@SessionID 			User's SessionID
--	@ShowCommonNames	Specifies whether or not Common Names should be shown
--	@SearchText			Text to be searched on
--	@SortOrderIndex		Index determining Sort Order
--
--
--  AUTHOR:     Ben Collier, Dorset Software
--  CREATED:    2003-10-07
--
SET NOCOUNT ON


--Use temp table to build output, so silly data errors such as duplicate accessions
-- don't duplicate in the list
DECLARE @SpecimensSearch TABLE
(
	[Item_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[Det_Item_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[Det_Item_Name] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[Item_Name] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[Life_Sciences] [bit] NULL,
	[Number] [varchar] (30) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[Hint] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL 
)

--Find all specimens with a determination match

INSERT INTO 
	@SpecimensSearch (Item_Key, Life_Sciences) 
SELECT DISTINCT SU.Collection_Unit_Key, SU.Life_Sciences
FROM SPECIMEN_UNIT SU
INNER JOIN Collection_Unit CU ON SU.Collection_Unit_Key = CU.Collection_Unit_Key 
 	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
	OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
INNER JOIN VW_SpecimenDetsEarth SDE ON SU.Collection_Unit_Key = SDE.Collection_Unit_Key
INNER JOIN Concept C ON SDE.Concept_Key = C.Concept_Key
INNER JOIN Concept CSearch ON CSearch.Meaning_Key=C.Meaning_Key
INNER JOIN Term TSearch ON TSearch.Term_Key=CSearch.Term_Key
WHERE (TSearch.Plaintext LIKE @SearchText + '%' AND SU.Life_Sciences=0) 

INSERT INTO 
	@SpecimensSearch (Item_Key, Life_Sciences) 
SELECT DISTINCT SU.Collection_Unit_Key, SU.Life_Sciences
FROM SPECIMEN_UNIT SU
INNER JOIN Collection_Unit CU ON SU.Collection_Unit_Key = CU.Collection_Unit_Key 
 	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
	OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
INNER JOIN VW_SpecimenDetsLife SDL ON SU.Collection_Unit_Key = SDL.Collection_Unit_Key
INNER JOIN Index_Taxon_Synonym ITS ON ITS.Taxon_List_Item_Key=SDL.Taxon_List_Item_Key
INNER JOIN INDEX_TAXON_NAME ITN	ON ITS.Synonym_List_Item_Key = ITN.Taxon_List_Item_Key
WHERE (ITN.Actual_Name LIKE @SearchText + '%' AND SU.Life_Sciences=1)


UPDATE @SpecimensSearch
SET	Det_Item_Key=CPref.Concept_Key,
		Item_Name = TPref.Item_Name,
		Number=CUN.Number,
		Det_Item_Name=TDet.Plaintext,
		Hint=TDet.Plaintext
FROM @SpecimensSearch SU
LEFT JOIN Collection_Unit_Number CUN ON SU.Item_key = CUN.Collection_Unit_Key 
		AND CUN.Type_Concept_Key = 'SYSTEM0000000001' AND CUN.Preferred=1
INNER JOIN VW_SpecimenDetsEarth SDE ON SDE.Collection_Unit_Key=SU.Item_Key
INNER JOIN Concept C ON C.Concept_Key=SDE.Concept_Key
INNER JOIN Term TDet ON TDet.Term_Key=C.Term_Key
INNER JOIN Concept CPref ON CPref.Meaning_Key=C.Meaning_Key
		AND CPref.List_Preferred=1
		AND CPref.Concept_Group_Key=C.Concept_Group_Key
INNER JOIN Term TPref ON TPref.Term_Key=CPref.Term_Key


UPDATE @SpecimensSearch
SET	Det_Item_Key=SDL.Taxon_List_Item_Key,
		Item_Name =
					dbo.ufn_GetFormattedTaxonNameByParams(
						Preferred_Name,
						Preferred_Name_Italic,
						Common_Name,
						Common_Name_Italic,
						null,
						@ShowCommonNames),
		Number=CUN.Number,
		Det_Item_Name=ITN.Actual_Name,
		Hint=ITN.Actual_Name
FROM @SpecimensSearch SU
LEFT JOIN Collection_Unit_Number CUN ON SU.Item_key = CUN.Collection_Unit_Key 
		AND CUN.Type_Concept_Key = 'SYSTEM0000000001' AND CUN.Preferred=1
INNER JOIN VW_SpecimenDetsLife SDL ON SDL.Collection_Unit_Key=SU.Item_Key
INNER JOIN Index_Taxon_Name ITN	ON ITN.Taxon_List_Item_Key=SDL.Taxon_List_Item_Key


-- Select table and sort appropriately
IF @SortOrderIndex = 0
	SELECT * from @SpecimensSearch
	ORDER BY Item_Name, Number
ELSE IF @SortOrderIndex = 1
	SELECT * from @SpecimensSearch
	ORDER BY Number, Item_Name

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Specimens_Select_ForSearchByAnyDetermination') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Specimens_Select_ForSearchByAnyDetermination'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByAnyDetermination TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByAnyDetermination TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByAnyDetermination TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByAnyDetermination TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByAnyDetermination TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByAnyDetermination TO [Dev - JNCC SQL]
END

GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Specimens_Select_ForSearchByDescription]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Specimens_Select_ForSearchByDescription]
GO

CREATE PROCEDURE [dbo].[usp_Specimens_Select_ForSearchByDescription] 
@UserDomainMask INT,
@SessionID CHAR(16),
@ShowCommonNames BIT,
@SearchText VARCHAR(100),
@SortOrderIndex TINYINT

AS
--  DESCRIPTION
--  Returns Specimens data based on a search using the Description parameter
--
--  PARAMETERS
--	NAME				DESCRIPTION
--	@UserDomainMask		User's Domain Mask restricting which records may be returned
--	@SessionID 			User's SessionID
--	@ShowCommonNames	Specifies whether or not Common Names should be shown
--	@SortOrderIndex		Index determining Sort Order
--
--
--  AUTHOR:     		Ben Collier, Dorset Software
--  CREATED:    		2003-10-07
--
SET NOCOUNT ON
-- Set of options for better use of vw_ConceptTerm.
SET ANSI_NULLS ON
SET ANSI_PADDING ON
SET ANSI_WARNINGS ON
SET ARITHABORT ON
SET CONCAT_NULL_YIELDS_NULL ON
SET QUOTED_IDENTIFIER ON
SET NO_BROWSETABLE OFF


--Use temp table to build output, so silly data errors such as duplicate accessions
-- don't duplicate in the list
DECLARE @SpecimensSearch TABLE
(
	[Item_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[Det_Item_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[Det_Item_Name] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[Item_Name] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[Life_Sciences] [bit] NULL,
	[Number] [varchar] (30) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[Hint] [text] COLLATE SQL_Latin1_General_CP1_CI_AS NULL 
)

INSERT INTO 
	@SpecimensSearch (Item_Key, Life_Sciences, Hint) 
SELECT 
	SU.Collection_Unit_Key, SU.Life_Sciences, M.Text
FROM Specimen_Unit SU
INNER JOIN Metadata M ON M.Record_Key=SU.Collection_Unit_Key
	AND M.Metadata_Type_Key='SYSTEM0000000006'
INNER JOIN Collection_Unit CU ON SU.Collection_Unit_Key = CU.Collection_Unit_Key 
 	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
	OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
WHERE 
	M.Text LIKE @SearchText + '%'


UPDATE @SpecimensSearch
SET	Det_Item_Key=CPref.Concept_Key,
		Item_Name = TPref.Item_Name,
		Number=CUN.Number,
		Det_Item_Name=TDet.Plaintext
FROM @SpecimensSearch SU
LEFT JOIN Collection_Unit_Number CUN ON SU.Item_key = CUN.Collection_Unit_Key 
		AND CUN.Type_Concept_Key = 'SYSTEM0000000001' AND CUN.Preferred=1
INNER JOIN VW_SpecimenDetsEarth SDE ON SDE.Collection_Unit_Key=SU.Item_Key
INNER JOIN Concept C ON C.Concept_Key=SDE.Concept_Key
INNER JOIN Term TDet ON TDet.Term_Key=C.Term_Key
INNER JOIN Concept CPref ON CPref.Meaning_Key=C.Meaning_Key
		AND CPref.List_Preferred=1
		AND CPref.Concept_Group_Key=C.Concept_Group_Key
INNER JOIN Term TPref ON TPref.Term_Key=CPref.Term_Key


UPDATE @SpecimensSearch
SET	Det_Item_Key=SDL.Taxon_List_Item_Key,
		Item_Name =
					dbo.ufn_GetFormattedTaxonNameByParams(
						Preferred_Name,
						Preferred_Name_Italic,
						Common_Name,
						Common_Name_Italic,
						null,
						@ShowCommonNames),
		Number=CUN.Number,
		Det_Item_Name=ITN.Actual_Name
FROM @SpecimensSearch SU
LEFT JOIN Collection_Unit_Number CUN ON SU.Item_key = CUN.Collection_Unit_Key 
		AND CUN.Type_Concept_Key = 'SYSTEM0000000001' AND CUN.Preferred=1
INNER JOIN VW_SpecimenDetsLife SDL ON SDL.Collection_Unit_Key=SU.Item_Key
INNER JOIN Index_Taxon_Name ITN	ON ITN.Taxon_List_Item_Key=SDL.Taxon_List_Item_Key

-- Select table and sort appropriately
IF @SortOrderIndex = 0
	SELECT * from @SpecimensSearch
	ORDER BY Det_Item_Name, Number
ELSE IF @SortOrderIndex = 1
	SELECT * from @SpecimensSearch
	ORDER BY Number, Item_Name

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Specimens_Select_ForSearchByDescription') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Specimens_Select_ForSearchByDescription'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByDescription TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByDescription TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByDescription TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByDescription TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByDescription TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByDescription TO [Dev - JNCC SQL]
END

GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Specimens_Select_ForSearchByDeterminationInGroup]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Specimens_Select_ForSearchByDeterminationInGroup]
GO

CREATE PROCEDURE [dbo].[usp_Specimens_Select_ForSearchByDeterminationInGroup] 
@UserDomainMask INT,
@SessionID CHAR(16),
@ShowCommonNames BIT,
@SearchText VARCHAR(100),
@SortOrderIndex TINYINT

AS

--  DESCRIPTION
--  Returns Specimens data based on the search parameter for Gathering Location
--
--  PARAMETERS
--	NAME				DESCRIPTION
--	@UserDomainMask		User's Domain Mask restricting which records may be returned
--	@SessionID 			User's SessionID
--	@ShowCommonNames	Specifies whether or not Common Names should be shown
--	@SearchText			Text to search On
--	@SortOrderIndex		Index determining Sort Order
--
--
--  AUTHOR:     		Ben Collier, Dorset Software
--  CREATED:    		2003-10-08
--
SET NOCOUNT ON

-- Set of options for better use of vw_ConceptTerm.
SET ANSI_NULLS ON
SET ANSI_PADDING ON
SET ANSI_WARNINGS ON
SET ARITHABORT ON
SET CONCAT_NULL_YIELDS_NULL ON
SET QUOTED_IDENTIFIER ON
SET NO_BROWSETABLE OFF

	--Use temp table to build output, so silly data errors such as duplicate accessions
	-- don't duplicate in the list
	DECLARE @SpecimensSearch TABLE
	(
		[Item_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[Det_Item_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[Det_Item_Name] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[Item_Name] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[Life_Sciences] [bit] NULL,
		[Number] [varchar] (30) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[Hint] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL 
	)

	--Create a temp table to hold the meanings of the contents of the groups that match the search
	DECLARE @SearchLineage TABLE (
		Child_Meaning_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS,
		PlainText varchar(150) COLLATE SQL_Latin1_General_CP1_CI_AS
	)

	INSERT INTO 	@SearchLineage
	SELECT DISTINCT CChild.Meaning_Key, CSearch.PlainText
	FROM 		vw_ConceptTerm CSearch
	INNER JOIN 	Concept_Group CG ON CG.Concept_Group_Key = CSearch.Concept_Group_Key
	INNER JOIN 	Local_Domain LD ON LD.Local_Domain_Key = CG.Local_Domain_Key
	INNER JOIN 	Domain D ON D.Domain_Key = LD.Domain_Key AND D.Has_Occurrences = 1
	INNER JOIN 	Concept CSynSearch ON CSynSearch.Meaning_Key = CSearch.Meaning_Key
	INNER JOIN 	Concept_Lineage CL ON CL.Concept_Key = CSynSearch.Concept_Key
	INNER JOIN 	Concept_Lineage CLChild ON CLChild.Lineage LIKE CL.Lineage + '\%'
	INNER JOIN 	Concept CChild ON CChild.Concept_Key = CLChild.Concept_Key AND (CChild.Concept_Group_Key = CSynSearch.Concept_Group_Key)
	WHERE 		CSearch.Plaintext LIKE @SearchText + '%'

	INSERT INTO 	@SpecimensSearch (Item_Key, Life_Sciences, Hint) 
	SELECT DISTINCT SDE.Collection_Unit_Key, 0 AS Life_Sciences, SL.Plaintext
	FROM 		@SearchLineage SL
	INNER JOIN 	vw_ConceptTerm CChildSyn ON CChildSyn.Meaning_Key = SL.Child_Meaning_Key
	INNER JOIN 	vw_SpecimenDetsEarth SDE ON SDE.Concept_Key = CChildSyn.Concept_Key
	INNER JOIN 	Collection_Unit CU ON CU.Collection_Unit_Key = SDE.Collection_Unit_Key	
	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
	
	
	INSERT INTO 	@SpecimensSearch (Item_Key, Life_Sciences, Hint) 
	SELECT DISTINCT SDL.Collection_Unit_Key AS Item_Key, 1 AS Life_Sciences, ITNSearch.Actual_Name
	FROM 		Index_Taxon_Name ITNSearch
	INNER JOIN 	Index_Taxon_Synonym ITSSearch ON ITSSearch.Taxon_List_Item_Key = ITNSearch.Taxon_List_Item_Key
	INNER JOIN 	Index_Taxon_Group ITG ON ITG.Taxon_List_Item_Key = ITSSearch.Synonym_List_Item_Key
	INNER JOIN 	Index_Taxon_Synonym ITSSyn ON ITSSyn.Taxon_List_Item_Key = ITG.Contained_List_Item_Key
	INNER JOIN 	Index_Taxon_Name ITNSyn ON ITNSyn.Taxon_List_Item_Key = ITSSyn.Synonym_List_Item_Key
	INNER JOIN 	vw_SpecimenDetsLife SDL ON SDL.Taxon_List_Item_Key = ITNSyn.Taxon_List_Item_Key
	INNER JOIN 	Collection_Unit CU ON CU.Collection_Unit_Key = SDL.Collection_Unit_Key
		AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
	WHERE 		ITNSearch.Actual_Name LIKE @SearchText + '%'

	UPDATE 	@SpecimensSearch
	SET	Det_Item_Key=CPref.Concept_Key,
		Item_Name = TPref.Item_Name,
		Number=CUN.Number,
		Det_Item_Name=TDet.Plaintext 
	FROM 		@SpecimensSearch SU
	INNER JOIN 	Collection_Unit_Number CUN ON SU.Item_key = CUN.Collection_Unit_Key AND CUN.Type_Concept_Key = 'SYSTEM0000000001' AND CUN.Preferred = 1
	INNER JOIN 	VW_SpecimenDetsEarth SDE ON SDE.Collection_Unit_Key=SU.Item_Key
	INNER JOIN 	Concept C ON C.Concept_Key=SDE.Concept_Key
	INNER JOIN 	Term TDet ON TDet.Term_Key=C.Term_Key
	INNER JOIN 	Concept CPref ON CPref.Meaning_Key = C.Meaning_Key AND CPref.List_Preferred = 1 AND CPref.Concept_Group_Key = C.Concept_Group_Key
	INNER JOIN 	Term TPref ON TPref.Term_Key = CPref.Term_Key

	UPDATE 	@SpecimensSearch
	SET	Det_Item_Key=SDL.Taxon_List_Item_Key,
		Item_Name = dbo.ufn_GetFormattedTaxonNameByParams(
					Preferred_Name,
					Preferred_Name_Italic,
					Common_Name,
					Common_Name_Italic,
					NULL,
					@ShowCommonNames),
		Number=CUN.Number,
		Det_Item_Name=ITN.Actual_Name
	FROM 		@SpecimensSearch SU
	INNER JOIN 	Collection_Unit_Number CUN ON SU.Item_key = CUN.Collection_Unit_Key 
		AND CUN.Type_Concept_Key = 'SYSTEM0000000001' AND CUN.Preferred = 1
	INNER JOIN 	VW_SpecimenDetsLife SDL ON SU.Item_Key = SDL.Collection_Unit_Key
	INNER JOIN 	Index_Taxon_Name ITN	ON ITN.Taxon_List_Item_Key=SDL.Taxon_List_Item_Key

	-- Select table and sort appropriately
	IF @SortOrderIndex = 0
		SELECT * from @SpecimensSearch
		ORDER BY Det_Item_Name, Number
	ELSE IF @SortOrderIndex = 1
		SELECT * from @SpecimensSearch
		ORDER BY Number, Item_Name

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Specimens_Select_ForSearchByDeterminationInGroup') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Specimens_Select_ForSearchByDeterminationInGroup'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByDeterminationInGroup TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByDeterminationInGroup TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByDeterminationInGroup TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByDeterminationInGroup TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByDeterminationInGroup TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByDeterminationInGroup TO [Dev - JNCC SQL]
END
GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Specimens_Select_ForSearchByGatheringDate]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Specimens_Select_ForSearchByGatheringDate]
GO

CREATE PROCEDURE [dbo].[usp_Specimens_Select_ForSearchByGatheringDate] 
@UserDomainMask INT,
@SessionID CHAR(16),
@ShowCommonNames BIT,
@SearchText VARCHAR(50),
@SortOrderIndex TINYINT

AS

--  DESCRIPTION
--  Returns Specimens data based on the search parameter for Gathering Date
--
--  PARAMETERS
--	NAME				DESCRIPTION
--	@UserDomainMask		User's Domain Mask restricting which records may be returned
--	@SessionID 			User's SessionID
--	@ShowCommonNames	Specifies whether or not Common Names should be shown
--	@SearchText			Text to search On
--	@SortOrderIndex		Index determining Sort Order
--
--
--  AUTHOR:     		Ben Collier, Dorset Software
--  CREATED:    		2003-10-09
--
SET NOCOUNT ON
-- Set of options for better use of vw_ConceptTerm.
SET ANSI_NULLS ON
SET ANSI_PADDING ON
SET ANSI_WARNINGS ON
SET ARITHABORT ON
SET CONCAT_NULL_YIELDS_NULL ON
SET QUOTED_IDENTIFIER ON
SET NO_BROWSETABLE OFF


--Use temp table to build output, so silly data errors such as duplicate accessions
-- don't duplicate in the list
DECLARE @SpecimensSearch TABLE
(
	[Item_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[Det_Item_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[Det_Item_Name] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[Item_Name] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[Life_Sciences] [bit] NULL,
	[Number] [varchar] (30) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[Hint] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL 
)

INSERT INTO 
	@SpecimensSearch (Item_Key, Life_Sciences, Hint) 
SELECT DISTINCT 
	SU.Collection_Unit_Key, 
	SU.Life_Sciences, 
	dbo.ufn_GetDateFromVagueDate(S.Vague_Date_Start, S.Vague_Date_End, S.Vague_Date_Type)
FROM Specimen_Unit SU
	INNER JOIN Collection_Unit CU ON SU.Collection_Unit_Key = CU.Collection_Unit_Key 
	 	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
		OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
	INNER JOIN Specimen_Field_Data SFD ON SFD.Collection_Unit_Key = SU.Collection_Unit_Key
		AND SFD.Gathering_Event=1
	LEFT JOIN Occurrence O ON O.Occurrence_Key = SFD.Occurrence_Key
	LEFT JOIN Taxon_Occurrence XO ON XO.Taxon_Occurrence_Key=SFD.Taxon_Occurrence_Key
	INNER JOIN [Sample] S ON S.Sample_Key=O.Sample_Key OR S.Sample_Key = XO.Sample_Key
WHERE 
	dbo.ufn_CBWrapperForDoVagueDatesOverlap(@SearchText, S.Vague_Date_Start, S.Vague_Date_End) = 1


UPDATE @SpecimensSearch
SET	Det_Item_Key=CPref.Concept_Key,
		Item_Name = TPref.Item_Name,
		Number=CUN.Number,
		Det_Item_Name=TDet.Plaintext
FROM @SpecimensSearch SU
LEFT JOIN Collection_Unit_Number CUN ON SU.Item_key = CUN.Collection_Unit_Key 
		AND CUN.Type_Concept_Key = 'SYSTEM0000000001' AND CUN.Preferred=1
INNER JOIN VW_SpecimenDetsEarth SDE ON SDE.Collection_Unit_Key=SU.Item_Key
INNER JOIN Concept C ON C.Concept_Key=SDE.Concept_Key
INNER JOIN Term TDet ON TDet.Term_Key=C.Term_Key
INNER JOIN Concept CPref ON CPref.Meaning_Key=C.Meaning_Key
		AND CPref.List_Preferred=1
		AND CPref.Concept_Group_Key=C.Concept_Group_Key
INNER JOIN Term TPref ON TPref.Term_Key=CPref.Term_Key


UPDATE @SpecimensSearch
SET	Det_Item_Key=SDL.Taxon_List_Item_Key,
		Item_Name =
					dbo.ufn_GetFormattedTaxonNameByParams(
						Preferred_Name,
						Preferred_Name_Italic,
						Common_Name,
						Common_Name_Italic,
						null,
						@ShowCommonNames),
		Number=CUN.Number,
		Det_Item_Name=ITN.Actual_Name,
		Hint=ITN.Actual_Name
FROM @SpecimensSearch SU
LEFT JOIN Collection_Unit_Number CUN ON SU.Item_key = CUN.Collection_Unit_Key 
		AND CUN.Type_Concept_Key = 'SYSTEM0000000001' AND CUN.Preferred=1
INNER JOIN VW_SpecimenDetsLife SDL ON SDL.Collection_Unit_Key=SU.Item_Key
INNER JOIN Index_Taxon_Name ITN	ON ITN.Taxon_List_Item_Key=SDL.Taxon_List_Item_Key

-- Select table and sort appropriately
IF @SortOrderIndex = 0
	SELECT * from @SpecimensSearch
	ORDER BY Det_Item_Name, Number
ELSE IF @SortOrderIndex = 1
	SELECT * from @SpecimensSearch
	ORDER BY Number, Item_Name


GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Specimens_Select_ForSearchByGatheringDate') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Specimens_Select_ForSearchByGatheringDate'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByGatheringDate TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByGatheringDate TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByGatheringDate TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByGatheringDate TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByGatheringDate TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByGatheringDate TO [Dev - JNCC SQL]
END

GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Specimens_Select_ForSearchByGatheringLocation]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Specimens_Select_ForSearchByGatheringLocation]
GO

CREATE PROCEDURE [dbo].[usp_Specimens_Select_ForSearchByGatheringLocation] 
@UserDomainMask INT,
@SessionID CHAR(16),
@ShowCommonNames BIT,
@SearchText VARCHAR(100),
@SortOrderIndex TINYINT

AS

--  DESCRIPTION
--  Returns Specimens data based on the search parameter for Gathering Location
--
--  PARAMETERS
--	NAME				DESCRIPTION
--	@UserDomainMask		User's Domain Mask restricting which records may be returned
--	@SessionID 			User's SessionID
--	@ShowCommonNames	Specifies whether or not Common Names should be shown
--	@SearchText			Text to search On
--	@SortOrderIndex		Index determining Sort Order
--
--
--  AUTHOR:     		Ben Collier, Dorset Software
--  CREATED:    		2003-10-08
--
SET NOCOUNT ON
-- Set of options for better use of vw_ConceptTerm.
SET ANSI_NULLS ON
SET ANSI_PADDING ON
SET ANSI_WARNINGS ON
SET ARITHABORT ON
SET CONCAT_NULL_YIELDS_NULL ON
SET QUOTED_IDENTIFIER ON
SET NO_BROWSETABLE OFF


--Use temp table to build output, so silly data errors such as duplicate accessions
-- don't duplicate in the list
DECLARE @SpecimensSearch TABLE
(
	[Item_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[Det_Item_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[Det_Item_Name] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[Item_Name] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[Life_Sciences] [bit] NULL,
	[Number] [varchar] (30) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[Hint] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL 
)

INSERT INTO 
	@SpecimensSearch (Item_Key, Life_Sciences, Hint) 
SELECT 
	DISTINCT SU.Collection_Unit_Key, SU.Life_Sciences, LN.Item_Name
FROM Specimen_Unit SU
	INNER JOIN Collection_Unit CU ON SU.Collection_Unit_Key = CU.Collection_Unit_Key 
	 	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
		OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
	INNER JOIN Specimen_Field_Data SFD ON SFD.Collection_Unit_Key = SU.Collection_Unit_Key
		AND SFD.Gathering_Event=1
	LEFT JOIN Occurrence O ON O.Occurrence_Key = SFD.Occurrence_Key
	LEFT JOIN Taxon_Occurrence XO ON XO.Taxon_Occurrence_Key=SFD.Taxon_Occurrence_Key
	INNER JOIN [Sample] S ON S.Sample_Key=O.Sample_Key OR S.Sample_Key = XO.Sample_Key
	INNER JOIN [Survey_Event] SE ON SE.Survey_Event_Key=S.Survey_Event_Key
	INNER JOIN Location_Name LN ON LN.Location_Key=S.Location_Key OR LN.Location_KEY=SE.Location_Key
WHERE 
	LN.Item_Name LIKE @SearchText + '%'

UPDATE @SpecimensSearch
SET	Det_Item_Key=CPref.Concept_Key,
		Item_Name = TPref.Item_Name,
		Number=CUN.Number,
		Det_Item_Name=TDet.Plaintext,
		Hint=TDet.Plaintext
FROM @SpecimensSearch SU
LEFT JOIN Collection_Unit_Number CUN ON SU.Item_key = CUN.Collection_Unit_Key 
		AND CUN.Type_Concept_Key = 'SYSTEM0000000001' AND CUN.Preferred=1
INNER JOIN VW_SpecimenDetsEarth SDE ON SDE.Collection_Unit_Key=SU.Item_Key
INNER JOIN Concept C ON C.Concept_Key=SDE.Concept_Key
INNER JOIN Term TDet ON TDet.Term_Key=C.Term_Key
INNER JOIN Concept CPref ON CPref.Meaning_Key=C.Meaning_Key
		AND CPref.List_Preferred=1
		AND CPref.Concept_Group_Key=C.Concept_Group_Key
INNER JOIN Term TPref ON TPref.Term_Key=CPref.Term_Key


UPDATE @SpecimensSearch
SET	Det_Item_Key=SDL.Taxon_List_Item_Key,
		Item_Name =
					dbo.ufn_GetFormattedTaxonNameByParams(
						Preferred_Name,
						Preferred_Name_Italic,
						Common_Name,
						Common_Name_Italic,
						null,
						@ShowCommonNames),
		Number=CUN.Number,
		Det_Item_Name=ITN.Actual_Name,
		Hint=ITN.Actual_Name
FROM @SpecimensSearch SU
LEFT JOIN Collection_Unit_Number CUN ON SU.Item_key = CUN.Collection_Unit_Key 
		AND CUN.Type_Concept_Key = 'SYSTEM0000000001' AND CUN.Preferred=1
INNER JOIN VW_SpecimenDetsLife SDL ON SDL.Collection_Unit_Key=SU.Item_Key
INNER JOIN Index_Taxon_Name ITN	ON ITN.Taxon_List_Item_Key=SDL.Taxon_List_Item_Key


-- Select table and sort appropriately
IF @SortOrderIndex = 0
	SELECT * from @SpecimensSearch
	ORDER BY Det_Item_Name, Number
ELSE IF @SortOrderIndex = 1
	SELECT * from @SpecimensSearch
	ORDER BY Number, Item_Name

GO


/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Specimens_Select_ForSearchByGatheringLocation') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Specimens_Select_ForSearchByGatheringLocation'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByGatheringLocation TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByGatheringLocation TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByGatheringLocation TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByGatheringLocation TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByGatheringLocation TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByGatheringLocation TO [Dev - JNCC SQL]
END

GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Specimens_Select_ForSearchByGeographicInformation]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Specimens_Select_ForSearchByGeographicInformation]
GO

CREATE PROCEDURE [dbo].[usp_Specimens_Select_ForSearchByGeographicInformation] 
@UserDomainMask INT,
@SessionID CHAR(16),
@ShowCommonNames BIT,
@SearchText VARCHAR(100),
@SortOrderIndex TINYINT

AS
--  DESCRIPTION
--  Returns Specimens data based on the search parameter for Geographic Information
--
--  PARAMETERS
--	NAME				DESCRIPTION
--	@UserDomainMask		User's Domain Mask restricting which records may be returned
--	@SessionID 			User's SessionID
--	@ShowCommonNames	Specifies whether or not Common Names should be shown
--	@SortOrderIndex		Index determining Sort Order
--
--
--  AUTHOR:     		Ben Collier, Dorset Software
--  CREATED:    		2003-10-07
--
SET NOCOUNT ON
-- Set of options for better use of vw_ConceptTerm.
SET ANSI_NULLS ON
SET ANSI_PADDING ON
SET ANSI_WARNINGS ON
SET ARITHABORT ON
SET CONCAT_NULL_YIELDS_NULL ON
SET QUOTED_IDENTIFIER ON
SET NO_BROWSETABLE OFF



--Use temp table to build output, so silly data errors such as duplicate accessions
-- don't duplicate in the list
DECLARE @SpecimensSearch TABLE
(
	[Item_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[Det_Item_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[Det_Item_Name] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[Item_Name] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[Life_Sciences] [bit] NULL,
	[Number] [varchar] (30) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[Hint] [text] COLLATE SQL_Latin1_General_CP1_CI_AS NULL 
)

INSERT INTO 
	@SpecimensSearch (Item_Key, Life_Sciences, Hint) 
SELECT 
	SU.Collection_Unit_Key, SU.Life_Sciences, M.Text
FROM Specimen_Unit SU
INNER JOIN Metadata M ON M.Record_Key=SU.Collection_Unit_Key
	AND M.Metadata_Type_Key='SYSTEM0000000005'
INNER JOIN Collection_Unit CU ON SU.Collection_Unit_Key = CU.Collection_Unit_Key 
 	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
	OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
WHERE 
	M.Text LIKE @SearchText + '%'


UPDATE @SpecimensSearch
SET	Det_Item_Key=CPref.Concept_Key,
		Item_Name = TPref.Item_Name,
		Number=CUN.Number,
		Det_Item_Name=TDet.Plaintext
FROM @SpecimensSearch SU
LEFT JOIN Collection_Unit_Number CUN ON SU.Item_key = CUN.Collection_Unit_Key 
		AND CUN.Type_Concept_Key = 'SYSTEM0000000001' AND CUN.Preferred=1
INNER JOIN VW_SpecimenDetsEarth SDE ON SDE.Collection_Unit_Key=SU.Item_Key
INNER JOIN Concept C ON C.Concept_Key=SDE.Concept_Key
INNER JOIN Term TDet ON TDet.Term_Key=C.Term_Key
INNER JOIN Concept CPref ON CPref.Meaning_Key=C.Meaning_Key
		AND CPref.List_Preferred=1
		AND CPref.Concept_Group_Key=C.Concept_Group_Key
INNER JOIN Term TPref ON TPref.Term_Key=CPref.Term_Key


UPDATE @SpecimensSearch
SET	Det_Item_Key=SDL.Taxon_List_Item_Key,
		Item_Name =
					dbo.ufn_GetFormattedTaxonNameByParams(
						Preferred_Name,
						Preferred_Name_Italic,
						Common_Name,
						Common_Name_Italic,
						null,
						@ShowCommonNames),
		Number=CUN.Number,
		Det_Item_Name=ITN.Actual_Name
FROM @SpecimensSearch SU
LEFT JOIN Collection_Unit_Number CUN ON SU.Item_key = CUN.Collection_Unit_Key 
		AND CUN.Type_Concept_Key = 'SYSTEM0000000001' AND CUN.Preferred=1
INNER JOIN VW_SpecimenDetsLife SDL ON SDL.Collection_Unit_Key=SU.Item_Key
INNER JOIN Index_Taxon_Name ITN	ON ITN.Taxon_List_Item_Key=SDL.Taxon_List_Item_Key

-- Select table and sort appropriately
IF @SortOrderIndex = 0
	SELECT * from @SpecimensSearch
	ORDER BY Det_Item_Name, Number
ELSE IF @SortOrderIndex = 1
	SELECT * from @SpecimensSearch
	ORDER BY Number, Item_Name

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Specimens_Select_ForSearchByGeographicInformation') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Specimens_Select_ForSearchByGeographicInformation'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByGeographicInformation TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByGeographicInformation TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByGeographicInformation TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByGeographicInformation TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByGeographicInformation TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByGeographicInformation TO [Dev - JNCC SQL]
END

GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Specimens_Select_ForSearchByPreferredAccNumber]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Specimens_Select_ForSearchByPreferredAccNumber]
GO

CREATE PROCEDURE [dbo].[usp_Specimens_Select_ForSearchByPreferredAccNumber] 
@UserDomainMask INT,
@SessionID CHAR(16),
@ShowCommonNames BIT,
@SearchText VARCHAR(30),
@SortOrderIndex TINYINT

AS

--  DESCRIPTION
--  Returns Specimens based on the Accession Number
--
--  PARAMETERS
--  NAME				DESCRIPTION
--	@UserDomainMask		User's Domain Mask restricting which records may be returned
--	@SessionID 			User's SessionID
--	@ShowCommonNames	Specifies whether or not Common Names should be shown
--	@SearchText			Text to be searched on
--	@SortOrderIndex		Index determining Sort Order
--
--
--  AUTHOR:     Ben Collier, Dorset Software
--  CREATED:    2003-10-07
--
SET NOCOUNT ON
-- Set of options for better use of vw_ConceptTerm.
SET ANSI_NULLS ON
SET ANSI_PADDING ON
SET ANSI_WARNINGS ON
SET ARITHABORT ON
SET CONCAT_NULL_YIELDS_NULL ON
SET QUOTED_IDENTIFIER ON
SET NO_BROWSETABLE OFF


--Use temp table to build output, so silly data errors such as duplicate accessions
-- don't duplicate in the list
DECLARE @SpecimensSearch TABLE
(
	[Item_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[Det_Item_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[Det_Item_Name] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[Item_Name] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[Life_Sciences] [bit] NULL,
	[Number] [varchar] (30) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[Hint] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL 
)

INSERT INTO 
	@SpecimensSearch (Item_Key, Life_Sciences, Hint) 
SELECT DISTINCT SU.Collection_Unit_Key, SU.Life_Sciences, M.Number
FROM SPECIMEN_UNIT SU
INNER JOIN Collection_Unit CU ON SU.Collection_Unit_Key = CU.Collection_Unit_Key 
 	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
	OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
INNER JOIN Movement_Collection_Unit MCU ON MCU.Collection_Unit_Key = SU.Collection_Unit_Key
INNER JOIN Movement_Direction MD ON MD.Movement_Direction_Key=MCU.Movement_Direction_Key
	AND MD.Outbound=0
INNER JOIN Movement_Of_Ownership MOE ON MOE.Movement_Direction_Key=MD.Movement_Direction_Key
LEFT JOIN Movement_Of_Ownership_Exclusion MOEE 
		ON MOEE.Movement_Of_Ownership_Key=MOE.Movement_Of_Ownership_Key
		AND SU.Collection_Unit_Key=MOEE.Collection_Unit_Key
INNER JOIN Movement M ON M.Movement_Key=MD.Movement_Key
	AND M.Movement_Type IN (0,1)
WHERE M.Number LIKE @SearchText + '%'
AND MOEE.Movement_Of_Ownership_Exclusion_Key IS NULL


UPDATE @SpecimensSearch
SET	Det_Item_Key=CPref.Concept_Key,
		Item_Name = TPref.Item_Name,
		Number=CUN.Number,
		Det_Item_Name=TDet.Plaintext
FROM @SpecimensSearch SU
LEFT JOIN Collection_Unit_Number CUN ON SU.Item_key = CUN.Collection_Unit_Key 
		AND CUN.Type_Concept_Key = 'SYSTEM0000000001' AND CUN.Preferred=1
INNER JOIN VW_SpecimenDetsEarth SDE ON SDE.Collection_Unit_Key=SU.Item_Key
INNER JOIN Concept C ON C.Concept_Key=SDE.Concept_Key
INNER JOIN Term TDet ON TDet.Term_Key=C.Term_Key
INNER JOIN Concept CPref ON CPref.Meaning_Key=C.Meaning_Key
		AND CPref.List_Preferred=1
		AND CPref.Concept_Group_Key=C.Concept_Group_Key
INNER JOIN Term TPref ON TPref.Term_Key=CPref.Term_Key


UPDATE @SpecimensSearch
SET	Det_Item_Key=SDL.Taxon_List_Item_Key,
		Item_Name =
					dbo.ufn_GetFormattedTaxonNameByParams(
						Preferred_Name,
						Preferred_Name_Italic,
						Common_Name,
						Common_Name_Italic,
						null,
						@ShowCommonNames),
		Number=CUN.Number,
		Det_Item_Name=ITN.Actual_Name
FROM @SpecimensSearch SU
LEFT JOIN Collection_Unit_Number CUN ON SU.Item_key = CUN.Collection_Unit_Key 
		AND CUN.Type_Concept_Key = 'SYSTEM0000000001' AND CUN.Preferred=1
INNER JOIN VW_SpecimenDetsLife SDL ON SDL.Collection_Unit_Key=SU.Item_Key
INNER JOIN Index_Taxon_Name ITN	ON ITN.Taxon_List_Item_Key=SDL.Taxon_List_Item_Key

-- Select table and sort appropriately
IF @SortOrderIndex = 0
	SELECT * from @SpecimensSearch
	ORDER BY Det_Item_Name, Number
ELSE IF @SortOrderIndex = 1
	SELECT * from @SpecimensSearch
	ORDER BY Number, Item_Name

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Specimens_Select_ForSearchByPreferredAccNumber') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Specimens_Select_ForSearchByPreferredAccNumber'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByPreferredAccNumber TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByPreferredAccNumber TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByPreferredAccNumber TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByPreferredAccNumber TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByPreferredAccNumber TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByPreferredAccNumber TO [Dev - JNCC SQL]
END

GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Specimens_Select_ForSearchByPreferredDetermination]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Specimens_Select_ForSearchByPreferredDetermination]
GO

CREATE PROCEDURE [dbo].[usp_Specimens_Select_ForSearchByPreferredDetermination] 
@UserDomainMask INT,
@SessionID CHAR(16),
@ShowCommonNames BIT,
@SearchText VARCHAR(150),
@SortOrderIndex TINYINT

AS

--  DESCRIPTION
--  Returns Specimens based on the the preferred determination
--
--  PARAMETERS
--  NAME				DESCRIPTION
--	@UserDomainMask		User's Domain Mask restricting which records may be returned
--	@SessionID 			User's SessionID
--	@ShowCommonNames	Specifies whether or not Common Names should be shown
--	@SearchText			Text to be searched on
--	@SortOrderIndex		Index determining Sort Order
--
--
--  AUTHOR:     Ben Collier, Dorset Software
--  CREATED:    2003-10-07
--
SET NOCOUNT ON
-- Set of options for better use of vw_ConceptTerm.
SET ANSI_NULLS ON
SET ANSI_PADDING ON
SET ANSI_WARNINGS ON
SET ARITHABORT ON
SET CONCAT_NULL_YIELDS_NULL ON
SET QUOTED_IDENTIFIER ON
SET NO_BROWSETABLE OFF



--Use temp table to build output, so silly data errors such as duplicate accessions
-- don't duplicate in the list
DECLARE @SpecimensSearch TABLE
(
	[Item_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[Det_Item_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[Det_Item_Name] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[Item_Name] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[Life_Sciences] [bit] NULL,
	[Number] [varchar] (30) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[Hint] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL 
)

--Find all specimens with a determination match

INSERT INTO 
	@SpecimensSearch (Item_Key, Life_Sciences) 
SELECT DISTINCT SU.Collection_Unit_Key, SU.Life_Sciences
FROM SPECIMEN_UNIT SU
INNER JOIN Collection_Unit CU ON SU.Collection_Unit_Key = CU.Collection_Unit_Key 
 	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
	OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
INNER JOIN VW_SpecimenDetsEarth SDE ON (SU.Collection_Unit_Key = SDE.Collection_Unit_Key)
	AND (SDE.Preferred_Determination_Key=SDE.Determination_Key)
INNER JOIN Concept C ON SDE.Concept_Key = C.Concept_Key
INNER JOIN Concept CSearch ON CSearch.Meaning_Key=C.Meaning_Key
INNER JOIN Term TSearch ON TSearch.Term_Key=CSearch.Term_Key
WHERE (TSearch.Plaintext LIKE @SearchText + '%' AND SU.Life_Sciences=0) 

INSERT INTO 
	@SpecimensSearch (Item_Key, Life_Sciences) 
SELECT DISTINCT SU.Collection_Unit_Key, SU.Life_Sciences
FROM SPECIMEN_UNIT SU
INNER JOIN Collection_Unit CU ON SU.Collection_Unit_Key = CU.Collection_Unit_Key 
 	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
	OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
INNER JOIN VW_SpecimenDetsLife SDL ON SU.Collection_Unit_Key = SDL.Collection_Unit_Key
	AND (SDL.Preferred_Taxon_Determination_Key=SDL.Taxon_Determination_Key)
INNER JOIN Index_Taxon_Synonym ITS ON ITS.Taxon_List_Item_Key=SDL.Taxon_List_Item_Key
INNER JOIN INDEX_TAXON_NAME ITN	ON ITS.Synonym_List_Item_Key = ITN.Taxon_List_Item_Key
WHERE (ITN.Actual_Name LIKE @SearchText + '%' AND SU.Life_Sciences=1)


UPDATE @SpecimensSearch
SET	Det_Item_Key=CPref.Concept_Key,
		Item_Name = TPref.Item_Name,
		Number=CUN.Number,
		Det_Item_Name=TDet.Plaintext,
		Hint=TDet.Plaintext
FROM @SpecimensSearch SU
LEFT JOIN Collection_Unit_Number CUN ON SU.Item_key = CUN.Collection_Unit_Key 
		AND CUN.Type_Concept_Key = 'SYSTEM0000000001' AND CUN.Preferred=1
INNER JOIN VW_SpecimenDetsEarth SDE ON SDE.Collection_Unit_Key=SU.Item_Key
INNER JOIN Concept C ON C.Concept_Key=SDE.Concept_Key
INNER JOIN Term TDet ON TDet.Term_Key=C.Term_Key
INNER JOIN Concept CPref ON CPref.Meaning_Key=C.Meaning_Key
		AND CPref.List_Preferred=1
		AND CPref.Concept_Group_Key=C.Concept_Group_Key
INNER JOIN Term TPref ON TPref.Term_Key=CPref.Term_Key


UPDATE @SpecimensSearch
SET	Det_Item_Key=SDL.Taxon_List_Item_Key,
		Item_Name =
					dbo.ufn_GetFormattedTaxonNameByParams(
						Preferred_Name,
						Preferred_Name_Italic,
						Common_Name,
						Common_Name_Italic,
						null,
						@ShowCommonNames),
		Number=CUN.Number,
		Det_Item_Name=ITN.Actual_Name,
		Hint=ITN.Actual_Name
FROM @SpecimensSearch SU
LEFT JOIN Collection_Unit_Number CUN ON SU.Item_key = CUN.Collection_Unit_Key 
		AND CUN.Type_Concept_Key = 'SYSTEM0000000001' AND CUN.Preferred=1
INNER JOIN VW_SpecimenDetsLife SDL ON SDL.Collection_Unit_Key=SU.Item_Key
INNER JOIN Index_Taxon_Name ITN	ON ITN.Taxon_List_Item_Key=SDL.Taxon_List_Item_Key

-- Select table and sort appropriately
IF @SortOrderIndex = 0
	SELECT * from @SpecimensSearch
	ORDER BY Det_Item_Name, Number
ELSE IF @SortOrderIndex = 1
	SELECT * from @SpecimensSearch
	ORDER BY Number, Det_Item_Name


GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Specimens_Select_ForSearchByPreferredDetermination') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Specimens_Select_ForSearchByPreferredDetermination'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByPreferredDetermination TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByPreferredDetermination TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByPreferredDetermination TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByPreferredDetermination TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByPreferredDetermination TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByPreferredDetermination TO [Dev - JNCC SQL]
END

GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Specimens_Select_ForSearchByPreferredRegNumber]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Specimens_Select_ForSearchByPreferredRegNumber]
GO

CREATE PROCEDURE [dbo].[usp_Specimens_Select_ForSearchByPreferredRegNumber] 
@UserDomainMask INT,
@SessionID CHAR(16),
@ShowCommonNames BIT,
@SearchText VARCHAR(30),
@SortOrderIndex TINYINT

AS
--  DESCRIPTION
--  Returns Specimens data based on the search parameter for Preferred Reg Number
--
--  PARAMETERS
--	NAME				DESCRIPTION
--	@UserDomainMask		User's Domain Mask restricting which records may be returned
--	@SessionID 			User's SessionID
--	@ShowCommonNames	Specifies whether or not Common Names should be shown
--	@SearchText			Text to search On
--	@SortOrderIndex		Index determining Sort Order
--
--
--  AUTHOR:     		Ben Collier, Dorset Software
--  CREATED:    		2003-10-07
--
SET NOCOUNT ON
-- Set of options for better use of vw_ConceptTerm.
SET ANSI_NULLS ON
SET ANSI_PADDING ON
SET ANSI_WARNINGS ON
SET ARITHABORT ON
SET CONCAT_NULL_YIELDS_NULL ON
SET QUOTED_IDENTIFIER ON
SET NO_BROWSETABLE OFF


--Use temp table to build output, so silly data errors such as duplicate accessions
-- don't duplicate in the list
DECLARE @SpecimensSearch TABLE
(
	[Item_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[Det_Item_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[Det_Item_Name] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[Item_Name] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[Life_Sciences] [bit] NULL,
	[Number] [varchar] (30) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[Hint] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL 
)

INSERT INTO 
	@SpecimensSearch (Item_Key, Life_Sciences, Hint) 
SELECT DISTINCT SU.Collection_Unit_Key, SU.Life_Sciences, CUN.Number
FROM Specimen_Unit SU
INNER JOIN Collection_Unit CU ON SU.Collection_Unit_Key = CU.Collection_Unit_Key 
   	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
		OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
INNER JOIN Collection_Unit_Number CUN ON SU.Collection_Unit_key = CUN.Collection_Unit_Key 
	AND CUN.Type_Concept_Key = 'SYSTEM0000000001'  --REGISTRATION NUMBER
	AND CUN.Preferred = 1
	AND CUN.Number LIKE @SearchText + '%'

UPDATE @SpecimensSearch
SET	Det_Item_Key=CPref.Concept_Key,
		Item_Name = TPref.Item_Name,
		Number=CUN.Number,
		Det_Item_Name=TDet.Plaintext
FROM @SpecimensSearch SU
LEFT JOIN Collection_Unit_Number CUN ON SU.Item_key = CUN.Collection_Unit_Key 
		AND CUN.Type_Concept_Key = 'SYSTEM0000000001' AND CUN.Preferred=1
INNER JOIN VW_SpecimenDetsEarth SDE ON SDE.Collection_Unit_Key=SU.Item_Key
INNER JOIN Concept C ON C.Concept_Key=SDE.Concept_Key
INNER JOIN Term TDet ON TDet.Term_Key=C.Term_Key
INNER JOIN Concept CPref ON CPref.Meaning_Key=C.Meaning_Key
		AND CPref.List_Preferred=1
		AND CPref.Concept_Group_Key=C.Concept_Group_Key
INNER JOIN Term TPref ON TPref.Term_Key=CPref.Term_Key


UPDATE @SpecimensSearch
SET	Det_Item_Key=SDL.Taxon_List_Item_Key,
		Item_Name =
					dbo.ufn_GetFormattedTaxonNameByParams(
						Preferred_Name,
						Preferred_Name_Italic,
						Common_Name,
						Common_Name_Italic,
						null,
						@ShowCommonNames),
		Number=CUN.Number,
		Det_Item_Name=ITN.Actual_Name
FROM @SpecimensSearch SU
LEFT JOIN Collection_Unit_Number CUN ON SU.Item_key = CUN.Collection_Unit_Key 
		AND CUN.Type_Concept_Key = 'SYSTEM0000000001' AND CUN.Preferred=1
INNER JOIN VW_SpecimenDetsLife SDL ON SDL.Collection_Unit_Key=SU.Item_Key
INNER JOIN Index_Taxon_Name ITN	ON ITN.Taxon_List_Item_Key=SDL.Taxon_List_Item_Key

-- Select table and sort appropriately
IF @SortOrderIndex = 0
	SELECT * from @SpecimensSearch
	ORDER BY Det_Item_Name, Number
ELSE IF @SortOrderIndex = 1
	SELECT * from @SpecimensSearch
	ORDER BY Number, Item_Name

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Specimens_Select_ForSearchByPreferredRegNumber') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Specimens_Select_ForSearchByPreferredRegNumber'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByPreferredRegNumber TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByPreferredRegNumber TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByPreferredRegNumber TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByPreferredRegNumber TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByPreferredRegNumber TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByPreferredRegNumber TO [Dev - JNCC SQL]
END

GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Specimens_Select_ForSearchByType]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Specimens_Select_ForSearchByType]
GO

CREATE PROCEDURE [dbo].[usp_Specimens_Select_ForSearchByType] 
@UserDomainMask INT,
@SessionID CHAR(16),
@ShowCommonNames BIT,
@SearchText VARCHAR(150),
@SortOrderIndex TINYINT

AS
--  DESCRIPTION
--  Returns Specimens data based on the search parameter for Specimen Type
--
--  PARAMETERS
--	NAME				DESCRIPTION
--	@UserDomainMask		User's Domain Mask restricting which records may be returned
--	@SessionID 			User's SessionID
--	@ShowCommonNames	Specifies whether or not Common Names should be shown
--	@SortOrderIndex		Index determining Sort Order
--
--
--  AUTHOR:     		Ben Collier, Dorset Software
--  CREATED:    		2003-10-07
--
SET NOCOUNT ON
-- Set of options for better use of vw_ConceptTerm.
SET ANSI_NULLS ON
SET ANSI_PADDING ON
SET ANSI_WARNINGS ON
SET ARITHABORT ON
SET CONCAT_NULL_YIELDS_NULL ON
SET QUOTED_IDENTIFIER ON
SET NO_BROWSETABLE OFF


--Use temp table to build output, so silly data errors such as duplicate accessions
-- don't duplicate in the list
DECLARE @SpecimensSearch TABLE
(
	[Item_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[Det_Item_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[Det_Item_Name] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[Item_Name] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[Life_Sciences] [bit] NULL,
	[Number] [varchar] (30) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[Hint] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL 
)

INSERT INTO 
	@SpecimensSearch (Item_Key, Life_Sciences, Hint) 
SELECT DISTINCT SU.Collection_Unit_Key, SU.Life_Sciences, T.Plaintext
FROM SPECIMEN_UNIT SU
INNER JOIN Collection_Unit CU ON SU.Collection_Unit_Key = CU.Collection_Unit_Key 
 	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
	OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
INNER JOIN Concept C ON C.Concept_Key = SU.Specimen_Type_Concept_Key
INNER JOIN Concept CSyn ON CSyn.Meaning_Key=C.Meaning_Key
INNER JOIN Term T ON T.Term_Key=CSyn.Term_Key
WHERE T.Plaintext LIKE @Searchtext + '%'

UPDATE @SpecimensSearch
SET	Det_Item_Key=CPref.Concept_Key,
		Item_Name = TPref.Item_Name,
		Number=CUN.Number,
		Det_Item_Name=TDet.Plaintext
FROM @SpecimensSearch SU
LEFT JOIN Collection_Unit_Number CUN ON SU.Item_key = CUN.Collection_Unit_Key 
		AND CUN.Type_Concept_Key = 'SYSTEM0000000001' AND CUN.Preferred=1
INNER JOIN VW_SpecimenDetsEarth SDE ON SDE.Collection_Unit_Key=SU.Item_Key
INNER JOIN Concept C ON C.Concept_Key=SDE.Concept_Key
INNER JOIN Term TDet ON TDet.Term_Key=C.Term_Key
INNER JOIN Concept CPref ON CPref.Meaning_Key=C.Meaning_Key
		AND CPref.List_Preferred=1
		AND CPref.Concept_Group_Key=C.Concept_Group_Key
INNER JOIN Term TPref ON TPref.Term_Key=CPref.Term_Key


UPDATE @SpecimensSearch
SET	Det_Item_Key=SDL.Taxon_List_Item_Key,
		Item_Name =
					dbo.ufn_GetFormattedTaxonNameByParams(
						Preferred_Name,
						Preferred_Name_Italic,
						Common_Name,
						Common_Name_Italic,
						null,
						@ShowCommonNames),
		Number=CUN.Number,
		Det_Item_Name=ITN.Actual_Name
FROM @SpecimensSearch SU
LEFT JOIN Collection_Unit_Number CUN ON SU.Item_key = CUN.Collection_Unit_Key 
		AND CUN.Type_Concept_Key = 'SYSTEM0000000001' AND CUN.Preferred=1
INNER JOIN VW_SpecimenDetsLife SDL ON SDL.Collection_Unit_Key=SU.Item_Key
INNER JOIN Index_Taxon_Name ITN	ON ITN.Taxon_List_Item_Key=SDL.Taxon_List_Item_Key

-- Select table and sort appropriately
IF @SortOrderIndex = 0
	SELECT * from @SpecimensSearch
	ORDER BY Det_Item_Name, Number
ELSE IF @SortOrderIndex = 1
	SELECT * from @SpecimensSearch
	ORDER BY Number, Item_Name

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Specimens_Select_ForSearchByType') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Specimens_Select_ForSearchByType'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByType TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByType TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByType TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByType TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByType TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByType TO [Dev - JNCC SQL]
END

GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Specimens_Select_ForStore]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Specimens_Select_ForStore]
GO

/*===========================================================================*\
  Description:	Returns Specimens data to the CollectionsBrowser for a given Store

  Parameters:	
	@ParentKey 		When specified, only the records associated with the parent key are returned
	@UserDomainMask		User's Domain Mask restricting which records may be returned
	@SessionID 		User's SessionID
	@ShowCommonNames	Specifies whether or not Common Names should be shown
	@SortOrderIndex		Index determining Sort Order

  Created:	October 2003

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Specimens_Select_ForStore] 
	@UserDomainMask INT,
	@SessionID CHAR(16),
	@ParentKey CHAR(16),
	@ShowCommonNames BIT,
	@SortOrderIndex TINYINT
AS

SET NOCOUNT ON
-- Set of options for better use of vw_ConceptTerm.
SET ANSI_NULLS ON
SET ANSI_PADDING ON
SET ANSI_WARNINGS ON
SET ARITHABORT ON
SET CONCAT_NULL_YIELDS_NULL ON
SET QUOTED_IDENTIFIER ON
SET NO_BROWSETABLE OFF


	-- Use temp table to build output, so silly data errors such as duplicate accessions
	-- don't duplicate in the list
	DECLARE @SpecimensSearch TABLE
	(
		[Item_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[Join_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[Det_Item_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[Det_Item_Name] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[Item_Name] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[Life_Sciences] [bit] NULL,
		[Number] [varchar] (30) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[Hint] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL 
	)

	INSERT INTO @SpecimensSearch (Item_Key, Join_Key, Life_Sciences) 
	SELECT DISTINCT SU.Collection_Unit_Key, SU.Collection_Unit_Key, SU.Life_Sciences
	FROM 		Specimen_Unit SU
	INNER JOIN 	Collection_Unit CU 
		ON SU.Collection_Unit_Key = CU.Collection_Unit_Key 
		AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
			OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
	WHERE 	CU.Current_Container_Collection_Unit_Key = @ParentKey


	UPDATE @SpecimensSearch
	SET	Det_Item_Key =
			CASE WHEN SU.Life_Sciences = 0 THEN CPref.Concept_Key ELSE TD.Taxon_List_Item_Key END,
		Item_Name = 
			CASE 
				WHEN SU.Life_Sciences = 0 THEN TPref.Item_Name 
				ELSE dbo.ufn_GetFormattedTaxonNameByParams(Preferred_Name, Preferred_Name_Italic,
					Common_Name, Common_Name_Italic, NULL, @ShowCommonNames)
			END,
		Number = CUN.Number,
		Det_Item_Name =
			CASE WHEN SU.Life_Sciences = 0 THEN TDet.Plaintext ELSE ITN.Actual_Name END

	FROM 		@SpecimensSearch SU
	LEFT JOIN 	Collection_Unit_Number CUN ON SU.Item_key = CUN.Collection_Unit_Key AND CUN.Type_Concept_Key = 'SYSTEM0000000001'
	INNER JOIN 	Specimen_Unit SUnit ON SUnit.Collection_Unit_Key=SU.Item_Key
	LEFT JOIN 	Determination D ON D.Determination_Key=SUnit.Preferred_Determination_Key
	LEFT JOIN 	Concept C ON C.Concept_Key=D.Concept_Key
	LEFT JOIN 	Term TDet ON TDet.Term_Key=C.Term_Key
	LEFT JOIN 	Concept CPref ON CPref.Meaning_Key=C.Meaning_Key AND CPref.List_Preferred=1 AND CPref.Concept_Group_Key=C.Concept_Group_Key
	LEFT JOIN 	Term TPref ON TPref.Term_Key=CPref.Term_Key
	LEFT JOIN 	Taxon_Determination TD ON SU.Item_Key = TD.Specimen_Collection_Unit_Key
	LEFT JOIN 	Index_Taxon_Name ITN	ON ITN.Taxon_List_Item_Key=TD.Taxon_List_Item_Key

-- Select table and sort appropriately
IF @SortOrderIndex = 0
	SELECT * FROM @SpecimensSearch
	ORDER BY Item_Name, Number
ELSE 
IF @SortOrderIndex = 1
	SELECT * FROM @SpecimensSearch
	ORDER BY Number, Item_Name
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Specimens_Select_ForStore') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Specimens_Select_ForStore'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForStore TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForStore TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForStore TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForStore TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForStore TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForStore TO [Dev - JNCC SQL]
END
GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Specimens_Select_ForTopLevel]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Specimens_Select_ForTopLevel]
GO

CREATE PROCEDURE [dbo].[usp_Specimens_Select_ForTopLevel] 
@UserDomainMask BIGINT,
@SessionID CHAR(16),
@ShowCommonNames BIT,
@SortOrderIndex TINYINT,
@Key CHAR(16) = NULL
AS

--  DESCRIPTION
--  Returns top level Specimens data to the CollectionsBrowser
--
--  PARAMETERS
--	NAME				DESCRIPTION
--	@Key 				Optional Key. When specified, only the single top level record is returned with that key
--	@UserDomainMask		User's Domain Mask restricting which records may be returned
--	@SessionID 			User's SessionID
--	@ShowCommonNames	Specifies whether or not Common Names should be shown
--	@SortOrderIndex		Index determining Sort Order
--
--
--  AUTHOR:     		Ben Collier, Dorset Software
--  CREATED:    		2003-08-14
--
SET NOCOUNT ON
-- Set of options for better use of vw_ConceptTerm.
SET ANSI_NULLS ON
SET ANSI_PADDING ON
SET ANSI_WARNINGS ON
SET ARITHABORT ON
SET CONCAT_NULL_YIELDS_NULL ON
SET QUOTED_IDENTIFIER ON
SET NO_BROWSETABLE OFF


--Use temp table to build output, so silly data errors such as duplicate accessions
-- don't duplicate in the list
DECLARE @Search TABLE
(
	[Item_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[Det_Item_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[Det_Item_Name] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[Item_Name] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[Life_Sciences] [bit] NULL,
	[Number] [varchar] (30) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[Hint] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL 
)

IF @Key IS NULL
	IF object_id('tempdb..#TempFilter') is not null
		-- Display data for a list of keys in the #TempFilter table
		INSERT INTO 
			@Search (Item_Key, Life_Sciences) 
		SELECT DISTINCT SU.Collection_Unit_Key, SU.Life_Sciences
		FROM SPECIMEN_UNIT SU
		INNER JOIN Collection_Unit CU ON SU.Collection_Unit_Key = CU.Collection_Unit_Key 
	   	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
			OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
		INNER JOIN #TempFilter ON #TempFilter.ItemKey=CU.Collection_Unit_Key
	ELSE
		INSERT INTO 
			@Search (Item_Key, Life_Sciences) 
		SELECT DISTINCT SU.Collection_Unit_Key, SU.Life_Sciences
		FROM SPECIMEN_UNIT SU
		INNER JOIN Collection_Unit CU ON SU.Collection_Unit_Key = CU.Collection_Unit_Key 
	   	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
			OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
ELSE
	-- Display data for a single key
	INSERT INTO 
		@Search (Item_Key, Life_Sciences) 
	SELECT DISTINCT SU.Collection_Unit_Key, SU.Life_Sciences
	FROM SPECIMEN_UNIT SU
	INNER JOIN Collection_Unit CU ON SU.Collection_Unit_Key = CU.Collection_Unit_Key 
   	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
		OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
	WHERE SU.Collection_Unit_Key=@Key

UPDATE @Search
SET	Det_Item_Key=CPref.Concept_Key,
		Item_Name = TPref.Item_Name,
		Number=CUN.Number,
		Det_Item_Name=TDet.Plaintext
FROM @Search SU
LEFT JOIN Collection_Unit_Number CUN ON SU.Item_key = CUN.Collection_Unit_Key 
		AND CUN.Type_Concept_Key = 'SYSTEM0000000001' AND CUN.Preferred=1
INNER JOIN VW_SpecimenDetsEarth SDE ON SDE.Collection_Unit_Key=SU.Item_Key
INNER JOIN Concept C ON C.Concept_Key=SDE.Concept_Key
INNER JOIN Term TDet ON TDet.Term_Key=C.Term_Key
INNER JOIN Concept CPref ON CPref.Meaning_Key=C.Meaning_Key
		AND CPref.List_Preferred=1
		AND CPref.Concept_Group_Key=C.Concept_Group_Key
INNER JOIN Term TPref ON TPref.Term_Key=CPref.Term_Key


UPDATE @Search
SET	Det_Item_Key=SDL.Taxon_List_Item_Key,
		Item_Name =
					dbo.ufn_GetFormattedTaxonNameByParams(
						Preferred_Name,
						Preferred_Name_Italic,
						Common_Name,
						Common_Name_Italic,
						null,
						@ShowCommonNames),
		Number=CUN.Number,
		Det_Item_Name=ITN.Actual_Name
FROM @Search SU
LEFT JOIN Collection_Unit_Number CUN ON SU.Item_key = CUN.Collection_Unit_Key 
		AND CUN.Type_Concept_Key = 'SYSTEM0000000001' AND CUN.Preferred=1
INNER JOIN VW_SpecimenDetsLife SDL ON SDL.Collection_Unit_Key=SU.Item_Key
INNER JOIN Index_Taxon_Name ITN	ON ITN.Taxon_List_Item_Key=SDL.Taxon_List_Item_Key

-- Select table and sort appropriately
IF @SortOrderIndex = 0
	SELECT * from @Search
	ORDER BY Det_Item_Name, Number
ELSE IF @SortOrderIndex = 1
	SELECT * from @Search
	ORDER BY Number, Item_Name

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Specimens_Select_ForTopLevel') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Specimens_Select_ForTopLevel'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForTopLevel TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForTopLevel TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForTopLevel TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForTopLevel TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForTopLevel TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForTopLevel TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_SpecimenTypes_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_SpecimenTypes_Select]
GO

GO
/*===========================================================================*\
  Description:	Returns the Specimen Types for the SpecimenGeneral frame.

  Parameters:	@Mask	The Specimen Unit Mask value

  Created:	September 2003

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SpecimenTypes_Select]
	@Mask int
AS

-- Set of options for better use of vw_ConceptTerm.
SET ANSI_NULLS ON
SET ANSI_PADDING ON
SET ANSI_WARNINGS ON
SET ARITHABORT ON
SET CONCAT_NULL_YIELDS_NULL ON
SET QUOTED_IDENTIFIER ON
SET NO_BROWSETABLE OFF
SET NOCOUNT ON

DECLARE @LocalKey CHAR(16)
SELECT @LocalKey=CG.Concept_Group_Key
FROM concept_group cg 
LEFT JOIN local_domain ld ON ld.local_domain_key = cg.local_domain_Key
LEFT JOIN domain d ON d.domain_key = ld.domain_key
WHERE D.Domain_Mask & @Mask > 0
	AND CG.Item_Name = 'Specimen Type'

SELECT CT.concept_key, CT.plaintext
	FROM VW_ConceptTerm CT 
WHERE CT.concept_group_key IN ('SYSTEM000000000J', @LocalKey)
ORDER BY CT.Plaintext

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SpecimenTypes_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SpecimenTypes_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SpecimenTypes_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SpecimenTypes_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SpecimenTypes_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_SpecimenTypes_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SpecimenTypes_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SpecimenTypes_Select TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Specimen_Delete') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_Specimen_Delete]
GO

/*===========================================================================*\
  Description:	Delete a record from the Specimen table.
		Ensures the Domain masks of the containing itmes are also updated.
		The assumpton is that a specimen being deleted is not a 
		container for any other specimens. They must have been either 
		deleted or moved to another container themselves.

  Parameters:	@SpecimenKey

  Created:	July 2003

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Specimen_Delete]
	@SpecimenKey char(16),
	@Timestamp timestamp = NULL
AS
	DECLARE @ExistingCollectionKey char(16),
		@ExistingContainerKey char(16),
		@SpecimenMask int,
		@SUTimestamp timestamp

	/*-------------------------------------------------------------*\
	| Initialise variables.						|
	\*-------------------------------------------------------------*/
	SELECT		@ExistingCollectionKey = S.Parent_Collection_Collection_Unit_Key, 
			@ExistingContainerKey = CU.Current_Container_Collection_Unit_Key
	FROM		Specimen_Unit S
	INNER JOIN 	Collection_Unit CU ON CU.Collection_Unit_Key = S.Collection_Unit_Key
	WHERE		S.Collection_Unit_Key = @SpecimenKey

	-- Retrieve the mask of the preferred concept for specimen.
	EXECUTE	usp_Get_Concept_Domain_Mask_From_Specimen @SpecimenKey, @SpecimenMask OUTPUT

	/*-------------------------------------------------------------*\
	| Do the table delete first. Or the containers will still have	|
	| the specimen and its mask!					|
	\*-------------------------------------------------------------*/

	BEGIN TRANSACTION
		/*-------------------------------------------------------------*\
		| Before continuing, we need to save the timestamp, because the |
		| update that follows this, could update the timestamp itself	|
		| causing the final delete to fail.				|
		\*-------------------------------------------------------------*/
		SELECT	@SUTimestamp = [Timestamp]
		FROM	Specimen_Unit
		WHERE	Collection_Unit_Key = @SpecimenKey
		/*---------------------------------------------------------------------------*\
		  The Determination table has a relationship to the Specimen_Unit table and 
		  the Specimen_Unit has a relationship to the Determination table (similarly
		  for the Taxon_Determination table). Hence, the Preferred_Determination and
		  Preferred_Taxon_Determination fields in the Specimen_Unit table must be
		  both made NULL before they can be deleted.
		\*---------------------------------------------------------------------------*/
		UPDATE	Specimen_Unit
		SET	Preferred_Determination_Key = NULL,
			Preferred_Taxon_Determination_Key = NULL
		WHERE	Collection_Unit_Key = @SpecimenKey

		/*---------------------------------------------------------------------------*\
		  Specimen_Unit -> Determination -> Occurrence
		\*---------------------------------------------------------------------------*/
		-- Delete Determinations linked ONLY to Specimen being deleted.
		DELETE	Determination
		WHERE	Specimen_Collection_Unit_Key = @SpecimenKey
		AND	Occurrence_Key IS NULL

		-- Clear only reference to Specimen for Determinations also linked to Occurrence
		UPDATE	Determination
		SET	Specimen_Collection_Unit_Key = NULL
		WHERE	Specimen_Collection_Unit_Key = @SpecimenKey
		AND	Occurrence_Key IS NOT NULL

		IF @@Error <> 0 GOTO RollbackAndExit

		/*---------------------------------------------------------------------------*\
		  Specimen_Unit -> Taxon_Determination -> Taxon_Occurrence
		\*---------------------------------------------------------------------------*/
		-- Delete Taxon Determinations linked ONLY to Specimen being deleted.
		DELETE	Taxon_Determination
		WHERE	Specimen_Collection_Unit_Key = @SpecimenKey
		AND	Taxon_Occurrence_Key IS NULL

		-- Clear only reference to Specimen for Taxon Determinations also linked to Taxon Occurrence
		UPDATE 	Taxon_Determination
		SET	Specimen_Collection_Unit_Key = NULL
		WHERE	Specimen_Collection_Unit_Key = @SpecimenKey
		AND	Taxon_Occurrence_Key IS NOT NULL

		IF @@Error <> 0 GOTO RollbackAndExit

		/*-------------------------------------------------------------*\
		  Other tables.
		\*-------------------------------------------------------------*/
		DELETE	Specimen_Field_Data
		WHERE	Collection_Unit_Key = @SpecimenKey

		IF @@Error <> 0 GOTO RollbackAndExit

		DELETE	Specimen_Label		
		WHERE	Collection_Unit_Key = @SpecimenKey	

		IF @@Error <> 0 GOTO RollbackAndExit	

		DELETE	Specimen_Unit
		WHERE	Collection_Unit_Key = @SpecimenKey
		AND	((@Timestamp = @SUTimestamp) OR (@Timestamp IS NULL))

		IF @@Error <> 0 GOTO RollbackAndExit

		/*-------------------------------------------------------------*\
		 @Timestamp is null if proc called from a store related process. 
		 In that case , the Collection_Unit table will be handled by the 
		 store related process that called the proc, and not here.
		\*-------------------------------------------------------------*/
		IF @Timestamp IS NOT NULL
			-- If specimen is also a store, let the dedicated procedure deal with it
			IF EXISTS(SELECT * FROM Store WHERE Collection_Unit_Key = @SpecimenKey)
				EXECUTE	usp_Store_Delete @SpecimenKey
			ELSE
			-- Otherwise, just delete record from Collection_Unit table.
				DELETE	Collection_Unit
				WHERE	Collection_Unit_Key = @SpecimenKey

		IF @@Error <> 0 GOTO RollbackAndExit

		/*-------------------------------------------------------------*\
		  Now switch specimen bit OFF from container and collection.
		\*-------------------------------------------------------------*/
		-- Update the container mask
		EXECUTE	usp_CollectionUnit_Update_DomainMask @ExistingContainerKey, @SpecimenMask, 0
		-- Update the collection mask
		EXECUTE	usp_Collection_Update_DomainMask @ExistingCollectionKey, @SpecimenMask, 0

		IF @@Error <> 0 GOTO RollbackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Specimen_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Specimen_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Specimen_Delete TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimen_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimen_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimen_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Specimen_Delete TO [Dev - JNCC SQL]
END
GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_StoragePlace_Select_ForCollectionUnit]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_StoragePlace_Select_ForCollectionUnit]
GO

/*===========================================================================*\
  Description:	Returns successive parent Stores for a specified Store

  Parameters:	@ParentKey 	Only the records associated with the parent key are returned

  Created:	August 2003

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $
\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_StoragePlace_Select_ForCollectionUnit] 
	@UserDomainMask INT,
	@SessionID CHAR(16),
	@ParentKey CHAR(16)
AS

SET NOCOUNT ON

	DECLARE @StoragePlace TABLE
	(
		[Item_Index] [int] NOT NULL,
		[Collection_Unit_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
		[Join_Key][char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
		[Item_Name] [varchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
		[Number] [varchar] (30) NULL,
		[Type] [varchar] (100) NULL,
		[Code] [varchar] (30)
	)
	
	
	DECLARE @Collection_Unit_Key CHAR(16), 
		@Parent_Collection_Unit_Key CHAR(16), 
		@Item_Name VARCHAR(100), 
		@Number varchar(30), 
		@Item_Index INT,
		@Store_Code VARCHAR(30),
		@Store_Type varchar(100)

	--Obtain parent of input store
	SELECT	@Parent_Collection_Unit_Key = CU.Current_Container_Collection_Unit_Key
	FROM 	Collection_Unit CU
	WHERE	((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
		OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
	AND CU.Collection_Unit_Key = @ParentKey

	SET @Item_Index = 0

	--Obtain successive parents
	WHILE @Parent_Collection_Unit_Key IS NOT NULL
	BEGIN
		SELECT	@Collection_Unit_Key = S.Collection_Unit_Key, 
			@Item_Name = S.Item_Name, 
			@Number = CUN.Number, 
			@Parent_Collection_Unit_Key = CU.Current_Container_Collection_Unit_Key,
			@Store_Code = CU.Current_Location_Code,
			@Store_Type = CT.Plaintext

		FROM 	Store S
		INNER JOIN	Collection_Unit CU
			ON S.Collection_Unit_Key = CU.Collection_Unit_Key
			AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
				OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
			AND S.Collection_Unit_Key = @Parent_Collection_Unit_Key

		LEFT JOIN 	Collection_Unit_Number CUN 
			ON S.Collection_Unit_Key = CUN.Collection_Unit_Key 
			AND CUN.Type_Concept_Key = 'SYSTEM0000000001'  --REGISTRATION NUMBER
			AND CUN.Preferred = 1
		LEFT JOIN	vw_ConceptTermPreferred AS CT ON CT.Concept_key = S.Store_Type_Concept_Key
	
		--Ensure we are not joining back to a previous store
		IF (@Item_Name IS NOT NULL) AND (@Collection_Unit_Key <> @ParentKey)
		AND NOT EXISTS(SELECT * FROM @StoragePlace WHERE Collection_Unit_Key = @Collection_Unit_Key)
		BEGIN
			INSERT @StoragePlace (Item_Index, Collection_Unit_Key, Join_Key, Item_Name, Number, Code, Type) 
			VALUES (@Item_Index, @Collection_Unit_Key, @Collection_Unit_Key, @Item_Name, @Number, @Store_Code, @Store_Type)

			SET @Item_Index = @Item_Index + 1
		END ELSE
			BREAK
	END

	--Return hierarchical list
	SELECT 	Collection_Unit_Key AS Item_Key, Join_Key, Item_Name, Number, Code, Type
	FROM 	@StoragePlace
	ORDER BY Item_Index DESC
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_StoragePlace_Select_ForCollectionUnit') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_StoragePlace_Select_ForCollectionUnit'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_StoragePlace_Select_ForCollectionUnit TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_StoragePlace_Select_ForCollectionUnit TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_StoragePlace_Select_ForCollectionUnit TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_StoragePlace_Select_ForCollectionUnit TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_StoragePlace_Select_ForCollectionUnit TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_StoragePlace_Select_ForCollectionUnit TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_StoreDetailsMemo_Get]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_StoreDetailsMemo_Get]
GO
 
/*===========================================================================*\
  Description:	Returns a the details memo content for the Store general tab

  Parameters:	@Key	Collection_Unit key
							@Output - output.  Note this is limited to 8000 characters.

  Created:	Oct 2003

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_StoreDetailsMemo_Get]
	@Key char(16),
	@Output varchar(8000) OUTPUT
AS

SET NOCOUNT ON

--Async query to get details memo content
DECLARE @Value VARCHAR(500) 

DECLARE data_cursor CURSOR FORWARD_ONLY FOR 
	SELECT CT.Plaintext COLLATE SQL_Latin1_General_CP1_CI_AS + ' ' + D.Lower_Value AS Value
	FROM Collection_Unit_Data D
	INNER JOIN VW_ConceptTerm CT ON CT.Concept_Key=D.Parameter_Concept_Key
	WHERE D.Collection_Unit_Key=@Key
	AND D.Is_Descriptor=1

OPEN data_cursor

FETCH NEXT FROM data_cursor INTO @Value

SET @Output = ''

WHILE @@Fetch_Status=0
BEGIN
  --New line if required
	IF @Output <> '' 
		SET @Output=@Output+CHAR(13)+CHAR(10)
  SET @Output=@Output+@Value	
	FETCH NEXT FROM data_cursor INTO @Value
END

CLOSE data_cursor
DEALLOCATE data_cursor

DECLARE data_cursor CURSOR FORWARD_ONLY FOR 
	SELECT 
		CASE WHEN D.Upper_Value IS NULL THEN
			D.Lower_Value 
		ELSE
			D.Lower_Value + ' - ' + D.Upper_Value
		END +
    CASE WHEN CTU.Plaintext IS NULL THEN 
			''
		ELSE
			' ' + CTU.Plaintext COLLATE SQL_Latin1_General_CP1_CI_AS
		END +
	  ' ' + CTP.Plaintext COLLATE SQL_Latin1_General_CP1_CI_AS +
		' (' + D.Applies_To + ')' AS Value
	FROM Collection_Unit_Data D
	LEFT JOIN VW_ConceptTerm CTU ON CTU.Concept_Key=D.Unit_Concept_Key
	INNER JOIN VW_ConceptTerm CTP ON CTP.Concept_Key=D.Parameter_Concept_Key
	WHERE D.Collection_Unit_Key=@Key
	AND D.Is_Descriptor=0

OPEN data_cursor

FETCH NEXT FROM data_cursor INTO @Value

WHILE @@Fetch_Status=0
BEGIN
  --New line if required
	IF @Output <> '' 
		SET @Output=@Output+CHAR(13)+CHAR(10)
  SET @Output=@Output+@Value	
	FETCH NEXT FROM data_cursor INTO @Value
END

CLOSE data_cursor
DEALLOCATE data_cursor
SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_StoreDetailsMemo_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_StoreDetailsMemo_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_StoreDetailsMemo_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_StoreDetailsMemo_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_StoreDetailsMemo_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_StoreDetailsMemo_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_StoreDetailsMemo_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_StoreDetailsMemo_Get TO [Dev - JNCC SQL]
END

GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_StoreHierarchy_Child_Select_ForStore]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_StoreHierarchy_Child_Select_ForStore]
GO

/*===========================================================================*\
  Description:	Returns successive child Stores for a specified Store

  Parameters:	
	@ParentKey 	Only the records associated with the parent key are returned
	@UserDomainMask	User's Domain Mask restricting which records may be returned
	@SessionID 	User's SessionID

  Created:	February 2004

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_StoreHierarchy_Child_Select_ForStore] 
	@UserDomainMask INT,
	@SessionID CHAR(16),
	@SortOrderIndex TINYINT,
	@ParentKey CHAR(16)
AS

SET NOCOUNT ON

	DECLARE @StoreHierarchy TABLE
	(
		[Collection_Unit_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
		[Item_Name] [varchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
		[Number] [varchar] (30) NULL,
		[Bottom_Level] [bit] NOT NULL
	)

	DECLARE @COLLECTION_UNIT_KEY CHAR(16), 
		@ITEM_NAME VARCHAR(100), 
		@Number varchar(30), 
		@Bottom_Level BIT

	SET @Bottom_Level = 0

	--Obtain first generation children
	INSERT INTO @StoreHierarchy (Collection_Unit_Key, Item_Name, Number, Bottom_Level) 
	SELECT 		S.Collection_Unit_Key, S.Item_Name, CUN.Number, 0
	FROM 		Store S
	INNER JOIN 	Collection_Unit CU ON S.Collection_Unit_Key = CU.Collection_Unit_Key
		AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
			OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
		AND CU.Current_Container_Collection_Unit_Key = @ParentKey

	LEFT JOIN 	Collection_Unit_Number CUN ON S.Collection_Unit_Key = CUN.Collection_Unit_Key 
		AND CUN.Type_Concept_Key = 'SYSTEM0000000001'  --REGISTRATION NUMBER
		AND CUN.Preferred = 1

	UPDATE	SH
	SET	SH.Bottom_Level = CASE WHEN S.Collection_Unit_Key IS NULL THEN 1 ELSE 0 END
	FROM	@StoreHierarchy SH
	LEFT JOIN Collection_Unit CU ON SH.Collection_Unit_Key = CU.Current_Container_Collection_Unit_Key
		AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
		OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
	-- Need to do a join onto Store because other Collection_Units could have their current container
	-- as this store. However, we are only interested in the Stores at the moment.
	LEFT JOIN Store as S on S.Collection_Unit_Key = CU.Collection_Unit_Key

	--Return hierarchical list
	IF @SortOrderIndex = 0
		SELECT	Collection_Unit_Key AS Item_Key, Collection_Unit_Key AS Join_Key, Item_Name, Number, Bottom_Level
		FROM	@StoreHierarchy
		ORDER BY Item_Name, Number
	ELSE 
		SELECT	Collection_Unit_Key AS Item_Key, Collection_Unit_Key AS Join_Key, Item_Name, Number, Bottom_Level
		FROM	@StoreHierarchy
		ORDER BY Number, Item_Name
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_StoreHierarchy_Child_Select_ForStore') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_StoreHierarchy_Child_Select_ForStore'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_StoreHierarchy_Child_Select_ForStore TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_StoreHierarchy_Child_Select_ForStore TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_StoreHierarchy_Child_Select_ForStore TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_StoreHierarchy_Child_Select_ForStore TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_StoreHierarchy_Child_Select_ForStore TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_StoreHierarchy_Child_Select_ForStore TO [Dev - JNCC SQL]
END
GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Store_Contains_Get]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Store_Contains_Get]
GO

CREATE PROCEDURE [dbo].[usp_Store_Contains_Get] 
@ContainerStoreKey CHAR(16),
@ContainedStoreKey CHAR(16),
@IsCurrentLocation BIT,
@Contains BIT OUTPUT
AS

--  DESCRIPTION
--  Returns successive parent Stores for a specified Store
--
--  PARAMETERS
--  NAME				DESCRIPTION
--	@ParentKey 			Only the records associated with the parent key are returned
--
--
--  AUTHOR:     		Ben Collier, Dorset Software
--  CREATED:    		2003-08-29
--
SET NOCOUNT ON


DECLARE @ContainedStoreContainers TABLE
(
	[Collection_Unit_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL
)


DECLARE @Parent_Collection_Unit_Key CHAR(16)

SET @Parent_Collection_Unit_Key = @ContainedStoreKey
SET @Contains = 0

--Obtain successive parents
WHILE @Parent_Collection_Unit_Key IS NOT NULL
BEGIN
	SELECT @Parent_Collection_Unit_Key = 
		CASE WHEN @IsCurrentLocation = 0 THEN CU.Usual_Container_Collection_Unit_Key 
		ELSE CU.Current_Container_Collection_Unit_Key END
	FROM STORE S 
	INNER JOIN COLLECTION_UNIT CU ON S.Collection_Unit_Key = CU.Collection_Unit_Key
			AND S.Collection_Unit_Key = @Parent_Collection_Unit_Key
	IF (@Parent_Collection_Unit_Key IS NULL) OR (@@RowCount = 0) OR (@Parent_Collection_Unit_Key = 'SYSTEM0000000000')
		--At top of storage hierarchy. No match found.
	BEGIN
		SET @Contains = 0
		BREAK
	END
	ELSE IF @Parent_Collection_Unit_Key = @ContainerStoreKey --Match found.
	BEGIN
		SET @Contains = 1
		BREAK
	END
	ELSE IF EXISTS(SELECT * FROM @ContainedStoreContainers WHERE Collection_Unit_Key = @Parent_Collection_Unit_Key)
	BEGIN --Recursive Store hierarchy found.
		SET @Contains = 1
		BREAK
	END
	ELSE --Log current Store 
		INSERT INTO @ContainedStoreContainers (Collection_Unit_Key) VALUES (@Parent_Collection_Unit_Key)

END


GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Store_Contains_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Store_Contains_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Store_Contains_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Store_Contains_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Store_Contains_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Store_Contains_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Store_Contains_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Store_Contains_Get TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Store_Delete') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_Store_Delete]
GO

/*===========================================================================*\
  Description:	Delete a record from the Store table.
		If the store is also a specimen, the dedicated procedure to 
		delete a specimen is run, it will take care of properly updating
		all domain masks. If the store is just that, its mask should
		already have been dealt with and be 0. If not, something wrong
		probably happened.

  Parameters:	@StoreKey

  Created:	July 2003

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Store_Delete]
	@StoreKey char(16),
	@Timestamp timestamp = NULL
AS
	BEGIN TRANSACTION
		-- Delete record from Store table.
		DELETE	Store
		WHERE	Collection_Unit_Key = @StoreKey
		AND	((@Timestamp = Timestamp) OR (@Timestamp IS NULL))

		IF @@Error <> 0 GOTO RollBackAndExit

		-- Delete CollectionUnitRelation records that use this Collection_Unit_Key
		EXECUTE	usp_CollectionUnitRelations_Delete @StoreKey

		IF @@Error <> 0 GOTO RollBackAndExit

		/*-------------------------------------------------------------*\
		 @Timestamp is null if proc called from a specimen related process. 
		 In that case , the Collection_Unit table will be handled by the 
		 specimen related process that called the proc, and not here.
		\*-------------------------------------------------------------*/
		IF @Timestamp IS NOT NULL
			-- If store is also a specimen, let the dedicated procedure deal with it
			IF EXISTS(SELECT * FROM Specimen_Unit WHERE Collection_Unit_Key = @StoreKey)
				EXECUTE	usp_Specimen_Delete @StoreKey
			ELSE
			-- Otherwise, just delete record from Collection_Unit table.
				DELETE	Collection_Unit
				WHERE	Collection_Unit_Key = @StoreKey

		IF @@Error <> 0 GOTO RollBackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit:
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Store_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Store_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Store_Delete TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Store_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Store_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Store_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Store_Delete TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SurveyEventKey_Get') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_SurveyEventKey_Get]
GO
/*===========================================================================*\
  Description: 	Returns a survey event key based on properties of the survey 
	       	event

  Created:	September 2003

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SurveyEventKey_Get]
	@Key char(16) OUTPUT,
	@VagueDateStart int,
	@VagueDateEnd int,
	@VagueDateType varchar(2),
	@SpatialRef varchar(40),
	@SpatialRefSystem varchar(4),
	@Lat float,
	@Long float,
	@SpatialRefQualifier varchar(20),
	@LocationKey char(16),
	@SurveyKey char(16),
	@LocationName varchar(100)
	
AS
	/*----------------------------------------------------------------------------*\
	  Match exact list of Field Collectors if #TempFieldCollectors has been 
	  created by Collections Browser.
	\*----------------------------------------------------------------------------*/
	IF object_id('tempdb..#TempFieldCollectors') IS NOT NULL
	BEGIN
		/*-----------------------------------------------------------------*\
		  Create a table variable to store the Survey_Event match keys.
		\*-----------------------------------------------------------------*/
		DECLARE @tableSurveyEventMatches TABLE (
			Survey_Event_Key char(16) COLLATE SQL_Latin1_General_CP1_CI_AS
		)
	
		DECLARE @CurrentFieldCollector char(16)
	
		/*------------------------------------------------------------------*\
		  Create the cursor.
		\*------------------------------------------------------------------*/
		DECLARE curFieldCollectors CURSOR LOCAL FAST_FORWARD FOR
			SELECT 	Name_Key
			FROM	#TempFieldCollectors
	
		OPEN	curFieldCollectors

		/*-------------------------------------------------------*\
		  Give @CurrentFieldCollector its first value.
		\*-------------------------------------------------------*/	
		FETCH NEXT
		FROM	curFieldCollectors
		INTO	@CurrentFieldCollector

		/*-------------------------------------------------------*\
		  Start looping through the field collectors.
		\*-------------------------------------------------------*/		
		WHILE @@Fetch_Status = 0
		BEGIN
			/*------------------------------------------------------------------------*\
			  If the there are no records in @tableSurveyEventMatches, insert all of
			  the Survey_Event records for the first Field Collector in the table.
			\*------------------------------------------------------------------------*/
			IF NOT EXISTS (SELECT * FROM @tableSurveyEventMatches)
			BEGIN
				INSERT INTO @tableSurveyEventMatches (
					Survey_Event_Key
				) 
				SELECT 	Survey_Event_Key
				FROM	Survey_Event_Recorder
				WHERE	Name_Key = @CurrentFieldCollector
			END
			ELSE
			/*------------------------------------------------------------------------*\
			  As there are records in @tableSurveyEventMatches, we can now start
			  removing records that aren't matched.
			\*------------------------------------------------------------------------*/ 
			BEGIN
				-- Delete non matches
				DELETE		@tableSurveyEventMatches
				FROM		@tableSurveyEventMatches AS SEM
				LEFT JOIN	Survey_Event_Recorder AS SER ON SER.Survey_Event_Key = SEM.Survey_Event_Key
									AND SER.Name_Key = @CurrentFieldCollector
				WHERE		SER.Survey_Event_Key IS NULL
			END

			/*---------------------------------------------------------------------*\
			  Get next field collector and put value into @CurrentFieldCollector.
			\*---------------------------------------------------------------------*/		
			FETCH NEXT
			FROM	curFieldCollectors
			INTO	@CurrentFieldCollector
		END
	
		CLOSE curFieldCollectors
		DEALLOCATE curFieldCollectors

		/*---------------------------------------------------------------------*\
		  Now match on Survey, Location, Date and Sample Recorders.
		\*---------------------------------------------------------------------*/		
		SELECT 	@Key = SE.Survey_Event_Key 
		FROM 	Survey_Event AS SE
		JOIN 	@tableSurveyEventMatches AS SEM ON SEM.Survey_Event_Key = SE.Survey_Event_Key
		WHERE	Survey_Key = @SurveyKey
		AND	((Vague_Date_Start = @VagueDateStart AND Vague_Date_End = @VagueDateEnd AND Vague_Date_Type = @VagueDateType) 
			OR 
			(Vague_Date_Type = 'U' AND (@VagueDateType='U' OR @VagueDateType IS NULL)))
		AND	(Location_key = @LocationKey
			OR
			(@LocationKey IS NULL AND Spatial_Ref = @SpatialRef AND Spatial_Ref_System = @SpatialRefSystem)
			OR
			(@LocationKey IS NULL AND @SpatialRef IS NULL AND Location_Name = @LocationName))
	END
	ELSE
		/*---------------------------------------------------------------------*\
		  This matches on Survey, Location and Date. I left this here in case
		  it is required somewhere else and #TempFieldCollectors hasn't been
		  created by the application.
		\*---------------------------------------------------------------------*/	
		SELECT 	@Key = SE.Survey_Event_Key
		FROM 	Survey_Event AS SE
		WHERE	Survey_Key = @SurveyKey
		AND	((Vague_Date_Start = @VagueDateStart AND Vague_Date_End = @VagueDateEnd AND Vague_Date_Type = @VagueDateType) 
			OR 
			(Vague_Date_Type = 'U' AND (@VagueDateType='U' OR @VagueDateType IS NULL)))
		AND	(Location_key = @LocationKey
			OR
			(@LocationKey IS NULL AND Spatial_Ref = @SpatialRef AND Spatial_Ref_System = @SpatialRefSystem)
			OR
			(@LocationKey IS NULL AND @SpatialRef IS NULL AND Location_Name = @LocationName))
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SurveyEventKey_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SurveyEventKey_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SurveyEventKey_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SurveyEventKey_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SurveyEventKey_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_SurveyEventKey_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SurveyEventKey_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SurveyEventKey_Get TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_SurveyEvent_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_SurveyEvent_Update]
GO

/*===========================================================================*\
  Description:	Updates a record into the Survey_Event table

  Parameters:	@Key char(16),
		@VagueDateStart int,
		@VagueDateEnd int,
		@VagueDateType varchar(2) = NULL,
		@SpatialRef varchar(40),
		@SpatialRefSystem varchar(4),
		@Lat float,
		@Long float,
		@SpatialRefQualifier varchar(20),
		@LocationKey char(16),
		@SurveyKey char(16),
		@Comment text,
		@ChangedBy char(16),
		@LocationName varchar(100)

  Created:	August 2004

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SurveyEvent_Update]
	@Key char(16),
	@VagueDateStart int,
	@VagueDateEnd int,
	@VagueDateType varchar(2) = NULL,
	@SpatialRef varchar(40),
	@SpatialRefSystem varchar(4),
	@Lat float,
	@Long float,
	@SpatialRefQualifier varchar(20),
	@LocationKey char(16),
	@SurveyKey char(16),
	@Comment text,
	@ChangedBy char(16),
	@LocationName varchar(100)
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION
		
		UPDATE 	Survey_Event
		SET 	Vague_Date_Start = @VagueDateStart, 
			Vague_Date_End = @VagueDateEnd, 
			Vague_Date_Type = @VagueDateType,
			Spatial_Ref = @SpatialRef, 
			Spatial_Ref_System = @SpatialRefSystem, 
			Spatial_Ref_Qualifier = @SpatialRefQualifier,
			Lat = @Lat, 
			Long = @Long, 
			Location_Key = @LocationKey,
			Survey_Key= @SurveyKey,
			Comment = @Comment,
			Location_Name = @LocationName,
			Changed_By = @ChangedBy,
			Changed_Date = GetDate()
		WHERE	Survey_Event_Key = @Key

		IF @@Error <> 0 GOTO RollbackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SurveyEvent_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SurveyEvent_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SurveyEvent_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SurveyEvent_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SurveyEvent_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SurveyEvent_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SurveyEvent_Update TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_SystemSuppliedData_BCP_Export]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_SystemSuppliedData_BCP_Export]
GO

/*===========================================================================*\
  Description:	Exports system supplied data as text files to the given
		output location.

  Parameters:	@OutputPath	Location for output files.

  Created:	September 2003

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SystemSuppliedData_BCP_Export]
	@OutputPath varchar(1000) = NULL
AS
	DECLARE @TableName varchar(50),
		@Cmd varchar(500),
		@PrimaryKey varchar(50)

	/*-------------------------------------------------------------*\
		Ensure developer's domains do not transfer through
	\*-------------------------------------------------------------*/
  UPDATE Collection_Unit SET domain_Mask=0 WHERE collection_Unit_Key like 'SYSTEM00%'

	/*-------------------------------------------------------------*\
	  Set default output path, if none provided.
	\*-------------------------------------------------------------*/
	IF @OutputPath IS NULL
		SET @OutputPath = 'C:\Temp\'
	
	/*-------------------------------------------------------------*\
	  Declare a cursor to loop through all the tables with a
	  System Supplied Data field.
	\*-------------------------------------------------------------*/
	DECLARE curTableNames CURSOR LOCAL FAST_FORWARD
	FOR
		SELECT		TableName = Convert(SysName, O.Name)
		FROM		SysObjects O
		INNER JOIN 	SysColumns C ON C.Id = O.Id
		WHERE		O.Type = 'U'
		AND		C.Number = 0
		AND		(C.Name = 'System_Supplied_Data' OR O.NAME IN ('Macro', 'Meaning', 'Language', 'Store', 'Collection_Unit', 'Database_Relationship'))
		ORDER BY 	O.Name

	/*-------------------------------------------------------------*\
	  Run BCP command on each table returned.
	\*-------------------------------------------------------------*/
	OPEN curTableNames
	FETCH NEXT FROM curTableNames INTO @TableName

  WHILE @@Fetch_Status = 0
	BEGIN
		--Obtain table's primary key 
		SELECT @PrimaryKey = KCU.Column_Name
		FROM Information_Schema.Table_Constraints AS TC
		INNER JOIN Information_Schema.Key_Column_Usage AS KCU
		  ON (TC.Table_Catalog = kcu.Table_Catalog and
		         TC.Table_Schema  = kcu.Table_Schema and
		         TC.Table_Name    = kcu.Table_Name and
		         TC.Constraint_Name = kcu.Constraint_Name)
		WHERE TC.Table_Catalog   = 'CollectionsDev'
		AND   TC.Table_Schema    = 'dbo'
		AND   TC.Table_Name      = @TableName
		AND   TC.Constraint_Type = 'PRIMARY KEY'

		IF @TableName IN ('Meaning', 'Store', 'Collection_Unit', 'Database_Relationship', 'Macro')
			SET @Cmd = 'bcp "SELECT * FROM ' + DB_Name() + '.dbo.' + @TableName + ' WHERE ' +
					'LEFT(' + @PrimaryKey + ', 8)=''SYSTEM00''" ' +
			 	 'queryout "' + @OutputPath + @TableName + '.txt" -m50 -T -c -t*@*@ -r!@!@ -S' + @@ServerName
		ELSE IF @TableName = 'Language'
			SET @Cmd = 'bcp "SELECT * FROM ' + DB_Name() + '.dbo.' + @TableName + 
				 '" queryout "' + @OutputPath + @TableName + '.txt" -m50 -T -c -t*@*@ -r!@!@ -S' + @@ServerName
		ELSE IF @TableName = 'Taxon_Dictionary_Name_Type_Mapping'
			SET @Cmd = 'bcp "SELECT * FROM ' + DB_Name() + '.dbo.' + @TableName + ' WHERE ' +
					'System_Supplied_Data = 1" ' +
				 'queryout "' + @OutputPath + @TableName + '.txt" -m50 -T -c -t*@*@ -r!@!@ -S' + @@ServerName
		ELSE
			SET @Cmd = 'bcp "SELECT * FROM ' + DB_Name() + '.dbo.' + @TableName + ' WHERE System_Supplied_Data = 1 AND ' +
					'LEFT(' + @PrimaryKey + ', 8)=''SYSTEM00''" ' +
			 	 'queryout "' + @OutputPath + @TableName + '.txt" -m50 -T -c -t*@*@ -r!@!@ -S' + @@ServerName

		EXEC Master..xp_CmdShell @Cmd, no_output

		FETCH NEXT FROM curTableNames INTO @TableName
	END

	/*-------------------------------------------------------------*\
	  Cleanup.
	\*-------------------------------------------------------------*/
	CLOSE curTableNames
	DEALLOCATE curTableNames
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SystemSuppliedData_BCP_Export') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SystemSuppliedData_BCP_Export'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SystemSuppliedData_BCP_Export TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SystemSuppliedData_BCP_Export TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SystemSuppliedData_BCP_Export TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_SystemSuppliedData_BCP_Import]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_SystemSuppliedData_BCP_Import]
GO

/*===========================================================================*\
  Description:	Imports system supplied data from text files located in the 
		given location.

  Parameters:	@InputPath	Location for input files.

  Created:	September 2003

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SystemSuppliedData_BCP_Import]
	@InputPath varchar(1000) = NULL
AS
	DECLARE @TableName varchar(50),
		@Cmd varchar(500)

	/*-------------------------------------------------------------*\
	  Set default output path, if none provided.
	\*-------------------------------------------------------------*/
	IF @InputPath IS NULL
		SET @InputPath = 'C:\Temp\'
	
	/*-------------------------------------------------------------*\
	  Declare a cursor to loop through all the tables with a
	  System Supplied Data field.
	\*-------------------------------------------------------------*/
	DECLARE curTableNames CURSOR LOCAL FAST_FORWARD
	FOR
		SELECT	DISTINCT	TableName = Convert(SysName, O.Name)
		FROM		SysObjects O
		INNER JOIN 	SysColumns C ON C.Id = O.Id
		WHERE		O.Type = 'U'
		AND		C.Number = 0
		AND		(C.Name = 'System_Supplied_Data' OR O.Name IN ('Macro', 'Meaning', 'Language', 'Store', 'Collection_Unit', 'Database_Relationship'))
		ORDER BY 	Convert(SysName, O.Name)

	/*-------------------------------------------------------------*\
	  Run BCP command on each table returned.
	\*-------------------------------------------------------------*/
	OPEN curTableNames
	FETCH NEXT FROM curTableNames INTO @TableName

	WHILE @@Fetch_Status = 0
	BEGIN
		SET @Cmd = 'bcp ' + DB_Name() + '.dbo.' + @TableName + 
			' in "' + @InputPath + @TableName + '.txt" -m50 -T -c -t*@*@ -r!@!@ -S' + @@ServerName

		EXEC Master..xp_CmdShell @Cmd, no_output

		FETCH NEXT FROM curTableNames INTO @TableName
	END

	/*-------------------------------------------------------------*\
	  Cleanup.
	\*-------------------------------------------------------------*/
	CLOSE curTableNames
	DEALLOCATE curTableNames
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SystemSuppliedData_BCP_Import') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SystemSuppliedData_BCP_Import'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SystemSuppliedData_BCP_Import TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SystemSuppliedData_BCP_Import TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SystemSuppliedData_BCP_Import TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_TaxonDesignationType_ImportConceptGroup]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_TaxonDesignationType_ImportConceptGroup]
GO

/*===========================================================================*\
  Description:	Import taxon designation types corresponding to the types
  				used in the specified taxon list.

  Parameters:	@job_id					Job identifier

  Created:		Dec 2003

  Last revision information:
	$Revision: 7 $
	$Date: 6/02/09 10:41 $
	$Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_TaxonDesignationType_ImportConceptGroup]
	@job_id					INT
AS
	SET NOCOUNT ON

	DECLARE     @concept_group_key				CHAR(16),
				@concept_key					CHAR(16),
				@taxon_designation_type_key		CHAR(16),
				@short_name						VARCHAR(40),
				@ins_user_key					CHAR(16),
				@ins_date						SMALLDATETIME,
				@upd_user_key					CHAR(16),
				@upd_date						SMALLDATETIME,
				@system							BIT

	/* determine parameters of job */
	SELECT		@concept_group_key				=	j.Concept_Group_Key
	FROM		Import_Export_Job				AS	j
	WHERE		j.Import_Export_Job_ID			=	@job_id

	IF @@ROWCOUNT = 0
	BEGIN
		RAISERROR ('Job does not exist or has not been configured', 16, 1)
		GOTO fail
	END

	EXECUTE		usp_Import_Export_Job_UpdateStatus	@job_id,
													'Importing designation types'
	IF @@ERROR <> 0 GOTO fail

	DECLARE		types	CURSOR LOCAL FAST_FORWARD FOR
	SELECT DISTINCT
				dt.Concept_Key,
				t.Item_Name,
				es.User_Name_Key,
				CONVERT(SMALLDATETIME,
						CONVERT(VARCHAR, es.Date_Time_Start, 112)),
				cs.User_Name_Key,
				CONVERT(SMALLDATETIME,
						CONVERT(VARCHAR, cs.Date_Time_Start, 112)),
				dt.System_Supplied_Data
	FROM		Concept							AS	c
	INNER JOIN	Concept_Designation				AS	d
	ON			d.Concept_Key					=	c.Concept_Key
	INNER JOIN	Concept							AS  dt
	ON			dt.Concept_Key					=	d.Designation_Type_Concept_Key
	INNER JOIN	Term							AS	t
	ON			t.Term_Key						=	dt.Term_Key
	INNER JOIN	Session							AS	es
	ON			es.Session_ID					=	dt.Entered_Session_ID
	LEFT JOIN	Session							AS	cs
	ON			cs.Session_ID					=	dt.Changed_Session_ID
	WHERE		c.Concept_Group_Key				=	@concept_group_key

	OPEN		types

	WHILE 1 = 1
	BEGIN
		FETCH		types
		INTO		@concept_key,
					@short_name,
					@ins_user_key,
					@ins_date,
					@upd_user_key,
					@upd_date,
					@system

		IF @@FETCH_STATUS <> 0 BREAK

		BEGIN TRANSACTION

		SELECT		@taxon_designation_type_key					=	Taxon_Designation_Type_Key
		FROM		Taxon_Dictionary_Designation_Type_Mapping
		WHERE		Concept_Designation_Type_Key				=	@concept_key

		IF @@ROWCOUNT > 0
		BEGIN
			/* update designation type */
			UPDATE		TAXON_DESIGNATION_TYPE
			SET			SHORT_NAME					=	@short_name,
						ENTERED_BY					=	@ins_user_key,
						ENTRY_DATE					=	@ins_date,
						CHANGED_BY					=	@upd_user_key,
						CHANGED_DATE				=	@upd_date,
						SYSTEM_SUPPLIED_DATA		=	@system
			WHERE		TAXON_DESIGNATION_TYPE_KEY	=	@taxon_designation_type_key

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END
		ELSE
		BEGIN
			/* create designation type */
			EXECUTE		spNextKey	'TAXON_DESIGNATION_TYPE',
									@taxon_designation_type_key		OUTPUT
			IF @@ERROR <> 0 GOTO fail_from_cursor

			INSERT		TAXON_DESIGNATION_TYPE (
						TAXON_DESIGNATION_TYPE_KEY,
						SHORT_NAME,
						ENTERED_BY,
						ENTRY_DATE,
						CHANGED_BY,
						CHANGED_DATE,
						SYSTEM_SUPPLIED_DATA)
			VALUES		(@taxon_designation_type_key,
						@short_name,
						@ins_user_key,
						@ins_date,
						@upd_user_key,
						@upd_date,
						@system)

			IF @@ERROR <> 0 GOTO fail_from_cursor

			/* record mapping */
			INSERT		Taxon_Dictionary_Designation_Type_Mapping (
						Taxon_Designation_Type_Key,
						Concept_Designation_Type_Key)
			VALUES		(@taxon_designation_type_key,
						@concept_key)

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END

		EXECUTE		usp_Import_Export_Job_RecordProcessed	@job_id
		IF @@ERROR <> 0 GOTO fail_from_cursor

		COMMIT TRANSACTION
	END

	CLOSE		types
	DEALLOCATE	types
	RETURN

fail_from_cursor:
	CLOSE		types
	DEALLOCATE	types

fail:
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_TaxonDesignationType_ImportConceptGroup failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TaxonDesignationType_ImportConceptGroup') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_TaxonDesignationType_ImportConceptGroup'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_TaxonDesignationType_ImportConceptGroup TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_TaxonDesignationType_ImportConceptGroup TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_TaxonDesignationType_ImportConceptGroup TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_TaxonDesignation_ImportConceptGroup]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_TaxonDesignation_ImportConceptGroup]
GO

/*===========================================================================*\
  Description:	Import taxon designations corresponding to the concept
				designations associated with the specified concept group.

  Parameters:	@job_id					Job identifier

  Created:		Dec 2003

  Last revision information:
	$Revision: 7 $
	$Date: 6/02/09 10:41 $
	$Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_TaxonDesignation_ImportConceptGroup]
	@job_id					INT
AS
	SET NOCOUNT ON

	DECLARE     @concept_group_key			CHAR(16),
				@concept_designation_key	CHAR(16),
				@taxon_designation_key		CHAR(16),
				@taxon_list_item_key		CHAR(16),
				@taxon_designation_type_key	CHAR(16),
				@date_from					DATETIME,
				@date_to					DATETIME,
				@entered_by					CHAR(16),
				@entry_date					SMALLDATETIME,
				@changed_by					CHAR(16),
				@changed_date				SMALLDATETIME,
				@system						BIT,
				@source_key					CHAR(16),
				@source_join_key			CHAR(16)

	/* determine parameters of job */
	SELECT		@concept_group_key			=	j.Concept_Group_Key
	FROM		Import_Export_Job			AS	j
	WHERE		j.Import_Export_Job_ID		=	@job_id

	IF @@ROWCOUNT = 0
	BEGIN
		RAISERROR ('Job does not exist or has not been configured', 16, 1)
		GOTO fail
	END

	EXECUTE		usp_Import_Export_Job_UpdateStatus	@job_id,
													'Exporting concept designations'
	IF @@ERROR <> 0 GOTO fail

	DECLARE		designations	CURSOR LOCAL FAST_FORWARD FOR
	SELECT		d.Concept_Designation_Key,
				cm.Taxon_List_Item_Key,
				tm.Taxon_Designation_Type_Key,
				CASE WHEN d.From_Vague_Date_Start >= -53688
					THEN DATEADD(day, d.From_Vague_Date_Start, '18991230')
							/* date that SQL server can represent */
					ELSE NULL
							/* null, or date that SQL server cannot represent */
				END,
				CASE WHEN d.To_Vague_Date_Start >= -53688
					THEN DATEADD(day, d.To_Vague_Date_Start, '18991230')
							/* date that SQL server can represent */
					ELSE NULL
							/* null, or date that SQL server cannot represent */
				END,
				es.User_Name_Key,
				CONVERT(SMALLDATETIME,
						CONVERT(VARCHAR, es.Date_Time_Start, 112)),
				cs.User_Name_Key,
				CONVERT(SMALLDATETIME,
						CONVERT(VARCHAR, cs.Date_Time_Start, 112)),
				d.System_Supplied_Data
	FROM        Concept										AS	c
	INNER JOIN	Concept_Designation							AS	d
	ON			d.Concept_Key								=	c.Concept_Key
	INNER JOIN	Taxon_Dictionary_Concept_Mapping			AS	cm
	ON			cm.Concept_Key								=	d.Concept_Key
	INNER JOIN	Taxon_Dictionary_Designation_Type_Mapping	AS	tm
	ON			tm.Concept_Designation_Type_Key				=	d.Designation_Type_Concept_Key
	INNER JOIN	Session										AS	es
	ON			es.Session_ID								=	d.Entered_Session_ID
	LEFT JOIN	Session										AS	cs
	ON			cs.Session_ID								=	d.Changed_Session_ID
	WHERE		c.Concept_Group_Key							=	@concept_group_key

	OPEN		designations

	WHILE 1 = 1
	BEGIN
		FETCH		designations
		INTO		@concept_designation_key,
					@taxon_list_item_key,
					@taxon_designation_type_key,
					@date_from,
					@date_to,
					@entered_by,
					@entry_date,
					@changed_by,
					@changed_date,
					@system

		IF @@FETCH_STATUS <> 0 BREAK

		BEGIN TRANSACTION

		SELECT		@taxon_designation_key							=	NULL,
					@source_join_key								=	NULL,
					@source_key										=	NULL

		SELECT		@taxon_designation_key							=	m.Taxon_Designation_Key,
					@source_key										=	j.Source_Key
		FROM		Taxon_Dictionary_Concept_Designation_Mapping	AS	m
		LEFT JOIN	Source_Join										AS	j
		ON			j.Source_Join_Key								=	m.Source_Join_Key
		WHERE		Taxon_Designation_Key							=	@concept_designation_key

		IF @source_key IS NULL
		BEGIN
			/* there is no existing mapping for the source join; pick an
			 * arbitrary join record (if there are any) and make this the
			 * mapped join.
			 */
			SELECT		@source_join_key	=	Source_Join_Key,
						@source_key			=	Source_Key
			FROM		Source_Join
			WHERE		Record_Key			=	@concept_designation_key
			AND			Table_Name			=	'Concept_Designation'
			ORDER BY	Source_Join_Key
		END

		IF @taxon_designation_key IS NOT NULL
		BEGIN
			/* update taxon designation */
			UPDATE		TAXON_DESIGNATION
			SET			DATE_FROM					=	@date_from,
						DATE_TO						=	@date_to,
						TAXON_DESIGNATION_TYPE_KEY	=	@taxon_designation_type_key,
						TAXON_LIST_ITEM_KEY			=	@taxon_list_item_key,
						SOURCE_KEY					=	@source_key,
						ENTERED_BY					=	@entered_by,
						ENTRY_DATE					=	@entry_date,
						CHANGED_BY					=	@changed_by,
						CHANGED_DATE				=	@changed_date,
						SYSTEM_SUPPLIED_DATA		=	@system
			WHERE		TAXON_DESIGNATION_KEY		=	@taxon_designation_key

			IF @@ERROR <> 0 GOTO fail_from_cursor

			IF @source_join_key IS NOT NULL
			BEGIN
				UPDATE		Taxon_Dictionary_Concept_Designation_Mapping
				SET			Source_Join_Key									=	@source_join_key
				WHERE		Taxon_Designation_Key							=	@taxon_designation_key

				IF @@ERROR <> 0 GOTO fail_from_cursor				
			END
		END
		ELSE
		BEGIN
			/* create taxon designation */
			EXECUTE		spNextKey	'TAXON_DESIGNATION',
									@taxon_designation_key	OUTPUT
			IF @@ERROR <> 0 GOTO fail_from_cursor

			INSERT		TAXON_DESIGNATION (
						TAXON_DESIGNATION_KEY,
						DATE_FROM,
						DATE_TO,
						TAXON_DESIGNATION_TYPE_KEY,
						TAXON_LIST_ITEM_KEY,
						SOURCE_KEY,
						ENTERED_BY,
						ENTRY_DATE,
						CHANGED_BY,
						CHANGED_DATE,
						SYSTEM_SUPPLIED_DATA)
			VALUES		(@taxon_designation_key,
						@date_from,
						@date_to,
						@taxon_designation_type_key,
						@taxon_list_item_key,
						@source_key,
						@entered_by,
						@entry_date,
						@changed_by,
						@changed_date,
						@system)

			IF @@ERROR <> 0 GOTO fail_from_cursor

			/* record mapping */
			INSERT		Taxon_Dictionary_Concept_Designation_Mapping (
						Taxon_Designation_Key,
						Concept_Designation_Key,
						Source_Join_Key)
			VALUES		(@taxon_designation_key,
						@concept_designation_key,
						@source_join_key)

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END

		EXECUTE		usp_Import_Export_Job_RecordProcessed	@job_id
		IF @@ERROR <> 0 GOTO fail_from_cursor

		COMMIT TRANSACTION
	END

	CLOSE		designations
	DEALLOCATE	designations
	RETURN

fail_from_cursor:
	CLOSE		designations
	DEALLOCATE	designations

fail:
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_TaxonDesignation_ImportConceptGroup failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TaxonDesignation_ImportConceptGroup') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_TaxonDesignation_ImportConceptGroup'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_TaxonDesignation_ImportConceptGroup TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_TaxonDesignation_ImportConceptGroup TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_TaxonDesignation_ImportConceptGroup TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_TaxonDetermination_Insert]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_TaxonDetermination_Insert]
GO

/*===========================================================================*\
  Description:	Inserts a record in the Taxon Determination table.
		Ensures the Domain mask of the specimen is also updated.

  Parameters:	@Key 
		@DeterminedItemKey
		@OccurrenceKey  
		@SpecimenCollectionUnitKey  
		@DeterminationTypeKey  
		@NomenclaturalStatusConceptKey 
		@Confidence
		@DeterminerNameKey 
		@InferredDeterminer 
		@DeterminerRoleKey 
		@VagueDateStart
		@VagueDateEnd 
		@VagueDateType 
		@UsedSpecimen 
		@Preferred
		@Method
		@Notes 
		@SessionID
		@IsForSpecimen		Indicates whether to update preferred 
					flag in Specimen_Unit or Determination.

  Created:	July 2003

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_TaxonDetermination_Insert]
	@Key char(16) output, 
	@DeterminedItemKey char(16), 
	@OccurrenceKey char(16), 
	@SpecimenCollectionUnitKey char(16), 
	@DeterminationTypeKey char(16), 
	@NomenclaturalStatusConceptKey char(16),
	@Confidence tinyint,
	@DeterminerNameKey char(16), 
	@InferredDeterminer tinyint,
	@DeterminerRoleKey char(16), 
	@VagueDateStart int, 
	@VagueDateEnd int, 
	@VagueDateType varchar(2),
	@UsedSpecimen bit,
	@Preferred bit,
	@Method text,
	@Notes text,
	@SessionID char(16),
	@IsForSpecimen bit
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF
	SET XACT_ABORT ON

	--Wrap everything in a transaction
	BEGIN TRANSACTION

		-- Get the user name key, as Taxon_Determination table doesn't have SessionID fields.
		DECLARE	@EnteredBy char(16),
			@PreferredForSpecimen bit

		SELECT	@EnteredBy = User_Name_Key FROM Session WHERE Session_ID = @SessionID
		SET	@PreferredForSpecimen = 0

		-- Get a new key first.
		EXECUTE spNextKey 'Taxon_Determination', @Key OUTPUT

		/*---------------------------------------------------------------------------------*\
		  Ensure only one preferred determination per occurrence.
		\*---------------------------------------------------------------------------------*/
		IF @IsForSpecimen = 1
		BEGIN
			-- Either Preferred passed in as true, or not preferred but not set in 
			-- Speciment Unit either.
			IF @Preferred = 1
			OR (@Preferred = 0 AND EXISTS ( SELECT * FROM Specimen_Unit 
							WHERE Collection_Unit_Key = @SpecimenCollectionUnitKey 
							AND Preferred_Taxon_Determination_Key IS NULL))
				SET @PreferredForSpecimen = 1

			-- Not used for Taxon_Occurrence, unless not already one present
			IF EXISTS(SELECT 1 FROM Taxon_Determination WHERE Taxon_Occurrence_Key=@OccurrenceKey AND Preferred=1)
				SET @Preferred = 0
		END ELSE BEGIN
			-- If new determination not preferred, but there isn't one already, change the flag.
			IF @Preferred = 0 
			AND NOT EXISTS(SELECT * FROM Taxon_Determination WHERE Taxon_Occurrence_Key = @OccurrenceKey AND Preferred = 1)
				SET @Preferred = 1
			ELSE
			-- If new determination is preferred, make sure previous preferred is turned off.
			IF @Preferred = 1
			BEGIN
				UPDATE	Taxon_Determination
				SET	Preferred = 0
				WHERE	Taxon_Occurrence_Key = @OccurrenceKey
				AND	Preferred = 1

				IF @@Error <> 0 GOTO RollbackAndExit
			END
		END

		/*---------------------------------------------------------------------------------*\
		  Do table insert.
		\*---------------------------------------------------------------------------------*/
		INSERT INTO Taxon_Determination (
			Taxon_Determination_Key, Taxon_List_Item_Key, Taxon_Occurrence_Key, 
			Vague_Date_Start, Vague_Date_End, Vague_Date_Type, Comment, Preferred, Determiner, 
			Determination_Type_Key, Determiner_Role_Key, Entered_By, Entry_Date, 
			Specimen_Collection_Unit_Key, Nomenclatural_Status_Concept_Key,
			Confidence, Used_Specimen, Method, Inferred_Determiner
		) VALUES (
			@Key, @DeterminedItemKey, @OccurrenceKey, 
			@VagueDateStart, @VagueDateEnd, @VagueDateType, @Notes, @Preferred, @DeterminerNameKey,
			@DeterminationTypeKey, @DeterminerRoleKey, @EnteredBy, GetDate(), 
			@SpecimenCollectionUnitKey, @NomenclaturalStatusConceptKey, 
			@Confidence, @UsedSpecimen, @Method, @InferredDeterminer
		)

		IF @@Error <> 0 GOTO RollbackAndExit

		
		IF @PreferredForSpecimen = 1
		BEGIN
			UPDATE	Specimen_Unit
			SET	Preferred_Taxon_Determination_Key = @Key
			WHERE	Collection_Unit_Key = @SpecimenCollectionUnitKey

			IF @@Error <> 0 GOTO RollbackAndExit
		END

		/*-------------------------------------------------------------*\
		  Switch bit of new mask ON in Collection_Unit.
		\*-------------------------------------------------------------*/
		IF @IsForSpecimen = 1 AND @PreferredForSpecimen = 1
		BEGIN
			DECLARE @ConceptKey char(16),
				@ConceptMask int

			-- Get the right concept before getting the mask
			SELECT	@ConceptKey = Concept_Key
			FROM	Taxon_Dictionary_Concept_Mapping
			WHERE	Taxon_List_Item_Key = @DeterminedItemKey

			-- Retrieve the mask of the new concept.
			EXECUTE	usp_Get_Concept_Domain_Mask @ConceptKey, @ConceptMask OUTPUT
			-- And switch appropriate bit ON in Collection_Unit
			EXECUTE	usp_CollectionUnit_Update_DomainMask @SpecimenCollectionUnitKey, @ConceptMask, 1

			IF @@Error <> 0 GOTO RollbackAndExit
		END

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TaxonDetermination_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_TaxonDetermination_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_TaxonDetermination_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_TaxonDetermination_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_TaxonDetermination_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_TaxonDetermination_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_TaxonDetermination_Insert TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_TaxonDetermination_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_TaxonDetermination_Update]
GO

/*===========================================================================*\
  Description:	Updates a record in the Determination table.
		Ensures the Domain mask of the specimen is also updated.

  Parameters:	@Key 
		@DeterminedItemKey 
		@OccurrenceKey 
		@SpecimenCollectionUnitKey  
		@DeterminationTypeKey  
		@NomenclaturalStatusConceptKey 
		@Confidence
		@DeterminerNameKey 
		@InferredDeterminer 
		@DeterminerRoleKey 
		@VagueDateStart
		@VagueDateEnd 
		@VagueDateType 
		@UsedSpecimen 
		@Preferred
		@Method
		@Notes 
		@SessionID 
		@Timestamp
		@IsForSpecimen		Indicates whether to update preferred 
					flag in Specimen_Unit or Determination.

  Created:	July 2003

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_TaxonDetermination_Update]
	@Key char(16),
	@DeterminedItemKey char(16), 
	@OccurrenceKey char(16), 
	@SpecimenCollectionUnitKey char(16), 
	@DeterminationTypeKey char(16), 
	@NomenclaturalStatusConceptKey char(16),
	@Confidence tinyint,
	@DeterminerNameKey char(16), 
	@InferredDeterminer tinyint,
	@DeterminerRoleKey char(16), 
	@VagueDateStart int, 
	@VagueDateEnd int, 
	@VagueDateType varchar(2),
	@UsedSpecimen bit,
	@Preferred bit,
	@Method text,
	@Notes text,
	@SessionID char(16),
	@Timestamp timestamp,
	@IsForSpecimen bit,
	@RecordsAffected int OUTPUT
AS
	DECLARE @SpecimenKey char(16),
		@CurrentConceptKey char(16),
		@CurrentConceptMask bigint,
		@CurrentPreferred bit,
		@ConceptKey char(16),
		@ConceptMask bigint,
		@Error int
SET NOCOUNT OFF

	-- Get correct concept for given @TaxonListItemKey
	SELECT	@ConceptKey = Concept_Key
	FROM	Taxon_Dictionary_Concept_Mapping
	WHERE	Taxon_List_Item_Key = @DeterminedItemKey

	-- Retrieve the mask of the new concept.
	EXECUTE	usp_Get_Concept_Domain_Mask @ConceptKey, @ConceptMask OUTPUT

	BEGIN TRANSACTION
		/*-------------------------------------------------------------*\
		  Determine if and where to update the preferred states.
		  And also if specimen mask needs to be update.

		  If @Preferred is 0, then we are updating non-preferred
		  determination and therefore, no need to reset the one that is
		  still preferred. This only ensures there is only 1 preferred
		  at any time for the specimen.

		  We only do this if the preferred determination has actually
		  changed determination. This is necessary to avoid a timestamp
		  error. For example - if @Preferred were 1, and there was no
		  checking to see if the preferred determination had changed,
		  the record's Preferred field and the record's Timestamp would
		  be altered. The main update would then fail because the
		  timestamp is different to the one passed into the procedure.
		\*-------------------------------------------------------------*/
		IF @IsForSpecimen = 1 AND @Preferred = 1 BEGIN
			DECLARE	@CurrentPrefDetKey char(16),
				@CurrentPrefTaxonDetKey char(16)

			-- Get existing preferred keys from Specimen_Unit table
			SELECT	@CurrentPrefDetKey = Preferred_Determination_Key,
				@CurrentPrefTaxonDetKey = Preferred_Taxon_Determination_Key
			FROM	Specimen_Unit
			WHERE	Collection_Unit_Key = @SpecimenCollectionUnitKey

			-- Changing to another preferred Taxon Determination
			IF @CurrentPrefTaxonDetKey <> @Key BEGIN
				-- Get existing concept's key
				SELECT		@CurrentConceptKey = TDM.Concept_Key
				FROM		Taxon_Determination TD
				INNER JOIN	Taxon_Dictionary_Concept_Mapping TDM ON TDM.Taxon_List_Item_Key = TD.Taxon_List_Item_Key
				WHERE		TD.Taxon_Determination_Key = @Key

				-- We're having a new preferred, so replace the old one.
				UPDATE	Specimen_Unit
				SET	Preferred_Taxon_Determination_Key = @Key
				WHERE	Collection_Unit_Key = @SpecimenCollectionUnitKey

				IF @@Error <> 0 GOTO RollbackAndExit

				-- Preferred state should NOT change in Taxon_Determination, so get existing value to
				-- override parameter value.
				SELECT	@Preferred = Preferred
				FROM	Taxon_Determination
				WHERE	Taxon_Determination_Key = @Key
			END

			-- Get existing concept's mask
			EXECUTE	usp_Get_Concept_Domain_Mask @CurrentConceptKey, @CurrentConceptMask OUTPUT

			-- Different mask, so switch current one OFF in Collection_Unit
			IF @CurrentConceptMask <> @ConceptMask BEGIN
				EXECUTE	usp_CollectionUnit_Update_DomainMask @SpecimenCollectionUnitKey, @CurrentConceptMask, 0
				IF @@Error <> 0 GOTO RollbackAndExit
			END
		END ELSE 
		-- Dealing with Taxon_Determination table only, for occurrences.
		-- Also means that Specimen_Collection_Unit_Key can be NULL, but that doesn't mean
		-- the value passed in is NULL though.
		IF @IsForSpecimen = 0 AND @Preferred = 1 BEGIN
			-- Not guaranteed there is an associated specimen key.
			SELECT		@CurrentConceptKey = TDM.Concept_Key,
					@SpecimenKey = TD.Specimen_Collection_Unit_Key
			FROM		Taxon_Determination TD
			INNER JOIN	Taxon_Dictionary_Concept_Mapping TDM ON TDM.Taxon_List_Item_Key = TD.Taxon_List_Item_Key
			WHERE		TD.Taxon_Occurrence_Key = @OccurrenceKey
			AND		TD.Preferred = 1

			-- We're having a new preferred, so switch the old one OFF
			UPDATE	Taxon_Determination
			SET	Preferred = 0
			WHERE	Taxon_Occurrence_Key = @OccurrenceKey
			AND	Preferred = 1

			IF @@Error <> 0 GOTO RollbackAndExit

			-- Get existing concept's mask.
			EXECUTE	usp_Get_Concept_Domain_Mask @CurrentConceptKey, @CurrentConceptMask OUTPUT

			-- New concept's mask different from current one. Refresh specimen mask is there is one.
			IF @SpecimenKey IS NOT NULL AND @CurrentConceptMask <> @ConceptMask BEGIN
				EXECUTE	usp_CollectionUnit_Update_DomainMask @SpecimenKey, @CurrentConceptMask, 0
				IF @@Error <> 0 GOTO RollbackAndExit
			END
		END

		/*-------------------------------------------------------------*\
		  Do the table update.
		\*-------------------------------------------------------------*/
		-- Get the user name key, as Taxon_Determination table doesn't have SessionID fields.
		DECLARE	@ChangedBy char(16)
		SELECT	@ChangedBy = User_Name_Key FROM Session WHERE Session_ID = @SessionID

		/*-------------------------------------------------------------------------------------*\
		  The main update statement has been split into two. The reason for this is because of 
		  incident 6174 which causes the following error: 
			"The query processor could not produce a query plan from the optimizer because 
			a query cannot update a text, ntext, or image column and a clustering key at 
			the same time."
		  Because we are doing two updates, use the Timestamp for the first update. If that one
		  fails, the next one won't run, and all is well.
		\*------------------------------------------------------------------------------------*/
		-- Update the text fields.
		UPDATE	Taxon_Determination
		SET	Comment = @Notes,
			Method = @Method
		WHERE	Taxon_Determination_Key = @Key
		AND	(@Timestamp = Timestamp)

		-- Do the main update
		UPDATE	Taxon_Determination
		SET	Taxon_List_Item_Key = @DeterminedItemKey,
			Taxon_Occurrence_Key = @OccurrenceKey,
			Vague_Date_Start = @VagueDateStart,
			Vague_Date_End = @VagueDateEnd,
			Vague_Date_Type = @VagueDateType,
			Preferred = @Preferred,
			Determiner = @DeterminerNameKey,
			Inferred_Determiner = @InferredDeterminer,	
			Determination_Type_Key = @DeterminationTypeKey,
			Determiner_Role_Key = @DeterminerRoleKey,
			Changed_By = @ChangedBy,
			Changed_Date = GetDate(),
			Specimen_Collection_Unit_Key = @SpecimenCollectionUnitKey,
			Nomenclatural_Status_Concept_Key = @NomenclaturalStatusConceptKey,
			Confidence = @Confidence,
			Used_Specimen = @UsedSpecimen
		WHERE	Taxon_Determination_Key = @Key

		SELECT	@RecordsAffected = @@ROWCOUNT,
			@Error = @@ERROR

		IF @Error <> 0 GOTO RollbackAndExit

		/*-------------------------------------------------------------*\
		  Switch bit of new mask ON in Collection_Unit.
		\*-------------------------------------------------------------*/
		IF @SpecimenCollectionUnitKey IS NOT NULL BEGIN
			EXECUTE	usp_CollectionUnit_Update_DomainMask @SpecimenCollectionUnitKey, @ConceptMask, 1
			IF @@Error <> 0 GOTO RollbackAndExit
		END

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TaxonDetermination_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_TaxonDetermination_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_TaxonDetermination_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_TaxonDetermination_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_TaxonDetermination_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_TaxonDetermination_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_TaxonDetermination_Update TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_TaxonFact_ImportConceptGroup]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_TaxonFact_ImportConceptGroup]
GO

/*===========================================================================*\
  Description:	Import taxon facts corresponding to the facts associated with
				a concept group.

  Parameters:	@job_id					Job identifier

  Created:		Jan 2004

  Last revision information:
	$Revision: 7 $
	$Date: 6/02/09 10:41 $
	$Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_TaxonFact_ImportConceptGroup]
	@job_id				INT
AS
	SET NOCOUNT ON

	DECLARE		@concept_group_key		CHAR(16),
				@thesaurus_fact_key		CHAR(16),
				@taxon_fact_key			CHAR(16),
				@type					VARCHAR(1),
				@taxon_version_key		CHAR(16),
				@entered_by				CHAR(16),
				@entry_date				SMALLDATETIME,
				@changed_by				CHAR(16),
				@changed_date			SMALLDATETIME,
				@source_key				CHAR(16),
				@source_join_key		CHAR(16)

	/* determine parameters of job */
	SELECT		@concept_group_key			=	j.Concept_Group_Key
	FROM		Import_Export_Job			AS	j
	WHERE		j.Import_Export_Job_ID		=	@job_id

	IF @@ROWCOUNT = 0
	BEGIN
		RAISERROR ('Job does not exist or has not been configured', 16, 1)
		GOTO fail
	END

	EXECUTE		usp_Import_Export_Job_UpdateStatus	@job_id,
													'Exporting facts'
	IF @@ERROR <> 0 GOTO fail

	DECLARE		facts	CURSOR LOCAL FAST_FORWARD FOR
	SELECT DISTINCT
				f.Thesaurus_Fact_Key,
				CASE f.Fact_Type_Concept_Key
					WHEN 'SYSTEM00000002NO' THEN 'T' /* HTML */
					WHEN 'SYSTEM00000002L9' THEN 'A' /* AVI */
					WHEN 'SYSTEM00000002L8' THEN 'W' /* WAV */
					WHEN 'SYSTEM00000000W0' THEN 'B' /* Bitmap */
					WHEN 'SYSTEM00000000VY' THEN 'J' /* JPEG */
					ELSE 'T' /* Unknown types mapped to HTML */
				END,
				vm.Taxon_Version_Key,
				es.User_Name_Key,
				CONVERT(SMALLDATETIME,
						CONVERT(VARCHAR, es.Date_Time_Start, 112)),
				cs.User_Name_Key,
				CONVERT(SMALLDATETIME,
						CONVERT(VARCHAR, cs.Date_Time_Start, 112))
	FROM		Concept									AS	c
	INNER JOIN	Thesaurus_Fact							AS	f
	ON			f.Meaning_Key							=	c.Meaning_Key
	INNER JOIN	Taxon_Dictionary_Term_Version_Mapping	AS	vm
	ON			vm.Term_Version_Key						=	c.Term_Version_Key
	INNER JOIN	Session									AS	es
	ON			es.Session_ID							=	f.Entered_Session_ID
	LEFT JOIN	Session									AS	cs
	ON			cs.Session_ID							=	f.Changed_Session_ID
	WHERE		c.Concept_Group_Key						=	@concept_group_key

	OPEN		facts

	WHILE 1 = 1
	BEGIN
		FETCH		facts
		INTO        @thesaurus_fact_key,
					@type,
					@taxon_version_key,
					@entered_by,
					@entry_date,
					@changed_by,
					@changed_date

		IF @@FETCH_STATUS <> 0 BREAK

		BEGIN TRANSACTION

		SELECT		@taxon_fact_key							=	NULL,
					@source_join_key						=	NULL,
					@source_key								=	NULL

		SELECT		@taxon_fact_key							=	m.Taxon_Fact_Key,
					@source_key								=	j.Source_Key
		FROM		Taxon_Dictionary_Thesaurus_Fact_Mapping	AS	m
		LEFT JOIN	Source_Join								AS	j
		ON			j.Source_Join_Key						=	m.Source_Join_Key
		WHERE		m.Thesaurus_Fact_Key					=	@thesaurus_fact_key

		IF @source_key IS NULL
		BEGIN
			/* there is no existing mapping for the source join; pick an
			 * arbitrary join record (if there are any) and make this the
			 * mapped join.
			 */
			SELECT		@source_join_key	=	Source_Join_Key,
						@source_key			=	Source_Key
			FROM		Source_Join
			WHERE		Record_Key			=	@thesaurus_fact_key
			AND			Table_Name			=	'Thesaurus_Fact'
			ORDER BY	Source_Join_Key
		END

		IF @taxon_fact_key IS NOT NULL
		BEGIN
			/* update existing taxon fact */
			UPDATE		TAXON_FACT
			SET			TITLE						=	tf.Item_Name,
						TYPE						=	CASE WHEN TYPE = 'S' AND @type = 'T'
															THEN 'S'
															ELSE @type
														END,
						DATA						=	tf.Data,
						TAXON_VERSION_KEY			=	@taxon_version_key,
						FACT_VAGUE_DATE_START		=	tf.Fact_Vague_Date_Start,
						FACT_VAGUE_DATE_END			=	tf.Fact_Vague_Date_End,
						FACT_VAGUE_DATE_TYPE		=	tf.Fact_Vague_Date_Type,
						ENTERED_BY					=	@entered_by,
						ENTRY_DATE					=	@entry_date,
						CHANGED_BY					=	@changed_by,
						CHANGED_DATE				=	@changed_date,
						SYSTEM_SUPPLIED_DATA		=	tf.System_Supplied_Data,
						SOURCE_KEY					=	@source_key
			FROM		Thesaurus_Fact				AS	tf,
						TAXON_FACT
			WHERE       tf.Thesaurus_Fact_Key		=	@thesaurus_fact_key
			AND			TAXON_FACT_KEY				=	@taxon_fact_key

			IF @@ERROR <> 0 GOTO fail_from_cursor

			IF @source_join_key IS NOT NULL
			BEGIN
				UPDATE		Taxon_Dictionary_Thesaurus_Fact_Mapping
				SET			Source_Join_Key							=	@source_join_key
				WHERE		Taxon_Fact_Key							=	@taxon_fact_key

				IF @@ERROR <> 0 GOTO fail_from_cursor
			END
		END
		ELSE
		BEGIN
			/* create taxon fact */
			EXECUTE		spNextKey	'TAXON_FACT',
									@taxon_fact_key		OUTPUT
			IF @@ERROR <> 0 GOTO fail_from_cursor

			INSERT		TAXON_FACT (
						TAXON_FACT_KEY,
						TITLE,
						TYPE,
						DATA,
						TAXON_VERSION_KEY,
						FACT_VAGUE_DATE_START,
						FACT_VAGUE_DATE_END,
						FACT_VAGUE_DATE_TYPE,
						ENTERED_BY,
						ENTRY_DATE,
						CHANGED_BY,
						CHANGED_DATE,
						SYSTEM_SUPPLIED_DATA,
						SOURCE_KEY)
			SELECT		@taxon_fact_key,
						tf.Item_Name,
						@type,
						tf.Data,
						@taxon_version_key,
						tf.Fact_Vague_Date_Start,
						tf.Fact_Vague_Date_End,
						tf.Fact_Vague_Date_Type,
						@entered_by,
						@entry_date,
						@changed_by,
						@changed_date,
						tf.System_Supplied_Data,
						@source_key
			FROM        Thesaurus_Fact				AS	tf
			WHERE		tf.Thesaurus_Fact_Key		=	@thesaurus_fact_key

			IF @@ERROR <> 0 GOTO fail_from_cursor

			/* record mapping */
			INSERT		Taxon_Dictionary_Thesaurus_Fact_Mapping (
						Taxon_Fact_Key,
						Thesaurus_Fact_Key,
						Source_Join_Key)
			VALUES		(@taxon_fact_key,
						@thesaurus_fact_key,
						@source_join_key)

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END

		IF @type <> 'T'
		BEGIN
			/* convert "Data" to an HTML anchor; this couldn't be done on the
			 * INSERT/UPDATE above because the '+' operator is not defined
			 * for the "text" data type.
			 */
			DECLARE		@data_ptr		BINARY(16),
						@data_length	INT

			SELECT		@data_ptr		=	TEXTPTR(DATA),
						@data_length	=	DATALENGTH(DATA)
			FROM		TAXON_FACT
			WHERE		TAXON_FACT_KEY	=	@taxon_fact_key

			UPDATETEXT	TAXON_FACT.DATA @data_ptr @data_length 0 '"></a>'
			IF @@ERROR <> 0 GOTO fail_from_cursor

			UPDATETEXT	TAXON_FACT.DATA @data_ptr 0 0 '<a href="'
			IF @@ERROR <> 0 GOTO fail_from_cursor
		END

		EXECUTE		usp_Import_Export_Job_RecordProcessed	@job_id
		IF @@ERROR <> 0 GOTO fail_from_cursor

		COMMIT TRANSACTION
	END

	CLOSE		facts
	DEALLOCATE	facts
	RETURN

fail_from_cursor:
	CLOSE		facts
	DEALLOCATE	facts

fail:
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_TaxonFact_ImportConceptGroup failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TaxonFact_ImportConceptGroup') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_TaxonFact_ImportConceptGroup'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_TaxonFact_ImportConceptGroup TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_TaxonFact_ImportConceptGroup TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_TaxonFact_ImportConceptGroup TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_TaxonListItem_ImportConceptGroup]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_TaxonListItem_ImportConceptGroup]
GO

/*===========================================================================*\
  Description:	Import taxon list items corresponding to the concepts in a
				concept group.

  Parameters:	@job_id					Job identifier

  Created:		Dec 2003

  Last revision information:
	$Revision: 7 $
	$Date: 6/02/09 10:41 $
	$Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_TaxonListItem_ImportConceptGroup]
	@job_id				INT
AS
	SET NOCOUNT ON

	DECLARE     @concept_group_key		CHAR(16),
				@concept_key			CHAR(16),
				@taxon_list_item_key	CHAR(16),
				@term_key				CHAR(16),
				@term_version_key		CHAR(16),
				@list_preferred			BIT,
				@meaning_key			CHAR(16),
				@taxon_key				CHAR(16),
				@taxon_version_key		CHAR(16),
				@preferred_name			CHAR(16),
				@sort_code				INT,
				@taxon_rank_key			CHAR(16),
				@ins_user_key			CHAR(16),
				@ins_date				SMALLDATETIME,
				@upd_user_key			CHAR(16),
				@upd_date				SMALLDATETIME,
				@system					BIT,
				@from_list_version_key	CHAR(16),
				@to_list_version_key	CHAR(16),
				@is_new					BIT

	/* determine parameters of job */
	SELECT      @concept_group_key			=	j.Concept_Group_Key
	FROM		Import_Export_Job			AS	j
	WHERE		j.Import_Export_Job_ID		=	@job_id

	IF @@ROWCOUNT = 0
	BEGIN
		RAISERROR ('Job does not exist or has not been configured', 16, 1)
		GOTO fail
	END

	EXECUTE		usp_Import_Export_Job_UpdateStatus	@job_id,
													'Exporting concepts'
	IF @@ERROR <> 0 GOTO fail

	DECLARE		concepts	CURSOR FAST_FORWARD LOCAL FOR
	SELECT		c.Concept_Key,
				c.Term_Key,
				c.Term_Version_Key,
				c.List_Preferred,
				c.Meaning_Key,
				c.Sort_Code,
				crm.Taxon_Rank_Key,
				es.User_Name_Key,
				CONVERT(SMALLDATETIME,
						CONVERT(VARCHAR, es.Date_Time_Start, 112)),
				cs.User_Name_Key,
				CONVERT(SMALLDATETIME,
						CONVERT(VARCHAR, cs.Date_Time_Start, 112)),
				c.System_Supplied_Data
	FROM		Concept									AS	c
	INNER JOIN	Taxon_Dictionary_Concept_Rank_Mapping	AS	crm
	ON			crm.Concept_Rank_Key					=	c.Concept_Rank_Key
	INNER JOIN	Session									AS	es
	ON			es.Session_ID							=	c.Entered_Session_ID
	LEFT JOIN	Session									AS	cs
	ON			cs.Session_ID							=	c.Changed_Session_ID
	WHERE		c.Concept_Group_Key						=	@concept_group_key
	ORDER BY	c.List_Preferred DESC	/* i.e. preferred names first */

	OPEN		concepts

	WHILE 1 = 1
	BEGIN
		FETCH		concepts
		INTO		@concept_key,
					@term_key,
					@term_version_key,
					@list_preferred,
					@meaning_key,
					@sort_code,
					@taxon_rank_key,
					@ins_user_key,
					@ins_date,
					@upd_user_key,
					@upd_date,
					@system

		IF @@FETCH_STATUS <> 0 BREAK

		BEGIN TRANSACTION

		SET ROWCOUNT 1

		SELECT      @from_list_version_key							=	gvm.Taxon_List_Version_Key
		FROM		Concept_History									AS	h
		INNER JOIN	Concept_Group_Version							AS	gv
		ON			gv.Concept_Group_Version_Key					=	h.Concept_Group_Version_From
		INNER JOIN	Taxon_Dictionary_Concept_Group_Version_Mapping	AS	gvm
		ON			gvm.Concept_Group_Version_Key					=	gv.Concept_Group_Version_Key
		WHERE		h.Concept_Key									=	@concept_key
		ORDER BY	gv.Sequence

		SELECT		@to_list_version_key							=	gvm.Taxon_List_Version_Key
		FROM		Concept_History									AS	h
		INNER JOIN	Concept_Group_Version							AS	gv
		ON			gv.Concept_Group_Version_Key					=	h.Concept_Group_Version_To
		INNER JOIN	Taxon_Dictionary_Concept_Group_Version_Mapping	AS	gvm
		ON			gvm.Concept_Group_Version_Key					=	gv.Concept_Group_Version_Key
		WHERE		h.Concept_Key									=	@concept_key
		ORDER BY	gv.Sequence DESC

		IF @@ROWCOUNT = 0
		BEGIN
			SET			@to_list_version_key	=	NULL
		END

		/* we do the term and term version mappings inside the 'SET ROWCOUNT 1'
		 * because a single term may map onto multiple taxa; we need to choose
		 * exactly one, but any one will do
		 */
		SELECT		@taxon_key						=	Taxon_Key
		FROM		Taxon_Dictionary_Term_Mapping
		WHERE		Term_Key						=	@term_key

		IF @@ROWCOUNT = 0 GOTO skip_item

		IF @term_version_key IS NULL
		BEGIN
			SET			@taxon_version_key	=	NULL
		END
		ELSE
		BEGIN
			SELECT		@taxon_version_key						=	m.Taxon_Version_Key
			FROM		Taxon_Dictionary_Term_Version_Mapping	AS	m
			INNER JOIN	TAXON_VERSION							AS	tv
			ON			tv.TAXON_VERSION_KEY					=	m.Taxon_Version_Key
			WHERE		m.Term_Version_Key						=	@term_version_key
			AND			tv.TAXON_KEY							=	@taxon_key

			IF @@ROWCOUNT = 0 GOTO skip_item				
		END

		SET ROWCOUNT 0

		/* check for existing mapping */
		SELECT		@taxon_list_item_key				=	Taxon_List_Item_Key
		FROM		Taxon_Dictionary_Concept_Mapping
		WHERE		Concept_Key							=	@concept_key

		SET			@is_new		=	CASE WHEN @@ROWCOUNT = 0 THEN 1 ELSE 0 END

		IF @is_new = 1
		BEGIN
			EXECUTE		spNextKey	'TAXON_LIST_ITEM',
									@taxon_list_item_key	OUTPUT
			IF @@ERROR <> 0 GOTO fail_from_cursor
		END

		/* work out preferred name */
		IF @list_preferred = 1
		BEGIN
			SET			@preferred_name		=	@taxon_list_item_key
		END
		ELSE
		BEGIN
			SET			@preferred_name						=	NULL

			SET ROWCOUNT 1

			SELECT		@preferred_name						=	m.Taxon_List_Item_Key
			FROM		Concept								AS	c
			INNER JOIN 	Taxon_Dictionary_Concept_Mapping	AS	m
			ON			m.Concept_Key						=	c.Concept_Key
			WHERE		c.Concept_Group_Key					=	@concept_group_key
			AND			c.Meaning_Key						=	@meaning_key
			AND			c.List_Preferred					=	1

			SET ROWCOUNT 0
		END

		IF @preferred_name IS NOT NULL AND @taxon_version_key IS NULL
		BEGIN
			/* create a minimal taxon version */
			EXECUTE		spNextKey	'TAXON_VERSION',
									@taxon_version_key	OUTPUT
			IF @@ERROR <> 0 GOTO fail_from_cursor

			INSERT		TAXON_VERSION (
						TAXON_VERSION_KEY,
						TAXON_KEY,
						ENTERED_BY,
						ENTRY_DATE,
						SYSTEM_SUPPLIED_DATA)
			VALUES		(@taxon_version_key,
						@taxon_key,
						@ins_user_key,
						@ins_date,
						@system)

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END

		IF @is_new = 0
		BEGIN
			IF @preferred_name IS NULL
			BEGIN
				/* concept is not selectable; remove any list items */
				DELETE		TAXON_LIST_ITEM
				WHERE		TAXON_LIST_ITEM_KEY		=	@taxon_list_item_key

				IF @@ERROR <> 0 GOTO fail_from_cursor
			END
			ELSE
			BEGIN
				/* update taxon list item */
				UPDATE		TAXON_LIST_ITEM
				SET			TAXON_VERSION_KEY		=	@taxon_version_key,
							TAXON_LIST_VERSION_KEY	=	@from_list_version_key,
							TAXON_LIST_VERSION_TO	=	@to_list_version_key,
							PREFERRED_NAME			=	@preferred_name,
							SORT_CODE				=	@sort_code,
							TAXON_RANK_KEY			=	@taxon_rank_key,
							ENTERED_BY				=	@ins_user_key,
							ENTRY_DATE				=	@ins_date,
							CHANGED_BY				=	@upd_user_key,
							CHANGED_DATE			=	@upd_date,
							SYSTEM_SUPPLIED_DATA	=	@system
				WHERE		TAXON_LIST_ITEM_KEY		=	@taxon_list_item_key

				IF @@ERROR <> 0 GOTO fail_from_cursor
			END
		END
		ELSE
		BEGIN
			/* create taxon list item */
			INSERT		TAXON_LIST_ITEM (
						TAXON_LIST_ITEM_KEY,
						TAXON_VERSION_KEY,
						TAXON_LIST_VERSION_KEY,
						TAXON_LIST_VERSION_TO,
						PREFERRED_NAME,
						SORT_CODE,
						TAXON_RANK_KEY,
						ENTERED_BY,
						ENTRY_DATE,
						CHANGED_BY,
						CHANGED_DATE,
						SYSTEM_SUPPLIED_DATA)
			VALUES		(@taxon_list_item_key,
						@taxon_version_key,
						@from_list_version_key,
						@to_list_version_key,
						@preferred_name,
						@sort_code,
						@taxon_rank_key,
						@ins_user_key,
						@ins_date,
						@upd_user_key,
						@upd_date,
						@system)

			IF @@ERROR <> 0 GOTO fail_from_cursor

			/* record mapping */
			INSERT		Taxon_Dictionary_Concept_Mapping (
						Taxon_List_Item_Key,
						Concept_key)
			VALUES		(@taxon_list_item_key,
						@concept_key)

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END
		
skip_item:
		EXECUTE		usp_Import_Export_Job_RecordProcessed	@job_id
		IF @@ERROR <> 0 GOTO fail_from_cursor

		COMMIT TRANSACTION
	END

	CLOSE		concepts
	DEALLOCATE	concepts
	RETURN

fail_from_cursor:
	CLOSE		concepts
	DEALLOCATE	concepts

fail:
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_TaxonListItem_ImportConceptGroup failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TaxonListItem_ImportConceptGroup') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_TaxonListItem_ImportConceptGroup'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_TaxonListItem_ImportConceptGroup TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_TaxonListItem_ImportConceptGroup TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_TaxonListItem_ImportConceptGroup TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_TaxonListItem_ImportRelationships]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_TaxonListItem_ImportRelationships]
GO

/*===========================================================================*\
  Description:	Import parent-child relationships between items from the
				specified concept group.

  Parameters:	@job_id					Job identifier

  Created:		Dec 2003

  Last revision information:
	$Revision: 7 $
	$Date: 6/02/09 10:41 $
	$Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_TaxonListItem_ImportRelationships]
	@job_id				INT
AS
	SET NOCOUNT ON

	DECLARE     @concept_group_key				CHAR(16),
				@thesaurus_relation_type_key	CHAR(16),
				@child_list_item_key			CHAR(16),
				@parent_list_item_key			CHAR(16)

	/* determine parameters of job */
	SELECT		@concept_group_key						=	g.Concept_Group_Key,
				@thesaurus_relation_type_key			=	g.Hierarchy_Relation_Type_Key
	FROM		Import_Export_Job						AS	j
	INNER JOIN	Concept_Group							AS	g
	ON			g.Concept_Group_Key						=	j.Concept_Group_Key
	WHERE		j.Import_Export_Job_ID					=	@job_id

	IF @@ROWCOUNT = 0
	BEGIN
		RAISERROR ('Job does not exist or has not been configured', 16, 1)
		GOTO fail
	END

	EXECUTE		usp_Import_Export_Job_UpdateStatus	@job_id,
													'Exporting concept relationships'
	IF @@ERROR <> 0 GOTO fail

	UPDATE		TAXON_LIST_ITEM
	SET			PARENT								=	pm.Taxon_List_Item_Key
	FROM		Concept								AS	c
	INNER JOIN	Taxon_Dictionary_Concept_Mapping	AS	cm
	ON			cm.Concept_Key						=	c.Concept_Key
	INNER JOIN	TAXON_LIST_ITEM
	ON			TAXON_LIST_ITEM.TAXON_LIST_ITEM_KEY	=	cm.Taxon_List_Item_Key
	LEFT JOIN	Concept_Relation					AS	r
	ON			r.To_Concept_Key					=	c.Concept_Key
	AND			r.Thesaurus_Relation_Type_Key		=	@thesaurus_relation_type_key
	LEFT JOIN	Taxon_Dictionary_Concept_Mapping	AS	pm
	ON			pm.Concept_Key						=	r.From_Concept_Key
	WHERE		c.Concept_Group_Key					=	@concept_group_key

	IF @@ERROR <> 0 GOTO fail
	RETURN

fail:
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_TaxonListItem_ImportRelationships failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TaxonListItem_ImportRelationships') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_TaxonListItem_ImportRelationships'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_TaxonListItem_ImportRelationships TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_TaxonListItem_ImportRelationships TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_TaxonListItem_ImportRelationships TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_TaxonListVersion_ImportConceptGroup]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_TaxonListVersion_ImportConceptGroup]
GO

/*===========================================================================*\
  Description:	Import taxon list versions corresponding to the versions
				of a concept group.

  Parameters:	@job_id					Job identifier

  Created:		Dec 2003

  Last revision information:
	$Revision: 7 $
	$Date: 6/02/09 10:41 $
	$Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_TaxonListVersion_ImportConceptGroup]
	@job_id					INT
AS
	SET NOCOUNT ON

	DECLARE		@concept_group_key				CHAR(16),
				@taxon_list_key					CHAR(16),
				@concept_group_version_key		CHAR(16),
				@version						INT,
				@vague_date_start				INT,
				@vague_date_end					INT,
				@vague_date_type				VARCHAR(2),
				@entered_by						CHAR(16),
				@entry_date						SMALLDATETIME,
				@changed_by						CHAR(16),
				@changed_date					SMALLDATETIME,
				@system							BIT,
				@taxon_list_version_key			CHAR(16),
				@source_key						CHAR(16),
				@source_join_key				CHAR(16)

	/* determine parameters of job */
	SELECT      @concept_group_key						=	m.Concept_Group_Key,
				@taxon_list_key							=	m.Taxon_List_Key
	FROM		Import_Export_Job						AS	j
	INNER JOIN	Taxon_Dictionary_Concept_Group_Mapping	AS	m
	ON			m.Concept_Group_Key						=	j.Concept_Group_Key
	WHERE		j.Import_Export_Job_ID					=	@job_id

	IF @@ROWCOUNT = 0
	BEGIN
		RAISERROR ('Job does not exist or has not been configured', 16, 1)
		GOTO fail
	END

	EXECUTE		usp_Import_Export_Job_UpdateStatus	@job_id,
													'Exporting concept group versions'
	IF @@ERROR <> 0 GOTO fail

	DECLARE		versions	CURSOR LOCAL FAST_FORWARD FOR
	SELECT		cgv.Concept_Group_Version_Key,
				cgv.Sequence,
				cgv.From_Vague_Date_Start,
				cgv.From_Vague_Date_End,
				cgv.From_Vague_Date_Type,
				es.User_Name_Key,
				CONVERT(SMALLDATETIME,
						CONVERT(VARCHAR, es.Date_Time_Start, 112)),
				cs.User_Name_Key,
				CONVERT(SMALLDATETIME,
						CONVERT(VARCHAR, cs.Date_Time_End, 112)),
				cgv.System_Supplied_Data
	FROM		Concept_Group_Version		AS	cgv
	INNER JOIN	Session						AS	es
	ON			es.Session_ID				=	cgv.Entered_Session_ID
	LEFT JOIN	Session						AS	cs
	ON			cs.Session_ID				=	cgv.Changed_Session_ID
	WHERE		Concept_Group_Key			=	@concept_group_key

	OPEN		versions

	WHILE 1 = 1
	BEGIN
		FETCH		versions
		INTO		@concept_group_version_key,
					@version,
					@vague_date_start,
					@vague_date_end,
					@vague_date_type,
					@entered_by,
					@entry_date,
					@changed_by,
					@changed_date,
					@system

		IF @@FETCH_STATUS <> 0 BREAK

		BEGIN TRANSACTION

		SELECT      @taxon_list_version_key							=	NULL,
					@source_join_key					   			=	NULL,
					@source_key										=	NULL

		SELECT		@taxon_list_version_key							=	m.Taxon_List_Version_Key,
					@source_key										=	j.Source_Key
		FROM		Taxon_Dictionary_Concept_Group_Version_Mapping	AS	m
		LEFT JOIN	Source_Join										AS	j
		ON			j.Source_Join_Key								=	m.Source_Join_Key
		WHERE		m.Concept_Group_Version_Key						=	@concept_group_version_key

		IF @source_key IS NULL
		BEGIN
			/* there is no existing mapping for the source join; pick an
			 * arbitrary join record (if there are any) and make this the
			 * mapped join.
			 */
			SELECT		@source_join_key	=	Source_Join_Key,
						@source_key			=	Source_Key
			FROM		Source_Join
			WHERE		Record_Key			=	@concept_group_version_key
			AND			Table_Name			=	'Concept_Group_Version'
			ORDER BY	Source_Join_Key
		END

		IF @taxon_list_version_key IS NOT NULL
		BEGIN
			/* update taxon list version */
			UPDATE		TAXON_LIST_VERSION
			SET			VERSION					=	@version,
						VAGUE_DATE_START		=	@vague_date_start,
						VAGUE_DATE_END			=	@vague_date_end,
						VAGUE_DATE_TYPE			=	@vague_date_type,
						SOURCE_KEY				=	@source_key,
						ENTERED_BY				=	@entered_by,
						ENTRY_DATE				=	@entry_date,
						CHANGED_BY				=	@changed_by,
						CHANGED_DATE			=	@changed_date,
						SYSTEM_SUPPLIED_DATA	=	@system
			WHERE		TAXON_LIST_VERSION_KEY	=	@taxon_list_version_key

			IF @@ERROR <> 0 GOTO fail_from_cursor

			IF @source_join_key IS NOT NULL
			BEGIN
				UPDATE		Taxon_Dictionary_Concept_Group_Version_Mapping
				SET			Source_Join_Key									=	@source_join_key
				WHERE		Taxon_List_Version_Key							=	@taxon_list_version_key

				IF @@ERROR <> 0 GOTO fail_from_cursor
			END
		END
		ELSE
		BEGIN
			/* create taxon list version */
			EXECUTE		spNextKey	'TAXON_LIST_VERSION',
									@taxon_list_version_key		OUTPUT
			IF @@ERROR <> 0 GOTO fail_from_cursor

			INSERT		TAXON_LIST_VERSION (
						TAXON_LIST_VERSION_KEY,
						TAXON_LIST_KEY,
						VERSION,
						VAGUE_DATE_START,
						VAGUE_DATE_END,
						VAGUE_DATE_TYPE,
						SOURCE_KEY,
						ENTERED_BY,
						ENTRY_DATE,
						CHANGED_BY,
						CHANGED_DATE,
						SYSTEM_SUPPLIED_DATA)
			VALUES		(@taxon_list_version_key,
						@taxon_list_key,
						@version,
						@vague_date_start,
						@vague_date_end,
						@vague_date_type,
						@source_key,
						@entered_by,
						@entry_date,
						@changed_by,
						@changed_date,
						@system)

			IF @@ERROR <> 0 GOTO fail_from_cursor

			/* record mapping */
			INSERT		Taxon_Dictionary_Concept_Group_Version_Mapping (
						Taxon_List_Version_Key,
						Concept_Group_Version_Key,
						Source_Join_Key)
			VALUES		(@taxon_list_version_key,
						@concept_group_version_key,
						@source_join_key)

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END

		EXECUTE		usp_Import_Export_Job_RecordProcessed	@job_id
		IF @@ERROR <> 0 GOTO fail_from_cursor
		
		COMMIT TRANSACTION
	END

	CLOSE		versions
	DEALLOCATE	versions
	RETURN

fail_from_cursor:
	CLOSE		versions
	DEALLOCATE	versions

fail:
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_TaxonListVersion_ImportConceptGroup failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TaxonListVersion_ImportConceptGroup') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_TaxonListVersion_ImportConceptGroup'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_TaxonListVersion_ImportConceptGroup TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_TaxonListVersion_ImportConceptGroup TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_TaxonListVersion_ImportConceptGroup TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_TaxonList_GetConceptGroup]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_TaxonList_GetConceptGroup]
GO

/*===========================================================================*\
  Description:	Determine the concept group (if any) associated with the
				specified taxon list.

  Parameters:	@taxon_list_key			Taxon list key
				@concept_group_key		[on exit] Concept group key, or NULL

  Created:		Jan 2004

  Last revision information:
	$Revision: 7 $
	$Date: 6/02/09 10:41 $
	$Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_TaxonList_GetConceptGroup]
	@taxon_list_key				CHAR(16),
	@concept_group_key			CHAR(16)	OUTPUT
AS
	SET NOCOUNT ON

	SELECT		@concept_group_key						=	Concept_Group_Key
	FROM		Taxon_Dictionary_Concept_Group_Mapping
	WHERE		Taxon_List_Key							=	@taxon_list_key

	IF @@ROWCOUNT = 0
	BEGIN
		SET			@concept_group_key	=	NULL
	END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TaxonList_GetConceptGroup') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_TaxonList_GetConceptGroup'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_TaxonList_GetConceptGroup TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_TaxonList_GetConceptGroup TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_TaxonList_GetConceptGroup TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_TaxonList_ImportConceptGroup]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_TaxonList_ImportConceptGroup]
GO

/*===========================================================================*\
  Description:	Import a concept group as a taxon list.

  Parameters:   @job_id					Job identifier
				@taxon_list_key			Taxon list key
				@concept_group_key		Concept group key

  Created:		Dec 2003

  Last revision information:
	$Revision: 7 $
	$Date: 6/02/09 10:41 $
	$Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_TaxonList_ImportConceptGroup]
	@job_id				INT,
	@taxon_list_key		CHAR(16),
	@concept_group_key	CHAR(16),
	@SessionID	CHAR(16)
AS
	SET NOCOUNT ON

	DECLARE		@existing_list_key		CHAR(16)

	SELECT		@existing_list_key						=	Taxon_List_Key
	FROM		Taxon_Dictionary_Concept_Group_Mapping
	WHERE		Concept_Group_Key						=	@concept_group_key

	IF @@ROWCOUNT = 0
	BEGIN
		/* record mapping */
		INSERT		Taxon_Dictionary_Concept_Group_Mapping (
					Taxon_List_Key,
					Concept_Group_Key)
		VALUES		(@taxon_list_key,
					@concept_group_key)

		IF @@ERROR <> 0 RETURN
	END
	ELSE IF @existing_list_key <> @taxon_list_key
	BEGIN
		RAISERROR (
			'Concept group has previously been imported into a different taxon list',
			16,
			1)
		RETURN
	END

	/* Calculate size of job */
	DECLARE		@record_count			INT

	SELECT		@record_count			=	COUNT(*)
	FROM		Concept_Group_Version
	WHERE		Concept_Group_Key		=	@concept_group_key

	SELECT		@record_count							=	@record_count * 3
															+ COUNT(DISTINCT c.Name_Type_Concept_Key)
															+ COUNT(DISTINCT c.Term_Key)
															+ COUNT(DISTINCT c.Term_Version_Key)
															+ COUNT(DISTINCT j.Source_Join_Key)
															+ COUNT(DISTINCT c.Concept_Rank_Key)
															+ COUNT(DISTINCT c.Concept_Key)
															+ COUNT(DISTINCT d.Designation_Type_Concept_Key)
															+ COUNT(DISTINCT d.Concept_Designation_Key)
															+ COUNT(DISTINCT f.Thesaurus_Fact_Key
																	+ vm.Term_Version_Key)
	FROM		Concept									AS	c
	LEFT JOIN	Source_Join								AS	j
	ON			j.Record_Key							=	c.Term_Key
	AND			j.Table_Name							=	'Term'
	LEFT JOIN	Concept_Designation						AS	d
	ON			d.Concept_Key							=	c.Concept_Key
	LEFT JOIN	Thesaurus_Fact							AS	f
	ON			f.Meaning_Key							=	c.Meaning_Key
	LEFT JOIN	Taxon_Dictionary_Term_Version_Mapping	AS	vm
	ON			vm.Term_Version_Key						=	c.Term_Version_Key
	WHERE		c.Concept_Group_Key						=	@concept_group_key

	EXECUTE		usp_Import_Export_Job_Configure		@job_id,
													@concept_group_key,
													@record_count
	IF @@ERROR <> 0 RETURN
	
	/* import versions */
	EXECUTE		usp_TaxonListVersion_ImportConceptGroup		@job_id
	IF @@ERROR <> 0 RETURN

	/* import name types */
	EXECUTE		usp_TaxonNameType_ImportConceptGroup	@job_id
	IF @@ERROR <> 0 RETURN

	/* import taxa */
	EXECUTE		usp_Taxon_ImportConceptGroup	@job_id
	IF @@ERROR <> 0 RETURN

	/* import taxon versions */
	EXECUTE		usp_TaxonVersion_ImportConceptGroup		@job_id, @SessionID
	IF @@ERROR <> 0 RETURN

	/* import taxon/source relationships */
	EXECUTE		usp_TaxonSources_ImportConceptGroup		@job_id
	IF @@ERROR <> 0 RETURN

	/* import taxon ranks */
	EXECUTE		usp_TaxonRank_ImportConceptGroup	@job_id
	IF @@ERROR <> 0 RETURN

	/* import taxon list items */
	EXECUTE		usp_TaxonListItem_ImportConceptGroup	@job_id
	IF @@ERROR <> 0 RETURN

	/* import hierarchical relationships */
	EXECUTE		usp_TaxonListItem_ImportRelationships	@job_id
	IF @@ERROR <> 0 RETURN

	/* import designation types */
	EXECUTE		usp_TaxonDesignationType_ImportConceptGroup		@job_id
	IF @@ERROR <> 0 RETURN

	/* import designations */
	EXECUTE		usp_TaxonDesignation_ImportConceptGroup		@job_id
	IF @@ERROR <> 0 RETURN

	/* import facts */
	EXECUTE		usp_TaxonFact_ImportConceptGroup	@job_id
	IF @@ERROR <> 0 RETURN 

	EXECUTE		usp_Import_Export_Job_UpdateStatus	@job_id,
													'Updating Taxon Names Index...'
	/* Discard Index_Taxon_Name records for the concept group */
	DELETE ITN
	FROM Index_Taxon_Name ITN
	INNER JOIN Taxon_Dictionary_Concept_Mapping TDM ON TDM.Taxon_List_Item_Key=ITN.Taxon_List_Item_Key
	INNER JOIN Concept C1 ON C1.Concept_Key=TDM.Concept_Key
			AND C1.Concept_Group_Key=@Concept_Group_Key
	
	/* Rebuild Index_Taxon_Name for the concept group */
	INSERT INTO Index_Taxon_Name (Taxon_List_Item_Key, Taxon_List_Version_Key,
	 Actual_Name, Actual_Name_Italic, Common_Name, Common_Name_Italic, 
	  Preferred_Name, Preferred_Name_Italic, Abbreviation, Authority, System_Supplied_Data )
	SELECT TLI.Taxon_List_Item_Key, TLI.Taxon_List_Version_Key, 
	  T.Item_Name, CASE WHEN TR3.List_Font_Italic = 1 AND T.Language = 'La' THEN 1 ELSE 0 END, 
	  T2.Item_Name, CASE WHEN TR3.List_Font_Italic = 1 AND T2.Language = 'La' THEN 1 ELSE 0 END, 
	  T3.Item_Name, CASE WHEN TR3.List_Font_Italic = 1 AND T3.Language = 'La' THEN 1 ELSE 0 END, 
	  T.Abbreviation, T.Authority, 1 
	FROM ((((((((Taxon_List_Item AS TLI 
	INNER JOIN Taxon_Dictionary_Concept_Mapping TDM ON TDM.Taxon_List_Item_Key=TLI.Taxon_List_Item_Key
	INNER JOIN Concept C1 ON C1.Concept_Key=TDM.Concept_Key
			AND C1.Concept_Group_Key=@Concept_Group_Key
	LEFT JOIN Taxon_version AS TV ON TV.Taxon_Version_Key = TLI.Taxon_Version_Key) 
	LEFT JOIN Taxon AS T ON T.Taxon_Key = TV.Taxon_Key) 
	LEFT JOIN Taxon_Common_Name AS TCN ON TCN.Taxon_List_Item_Key = TLI.Taxon_List_Item_Key) 
	LEFT JOIN Taxon_Version AS TV2 ON TV2.Taxon_Version_Key = TCN.Taxon_Version_Key) 
	LEFT JOIN Taxon AS T2 ON T2.Taxon_Key = TV2.Taxon_Key) 
	LEFT JOIN Taxon_List_Item AS TLI3 ON TLI3.Taxon_List_Item_Key = TLI.Preferred_Name) 
	LEFT JOIN Taxon_Rank AS TR3 ON TR3.Taxon_Rank_Key = TLI3.Taxon_Rank_Key) 
	LEFT JOIN Taxon_Version AS TV3 ON TV3.Taxon_Version_Key = TLI3.Taxon_Version_Key) 
	LEFT JOIN Taxon AS T3 ON T3.Taxon_Key = TV3.Taxon_Key 
	WHERE TLI.Taxon_List_Version_To IS NULL

	UPDATE Import_Export_Job
	SET Records_Processed = Records_Processed + @@ROWCOUNT
	WHERE Import_Export_Job_ID = @job_id
	
	EXECUTE		usp_Import_Export_Job_UpdateStatus	@job_id,
													'Exporting Taxon Common Names...'	

	/* Create a local table containing the taxon common name data */
	DECLARE @TaxonCommonName TABLE (
		Taxon_List_Item_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS PRIMARY KEY,
		Taxon_Version_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS
	)

	INSERT INTO @TaxonCommonName
	SELECT DISTINCT TDM1.Taxon_List_Item_Key, TLI.Taxon_Version_Key
	FROM Taxon_Dictionary_Concept_Mapping TDM1 
	INNER JOIN Concept C1 ON C1.Concept_Key=TDM1.Concept_Key
			AND C1.Concept_Group_Key=@Concept_Group_Key
	LEFT JOIN (
			Concept C2 
			INNER JOIN Term T ON T.Term_Key=C2.Term_Key
			INNER JOIN Language L ON L.Language_Key=T.Language_Key AND L.Priority=1
		) ON C2.Meaning_Key=C1.Meaning_Key
			AND C2.Preferred=1
			AND C2.Name_Type_Concept_Key='SYSTEM000000000L'
	LEFT JOIN Concept C3 ON C3.Meaning_Key=C1.Meaning_Key
		AND C3.List_Preferred=1
		AND C3.Concept_Group_Key=C1.Concept_Group_Key
	INNER JOIN Taxon_Dictionary_Concept_Mapping TDM2 ON TDM2.Concept_Key=ISNULL(C2.Concept_Key, C3.Concept_Key)
	INNER JOIN Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key=TDM2.Taxon_List_Item_Key

	UPDATE Import_Export_Job
	SET Records_Processed = Records_Processed + @@ROWCOUNT
	WHERE Import_Export_Job_ID = @job_id

	/* Update existing taxon common name records that are out of date */
	UPDATE TCN
	SET Taxon_Version_Key=TCNTmp.Taxon_Version_Key
	FROM @TaxonCommonName TCNTmp
	INNER JOIN Taxon_Common_Name TCN ON TCN.Taxon_List_Item_Key=TCNTmp.Taxon_List_Item_Key
	WHERE TCN.Taxon_Version_Key=TCNTmp.Taxon_Version_Key
		
	/* Insert any new required taxon common name records */
	INSERT INTO Taxon_Common_Name
	SELECT DISTINCT TCNTmp.Taxon_List_Item_Key, TCNTmp.Taxon_Version_Key
	FROM @TaxonCommonName TCNTmp
	LEFT JOIN Taxon_Common_Name TCN ON TCN.Taxon_List_Item_Key=TCNTmp.Taxon_List_Item_Key
	WHERE TCN.Taxon_List_Item_Key IS NULL

	UPDATE Import_Export_Job
	SET Records_Processed = Records_Processed + @@ROWCOUNT
	WHERE Import_Export_Job_ID = @job_id


GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TaxonList_ImportConceptGroup') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_TaxonList_ImportConceptGroup'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_TaxonList_ImportConceptGroup TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_TaxonList_ImportConceptGroup TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_TaxonList_ImportConceptGroup TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_TaxonList_Select]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_TaxonList_Select]
GO

/*===========================================================================*\
  Description:	Select all taxon lists.

  Parameters:

  Created:		Jan 2004

  Last revision information:
	$Revision: 7 $
	$Date: 6/02/09 10:41 $
	$Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_TaxonList_Select]
AS
	SET NOCOUNT ON

	SELECT		TAXON_LIST_KEY,
				ITEM_NAME
	FROM		TAXON_LIST
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TaxonList_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_TaxonList_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_TaxonList_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_TaxonList_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_TaxonList_Select TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_TaxonNameType_ImportConceptGroup]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_TaxonNameType_ImportConceptGroup]
GO

/*===========================================================================*\
  Description:	Import taxon name types corresponding to the name type
				concepts used by the concepts in the specified concept group.

  Parameters:	@job_id					Job identifier

  Created:		Dec 2003

  Last revision information:
	$Revision: 7 $
	$Date: 6/02/09 10:41 $
	$Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_TaxonNameType_ImportConceptGroup]
	@job_id					INT
AS
	SET NOCOUNT ON

	DECLARE     @concept_group_key		CHAR(16),
				@name_type_concept_key	CHAR(16),
				@short_name				VARCHAR(20),
				@authority				VARCHAR(50),
				@entered_by				CHAR(16),
				@entry_date				SMALLDATETIME,
				@system					BIT,
				@taxon_name_type_key	CHAR(16),
				@system_mapping			BIT

	/* determine parameters of job */
	SELECT      @concept_group_key			=	j.Concept_Group_Key
	FROM		Import_Export_Job			AS	j
	WHERE		j.Import_Export_Job_ID		=	@job_id

	IF @@ROWCOUNT = 0
	BEGIN
		RAISERROR ('Job does not exist or has not been configured', 16, 1)
		GOTO fail
	END

	EXECUTE		usp_Import_Export_Job_UpdateStatus	@job_id,
													'Exporting thesaurus name types'
	IF @@ERROR <> 0 GOTO fail

	DECLARE		name_types	CURSOR LOCAL FAST_FORWARD FOR
	SELECT DISTINCT
				nt.Concept_Key,
				t.Plaintext,
				tv.Author_And_Date,
				es.User_Name_Key,
				CONVERT(SMALLDATETIME,
						CONVERT(VARCHAR, es.Date_Time_Start, 112)),
				nt.System_Supplied_Data
	FROM		Concept						AS	c
	INNER JOIN	Concept						AS	nt
	ON			nt.Concept_Key				=	c.Name_Type_Concept_Key
	INNER JOIN	Term						AS	t
	ON			t.Term_Key					=	nt.Term_Key
	LEFT JOIN	Term_Version				AS	tv
	ON			tv.Term_Version_Key			=	nt.Term_Version_Key
	INNER JOIN	Session						AS	es
	ON			es.Session_ID				=	nt.Entered_Session_ID
	WHERE		c.Concept_Group_Key			=	@concept_group_key

	OPEN		name_types

	WHILE 1 = 1
	BEGIN
		FETCH		name_types
		INTO        @name_type_concept_key,
					@short_name,
					@authority,
					@entered_by,
					@entry_date,
					@system

		IF @@FETCH_STATUS <> 0 BREAK

		BEGIN TRANSACTION

		SELECT		@taxon_name_type_key				=	Taxon_Name_Type_Key,
					@system_mapping						=	System_Supplied_Data
		FROM		Taxon_Dictionary_Name_Type_Mapping
		WHERE		Thesaurus_Name_Type_Key				=	@name_type_concept_key

		IF @@ROWCOUNT > 0
		BEGIN
			IF @system_mapping = 0
			BEGIN
				/* update name type */
				UPDATE		TAXON_NAME_TYPE
				SET			SHORT_NAME				=	@short_name,
							AUTHORITY				=	@authority,
							ENTERED_BY				=	@entered_by,
							ENTRY_DATE				=	@entry_date,
							SYSTEM_SUPPLIED_DATA	=	@system
				WHERE		TAXON_NAME_TYPE_KEY		=	@taxon_name_type_key

				IF @@ERROR <> 0 GOTO fail_from_cursor
			END
		END
		ELSE
		BEGIN
			/* create name type */
			EXECUTE		spNextKey	'TAXON_NAME_TYPE',
									@taxon_name_type_key	OUTPUT
			IF @@ERROR <> 0 GOTO fail_from_cursor

			INSERT		TAXON_NAME_TYPE (
						TAXON_NAME_TYPE_KEY,
						SHORT_NAME,
						AUTHORITY,
						ENTERED_BY,
						ENTRY_DATE,
						SYSTEM_SUPPLIED_DATA)
			VALUES		(@taxon_name_type_key,
						@short_name,
						@authority,
						@entered_by,
						@entry_date,
						@system)

			IF @@ERROR <> 0 GOTO fail_from_cursor

			/* record mapping */
			INSERT		Taxon_Dictionary_Name_Type_Mapping (
						Taxon_Name_Type_Key,
						Thesaurus_Name_Type_Key,
						System_Supplied_Data)
			VALUES		(@taxon_name_type_key,
						@name_type_concept_key,
						0)

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END

		EXECUTE		usp_Import_Export_Job_RecordProcessed	@job_id
		IF @@ERROR <> 0 GOTO fail_from_cursor

		COMMIT TRANSACTION
	END

	CLOSE		name_types
	DEALLOCATE	name_types
	RETURN

fail_from_cursor:
	CLOSE		name_types
	DEALLOCATE	name_types

fail:
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_TaxonNameType_ImportConceptGroup failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TaxonNameType_ImportConceptGroup') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_TaxonNameType_ImportConceptGroup'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_TaxonNameType_ImportConceptGroup TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_TaxonNameType_ImportConceptGroup TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_TaxonNameType_ImportConceptGroup TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TaxonOccurrences_Select_ForSearch') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_TaxonOccurrences_Select_ForSearch]
GO

/*===========================================================================*\
  Description:	Returns a list of Taxon Occurrences.

  Parameters:	@SearchText

  Created:	November 2003

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_TaxonOccurrences_Select_ForSearch]
	@SearchText varchar(150)
AS

SET NOCOUNT ON

	SELECT DISTINCT XO.Taxon_Occurrence_Key AS [Item_Key],
			ITN.Actual_Name + ' - ' +
			dbo.ufn_GetDateFromVagueDate(S.Vague_Date_Start, S.Vague_Date_End, S.Vague_Date_Type) +
			' - ' + 
			CASE 
				WHEN LN.Item_Name IS NULL THEN
					CASE WHEN S.Spatial_Ref IS NULL THEN '' ELSE S.Spatial_Ref END
				ELSE LN.Item_Name + ' (' + 
					CASE 
						WHEN S.Spatial_Ref IS NULL THEN L.Spatial_Ref
						ELSE S.Spatial_Ref END +
					')'
			END
			AS SearchTerm,
			CASE ITN.Actual_Name_Italic
				WHEN 1 THEN '<i>' + ITN.Actual_Name + '</i>'
				ELSE ITN.Actual_Name 
			END + ' - ' +
			dbo.ufn_GetDateFromVagueDate(S.Vague_Date_Start, S.Vague_Date_End, S.Vague_Date_Type) +
			' - ' + 
			CASE 
				WHEN LN.Item_Name IS NULL THEN
					CASE WHEN S.Spatial_Ref IS NULL THEN '' ELSE S.Spatial_Ref END
				ELSE LN.Item_Name + ' (' + 
					CASE 
						WHEN S.Spatial_Ref IS NULL THEN L.Spatial_Ref
						ELSE S.Spatial_Ref END +
					')'
			END
			AS DisplayTerm
	FROM		Taxon_Occurrence XO
	INNER JOIN	Taxon_Determination TD ON XO.Taxon_Occurrence_Key = TD.Taxon_Occurrence_Key AND TD.Preferred = 1
	INNER JOIN	Taxon_Dictionary_Concept_Mapping TDCM ON TDCM.Taxon_List_Item_Key = TD.Taxon_List_Item_Key
	INNER JOIN	Index_Taxon_Name ITN ON ITN.Taxon_List_Item_Key=TD.Taxon_List_Item_Key
	INNER JOIN 	Sample S ON S.Sample_Key = XO.Sample_Key
	LEFT JOIN	Location L ON L.Location_Key = S.Location_Key 
	LEFT JOIN	Location_Name LN ON LN.Location_Key = L.Location_Key AND LN.Preferred = 1
	WHERE		ITN.Actual_Name LIKE @SearchText + '%'
	ORDER BY	SearchTerm

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TaxonOccurrences_Select_ForSearch') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_TaxonOccurrences_Select_ForSearch'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_TaxonOccurrences_Select_ForSearch TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_TaxonOccurrences_Select_ForSearch TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_TaxonOccurrences_Select_ForSearch TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_TaxonOccurrences_Select_ForSearch TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_TaxonOccurrences_Select_ForSearch TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_TaxonOccurrences_Select_ForSearch TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_TaxonOccurrence_TaxonDetermination_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_TaxonOccurrence_TaxonDetermination_Update]
GO

/*===========================================================================*\
  Description:	

  Parameters:	@TaxonOccurrenceKey	-Taxon Occurrence Key
		@SpecimenUnitKey	-Specimen Unit Key

  Created:	Feb 2004

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_TaxonOccurrence_TaxonDetermination_Update]
	@TaxonOccurrenceKey char(16),
	@SpecimenUnitKey char(16)
AS

	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION	
		/*----------------------------------------------------------------------------*\
		  A new Taxon_Determination record needs to be created and associated with the
		  new Taxon_Occurrence record. This will be a duplicate record of the 
		  Taxon_Determination record who's key is in the Preferred_Taxon_Determination
		  field of the Specimen_Unit table.
		\*----------------------------------------------------------------------------*/

		DECLARE @NewTaxonDeterminationKey char(16)

		EXECUTE spNextKey 'Taxon_Determination', @NewTaxonDeterminationKey OUTPUT

		INSERT INTO Taxon_Determination (
			Taxon_Determination_Key, Taxon_List_Item_Key, Taxon_Occurrence_Key, Vague_Date_Start,
			Vague_Date_End, Vague_Date_Type, Comment, Preferred, Determiner, Determination_Type_Key,
			Determiner_Role_Key, Entered_By, Entry_Date, Changed_By, Changed_Date, Source_Key,
			Custodian, Specimen_Collection_Unit_Key, Nomenclatural_Status_Concept_Key, Confidence,
			Used_Specimen, Method, Inferred_Determiner
		)
		SELECT 	@NewTaxonDeterminationKey, Taxon_List_Item_Key, @TaxonOccurrenceKey, Vague_Date_Start,
			Vague_Date_End, Vague_Date_Type, Comment, 1, Determiner, Determination_Type_Key,
			Determiner_Role_Key, Entered_By, Entry_Date, Changed_By, Changed_Date, Source_Key,
			Custodian, Specimen_Collection_Unit_Key, Nomenclatural_Status_Concept_Key, Confidence,
			Used_Specimen, Method, Inferred_Determiner
		FROM		Taxon_Determination AS TD
		INNER JOIN 	Specimen_Unit AS SU ON SU.Preferred_Taxon_Determination_Key = TD.Taxon_Determination_Key
		WHERE		SU.Collection_Unit_Key = @SpecimenUnitKey

		IF @@Error <> 0 GOTO RollBackAndExit

		-- Update Validation flag in Taxon Occurrence
		DECLARE	@ValidationLevel int
		SELECT	@ValidationLevel = Validation_Level
		FROM	Taxon_Determination TD 
		JOIN	Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key = TD.Taxon_List_Item_Key
		JOIN	Taxon_Version TV ON TV.Taxon_Version_Key = TLI.Taxon_Version_Key
		WHERE	TD.Taxon_Determination_Key = @NewTaxonDeterminationKey

		DECLARE	@CompetencyLevel int
		SELECT	@CompetencyLevel = DR.Validation_Competency
		FROM	Taxon_Determination TD 
		JOIN	Determiner_Role DR ON DR.Determiner_Role_Key = TD.Determiner_Role_Key
		WHERE	TD.Taxon_Determination_Key = @NewTaxonDeterminationKey

		UPDATE	Taxon_Occurrence
		SET	Verified = 
				CASE 
					WHEN @ValidationLevel IS NULL THEN 0
					WHEN @ValidationLevel <= @CompetencyLevel THEN 2
					ELSE 1
				END
		WHERE	Taxon_Occurrence_Key = @TaxonOccurrenceKey

		IF @@Error <> 0 GOTO RollBackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TaxonOccurrence_TaxonDetermination_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_TaxonOccurrence_TaxonDetermination_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_TaxonOccurrence_TaxonDetermination_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_TaxonOccurrence_TaxonDetermination_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_TaxonOccurrence_TaxonDetermination_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_TaxonOccurrence_TaxonDetermination_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_TaxonOccurrence_TaxonDetermination_Update TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TaxonOccurrence_Update_ForSample') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_TaxonOccurrence_Update_ForSample]
GO

/*===========================================================================*\
  Description:	Updates the Sample Key of an occurrence.

  Parameters:	@Key
		@SampleKey

  Created:	August 2004

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_TaxonOccurrence_Update_ForSample]
	@Key char(16),
	@SampleKey char(16)
AS

	BEGIN TRANSACTION

		UPDATE	Taxon_Occurrence
		SET	Sample_Key = @SampleKey
		WHERE	Taxon_Occurrence_Key = @Key

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TaxonOccurrence_Update_ForSample') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_TaxonOccurrence_Update_ForSample'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_TaxonOccurrence_Update_ForSample TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_TaxonOccurrence_Update_ForSample TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_TaxonOccurrence_Update_ForSample TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_TaxonOccurrence_Update_ForSample TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_TaxonOccurrence_Update_ForSample TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_TaxonRank_ImportConceptGroup]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_TaxonRank_ImportConceptGroup]
GO

/*===========================================================================*\
  Description:	Import taxon ranks corresponding to concept ranks from the
				specified concept group.

  Parameters:	@job_id					Job identifier

  Created:		Dec 2003

  Last revision information:
	$Revision: 7 $
	$Date: 6/02/09 10:41 $
	$Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_TaxonRank_ImportConceptGroup]
	@job_id				INT
AS
	SET NOCOUNT ON

	DECLARE     @concept_group_key		CHAR(16),
				@concept_rank_key		CHAR(16),
				@sequence				SMALLINT,
				@short_name				VARCHAR(20),
				@long_name				VARCHAR(100),
				@entered_by				CHAR(16),
				@entry_date				SMALLDATETIME,
				@changed_by				CHAR(16),
				@changed_date			SMALLDATETIME,
				@system					BIT,
				@taxon_rank_key			CHAR(16)

	/* determine parameters of job */
	SELECT      @concept_group_key			=	j.Concept_Group_Key
	FROM		Import_Export_Job			AS	j
	WHERE		j.Import_Export_Job_ID		=	@job_id

	IF @@ROWCOUNT = 0
	BEGIN
		RAISERROR ('Job does not exist or has not been configured', 16, 1)
		GOTO fail
	END

	EXECUTE		usp_Import_Export_Job_UpdateStatus	@job_id,
													'Exporting concept ranks'
	IF @@ERROR <> 0 GOTO fail

	DECLARE		ranks	CURSOR LOCAL FAST_FORWARD FOR
	SELECT DISTINCT
				cr.Concept_Rank_Key,
				cr.Sort_Order,
				cr.Abbreviation,
				cr.Item_Name,
				es.User_Name_Key,
				CONVERT(SMALLDATETIME,
						CONVERT(VARCHAR, es.Date_Time_Start, 112)),
				cs.User_Name_Key,
				CONVERT(SMALLDATETIME,
						CONVERT(VARCHAR, cs.Date_Time_Start, 112)),
				cr.System_Supplied_Data
	FROM		Concept				   			AS	c
	INNER JOIN	Concept_Rank					AS	cr
	ON			cr.Concept_Rank_Key				=	c.Concept_Rank_Key
	INNER JOIN	Session							AS	es
	ON			es.Session_ID					=	cr.Entered_Session_ID
	LEFT JOIN	Session							AS	cs
	ON			cs.Session_ID					=	cr.Changed_Session_ID
	WHERE		c.Concept_Group_Key				=	@concept_group_key

	OPEN		ranks

	WHILE 1 = 1
	BEGIN
		FETCH		ranks
		INTO		@concept_rank_key,
					@sequence,
					@short_name,
					@long_name,
					@entered_by,
					@entry_date,
					@changed_by,
					@changed_date,
					@system

		IF @@FETCH_STATUS <> 0 BREAK

		BEGIN TRANSACTION

		SELECT		@taxon_rank_key							=	Taxon_Rank_Key
		FROM		Taxon_Dictionary_Concept_Rank_Mapping
		WHERE		Concept_Rank_Key						=	@concept_rank_key

		IF @@ROWCOUNT > 0
		BEGIN
			/* update taxon rank */
		   UPDATE		TAXON_RANK
		   SET			SEQUENCE				=	@sequence,
						SHORT_NAME				=	@short_name,
						LONG_NAME				=	@long_name,
						ENTERED_BY				=	@entered_by,
						ENTRY_DATE				=	@entry_date,
						CHANGED_BY				=	@changed_by,
						CHANGED_DATE			=	@changed_date,
						SYSTEM_SUPPLIED_DATA	=	@system
		   WHERE		TAXON_RANK_KEY			=	@taxon_rank_key

		   IF @@ERROR <> 0 GOTO fail_from_cursor
		END
		ELSE
		BEGIN
			/* create taxon rank */
			EXECUTE		spNextKey		'TAXON_RANK',
										@taxon_rank_key		OUTPUT
			IF @@ERROR <> 0 GOTO fail_from_cursor

			INSERT		TAXON_RANK (
						TAXON_RANK_KEY,
						SEQUENCE,
						SHORT_NAME,
						LONG_NAME,
						LIST_FONT_ITALIC,
						DISPLAY_IN_DETAILS,
						ENTERED_BY,
						ENTRY_DATE,
						CHANGED_BY,
						CHANGED_DATE,
						SYSTEM_SUPPLIED_DATA)
			VALUES		(@taxon_rank_key,
						@sequence,
						@short_name,
						@long_name,
						0,
						0,
						@entered_by,
						@entry_date,
						@changed_by,
						@changed_date,
						@system)

			IF @@ERROR <> 0 GOTO fail_from_cursor

			/* record mapping */
			INSERT		Taxon_Dictionary_Concept_Rank_Mapping (
						Taxon_Rank_Key,
						Concept_Rank_Key)
			VALUES		(@taxon_rank_key,
						@concept_rank_key)

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END

		EXECUTE		usp_Import_Export_Job_RecordProcessed	@job_id
		IF @@ERROR <> 0 GOTO fail

		COMMIT TRANSACTION
	END

	CLOSE		ranks
	DEALLOCATE	ranks
	RETURN

fail_from_cursor:
	CLOSE		ranks
	DEALLOCATE	ranks

fail:
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_TaxonRank_ImportConceptGroup failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TaxonRank_ImportConceptGroup') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_TaxonRank_ImportConceptGroup'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_TaxonRank_ImportConceptGroup TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_TaxonRank_ImportConceptGroup TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_TaxonRank_ImportConceptGroup TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_TaxonSources_ImportConceptGroup]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_TaxonSources_ImportConceptGroup]
GO

/*===========================================================================*\
  Description:	Import taxon/source relationships corresponding to the
  				term/source relationships in a concept group.

  Parameters:   @job_id					Job identifier

  Created:		Jan 2004

  Last revision information:
	$Revision: 7 $
	$Date: 6/02/09 10:41 $
	$Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_TaxonSources_ImportConceptGroup]
	@job_id				INT
AS
	SET NOCOUNT ON

	DECLARE		@concept_group_key		CHAR(16),
				@source_join_key		CHAR(16),
				@taxon_key				CHAR(16),
				@source_key				CHAR(16),
				@original				BIT,
				@source_link_key		CHAR(16)	

	/* determine parameters of job */
	SELECT		@concept_group_key		=	Concept_Group_Key
	FROM		Import_Export_Job
	WHERE		Import_Export_Job_ID	=	@job_id

	IF @@ROWCOUNT = 0
	BEGIN
		RAISERROR ('Job does not exist or has not been configured', 16, 1)
		GOTO fail
	END

	EXECUTE		usp_Import_Export_Job_UpdateStatus	@job_id,
													'Exporting term/source relationships'
	IF @@ERROR <> 0 GOTO fail

	DECLARE		sources		CURSOR LOCAL FAST_FORWARD FOR
	SELECT		j.Source_Join_Key,
				tm.Taxon_Key,
				j.Source_Key,
				j.Original
	FROM		Concept							AS	c
	INNER JOIN	Source_Join						AS	j
	ON			j.Record_Key					=	c.Term_Key
	AND			j.Table_Name					=	'Term'
	INNER JOIN	Taxon_Dictionary_Term_Mapping	AS	tm
	ON			tm.Term_Key						=	j.Record_Key
	WHERE		c.Concept_Group_Key				=	@concept_group_key

	OPEN		sources

	WHILE 1 = 1
	BEGIN
		FETCH		sources
		INTO		@source_join_key,
					@taxon_key,
					@source_key,
					@original

		IF @@FETCH_STATUS <> 0 BREAK

		BEGIN TRANSACTION

		SELECT		@source_link_key						=	Source_Link_Key
		FROM		Taxon_Dictionary_Term_Sources_Mapping
		WHERE		Source_Join_Key							=	@source_join_key

		IF @@ROWCOUNT > 0
		BEGIN
			/* update existing taxon source */
			UPDATE		TAXON_SOURCES
			SET			TAXON_KEY			=	@taxon_key,
						SOURCE_KEY			=	@source_key,
						ORIGINAL			=	@original
			WHERE		SOURCE_LINK_KEY		=	@source_link_key

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END
		ELSE
		BEGIN
			/* create a new taxon source */
			EXECUTE		spNextKey	'TAXON_SOURCES',
									@source_link_key	OUTPUT
			IF @@ERROR <> 0 GOTO fail_from_cursor

			INSERT		TAXON_SOURCES (
						SOURCE_LINK_KEY,
						TAXON_KEY,
						SOURCE_KEY,
						ORIGINAL)
			VALUES		(@source_link_key,
						@taxon_key,
						@source_key,
						@original)

			IF @@ERROR <> 0 GOTO fail_from_cursor

			/* record mapping */
			INSERT		Taxon_Dictionary_Term_Sources_Mapping (
						Source_Link_Key,
						Source_Join_Key)
			VALUES		(@source_link_key,
						@source_join_key)

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END

		/* update progress counter */
		EXECUTE		usp_Import_Export_Job_RecordProcessed	@job_id
		IF @@ERROR <> 0 GOTO fail_from_cursor
		
		COMMIT TRANSACTION
	END

	CLOSE		sources
	DEALLOCATE	sources
	RETURN

fail_from_cursor:
	CLOSE		sources
	DEALLOCATE	sources

fail:
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_TaxonSources_ImportConceptGroup failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TaxonSources_ImportConceptGroup') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_TaxonSources_ImportConceptGroup'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_TaxonSources_ImportConceptGroup TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_TaxonSources_ImportConceptGroup TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_TaxonSources_ImportConceptGroup TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_TaxonVersion_ImportConceptGroup]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_TaxonVersion_ImportConceptGroup]
GO

/*===========================================================================*\
  Description:	Import taxon versions corresponding to items in a concept
				group.

  Parameters:	@job_id					Job identifier

  Created:		Dec 2003

  Last revision information:
	$Revision: 7 $
	$Date: 6/02/09 10:41 $
	$Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_TaxonVersion_ImportConceptGroup]
	@job_id					INT,
	@SessionID			CHAR(16)
AS
	SET NOCOUNT ON

 DECLARE     @concept_group_key			CHAR(16),
				@term_version_key			CHAR(16),
				@taxon_key					CHAR(16),
				@attribute					VARCHAR(10),
				@entered_by					CHAR(16),
				@entry_date					SMALLDATETIME,
				@changed_by					CHAR(16),
				@changed_date				SMALLDATETIME,
				@system						BIT,
				@taxon_version_key			CHAR(16),
				@source_key					CHAR(16),
				@source_join_key			CHAR(16),
				@prior_term_version_key		CHAR(16),
				@concept_key	CHAR(16),
				@term_key		CHAR(16)

SET @Concept_Group_Key='DSS0039400000003'
SET @SessionID='DSS0039400000001'
 

	DECLARE		versions	CURSOR LOCAL FAST_FORWARD FOR
	SELECT DISTINCT
				tv.Term_Version_Key,
				tm.Taxon_Key,
				tv.Version_Label,
				es.User_Name_Key,
				CONVERT(SMALLDATETIME,
						CONVERT(VARCHAR, es.Date_Time_Start, 112)),
				cs.User_Name_Key,
				CONVERT(SMALLDATETIME,
						CONVERT(VARCHAR, cs.Date_Time_Start, 112)),
				ISNULL(tv.System_Supplied_Data, c.System_Supplied_Data),
				c.Concept_Key,
				tm.Term_key
	FROM		Concept							AS	c
	LEFT JOIN	Term_Version					AS	tv
	ON			tv.Term_Version_Key				=	c.Term_Version_Key
	INNER JOIN	Taxon_Dictionary_Term_Mapping	AS	tm
	ON			tm.Term_Key						=	c.Term_Key
	INNER JOIN	Session							AS	es
	ON			es.Session_ID					=	 ISNULL(tv.Entered_Session_ID, @SessionID)
	LEFT JOIN	Session							AS	cs
	ON			cs.Session_ID					=	tv.Changed_Session_ID
	WHERE		c.Concept_Group_Key				=	@concept_group_key
	ORDER BY	tv.Term_Version_Key

	SET			@prior_term_version_key			=	''

	OPEN		versions

	WHILE 1 = 1
	BEGIN
		FETCH		versions
		INTO		@term_version_key,
					@taxon_key,
					@attribute,
					@entered_by,
					@entry_date,
					@changed_by,
					@changed_date,
					@system,
					@concept_key,
					@term_key

		IF @@FETCH_STATUS <> 0 BREAK

		BEGIN TRANSACTION

		SELECT      @taxon_version_key						=	NULL,
					@source_join_key						=	NULL,
					@source_key								=	NULL

		SELECT		@taxon_version_key						=	m.Taxon_Version_Key,
					@source_key								=	j.Source_Key
		FROM		Taxon_Dictionary_Term_Version_Mapping	AS	m
		LEFT JOIN	Source_Join								AS	j
		ON			j.Source_Join_Key						=	m.Source_Join_Key
		WHERE		m.Term_Version_Key						=	@term_version_key

		IF @source_key IS NULL
		BEGIN
			/* there is no existing mapping for the source join; pick an
			 * arbitrary join record (if there are any) and make this the
			 * mapped join.
			 */
			SELECT		@source_join_key	=	Source_Join_Key,
						@source_key			=	Source_Key
			FROM		Source_Join
			WHERE		Record_Key			=	@term_version_key
			AND			Table_Name			=	'Term_Version'
			ORDER BY	Source_Join_Key
		END

		IF @taxon_version_key IS NOT NULL
		BEGIN
			/* update taxon version */
			UPDATE		TAXON_VERSION
			SET			TAXON_KEY				=	@taxon_key,
						ATTRIBUTE				=	@attribute,
						SOURCE_KEY				=	@source_key,
						ENTERED_BY				=	@entered_by,
						ENTRY_DATE				=	@entry_date,
						CHANGED_BY				=	@changed_by,
						CHANGED_DATE			=	@changed_date,
						SYSTEM_SUPPLIED_DATA   	=	@system
			WHERE		TAXON_VERSION_KEY		=	@taxon_version_key

			IF @@ERROR <> 0 GOTO fail_from_cursor

			IF @source_join_key IS NOT NULL
			BEGIN
				UPDATE		Taxon_Dictionary_Term_Version_Mapping
				SET			Source_Join_Key							=	@source_join_key
				WHERE		Taxon_Version_Key						=	@taxon_version_key

				IF @@ERROR <> 0 GOTO fail_from_cursor
			END
		END
		ELSE
		BEGIN
			/* create taxon version */
			EXECUTE		spNextKey		'TAXON_VERSION',
										@taxon_version_key	OUTPUT
			IF @@ERROR <> 0 GOTO fail_from_cursor

			INSERT		TAXON_VERSION (
						TAXON_VERSION_KEY,
						TAXON_KEY,
						ATTRIBUTE,
						UK_NATIVE,
						SOURCE_KEY,
						ENTERED_BY,
						ENTRY_DATE,
						CHANGED_BY,
						CHANGED_DATE,
						SYSTEM_SUPPLIED_DATA)
			VALUES		(@taxon_version_key,
						@taxon_key,
						@attribute,
						0,
						@source_key,
						@entered_by,
						@entry_date,
						@changed_by,
						@changed_date,
						@system)

			IF @@ERROR <> 0 GOTO fail_from_cursor

			/* If no term version, then create one for synchronisation reasons */
			IF @Term_Version_Key IS NULL
			BEGIN
				EXECUTE		spNextKey		'TERM_VERSION',
										@term_version_key	OUTPUT

				INSERT INTO Term_Version (
						Term_Version_Key,
						Term_Key,
						Entered_Session_ID,
						System_Supplied_Data)
				VALUES (
						@Term_Version_Key,
						@Term_Key,
						@SessionId,
						@system)
				
				UPDATE Concept 
				SET Term_Version_Key=@Term_Version_Key
				WHERE Concept_Key=@Concept_Key

			END

			/* record mapping */
			INSERT		Taxon_Dictionary_Term_Version_Mapping (
						Taxon_Version_Key,
						Term_Version_Key,
						Source_Join_Key)
			VALUES		(@taxon_version_key,
						@term_version_key,
						@source_join_key)

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END

		IF @term_version_key <> @prior_term_version_key
		BEGIN
			/* Use of @prior_term_version_key is a hack for the case where
			 * a single Term corresponds to multiple Taxon records; we don't
			 * increment the progress count until all the taxa have been
			 * considered.
			 */
			SET			@prior_term_version_key		=	@term_version_key

		END

		COMMIT TRANSACTION
	END

	CLOSE		versions
	DEALLOCATE	versions
	RETURN

fail_from_cursor:
	CLOSE		versions
	DEALLOCATE	versions

fail:
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_TaxonVersion_ImportConceptGroup failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TaxonVersion_ImportConceptGroup') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_TaxonVersion_ImportConceptGroup'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_TaxonVersion_ImportConceptGroup TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_TaxonVersion_ImportConceptGroup TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_TaxonVersion_ImportConceptGroup TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_Taxon_ImportConceptGroup]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_Taxon_ImportConceptGroup]
GO

/*===========================================================================*\
  Description:	Import taxa corresponding to terms in a concept group.

  Parameters:	@job_id					Job identifier

  Created:		Dec 2003

  Last revision information:
	$Revision: 7 $
	$Date: 6/02/09 10:41 $
	$Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Taxon_ImportConceptGroup]
	@job_id				CHAR(16)
AS
	SET NOCOUNT ON

	DECLARE     @concept_group_key		CHAR(16),
				@term_key				CHAR(16),
				@italic					BIT,
				@item_name				VARCHAR(60),
				@authority				VARCHAR(65),
				@language				VARCHAR(2),
				@taxon_name_type_key	CHAR(16),
				@entered_by				CHAR(16),
				@entry_date				SMALLDATETIME,
				@changed_by				CHAR(16),
				@changed_date			SMALLDATETIME,
				@system					BIT,
				@Taxon_Key				CHAR(16)

	/* determine parameters of job */
	SELECT      @concept_group_key			=	j.Concept_Group_Key
	FROM		Import_Export_Job			AS	j
	WHERE		j.Import_Export_Job_ID		=	@job_id

	IF @@ROWCOUNT = 0
	BEGIN
		RAISERROR ('Job does not exist or has not been configured', 16, 1)
		GOTO fail
	END

	EXECUTE		usp_Import_Export_Job_UpdateStatus	@job_id,
													'Exporting terms'
	IF @@ERROR <> 0 GOTO fail

	DECLARE		terms	CURSOR LOCAL FAST_FORWARD FOR
	SELECT DISTINCT
				t.Term_Key,
				CASE WHEN LEFT(t.Item_Name, 3) = '<i>'
					THEN 1
					ELSE 0
				END,
				t.Plaintext COLLATE SQL_Latin1_General_CP1_CI_AS,
				tv.Author_And_Date,
				t.Language_Key,
				tnt.Taxon_Name_Type_Key,
				es.User_Name_Key,
				CONVERT(SMALLDATETIME,
						CONVERT(VARCHAR, es.Date_Time_Start, 112)),
				cs.User_Name_Key,
				CONVERT(SMALLDATETIME,
						CONVERT(VARCHAR, cs.Date_Time_Start, 112)),
				t.System_Supplied_Data
	FROM		Concept								AS	c
	INNER JOIN	Term								AS	t
	ON			t.Term_Key							=	c.Term_Key
	LEFT JOIN	Term_Version						AS	tv
	ON			tv.Term_Version_Key					=	c.Term_Version_Key
	INNER JOIN	Taxon_Dictionary_Name_Type_Mapping	AS	tnt
	ON			tnt.Thesaurus_Name_Type_Key			=	c.Name_Type_Concept_Key
	INNER JOIN	Session								AS	es
	ON			es.Session_ID						=	t.Entered_Session_ID
	LEFT JOIN	Session								AS	cs
	ON			cs.Session_ID						=	t.Changed_Session_ID
	WHERE		c.Concept_Group_Key					=	@concept_group_key

	OPEN		terms

	WHILE 1 = 1
	BEGIN
		FETCH		terms
		INTO		@term_key,
					@italic,
					@item_name,
					@authority,
					@language,
					@taxon_name_type_key,
					@entered_by,
					@entry_date,
					@changed_by,
					@changed_date,
					@system

		IF @@FETCH_STATUS <> 0 BREAK

		BEGIN TRANSACTION

		SELECT		@taxon_key						=	tdm.Taxon_Key
		FROM		Taxon_Dictionary_Term_Mapping	AS	tdm
		INNER JOIN	TAXON							AS	tx
		ON			tx.TAXON_KEY					=	tdm.Taxon_Key
		WHERE		tdm.Term_Key					=	@term_key
		AND			tx.TAXON_NAME_TYPE_KEY			=	@taxon_name_type_key

		IF @@ROWCOUNT > 0
		BEGIN
			/* update taxon */
			UPDATE		TAXON
			SET			ITEM_NAME					=	@item_name,
						AUTHORITY					=	@authority,
						LANGUAGE					=	@language,
						ENTERED_BY					=	@entered_by,
						ENTRY_DATE					=	@entry_date,
						CHANGED_BY					=	@changed_by,
						CHANGED_DATE				=	@changed_date,
						SYSTEM_SUPPLIED_DATA		=	@system
			WHERE		TAXON_KEY					=	@taxon_key

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END
		ELSE
		BEGIN
			/* create new taxon */
			EXECUTE		spNextKey	'TAXON',
									@taxon_key	OUTPUT
			IF @@ERROR <> 0 GOTO fail_from_cursor

			INSERT		TAXON (
						TAXON_KEY,
						ITEM_NAME,
						AUTHORITY,
						LANGUAGE,
						TAXON_NAME_TYPE_KEY,
						ENTERED_BY,
						ENTRY_DATE,
						CHANGED_BY,
						CHANGED_DATE,
						SYSTEM_SUPPLIED_DATA)
			VALUES		(@taxon_key,
						@item_name,
						@authority,
						@language,
						@taxon_name_type_key,
						@entered_by,
						@entry_date,
						@changed_by,
						@changed_date,
						@system)

			IF @@ERROR <> 0 GOTO fail_from_cursor

			/* record mapping */
			INSERT		Taxon_Dictionary_Term_Mapping (
						Taxon_Key,
						Italic_Font,
						Term_Key)
			VALUES		(@taxon_key,
						@italic,
						@term_key)

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END

		EXECUTE		usp_Import_Export_Job_RecordProcessed	@job_id
		IF @@ERROR <> 0 GOTO fail_from_cursor

		COMMIT TRANSACTION
	END

	CLOSE		terms
	DEALLOCATE	terms
	RETURN

fail_from_cursor:
	CLOSE		terms
	DEALLOCATE	terms

fail:
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_Taxon_ImportConceptGroup failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Taxon_ImportConceptGroup') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Taxon_ImportConceptGroup'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Taxon_ImportConceptGroup TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Taxon_ImportConceptGroup TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Taxon_ImportConceptGroup TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_TermHTMLDetails_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_TermHTMLDetails_Select]
GO



/*===========================================================================*\
  Description:	Returns multiple recordsets suitable for populating the HTML
			details of a concept, excluding the name.  
			The following recordsets are returned:
				Designations
				Facts
				Sources
				Hyperlinks

  Parameters:	@Key	Collection key

  Created:	Dec 2003

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_TermHTMLDetails_Select]
	@ConceptKey char(16)
AS

SET NOCOUNT ON

/*===========================================================================*\	
	Recordset for each designation
\*===========================================================================*/
	SELECT 
		CT.Item_Name, 
  	dbo.ufn_GetDateFromVagueDate(
					CD.From_Vague_Date_Start, 
					CD.From_Vague_Date_End, 
					CD.From_Vague_Date_Type) AS DateFrom,
		dbo.ufn_GetDateFromVagueDate(
					CD.To_Vague_Date_Start, 
					CD.To_Vague_Date_End, 
					CD.To_Vague_Date_Type) AS DateTo,
		MGeo.[Text] AS Geographic_Context,
		MC.[Text] AS Constraints
	FROM Concept_Designation CD
	INNER JOIN VW_ConceptTerm CT ON CT.Concept_Key=CD.Designation_Type_Concept_Key
	LEFT JOIN Metadata MGeo ON MGeo.Record_Key=CD.Concept_Designation_Key AND MGeo.Metadata_Type_Key='SYSTEM0000000001'
	LEFT JOIN Metadata MC ON MC.Record_Key=CD.Concept_Designation_Key AND MC.Metadata_Type_Key='SYSTEM0000000002'
	WHERE CD.Concept_Key=@ConceptKey
	

/*===========================================================================*\	
	Temporary table to hold the list of term version keys that we might need
	facts for.  These are all the related terms versions which at least have 
	some overlap with the current term version.
\*===========================================================================*/
	CREATE TABLE #TermVersionKeys (Term_Version_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS)

	INSERT INTO #TermVersionKeys (Term_Version_Key)
		SELECT Term_Version_Key FROM Concept WHERE Concept_Key=@ConceptKey
		
	WHILE @@RowCount>0
		INSERT INTO #TermVersionKeys (Term_Version_Key) 
		SELECT TVR.To_Term_Version_Key 
		FROM Term_Version_Relation TVR
		INNER JOIN #TermVersionKeys TVK ON TVK.Term_Version_Key=TVR.From_Term_Version_Key
		INNER JOIN Thesaurus_Relation_Type TRT on TRT.Thesaurus_Relation_Type_Key=TVR.Thesaurus_Relation_Type_Key
		INNER JOIN Semantic_Relation SR ON SR.Semantic_Relation_Key=TRT.Semantic_Relation_Key
				AND SR.Forward_Equivalence_Possible=1
		LEFT JOIN #TermVersionKeys TVK2 ON TVK2.Term_Version_Key=TVR.To_Term_Version_Key
		WHERE TVK2.Term_Version_Key IS NULL	

	WHILE @@RowCount>0
		INSERT INTO #TermVersionKeys (Term_Version_Key) 
		SELECT TVR.From_Term_Version_Key 
		FROM Term_Version_Relation TVR
		INNER JOIN #TermVersionKeys TVK ON TVK.Term_Version_Key=TVR.To_Term_Version_Key
		INNER JOIN Thesaurus_Relation_Type TRT on TRT.Thesaurus_Relation_Type_Key=TVR.Thesaurus_Relation_Type_Key
		INNER JOIN Semantic_Relation SR ON SR.Semantic_Relation_Key=TRT.Semantic_Relation_Key
				AND SR.Reverse_Equivalence_Possible=1
		LEFT JOIN #TermVersionKeys TVK2 ON TVK2.Term_Version_Key=TVR.From_Term_Version_Key
		WHERE TVK2.Term_Version_Key IS NULL


/*===========================================================================*\	
	Temporary table to hold the list of lineage concept keys that we need to 
	look at for inherited facts
\*===========================================================================*/
	CREATE TABLE #InheritedConcepts (Concept_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS)

	DECLARE @LineageID integer
	DECLARE @Lineage varchar(2000)
	DECLARE @CharPos integer
	DECLARE @ConceptGroupKey char(16)

	--First find all the parent lineages
  DECLARE Lineages_Cursor CURSOR LOCAL FORWARD_ONLY FOR
    SELECT Lineage_ID FROM Concept_Lineage WHERE Concept_Key=@ConceptKey

	OPEN Lineages_Cursor

  FETCH NEXT FROM Lineages_Cursor INTO @LineageID
  
  WHILE @@FETCH_STATUS=0
	BEGIN

	  --select the lineage and concept group
		SELECT @Lineage = CL.Lineage, @ConceptGroupKey = C.Concept_Group_Key
		FROM Concept_Lineage CL
		INNER JOIN Concept C on C.Concept_Key=CL.Concept_Key
		WHERE CL.Concept_Key=@ConceptKey
		AND CL.Lineage_ID=@LineageID
		
		SET @CharPos=1
		
		--Find each ancestor, start at top of tree and work down
		WHILE @CharPos<LEN(@Lineage)
		BEGIN
		  IF SUBSTRING(@Lineage, @CharPos, 1)='\'
			  INSERT INTO #InheritedConcepts
			    SELECT DISTINCT C.Concept_Key
					FROM Concept C
		      INNER JOIN Concept_Lineage CL ON CL.Concept_Key=C.Concept_Key
				  WHERE C.Concept_Group_Key=@ConceptGroupKey
			    AND CL.Lineage=Left(@Lineage, @CharPos-1)
		  SET @CharPos=@CharPos+1
		END

		FETCH NEXT FROM Lineages_Cursor INTO @LineageID

	END

	CLOSE Lineages_Cursor
	DEALLOCATE Lineages_Cursor

/*===========================================================================*\	
	Temporary table to hold the list of fact keys that are relevant
\*===========================================================================*/
	CREATE TABLE #Fact (Thesaurus_Fact_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS)
	
	INSERT INTO #Fact
			--Concept facts
			SELECT DISTINCT TF.Thesaurus_Fact_Key
			FROM Thesaurus_Fact TF
			WHERE TF.Concept_Key=@ConceptKey
			UNION
			--Meaning facts
			SELECT TF.Thesaurus_Fact_Key
			FROM Thesaurus_Fact TF
			INNER JOIN Concept C ON C.Meaning_Key=TF.Meaning_Key
			WHERE C.Concept_Key=@ConceptKey
			UNION
			--Term version facts
			SELECT TF.Thesaurus_Fact_Key
			FROM Thesaurus_Fact TF
			INNER JOIN Term_Version TV on TV.Term_Version_Key=TF.Term_Version_Key
			INNER JOIN Concept C ON C.Term_Version_Key=TV.Term_Version_Key
			WHERE C.Concept_Key=@ConceptKey
			UNION
			--Facts for related term versions
			SELECT TF.Thesaurus_Fact_Key
			FROM Thesaurus_Fact TF
			INNER JOIN #TermVersionKeys TVK ON TVK.Term_Version_Key=TF.Term_Version_Key
			WHERE TF.Related_Term_Versions=1
			--Inherited concept facts
			UNION
			SELECT DISTINCT TF.Thesaurus_Fact_Key
			FROM Thesaurus_Fact TF
			INNER JOIN #InheritedConcepts IC ON IC.Concept_Key=TF.Concept_Key
			WHERE TF.Inherited=1
			UNION
			--Inherited meaning facts
			SELECT DISTINCT TF.Thesaurus_Fact_Key
			FROM Thesaurus_Fact TF
			INNER JOIN Concept C ON C.Meaning_Key=TF.Meaning_Key
			INNER JOIN #InheritedConcepts IC ON IC.Concept_Key=C.Concept_Key			
			WHERE TF.Inherited=1

/*===========================================================================*\	
	Recordset for each fact, using a subquery to ensure a distinct list
\*===========================================================================*/
	SELECT Item_Name, Data  
	FROM Thesaurus_Fact TF
	INNER JOIN #Fact ON #Fact.Thesaurus_Fact_Key=TF.Thesaurus_Fact_Key

/*===========================================================================*\	
	Recordset for each source
\*===========================================================================*/
	SELECT 
		R.Source_Key,
		RA.Author + ' - ' +
				dbo.ufn_GetDateFromVagueDate(
						R.Year_Vague_Date_Start, 
						Year_Vague_Date_End, 
						Year_Vague_Date_Type) AS AuthorAndDate,
		CASE WHEN R.TITLE IS NULL THEN
			R.FULL_REFERENCE
		ELSE
			R.Title
		END AS SourceTitle
	FROM Reference R
	INNER JOIN VW_REFERENCE_AUTHORS RA ON RA.Source_Key=R.Source_Key
	INNER JOIN Source_Join SJ ON SJ.Source_Key=R.Source_Key
	INNER JOIN Concept C ON C.Concept_Key=@ConceptKey
	LEFT JOIN Term_Version TV ON TV.Term_Version_Key=C.Term_Version_Key
	LEFT JOIN Concept_Designation CD ON CD.Concept_Key=C.Concept_Key
	WHERE (SJ.Table_Name='Concept' AND SJ.Record_Key=@ConceptKey)
		OR (SJ.Table_Name='Term' AND SJ.Record_Key=C.Term_Key)
		OR (SJ.Table_Name='Term_Version' AND SJ.Record_Key=TV.Term_Version_Key)
		OR (SJ.Table_Name='Concept_Designation' AND SJ.Record_Key=CD.Concept_Designation_Key)
		OR (SJ.Table_Name='Thesaurus_Fact' AND SJ.Record_Key IN (SELECT Thesaurus_Fact_Key FROM #Fact))

	
/*===========================================================================*\	
	Recordset for each web link
\*===========================================================================*/
	SELECT 
		DH.Item_Name, 
		DH.Image_File, 
		CASE DH.Use_Concept_Key
			WHEN 1 THEN DH.URL + C.Concept_Key
			WHEN 0 THEN DH.URL + REPLACE(CT.Plaintext COLLATE SQL_Latin1_General_CP1_CI_AS, ' ', DH.Word_Separator)
		END AS Hyperlink
	FROM Domain_Hyperlink DH
	INNER JOIN Concept_Group CG ON CG.Local_Domain_Key=DH.Local_Domain_Key
	INNER JOIN Concept C ON C.Concept_Group_Key=CG.Concept_Group_Key
	INNER JOIN VW_ConceptTerm CT ON CT.Concept_Key=C.Concept_Key
	WHERE C.Concept_Key=@ConceptKey

	--Cleanup the temporary table
	DROP TABLE #Fact
	DROP TABLE #TermVersionKeys
	
	SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TermHTMLDetails_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_TermHTMLDetails_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_TermHTMLDetails_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_TermHTMLDetails_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_TermHTMLDetails_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_TermHTMLDetails_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_TermHTMLDetails_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_TermHTMLDetails_Select TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_TermVersion_ImportTaxonList]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_TermVersion_ImportTaxonList]
GO

/*===========================================================================*\
  Description:	Import term versions corresponding to items in a taxon list.

  Parameters:   @job_id					Job identifier

  Created:		Nov 2003

  Last revision information:
	$Revision: 7 $
	$Date: 6/02/09 10:41 $
	$Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_TermVersion_ImportTaxonList]
	@job_id				INT
AS
	SET NOCOUNT ON

	DECLARE     @taxon_list_key			CHAR(16),
				@taxon_version_key		CHAR(16),
				@term_version_key		CHAR(16),
				@term_key				CHAR(16),
				@version				VARCHAR(10),
				@author_and_date		VARCHAR(40),
				@source_key				CHAR(16),
				@source_join_key		CHAR(16),	
				@ins_user_key			CHAR(16),
				@ins_date				SMALLDATETIME,
				@ins_session_id			CHAR(16),
				@upd_user_key			CHAR(16),
				@upd_date				SMALLDATETIME,
				@upd_session_id			CHAR(16),
				@system					BIT

	/* determine parameters of job */
	SELECT		@taxon_list_key							=	m.Taxon_List_Key
	FROM		Import_Export_Job						AS	j
	INNER JOIN	Taxon_Dictionary_Concept_Group_Mapping	AS	m
	ON			m.Concept_Group_Key						=	j.Concept_Group_Key
	WHERE		j.Import_Export_Job_ID					=	@job_id

	IF @@ROWCOUNT = 0
	BEGIN
		RAISERROR ('Job does not exist or has not been configured', 16, 1)
		RETURN
	END

	EXECUTE		usp_Import_Export_Job_UpdateStatus	@job_id,
													'Importing term versions'
	IF @@ERROR <> 0 RETURN

	DECLARE		@versions	TABLE (
				Taxon_Version_Key	CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS,
				List_Font_Italic	BIT)

	INSERT		@versions
	SELECT      tli.TAXON_VERSION_KEY,
				tr.List_Font_Italic
	FROM        TAXON_LIST_VERSION				AS	tlv
	INNER JOIN	TAXON_LIST_ITEM					AS	tli
	ON			tli.TAXON_LIST_VERSION_KEY		=	tlv.TAXON_LIST_VERSION_KEY
	INNER JOIN	TAXON_RANK						AS	tr
	ON			tr.TAXON_RANK_KEY				=	tli.TAXON_RANK_KEY
	WHERE		tlv.TAXON_LIST_KEY				=	@taxon_list_key

	DECLARE		versions	CURSOR LOCAL FAST_FORWARD FOR
	SELECT DISTINCT
				tv.TAXON_VERSION_KEY,
				tm.Term_Key,
				tv.ATTRIBUTE,
				tx.AUTHORITY,
				tv.SOURCE_KEY,
				tv.ENTERED_BY,
				tv.ENTRY_DATE,
				tv.CHANGED_BY,
				tv.CHANGED_DATE,
				tv.SYSTEM_SUPPLIED_DATA
	FROM		@versions						AS	v0
	INNER JOIN	TAXON_VERSION					AS	tv
	ON			tv.TAXON_VERSION_KEY			=	v0.TAXON_VERSION_KEY
	INNER JOIN	TAXON							AS	tx
	ON			tx.TAXON_KEY					=	tv.TAXON_KEY
	INNER JOIN	Taxon_Dictionary_Term_Mapping	AS	tm
	ON			tm.Taxon_Key					=	tx.TAXON_KEY
	AND			tm.Italic_Font					=	CASE WHEN tx.LANGUAGE = 'La'
														 AND v0.LIST_FONT_ITALIC = 1
														THEN 1
														ELSE 0
													END

	OPEN		versions

	WHILE 1 = 1
	BEGIN
		FETCH		versions
		INTO		@taxon_version_key,
					@term_key,
					@version,
					@author_and_date,
					@source_key,
					@ins_user_key,
					@ins_date,
					@upd_user_key,
					@upd_date,
					@system

		IF @@FETCH_STATUS <> 0 BREAK

		BEGIN TRANSACTION

		/* obtain session identifiers */
		EXECUTE		usp_Session_ForDate		@ins_user_key,
											@ins_date,
											@ins_session_id		OUTPUT
		IF @@ERROR <> 0 GOTO fail_from_cursor

		IF @upd_user_key IS NULL
		BEGIN
			SET			@upd_session_id		=	NULL
		END
		ELSE
		BEGIN
			EXECUTE		usp_Session_ForDate		@upd_user_key,
												@upd_date,
												@upd_session_id		OUTPUT
			IF @@ERROR <> 0 GOTO fail_from_cursor
		END

		/* look for existing mapping */
		SELECT		@term_version_key						=	Term_Version_Key,
					@source_join_key						=	Source_Join_Key
		FROM		Taxon_Dictionary_Term_Version_Mapping
		WHERE		Taxon_Version_Key						=	@taxon_version_key

		IF @@ROWCOUNT > 0
		BEGIN
			/* update term version */
			UPDATE		Term_Version
			SET			Version_Label			=	@version,
						Author_And_Date			=	@author_and_date,
						Changed_Session_ID		=	@upd_session_id,
						System_Supplied_Data	=	@system
			FROM		Term_Version
			WHERE		Term_Version_Key		=	@term_version_key

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END
		ELSE
		BEGIN
			/* create term version */
			EXECUTE		spNextKey	'Term_Version',
									@term_version_key	OUTPUT
			IF @@ERROR <> 0 GOTO fail_from_cursor

			SET			@source_join_key		=	NULL

			INSERT		Term_Version (
						Term_Version_Key,
						Term_Key,
						Version_Label,
						Author_And_Date,
						Entered_Session_ID,
						Changed_Session_ID,
						System_Supplied_Data)
			SELECT		@term_version_key,
						@term_key,
						@version,
						@author_and_date,
						@ins_session_id,
						@upd_session_id,
						@system

			IF @@ERROR <> 0 GOTO fail_from_cursor

			/* record mapping */
			INSERT		Taxon_Dictionary_Term_Version_Mapping (
						Taxon_Version_Key,
						Term_Version_Key)
			VALUES 		(@taxon_version_key,
						@term_version_key)

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END

		/* make any changes required in Source_Join */
		IF @source_key IS NULL
		BEGIN
			UPDATE		Taxon_Dictionary_Term_Version_Mapping
			SET			Source_Join_Key							=	NULL
			WHERE		Term_Version_Key						=	@term_version_key

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END

		EXECUTE		usp_SourceJoin_RecordImported	@source_join_key	OUTPUT,
													'Term_Version',
													@term_version_key,
													@source_key,
													@ins_session_id,
													@system
		IF @@ERROR <> 0 GOTO fail_from_cursor

		IF @source_key IS NOT NULL
		BEGIN
			UPDATE		Taxon_Dictionary_Term_Version_Mapping
			SET			Source_Join_Key							=	@source_join_key
			WHERE		Term_Version_Key						=	@term_version_key

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END

		/* update progress counter */
		EXECUTE		usp_Import_Export_Job_RecordProcessed	@job_id
		IF @@ERROR <> 0 GOTO fail_from_cursor

		COMMIT TRANSACTION
	END

	CLOSE		versions
	DEALLOCATE	versions
	RETURN

fail_from_cursor:
	CLOSE		versions
	DEALLOCATE	versions

fail:
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_TermVersion_ImportTaxonList failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TermVersion_ImportTaxonList') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_TermVersion_ImportTaxonList'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_TermVersion_ImportTaxonList TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_TermVersion_ImportTaxonList TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_TermVersion_ImportTaxonList TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_Term_ImportTaxonList]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_Term_ImportTaxonList]
GO

/*===========================================================================*\
  Description:	Import terms corresponding to items in a taxon list.

  Parameters:   @job_id					Job identifier

  Created:		Nov 2003

  Last revision information:
	$Revision: 7 $
	$Date: 6/02/09 10:41 $
	$Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Term_ImportTaxonList]
	@job_id				INT
AS
	SET NOCOUNT ON

	DECLARE     @concept_group_key	CHAR(16),
				@taxon_list_key		CHAR(16),
				@taxon_key			CHAR(16),
				@term_key			CHAR(16),
				@item_name      	VARCHAR(60),
				@language			VARCHAR(2),
				@ins_user_key		CHAR(16),
				@ins_date			SMALLDATETIME,
				@ins_session_id		CHAR(16),
				@upd_user_key		CHAR(16),
				@upd_date			SMALLDATETIME,
				@upd_session_id		CHAR(16),
				@system				BIT,
				@italic				BIT,
				@plaintext			NVARCHAR(300),
				@create_term		BIT

	/* determine parameters of job */
	SELECT		@concept_group_key						=	m.Concept_Group_Key,
				@taxon_list_key							=	m.Taxon_List_Key
	FROM		Import_Export_Job						AS	j
	INNER JOIN	Taxon_Dictionary_Concept_Group_Mapping	AS	m
	ON			m.Concept_Group_Key						=	j.Concept_Group_Key
	WHERE		j.Import_Export_Job_ID					=	@job_id

	IF @@ROWCOUNT = 0
	BEGIN
		RAISERROR ('Job does not exist or has not been configured', 16, 1)
		RETURN
	END

	EXECUTE		usp_Import_Export_Job_UpdateStatus	@job_id,
													'Importing terms'
	IF @@ERROR <> 0 RETURN

	DECLARE		@versions	TABLE (
				Taxon_Version_Key	CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS,
				List_Font_Italic	BIT)

	INSERT		@versions
    SELECT      tli.TAXON_VERSION_KEY,
                tr.List_Font_Italic
	FROM        TAXON_LIST_VERSION				AS	tlv
	INNER JOIN	TAXON_LIST_ITEM					AS	tli
	ON			tli.TAXON_LIST_VERSION_KEY		=	tlv.TAXON_LIST_VERSION_KEY
	INNER JOIN	TAXON_RANK						AS	tr
	ON			tr.TAXON_RANK_KEY				=	tli.TAXON_RANK_KEY
	WHERE		tlv.TAXON_LIST_KEY				=	@taxon_list_key

	DECLARE		terms	CURSOR LOCAL FAST_FORWARD FOR
	SELECT DISTINCT
				t.TAXON_KEY,
				t.ITEM_NAME,
				t.LANGUAGE,
				t.ENTERED_BY,
				t.ENTRY_DATE,
				t.CHANGED_BY,
				t.CHANGED_DATE,
				t.SYSTEM_SUPPLIED_DATA,
				CASE WHEN t.LANGUAGE = 'La'
					 AND v0.LIST_FONT_ITALIC = 1
					THEN 1
					ELSE 0
				END
	FROM		@versions							AS	v0
	INNER JOIN	TAXON_VERSION						AS	tv
	ON			tv.TAXON_VERSION_KEY				=	v0.TAXON_VERSION_KEY
	INNER JOIN	TAXON								AS	t
	ON			t.TAXON_KEY							=	tv.TAXON_KEY

	OPEN		terms

	WHILE 1 = 1
	BEGIN
		FETCH		terms
		INTO		@taxon_key,
					@plaintext,
					@language,
					@ins_user_key,
					@ins_date,
					@upd_user_key,
					@upd_date,
					@system,
					@italic

		IF @@FETCH_STATUS <> 0 BREAK

		SET			@item_name		=	CASE WHEN @italic = 0
											THEN @plaintext
											ELSE '<i>' + @plaintext + '</i>'
										END

		BEGIN TRANSACTION										

		/* obtain session identifiers */
		EXECUTE		usp_Session_ForDate		@ins_user_key,
											@ins_date,
											@ins_session_id		OUTPUT
		IF @@ERROR <> 0 GOTO fail_from_cursor

		IF @upd_user_key IS NULL
		BEGIN
			SET			@upd_session_id		=	NULL
		END
		ELSE
		BEGIN
			EXECUTE		usp_Session_ForDate		@upd_user_key,
												@upd_date,
												@upd_session_id		OUTPUT
			IF @@ERROR <> 0 GOTO fail_from_cursor
		END

		/* check for existing mapping */
		SELECT		@term_key						=	Term_Key
		FROM		Taxon_Dictionary_Term_Mapping
		WHERE		Taxon_Key						=	@taxon_key
		AND			Italic_Font						=	@italic

		SELECT		@create_term	=	CASE WHEN @@ROWCOUNT = 0
											THEN 1
											ELSE 0
										END

		IF @create_term = 0
		BEGIN
			IF NOT EXISTS (	SELECT		1
							FROM		Term
							WHERE		Term_Key				=	@term_key
							AND			Language_Key			=	@language
							AND			Item_Name				=	@item_name )
			BEGIN
				/* term has been modified */
				IF EXISTS (	SELECT		1
							FROM		Concept
							WHERE		Term_Key			=	@term_key
							AND			Concept_Group_Key	<>	@concept_group_key )
				BEGIN
					/* term is linked outside this concept group */
					SET			@create_term	=	1
				END
				ELSE
				BEGIN
					/* term linked only within this concept group */
					DECLARE		@new_term_key	CHAR(16)

					SELECT		@new_term_key	=	Term_Key
					FROM		Term
					WHERE		Language_Key	=	@language
					AND			Item_Name		=	@item_name
					AND			Term_Key		<>	@term_key

					IF @@ROWCOUNT = 0
					BEGIN
						/* update the current term */
						UPDATE		Term
						SET         Language_Key		=	@language,
									Item_Name			=	@item_name,
									Plaintext			=	@plaintext,
									Changed_Session_ID	=	@upd_session_id
						WHERE		Term_Key			=	@term_key

						IF @@ERROR <> 0 GOTO fail_from_cursor
					END
					ELSE
					BEGIN
						/* remove current term */
						UPDATE		Concept
						SET			Term_Key			=	@new_term_key,
									Term_Version_Key	=	NULL
						WHERE		Term_Key			=	@term_key

						IF @@ERROR <> 0 GOTO fail_from_cursor

						DELETE		Term_Version
						WHERE		Term_Key		=	@term_key

						IF @@ERROR <> 0 GOTO fail_from_cursor
						
						DELETE		Term
						WHERE		Term_Key		=	@term_key

						IF @@ERROR <> 0 GOTO fail_from_cursor

						/* link to the existing term that already
						 * has the new details */
						INSERT		Taxon_Dictionary_Term_Mapping (
									Taxon_Key,
									Italic_Font,
									Term_Key)
						VALUES		(@taxon_key,
									@italic,
									@new_term_key)

						IF @@ERROR <> 0 GOTO fail_from_cursor
					END
				END
			END /* term has been modified */
		END /* if @create_term = 0 */

		IF @create_term = 1
		BEGIN
			/* check for existing term that could be used */
			SELECT		@term_key		=	Term_Key
			FROM		Term
			WHERE		Language_Key	=	@language
			AND			Item_Name		=	@item_name

			IF @@ROWCOUNT > 0
			BEGIN
				/* map taxon onto the existing term */
				INSERT		Taxon_Dictionary_Term_Mapping (
							Taxon_Key,
							Italic_Font,
							Term_Key)
				VALUES		(@taxon_key,
							@italic,
							@term_key)

				IF @@ERROR <> 0 GOTO fail_from_cursor
			END
			ELSE
			BEGIN
				/* create term */
				EXECUTE		spNextKey	'Term',
										@term_key	OUTPUT
				IF @@ERROR <> 0 GOTO fail_from_cursor

				INSERT		Term (
							Term_Key,
							Language_Key,
							Item_Name,
							Plaintext,
							Entered_Session_ID,
							Changed_Session_ID,
							System_Supplied_Data)
				VALUES		(@term_key,
							@language,
							@item_name,
							@plaintext,
							@ins_session_id,
							@upd_session_id,
							@system)

				IF @@ERROR <> 0 GOTO fail_from_cursor

				/* record mapping */
				INSERT		Taxon_Dictionary_Term_Mapping
							(Taxon_Key,
							Italic_Font,
							Term_Key)
				VALUES		(@taxon_key,
							@italic,
							@term_key)

				IF @@ERROR <> 0 GOTO fail_from_cursor
			END
		END

		EXECUTE		usp_Import_Export_Job_RecordProcessed	@job_id
		IF @@ERROR <> 0 GOTO fail_from_cursor

		COMMIT TRANSACTION
	END

	CLOSE		terms
	DEALLOCATE	terms
	RETURN

fail_from_cursor:
	CLOSE		terms
	DEALLOCATE	terms

fail:
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_Term_ImportTaxonList failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Term_ImportTaxonList') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Term_ImportTaxonList'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Term_ImportTaxonList TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Term_ImportTaxonList TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Term_ImportTaxonList TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_ThesaurusFact_ImportTaxonList]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_ThesaurusFact_ImportTaxonList]
GO

/*===========================================================================*\
  Description:	Import thesaurus facts corresponding to the facts associated
  				with a taxon list.

  Parameters:	@job_id					Job identifier

  Created:		Dec 2003

  Last revision information:
	$Revision: 7 $
	$Date: 6/02/09 10:41 $
	$Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ThesaurusFact_ImportTaxonList]
	@job_id				INT
AS
	SET NOCOUNT ON

	DECLARE		@taxon_list_key			CHAR(16),
				@taxon_fact_key			CHAR(16),
				@type					VARCHAR(1),
				@meaning_key			CHAR(16),
				@ins_user_key			CHAR(16),
				@ins_date				SMALLDATETIME,
				@ins_session_id			CHAR(16),
				@upd_user_key			CHAR(16),
				@upd_date				SMALLDATETIME,
				@upd_session_id			CHAR(16),
				@system					BIT,
				@source_key				CHAR(16),
				@thesaurus_fact_key		CHAR(16),
				@fact_type_concept_key	CHAR(16),
				@source_join_key		CHAR(16)

	/* determine parameters of job */
	SELECT		@taxon_list_key							=	m.Taxon_List_Key
	FROM		Import_Export_Job						AS	j
	INNER JOIN	Taxon_Dictionary_Concept_Group_Mapping	AS	m
	ON			m.Concept_Group_Key						=	j.Concept_Group_Key
	WHERE		j.Import_Export_Job_ID					=	@job_id

	IF @@ROWCOUNT = 0
	BEGIN
		RAISERROR ('Job does not exist or has not been configured', 16, 1)
		RETURN
	END

	EXECUTE		usp_Import_Export_Job_UpdateStatus	@job_id,
													'Importing facts'
	IF @@ERROR <> 0 RETURN

	DECLARE		@versions	TABLE (
				Taxon_Version_Key	CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS)

	INSERT		@versions
	SELECT		tli.TAXON_VERSION_KEY
	FROM		TAXON_LIST_VERSION						AS	tlv
	INNER JOIN	TAXON_LIST_ITEM							AS	tli
	ON			tli.TAXON_LIST_VERSION_KEY				=	tlv.TAXON_LIST_VERSION_KEY
	WHERE		tlv.TAXON_LIST_KEY						=	@taxon_list_key

	DECLARE		facts	CURSOR LOCAL FAST_FORWARD FOR
	SELECT DISTINCT
				tf.TAXON_FACT_KEY,
				tf.TYPE,
				CASE tf.TYPE
					WHEN 'T' THEN 'SYSTEM00000002NO' /* HTML */
					WHEN 'S' THEN 'SYSTEM00000002NO' /* HTML */
					WHEN 'A' THEN 'SYSTEM00000002L9' /* AVI */
					WHEN 'W' THEN 'SYSTEM00000002L8' /* WAV */
					WHEN 'B' THEN 'SYSTEM00000000W0' /* Bitmap */
					WHEN 'J' THEN 'SYSTEM00000000VY' /* JPEG */
				END,
				c.Meaning_Key,
				tf.ENTERED_BY,
				tf.ENTRY_DATE,
				tf.CHANGED_BY,
				tf.CHANGED_DATE,
				tf.SYSTEM_SUPPLIED_DATA,
				tf.SOURCE_KEY
	FROM		@versions								AS	tli
	INNER JOIN	TAXON_FACT								AS	tf
	ON			tf.TAXON_VERSION_KEY					=	tli.TAXON_VERSION_KEY
	INNER JOIN	Taxon_Dictionary_Term_Version_Mapping	AS	m
	ON			m.Taxon_Version_Key						=	tf.TAXON_VERSION_KEY
	INNER JOIN	Concept									AS	c
	ON			c.Term_Version_Key						=	m.Term_Version_Key

	OPEN		facts

	WHILE 1 = 1
	BEGIN
		FETCH		facts
		INTO        @taxon_fact_key,
					@type,
					@fact_type_concept_key,
					@meaning_key,
					@ins_user_key,
					@ins_date,
					@upd_user_key,
					@upd_date,
					@system,
					@source_key

		IF @@FETCH_STATUS <> 0 BREAK

		BEGIN TRANSACTION

		/* obtain session identifiers */
		EXECUTE		usp_Session_ForDate		@ins_user_key,
											@ins_date,
											@ins_session_id		OUTPUT
		IF @@ERROR <> 0 GOTO fail_from_cursor

		IF @upd_user_key IS NULL
		BEGIN
			SET			@upd_session_id		=	NULL
		END
		ELSE
		BEGIN
			EXECUTE		usp_Session_ForDate		@upd_user_key,
												@upd_date,
												@upd_session_id		OUTPUT
			IF @@ERROR <> 0 GOTO fail_from_cursor
		END

		SELECT		@thesaurus_fact_key						=	Thesaurus_Fact_Key,
					@source_join_key						=	Source_Join_Key
		FROM		Taxon_Dictionary_Thesaurus_Fact_Mapping
		WHERE		Taxon_Fact_Key							=	@taxon_fact_key

		IF @@ROWCOUNT > 0
		BEGIN
			/* update existing thesaurus fact */
			UPDATE		Thesaurus_Fact
			SET			Item_Name				=	tf.TITLE,
						Data
							=	CASE
									WHEN @type = 'T' OR @type = 'S' THEN tf.DATA
									WHEN CHARINDEX('href="', tf.DATA) > 0 THEN
										SUBSTRING(
											tf.DATA,
											CHARINDEX('href="', tf.DATA) + 6,
											CHARINDEX('"', tf.DATA, CHARINDEX('href="', tf.DATA) + 6)
											- (CHARINDEX('href="', tf.DATA) + 6))
									WHEN CHARINDEX('href=''', tf.DATA) > 0 THEN
										SUBSTRING(
											tf.DATA,
											CHARINDEX('href=''', tf.DATA) + 6,
											CHARINDEX('''', tf.DATA, CHARINDEX('href=''', tf.DATA) + 6)
										- (CHARINDEX('href=''', tf.DATA) + 6))
									ELSE SUBSTRING(
											tf.DATA,
											CHARINDEX('href=', tf.DATA) + 5,
											PATINDEX(
												'%[ >]%',
												SUBSTRING(
													tf.DATA,
													CHARINDEX('href=', tf.DATA) + 5,
													DATALENGTH(tf.DATA))) - 1)
								END,
						Meaning_Key				=	@meaning_key,
						Concept_Key				=	NULL,
						Term_Version_Key    	=	NULL,
						Inherited				=	0,
						Fact_Vague_Date_Start	=	tf.FACT_VAGUE_DATE_START,
						Fact_Vague_Date_End		=	tf.FACT_VAGUE_DATE_END,
						Fact_Vague_Date_Type	=	ISNULL(
														tf.FACT_VAGUE_DATE_TYPE,
														'U'),
						Fact_Type_Concept_Key	=	@fact_type_concept_key,
						Entered_Session_ID		=	@ins_session_id,
						Changed_Session_ID		=	@upd_session_id,
						System_Supplied_Data	=	@system
			FROM		TAXON_FACT			   	AS	tf,
						Thesaurus_Fact
			WHERE		tf.TAXON_FACT_KEY		=	@taxon_fact_key
			AND			Thesaurus_Fact_Key		=	@thesaurus_fact_key

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END
		ELSE
		BEGIN
			/* create thesaurus fact */
			EXECUTE		spNextKey	'Thesaurus_Fact',
									@thesaurus_fact_key		OUTPUT
			IF @@ERROR <> 0 GOTO fail_from_cursor

			SET			@source_join_key			=	NULL

			INSERT		Thesaurus_Fact (
						Thesaurus_Fact_Key,
						Item_Name,
						Data,
						Meaning_Key,
						Language_Key,
						Fact_Vague_Date_Start,
						Fact_Vague_Date_End,
						Fact_Vague_Date_Type,
						Fact_Type_Concept_Key,
						Related_Term_Versions,
						Entered_Session_ID,
						Changed_Session_ID,
						System_Supplied_Data)
			SELECT		@thesaurus_fact_key,
						tf.TITLE,
						CASE
							WHEN @type = 'T' OR @type = 'S' THEN tf.DATA
							WHEN CHARINDEX('href="', tf.DATA) > 0 THEN
								SUBSTRING(
									tf.DATA,
									CHARINDEX('href="', tf.DATA) + 6,
									CHARINDEX('"', tf.DATA, CHARINDEX('href="', tf.DATA) + 6)
									- (CHARINDEX('href="', tf.DATA) + 6))
							WHEN CHARINDEX('href=''', tf.DATA) > 0 THEN
								SUBSTRING(
									tf.DATA,
									CHARINDEX('href=''', tf.DATA) + 6,
									CHARINDEX('''', tf.DATA, CHARINDEX('href=''', tf.DATA) + 6)
								- (CHARINDEX('href=''', tf.DATA) + 6))
							ELSE SUBSTRING(
									tf.DATA,
									CHARINDEX('href=', tf.DATA) + 5,
									PATINDEX(
										'%[ >]%',
										SUBSTRING(
											tf.DATA,
											CHARINDEX('href=', tf.DATA) + 5,
											DATALENGTH(tf.DATA))) - 1)
						END,
						@meaning_key,
						'en',
						tf.FACT_VAGUE_DATE_START,
						tf.FACT_VAGUE_DATE_END,
						ISNULL(tf.FACT_VAGUE_DATE_TYPE, 'U'),
						@fact_type_concept_key,
						0,
						@ins_session_id,
						@upd_session_id,
						@system
			FROM		TAXON_FACT					AS	tf
			WHERE		tf.TAXON_FACT_KEY			=	@taxon_fact_key

			IF @@ERROR <> 0 GOTO fail_from_cursor

			/* record mapping */
			INSERT		Taxon_Dictionary_Thesaurus_Fact_Mapping (
						Taxon_Fact_Key,
						Thesaurus_Fact_Key)
			VALUES		(@taxon_fact_key,
						@thesaurus_fact_key)

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END

		/* make any changes required in Source_Join */
		IF @source_key IS NULL
		BEGIN
			UPDATE		Taxon_Dictionary_Thesaurus_Fact_Mapping
			SET			Source_Join_Key							=	NULL
			WHERE		Taxon_Fact_Key							=	@taxon_fact_key

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END

		EXECUTE		usp_SourceJoin_RecordImported	@source_join_key	OUTPUT,
													'Thesaurus_Fact',
													@thesaurus_fact_key,
													@source_key,
													@ins_session_id,
													@system
		IF @@ERROR <> 0 GOTO fail_from_cursor

		IF @source_key IS NOT NULL
		BEGIN
			UPDATE		Taxon_Dictionary_Thesaurus_Fact_Mapping
			SET			Source_Join_Key							=	@source_join_key
			WHERE		Taxon_Fact_Key							=	@taxon_fact_key

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END

		/* update progress counter */
		EXECUTE		usp_Import_Export_Job_RecordProcessed	@job_id
		IF @@ERROR <> 0 GOTO fail_from_cursor
		
		COMMIT TRANSACTION
	END

	CLOSE		facts
	DEALLOCATE	facts
	RETURN

fail_from_cursor:
	CLOSE		facts
	DEALLOCATE	facts

fail:
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_ThesaurusFact_ImportTaxonList failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ThesaurusFact_ImportTaxonList') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ThesaurusFact_ImportTaxonList'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ThesaurusFact_ImportTaxonList TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ThesaurusFact_ImportTaxonList TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ThesaurusFact_ImportTaxonList TO [Dev - JNCC SQL]
END
GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_UserDomainAccess_Get]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_UserDomainAccess_Get]
GO

/*===========================================================================*\
  Description:	Returns all domains and security settings for a given user.

  Parameters:
	@UserID		User for which settings are to be returned.

  Created:	February 2004

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_UserDomainAccess_Get] 
	@UserID CHAR(16)
AS

	DECLARE @AddDomainMask INT
	DECLARE @EditDomainMask INT
	
	SET NOCOUNT ON
	
	-- Use DISTINCT in the SUM(), in case some domains have the same mask!
	SELECT 		@AddDomainMask = SUM(DISTINCT D.Domain_Mask)
	FROM 		User_Domain_Access UDA
	INNER JOIN	Domain D ON UDA.Domain_Key = D.Domain_Key AND D.Has_Occurrences = 1
	WHERE 		UDA.Name_Key = @UserID
	AND 		UDA.Allow_Add = 1
	
	-- Use DISTINCT in the SUM(), in case some domains have the same mask!
	SELECT 		@EditDomainMask = SUM(DISTINCT D.Domain_Mask)
	FROM 		User_Domain_Access UDA
	INNER JOIN	Domain D ON UDA.Domain_Key = D.Domain_Key AND D.Has_Occurrences = 1
	WHERE 		UDA.Name_Key = @UserID
	AND 		UDA.Allow_Edit = 1
	
	SET @AddDomainMask = ISNULL(@AddDomainMask, 0)
	SET @EditDomainMask = ISNULL(@EditDomainMask, 0)
	
	--Return results
	SELECT @AddDomainMask AS Add_Domain_Mask, @EditDomainMask AS Edit_Domain_Mask
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_UserDomainAccess_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_UserDomainAccess_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_UserDomainAccess_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_UserDomainAccess_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_UserDomainAccess_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_UserDomainAccess_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_UserDomainAccess_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_UserDomainAccess_Get TO [Dev - JNCC SQL]
END
GO

