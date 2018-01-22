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

	ORDER BY Collation_From_Vague_Date_Start DESC, Collation_From_Vague_Date_End DESC, Item_Name, Number

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

	ORDER BY Item_Name, Number


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

	ORDER BY Number

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