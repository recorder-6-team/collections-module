If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Stores_Select_ForLinkedOther]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Stores_Select_ForLinkedOther]
GO

/*===========================================================================*\
  Description:	Returns Related Stores for a specified Store.

  Parameters:	
	@ParentKey 	When specified, only the records associated with the parent key are returned
	@UserDomainMask	User's Domain Mask restricting which records may be returned
	@SessionID 	User's SessionID
	@SortOrderIndex	Index determining Sort Order

  Created:	September 2003

  Last revision information:
    $Revision: 8 $
    $Date: 13/04/04 11:14 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Stores_Select_ForLinkedOther] 
	@UserDomainMask INT,
	@SessionID CHAR(16),
	@SortOrderIndex TINYINT,
	@ParentKey CHAR(16)
AS

SET NOCOUNT ON

IF @SortOrderIndex = 0
	SELECT 		CUR.Collection_Unit_Relation_Key AS Item_Key,
			CUR.Collection_Unit_Relation_Key AS Join_Key,
			CASE WHEN S1.Collection_Unit_Key IS NOT NULL THEN S1.Collection_Unit_Key ELSE S2.Collection_Unit_Key END AS Hyperlink_Item_Key, 
			CASE WHEN S1.Collection_Unit_Key IS NOT NULL THEN S1.Collection_Unit_Key ELSE S2.Collection_Unit_Key END AS Drag_Drop_Item_Key, 
			CASE WHEN S1.Collection_Unit_Key IS NOT NULL THEN S1.Item_Name ELSE S2.Item_Name END AS Item_Name,
			CASE WHEN S1.Collection_Unit_Key IS NOT NULL THEN CUN1.Number ELSE CUN2.Number END AS Number

	FROM 		Collection_Unit_Relation CUR
	INNER JOIN	Thesaurus_Relation_Type TRT ON CUR.Thesaurus_Relation_Type_Key = TRT.Thesaurus_Relation_Type_Key
	INNER JOIN	Semantic_Relation SR ON TRT.Semantic_Relation_Key = SR.Semantic_Relation_Key --AND SR.Unidirectional = 0
	LEFT JOIN 	(Store S1
			INNER JOIN Collection_Unit CU1
				ON S1.Collection_Unit_Key = CU1.Collection_Unit_Key
				AND ((CU1.Domain_Mask & @UserDomainMask > 0) OR (CU1.Entered_Session_ID = @SessionID) 
					OR (CU1.Changed_Session_ID = @SessionID) OR (CU1.Domain_Mask = 0))
			)
		ON CUR.To_Collection_Unit_Key = S1.Collection_Unit_Key AND CUR.From_Collection_Unit_Key = @ParentKey

	LEFT JOIN 	Collection_Unit_Number CUN1
		ON S1.Collection_Unit_Key = CUN1.Collection_Unit_Key 
		AND CUN1.Type_Concept_Key = 'SYSTEM0000000001'  --REGISTRATION NUMBER
		AND CUN1.Preferred = 1

	LEFT JOIN 	(Store S2
			INNER JOIN Collection_Unit CU2
				ON S2.Collection_Unit_Key = CU2.Collection_Unit_Key
				AND ((CU2.Domain_Mask & @UserDomainMask > 0) OR (CU2.Entered_Session_ID = @SessionID) 
					OR (CU2.Changed_Session_ID = @SessionID) OR (CU2.Domain_Mask = 0))
			)
		ON CUR.From_Collection_Unit_Key = S2.Collection_Unit_Key AND CUR.To_Collection_Unit_Key = @ParentKey
		AND SR.Unidirectional = 0

	LEFT JOIN 	Collection_Unit_Number CUN2
		ON S2.Collection_Unit_Key = CUN2.Collection_Unit_Key 
		AND CUN2.Type_Concept_Key = 'SYSTEM0000000001'  --REGISTRATION NUMBER
		AND CUN2.Preferred = 1

	WHERE 		(S1.Collection_Unit_Key IS NOT NULL)
	OR 		(S2.Collection_Unit_Key IS NOT NULL)

	ORDER BY 	Item_Name, Number

ELSE IF @SortOrderIndex = 1
	SELECT 		CUR.Collection_Unit_Relation_Key AS Item_Key,
			CUR.Collection_Unit_Relation_Key AS Join_Key,
			CASE WHEN S1.Collection_Unit_Key IS NOT NULL THEN S1.Collection_Unit_Key ELSE S2.Collection_Unit_Key END AS Hyperlink_Item_Key, 
			CASE WHEN S1.Collection_Unit_Key IS NOT NULL THEN S1.Collection_Unit_Key ELSE S2.Collection_Unit_Key END AS Drag_Drop_Item_Key, 
			CASE WHEN S1.Collection_Unit_Key IS NOT NULL THEN S1.Item_Name ELSE S2.Item_Name END AS Item_Name,
			CASE WHEN S1.Collection_Unit_Key IS NOT NULL THEN CUN1.Number ELSE CUN2.Number END AS Number

	FROM 		Collection_Unit_Relation CUR
	INNER JOIN	Thesaurus_Relation_Type TRT ON CUR.Thesaurus_Relation_Type_Key = TRT.Thesaurus_Relation_Type_Key
	INNER JOIN	Semantic_Relation SR ON TRT.Semantic_Relation_Key = SR.Semantic_Relation_Key --AND SR.Unidirectional = 0
	LEFT JOIN 	(Store S1
			INNER JOIN Collection_Unit CU1
				ON S1.Collection_Unit_Key = CU1.Collection_Unit_Key
				AND ((CU1.Domain_Mask & @UserDomainMask > 0) OR (CU1.Entered_Session_ID = @SessionID) 
					OR (CU1.Changed_Session_ID = @SessionID) OR (CU1.Domain_Mask = 0))
			)
		ON CUR.To_Collection_Unit_Key = S1.Collection_Unit_Key AND CUR.From_Collection_Unit_Key = @ParentKey

	LEFT JOIN 	Collection_Unit_Number CUN1
		ON S1.Collection_Unit_Key = CUN1.Collection_Unit_Key 
		AND CUN1.Type_Concept_Key = 'SYSTEM0000000001'  --REGISTRATION NUMBER
		AND CUN1.Preferred = 1

	LEFT JOIN 	(Store S2
			INNER JOIN Collection_Unit CU2
				ON S2.Collection_Unit_Key = CU2.Collection_Unit_Key
				AND ((CU2.Domain_Mask & @UserDomainMask > 0) OR (CU2.Entered_Session_ID = @SessionID) 
					OR (CU2.Changed_Session_ID = @SessionID) OR (CU2.Domain_Mask = 0))
			)
		ON CUR.From_Collection_Unit_Key = S2.Collection_Unit_Key AND CUR.To_Collection_Unit_Key = @ParentKey
		AND SR.Unidirectional = 0

	LEFT JOIN 	Collection_Unit_Number CUN2
		ON S2.Collection_Unit_Key = CUN2.Collection_Unit_Key 
		AND CUN2.Type_Concept_Key = 'SYSTEM0000000001'  --REGISTRATION NUMBER
		AND CUN2.Preferred = 1

	WHERE 		(S1.Collection_Unit_Key IS NOT NULL)
	OR 		(S2.Collection_Unit_Key IS NOT NULL)
	
	ORDER BY 	Number, Item_Name

ELSE IF @SortOrderIndex = 2
	SELECT 		CUR.Collection_Unit_Relation_Key AS Item_Key,
			CUR.Collection_Unit_Relation_Key AS Join_Key,
			CASE WHEN S1.Collection_Unit_Key IS NOT NULL THEN S1.Collection_Unit_Key ELSE S2.Collection_Unit_Key END AS Hyperlink_Item_Key, 
			CASE WHEN S1.Collection_Unit_Key IS NOT NULL THEN S1.Collection_Unit_Key ELSE S2.Collection_Unit_Key END AS Drag_Drop_Item_Key, 
			CASE WHEN S1.Collection_Unit_Key IS NOT NULL THEN S1.Item_Name ELSE S2.Item_Name END AS Item_Name,
			CASE WHEN S1.Collection_Unit_Key IS NOT NULL THEN CUN1.Number ELSE CUN2.Number END AS Number

	FROM 		Collection_Unit_Relation CUR
	INNER JOIN	Thesaurus_Relation_Type TRT ON CUR.Thesaurus_Relation_Type_Key = TRT.Thesaurus_Relation_Type_Key
	INNER JOIN	Semantic_Relation SR ON TRT.Semantic_Relation_Key = SR.Semantic_Relation_Key --AND SR.Unidirectional = 0
	LEFT JOIN 	(Store S1
			INNER JOIN Collection_Unit CU1
				ON S1.Collection_Unit_Key = CU1.Collection_Unit_Key
				AND ((CU1.Domain_Mask & @UserDomainMask > 0) OR (CU1.Entered_Session_ID = @SessionID) 
					OR (CU1.Changed_Session_ID = @SessionID) OR (CU1.Domain_Mask = 0))
			)
		ON CUR.To_Collection_Unit_Key = S1.Collection_Unit_Key AND CUR.From_Collection_Unit_Key = @ParentKey

	LEFT JOIN 	Collection_Unit_Number CUN1
		ON S1.Collection_Unit_Key = CUN1.Collection_Unit_Key 
		AND CUN1.Type_Concept_Key = 'SYSTEM0000000001'  --REGISTRATION NUMBER
		AND CUN1.Preferred = 1

	LEFT JOIN 	(Store S2
			INNER JOIN Collection_Unit CU2
				ON S2.Collection_Unit_Key = CU2.Collection_Unit_Key
				AND ((CU2.Domain_Mask & @UserDomainMask > 0) OR (CU2.Entered_Session_ID = @SessionID) 
					OR (CU2.Changed_Session_ID = @SessionID) OR (CU2.Domain_Mask = 0))
			)
		ON CUR.From_Collection_Unit_Key = S2.Collection_Unit_Key AND CUR.To_Collection_Unit_Key = @ParentKey
		AND SR.Unidirectional = 0

	LEFT JOIN 	Collection_Unit_Number CUN2
		ON S2.Collection_Unit_Key = CUN2.Collection_Unit_Key 
		AND CUN2.Type_Concept_Key = 'SYSTEM0000000001'  --REGISTRATION NUMBER
		AND CUN2.Preferred = 1

	WHERE 		(S1.Collection_Unit_Key IS NOT NULL)
	OR 		(S2.Collection_Unit_Key IS NOT NULL)
	
	ORDER BY
		CASE WHEN CU1.Collection_Unit_Key IS NOT NULL THEN CU1.Current_Location_Code ELSE CU2.Current_Location_Code END, 
		Item_Name, Number
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Stores_Select_ForLinkedOther') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Stores_Select_ForLinkedOther'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Stores_Select_ForLinkedOther TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Stores_Select_ForLinkedOther TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Stores_Select_ForLinkedOther TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Stores_Select_ForLinkedOther TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Stores_Select_ForLinkedOther TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Stores_Select_ForLinkedOther TO [Dev - JNCC SQL]
END
GO