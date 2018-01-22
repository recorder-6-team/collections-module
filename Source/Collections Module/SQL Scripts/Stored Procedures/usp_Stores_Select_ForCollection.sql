If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Stores_Select_ForCollection]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Stores_Select_ForCollection]
GO

/*===========================================================================*\
  Description:	Returns Stores data to the CollectionsBrowser for a given Collection.

  Parameters:
	@ParentKey 	When specified, only the records associated with the parent key are returned
	@UserDomainMask	User's Domain Mask restricting which records may be returned
	@SessionID 	User's SessionID
	@SortOrderIndex	Index determining Sort Order

  Created:	August 2003
\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Stores_Select_ForCollection] 
	@UserDomainMask INT,
	@SessionID CHAR(16),
	@SortOrderIndex TINYINT,
	@ParentKey CHAR(16)
AS

SET NOCOUNT ON

IF @SortOrderIndex = 0
	SELECT DISTINCT S.Collection_Unit_Key AS Item_Key, 
				S.Item_Name + IsNull(' - ' + CU2.Current_Location_Code, IsNull(' - ' + CU2.Usual_Location_Code, '')) AS Item_Name,  
				Number, 
				S.Collection_Unit_Key AS Join_Key
	FROM		Specimen_Unit SU
	INNER JOIN	Collection_Unit CU ON SU.Collection_Unit_Key = CU.Collection_Unit_Key 
									AND Parent_Collection_Collection_Unit_Key = @ParentKey
	INNER JOIN	Store S ON CU.Current_Container_Collection_Unit_Key = S.Collection_Unit_Key
	INNER JOIN	Collection_Unit AS CU2 ON CU2.Collection_Unit_Key = S.Collection_Unit_Key
									AND ((CU2.Domain_Mask & @UserDomainMask > 0) OR (CU2.Entered_Session_ID = @SessionID) 
										OR (CU2.Changed_Session_ID = @SessionID) OR (CU2.Domain_Mask = 0))
	LEFT JOIN 	Collection_Unit_Number CUN 
		ON S.Collection_Unit_Key = CUN.Collection_Unit_Key 
		AND CUN.Type_Concept_Key = 'SYSTEM0000000001'  --REGISTRATION NUMBER
		AND CUN.Preferred = 1

	ORDER BY Item_Name, Number
ELSE 
IF @SortOrderIndex = 1
	SELECT DISTINCT S.Collection_Unit_Key AS Item_Key,
				S.Item_Name + IsNull(' - ' + CU.Current_Location_Code, IsNull(' - ' + CU.Usual_Location_Code, '')) AS Item_Name,  
				Number, 
				S.Collection_Unit_Key AS Join_Key
	FROM		Specimen_Unit SU
	INNER JOIN	Collection_Unit CU ON SU.Collection_Unit_Key = CU.Collection_Unit_Key 
									AND Parent_Collection_Collection_Unit_Key = @ParentKey
	INNER JOIN	Store S ON CU.Current_Container_Collection_Unit_Key = S.Collection_Unit_Key
	INNER JOIN	Collection_Unit AS CU2 ON CU2.Collection_Unit_Key = S.Collection_Unit_Key
									AND ((CU2.Domain_Mask & @UserDomainMask > 0) OR (CU2.Entered_Session_ID = @SessionID) 
										OR (CU2.Changed_Session_ID = @SessionID) OR (CU2.Domain_Mask = 0))
	LEFT JOIN 	Collection_Unit_Number CUN 
		ON S.Collection_Unit_Key = CUN.Collection_Unit_Key 
		AND CUN.Type_Concept_Key = 'SYSTEM0000000001'  --REGISTRATION NUMBER
		AND CUN.Preferred = 1

	ORDER BY Number, Item_Name
ELSE 
IF @SortOrderIndex = 2
	SELECT DISTINCT S.Collection_Unit_Key AS Item_Key, 
				S.Item_Name + IsNull(' - ' + CU.Current_Location_Code, IsNull(' - ' + CU.Usual_Location_Code, '')) AS Item_Name,  
				Number, 
				S.Collection_Unit_Key AS Join_Key, 
				CU.Current_Location_Code
	FROM		Specimen_Unit SU
	INNER JOIN	Collection_Unit CU ON SU.Collection_Unit_Key = CU.Collection_Unit_Key 
									AND Parent_Collection_Collection_Unit_Key = @ParentKey
	INNER JOIN	Store S ON CU.Current_Container_Collection_Unit_Key = S.Collection_Unit_Key
	INNER JOIN	Collection_Unit AS CU2 ON CU2.Collection_Unit_Key = S.Collection_Unit_Key
									AND ((CU2.Domain_Mask & @UserDomainMask > 0) OR (CU2.Entered_Session_ID = @SessionID) 
										OR (CU2.Changed_Session_ID = @SessionID) OR (CU2.Domain_Mask = 0))
	LEFT JOIN 	Collection_Unit_Number CUN 
		ON S.Collection_Unit_Key = CUN.Collection_Unit_Key 
		AND CUN.Type_Concept_Key = 'SYSTEM0000000001'  --REGISTRATION NUMBER
		AND CUN.Preferred = 1

	ORDER BY CU.Current_Location_Code, Item_Name, Number
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Stores_Select_ForCollection') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Stores_Select_ForCollection'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Stores_Select_ForCollection TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Stores_Select_ForCollection TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Stores_Select_ForCollection TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Stores_Select_ForCollection TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Stores_Select_ForCollection TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Stores_Select_ForCollection TO [Dev - JNCC SQL]
END
GO