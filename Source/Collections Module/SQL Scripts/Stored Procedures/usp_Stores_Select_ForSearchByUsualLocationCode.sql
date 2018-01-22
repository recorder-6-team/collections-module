If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Stores_Select_ForSearchByUsualLocationCode]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Stores_Select_ForSearchByUsualLocationCode]
GO

CREATE PROCEDURE [dbo].[usp_Stores_Select_ForSearchByUsualLocationCode] 
@UserDomainMask INT,
@SessionID CHAR(16),
@SortOrderIndex TINYINT,
@SearchText VARCHAR(30)

AS

--  DESCRIPTION
--  Returns Stores data based on the search parameter for Usual Location
--
--  PARAMETERS
--	NAME				DESCRIPTION
--	@SearchText			Text to be used for search
--	@UserDomainMask		User's Domain Mask restricting which records may be returned
--	@SessionID 			User's SessionID
--	@SortOrderIndex		Index determining Sort Order
--
--
--  AUTHOR:				Ben Collier, Dorset Software
--  CREATED:			2003-09-15
--
SET NOCOUNT ON

IF @SortOrderIndex = 0
	SELECT S.Collection_Unit_Key AS Item_Key, 	
				S.Item_Name + IsNull(' - ' + CU.Current_Location_Code, IsNull(' - ' + CU.Usual_Location_Code, '')) AS Item_Name, 
				Number, S.Item_Name AS Hint
	FROM 
	STORE S
	    INNER JOIN
	   	    COLLECTION_UNIT CU 
	    ON S.Collection_Unit_Key = CU.Collection_Unit_Key
	       	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
				OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
			AND CU.Usual_Location_Code LIKE @SearchText + '%'
		LEFT JOIN 
			COLLECTION_UNIT_NUMBER CUN 
		ON S.Collection_Unit_Key = CUN.Collection_Unit_Key 
			AND CUN.Type_Concept_Key = 'SYSTEM0000000001'  --REGISTRATION NUMBER
			AND CUN.Preferred = 1
	ORDER BY S.Item_Name, Number
ELSE IF @SortOrderIndex = 1
	SELECT S.Collection_Unit_Key AS Item_Key, 	
				S.Item_Name + IsNull(' - ' + CU.Current_Location_Code, IsNull(' - ' + CU.Usual_Location_Code, '')) AS Item_Name, 
				Number, Number AS Hint
	FROM 
	STORE S
	    INNER JOIN
	   	    COLLECTION_UNIT CU 
	    ON S.Collection_Unit_Key = CU.Collection_Unit_Key
	       	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
				OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
			AND CU.Usual_Location_Code LIKE @SearchText + '%'
		LEFT JOIN 
			COLLECTION_UNIT_NUMBER CUN 
		ON S.Collection_Unit_Key = CUN.Collection_Unit_Key 
			AND CUN.Type_Concept_Key = 'SYSTEM0000000001'  --REGISTRATION NUMBER
			AND CUN.Preferred = 1
	ORDER BY Number, S.Item_Name
ELSE IF @SortOrderIndex = 2
	SELECT S.Collection_Unit_Key AS Item_Key, S.Item_Name, 	
				S.Item_Name + IsNull(' - ' + CU.Current_Location_Code, IsNull(' - ' + CU.Usual_Location_Code, '')) AS Item_Name, 
				CU.Current_Location_Code AS Hint
	FROM 
	STORE S
	    INNER JOIN
	   	    COLLECTION_UNIT CU 
	    ON S.Collection_Unit_Key = CU.Collection_Unit_Key
	       	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
				OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
			AND CU.Usual_Location_Code LIKE @SearchText + '%'
		LEFT JOIN 
			COLLECTION_UNIT_NUMBER CUN 
		ON S.Collection_Unit_Key = CUN.Collection_Unit_Key 
			AND CUN.Type_Concept_Key = 'SYSTEM0000000001'  --REGISTRATION NUMBER
			AND CUN.Preferred = 1
	ORDER BY Current_Location_Code, S.Item_Name, Number

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Stores_Select_ForSearchByUsualLocationCode') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Stores_Select_ForSearchByUsualLocationCode'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Stores_Select_ForSearchByUsualLocationCode TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Stores_Select_ForSearchByUsualLocationCode TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Stores_Select_ForSearchByUsualLocationCode TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Stores_Select_ForSearchByUsualLocationCode TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Stores_Select_ForSearchByUsualLocationCode TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Stores_Select_ForSearchByUsualLocationCode TO [Dev - JNCC SQL]
END

GO