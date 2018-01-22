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
    $Revision: 5 $
    $Date: 23/09/04 18:20 $
    $Author: Anthonysimpson $

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

	--Obtain first generation children
	INSERT INTO @StoreHierarchy (Collection_Unit_Key, Item_Name, Number, Bottom_Level) 
	SELECT 		S.Collection_Unit_Key, 
				S.Item_Name + IsNull(' - ' + CU.Current_Location_Code, IsNull(' - ' + CU.Usual_Location_Code, '')) AS Item_Name, 
				CUN.Number, 0
	FROM 		Store S
	INNER JOIN 	Collection_Unit CU ON S.Collection_Unit_Key = CU.Collection_Unit_Key
				AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
					OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
				AND CU.Current_Container_Collection_Unit_Key = @ParentKey

	LEFT JOIN 	Collection_Unit_Number CUN ON S.Collection_Unit_Key = CUN.Collection_Unit_Key 
				AND CUN.Type_Concept_Key = 'SYSTEM0000000001'  --REGISTRATION NUMBER
				AND CUN.Preferred = 1

	UPDATE		SH
	SET			SH.Bottom_Level = CASE WHEN S.Collection_Unit_Key IS NULL THEN 1 ELSE 0 END
	FROM		@StoreHierarchy SH
	LEFT JOIN 	Collection_Unit CU ON SH.Collection_Unit_Key = CU.Current_Container_Collection_Unit_Key
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