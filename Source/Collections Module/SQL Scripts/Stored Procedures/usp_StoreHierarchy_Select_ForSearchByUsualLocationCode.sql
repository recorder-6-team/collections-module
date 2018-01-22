/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_StoreHierarchy_Select_ForSearchByUsualLocationCode]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_StoreHierarchy_Select_ForSearchByUsualLocationCode]
GO

/*===========================================================================*\
  Description:	Returns top level Store Hierarchy nodes with matching Store
				names. To qualify as a top level node, they have to have no 
				current location.

  Parameters:	@UserDomainMask		User's Domain Mask restricting which 
									records may be returned
				@SessionID 			User's SessionID
				@SortOrderIndex		Index determining Sort Order
				@SearchText			Usual location code to match on

  Created:		September 2004

  Last revision information:
    $Revision: 3 $
    $Date: 23/09/04 18:27 $
    $Author: Anthonysimpson $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_StoreHierarchy_Select_ForSearchByUsualLocationCode] 
@UserDomainMask INT,
@SessionID CHAR(16),
@SortOrderIndex TINYINT,
@SearchText VARCHAR(30)

AS

	SET NOCOUNT ON
	
	-- Create a table variable to put the initial results into. We use the results in this
	-- table to work out whether there are any stores contained within it.
	DECLARE @StoreHierarchy TABLE
	(
		[Collection_Unit_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
		[Item_Name] [varchar] (150), 
		[Number] [varchar] (30) NULL,
		[Bottom_Level] [bit] NOT NULL,
		[Current_Location_Code] [varchar] (30) NULL
	)
	
	-- Insert the initials results into the table variable.
	INSERT INTO @StoreHierarchy (Collection_Unit_Key, Item_Name, Number, Bottom_Level, Current_Location_Code) 
	SELECT 		S.Collection_Unit_Key AS Item_Key, 
				S.Item_Name + IsNull(' - ' + CU.Current_Location_Code, IsNull(' - ' + CU.Usual_Location_Code, '')) AS Item_Name,  
				Number,
				0,
				CU.Current_Location_Code
	FROM 		Store AS S
	INNER JOIN	Collection_Unit AS CU ON CU.Collection_Unit_Key = S.Collection_Unit_Key
	    			AND ((CU.Domain_Mask & @UserDomainMask > 0) 
						OR (CU.Entered_Session_ID = @SessionID) 
						OR (CU.Changed_Session_ID = @SessionID) 
						OR (CU.Domain_Mask = 0))
					AND CU.Usual_Location_Code LIKE @SearchText + '%'
	LEFT JOIN	Collection_Unit_Number AS CUN ON CUN.Collection_Unit_Key = S.Collection_Unit_Key 
					AND CUN.Type_Concept_Key = 'SYSTEM0000000001'  --REGISTRATION NUMBER
					AND CUN.Preferred = 1
	WHERE		CU.Current_Container_Collection_Unit_Key IS NULL
	
	-- Work out whether the top level node is also the bottom level node.
	UPDATE		SH
	SET			SH.Bottom_Level = CASE WHEN S.Collection_Unit_Key IS NULL THEN 1 ELSE 0 END
	FROM		@StoreHierarchy AS SH
	LEFT JOIN 	Collection_Unit CU ON CU.Current_Container_Collection_Unit_Key = SH.Collection_Unit_Key 
					AND ((CU.Domain_Mask & @UserDomainMask > 0) 
						OR (CU.Entered_Session_ID = @SessionID) 
						OR (CU.Changed_Session_ID = @SessionID) 
						OR (CU.Domain_Mask = 0))
				-- Need to do a join onto Store because other Collection_Units could have their current container
				-- as this store. However, we are only interested in the Stores at the moment.
	LEFT JOIN 	Store AS S ON S.Collection_Unit_Key = CU.Collection_Unit_Key
	
	-- Select the results for the result set.
	SELECT		Collection_Unit_Key AS Item_Key, 
				Collection_Unit_Key AS Join_Key, 
				Item_Name, 
				Number, 
				Bottom_Level
	FROM		@StoreHierarchy
	ORDER BY 	CASE @SortOrderIndex WHEN 0 THEN Item_Name 
									 WHEN 1 THEN Number
									 WHEN 2 THEN Current_Location_Code
				END,
				CASE @SortOrderIndex WHEN 0 THEN Number
									 WHEN 1 THEN Item_Name
									 WHEN 2 THEN Item_Name
				END,
				CASE @SortOrderIndex WHEN 2 THEN Number
				END

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_StoreHierarchy_Select_ForSearchByUsualLocationCode') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_StoreHierarchy_Select_ForSearchByUsualLocationCode'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_StoreHierarchy_Select_ForSearchByUsualLocationCode TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_StoreHierarchy_Select_ForSearchByUsualLocationCode TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_StoreHierarchy_Select_ForSearchByUsualLocationCode TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_StoreHierarchy_Select_ForSearchByUsualLocationCode TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_StoreHierarchy_Select_ForSearchByUsualLocationCode TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_StoreHierarchy_Select_ForSearchByUsualLocationCode TO [Dev - JNCC SQL]
END

GO