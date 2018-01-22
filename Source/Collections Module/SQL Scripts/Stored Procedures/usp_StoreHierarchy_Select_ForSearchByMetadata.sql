If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_StoreHierarchy_Select_ForSearchByMetadata]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_StoreHierarchy_Select_ForSearchByMetadata]
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

CREATE PROCEDURE [dbo].[usp_StoreHierarchy_Select_ForSearchByMetadata] 
@UserDomainMask INT,
@SessionID CHAR(16),
@SortOrderIndex TINYINT,
@SearchText VARCHAR(30),
@MetaDataType VARCHAR(100)

AS

--  DESCRIPTION
--  Returns a store hierarchy based on the search parameter for the specified type of metadata
--
--	PARAMETERS
--	NAME				DESCRIPTION
--	@UserDomainMask		User's Domain Mask restricting which records may be returned
--	@SessionID 			User's SessionID
--	@SortOrderIndex		Index determining Sort Order
--	@SearchText			Text to be used for search
--  @MetaDataType       The type of metadata to be searching on
--
--
--  AUTHOR:				David Kelly, Dorset Software
--  CREATED:			2007-09-04
--

	SET NOCOUNT ON
	DECLARE @MetaDataTypeKey CHAR(16)
	SET @MetaDataTypeKey = (SELECT MetaData_Type_Key From Metadata_Type WHERE Item_Name = @MetaDataType AND Table_Name = 'Store')

	
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
	LEFT JOIN	Collection_Unit_Number AS CUN ON CUN.Collection_Unit_Key = S.Collection_Unit_Key 
					AND CUN.Type_Concept_Key = 'SYSTEM0000000001'  --REGISTRATION NUMBER
					AND CUN.Preferred = 1
	INNER JOIN METADATA MT ON S.Collection_Unit_Key = MT.Record_Key
					AND MT.MetaData_Type_Key = @MetaDataTypeKey
					AND MT.TEXT LIKE @SearchText + '%'
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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_StoreHierarchy_Select_ForSearchByMetadata') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_StoreHierarchy_Select_ForSearchByMetadata'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_StoreHierarchy_Select_ForSearchByMetadata TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_StoreHierarchy_Select_ForSearchByMetadata TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_StoreHierarchy_Select_ForSearchByMetadata TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_StoreHierarchy_Select_ForSearchByMetadata TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_StoreHierarchy_Select_ForSearchByMetadata TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_StoreHierarchy_Select_ForSearchByMetadata TO [Dev - JNCC SQL]
END

GO

