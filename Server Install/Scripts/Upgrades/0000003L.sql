/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF OBJECT_ID('dbo.usp_IndexTaxonName_Rebuild_ForSingleList') IS NOT NULL
	DROP PROCEDURE dbo.usp_IndexTaxonName_Rebuild_ForSingleList
GO

/*===========================================================================*\
  Description:
	Rebuilds the Index_Taxon_Name table for the given list.

  Parameters:
	@taxon_list_key	Key of the list to refresh.

  Created:	May 2009

  Last revision information:
	$Revision: 1 $
	$Date: 18/05/09 15:13 $
	$Author: Ericsalmon $

\*===========================================================================*/

CREATE PROCEDURE dbo.usp_IndexTaxonName_Rebuild_ForSingleList
	@taxon_list_key	CHAR(16)
AS
	SET NOCOUNT ON

	-- Clear all items for all versions.
	DELETE 	Index_Taxon_Name
	WHERE 	Taxon_List_Version_Key IN (
			SELECT 	Taxon_List_Version_Key
			FROM	Taxon_List_Version
			WHERE	Taxon_List_Key = @taxon_list_key
	)
	
	-- Populate the table with names and common names.
	INSERT INTO Index_Taxon_Name (
			Taxon_List_Item_Key, 
			Taxon_List_Version_Key,
			Actual_Name, 
			Actual_Name_Italic, 
			Common_Name, 
			Common_Name_Italic,
			Preferred_Name, 
			Preferred_Name_Italic, 
			Abbreviation, 
			Authority, 
			System_Supplied_Data
	)
	SELECT 	TLI.Taxon_List_Item_Key, 
			TLI.Taxon_List_Version_Key,
			T.Item_Name, 
			CASE WHEN TR3.List_Font_Italic = 1 AND T.Language = 'La' THEN 1 ELSE 0 END,
			T2.Item_Name, 
			CASE WHEN TR3.List_Font_Italic = 1 AND T2.Language = 'La' THEN 1 ELSE 0 END,
			T3.Item_Name, 
			CASE WHEN TR3.List_Font_Italic = 1 AND T3.Language = 'La' THEN 1 ELSE 0 END,
			T.Abbreviation, 
			T.Authority, 
			1
	FROM 		Taxon_List_Item 	TLI
	INNER JOIN	Taxon_List_version	TLV		ON TLV.Taxon_List_Version_Key = TLI.Taxon_List_Version_Key
	LEFT JOIN 	Taxon_version 		TV 		ON TV.Taxon_Version_Key 	= TLI.Taxon_Version_Key
	LEFT JOIN 	Taxon 				T 		ON T.Taxon_Key 				= TV.Taxon_Key
	LEFT JOIN 	Taxon_Common_Name 	TCN 	ON TCN.Taxon_List_Item_Key 	= TLI.Taxon_List_Item_Key
	LEFT JOIN 	Taxon_Version 		TV2 	ON TV2.Taxon_Version_Key 	= TCN.Taxon_Version_Key
	LEFT JOIN 	Taxon 				T2 		ON T2.Taxon_Key 			= TV2.Taxon_Key
	LEFT JOIN 	Taxon_List_Item 	TLI3 	ON TLI3.Taxon_List_Item_Key = TLI.Preferred_Name
	LEFT JOIN 	Taxon_Rank 			TR3 	ON TR3.Taxon_Rank_Key 		= TLI3.Taxon_Rank_Key
	LEFT JOIN 	Taxon_Version 		TV3 	ON TV3.Taxon_Version_Key 	= TLI3.Taxon_Version_Key
	LEFT JOIN 	Taxon 				T3 		ON T3.Taxon_Key 			= TV3.Taxon_Key
	WHERE 		TLI.Taxon_List_Version_To IS NULL
	AND			TLV.Taxon_List_Key = @taxon_list_key
	
	-- Populate the table with user names.
	INSERT INTO Index_Taxon_Name ( 
			Taxon_List_Item_Key, 
			Taxon_List_Version_Key,
			Actual_Name, 
			Actual_Name_Italic, 
			Common_Name, 
			Common_Name_Italic,
			Preferred_Name, 
			Preferred_Name_Italic, 
			System_Supplied_Data
	)
	SELECT 	TLI.Taxon_List_Item_Key, 
			TLI.Taxon_List_Version_Key,
			TUN.Item_Name, 
			CASE WHEN TR3.List_Font_Italic = 1 AND TUN.Language = 'La' THEN 1 ELSE 0 END,
			T2.Item_Name, 
			CASE WHEN TR3.List_Font_Italic = 1 AND T2.Language = 'La' THEN 1 ELSE 0 END,
			T3.Item_Name,
			CASE WHEN TR3.List_Font_Italic = 1 AND T3.Language = 'La' THEN 1 ELSE 0 END,
			0
	FROM 		Taxon_User_Name 	TUN
	LEFT JOIN 	Taxon_List_Item 	TLI 	ON TLI.Taxon_List_Item_Key 		= TUN.Taxon_List_Item_Key
	LEFT JOIN	Taxon_List_Version	TLV		ON TLV.Taxon_List_Version_Key	= TLI.Taxon_List_Version_Key
	LEFT JOIN 	Taxon_version 		TV 		ON TV.Taxon_Version_Key 		= TLI.Taxon_Version_Key
	LEFT JOIN 	Taxon_Common_Name 	TCN 	ON TCN.Taxon_List_Item_Key 		= TLI.Taxon_List_Item_Key
	LEFT JOIN 	Taxon_Version 		TV2 	ON TV2.Taxon_Version_Key 		= TCN.Taxon_Version_Key
	LEFT JOIN 	Taxon 				T2 		ON T2.Taxon_Key 				= TV2.Taxon_Key
	LEFT JOIN 	Taxon_List_Item 	TLI3 	ON TLI3.Taxon_List_Item_Key 	= TLI.Preferred_Name
	LEFT JOIN 	Taxon_Rank 			TR3 	ON TR3.Taxon_Rank_Key 			= TLI3.Taxon_Rank_Key
	LEFT JOIN 	Taxon_Version 		TV3 	ON TV3.Taxon_Version_Key 		= TLI3.Taxon_Version_Key
	LEFT JOIN 	Taxon 				T3 		ON T3.Taxon_Key 				= TV3.Taxon_Key
	WHERE 		TLI.Taxon_List_Version_To IS NULL
	AND			TLV.Taxon_List_Key 		= @taxon_list_key
	
	-- Updates for remaining values.
	UPDATE 	Index_Taxon_Name
	SET 	Common_Name 		= TUN.Item_Name,
			Common_Name_Italic 	= CASE WHEN TR.List_Font_Italic = 1 AND TUN.Language = 'La' THEN 1 ELSE 0 END
	FROM 	Index_Taxon_Name 	ITN
	JOIN 	Taxon_User_Name 	TUN ON TUN.Taxon_List_Item_Key 		= ITN.Taxon_List_Item_Key
	JOIN 	Taxon_List_Item 	TLI ON TLI.Taxon_List_Item_Key 		= ITN.Taxon_List_Item_Key
	JOIN	Taxon_List_Version	TLV ON TLV.Taxon_List_Version_Key 	= TLI.Taxon_List_Version_Key
	JOIN 	Taxon_Rank 			TR 	ON TR.Taxon_Rank_Key 			= TLI.Taxon_Rank_Key
	WHERE 	TUN.Preferred = 1
	AND		TLV.Taxon_List_Key 		= @taxon_list_key
	
	UPDATE 	ITN
	SET 	ITN.Preferred_List 		= TL.Preferred, 
			ITN.Allow_Data_Entry 	= TLT.Allow_Data_Entry
	FROM 	Index_Taxon_Name 	ITN
	JOIN 	Taxon_List_Version 	TLV ON TLV.Taxon_List_Version_Key 	= ITN.Taxon_List_Version_Key
	JOIN 	Taxon_List 			TL 	ON TL.Taxon_List_Key			= TLV.Taxon_List_Key
	JOIN 	Taxon_List_Type 	TLT ON TL.Taxon_List_Type_Key		= TLT.Taxon_List_Type_Key
	WHERE 	TL.Taxon_List_Key 	= @taxon_list_key
	
	UPDATE 	ITN
	SET 	ITN.Preferred_List = 1
	FROM 	Taxon_List_Item 	TLI
	JOIN 	Index_Taxon_Name 	ITN ON 	ITN.Taxon_List_Item_Key 	= TLI.Taxon_List_Item_Key
	JOIN 	Taxon_Version		TV 	ON 	TLI.Taxon_Version_Key 		= TV.Taxon_Version_Key
	JOIN 	Taxon_Group 		TG 	ON 	TV.Output_Group_Key 		= TG.Taxon_Group_Key
	JOIN 	Taxon_List_Version 	TLV	ON 	TLI.Taxon_List_Version_Key 	= TLV.Taxon_List_Version_Key 
	      							AND TG.Use_Taxon_List_Key 		= TLV.Taxon_List_Key
	WHERE 	TLV.Taxon_List_Key = @taxon_list_key
	
	UPDATE 	ITN
	SET 	Has_Children = 1
	FROM 	Index_Taxon_Name 	ITN
	JOIN 	Taxon_List_Item 	TLIChild	ON TLIChild.Parent = ITN.Taxon_List_Item_Key
	JOIN	Taxon_List_Version	TLV			ON TLV.Taxon_List_Version_Key = ITN.Taxon_List_Version_Key
	WHERE 	TLV.Taxon_List_Key = @taxon_list_key
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
PRINT 'Setting up security on procedure usp_IndexTaxonName_Rebuild_ForSingleList'
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
	GRANT EXECUTE ON dbo.usp_IndexTaxonName_Rebuild_ForSingleList TO R2k_Administrator
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
	GRANT EXECUTE ON dbo.usp_IndexTaxonName_Rebuild_ForSingleList TO R2k_FullEdit
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
	GRANT EXECUTE ON dbo.usp_IndexTaxonName_Rebuild_ForSingleList TO "Dev - JNCC SQL"
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF OBJECT_ID('dbo.usp_TaxonListItem_EnableAllListVersions') IS NOT NULL
	DROP PROCEDURE dbo.usp_TaxonListItem_EnableAllListVersions
GO

/*===========================================================================*\
  Description:
	Enable data entry for all taxa across all list versions linked to the 
	given list.

  Parameters:	@taxon_list_key	Taxon List key

  Created:		Oct 2008

  Last revision information:
	$Revision: 1 $
	$Date: 18/05/09 15:13 $
	$Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE dbo.usp_TaxonListItem_EnableAllListVersions
	@taxon_list_key CHAR(16)
AS
	SET NOCOUNT ON

	DECLARE @Expired TABLE (
		Taxon_Version_Key	CHAR(16) COLLATE Database_Default,
		Version				INT
	)

	DECLARE @LatestListVersionKey CHAR(16)
	SELECT	@LatestListVersionKey = Taxon_List_Version_Key
	FROM	Taxon_List_Version TLV
	WHERE 	Taxon_List_Key = @taxon_List_key
	AND		Version >= (
				SELECT 	MAX(Version)
				FROM 	Taxon_List_Version
				WHERE 	Taxon_List_Key = @taxon_List_key
				AND 	Version_Is_Amendment = 0
			)

	-- Get the latest version of expired items.
	INSERT INTO @Expired (Taxon_Version_Key, Version)
	SELECT	TLI1.Taxon_Version_Key, MAX(TLV1.Version)
	FROM	Taxon_List_Item		TLI1
	JOIN	Taxon_List_Version	TLV1	ON	TLV1.Taxon_List_Version_Key = TLI1.Taxon_List_Version_Key
										AND	TLV1.Taxon_List_Key			= @taxon_list_key
	WHERE	TLI1.Taxon_Version_Key NOT IN (
		SELECT	TLI2.Taxon_Version_Key
		FROM	Taxon_List_Item		TLI2
		JOIN	Taxon_List_Version	TLV2	ON	TLV2.Taxon_List_Version_Key = TLI2.Taxon_List_Version_Key
											AND	TLV2.Taxon_List_Key			= @taxon_list_key
		AND		TLI2.Taxon_List_Version_To 	IS 	NULL
		AND		TLI2.Taxon_List_Version_Key = 	@LatestListVersionKey
	)
	GROUP BY TLI1.Taxon_Version_Key

	-- Now set the Taxon_List_Version_To to NULL to "re-enable" data entry for these.
	UPDATE	TLI
	SET		Taxon_List_Version_To 		= 	NULL,
			Taxon_List_Version_Key		= 	@LatestListVersionKey
	FROM	@Expired			E
	JOIN	Taxon_List_Item		TLI	ON 	TLI.Taxon_Version_Key		= E.Taxon_Version_Key
	JOIN	Taxon_List_Version	TLV	ON	TLV.Taxon_List_Version_Key 	= TLI.Taxon_List_Version_Key
									AND	TLV.Version					= E.Version
									AND	TLV.Taxon_List_Key			= @taxon_list_key
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
PRINT 'Setting up security on procedure usp_TaxonListItem_EnableAllListVersions'
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
	GRANT EXECUTE ON dbo.usp_TaxonListItem_EnableAllListVersions TO R2k_Administrator
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
	GRANT EXECUTE ON dbo.usp_TaxonListItem_EnableAllListVersions TO R2k_FullEdit
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
	GRANT EXECUTE ON dbo.usp_TaxonListItem_EnableAllListVersions TO "Dev - JNCC SQL"
GO
