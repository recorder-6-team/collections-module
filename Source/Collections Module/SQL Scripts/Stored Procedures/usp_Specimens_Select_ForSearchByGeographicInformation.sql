If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Specimens_Select_ForSearchByGeographicInformation]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Specimens_Select_ForSearchByGeographicInformation]
GO

/*===========================================================================*\
  Description:
	Returns Specimens data based on the search parameter for Geographic Information.

  Parameters:
	@UserDomainMask		User's Domain Mask restricting which records may be returned
	@SessionID 		User's SessionID
	@ShowCommonNames	Specifies whether or not Common Names should be shown
	@SearchText		Text to be searched on
	@SortOrderIndex		Index determining Sort Order

  Created:
	October 2003

  Last revision information:
    $Revision: 17 $
    $Date: 30/10/12 16:48 $
    $Author: Alexanderpadley $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_Specimens_Select_ForSearchByGeographicInformation] 
	@UserDomainMask INT,
	@SessionID CHAR(16),
	@ShowCommonNames BIT,
	@SearchText VARCHAR(100),
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
		[Hint] [text] COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[NomenclaturalStatus] VARCHAR(250) NULL 
	)

	INSERT INTO @SpecimensSearch (Item_Key, Life_Sciences, Hint, NomenclaturalStatus) 
	SELECT 	SU.Collection_Unit_Key, SU.Life_Sciences, M.Text, 
					CASE Life_Sciences
						WHEN 1 THEN
							SUBSTRING(( 
								SELECT	C.Published_Term + ','
								FROM	Taxon_Determination D 
								INNER JOIN	Concept C
									ON	D.Nomenclatural_Status_Concept_Key = C.Concept_Key
								WHERE	D.Nomenclatural_Status_Concept_Key IS NOT NULL
									AND	D.Specimen_Collection_Unit_Key = SU.Collection_Unit_Key
								ORDER BY	D.Taxon_Determination_Key
									FOR XML PATH('')
							), 0, 250)
						ELSE
							SUBSTRING(( 
								SELECT	C.Published_Term + ','
								FROM	Determination D 
								INNER JOIN	Concept C
									ON	D.Nomenclatural_Status_Concept_Key = C.Concept_Key
								WHERE	D.Nomenclatural_Status_Concept_Key IS NOT NULL
									AND	D.Specimen_Collection_Unit_Key = SU.Collection_Unit_Key
								ORDER BY	D.Determination_Key
									FOR XML PATH('')
							), 0, 250) 
					END
	FROM 	Specimen_Unit SU
	JOIN 	Metadata M ON M.Record_Key=SU.Collection_Unit_Key
			AND M.Metadata_Type_Key='SYSTEM0000000005'
	JOIN 	Collection_Unit CU ON SU.Collection_Unit_Key = CU.Collection_Unit_Key 
	 		AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
			OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
	WHERE 	M.Text LIKE @SearchText + '%'

	UPDATE		@SpecimensSearch
	SET			Number = CUN.Number
	FROM 		@SpecimensSearch 		SU
	LEFT JOIN 	Collection_Unit_Number 	CUN ON 	SU.Item_key 	= CUN.Collection_Unit_Key 
											AND CUN.Preferred	= 1

	UPDATE 	@SpecimensSearch
	SET	Det_Item_Key=CPref.Concept_Key,
		Item_Name = CPref.Published_Term,
		Det_Item_Name=TDet.Plaintext
	FROM @SpecimensSearch SU
	INNER JOIN VW_SpecimenDetsEarth SDE ON SDE.Collection_Unit_Key=SU.Item_Key
		AND SDE.Preferred_Determination_Key=SDE.Determination_Key
	INNER JOIN Concept C ON C.Concept_Key=SDE.Concept_Key
	INNER JOIN Term TDet ON TDet.Term_Key=C.Term_Key
	INNER JOIN Concept CPref ON CPref.Meaning_Key=C.Meaning_Key
			AND CPref.List_Preferred=1
			AND CPref.Concept_Group_Key=C.Concept_Group_Key
	

	UPDATE @SpecimensSearch
	SET	Det_Item_Key = SDL.Taxon_List_Item_Key,
		Item_Name = dbo.ufn_GetFormattedTaxonNameByParams(
				Preferred_Name,
				Preferred_Name_Italic,
				Common_Name,
				Common_Name_Italic,
				NULL,
				@ShowCommonNames),
		Det_Item_Name = ITN.Actual_Name
	FROM 	@SpecimensSearch SU
	INNER JOIN VW_SpecimenDetsLife SDL ON SDL.Collection_Unit_Key = SU.Item_Key
			AND SDL.Preferred_Taxon_Determination_Key = SDL.Taxon_Determination_Key
	INNER JOIN Index_Taxon_Name ITN	ON ITN.Taxon_List_Item_Key = SDL.Taxon_List_Item_Key

	-- Select table and sort appropriately
	IF @SortOrderIndex = 0
		SELECT * FROM @SpecimensSearch
		ORDER BY Det_Item_Name, Number
	ELSE IF @SortOrderIndex = 1
		SELECT * FROM @SpecimensSearch
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