SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

/*============================================================================*\
  Drop stored proc before re-creating.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_Specimens_Select_ForCollection') IS NOT NULL
	DROP PROCEDURE dbo.usp_Specimens_Select_ForCollection
GO

/*============================================================================*\
  Description:	Returns specimens' data to the Collections Browser for a
				specified collection.

  Parameters:	@UserDomainMask			User's Domain Mask restricting which
										records may be returned
				@SessionID				User's SessionID
				@ParentKey				Identifies the collection
				@ShowCommonNames		Specifies whether Common Names are shown
				@ShowOriginalSpecimenNames
										0 => preferred names are shown
										1 => names originally entered are shown
				@SortOrderIndex			0 => results sorted by Item_Name, Number
										1 => results sorted by Number, Item_Name

  Created:		August 2003

  Last revision information:
	$Revision: 20 $
	$Date: 13/03/13 8:38 $
	$Author: Alexanderpadley $
\*============================================================================*/
CREATE PROCEDURE dbo.usp_Specimens_Select_ForCollection
	@UserDomainMask 					INT,
	@SessionID 							CHAR(16),
	@ParentKey 							CHAR(16),
	@ShowCommonNames 					BIT,
	@ShowOriginalSpecimenNames			BIT,
	@SortOrderIndex 					TINYINT
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
		Item_Key		CHAR(16) 		COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		Join_Key		CHAR(16) 		COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		Det_Item_Key	CHAR(16) 		COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		Det_Item_Name	NVARCHAR(150) 	COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		Item_Name		NVARCHAR(150) 	COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		Life_Sciences	BIT NULL,
		Number			VARCHAR(30) 	COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		Hint			NVARCHAR(150) 	COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		NomenclaturalStatus	NVARCHAR(250) NULL 
	)

	INSERT INTO	@SpecimensSearch (Item_Key, Join_Key, Life_Sciences, NomenclaturalStatus) 
	SELECT 	DISTINCT SU.Collection_Unit_Key, SU.Collection_Unit_Key, SU.Life_Sciences,
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
	FROM 	Specimen_Unit 	SU
	JOIN 	Collection_Unit CU 	ON 	SU.Collection_Unit_Key = CU.Collection_Unit_Key
								AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
								OR 	(CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
	WHERE 	SU.Parent_Collection_Collection_Unit_Key = @ParentKey

	UPDATE		@SpecimensSearch
	SET			Number = CUN.Number
	FROM 		@SpecimensSearch 		SU
	LEFT JOIN 	Collection_Unit_Number 	CUN ON 	SU.Item_key 	= CUN.Collection_Unit_Key 
											AND CUN.Preferred	= 1

	UPDATE 	@SpecimensSearch
	SET		Det_Item_Key	= CPref.Concept_Key,
			Item_Name 		= CASE @ShowOriginalSpecimenNames
								WHEN 1 THEN C.Published_Term
								ELSE CPref.Published_Term END,
			Det_Item_Name	= TDet.Plaintext
	FROM 	@SpecimensSearch 		SU
	JOIN 	VW_SpecimenDetsEarth 	SDE 	ON 	SDE.Collection_Unit_Key			= SU.Item_Key
											AND SDE.Preferred_Determination_Key	= SDE.Determination_Key
	JOIN 	Concept 				C 		ON 	C.Concept_Key					= SDE.Concept_Key
	JOIN 	Term 					TDet 	ON 	TDet.Term_Key					= C.Term_Key
	JOIN 	Concept 				CPref 	ON 	CPref.Meaning_Key				= C.Meaning_Key
											AND CPref.List_Preferred			= 1
											AND CPref.Concept_Group_Key			= C.Concept_Group_Key
	
	UPDATE 	@SpecimensSearch
	SET		Det_Item_Key	= SDL.Taxon_List_Item_Key,
			Item_Name 		= dbo.ufn_GetFormattedTaxonNameByParams(
				CASE @ShowOriginalSpecimenNames
					WHEN 1 THEN Actual_Name
					ELSE Preferred_Name END,
				CASE @ShowOriginalSpecimenNames
					WHEN 1 THEN Actual_Name_Italic
					ELSE Preferred_Name_Italic END,
				Common_Name,
				Common_Name_Italic,
				NULL,
				@ShowCommonNames),
			Det_Item_Name	= ITN.Actual_Name
	FROM 	@SpecimensSearch 	SU
	JOIN 	VW_SpecimenDetsLife SDL ON 	SDL.Collection_Unit_Key					= SU.Item_Key
									AND SDL.Preferred_Taxon_Determination_Key 	= SDL.Taxon_Determination_Key
	JOIN 	Index_Taxon_Name 	ITN	ON 	ITN.Taxon_List_Item_Key					= SDL.Taxon_List_Item_Key

	-- Select table and sort appropriately
	IF @SortOrderIndex = 0
		SELECT * FROM @SpecimensSearch
		ORDER BY Item_Name, Number
	ELSE 
	IF @SortOrderIndex = 1
		SELECT * FROM @SpecimensSearch
		ORDER BY Number, Item_Name
GO

/*============================================================================*\
  Grant permissions.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_Specimens_Select_ForCollection') IS NOT NULL
BEGIN
	PRINT 'Setting up security on procedure usp_Specimens_Select_ForCollection'
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForCollection TO R2k_AddOnly
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForCollection TO R2k_Administrator
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForCollection TO R2k_FullEdit
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForCollection TO R2k_ReadOnly
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForCollection TO R2k_RecordCardsOnly
END
GO