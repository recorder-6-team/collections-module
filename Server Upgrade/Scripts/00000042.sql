SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

/*============================================================================*\
  Drop stored proc before re-creating.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_Specimens_Select_ForSearchByObservationLocation') IS NOT NULL
	DROP PROCEDURE dbo.usp_Specimens_Select_ForSearchByObservationLocation
GO

/*============================================================================*\
  Description:	Returns specimens based on their observation location.

  Parameters:	@UserDomainMask			User's Domain Mask restricting which
										records may be returned
				@SessionID 				User's SessionID
				@ShowCommonNames		Specifies whether Common Names are shown
				@ShowOriginalSpecimenNames
										0 => preferred names are shown
										1 => names originally entered are shown
				@SearchText				Text to search for
				@SortOrderIndex			0 => results sorted by Item_Name, Number
										1 => results sorted by Number, Item_Name

  Created:		September 2010

  Last revision information:
    $Revision: 1 $
    $Date: 14/10/10 12:49 $
    $Author: Robertjohnson $
\*============================================================================*/
CREATE PROCEDURE dbo.usp_Specimens_Select_ForSearchByObservationLocation
	@UserDomainMask						INT,
	@SessionID							CHAR(16),
	@ShowCommonNames					BIT,
	@ShowOriginalSpecimenNames			BIT,
	@SearchText							VARCHAR(50),
	@SortOrderIndex						TINYINT
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
		[Hint] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL 
	)

	INSERT INTO		@SpecimensSearch 
					(
						Item_Key, 
						Life_Sciences
					) 
	SELECT			DISTINCT 
					SU.Collection_Unit_Key, 
					SU.Life_Sciences
	FROM			Specimen_Unit SU
	INNER JOIN		Collection_Unit CU 
	ON				SU.Collection_Unit_Key	=		CU.Collection_Unit_Key 
	 	AND			((CU.Domain_Mask & @UserDomainMask > 0)		OR (CU.Entered_Session_ID = @SessionID) 
		OR			(CU.Changed_Session_ID	=		@SessionID) OR (CU.Domain_Mask = 0))
	INNER JOIN		Specimen_Field_Data		AS		SFD 
	ON				SFD.Collection_Unit_Key =		SU.Collection_Unit_Key
	LEFT JOIN		Occurrence				AS		O	
	ON				O.Occurrence_Key		=		SFD.Occurrence_Key
	LEFT JOIN		Taxon_Occurrence		AS		XO
	ON				XO.Taxon_Occurrence_Key	=		SFD.Taxon_Occurrence_Key
	INNER JOIN		[Sample]				AS		S
	ON				S.Sample_Key			=		O.Sample_Key 
		OR			S.Sample_Key			=		XO.Sample_Key
	LEFT JOIN		Metadata				AS		M
	ON				SU.collection_unit_key	=		M.Record_Key
	LEFT JOIN		Metadata_Type			AS		MT
	ON				M.metadata_type_key		=		MT.metadata_type_key
	AND				MT.Item_Name			=		'Provenance'
	WHERE			(ISNULL(S.Location_Name, '')	LIKE	@SearchText + '%'
		OR			ISNULL(M.Text, '')				LIKE	@SearchText + '%')		

	UPDATE			@SpecimensSearch
	SET				Number							=		CUN.Number
	FROM 			@SpecimensSearch				AS		SU
	LEFT JOIN		Collection_Unit_Number			AS		CUN
	ON				SU.Item_key						=		CUN.Collection_Unit_Key 
	AND				CUN.Preferred					=		1
	
	UPDATE			@SpecimensSearch
	SET				Det_Item_Key					=		CPref.Concept_Key,
					Item_Name						=		CASE @ShowOriginalSpecimenNames
																WHEN 1
																THEN TDet.Item_Name
																ELSE TPref.Item_Name
															END,
					Det_Item_Name					=		TDet.Plaintext
	FROM			@SpecimensSearch				AS		SU
	INNER JOIN		VW_SpecimenDetsEarth			AS		SDE
	ON				SDE.Collection_Unit_Key			=		SU.Item_Key
	AND				SDE.Preferred_Determination_Key	=		SDE.Determination_Key
	INNER JOIN		Concept							AS		C
	ON				C.Concept_Key					=		SDE.Concept_Key
	INNER JOIN		Term							AS		TDet
	ON				TDet.Term_Key					=		C.Term_Key
	INNER JOIN		Concept							AS		CPref
	ON				CPref.Meaning_Key				=		C.Meaning_Key
	AND				CPref.List_Preferred			=		1
	AND				CPref.Concept_Group_Key			=		C.Concept_Group_Key
	INNER JOIN		Term							AS		TPref
	ON				TPref.Term_Key					=		CPref.Term_Key

	UPDATE			@SpecimensSearch
	SET				Det_Item_Key					=		SDL.Taxon_List_Item_Key,
					Item_Name						=		dbo.ufn_GetFormattedTaxonNameByParams(
																	CASE @ShowOriginalSpecimenNames
																		WHEN 1
																		THEN Actual_Name
																		ELSE Preferred_Name
																	END,
																	CASE @ShowOriginalSpecimenNames
																		WHEN 1
																		THEN Actual_Name_Italic
																		ELSE Preferred_Name_Italic
																	END,
																	Common_Name,
																	Common_Name_Italic,
																	null,
																	@ShowCommonNames),
					Det_Item_Name					=		ITN.Actual_Name,
					Hint							=		ITN.Actual_Name
	FROM			@SpecimensSearch				AS		SU
	INNER JOIN		VW_SpecimenDetsLife				AS		SDL
	ON				SDL.Collection_Unit_Key			=		SU.Item_Key
	AND				SDL.Taxon_Determination_Key		=		SDL.Preferred_Taxon_Determination_Key
	INNER JOIN		Index_Taxon_Name				AS		ITN
	ON				ITN.Taxon_List_Item_Key			=		SDL.Taxon_List_Item_Key

	-- Select table and sort appropriately
	IF @SortOrderIndex = 0
		SELECT * FROM @SpecimensSearch
		ORDER BY Det_Item_Name, Number
	ELSE IF @SortOrderIndex = 1
		SELECT * FROM @SpecimensSearch
		ORDER BY Number, Item_Name
GO

/*============================================================================*\
  Grant permissions.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_Specimens_Select_ForSearchByObservationLocation') IS NOT NULL
BEGIN
	PRINT 'Setting up security on procedure usp_Specimens_Select_ForSearchByObservationLocation'
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByObservationLocation TO R2k_AddOnly
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByObservationLocation TO R2k_Administrator
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByObservationLocation TO R2k_FullEdit
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByObservationLocation TO R2k_ReadOnly
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByGatheringDate TO R2k_RecordCardsOnly
END
GO