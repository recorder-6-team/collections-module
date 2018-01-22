SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

CREATE INDEX IX_Collection_Unit_Number_Collection_Unit_Key ON dbo.Collection_Unit_Number (Collection_Unit_Key)
GO

SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

IF OBJECT_ID('dbo.usp_Specimens_Select_ForSearchByGeoArea') IS NOT NULL
	DROP PROCEDURE dbo.usp_Specimens_Select_ForSearchByGeoArea;
GO

/*============================================================================*\
  Description:	Returns specimens based on their geographic area. Includes 
				matching on synonymns.
  Created: July 2016

  Last revision information:
		$Revision: 2 $
		$Date: 4/08/16 9:26 $
		$Author: Andrewkemp $
\*============================================================================*/

CREATE PROCEDURE [dbo].[usp_Specimens_Select_ForSearchByGeoArea]
	@UserDomainMask						INT,
	@SessionID							CHAR(16),
	@ShowCommonNames					BIT,
	@ShowOriginalSpecimenNames			BIT,
	@SearchText							VARCHAR(100),
	@SortOrderIndex						TINYINT
AS
	SET NOCOUNT ON

	-- Firstly, identify the geographic areas to consider:
	DECLARE @GeoArea TABLE (
		Concept_Key						CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS,
		Published_Term					NVARCHAR(500)  COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
		PRIMARY KEY						(Concept_Key)
	)

	INSERT INTO	@GeoArea (
				Concept_Key,
				Published_Term)
	SELECT DISTINCT
				s1.Concept_Key,
				s1.Published_Term
	FROM		dbo.Domain				AS	d
	INNER JOIN	dbo.Local_Domain		AS	l
	ON			l.Domain_Key			=	d.Domain_Key
	INNER JOIN	dbo.Concept_Group		AS	g
	ON			g.Local_Domain_Key		=	l.Local_Domain_Key
	INNER JOIN	dbo.Concept				AS	c0
	ON			c0.Concept_Group_Key	=	g.Concept_Group_Key
	-- take synonyms of the matching concepts
	INNER JOIN	dbo.Concept				AS	s0					
	ON			s0.Meaning_Key			=	c0.Meaning_Key
	-- take descendants of those synonyms
	INNER JOIN	dbo.Concept_Lineage		AS	l0
	ON			l0.Concept_Key			=	s0.Concept_Key
	INNER JOIN	dbo.Concept_Lineage		AS	l1
	ON			l1.Lineage				=	l0.Lineage
	OR			l1.Lineage				LIKE l0.Lineage + '\%'
	INNER JOIN	dbo.Concept				AS	c1
	ON			c1.Concept_Key			=	l1.Concept_Key
	AND			c1.Concept_Group_Key	=	s0.Concept_Group_Key
	-- finally, take synonyms of those descendants
	INNER JOIN	dbo.Concept				AS	s1
	ON			s1.Meaning_Key			=	c1.Meaning_Key
	-- search for terms beginning with @Search in the "Geography" subject area
	WHERE		d.Subject_Area_Key		=	'DSS0039400000005'
	AND			c0.Published_Term		LIKE @SearchText + '%'

	-- Now we look for specimens that have survey events in those areas:

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
		[Hint] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[NomenclaturalStatus] VARCHAR(250) NULL  
	)

	-- Life sciences (Taxon_Occurrence)
	INSERT INTO @SpecimensSearch (Item_Key, Life_Sciences, NomenclaturalStatus) 
	SELECT 	DISTINCT SU.Collection_Unit_Key, SU.Life_Sciences,
							SUBSTRING(( 
								SELECT	C.Published_Term + ','
								FROM	Taxon_Determination D 
								INNER JOIN	Concept C
									ON	D.Nomenclatural_Status_Concept_Key = C.Concept_Key
								WHERE	D.Specimen_Collection_Unit_Key = SU.Collection_Unit_Key
								ORDER BY	D.Taxon_Determination_Key
									FOR XML PATH('')
							), 0, 250)
	FROM 	Specimen_Unit SU
	INNER JOIN Collection_Unit CU ON SU.Collection_Unit_Key = CU.Collection_Unit_Key 
	 	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
		OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
	INNER JOIN Specimen_Field_Data SFD ON SFD.Collection_Unit_Key = SU.Collection_Unit_Key
	INNER JOIN Taxon_Occurrence XO ON XO.Taxon_Occurrence_Key=SFD.Taxon_Occurrence_Key
	INNER JOIN [Sample] S ON S.Sample_Key = XO.Sample_Key
	INNER JOIN [Survey_Event] SE ON SE.Survey_Event_Key=S.Survey_Event_Key
	INNER JOIN Survey_Event_Geo_Area SEGA ON SEGA.Survey_Event_Key=SE.Survey_Event_Key
	INNER JOIN @GeoArea AS GA ON GA.Concept_Key = SEGA.Concept_Key

	-- Not life sciences (Occurrence)
	INSERT INTO @SpecimensSearch (Item_Key, Life_Sciences, NomenclaturalStatus) 
	SELECT 	DISTINCT SU.Collection_Unit_Key, SU.Life_Sciences, 
							SUBSTRING(( 
								SELECT	C.Published_Term + ','
								FROM	Determination D 
								INNER JOIN	Concept C
									ON	D.Nomenclatural_Status_Concept_Key = C.Concept_Key
								WHERE	D.Specimen_Collection_Unit_Key = SU.Collection_Unit_Key
								ORDER BY	D.Determination_Key
									FOR XML PATH('')
							), 0, 250) 
	FROM 	Specimen_Unit SU
	INNER JOIN Collection_Unit CU ON SU.Collection_Unit_Key = CU.Collection_Unit_Key 
	 	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
		OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
	INNER JOIN Specimen_Field_Data SFD ON SFD.Collection_Unit_Key = SU.Collection_Unit_Key
	INNER JOIN Occurrence O ON O.Occurrence_Key = SFD.Occurrence_Key
	INNER JOIN [Sample] S ON S.Sample_Key=O.Sample_Key
	INNER JOIN [Survey_Event] SE ON SE.Survey_Event_Key=S.Survey_Event_Key
	INNER JOIN Survey_Event_Geo_Area SEGA ON SEGA.Survey_Event_Key=SE.Survey_Event_Key
	INNER JOIN @GeoArea AS GA ON GA.Concept_Key = SEGA.Concept_Key

	UPDATE		@SpecimensSearch
	SET			Number = CUN.Number
	FROM 		@SpecimensSearch 		SU
	INNER JOIN 	Collection_Unit_Number 	CUN ON 	SU.Item_key 	= CUN.Collection_Unit_Key 
											AND CUN.Preferred	= 1

	UPDATE @SpecimensSearch
	SET	Det_Item_Key=CPref.Concept_Key,
		Item_Name = CASE @ShowOriginalSpecimenNames
						WHEN 1 THEN C.Published_Term
						ELSE CPref.Published_Term END,
		Det_Item_Name=TDet.Plaintext,
		Hint=TDet.Plaintext
	FROM @SpecimensSearch SU
	INNER JOIN VW_SpecimenDetsEarth SDE ON SDE.Collection_Unit_Key=SU.Item_Key
		AND SDE.Preferred_Determination_Key=SDE.Determination_Key
	INNER JOIN Concept C ON C.Concept_Key=SDE.Concept_Key
	INNER JOIN Term TDet ON TDet.Term_Key=C.Term_Key
	INNER JOIN Concept CPref ON CPref.Meaning_Key=C.Meaning_Key
		AND CPref.List_Preferred=1
		AND CPref.Concept_Group_Key=C.Concept_Group_Key
	

	UPDATE @SpecimensSearch
	SET	Det_Item_Key=SDL.Taxon_List_Item_Key,
		Item_Name = dbo.ufn_GetFormattedTaxonNameByParams(
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
		Det_Item_Name=ITN.Actual_Name,
		Hint=ITN.Actual_Name
	FROM @SpecimensSearch SU
	INNER JOIN VW_SpecimenDetsLife SDL ON SDL.Collection_Unit_Key=SU.Item_Key
			AND SDL.Preferred_Taxon_Determination_Key = SDL.Taxon_Determination_Key
	INNER JOIN Index_Taxon_Name ITN	ON ITN.Taxon_List_Item_Key=SDL.Taxon_List_Item_Key

	-- Select table and sort appropriately
	IF @SortOrderIndex = 0
		SELECT * FROM @SpecimensSearch
		ORDER BY Item_Name, Number
	ELSE IF @SortOrderIndex = 1
		SELECT * FROM @SpecimensSearch
		ORDER BY Number, Item_Name
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Specimens_Select_ForSearchByGeoArea') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Specimens_Select_ForSearchByGeoArea'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByGeoArea TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByGeoArea TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByGeoArea TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByGeoArea TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByGeoArea TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByGeoArea TO [Dev - JNCC SQL]
END

GO