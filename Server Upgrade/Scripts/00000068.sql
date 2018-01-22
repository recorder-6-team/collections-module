/*===========================================================================*\
  Description:
	Further Updates for CCN 156 - Nomenclatural Status to also 
		be used for Leaf Nodes.

  Created:
	March 2013

  Last revision information:
    $Revision: 1 $
    $Date: 13/03/13 8:46 $
    $Author: Alexanderpadley $

\*===========================================================================*/

SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

/*============================================================================*\
  Drop stored proc before re-creating.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_Specimens_Select_ForValuation') IS NOT NULL
	DROP PROCEDURE dbo.usp_Specimens_Select_ForValuation
GO

/*============================================================================*\
  Description:	Returns specimens' data for a specified valuation.

  Parameters:	@UserDomainMask			User's Domain Mask restricting which
										records may be returned
				@SessionID				User's SessionID
				@ParentKey				Identifies the valuation
				@UserID					Name_Key of current user
				@ShowCommonNames		Specifies whether Common Names are shown
				@ShowOriginalSpecimenNames
										0 => preferred names are shown
										1 => names originally entered are shown
				@SortOrderIndex			0 => results sorted by Item_Name, Number
										1 => results sorted by Number, Item_Name

  Created:		August 2003

  Last revision information:
    $Revision: 1 $
    $Date: 13/03/13 8:46 $
    $Author: Alexanderpadley $
\*============================================================================*/
CREATE PROCEDURE dbo.usp_Specimens_Select_ForValuation
	@UserDomainMask						INT,
	@SessionID							CHAR(16),
	@ParentKey							CHAR(16),
	@UserID								CHAR(16),
	@ShowCommonNames					BIT,
	@ShowOriginalSpecimenNames			BIT,
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

	INSERT INTO	@SpecimensSearch (
				Item_Key,
				Join_Key,
				Life_Sciences,
				NomenclaturalStatus)
	SELECT		s.Collection_Unit_Key,
				v.Collection_Unit_Valuation_Key,
				s.Life_Sciences,
				CASE Life_Sciences
					WHEN 1 THEN
						SUBSTRING(( 
							SELECT	C.Published_Term + ','
							FROM	Taxon_Determination D 
							INNER JOIN	Concept C
								ON	D.Nomenclatural_Status_Concept_Key = C.Concept_Key
							WHERE	D.Nomenclatural_Status_Concept_Key IS NOT NULL
								AND	D.Specimen_Collection_Unit_Key = s.Collection_Unit_Key
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
								AND	D.Specimen_Collection_Unit_Key = s.Collection_Unit_Key
							ORDER BY	D.Determination_Key
								FOR XML PATH('')
						), 0, 250) 
				END
	FROM 		Collection_Unit_Valuation			AS	v
	INNER JOIN	"User"								AS	u
	ON			u.Name_Key							=	@UserID 
	INNER JOIN	Specimen_Unit						AS	s
	ON			s.Collection_Unit_Key				=	v.Collection_Unit_Key
	INNER JOIN	Collection_Unit						AS	cu
	ON			cu.Collection_Unit_Key				=	s.Collection_Unit_Key
	WHERE		v.Valuation_Key						=	@ParentKey
	AND			u.Allow_Finance						=	1
	AND			(cu.Domain_Mask & @UserDomainMask	>	0
	OR			cu.Entered_Session_ID				=	@SessionID
	OR			cu.Changed_Session_ID				=	@SessionID
	OR			cu.Domain_Mask						=	0)
	
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
IF OBJECT_ID('dbo.usp_Specimens_Select_ForValuation') IS NOT NULL
BEGIN
   	PRINT 'Setting up security on procedure usp_Specimens_Select_ForValuation'
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_AddOnly')
       	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForValuation TO R2k_AddOnly
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForValuation TO R2k_Administrator
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForValuation TO R2k_FullEdit
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForValuation TO R2k_ReadOnly
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForValuation TO R2k_RecordCardsOnly
END
GO

/*============================================================================*\
  Drop stored proc before re-creating.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_Specimens_Select_ForStore') IS NOT NULL
	DROP PROCEDURE dbo.usp_Specimens_Select_ForStore
GO

/*============================================================================*\
  Description:	Returns specimens' data to the Collections Browser for a
				specified store.

  Parameters:	@ParentKey				Identifies the store
				@UserDomainMask			User's domain mask restricting which
										records may be returned
				@SessionID 				User's SessionID
				@ShowCommonNames		Specifies whether Common Names are shown
				@ShowOriginalSpecimenNames
										0 => preferred names are shown
										1 => names originally entered are shown
				@SortOrderIndex			0 => results sorted by Item_Name, Number
										1 => results sorted by Number, Item_Name

  Created:		October 2003

  Last revision information:
	$Revision: 1 $
	$Date: 13/03/13 8:46 $
	$Author: Alexanderpadley $
\*============================================================================*/
CREATE PROCEDURE dbo.usp_Specimens_Select_ForStore
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
		[NomenclaturalStatus] VARCHAR(250) NULL  
	)

	INSERT INTO @SpecimensSearch (Item_Key, Join_Key, Life_Sciences, NomenclaturalStatus) 
	SELECT 	DISTINCT	SU.Collection_Unit_Key, 
						SU.Collection_Unit_Key, 
						SU.Life_Sciences,
						CASE SU.Life_Sciences
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
	WHERE 	CU.Current_Container_Collection_Unit_Key = @ParentKey



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
IF OBJECT_ID('dbo.usp_Specimens_Select_ForStore') IS NOT NULL
BEGIN
	PRINT 'Setting up security on procedure usp_Specimens_Select_ForStore'
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForStore TO R2k_AddOnly
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForStore TO R2k_Administrator
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForStore TO R2k_FullEdit
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForStore TO R2k_ReadOnly
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForStore TO R2k_RecordCardsOnly
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Specimens_Select_ForSearch]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Specimens_Select_ForSearch]
GO

/*===========================================================================*\
  Description:	Returns Collection_Unit_Key and DisplayTerm when search characters 
		are entered. The Specimen_Unit table does not have a Display_Caption 
		or Search_Caption field, so the caption must be constructed through 
		joins to other tables.

  Parameters:	@UserDomainMask		User's Domain Mask restricting which records may be returned.
		@SessionID 		User's SessionID.
		@SearchText 		Search text used to find collections.

  Created:	September 2003

  Last revision information:
    $Revision: 1 $
    $Date: 13/03/13 8:46 $
    $Author: Alexanderpadley $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Specimens_Select_ForSearch] 
	@UserDomainMask int,
	@SessionID char(16),
	@ShowCommonNames BIT,
	@SearchText varchar(100)
AS

SET NOCOUNT ON


--Use temp table to build output, so silly data errors such as duplicate accessions
-- don't duplicate in the list
DECLARE @SpecimensSearch TABLE
(
	[Item_Key] [char] (16)				COLLATE database_default NULL,
	[DisplayTerm] [nvarchar] (150)		COLLATE database_default NULL,
	[SearchTerm] [nvarchar] (150)		COLLATE database_default NULL,
	[Life_Sciences] [bit] NULL,
	[NomenclaturalStatus] VARCHAR(250) NULL 
)

--Find all specimens with an earth sciences determination match
INSERT INTO 
	@SpecimensSearch (Item_Key, Life_Sciences, SearchTerm, DisplayTerm, NomenclaturalStatus) 
SELECT DISTINCT 
	SU.Collection_Unit_Key COLLATE database_default, 
	0,
	CSearch.Published_Term COLLATE database_default AS SearchTerm,
	CSearch.Published_Term COLLATE database_default AS DisplayTerm,
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
FROM SPECIMEN_UNIT SU
INNER JOIN Collection_Unit CU ON SU.Collection_Unit_Key = CU.Collection_Unit_Key 
 	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
	OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
LEFT JOIN VW_SpecimenDetsEarth SDE ON SU.Collection_Unit_Key = SDE.Collection_Unit_Key
LEFT JOIN Concept C ON SDE.Concept_Key = C.Concept_Key
LEFT JOIN Concept CSearch ON CSearch.Meaning_Key=C.Meaning_Key
LEFT JOIN Search_Term ST ON ST.Concept_Key = CSearch.Concept_Key
WHERE ST.Plaintext LIKE @SearchText + '%' AND SU.Life_Sciences=0

--Find all specimens with a life sciences determination match
INSERT INTO 
	@SpecimensSearch (Item_Key, Life_Sciences, SearchTerm, DisplayTerm, NomenclaturalStatus) 
SELECT DISTINCT 
	SU.Collection_Unit_Key COLLATE database_default, 
	1,
	ITN.Actual_Name	COLLATE database_default AS SearchTerm,
	CASE ITN.Actual_Name_Italic	
		WHEN 1 THEN '<i>' + ITN.Actual_Name + '</i>' 
		ELSE ITN.Actual_Name			
	END COLLATE database_default AS DisplayTerm,
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
FROM SPECIMEN_UNIT SU
INNER JOIN Collection_Unit CU ON SU.Collection_Unit_Key = CU.Collection_Unit_Key 
 	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
	OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
LEFT JOIN VW_SpecimenDetsLife SDL ON SU.Collection_Unit_Key = SDL.Collection_Unit_Key
LEFT JOIN Index_Taxon_Synonym ITS ON ITS.Taxon_List_Item_Key=SDL.Taxon_List_Item_Key
LEFT JOIN INDEX_TAXON_NAME ITN	ON ITS.Synonym_List_Item_Key = ITN.Taxon_List_Item_Key
WHERE ITN.Actual_Name LIKE @SearchText + '%' AND SU.Life_Sciences = 1

-- Find all collection numbers
INSERT INTO
	@SpecimensSearch (Item_Key, Life_Sciences, SearchTerm, DisplayTerm, NomenclaturalStatus)
SELECT DISTINCT
	SU.Collection_Unit_Key COLLATE database_default,
	SU.Life_Sciences,
	CSearch.Published_Term COLLATE database_default AS SearchTerm,
	CSearch.Published_Term COLLATE database_default AS DisplayTerm,
	CASE SU.Life_Sciences
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
FROM SPECIMEN_UNIT SU
INNER JOIN	Collection_Unit CU ON SU.Collection_Unit_Key = CU.Collection_Unit_Key
 	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
	OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
LEFT JOIN Collection_Unit_Number CUN ON CU.Collection_Unit_Key = CUN.Collection_Unit_Key
LEFT JOIN VW_SpecimenDetsEarth SDE ON SU.Collection_Unit_Key = SDE.Collection_Unit_Key
LEFT JOIN Concept C ON SDE.Concept_Key = C.Concept_Key
LEFT JOIN Concept CSearch ON CSearch.Meaning_Key=C.Meaning_Key
--LEFT JOIN Search_Term ST ON ST.Concept_Key = CSearch.Concept_Key
WHERE	CUN.Number LIKE @SearchText + '%'
	-- This should prevent duplicates which have similar CUK and Search Term?
	--AND	ST.Plaintext NOT LIKE @SearchText + '%'

-- Update the number in case there are 2 registrations for a specimen, so we don't duplicate
-- the rows in the output results.
UPDATE @SpecimensSearch
SET 
		SearchTerm = SearchTerm + ' - ' + CUN.Number,
		DisplayTerm = DisplayTerm + ' - ' + CUN.Number
FROM @SpecimensSearch SU
INNER JOIN Collection_Unit_Number CUN ON SU.Item_key = CUN.Collection_Unit_Key 	AND CUN.Preferred = 1

-- Select table and sort appropriately
SELECT * from @SpecimensSearch
ORDER BY SearchTerm

GO




/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Specimens_Select_ForSearch') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_Specimens_Select_ForSearch'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
       	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearch TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearch TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearch TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearch TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearch TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
       	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearch TO [Dev - JNCC SQL]
END
GO

/*============================================================================*\
  Drop stored proc before re-creating.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_Specimens_Select_ForMovementOut') IS NOT NULL
	DROP PROCEDURE dbo.usp_Specimens_Select_ForMovementOut
GO

/*============================================================================*\
 Description:	Returns specimens for a specified movement.

  Parameters:	@UserDomainMask			User's Domain Mask restricting which
										records may be returned
				@SessionID 				User's SessionID
				@ParentKey 				Identifies the movement
				@ShowCommonNames		Specifies whether Common Names are shown
				@ShowOriginalSpecimenNames
										0 => preferred names are shown
										1 => names originally entered are shown
				@SortOrderIndex			0 => results sorted by Item_Name, Number
										1 => results sorted by Number, Item_Name

  Created:		August 2003

  Last revision information:
    $Revision: 1 $
    $Date: 13/03/13 8:46 $
    $Author: Alexanderpadley $
\*============================================================================*/
CREATE PROCEDURE dbo.usp_Specimens_Select_ForMovementOut
	@UserDomainMask						INT,
	@SessionID							CHAR(16),
	@ParentKey							CHAR(16),
	@ShowCommonNames					BIT,
	@ShowOriginalSpecimenNames			BIT,
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
		NomenclaturalStatus NVARCHAR(250) NULL 
	)
	
	INSERT INTO	@SpecimensSearch (
				Item_Key,
				Join_Key,
				Life_Sciences,
				NomenclaturalStatus)
	SELECT		s.Collection_Unit_Key,
				mcu.Movement_Collection_Unit_Key,
				s.Life_Sciences,
				CASE s.Life_Sciences
					WHEN 1 THEN
						SUBSTRING(( 
							SELECT	C.Published_Term + ','
							FROM	Taxon_Determination D 
							INNER JOIN	Concept C
								ON	D.Nomenclatural_Status_Concept_Key = C.Concept_Key
							WHERE	D.Nomenclatural_Status_Concept_Key IS NOT NULL
								AND	D.Specimen_Collection_Unit_Key = s.Collection_Unit_Key
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
								AND	D.Specimen_Collection_Unit_Key = s.Collection_Unit_Key
							ORDER BY	D.Determination_Key
								FOR XML PATH('')
						), 0, 250) 
				END
	FROM 		Movement							AS	m
	INNER JOIN	Movement_Direction					AS	d
	ON			d.Movement_Key						=	m.Movement_Key
	INNER JOIN	Movement_Collection_Unit			AS	mcu
	ON			mcu.Movement_Direction_Key			=	d.Movement_Direction_Key
	INNER JOIN	Specimen_Unit						AS	s
	ON			s.Collection_Unit_Key				=	mcu.Collection_Unit_Key
	INNER JOIN	Collection_Unit						AS	cu
	ON			cu.Collection_Unit_Key				=	s.Collection_Unit_Key
	WHERE		m.Movement_Key						=	@ParentKey
	AND			d.Outbound							=	1
	AND			(cu.Domain_Mask & @UserDomainMask	>	0
	OR			cu.Entered_Session_ID				=	@SessionID
	OR			cu.Changed_Session_ID				=	@SessionID
	OR			cu.Domain_Mask						=	0)
	
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
IF OBJECT_ID('dbo.usp_Specimens_Select_ForMovementOut') IS NOT NULL
BEGIN
	PRINT 'Setting up security on procedure usp_Specimens_Select_ForMovementOut'
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForMovementOut TO R2k_AddOnly
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForMovementOut TO R2k_Administrator
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForMovementOut TO R2k_FullEdit
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForMovementOut TO R2k_ReadOnly
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForMovementOut TO R2k_RecordCardsOnly
END
GO

/*============================================================================*\
  Drop stored proc before re-creating.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_Specimens_Select_ForMovementIn') IS NOT NULL
	DROP PROCEDURE dbo.usp_Specimens_Select_ForMovementIn
GO

/*============================================================================*\
 Description:	Returns specimens for a specified movement.

  Parameters:	@UserDomainMask			User's Domain Mask restricting which
										records may be returned
				@SessionID 				User's SessionID
				@ParentKey 				Identifies the movement
				@ShowCommonNames		Specifies whether Common Names are shown
				@ShowOriginalSpecimenNames
										0 => preferred names are shown
										1 => names originally entered are shown
				@SortOrderIndex			0 => results sorted by Item_Name, Number
										1 => results sorted by Number, Item_Name

  Created:		August 2003

  Last revision information:
    $Revision: 1 $
    $Date: 13/03/13 8:46 $
    $Author: Alexanderpadley $
\*============================================================================*/
CREATE PROCEDURE dbo.usp_Specimens_Select_ForMovementIn
	@UserDomainMask						INT,
	@SessionID							CHAR(16),
	@ParentKey							CHAR(16),
	@ShowCommonNames					BIT,
	@ShowOriginalSpecimenNames			BIT,
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
	
	INSERT INTO	@SpecimensSearch (
				Item_Key,
				Join_Key,
				Life_Sciences,
				NomenclaturalStatus)
	SELECT		s.Collection_Unit_Key,
				mcu.Movement_Collection_Unit_Key,
				s.Life_Sciences,
				CASE Life_Sciences
					WHEN 1 THEN
						SUBSTRING(( 
							SELECT	C.Published_Term + ','
							FROM	Taxon_Determination D 
							INNER JOIN	Concept C
								ON	D.Nomenclatural_Status_Concept_Key = C.Concept_Key
							WHERE	D.Nomenclatural_Status_Concept_Key IS NOT NULL
								AND	D.Specimen_Collection_Unit_Key = s.Collection_Unit_Key
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
								AND	D.Specimen_Collection_Unit_Key = s.Collection_Unit_Key
							ORDER BY	D.Determination_Key
								FOR XML PATH('')
						), 0, 250) 
				END
	FROM 		Movement							AS	m
	INNER JOIN	Movement_Direction					AS	d
	ON			d.Movement_Key						=	m.Movement_Key
	INNER JOIN	Movement_Collection_Unit			AS	mcu
	ON			mcu.Movement_Direction_Key			=	d.Movement_Direction_Key
	INNER JOIN	Specimen_Unit						AS	s
	ON			s.Collection_Unit_Key				=	mcu.Collection_Unit_Key
	INNER JOIN	Collection_Unit						AS	cu
	ON			cu.Collection_Unit_Key				=	s.Collection_Unit_Key
	WHERE		m.Movement_Key						=	@ParentKey
	AND			d.Outbound							=	0
	AND			(cu.Domain_Mask & @UserDomainMask	>	0
	OR			cu.Entered_Session_ID				=	@SessionID
	OR			cu.Changed_Session_ID				=	@SessionID
	OR			cu.Domain_Mask						=	0)
	
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
IF OBJECT_ID('dbo.usp_Specimens_Select_ForMovementIn') IS NOT NULL
BEGIN
	PRINT 'Setting up security on procedure usp_Specimens_Select_ForMovementIn'
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForMovementIn TO R2k_AddOnly
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForMovementIn TO R2k_Administrator
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForMovementIn TO R2k_FullEdit
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForMovementIn TO R2k_ReadOnly
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForMovementIn TO R2k_RecordCardsOnly
END
GO

/*============================================================================*\
  Drop stored proc before re-creating.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_Specimens_Select_ForMovement') IS NOT NULL
	DROP PROCEDURE dbo.usp_Specimens_Select_ForMovement
GO

/*============================================================================*\
  Description:	Returns specimens for a specified movement.

  Parameters:	@UserDomainMask			User's Domain Mask restricting which
										records may be returned
				@SessionID 				User's SessionID
				@ParentKey 				Identifies the movement
				@ShowCommonNames		Specifies whether Common Names are shown
				@ShowOriginalSpecimenNames
										0 => preferred names are shown
										1 => names originally entered are shown
				@SortOrderIndex			0 => results sorted by Item_Name, Number
										1 => results sorted by Number, Item_Name

  Created:		August 2003

  Last revision information:
	$Revision: 1 $
	$Date: 13/03/13 8:46 $
	$Author: Alexanderpadley $
\*============================================================================*/
CREATE PROCEDURE dbo.usp_Specimens_Select_ForMovement
	@UserDomainMask						INT,
	@SessionID							CHAR(16),
	@ParentKey							CHAR(16),
	@ShowCommonNames					BIT,
	@ShowOriginalSpecimenNames			BIT,
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
	
	INSERT INTO	@SpecimensSearch (
				Item_Key,
				Join_Key,
				Life_Sciences,
				NomenclaturalStatus)
	SELECT		s.Collection_Unit_Key,
				mcu.Movement_Collection_Unit_Key,
				s.Life_Sciences,
				CASE Life_Sciences
					WHEN 1 THEN
						SUBSTRING(( 
							SELECT	C.Published_Term + ','
							FROM	Taxon_Determination D 
							INNER JOIN	Concept C
								ON	D.Nomenclatural_Status_Concept_Key = C.Concept_Key
							WHERE	D.Nomenclatural_Status_Concept_Key IS NOT NULL
								AND	D.Specimen_Collection_Unit_Key = s.Collection_Unit_Key
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
								AND	D.Specimen_Collection_Unit_Key = s.Collection_Unit_Key
							ORDER BY	D.Determination_Key
								FOR XML PATH('')
						), 0, 250) 
				END
	FROM 		Movement							AS	m
	INNER JOIN	Movement_Direction					AS	d
	ON			d.Movement_Key						=	m.Movement_Key
	INNER JOIN	Movement_Collection_Unit			AS	mcu
	ON			mcu.Movement_Direction_Key			=	d.Movement_Direction_Key
	INNER JOIN	Specimen_Unit						AS	s
	ON			s.Collection_Unit_Key				=	mcu.Collection_Unit_Key
	INNER JOIN	Collection_Unit						AS	cu
	ON			cu.Collection_Unit_Key				=	s.Collection_Unit_Key
	WHERE		m.Movement_Key						=	@ParentKey
	AND			(cu.Domain_Mask & @UserDomainMask	>	0
	OR			cu.Entered_Session_ID				=	@SessionID
	OR			cu.Changed_Session_ID				=	@SessionID
	OR			cu.Domain_Mask						=	0)
	
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
IF OBJECT_ID('dbo.usp_Specimens_Select_ForMovement') IS NOT NULL
BEGIN
	PRINT 'Setting up security on procedure usp_Specimens_Select_ForMovement'
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForMovement TO R2k_AddOnly
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForMovement TO R2k_Administrator
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForMovement TO R2k_FullEdit
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForMovement TO R2k_ReadOnly
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForMovement TO R2k_RecordCardsOnly
END
GO

/*============================================================================*\
  Drop stored proc before re-creating.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_Specimens_Select_ForLinkedOther') IS NOT NULL
	DROP PROCEDURE dbo.usp_Specimens_Select_ForLinkedOther
GO

/*============================================================================*\
  Description:	Returns specimens that are related to a particular specimen.

  Parameters:	@UserDomainMask			User's Domain Mask restricting which
										records may be returned
				@SessionID 				User's SessionID
				@ParentKey 				Identifies the specimen
				@ShowCommonNames		Specifies whether Common Names are shown
				@ShowOriginalSpecimenNames
										0 => preferred names are shown
										1 => names originally entered are shown
				@SortOrderIndex			0 => results sorted by Item_Name, Number
										1 => results sorted by Number, Item_Name

  Created:		August 2003

  Last revision information:
    $Revision: 1 $
    $Date: 13/03/13 8:46 $
    $Author: Alexanderpadley $
\*============================================================================*/
CREATE PROCEDURE dbo.usp_Specimens_Select_ForLinkedOther
	@UserDomainMask						INT,
	@SessionID							CHAR(16),
	@ParentKey							CHAR(16),
	@ShowCommonNames					BIT,
	@ShowOriginalSpecimenNames			BIT,
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

	DECLARE @SpecimensSearch TABLE
	(
		Item_Key			CHAR(16) 		COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
		Join_Key			CHAR(16) 		COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
		Hyperlink_Item_Key	CHAR(16) 		COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
		Drag_Drop_Item_Key	CHAR(16) 		COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
		Det_Item_Key		CHAR(16) 		COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		Item_Name			NVARCHAR(150) 	COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		Life_Sciences		BIT NULL,
		Number				VARCHAR(30) 	COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		NomenclaturalStatus	NVARCHAR(250) NULL 
	)
	
	INSERT INTO	@SpecimensSearch (
				Item_Key,
				Join_Key,
				Hyperlink_Item_Key,
				Drag_Drop_Item_Key,
				Life_Sciences,
				NomenclaturalStatus)
	SELECT		cr.Collection_Unit_Relation_Key,
				cr.Collection_Unit_Relation_Key,
				s.Collection_Unit_Key,
				s.Collection_Unit_Key,
				s.Life_Sciences,
				CASE s.Life_Sciences
					WHEN 1 THEN
						SUBSTRING(( 
							SELECT	C.Published_Term + ','
							FROM	Taxon_Determination D 
							INNER JOIN	Concept C
								ON	D.Nomenclatural_Status_Concept_Key = C.Concept_Key
							WHERE	D.Nomenclatural_Status_Concept_Key IS NOT NULL
								AND	D.Specimen_Collection_Unit_Key = s.Collection_Unit_Key
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
								AND	D.Specimen_Collection_Unit_Key = s.Collection_Unit_Key
							ORDER BY	D.Determination_Key
								FOR XML PATH('')
						), 0, 250) 
				END
	FROM 		Collection_Unit_Relation			AS	cr
	INNER JOIN	Thesaurus_Relation_Type				AS	rt
	ON			rt.Thesaurus_Relation_Type_Key		=	cr.Thesaurus_Relation_Type_Key
	INNER JOIN	Semantic_Relation					AS	sr
	ON			sr.Semantic_Relation_Key			=	rt.Semantic_Relation_Key
	INNER JOIN	Specimen_Unit						AS	s
	ON			s.Collection_Unit_Key				=	CASE WHEN cr.From_Collection_Unit_Key = @ParentKey
															THEN cr.To_Collection_Unit_Key
															ELSE cr.From_Collection_Unit_Key
														END
	INNER JOIN	Collection_Unit						AS	cu
	ON			cu.Collection_Unit_Key				=	s.Collection_Unit_Key
	WHERE		(cr.From_Collection_Unit_Key		=	@ParentKey
	OR			(sr.Unidirectional					=	0
	AND			cr.To_Collection_Unit_Key			=	@ParentKey))
	AND			(cu.Domain_Mask & @UserDomainMask	>	0
	OR			cu.Entered_Session_ID				=	@SessionID
	OR			cu.Changed_Session_ID				=	@SessionID
	OR			cu.Domain_Mask						=	0)
	
	UPDATE		@SpecimensSearch
	SET			Number = CUN.Number
	FROM 		@SpecimensSearch 		SU
	LEFT JOIN 	Collection_Unit_Number 	CUN ON 	SU.Hyperlink_Item_Key	= CUN.Collection_Unit_Key 
											AND CUN.Preferred	= 1

	UPDATE 	@SpecimensSearch
	SET		Det_Item_Key	= CPref.Concept_Key,
			Item_Name 		= CASE @ShowOriginalSpecimenNames
								WHEN 1 THEN C.Published_Term
								ELSE CPref.Published_Term END
	FROM 	@SpecimensSearch 		SU
	JOIN 	VW_SpecimenDetsEarth 	SDE 	ON 	SDE.Collection_Unit_Key			= SU.Hyperlink_Item_Key
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
				@ShowCommonNames)
	FROM 	@SpecimensSearch 	SU
	JOIN 	VW_SpecimenDetsLife SDL ON 	SDL.Collection_Unit_Key					= SU.Hyperlink_Item_Key
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
IF OBJECT_ID('dbo.usp_Specimens_Select_ForLinkedOther') IS NOT NULL
BEGIN
	PRINT 'Setting up security on procedure usp_Specimens_Select_ForLinkedOther'
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForLinkedOther TO R2k_AddOnly
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForLinkedOther TO R2k_Administrator
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForLinkedOther TO R2k_FullEdit
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForLinkedOther TO R2k_ReadOnly
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForLinkedOther TO R2k_RecordCardsOnly
END
GO

/*============================================================================*\
  Drop stored proc before re-creating.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_Specimens_Select_ForJob') IS NOT NULL
	DROP PROCEDURE dbo.usp_Specimens_Select_ForJob
GO

/*============================================================================*\
  Description:	Returns specimens associated with a specified job.

  Parameters:	@UserDomainMask			User's Domain Mask restricting which
										records may be returned
				@SessionID 				User's SessionID
				@ParentKey 				Identifies the job
				@ShowCommonNames		Specifies whether Common Names are shown
				@ShowOriginalSpecimenNames
										0 => preferred names are shown
										1 => names originally entered are shown
				@SortOrderIndex			0 => results sorted by Item_Name, Number
										1 => results sorted by Number, Item_Name

  Created:		August 2003

  Last revision information:
    $Revision: 1 $
    $Date: 13/03/13 8:46 $
    $Author: Alexanderpadley $
\*============================================================================*/
CREATE PROCEDURE dbo.usp_Specimens_Select_ForJob
	@UserDomainMask						INT,
	@SessionID							CHAR(16),
	@ParentKey							CHAR(16),
	@ShowCommonNames					BIT,
	@ShowOriginalSpecimenNames			BIT,
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

	DECLARE @SpecimensSearch TABLE
	(
		Item_Key		CHAR(16) 		COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		Join_Key		CHAR(16) 		COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		Det_Item_Key	CHAR(16) 		COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		PlainText		NVARCHAR(150) 	COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		Item_Name		NVARCHAR(150) 	COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		Life_Sciences	BIT NULL,
		Number			VARCHAR(30) 	COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		NomenclaturalStatus	NVARCHAR(250) NULL 
	)
	
	INSERT INTO	@SpecimensSearch (
				Item_Key,
				Join_Key,
				Life_Sciences,
				NomenclaturalStatus)
	SELECT		s.Collection_Unit_Key,
				t.Collection_Unit_Task_Key,
				s.Life_Sciences,
				CASE Life_Sciences
					WHEN 1 THEN
						SUBSTRING(( 
							SELECT	C.Published_Term + ','
							FROM	Taxon_Determination D 
							INNER JOIN	Concept C
								ON	D.Nomenclatural_Status_Concept_Key = C.Concept_Key
							WHERE	D.Nomenclatural_Status_Concept_Key IS NOT NULL
								AND	D.Specimen_Collection_Unit_Key = s.Collection_Unit_Key
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
								AND	D.Specimen_Collection_Unit_Key = s.Collection_Unit_Key
							ORDER BY	D.Determination_Key
								FOR XML PATH('')
						), 0, 250) 
				END
	FROM 		Conservation_Task					AS	ct
	INNER JOIN	Collection_Unit_Task				AS	t
	ON			t.Conservation_Task_Key				=	ct.Conservation_Task_Key
	INNER JOIN	Specimen_Unit						AS	s
	ON			s.Collection_Unit_Key				=	t.Collection_Unit_Key
	INNER JOIN	Collection_Unit						AS	cu
	ON			cu.Collection_Unit_Key				=	s.Collection_Unit_Key
	WHERE		ct.Conservation_Job_Key				=	@ParentKey
	AND			(cu.Domain_Mask & @UserDomainMask	>	0
	OR			cu.Entered_Session_ID				=	@SessionID
	OR			cu.Changed_Session_ID				=	@SessionID
	OR			cu.Domain_Mask						=	0)
	
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
			PlainText		= TDet.Plaintext
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
			PlainText		= ITN.Actual_Name
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
IF OBJECT_ID('dbo.usp_Specimens_Select_ForJob') IS NOT NULL
BEGIN
	PRINT 'Setting up security on procedure usp_Specimens_Select_ForJob'
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForJob TO R2k_AddOnly
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForJob TO R2k_Administrator
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForJob TO R2k_FullEdit
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForJob TO R2k_ReadOnly
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForJob TO R2k_RecordCardsOnly
END
GO

/*============================================================================*\
  Drop stored proc before re-creating.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_Specimens_Select_ForEnquiry') IS NOT NULL
	DROP PROCEDURE dbo.usp_Specimens_Select_ForEnquiry
GO

/*============================================================================*\
  Description:	Returns specimens for a specified enquiry.

  Parameters:	@UserDomainMask			User's Domain Mask restricting which
										records may be returned
				@SessionID 				User's SessionID
				@ParentKey 				Identifies the enquiry
				@ShowCommonNames		Specifies whether Common Names are shown
				@ShowOriginalSpecimenNames
										0 => preferred names are shown
										1 => names originally entered are shown
				@SortOrderIndex			0 => results sorted by Item_Name, Number
										1 => results sorted by Number, Item_Name

  Created:		August 2003

  Last revision information:
	$Revision: 1 $
	$Date: 13/03/13 8:46 $
	$Author: Alexanderpadley $
\*============================================================================*/
CREATE PROCEDURE dbo.usp_Specimens_Select_ForEnquiry
	@UserDomainMask						INT,
	@SessionID							CHAR(16),
	@ParentKey							CHAR(16),
	@ShowCommonNames					BIT,
	@ShowOriginalSpecimenNames			BIT,
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
	
	INSERT INTO	@SpecimensSearch (
				Item_Key,
				Join_Key,
				Life_Sciences,
				NomenclaturalStatus)
	SELECT		s.Collection_Unit_Key,
				q.Collection_Unit_Enquiry_Key,
				s.Life_Sciences,
				CASE s.Life_Sciences
					WHEN 1 THEN
						SUBSTRING(( 
							SELECT	C.Published_Term + ','
							FROM	Taxon_Determination D 
							INNER JOIN	Concept C
								ON	D.Nomenclatural_Status_Concept_Key = C.Concept_Key
							WHERE	D.Nomenclatural_Status_Concept_Key IS NOT NULL
								AND	D.Specimen_Collection_Unit_Key = s.Collection_Unit_Key
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
								AND	D.Specimen_Collection_Unit_Key = s.Collection_Unit_Key
							ORDER BY	D.Determination_Key
								FOR XML PATH('')
						), 0, 250) 
				END
	FROM 		Collection_Unit_Enquiry				AS	q
	INNER JOIN	Specimen_Unit						AS	s
	ON			s.Collection_Unit_Key				=	q.Collection_Unit_Key
	INNER JOIN	Collection_Unit						AS	cu
	ON			cu.Collection_Unit_Key				=	s.Collection_Unit_Key
	WHERE		q.Enquiry_Key						=	@ParentKey
	AND			(cu.Domain_Mask & @UserDomainMask	>	0
	OR			cu.Entered_Session_ID				=	@SessionID
	OR			cu.Changed_Session_ID				=	@SessionID
	OR			cu.Domain_Mask						=	0)
	
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
IF OBJECT_ID('dbo.usp_Specimens_Select_ForEnquiry') IS NOT NULL
BEGIN
	PRINT 'Setting up security on procedure usp_Specimens_Select_ForEnquiry'
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForEnquiry TO R2k_AddOnly
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForEnquiry TO R2k_Administrator
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForEnquiry TO R2k_FullEdit
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForEnquiry TO R2k_ReadOnly
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForEnquiry TO R2k_RecordCardsOnly
END
GO

/*============================================================================*\
  Drop stored proc before re-creating.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_Specimens_Select_ForConditionCheck') IS NOT NULL
	DROP PROCEDURE dbo.usp_Specimens_Select_ForConditionCheck
GO

/*===========================================================================*\
  Description:	Returns specimens associated with a specified condition check

  Parameters:	@UserDomainMask			User's Domain Mask restricting which
										records may be returned
				@SessionID 				User's SessionID
				@ParentKey 				Identifies the condition check
				@ShowCommonNames		Specifies whether Common Names are shown
				@ShowOriginalSpecimenNames
										0 => preferred names are shown
										1 => names originally entered are shown
				@SortOrderIndex			0 => results sorted by Item_Name, Number
										1 => results sorted by Number, Item_Name

  Created:		August 2003

  Last revision information:
    $Revision: 1 $
    $Date: 13/03/13 8:46 $
    $Author: Alexanderpadley $
\*===========================================================================*/
CREATE PROCEDURE dbo.usp_Specimens_Select_ForConditionCheck
	@UserDomainMask						INT,
	@SessionID							CHAR(16),
	@ParentKey							CHAR(16),
	@ShowCommonNames					BIT,
	@ShowOriginalSpecimenNames			BIT,
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
	
	INSERT INTO	@SpecimensSearch (
				Item_Key,
				Join_Key,
				Life_Sciences,
				NomenclaturalStatus)
	SELECT		s.Collection_Unit_Key,
				k.Collection_Unit_Check_Key,
				s.Life_Sciences,
				CASE s.Life_Sciences
					WHEN 1 THEN
						SUBSTRING(( 
							SELECT	C.Published_Term + ','
							FROM	Taxon_Determination D 
							INNER JOIN	Concept C
								ON	D.Nomenclatural_Status_Concept_Key = C.Concept_Key
							WHERE	D.Nomenclatural_Status_Concept_Key IS NOT NULL
								AND	D.Specimen_Collection_Unit_Key = s.Collection_Unit_Key
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
								AND	D.Specimen_Collection_Unit_Key = s.Collection_Unit_Key
							ORDER BY	D.Determination_Key
								FOR XML PATH('')
						), 0, 250) 
				END
	FROM 		Collection_Unit_Check				AS	k
	INNER JOIN	Specimen_Unit						AS	s
	ON			s.Collection_Unit_Key				=	k.Collection_Unit_Key
	INNER JOIN	Collection_Unit						AS	cu
	ON			cu.Collection_Unit_Key				=	s.Collection_Unit_Key
	WHERE		k.Conservation_Check_Key			=	@ParentKey
	AND			(cu.Domain_Mask & @UserDomainMask	>	0
	OR			cu.Entered_Session_ID				=	@SessionID
	OR			cu.Changed_Session_ID				=	@SessionID
	OR			cu.Domain_Mask						=	0)
	
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
IF OBJECT_ID('dbo.usp_Specimens_Select_ForConditionCheck') IS NOT NULL
BEGIN
	PRINT 'Setting up security on procedure usp_Specimens_Select_ForConditionCheck'
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForConditionCheck TO R2k_AddOnly
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForConditionCheck TO R2k_Administrator
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForConditionCheck TO R2k_FullEdit
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForConditionCheck TO R2k_ReadOnly
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForConditionCheck TO R2k_RecordCardsOnly
END
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
	$Revision: 1 $
	$Date: 13/03/13 8:46 $
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

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_SpecimensDetermined_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_SpecimensDetermined_Select]
GO

/*===========================================================================*\
  Description:	Returns the list of all preferred determinations and the
		preferred number for the specimens linked to the currently
		selected collection. Returns the list of all determinations for
		specimen top level node.

  Parameters:	@Key			Key of the determiner.
		@CollectionUnitKey	Key of the collection unit
		@KeyIsSpecimen		Bit saying whether the key is a specimen

  Created:	October 2003

  Last revision information:
    $Revision: 1 $
    $Date: 13/03/13 8:46 $
    $Author: Alexanderpadley $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SpecimensDetermined_Select]
	@Key char(16),
	@CollectionUnitKey char(16),
	@UserDomainMask int,
	@SessionID char(16),
	@ShowCommonNames bit,
	@KeyIsSpecimen bit
AS

SET NOCOUNT ON

IF @KeyIsSpecimen = 1
BEGIN
	SELECT DISTINCT
			SU.Collection_Unit_Key AS Item_Key, 
			CASE SU.Life_Sciences 
				WHEN 0 THEN 
					CTPref.Item_Name COLLATE SQL_Latin1_General_CP1_CI_AS
				ELSE dbo.ufn_GetFormattedTaxonNameByParams(
								ITN.Preferred_Name,
								0,
								ITN.Common_Name,
								0,
								NULL,
								@ShowCommonNames)
			END + 
			CASE 
				WHEN CUN.Number IS NULL THEN '' 
				ELSE ' - ' + CUN.Number 
			END AS Item_Name,
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
			END AS NomenclaturalStatus
	FROM 		Specimen_Unit AS SU
	INNER JOIN	Collection_Unit AS CU ON CU.Collection_Unit_Key = SU.Collection_Unit_Key	
					AND ((CU.Domain_Mask & @UserDomainMask > 0) 
						OR (CU.Entered_Session_ID = @SessionID) 
						OR (CU.Changed_Session_ID = @SessionID) 
						OR (CU.Domain_Mask = 0))
	LEFT JOIN	Taxon_Determination AS TD ON TD.Taxon_Determination_Key = SU.Preferred_Taxon_Determination_Key
	LEFT JOIN	Determination AS D ON D.Determination_Key = SU.Preferred_Determination_Key
	INNER JOIN	Individual AS I ON (I.Name_Key = TD.Determiner
						OR I.Name_Key = D.Determiner_Name_Key)
					AND I.Name_Key = @Key
	LEFT JOIN 	VW_ConceptTermPreferred CTPref ON CTPref.Concept_Key=D.Concept_Key
	LEFT JOIN 	Index_Taxon_Name ITN ON ITN.Taxon_List_Item_Key=TD.Taxon_List_Item_Key
	LEFT JOIN 	Collection_Unit_Number CUN ON SU.Collection_Unit_key = CUN.Collection_Unit_Key 
			 			AND CUN.Preferred = 1
	WHERE 		SU.Collection_unit_key = @CollectionUnitKey
	ORDER BY 	Item_Name
END
ELSE
BEGIN
	SELECT DISTINCT
			SU.Collection_Unit_Key AS Item_Key, 
			CASE SU.Life_Sciences 
				WHEN 0 THEN 
					CTPref.PlainText COLLATE SQL_Latin1_General_CP1_CI_AS
				ELSE dbo.ufn_GetFormattedTaxonNameByParams(
								ITN.Preferred_Name,
								0,
								ITN.Common_Name,
								0,
								NULL,
								@ShowCommonNames)
			END + 
			CASE 
				WHEN CUN.Number IS NULL THEN '' 
				ELSE ' - ' + CUN.Number 
			END AS Item_Name,
			CASE SU.Life_Sciences
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
			END AS NomenclaturalStatus
	FROM 		Specimen_Unit AS SU
	INNER JOIN	Collection_Unit AS CU ON CU.Collection_Unit_Key = SU.Collection_Unit_Key	
					AND ((CU.Domain_Mask & @UserDomainMask > 0) 
						OR (CU.Entered_Session_ID = @SessionID) 
						OR (CU.Changed_Session_ID = @SessionID) 
						OR (CU.Domain_Mask = 0))
	LEFT JOIN	Taxon_Determination AS TD ON TD.Taxon_Determination_Key = SU.Preferred_Taxon_Determination_Key
	LEFT JOIN	Determination AS D ON D.Determination_Key = SU.Preferred_Determination_Key
	INNER JOIN	Individual AS I ON (I.Name_Key = TD.Determiner
						OR I.Name_Key = D.Determiner_Name_Key)
					AND I.Name_Key = @Key
	LEFT JOIN 	VW_ConceptTermPreferred CTPref ON CTPref.Concept_Key=D.Concept_Key
	LEFT JOIN 	Index_Taxon_Name ITN ON ITN.Taxon_List_Item_Key=TD.Taxon_List_Item_Key
	LEFT JOIN 	Collection_Unit_Number CUN ON SU.Collection_Unit_key = CUN.Collection_Unit_Key 
			 			AND CUN.Preferred = 1
	WHERE 		SU.Parent_Collection_Collection_unit_key = @CollectionUnitKey
	ORDER BY 	Item_Name
END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SpecimensDetermined_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SpecimensDetermined_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SpecimensDetermined_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SpecimensDetermined_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SpecimensDetermined_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_SpecimensDetermined_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SpecimensDetermined_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SpecimensDetermined_Select TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_SpecimensCollected_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_SpecimensCollected_Select]
GO

/*===========================================================================*\
  Description:	Returns the list of all preferred determinations and the
		preferred number for the specimens linked to the currently
		selected collection. Returns the list of all determinations for 
		specimen top level node.

  Parameters:	@Key			Key of the event recorder person
		@CollectionUnitKey	Specimen or Collection Unit key
		@KeyIsSpecimen		Bit saying whether the key is a specimen

  Created:	October 2003

  Last revision information:
    $Revision: 1 $
    $Date: 13/03/13 8:46 $
    $Author: Alexanderpadley $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SpecimensCollected_Select]
	@Key char(16),
	@CollectionUnitKey char(16),
	@UserDomainMask int,
	@SessionID char(16),
	@ShowCommonNames bit,
	@KeyIsSpecimen bit
AS

SET NOCOUNT ON

IF @KeyIsSpecimen = 1
	SELECT DISTINCT
			SU.Collection_Unit_Key AS Item_Key,
			CASE WHEN SU.Life_Sciences = 0
			THEN CTP.Item_Name COLLATE SQL_Latin1_General_CP1_CI_AS
			ELSE dbo.ufn_GetFormattedTaxonNameByParams(
								Preferred_Name,
								0,
								Common_Name,
								0,
								NULL,
								@ShowCommonNames)
			END + CASE WHEN CUN.Number IS NULL THEN '' ELSE ' - ' + CUN.Number END AS Item_Name,
			CASE SU.Life_Sciences
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
			END AS NomenclaturalStatus
	FROM		Individual AS I
	INNER JOIN	Survey_Event_Recorder AS SER ON SER.Name_Key = I.Name_Key
	INNER JOIN	Sample_Recorder AS SR ON SR.SE_Recorder_Key = SER.SE_Recorder_Key
	INNER JOIN	[Sample] AS S ON S.Sample_Key = SR.Sample_Key
	LEFT JOIN	Occurrence AS O ON O.Sample_Key = S.Sample_Key
	LEFT JOIN	Taxon_Occurrence AS XO ON XO.Sample_Key = S.Sample_Key
	INNER JOIN	Specimen_Field_Data AS SFD ON (SFD.Occurrence_Key = O.Occurrence_Key
							OR SFD.Taxon_Occurrence_Key = XO.Taxon_Occurrence_Key)
						AND SFD.Gathering_Event = 1
	INNER JOIN	Specimen_Unit AS SU ON SU.Collection_Unit_Key = SFD.Collection_Unit_Key
					AND SU.Collection_Unit_Key = @CollectionUnitKey
	INNER JOIN	Collection_Unit AS CU ON CU.Collection_Unit_Key = SU.Collection_Unit_Key 
					AND ((CU.Domain_Mask & @UserDomainMask > 0) 
						OR (CU.Entered_Session_ID = @SessionID)
						OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
					
	LEFT JOIN	(Determination AS D
	INNER JOIN	VW_ConceptTermPreferred AS CTP ON CTP.Concept_Key = D.Concept_Key)
			ON D.Determination_Key = SU.Preferred_Determination_Key
	LEFT JOIN 	(Taxon_Determination AS TD 
				INNER JOIN 	Index_Taxon_Name ITN ON ITN.Taxon_List_Item_Key = TD.Taxon_List_Item_Key)
			ON TD.Taxon_Determination_Key = SU.Preferred_Taxon_Determination_Key  
	LEFT JOIN 	Collection_Unit_Number CUN ON SU.Collection_Unit_key = CUN.Collection_Unit_Key 
					 AND CUN.Preferred = 1
	
	WHERE		I.Name_Key = @Key
	ORDER BY	Item_Name
ELSE
	SELECT DISTINCT
			SU.Collection_Unit_Key AS Item_Key,
			CASE WHEN SU.Life_Sciences = 0
			THEN CTP.PlainText COLLATE SQL_Latin1_General_CP1_CI_AS
			ELSE dbo.ufn_GetFormattedTaxonNameByParams(
								Preferred_Name,
								0,
								Common_Name,
								0,
								NULL,
								@ShowCommonNames)
			END + CASE WHEN CUN.Number IS NULL THEN '' ELSE ' - ' + CUN.Number END AS Item_Name,
			CASE SU.Life_Sciences
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
			END AS NomenclaturalStatus
	FROM		Individual AS I
	INNER JOIN	Survey_Event_Recorder AS SER ON SER.Name_Key = I.Name_Key
	INNER JOIN	Sample_Recorder AS SR ON SR.SE_Recorder_Key = SER.SE_Recorder_Key
	INNER JOIN	[Sample] AS S ON S.Sample_Key = SR.Sample_Key
	LEFT JOIN	Occurrence AS O ON O.Sample_Key = S.Sample_Key
	LEFT JOIN	Taxon_Occurrence AS XO ON XO.Sample_Key = S.Sample_Key
	INNER JOIN	Specimen_Field_Data AS SFD ON (SFD.Occurrence_Key = O.Occurrence_Key
							OR SFD.Taxon_Occurrence_Key = XO.Taxon_Occurrence_Key)
						AND SFD.Gathering_Event = 1
	INNER JOIN	Specimen_Unit AS SU ON SU.Collection_Unit_Key = SFD.Collection_Unit_Key
					AND SU.Parent_Collection_Collection_Unit_Key = @CollectionUnitKey
	INNER JOIN	Collection_Unit AS CU ON CU.Collection_Unit_Key = SU.Collection_Unit_Key 
					AND ((CU.Domain_Mask & @UserDomainMask > 0) 
						OR (CU.Entered_Session_ID = @SessionID)
						OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
	LEFT JOIN	(Determination AS D
				INNER JOIN	VW_ConceptTermPreferred AS CTP ON CTP.Concept_Key = D.Concept_Key)
			ON D.Determination_Key = SU.Preferred_Determination_Key
	LEFT JOIN 	(Taxon_Determination AS TD 
				INNER JOIN 	Index_Taxon_Name ITN ON ITN.Taxon_List_Item_Key = TD.Taxon_List_Item_Key)
			ON TD.Taxon_Determination_Key = SU.Preferred_Taxon_Determination_Key  
	LEFT JOIN 	Collection_Unit_Number CUN ON SU.Collection_Unit_key = CUN.Collection_Unit_Key 
					 AND CUN.Preferred = 1
	
	WHERE		I.Name_Key = @Key
	ORDER BY	Item_Name
SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SpecimensCollected_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SpecimensCollected_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SpecimensCollected_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SpecimensCollected_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SpecimensCollected_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_SpecimensCollected_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SpecimensCollected_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SpecimensCollected_Select TO [Dev - JNCC SQL]
END

GO