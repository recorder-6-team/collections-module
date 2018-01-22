SET ANSI_NULLS ON
SET QUOTED_IDENTIFIER ON

SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
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
	$Revision: 5 $
	$Date: 11/03/10 16:54 $
	$Author: Simonwood $
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
		Hint			NVARCHAR(150) 	COLLATE SQL_Latin1_General_CP1_CI_AS NULL 
	)

	INSERT INTO @SpecimensSearch (Item_Key, Join_Key, Life_Sciences) 
	SELECT 	DISTINCT SU.Collection_Unit_Key, SU.Collection_Unit_Key, SU.Life_Sciences
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
								WHEN 1 THEN TDet.Item_Name
								ELSE TPref.Item_Name END,
			Det_Item_Name	= TDet.Plaintext
	FROM 	@SpecimensSearch 		SU
	JOIN 	VW_SpecimenDetsEarth 	SDE 	ON 	SDE.Collection_Unit_Key			= SU.Item_Key
											AND SDE.Preferred_Determination_Key	= SDE.Determination_Key
	JOIN 	Concept 				C 		ON 	C.Concept_Key					= SDE.Concept_Key
	JOIN 	Term 					TDet 	ON 	TDet.Term_Key					= C.Term_Key
	JOIN 	Concept 				CPref 	ON 	CPref.Meaning_Key				= C.Meaning_Key
											AND CPref.List_Preferred			= 1
											AND CPref.Concept_Group_Key			= C.Concept_Group_Key
	JOIN 	Term 					TPref 	ON 	TPref.Term_Key					= CPref.Term_Key

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

SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

/*============================================================================*\
  Drop stored proc before re-creating.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_Specimens_Select_ForTopLevel') IS NOT NULL
	DROP PROCEDURE dbo.usp_Specimens_Select_ForTopLevel
GO

/*============================================================================*\
  Description:	Returns top level specimens' data to the Collections Browser.

  Parameters:	@UserDomainMask			User's Domain Mask restricting which
										records may be returned
				@SessionID 				User's SessionID
				@ShowCommonNames		Specifies whether Common Names are shown
				@ShowOriginalSpecimenNames
										0 => preferred names are shown
										1 => names originally entered are shown
				@SortOrderIndex			0 => results sorted by Item_Name, Number
										1 => results sorted by Number, Item_Name
				@Key 					[Optional] If specified, identifies
										a specimen which will be the only record
										returned.
  Created:
	August 2003

  Last revision information:
	$Revision: 5 $
	$Date: 11/03/10 16:54 $
	$Author: Simonwood $
\*============================================================================*/
CREATE PROCEDURE dbo.usp_Specimens_Select_ForTopLevel
	@UserDomainMask						BIGINT,
	@SessionID							CHAR(16),
	@ShowCommonNames					BIT,
	@ShowOriginalSpecimenNames			BIT,
	@SortOrderIndex						TINYINT,
	@Key								CHAR(16) = NULL
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
	DECLARE @Search TABLE
	(
		[Item_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[Det_Item_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[Det_Item_Name] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[Item_Name] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[Life_Sciences] [bit] NULL,
		[Number] [varchar] (30) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[Hint] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL 
	)

	IF @Key IS NULL
		IF object_id('tempdb..#TempFilter') IS NOT NULL
			-- Display data for a list of keys in the #TempFilter table
			INSERT INTO @Search (Item_Key, Life_Sciences) 
			SELECT DISTINCT SU.Collection_Unit_Key, SU.Life_Sciences
			FROM 		Specimen_Unit 	SU
			INNER JOIN 	Collection_Unit CU 	ON 	SU.Collection_Unit_Key = CU.Collection_Unit_Key 
											AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
											OR 	(CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
			INNER JOIN 	#TempFilter 		ON 	#TempFilter.ItemKey=CU.Collection_Unit_Key
		ELSE
			INSERT INTO @Search (Item_Key, Life_Sciences) 
			SELECT DISTINCT SU.Collection_Unit_Key, SU.Life_Sciences
			FROM 		Specimen_Unit 	SU
			INNER JOIN 	Collection_Unit CU 	ON 	SU.Collection_Unit_Key = CU.Collection_Unit_Key 
											AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
											OR 	(CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
	ELSE
		-- Display data for a single key
		INSERT INTO @Search (Item_Key, Life_Sciences) 
		SELECT DISTINCT SU.Collection_Unit_Key, SU.Life_Sciences
		FROM 		Specimen_Unit 	SU
		INNER JOIN 	Collection_Unit CU 	ON 	SU.Collection_Unit_Key = CU.Collection_Unit_Key 
										AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
										OR 	(CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
		WHERE SU.Collection_Unit_Key = @Key

	UPDATE		@Search
	SET			Number = CUN.Number
	FROM 		@Search 				SU
	LEFT JOIN 	Collection_Unit_Number 	CUN ON 	SU.Item_key 	= CUN.Collection_Unit_Key 
											AND CUN.Preferred	= 1

	UPDATE 	@Search
	SET		Det_Item_Key = CPref.Concept_Key,
			Item_Name = CASE @ShowOriginalSpecimenNames
							WHEN 1 THEN TDet.Item_Name
							ELSE TPref.Item_Name END,
			Det_Item_Name = TDet.Plaintext
	FROM @Search SU
	INNER JOIN VW_SpecimenDetsEarth SDE ON SDE.Collection_Unit_Key = SU.Item_Key
			AND SDE.Preferred_Determination_key	= SDE.Determination_Key
	INNER JOIN Concept C ON C.Concept_Key=SDE.Concept_Key
	INNER JOIN Term TDet ON TDet.Term_Key=C.Term_Key
	INNER JOIN Concept CPref ON CPref.Meaning_Key=C.Meaning_Key
			AND CPref.List_Preferred=1
			AND CPref.Concept_Group_Key=C.Concept_Group_Key
	INNER JOIN Term TPref ON TPref.Term_Key=CPref.Term_Key

	UPDATE @Search
	SET		Det_Item_Key=SDL.Taxon_List_Item_Key,
			Item_Name =
						dbo.ufn_GetFormattedTaxonNameByParams(
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
			Det_Item_Name=ITN.Actual_Name
	FROM @Search SU
	INNER JOIN VW_SpecimenDetsLife SDL ON SDL.Collection_Unit_Key=SU.Item_Key
		AND SDL.Preferred_Taxon_Determination_key=SDL.Taxon_Determination_Key
	INNER JOIN Index_Taxon_Name ITN	ON ITN.Taxon_List_Item_Key=SDL.Taxon_List_Item_Key

	-- Select table and sort appropriately
	IF @SortOrderIndex = 0
		SELECT * FROM @Search
		ORDER BY Det_Item_Name, Number
	ELSE IF @SortOrderIndex = 1
		SELECT * FROM @Search
		ORDER BY Number, Item_Name
GO

/*============================================================================*\
  Grant permissions.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_Specimens_Select_ForTopLevel') IS NOT NULL
BEGIN
	PRINT 'Setting up security on procedure usp_Specimens_Select_ForTopLevel'
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForTopLevel TO R2k_AddOnly
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForTopLevel TO R2k_Administrator
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForTopLevel TO R2k_FullEdit
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForTopLevel TO R2k_ReadOnly
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForTopLevel TO R2k_RecordCardsOnly
END
GO

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
	$Revision: 5 $
	$Date: 11/03/10 16:54 $
	$Author: Simonwood $
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
		Hint			NVARCHAR(150) 	COLLATE SQL_Latin1_General_CP1_CI_AS NULL 
	)

	INSERT INTO	@SpecimensSearch (Item_Key, Join_Key, Life_Sciences) 
	SELECT 	DISTINCT SU.Collection_Unit_Key, SU.Collection_Unit_Key, SU.Life_Sciences
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
								WHEN 1 THEN TDet.Item_Name
								ELSE TPref.Item_Name END,
			Det_Item_Name	= TDet.Plaintext
	FROM 	@SpecimensSearch 		SU
	JOIN 	VW_SpecimenDetsEarth 	SDE 	ON 	SDE.Collection_Unit_Key			= SU.Item_Key
											AND SDE.Preferred_Determination_Key	= SDE.Determination_Key
	JOIN 	Concept 				C 		ON 	C.Concept_Key					= SDE.Concept_Key
	JOIN 	Term 					TDet 	ON 	TDet.Term_Key					= C.Term_Key
	JOIN 	Concept 				CPref 	ON 	CPref.Meaning_Key				= C.Meaning_Key
											AND CPref.List_Preferred			= 1
											AND CPref.Concept_Group_Key			= C.Concept_Group_Key
	JOIN 	Term 					TPref 	ON 	TPref.Term_Key					= CPref.Term_Key

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

SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
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
	$Revision: 5 $
	$Date: 11/03/10 16:54 $
	$Author: Simonwood $
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
		Hint			NVARCHAR(150) 	COLLATE SQL_Latin1_General_CP1_CI_AS NULL 
	)
	
	INSERT INTO	@SpecimensSearch (
				Item_Key,
				Join_Key,
				Life_Sciences)
	SELECT		s.Collection_Unit_Key,
				q.Collection_Unit_Enquiry_Key,
				s.Life_Sciences
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
								WHEN 1 THEN TDet.Item_Name
								ELSE TPref.Item_Name END,
			Det_Item_Name	= TDet.Plaintext
	FROM 	@SpecimensSearch 		SU
	JOIN 	VW_SpecimenDetsEarth 	SDE 	ON 	SDE.Collection_Unit_Key			= SU.Item_Key
											AND SDE.Preferred_Determination_Key	= SDE.Determination_Key
	JOIN 	Concept 				C 		ON 	C.Concept_Key					= SDE.Concept_Key
	JOIN 	Term 					TDet 	ON 	TDet.Term_Key					= C.Term_Key
	JOIN 	Concept 				CPref 	ON 	CPref.Meaning_Key				= C.Meaning_Key
											AND CPref.List_Preferred			= 1
											AND CPref.Concept_Group_Key			= C.Concept_Group_Key
	JOIN 	Term 					TPref 	ON 	TPref.Term_Key					= CPref.Term_Key

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
    $Revision: 5 $
    $Date: 11/03/10 16:54 $
    $Author: Simonwood $
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
		Hint			NVARCHAR(150) 	COLLATE SQL_Latin1_General_CP1_CI_AS NULL 
	)

	INSERT INTO	@SpecimensSearch (
				Item_Key,
				Join_Key,
				Life_Sciences)
	SELECT		s.Collection_Unit_Key,
				v.Collection_Unit_Valuation_Key,
				s.Life_Sciences
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
								WHEN 1 THEN TDet.Item_Name
								ELSE TPref.Item_Name END,
			Det_Item_Name	= TDet.Plaintext
	FROM 	@SpecimensSearch 		SU
	JOIN 	VW_SpecimenDetsEarth 	SDE 	ON 	SDE.Collection_Unit_Key			= SU.Item_Key
											AND SDE.Preferred_Determination_Key	= SDE.Determination_Key
	JOIN 	Concept 				C 		ON 	C.Concept_Key					= SDE.Concept_Key
	JOIN 	Term 					TDet 	ON 	TDet.Term_Key					= C.Term_Key
	JOIN 	Concept 				CPref 	ON 	CPref.Meaning_Key				= C.Meaning_Key
											AND CPref.List_Preferred			= 1
											AND CPref.Concept_Group_Key			= C.Concept_Group_Key
	JOIN 	Term 					TPref 	ON 	TPref.Term_Key					= CPref.Term_Key

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

SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
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
	$Revision: 5 $
	$Date: 11/03/10 16:54 $
	$Author: Simonwood $
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
		Hint			NVARCHAR(150) 	COLLATE SQL_Latin1_General_CP1_CI_AS NULL 
	)
	
	INSERT INTO	@SpecimensSearch (
				Item_Key,
				Join_Key,
				Life_Sciences)
	SELECT		s.Collection_Unit_Key,
				mcu.Movement_Collection_Unit_Key,
				s.Life_Sciences
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
								WHEN 1 THEN TDet.Item_Name
								ELSE TPref.Item_Name END,
			Det_Item_Name	= TDet.Plaintext
	FROM 	@SpecimensSearch 		SU
	JOIN 	VW_SpecimenDetsEarth 	SDE 	ON 	SDE.Collection_Unit_Key			= SU.Item_Key
											AND SDE.Preferred_Determination_Key	= SDE.Determination_Key
	JOIN 	Concept 				C 		ON 	C.Concept_Key					= SDE.Concept_Key
	JOIN 	Term 					TDet 	ON 	TDet.Term_Key					= C.Term_Key
	JOIN 	Concept 				CPref 	ON 	CPref.Meaning_Key				= C.Meaning_Key
											AND CPref.List_Preferred			= 1
											AND CPref.Concept_Group_Key			= C.Concept_Group_Key
	JOIN 	Term 					TPref 	ON 	TPref.Term_Key					= CPref.Term_Key

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

SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
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
    $Revision: 5 $
    $Date: 11/03/10 16:54 $
    $Author: Simonwood $
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
		Hint			NVARCHAR(150) 	COLLATE SQL_Latin1_General_CP1_CI_AS NULL 
	)
	
	INSERT INTO	@SpecimensSearch (
				Item_Key,
				Join_Key,
				Life_Sciences)
	SELECT		s.Collection_Unit_Key,
				mcu.Movement_Collection_Unit_Key,
				s.Life_Sciences
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
								WHEN 1 THEN TDet.Item_Name
								ELSE TPref.Item_Name END,
			Det_Item_Name	= TDet.Plaintext
	FROM 	@SpecimensSearch 		SU
	JOIN 	VW_SpecimenDetsEarth 	SDE 	ON 	SDE.Collection_Unit_Key			= SU.Item_Key
											AND SDE.Preferred_Determination_Key	= SDE.Determination_Key
	JOIN 	Concept 				C 		ON 	C.Concept_Key					= SDE.Concept_Key
	JOIN 	Term 					TDet 	ON 	TDet.Term_Key					= C.Term_Key
	JOIN 	Concept 				CPref 	ON 	CPref.Meaning_Key				= C.Meaning_Key
											AND CPref.List_Preferred			= 1
											AND CPref.Concept_Group_Key			= C.Concept_Group_Key
	JOIN 	Term 					TPref 	ON 	TPref.Term_Key					= CPref.Term_Key

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

SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
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
    $Revision: 5 $
    $Date: 11/03/10 16:54 $
    $Author: Simonwood $
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
		Hint			NVARCHAR(150) 	COLLATE SQL_Latin1_General_CP1_CI_AS NULL 
	)
	
	INSERT INTO	@SpecimensSearch (
				Item_Key,
				Join_Key,
				Life_Sciences)
	SELECT		s.Collection_Unit_Key,
				mcu.Movement_Collection_Unit_Key,
				s.Life_Sciences
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
								WHEN 1 THEN TDet.Item_Name
								ELSE TPref.Item_Name END,
			Det_Item_Name	= TDet.Plaintext
	FROM 	@SpecimensSearch 		SU
	JOIN 	VW_SpecimenDetsEarth 	SDE 	ON 	SDE.Collection_Unit_Key			= SU.Item_Key
											AND SDE.Preferred_Determination_Key	= SDE.Determination_Key
	JOIN 	Concept 				C 		ON 	C.Concept_Key					= SDE.Concept_Key
	JOIN 	Term 					TDet 	ON 	TDet.Term_Key					= C.Term_Key
	JOIN 	Concept 				CPref 	ON 	CPref.Meaning_Key				= C.Meaning_Key
											AND CPref.List_Preferred			= 1
											AND CPref.Concept_Group_Key			= C.Concept_Group_Key
	JOIN 	Term 					TPref 	ON 	TPref.Term_Key					= CPref.Term_Key

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

SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
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
    $Revision: 5 $
    $Date: 11/03/10 16:54 $
    $Author: Simonwood $
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
		Hint			NVARCHAR(150) 	COLLATE SQL_Latin1_General_CP1_CI_AS NULL 
	)
	
	INSERT INTO	@SpecimensSearch (
				Item_Key,
				Join_Key,
				Life_Sciences)
	SELECT		s.Collection_Unit_Key,
				k.Collection_Unit_Check_Key,
				s.Life_Sciences
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
								WHEN 1 THEN TDet.Item_Name
								ELSE TPref.Item_Name END,
			Det_Item_Name	= TDet.Plaintext
	FROM 	@SpecimensSearch 		SU
	JOIN 	VW_SpecimenDetsEarth 	SDE 	ON 	SDE.Collection_Unit_Key			= SU.Item_Key
											AND SDE.Preferred_Determination_Key	= SDE.Determination_Key
	JOIN 	Concept 				C 		ON 	C.Concept_Key					= SDE.Concept_Key
	JOIN 	Term 					TDet 	ON 	TDet.Term_Key					= C.Term_Key
	JOIN 	Concept 				CPref 	ON 	CPref.Meaning_Key				= C.Meaning_Key
											AND CPref.List_Preferred			= 1
											AND CPref.Concept_Group_Key			= C.Concept_Group_Key
	JOIN 	Term 					TPref 	ON 	TPref.Term_Key					= CPref.Term_Key

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

SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
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
    $Revision: 5 $
    $Date: 11/03/10 16:54 $
    $Author: Simonwood $
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
		Number				VARCHAR(30) 	COLLATE SQL_Latin1_General_CP1_CI_AS NULL 
	)
	
	INSERT INTO	@SpecimensSearch (
				Item_Key,
				Join_Key,
				Hyperlink_Item_Key,
				Drag_Drop_Item_Key,
				Life_Sciences)
	SELECT		cr.Collection_Unit_Relation_Key,
				cr.Collection_Unit_Relation_Key,
				s.Collection_Unit_Key,
				s.Collection_Unit_Key,
				s.Life_Sciences
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
								WHEN 1 THEN TDet.Item_Name
								ELSE TPref.Item_Name END
	FROM 	@SpecimensSearch 		SU
	JOIN 	VW_SpecimenDetsEarth 	SDE 	ON 	SDE.Collection_Unit_Key			= SU.Hyperlink_Item_Key
											AND SDE.Preferred_Determination_Key	= SDE.Determination_Key
	JOIN 	Concept 				C 		ON 	C.Concept_Key					= SDE.Concept_Key
	JOIN 	Term 					TDet 	ON 	TDet.Term_Key					= C.Term_Key
	JOIN 	Concept 				CPref 	ON 	CPref.Meaning_Key				= C.Meaning_Key
											AND CPref.List_Preferred			= 1
											AND CPref.Concept_Group_Key			= C.Concept_Group_Key
	JOIN 	Term 					TPref 	ON 	TPref.Term_Key					= CPref.Term_Key

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

SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
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
    $Revision: 5 $
    $Date: 11/03/10 16:54 $
    $Author: Simonwood $
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
		Number			VARCHAR(30) 	COLLATE SQL_Latin1_General_CP1_CI_AS NULL 
	)
	
	INSERT INTO	@SpecimensSearch (
				Item_Key,
				Join_Key,
				Life_Sciences)
	SELECT		s.Collection_Unit_Key,
				t.Collection_Unit_Task_Key,
				s.Life_Sciences
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
								WHEN 1 THEN TDet.Item_Name
								ELSE TPref.Item_Name END,
			PlainText		= TDet.Plaintext
	FROM 	@SpecimensSearch 		SU
	JOIN 	VW_SpecimenDetsEarth 	SDE 	ON 	SDE.Collection_Unit_Key			= SU.Item_Key
											AND SDE.Preferred_Determination_Key	= SDE.Determination_Key
	JOIN 	Concept 				C 		ON 	C.Concept_Key					= SDE.Concept_Key
	JOIN 	Term 					TDet 	ON 	TDet.Term_Key					= C.Term_Key
	JOIN 	Concept 				CPref 	ON 	CPref.Meaning_Key				= C.Meaning_Key
											AND CPref.List_Preferred			= 1
											AND CPref.Concept_Group_Key			= C.Concept_Group_Key
	JOIN 	Term 					TPref 	ON 	TPref.Term_Key					= CPref.Term_Key

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

SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

/*============================================================================*\
  Drop stored proc before re-creating.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_Specimens_Select_ForSearchByAnyDetermination') IS NOT NULL
	DROP PROCEDURE dbo.usp_Specimens_Select_ForSearchByAnyDetermination
GO

/*============================================================================*\
  Description:	Returns specimens based on any of their determinations.

  Parameters:
				@UserDomainMask			User's Domain Mask restricting which
										records may be returned
				@SessionID 				User's SessionID
				@ShowCommonNames		Specifies whether Common Names are shown
				@ShowOriginalSpecimenNames
										0 => preferred names are shown
										1 => names originally entered are shown
				@SearchText				Text to search for
				@SortOrderIndex			0 => results sorted by Item_Name, Number
										1 => results sorted by Number, Item_Name

  Created:		October 2003

  Last revision information:
    $Revision: 5 $
    $Date: 11/03/10 16:54 $
    $Author: Simonwood $
\*============================================================================*/
CREATE PROCEDURE dbo.usp_Specimens_Select_ForSearchByAnyDetermination
	@UserDomainMask						INT,
	@SessionID							CHAR(16),
	@ShowCommonNames					BIT,
	@ShowOriginalSpecimenNames			BIT,
	@SearchText							VARCHAR(150),
	@SortOrderIndex						TINYINT
AS
	SET NOCOUNT ON

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

	--Find all specimens with a determination match
	INSERT INTO @SpecimensSearch (Item_Key, Life_Sciences) 
	SELECT DISTINCT SU.Collection_Unit_Key, SU.Life_Sciences
	FROM 	SPECIMEN_UNIT SU
	JOIN 	Collection_Unit CU ON SU.Collection_Unit_Key = CU.Collection_Unit_Key 
		 	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
			OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
	JOIN 	VW_SpecimenDetsEarth SDE ON SU.Collection_Unit_Key = SDE.Collection_Unit_Key
	JOIN 	Concept C ON SDE.Concept_Key = C.Concept_Key
	JOIN 	Concept CSearch ON CSearch.Meaning_Key=C.Meaning_Key
	JOIN 	Term TSearch ON TSearch.Term_Key=CSearch.Term_Key
	WHERE 	(TSearch.Plaintext LIKE @SearchText + '%' AND SU.Life_Sciences=0) 

	INSERT INTO @SpecimensSearch (Item_Key, Life_Sciences) 
	SELECT DISTINCT SU.Collection_Unit_Key, SU.Life_Sciences
	FROM 	SPECIMEN_UNIT SU
	JOIN 	Collection_Unit CU ON SU.Collection_Unit_Key = CU.Collection_Unit_Key 
	 		AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
			OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
	JOIN 	VW_SpecimenDetsLife SDL ON SU.Collection_Unit_Key = SDL.Collection_Unit_Key
	JOIN 	Index_Taxon_Synonym ITS ON ITS.Taxon_List_Item_Key=SDL.Taxon_List_Item_Key
	JOIN 	INDEX_TAXON_NAME ITN	ON ITS.Synonym_List_Item_Key = ITN.Taxon_List_Item_Key
	WHERE 	(ITN.Actual_Name LIKE @SearchText + '%' AND SU.Life_Sciences=1)

	UPDATE 		@SpecimensSearch
	SET			Number = CUN.Number
	FROM 		@SpecimensSearch 		SU
	LEFT JOIN 	Collection_Unit_Number 	CUN ON 	SU.Item_key 	= CUN.Collection_Unit_Key 
											AND CUN.Preferred	= 1

	UPDATE @SpecimensSearch
	SET	Det_Item_Key=CPref.Concept_Key,
		Item_Name = CASE @ShowOriginalSpecimenNames
						WHEN 1 THEN TDet.Item_Name
						ELSE TPref.Item_Name END,
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
	INNER JOIN Term TPref ON TPref.Term_Key=CPref.Term_Key

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
				null,
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

/*============================================================================*\
  Grant permissions.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_Specimens_Select_ForSearchByAnyDetermination') IS NOT NULL
BEGIN
	PRINT 'Setting up security on procedure usp_Specimens_Select_ForSearchByAnyDetermination'
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByAnyDetermination TO R2k_AddOnly
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByAnyDetermination TO R2k_Administrator
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByAnyDetermination TO R2k_FullEdit
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByAnyDetermination TO R2k_ReadOnly
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByAnyDetermination TO R2k_RecordCardsOnly
END
GO

SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

/*============================================================================*\
  Drop stored proc before re-creating.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_Specimens_Select_ForSearchByType') IS NOT NULL
	DROP PROCEDURE dbo.usp_Specimens_Select_ForSearchByType
GO

/*============================================================================*\
  Description:	Returns specimens based on their specimen type.

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

  Created:		October 2003

  Last revision information:
    $Revision: 5 $
    $Date: 11/03/10 16:54 $
    $Author: Simonwood $
\*============================================================================*/
CREATE PROCEDURE dbo.usp_Specimens_Select_ForSearchByType
	@UserDomainMask						INT,
	@SessionID							CHAR(16),
	@ShowCommonNames					BIT,
	@ShowOriginalSpecimenNames			BIT,
	@SearchText							VARCHAR(150),
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

	INSERT INTO @SpecimensSearch (Item_Key, Life_Sciences, Hint) 
	SELECT DISTINCT SU.Collection_Unit_Key, SU.Life_Sciences, T.Plaintext
	FROM 	SPECIMEN_UNIT SU
	JOIN 	Collection_Unit CU ON SU.Collection_Unit_Key = CU.Collection_Unit_Key 
	 		AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
			OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
	JOIN 	Concept C ON C.Concept_Key = SU.Specimen_Type_Concept_Key
	JOIN 	Concept CSyn ON CSyn.Meaning_Key=C.Meaning_Key
	JOIN 	Term T ON T.Term_Key=CSyn.Term_Key
	WHERE 	T.Plaintext LIKE @Searchtext + '%'

	UPDATE		@SpecimensSearch
	SET			Number = CUN.Number
	FROM 		@SpecimensSearch 		SU
	LEFT JOIN 	Collection_Unit_Number 	CUN ON 	SU.Item_key 	= CUN.Collection_Unit_Key 
											AND CUN.Preferred	= 1

	UPDATE @SpecimensSearch
	SET	Det_Item_Key=CPref.Concept_Key,
		Item_Name = CASE @ShowOriginalSpecimenNames
						WHEN 1 THEN TDet.Item_Name
						ELSE TPref.Item_Name END,
		Det_Item_Name=TDet.Plaintext
	FROM @SpecimensSearch SU
	INNER JOIN VW_SpecimenDetsEarth SDE ON SDE.Collection_Unit_Key=SU.Item_Key
		AND SDE.Preferred_Determination_Key=SDE.Determination_Key
	INNER JOIN Concept C ON C.Concept_Key=SDE.Concept_Key
	INNER JOIN Term TDet ON TDet.Term_Key=C.Term_Key
	INNER JOIN Concept CPref ON CPref.Meaning_Key=C.Meaning_Key
		AND CPref.List_Preferred=1
		AND CPref.Concept_Group_Key=C.Concept_Group_Key
	INNER JOIN Term TPref ON TPref.Term_Key=CPref.Term_Key

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
				null,
				@ShowCommonNames),
		Det_Item_Name=ITN.Actual_Name
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

/*============================================================================*\
  Grant permissions.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_Specimens_Select_ForSearchByType') IS NOT NULL
BEGIN
	PRINT 'Setting up security on procedure usp_Specimens_Select_ForSearchByType'
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByType TO R2k_AddOnly
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByType TO R2k_Administrator
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByType TO R2k_FullEdit
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByType TO R2k_ReadOnly
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByType TO R2k_RecordCardsOnly
END
GO

SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

/*============================================================================*\
  Drop stored proc before re-creating.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_Specimens_Select_ForSearchByPreferredDetermination') IS NOT NULL
	DROP PROCEDURE dbo.usp_Specimens_Select_ForSearchByPreferredDetermination
GO

/*============================================================================*\
  Description:	Returns specimens based on their preferred determinations.

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

  Created:		October 2003

  Last revision information:
    $Revision: 5 $
    $Date: 11/03/10 16:54 $
    $Author: Simonwood $
\*============================================================================*/
CREATE PROCEDURE dbo.usp_Specimens_Select_ForSearchByPreferredDetermination
	@UserDomainMask						INT,
	@SessionID							CHAR(16),
	@ShowCommonNames					BIT,
	@ShowOriginalSpecimenNames			BIT,
	@SearchText							VARCHAR(150),
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

	--Find all specimens with a determination match
	INSERT INTO @SpecimensSearch (Item_Key, Life_Sciences) 
	SELECT DISTINCT SU.Collection_Unit_Key, SU.Life_Sciences
	FROM 	SPECIMEN_UNIT SU
	JOIN 	Collection_Unit CU ON SU.Collection_Unit_Key = CU.Collection_Unit_Key 
	 		AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
			OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
	JOIN 	VW_SpecimenDetsEarth SDE ON (SU.Collection_Unit_Key = SDE.Collection_Unit_Key)
			AND (SDE.Preferred_Determination_Key=SDE.Determination_Key)
	JOIN 	Concept C ON SDE.Concept_Key = C.Concept_Key
	JOIN 	Concept CSearch ON CSearch.Meaning_Key=C.Meaning_Key
	JOIN 	Term TSearch ON TSearch.Term_Key=CSearch.Term_Key
	WHERE (TSearch.Plaintext LIKE @SearchText + '%' AND SU.Life_Sciences=0) 

	INSERT INTO @SpecimensSearch (Item_Key, Life_Sciences) 
	SELECT DISTINCT SU.Collection_Unit_Key, SU.Life_Sciences
	FROM 	SPECIMEN_UNIT SU
	JOIN 	Collection_Unit CU ON SU.Collection_Unit_Key = CU.Collection_Unit_Key 
	 		AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
			OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
	JOIN 	VW_SpecimenDetsLife SDL ON SU.Collection_Unit_Key = SDL.Collection_Unit_Key
			AND (SDL.Preferred_Taxon_Determination_Key=SDL.Taxon_Determination_Key)
	JOIN 	Index_Taxon_Synonym ITS ON ITS.Taxon_List_Item_Key=SDL.Taxon_List_Item_Key
	JOIN 	INDEX_TAXON_NAME ITN	ON ITS.Synonym_List_Item_Key = ITN.Taxon_List_Item_Key
	WHERE 	(ITN.Actual_Name LIKE @SearchText + '%' AND SU.Life_Sciences=1)

	UPDATE		@SpecimensSearch
	SET			Number = CUN.Number
	FROM 		@SpecimensSearch 		SU
	LEFT JOIN 	Collection_Unit_Number 	CUN ON 	SU.Item_key 	= CUN.Collection_Unit_Key 
											AND CUN.Preferred	= 1

	UPDATE @SpecimensSearch
	SET	Det_Item_Key=CPref.Concept_Key,
		Item_Name = CASE @ShowOriginalSpecimenNames
						WHEN 1 THEN TDet.Item_Name
						ELSE TPref.Item_Name END,
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
	INNER JOIN Term TPref ON TPref.Term_Key=CPref.Term_Key

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
				null,
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

/*============================================================================*\
  Grant permissions.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_Specimens_Select_ForSearchByPreferredDetermination') IS NOT NULL
BEGIN
	PRINT 'Setting up security on procedure usp_Specimens_Select_ForSearchByPreferredDetermination'
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByPreferredDetermination TO R2k_AddOnly
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByPreferredDetermination TO R2k_Administrator
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByPreferredDetermination TO R2k_FullEdit
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByPreferredDetermination TO R2k_ReadOnly
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByPreferredDetermination TO R2k_RecordCardsOnly
END
GO

SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

/*============================================================================*\
  Drop stored proc before re-creating.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_Specimens_Select_ForSearchByDeterminationInGroup') IS NOT NULL
	DROP PROCEDURE dbo.usp_Specimens_Select_ForSearchByDeterminationInGroup
GO

/*============================================================================*\
  Description:	Returns specimens that have determinations whose concepts are
				hierarchically contained by concepts that match the search text.

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

  Created:		October 2003

  Last revision information:
    $Revision: 5 $
    $Date: 11/03/10 16:54 $
    $Author: Simonwood $
\*============================================================================*/
CREATE PROCEDURE dbo.usp_Specimens_Select_ForSearchByDeterminationInGroup
	@UserDomainMask						INT,
	@SessionID							CHAR(16),
	@ShowCommonNames					BIT,
	@ShowOriginalSpecimenNames			BIT,
	@SearchText							VARCHAR(150),
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

	--Create a temp table to hold the meanings of the contents of the groups that match the search
	DECLARE @SearchLineage TABLE (
		Child_Meaning_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS,
		PlainText varchar(150) COLLATE SQL_Latin1_General_CP1_CI_AS
	)

	INSERT INTO 	@SearchLineage
	SELECT DISTINCT CChild.Meaning_Key, CSearch.PlainText
	FROM 		vw_ConceptTerm CSearch
	INNER JOIN 	Concept_Group CG ON CG.Concept_Group_Key = CSearch.Concept_Group_Key
	INNER JOIN 	Local_Domain LD ON LD.Local_Domain_Key = CG.Local_Domain_Key
	INNER JOIN 	Domain D ON D.Domain_Key = LD.Domain_Key AND D.Has_Occurrences = 1
	INNER JOIN 	Concept CSynSearch ON CSynSearch.Meaning_Key = CSearch.Meaning_Key
	INNER JOIN 	Concept_Lineage CL ON CL.Concept_Key = CSynSearch.Concept_Key
	INNER JOIN 	Concept_Lineage CLChild ON CLChild.Lineage LIKE CL.Lineage + '\%'
	INNER JOIN 	Concept CChild ON CChild.Concept_Key = CLChild.Concept_Key AND (CChild.Concept_Group_Key = CSynSearch.Concept_Group_Key)
	WHERE 		CSearch.Plaintext LIKE @SearchText + '%'

	INSERT INTO 	@SpecimensSearch (Item_Key, Life_Sciences, Hint) 
	SELECT DISTINCT SDE.Collection_Unit_Key, 0 AS Life_Sciences, SL.Plaintext
	FROM 		@SearchLineage SL
	INNER JOIN 	vw_ConceptTerm CChildSyn ON CChildSyn.Meaning_Key = SL.Child_Meaning_Key
	INNER JOIN 	vw_SpecimenDetsEarth SDE ON SDE.Concept_Key = CChildSyn.Concept_Key
	INNER JOIN 	Collection_Unit CU ON CU.Collection_Unit_Key = SDE.Collection_Unit_Key	
	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
	
	INSERT INTO 	@SpecimensSearch (Item_Key, Life_Sciences, Hint) 
	SELECT DISTINCT SDL.Collection_Unit_Key AS Item_Key, 1 AS Life_Sciences, ITNSearch.Actual_Name
	FROM 		Index_Taxon_Name ITNSearch
	INNER JOIN 	Index_Taxon_Synonym ITSSearch ON ITSSearch.Taxon_List_Item_Key = ITNSearch.Taxon_List_Item_Key
	INNER JOIN 	Index_Taxon_Group ITG ON ITG.Taxon_List_Item_Key = ITSSearch.Synonym_List_Item_Key
	INNER JOIN 	Index_Taxon_Synonym ITSSyn ON ITSSyn.Taxon_List_Item_Key = ITG.Contained_List_Item_Key
	INNER JOIN 	Index_Taxon_Name ITNSyn ON ITNSyn.Taxon_List_Item_Key = ITSSyn.Synonym_List_Item_Key
	INNER JOIN 	vw_SpecimenDetsLife SDL ON SDL.Taxon_List_Item_Key = ITNSyn.Taxon_List_Item_Key
	INNER JOIN 	Collection_Unit CU ON CU.Collection_Unit_Key = SDL.Collection_Unit_Key
		AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
	WHERE 		ITNSearch.Actual_Name LIKE @SearchText + '%'

	UPDATE		@SpecimensSearch
	SET			Number = CUN.Number
	FROM 		@SpecimensSearch 		SU
	LEFT JOIN 	Collection_Unit_Number 	CUN ON 	SU.Item_key 	= CUN.Collection_Unit_Key 
											AND CUN.Preferred	= 1

	UPDATE 	@SpecimensSearch
	SET	Det_Item_Key=CPref.Concept_Key,
		Item_Name = CASE @ShowOriginalSpecimenNames
						WHEN 1 THEN TDet.Item_Name
						ELSE TPref.Item_Name END,
		Det_Item_Name=TDet.Plaintext 
	FROM 		@SpecimensSearch SU
	INNER JOIN 	VW_SpecimenDetsEarth SDE ON SDE.Collection_Unit_Key=SU.Item_Key
		AND SDE.Preferred_Determination_Key=SDE.Determination_Key
	INNER JOIN 	Concept C ON C.Concept_Key=SDE.Concept_Key
	INNER JOIN 	Term TDet ON TDet.Term_Key=C.Term_Key
	INNER JOIN 	Concept CPref ON CPref.Meaning_Key = C.Meaning_Key AND CPref.List_Preferred = 1 AND CPref.Concept_Group_Key = C.Concept_Group_Key
	INNER JOIN 	Term TPref ON TPref.Term_Key = CPref.Term_Key

	UPDATE 	@SpecimensSearch
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
		Det_Item_Name=ITN.Actual_Name
	FROM 		@SpecimensSearch SU
	INNER JOIN 	VW_SpecimenDetsLife SDL ON SU.Item_Key = SDL.Collection_Unit_Key
	INNER JOIN 	Index_Taxon_Name ITN	ON ITN.Taxon_List_Item_Key=SDL.Taxon_List_Item_Key

	-- Select table and sort appropriately
	IF @SortOrderIndex = 0
		SELECT * FROM @SpecimensSearch
		ORDER BY Item_Name, Number
	ELSE IF @SortOrderIndex = 1
		SELECT * FROM @SpecimensSearch
		ORDER BY Number, Item_Name
GO

/*============================================================================*\
  Grant permissions.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_Specimens_Select_ForSearchByDeterminationInGroup') IS NOT NULL
BEGIN
	PRINT 'Setting up security on procedure usp_Specimens_Select_ForSearchByDeterminationInGroup'
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByDeterminationInGroup TO R2k_AddOnly
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByDeterminationInGroup TO R2k_Administrator
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByDeterminationInGroup TO R2k_FullEdit
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByDeterminationInGroup TO R2k_ReadOnly
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByDeterminationInGroup TO R2k_RecordCardsOnly
END
GO

SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

/*============================================================================*\
  Drop stored proc before re-creating.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_Specimens_Select_ForSearchByPreferredNumber') IS NOT NULL
	DROP PROCEDURE dbo.usp_Specimens_Select_ForSearchByPreferredNumber
GO

/*============================================================================*\
  Description:	Returns specimens based on their preferred number.

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

  Created:		October 2003

  Last revision information:
    $Revision: 5 $
    $Date: 11/03/10 16:54 $
    $Author: Simonwood $
\*============================================================================*/
CREATE PROCEDURE dbo.usp_Specimens_Select_ForSearchByPreferredNumber
	@UserDomainMask						INT,
	@SessionID							CHAR(16),
	@ShowCommonNames					BIT,
	@ShowOriginalSpecimenNames			BIT,
	@SearchText							VARCHAR(30),
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

	INSERT INTO @SpecimensSearch (Item_Key, Life_Sciences, Hint) 
	SELECT DISTINCT SU.Collection_Unit_Key, SU.Life_Sciences, CUN.Number
	FROM 	Specimen_Unit SU
	JOIN 	Collection_Unit CU ON SU.Collection_Unit_Key = CU.Collection_Unit_Key 
	   		AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
			OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
	JOIN 	Collection_Unit_Number CUN ON SU.Collection_Unit_key = CUN.Collection_Unit_Key 
			AND CUN.Preferred = 1
			AND CUN.Number LIKE @SearchText + '%'

	UPDATE		@SpecimensSearch
	SET			Number = CUN.Number
	FROM 		@SpecimensSearch 		SU
	LEFT JOIN 	Collection_Unit_Number 	CUN ON 	SU.Item_key 	= CUN.Collection_Unit_Key 
											AND CUN.Preferred	= 1

	UPDATE @SpecimensSearch
	SET	Det_Item_Key=CPref.Concept_Key,
		Item_Name = CASE @ShowOriginalSpecimenNames
						WHEN 1 THEN TDet.Item_Name
						ELSE TPref.Item_Name END,
		Det_Item_Name=TDet.Plaintext
	FROM @SpecimensSearch SU
	INNER JOIN VW_SpecimenDetsEarth SDE ON SDE.Collection_Unit_Key=SU.Item_Key
		AND SDE.Preferred_Determination_Key=SDE.Determination_Key
	INNER JOIN Concept C ON C.Concept_Key=SDE.Concept_Key
	INNER JOIN Term TDet ON TDet.Term_Key=C.Term_Key
	INNER JOIN Concept CPref ON CPref.Meaning_Key=C.Meaning_Key
		AND CPref.List_Preferred=1
		AND CPref.Concept_Group_Key=C.Concept_Group_Key
	INNER JOIN Term TPref ON TPref.Term_Key=CPref.Term_Key

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
				null,
				@ShowCommonNames),
		Det_Item_Name=ITN.Actual_Name
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

/*============================================================================*\
  Grant permissions.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_Specimens_Select_ForSearchByPreferredNumber') IS NOT NULL
BEGIN
	PRINT 'Setting up security on procedure usp_Specimens_Select_ForSearchByPreferredNumber'
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByPreferredNumber TO R2k_AddOnly
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByPreferredNumber TO R2k_Administrator
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByPreferredNumber TO R2k_FullEdit
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByPreferredNumber TO R2k_ReadOnly
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByPreferredNumber TO R2k_RecordCardsOnly
END
GO

SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

/*============================================================================*\
  Drop stored proc before re-creating.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_Specimens_Select_ForSearchByAnyNumber') IS NOT NULL
	DROP PROCEDURE dbo.usp_Specimens_Select_ForSearchByAnyNumber
GO

/*=============================================================================*\
  Description:	Returns specimens based on their numbers.

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

  Created:		October 2003

  Last revision information:
    $Revision: 5 $
    $Date: 11/03/10 16:54 $
    $Author: Simonwood $
\*============================================================================*/
CREATE PROCEDURE dbo.usp_Specimens_Select_ForSearchByAnyNumber
	@UserDomainMask						INT,
	@SessionID							CHAR(16),
	@ShowCommonNames					BIT,
	@ShowOriginalSpecimenNames			BIT,
	@SearchText							VARCHAR(30),
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

	INSERT INTO @SpecimensSearch (Item_Key, Life_Sciences, Hint) 
	SELECT DISTINCT SU.Collection_Unit_Key, SU.Life_Sciences, CUN.Number
	FROM 	Specimen_Unit SU
	JOIN 	Collection_Unit CU ON SU.Collection_Unit_Key = CU.Collection_Unit_Key 
	   		AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
			OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
	JOIN 	Collection_Unit_Number CUN ON SU.Collection_Unit_key = CUN.Collection_Unit_Key 
			AND CUN.Number LIKE @SearchText + '%'

	UPDATE 		@SpecimensSearch
	SET			Number = CUN.Number
	FROM 		@SpecimensSearch 		SU
	LEFT JOIN 	Collection_Unit_Number 	CUN 
				ON SU.Item_key = CUN.Collection_Unit_Key 
				AND CUN.Preferred=1

	UPDATE @SpecimensSearch
	SET	Det_Item_Key=CPref.Concept_Key,
		Item_Name = CASE @ShowOriginalSpecimenNames
						WHEN 1 THEN TDet.Item_Name
						ELSE TPref.Item_Name END,
		Det_Item_Name=TDet.Plaintext
	FROM @SpecimensSearch SU
	INNER JOIN VW_SpecimenDetsEarth SDE ON SDE.Collection_Unit_Key=SU.Item_Key
		AND SDE.Preferred_Determination_Key=SDE.Determination_Key
	INNER JOIN Concept C ON C.Concept_Key=SDE.Concept_Key
	INNER JOIN Term TDet ON TDet.Term_Key=C.Term_Key
	INNER JOIN Concept CPref ON CPref.Meaning_Key=C.Meaning_Key
		AND CPref.List_Preferred=1
		AND CPref.Concept_Group_Key=C.Concept_Group_Key
	INNER JOIN Term TPref ON TPref.Term_Key=CPref.Term_Key

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
				null,
				@ShowCommonNames),
		Det_Item_Name=ITN.Actual_Name
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

/*============================================================================*\
  Grant permissions.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_Specimens_Select_ForSearchByAnyNumber') IS NOT NULL
BEGIN
	PRINT 'Setting up security on procedure usp_Specimens_Select_ForSearchByAnyNumber'
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByAnyNumber TO R2k_AddOnly
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByAnyNumber TO R2k_Administrator
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByAnyNumber TO R2k_FullEdit
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByAnyNumber TO R2k_ReadOnly
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByAnyNumber TO R2k_RecordCardsOnly
END
GO

SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

/*============================================================================*\
  Drop stored proc before re-creating.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_Specimens_Select_ForSearchByPreferredAccNumber') IS NOT NULL
	DROP PROCEDURE dbo.usp_Specimens_Select_ForSearchByPreferredAccNumber
GO

/*============================================================================*\
  Description:	Returns specimens based on their preferred accession numbers.

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

  Created:		October 2003

  Last revision information:
    $Revision: 5 $
    $Date: 11/03/10 16:54 $
    $Author: Simonwood $
\*============================================================================*/
CREATE PROCEDURE dbo.usp_Specimens_Select_ForSearchByPreferredAccNumber
	@UserDomainMask						INT,
	@SessionID							CHAR(16),
	@ShowCommonNames					BIT,
	@ShowOriginalSpecimenNames			BIT,
	@SearchText							VARCHAR(30),
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

	INSERT INTO @SpecimensSearch (Item_Key, Life_Sciences, Hint) 
	SELECT DISTINCT SU.Collection_Unit_Key, SU.Life_Sciences, M.Number
	FROM SPECIMEN_UNIT SU
	INNER JOIN Collection_Unit CU ON SU.Collection_Unit_Key = CU.Collection_Unit_Key 
	 	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
		OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
	INNER JOIN Movement_Collection_Unit MCU ON MCU.Collection_Unit_Key = SU.Collection_Unit_Key
	INNER JOIN Movement_Direction MD ON MD.Movement_Direction_Key=MCU.Movement_Direction_Key
		AND MD.Outbound=0
	INNER JOIN Movement_Of_Ownership MOE ON MOE.Movement_Direction_Key=MD.Movement_Direction_Key
	LEFT JOIN Movement_Of_Ownership_Exclusion MOEE 
		ON MOEE.Movement_Of_Ownership_Key=MOE.Movement_Of_Ownership_Key
		AND SU.Collection_Unit_Key=MOEE.Collection_Unit_Key
	INNER JOIN Movement M ON M.Movement_Key=MD.Movement_Key
		AND M.Movement_Type IN (0,1)
	WHERE M.Number LIKE @SearchText + '%'
	AND MOEE.Movement_Of_Ownership_Exclusion_Key IS NULL

	UPDATE		@SpecimensSearch
	SET			Number = CUN.Number
	FROM 		@SpecimensSearch 		SU
	LEFT JOIN 	Collection_Unit_Number 	CUN ON 	SU.Item_key 	= CUN.Collection_Unit_Key 
											AND CUN.Preferred	= 1

	UPDATE @SpecimensSearch
	SET	Det_Item_Key=CPref.Concept_Key,
		Item_Name = CASE @ShowOriginalSpecimenNames
						WHEN 1 THEN TDet.Item_Name
						ELSE TPref.Item_Name END,
		Det_Item_Name=TDet.Plaintext
	FROM @SpecimensSearch SU
	INNER JOIN VW_SpecimenDetsEarth SDE ON SDE.Collection_Unit_Key=SU.Item_Key
		AND SDE.Preferred_Determination_Key=SDE.Determination_Key
	INNER JOIN Concept C ON C.Concept_Key=SDE.Concept_Key
	INNER JOIN Term TDet ON TDet.Term_Key=C.Term_Key
	INNER JOIN Concept CPref ON CPref.Meaning_Key=C.Meaning_Key
		AND CPref.List_Preferred=1
		AND CPref.Concept_Group_Key=C.Concept_Group_Key
	INNER JOIN Term TPref ON TPref.Term_Key=CPref.Term_Key

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
				null,
				@ShowCommonNames),
		Det_Item_Name=ITN.Actual_Name
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

/*============================================================================*\
  Grant permissions.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_Specimens_Select_ForSearchByPreferredAccNumber') IS NOT NULL
BEGIN
	PRINT 'Setting up security on procedure usp_Specimens_Select_ForSearchByPreferredAccNumber'
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByPreferredAccNumber TO R2k_AddOnly
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByPreferredAccNumber TO R2k_Administrator
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByPreferredAccNumber TO R2k_FullEdit
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByPreferredAccNumber TO R2k_ReadOnly
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByPreferredAccNumber TO R2k_RecordCardsOnly
END
GO

SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

/*============================================================================*\
  Drop stored proc before re-creating.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_Specimens_Select_ForSearchByGatheringDate') IS NOT NULL
	DROP PROCEDURE dbo.usp_Specimens_Select_ForSearchByGatheringDate
GO

/*============================================================================*\
  Description:	Returns specimens based on their gathering date.

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

  Created:		October 2003

  Last revision information:
    $Revision: 5 $
    $Date: 11/03/10 16:54 $
    $Author: Simonwood $
\*============================================================================*/
CREATE PROCEDURE dbo.usp_Specimens_Select_ForSearchByGatheringDate
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

	INSERT INTO @SpecimensSearch (Item_Key, Life_Sciences, Hint) 
	SELECT DISTINCT 
		SU.Collection_Unit_Key, 
		SU.Life_Sciences, 
		dbo.ufn_GetDateFromVagueDate(S.Vague_Date_Start, S.Vague_Date_End, S.Vague_Date_Type)
	FROM Specimen_Unit SU
	INNER JOIN Collection_Unit CU ON SU.Collection_Unit_Key = CU.Collection_Unit_Key 
	 	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
		OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
	INNER JOIN Specimen_Field_Data SFD ON SFD.Collection_Unit_Key = SU.Collection_Unit_Key
		AND SFD.Gathering_Event=1
	LEFT JOIN Occurrence O ON O.Occurrence_Key = SFD.Occurrence_Key
	LEFT JOIN Taxon_Occurrence XO ON XO.Taxon_Occurrence_Key=SFD.Taxon_Occurrence_Key
	INNER JOIN [Sample] S ON S.Sample_Key=O.Sample_Key OR S.Sample_Key = XO.Sample_Key
	WHERE dbo.ufn_CBWrapperForDoVagueDatesOverlap(@SearchText, S.Vague_Date_Start, S.Vague_Date_End) = 1

	UPDATE		@SpecimensSearch
	SET			Number = CUN.Number
	FROM 		@SpecimensSearch 		SU
	LEFT JOIN 	Collection_Unit_Number 	CUN ON 	SU.Item_key 	= CUN.Collection_Unit_Key 
											AND CUN.Preferred	= 1

	UPDATE @SpecimensSearch
	SET	Det_Item_Key=CPref.Concept_Key,
		Item_Name = CASE @ShowOriginalSpecimenNames
						WHEN 1 THEN TDet.Item_Name
						ELSE TPref.Item_Name END,
		Det_Item_Name=TDet.Plaintext
	FROM @SpecimensSearch SU
	INNER JOIN VW_SpecimenDetsEarth SDE ON SDE.Collection_Unit_Key=SU.Item_Key
		AND SDE.Preferred_Determination_Key=SDE.Determination_Key
	INNER JOIN Concept C ON C.Concept_Key=SDE.Concept_Key
	INNER JOIN Term TDet ON TDet.Term_Key=C.Term_Key
	INNER JOIN Concept CPref ON CPref.Meaning_Key=C.Meaning_Key
		AND CPref.List_Preferred=1
		AND CPref.Concept_Group_Key=C.Concept_Group_Key
	INNER JOIN Term TPref ON TPref.Term_Key=CPref.Term_Key

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
				null,
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

/*============================================================================*\
  Grant permissions.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_Specimens_Select_ForSearchByGatheringDate') IS NOT NULL
BEGIN
	PRINT 'Setting up security on procedure usp_Specimens_Select_ForSearchByGatheringDate'
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByGatheringDate TO R2k_AddOnly
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByGatheringDate TO R2k_Administrator
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByGatheringDate TO R2k_FullEdit
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByGatheringDate TO R2k_ReadOnly
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByGatheringDate TO R2k_RecordCardsOnly
END
GO

SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

/*============================================================================*\
  Drop stored proc before re-creating.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_Specimens_Select_ForSearchByGatheringLocation') IS NOT NULL
	DROP PROCEDURE dbo.usp_Specimens_Select_ForSearchByGatheringLocation
GO

/*============================================================================*\
  Description:	Returns specimens based on their gathering location.

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

  Created:		October 2003

  Last revision information:
    $Revision: 5 $
    $Date: 11/03/10 16:54 $
    $Author: Simonwood $
\*============================================================================*/
CREATE PROCEDURE dbo.usp_Specimens_Select_ForSearchByGatheringLocation
	@UserDomainMask						INT,
	@SessionID							CHAR(16),
	@ShowCommonNames					BIT,
	@ShowOriginalSpecimenNames			BIT,
	@SearchText							VARCHAR(100),
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

	INSERT INTO @SpecimensSearch (Item_Key, Life_Sciences, Hint) 
	SELECT 	DISTINCT SU.Collection_Unit_Key, SU.Life_Sciences, LN.Item_Name
	FROM 	Specimen_Unit SU
	INNER JOIN Collection_Unit CU ON SU.Collection_Unit_Key = CU.Collection_Unit_Key 
	 	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
		OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
	INNER JOIN Specimen_Field_Data SFD ON SFD.Collection_Unit_Key = SU.Collection_Unit_Key
		AND SFD.Gathering_Event=1
	LEFT JOIN Occurrence O ON O.Occurrence_Key = SFD.Occurrence_Key
	LEFT JOIN Taxon_Occurrence XO ON XO.Taxon_Occurrence_Key=SFD.Taxon_Occurrence_Key
	INNER JOIN [Sample] S ON S.Sample_Key=O.Sample_Key OR S.Sample_Key = XO.Sample_Key
	INNER JOIN [Survey_Event] SE ON SE.Survey_Event_Key=S.Survey_Event_Key
	INNER JOIN Location_Name LN ON LN.Location_Key=S.Location_Key OR LN.Location_KEY=SE.Location_Key
	WHERE LN.Item_Name LIKE @SearchText + '%'

	UPDATE		@SpecimensSearch
	SET			Number = CUN.Number
	FROM 		@SpecimensSearch 		SU
	LEFT JOIN 	Collection_Unit_Number 	CUN ON 	SU.Item_key 	= CUN.Collection_Unit_Key 
											AND CUN.Preferred	= 1

	UPDATE @SpecimensSearch
	SET	Det_Item_Key=CPref.Concept_Key,
		Item_Name = CASE @ShowOriginalSpecimenNames
						WHEN 1 THEN TDet.Item_Name
						ELSE TPref.Item_Name END,
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
	INNER JOIN Term TPref ON TPref.Term_Key=CPref.Term_Key


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

/*============================================================================*\
  Grant permissions.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_Specimens_Select_ForSearchByGatheringLocation') IS NOT NULL
BEGIN
	PRINT 'Setting up security on procedure usp_Specimens_Select_ForSearchByGatheringLocation'
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByGatheringLocation TO R2k_AddOnly
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByGatheringLocation TO R2k_Administrator
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByGatheringLocation TO R2k_FullEdit
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByGatheringLocation TO R2k_ReadOnly
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByGatheringLocation TO R2k_RecordCardsOnly
END
GO

SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

/*============================================================================*\
  Drop stored proc before re-creating.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_Specimens_Select_ForSearchByMetadata') IS NOT NULL
	DROP PROCEDURE dbo.usp_Specimens_Select_ForSearchByMetadata
GO

/*============================================================================*\
  Description:	Returns specimens based on their metadata of the specified type.

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
				@MetaDataType			The type of metadata to search on

  Created:		September 2007

  Last revision information:
    $Revision: 5 $
    $Date: 11/03/10 16:54 $
    $Author: Simonwood $
\*============================================================================*/
CREATE PROCEDURE dbo.usp_Specimens_Select_ForSearchByMetadata
	@UserDomainMask						INT,
	@SessionID							CHAR(16),
	@ShowCommonNames					BIT,
	@ShowOriginalSpecimenNames			BIT,
	@SearchText							VARCHAR(100),
	@SortOrderIndex						TINYINT,
	@MetaDataType						VARCHAR(100)
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

	DECLARE @MetaDataTypeKey CHAR(16)
	SET @MetaDataTypeKey = (SELECT MetaData_Type_Key From Metadata_Type WHERE Item_Name = @MetaDataType AND Table_Name = 'Specimen')


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
		[Hint] [text] COLLATE SQL_Latin1_General_CP1_CI_AS NULL 
	)

	INSERT INTO @SpecimensSearch (Item_Key, Life_Sciences, Hint) 
	SELECT 	SU.Collection_Unit_Key, SU.Life_Sciences, M.Text
	FROM 	Specimen_Unit SU
	JOIN 	Metadata M ON M.Record_Key=SU.Collection_Unit_Key
			AND M.Metadata_Type_Key=@MetaDataTypeKey
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
		Item_Name = CASE @ShowOriginalSpecimenNames
						WHEN 1 THEN TDet.Item_Name
						ELSE TPref.Item_Name END,
		Det_Item_Name=TDet.Plaintext
	FROM @SpecimensSearch SU
	INNER JOIN VW_SpecimenDetsEarth SDE ON SDE.Collection_Unit_Key=SU.Item_Key
		AND SDE.Preferred_Determination_Key=SDE.Determination_Key
	INNER JOIN Concept C ON C.Concept_Key=SDE.Concept_Key
	INNER JOIN Term TDet ON TDet.Term_Key=C.Term_Key
	INNER JOIN Concept CPref ON CPref.Meaning_Key=C.Meaning_Key
			AND CPref.List_Preferred=1
			AND CPref.Concept_Group_Key=C.Concept_Group_Key
	INNER JOIN Term TPref ON TPref.Term_Key=CPref.Term_Key


	UPDATE @SpecimensSearch
	SET	Det_Item_Key = SDL.Taxon_List_Item_Key,
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
		Det_Item_Name = ITN.Actual_Name
	FROM 	@SpecimensSearch SU
	INNER JOIN VW_SpecimenDetsLife SDL ON SDL.Collection_Unit_Key = SU.Item_Key
			AND SDL.Preferred_Taxon_Determination_Key = SDL.Taxon_Determination_Key
	INNER JOIN Index_Taxon_Name ITN	ON ITN.Taxon_List_Item_Key = SDL.Taxon_List_Item_Key

	-- Select table and sort appropriately
	IF @SortOrderIndex = 0
		SELECT * FROM @SpecimensSearch
		ORDER BY Item_Name, Number
	ELSE IF @SortOrderIndex = 1
		SELECT * FROM @SpecimensSearch
		ORDER BY Number, Item_Name
GO

/*============================================================================*\
  Grant permissions.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_Specimens_Select_ForSearchByMetadata') IS NOT NULL
BEGIN
	PRINT 'Setting up security on procedure usp_Specimens_Select_ForSearchByMetadata'
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByMetadata TO R2k_AddOnly
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByMetadata TO R2k_Administrator
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByMetadata TO R2k_FullEdit
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByMetadata TO R2k_ReadOnly
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByMetadata TO R2k_RecordCardsOnly
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_Concept_Select_Synonyms]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_Concept_Select_Synonyms]
GO

/*===========================================================================*\
  Description:	Names of synonyms of the specified concept.

  Parameters:   
	@concept_key	Concept key

  Created:	Jan 2004

  Last revision information:
	$Revision: 5 $
	$Date: 11/03/10 16:54 $
	$Author: Simonwood $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Concept_Select_Synonyms]
	@concept_key		CHAR(16)
AS
	SET NOCOUNT ON

	SELECT			DISTINCT
					t.Item_Name,
					ISNULL
					(
						TV.Author_And_Date,
						''
					)						AS		Authority, 
					t.PlainText,
					g.Item_Name				AS		Group_Name
	
	FROM			Concept					AS		c
	INNER JOIN		Concept					AS		s
	ON				s.Meaning_Key			=		c.Meaning_Key
	
	INNER JOIN		Term					AS		t
	ON				t.Term_Key				=		s.Term_Key
	

	INNER JOIN		dbo.Term_Version		AS		TV
	ON				TV.Term_Version_Key		=		s.Term_Version_Key

	INNER JOIN		Concept_Group			AS 		g
	ON				g.Concept_Group_Key		=		s.Concept_Group_Key
	WHERE			c.Concept_Key			=		@concept_key
	AND				s.Concept_Key			<>		@concept_key
	ORDER BY		g.Item_Name
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_Select_Synonyms') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Concept_Select_Synonyms'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concept_Select_Synonyms TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concept_Select_Synonyms TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Concept_Select_Synonyms TO [Dev - JNCC SQL]
END
GO

IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_PotentialSynonyms_Select_ForMerge]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
	DROP PROCEDURE [dbo].[usp_PotentialSynonyms_Select_ForMerge]
GO

/*===========================================================================*\
  Description:	
	Returns concepts that are potential synonyms.

  Parameters:	
  @ConceptGroupKey 
	When specified, only the Concepts in the group are scanned. Otherwise,
	the whole thesaurus is scanned.

  Created:	December 2006

  Last revision information:
    $Revision: 5 $
    $Date: 11/03/10 16:54 $
    $Author: Simonwood $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_PotentialSynonyms_Select_ForMerge] 
	@ConceptGroupKey	CHAR(16),
	@MaxRowCount		INT
AS
	SET NOCOUNT ON

	SET ROWCOUNT @MaxRowCount

	SELECT			DISTINCT 
					CSource.Concept_Key			AS	SourceConceptKey,
					CG.Item_Name				AS	SourceGroup,
					ISNULL
					(
						TV1.Author_And_Date,
						''
					)							AS	SourceAuthority,
					TSource.Item_Name			AS	SourceConcept,
					TSource.PlainText,			-- There for the ORDER BY
					CPotSyn.Concept_Key			AS	SynonymConceptKey,
					CGPotSyn.Item_Name			AS	SynonymGroup,
					ISNULL
					(
						TV2.Author_And_Date,
						''
					)							AS	SynonymAuthority,
					TPotSyn.Item_Name			AS	SynonymConcept,
					TPotentials.Item_Name		AS	SharedTerm,
					CSource.Custodian,
					CSource.Timestamp
	FROM			Concept						AS	CSource
	INNER JOIN		Concept						AS	CSynonyms 		
	ON				CSynonyms.Meaning_Key		=	CSource.Meaning_Key
	INNER JOIN		Term						AS	TSource
	ON				TSource.Term_Key			=	CSource.Term_Key
	INNER JOIN		Term						AS	TSynonyms
	ON				TSynonyms.Term_Key			=	CSynonyms.Term_Key
	INNER JOIN		Term						AS	TPotentials
	ON				TPotentials.Item_Name		=	TSynonyms.Item_Name
	AND				TPotentials.Language_Key	=	TSynonyms.Language_Key
	INNER JOIN		Concept						AS	CPotentials
	ON				CPotentials.Term_Key		=	TPotentials.Term_Key
	INNER JOIN		Concept						AS	CPotSyn
	ON				CPotSyn.Meaning_Key			=	CPotentials.Meaning_Key
	INNER JOIN		Term						AS	TPotSyn
	ON				TPotSyn.Term_Key			=	CPotSyn.Term_Key
	LEFT JOIN		Concept						AS	CExclude
	ON				CExclude.Concept_Key		=	CPotentials.Concept_Key
	AND				CExclude.Meaning_Key		=	CSource.Meaning_Key
	INNER JOIN		Concept_Group				AS	CG
	ON				CG.Concept_Group_Key		=	CSource.Concept_Group_Key
	INNER JOIN		Concept_Group				AS	CGPotSyn
	ON				CGPotSyn.Concept_Group_Key	=	CPotSyn.Concept_Group_Key
	LEFT JOIN		dbo.Term_Version			AS		TV1
	ON				TV1.Term_Version_Key		=		CSource.Term_Version_Key
	LEFT JOIN		dbo.Term_Version			AS		TV2
	ON				TV2.Term_Version_Key		=		CSynonyms.Term_Version_Key
	WHERE			(@ConceptGroupKey IS NULL 
	OR				CSource.Concept_Group_Key	=	@ConceptGroupKey)
	AND				CExclude.Concept_Key IS NULL
	AND				CSource.List_Preferred		=	1
	AND				CPotSyn.List_Preferred		=	1
	ORDER BY		TSource.PlainText
	
	SET ROWCOUNT 0
	SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_PotentialSynonyms_Select_ForMerge') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_PotentialSynonyms_Select_ForMerge'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_PotentialSynonyms_Select_ForMerge TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_PotentialSynonyms_Select_ForMerge TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_PotentialSynonyms_Select_ForMerge TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_PotentialSynonyms_Select_ForMerge TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_PotentialSynonyms_Select_ForMerge TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_PotentialSynonyms_Select_ForMerge TO [Dev - JNCC SQL]
END
GO

IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_DuplicateTerms_Select_ForMerge]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
	DROP PROCEDURE [dbo].[usp_DuplicateTerms_Select_ForMerge]
GO

/*===========================================================================*\
  Description:	Returns Collection Units for a specified movement.

  Parameters:	
  @ConceptGroupKey 
	When specified, only the Concepts in the group are scanned. Otherwise,
	the whole thesaurus is scanned.

  Created:	December 2006

  Last revision information:
    $Revision: 5 $
    $Date: 11/03/10 16:54 $
    $Author: Simonwood $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_DuplicateTerms_Select_ForMerge]
	@ConceptGroupKey	CHAR(16),
	@MaxRowCount		INT
AS
	SET NOCOUNT ON

	SET ROWCOUNT @MaxRowCount

	SELECT		DISTINCT
				T1.PlainText			AS	Term,
				T1.Term_Key 			AS	TermKey1, 
				T2.Term_Key 			AS	TermKey2, 
				CG1.Item_Name 			AS	Group1,
				ISNULL
				(
					TV1.Author_And_Date,
					''
				)						AS	Authority1,
				C1Pref.Concept_Key		AS	PreferredKey1,
				C1Pref.PlainText 		AS	Preferred1,
				CG2.Item_Name 			AS	Group2,
				ISNULL
				(
					TV2.Author_And_Date,
					''
				)						AS	Authority2, 
				C2Pref.Concept_Key		AS	PreferredKey2,
				C2Pref.PlainText 		AS	Preferred2,
				T1.Custodian,
				T1.Timestamp

	FROM		Term T1
	-- Find different term with same text
	JOIN		Term T2					ON	T2.PlainText = T1.PlainText
	AND			T2.Language_Key			=	T1.Language_Key
	AND			T2.Term_Key				<>	T1.Term_Key
	-- Find Concept using the term
	JOIN		Concept					AS	C1
	ON			C1.Term_Key				=	T1.Term_Key
	-- Find synonym that is list preferred
	JOIN		VW_ConceptTerm			AS	C1Pref
	ON			C1Pref.Meaning_Key		=	C1.Meaning_Key
	AND			C1Pref.List_Preferred	=	1
	-- Get group name
	JOIN		Concept_Group			AS	CG1
	ON			CG1.Concept_Group_Key	=	C1Pref.Concept_Group_Key
	-- Find other concept using same term
	JOIN		Concept					AS	C2
	ON			C2.Term_Key				=	T2.Term_Key
	-- Find synonym that is list preferred
	JOIN		VW_ConceptTerm			AS	C2Pref
	ON			C2Pref.Meaning_Key		=	C2.Meaning_Key
	AND			C2Pref.List_Preferred	=	1
	-- Get group name
	JOIN		Concept_Group			AS	CG2
	ON			CG2.Concept_Group_Key	=	C2Pref.Concept_Group_Key
	
	LEFT JOIN	dbo.Term_Version		AS		TV1
	ON			TV1.Term_Version_Key	=		C1.Term_Version_Key
	LEFT JOIN	dbo.Term_Version		AS		TV2
	ON			TV2.Term_Version_Key	=		C2.Term_Version_Key

	WHERE		(@ConceptGroupKey IS NULL
	OR			C1.Concept_Group_Key	=	@ConceptGroupKey)
	-- Only want different concepts sharing term.
	AND			C1Pref.Concept_Key		<>	C2Pref.Concept_Key
	ORDER BY	T1.PlainText,
				CG1.Item_Name,
				C1Pref.PlainText,
				C2Pref.PlainText

	SET ROWCOUNT 0

	SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_DuplicateTerms_Select_ForMerge') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_DuplicateTerms_Select_ForMerge'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_DuplicateTerms_Select_ForMerge TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_DuplicateTerms_Select_ForMerge TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_DuplicateTerms_Select_ForMerge TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_DuplicateTerms_Select_ForMerge TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_DuplicateTerms_Select_ForMerge TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_DuplicateTerms_Select_ForMerge TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Locations_Select_AllSubLocationsForLevel') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].usp_Locations_Select_AllSubLocationsForLevel
GO

/*===========================================================================*\
  Description:	Returns All locations and features below a given location.

  Parameters:
	@Key	Location key

  Created:	November 2009

  Last revision information:
    $Revision: 5 $
    $Date: 11/03/10 16:54 $
    $Author: Simonwood $

\*===========================================================================*/
CREATE PROCEDURE [dbo].usp_Locations_Select_AllSubLocationsForLevel
	@Key 	CHAR(16)
AS
	SET NOCOUNT ON

	DECLARE	@Results
	TABLE 
	(
		ItemKey	CHAR(16) COLLATE Database_Default
	)
	
	INSERT INTO		@Results
	SELECT			Location_Key
	FROM			dbo.Location
	WHERE			Location_Key		=		@Key
	
	-- Gather hierarchy
	WHILE @@RowCount > 0
		INSERT INTO		@Results
		SELECT			Location_Key
		FROM			dbo.Location
		JOIN			@Results
		ON				ItemKey			=		Parent_Key
		-- Don't want to loop forever!
		WHERE			Location_Key	NOT IN	(SELECT ItemKey FROM @Results)
	
	SELECT	ItemKey
	FROM	@Results
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Locations_Select_AllSubLocationsForLevel') AND SysStat & 0xf = 4)
BEGIN
   	PRINT 'Setting up security on procedure usp_Locations_Select_AllSubLocationsForLevel'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Locations_Select_AllSubLocationsForLevel TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Locations_Select_AllSubLocationsForLevel TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Locations_Select_AllSubLocationsForLevel TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Locations_Select_AllSubLocationsForLevel TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Locations_Select_AllSubLocationsForLevel TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
       	GRANT EXECUTE ON dbo.usp_Locations_Select_AllSubLocationsForLevel TO [Dev - JNCC SQL]
END
GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Specimen_GetFinderData]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Specimen_GetFinderData]
GO
    
/*===========================================================================*\
  Description:	Gets all the data needed to add dropped specimens to one of the
				specimen finder grids
  Parameters:	 

  Created:	October 2007

  Last revision information:
    $Revision: 5 $
    $Date: 11/03/10 16:54 $
    $Author: Simonwood $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_Specimen_GetFinderData]
  @CollectionUnitKey  Char(16),
  @UserDomainMask Int,
  @ShowCommonNames Bit,
  @SessionID      Char(16)
 AS

-- Because of multiple preferred list synoynms - i.e. Amber and Ambre - different language but both preferred. Just select the first.
SELECT		TOP 1
			SU.Collection_Unit_Key					AS		Item_Key, 
			CASE 
				WHEN C.Concept_Key IS NOT NULL
				THEN C.Concept_Key 
				WHEN ITN.Taxon_List_Item_Key IS NOT NULL
				THEN ITN.Taxon_List_Item_Key 
			END										AS		Det_Item_Key, 
			CASE 
				WHEN SU.Life_Sciences = 0
				THEN TP.Item_Name 
				ELSE dbo.ufn_GetFormattedTaxonNameByParams(
						ITN.Actual_Name,
						ITN.Actual_Name_Italic,
						ITN.Common_Name, 
						ITN.Common_Name_Italic,
						ITN.Authority,
						@ShowCommonNames)
			END										AS		Item_Name, 
			SU.Life_Sciences, 
			dbo.ufn_GetPrefNumber(
					SU.Collection_Unit_Key)			AS		Number, 
			CU.Current_Location_Code, 
			SU.Specimen_Type_Concept_Key, 
			STTP.Item_Name							AS		Specimen_Type,
			C.List_Code, 
			DM.Item_Name							AS		Domain_Name 
FROM		SPECIMEN_UNIT							AS		SU 
INNER JOIN	COLLECTION_UNIT							AS		CU
ON			SU.Collection_Unit_Key					=		CU.Collection_Unit_Key 
AND			((CU.Domain_Mask & @UserDomainMask > 0)
OR			(CU.Domain_Mask = 0)
OR			(CU.Entered_Session_ID = @SessionID)
OR			(CU.Changed_Session_ID = @SessionID)) 
LEFT JOIN	COLLECTION_UNIT_NUMBER					AS		CUN
ON			SU.Collection_Unit_key					=		CUN.Collection_Unit_Key 
AND			CUN.Preferred							=		1 
LEFT JOIN	DETERMINATION							AS		D
ON			SU.Preferred_Determination_Key			=		D.Determination_Key 
LEFT JOIN	Concept									AS		C
ON			D.Concept_Key							=		C.Concept_Key 
LEFT JOIN	Concept									AS		CP
ON			CP.Meaning_Key							=		C.Meaning_Key
AND			CP.Concept_Group_Key					=		C.Concept_Group_Key
AND			CP.List_Preferred						=		1 
LEFT JOIN	Term									AS		TP
ON			CP.Term_Key								=		TP.Term_Key 
LEFT JOIN	TAXON_DETERMINATION						AS		TD
ON			SU.Preferred_Taxon_Determination_Key	=		TD.Taxon_Determination_Key 
LEFT JOIN	INDEX_TAXON_NAME						AS		ITN
ON			TD.Taxon_List_Item_Key					=		ITN.Taxon_List_Item_Key 
LEFT JOIN	Concept_Group							AS		CG
ON			C.Concept_Group_Key						=		CG.Concept_Group_Key 
LEFT JOIN	Local_Domain							AS		LD
ON			CG.Local_Domain_Key						=		LD.Local_Domain_Key 
LEFT JOIN	Domain									AS		DM
ON			LD.Domain_Key							=		DM.Domain_Key
INNER JOIN	Concept									AS		STC
ON			SU.Specimen_Type_Concept_Key			=		STC.Concept_Key 
LEFT JOIN	Concept									AS		STCP
ON			STCP.Meaning_Key						=		STC.Meaning_Key 
AND			STCP.Concept_Group_Key					=		STC.Concept_Group_Key
AND			STCP.List_Preferred						=		1 
LEFT JOIN	Term									AS		STTP
ON			STCP.Term_Key							=		STTP.Term_Key
WHERE		SU.Collection_Unit_Key					=		@CollectionUnitKey
ORDER BY	3 
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Specimen_GetFinderData') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Specimen_GetFinderData'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Specimen_GetFinderData TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimen_GetFinderData TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimen_GetFinderData TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimen_GetFinderData TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Specimen_GetFinderData TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SpecimensMailMerge_Select') AND SysStat & 0xf = 4)
    DROP PROCEDURE [dbo].[usp_SpecimensMailMerge_Select]
GO

/*===========================================================================*\
  Description:	Returns information about the specified specimens for a 
		Specimens Mail Merge Output report.

  Parameters:	Use #SpecimensMailMergeKeys as source.

  Created:	September 2004

  Last revision information:
    $Revision: 5 $
    $Date: 11/03/10 16:54 $
    $Author: Simonwood $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SpecimensMailMerge_Select]
AS

	SELECT		dbo.ufn_GetPrefNumber(specimen.Collection_Unit_Key)			AS	RegNo, 
				CASE
					WHEN specimen.Life_Sciences = 0
					THEN ISNULL(CT.Item_Name, 'No Determination') 
					ELSE ISNULL(dbo.ufn_GetFormattedTaxonNameByParams(
							ITN.Actual_Name, ITN.Actual_Name_Italic,
							ITN.Common_Name, ITN.Common_Name_Italic,
							ITN.Authority, 1), 'No Determination') 
				END														AS	Determination, 
				CASE
					WHEN specimen.Life_Sciences = 0
					THEN ISNULL(CT.Item_Name, 'No Determination') 
					ELSE ISNULL(ITN.Actual_Name, 'No Determination') 
				END														AS	PlainDetermination, 
				dbo.ufn_GetFieldCollectors(
						specimen.Collection_Unit_Key)							AS	FieldCollector, 
				dbo.ufn_GetSpecimenGatheringSite(
						specimen.Collection_Unit_Key)							AS	GatheringSite, 
				dbo.ufn_GetDateFromVagueDate(S.Vague_Date_Start,
						S.Vague_Date_End, S.Vague_Date_Type)			AS	GatheringDate 

	FROM  		Specimen_Unit											AS	specimen 
	LEFT JOIN 	Determination											AS	D
	ON			D.Determination_Key										=	specimen.Preferred_Determination_Key 
	LEFT JOIN 	Occurrence												AS	O
	ON			O.Occurrence_Key										=	D.Determination_Key 
	LEFT JOIN 	VW_ConceptTermPreferred									AS	CT
	ON			CT.Concept_Key											=	D.Concept_Key 
	LEFT JOIN 	Taxon_Determination										AS	TD
	ON			specimen.Preferred_Taxon_Determination_Key					=	TD.Taxon_Determination_Key 
	LEFT JOIN 	Taxon_Occurrence										AS	XO
	ON			XO.Taxon_Occurrence_Key									=	TD.Taxon_Determination_Key 
	LEFT JOIN 	Index_Taxon_Name										AS	ITN
	ON			ITN.Taxon_List_Item_Key									=	TD.Taxon_List_Item_Key 
	LEFT JOIN 	[Sample]												AS	S
	ON			S.Sample_Key											IN	(XO.Sample_Key, O.Sample_Key) 

	WHERE		specimen.Collection_Unit_Key									IN	(SELECT	Specimen_Unit_Key FROM #TempSpecimenKeys)

	ORDER BY	PlainDetermination

	FOR XML AUTO
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SpecimensMailMerge_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SpecimensMailMerge_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SpecimensMailMerge_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SpecimensMailMerge_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SpecimensMailMerge_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_SpecimensMailMerge_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SpecimensMailMerge_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SpecimensMailMerge_Select TO [Dev - JNCC SQL]
END
GO

SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

/*============================================================================*\
  Drop stored proc before re-creating.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_PotentialSynonyms_Select_ForImportedConcept') IS NOT NULL
	DROP PROCEDURE dbo.usp_PotentialSynonyms_Select_ForImportedConcept
GO

/*============================================================================*\
  Description:  List-preferred potential synonyms of the specified concept.

  Parameters:   @concept_key            Concept key

  Created:      Jan 2004

  Last revision information:
	$Revision: 5 $
	$Date: 11/03/10 16:54 $
	$Author: Simonwood $
\*============================================================================*/
CREATE PROCEDURE dbo.usp_PotentialSynonyms_Select_ForImportedConcept
	@concept_key        CHAR(16)
AS
	SET NOCOUNT ON

	DECLARE     @src_meaning_key    CHAR(16),
				@src_group_key      CHAR(16)

	SELECT      @src_meaning_key    =   Meaning_Key,
				@src_group_key      =   Concept_Group_Key
	FROM        Concept
	WHERE       Concept_Key         =   @concept_key

	/* work out all current synonyms of the concept */
	DECLARE     @current_synonyms   TABLE (
		Language_Key        CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS,
		Plaintext           NVARCHAR(300) COLLATE SQL_Latin1_General_CP1_CI_AI
		PRIMARY KEY (Language_Key, Plaintext))

	INSERT      @current_synonyms (
				Language_Key,
				Plaintext)
	SELECT DISTINCT
				t.Language_Key,
				t.Plaintext
	FROM        Concept             AS  c
	INNER JOIN  Term                AS  t
	ON          t.Term_Key          =   c.Term_Key
	WHERE       c.Meaning_Key       =   @src_meaning_key

	IF @@ERROR <> 0 RETURN        

	/* work out all list-preferred potential synonyms */
	DECLARE     @potential  TABLE (
			Concept_Key         CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS PRIMARY KEY,
			Concept_Group_Key   CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS,
			Meaning_Key         CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS,
			Term_Key            CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS,
			Author_Copy         VARCHAR(100) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
			Different_Group     BIT,
			Session_Start       DATETIME)

	INSERT      @potential (
				Concept_Key,
				Concept_Group_Key,
				Meaning_Key,
				Term_Key,
				Author_Copy,
				Different_Group,
				Session_Start)
	SELECT DISTINCT
				psyn.Concept_Key,
				psyn.Concept_Group_Key,
				psyn.Meaning_Key,
				psyn.Term_Key,
				psyn.Author_Copy,
				CASE psyn.Concept_Group_Key
					WHEN @src_group_key THEN 0
					ELSE 1
				END,
				s.Date_Time_Start
	FROM        @current_synonyms   AS  curr
	INNER JOIN  Term                AS  tpot WITH (INDEX (IX_Plaintext))
	ON          tpot.Plaintext      =   curr.Plaintext
	AND         tpot.Language_Key   =   curr.Language_Key
	INNER JOIN  Concept             AS  pot
	ON          pot.Term_Key        =   tpot.Term_Key
	INNER JOIN  Concept             AS  psyn
	ON          psyn.Meaning_Key    =   pot.Meaning_Key
	AND         psyn.List_Preferred =   1
	INNER JOIN  Session             AS  s
	ON          s.Session_ID        =   psyn.Entered_Session_ID
	WHERE       pot.Meaning_Key     <>  @src_meaning_key  /* not currently a synonym */

	IF @@ERROR <> 0 RETURN

	/* select most recently entered list-preferred concept for each
	 * potential synonym */
	SELECT      p.Concept_Key       AS  Item_Key,
				ISNULL(
					t.Item_Name + ' ' + p.Author_Copy,
					t.Item_Name)    AS  Item_Name,
				g.Concept_Group_Key	AS	Group_Key,
				g.Item_Name         AS  Group_Name,
				g.Authority
	FROM        @potential          AS  p
	INNER JOIN  Term                AS  t
	ON          t.Term_Key          =   p.Term_Key
	INNER JOIN  Concept_Group       AS  g
	ON          g.Concept_Group_Key =   p.Concept_Group_Key
	WHERE       p.Session_Start     =   (   SELECT      TOP 1 Session_Start
											FROM        @potential      AS  p2
											WHERE       p2.Meaning_Key  =   p.Meaning_Key
											ORDER BY    p2.Different_Group,
														p2.Session_Start DESC)
GO

/*============================================================================*\
  Grant permissions.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_PotentialSynonyms_Select_ForImportedConcept') IS NOT NULL
BEGIN
	PRINT 'Setting up security on procedure usp_PotentialSynonyms_Select_ForImportedConcept'
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_AddOnly')
			GRANT EXECUTE ON dbo.usp_PotentialSynonyms_Select_ForImportedConcept TO R2k_AddOnly
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_PotentialSynonyms_Select_ForImportedConcept TO R2k_Administrator
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_PotentialSynonyms_Select_ForImportedConcept TO R2k_FullEdit
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_PotentialSynonyms_Select_ForImportedConcept TO R2k_ReadOnly
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_PotentialSynonyms_Select_ForImportedConcept TO R2k_RecordCardsOnly
END
GO

