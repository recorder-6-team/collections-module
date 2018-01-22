/*===========================================================================*\
  Description:
	Updates for CCN 156 - Updates to the Specimen Select Stored procedures.

  Created:
	October 2012

  Last revision information:
    $Revision: 3 $
    $Date: 30/10/12 16:48 $
    $Author: Alexanderpadley $

\*===========================================================================*/

/*============================================================================*\
  Drop stored proc before re-creating.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_Specimens_Select_ForSearchByGatheringDate') IS NOT NULL
	DROP PROCEDURE dbo.usp_Specimens_Select_ForSearchByGatheringDate
GO

/*============================================================================*\
  Description:	Returns specimens based on their gathering date.
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
		[Hint] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[NomenclaturalStatus] VARCHAR(250) NULL 
	)

	INSERT INTO @SpecimensSearch (Item_Key, Life_Sciences, Hint, NomenclaturalStatus) 
	SELECT DISTINCT 
		SU.Collection_Unit_Key, 
		SU.Life_Sciences, 
		dbo.ufn_GetDateFromVagueDate(S.Vague_Date_Start, S.Vague_Date_End, S.Vague_Date_Type), 
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
						WHEN 1 THEN C.Published_Term
						ELSE CPref.Published_Term END,
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

/*============================================================================*\
  Drop stored proc before re-creating.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_Specimens_Select_ForSearchByDeterminationInGroup') IS NOT NULL
	DROP PROCEDURE dbo.usp_Specimens_Select_ForSearchByDeterminationInGroup
GO

/*============================================================================*\
  Description:	Returns specimens that have determinations whose concepts are
				hierarchically contained by concepts that match the search text.

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
		[Hint] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[NomenclaturalStatus] VARCHAR(250) NULL 
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
	LEFT JOIN	Search_Term ST ON ST.Concept_Key = CSearch.Concept_Key
	WHERE 		ST.Plaintext LIKE @SearchText + '%'

	INSERT INTO 	@SpecimensSearch (Item_Key, Life_Sciences, Hint, NomenclaturalStatus) 
	SELECT DISTINCT SDE.Collection_Unit_Key, 0 AS Life_Sciences, SL.Plaintext, 
		SUBSTRING(( 
				SELECT	C.Published_Term + ','
				FROM	Determination D 
				INNER JOIN	Concept C
					ON	D.Nomenclatural_Status_Concept_Key = C.Concept_Key
				WHERE	D.Nomenclatural_Status_Concept_Key IS NOT NULL
					AND	D.Specimen_Collection_Unit_Key = CU.Collection_Unit_Key
				ORDER BY	D.Determination_Key
					FOR XML PATH('')
			), 0, 250) 
	FROM 		@SearchLineage SL
	INNER JOIN 	vw_ConceptTerm CChildSyn ON CChildSyn.Meaning_Key = SL.Child_Meaning_Key
	INNER JOIN 	vw_SpecimenDetsEarth SDE ON SDE.Concept_Key = CChildSyn.Concept_Key
	INNER JOIN 	Collection_Unit CU ON CU.Collection_Unit_Key = SDE.Collection_Unit_Key	
	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
	
	INSERT INTO 	@SpecimensSearch (Item_Key, Life_Sciences, Hint, NomenclaturalStatus) 
	SELECT DISTINCT SDL.Collection_Unit_Key AS Item_Key, 1 AS Life_Sciences, ITNSearch.Actual_Name, 
		SUBSTRING(( 
				SELECT	C.Published_Term + ','
				FROM	Taxon_Determination D 
				INNER JOIN	Concept C
					ON	D.Nomenclatural_Status_Concept_Key = C.Concept_Key
				WHERE	D.Nomenclatural_Status_Concept_Key IS NOT NULL
					AND	D.Specimen_Collection_Unit_Key = CU.Collection_Unit_Key
				ORDER BY	D.Taxon_Determination_Key
					FOR XML PATH('')
			), 0, 250)
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
						WHEN 1 THEN C.Published_Term
						ELSE CPref.Published_Term END,
		Det_Item_Name=TDet.Plaintext 
	FROM 		@SpecimensSearch SU
	INNER JOIN 	VW_SpecimenDetsEarth SDE ON SDE.Collection_Unit_Key=SU.Item_Key
		AND SDE.Preferred_Determination_Key=SDE.Determination_Key
	INNER JOIN 	Concept C ON C.Concept_Key=SDE.Concept_Key
	INNER JOIN 	Term TDet ON TDet.Term_Key=C.Term_Key
	INNER JOIN 	Concept CPref ON CPref.Meaning_Key = C.Meaning_Key AND CPref.List_Preferred = 1 AND CPref.Concept_Group_Key = C.Concept_Group_Key
	
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

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Specimens_Select_ForSearchByDescription]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Specimens_Select_ForSearchByDescription]
GO

/*===========================================================================*\
  Description:
	Returns Specimens data based on a search using the Description parameter.


\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_Specimens_Select_ForSearchByDescription] 
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
	SELECT 	SU.Collection_Unit_Key, 
			SU.Life_Sciences, 
			M.Text, 
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
			AND M.Metadata_Type_Key='SYSTEM0000000006'
	JOIN 	Collection_Unit CU ON SU.Collection_Unit_Key = CU.Collection_Unit_Key 
	 		AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
			OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
	WHERE 	M.Text LIKE @SearchText + '%'

	UPDATE		@SpecimensSearch
	SET			Number = CUN.Number
	FROM 		@SpecimensSearch 		SU
	LEFT JOIN 	Collection_Unit_Number 	CUN ON 	SU.Item_key 	= CUN.Collection_Unit_Key 
											AND CUN.Preferred	= 1

	UPDATE @SpecimensSearch
	SET	Det_Item_Key=CPref.Concept_Key,
		Item_Name = CPref.Published_Term,
		Det_Item_Name=TDet.Plaintext
	FROM @SpecimensSearch SU
	INNER JOIN VW_SpecimenDetsEarth SDE ON SDE.Collection_Unit_Key=SU.Item_Key
	INNER JOIN Concept C ON C.Concept_Key=SDE.Concept_Key
	INNER JOIN Term TDet ON TDet.Term_Key=C.Term_Key
	INNER JOIN Concept CPref ON CPref.Meaning_Key=C.Meaning_Key
		AND CPref.List_Preferred=1
		AND CPref.Concept_Group_Key=C.Concept_Group_Key
	
	UPDATE @SpecimensSearch
	SET	Det_Item_Key=SDL.Taxon_List_Item_Key,
		Item_Name = dbo.ufn_GetFormattedTaxonNameByParams(
				Preferred_Name,
				Preferred_Name_Italic,
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
		ORDER BY Det_Item_Name, Number
	ELSE IF @SortOrderIndex = 1
		SELECT * FROM @SpecimensSearch
		ORDER BY Number, Item_Name
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Specimens_Select_ForSearchByDescription') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_Specimens_Select_ForSearchByDescription'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
       	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByDescription TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByDescription TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByDescription TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByDescription TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByDescription TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
       	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByDescription TO [Dev - JNCC SQL]
END
GO

/*============================================================================*\
  Drop stored proc before re-creating.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_Specimens_Select_ForSearchByAnyNumber') IS NOT NULL
	DROP PROCEDURE dbo.usp_Specimens_Select_ForSearchByAnyNumber
GO

/*=============================================================================*\
  Description:	Returns specimens based on their numbers.

  
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
		[Hint] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[NomenclaturalStatus] VARCHAR(250) NULL  
	)

	INSERT INTO @SpecimensSearch (Item_Key, Life_Sciences, Hint, NomenclaturalStatus) 
	SELECT DISTINCT SU.Collection_Unit_Key, 
					SU.Life_Sciences, 
					CUN.Number, 
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
						WHEN 1 THEN C.Published_Term
						ELSE CPref.Published_Term END,
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

/*============================================================================*\
  Drop stored proc before re-creating.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_Specimens_Select_ForSearchByAnyDetermination') IS NOT NULL
	DROP PROCEDURE dbo.usp_Specimens_Select_ForSearchByAnyDetermination
GO

/*============================================================================*\
  Description:	Returns specimens based on any of their determinations.
  
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
		[Hint] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[NomenclaturalStatus] VARCHAR(250) NULL 
	)

	--Find all specimens with a determination match
	INSERT INTO @SpecimensSearch (Item_Key, Life_Sciences, NomenclaturalStatus) 
	SELECT DISTINCT SU.Collection_Unit_Key, 
					SU.Life_Sciences, 
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
	FROM 	SPECIMEN_UNIT SU
	JOIN 	Collection_Unit CU ON SU.Collection_Unit_Key = CU.Collection_Unit_Key 
		 	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
			OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
	JOIN 	VW_SpecimenDetsEarth SDE ON SU.Collection_Unit_Key = SDE.Collection_Unit_Key
	JOIN 	Concept C ON SDE.Concept_Key = C.Concept_Key
	JOIN 	Concept CSearch ON CSearch.Meaning_Key=C.Meaning_Key
	LEFT JOIN Search_Term ST ON ST.Concept_Key = CSearch.Concept_Key
	WHERE 	(ST.Plaintext LIKE @SearchText + '%'
			 AND SU.Life_Sciences=0) 

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

/*============================================================================*\
  Drop stored proc before re-creating.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_Specimens_Select_ForSearchByType') IS NOT NULL
	DROP PROCEDURE dbo.usp_Specimens_Select_ForSearchByType
GO

/*============================================================================*\
  Description:	Returns specimens based on their specimen type.

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
		[Hint] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[NomenclaturalStatus] VARCHAR(250) NULL  
	)

	INSERT INTO @SpecimensSearch (Item_Key, Life_Sciences, Hint, NomenclaturalStatus) 
	SELECT DISTINCT SU.Collection_Unit_Key, 
					SU.Life_Sciences, 
					T.Plaintext, 
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
	FROM 		SPECIMEN_UNIT SU
	JOIN 		Collection_Unit CU ON SU.Collection_Unit_Key = CU.Collection_Unit_Key 
	 			AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
				OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
	JOIN 		Concept C ON C.Concept_Key = SU.Specimen_Type_Concept_Key
	JOIN 		Concept CSyn ON CSyn.Meaning_Key=C.Meaning_Key
	JOIN 		Term T ON T.Term_Key=CSyn.Term_Key
	LEFT JOIN	Search_Term ST ON ST.Concept_Key = CSyn.Concept_Key
	WHERE 		ST.Plaintext LIKE @Searchtext + '%'

	UPDATE		@SpecimensSearch
	SET			Number = CUN.Number
	FROM 		@SpecimensSearch 		SU
	LEFT JOIN 	Collection_Unit_Number 	CUN ON 	SU.Item_key 	= CUN.Collection_Unit_Key 
											AND CUN.Preferred	= 1

	UPDATE @SpecimensSearch
	SET	Det_Item_Key=CPref.Concept_Key,
		Item_Name = CASE @ShowOriginalSpecimenNames
						WHEN 1 THEN C.Published_Term
						ELSE CPref.Published_Term END,
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

/*============================================================================*\
  Drop stored proc before re-creating.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_Specimens_Select_ForSearchByPreferredNumber') IS NOT NULL
	DROP PROCEDURE dbo.usp_Specimens_Select_ForSearchByPreferredNumber
GO

/*============================================================================*\
  Description:	Returns specimens based on their preferred number.

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
		[Hint] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[NomenclaturalStatus] VARCHAR(250) NULL
	)

	INSERT INTO @SpecimensSearch (Item_Key, Life_Sciences, Hint, NomenclaturalStatus) 
	SELECT DISTINCT	SU.Collection_Unit_Key, 
					SU.Life_Sciences, 
					CUN.Number, 
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
						WHEN 1 THEN C.Published_Term
						ELSE CPref.Published_Term END,
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

/*============================================================================*\
  Drop stored proc before re-creating.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_Specimens_Select_ForSearchByPreferredDetermination') IS NOT NULL
	DROP PROCEDURE dbo.usp_Specimens_Select_ForSearchByPreferredDetermination
GO

/*============================================================================*\
  Description:	Returns specimens based on their preferred determinations.

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
		[Hint] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[NomenclaturalStatus] VARCHAR(250) NULL 
	)

	--Find all specimens with a determination match
	INSERT INTO @SpecimensSearch (Item_Key, Life_Sciences, NomenclaturalStatus) 
	SELECT DISTINCT SU.Collection_Unit_Key, 
					SU.Life_Sciences,
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
	FROM 	SPECIMEN_UNIT SU
	JOIN 	Collection_Unit CU ON SU.Collection_Unit_Key = CU.Collection_Unit_Key 
	 		AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
			OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
	JOIN 	VW_SpecimenDetsEarth SDE ON (SU.Collection_Unit_Key = SDE.Collection_Unit_Key)
			AND (SDE.Preferred_Determination_Key=SDE.Determination_Key)
	JOIN 	Concept C ON SDE.Concept_Key = C.Concept_Key
	JOIN 	Concept CSearch ON CSearch.Meaning_Key=C.Meaning_Key
	JOIN 	Term TSearch ON TSearch.Term_Key=CSearch.Term_Key
	LEFT JOIN Search_Term ST ON ST.Concept_Key = CSearch.Concept_Key
	WHERE (ST.Plaintext LIKE @SearchText + '%' 
			AND SU.Life_Sciences=0) 

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

/*============================================================================*\
  Drop stored proc before re-creating.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_Specimens_Select_ForSearchByPreferredAccNumber') IS NOT NULL
	DROP PROCEDURE dbo.usp_Specimens_Select_ForSearchByPreferredAccNumber
GO

/*============================================================================*\
  Description:	Returns specimens based on their preferred accession numbers.

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
		[Hint] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[NomenclaturalStatus] VARCHAR(250) NULL 
	)

	INSERT INTO @SpecimensSearch (Item_Key, Life_Sciences, Hint, NomenclaturalStatus) 
	SELECT DISTINCT SU.Collection_Unit_Key, 
					SU.Life_Sciences, 
					M.Number,
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
						WHEN 1 THEN C.Published_Term
						ELSE CPref.Published_Term END,
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

/*============================================================================*\
  Drop stored proc before re-creating.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_Specimens_Select_ForSearchByObservationLocation') IS NOT NULL
	DROP PROCEDURE dbo.usp_Specimens_Select_ForSearchByObservationLocation
GO

/*============================================================================*\
  Description:	Returns specimens based on their observation location.

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
		[Hint] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[NomenclaturalStatus] VARCHAR(250) NULL  
	)

	INSERT INTO		@SpecimensSearch 
					(
						Item_Key, 
						Life_Sciences,
						NomenclaturalStatus
					) 
	SELECT			DISTINCT 
					SU.Collection_Unit_Key, 
					SU.Life_Sciences, 
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
																THEN C.Published_Term
																ELSE CPref.Published_Term
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

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Specimens_Select_ForSearchByNomenclaturalStatus]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Specimens_Select_ForSearchByNomenclaturalStatus]
GO

/*===========================================================================*\
  Description:
	Returns Specimens data based on the search parameter for Nomenclatural Status.


\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_Specimens_Select_ForSearchByNomenclaturalStatus] 
	@UserDomainMask				INT,
	@SessionID					CHAR(16),
	@ShowCommonNames			BIT,
	@ShowOriginalSpecimenNames	BIT,
	@SearchText					VARCHAR(50),
	@SortOrderIndex				TINYINT
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
		[Hint] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[NomenclaturalStatus] VARCHAR(250) NULL   
	)

	INSERT INTO		@SpecimensSearch
					(
						Item_Key,
						Life_Sciences,
						NomenclaturalStatus
					) 
	SELECT			DISTINCT
					SU.Collection_Unit_Key,
					SU.Life_Sciences, 
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
	FROM			dbo.VW_ConceptTerm				AS		CT
	LEFT JOIN		dbo.Search_Term					AS		ST
	ON				ST.Concept_Key					=		CT.Concept_Key
	INNER JOIN		dbo.Determination				AS		D
	ON				CT.Concept_Key					=		D.Nomenclatural_Status_Concept_Key
	INNER JOIN		dbo.Specimen_Unit				AS		SU
	ON				D.Specimen_Collection_Unit_Key	=		SU.Collection_Unit_Key
	INNER JOIN		Collection_Unit CU 
	ON				SU.Collection_Unit_Key = CU.Collection_Unit_Key 
				 	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
					OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
	WHERE			ST.Plaintext					LIKE	@SearchText + '%'

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
																THEN C.Published_Term
																ELSE CPref.Published_Term
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

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Specimens_Select_ForSearchByNomenclaturalStatus') AND SysStat & 0xf = 4)
BEGIN
   	PRINT 'Setting up security on procedure usp_Specimens_Select_ForSearchByNomenclaturalStatus'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
       	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByNomenclaturalStatus TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByNomenclaturalStatus TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByNomenclaturalStatus TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByNomenclaturalStatus TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByNomenclaturalStatus TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
       	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByNomenclaturalStatus TO [Dev - JNCC SQL]
END
GO

/*============================================================================*\
  Drop stored proc before re-creating.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_Specimens_Select_ForSearchByMetadata') IS NOT NULL
	DROP PROCEDURE dbo.usp_Specimens_Select_ForSearchByMetadata
GO

/*============================================================================*\
  Description:	Returns specimens based on their metadata of the specified type.

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
						WHEN 1 THEN C.Published_Term
						ELSE CPref.Published_Term END,
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

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Specimens_Select_ForSearchByGeographicInformation]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Specimens_Select_ForSearchByGeographicInformation]
GO

/*===========================================================================*\
  Description:
	Returns Specimens data based on the search parameter for Geographic Information.


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

/*============================================================================*\
  Drop stored proc before re-creating.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_Specimens_Select_ForSearchByGatheringLocation') IS NOT NULL
	DROP PROCEDURE dbo.usp_Specimens_Select_ForSearchByGatheringLocation
GO

/*============================================================================*\
  Description:	Returns specimens based on their gathering location.

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
		[Hint] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[NomenclaturalStatus] VARCHAR(250) NULL  
	)

	INSERT INTO @SpecimensSearch (Item_Key, Life_Sciences, Hint, NomenclaturalStatus) 
	SELECT 	DISTINCT SU.Collection_Unit_Key, SU.Life_Sciences, LN.Item_Name, 
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
	$Revision: 3 $
	$Date: 30/10/12 16:48 $
	$Author: Alexanderpadley $
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

	-- Use temp table to build output, so silly data errors such as duplicate accessions
	-- don't duplicate in the list. A temporary table has been used rather than a
	-- table variable since it stores a large number of records.
	CREATE TABLE #Search
	(
		-- defined as primary key to create an index on this column
		[Item_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS PRIMARY KEY,
		[Det_Item_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[Det_Item_Name] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[Item_Name] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[Life_Sciences] [bit] NULL,
		[Number] [varchar] (30) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[Hint] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[NomenclaturalStatus] VARCHAR(250) NULL 
	)

	IF @Key IS NULL
		IF object_id('tempdb..#TempFilter') IS NOT NULL
			-- Display data for a list of keys in the #TempFilter table
			INSERT INTO #Search (Item_Key, Life_Sciences, NomenclaturalStatus) 
			SELECT DISTINCT SU.Collection_Unit_Key, SU.Life_Sciences,
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
			FROM 		Specimen_Unit 	SU
			INNER JOIN 	Collection_Unit CU 	ON 	SU.Collection_Unit_Key = CU.Collection_Unit_Key 
											AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
											OR 	(CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
			INNER JOIN 	#TempFilter 		ON 	#TempFilter.ItemKey=CU.Collection_Unit_Key
		ELSE
			INSERT INTO #Search (Item_Key, Life_Sciences, NomenclaturalStatus) 
			SELECT DISTINCT SU.Collection_Unit_Key, SU.Life_Sciences,
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
			FROM 		Specimen_Unit 	SU
			INNER JOIN 	Collection_Unit CU 	ON 	SU.Collection_Unit_Key = CU.Collection_Unit_Key 
											AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
											OR 	(CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
	ELSE
		-- Display data for a single key
		INSERT INTO #Search (Item_Key, Life_Sciences) 
		SELECT DISTINCT SU.Collection_Unit_Key, SU.Life_Sciences
		FROM 		Specimen_Unit 	SU
		INNER JOIN 	Collection_Unit CU 	ON 	SU.Collection_Unit_Key = CU.Collection_Unit_Key 
										AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
										OR 	(CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
		WHERE SU.Collection_Unit_Key = @Key

	UPDATE		#Search
	SET			Number = CUN.Number
	FROM 		#Search 				SU
	LEFT JOIN 	Collection_Unit_Number 	CUN ON 	SU.Item_key 	= CUN.Collection_Unit_Key 
											AND CUN.Preferred	= 1

	UPDATE 	#Search
	SET		Det_Item_Key = CPref.Concept_Key,
			Item_Name = CASE @ShowOriginalSpecimenNames
							WHEN 1 THEN C.Published_Term
							ELSE CPref.Published_Term END,
			Det_Item_Name = TDet.Plaintext
	FROM #Search SU
	INNER JOIN VW_SpecimenDetsEarth SDE ON SDE.Collection_Unit_Key = SU.Item_Key
			AND SDE.Preferred_Determination_key	= SDE.Determination_Key
	INNER JOIN Concept C ON C.Concept_Key=SDE.Concept_Key
	INNER JOIN Term TDet ON TDet.Term_Key=C.Term_Key
	INNER JOIN Concept CPref ON CPref.Meaning_Key=C.Meaning_Key
			AND CPref.List_Preferred=1
			AND CPref.Concept_Group_Key=C.Concept_Group_Key
	
	UPDATE #Search
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
	FROM #Search SU
	INNER JOIN VW_SpecimenDetsLife SDL ON SDL.Collection_Unit_Key=SU.Item_Key
		AND SDL.Preferred_Taxon_Determination_key=SDL.Taxon_Determination_Key
	INNER JOIN Index_Taxon_Name ITN	ON ITN.Taxon_List_Item_Key=SDL.Taxon_List_Item_Key

	-- Select table and sort appropriately
	IF @SortOrderIndex = 0
		SELECT * FROM #Search
		ORDER BY Det_Item_Name, Number
	ELSE IF @SortOrderIndex = 1
		SELECT * FROM #Search
		ORDER BY Number, Item_Name

	DROP TABLE #Search
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