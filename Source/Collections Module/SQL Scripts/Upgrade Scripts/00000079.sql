SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

/*============================================================================*\
Drop stored proc before re-creating.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_Domain_Get_ForConcept') IS NOT NULL
    DROP PROCEDURE dbo.usp_Domain_Get_ForConcept
GO

/*============================================================================*\
Description:    Gets the domain key for the specified concept.

Parameters:     @ConceptKey             Identifies the concept.
                @DomainKey              Identifies the corresponding domain.

Created:        September 2017
\*============================================================================*/
CREATE PROCEDURE dbo.usp_Domain_Get_ForConcept
    @ConceptKey                         CHAR(16),
    @DomainKey                          CHAR(16) OUTPUT
AS
    SELECT      @DomainKey              =   l.Domain_Key
    FROM        dbo.Concept             AS  c
    INNER JOIN  dbo.Concept_Group       AS  g
    ON          g.Concept_Group_Key     =   c.Concept_Group_Key
    INNER JOIN  dbo.Local_Domain        AS  l
    ON          l.Local_Domain_Key      =   g.Local_Domain_Key
    WHERE       c.Concept_Key           =   @ConceptKey
GO

/*============================================================================*\
Grant permissions.
\*============================================================================*/
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
    GRANT EXECUTE ON dbo.usp_Domain_Get_ForConcept TO R2k_AddOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
    GRANT EXECUTE ON dbo.usp_Domain_Get_ForConcept TO R2k_Administrator
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
    GRANT EXECUTE ON dbo.usp_Domain_Get_ForConcept TO R2k_FullEdit
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
    GRANT EXECUTE ON dbo.usp_Domain_Get_ForConcept TO R2k_RecordCardsOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
    GRANT EXECUTE ON dbo.usp_Domain_Get_ForConcept TO "Dev - JNCC SQL"
GO

SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

/*============================================================================*\
Drop stored proc before re-creating.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_Multimedia_Select_Preferred') IS NOT NULL
    DROP PROCEDURE dbo.usp_Multimedia_Select_Preferred
GO

/*============================================================================*\
Description:    Gets the preferred multimedia item for the specified record.

Parameters:     @TableName              Names the table containing the record.
                @RecordKey              Identifies the record.

Created:        September 2017
\*============================================================================*/
CREATE PROCEDURE dbo.usp_Multimedia_Select_Preferred
    @TableName                          VARCHAR(50),
    @RecordKey                          CHAR(16) OUTPUT
AS
    SELECT      f.SOURCE_KEY,
                f.FILE_NAME,
                f.Title,
                f.Timestamp
    FROM        dbo.Source_Join         AS  j
    INNER JOIN  dbo.SOURCE_FILE         AS  f
    ON          f.SOURCE_KEY            =   j.Source_Key
    AND         f.Preferred             =   1
    WHERE       j.Table_Name            =   @TableName
    AND         j.Record_Key            =   @RecordKey
GO

/*============================================================================*\
Grant permissions.
\*============================================================================*/
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
    GRANT EXECUTE ON dbo.usp_Multimedia_Select_Preferred TO R2k_AddOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
    GRANT EXECUTE ON dbo.usp_Multimedia_Select_Preferred TO R2k_Administrator
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
    GRANT EXECUTE ON dbo.usp_Multimedia_Select_Preferred TO R2k_FullEdit
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
    GRANT EXECUTE ON dbo.usp_Multimedia_Select_Preferred TO R2k_RecordCardsOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
    GRANT EXECUTE ON dbo.usp_Multimedia_Select_Preferred TO "Dev - JNCC SQL"
GO

SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

/*============================================================================*\
  Drop function before re-creating.
\*============================================================================*/
IF OBJECT_ID('dbo.ufn_EscapeLikePattern') IS NOT NULL
    DROP FUNCTION dbo.ufn_EscapeLikePattern
GO

/*============================================================================*\
  Description:  Escapes any LIKE wildcard characters in the given string,
                using '\' as the escape character.

  Created:      September 2017
\*============================================================================*/
CREATE FUNCTION dbo.ufn_EscapeLikePattern(
    @Text                               NVARCHAR(MAX))
    RETURNS                             NVARCHAR(MAX)
AS
BEGIN
    RETURN      REPLACE(REPLACE(REPLACE(REPLACE(@Text,
                '\', '\\'),
                '[', '\['),
                '_', '\_'),
                '%', '\%')
END
GO

/*============================================================================*\
  Grant permissions.
\*============================================================================*/
IF OBJECT_ID('dbo.ufn_EscapeLikePattern') IS NOT NULL
BEGIN
    PRINT 'Setting up security on function ufn_EscapeLikePattern'
    IF EXISTS (SELECT * FROM sysusers WHERE NAME = 'R2k_AddOnly')
        GRANT EXECUTE ON dbo.ufn_EscapeLikePattern TO R2k_AddOnly
    IF EXISTS (SELECT * FROM sysusers WHERE NAME = 'R2k_Administrator')
        GRANT EXECUTE ON dbo.ufn_EscapeLikePattern TO R2k_Administrator
    IF EXISTS (SELECT * FROM sysusers WHERE NAME = 'R2k_FullEdit')
        GRANT EXECUTE ON dbo.ufn_EscapeLikePattern TO R2k_FullEdit
    IF EXISTS (SELECT * FROM sysusers WHERE NAME = 'R2k_ReadOnly')
        GRANT EXECUTE ON dbo.ufn_EscapeLikePattern TO R2k_ReadOnly
    IF EXISTS (SELECT * FROM sysusers WHERE NAME = 'R2k_RecordCardsOnly')
        GRANT EXECUTE ON dbo.ufn_EscapeLikePattern TO R2k_RecordCardsOnly
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
					STUFF(( SELECT      ' ' + n.Number
							FROM        dbo.Collection_Unit_Number      AS  n
							WHERE       n.Collection_Unit_Key           =   SU.Collection_Unit_Key
							AND         n.Type_Concept_Key              <>  'SYSTEM0000000001'
							ORDER BY    n.Collection_Unit_Number_Key    DESC
							FOR XML PATH(''), TYPE
						).value('(./text())[1]', 'NVARCHAR(MAX)'), 1, 1, '') AS Number,
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
	JOIN	Collection_Unit_Number CUN
	ON		SU.Collection_Unit_key = CUN.Collection_Unit_Key 
	AND		CUN.Number LIKE dbo.ufn_EscapeLikePattern(@SearchText) + '%' ESCAPE '\'

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

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Collections_Select_ForLinkedOther]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Collections_Select_ForLinkedOther]
GO

/*===========================================================================*\
  Description:	Returns Related Collections for a specified Collection

  Parameters:	
	@ParentKey 	When specified, only the records associated with the parent key are returned
	@SortOrderIndex	Index determining Sort Order
	@UserDomainMask	User's Domain Mask restricting which records may be returned
	@SessionID 	User's SessionID

  Created:	August 2003
\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Collections_Select_ForLinkedOther] 
	@UserDomainMask INT,
	@SessionID CHAR(16),
	@SortOrderIndex TINYINT,
	@ParentKey CHAR(16)
AS

SET NOCOUNT ON

IF @SortOrderIndex = 0
	SELECT DISTINCT --DISTINCT BECAUSE OF TWO MOVEMENT_DIRECTION records
		CUR.Collection_Unit_Relation_Key AS Item_Key,
		CUR.Collection_Unit_Relation_Key AS Join_Key,
		CASE WHEN C1.Collection_Unit_Key IS NOT NULL THEN C1.Collection_Unit_Key ELSE C2.Collection_Unit_Key END AS Hyperlink_Item_Key, 
		CASE WHEN C1.Collection_Unit_Key IS NOT NULL THEN C1.Collection_Unit_Key ELSE C2.Collection_Unit_Key END AS Drag_Drop_Item_Key, 
		CASE WHEN C1.Collection_Unit_Key IS NOT NULL THEN C1.Item_Name ELSE C2.Item_Name END AS Item_Name,
		CASE WHEN C1.Collection_Unit_Key IS NOT NULL THEN M1.Number ELSE M2.Number END AS Number,
		CASE WHEN C1.Collection_Unit_Key IS NOT NULL THEN C1.Collation_From_Vague_Date_Start ELSE C2.Collation_From_Vague_Date_Start END AS Collation_From_Vague_Date_Start,
		CASE WHEN C1.Collection_Unit_Key IS NOT NULL THEN C1.Collation_From_Vague_Date_End ELSE C2.Collation_From_Vague_Date_End END AS Collation_From_Vague_Date_End,
		CASE WHEN C1.Collection_Unit_Key IS NOT NULL THEN C1.Collation_From_Vague_Date_Type ELSE C2.Collation_From_Vague_Date_Type END AS Collation_From_Vague_Date_Type

	FROM 		Collection_Unit_Relation CUR
	INNER JOIN	Thesaurus_Relation_Type TRT ON CUR.Thesaurus_Relation_Type_Key = TRT.Thesaurus_Relation_Type_Key
	INNER JOIN	Semantic_Relation SR ON TRT.Semantic_Relation_Key = SR.Semantic_Relation_Key --AND SR.Unidirectional = 0
	LEFT JOIN 	(Collection C1
			INNER JOIN	Collection_Unit CU1
				ON C1.Collection_Unit_Key = CU1.Collection_Unit_Key
				AND ((CU1.Domain_Mask & @UserDomainMask > 0) OR (CU1.Entered_Session_ID = @SessionID) 
					OR (CU1.Changed_Session_ID = @SessionID) OR (CU1.Domain_Mask = 0))
			)
		ON CUR.To_Collection_Unit_Key = C1.Collection_Unit_Key AND CUR.From_Collection_Unit_Key = @ParentKey

	LEFT JOIN	(Movement_Collection_Unit MCU1
			INNER JOIN	Movement_Direction MD1
				ON MCU1.Movement_Direction_Key = MD1.Movement_Direction_Key 
				AND (MD1.Outbound = 0)

			INNER JOIN	Movement M1 ON MD1.Movement_Key = M1.Movement_Key AND M1.Movement_Type IN (0, 1)
			LEFT JOIN	(Concept CON1
					INNER JOIN Term T1 ON CON1.Term_Key = T1.Term_Key
					)
				ON CON1.Concept_Key = 'SYSTEM0000000006' --ACCESSION NUMBER
			)
		ON CU1.Collection_Unit_Key = MCU1.Collection_Unit_Key

	LEFT JOIN 	(Collection C2
			INNER JOIN 	Collection_Unit CU2
				ON C2.Collection_Unit_Key = CU2.Collection_Unit_Key
				AND ((CU2.Domain_Mask & @UserDomainMask > 0) OR (CU2.Entered_Session_ID = @SessionID) 
					OR (CU2.Changed_Session_ID = @SessionID) OR (CU2.Domain_Mask = 0))
			)
		ON CUR.From_Collection_Unit_Key = C2.Collection_Unit_Key AND CUR.To_Collection_Unit_Key = @ParentKey
		AND SR.Unidirectional = 0

	LEFT JOIN	(Movement_Collection_Unit MCU2
			INNER JOIN	Movement_Direction MD2
				ON MCU2.Movement_Direction_Key = MD2.Movement_Direction_Key 
				AND (MD2.Outbound = 0)

			INNER JOIN	Movement M2 ON MD2.Movement_Key = M2.Movement_Key AND M2.Movement_Type IN (0, 1)
			LEFT JOIN	(Concept CON2
					INNER JOIN Term T2 ON CON2.Term_Key = T2.Term_Key
					)
				ON CON2.Concept_Key = 'SYSTEM0000000006' --ACCESSION NUMBER
			)
		ON CU2.Collection_Unit_Key = MCU2.Collection_Unit_Key

	WHERE 	(C1.Collection_Unit_Key IS NOT NULL) 
	OR 	(C2.Collection_Unit_Key IS NOT NULL)

	ORDER BY Collation_From_Vague_Date_Start DESC, Collation_From_Vague_Date_End DESC, Item_Name, Number

ELSE IF @SortOrderIndex = 1
	SELECT DISTINCT --DISTINCT BECAUSE OF TWO MOVEMENT_DIRECTION records
		CUR.Collection_Unit_Relation_Key AS Item_Key,
		CUR.Collection_Unit_Relation_Key AS Join_Key,
		CASE WHEN C1.Collection_Unit_Key IS NOT NULL THEN C1.Collection_Unit_Key ELSE C2.Collection_Unit_Key END AS Hyperlink_Item_Key, 
		CASE WHEN C1.Collection_Unit_Key IS NOT NULL THEN C1.Collection_Unit_Key ELSE C2.Collection_Unit_Key END AS Drag_Drop_Item_Key, 
		CASE WHEN C1.Collection_Unit_Key IS NOT NULL THEN C1.Item_Name ELSE C2.Item_Name END AS Item_Name,
		CASE WHEN C1.Collection_Unit_Key IS NOT NULL THEN M1.Number ELSE M2.Number END AS Number,
		CASE WHEN C1.Collection_Unit_Key IS NOT NULL THEN C1.Collation_From_Vague_Date_Start ELSE C2.Collation_From_Vague_Date_Start END AS Collation_From_Vague_Date_Start,
		CASE WHEN C1.Collection_Unit_Key IS NOT NULL THEN C1.Collation_From_Vague_Date_End ELSE C2.Collation_From_Vague_Date_End END AS Collation_From_Vague_Date_End,
		CASE WHEN C1.Collection_Unit_Key IS NOT NULL THEN C1.Collation_From_Vague_Date_Type ELSE C2.Collation_From_Vague_Date_Type END AS Collation_From_Vague_Date_Type

	FROM 		Collection_Unit_Relation CUR
	INNER JOIN	Thesaurus_Relation_Type TRT ON CUR.Thesaurus_Relation_Type_Key = TRT.Thesaurus_Relation_Type_Key
	INNER JOIN	Semantic_Relation SR ON TRT.Semantic_Relation_Key = SR.Semantic_Relation_Key --AND SR.Unidirectional = 0
	LEFT JOIN 	(Collection C1
			INNER JOIN	Collection_Unit CU1
				ON C1.Collection_Unit_Key = CU1.Collection_Unit_Key
				AND ((CU1.Domain_Mask & @UserDomainMask > 0) OR (CU1.Entered_Session_ID = @SessionID) 
					OR (CU1.Changed_Session_ID = @SessionID) OR (CU1.Domain_Mask = 0))
			)
		ON CUR.To_Collection_Unit_Key = C1.Collection_Unit_Key AND CUR.From_Collection_Unit_Key = @ParentKey

	LEFT JOIN	(Movement_Collection_Unit MCU1
			INNER JOIN	Movement_Direction MD1
				ON MCU1.Movement_Direction_Key = MD1.Movement_Direction_Key 
				AND (MD1.Outbound = 0)

			INNER JOIN	Movement M1 ON MD1.Movement_Key = M1.Movement_Key AND M1.Movement_Type IN (0, 1)
			LEFT JOIN	(Concept CON1
					INNER JOIN Term T1 ON CON1.Term_Key = T1.Term_Key
					)
				ON CON1.Concept_Key = 'SYSTEM0000000006' --ACCESSION NUMBER
			)
		ON CU1.Collection_Unit_Key = MCU1.Collection_Unit_Key

	LEFT JOIN 	(Collection C2
			INNER JOIN 	Collection_Unit CU2
				ON C2.Collection_Unit_Key = CU2.Collection_Unit_Key
				AND ((CU2.Domain_Mask & @UserDomainMask > 0) OR (CU2.Entered_Session_ID = @SessionID) 
					OR (CU2.Changed_Session_ID = @SessionID) OR (CU2.Domain_Mask = 0))
			)
		ON CUR.From_Collection_Unit_Key = C2.Collection_Unit_Key AND CUR.To_Collection_Unit_Key = @ParentKey
		AND SR.Unidirectional = 0

	LEFT JOIN	(Movement_Collection_Unit MCU2
			INNER JOIN	Movement_Direction MD2
				ON MCU2.Movement_Direction_Key = MD2.Movement_Direction_Key 
				AND (MD2.Outbound = 0)

			INNER JOIN	Movement M2 ON MD2.Movement_Key = M2.Movement_Key AND M2.Movement_Type IN (0, 1)
			LEFT JOIN	(Concept CON2
					INNER JOIN Term T2 ON CON2.Term_Key = T2.Term_Key
					)
				ON CON2.Concept_Key = 'SYSTEM0000000006' --ACCESSION NUMBER
			)
		ON CU2.Collection_Unit_Key = MCU2.Collection_Unit_Key

	WHERE 	(C1.Collection_Unit_Key IS NOT NULL) 
	OR 	(C2.Collection_Unit_Key IS NOT NULL)

	ORDER BY Item_Name, Number


ELSE IF @SortOrderIndex = 2
	SELECT DISTINCT --DISTINCT BECAUSE OF TWO MOVEMENT_DIRECTION records
		CUR.Collection_Unit_Relation_Key AS Item_Key,
		CUR.Collection_Unit_Relation_Key AS Join_Key,
		CASE WHEN C1.Collection_Unit_Key IS NOT NULL THEN C1.Collection_Unit_Key ELSE C2.Collection_Unit_Key END AS Hyperlink_Item_Key, 
		CASE WHEN C1.Collection_Unit_Key IS NOT NULL THEN C1.Collection_Unit_Key ELSE C2.Collection_Unit_Key END AS Drag_Drop_Item_Key, 
		CASE WHEN C1.Collection_Unit_Key IS NOT NULL THEN C1.Item_Name ELSE C2.Item_Name END AS Item_Name,
		CASE WHEN C1.Collection_Unit_Key IS NOT NULL THEN M1.Number ELSE M2.Number END AS Number,
		CASE WHEN C1.Collection_Unit_Key IS NOT NULL THEN C1.Collation_From_Vague_Date_Start ELSE C2.Collation_From_Vague_Date_Start END AS Collation_From_Vague_Date_Start,
		CASE WHEN C1.Collection_Unit_Key IS NOT NULL THEN C1.Collation_From_Vague_Date_End ELSE C2.Collation_From_Vague_Date_End END AS Collation_From_Vague_Date_End,
		CASE WHEN C1.Collection_Unit_Key IS NOT NULL THEN C1.Collation_From_Vague_Date_Type ELSE C2.Collation_From_Vague_Date_Type END AS Collation_From_Vague_Date_Type

	FROM 		Collection_Unit_Relation CUR
	INNER JOIN	Thesaurus_Relation_Type TRT ON CUR.Thesaurus_Relation_Type_Key = TRT.Thesaurus_Relation_Type_Key
	INNER JOIN	Semantic_Relation SR ON TRT.Semantic_Relation_Key = SR.Semantic_Relation_Key --AND SR.Unidirectional = 0
	LEFT JOIN 	(Collection C1
			INNER JOIN	Collection_Unit CU1
				ON C1.Collection_Unit_Key = CU1.Collection_Unit_Key
				AND ((CU1.Domain_Mask & @UserDomainMask > 0) OR (CU1.Entered_Session_ID = @SessionID) 
					OR (CU1.Changed_Session_ID = @SessionID) OR (CU1.Domain_Mask = 0))
			)
		ON CUR.To_Collection_Unit_Key = C1.Collection_Unit_Key AND CUR.From_Collection_Unit_Key = @ParentKey

	LEFT JOIN	(Movement_Collection_Unit MCU1
			INNER JOIN	Movement_Direction MD1
				ON MCU1.Movement_Direction_Key = MD1.Movement_Direction_Key 
				AND (MD1.Outbound = 0)

			INNER JOIN	Movement M1 ON MD1.Movement_Key = M1.Movement_Key AND M1.Movement_Type IN (0, 1)
			LEFT JOIN	(Concept CON1
					INNER JOIN Term T1 ON CON1.Term_Key = T1.Term_Key
					)
				ON CON1.Concept_Key = 'SYSTEM0000000006' --ACCESSION NUMBER
			)
		ON CU1.Collection_Unit_Key = MCU1.Collection_Unit_Key

	LEFT JOIN 	(Collection C2
			INNER JOIN 	Collection_Unit CU2
				ON C2.Collection_Unit_Key = CU2.Collection_Unit_Key
				AND ((CU2.Domain_Mask & @UserDomainMask > 0) OR (CU2.Entered_Session_ID = @SessionID) 
					OR (CU2.Changed_Session_ID = @SessionID) OR (CU2.Domain_Mask = 0))
			)
		ON CUR.From_Collection_Unit_Key = C2.Collection_Unit_Key AND CUR.To_Collection_Unit_Key = @ParentKey
		AND SR.Unidirectional = 0

	LEFT JOIN	(Movement_Collection_Unit MCU2
			INNER JOIN	Movement_Direction MD2
				ON MCU2.Movement_Direction_Key = MD2.Movement_Direction_Key 
				AND (MD2.Outbound = 0)

			INNER JOIN	Movement M2 ON MD2.Movement_Key = M2.Movement_Key AND M2.Movement_Type IN (0, 1)
			LEFT JOIN	(Concept CON2
					INNER JOIN Term T2 ON CON2.Term_Key = T2.Term_Key
					)
				ON CON2.Concept_Key = 'SYSTEM0000000006' --ACCESSION NUMBER
			)
		ON CU2.Collection_Unit_Key = MCU2.Collection_Unit_Key

	WHERE 	(C1.Collection_Unit_Key IS NOT NULL) 
	OR 	(C2.Collection_Unit_Key IS NOT NULL)

	ORDER BY Number

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Collections_Select_ForLinkedOther') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Collections_Select_ForLinkedOther'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Collections_Select_ForLinkedOther TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Collections_Select_ForLinkedOther TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Collections_Select_ForLinkedOther TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Collections_Select_ForLinkedOther TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Collections_Select_ForLinkedOther TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Collections_Select_ForLinkedOther TO [Dev - JNCC SQL]
END
GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Stores_Select_ForCollection]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Stores_Select_ForCollection]
GO

/*===========================================================================*\
  Description:	Returns Stores data to the CollectionsBrowser for a given Collection.

  Parameters:
	@ParentKey 	When specified, only the records associated with the parent key are returned
	@UserDomainMask	User's Domain Mask restricting which records may be returned
	@SessionID 	User's SessionID
	@SortOrderIndex	Index determining Sort Order

  Created:	August 2003
\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Stores_Select_ForCollection] 
	@UserDomainMask INT,
	@SessionID CHAR(16),
	@SortOrderIndex TINYINT,
	@ParentKey CHAR(16)
AS

SET NOCOUNT ON

IF @SortOrderIndex = 0
	SELECT DISTINCT S.Collection_Unit_Key AS Item_Key, 
				S.Item_Name + IsNull(' - ' + CU2.Current_Location_Code, IsNull(' - ' + CU2.Usual_Location_Code, '')) AS Item_Name,  
				Number, 
				S.Collection_Unit_Key AS Join_Key
	FROM		Specimen_Unit SU
	INNER JOIN	Collection_Unit CU ON SU.Collection_Unit_Key = CU.Collection_Unit_Key 
									AND Parent_Collection_Collection_Unit_Key = @ParentKey
	INNER JOIN	Store S ON CU.Current_Container_Collection_Unit_Key = S.Collection_Unit_Key
	INNER JOIN	Collection_Unit AS CU2 ON CU2.Collection_Unit_Key = S.Collection_Unit_Key
									AND ((CU2.Domain_Mask & @UserDomainMask > 0) OR (CU2.Entered_Session_ID = @SessionID) 
										OR (CU2.Changed_Session_ID = @SessionID) OR (CU2.Domain_Mask = 0))
	LEFT JOIN 	Collection_Unit_Number CUN 
		ON S.Collection_Unit_Key = CUN.Collection_Unit_Key 
		AND CUN.Type_Concept_Key = 'SYSTEM0000000001'  --REGISTRATION NUMBER
		AND CUN.Preferred = 1

	ORDER BY Item_Name, Number
ELSE 
IF @SortOrderIndex = 1
	SELECT DISTINCT S.Collection_Unit_Key AS Item_Key,
				S.Item_Name + IsNull(' - ' + CU.Current_Location_Code, IsNull(' - ' + CU.Usual_Location_Code, '')) AS Item_Name,  
				Number, 
				S.Collection_Unit_Key AS Join_Key
	FROM		Specimen_Unit SU
	INNER JOIN	Collection_Unit CU ON SU.Collection_Unit_Key = CU.Collection_Unit_Key 
									AND Parent_Collection_Collection_Unit_Key = @ParentKey
	INNER JOIN	Store S ON CU.Current_Container_Collection_Unit_Key = S.Collection_Unit_Key
	INNER JOIN	Collection_Unit AS CU2 ON CU2.Collection_Unit_Key = S.Collection_Unit_Key
									AND ((CU2.Domain_Mask & @UserDomainMask > 0) OR (CU2.Entered_Session_ID = @SessionID) 
										OR (CU2.Changed_Session_ID = @SessionID) OR (CU2.Domain_Mask = 0))
	LEFT JOIN 	Collection_Unit_Number CUN 
		ON S.Collection_Unit_Key = CUN.Collection_Unit_Key 
		AND CUN.Type_Concept_Key = 'SYSTEM0000000001'  --REGISTRATION NUMBER
		AND CUN.Preferred = 1

	ORDER BY Number, Item_Name
ELSE 
IF @SortOrderIndex = 2
	SELECT DISTINCT S.Collection_Unit_Key AS Item_Key, 
				S.Item_Name + IsNull(' - ' + CU.Current_Location_Code, IsNull(' - ' + CU.Usual_Location_Code, '')) AS Item_Name,  
				Number, 
				S.Collection_Unit_Key AS Join_Key, 
				CU.Current_Location_Code
	FROM		Specimen_Unit SU
	INNER JOIN	Collection_Unit CU ON SU.Collection_Unit_Key = CU.Collection_Unit_Key 
									AND Parent_Collection_Collection_Unit_Key = @ParentKey
	INNER JOIN	Store S ON CU.Current_Container_Collection_Unit_Key = S.Collection_Unit_Key
	INNER JOIN	Collection_Unit AS CU2 ON CU2.Collection_Unit_Key = S.Collection_Unit_Key
									AND ((CU2.Domain_Mask & @UserDomainMask > 0) OR (CU2.Entered_Session_ID = @SessionID) 
										OR (CU2.Changed_Session_ID = @SessionID) OR (CU2.Domain_Mask = 0))
	LEFT JOIN 	Collection_Unit_Number CUN 
		ON S.Collection_Unit_Key = CUN.Collection_Unit_Key 
		AND CUN.Type_Concept_Key = 'SYSTEM0000000001'  --REGISTRATION NUMBER
		AND CUN.Preferred = 1

	ORDER BY CU.Current_Location_Code, Item_Name, Number
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Stores_Select_ForCollection') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Stores_Select_ForCollection'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Stores_Select_ForCollection TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Stores_Select_ForCollection TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Stores_Select_ForCollection TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Stores_Select_ForCollection TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Stores_Select_ForCollection TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Stores_Select_ForCollection TO [Dev - JNCC SQL]
END
GO