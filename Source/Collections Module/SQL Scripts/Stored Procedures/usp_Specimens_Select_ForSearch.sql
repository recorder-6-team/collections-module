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
    $Revision: 23 $
    $Date: 13/03/13 8:38 $
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