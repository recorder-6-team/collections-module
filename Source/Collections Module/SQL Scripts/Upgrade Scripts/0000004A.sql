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
    $Date: 31/01/11 14:09 $
    $Author: Jamesbichard $

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
	[Life_Sciences] [bit] NULL
)

--Find all specimens with a determination match
INSERT INTO 
	@SpecimensSearch (Item_Key, Life_Sciences, SearchTerm, DisplayTerm) 
SELECT DISTINCT 
	SU.Collection_Unit_Key					COLLATE database_default, 
	SU.Life_Sciences,
	CASE Su.Life_Sciences 
		WHEN 0 THEN TSearch.Plaintext		
		ELSE ITN.Actual_Name				
	END COLLATE database_default AS SearchTerm,
	CASE Su.Life_Sciences 
		WHEN 0 THEN TSearch.Item_Name		
		ELSE CASE ITN.Actual_Name_Italic	
			WHEN 1 THEN '<i>' + ITN.Actual_Name + '</i>' 
			ELSE ITN.Actual_Name			
		END
	END COLLATE database_default AS DisplayTerm
	
FROM SPECIMEN_UNIT SU
INNER JOIN Collection_Unit CU ON SU.Collection_Unit_Key = CU.Collection_Unit_Key 
 	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
	OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
LEFT JOIN VW_SpecimenDetsEarth SDE ON SU.Collection_Unit_Key = SDE.Collection_Unit_Key
LEFT JOIN Concept C ON SDE.Concept_Key = C.Concept_Key
LEFT JOIN Concept CSearch ON CSearch.Meaning_Key=C.Meaning_Key
LEFT JOIN Term TSearch ON TSearch.Term_Key=CSearch.Term_Key
LEFT JOIN VW_SpecimenDetsLife SDL ON SU.Collection_Unit_Key = SDL.Collection_Unit_Key
LEFT JOIN Index_Taxon_Synonym ITS ON ITS.Taxon_List_Item_Key=SDL.Taxon_List_Item_Key
LEFT JOIN INDEX_TAXON_NAME ITN	ON ITS.Synonym_List_Item_Key = ITN.Taxon_List_Item_Key
WHERE 
	(TSearch.Plaintext LIKE @SearchText + '%' AND SU.Life_Sciences=0) 
	OR 
	(ITN.Actual_Name LIKE @SearchText + '%' AND SU.Life_Sciences=1)

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