If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Determinations_Select_ForSearch]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Determinations_Select_ForSearch]
GO

CREATE PROCEDURE [dbo].[usp_Determinations_Select_ForSearch] 
@SearchText VARCHAR(100)

AS
--
--  DESCRIPTION
--  Returns Concept_Key and DisplayTerm when search characters are entered.
--
--	PARAMETERS
--	NAME			DESCRIPTION
--	@SearchText 		Search text used to find collections.
--
--  AUTHOR:			Anthony Simpson, Dorset Software
--  CREATED:			2003-10-20

SET NOCOUNT ON

	SELECT DISTINCT D.Concept_Key AS Item_Key, VW.Item_Name AS DisplayTerm, VW.Item_Name AS SearchTerm
	FROM		Determination AS D
	LEFT JOIN	Search_Term ST ON ST.Concept_Key = D.Concept_Key
	INNER JOIN	VW_ConceptTerm AS VW ON VW.Concept_Key = D.Concept_Key
	AND 		ST.Plaintext LIKE @SearchText + '%'
	
	UNION
	
	SELECT 		TD.Taxon_List_Item_Key AS Item_Key, ITN.Preferred_Name AS DisplayTerm, ITN.Preferred_Name AS SearchTerm
	FROM		Taxon_Determination AS TD
	INNER JOIN	Index_Taxon_Name AS ITN ON ITN.Taxon_List_Item_Key = TD.Taxon_List_Item_Key
	AND 		ITN.Preferred_Name LIKE @SearchText + '%'

	ORDER BY DisplayTerm
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Determinations_Select_ForSearch') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Determinations_Select_ForSearch'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Determinations_Select_ForSearch TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Determinations_Select_ForSearch TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Determinations_Select_ForSearch TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Determinations_Select_ForSearch TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Determinations_Select_ForSearch TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Determinations_Select_ForSearch TO [Dev - JNCC SQL]
END

GO