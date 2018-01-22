/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TaxonOccurrences_Select_ForSearch') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_TaxonOccurrences_Select_ForSearch]
GO

/*===========================================================================*\
  Description:	Returns a list of Taxon Occurrences.

  Parameters:	@SearchText

  Created:	November 2003

  Last revision information:
    $Revision: 3 $
    $Date: 21/07/04 11:37 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_TaxonOccurrences_Select_ForSearch]
	@SearchText varchar(150)
AS

SET NOCOUNT ON

	SELECT DISTINCT XO.Taxon_Occurrence_Key AS [Item_Key],
			ITN.Actual_Name + ' - ' +
			dbo.ufn_GetDateFromVagueDate(S.Vague_Date_Start, S.Vague_Date_End, S.Vague_Date_Type) +
			' - ' + 
			CASE 
				WHEN LN.Item_Name IS NULL THEN
					CASE WHEN S.Spatial_Ref IS NULL THEN '' ELSE S.Spatial_Ref END
				ELSE LN.Item_Name + ' (' + 
					CASE 
						WHEN S.Spatial_Ref IS NULL THEN L.Spatial_Ref
						ELSE S.Spatial_Ref END +
					')'
			END
			AS SearchTerm,
			CASE ITN.Actual_Name_Italic
				WHEN 1 THEN '<i>' + ITN.Actual_Name + '</i>'
				ELSE ITN.Actual_Name 
			END + ' - ' +
			dbo.ufn_GetDateFromVagueDate(S.Vague_Date_Start, S.Vague_Date_End, S.Vague_Date_Type) +
			' - ' + 
			CASE 
				WHEN LN.Item_Name IS NULL THEN
					CASE WHEN S.Spatial_Ref IS NULL THEN '' ELSE S.Spatial_Ref END
				ELSE LN.Item_Name + ' (' + 
					CASE 
						WHEN S.Spatial_Ref IS NULL THEN L.Spatial_Ref
						ELSE S.Spatial_Ref END +
					')'
			END
			AS DisplayTerm
	FROM		Taxon_Occurrence XO
	INNER JOIN	Taxon_Determination TD ON XO.Taxon_Occurrence_Key = TD.Taxon_Occurrence_Key AND TD.Preferred = 1
	INNER JOIN	Taxon_Dictionary_Concept_Mapping TDCM ON TDCM.Taxon_List_Item_Key = TD.Taxon_List_Item_Key
	INNER JOIN	Index_Taxon_Name ITN ON ITN.Taxon_List_Item_Key=TD.Taxon_List_Item_Key
	INNER JOIN 	Sample S ON S.Sample_Key = XO.Sample_Key
	LEFT JOIN	Location L ON L.Location_Key = S.Location_Key 
	LEFT JOIN	Location_Name LN ON LN.Location_Key = L.Location_Key AND LN.Preferred = 1
	WHERE		ITN.Actual_Name LIKE @SearchText + '%'
	ORDER BY	SearchTerm

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TaxonOccurrences_Select_ForSearch') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_TaxonOccurrences_Select_ForSearch'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_TaxonOccurrences_Select_ForSearch TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_TaxonOccurrences_Select_ForSearch TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_TaxonOccurrences_Select_ForSearch TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_TaxonOccurrences_Select_ForSearch TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_TaxonOccurrences_Select_ForSearch TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_TaxonOccurrences_Select_ForSearch TO [Dev - JNCC SQL]
END
GO
