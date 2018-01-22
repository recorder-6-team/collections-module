/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Occurrences_Select_ForSearch]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Occurrences_Select_ForSearch]
GO

/*===========================================================================*\
  Description:	Returns a list of Occurrences

  Parameters:	@SearchText

  Created:	November 2003

  Last revision information:
    $Revision: 7 $
    $Date: 9/08/11 10:11 $
    $Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Occurrences_Select_ForSearch]
	@SearchText varchar(150)
AS

SET NOCOUNT ON

	SELECT DISTINCT O.Occurrence_Key AS [Item_Key],
			CT.Item_Name + ' - ' +
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
			CT.Item_Name + ' - ' +
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
	FROM		Occurrence O
	INNER JOIN	Determination D ON O.Occurrence_Key = D.Occurrence_Key 
	INNER JOIN	vw_ConceptTermPreferred CT ON D.Concept_Key = CT.Concept_Key
	LEFT JOIN	Search_Term ST ON ST.Concept_Key = CT.Concept_Key
	INNER JOIN 	Sample S ON S.Sample_Key = O.Sample_Key
	LEFT JOIN	Location L ON L.Location_Key = S.Location_Key 
	LEFT JOIN	Location_Name LN ON LN.Location_Key = L.Location_Key AND LN.Preferred = 1
	WHERE		ST.PlainText LIKE @SearchText + '%'
	ORDER BY	SearchTerm

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Occurrences_Select_ForSearch') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Occurrences_Select_ForSearch'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Occurrences_Select_ForSearch TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Occurrences_Select_ForSearch TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Occurrences_Select_ForSearch TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Occurrences_Select_ForSearch TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Occurrences_Select_ForSearch TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Occurrences_Select_ForSearch TO [Dev - JNCC SQL]
END

GO