SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Determinations_Select_ForSpecimen]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Determinations_Select_ForSpecimen]
GO

CREATE PROCEDURE [dbo].[usp_Determinations_Select_ForSpecimen] 
@ParentKey CHAR(16)

AS

--  DESCRIPTION
--  Returns Determinations (Non-Recorder) for a specified Specimen
--
--  PARAMETERS
--  NAME			DESCRIPTION
--	@ParentKey 		Only the records associated with the parent key are returned
--
--
--  AUTHOR:     		Ben Collier, Dorset Software
--  CREATED:    		2003-08-26
--
SET NOCOUNT ON

-- This will affect the entire session. But Recorder shouldn't notice the difference.
SET DateFormat dmy
SELECT DISTINCT
	D.Determination_Key AS Item_Key, 
	CT.Item_Name AS Item_Name, 
	CR.Concept_Rank_Key, 
	Color_R, 
	Color_G, 
	Color_B,
	dbo.ufn_GetConceptAncestorPath(CG.Item_Name, D.Concept_Key) AS Hint,
	CASE WHEN ISDATE(
			dbo.[ufn_GetDateFromVagueDate](D.Vague_Date_start, D.Vague_Date_End, D.Vague_Date_Type)) = 1
		THEN CAST(dbo.[ufn_GetDateFromVagueDate](D.Vague_Date_start, D.Vague_Date_End, D.Vague_Date_Type) AS DATETIME)
		ELSE NULL
	END AS VagueDate
FROM
	DETERMINATION D 
	INNER JOIN VW_ConceptTerm CT ON D.Concept_Key = CT.Concept_Key
	INNER JOIN	Concept_Group CG ON	CT.Concept_Group_Key = CG.Concept_Group_Key
	LEFT JOIN Concept_Rank CR ON CT.Concept_Rank_Key = CR.Concept_Rank_Key
	LEFT JOIN Concept_Lineage CL on D.Concept_Key = CL.Concept_Key
WHERE D.Specimen_Collection_Unit_Key=@ParentKey
ORDER BY	VagueDate DESC

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Determinations_Select_ForSpecimen') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Determinations_Select_ForSpecimen'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Determinations_Select_ForSpecimen TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Determinations_Select_ForSpecimen TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Determinations_Select_ForSpecimen TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Determinations_Select_ForSpecimen TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Determinations_Select_ForSpecimen TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Determinations_Select_ForSpecimen TO [Dev - JNCC SQL]
END

GO