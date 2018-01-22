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
    $Date: 11/03/10 16:59 $
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