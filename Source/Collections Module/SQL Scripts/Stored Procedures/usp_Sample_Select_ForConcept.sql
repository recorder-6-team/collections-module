/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Sample_Select_ForConcept]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Sample_Select_ForConcept]
GO

/*===========================================================================*\
  Description:	Returns a list of samples linked to a term/concept.

  Parameters:	@Key	Concept key

  Created:	February 2004

  Last revision information:
    $Revision: 2 $
    $Date: 5/04/04 13:44 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Sample_Select_ForConcept]
	@Key char(16)
AS

SET NOCOUNT ON

	SELECT DISTINCT S.Sample_Key, S.Lat, S.Long, S.Spatial_Ref, S.Spatial_Ref_System, S.Vague_Date_Start
	
	FROM		Concept C
	-- Joins to get all children and synonyms.
	INNER JOIN 	Concept CSyn ON CSyn.Meaning_Key = C.Meaning_Key
	INNER JOIN 	Concept_Lineage CL ON CL.Concept_Key = CSyn.Concept_Key
	INNER JOIN 	Concept_Lineage CLChild ON CLChild.Lineage LIKE CL.Lineage + '\%' OR CLChild.Lineage = CL.Lineage
	INNER JOIN 	Concept CChild ON CChild.Concept_Key = CLChild.Concept_Key AND (CChild.Concept_Group_Key = CSyn.Concept_Group_Key)
	INNER JOIN 	Concept CChildSyn ON CChildSyn.Meaning_Key = CChild.Meaning_Key
	INNER JOIN 	vw_ConceptTerm CTChildren ON CTChildren.Concept_Key = CChildSyn.Concept_Key

	INNER JOIN	Determination D ON D.Concept_Key = CChildSyn.Concept_Key
	-- Go through the Specimen route
	LEFT JOIN	(Specimen_Unit SU 
				INNER JOIN	Specimen_Field_Data SFD ON SFD.Collection_Unit_Key = SU.Collection_Unit_Key
				LEFT JOIN	Occurrence O ON O.Occurrence_Key = SFD.Occurrence_Key
				LEFT JOIN	Taxon_Occurrence TOX ON TOX.Taxon_Occurrence_Key = SFD.Taxon_Occurrence_Key)
			ON SU.Collection_Unit_Key = D.Specimen_Collection_Unit_Key

	-- Go straight to Occurrence
	LEFT JOIN	Occurrence O2 ON O2.Occurrence_Key = D.Occurrence_Key AND D.Preferred = 1
	-- And link all to Sample
	INNER JOIN	[Sample] S ON S.Sample_Key = O.Sample_Key OR S.Sample_Key = TOX.Sample_Key OR S.Sample_Key = O2.Sample_Key
	
	WHERE		C.Concept_Key = @Key
	AND		S.Lat IS NOT NULL
	AND		S.Long IS NOT NULL
	AND		S.Spatial_Ref IS NOT NULL
	AND		S.Spatial_Ref_System IS NOT NULL

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Sample_Select_ForConcept') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Sample_Select_ForConcept'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Sample_Select_ForConcept TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Sample_Select_ForConcept TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Sample_Select_ForConcept TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Sample_Select_ForConcept TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Sample_Select_ForConcept TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Sample_Select_ForConcept TO [Dev - JNCC SQL]
END
GO