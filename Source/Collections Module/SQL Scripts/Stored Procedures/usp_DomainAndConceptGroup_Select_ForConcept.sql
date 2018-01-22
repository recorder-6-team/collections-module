/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_DomainAndConceptGroup_Select_ForConcept]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_DomainAndConceptGroup_Select_ForConcept]
GO

/*===========================================================================*\
  Description: Returns the domain and concept group key for a concept

  Parameters:	@ConceptKey	

  Created:	August 2003

  Last revision information:
    $Revision: 4 $
    $Date: 6/01/04 11:18 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_DomainAndConceptGroup_Select_ForConcept]
	@ConceptKey varchar(100)

AS
	SELECT 		
			LD.Domain_Key, 
			CG.Concept_Group_Key, 
			C.Is_Current, 
			CGV.Concept_Group_Version_Key 
	FROM 		Concept C
	INNER JOIN 	Concept_Group CG ON CG.Concept_Group_Key=C.Concept_Group_Key
	INNER JOIN 	Local_Domain LD ON LD.Local_Domain_Key=CG.Local_Domain_Key
  	LEFT JOIN 	Concept_Group_Version CGV ON CGV.Concept_Group_Key=C.Concept_Group_Key
      					    	 AND CGV.Concept_Group_Key=CG.Concept_Group_Key
  	LEFT JOIN 	Concept_History CH ON CH.Concept_Key=C.Concept_Key
  	LEFT JOIN 	Concept_Group_Version CGV1 ON CGV1.Concept_Group_Version_Key=CH.Concept_Group_Version_From
  	LEFT JOIN 	Concept_Group_Version CGV2 ON CGV2.Concept_Group_Version_Key=CH.Concept_Group_Version_To
	WHERE 		C.Concept_Key=@ConceptKey
	AND 		(CGV1.Concept_Group_Version_Key IS NULL OR CGV1.Sequence<=CGV.Sequence)
  	AND 		(CGV2.Concept_Group_Version_Key IS NULL OR CGV2.Sequence>=CGV.Sequence)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_DomainAndConceptGroup_Select_ForConcept') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_DomainAndConceptGroup_Select_ForConcept'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_DomainAndConceptGroup_Select_ForConcept TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_DomainAndConceptGroup_Select_ForConcept TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_DomainAndConceptGroup_Select_ForConcept TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_DomainAndConceptGroup_Select_ForConcept TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_DomainAndConceptGroup_Select_ForConcept TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_DomainAndConceptGroup_Select_ForConcept TO [Dev - JNCC SQL]
END

GO
