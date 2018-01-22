/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptVersionListPreferred_Select_ForConcept]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptVersionListPreferred_Select_ForConcept]
GO

/*===========================================================================*\
  Description:	Retrieves details of the list preferred concept using any concept
								key as input.  Input could be a synonym, but proper name will be 
								returned.  Preferred name is picked from the supplied list version

  Parameters:	@ConceptGroupVersionKey - key of the concept group version to obtain 
																				name
							@ConceptKey - concept key

  Created:	August 2003

  Last revision information:
    $Revision: 4 $
    $Date: 3/08/11 15:00 $
    $Author: Simonlewis $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptVersionListPreferred_Select_ForConcept]
	@ConceptGroupVersionKey char(16),
  @ConceptKey varchar(16)
AS


SELECT CPref.Concept_Key, CPref.Published_Term AS Item_Name, CPref.Concept_Rank_Key, 
  CASE WHEN (CGV1Child.Concept_Group_Version_Key IS NULL OR CGV1Child.Sequence<=CGVChild.Sequence)
      AND (CGV2Child.Concept_Group_Version_Key IS NULL OR CGV2Child.Sequence>=CGVChild.Sequence) 
      AND CChild.Concept_Key IS NOT NULL THEN
    1
  ELSE
    0 
  END AS HasChildren
FROM Concept C
  INNER JOIN Concept CPref on CPref.Meaning_Key=C.Meaning_Key
  LEFT JOIN Concept_Relation CR on CR.From_Concept_Key=CPref.Concept_Key
  INNER JOIN Concept_Group_Version CGV on CGV.Concept_Group_Key=CPref.Concept_Group_Key
      AND CGV.Concept_Group_Version_Key=@ConceptGroupVersionKey
  LEFT JOIN Concept_History CH on CH.Concept_Key=CPref.Concept_Key
  LEFT JOIN Concept_Group_Version CGV1 ON CGV1.Concept_Group_Version_Key=CH.Concept_Group_Version_From
  LEFT JOIN Concept_Group_Version CGV2 ON CGV2.Concept_Group_Version_Key=CH.Concept_Group_Version_To
  --And ensure the children are checked in the correct concept group version
  LEFT JOIN Concept CChild on CChild.Concept_Key=CR.To_Concept_Key
  LEFT JOIN Concept_Group_Version CGVChild on CGVChild.Concept_Group_Key=CChild.Concept_Group_Key
      AND CGVChild.Concept_Group_Version_Key=@ConceptGroupVersionKey
  LEFT JOIN Concept_History CHChild on CHChild.Concept_Key=CChild.Concept_Key
  LEFT JOIN Concept_Group_Version CGV1Child ON CGV1Child.Concept_Group_Version_Key=CHChild.Concept_Group_Version_From
  LEFT JOIN Concept_Group_Version CGV2Child ON CGV2Child.Concept_Group_Version_Key=CHChild.Concept_Group_Version_To
WHERE C.Concept_Key=@ConceptKey
  AND CPref.List_Preferred=1
  AND (CGV1.Concept_Group_Version_Key IS NULL OR CGV1.Sequence<=CGV.Sequence)
  AND (CGV2.Concept_Group_Version_Key IS NULL OR CGV2.Sequence>=CGV.Sequence)


GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptVersionListPreferred_Select_ForConcept') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptVersionListPreferred_Select_ForConcept'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptVersionListPreferred_Select_ForConcept TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptVersionListPreferred_Select_ForConcept TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptVersionListPreferred_Select_ForConcept TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ConceptVersionListPreferred_Select_ForConcept TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptVersionListPreferred_Select_ForConcept TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptVersionListPreferred_Select_ForConcept TO [Dev - JNCC SQL]
END

GO

