/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptListPreferred_Select_ForConcept]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptListPreferred_Select_ForConcept]
GO

/*===========================================================================*\
  Description:	Retrieves details of the list preferred concept using any concept
								key as input.  Input could be a synonym, but proper name will be 
								returned.

  Parameters:	@ConceptGroupKey - key of the concept group
							@ConceptKey - concept key

  Created:	August 2003

  Last revision information:
    $Revision: 4 $
    $Date: 3/08/11 14:50 $
    $Author: Simonlewis $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptListPreferred_Select_ForConcept]
	@ConceptGroupKey char(16),
  @ConceptKey varchar(16)
AS


SELECT CPref.Concept_Key, CPref.Published_Term	AS	Item_Name, CPref.Concept_Rank_Key, 
  CASE WHEN CR.Concept_Relation_Key IS NULL THEN 0 ELSE 1 END AS HasChildren
FROM Concept C
  INNER JOIN Concept CPref on CPref.Meaning_Key=C.Meaning_Key
  LEFT JOIN Concept_Relation CR on CR.From_Concept_Key=CPref.Concept_Key
WHERE C.Concept_Key=@ConceptKey
  AND CPref.List_Preferred=1
  AND CPref.Concept_Group_Key=@ConceptGroupKey
	AND CPref.Is_Current=1


GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptListPreferred_Select_ForConcept') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptListPreferred_Select_ForConcept'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptListPreferred_Select_ForConcept TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptListPreferred_Select_ForConcept TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptListPreferred_Select_ForConcept TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ConceptListPreferred_Select_ForConcept TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptListPreferred_Select_ForConcept TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptListPreferred_Select_ForConcept TO [Dev - JNCC SQL]
END

GO

