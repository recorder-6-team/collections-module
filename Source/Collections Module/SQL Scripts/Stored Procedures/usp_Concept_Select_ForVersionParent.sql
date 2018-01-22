/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Concept_Select_ForVersionParent]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Concept_Select_ForVersionParent]
GO

/*===========================================================================*\
  Description: Returns a list of concepts that are with the supplied parent,
      				 for a given concent group version	

  Parameters:	@ParentConceptKey
							@ConceptGroupVersionKey
							@HierarchyRelationTypeKey - relationship type used to populate
							hierarchy.

  Created:	September 2003

  Last revision information:
    $Revision: 8 $
    $Date: 23/04/08 16:34 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Concept_Select_ForVersionParent]
	@ParentConceptKey varchar(100),
  @ConceptGroupVersionKey char(16),
  @HierarchyRelationTypeKey char(16)
AS

SET ANSI_NULLS ON
SET ANSI_PADDING ON
SET ANSI_WARNINGS ON
SET ARITHABORT ON
SET CONCAT_NULL_YIELDS_NULL ON
SET QUOTED_IDENTIFIER ON
SET NO_BROWSETABLE OFF

SELECT DISTINCT CT.Concept_Key, CT.Item_Name, CT.Concept_Rank_Key, CT.Sort_Code,
	CASE WHEN CRChild.To_Concept_Key IS NULL THEN 0 ELSE 1 END AS HasChildren
FROM VW_ConceptTerm CT
-- Find any parents so we can filter for top level
INNER JOIN Concept_Relation CRParent 
	ON CRParent.To_Concept_Key=CT.Concept_Key
	AND CRParent.Thesaurus_Relation_Type_Key=@HierarchyRelationTypeKey
	AND CRParent.From_Concept_Key = @ParentConceptKey
-- Filter all concepts from concept group
INNER JOIN Concept_Group_Version CGV 
		ON CGV.Concept_Group_Key=CT.Concept_Group_Key
		AND CGV.Concept_Group_Version_Key=@ConceptGroupVersionKey
-- Filter to all concepts where history puts them into the concept group version
INNER JOIN (
	Concept_History CH
	LEFT JOIN Concept_Group_Version CGVFrom ON CGVFrom.Concept_Group_Version_Key=CH.Concept_Group_Version_From
	LEFT JOIN Concept_Group_Version CGVTo ON CGVTo.Concept_Group_Version_Key=CH.Concept_Group_Version_To
		) ON CH.Concept_Key=CT.Concept_Key
-- Find any children so we can return the Has_Children flag
LEFT JOIN (Concept_Relation CRChild 		
	-- Filter to all concepts where history puts them into the concept group version
	INNER JOIN Concept_History CHChild ON CHChild.Concept_Key=CRChild.To_Concept_Key
	LEFT JOIN Concept_Group_Version CGVFromChild ON CGVFromChild.Concept_Group_Version_Key=CHChild.Concept_Group_Version_From
	LEFT JOIN Concept_Group_Version CGVToChild ON CGVToChild.Concept_Group_Version_Key=CHChild.Concept_Group_Version_To
	) ON CRChild.From_Concept_Key=CT.Concept_Key AND CRChild.Thesaurus_Relation_Type_Key=@HierarchyRelationTypeKey
WHERE (CH.Concept_Group_Version_From IS NULL OR CGVFrom.Sequence<=CGV.Sequence)
	AND (CH.Concept_Group_Version_To IS NULL OR CGVTo.Sequence>=CGV.Sequence)
	AND (CHChild.Concept_Group_Version_From IS NULL OR CGVFromChild.Sequence<=CGV.Sequence)
	AND (CHChild.Concept_Group_Version_To IS NULL OR CGVToChild.Sequence>=CGV.Sequence)
ORDER BY CT.Sort_Code

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_Select_ForVersionParent') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Concept_Select_ForVersionParent'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Concept_Select_ForVersionParent TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForVersionParent TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForVersionParent TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForVersionParent TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForVersionParent TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Concept_Select_ForVersionParent TO [Dev - JNCC SQL]
END

GO
