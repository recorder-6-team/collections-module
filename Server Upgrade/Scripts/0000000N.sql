/*===========================================================================*\
  Drop function before re-creating.
\*===========================================================================*/

IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[VW_ConceptChildren]') 
	   AND    ObjectProperty(Id, N'IsView') = 1)
    DROP VIEW [dbo].[VW_ConceptChildren]
GO

SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO

/*===========================================================================*\
  Description:	View that lists concepts with child concepts

  Created:	August 2004

  Last revision information:
    $Revision: 2 $
    $Date: 3/08/04 15:50 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE VIEW [dbo].[VW_ConceptChildren]
AS
	SELECT CParent.Concept_Key AS Parent_Concept_Key, CChildSyn.Concept_Key AS Child_Concept_Key
	FROM Concept CParent
	INNER JOIN Concept CParentSyn ON CParentSyn.Meaning_Key=CParent.Meaning_Key
	INNER JOIN Concept_Lineage CL1 ON CL1.Concept_Key=CParentSyn.Concept_Key
	INNER JOIN Concept_Lineage CL2 
			ON CL2.Lineage=CL1.Lineage 
			OR CL2.Lineage LIKE CL1.Lineage + '\%'
	INNER JOIN Concept CChild ON CChild.Concept_Key=CL2.Concept_Key
			AND CChild.Concept_Group_Key=CParent.Concept_Group_Key
	INNER JOIN Concept CChildSyn ON CChildSyn.Meaning_Key=CChild.Meaning_Key
GO

SET NUMERIC_ROUNDABORT OFF 
SET ARITHABORT  OFF 
GO

SET QUOTED_IDENTIFIER OFF
GO
SET ANSI_NULLS OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
GRANT SELECT ON [dbo].[VW_ConceptChildren] TO [Public]
GO