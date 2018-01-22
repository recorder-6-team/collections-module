/*===========================================================================*\
  Drop function before re-creating.
\*===========================================================================*/

IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[VW_ConceptChildrenOnly]') 
	   AND    ObjectProperty(Id, N'IsView') = 1)
    DROP VIEW [dbo].[VW_ConceptChildrenOnly]
GO

SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO

/*===========================================================================*\
  Description:	View that lists concepts together with all child concepts.

  Created:	August 2004

  Last revision information:
    $Revision: 1 $
    $Date: 14/10/04 15:14 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE VIEW [dbo].[VW_ConceptChildrenOnly]
AS
	SELECT	DISTINCT
		P.Concept_Key AS Parent_Concept_Key, CSyn.Concept_Key AS Child_Concept_Key

	FROM	Concept P
	JOIN	Concept PSyn		ON PSyn.Concept_Group_Key = P.Concept_Group_Key
					AND PSyn.Meaning_Key = P.Meaning_Key
	JOIN  	Concept_Lineage PL	ON PL.Concept_Key = PSyn.Concept_Key
	JOIN  	Concept_Lineage CL 	ON CL.Lineage LIKE PL.Lineage + '\%'
	JOIN  	Concept C		ON C.Concept_Key = CL.Concept_Key
					AND C.Concept_Group_Key = P.Concept_Group_Key
	JOIN  	Concept CSyn		ON CSyn.Concept_Group_Key = C.Concept_Group_Key
					AND CSyn.Meaning_Key = C.Meaning_Key
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
GRANT SELECT ON [dbo].[VW_ConceptChildrenOnly] TO [Public]
GO