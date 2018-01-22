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
  Description:	View that lists concepts together with all child concepts.

  Created:	August 2004

  Last revision information:
    $Revision: 1 $
    $Date: 18/10/04 12:10 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE VIEW [dbo].[VW_ConceptChildren]
AS
    SELECT      c.Concept_Key           AS  Parent_Concept_Key,
                s.Concept_Key           AS  Child_Concept_Key
    FROM        Concept                 AS  c
    INNER JOIN  Concept                 AS  s
    ON          s.Concept_Group_Key     =   c.Concept_Group_Key
    AND         s.Meaning_Key           =   c.Meaning_Key
    UNION
    SELECT      p.Concept_Key           AS  Parent_Concept_Key,
                cs.Concept_Key          AS  Child_Concept_Key
    FROM        Concept                 AS  p
    INNER JOIN  Concept                 AS  ps
    ON          ps.Concept_Group_Key    =   p.Concept_Group_Key
    AND         ps.Meaning_Key          =   p.Meaning_Key
    INNER JOIN  Concept_Lineage         AS  pl
    ON          pl.Concept_Key          =   ps.Concept_Key
    INNER JOIN  Concept_Lineage         AS  cl
    ON          cl.Lineage              LIKE pl.Lineage + '\%'
    INNER JOIN  Concept                 AS  c
    ON          c.Concept_Key           =   cl.Concept_Key
    AND         c.Concept_Group_Key     =   p.Concept_Group_Key
    INNER JOIN  Concept                 AS  cs
    ON          cs.Concept_Group_Key    =   c.Concept_Group_Key
    AND         cs.Meaning_Key          =   c.Meaning_Key
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
    $Date: 18/10/04 12:10 $
    $Author: Johnvanbreda $

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