/*===========================================================================*\
  Drop function before re-creating.
\*===========================================================================*/

IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[VW_CollectionUnitAccessionNumber]') 
	   AND    ObjectProperty(Id, N'IsView') = 1)
    DROP VIEW [dbo].[VW_CollectionUnitAccessionNumber]
GO

SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO

/*===========================================================================*\
  Description:	View that lists collection units and their accession numbers

  Created:	August 2004

  Last revision information:
    $Revision: 2 $
    $Date: 25/09/08 14:03 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE VIEW [dbo].[VW_CollectionUnitAccessionNumber]
AS

SELECT	
		CU.Collection_Unit_Key, 
		M.Number AS Accession_Number
FROM		Collection_Unit CU
LEFT JOIN 	(Movement_Collection_Unit MCU 
INNER JOIN 	Movement_Direction MD 
		ON MD.Movement_Direction_Key = MCU.Movement_Direction_Key
		AND MD.OutBound = 0			-- Inbound = 0
INNER JOIN 	Movement M 
		ON M.Movement_Key = MD.Movement_Key
		AND		M.Movement_Type IN (0, 1))
ON MCU.Collection_Unit_Key = CU.Collection_Unit_Key

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
GRANT SELECT ON [dbo].[VW_CollectionUnitAccessionNumber] TO [Public]
GO

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
    $Revision: 2 $
    $Date: 25/09/08 14:03 $
    $Author: Ericsalmon $

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
    $Revision: 2 $
    $Date: 25/09/08 14:03 $
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

/*===========================================================================*\
  Drop function before re-creating.
\*===========================================================================*/

IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[VW_ConceptTerm]') 
	   AND    ObjectProperty(Id, N'IsView') = 1)
    DROP VIEW [dbo].[VW_ConceptTerm]
GO

SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO

/*===========================================================================*\
  Description:	View that lists concepts with the actual term details.

  Created:	August 2003

  Last revision information:
    $Revision: 2 $
    $Date: 25/09/08 14:03 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE VIEW [dbo].[VW_ConceptTerm]
WITH SCHEMABINDING
AS
	SELECT 	C.Concept_Key, 
		T.Item_Name, 
		T.Plaintext,
		C.Author_Copy,
		C.Concept_Group_Key, 
		C.Concept_Rank_Key,
		C.Sort_Code, 
		C.Meaning_Key,
		List_Preferred, 
		Is_Current
	FROM	dbo.Concept C
	INNER JOIN dbo.Term T on T.Term_Key=C.Term_Key
GO

/*===========================================================================*\
  Create indexes - possible because the view is Schema bound
\*===========================================================================*/
SET ARITHABORT ON 
SET NUMERIC_ROUNDABORT OFF
GO

CREATE UNIQUE CLUSTERED INDEX IX_Concept_Key
ON [dbo].[VW_ConceptTerm](Concept_Key)
GO

CREATE INDEX IX_AuthorCopy ON [dbo].[VW_ConceptTerm](Author_Copy)
CREATE INDEX IX_ConceptGroupKey ON [dbo].[VW_ConceptTerm](Concept_Group_Key)
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
GRANT SELECT ON [dbo].[VW_ConceptTerm] TO [Public]
GO

/*===========================================================================*\
  Drop function before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[VW_ConceptTermCommon]')
	   AND    ObjectProperty(Id, N'IsView') = 1)
    DROP VIEW [dbo].[VW_ConceptTermCommon]
GO

SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO

/*===========================================================================*\
  Description:	View that lists concepts with the common term details

  Created:	August 2003

  Last revision information:
    $Revision: 2 $
    $Date: 25/09/08 14:03 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE VIEW [dbo].[VW_ConceptTermCommon]
WITH SCHEMABINDING
AS
	SELECT 		C1.Concept_Key, T.Item_Name, T.Plaintext, C2.Author_Copy, C1.Concept_Group_Key
	FROM 		dbo.Concept C1
	INNER JOIN 	dbo.Meaning M ON M.Meaning_Key = C1.Concept_Key
	INNER JOIN 	dbo.Concept C2 ON C2.Meaning_Key = M.Meaning_Key
	INNER JOIN 	dbo.Term T ON T.Term_Key = C2.Term_Key
	INNER JOIN 	dbo.Language L ON L.Language_Key = T.Language_Key
	WHERE 		C2.Name_Type_Concept_Key = 'SYSTEM000000000L'
	AND 		L.Priority = 1
GO

SET QUOTED_IDENTIFIER OFF
GO
SET ANSI_NULLS OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
GRANT SELECT ON [dbo].[VW_ConceptTermCommon] TO [Public]
GO

/*===========================================================================*\
  Drop function before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[VW_ConceptTermPreferred]') 
	   AND    ObjectProperty(Id, N'IsView') = 1)
    DROP VIEW [dbo].[VW_ConceptTermPreferred]
GO

SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO

/*===========================================================================*\
  Description:	View that lists concepts with the preferred term details

  Created:	August 2003

  Last revision information:
    $Revision: 2 $
    $Date: 25/09/08 14:03 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE VIEW [dbo].[VW_ConceptTermPreferred]
AS
	SELECT 	C.Concept_Key, 
			T.Item_Name, 
			T.Plaintext, 
			C.Author_Copy, 
			C.Concept_Group_Key, 
			C.Concept_Rank_Key, 
			C.Sort_Code,
			CP.Concept_Key AS Preferred_Concept_Key
	FROM 	Concept C
	JOIN 	Concept CP 	ON 	CP.Meaning_Key			=	C.Meaning_Key
						AND CP.List_Preferred		=	1
						AND CP.Concept_Group_Key	=	C.Concept_Group_Key
	JOIN 	Term 	T 	ON	T.Term_Key				=	CP.Term_Key
GO

SET QUOTED_IDENTIFIER OFF
GO
SET ANSI_NULLS OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
GRANT SELECT ON [dbo].[VW_ConceptTermPreferred] TO [Public]
GO

/*===========================================================================*\
  Drop function before re-creating.
\*===========================================================================*/

IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[VW_SpecimenDetsEarth]') 
	   AND    ObjectProperty(Id, N'IsView') = 1)
    DROP VIEW [dbo].[VW_SpecimenDetsEarth]
GO

SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO

/*===========================================================================*\
  Description:	View that specimens with their determinations

  Created:	June 2004

  Last revision information:
    $Revision: 2 $
    $Date: 25/09/08 14:03 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE VIEW VW_SpecimenDetsEarth
WITH SCHEMABINDING
AS
SELECT 
	SU.Collection_Unit_Key, 
	D.Concept_Key AS Concept_Key,
	SU.Preferred_Determination_Key,
	D.Determination_Key
FROM dbo.Specimen_Unit SU
INNER JOIN dbo.Determination D ON SU.Collection_Unit_Key = D.Specimen_Collection_Unit_Key
WHERE SU.Life_Sciences=0
GO

/*===========================================================================*\
  Create indexes - possible because the view is Schema bound
\*===========================================================================*/
SET ARITHABORT ON 
SET NUMERIC_ROUNDABORT OFF
GO

CREATE UNIQUE CLUSTERED INDEX VW_SpecimenDetsEarth
ON [dbo].[VW_SpecimenDetsEarth] (Collection_Unit_Key, Determination_Key)
GO

CREATE INDEX IX_DeterminationKey ON [dbo].[VW_SpecimenDetsEarth] (Concept_Key)

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
GRANT SELECT ON [dbo].[VW_SpecimenDetsEarth] TO [Public]
GO

/*===========================================================================*\
  Drop function before re-creating.
\*===========================================================================*/

IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[VW_SpecimenDetsLife]') 
	   AND    ObjectProperty(Id, N'IsView') = 1)
    DROP VIEW [dbo].[VW_SpecimenDetsLife]
GO

SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO

/*===========================================================================*\
  Description:	View that specimens with their determinations
			Note that this view is recreated after import by 
			usp_PostImport_Collections, so any edits should be reflected there

  Created:	June 2004

  Last revision information:
    $Revision: 2 $
    $Date: 25/09/08 14:03 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE VIEW VW_SpecimenDetsLife
WITH SCHEMABINDING
AS
SELECT 
	SU.Collection_Unit_Key, 
	TD.Taxon_List_Item_Key AS Taxon_List_Item_Key,
	SU.Preferred_Taxon_Determination_Key,
	TD.Taxon_Determination_Key
FROM dbo.Specimen_Unit SU
INNER JOIN dbo.Taxon_Determination TD ON SU.Collection_Unit_Key = TD.Specimen_Collection_Unit_Key
WHERE SU.Life_Sciences=1
GO

/*===========================================================================*\
  Create indexes - possible because the view is Schema bound
\*===========================================================================*/
SET ARITHABORT ON 
SET NUMERIC_ROUNDABORT OFF
GO

CREATE UNIQUE CLUSTERED INDEX VW_SpecimenDetsLife
ON [dbo].[VW_SpecimenDetsLife] (Collection_Unit_Key, Taxon_Determination_Key)
GO

CREATE INDEX IX_DeterminationKey ON [dbo].[VW_SpecimenDetsLife] (Taxon_List_Item_Key)

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
GRANT SELECT ON [dbo].[VW_SpecimenDetsLife] TO [Public]
GO