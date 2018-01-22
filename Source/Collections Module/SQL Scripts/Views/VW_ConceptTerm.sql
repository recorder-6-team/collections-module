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
    $Revision: 9 $
    $Date: 3/08/11 10:16 $
    $Author: Simonlewis $

\*===========================================================================*/
CREATE VIEW [dbo].[VW_ConceptTerm]
WITH SCHEMABINDING
AS
	SELECT 	C.Concept_Key,
		C.Published_Term		AS		Item_Name,
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