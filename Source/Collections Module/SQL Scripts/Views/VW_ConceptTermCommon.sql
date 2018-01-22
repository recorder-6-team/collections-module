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
    $Revision: 6 $
    $Date: 3/08/11 10:57 $
    $Author: Simonlewis $

\*===========================================================================*/
CREATE VIEW [dbo].[VW_ConceptTermCommon]
WITH SCHEMABINDING
AS
	SELECT 		C1.Concept_Key, C2.Published_Term AS Item_Name, T.Plaintext, C2.Author_Copy, C1.Concept_Group_Key
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
