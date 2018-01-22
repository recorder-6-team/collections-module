/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_CommonName_Get]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_CommonName_Get]
GO

/*===========================================================================*\
  Description: Returns the common name for a concept

  Parameters:	@ParentConceptKey
							@HierarchyRelationTypeKey - relationship type used to populate
							hierarchy.

  Created:	August 2003

  Last revision information:
    $Revision: 5 $
    $Date: 3/08/11 11:42 $
    $Author: Simonlewis $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_CommonName_Get]
	@ConceptKey char(16),
	@CommonName varchar(150) output
AS

SELECT Top 1 	@CommonName = C2.Published_Term
FROM 		Concept C1
INNER JOIN 	Concept C2 	on C2.Meaning_Key=C1.Meaning_Key
    				AND C2.Name_Type_Concept_Key='SYSTEM000000000L'
				AND C2.Preferred = 1
INNER JOIN 	Term T 		on T.Term_Key=C2.Term_Key
INNER JOIN 	Language L 	on L.Language_Key=T.Language_Key
    				AND L.Priority=1
WHERE 		C1.Concept_Key = @ConceptKey

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_CommonName_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_CommonName_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_CommonName_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_CommonName_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_CommonName_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_CommonName_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_CommonName_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_CommonName_Get TO [Dev - JNCC SQL]
END

GO
