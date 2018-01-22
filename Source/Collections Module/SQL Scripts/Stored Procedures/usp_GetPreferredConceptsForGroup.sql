/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_GetPreferredConceptsForGroup]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_GetPreferredConceptsForGroup]
GO

/*===========================================================================*\
  Description:	Returns all preferred concepts and their name for a given
		concept group,

  Parameters:	@ConceptGroupKey

  Created:	August 2003

  Last revision information:
    $Revision: 2 $
    $Date: 3/08/11 15:30 $
    $Author: Simonlewis $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_GetPreferredConceptsForGroup]
	@ConceptGroupKey int
AS

	SELECT C.Concept_Key, C.Published_Term AS Item_Name FROM Concept C
    INNER JOIN Term T ON T.Term_Key = C.Term_Key
	    WHERE C.Concept_Group_Key = @ConceptGroupKey
        AND C.List_Preferred = 1
	AND C.Is_Current = 1
	ORDER BY C.Sort_Code, T.Plaintext
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_GetPreferredConceptsForGroup') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_GetPreferredConceptsForGroup'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_GetPreferredConceptsForGroup TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_GetPreferredConceptsForGroup TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_GetPreferredConceptsForGroup TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_GetPreferredConceptsForGroup TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_GetPreferredConceptsForGroup TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_GetPreferredConceptsForGroup TO [Dev - JNCC SQL]
END

GO