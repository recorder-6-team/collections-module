/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptKey_Get_ForConceptGroupAndItemName]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptKey_Get_ForConceptGroupAndItemName]
GO

/*===========================================================================*\
  Description:	Retrieves the concept key when given the concept title and a
		concept group key.

  Parameters:	@ConceptGroupKey
		@PlainText - The concept title.
		@ConceptKey - The concept key is outputted.

  Created:	October 2003

  Last revision information:
    $Revision: 3 $
    $Date: 3/08/11 14:47 $
    $Author: Simonlewis $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_ConceptKey_Get_ForConceptGroupAndItemName]
	@ConceptGroupKey char(16),
	@PlainText varchar(100),
	@ConceptKey char(16) OUTPUT
AS

SET NOCOUNT ON

	SELECT 		@ConceptKey = C.Concept_Key 
	FROM 		Concept C 
	INNER JOIN 	Term T ON T.Term_Key = C.Term_Key 
	WHERE 		(C.Concept_Group_Key = @ConceptGroupKey)
	AND 		(C.List_Preferred = 1)
	AND 		(C.Is_Current = 1)
	AND 		PlainText = @PlainText
	ORDER BY 	C.Sort_Code, T.Plaintext
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptKey_Get_ForConceptGroupAndItemName') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptKey_Get_ForConceptGroupAndItemName'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptKey_Get_ForConceptGroupAndItemName TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptKey_Get_ForConceptGroupAndItemName TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptKey_Get_ForConceptGroupAndItemName TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ConceptKey_Get_ForConceptGroupAndItemName TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptKey_Get_ForConceptGroupAndItemName TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptKey_Get_ForConceptGroupAndItemName TO [Dev - JNCC SQL]
END

GO