/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptsLinkedToTermCount_Get]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptsLinkedToTermCount_Get]
GO

/*===========================================================================*\
  Description:	Returns the number of concepts linked to a term.

  Parameters:	@Count (OUTPUT)
		@ConceptKey	

  Created:	January 2004

  Last revision information:
    $Revision: 2 $
    $Date: 23/02/04 16:37 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptsLinkedToTermCount_Get]
	@Count int OUTPUT,
	@ConceptKey char(16)
AS
	SELECT 		@Count = COUNT(DISTINCT C2.Concept_Key)
	FROM		Concept AS C1
	INNER JOIN	Concept AS C2 	ON C2.Term_Key = C1.Term_Key
					AND C2.Concept_Key <> C1.Concept_Key
	WHERE		C1.Concept_Key = @ConceptKey
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptsLinkedToTermCount_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptsLinkedToTermCount_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptsLinkedToTermCount_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptsLinkedToTermCount_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptsLinkedToTermCount_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ConceptsLinkedToTermCount_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptsLinkedToTermCount_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptsLinkedToTermCount_Get TO [Dev - JNCC SQL]
END

GO