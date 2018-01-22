/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptsAreSynonyms_Get]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptsAreSynonyms_Get]
GO

/*===========================================================================*\
  Description:	Takes two Concept keys are sees if they are synonyms. If they
		are, 1 is returned. Otherwise 0.

  Parameters:	@ConceptKey1	
		@ConceptKey2
		@AreSynonyms bit output

  Created:	February 2004

  Last revision information:
    $Revision: 1 $
    $Date: 26/02/04 17:23 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptsAreSynonyms_Get]
	@ConceptKey1 char(16),
	@ConceptKey2 char(16),
	@AreSynonyms bit OUTPUT
AS

SET NOCOUNT ON
	DECLARE @Count INT

	SELECT @Count = Count(*)
	FROM		Concept AS C1
	INNER JOIN	Concept AS C2 	ON C2.Meaning_Key = C1.Meaning_Key
					AND C2.Concept_Key = @ConceptKey2
	WHERE		C1.Concept_Key = @ConceptKey1

	IF @Count > 0 	SET @AreSynonyms = 1
	ELSE		SET @AreSynonyms = 0

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptsAreSynonyms_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptsAreSynonyms_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptsAreSynonyms_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptsAreSynonyms_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptsAreSynonyms_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ConceptsAreSynonyms_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptsAreSynonyms_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptsAreSynonyms_Get TO [Dev - JNCC SQL]
END

GO