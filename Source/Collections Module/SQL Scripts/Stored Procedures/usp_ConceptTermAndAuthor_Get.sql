/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptTermAndAuthor_Get]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptTermAndAuthor_Get]
GO

/*===========================================================================*\
  Description: Returns the published term of a Concept.

  Parameters:	@ConceptKey	

  Created:	August 2003

  Last revision information:
    $Revision: 4 $
    $Date: 4/08/11 16:25 $
    $Author: Simonlewis $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptTermAndAuthor_Get]
	@ConceptKey varchar(100),
    @ConceptTermAndAuthor varchar(505) output

AS

  SELECT @ConceptTermAndAuthor =	Published_Term
  FROM Concept
  WHERE Concept_Key = @ConceptKey

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptTermAndAuthor_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptTermAndAuthor_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptTermAndAuthor_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptTermAndAuthor_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptTermAndAuthor_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ConceptTermAndAuthor_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptTermAndAuthor_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptTermAndAuthor_Get TO [Dev - JNCC SQL]
END

GO