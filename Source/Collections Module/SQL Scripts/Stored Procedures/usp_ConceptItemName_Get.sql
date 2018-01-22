/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptItemName_Get]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptItemName_Get]
GO

/*===========================================================================*\
  Description:	Returns the actual name of a concept.  The common name is 
				appended if required.
				N.B. This is a wrapper for the function.

  Parameters:	@Key 		Concept_Key
							@ItemName	Output - 303 characters = 150 + 150 for common + 3 for 
								space and 2 brackets
							@IncludeCommonName (optional)
							@IncludeAuthor (optional)
							@Formatted (optional)

  Created:	October 2003

  Last revision information:
    $Revision: 6 $
    $Date: 9/01/04 13:46 $
    $Author: Bencollier $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptItemName_Get]
	@Key CHAR(16),
	@IncludeCommonName BIT,
  	@IncludeAuthor BIT,
	@Formatted BIT,
	@ItemName VARCHAR(303) OUTPUT
AS

SET NOCOUNT ON
SELECT @ItemName = dbo.ufn_ConceptItemName_Get(@Key, @IncludeCommonName, @IncludeAuthor, @Formatted)

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptItemName_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptItemName_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptItemName_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptItemName_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptItemName_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ConceptItemName_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptItemName_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptItemName_Get TO [Dev - JNCC SQL]
END

GO

