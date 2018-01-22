SET ANSI_NULLS ON
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].usp_Concept_Select_ForTermVersion')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].usp_Concept_Select_ForTermVersion
GO

/*===========================================================================*\
  Description:	Returns concepts with a specified term version.

  Parameters: @TermVersionKey  - term version to search for
				@ConceptKey - optional parameter to exclude specified concept	

  Created:	August 2011

  Last revision information:
    $Revision: 1 $
    $Date: 11/08/11 11:56 $
    $Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].usp_Concept_Select_ForTermVersion
	@TermVersionKey char(16),
	@ConceptKey char(16) = NULL
AS
	SELECT Concept_Key
	FROM Concept
	WHERE Term_Version_Key = @TermVersionKey
	AND (Concept_Key <> @ConceptKey OR @ConceptKey = null)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_Select_ForTermVersion') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Concept_Select_ForTermVersion'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Concept_Select_ForTermVersion TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForTermVersion TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForTermVersion TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForTermVersion TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForTermVersion TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Concept_Select_ForTermVersion TO [Dev - JNCC SQL]
END

GO
