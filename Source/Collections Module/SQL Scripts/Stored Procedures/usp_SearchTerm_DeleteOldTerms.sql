SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

/*============================================================================*\
	Drop stored proc before re-creating.
\*============================================================================*/
IF OBJECT_ID(N'dbo.usp_SearchTerm_DeleteOldTerms') IS NOT NULL
	DROP PROCEDURE dbo.usp_SearchTerm_DeleteOldTerms
GO

/*============================================================================*\
	Description:
		Clears out old system generated search terms before inserting new ones

	Parameters:
		

	Created: August 2011

	Last revision information:
		$Revision: 1 $
		$Date: 2/08/11 12:59 $
		$Author: Jamesbichard $
\*============================================================================*/

CREATE PROCEDURE dbo.usp_SearchTerm_DeleteOldTerms
	@ConceptKey char(16)
AS
	SET NOCOUNT ON

	DELETE FROM Search_Term
	WHERE Concept_Key = @ConceptKey
	AND System_Generated = 1

GO

/*============================================================================*\
	Grant permissions.
\*============================================================================*/
PRINT 'Setting up security on procedure usp_SearchTerm_DeleteOldTerms'

IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
    GRANT EXECUTE ON dbo.usp_SearchTerm_DeleteOldTerms TO R2k_AddOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
	GRANT EXECUTE ON dbo.usp_SearchTerm_DeleteOldTerms TO R2k_Administrator
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
	GRANT EXECUTE ON dbo.usp_SearchTerm_DeleteOldTerms TO R2k_FullEdit
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
	GRANT EXECUTE ON dbo.usp_SearchTerm_DeleteOldTerms TO R2k_ReadOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
	GRANT EXECUTE ON dbo.usp_SearchTerm_DeleteOldTerms TO R2k_RecordCardsOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
    GRANT EXECUTE ON dbo.usp_SearchTerm_DeleteOldTerms TO "Dev - JNCC SQL"
GO