SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

/*============================================================================*\
	Drop stored proc before re-creating.
\*============================================================================*/
IF OBJECT_ID(N'dbo.usp_SearchTerm_Select') IS NOT NULL
	DROP PROCEDURE dbo.usp_SearchTerm_Select
GO

/*============================================================================*\
	Description:
		Select search terms

	Parameters:
		

	Created: August 2011

	Last revision information:
		$Revision: 2 $
		$Date: 8/08/11 13:31 $
		$Author: Jamesbichard $
\*============================================================================*/

CREATE PROCEDURE dbo.usp_SearchTerm_Select
	@ConceptKey char(16),
	@SystemGenerated bit = null
AS
	SELECT 
		Search_Term_Key, 
		Plaintext,
		null as Custodian,
		null as Timestamp
	FROM Search_Term
	WHERE Concept_Key = @ConceptKey
	AND (System_Generated = @SystemGenerated OR @SystemGenerated IS NULL)
GO

/*============================================================================*\
	Grant permissions.
\*============================================================================*/
PRINT 'Setting up security on procedure usp_SearchTerm_Select'

IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
    GRANT EXECUTE ON dbo.usp_SearchTerm_Select TO R2k_AddOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
	GRANT EXECUTE ON dbo.usp_SearchTerm_Select TO R2k_Administrator
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
	GRANT EXECUTE ON dbo.usp_SearchTerm_Select TO R2k_FullEdit
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
	GRANT EXECUTE ON dbo.usp_SearchTerm_Select TO R2k_ReadOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
	GRANT EXECUTE ON dbo.usp_SearchTerm_Select TO R2k_RecordCardsOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
    GRANT EXECUTE ON dbo.usp_SearchTerm_Select TO "Dev - JNCC SQL"
GO