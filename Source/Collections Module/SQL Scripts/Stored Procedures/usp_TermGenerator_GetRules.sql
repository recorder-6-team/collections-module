SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

/*============================================================================*\
	Drop stored proc before re-creating.
\*============================================================================*/
IF OBJECT_ID(N'dbo.usp_TermGenerator_GetRules') IS NOT NULL
	DROP PROCEDURE dbo.usp_TermGenerator_GetRules
GO

/*============================================================================*\
	Description:
		Returns all of the term generators in the system.

	Created: July 2011

	Last revision information:
		$Revision: 2 $
		$Date: 15/08/11 9:03 $
		$Author: Jamesbichard $
\*============================================================================*/

CREATE PROCEDURE dbo.usp_TermGenerator_GetRules
AS
	SELECT
		Term_Generator_Key,
		Item_Name
	FROM dbo.Term_Generator
	ORDER BY Item_Name
		
GO

/*============================================================================*\
	Grant permissions.
\*============================================================================*/
PRINT 'Setting up security on procedure usp_TermGenerator_GetRules'

IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
    GRANT EXECUTE ON dbo.usp_TermGenerator_GetRules TO R2k_AddOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
	GRANT EXECUTE ON dbo.usp_TermGenerator_GetRules TO R2k_Administrator
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
	GRANT EXECUTE ON dbo.usp_TermGenerator_GetRules TO R2k_FullEdit
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
	GRANT EXECUTE ON dbo.usp_TermGenerator_GetRules TO R2k_ReadOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
	GRANT EXECUTE ON dbo.usp_TermGenerator_GetRules TO R2k_RecordCardsOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
    GRANT EXECUTE ON dbo.usp_TermGenerator_GetRules TO "Dev - JNCC SQL"
GO