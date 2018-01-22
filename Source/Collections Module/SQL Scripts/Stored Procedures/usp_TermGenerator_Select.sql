SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

/*============================================================================*\
	Drop function before re-creating.
\*============================================================================*/
IF OBJECT_ID(N'dbo.usp_TermGenerator_Select') IS NOT NULL
	DROP PROCEDURE dbo.usp_TermGenerator_Select
GO

/*============================================================================*\
	Description:
		Returns published term function for specified key. Key can directly be
		a term generator key, or can be a concept key, for which the 
		associated term generator key is obtained.

	Created: July 2011

	Last revision information:
		$Revision: 2 $
		$Date: 8/08/11 13:32 $
		$Author: Jamesbichard $
\*============================================================================*/

CREATE PROCEDURE dbo.usp_TermGenerator_Select(
	@Key CHAR(16)
)
AS
BEGIN

	SELECT Published_Term_Function, Search_Term_Procedure
	FROM Term_Generator
	WHERE Term_Generator_Key = @Key
END
GO

/*============================================================================*\
	Grant permissions.
\*============================================================================*/
PRINT 'Setting up security on function usp_TermGenerator_Select'

IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
    GRANT EXECUTE ON dbo.usp_TermGenerator_Select TO R2k_AddOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
	GRANT EXECUTE ON dbo.usp_TermGenerator_Select TO R2k_Administrator
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
	GRANT EXECUTE ON dbo.usp_TermGenerator_Select TO R2k_FullEdit
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
	GRANT EXECUTE ON dbo.usp_TermGenerator_Select TO R2k_ReadOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
	GRANT EXECUTE ON dbo.usp_TermGenerator_Select TO R2k_RecordCardsOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
    GRANT EXECUTE ON dbo.usp_TermGenerator_Select TO "Dev - JNCC SQL"
GO