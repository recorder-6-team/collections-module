SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

/*============================================================================*\
	Drop stored proc before re-creating.
\*============================================================================*/
IF OBJECT_ID(N'dbo.usp_SearchTerm_Update') IS NOT NULL
	DROP PROCEDURE dbo.usp_SearchTerm_Update
GO

/*============================================================================*\
	Description:
		Updates search term

	Parameters:
		

	Created: August 2011

	Last revision information:
		$Revision: 1 $
		$Date: 8/08/11 13:29 $
		$Author: Jamesbichard $
\*============================================================================*/

CREATE PROCEDURE dbo.usp_SearchTerm_Update
	@Key char(16),
	@Plaintext nvarchar(450)
AS
	SET NOCOUNT ON
	
	UPDATE Search_Term
	SET Plaintext = @Plaintext
	WHERE Search_Term_Key = @Key
GO

/*============================================================================*\
	Grant permissions.
\*============================================================================*/
PRINT 'Setting up security on procedure usp_SearchTerm_Update'

IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
    GRANT EXECUTE ON dbo.usp_SearchTerm_Update TO R2k_AddOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
	GRANT EXECUTE ON dbo.usp_SearchTerm_Update TO R2k_Administrator
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
	GRANT EXECUTE ON dbo.usp_SearchTerm_Update TO R2k_FullEdit
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
	GRANT EXECUTE ON dbo.usp_SearchTerm_Update TO R2k_ReadOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
	GRANT EXECUTE ON dbo.usp_SearchTerm_Update TO R2k_RecordCardsOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
    GRANT EXECUTE ON dbo.usp_SearchTerm_Update TO "Dev - JNCC SQL"
GO