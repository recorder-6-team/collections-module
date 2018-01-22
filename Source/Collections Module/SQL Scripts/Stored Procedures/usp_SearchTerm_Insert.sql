SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

/*============================================================================*\
	Drop stored proc before re-creating.
\*============================================================================*/
IF OBJECT_ID(N'dbo.usp_SearchTerm_Insert') IS NOT NULL
	DROP PROCEDURE dbo.usp_SearchTerm_Insert
GO

/*============================================================================*\
	Description:
		Insert search term

	Parameters:
		

	Created: August 2011

	Last revision information:
		$Revision: 2 $
		$Date: 8/08/11 13:30 $
		$Author: Jamesbichard $
\*============================================================================*/

CREATE PROCEDURE dbo.usp_SearchTerm_Insert
	@Key char(16) OUTPUT,
	@ConceptKey char(16),
	@SystemGenerated bit,
	@Plaintext nvarchar(450)
AS
	SET NOCOUNT ON
	
	EXECUTE spNextKey 'Search_Term', @Key OUTPUT

	INSERT INTO Search_Term(Search_Term_Key, Concept_Key, Plaintext, System_Generated)
	VALUES (@Key, @ConceptKey, @Plaintext, @SystemGenerated)
GO

/*============================================================================*\
	Grant permissions.
\*============================================================================*/
PRINT 'Setting up security on procedure usp_SearchTerm_Insert'

IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
    GRANT EXECUTE ON dbo.usp_SearchTerm_Insert TO R2k_AddOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
	GRANT EXECUTE ON dbo.usp_SearchTerm_Insert TO R2k_Administrator
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
	GRANT EXECUTE ON dbo.usp_SearchTerm_Insert TO R2k_FullEdit
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
	GRANT EXECUTE ON dbo.usp_SearchTerm_Insert TO R2k_ReadOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
	GRANT EXECUTE ON dbo.usp_SearchTerm_Insert TO R2k_RecordCardsOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
    GRANT EXECUTE ON dbo.usp_SearchTerm_Insert TO "Dev - JNCC SQL"
GO