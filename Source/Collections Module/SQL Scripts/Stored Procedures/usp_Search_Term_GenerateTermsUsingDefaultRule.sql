SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

/*============================================================================*\
	Drop stored proc before re-creating.
\*============================================================================*/
IF OBJECT_ID(N'dbo.usp_Search_Term_GenerateTermsUsingDefaultRule') IS NOT NULL
	DROP PROCEDURE dbo.usp_Search_Term_GenerateTermsUsingDefaultRule
GO

/*============================================================================*\
	Description:
		The default stored procedure used to generate the search terms for a
		concept.

	Parameters:
		@Plaintext:			The base term.
		@AuthorAndDate:		The author and date.
		@Attributes:		Additional attributes.
		@RankKey:			Identifies the rank of the concept.
		@ParentConceptKey:	Identifies the parent concept, if any.
		@PublishedTerm:		The published term.

	Created: July 2011

	Last revision information:
		$Revision: 5 $
		$Date: 17/11/11 16:32 $
		$Author: Jamesbichard $
\*============================================================================*/

CREATE PROCEDURE dbo.usp_Search_Term_GenerateTermsUsingDefaultRule
	@Plaintext NVARCHAR(150),
	@AuthorAndDate VARCHAR(100),
	@Attributes VARCHAR(100),
	@RankKey CHAR(16),
	@ParentConceptKey CHAR(16),
	@PublishedTerm NVARCHAR(256)
AS
	SET NOCOUNT ON
	
	DECLARE @SearchTerm TABLE
	(
		Search_Term NVARCHAR(450)
	)

	INSERT INTO	@SearchTerm
	SELECT		@Plaintext

	IF @PublishedTerm <> @Plaintext
	BEGIN
		INSERT INTO	@SearchTerm
		SELECT		dbo.ufn_RemoveHtmlMarkup(@PublishedTerm)
	END
	
	IF @AuthorAndDate IS NOT NULL AND LEN(@AuthorAndDate) > 0
	BEGIN
		INSERT INTO	@SearchTerm
		SELECT		@Plaintext + ' ' + @AuthorAndDate
	END

	IF @Attributes IS NOT NULL AND LEN(@Attributes) > 0
	BEGIN
		INSERT INTO	@SearchTerm
		SELECT		@Plaintext + ' ' + @Attributes
	END

	IF @AuthorAndDate IS NOT NULL AND LEN(@AuthorAndDate) > 0 
		AND @Attributes IS NOT NULL AND LEN(@Attributes) > 0
	BEGIN
		INSERT INTO	@SearchTerm
		SELECT		@Plaintext + ' ' + @AuthorAndDate + ' ' + @Attributes
	END

	SELECT DISTINCT
		Search_Term
	FROM @SearchTerm
GO

/*============================================================================*\
	Grant permissions.
\*============================================================================*/
PRINT 'Setting up security on procedure usp_Search_Term_GenerateTermsUsingDefaultRule'

IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
    GRANT EXECUTE ON dbo.usp_Search_Term_GenerateTermsUsingDefaultRule TO R2k_AddOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
	GRANT EXECUTE ON dbo.usp_Search_Term_GenerateTermsUsingDefaultRule TO R2k_Administrator
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
	GRANT EXECUTE ON dbo.usp_Search_Term_GenerateTermsUsingDefaultRule TO R2k_FullEdit
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
	GRANT EXECUTE ON dbo.usp_Search_Term_GenerateTermsUsingDefaultRule TO R2k_ReadOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
	GRANT EXECUTE ON dbo.usp_Search_Term_GenerateTermsUsingDefaultRule TO R2k_RecordCardsOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
    GRANT EXECUTE ON dbo.usp_Search_Term_GenerateTermsUsingDefaultRule TO "Dev - JNCC SQL"
GO