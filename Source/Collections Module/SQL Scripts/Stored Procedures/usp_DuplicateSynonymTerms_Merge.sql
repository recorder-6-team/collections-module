IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_DuplicateSynonymTerms_Merge]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
	DROP PROCEDURE [dbo].[usp_DuplicateSynonymTerms_Merge]
GO

/*===========================================================================*\
  Description:	
	De-duplicates terms found in synonyms of given concept.

  Parameters:	
	@ConceptKey

  Created:	
	January 2006

  Last revision information:
    $Revision: 1 $
    $Date: 4/01/07 12:34 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_DuplicateSynonymTerms_Merge]
	@ConceptKey	CHAR(16)
AS
	SET NOCOUNT ON

	-- Temp table used a a work area to de-duplicate terms.
	DECLARE @T TABLE (
		PlainText	VARCHAR(50),
		ConceptKey	CHAR(16),
		TermKey		CHAR(16),
		LanguageKey	VARCHAR(4)
	)

	-- Get all synonyms for given concept into a temp table. Also return the TermKey.
	INSERT INTO @T
	SELECT DISTINCT	
		T.PlainText,
		S.Concept_Key,
		S.Term_Key,
		T.Language_Key
	FROM	VW_ConceptTerm C
	JOIN	Concept S	ON 	S.Meaning_Key  = C.Meaning_Key
	JOIN	Term T		ON 	T.Term_Key     = S.Term_Key
	WHERE	C.Concept_Key = @ConceptKey

	DECLARE	@NewTermKey CHAR(16), 
		@OldTermKey CHAR(16)

	-- Repeat until all duplicates handled.
	WHILE 1 = 1 BEGIN
		SELECT 	@NewTermKey = T1.TermKey, @OldTermKey = T2.TermKey
		FROM 	@T T1
		JOIN	@T T2 	ON 	T2.PlainText   =  T1.PlainText
				AND	T2.LanguageKey =  T1.LanguageKey
				AND	T2.TermKey     <> T1.TermKey

		-- If nothing selected, nothing more to do.
		IF @@ROWCOUNT = 0 BREAK

		-- Merge terms.
		EXECUTE	usp_DuplicateTerms_Merge @NewTermKey, @OldTermKey

		-- Update to change replaced key with kept key.
		UPDATE	@T SET TermKey = @NewTermKey WHERE TermKey = @OldTermKey
	END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_DuplicateSynonymTerms_Merge') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_DuplicateSynonymTerms_Merge'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_DuplicateSynonymTerms_Merge TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_DuplicateSynonymTerms_Merge TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_DuplicateSynonymTerms_Merge TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_DuplicateSynonymTerms_Merge TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_DuplicateSynonymTerms_Merge TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_DuplicateSynonymTerms_Merge TO [Dev - JNCC SQL]
END
