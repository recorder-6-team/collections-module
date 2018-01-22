/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_UpdateAutomaticTerms') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].usp_Concept_UpdateAutomaticTerms
GO

/*===========================================================================*\
  Description:	Update published terms (if required) and search terms for a 
				given concept

  Parameters:	

  Created:	August 2011

  Last revision information:
    $Revision: 6 $
    $Date: 13/05/14 15:59 $
    $Author: Christopherknight $

\*===========================================================================*/
CREATE PROCEDURE [dbo].usp_Concept_UpdateAutomaticTerms
	@ConceptKey 			CHAR(16),
	@AutomaticUpdate		BIT
AS
	DECLARE @TermGeneratorKey CHAR(16)
	SELECT @TermGeneratorKey = dbo.ufn_GetTermGenerator(@ConceptKey, 0, 0)

	IF @AutomaticUpdate = 1
	BEGIN
		DECLARE @Plaintext VARCHAR(100),
				@ParentKey CHAR(16),
				@AuthorAndDate VARCHAR(100),
				@Attributes VARCHAR(100),
				@ConceptRankKey CHAR(16),
				@PublishedTerm NVARCHAR(450)

		SELECT TOP 1 @Plaintext = t.Plaintext,
				@AuthorAndDate = tv.Author_And_Date,
				@Attributes = tv.Version_Label,	
				@ConceptRankKey = c.Concept_Rank_Key,
				@ParentKey = cp.Parent_Concept_Key
		FROM Concept c
		INNER JOIN Term t ON c.Term_Key = t.Term_Key
		LEFT JOIN Term_Version tv ON c.Term_Version_Key = tv.Term_Version_Key
		INNER JOIN Concept_Group cg ON c.Concept_Group_Key = cg.Concept_Group_Key
		LEFT JOIN VW_ConceptChildren cp ON c.Concept_Key = cp.Child_Concept_Key
		WHERE c.Concept_Key = @ConceptKey
	
		CREATE TABLE #PublishedTerm (
			Published_Term NVARCHAR(450)
		)
		
		INSERT INTO #PublishedTerm
		EXEC usp_Concept_GeneratePublishedTerm 
			@Plaintext,
			@AuthorAndDate,
			@Attributes,	
			@ConceptRankKey,
			@ParentKey,
			@TermGeneratorKey

		SELECT @PublishedTerm = Published_Term
		FROM #PublishedTerm
	
		DROP TABLE #PublishedTerm

		UPDATE Concept
		SET Published_Term = @PublishedTerm
		WHERE Concept_Key = @ConceptKey
	END

	EXEC usp_SearchTerm_DeleteOldTerms @ConceptKey
	EXEC usp_SearchTerm_Generate @ConceptKey, @TermGeneratorKey

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/

	PRINT 'Setting up security on procedure usp_Concept_UpdateAutomaticTerms'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_Concept_UpdateAutomaticTerms TO R2k_AddOnly
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concept_UpdateAutomaticTerms TO R2k_Administrator
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concept_UpdateAutomaticTerms TO R2k_FullEdit
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Concept_UpdateAutomaticTerms TO R2k_ReadOnly
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Concept_UpdateAutomaticTerms TO R2k_RecordCardsOnly
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
		GRANT EXECUTE ON dbo.usp_Concept_UpdateAutomaticTerms TO "Dev - JNCC SQL"

GO