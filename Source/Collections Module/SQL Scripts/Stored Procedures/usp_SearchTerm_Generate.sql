/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].usp_SearchTerm_Generate')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].usp_SearchTerm_Generate
GO

/*===========================================================================*\
  Description:	Generate search terms for a concept

  Parameters:	

  Created:	August 2011

  Last revision information:
    $Revision: 5 $
    $Date: 17/08/11 9:18 $
    $Author: Jamesbichard $

\*===========================================================================*/

CREATE PROCEDURE dbo.usp_SearchTerm_Generate 
	@ConceptKey CHAR(16), 
	@TermGeneratorKey CHAR(16)
AS
	DECLARE @SearchTermsProc NVARCHAR(257)

	SELECT @SearchTermsProc = Search_Term_Procedure
	FROM Term_Generator
	WHERE Term_Generator_Key = @TermGeneratorKey

	DECLARE @Plaintext NVARCHAR(150),
			@AuthorAndDate VARCHAR(100),
			@Attributes VARCHAR(100),
			@RankKey CHAR(16),
			@ParentKey CHAR(16),
			@PublishedTerm NVARCHAR(450),
			@HierarchyRelationTypeKey CHAR(16)
	SELECT 
		@Plaintext = t.Plaintext,
		@AuthorAndDate = tv.Author_And_Date,
		@Attributes = tv.Version_Label,
		@RankKey = c.Concept_Rank_Key,
		@PublishedTerm = c.Published_Term
	FROM Concept c
	LEFT JOIN Term_Version tv ON tv.Term_Version_Key = c.Term_Version_Key
	INNER JOIN Term t ON t.Term_Key = c.Term_Key
	WHERE c.Concept_Key	= @ConceptKey

	SELECT @HierarchyRelationTypeKey = cg.Hierarchy_Relation_Type_Key
	FROM Concept_Group cg
	INNER JOIN Concept c ON c.Concept_Group_Key = cg.Concept_Group_Key
	WHERE c.Concept_Key = @ConceptKey

	IF @HierarchyRelationTypeKey IS NOT NULL
	BEGIN
		CREATE TABLE #ParentKeys (
			Parent_Key CHAR(16),
			Item_Name VARCHAR(100),
			Sort_Code INT,
			HasParents BIT,
			Rank_Key CHAR(16)
		)

		INSERT INTO #ParentKeys
		EXEC usp_Concept_Select_ForChild @ConceptKey, @HierarchyRelationTypeKey
	
		SELECT @ParentKey = Parent_Key
		FROM #ParentKeys
	
		DROP TABLE #ParentKeys
	END

	DECLARE @sql NVARCHAR(4000)
	DECLARE @params NVARCHAR(500)

	SELECT @sql = '
		CREATE TABLE #SearchTerms (
			Term NVARCHAR(450))

		INSERT INTO #SearchTerms
		EXEC ' + @SearchTermsProc + ' @Plaintext, @AuthorAndDate, ' + 
				'@Attributes, @RankKey, ' +
				'@ParentKey, @PublishedTerm
		
		DECLARE @term NVARCHAR(450),
				@searchTermKey CHAR(16)
		DECLARE terms CURSOR LOCAL FAST_FORWARD FOR
		SELECT * FROM #SearchTerms
		
		OPEN terms

		WHILE 1 = 1
		BEGIN
			FETCH		terms
			INTO        @term
	
			IF @@FETCH_STATUS <> 0 BREAK
			EXEC usp_SearchTerm_Insert @searchTermKey, ''' + @ConceptKey + ''', 1, @term
		END

		CLOSE terms
		DEALLOCATE terms
		DROP TABLE #SearchTerms'

	SET @params = '@Plaintext NVARCHAR(150), @PublishedTerm NVARCHAR(450), ' +
					'@AuthorAndDate VARCHAR(100), @Attributes VARCHAR(100), ' +
					'@RankKey CHAR(16), @ParentKey CHAR(16)'

	EXEC sp_executesql @sql, @params, 
			@Plaintext = @Plaintext,
			@PublishedTerm = @PublishedTerm,
			@AuthorAndDate = @AuthorAndDate,
			@Attributes = @Attributes,
			@RankKey = @RankKey,
			@ParentKey = @ParentKey
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SearchTerm_Generate') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SearchTerm_Generate'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
			GRANT EXECUTE ON dbo.usp_SearchTerm_Generate TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SearchTerm_Generate TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SearchTerm_Generate TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SearchTerm_Generate TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SearchTerm_Generate TO [Dev - JNCC SQL]
END
GO
