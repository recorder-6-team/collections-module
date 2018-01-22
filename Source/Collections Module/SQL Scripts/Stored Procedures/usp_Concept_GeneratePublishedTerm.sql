/*===========================================================================*\
  Drop function before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].usp_Concept_GeneratePublishedTerm'))
    DROP PROCEDURE [dbo].usp_Concept_GeneratePublishedTerm
GO

/*===========================================================================*\
  Description:	A wrapper to call the function in the term generator
				table for generating published term

  Parameters:	 

  Created:	August 2011

  Last revision information:
    $Revision: 4 $
    $Date: 2/09/11 15:24 $
    $Author: Jamesbichard $

\*===========================================================================*/

CREATE PROCEDURE dbo.usp_Concept_GeneratePublishedTerm  
	@Plaintext VARCHAR(100),
	@AuthorAndDate VARCHAR(100),
	@Attributes VARCHAR(100),
	@RankKey CHAR(16),
	@ParentKey CHAR(16), 
	@TermGeneratorKey CHAR(16)
AS
BEGIN
	DECLARE @PublishedTermFunction NVARCHAR(257)

	SELECT @PublishedTermFunction = Published_Term_Function
	FROM Term_Generator
	WHERE Term_Generator_Key = @TermGeneratorKey

	
	DECLARE @sql NVARCHAR(4000)
	DECLARE @params NVARCHAR(500)

	SELECT @sql = '
		SELECT ' + @PublishedTermFunction + '(@Plaintext, @AuthorAndDate, @Attributes, @RankKey, @ParentKey)'
	
	SET @params = '@Plaintext NVARCHAR(150), ' +
				'@AuthorAndDate VARCHAR(100), @Attributes VARCHAR(100), ' +
				'@RankKey CHAR(16), @ParentKey CHAR(16)'

	EXEC sp_executesql @sql, @params, 
			@Plaintext = @Plaintext,
			@AuthorAndDate = @AuthorAndDate,
			@Attributes = @Attributes,
			@RankKey = @RankKey,
			@ParentKey = @ParentKey
END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_GeneratePublishedTerm') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Concept_GeneratePublishedTerm'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
			GRANT EXECUTE ON dbo.usp_Concept_GeneratePublishedTerm TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concept_GeneratePublishedTerm TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concept_GeneratePublishedTerm TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Concept_GeneratePublishedTerm TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Concept_GeneratePublishedTerm TO [Dev - JNCC SQL]
END
GO
