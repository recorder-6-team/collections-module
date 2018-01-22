SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

/*============================================================================*\
	Drop function before re-creating.
\*============================================================================*/
IF OBJECT_ID(N'dbo.ufn_GeneratePublishedTermBasicTermRankAuthority') IS NOT NULL
	DROP FUNCTION dbo.ufn_GeneratePublishedTermBasicTermRankAuthority
GO

/*============================================================================*\
	Description:
		Function used to calculate a published term in the format
		<i>[Basic Term]</i> [Rank Abbreviation] <i>[Basic Term]</i> [Authority]
		See CCN 165

	Created: February 2014

	Last revision information:
		$Revision: 2 $
		$Date: 18/02/15 19:29 $
		$Author: Simonwood $
\*============================================================================*/

CREATE FUNCTION dbo.ufn_GeneratePublishedTermBasicTermRankAuthority
(
	@Plaintext NVARCHAR(150),
	@AuthorAndDate VARCHAR(100),
	@Attributes VARCHAR(100),
	@RankKey CHAR(16),
	@ParentConceptKey CHAR(16)
)
RETURNS NVARCHAR(256)
AS
BEGIN
	DECLARE @RankAbbreviation  VARCHAR(10)
	DECLARE @ParentAuthority VARCHAR(100)
	DECLARE @LastSpace INT
	DECLARE @Len INT

	SET @PlainText = LTRIM(RTRIM(@Plaintext))

	SET @RankAbbreviation = (SELECT Abbreviation
		FROM dbo.Concept_Rank
		WHERE Concept_Rank_Key = @RankKey)
	SET @ParentAuthority = (SELECT cg.Authority
		FROM dbo.Concept_Group cg
		INNER JOIN dbo.Concept c
		ON cg.Concept_Group_Key = c.Concept_Group_Key
		WHERE c.Concept_Key = @ParentConceptKey)

	
	SET @LastSpace = CharIndex(' ', REVERSE(@Plaintext))
	SET @Len = LEN(@Plaintext)


	RETURN '<i>' + LEFT(@Plaintext, @Len -@LastSpace)  + '</i> ' + ISNULL(@RankAbbreviation, '') + '<i>'  + 
	RIGHT(@Plaintext, @LastSpace) +  '</i> '
	+ ISNULL(@AuthorAndDate, '')
END
GO

/*============================================================================*\
	Grant permissions.
\*============================================================================*/
PRINT 'Setting up security on function ufn_GeneratePublishedTermBasicTermRankAuthority'

IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
    GRANT EXECUTE ON dbo.ufn_GeneratePublishedTermBasicTermRankAuthority TO R2k_AddOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
	GRANT EXECUTE ON dbo.ufn_GeneratePublishedTermBasicTermRankAuthority TO R2k_Administrator
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
	GRANT EXECUTE ON dbo.ufn_GeneratePublishedTermBasicTermRankAuthority TO R2k_FullEdit
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
	GRANT EXECUTE ON dbo.ufn_GeneratePublishedTermBasicTermRankAuthority TO R2k_ReadOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
	GRANT EXECUTE ON dbo.ufn_GeneratePublishedTermBasicTermRankAuthority TO R2k_RecordCardsOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
    GRANT EXECUTE ON dbo.ufn_GeneratePublishedTermBasicTermRankAuthority TO "Dev - JNCC SQL"
GO