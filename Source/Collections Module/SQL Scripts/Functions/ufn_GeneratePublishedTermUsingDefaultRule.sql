SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

/*============================================================================*\
	Drop function before re-creating.
\*============================================================================*/
IF OBJECT_ID(N'dbo.ufn_GeneratePublishedTermUsingDefaultRule') IS NOT NULL
	DROP FUNCTION dbo.ufn_GeneratePublishedTermUsingDefaultRule
GO

/*============================================================================*\
	Description:
		The default function used to calculate a full published term based on
		the supplied concept information. Simply returns the given plaintext.

	Created: July 2011

	Last revision information:
		$Revision: 3 $
		$Date: 2/08/12 11:30 $
		$Author: Andrewvanbreda $
\*============================================================================*/

CREATE FUNCTION dbo.ufn_GeneratePublishedTermUsingDefaultRule
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
	RETURN @Plaintext + ' ' + ISNULL(@Attributes, '') + ' ' + ISNULL(@AuthorAndDate, '')
END
GO

/*============================================================================*\
	Grant permissions.
\*============================================================================*/
PRINT 'Setting up security on function ufn_GeneratePublishedTermUsingDefaultRule'

IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
    GRANT EXECUTE ON dbo.ufn_GeneratePublishedTermUsingDefaultRule TO R2k_AddOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
	GRANT EXECUTE ON dbo.ufn_GeneratePublishedTermUsingDefaultRule TO R2k_Administrator
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
	GRANT EXECUTE ON dbo.ufn_GeneratePublishedTermUsingDefaultRule TO R2k_FullEdit
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
	GRANT EXECUTE ON dbo.ufn_GeneratePublishedTermUsingDefaultRule TO R2k_ReadOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
	GRANT EXECUTE ON dbo.ufn_GeneratePublishedTermUsingDefaultRule TO R2k_RecordCardsOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
    GRANT EXECUTE ON dbo.ufn_GeneratePublishedTermUsingDefaultRule TO "Dev - JNCC SQL"
GO