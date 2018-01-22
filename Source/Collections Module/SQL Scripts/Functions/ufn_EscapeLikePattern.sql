SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

/*============================================================================*\
  Drop function before re-creating.
\*============================================================================*/
IF OBJECT_ID('dbo.ufn_EscapeLikePattern') IS NOT NULL
    DROP FUNCTION dbo.ufn_EscapeLikePattern
GO

/*============================================================================*\
  Description:  Escapes any LIKE wildcard characters in the given string,
                using '\' as the escape character.

  Created:      September 2017
\*============================================================================*/
CREATE FUNCTION dbo.ufn_EscapeLikePattern(
    @Text                               NVARCHAR(MAX))
    RETURNS                             NVARCHAR(MAX)
AS
BEGIN
    RETURN      REPLACE(REPLACE(REPLACE(REPLACE(@Text,
                '\', '\\'),
                '[', '\['),
                '_', '\_'),
                '%', '\%')
END
GO

/*============================================================================*\
  Grant permissions.
\*============================================================================*/
IF OBJECT_ID('dbo.ufn_EscapeLikePattern') IS NOT NULL
BEGIN
    PRINT 'Setting up security on function ufn_EscapeLikePattern'
    IF EXISTS (SELECT * FROM sysusers WHERE NAME = 'R2k_AddOnly')
        GRANT EXECUTE ON dbo.ufn_EscapeLikePattern TO R2k_AddOnly
    IF EXISTS (SELECT * FROM sysusers WHERE NAME = 'R2k_Administrator')
        GRANT EXECUTE ON dbo.ufn_EscapeLikePattern TO R2k_Administrator
    IF EXISTS (SELECT * FROM sysusers WHERE NAME = 'R2k_FullEdit')
        GRANT EXECUTE ON dbo.ufn_EscapeLikePattern TO R2k_FullEdit
    IF EXISTS (SELECT * FROM sysusers WHERE NAME = 'R2k_ReadOnly')
        GRANT EXECUTE ON dbo.ufn_EscapeLikePattern TO R2k_ReadOnly
    IF EXISTS (SELECT * FROM sysusers WHERE NAME = 'R2k_RecordCardsOnly')
        GRANT EXECUTE ON dbo.ufn_EscapeLikePattern TO R2k_RecordCardsOnly
END
GO
