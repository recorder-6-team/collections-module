SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

/*============================================================================*\
Drop stored proc before re-creating.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_Multimedia_Select_Preferred') IS NOT NULL
    DROP PROCEDURE dbo.usp_Multimedia_Select_Preferred
GO

/*============================================================================*\
Description:    Gets the preferred multimedia item for the specified record.

Parameters:     @TableName              Names the table containing the record.
                @RecordKey              Identifies the record.

Created:        September 2017
\*============================================================================*/
CREATE PROCEDURE dbo.usp_Multimedia_Select_Preferred
    @TableName                          VARCHAR(50),
    @RecordKey                          CHAR(16) OUTPUT
AS
    SELECT      f.SOURCE_KEY,
                f.FILE_NAME,
                f.Title,
                f.Timestamp
    FROM        dbo.Source_Join         AS  j
    INNER JOIN  dbo.SOURCE_FILE         AS  f
    ON          f.SOURCE_KEY            =   j.Source_Key
    AND         f.Preferred             =   1
    WHERE       j.Table_Name            =   @TableName
    AND         j.Record_Key            =   @RecordKey
GO

/*============================================================================*\
Grant permissions.
\*============================================================================*/
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
    GRANT EXECUTE ON dbo.usp_Multimedia_Select_Preferred TO R2k_AddOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
    GRANT EXECUTE ON dbo.usp_Multimedia_Select_Preferred TO R2k_Administrator
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
    GRANT EXECUTE ON dbo.usp_Multimedia_Select_Preferred TO R2k_FullEdit
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
    GRANT EXECUTE ON dbo.usp_Multimedia_Select_Preferred TO R2k_RecordCardsOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
    GRANT EXECUTE ON dbo.usp_Multimedia_Select_Preferred TO "Dev - JNCC SQL"
GO
