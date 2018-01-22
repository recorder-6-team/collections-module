/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
       FROM   SysObjects 
       WHERE  Id = Object_Id(N'[dbo].[usp_InternalReferences_Select_ForSearch]')
       AND    Type = 'P')
    DROP PROCEDURE [dbo].[usp_InternalReferences_Select_ForSearch]
GO

/*===========================================================================*\
  Description:  Search procedure for Internal References

  Parameters:   @SearchText 

  Created:  November 2003

  Last revision information:
    $Revision: 4 $
    $Date: 10/10/12 11:05 $
    $Author: Alexanderpadley $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_InternalReferences_Select_ForSearch]
    @SearchText VARCHAR(100)
AS
    SET NOCOUNT ON

    SELECT      r.Source_Key                                AS  Item_Key,
                ISNULL(a.Author
                + ' - '
                + dbo.ufn_GetDateFromVagueDate(
                    r.Year_Vague_Date_Start,
                    r.Year_Vague_Date_End,
                    r.Year_Vague_Date_Type)
                + ', '
                + dbo.ufn_RtfToPlainText(r.Full_Reference),'')  AS  DisplayTerm,
                ISNULL(a.Author
                + ' - '
                + dbo.ufn_GetDateFromVagueDate(
                    r.Year_Vague_Date_Start,
                    r.Year_Vague_Date_End,
                    r.Year_Vague_Date_Type)
                + ', '
                + dbo.ufn_RtfToPlainText(r.Full_Reference),'')  AS  SearchTerm
    FROM        Reference                                   AS  r
    INNER JOIN  VW_Reference_Authors                        AS  a
    ON          a.Source_Key                                =   r.Source_Key
    WHERE       a.Author                                    LIKE @SearchText + '%'
    OR          dbo.ufn_RtfToPlainText(r.Title)             LIKE @SearchText + '%'
    OR          dbo.ufn_RtfToPlainText(r.Full_Reference)    LIKE @SearchText + '%'
    ORDER BY    a.Author,
                r.Year_Vague_Date_Start,
                dbo.ufn_RtfToPlainText(r.Full_Reference)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_InternalReferences_Select_ForSearch') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_InternalReferences_Select_ForSearch'
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
            GRANT EXECUTE ON dbo.usp_InternalReferences_Select_ForSearch TO [R2k_AddOnly]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
        GRANT EXECUTE ON dbo.usp_InternalReferences_Select_ForSearch TO [R2k_Administrator]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
        GRANT EXECUTE ON dbo.usp_InternalReferences_Select_ForSearch TO [R2k_FullEdit]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
        GRANT EXECUTE ON dbo.usp_InternalReferences_Select_ForSearch TO [R2k_ReadOnly]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
        GRANT EXECUTE ON dbo.usp_InternalReferences_Select_ForSearch TO [R2k_RecordCardsOnly]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_InternalReferences_Select_ForSearch TO [Dev - JNCC SQL]
END

GO