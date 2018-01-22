IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Stores_Select_ForTopLevel]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
    DROP PROCEDURE [dbo].[usp_Stores_Select_ForTopLevel]
GO


/*===========================================================================*\
  Description:    Returns top level Stores data to the CollectionsBrowser.

  Parameters:
    @Key                Optional Key. When specified, only the single top level record is returned with that key
    @UserDomainMask     User's Domain Mask restricting which records may be returned
    @SessionID          User's SessionID
    @SortOrderIndex     Index determining Sort Order

  Created:    2003-08-15

  Last revision information:
    $Revision: 14 $
    $Date: 15/11/07 11:47 $
    $Author: Ericsalmon $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_Stores_Select_ForTopLevel] 
    @UserDomainMask INT,
    @SessionID      CHAR(16),
    @SortOrderIndex TINYINT,
    @Key            CHAR(16) = NULL
AS
    SET NOCOUNT ON

    -- Create  a table to hold the items we are looking for
    DECLARE @Search TABLE (ItemKey CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS PRIMARY KEY)

    IF @Key IS NOT NULL
        INSERT INTO @Search VALUES (@Key)
    ELSE IF object_id('tempdb..#TempFilter') IS NOT NULL
        INSERT INTO @Search SELECT DISTINCT ItemKey FROM #TempFilter
    ELSE
        INSERT INTO @Search SELECT Collection_Unit_Key FROM Store

    DECLARE @Results	TABLE (
            Item_Key    CHAR(16)        NOT NULL,
            Item_Name   VARCHAR(160)    NOT NULL,
            Hint        VARCHAR(30)     NULL,
            Number      VARCHAR(30)     NULL
    )

    INSERT INTO @Results
    SELECT      S.Collection_Unit_Key,     
                S.Item_Name + IsNull(' - ' + CU.Current_Location_Code, IsNull(' - ' + CU.Usual_Location_Code, '')), 
                CU.Current_Location_Code,
                Number
    FROM        Store                   S
    INNER JOIN  Collection_Unit         CU  ON  CU.Collection_Unit_Key              =   S.Collection_Unit_Key
                                            AND ((CU.Domain_Mask & @UserDomainMask  >   0) 
                                            OR   (CU.Entered_Session_ID             =   @SessionID) 
                                            OR   (CU.Changed_Session_ID             =   @SessionID) 
                                            OR   (CU.Domain_Mask                    =   0))
    LEFT JOIN   Collection_Unit_Number  CUN ON  CUN.Collection_Unit_Key             =   S.Collection_Unit_Key 
                                            AND CUN.Preferred                       =   1
                                            -- Because of historical data, there could be multiple preferred.
                                            -- Only want one though.
                                            AND CUN.Collection_Unit_Number_Key      =   (
                                                SELECT  TOP 1 CUN2.Collection_Unit_Number_Key 
                                                FROM    Collection_Unit_Number CUN2 
                                                WHERE   CUN2.Collection_Unit_Key    =   CUN.Collection_Unit_Key 
                                                AND     CUN2.Preferred              =   1
                                            )
    INNER JOIN  @Search                 SR  ON     SR.ItemKey                       =   CU.Collection_Unit_Key


    IF @SortOrderIndex = 0
        SELECT      *
        FROM        @Results
        ORDER BY    Item_Name, Number
    ELSE
    IF @SortOrderIndex = 1
        SELECT      *
        FROM        @Results
        ORDER BY    Number, Item_Name
    ELSE
    IF @SortOrderIndex = 2
        SELECT      *
        FROM        @Results
        ORDER BY    Hint, Item_Name, Number
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Stores_Select_ForTopLevel') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_Stores_Select_ForTopLevel'
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        GRANT EXECUTE ON dbo.usp_Stores_Select_ForTopLevel TO [R2k_AddOnly]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
        GRANT EXECUTE ON dbo.usp_Stores_Select_ForTopLevel TO [R2k_Administrator]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
        GRANT EXECUTE ON dbo.usp_Stores_Select_ForTopLevel TO [R2k_FullEdit]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
        GRANT EXECUTE ON dbo.usp_Stores_Select_ForTopLevel TO [R2k_ReadOnly]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
        GRANT EXECUTE ON dbo.usp_Stores_Select_ForTopLevel TO [R2k_RecordCardsOnly]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_Stores_Select_ForTopLevel TO [Dev - JNCC SQL]
END

GO