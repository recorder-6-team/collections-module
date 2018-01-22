SET QUOTED_IDENTIFIER ON
SET NOCOUNT ON
GO

/*============================================================================*\
  Drop procedure before re-creating.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_Domains_Select_ForSpecimen') IS NOT NULL
    DROP PROCEDURE dbo.usp_Domains_Select_ForSpecimen
GO

/*============================================================================*\
  Description:  Gets the domains in which determinations have neen made for
                a particular specimen.

  Parameters:   @ParentKey              Identifies the specimen
                @LifeSciences           0 => consider Determination records
                                        1 => consider TAXON_DETERMINATION
                                             records

  Created:      November 2009

  Last revision information:
    $Revision: 1 $
    $Date: 4/11/09 12:33 $
    $Author: Andrewkemp $
\*============================================================================*/
CREATE PROCEDURE dbo.usp_Domains_Select_ForSpecimen
    @ParentKey                          CHAR(16),
    @LifeSciences                       BIT
AS
    SET NOCOUNT ON

    IF @LifeSciences = 0
    BEGIN
        SELECT DISTINCT
                    LTRIM(STR(d.Domain_Mask))           AS  Item_Key,
                    dbo.ufn_GetDomainsFromMask(
                            d.Domain_Mask)              AS  Item_Name
        FROM        Determination                       AS  e
        INNER JOIN  Concept                             AS  c
        ON          c.Concept_Key                       =   e.Concept_Key
        INNER JOIN  Concept_Group                       AS  g
        ON          g.Concept_Group_Key                 =   c.Concept_Group_Key
        INNER JOIN  Local_Domain                        AS  l
        ON          l.Local_Domain_Key                  =   g.Local_Domain_Key
        INNER JOIN  Domain                              AS  d
        ON          d.Domain_Key                        =   l.Domain_Key
        WHERE       e.Specimen_Collection_Unit_Key      =   @ParentKey
        ORDER BY    Item_Name
    END
    ELSE
    BEGIN
        SELECT DISTINCT
                    LTRIM(STR(d.Domain_Mask))           AS  Item_Key,
                    dbo.ufn_GetDomainsFromMask(
                            d.Domain_Mask)              AS  Item_Name
        FROM        TAXON_DETERMINATION                 AS  e
        INNER JOIN  Taxon_Dictionary_Concept_Mapping    AS  m
        ON          m.Taxon_List_Item_Key               =   e.Taxon_List_Item_Key  
        INNER JOIN  Concept                             AS  c
        ON          c.Concept_Key                       =   m.Concept_Key  
        INNER JOIN  Concept_Group                       AS  g
        ON          g.Concept_Group_Key                 =   c.Concept_Group_Key  
        INNER JOIN  Local_Domain                        AS  l
        ON          l.Local_Domain_Key                  =   g.Local_Domain_Key
        INNER JOIN  Domain                              AS  d
        ON          d.Domain_Key                        =   l.Domain_Key
        WHERE       e.Specimen_Collection_Unit_Key      =   @ParentKey
        ORDER BY    Item_Name
    END
GO

/*============================================================================*\
  Grant permissions.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_Domains_Select_ForSpecimen') IS NOT NULL
BEGIN
    PRINT 'Setting up security on procedure usp_Domains_Select_ForSpecimen'
    IF EXISTS (SELECT * FROM sysusers WHERE NAME = 'R2k_AddOnly')
        GRANT EXECUTE ON dbo.usp_Domains_Select_ForSpecimen TO R2k_AddOnly
    IF EXISTS (SELECT * FROM sysusers WHERE NAME = 'R2k_Administrator')
        GRANT EXECUTE ON dbo.usp_Domains_Select_ForSpecimen TO R2k_Administrator
    IF EXISTS (SELECT * FROM sysusers WHERE NAME = 'R2k_FullEdit')
        GRANT EXECUTE ON dbo.usp_Domains_Select_ForSpecimen TO R2k_FullEdit
    IF EXISTS (SELECT * FROM sysusers WHERE NAME = 'R2k_ReadOnly')
        GRANT EXECUTE ON dbo.usp_Domains_Select_ForSpecimen TO R2k_ReadOnly
    IF EXISTS (SELECT * FROM sysusers WHERE NAME = 'R2k_RecordCardsOnly')
        GRANT EXECUTE ON dbo.usp_Domains_Select_ForSpecimen TO R2k_RecordCardsOnly
END
GO
