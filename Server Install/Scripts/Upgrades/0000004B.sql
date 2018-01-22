SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

/*============================================================================*\
  Drop procedure before re-creating.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_Determinations_Select_ForSpecimenAndDomain') IS NOT NULL
    DROP PROCEDURE dbo.usp_Determinations_Select_ForSpecimenAndDomain
GO

/*============================================================================*\
  Description:  Returns determinations for a particular specimen and domain
                mask.

  Parameters:   @SpecimenCollectionUnitKey  Identifies the specimen
                @DomainMask                 The domain mask

  Created:      November 2009

  Last revision information:
    $Revision: 1 $
    $Date: 14/02/11 14:41 $
    $Author: Jamesbichard $
\*============================================================================*/
CREATE PROCEDURE dbo.usp_Determinations_Select_ForSpecimenAndDomain
    @SpecimenCollectionUnitKey          CHAR(16),
    @DomainMask                         INT,
    @LifeSciences                       BIT,
    @ShowCommonNames                    BIT
AS
    SET NOCOUNT ON

    IF @LifeSciences = 0
    BEGIN
        SELECT      e.Determination_Key                 AS  Item_Key,   
                    t.Item_Name                         AS  Item_Name,   
                    r.Concept_Rank_Key,   
                    r.Color_R,   
                    r.Color_G,   
                    r.Color_B,
					g.Item_Name							AS	Hint   
        FROM        Determination                       AS  e
        INNER JOIN  VW_ConceptTerm                      AS  t
        ON          t.Concept_Key                       =   e.Concept_Key  
        INNER JOIN  Concept_Group                       AS  g
        ON          g.Concept_Group_Key                 =   t.Concept_Group_Key
        INNER JOIN  Local_Domain                        AS  l
        ON          l.Local_Domain_Key                  =   g.Local_Domain_Key
        INNER JOIN  Domain                              AS  d
        ON          d.Domain_Key                        =   l.Domain_Key
        LEFT JOIN   Concept_Rank                        AS  r
        ON          r.Concept_Rank_Key                  =   t.Concept_Rank_Key  
        WHERE       e.Specimen_Collection_Unit_Key      =   @SpecimenCollectionUnitKey
        AND         d.Domain_Mask & @DomainMask         <>  0
        ORDER BY    t.Item_Name
    END
    ELSE
    BEGIN
        SELECT      e.Taxon_Determination_Key           AS  Item_Key,   
                    dbo.ufn_GetFormattedTaxonNameByParams(
                            n.Actual_Name,
                            n.Actual_Name_Italic,
                            n.Common_Name,
                            n.Common_Name_Italic,
                            n.Authority,
                            @ShowCommonNames)           AS  Item_Name,  
                    r.Concept_Rank_Key,
                    r.Color_R,
                    r.Color_G,
                    r.Color_B,
					g.Item_Name							AS	Hint   
        FROM        Specimen_Unit                       AS  u  
        INNER JOIN  TAXON_DETERMINATION                 AS  e  
        ON          e.Specimen_Collection_Unit_Key      =   u.Collection_Unit_Key
        INNER JOIN  INDEX_TAXON_NAME                    AS  n  
        ON          n.Taxon_List_Item_Key               =   e.Taxon_List_Item_Key  
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
        LEFT JOIN   Concept                             AS  sc  
        ON          sc.Concept_Key                      =   e.Nomenclatural_Status_Concept_Key
        LEFT JOIN   Concept_Rank                        AS  r  
        ON          r.Concept_Rank_Key                  =   sc.Concept_Rank_Key
        WHERE       u.Collection_Unit_Key               =   @SpecimenCollectionUnitKey
        AND         d.Domain_Mask & @DomainMask         <>  0
        ORDER BY    Item_Name
    END
GO

/*============================================================================*\
  Grant permissions.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_Determinations_Select_ForSpecimenAndDomain') IS NOT NULL
BEGIN
    PRINT 'Setting up security on procedure usp_Determinations_Select_ForSpecimenAndDomain'
    IF EXISTS (SELECT * FROM sysusers WHERE NAME = 'R2k_AddOnly')
        GRANT EXECUTE ON dbo.usp_Determinations_Select_ForSpecimenAndDomain TO R2k_AddOnly
    IF EXISTS (SELECT * FROM sysusers WHERE NAME = 'R2k_Administrator')
        GRANT EXECUTE ON dbo.usp_Determinations_Select_ForSpecimenAndDomain TO R2k_Administrator
    IF EXISTS (SELECT * FROM sysusers WHERE NAME = 'R2k_FullEdit')
        GRANT EXECUTE ON dbo.usp_Determinations_Select_ForSpecimenAndDomain TO R2k_FullEdit
    IF EXISTS (SELECT * FROM sysusers WHERE NAME = 'R2k_ReadOnly')
        GRANT EXECUTE ON dbo.usp_Determinations_Select_ForSpecimenAndDomain TO R2k_ReadOnly
    IF EXISTS (SELECT * FROM sysusers WHERE NAME = 'R2k_RecordCardsOnly')
        GRANT EXECUTE ON dbo.usp_Determinations_Select_ForSpecimenAndDomain TO R2k_RecordCardsOnly
END
GO
