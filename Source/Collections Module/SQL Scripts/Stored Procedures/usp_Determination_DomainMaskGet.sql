SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

/*============================================================================*\
  Drop procedure before re-creating.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_Determination_DomainMaskGet') IS NOT NULL
    DROP PROCEDURE dbo.usp_Determination_DomainMaskGet
GO

/*============================================================================*\
  Description:  Gets the mask of the domain associated with the specified
                determination.

  Parameters:   @DeterminationKey       Identifies the Determination or
                                        TAXON_DETERMINATION record
                @LifeSciences           0 => @DeterminationKey identifies a
                                             Determination record
                                        1 => @DeterminationKey identifies a
                                             TAXON_DETERMINATION record
                @DomainMask             [OUTPUT] The domain mask

  Created:      November 2009

  Last revision information:
    $Revision: 2 $
    $Date: 5/11/09 16:44 $
    $Author: Andrewkemp $
\*============================================================================*/
CREATE PROCEDURE dbo.usp_Determination_DomainMaskGet
    @DeterminationKey                   CHAR(16),
    @LifeSciences                       BIT,
    @DomainMask                         INT OUTPUT
AS
    SET NOCOUNT ON

    DECLARE     @ConceptKey             CHAR(16)

    IF @LifeSciences = 0
    BEGIN
        SELECT      @ConceptKey                         =   e.Concept_Key
        FROM        Determination                       AS  e
        WHERE       e.Determination_Key                 =   @DeterminationKey
    END
    ELSE
    BEGIN
        SELECT      @ConceptKey                         =   m.Concept_Key
        FROM        TAXON_DETERMINATION                 AS  e
        INNER JOIN  Taxon_Dictionary_Concept_Mapping    AS  m
        ON          m.Taxon_List_Item_Key               =   e.TAXON_LIST_ITEM_KEY
        WHERE       e.TAXON_DETERMINATION_KEY           =   @DeterminationKey
    END

    SELECT      @DomainMask             =   d.Domain_Mask
    FROM        Concept                 AS  c
    INNER JOIN  Concept_Group           AS  g
    ON          g.Concept_Group_Key     =   c.Concept_Group_Key
    INNER JOIN  Local_Domain            AS  l
    ON          l.Local_Domain_Key      =   g.Local_Domain_Key
    INNER JOIN  Domain                  AS  d
    ON          d.Domain_Key            =   l.Domain_Key
    WHERE       c.Concept_Key           =   @ConceptKey

    IF @@ROWCOUNT = 0
        SET         @DomainMask             =   0
GO

/*============================================================================*\
  Grant permissions.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_Determination_DomainMaskGet') IS NOT NULL
BEGIN
    PRINT 'Setting up security on procedure usp_Determination_DomainMaskGet'
    IF EXISTS (SELECT * FROM sysusers WHERE NAME = 'R2k_AddOnly')
        GRANT EXECUTE ON dbo.usp_Determination_DomainMaskGet TO R2k_AddOnly
    IF EXISTS (SELECT * FROM sysusers WHERE NAME = 'R2k_Administrator')
        GRANT EXECUTE ON dbo.usp_Determination_DomainMaskGet TO R2k_Administrator
    IF EXISTS (SELECT * FROM sysusers WHERE NAME = 'R2k_FullEdit')
        GRANT EXECUTE ON dbo.usp_Determination_DomainMaskGet TO R2k_FullEdit
    IF EXISTS (SELECT * FROM sysusers WHERE NAME = 'R2k_ReadOnly')
        GRANT EXECUTE ON dbo.usp_Determination_DomainMaskGet TO R2k_ReadOnly
    IF EXISTS (SELECT * FROM sysusers WHERE NAME = 'R2k_RecordCardsOnly')
        GRANT EXECUTE ON dbo.usp_Determination_DomainMaskGet TO R2k_RecordCardsOnly
END
GO
