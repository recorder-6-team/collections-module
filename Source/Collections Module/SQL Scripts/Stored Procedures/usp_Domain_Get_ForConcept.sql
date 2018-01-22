SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

/*============================================================================*\
Drop stored proc before re-creating.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_Domain_Get_ForConcept') IS NOT NULL
    DROP PROCEDURE dbo.usp_Domain_Get_ForConcept
GO

/*============================================================================*\
Description:    Gets the domain key for the specified concept.

Parameters:     @ConceptKey             Identifies the concept.
                @DomainKey              Identifies the corresponding domain.

Created:        September 2017
\*============================================================================*/
CREATE PROCEDURE dbo.usp_Domain_Get_ForConcept
    @ConceptKey                         CHAR(16),
    @DomainKey                          CHAR(16) OUTPUT
AS
    SELECT      @DomainKey              =   l.Domain_Key
    FROM        dbo.Concept             AS  c
    INNER JOIN  dbo.Concept_Group       AS  g
    ON          g.Concept_Group_Key     =   c.Concept_Group_Key
    INNER JOIN  dbo.Local_Domain        AS  l
    ON          l.Local_Domain_Key      =   g.Local_Domain_Key
    WHERE       c.Concept_Key           =   @ConceptKey
GO

/*============================================================================*\
Grant permissions.
\*============================================================================*/
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
    GRANT EXECUTE ON dbo.usp_Domain_Get_ForConcept TO R2k_AddOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
    GRANT EXECUTE ON dbo.usp_Domain_Get_ForConcept TO R2k_Administrator
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
    GRANT EXECUTE ON dbo.usp_Domain_Get_ForConcept TO R2k_FullEdit
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
    GRANT EXECUTE ON dbo.usp_Domain_Get_ForConcept TO R2k_RecordCardsOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
    GRANT EXECUTE ON dbo.usp_Domain_Get_ForConcept TO "Dev - JNCC SQL"
GO
