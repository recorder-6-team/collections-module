SET QUOTED_IDENTIFIER ON
GO

ALTER TABLE dbo.Collection_Unit_Number
ALTER COLUMN Preferred TINYINT NOT NULL

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_CollectionUnitNumber_Insert]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_CollectionUnitNumber_Insert]
GO
/*===========================================================================*\
  Description:	Inserts a record into the Collection Unit Number table.
  Parameters:	@Key 
		@CollectionUnitKey 
		@Number 
		@TypeConceptKey 
		@Preferred
		@Notes 
		@SessionID 

  Created:	July 2003

  Last revision information:
    $Revision: 2 $
    $Date: 5/11/09 16:51 $
    $Author: Andrewkemp $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_CollectionUnitNumber_Insert]
	@Key char(16) OUTPUT,
	@CollectionUnitKey char(16),
	@Number varchar(30),
	@TypeConceptKey char(16),
	@Preferred tinyint,
	@Notes text,
	@SessionID char(16)
	
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF
	
	EXECUTE spNextKey 'Collection_Unit_Number', @Key OUTPUT

	BEGIN TRANSACTION
		-- Make sure every other Collection_Unit_Number record for this Collection unit record and 
		-- this this type are set to not preferred.
		IF @Preferred = 1 BEGIN
			UPDATE 	Collection_Unit_Number
			SET 	Preferred = 0
			WHERE 	Collection_Unit_Key = @CollectionUnitKey
		
			IF @@Error <> 0 GOTO RollbackAndExit
		END
	
		INSERT INTO Collection_Unit_Number (
			Collection_Unit_Number_Key,
			Collection_Unit_Key,
			Number,
			Type_Concept_Key,
			Preferred,
			Notes,
			Entered_Session_ID
		) VALUES (
			@Key,
			@CollectionUnitKey,
			@Number,
			@TypeConceptKey,
			@Preferred,
			@Notes,
			@SessionID )

		IF @@Error <> 0 GOTO RollbackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_CollectionUnitNumber_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_CollectionUnitNumber_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitNumber_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_CollectionUnitNumber_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_CollectionUnitNumber_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_CollectionUnitNumber_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitNumber_Insert TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_CollectionUnitNumber_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_CollectionUnitNumber_Update]
GO

/*===========================================================================*\
  Description:	Updates a record into the Collection Unit Number table

  Parameters:	@Key

		@Timestamp

  Created:	September 2003

  Last revision information:
    $Revision: 2 $
    $Date: 5/11/09 16:51 $
    $Author: Andrewkemp $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_CollectionUnitNumber_Update]
	@Key char(16),
	@CollectionUnitKey char(16),
	@Number varchar(30),
	@TypeConceptKey char(16),
	@Preferred tinyint,
	@Notes text,
	@SessionID char(16),
	@Timestamp timestamp,
	@RecordsAffected int OUTPUT

AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	DECLARE @Error int
	/*---------------------------*\
	  Actually updates the table
	\*---------------------------*/
	BEGIN TRANSACTION

		IF @Preferred = 1
			UPDATE 	Collection_Unit_Number
			SET	Preferred = 0
			WHERE	Collection_Unit_Key = @CollectionUnitKey
			AND	Collection_Unit_Number_Key <> @Key -- So we don't get timestamp problems.
		
		UPDATE 	Collection_Unit_Number
		SET 	Collection_Unit_Key= @CollectionUnitKey,
			Number = @Number,
			Type_Concept_Key = @TypeConceptKey,
			Preferred = @Preferred,
			Notes = @Notes,
			Changed_Session_ID = @SessionID

		WHERE	Collection_Unit_Number_Key = @Key
		AND		[Timestamp] = @Timestamp
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Collection_Unit_Number WHERE Collection_Unit_Number_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_CollectionUnitNumber_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_CollectionUnitNumber_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitNumber_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_CollectionUnitNumber_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_CollectionUnitNumber_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_CollectionUnitNumber_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitNumber_Update TO [Dev - JNCC SQL]
END
GO

SET ANSI_NULLS ON
GO

/*============================================================================*\
  Drop function before re-creating.
\*============================================================================*/
IF OBJECT_ID('dbo.ufn_GetDomainsFromMask') IS NOT NULL
	DROP FUNCTION dbo.ufn_GetDomainsFromMask
GO

/*============================================================================*\
  Description:	Returns a semi-colon delimited list of domain names for the 
				given domain mask.

  Parameters:	@Mask					Mask to decode

  Created:		November 2009

  Last revision information:
    $Revision: 2 $
    $Date: 5/11/09 16:51 $
    $Author: Andrewkemp $

\*============================================================================*/
CREATE FUNCTION dbo.ufn_GetDomainsFromMask(
	@Mask								INT)
	RETURNS								VARCHAR(1000)
AS
BEGIN
	DECLARE		@Name					VARCHAR(100),
				@Domains				VARCHAR(1000)
	
	SET			@Domains				=	''
	
	DECLARE		domain_cursor CURSOR FOR
	SELECT		Item_Name
	FROM		Domain
	WHERE		Domain_Mask & @Mask		>	0
	ORDER BY	Item_Name
	
	OPEN		domain_cursor

	FETCH NEXT
	FROM		domain_cursor
	INTO		@Name

	WHILE @@FETCH_STATUS = 0 
	BEGIN
		IF @Domains <> ''
			SET			@Domains				=	@Domains + '; ' + @Name
		ELSE
			SET			@Domains				=	@Name

		FETCH NEXT
		FROM		domain_cursor
		INTO		@Name
	END

	CLOSE		domain_cursor
	DEALLOCATE	domain_cursor

	RETURN		@Domains
END
GO

/*============================================================================*\
  Grant permissions.
\*============================================================================*/
IF OBJECT_ID('dbo.ufn_GetDomainsFromMask') IS NOT NULL
BEGIN
    PRINT 'Setting up security on function ufn_GetDomainsFromMask'
	IF EXISTS (SELECT * FROM sysusers WHERE NAME = 'R2k_AddOnly')
       	GRANT EXECUTE ON dbo.ufn_GetDomainsFromMask TO R2k_AddOnly
	IF EXISTS (SELECT * FROM sysusers WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.ufn_GetDomainsFromMask TO R2k_Administrator
	IF EXISTS (SELECT * FROM sysusers WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.ufn_GetDomainsFromMask TO R2k_FullEdit
	IF EXISTS (SELECT * FROM sysusers WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.ufn_GetDomainsFromMask TO R2k_ReadOnly
	IF EXISTS (SELECT * FROM sysusers WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.ufn_GetDomainsFromMask TO R2k_RecordCardsOnly
END
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
    $Date: 5/11/09 16:51 $
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
    $Revision: 2 $
    $Date: 5/11/09 16:51 $
    $Author: Andrewkemp $
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
                    r.Color_B   
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
                    r.Color_B   
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
    $Revision: 2 $
    $Date: 5/11/09 16:51 $
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

/*============================================================================*\
  Drop stored proc before re-creating.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_DomainsForMask_Get') IS NOT NULL
	DROP PROCEDURE dbo.usp_DomainsForMask_Get
GO

/*============================================================================*\
  Description:	Returns a semi-colon delimited list of domain names for the 
				given domain mask.

  Parameters:	@Mask					Mask to decode
				@Domains				List of domains

  Created:		August 2003

  Last revision information:
	$Revision: 2 $
	$Date: 5/11/09 16:51 $
	$Author: Andrewkemp $

\*============================================================================*/
CREATE PROCEDURE dbo.usp_DomainsForMask_Get
	@Mask								INT,
	@Domains							VARCHAR(1000) OUTPUT
AS
	SELECT		@Domains				=	dbo.ufn_GetDomainsFromMask(@Mask)
GO

/*============================================================================*\
  Grant permissions.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_DomainsForMask_Get') IS NOT NULL
BEGIN
	PRINT 'Setting up security on procedure usp_DomainsForMask_Get'
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_DomainsForMask_Get TO R2k_AddOnly
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_DomainsForMask_Get TO R2k_Administrator
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_DomainsForMask_Get TO R2k_FullEdit
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_DomainsForMask_Get TO R2k_ReadOnly
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_DomainsForMask_Get TO R2k_RecordCardsOnly
END
GO
