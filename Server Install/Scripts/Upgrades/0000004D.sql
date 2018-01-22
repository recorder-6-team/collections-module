SET QUOTED_IDENTIFIER ON 
SET ANSI_NULLS ON 
GO

/*============================================================================*\
  Drop view before re-creating.
\*============================================================================*/
IF OBJECT_ID('dbo.VW_SpecimenDetsEarth') IS NOT NULL
    DROP VIEW dbo.VW_SpecimenDetsEarth
GO

IF OBJECT_ID('dbo.vw_EarthSpecimenDeterminationsBySpecimen') IS NOT NULL
	DROP VIEW dbo.vw_EarthSpecimenDeterminationsBySpecimen
GO

IF OBJECT_ID('dbo.vw_EarthSpecimenDeterminationsBySpecimenPreferred') IS NOT NULL
	DROP VIEW dbo.vw_EarthSpecimenDeterminationsBySpecimenPreferred
GO

/*============================================================================*\
  Create helper views; these are necessary because we cannot create an index
  directly on a view that contains a UNION.
\*============================================================================*/
GO

CREATE VIEW dbo.vw_EarthSpecimenDeterminationsBySpecimen
	WITH SCHEMABINDING
AS	
	SELECT		u.Collection_Unit_Key, 
				d.Concept_Key,
				u.Preferred_Determination_Key,
				d.Determination_Key
	FROM		dbo.Specimen_Unit				AS	u
	INNER JOIN	dbo.Determination				AS	d
	ON			d.Specimen_Collection_Unit_Key	=	u.Collection_Unit_Key
	WHERE		u.Life_Sciences					=	0	
GO

CREATE VIEW dbo.vw_EarthSpecimenDeterminationsBySpecimenPreferred
	WITH SCHEMABINDING
AS	
	SELECT		u.Collection_Unit_Key, 
				d.Concept_Key,
				u.Preferred_Determination_Key,
				d.Determination_Key
	FROM		dbo.Specimen_Unit				AS	u
	INNER JOIN	dbo.Determination				AS	d
	ON			d.Determination_Key				=	u.Preferred_Determination_Key
	WHERE		u.Life_Sciences					=	0	
GO

/*============================================================================*\
  Create indexes - possible because the views are schema bound.
\*============================================================================*/
SET ARITHABORT ON 
SET NUMERIC_ROUNDABORT OFF
GO

CREATE UNIQUE CLUSTERED INDEX UQ_EarthSpecimenDeterminationsBySpecimen
ON dbo.vw_EarthSpecimenDeterminationsBySpecimen (Collection_Unit_Key, Determination_Key)
GO

CREATE INDEX IX_EarthSpecimenDeterminationsBySpecimen_Concept
ON dbo.vw_EarthSpecimenDeterminationsBySpecimen (Concept_Key)
GO

CREATE UNIQUE CLUSTERED INDEX UQ_EarthSpecimenDeterminationsBySpecimenPreferred
ON dbo.vw_EarthSpecimenDeterminationsBySpecimenPreferred (Collection_Unit_Key, Determination_Key)
GO

CREATE INDEX IX_EarthSpecimenDeterminationsBySpecimenPreferred_Concept
ON dbo.vw_EarthSpecimenDeterminationsBySpecimenPreferred (Concept_Key)
GO

SET NUMERIC_ROUNDABORT OFF 
SET ARITHABORT  OFF 
GO

/*===========================================================================*\
  Description:	View that specimens with their determinations

  Created:	June 2004

  Last revision information:
    $Revision: 1 $
    $Date: 21/02/11 18:08 $
    $Author: Andrewkemp $

\*===========================================================================*/
CREATE VIEW VW_SpecimenDetsEarth
WITH SCHEMABINDING
AS
	SELECT		Collection_Unit_Key, 
				Concept_Key,
				Preferred_Determination_Key,
				Determination_Key
	FROM		dbo.vw_EarthSpecimenDeterminationsBySpecimen
	UNION
	SELECT		Collection_Unit_Key, 
				Concept_Key,
				Preferred_Determination_Key,
				Determination_Key
	FROM		dbo.vw_EarthSpecimenDeterminationsBySpecimenPreferred
GO

/*============================================================================*\
  Grant permissions.
\*============================================================================*/
GRANT SELECT ON dbo.VW_SpecimenDetsEarth TO Public
GO

SET QUOTED_IDENTIFIER ON 
SET ANSI_NULLS ON 
GO

/*============================================================================*\
  Drop view before re-creating.
\*============================================================================*/
IF OBJECT_ID('dbo.VW_SpecimenDetsLife') IS NOT NULL
    DROP VIEW dbo.VW_SpecimenDetsLife
GO

IF OBJECT_ID('dbo.vw_LifeSpecimenDeterminationsBySpecimen') IS NOT NULL
	DROP VIEW dbo.vw_LifeSpecimenDeterminationsBySpecimen
GO

IF OBJECT_ID('dbo.vw_LifeSpecimenDeterminationsBySpecimenPreferred') IS NOT NULL
	DROP VIEW dbo.vw_LifeSpecimenDeterminationsBySpecimenPreferred
GO

/*============================================================================*\
  Create helper views; these are necessary because we cannot create an index
  directly on a view that contains a UNION.
\*============================================================================*/
GO

CREATE VIEW dbo.vw_LifeSpecimenDeterminationsBySpecimen
	WITH SCHEMABINDING
AS	
	SELECT		u.Collection_Unit_Key, 
				d.Taxon_List_Item_Key,
				u.Preferred_Taxon_Determination_Key,
				d.Taxon_Determination_Key				
	FROM		dbo.Specimen_Unit			AS	u
	INNER JOIN	dbo.Taxon_Determination		AS	d
	ON			u.Collection_Unit_Key		=	d.Specimen_Collection_Unit_Key
	WHERE		u.Life_Sciences				=	1
GO

CREATE VIEW dbo.vw_LifeSpecimenDeterminationsBySpecimenPreferred
	WITH SCHEMABINDING
AS	
	SELECT		u.Collection_Unit_Key,
				d.Taxon_List_Item_Key,
				u.Preferred_Taxon_Determination_Key,
				d.Taxon_Determination_Key
	FROM		dbo.Specimen_Unit			AS	u
	INNER JOIN	dbo.Taxon_Determination		AS	d
	ON			d.Taxon_Determination_Key	=	u.Preferred_Taxon_Determination_Key
	WHERE		u.Life_Sciences				=	1
GO

/*============================================================================*\
  Create indexes - possible because the views are schema bound.
\*============================================================================*/
SET ARITHABORT ON 
SET NUMERIC_ROUNDABORT OFF
GO

CREATE UNIQUE CLUSTERED INDEX UQ_LifeSpecimenDeterminationsBySpecimen
ON dbo.vw_LifeSpecimenDeterminationsBySpecimen (Collection_Unit_Key, Taxon_Determination_Key)
GO

CREATE INDEX IX_LifeSpecimenDeterminationsBySpecimen_ListItem
ON dbo.vw_LifeSpecimenDeterminationsBySpecimen (Taxon_List_Item_Key)
GO

CREATE UNIQUE CLUSTERED INDEX UQ_LifeSpecimenDeterminationsBySpecimenPreferred
ON dbo.vw_LifeSpecimenDeterminationsBySpecimenPreferred (Collection_Unit_Key, Taxon_Determination_Key)
GO

CREATE INDEX IX_LifeSpecimenDeterminationsBySpecimenPreferred_ListItem
ON dbo.vw_LifeSpecimenDeterminationsBySpecimenPreferred (Taxon_List_Item_Key)
GO

SET NUMERIC_ROUNDABORT OFF 
SET ARITHABORT  OFF 
GO

/*============================================================================*\
  Description:	View that specimens with their determinations
			Note that this view is recreated after import by 
			usp_PostImport_Collections, so any edits should be reflected there

  Created:	June 2004

  Last revision information:
    $Revision: 1 $
    $Date: 21/02/11 18:08 $
    $Author: Andrewkemp $

\*============================================================================*/
CREATE VIEW dbo.VW_SpecimenDetsLife
	WITH SCHEMABINDING
AS
	SELECT		Collection_Unit_Key,
				Taxon_List_Item_Key,
				Preferred_Taxon_Determination_Key,
				Taxon_Determination_Key
	FROM		dbo.vw_LifeSpecimenDeterminationsBySpecimen
	UNION
	SELECT		Collection_Unit_Key,
				Taxon_List_Item_Key,
				Preferred_Taxon_Determination_Key,
				Taxon_Determination_Key
	FROM		dbo.vw_LifeSpecimenDeterminationsBySpecimenPreferred	
GO

/*============================================================================*\
  Grant permissions.
\*============================================================================*/
GRANT SELECT ON dbo.VW_SpecimenDetsLife TO Public
GO

SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

/*============================================================================*\
Drop stored proc before re-creating.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_Specimen_SetPreferredDeterminationFromOccurrence') IS NOT NULL
	DROP PROCEDURE dbo.usp_Specimen_SetPreferredDeterminationFromOccurrence
GO	

/*============================================================================*\
Description:	Sets the preferred determination or preferred taxon
				determination of a specimen to the preferred determination of
				the specified occurrence or taxon occurrence.

Parameters:		@SpecimenCollectionUnitKey	Identifies the specimen.
				@OccurrenceKey				Identifies the occurrence.
				@TaxonOccurrenceKey			Identifies the taxon occurrence.

Created:		February 2011

Last revision information:
	$Revision: 1 $
	$Date: 21/02/11 18:08 $
	$Author: Andrewkemp $
\*============================================================================*/
CREATE PROCEDURE dbo.usp_Specimen_SetPreferredDeterminationFromOccurrence
	@SpecimenCollectionUnitKey			CHAR(16),
	@OccurrenceKey						CHAR(16),
	@TaxonOccurrenceKey					CHAR(16)
AS
	DECLARE			@DeterminedItemKey	CHAR(16),
					@ConceptKey			CHAR(16),
					@ConceptMask		INT
					
	IF @TaxonOccurrenceKey <> ''
	BEGIN
		UPDATE		u
		SET			u.Preferred_Taxon_Determination_Key	=	d.Taxon_Determination_Key,
					@DeterminedItemKey					=	d.Taxon_List_Item_Key
		FROM		dbo.Specimen_Unit					AS	u,
					dbo.TAXON_DETERMINATION				AS	d
		WHERE		u.Collection_Unit_Key				=	@SpecimenCollectionUnitKey
		AND			d.TAXON_OCCURRENCE_KEY				=	@TaxonOccurrenceKey
		AND			d.Preferred							=	1

		SELECT		@ConceptKey							=	Concept_Key
		FROM		Taxon_Dictionary_Concept_Mapping
		WHERE		Taxon_List_Item_Key					=	@DeterminedItemKey

	END
	ELSE
	BEGIN
		UPDATE		u
		SET			u.Preferred_Determination_Key		=	d.Determination_Key,
					@ConceptKey							=	d.Concept_Key
		FROM		dbo.Specimen_Unit					AS	u,
					dbo.Determination					AS	d
		WHERE		u.Collection_Unit_Key				=	@SpecimenCollectionUnitKey
		AND			d.Occurrence_Key					=	@OccurrenceKey
		AND			d.Preferred							=	1
	END
	
	EXECUTE		dbo.usp_Get_Concept_Domain_Mask				@ConceptKey,
															@ConceptMask OUTPUT
													
	EXECUTE		dbo.usp_CollectionUnit_Update_DomainMask	@SpecimenCollectionUnitKey,
															@ConceptMask,
															1
GO

/*============================================================================*\
Grant permissions.
\*============================================================================*/
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
   	GRANT EXECUTE ON dbo.usp_Specimen_SetPreferredDeterminationFromOccurrence TO R2k_AddOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
	GRANT EXECUTE ON dbo.usp_Specimen_SetPreferredDeterminationFromOccurrence TO R2k_Administrator
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
	GRANT EXECUTE ON dbo.usp_Specimen_SetPreferredDeterminationFromOccurrence TO R2k_FullEdit
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
	GRANT EXECUTE ON dbo.usp_Specimen_SetPreferredDeterminationFromOccurrence TO R2k_RecordCardsOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
   	GRANT EXECUTE ON dbo.usp_Specimen_SetPreferredDeterminationFromOccurrence TO "Dev - JNCC SQL"
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_PreImport_Collections]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_PreImport_Collections]
GO

/*===========================================================================*\
  Description:	Drop constraints that impede import of collections data

  Created:	Oct 2004

  Last revision information:
    $Revision: 1 $
    $Date: 21/02/11 18:08 $
    $Author: Andrewkemp $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_PreImport_Collections]
AS

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[FK_Specimen_Unit_Determination]') and OBJECTPROPERTY(id, N'IsForeignKey') = 1)
ALTER TABLE [dbo].[Specimen_Unit] DROP CONSTRAINT FK_Specimen_Unit_Determination

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[FK_Specimen_Unit_TAXON_DETERMINATION]') and OBJECTPROPERTY(id, N'IsForeignKey') = 1)
ALTER TABLE [dbo].[Specimen_Unit] DROP CONSTRAINT FK_Specimen_Unit_TAXON_DETERMINATION

--Remove the indexed view indices that affects Taxon_Determination table
--because they can prevent imports

IF EXISTS (	SELECT		1
			FROM		SysIndexes
			WHERE		Name		=	'IX_LifeSpecimenDeterminationsBySpecimen_ListItem'
			AND			ID			=	OBJECT_ID('dbo.vw_LifeSpecimenDeterminationsBySpecimen'))
	DROP INDEX	dbo.vw_LifeSpecimenDeterminationsBySpecimen.IX_LifeSpecimenDeterminationsBySpecimen_ListItem
	
IF EXISTS (	SELECT		1
			FROM		SysIndexes
			WHERE		Name		=	'UQ_LifeSpecimenDeterminationsBySpecimen'
			AND			ID			=	OBJECT_ID('dbo.vw_LifeSpecimenDeterminationsBySpecimen'))
	DROP INDEX	dbo.vw_LifeSpecimenDeterminationsBySpecimen.UQ_LifeSpecimenDeterminationsBySpecimen

IF EXISTS (	SELECT		1
			FROM		SysIndexes
			WHERE		Name		=	'IX_LifeSpecimenDeterminationsBySpecimenPreferred_ListItem'
			AND			ID			=	OBJECT_ID('dbo.vw_LifeSpecimenDeterminationsBySpecimenPreferred'))
	DROP INDEX	dbo.vw_LifeSpecimenDeterminationsBySpecimenPreferred.IX_LifeSpecimenDeterminationsBySpecimenPreferred_ListItem
	
IF EXISTS (	SELECT		1
			FROM		SysIndexes
			WHERE		Name		=	'UQ_LifeSpecimenDeterminationsBySpecimenPreferred'
			AND			ID			=	OBJECT_ID('dbo.vw_LifeSpecimenDeterminationsBySpecimenPreferred'))
	DROP INDEX	dbo.vw_LifeSpecimenDeterminationsBySpecimenPreferred.UQ_LifeSpecimenDeterminationsBySpecimenPreferred
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_PreImport_Collections') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_PreImport_Collections'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_PreImport_Collections TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_PreImport_Collections TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_PreImport_Collections TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_PostImport_Collections]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_PostImport_Collections]
GO

/*===========================================================================*\
  Description:	Recreate constraints that impede import of collections data

  Created:	Oct 2004

  Last revision information:
    $Revision: 1 $
    $Date: 21/02/11 18:08 $
    $Author: Andrewkemp $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_PostImport_Collections]
AS

IF NOT EXISTS (select * from dbo.sysobjects where id = object_id(N'[dbo].[FK_Specimen_Unit_Determination]') and OBJECTPROPERTY(id, N'IsForeignKey') = 1)
ALTER TABLE [dbo].[Specimen_Unit] ADD 
	CONSTRAINT [FK_Specimen_Unit_Determination] FOREIGN KEY 
	(
		[Preferred_Determination_Key]
	) REFERENCES [dbo].[Determination] (
		[Determination_Key]
	)

IF NOT EXISTS (select * from dbo.sysobjects where id = object_id(N'[dbo].[FK_Specimen_Unit_TAXON_DETERMINATION]') and OBJECTPROPERTY(id, N'IsForeignKey') = 1)
ALTER TABLE [dbo].[Specimen_Unit] ADD 
	CONSTRAINT [FK_Specimen_Unit_TAXON_DETERMINATION] FOREIGN KEY 
	(
		[Preferred_Taxon_Determination_Key]
	) REFERENCES [dbo].[TAXON_DETERMINATION] (
		[TAXON_DETERMINATION_KEY]
	)

	/*===========================================================================*\
	  Create indexes on VW_SpecimenDetsLife that we dropped pre-import
	\*===========================================================================*/
	SET ARITHABORT ON 
	SET NUMERIC_ROUNDABORT OFF

	CREATE UNIQUE CLUSTERED INDEX UQ_LifeSpecimenDeterminationsBySpecimen
	ON dbo.vw_LifeSpecimenDeterminationsBySpecimen (Collection_Unit_Key, Taxon_Determination_Key)

	CREATE INDEX IX_LifeSpecimenDeterminationsBySpecimen_ListItem
	ON dbo.vw_LifeSpecimenDeterminationsBySpecimen (Taxon_List_Item_Key)

	CREATE UNIQUE CLUSTERED INDEX UQ_LifeSpecimenDeterminationsBySpecimenPreferred
	ON dbo.vw_LifeSpecimenDeterminationsBySpecimenPreferred (Collection_Unit_Key, Taxon_Determination_Key)

	CREATE INDEX IX_LifeSpecimenDeterminationsBySpecimenPreferred_ListItem
	ON dbo.vw_LifeSpecimenDeterminationsBySpecimenPreferred (Taxon_List_Item_Key)

	SET NUMERIC_ROUNDABORT OFF 
	SET ARITHABORT  OFF 
GO


/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_PostImport_Collections') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_PostImport_Collections'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_PostImport_Collections TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_PostImport_Collections TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_PostImport_Collections TO [Dev - JNCC SQL]
END
GO