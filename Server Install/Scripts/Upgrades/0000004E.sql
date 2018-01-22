SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF OBJECT_ID('dbo.usp_QEDataItem_Select_ForRow') IS NOT NULL
    DROP PROCEDURE dbo.usp_QEDataItem_Select_ForRow
GO

/*===========================================================================*\
  Description: 	Returns all the QE_Data_Items for a row.

  Parameters:	@Key					QE_Data_Row_Key

  Created:	Jan 2004

  Last revision information:
    $Revision: 2 $
    $Date: 12/04/11 15:59 $
    $Author: Andrewkemp $

\*===========================================================================*/
CREATE PROCEDURE dbo.usp_QEDataItem_Select_ForRow
	@Key		CHAR(16)
AS

	SELECT		QE_Data_Item_Key,
				QE_Template_Field_Key, 
				Data_Value, 
				Data_Display,
				Timestamp
	FROM		QE_Data_Item 
	WHERE		QE_Data_Row_Key			=	@Key
	ORDER BY	QE_Template_Field_Key,
				Position
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_QEDataItem_Select_ForRow') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_QEDataItem_Select_ForRow'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_QEDataItem_Select_ForRow TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_QEDataItem_Select_ForRow TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_QEDataItem_Select_ForRow TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_QEDataItem_Select_ForRow TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_QEDataItem_Select_ForRow TO [Dev - JNCC SQL]
END
GO

-- VI 23208 (revisited)
IF OBJECT_ID('dbo.usp_Specimen_SetPreferredDeterminationFromOccurrence') IS NOT NULL
	DROP PROCEDURE dbo.usp_Specimen_SetPreferredDeterminationFromOccurrence
GO	

/*===========================================================================*\
  Drop function before re-creating.
\*===========================================================================*/

IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[VW_SpecimenDetsEarth]') 
	   AND    ObjectProperty(Id, N'IsView') = 1)
    DROP VIEW [dbo].[VW_SpecimenDetsEarth]
GO

IF OBJECT_ID('dbo.vw_EarthSpecimenDeterminationsBySpecimen') IS NOT NULL
	DROP VIEW dbo.vw_EarthSpecimenDeterminationsBySpecimen
GO

IF OBJECT_ID('dbo.vw_EarthSpecimenDeterminationsBySpecimenPreferred') IS NOT NULL
	DROP VIEW dbo.vw_EarthSpecimenDeterminationsBySpecimenPreferred
GO

SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO

/*===========================================================================*\
  Description:	View that specimens with their determinations

  Created:	June 2004

  Last revision information:
    $Revision: 2 $
    $Date: 12/04/11 15:59 $
    $Author: Andrewkemp $

\*===========================================================================*/
CREATE VIEW VW_SpecimenDetsEarth
WITH SCHEMABINDING
AS
SELECT 
	SU.Collection_Unit_Key, 
	D.Concept_Key AS Concept_Key,
	SU.Preferred_Determination_Key,
	D.Determination_Key
FROM dbo.Specimen_Unit SU
INNER JOIN dbo.Determination D ON SU.Collection_Unit_Key = D.Specimen_Collection_Unit_Key
WHERE SU.Life_Sciences=0
GO

/*===========================================================================*\
  Create indexes - possible because the view is Schema bound
\*===========================================================================*/
SET ARITHABORT ON 
SET NUMERIC_ROUNDABORT OFF
GO

CREATE UNIQUE CLUSTERED INDEX VW_SpecimenDetsEarth
ON [dbo].[VW_SpecimenDetsEarth] (Collection_Unit_Key, Determination_Key)
GO

CREATE INDEX IX_DeterminationKey ON [dbo].[VW_SpecimenDetsEarth] (Concept_Key)

GO

SET NUMERIC_ROUNDABORT OFF 
SET ARITHABORT  OFF 
GO

SET QUOTED_IDENTIFIER OFF
GO
SET ANSI_NULLS OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
GRANT SELECT ON [dbo].[VW_SpecimenDetsEarth] TO [Public]
GO

/*===========================================================================*\
  Drop function before re-creating.
\*===========================================================================*/

IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[VW_SpecimenDetsLife]') 
	   AND    ObjectProperty(Id, N'IsView') = 1)
    DROP VIEW [dbo].[VW_SpecimenDetsLife]
GO

IF OBJECT_ID('dbo.vw_LifeSpecimenDeterminationsBySpecimen') IS NOT NULL
	DROP VIEW dbo.vw_LifeSpecimenDeterminationsBySpecimen
GO

IF OBJECT_ID('dbo.vw_LifeSpecimenDeterminationsBySpecimenPreferred') IS NOT NULL
	DROP VIEW dbo.vw_LifeSpecimenDeterminationsBySpecimenPreferred
GO

SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO

/*===========================================================================*\
  Description:	View that specimens with their determinations
			Note that this view is recreated after import by 
			usp_PostImport_Collections, so any edits should be reflected there

  Created:	June 2004

  Last revision information:
    $Revision: 2 $
    $Date: 12/04/11 15:59 $
    $Author: Andrewkemp $

\*===========================================================================*/
CREATE VIEW VW_SpecimenDetsLife
WITH SCHEMABINDING
AS
SELECT 
	SU.Collection_Unit_Key, 
	TD.Taxon_List_Item_Key AS Taxon_List_Item_Key,
	SU.Preferred_Taxon_Determination_Key,
	TD.Taxon_Determination_Key
FROM dbo.Specimen_Unit SU
INNER JOIN dbo.Taxon_Determination TD ON SU.Collection_Unit_Key = TD.Specimen_Collection_Unit_Key
WHERE SU.Life_Sciences=1
GO

/*===========================================================================*\
  Create indexes - possible because the view is Schema bound
\*===========================================================================*/
SET ARITHABORT ON 
SET NUMERIC_ROUNDABORT OFF
GO

CREATE UNIQUE CLUSTERED INDEX VW_SpecimenDetsLife
ON [dbo].[VW_SpecimenDetsLife] (Collection_Unit_Key, Taxon_Determination_Key)
GO

CREATE INDEX IX_DeterminationKey ON [dbo].[VW_SpecimenDetsLife] (Taxon_List_Item_Key)

GO

SET NUMERIC_ROUNDABORT OFF 
SET ARITHABORT  OFF 
GO

SET QUOTED_IDENTIFIER OFF
GO
SET ANSI_NULLS OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
GRANT SELECT ON [dbo].[VW_SpecimenDetsLife] TO [Public]
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
    $Revision: 2 $
    $Date: 12/04/11 15:59 $
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

CREATE UNIQUE CLUSTERED INDEX VW_SpecimenDetsLife
ON [dbo].[VW_SpecimenDetsLife] (Collection_Unit_Key, Taxon_Determination_Key)

CREATE INDEX IX_DeterminationKey ON [dbo].[VW_SpecimenDetsLife] (Taxon_List_Item_Key)

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
    $Revision: 2 $
    $Date: 12/04/11 15:59 $
    $Author: Andrewkemp $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_PreImport_Collections]
AS

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[FK_Specimen_Unit_Determination]') and OBJECTPROPERTY(id, N'IsForeignKey') = 1)
ALTER TABLE [dbo].[Specimen_Unit] DROP CONSTRAINT FK_Specimen_Unit_Determination

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[FK_Specimen_Unit_TAXON_DETERMINATION]') and OBJECTPROPERTY(id, N'IsForeignKey') = 1)
ALTER TABLE [dbo].[Specimen_Unit] DROP CONSTRAINT FK_Specimen_Unit_TAXON_DETERMINATION

--Remove the indexed view indices that affects Taxon_Determination table because it can prevent imports
IF EXISTS(SELECT 1 FROM SysIndexes WHERE Name='IX_DeterminationKey' AND ID=OBJECT_ID('VW_SpecimenDetsLife'))
	DROP INDEX VW_SpecimenDetsLife.IX_DeterminationKey
IF EXISTS(SELECT 1 FROM SysIndexes WHERE Name='VW_SpecimenDetsLife' AND ID=OBJECT_ID('VW_SpecimenDetsLife'))
	DROP INDEX VW_SpecimenDetsLife.VW_SpecimenDetsLife


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
