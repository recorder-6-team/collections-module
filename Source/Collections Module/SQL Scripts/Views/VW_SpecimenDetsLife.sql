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
    $Revision: 5 $
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