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
    $Revision: 4 $
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