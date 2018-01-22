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
    $Revision: 5 $
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