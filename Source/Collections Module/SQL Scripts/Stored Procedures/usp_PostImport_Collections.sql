SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
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
    $Revision: 6 $
    $Date: 2/09/11 15:25 $
    $Author: Jamesbichard $

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