IF EXISTS (select * from sysindexes
				where id=object_id('TAXON_DETERMINATION') and name='IX_Specimen_Collection_Unit_Key')
DROP INDEX IX_Specimen_Collection_Unit_Key ON TAXON_DETERMINATION
GO

/*=================================================================================*\
	
	Description:	Create an index on TAXON_DETERMINATION.Specimen_Collection_Unit_Key.
					This speeds up usp_DeterminationsRecorder_Select_ForSpecimen 
					from around 15 seconds to almost instantaneous

	Created:		October 2010

\*=================================================================================*/

CREATE NONCLUSTERED INDEX IX_Specimen_Collection_Unit_Key ON dbo.TAXON_DETERMINATION (Specimen_Collection_Unit_Key)
GO