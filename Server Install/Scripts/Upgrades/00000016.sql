/*===========================================================================*\
  Changes to QE_Template_Field to handle taxon-specific ocurrence data.
\*===========================================================================*/
-- New columns first.
IF NOT EXISTS(SELECT 1 FROM Syscolumns WHERE Name='Taxon_Measurement_Qualifier_Key' and ID=Object_ID('QE_Template_Field'))
	ALTER TABLE [dbo].[QE_Template_Field] ADD
		Taxon_Measurement_Qualifier_Key CHAR(16) NULL

IF NOT EXISTS(SELECT 1 FROM Syscolumns WHERE Name='Taxon_Measurement_Unit_Key' and ID=Object_ID('QE_Template_Field'))
	ALTER TABLE [dbo].[QE_Template_Field] ADD
		Taxon_Measurement_Unit_Key CHAR(16) NULL

IF NOT EXISTS(SELECT 1 FROM Syscolumns WHERE Name='Measurement_Is_TaxonData' and ID=Object_ID('QE_Template_Field'))
	ALTER TABLE [dbo].[QE_Template_Field] ADD
		Measurement_Is_TaxonData BIT NULL
GO

-- Relationships next.
IF NOT EXISTS(SELECT 1 FROM SysObjects WHERE Name='FK_QE_Template_Field_MEASUREMENT_QUALIFIER' AND Parent_Obj=OBJECT_ID('QE_Template_Field'))
ALTER TABLE dbo.QE_Template_Field ADD
	CONSTRAINT
		FK_QE_Template_Field_MEASUREMENT_QUALIFIER FOREIGN KEY (Taxon_Measurement_Qualifier_Key) 
		REFERENCES dbo.MEASUREMENT_QUALIFIER (MEASUREMENT_QUALIFIER_KEY),
	CONSTRAINT
		FK_QE_Template_Field_MEASUREMENT_UNIT FOREIGN KEY (Taxon_Measurement_Unit_Key) 
		REFERENCES dbo.MEASUREMENT_UNIT (MEASUREMENT_UNIT_KEY)
GO




