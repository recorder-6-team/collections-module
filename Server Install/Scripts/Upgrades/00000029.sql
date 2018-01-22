IF NOT EXISTS(SELECT 1 FROM Syscolumns WHERE NAME='Metadata_Type_Key' AND ID=OBJECT_ID('QE_Template_Field'))
ALTER TABLE dbo.QE_Template_Field ADD
	Metadata_Type_Key char(16) NULL
GO
