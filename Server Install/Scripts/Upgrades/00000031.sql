-- VI 17293 - CCN 65
IF NOT EXISTS (SELECT 1 FROM SysColumns WHERE Name = 'Checked' AND ID = OBJECT_ID('Specimen_Unit'))
ALTER TABLE dbo.Specimen_Unit
	ADD Checked BIT NOT NULL CONSTRAINT DF_Specimen_Unit_Checked DEFAULT (0)
GO
