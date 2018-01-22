IF NOT Exists(SELECT * FROM SysColumns WHERE Name = 'Title' AND Id = Object_Id('Report_Block_In_Section'))
	ALTER TABLE Report_Block_In_Section
	ADD Title VARCHAR(100) NULL
GO

UPDATE RBS
SET RBS.Title=RB.Title
FROM Report_Block_In_Section RBS
INNER JOIN Report_Block RB ON RB.Report_Block_Key=RBS.Report_Block_Key
GO

ALTER TABLE Report_Block_In_Section
	ALTER COLUMN Title VARCHAR(100) NOT NULL
GO

ALTER TABLE Report_Block
DROP COLUMN Title