
ALTER TABLE Conservation_Job
	ALTER COLUMN Search_Caption nvarchar(150)

IF EXISTS(SELECT Name FROM SysIndexes WHERE Name = 'IX_Conservation_Job_Display_Caption')
	DROP INDEX dbo.Conservation_Job.IX_Conservation_Job_Display_Caption

ALTER TABLE Conservation_Job
	ALTER COLUMN Display_Caption nvarchar(150)

CREATE NONCLUSTERED INDEX IX_Conservation_Job_Display_Caption ON dbo.Conservation_Job
	(
	Display_Caption
	)

-- In case some fields were entered with more than 20 characters, need to chop before resizing column
UPDATE Valuation
SET	Ref_Number = Left(Ref_Number, 20)

ALTER TABLE Valuation
	ALTER COLUMN Ref_Number varchar(20)



