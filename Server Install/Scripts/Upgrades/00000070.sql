/*===========================================================================*\
  Description:
	Table update for CCN 168
	Adds the column "Publish_To_Web" to the the Specimen_Unit table

  Created:
	February 2014


\*===========================================================================*/

SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

IF NOT EXISTS (SELECT 1 
		FROM INFORMATION_SCHEMA.COLUMNS
		WHERE TABLE_NAME = 'Specimen_Unit'
		AND COLUMN_NAME = 'Publish_To_Web'
		AND TABLE_SCHEMA = 'dbo')
BEGIN

	ALTER TABLE dbo.Specimen_Unit
		ADD Publish_To_Web BIT NOT NULL DEFAULT(0)

END
GO