-- Correct a typo in the column name
IF Exists(SELECT * FROM SysColumns WHERE Name = 'System_Supplied_Dat' AND Id = Object_Id('Report_Block_Order'))
	EXEC sp_rename 'Report_Block_Order.System_Supplied_Dat', 'System_Supplied_Data', 'COLUMN'

