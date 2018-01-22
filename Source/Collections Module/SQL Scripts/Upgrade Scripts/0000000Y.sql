IF NOT Exists(SELECT * FROM SysColumns WHERE Name = 'Diagram_XML' AND Id = Object_Id('Store'))
	ALTER TABLE Store 
	ADD Diagram_XML TEXT COLLATE SQL_Latin1_General_CP1_CI_AS