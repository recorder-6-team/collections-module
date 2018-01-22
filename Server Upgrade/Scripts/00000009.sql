IF NOT Exists(SELECT * FROM SysColumns WHERE Name = 'ID_Seed' AND Id = Object_Id('Macro'))
	ALTER TABLE Macro
	ADD ID_Seed INT NOT NULL DEFAULT 1