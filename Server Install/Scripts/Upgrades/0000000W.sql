
-- CCN7 - Determiner Role added to quick entry
IF NOT Exists(SELECT 1 FROM QE_Field WHERE QE_Field_Key = 'SYSTEM000000000R') 
	INSERT INTO QE_Field (	QE_Field_Key, Item_Name, Data_Type, Field_Name, Table_Name, Template_Type, Default_Size, Entered_Session_ID, System_Supplied_Data) 
	VALUES ('SYSTEM000000000R', 'Determiner Role', 18, 'Determiner_Role_Key', 'Determination', 7, 20, 'SYSTEM0000000000', 1)
