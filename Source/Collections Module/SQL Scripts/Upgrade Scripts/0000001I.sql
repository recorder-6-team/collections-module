IF NOT EXISTS(SELECT 1 FROM QE_Field WHERE QE_Field_Key='SYSTEM000000000S')
	INSERT INTO QE_Field (
		QE_Field_Key, 
		Item_Name, 
		Data_Type, 
		Field_Name, 
		Table_Name, 
		Template_Type, 
		Default_Size, 
		Entered_Session_ID, 
		System_Supplied_Data, 
		Custodian)
	VALUES (
		'SYSTEM000000000S', 
		'Staff Responsible', 
		19,
		'Staff_Responsible_Name_Key', 
		'Movement', 
		1,
		70, 
		'SYSTEM0000000000',
		1,
		'SYSTEM00')

UPDATE QE_Field 
SET 	Item_Name = 'Accession No with acquisition', 
	Field_Name='Acquisition_Number', 
	Table_Name='Movement',
	Data_Type=17
WHERE QE_Field_Key='SYSTEM000000000I'

UPDATE QE_Field 
SET Field_Name='Accession_Number', Table_Name='Movement' 
WHERE QE_Field_Key='SYSTEM000000000P'