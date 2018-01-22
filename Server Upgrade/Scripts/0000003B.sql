/* Add Section_Header column for file name */
IF NOT Exists(SELECT * FROM SysColumns WHERE Name = 'Section_Header' AND Id = Object_Id('Report_Section')) 
	ALTER TABLE Report_Section 
		ADD	Section_Header varchar(255) NULL
GO

/* Set all sections to same header by default */
UPDATE 	Report_Section
SET		Section_Header = 'Section_Header.txt'
WHERE	Report_Section_Key <> 'SYSTEM0000000026'

/* Add Signature section, with no Section_Header */
IF NOT Exists(SELECT 1 FROM Report_Section WHERE Report_Section_Key = 'SYSTEM0000000026') BEGIN
	INSERT INTO Report_Section (Report_Section_Key, Details_Report_Key, "Sequence", Entered_Session_Id, System_Supplied_Data)
	VALUES('SYSTEM0000000026', 'SYSTEM0000000008', 7, 'SYSTEM0000000000', 1)
	
	INSERT INTO Report_Block (Report_Block_Key, Row_File, Entered_Session_Id, System_Supplied_Data)
	VALUES('SYSTEM000000003S', 'Row_SignatureDate.txt', 'SYSTEM0000000000', 1)
	
	INSERT INTO Report_Block_In_Section (Report_Block_In_Section_Key, Report_Block_Key, Report_Section_Key, Population_SQL, Population_SQL_Record_Count, "Sequence", Entered_Session_Id, System_Supplied_Data, Title)
	VALUES('SYSTEM000000002G', 'SYSTEM000000003S', 'SYSTEM0000000026', 'SELECT 1 AS Count', 'SELECT 1 As Count', 1, 'SYSTEM0000000000', 1, 'Signature')
END
