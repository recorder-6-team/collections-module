IF NOT EXISTS(SELECT 1 FROM Report_Section WHERE Report_Section_Key='SYSTEM0000000025')
	INSERT INTO Report_Section (Report_Section_Key, Details_Report_Key, Sequence, 
	Entered_Session_ID, System_Supplied_Data, Custodian)
	VALUES ('SYSTEM0000000025', 'SYSTEM0000000000', 12,
	'SYSTEM0000000000', 1, 'SYSTEM00')

GO

IF NOT EXISTS(SELECT 1 FROM Report_Block WHERE Report_Block_Key='SYSTEM000000003R')
INSERT INTO Report_Block (Report_Block_Key, Header_File, Row_File, Footer_File,
Entered_Session_ID, System_Supplied_Data, Custodian)
VALUES ('SYSTEM000000003R', 'Hdr_Specimen_Metadata.txt', 'Row_Specimen_Metadata.txt',
'Table_End.txt', 'SYSTEM0000000000', 1, 'SYSTEM00')

GO

IF NOT EXISTS(SELECT 1 FROM Report_Block_Order WHERE Report_Block_Order_Key='SYSTEM0000000080')
INSERT INTO Report_Block_Order (Report_Block_Order_Key, Report_Block_Key, Item_Name,
Order_Clause_SQL, Entered_Session_ID, System_Supplied_Data, Custodian)
VALUES ('SYSTEM0000000080', 'SYSTEM000000003R', 'Type', 'ORDER BY M.Metadata_Type_Key',
'SYSTEM0000000000', 1, 'SYSTEM00')

GO

IF NOT EXISTS(SELECT 1 FROM Report_Block_In_Section WHERE Report_Block_In_Section_Key='SYSTEM000000002F')
INSERT INTO Report_Block_In_Section(Report_Block_In_Section_Key, Report_Block_Key,
Report_Section_Key, Population_SQL, Population_SQL_Record_Count, Sequence, Title, 
Entered_Session_ID, System_Supplied_Data, Custodian)
VALUES ('SYSTEM000000002F', 'SYSTEM000000003R', 'SYSTEM0000000025',
'--Metadata
SELECT MT.Item_Name AS Item, M.Text AS Value
FROM Metadata M
INNER JOIN Metadata_Type MT
ON  M.Metadata_Type_Key = MT.Metadata_Type_Key
WHERE MT.Table_Name = ''Specimen'' AND M.Record_Key = ''<#ReportKey>''',
'SELECT COUNT(*) AS Count
FROM Metadata M
INNER JOIN Metadata_Type MT
ON  M.Metadata_Type_Key = MT.Metadata_Type_Key
WHERE MT.Table_Name = ''Specimen'' AND M.Record_Key = ''<#ReportKey>''',
1, 'Metadata', 'SYSTEM0000000000', 1, 'SYSTEM00')

GO