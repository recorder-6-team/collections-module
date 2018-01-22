DELETE FROM [List_Report]
DELETE FROM [Report_Block_In_Section]
DELETE FROM [Report_Section]
DELETE FROM [Report_Block_Order]
DELETE FROM [Report_Block]
DELETE FROM [Details_Report]
INSERT INTO [Details_Report] ([Details_Report_Key], [Item_Name], [Reported_Table], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000000', 'Specimen Details', 'Specimen_Unit', 'SYSTEM00000000  ', NULL, 1, 'SYSTEM00');
INSERT INTO [Details_Report] ([Details_Report_Key], [Item_Name], [Reported_Table], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000001', 'Task Details', 'Conservation_Task', 'SYSTEM00000000  ', NULL, 1, 'SYSTEM00');
INSERT INTO [Details_Report] ([Details_Report_Key], [Item_Name], [Reported_Table], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000002', 'Valuation Details', 'Valuation', 'SYSTEM00000000  ', NULL, 1, 'SYSTEM00');
INSERT INTO [Details_Report] ([Details_Report_Key], [Item_Name], [Reported_Table], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000003', 'Job Details', 'Conservation_Job', 'SYSTEM00000000  ', NULL, 1, 'SYSTEM00');
INSERT INTO [Details_Report] ([Details_Report_Key], [Item_Name], [Reported_Table], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000004', 'Store Details', 'Store', 'SYSTEM00000000  ', NULL, 1, 'SYSTEM00');
INSERT INTO [Details_Report] ([Details_Report_Key], [Item_Name], [Reported_Table], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000005', 'Accession Details', 'Movement', 'SYSTEM00000000  ', NULL, 1, 'SYSTEM00');
INSERT INTO [Details_Report] ([Details_Report_Key], [Item_Name], [Reported_Table], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000006', 'Condition Check Details', 'Conservation_Check', 'SYSTEM00000000  ', NULL, 1, 'SYSTEM00');
INSERT INTO [Details_Report] ([Details_Report_Key], [Item_Name], [Reported_Table], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000007', 'Collection Details', 'Collection', 'SYSTEM00000000  ', NULL, 1, 'SYSTEM00');
INSERT INTO [Details_Report] ([Details_Report_Key], [Item_Name], [Reported_Table], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000008', 'Exchanges Form', 'Movement', 'SYSTEM00000000  ', NULL, 1, 'SYSTEM00');
INSERT INTO [Details_Report] ([Details_Report_Key], [Item_Name], [Reported_Table], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000009', 'Movement Details', 'Movement', 'SYSTEM00000000  ', NULL, 1, 'SYSTEM00');
INSERT INTO [Details_Report] ([Details_Report_Key], [Item_Name], [Reported_Table], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000000A', 'Loan Form', 'Movement', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block] ([Report_Block_Key], [Header_File], [Row_File], [Footer_File], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000000', 'Hdr_Accessions_List.txt', 'Row_Accessions_List.txt', 'Table_End.txt', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block] ([Report_Block_Key], [Header_File], [Row_File], [Footer_File], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000001', 'Hdr_Collections_List.txt', 'Row_Collections_List.txt', 'Table_End.txt', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block] ([Report_Block_Key], [Header_File], [Row_File], [Footer_File], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000002', 'Hdr_Jobs_List.txt', 'Row_Jobs_List.txt', 'Table_End.txt', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block] ([Report_Block_Key], [Header_File], [Row_File], [Footer_File], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000003', 'Hdr_ConditionChecks_List.txt', 'Row_ConditionChecks_List.txt', 'Table_End.txt', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block] ([Report_Block_Key], [Header_File], [Row_File], [Footer_File], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000004', 'Hdr_Exchanges_List.txt', 'Row_Exchanges_List.txt', 'Table_End.txt', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block] ([Report_Block_Key], [Header_File], [Row_File], [Footer_File], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000005', 'Hdr_FundingSources_List.txt', 'Row_FundingSources_List.txt', 'Table_End.txt', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block] ([Report_Block_Key], [Header_File], [Row_File], [Footer_File], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000007', 'Hdr_Loans_List.txt', 'Row_Loans_List.txt', 'Table_End.txt', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block] ([Report_Block_Key], [Header_File], [Row_File], [Footer_File], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000008', 'Hdr_Movements_List.txt', 'Row_Movements_List.txt', 'Table_End.txt', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block] ([Report_Block_Key], [Header_File], [Row_File], [Footer_File], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000009', 'Hdr_Specimens_List.txt', 'Row_Specimens_List.txt', 'Table_End.txt', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block] ([Report_Block_Key], [Header_File], [Row_File], [Footer_File], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000010', 'Hdr_Stores_List.txt', 'Row_Stores_List.txt', 'Table_End.txt', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block] ([Report_Block_Key], [Header_File], [Row_File], [Footer_File], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000011', 'Hdr_Valuations_List.txt', 'Row_Valuations_List.txt', 'Table_End.txt', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block] ([Report_Block_Key], [Header_File], [Row_File], [Footer_File], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000012', NULL, 'Row_Specimen_Details_General.txt', NULL, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block] ([Report_Block_Key], [Header_File], [Row_File], [Footer_File], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000013', 'Hdr_Collection_Unit_Measurements_List.txt', 'Row_Collection_Unit_Measurements_List.txt', 'Table_End.txt', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block] ([Report_Block_Key], [Header_File], [Row_File], [Footer_File], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000014', 'Hdr_Collection_Unit_Descriptors_List.txt', 'Row_Collection_Unit_Descriptors_List.txt', 'Table_End.txt', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block] ([Report_Block_Key], [Header_File], [Row_File], [Footer_File], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000015', 'Hdr_Specimen_Linked_Specimens_List.txt', 'Row_Specimen_Linked_Specimens_List.txt', 'Table_End.txt', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block] ([Report_Block_Key], [Header_File], [Row_File], [Footer_File], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000016', 'Hdr_Specimen_Determinations_List.txt', 'Row_Specimen_Determinations_List.txt', 'Table_End.txt', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block] ([Report_Block_Key], [Header_File], [Row_File], [Footer_File], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000017', 'Hdr_Specimen_GatheringData_List.txt', 'Row_Specimen_GatheringData_List.txt', 'Table_End.txt', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block] ([Report_Block_Key], [Header_File], [Row_File], [Footer_File], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000018', 'Hdr_Specimen_Storage_Place_List.txt', 'Row_Specimen_Storage_Place_List.txt', 'Table_End.txt', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block] ([Report_Block_Key], [Header_File], [Row_File], [Footer_File], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000019', 'Hdr_Specimen_InscriptionsAndLabels_List.txt', 'Row_Specimen_InscriptionsAndLabels_List.txt', 'Table_End.txt', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block] ([Report_Block_Key], [Header_File], [Row_File], [Footer_File], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000020', 'Hdr_Sources_List.txt', 'Row_Sources_List.txt', 'Table_End.txt', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block] ([Report_Block_Key], [Header_File], [Row_File], [Footer_File], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000021', 'Hdr_External_Refs_List.txt', 'Row_External_Refs_List.txt', 'Table_End.txt', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block] ([Report_Block_Key], [Header_File], [Row_File], [Footer_File], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000022', NULL, 'Row_Task_Details_General.txt', NULL, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block] ([Report_Block_Key], [Header_File], [Row_File], [Footer_File], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000023', NULL, 'Row_Multimedia.txt', NULL, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block] ([Report_Block_Key], [Header_File], [Row_File], [Footer_File], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000024', NULL, 'Row_Valuation_Details_General.txt', NULL, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block] ([Report_Block_Key], [Header_File], [Row_File], [Footer_File], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000027', NULL, 'Row_Job_Details_General.txt', NULL, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block] ([Report_Block_Key], [Header_File], [Row_File], [Footer_File], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000028', 'Hdr_Tasks_List.txt', 'Row_Tasks_List.txt', 'Table_End.txt', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block] ([Report_Block_Key], [Header_File], [Row_File], [Footer_File], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000029', 'Hdr_Job_Materials_List.txt', 'Row_Job_Materials_List.txt', 'Table_End.txt', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block] ([Report_Block_Key], [Header_File], [Row_File], [Footer_File], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000032', NULL, 'Row_Store_Details_General.txt', NULL, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block] ([Report_Block_Key], [Header_File], [Row_File], [Footer_File], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000033', NULL, 'Row_Accession_Details_General.txt', NULL, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block] ([Report_Block_Key], [Header_File], [Row_File], [Footer_File], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000034', 'Hdr_Acquisitions_List.txt', 'Row_Acquisitions_List.txt', 'Table_End.txt', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block] ([Report_Block_Key], [Header_File], [Row_File], [Footer_File], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000035', NULL, 'Row_ConditionCheck_Details_General.txt', NULL, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block] ([Report_Block_Key], [Header_File], [Row_File], [Footer_File], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000036', 'Hdr_CC_Stores_List.txt', 'Row_CC_Stores_List.txt', 'Table_End.txt', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block] ([Report_Block_Key], [Header_File], [Row_File], [Footer_File], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000037', 'Hdr_CC_Collections_List.txt', 'Row_CC_Collections_List.txt', 'Table_End.txt', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block] ([Report_Block_Key], [Header_File], [Row_File], [Footer_File], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000038', NULL, 'Row_Movement_Details_General.txt', NULL, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block] ([Report_Block_Key], [Header_File], [Row_File], [Footer_File], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000039', 'Hdr_Authorisation.txt', 'Row_Authorisation.txt', 'Table_End.txt', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block] ([Report_Block_Key], [Header_File], [Row_File], [Footer_File], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000003A', NULL, 'Row_Exchanges_Form_Details.txt', NULL, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block] ([Report_Block_Key], [Header_File], [Row_File], [Footer_File], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000003B', 'Hdr_Exchanges_Specimens_List.txt', 'Row_Exchanges_Specimens_List.txt', 'Table_End.txt', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block] ([Report_Block_Key], [Header_File], [Row_File], [Footer_File], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000003C', 'Hdr_Exchanges_Collections_List.txt', 'Row_Exchanges_Collections_List.txt', 'Table_End.txt', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block] ([Report_Block_Key], [Header_File], [Row_File], [Footer_File], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000003D', 'Hdr_Exchanges_Stores_List.txt', 'Row_Exchanges_Stores_List.txt', 'Table_End.txt', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block] ([Report_Block_Key], [Header_File], [Row_File], [Footer_File], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000003E', 'Hdr_Exchanges_Valuations_List.txt', 'Row_Exchanges_Valuations_List.txt', 'Table_End.txt', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block] ([Report_Block_Key], [Header_File], [Row_File], [Footer_File], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000003F', NULL, 'Row_Collections_Details_General.txt', NULL, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block] ([Report_Block_Key], [Header_File], [Row_File], [Footer_File], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000003G', 'Hdr_Collections_List.txt', 'Row_Collections_List.txt', 'Table_End.txt', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block] ([Report_Block_Key], [Header_File], [Row_File], [Footer_File], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000003H', 'Hdr_Collections_List.txt', 'Row_Collections_List.txt', 'Table_End.txt', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block] ([Report_Block_Key], [Header_File], [Row_File], [Footer_File], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000003I', 'Hdr_Collections_List.txt', 'Row_Collections_List.txt', 'Table_End.txt', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block] ([Report_Block_Key], [Header_File], [Row_File], [Footer_File], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000003J', NULL, 'Row_Store_Details_General.txt', NULL, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block] ([Report_Block_Key], [Header_File], [Row_File], [Footer_File], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000003K', 'Hdr_Store_Locality_List.txt', 'Row_Store_Locality_List.txt', 'Table_End.txt', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block] ([Report_Block_Key], [Header_File], [Row_File], [Footer_File], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000003L', NULL, 'Row_Loan_Form_Details.txt', NULL, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block] ([Report_Block_Key], [Header_File], [Row_File], [Footer_File], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000003M', 'Hdr_Loan_Form_Specimens_List.txt', 'Row_Loan_Form_Specimens_List.txt', 'Table_End.txt', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block] ([Report_Block_Key], [Header_File], [Row_File], [Footer_File], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000003N', 'Hdr_Loan_Form_Collections_List.txt', 'Row_Loan_Form_Collections_List.txt', 'Table_End.txt', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block] ([Report_Block_Key], [Header_File], [Row_File], [Footer_File], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000003O', 'Hdr_Loan_Form_Stores_List.txt', 'Row_Loan_Form_Stores_List.txt', 'Table_End.txt', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block] ([Report_Block_Key], [Header_File], [Row_File], [Footer_File], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000003P', NULL, 'Row_Loan_Form_Agreement.txt', NULL, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block] ([Report_Block_Key], [Header_File], [Row_File], [Footer_File], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000003Q', 'Hdr_Loan_Collections_List.txt', 'Row_Loan_Collections_List.txt', 'Table_End.txt', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000000', 'SYSTEM0000000001', 'Accession No', 'ORDER BY AccNo', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000001', 'SYSTEM0000000001', 'Name', 'ORDER BY [Name]', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000002', 'SYSTEM0000000001', 'Topic', 'ORDER BY Topic', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000003', 'SYSTEM0000000001', 'Assembled By', 'ORDER BY AssembledBy', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000004', 'SYSTEM0000000001', 'Store', 'ORDER BY Store', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000005', 'SYSTEM0000000013', 'Applies To', 'ORDER BY Applies_To', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000006', 'SYSTEM0000000013', 'Parameter', 'ORDER BY Parameter_Term', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000007', 'SYSTEM0000000013', 'Value', 'ORDER BY Value', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000008', 'SYSTEM0000000013', 'Method', 'ORDER BY Method_Term', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000009', 'SYSTEM0000000014', 'Applies To', 'ORDER BY Applies_To', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000010', 'SYSTEM0000000014', 'Parameter', 'ORDER BY Parameter_Term', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000011', 'SYSTEM0000000014', 'Value', 'ORDER BY Value', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000012', 'SYSTEM0000000014', 'Method', 'ORDER BY Method_Term', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000013', 'SYSTEM0000000015', 'Reg No', 'ORDER BY RegNo', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000014', 'SYSTEM0000000015', 'Determination', 'ORDER BY Determination', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000015', 'SYSTEM0000000015', 'Type', 'ORDER BY Type', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000016', 'SYSTEM0000000015', 'Field Collector', 'ORDER BY FieldCollector', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000017', 'SYSTEM0000000015', 'Gathering Site', 'ORDER BY GatheringSite', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000018', 'SYSTEM0000000015', 'Gathering Date', 'ORDER BY GatheringDate', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000019', 'SYSTEM0000000015', 'Store', 'ORDER BY StoreCode', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000020', 'SYSTEM0000000016', 'Determination', 'ORDER BY Determination', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000021', 'SYSTEM0000000016', 'Determiner', 'ORDER BY Determiner', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000022', 'SYSTEM0000000016', 'Date', 'ORDER BY DetDate', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000023', 'SYSTEM0000000016', 'Type', 'ORDER BY Type', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000024', 'SYSTEM0000000016', 'Confidence', 'ORDER BY Confidence', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000025', 'SYSTEM0000000017', 'Survey Name', 'ORDER BY SurveyName', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000026', 'SYSTEM0000000017', 'Location', 'ORDER BY LocationName', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000027', 'SYSTEM0000000017', 'Collectors', 'ORDER BY FieldCollectors', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000028', 'SYSTEM0000000017', 'Date', 'ORDER BY SampleDate', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000029', 'SYSTEM0000000019', 'Type', 'ORDER BY Type', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000030', 'SYSTEM0000000019', 'Inscription', 'ORDER BY Item_Name', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000031', 'SYSTEM0000000019', 'Author', 'ORDER BY Author', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000032', 'SYSTEM0000000020', 'Document', 'ORDER BY CAST(R.Title AS VARCHAR(100))', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000033', 'SYSTEM0000000020', 'Original', 'ORDER BY Original', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000034', 'SYSTEM0000000020', 'Type', 'ORDER BY Type', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000035', 'SYSTEM0000000021', 'Item_Name', 'ORDER BY Item_Name', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000036', 'SYSTEM0000000009', 'Reg No.', 'ORDER BY RegNo', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000037', 'SYSTEM0000000009', 'Determination', 'ORDER BY Determination', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000038', 'SYSTEM0000000009', 'Type', 'ORDER BY Type', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000039', 'SYSTEM0000000009', 'Field Collector', 'ORDER BY FieldCollector', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000040', 'SYSTEM0000000009', 'Gathering Site', 'ORDER BY GatheringSite', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000041', 'SYSTEM0000000009', 'Gathering Date', 'ORDER BY S.Vague_Date_Start, S.Vague_Date_End', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000043', 'SYSTEM0000000009', 'Store', 'ORDER BY Store', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000050', 'SYSTEM0000000028', 'Date Set', 'ORDER BY Set_Vague_Date_Start', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000051', 'SYSTEM0000000028', 'Priority', 'ORDER BY Priority', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000052', 'SYSTEM0000000028', 'Status', 'ORDER BY Status', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000053', 'SYSTEM0000000028', 'Type', 'ORDER BY Type', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000054', 'SYSTEM0000000028', 'Identified By', 'ORDER BY IdentifiedBy', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000055', 'SYSTEM0000000029', 'Material', 'ORDER BY Material', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000058', 'SYSTEM0000000034', 'Date', 'ORDER BY AcqDate', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000059', 'SYSTEM0000000034', 'Acquired From', 'ORDER BY AcquiredFrom', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000005A', 'SYSTEM0000000034', 'Completed', 'ORDER BY Completed DESC', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000005B', 'SYSTEM0000000034', 'Method', 'ORDER BY Method DESC', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000005C', 'SYSTEM0000000034', 'Amount Paid', 'ORDER BY AmountPaid DESC', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000005D', 'SYSTEM0000000000', 'Accession No.', 'ORDER BY AccNo', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000005E', 'SYSTEM0000000000', 'Date', 'ORDER BY Exp_Vague_Date_Start', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000005F', 'SYSTEM0000000000', 'Accessed From', 'ORDER BY AccessedFrom', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000005G', 'SYSTEM0000000000', 'Staff Responsible', 'ORDER BY StaffResp', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000005H', 'SYSTEM0000000000', 'Department', 'ORDER BY Department', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000005I', 'SYSTEM0000000002', 'Job No.', 'ORDER BY JobNo', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000005J', 'SYSTEM0000000002', 'Job Name', 'ORDER BY [Name]', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000005K', 'SYSTEM0000000002', 'Start Date', 'ORDER BY CJ.From_Vague_Date_Start, From_Vague_Date_End', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000005L', 'SYSTEM0000000002', 'Duration', 'ORDER BY CTUnit.Item_Name, CJ.Duration', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000005M', 'SYSTEM0000000002', 'Material Cost', 'ORDER BY MaterialCost', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000005N', 'SYSTEM0000000002', 'Status', 'ORDER BY Status', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000005O', 'SYSTEM0000000003', 'Reference No.', 'ORDER BY RefNo', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000005P', 'SYSTEM0000000003', 'Type', 'ORDER BY Type', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000005Q', 'SYSTEM0000000003', 'Condition', 'ORDER BY Condition', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000005R', 'SYSTEM0000000003', 'Checked By', 'ORDER BY CheckedBy', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000005S', 'SYSTEM0000000003', 'Date', 'ORDER BY CC.Vague_Date_Start, CC.Vague_Date_End', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000005T', 'SYSTEM0000000004', 'Reference No.', 'ORDER BY RefNo', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000005U', 'SYSTEM0000000004', 'Date', 'ORDER BY CC.Vague_Date_Start, CC.Vague_Date_End', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000005V', 'SYSTEM0000000004', 'Exchanged With', 'ORDER BY ExchangedWith', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000005W', 'SYSTEM0000000004', 'Staff Responsible', 'ORDER BY StaffResp', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000005X', 'SYSTEM0000000004', 'Department', 'ORDER BY Department', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000005Y', 'SYSTEM0000000005', 'Date', 'ORDER BY F.Vague_Date_Start, F.Vague_Date_End', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000005Z', 'SYSTEM0000000005', 'Funded By', 'ORDER BY FundedBy', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000060', 'SYSTEM0000000005', 'Amount', 'ORDER BY Amount', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000061', 'SYSTEM0000000007', 'Reference No.', 'ORDER BY RefNo', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000062', 'SYSTEM0000000007', 'Date', 'ORDER BY M.Exp_Vague_Date_Start, M.Exp_Vague_Date_End', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000063', 'SYSTEM0000000007', 'Loaned To', 'ORDER BY LoanedTo', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000064', 'SYSTEM0000000007', 'Staff Responsible', 'ORDER BY StaffResp', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000067', 'SYSTEM0000000007', 'Department', 'ORDER BY Department', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000068', 'SYSTEM0000000007', 'Status', 'ORDER BY Status', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000069', 'SYSTEM0000000008', 'Reference No.', 'ORDER BY RefNo', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000006A', 'SYSTEM0000000008', 'Date', 'ORDER BY M.Exp_Vague_Date_Start, M.Exp_Vague_Date_End', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000006B', 'SYSTEM0000000008', 'Type', 'ORDER BY Type', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000006C', 'SYSTEM0000000008', 'Moved To', 'ORDER BY MovedTo', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000006D', 'SYSTEM0000000008', 'Staff Responsible', 'ORDER BY StaffResp', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000006E', 'SYSTEM0000000008', 'Department', 'ORDER BY Department', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000006F', 'SYSTEM0000000010', 'Reference No.', 'ORDER BY RefNo', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000006G', 'SYSTEM0000000010', 'Name', 'ORDER BY [Name]', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000006H', 'SYSTEM0000000010', 'Type', 'ORDER BY Type', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000006I', 'SYSTEM0000000010', 'Store', 'ORDER BY Store', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000006J', 'SYSTEM0000000011', 'Reference No.', 'ORDER BY RefNo', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000006K', 'SYSTEM0000000011', 'Date', 'ORDER BY V.Vague_Date_Start, V.Vague_Date_End', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000006L', 'SYSTEM0000000011', 'Type', 'ORDER BY Type', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000006M', 'SYSTEM0000000011', 'Valued By', 'ORDER BY Type', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000006N', 'SYSTEM0000000011', 'Value', 'ORDER BY V.Value_Amount', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000006O', 'SYSTEM0000000011', 'Valid', 'ORDER BY Valid', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000006P', 'SYSTEM000000003C', 'Accession No', 'ORDER BY AccNo', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000006Q', 'SYSTEM000000003C', 'Name', 'ORDER BY [Name]', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000006R', 'SYSTEM000000003C', 'Topic', 'ORDER BY Topic', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000006S', 'SYSTEM000000003C', 'Assembled By', 'ORDER BY AssembledBy', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000006T', 'SYSTEM000000003C', 'Store', 'ORDER BY Store', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000006U', 'SYSTEM000000003B', 'Reg No.', 'ORDER BY RegNo', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000006V', 'SYSTEM000000003B', 'Determination', 'ORDER BY Determination', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000006W', 'SYSTEM000000003B', 'Type', 'ORDER BY Type', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000006X', 'SYSTEM000000003B', 'Field Collector', 'ORDER BY FieldCollector', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000006Y', 'SYSTEM000000003B', 'Gathering Site', 'ORDER BY GatheringSite', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000006Z', 'SYSTEM000000003B', 'Gathering Date', 'ORDER BY S.Vague_Date_Start, S.Vague_Date_End', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000070', 'SYSTEM000000003B', 'Store', 'ORDER BY Store', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000071', 'SYSTEM000000003D', 'Reference No.', 'ORDER BY RefNo', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000072', 'SYSTEM000000003D', 'Name', 'ORDER BY [Name]', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000073', 'SYSTEM000000003D', 'Type', 'ORDER BY Type', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000074', 'SYSTEM000000003D', 'Store', 'ORDER BY Store', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000075', 'SYSTEM000000003G', 'Accession No', 'ORDER BY AccNo', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000076', 'SYSTEM000000003G', 'Name', 'ORDER BY [Name]', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000077', 'SYSTEM000000003G', 'Topic', 'ORDER BY Topic', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000078', 'SYSTEM000000003G', 'Assembled By', 'ORDER BY AssembledBy', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000079', 'SYSTEM000000003G', 'Store', 'ORDER BY Store', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000007A', 'SYSTEM000000003H', 'Accession No', 'ORDER BY AccNo', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000007B', 'SYSTEM000000003H', 'Name', 'ORDER BY [Name]', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000007C', 'SYSTEM000000003H', 'Topic', 'ORDER BY C.Topic', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000007D', 'SYSTEM000000003H', 'Assembled By', 'ORDER BY AssembledBy', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000007E', 'SYSTEM000000003H', 'Store', 'ORDER BY Store', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000007F', 'SYSTEM000000003I', 'Accession No', 'ORDER BY AccNo', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000007G', 'SYSTEM000000003I', 'Name', 'ORDER BY [Name]', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000007H', 'SYSTEM000000003I', 'Topic', 'ORDER BY Topic', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000007I', 'SYSTEM000000003I', 'Assembled By', 'ORDER BY AssembledBy', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000007J', 'SYSTEM000000003I', 'Store', 'ORDER BY Store', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000007K', 'SYSTEM000000003K', 'Reference No.', 'ORDER BY RefNo', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000007L', 'SYSTEM000000003K', 'Type', 'ORDER BY Type', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000007M', 'SYSTEM000000003K', 'Loc. Code', 'ORDER BY Store', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000007N', 'SYSTEM000000003N', 'Accession No', 'ORDER BY AccNo', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000007O', 'SYSTEM000000003N', 'Name', 'ORDER BY [Name]', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000007P', 'SYSTEM000000003N', 'Topic', 'ORDER BY Topic', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000007Q', 'SYSTEM000000003N', 'Assembled By', 'ORDER BY AssembledBy', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000007R', 'SYSTEM000000003M', 'Reg No.', 'ORDER BY RegNo', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000007S', 'SYSTEM000000003M', 'Determination', 'ORDER BY Determination', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000007T', 'SYSTEM000000003M', 'Type', 'ORDER BY Type', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000007U', 'SYSTEM000000003M', 'Field Collector', 'ORDER BY FieldCollector', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000007V', 'SYSTEM000000003M', 'Gathering Site', 'ORDER BY GatheringSite', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000007W', 'SYSTEM000000003M', 'Gathering Date', 'ORDER BY S.Vague_Date_Start, S.Vague_Date_End', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000007X', 'SYSTEM000000003O', 'Reference No.', 'ORDER BY RefNo', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000007Y', 'SYSTEM000000003O', 'Name', 'ORDER BY [Name]', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_Order] ([Report_Block_Order_Key], [Report_Block_Key], [Item_Name], [Order_Clause_SQL], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000007Z', 'SYSTEM000000003O', 'Type', 'ORDER BY Type', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Section] ([Report_Section_Key], [Section_List_SQL], [Item_Name_Macro], [Details_Report_Key], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000000', NULL, NULL, 'SYSTEM0000000000', 2, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Section] ([Report_Section_Key], [Section_List_SQL], [Item_Name_Macro], [Details_Report_Key], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000001', NULL, NULL, 'SYSTEM0000000000', 3, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Section] ([Report_Section_Key], [Section_List_SQL], [Item_Name_Macro], [Details_Report_Key], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000002', NULL, NULL, 'SYSTEM0000000000', 4, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Section] ([Report_Section_Key], [Section_List_SQL], [Item_Name_Macro], [Details_Report_Key], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000003', NULL, NULL, 'SYSTEM0000000000', 5, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Section] ([Report_Section_Key], [Section_List_SQL], [Item_Name_Macro], [Details_Report_Key], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000004', NULL, NULL, 'SYSTEM0000000000', 6, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Section] ([Report_Section_Key], [Section_List_SQL], [Item_Name_Macro], [Details_Report_Key], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000005', NULL, NULL, 'SYSTEM0000000000', 7, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Section] ([Report_Section_Key], [Section_List_SQL], [Item_Name_Macro], [Details_Report_Key], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000006', NULL, NULL, 'SYSTEM0000000000', 8, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Section] ([Report_Section_Key], [Section_List_SQL], [Item_Name_Macro], [Details_Report_Key], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000007', NULL, NULL, 'SYSTEM0000000000', 9, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Section] ([Report_Section_Key], [Section_List_SQL], [Item_Name_Macro], [Details_Report_Key], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000008', NULL, NULL, 'SYSTEM0000000000', 10, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Section] ([Report_Section_Key], [Section_List_SQL], [Item_Name_Macro], [Details_Report_Key], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000009', NULL, NULL, 'SYSTEM0000000000', 11, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Section] ([Report_Section_Key], [Section_List_SQL], [Item_Name_Macro], [Details_Report_Key], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000000A', NULL, NULL, 'SYSTEM0000000001', 1, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Section] ([Report_Section_Key], [Section_List_SQL], [Item_Name_Macro], [Details_Report_Key], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000000B', NULL, NULL, 'SYSTEM0000000001', 2, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Section] ([Report_Section_Key], [Section_List_SQL], [Item_Name_Macro], [Details_Report_Key], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000000C', NULL, NULL, 'SYSTEM0000000001', 3, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Section] ([Report_Section_Key], [Section_List_SQL], [Item_Name_Macro], [Details_Report_Key], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000000D', NULL, NULL, 'SYSTEM0000000001', 4, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Section] ([Report_Section_Key], [Section_List_SQL], [Item_Name_Macro], [Details_Report_Key], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000000E', NULL, NULL, 'SYSTEM0000000002', 1, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Section] ([Report_Section_Key], [Section_List_SQL], [Item_Name_Macro], [Details_Report_Key], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000000F', NULL, NULL, 'SYSTEM0000000002', 2, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Section] ([Report_Section_Key], [Section_List_SQL], [Item_Name_Macro], [Details_Report_Key], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000000G', NULL, NULL, 'SYSTEM0000000002', 3, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Section] ([Report_Section_Key], [Section_List_SQL], [Item_Name_Macro], [Details_Report_Key], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000000H', NULL, NULL, 'SYSTEM0000000002', 4, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Section] ([Report_Section_Key], [Section_List_SQL], [Item_Name_Macro], [Details_Report_Key], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000000I', NULL, NULL, 'SYSTEM0000000002', 5, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Section] ([Report_Section_Key], [Section_List_SQL], [Item_Name_Macro], [Details_Report_Key], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000000J', NULL, NULL, 'SYSTEM0000000003', 1, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Section] ([Report_Section_Key], [Section_List_SQL], [Item_Name_Macro], [Details_Report_Key], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000000K', NULL, NULL, 'SYSTEM0000000003', 2, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Section] ([Report_Section_Key], [Section_List_SQL], [Item_Name_Macro], [Details_Report_Key], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000000L', NULL, NULL, 'SYSTEM0000000003', 3, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Section] ([Report_Section_Key], [Section_List_SQL], [Item_Name_Macro], [Details_Report_Key], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000000M', NULL, NULL, 'SYSTEM0000000003', 4, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Section] ([Report_Section_Key], [Section_List_SQL], [Item_Name_Macro], [Details_Report_Key], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000000N', 'SELECT DISTINCT 
  ISNULL(S.Collection_Unit_Key, '''') AS Collection_Unit_Key, 
	ISNULL(CUS.Current_Location_Code, ISNULL(S.Item_Name, ''No current store'')) AS Current_Location_Code
FROM Conservation_Task CT 
INNER JOIN Collection_Unit_Task CUT
    ON CUT.Conservation_Task_Key=CT.Conservation_Task_Key
INNER JOIN Collection_Unit CU 
    ON CU.Collection_Unit_Key=CUT.Collection_Unit_Key
LEFT JOIN Collection_Unit CUS ON CUS.Collection_Unit_Key=CU.Current_Container_Collection_Unit_Key
LEFT JOIN Store S ON S.Collection_Unit_Key=CUS.Collection_Unit_Key
WHERE CT.Conservation_Job_Key=''<#ReportKey>''', 'Storage Location <#SectionNumber> - <#Current_Location_Code>', 'SYSTEM0000000003', 5, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Section] ([Report_Section_Key], [Section_List_SQL], [Item_Name_Macro], [Details_Report_Key], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000000O', NULL, NULL, 'SYSTEM0000000003', 6, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Section] ([Report_Section_Key], [Section_List_SQL], [Item_Name_Macro], [Details_Report_Key], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000000P', NULL, NULL, 'SYSTEM0000000003', 7, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Section] ([Report_Section_Key], [Section_List_SQL], [Item_Name_Macro], [Details_Report_Key], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000000Q', NULL, NULL, 'SYSTEM0000000004', 1, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Section] ([Report_Section_Key], [Section_List_SQL], [Item_Name_Macro], [Details_Report_Key], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000000R', NULL, NULL, 'SYSTEM0000000004', 2, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Section] ([Report_Section_Key], [Section_List_SQL], [Item_Name_Macro], [Details_Report_Key], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000000S', NULL, NULL, 'SYSTEM0000000004', 3, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Section] ([Report_Section_Key], [Section_List_SQL], [Item_Name_Macro], [Details_Report_Key], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000000T', NULL, NULL, 'SYSTEM0000000005', 1, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Section] ([Report_Section_Key], [Section_List_SQL], [Item_Name_Macro], [Details_Report_Key], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000000U', NULL, NULL, 'SYSTEM0000000005', 2, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Section] ([Report_Section_Key], [Section_List_SQL], [Item_Name_Macro], [Details_Report_Key], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000000V', NULL, NULL, 'SYSTEM0000000005', 3, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Section] ([Report_Section_Key], [Section_List_SQL], [Item_Name_Macro], [Details_Report_Key], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000000W', NULL, NULL, 'SYSTEM0000000005', 4, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Section] ([Report_Section_Key], [Section_List_SQL], [Item_Name_Macro], [Details_Report_Key], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000000X', NULL, NULL, 'SYSTEM0000000005', 5, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Section] ([Report_Section_Key], [Section_List_SQL], [Item_Name_Macro], [Details_Report_Key], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000000Y', NULL, NULL, 'SYSTEM0000000005', 6, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Section] ([Report_Section_Key], [Section_List_SQL], [Item_Name_Macro], [Details_Report_Key], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000000Z', NULL, NULL, 'SYSTEM0000000005', 7, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Section] ([Report_Section_Key], [Section_List_SQL], [Item_Name_Macro], [Details_Report_Key], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000010', NULL, NULL, 'SYSTEM0000000005', 8, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Section] ([Report_Section_Key], [Section_List_SQL], [Item_Name_Macro], [Details_Report_Key], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000011', NULL, NULL, 'SYSTEM0000000005', 9, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Section] ([Report_Section_Key], [Section_List_SQL], [Item_Name_Macro], [Details_Report_Key], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000012', NULL, NULL, 'SYSTEM0000000006', 1, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Section] ([Report_Section_Key], [Section_List_SQL], [Item_Name_Macro], [Details_Report_Key], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000013', NULL, NULL, 'SYSTEM0000000006', 2, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Section] ([Report_Section_Key], [Section_List_SQL], [Item_Name_Macro], [Details_Report_Key], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000014', NULL, NULL, 'SYSTEM0000000006', 3, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Section] ([Report_Section_Key], [Section_List_SQL], [Item_Name_Macro], [Details_Report_Key], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000015', NULL, NULL, 'SYSTEM0000000006', 4, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Section] ([Report_Section_Key], [Section_List_SQL], [Item_Name_Macro], [Details_Report_Key], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000016', NULL, NULL, 'SYSTEM0000000006', 5, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Section] ([Report_Section_Key], [Section_List_SQL], [Item_Name_Macro], [Details_Report_Key], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000017', NULL, NULL, 'SYSTEM0000000006', 6, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Section] ([Report_Section_Key], [Section_List_SQL], [Item_Name_Macro], [Details_Report_Key], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000018', NULL, NULL, 'SYSTEM0000000006', 7, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Section] ([Report_Section_Key], [Section_List_SQL], [Item_Name_Macro], [Details_Report_Key], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000019', NULL, NULL, 'SYSTEM0000000009', 1, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Section] ([Report_Section_Key], [Section_List_SQL], [Item_Name_Macro], [Details_Report_Key], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000001A', NULL, NULL, 'SYSTEM0000000009', 2, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Section] ([Report_Section_Key], [Section_List_SQL], [Item_Name_Macro], [Details_Report_Key], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000001B', NULL, NULL, 'SYSTEM0000000009', 3, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Section] ([Report_Section_Key], [Section_List_SQL], [Item_Name_Macro], [Details_Report_Key], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000001C', NULL, NULL, 'SYSTEM0000000009', 4, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Section] ([Report_Section_Key], [Section_List_SQL], [Item_Name_Macro], [Details_Report_Key], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000001D', NULL, NULL, 'SYSTEM0000000009', 5, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Section] ([Report_Section_Key], [Section_List_SQL], [Item_Name_Macro], [Details_Report_Key], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000001E', NULL, NULL, 'SYSTEM0000000009', 6, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Section] ([Report_Section_Key], [Section_List_SQL], [Item_Name_Macro], [Details_Report_Key], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000001F', NULL, NULL, 'SYSTEM0000000009', 7, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Section] ([Report_Section_Key], [Section_List_SQL], [Item_Name_Macro], [Details_Report_Key], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000001G', NULL, NULL, 'SYSTEM0000000008', 1, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Section] ([Report_Section_Key], [Section_List_SQL], [Item_Name_Macro], [Details_Report_Key], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000001H', NULL, 'Material & Ownership Transferred', 'SYSTEM0000000008', 2, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Section] ([Report_Section_Key], [Section_List_SQL], [Item_Name_Macro], [Details_Report_Key], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000001I', NULL, 'Material & Ownership Received', 'SYSTEM0000000008', 3, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Section] ([Report_Section_Key], [Section_List_SQL], [Item_Name_Macro], [Details_Report_Key], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000001J', NULL, NULL, 'SYSTEM0000000008', 4, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Section] ([Report_Section_Key], [Section_List_SQL], [Item_Name_Macro], [Details_Report_Key], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000001K', NULL, NULL, 'SYSTEM0000000008', 5, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Section] ([Report_Section_Key], [Section_List_SQL], [Item_Name_Macro], [Details_Report_Key], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000001L', NULL, NULL, 'SYSTEM0000000008', 6, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Section] ([Report_Section_Key], [Section_List_SQL], [Item_Name_Macro], [Details_Report_Key], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000001M', NULL, NULL, 'SYSTEM0000000007', 1, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Section] ([Report_Section_Key], [Section_List_SQL], [Item_Name_Macro], [Details_Report_Key], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000001N', NULL, 'Collections', 'SYSTEM0000000007', 2, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Section] ([Report_Section_Key], [Section_List_SQL], [Item_Name_Macro], [Details_Report_Key], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000001O', NULL, NULL, 'SYSTEM0000000007', 3, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Section] ([Report_Section_Key], [Section_List_SQL], [Item_Name_Macro], [Details_Report_Key], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000001P', NULL, NULL, 'SYSTEM0000000007', 4, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Section] ([Report_Section_Key], [Section_List_SQL], [Item_Name_Macro], [Details_Report_Key], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000001Q', NULL, NULL, 'SYSTEM0000000007', 5, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Section] ([Report_Section_Key], [Section_List_SQL], [Item_Name_Macro], [Details_Report_Key], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000001R', NULL, NULL, 'SYSTEM0000000007', 6, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Section] ([Report_Section_Key], [Section_List_SQL], [Item_Name_Macro], [Details_Report_Key], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000001S', NULL, NULL, 'SYSTEM0000000007', 7, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Section] ([Report_Section_Key], [Section_List_SQL], [Item_Name_Macro], [Details_Report_Key], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000001T', NULL, NULL, 'SYSTEM0000000007', 8, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Section] ([Report_Section_Key], [Section_List_SQL], [Item_Name_Macro], [Details_Report_Key], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000001U', NULL, NULL, 'SYSTEM0000000004', 4, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Section] ([Report_Section_Key], [Section_List_SQL], [Item_Name_Macro], [Details_Report_Key], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000001V', NULL, NULL, 'SYSTEM0000000004', 5, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Section] ([Report_Section_Key], [Section_List_SQL], [Item_Name_Macro], [Details_Report_Key], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000001W', NULL, NULL, 'SYSTEM0000000004', 6, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Section] ([Report_Section_Key], [Section_List_SQL], [Item_Name_Macro], [Details_Report_Key], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000001X', NULL, NULL, 'SYSTEM0000000004', 7, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Section] ([Report_Section_Key], [Section_List_SQL], [Item_Name_Macro], [Details_Report_Key], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000001Y', NULL, NULL, 'SYSTEM0000000004', 8, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Section] ([Report_Section_Key], [Section_List_SQL], [Item_Name_Macro], [Details_Report_Key], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000001Z', NULL, NULL, 'SYSTEM0000000004', 9, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Section] ([Report_Section_Key], [Section_List_SQL], [Item_Name_Macro], [Details_Report_Key], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000020', NULL, NULL, 'SYSTEM000000000A', 1, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Section] ([Report_Section_Key], [Section_List_SQL], [Item_Name_Macro], [Details_Report_Key], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000021', NULL, 'Material Loaned', 'SYSTEM000000000A', 2, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Section] ([Report_Section_Key], [Section_List_SQL], [Item_Name_Macro], [Details_Report_Key], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000022', NULL, NULL, 'SYSTEM000000000A', 3, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Section] ([Report_Section_Key], [Section_List_SQL], [Item_Name_Macro], [Details_Report_Key], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000023', NULL, NULL, 'SYSTEM000000000A', 4, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Section] ([Report_Section_Key], [Section_List_SQL], [Item_Name_Macro], [Details_Report_Key], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000024', NULL, NULL, 'SYSTEM0000000000', 1, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [Report_Block_In_Section] ([Report_Block_In_Section_Key], [Report_Block_Key], [Report_Section_Key], [Population_SQL], [Population_SQL_Record_Count], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian], [Title]) VALUES ('SYSTEM0000000000', 'SYSTEM0000000012', 'SYSTEM0000000000', '-- Set of options for better use of vw_ConceptTerm.
SET NOCOUNT ON
SET ANSI_NULLS ON
SET ANSI_PADDING ON
SET ANSI_WARNINGS ON
SET ARITHABORT ON
SET CONCAT_NULL_YIELDS_NULL ON
SET QUOTED_IDENTIFIER ON
SET NO_BROWSETABLE OFF

DECLARE @Status VARCHAR(50)
DECLARE @SurveyOrganiser VARCHAR(100)
DECLARE @GeographicInfo VARCHAR(8000)
DECLARE @Department VARCHAR(100)
DECLARE @Key CHAR(16)
DECLARE @ShowCommonNames BIT

SET @Key = ''<#ReportKey>''
SET @ShowCommonNames = <#ShowCommonNames>

--Find Nomanclature status
EXEC usp_CollectionUnitDepartment_Get @Key, @Department OUTPUT

--Find specimen status
EXEC usp_CollectionUnitStatus_Get @Key, @Status OUTPUT

--Find Geographic Info
SELECT @GeographicInfo = M.[Text]
FROM Metadata M 
INNER JOIN Metadata_Type MT ON 
    M.Metadata_Type_Key = MT.Metadata_Type_Key
    AND M.Metadata_Type_Key = ''SYSTEM0000000005'' --Geo Info for Specimen
    AND MT.Table_Name = ''Specimen''
WHERE M.Record_Key=@Key

--Find Survey Organiser
SELECT @SurveyOrganiser = dbo.ufn_GetFormattedName(SY.Run_By)
FROM
	SPECIMEN_UNIT SU
	INNER JOIN
		SPECIMEN_FIELD_DATA SFD
	ON SU.Collection_Unit_Key = SFD.Collection_Unit_Key
		AND SFD.Gathering_Event = 1
	LEFT JOIN 
		COLLECTION_UNIT_NUMBER CUN
	ON SU.Collection_Unit_key = CUN.Collection_Unit_Key 
	LEFT JOIN 
		OCCURRENCE O
	ON SFD.Occurrence_Key = O.Occurrence_Key
	LEFT JOIN
		TAXON_OCCURRENCE XO
	ON SFD.Taxon_Occurrence_Key = XO.Taxon_Occurrence_Key
	INNER JOIN
		SAMPLE SA
	ON (O.Sample_Key = SA.Sample_Key) OR (XO.Sample_Key = SA.Sample_Key)
	INNER JOIN
		Survey_Event SE
	ON SA.Survey_Event_Key = SE.Survey_Event_Key
	INNER JOIN
		Survey SY
	ON SE.Survey_Key = SY.Survey_Key
WHERE SU.Collection_Unit_Key=@Key

--Find all other required information
SELECT TOP 1 
	CASE WHEN SU.Life_Sciences = 0 THEN 
		ISNULL(dbo.ufn_ConceptItemName_Get(D.Concept_Key, @ShowCommonNames, 0, 1), ''No Determination'') 
	ELSE
		ISNULL(dbo.ufn_GetFormattedTaxonNameByParams(ITN.Actual_Name, ITN.Actual_Name_Italic, ITN.Common_Name, 
					ITN.Common_Name_Italic, ITN.Authority, @ShowCommonNames), ''No Determination'') 
	END AS Determination,
	CASE WHEN CTN1.PlainText IS NULL THEN CTN2.PlainText
		ELSE CTN1.PlainText
	END AS NomenclatureStatus,
	CUN.Number AS RegNumber,
	M.Number AS AccNumber,
	CTType.PlainText AS Type,
	@GeographicInfo AS GeographicInfo,
	@Status AS Status,
	dbo.ufn_GetFormattedName(CUName.Name_Key) AS Owner,
	ISNULL(C.Item_Name + '' - '' + CM.Number, C.Item_Name) AS CollectionName,
	@Department AS Department,
	ISNULL(S.Item_Name + '' - '' + SCUN.Number, S.Item_Name) AS StoreName,
	@SurveyOrganiser AS SurveyOrganiser,
	CU.Current_Location_Code AS StoreCode
FROM				
	SPECIMEN_UNIT AS SU
	INNER JOIN
		Collection_Unit CU
	ON SU.Collection_Unit_Key = CU.Collection_Unit_Key
		AND SU.Collection_Unit_Key = @Key
	INNER JOIN 	
		VW_ConceptTerm AS CTType ON 
	SU.Specimen_Type_Concept_Key = CTType.Concept_Key

	LEFT JOIN
		Collection_Unit_Name CUName 
	ON SU.Collection_Unit_Key = CUName.Collection_Unit_Key
		AND CUName.Relation_Type_Concept_Key = ''SYSTEM00000000I7'' --Owner

	LEFT JOIN
		Store S
	ON CU.Current_Container_Collection_Unit_Key = S.Collection_Unit_Key
	LEFT JOIN
		Collection_Unit_Number AS SCUN
	ON S.Collection_Unit_Key = SCUN.Collection_Unit_Key
		AND SCUN.Type_Concept_Key = ''SYSTEM0000000001'' -- Registration Number
		AND SCUN.Preferred = 1

	LEFT JOIN	
		Collection C 
	ON SU.Parent_Collection_Collection_Unit_Key = C.Collection_Unit_Key
	LEFT JOIN
		(MOVEMENT_COLLECTION_UNIT CMCU
		INNER JOIN
			MOVEMENT_DIRECTION CMD
		ON CMCU.Movement_Direction_Key = CMD.Movement_Direction_Key 
			AND (CMD.Outbound = 0)
		INNER JOIN
			MOVEMENT CM
		ON CMD.Movement_Key = CM.Movement_Key AND (CM.Movement_Type = 0 OR CM.Movement_Type = 1))
	ON C.Collection_Unit_Key = CMCU.Collection_Unit_Key

	LEFT JOIN 
		Collection_Unit_Number CUN 
	ON SU.Collection_Unit_key = CUN.Collection_Unit_Key
		AND CUN.Preferred = 1 
		AND CUN.Type_Concept_Key = ''SYSTEM0000000001''

	LEFT JOIN 
		Determination D 
	ON D.Determination_Key = SU.Preferred_Determination_Key

	LEFT JOIN 
		VW_ConceptTermPreferred CTN1
	ON D.Nomenclatural_Status_Concept_Key = CTN1.Concept_Key

	LEFT JOIN 
		Taxon_Determination TD 
	ON SU.Preferred_Taxon_Determination_Key = TD.Taxon_Determination_Key
	LEFT JOIN 
		Index_Taxon_Name ITN 
	ON ITN.Taxon_List_Item_Key = TD.Taxon_List_Item_Key
	LEFT JOIN 
		VW_ConceptTermPreferred CTN2
	ON TD.Nomenclatural_Status_Concept_Key = CTN2.Concept_Key

	LEFT JOIN
		(MOVEMENT_COLLECTION_UNIT MCU
		INNER JOIN
			MOVEMENT_DIRECTION MD
		ON MCU.Movement_Direction_Key = MD.Movement_Direction_Key 
			AND (MD.Outbound = 0)
		INNER JOIN
			MOVEMENT M
		ON MD.Movement_Key = M.Movement_Key AND (M.Movement_Type = 0 OR M.Movement_Type = 1))
	ON CU.Collection_Unit_Key = MCU.Collection_Unit_Key', 'SELECT 1 AS [Count]', 1, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00', 'General');
INSERT INTO [Report_Block_In_Section] ([Report_Block_In_Section_Key], [Report_Block_Key], [Report_Section_Key], [Population_SQL], [Population_SQL_Record_Count], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian], [Title]) VALUES ('SYSTEM0000000001', 'SYSTEM0000000013', 'SYSTEM0000000001', 'SET NOCOUNT ON
-- Set of options for better use of vw_ConceptTerm.
SET ANSI_NULLS ON
SET ANSI_PADDING ON
SET ANSI_WARNINGS ON
SET ARITHABORT ON
SET CONCAT_NULL_YIELDS_NULL ON
SET QUOTED_IDENTIFIER ON
SET NO_BROWSETABLE OFF

SELECT 		CUD.Collection_Unit_Data_Key AS Item_Key,
		CUD.Applies_To,
		CTM.Item_Name AS Method_Term,
		CUD.Duration,
		CUD.Accuracy,
		CTP.Item_Name AS Parameter_Term,
		CTU.Item_Name AS Unit_Term,
		CASE
			WHEN CUD.Upper_Value IS NULL THEN CUD.Lower_Value
			ELSE CUD.Lower_Value + '' - '' + CUD.Upper_Value				
		END AS Value
FROM 		Collection_Unit_Data CUD
INNER JOIN 	vw_ConceptTerm CTP ON CTP.Concept_Key = CUD.Parameter_Concept_Key
LEFT JOIN 	vw_ConceptTerm CTM ON CTM.Concept_Key = CUD.Method_Concept_Key
LEFT JOIN 	vw_ConceptTerm CTU ON CTU.Concept_Key = CUD.Unit_Concept_Key
LEFT JOIN 	Collection_Unit CU ON CU.Collection_Unit_Key = CUD.Collection_Unit_Key
WHERE 		CUD.Collection_Unit_Key = ''<#ReportKey>''
	AND 		CUD.Is_Descriptor = 0', 'SET NOCOUNT ON
SELECT COUNT(*) AS [Count]
FROM 		Collection_Unit_Data
WHERE 		Collection_Unit_Key = ''<#ReportKey>''
	AND 	Is_Descriptor = 0

SET NOCOUNT OFF', 1, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00', 'Measurements');
INSERT INTO [Report_Block_In_Section] ([Report_Block_In_Section_Key], [Report_Block_Key], [Report_Section_Key], [Population_SQL], [Population_SQL_Record_Count], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian], [Title]) VALUES ('SYSTEM0000000002', 'SYSTEM0000000014', 'SYSTEM0000000002', 'SET NOCOUNT ON
/* Set of options for better use of vw_ConceptTerm. */
SET ANSI_NULLS ON
SET ANSI_PADDING ON
SET ANSI_WARNINGS ON
SET ARITHABORT ON
SET CONCAT_NULL_YIELDS_NULL ON
SET QUOTED_IDENTIFIER ON
SET NO_BROWSETABLE OFF

SELECT 	CUD.Collection_Unit_Data_Key AS Item_Key,
	CUD.Applies_To,
	CTM.Item_Name AS Method_Term,
	CUD.Duration,
	CUD.Accuracy,
	CTP.Item_Name AS Parameter_Term,
	CTU.Item_Name AS Unit_Term,
	CUD.Lower_Value AS Value
FROM 		Collection_Unit_Data CUD
INNER JOIN 	vw_ConceptTerm CTP ON CTP.Concept_Key = CUD.Parameter_Concept_Key
LEFT JOIN 	vw_ConceptTerm CTM ON CTM.Concept_Key = CUD.Method_Concept_Key
LEFT JOIN 	vw_ConceptTerm CTU ON CTU.Concept_Key = CUD.Unit_Concept_Key
LEFT JOIN 	Collection_Unit CU ON CU.Collection_Unit_Key = CUD.Collection_Unit_Key
WHERE 		CUD.Collection_Unit_Key = ''<#ReportKey>''
AND 		CUD.Is_Descriptor = 1', 'SET NOCOUNT ON

SELECT COUNT(*) AS [Count]
FROM 		Collection_Unit_Data
WHERE 		Collection_Unit_Key = ''<#ReportKey>''
AND 		Is_Descriptor = 1

SET NOCOUNT OFF', 1, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00', 'Descriptors');
INSERT INTO [Report_Block_In_Section] ([Report_Block_In_Section_Key], [Report_Block_Key], [Report_Section_Key], [Population_SQL], [Population_SQL_Record_Count], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian], [Title]) VALUES ('SYSTEM0000000003', 'SYSTEM0000000015', 'SYSTEM0000000003', 'SET NOCOUNT ON
-- Set of options for better use of vw_ConceptTerm.
SET ANSI_NULLS ON
SET ANSI_PADDING ON
SET ANSI_WARNINGS ON
SET ARITHABORT ON
SET CONCAT_NULL_YIELDS_NULL ON
SET QUOTED_IDENTIFIER ON
SET NO_BROWSETABLE OFF

DECLARE @Key CHAR(16)
DECLARE @ShowCommonNames BIT

SET @Key = ''<#ReportKey>''
SET @ShowCommonNames = 1

SET NOCOUNT ON

DECLARE @ResultTable TABLE
(	[Collection_Unit_Key] [char] (16) COLLATE database_default NOT NULL,
	[RegNo] [varchar] (30) COLLATE database_default NULL,
	[Determination] [nvarchar] (185) COLLATE database_default NOT NULL,
	[Type] [nvarchar] (150) COLLATE database_default NULL,
	[FieldCollector] [varchar] (70) COLLATE database_default NULL,
	[GatheringSite] [varchar] (100) COLLATE database_default NULL,
	[GatheringDate] [varchar] (50) COLLATE database_default NULL,
	[StoreCode] [varchar] (30) COLLATE database_default NULL
)

--Find all basic data
INSERT INTO	@ResultTable
	([Collection_Unit_Key],
	[RegNo],
	[Determination],
	[Type],
	[FieldCollector],
	[StoreCode],
	[GatheringDate],
	[GatheringSite]
	)
SELECT DISTINCT
	SU.Collection_Unit_Key,
	CUN.Number AS RegNo,
	CASE WHEN SU.Life_Sciences = 0 THEN 
		ISNULL(dbo.ufn_ConceptItemName_Get(D.Concept_Key, @ShowCommonNames, 0, 1), ''No Determination'') 
	ELSE
		ISNULL(dbo.ufn_GetFormattedTaxonNameByParams(ITN.Actual_Name, ITN.Actual_Name_Italic, ITN.Common_Name, 
					ITN.Common_Name_Italic, ITN.Authority, @ShowCommonNames), ''No Determination'') 
	END AS Determination,
	CTType.Item_Name AS Type,
 	dbo.ufn_GetFieldCollectors(SU.Collection_Unit_Key) AS FieldCollector,
	CU.Current_Location_Code AS StoreCode,
	dbo.ufn_GetDateFromVagueDate(S.Vague_Date_Start, S.Vague_date_End, S.Vague_Date_Type),
	LN.Item_Name
FROM COLLECTION_UNIT_RELATION CUR
INNER JOIN THESAURUS_RELATION_TYPE TRT ON CUR.Thesaurus_Relation_Type_Key = TRT.Thesaurus_Relation_Type_Key
INNER JOIN SEMANTIC_RELATION SR
    ON TRT.Semantic_Relation_Key = SR.Semantic_Relation_Key
    AND (CUR.From_Collection_Unit_Key = @Key)
    OR ((CUR.To_Collection_Unit_Key = @Key) AND (SR.Unidirectional = 0))
LEFT JOIN	Specimen_Unit SU 
    ON (CUR.From_Collection_Unit_Key = @Key AND CUR.To_Collection_Unit_Key = SU.Collection_Unit_Key)
		OR (CUR.To_Collection_Unit_Key = @Key AND CUR.From_Collection_Unit_Key = SU.Collection_Unit_Key)
INNER JOIN Collection_Unit CU ON SU.Collection_Unit_Key = CU.Collection_Unit_Key
INNER JOIN VW_ConceptTerm AS CTType ON SU.Specimen_Type_Concept_Key = CTType.Concept_Key
LEFT JOIN Collection_Unit_Number CUN 
    ON SU.Collection_Unit_key = CUN.Collection_Unit_Key
		AND CUN.Preferred = 1 
		AND CUN.Type_Concept_Key = ''SYSTEM0000000001''--Reg Number
LEFT JOIN Determination D ON D.Determination_Key = SU.Preferred_Determination_Key
LEFT JOIN VW_ConceptTermPreferred CTN1 ON D.Nomenclatural_Status_Concept_Key = CTN1.Concept_Key
LEFT JOIN Taxon_Determination TD ON SU.Preferred_Taxon_Determination_Key = TD.Taxon_Determination_Key
LEFT JOIN Index_Taxon_Name ITN ON ITN.Taxon_List_Item_Key = TD.Taxon_List_Item_Key
LEFT JOIN Specimen_Field_Data SFD 
    ON SFD.Collection_Unit_Key=SU.Collection_Unit_Key
		AND SFD.Gathering_Event=1
LEFT JOIN Taxon_Occurrence XO ON XO.Taxon_Occurrence_Key=SFD.Taxon_Occurrence_Key
LEFT JOIN Occurrence O ON O.Occurrence_Key=SFD.Occurrence_Key
LEFT JOIN Sample S ON S.Sample_Key IN (XO.Sample_Key, O.Sample_Key)
LEFT JOIN Location_Name LN 
    ON LN.Location_Key=S.Location_Key 
    AND LN.Preferred=1

SELECT 
	[Collection_Unit_Key],
	[RegNo],
	[Determination],
	[Type],
	[FieldCollector],
	[GatheringSite],
	[GatheringDate],
	[StoreCode]
FROM
	@ResultTable', 'DECLARE @Key CHAR(16)

SET @Key = ''<#ReportKey>''
SET NOCOUNT ON

SELECT COUNT(DISTINCT SU.Collection_Unit_Key) AS [Count]
FROM COLLECTION_UNIT_RELATION CUR
LEFT JOIN	THESAURUS_RELATION_TYPE TRT	ON CUR.Thesaurus_Relation_Type_Key = TRT.Thesaurus_Relation_Type_Key
INNER JOIN SEMANTIC_RELATION SR
    ON TRT.Semantic_Relation_Key = SR.Semantic_Relation_Key
		AND (CUR.From_Collection_Unit_Key = @Key)
		OR ((CUR.To_Collection_Unit_Key = @Key) AND (SR.Unidirectional = 0))
LEFT JOIN	Specimen_Unit SU 
    ON (CUR.From_Collection_Unit_Key = @Key AND CUR.To_Collection_Unit_Key = SU.Collection_Unit_Key)
    OR (CUR.To_Collection_Unit_Key = @Key AND CUR.From_Collection_Unit_Key = SU.Collection_Unit_Key)', 1, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00', 'Linked Specimens');
INSERT INTO [Report_Block_In_Section] ([Report_Block_In_Section_Key], [Report_Block_Key], [Report_Section_Key], [Population_SQL], [Population_SQL_Record_Count], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian], [Title]) VALUES ('SYSTEM0000000004', 'SYSTEM0000000016', 'SYSTEM0000000004', '
-- Set of options for better use of vw_ConceptTerm.
SET NOCOUNT ON
SET ANSI_NULLS ON
SET ANSI_PADDING ON
SET ANSI_WARNINGS ON
SET ARITHABORT ON
SET CONCAT_NULL_YIELDS_NULL ON
SET QUOTED_IDENTIFIER ON
SET NO_BROWSETABLE OFF

DECLARE @Key CHAR(16)
DECLARE @ShowCommonNames BIT

SET @Key = ''<#ReportKey>''
SET @ShowCommonNames = 1

SELECT
	CASE WHEN SU.Life_Sciences = 0 THEN 
		ISNULL(dbo.ufn_ConceptItemName_Get(D.Concept_Key, @ShowCommonNames, 0, 1), ''No Determination'') 
	ELSE
		ISNULL(dbo.ufn_GetFormattedTaxonNameByParams(ITN.Actual_Name, ITN.Actual_Name_Italic, ITN.Common_Name, 
					ITN.Common_Name_Italic, ITN.Authority, @ShowCommonNames), ''No Determination'') 
	END AS Determination,
	
	CASE WHEN SU.Life_Sciences = 0 THEN
		dbo.ufn_GetFormattedName(D.Determiner_Name_Key)
	ELSE
		dbo.ufn_GetFormattedName(TD.Determiner)
	END AS Determiner,
	CASE WHEN SU.Life_Sciences = 0 THEN
		dbo.ufn_GetDateFromVagueDate(D.Vague_Date_Start, D.Vague_Date_End, D.Vague_Date_Type)
	ELSE
		dbo.ufn_GetDateFromVagueDate(TD.Vague_Date_Start, TD.Vague_Date_End, TD.Vague_Date_Type)
	END AS DetDate,
	CASE WHEN SU.Life_Sciences = 0 THEN
		DT.Short_Name
	ELSE
		TDT.Short_Name
	END AS Type,
	CASE WHEN SU.Life_Sciences = 0 THEN
		dbo.ufn_GetTranslation_String_From_Value(D.Confidence, ''Confidence'')
	ELSE
		dbo.ufn_GetTranslation_String_From_Value(TD.Confidence, ''Confidence'')
	END AS Confidence
FROM 
	Specimen_Unit SU

	LEFT JOIN 
		(Taxon_Determination TD
		INNER JOIN
			Determination_Type TDT
		ON TD.Determination_Type_Key = TDT.Determination_Type_Key
		INNER JOIN
			Index_Taxon_Name ITN
		ON TD.Taxon_List_Item_Key = ITN.Taxon_List_Item_Key
		LEFT JOIN
			Concept C
		ON TD.Nomenclatural_Status_Concept_Key = C.Concept_Key)
	ON SU.Collection_Unit_Key = TD.Specimen_Collection_Unit_Key 

	LEFT JOIN
		(Determination D
		INNER JOIN
			Determination_Type DT
		ON D.Determination_Type_Key = DT.Determination_Type_Key
		INNER JOIN 
			VW_ConceptTerm CT 
		ON D.Concept_Key = CT.Concept_Key)
	ON SU.Collection_Unit_Key = D.Specimen_Collection_Unit_Key 
WHERE SU.Collection_Unit_Key = @Key', 'DECLARE @Key CHAR(16)

SET NOCOUNT ON
SET @Key = ''<#ReportKey>''

SELECT COUNT(*) AS [Count]
FROM 
	Specimen_Unit SU

	LEFT JOIN 
		Taxon_Determination TD
	ON SU.Collection_Unit_Key = TD.Specimen_Collection_Unit_Key 

	LEFT JOIN
		Determination D
	ON SU.Collection_Unit_Key = D.Specimen_Collection_Unit_Key 
WHERE SU.Collection_Unit_Key = @Key', 1, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00', 'Determinations');
INSERT INTO [Report_Block_In_Section] ([Report_Block_In_Section_Key], [Report_Block_Key], [Report_Section_Key], [Population_SQL], [Population_SQL_Record_Count], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian], [Title]) VALUES ('SYSTEM0000000005', 'SYSTEM0000000017', 'SYSTEM0000000005', 'SET NOCOUNT ON
DECLARE @Key CHAR(16)

SET @Key = ''<#ReportKey>''

SELECT DISTINCT
 	SUR.Item_Name AS SurveyName,
	LN.Item_Name AS LocationName,
	dbo.ufn_GetFieldCollectors(@Key) AS FieldCollectors,
	dbo.ufn_GetDateFromVagueDate(S.Vague_Date_Start, S.Vague_Date_End, S.Vague_Date_Type) AS SampleDate
FROM
	Specimen_Unit SU
	INNER JOIN 
		Specimen_Field_Data SFD 
	ON SU.Collection_Unit_Key = SFD.Collection_Unit_Key
		AND SU.Collection_Unit_Key = @Key
		AND SFD.Gathering_Event = 1
	LEFT JOIN 
		Occurrence O 
	ON SFD.Occurrence_Key = O.Occurrence_Key
	LEFT JOIN
		Taxon_Occurrence XO
	ON SFD.Taxon_Occurrence_Key = XO.Taxon_Occurrence_Key
	INNER JOIN 
		[Sample] S
	ON ((O.Sample_Key = S.Sample_Key) OR (XO.Sample_Key = S.Sample_Key))
	INNER JOIN
		Survey_Event SE
	ON S.Survey_Event_Key = SE.Survey_Event_Key
	INNER JOIN
		Survey SUR
	ON SE.Survey_Key = SUR.Survey_Key
	LEFT JOIN
		Location_Name LN
	ON S.Location_Key = LN.Location_Key', 'SET NOCOUNT ON
DECLARE @Key CHAR(16)

SET @Key = ''<#ReportKey>''

SELECT COUNT(DISTINCT S.Sample_Key) AS [Count]
FROM
	Specimen_Unit SU
	INNER JOIN 
		Specimen_Field_Data SFD 
	ON SU.Collection_Unit_Key = SFD.Collection_Unit_Key
		AND SU.Collection_Unit_Key = @Key
		AND SFD.Gathering_Event = 1
	LEFT JOIN 
		Occurrence O 
	ON SFD.Occurrence_Key = O.Occurrence_Key
	LEFT JOIN
		Taxon_Occurrence XO
	ON SFD.Taxon_Occurrence_Key = XO.Taxon_Occurrence_Key
	INNER JOIN 
		[Sample] S
	ON ((O.Sample_Key = S.Sample_Key) OR (XO.Sample_Key = S.Sample_Key))
', 1, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00', 'Gathering Data');
INSERT INTO [Report_Block_In_Section] ([Report_Block_In_Section_Key], [Report_Block_Key], [Report_Section_Key], [Population_SQL], [Population_SQL_Record_Count], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian], [Title]) VALUES ('SYSTEM0000000006', 'SYSTEM0000000018', 'SYSTEM0000000006', 'EXEC usp_StoragePlace_Select_ForCollectionUnit <#UserDomainMask>, ''<#SessionID>'', ''<#ReportKey>''', '
DECLARE 
	@Collection_Unit_Key CHAR(16), 
	@Parent_Collection_Unit_Key CHAR(16), 
	@Item_Index INT

/* Obtain parent of input store */
SELECT @Parent_Collection_Unit_Key = CU.Current_Container_Collection_Unit_Key
FROM Collection_Unit CU
INNER JOIN Store S ON S.Collection_Unit_Key=CU.Current_Container_Collection_Unit_Key
WHERE CU.Collection_Unit_Key = ''<#ReportKey>''

SET @Item_Index = 0

/* Obtain successive parents */
WHILE @Parent_Collection_Unit_Key IS NOT NULL
BEGIN
SELECT 
	@Collection_Unit_Key = S.Collection_Unit_Key, 
	@Parent_Collection_Unit_Key = CU.Current_Container_Collection_Unit_Key
FROM Store S
INNER JOIN Collection_Unit CU ON S.Collection_Unit_Key = CU.Collection_Unit_Key
		AND S.Collection_Unit_Key = @Parent_Collection_Unit_Key

SET @Item_Index = @Item_Index + 1
END

/* Return the hierarchical record count */
SELECT @Item_Index AS [Count]
', 1, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00', 'Storage Place');
INSERT INTO [Report_Block_In_Section] ([Report_Block_In_Section_Key], [Report_Block_Key], [Report_Section_Key], [Population_SQL], [Population_SQL_Record_Count], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian], [Title]) VALUES ('SYSTEM0000000007', 'SYSTEM0000000019', 'SYSTEM0000000007', 'SET NOCOUNT ON
SELECT 
	Specimen_Label_Key AS Item_Key, 
	Inscription AS Item_Name, 
	dbo.ufn_GetTranslation_String_From_Value(Is_Inscription, ''LabelInscription'') AS Type,
	dbo.ufn_GetFormattedName(Author_Name_Key) AS Author
FROM 
	SPECIMEN_LABEL
WHERE Collection_Unit_Key = ''<#ReportKey>''', 'SET NOCOUNT ON
SELECT COUNT(*) AS [Count]
FROM 
	SPECIMEN_LABEL
WHERE Collection_Unit_Key = ''<#ReportKey>''', 1, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00', 'Inscriptions & Labels');
INSERT INTO [Report_Block_In_Section] ([Report_Block_In_Section_Key], [Report_Block_Key], [Report_Section_Key], [Population_SQL], [Population_SQL_Record_Count], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian], [Title]) VALUES ('SYSTEM0000000008', 'SYSTEM0000000020', 'SYSTEM0000000008', 'SET NOCOUNT ON
SELECT 
    dbo.ufn_RtfToPlainText(R.Title) AS Document,
	dbo.ufn_GetTranslation_String_From_Value(SJ.Original, ''YesNo'') AS Original,
	R.Reference_Type AS Type
FROM
	Source_Join AS SJ
	INNER JOIN
		Reference AS R
	ON R.Source_Key = SJ.Source_Key
	INNER JOIN
		Source AS S 
	ON S.Source_Key = SJ.Source_Key 
	AND S.Internal = 1
WHERE SJ.Record_Key = ''<#ReportKey>''
	AND SJ.Table_Name = ''Specimen_Unit''', 'SET NOCOUNT ON
SELECT COUNT(*) AS [Count]
FROM
	Source_Join AS SJ
	INNER JOIN
		Reference AS R
	ON R.Source_Key = SJ.Source_Key
	INNER JOIN
		Source AS S 
	ON S.Source_Key = SJ.Source_Key 
	AND S.Internal = 1
WHERE SJ.Record_Key = ''<#ReportKey>''
	AND SJ.Table_Name = ''Specimen_Unit''', 1, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00', 'Sources');
INSERT INTO [Report_Block_In_Section] ([Report_Block_In_Section_Key], [Report_Block_Key], [Report_Section_Key], [Population_SQL], [Population_SQL_Record_Count], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian], [Title]) VALUES ('SYSTEM0000000009', 'SYSTEM0000000021', 'SYSTEM0000000009', 'SET NOCOUNT ON
SELECT SF.[File_Name] AS Item_Name
FROM		Source_Join AS SJ
INNER JOIN	Source_File AS SF ON SF.Source_Key = SJ.Source_Key
INNER JOIN	Source AS S ON S.Source_Key = SJ.Source_Key AND S.Internal = 0
WHERE		SJ.Record_Key = ''<#ReportKey>''
AND 		Table_Name = ''Specimen_Unit''', 'SET NOCOUNT ON
SELECT COUNT(*) AS [Count]
FROM		Source_Join AS SJ
INNER JOIN	Source_File AS SF ON SF.Source_Key = SJ.Source_Key
INNER JOIN	Source AS S ON S.Source_Key = SJ.Source_Key AND S.Internal = 0
WHERE		SJ.Record_Key = ''<#ReportKey>''
AND 		Table_Name = ''Specimen_Unit''', 1, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00', 'External References');
INSERT INTO [Report_Block_In_Section] ([Report_Block_In_Section_Key], [Report_Block_Key], [Report_Section_Key], [Population_SQL], [Population_SQL_Record_Count], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian], [Title]) VALUES ('SYSTEM000000000A', 'SYSTEM0000000022', 'SYSTEM000000000A', '
SELECT
  ct.Conservation_Task_Key, 
  cc.Ref_Number + '' - '' + dbo.ufn_GetFormattedName(cc.Checked_By_Name_Key) + '' - '' +
    dbo.ufn_GetDateFromVagueDate(cc.Vague_Date_Start, cc.Vague_Date_End, cc.Vague_Date_Type) AS [Check], 
  ctype.PlainText AS Type,
  dbo.ufn_GetTranslation_String_From_Value(ct.Status, ''STATUS'') AS Status,
  dbo.ufn_GetTranslation_String_From_Value(ct.Priority, ''PRIORITY'') AS Priority,
  ct.Task_Action AS [Action],
  CAST(ct.Duration AS VARCHAR(20)) + '' '' + cdur.PlainText AS Duration,
  dbo.ufn_GetFormattedName(ct.Identifier_Name_Key) AS IdentifiedBy,
  dbo.ufn_GetDateFromVagueDate(ct.Set_Vague_Date_Start, ct.Set_Vague_Date_End, ct.Set_Vague_Date_Type) AS DateSet,
  CAST(cj.Job_Number as varchar(20)) + '' - '' + cj.Item_Name + '' - '' +
    dbo.ufn_GetTranslation_String_From_Value(ct.Status, ''STATUS'') AS JobAssignedTo
FROM				Conservation_Task ct
LEFT JOIN		Conservation_Check cc on cc.Conservation_Check_Key = ct.Conservation_Check_Key
INNER JOIN 	VW_ConceptTerm ctype on ctype.Concept_Key = ct.Type_Concept_Key
LEFT JOIN 	VW_ConceptTerm cdur on cdur.Concept_Key = ct.Duration_Unit_Concept_Key
LEFT JOIN 	Conservation_Job cj on cj.Conservation_Job_Key=ct.Conservation_Job_Key
WHERE				ct.Conservation_Task_Key = ''<#ReportKey>''', 'SELECT 1 AS [Count]', 1, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00', 'General');
INSERT INTO [Report_Block_In_Section] ([Report_Block_In_Section_Key], [Report_Block_Key], [Report_Section_Key], [Population_SQL], [Population_SQL_Record_Count], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian], [Title]) VALUES ('SYSTEM000000000B', 'SYSTEM0000000023', 'SYSTEM000000000B', '
SELECT 	[FILE_NAME]
FROM		Source_File
JOIN		Source_Join ON Source_file.Source_Key = Source_Join.Source_Key
WHERE		Source_Join.Table_Name = ''Conservation_Task''
AND			Source_File.Preferred = 1
AND			Source_Join.Record_Key = ''<#ReportKey>''
	', 'SELECT 1 AS [Count]', 1, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00', 'Multimedia');
INSERT INTO [Report_Block_In_Section] ([Report_Block_In_Section_Key], [Report_Block_Key], [Report_Section_Key], [Population_SQL], [Population_SQL_Record_Count], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian], [Title]) VALUES ('SYSTEM000000000C', 'SYSTEM0000000020', 'SYSTEM000000000C', '
SELECT
    dbo.ufn_RtfToPlainText(R.Title) AS Document,
				dbo.ufn_GetTranslation_String_From_Value(SJ.Original, ''YesNo'') AS Original,
				R.Reference_Type AS Type
FROM		Source_Join AS SJ
JOIN		Reference AS R ON SJ.Source_Key = R.Source_Key
JOIN		Source AS S ON SJ.Source_Key = S.Source_Key
WHERE		SJ.Table_Name = ''Conservation_Task''
AND			S.Internal = 1
AND			SJ.Record_Key = ''<#ReportKey>''', '
SELECT	Count(*) AS [Count]
FROM		Source_Join AS SJ
JOIN		Source AS S ON SJ.Source_Key = S.Source_Key
WHERE		SJ.Table_Name = ''Conservation_Task''
AND			S.Internal = 1
AND			SJ.Record_Key = ''<#ReportKey>''
	', 1, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00', 'Sources');
INSERT INTO [Report_Block_In_Section] ([Report_Block_In_Section_Key], [Report_Block_Key], [Report_Section_Key], [Population_SQL], [Population_SQL_Record_Count], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian], [Title]) VALUES ('SYSTEM000000000D', 'SYSTEM0000000021', 'SYSTEM000000000D', '
SELECT	SF.[File_Name] AS Item_Name
FROM		Source_Join AS SJ
JOIN		Source_File AS SF ON SF.Source_Key = SJ.Source_Key
JOIN		Source AS S ON S.Source_Key = SJ.Source_Key
WHERE		SJ.Record_Key = ''<#ReportKey>''
AND			S.Internal = 0
AND 		Table_Name = ''Conservation_Task''
	', '
SELECT	COUNT (*) AS [Count]
FROM		Source_Join AS SJ
JOIN		Source AS S ON S.Source_Key = SJ.Source_Key
WHERE		SJ.Record_Key = ''<#ReportKey>''
AND			S.Internal = 0
AND 		Table_Name = ''Conservation_Task''
	', 1, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00', 'External References');
INSERT INTO [Report_Block_In_Section] ([Report_Block_In_Section_Key], [Report_Block_Key], [Report_Section_Key], [Population_SQL], [Population_SQL_Record_Count], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian], [Title]) VALUES ('SYSTEM000000000E', 'SYSTEM0000000024', 'SYSTEM000000000E', 'SELECT
    V.Ref_Number AS ReferenceNumber,
    dbo.ufn_GetDateFromVagueDate(V.Vague_Date_Start, V.Vague_Date_End, V.Vague_Date_Type) AS [Date],
    CType.PlainText AS Type,
    dbo.ufn_GetFormattedName(V.Valued_By_Name_Key) AS ValuedBy,
    ISNULL(SUBSTRING(TF.Data, 1, 4), '''') + 
      CASE WHEN V.Value_Amount=ROUND(V.Value_Amount, 0) THEN
        --Lop of the decimal places if not required  
        LEFT(CONVERT(VARCHAR(20), V.Value_Amount, 1), LEN(CONVERT(VARCHAR(20), V.Value_Amount, 1))-3)
      ELSE
        CONVERT(VARCHAR(20), V.Value_Amount, 1)
      END + 
      CASE 
        WHEN TF.DATA IS NULL THEN
          '' '' + Ccurr.Item_Name
        ELSE
          ''''
      END
    AS Value,
    ISNULL(dbo.ufn_GetDateFromVagueDate(V.Valid_From_Vague_Date_Start, V.Valid_From_Vague_Date_End, V.Valid_From_Vague_Date_Type), '''') + '' - '' 
        + ISNULL(dbo.ufn_GetDateFromVagueDate(V.Valid_To_Vague_Date_Start, V.Valid_To_Vague_Date_End, V.Valid_To_Vague_Date_Type), '''') 
    AS Valid,
    V.[Description]
FROM			Valuation V
LEFT JOIN	VW_ConceptTerm CType ON V.Type_Concept_Key = CType.Concept_Key
LEFT JOIN	Concept C ON C.Concept_Key = V.Currency_Concept_Key
LEFT JOIN VW_ConceptTerm Ccurr ON Ccurr.Concept_Key=V.Currency_Concept_Key
LEFT JOIN	Thesaurus_Fact TF
	ON (TF.Meaning_Key = C.Meaning_Key AND
			TF.Fact_Type_Concept_Key = ''SYSTEM000000009A'')
WHERE			V.Valuation_Key = ''<#ReportKey>''', 'SELECT 1 AS [Count]', 1, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00', 'General');
INSERT INTO [Report_Block_In_Section] ([Report_Block_In_Section_Key], [Report_Block_Key], [Report_Section_Key], [Population_SQL], [Population_SQL_Record_Count], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian], [Title]) VALUES ('SYSTEM000000000F', 'SYSTEM0000000009', 'SYSTEM000000000F', '
SELECT DISTINCT
  dbo.ufn_GetRegNumber(SU.Collection_Unit_Key) AS RegNo,
  CASE SU.Life_Sciences
    WHEN 1 THEN ITN.Preferred_Name
    ELSE CTP.Item_Name
  END AS Determination,
  CType.PlainText AS Type,
  dbo.ufn_GetFieldCollectors(CU.Collection_Unit_Key) AS FieldCollector,
  LN.Item_Name AS GatheringSite,
  dbo.ufn_GetDateFromVagueDate(S.Vague_Date_Start, S.Vague_Date_End, S.Vague_Date_Type) AS GatheringDate,
  CU.Current_Location_Code AS Store
FROM			Collection_Unit_Valuation AS CUV
JOIN			Collection_Unit AS CU ON CUV.Collection_Unit_Key = CU.Collection_Unit_Key
JOIN			Specimen_Unit	AS SU ON CUV.Collection_Unit_Key = SU.Collection_Unit_Key
LEFT JOIN	(Determination AS D
					JOIN vw_ConceptTermPreferred CTP ON D.Concept_Key = CTP.Concept_Key)
					ON SU.Preferred_Determination_Key = D.Determination_Key
LEFT JOIN	(Taxon_Determination AS TD
					JOIN Index_Taxon_Name AS ITN ON TD.Taxon_List_Item_Key = ITN.Taxon_List_Item_Key)
					ON SU.Preferred_Taxon_Determination_Key = TD.Taxon_Determination_Key
LEFT JOIN	vw_ConceptTerm CType ON SU.Specimen_Type_Concept_Key = CType.Concept_Key
LEFT JOIN (Specimen_Field_Data SFD
					LEFT JOIN Taxon_Occurrence AS XO ON SFD.Taxon_Occurrence_Key = XO.Taxon_Occurrence_Key
					LEFT JOIN Occurrence AS O ON SFD.Occurrence_Key = O.Occurrence_Key
					JOIN [Sample] AS S ON ISNULL(O.Sample_Key, XO.Sample_Key) = S.Sample_Key
					JOIN Location_Name AS LN ON S.Location_Key = LN.Location_Key AND LN.Preferred = 1)
					ON CU.Collection_Unit_Key = SFD.Collection_Unit_Key
					AND SFD.Gathering_Event = 1  
WHERE			CUV.Valuation_Key = ''<#ReportKey>''', 'SELECT Count(*) AS [Count]
FROM Collection_Unit_Valuation AS CUV
JOIN Specimen_Unit AS SU ON CUV.Collection_Unit_Key = SU.Collection_Unit_Key
WHERE CUV.Valuation_Key = ''<#ReportKey>''', 1, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00', 'Specimens');
INSERT INTO [Report_Block_In_Section] ([Report_Block_In_Section_Key], [Report_Block_Key], [Report_Section_Key], [Population_SQL], [Population_SQL_Record_Count], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian], [Title]) VALUES ('SYSTEM000000000G', 'SYSTEM000000003Q', 'SYSTEM000000000G', '
SELECT M.Number AS AccNo,
					C.Item_Name AS [Name],
					C.Topic AS Topic,
					dbo.ufn_GetFormattedName(C.Assembler_Name_Key) AS AssembledBy
FROM			Collection_Unit_Valuation AS CUV
JOIN			Collection_Unit AS CU ON CUV.Collection_Unit_Key = CU.Collection_Unit_Key
LEFT JOIN	(Movement_Collection_Unit AS MCU
					JOIN Movement_Direction AS MD ON MCU.Movement_Direction_Key = MD.Movement_Direction_Key AND MD.Outbound = 0
					JOIN Movement AS M ON MD.Movement_Key = M.Movement_Key AND M.Movement_Type IN (0, 1)
					JOIN Movement_Of_Ownership AS MOO ON MD.Movement_Direction_Key = MOO.Movement_Direction_Key)
					ON CU.Collection_Unit_Key = MCU.Collection_Unit_Key
JOIN			Collection AS C ON CU.Collection_Unit_Key = C.Collection_Unit_Key
LEFT JOIN	Collection_Unit_Number AS CUN
						ON CU.Collection_Unit_Key = CUN.Collection_Unit_Key
						AND CUN.Type_Concept_Key = ''SYSTEM0000000001''
						AND CUN.Preferred = 1
WHERE			CUV.Valuation_Key = ''<#ReportKey>''', 'SELECT Count(*) AS [Count]
FROM Collection_Unit_Valuation AS CUV
JOIN Collection AS C ON CUV.Collection_Unit_Key = C.Collection_Unit_Key
WHERE CUV.Valuation_Key = ''<#ReportKey>''', 1, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00', 'Collections');
INSERT INTO [Report_Block_In_Section] ([Report_Block_In_Section_Key], [Report_Block_Key], [Report_Section_Key], [Population_SQL], [Population_SQL_Record_Count], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian], [Title]) VALUES ('SYSTEM000000000H', 'SYSTEM0000000020', 'SYSTEM000000000H', '
SELECT
    dbo.ufn_RtfToPlainText(R.Title) AS Document,
				dbo.ufn_GetTranslation_String_From_Value(SJ.Original, ''YesNo'') AS Original,
				R.Reference_Type AS Type
FROM		Source_Join AS SJ
JOIN		Reference AS R ON SJ.Source_Key = R.Source_Key
JOIN		Source AS S ON SJ.Source_Key = S.Source_Key
WHERE		SJ.Table_Name = ''Valuation''
AND			S.Internal = 1
AND			SJ.Record_Key = ''<#ReportKey>''', '
SELECT	Count(*) AS [Count]
FROM		Source_Join AS SJ
JOIN		Source AS S ON SJ.Source_Key = S.Source_Key
WHERE		SJ.Table_Name = ''Valuation''
AND			S.Internal = 1
AND			SJ.Record_Key = ''<#ReportKey>''
	', 1, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00', 'Sources');
INSERT INTO [Report_Block_In_Section] ([Report_Block_In_Section_Key], [Report_Block_Key], [Report_Section_Key], [Population_SQL], [Population_SQL_Record_Count], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian], [Title]) VALUES ('SYSTEM000000000I', 'SYSTEM0000000021', 'SYSTEM000000000I', '
SELECT	SF.[File_Name] AS Item_Name
FROM		Source_Join AS SJ
JOIN		Source_File AS SF ON SF.Source_Key = SJ.Source_Key
JOIN		Source AS S ON S.Source_Key = SJ.Source_Key
WHERE		SJ.Record_Key = ''<#ReportKey>''
AND			S.Internal = 0
AND 		Table_Name = ''Valuation''
	', '
SELECT	COUNT (*) AS [Count]
FROM		Source_Join AS SJ
JOIN		Source AS S ON S.Source_Key = SJ.Source_Key
WHERE		SJ.Record_Key = ''<#ReportKey>''
AND			S.Internal = 0
AND 		Table_Name = ''Valuation''
	', 1, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00', 'External References');
INSERT INTO [Report_Block_In_Section] ([Report_Block_In_Section_Key], [Report_Block_Key], [Report_Section_Key], [Population_SQL], [Population_SQL_Record_Count], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian], [Title]) VALUES ('SYSTEM000000000J', 'SYSTEM0000000027', 'SYSTEM000000000J', '
DECLARE		@Name			varchar(100)
DECLARE		@Names		VARCHAR(1000)
DECLARE		StaffCursor CURSOR FAST_FORWARD FOR
SELECT		dbo.ufn_GetFormattedName(Name_Key) AS Name_Key
FROM			Conservation_Job_Staff
WHERE			Conservation_Job_Key = ''<#ReportKey>''

OPEN			StaffCursor

FETCH NEXT FROM StaffCursor INTO @Name

SET @Names = ''''

WHILE			@@FETCH_STATUS = 0
BEGIN
	SET @Names = @Names + @Name + '', ''
	FETCH NEXT FROM StaffCursor INTO @Name
END

CLOSE StaffCursor
DEALLOCATE StaffCursor

IF LEN(@Names) >= 2
	SET @Names = LEFT(@Names, LEN(@Names) - 1)

DECLARE	@Mask								INT
DECLARE	@KnownSubjectAreas	VARCHAR(1000)

SELECT	@Mask = Domain_Mask
FROM		Conservation_Job
WHERE		Conservation_Job_Key = ''<#ReportKey>''

EXEC usp_DomainsForMask_Get @Mask, @KnownSubjectAreas OUTPUT

SELECT		Convert(VARCHAR(20), CJ.Job_Number) + '' - '' +
					CASE CJ.Status
						WHEN 0 THEN ''Pending''
						WHEN 1 THEN ''Open''
						WHEN 2 THEN ''Closed''
						WHEN 3 THEN ''Postponed''
						WHEN 4 THEN ''Abandoned''
					END AS JobNo,
					ISNULL(dbo.ufn_GetDateFromVagueDate(CJ.From_Vague_Date_Start, CJ.From_Vague_Date_End, CJ.From_Vague_Date_Type), '''') + '' - '' + ISNULL(dbo.ufn_GetDateFromVagueDate(CJ.To_Vague_Date_Start, CJ.To_Vague_Date_End, CJ.To_Vague_Date_Type), '''') AS [Date],
					CJ.Item_Name AS [Name],
					CJ.Details,
					@Names AS StaffInvolved,
					ISNULL(SUBSTRING(TF.Data, 1, 4), '''') + CONVERT(VARCHAR(20), CJ.Cost, 1) AS Cost,
					@KnownSubjectAreas AS KnownSubjectAreas
FROM			Conservation_Job AS CJ
LEFT JOIN	Concept C ON C.Concept_Key = CJ.Currency_Concept_Key
LEFT JOIN	Thesaurus_Fact TF
	ON (TF.Meaning_Key = C.Meaning_Key AND
			TF.Fact_Type_Concept_Key = ''SYSTEM000000009A'')
WHERE			CJ.Conservation_Job_Key = ''<#ReportKey>''
	', '
SELECT 1 AS [Count]
	', 1, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00', 'General');
INSERT INTO [Report_Block_In_Section] ([Report_Block_In_Section_Key], [Report_Block_Key], [Report_Section_Key], [Population_SQL], [Population_SQL_Record_Count], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian], [Title]) VALUES ('SYSTEM000000000K', 'SYSTEM0000000028', 'SYSTEM000000000K', 'SELECT  dbo.ufn_GetDateFromVagueDate(CT.Set_Vague_Date_Start, CT.Set_Vague_Date_End, CT.Set_Vague_Date_Type) AS DateSet, CASE CT.Priority WHEN 0 THEN ''Low'' WHEN 1 THEN ''Medium'' WHEN 2 THEN ''High'' WHEN 3 THEN ''Urgent'' END AS Priority, CASE CT.Status WHEN 0 THEN ''Pending'' WHEN 1 THEN ''Open'' WHEN 2 THEN ''Closed'' WHEN 3 THEN ''Postponed'' WHEN 4 THEN ''Abandoned'' END AS Status, CType.PlainText AS Type, CONVERT(VARCHAR(20), Duration) + '' '' + CUnit.PlainText AS Duration, CT.Task_Action AS [Action], dbo.ufn_GetFormattedName(CT.Identifier_Name_Key) AS IdentifiedBy FROM Conservation_Task AS CT JOIN Conservation_Check AS CC ON CC.Conservation_Check_Key = CT.Conservation_Check_Key AND CT.Conservation_Job_Key = ''<#ReportKey>'' AND ((CC.Domain_Mask & <#UserDomainMask> > 0) OR (CC.Entered_Session_ID = ''<#SessionID>'') OR (CC.Changed_Session_ID = ''<#SessionID>'') OR (CC.Domain_Mask = 0)) LEFT JOIN VW_ConceptTerm AS CType ON CT.Type_Concept_Key = CType.Concept_Key LEFT JOIN VW_ConceptTerm AS CUnit ON CT.Duration_Unit_Concept_Key = CUnit.Concept_Key', '
SELECT		Count(*) AS [Count]
FROM			Conservation_Task AS CT
WHERE			CT.Conservation_Job_Key = ''<#ReportKey>''
	', 1, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00', 'Tasks Included');
INSERT INTO [Report_Block_In_Section] ([Report_Block_In_Section_Key], [Report_Block_Key], [Report_Section_Key], [Population_SQL], [Population_SQL_Record_Count], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian], [Title]) VALUES ('SYSTEM000000000L', 'SYSTEM0000000029', 'SYSTEM000000000L', '
SELECT		CMaterial.PlainText AS Material,
					CJM.Quantity + '' '' + CUnit.PlainText COLLATE SQL_LATIN1_GENERAL_CP1_CI_AS AS Quantity
FROM			Conservation_Job_Material AS CJM
LEFT JOIN VW_ConceptTerm AS CMaterial	ON CJM.Material_Concept_Key = CMaterial.Concept_Key
LEFT JOIN	VW_ConceptTerm AS CUnit ON CJM.Unit_Concept_Key = CUnit.Concept_Key
WHERE			CJM.Conservation_Job_Key = ''<#ReportKey>''
	', '
SELECT		Count(*) AS [Count]
FROM			Conservation_Job_Material AS CJM
WHERE			CJM.Conservation_Job_Key = ''<#ReportKey>''
	', 1, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00', 'Material List');
INSERT INTO [Report_Block_In_Section] ([Report_Block_In_Section_Key], [Report_Block_Key], [Report_Section_Key], [Population_SQL], [Population_SQL_Record_Count], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian], [Title]) VALUES ('SYSTEM000000000M', 'SYSTEM0000000005', 'SYSTEM000000000M', 'SELECT dbo.ufn_GetDateFromVagueDate(CJF.Vague_Date_Start, CJF.Vague_Date_End, CJF.Vague_Date_Type) AS [Date], dbo.ufn_GetFormattedName(CJF.Funded_By_Name_Key) AS FundedBy, ISNULL(CAST(TF.Data AS VARCHAR(10)), '''') + CONVERT(VARCHAR(20), Amount, 1) + CASE WHEN TF.Data IS NULL THEN ISNULL('' '' + C.Item_Name, '''') ELSE '''' END AS Amount, CJF.Details FROM Conservation_Job_Funding AS CJF LEFT JOIN vw_ConceptTerm C ON CJF.Currency_Concept_Key = C.Concept_Key LEFT JOIN Thesaurus_Fact TF ON (TF.Meaning_Key = C.Meaning_Key AND TF.Fact_Type_Concept_Key = ''SYSTEM000000009A'') WHERE CJF.Conservation_Job_Key = ''<#ReportKey>''', 'SELECT Count(*) AS [Count]
FROM Conservation_Job_Funding AS CJF
WHERE CJF.Conservation_Job_Key = ''<#ReportKey>''', 1, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00', 'Funding List');
INSERT INTO [Report_Block_In_Section] ([Report_Block_In_Section_Key], [Report_Block_Key], [Report_Section_Key], [Population_SQL], [Population_SQL_Record_Count], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian], [Title]) VALUES ('SYSTEM000000000N', 'SYSTEM0000000009', 'SYSTEM000000000N', '
SET ANSI_NULLS ON
SET ANSI_PADDING ON  
SET ANSI_WARNINGS ON  
SET CONCAT_NULL_YIELDS_NULL ON  
SET QUOTED_IDENTIFIER ON  
SET NO_BROWSETABLE OFF    

SELECT
  dbo.ufn_GetRegNumber(SU.Collection_Unit_Key) AS RegNo,
  CASE  
    WHEN SU.Life_Sciences = 0 THEN ISNULL(CT.Item_Name, ''No Determination'')
  ELSE
    ISNULL(dbo.ufn_GetFormattedTaxonNameByParams(
        ITN.Actual_Name, ITN.Actual_Name_Italic, ITN.Common_Name,
        ITN.Common_Name_Italic, ITN.Authority, 1), ''No Determination'')
  END AS Determination,
  CTType.PlainText AS Type,
  dbo.ufn_GetFieldCollectors(SU.Collection_Unit_Key) AS FieldCollector,
  dbo.ufn_GetSpecimenGatheringSite(SU.Collection_Unit_Key) AS GatheringSite,
  dbo.ufn_GetDateFromVagueDate(
      S.Vague_Date_Start, S.Vague_Date_End, S.Vague_Date_Type) AS GatheringDate,
  CU.Current_Location_Code AS Store
FROM  Specimen_Unit AS SU  
INNER JOIN Collection_Unit AS CU 
    ON CU.Collection_Unit_Key = SU.Collection_Unit_Key  
    AND ISNULL(CU.Current_Container_Collection_Unit_Key, '''')=LTRIM(RTRIM(''<#SectionKey>''))
INNER JOIN  VW_ConceptTerm AS CTType ON CTType.Concept_Key = SU.Specimen_Type_Concept_Key  
INNER JOIN Collection_Unit_Task CUT ON CUT.Collection_Unit_Key=CU.Collection_Unit_Key
INNER JOIN Conservation_Task CTK
    ON CTK.Conservation_Task_Key=CUT.Conservation_Task_Key
    AND CTK.Conservation_Job_Key=''<#ReportKey>''
LEFT JOIN  Determination D ON D.Determination_Key = SU.Preferred_Determination_Key
LEFT JOIN  Occurrence O ON O.Occurrence_Key=D.Determination_Key
LEFT JOIN  VW_ConceptTermPreferred CT ON CT.Concept_Key = D.Concept_Key  
LEFT JOIN  Taxon_Determination TD ON SU.Preferred_Taxon_Determination_Key = TD.Taxon_Determination_Key  
LEFT JOIN  Taxon_Occurrence XO ON XO.Taxon_Occurrence_Key=TD.Taxon_Determination_Key
LEFT JOIN  Index_Taxon_Name ITN ON ITN.Taxon_List_Item_Key = TD.Taxon_List_Item_Key  
LEFT JOIN  [Sample] S ON S.Sample_Key IN (XO.Sample_Key, O.Sample_Key)', 'SELECT COUNT(SU.Collection_Unit_Key) AS COUNT
FROM  Specimen_Unit AS SU  
INNER JOIN Collection_Unit AS CU 
    ON CU.Collection_Unit_Key = SU.Collection_Unit_Key  
    AND ISNULL(CU.Current_Container_Collection_Unit_Key, '''')=LTRIM(RTRIM(''<#SectionKey>''))
INNER JOIN Collection_Unit_Task CUT ON CUT.Collection_Unit_Key=CU.Collection_Unit_Key
INNER JOIN Conservation_Task CTK
    ON CTK.Conservation_Task_Key=CUT.Conservation_Task_Key
    AND CTK.Conservation_Job_Key=''<#ReportKey>''', 1, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00', 'Specimens');
INSERT INTO [Report_Block_In_Section] ([Report_Block_In_Section_Key], [Report_Block_Key], [Report_Section_Key], [Population_SQL], [Population_SQL_Record_Count], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian], [Title]) VALUES ('SYSTEM000000000O', 'SYSTEM0000000020', 'SYSTEM000000000O', '
SELECT
    dbo.ufn_RtfToPlainText(R.Title) AS Document,
				dbo.ufn_GetTranslation_String_From_Value(SJ.Original, ''YesNo'') AS Original,
				R.Reference_Type AS Type
FROM		Source_Join AS SJ
JOIN		Reference AS R ON SJ.Source_Key = R.Source_Key
JOIN		Source AS S ON SJ.Source_Key = S.Source_Key
WHERE		SJ.Table_Name = ''Conservation_Job''
AND			S.Internal = 1
AND			SJ.Record_Key = ''<#ReportKey>''', '
SELECT	Count(*) AS [Count]
FROM		Source_Join AS SJ
JOIN		Source AS S ON SJ.Source_Key = S.Source_Key
WHERE		SJ.Table_Name = ''Conservation_Job''
AND			S.Internal = 1
AND			SJ.Record_Key = ''<#ReportKey>''
	', 1, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00', 'Sources');
INSERT INTO [Report_Block_In_Section] ([Report_Block_In_Section_Key], [Report_Block_Key], [Report_Section_Key], [Population_SQL], [Population_SQL_Record_Count], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian], [Title]) VALUES ('SYSTEM000000000P', 'SYSTEM0000000021', 'SYSTEM000000000P', '
SELECT	SF.[File_Name] AS Item_Name
FROM		Source_Join AS SJ
JOIN		Source_File AS SF ON SF.Source_Key = SJ.Source_Key
JOIN		Source AS S ON S.Source_Key = SJ.Source_Key
WHERE		SJ.Record_Key = ''<#ReportKey>''
AND			S.Internal = 0
AND 		Table_Name = ''Conservation_Job''
	', '
SELECT	COUNT (*) AS [Count]
FROM		Source_Join AS SJ
JOIN		Source AS S ON S.Source_Key = SJ.Source_Key
WHERE		SJ.Record_Key = ''<#ReportKey>''
AND			S.Internal = 0
AND 		Table_Name = ''Conservation_Job''
	', 1, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00', 'External References');
INSERT INTO [Report_Block_In_Section] ([Report_Block_In_Section_Key], [Report_Block_Key], [Report_Section_Key], [Population_SQL], [Population_SQL_Record_Count], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian], [Title]) VALUES ('SYSTEM000000000Q', 'SYSTEM000000003J', 'SYSTEM000000000Q', '
SELECT 
  CUN.Number,
  S.Item_Name AS [Name],
  CASE
    WHEN SU.Collection_Unit_Key IS NULL THEN dbo.ufn_GetTranslation_String_From_Value(0, ''YESNO'')
    ELSE dbo.ufn_GetTranslation_String_From_Value(1, ''YESNO'') + '' - '' + CTC.Item_Name + '': '' + CUN.Number
  END AS Specimen,
  CU.Current_Location_Code AS StoreCode,
  S.Comment AS Comments
FROM Store S
INNER JOIN Collection_Unit CU 
    ON CU.Collection_Unit_Key=S.Collection_Unit_Key
LEFT JOIN Collection_Unit_Number CUN 
    ON CUN.Collection_Unit_Key=S.Collection_Unit_Key
    AND CUN.Preferred=1
    AND CUN.Type_Concept_Key=''SYSTEM0000000001''
LEFT JOIN VW_ConceptTermCommon CTC
    ON CTC.Concept_Key=CUN.Type_Concept_Key
LEFT JOIN Specimen_Unit SU
    ON SU.Collection_Unit_Key=S.Collection_Unit_Key
WHERE S.Collection_Unit_Key=''<#ReportKey>''', '
SELECT 1 AS [Count]', 1, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00', 'General');
INSERT INTO [Report_Block_In_Section] ([Report_Block_In_Section_Key], [Report_Block_Key], [Report_Section_Key], [Population_SQL], [Population_SQL_Record_Count], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian], [Title]) VALUES ('SYSTEM000000000R', 'SYSTEM0000000013', 'SYSTEM000000000R', '
SELECT
		CUD.Applies_To,
    CTM.Item_Name AS Method_Term,
    CUD.Duration,
    CUD.Accuracy,
    CTP.Item_Name AS Parameter_Term,
    CTU.Item_Name AS Unit_Term,
    CASE
     WHEN CUD.Upper_Value IS NULL THEN CUD.Lower_Value
     ELSE CUD.Lower_Value + '' - '' + CUD.Upper_Value
    END AS Value
  FROM   Collection_Unit_Data CUD
  INNER JOIN  vw_ConceptTerm CTP ON CTP.Concept_Key = CUD.Parameter_Concept_Key
  LEFT JOIN  vw_ConceptTerm CTM ON CTM.Concept_Key = CUD.Method_Concept_Key
  LEFT JOIN  vw_ConceptTerm CTU ON CTU.Concept_Key = CUD.Unit_Concept_Key
  LEFT JOIN  Collection_Unit CU ON CU.Collection_Unit_Key = CUD.Collection_Unit_Key
  WHERE   CUD.Collection_Unit_Key = ''<#ReportKey>''
  AND   CUD.Is_Descriptor = 0
	', '
SELECT COUNT(*) AS [Count]
FROM   Collection_Unit_Data
WHERE   Collection_Unit_Key = ''<#ReportKey>''
AND  Is_Descriptor = 0
	', 1, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00', 'Measurements');
INSERT INTO [Report_Block_In_Section] ([Report_Block_In_Section_Key], [Report_Block_Key], [Report_Section_Key], [Population_SQL], [Population_SQL_Record_Count], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian], [Title]) VALUES ('SYSTEM000000000S', 'SYSTEM0000000014', 'SYSTEM000000000S', '
SELECT
	 CUD.Collection_Unit_Data_Key AS Item_Key,
   CUD.Applies_To,
   CTM.Item_Name AS Method_Term,
   CUD.Duration,
   CUD.Accuracy,
   CTP.Item_Name AS Parameter_Term,
   CTU.Item_Name AS Unit_Term,
   CUD.Lower_Value AS Value
FROM Collection_Unit_Data CUD
INNER JOIN  vw_ConceptTerm CTP ON CTP.Concept_Key = CUD.Parameter_Concept_Key
LEFT JOIN  vw_ConceptTerm CTM ON CTM.Concept_Key = CUD.Method_Concept_Key
LEFT JOIN  vw_ConceptTerm CTU ON CTU.Concept_Key = CUD.Unit_Concept_Key
LEFT JOIN  Collection_Unit CU ON CU.Collection_Unit_Key = CUD.Collection_Unit_Key
WHERE   CUD.Collection_Unit_Key = ''<#ReportKey>''
AND   CUD.Is_Descriptor = 1
	', '
SELECT COUNT(*) AS [Count]
FROM   Collection_Unit_Data
WHERE   Collection_Unit_Key = ''<#ReportKey>''
AND  Is_Descriptor = 1
	', 1, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00', 'Descriptors');
INSERT INTO [Report_Block_In_Section] ([Report_Block_In_Section_Key], [Report_Block_Key], [Report_Section_Key], [Population_SQL], [Population_SQL_Record_Count], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian], [Title]) VALUES ('SYSTEM000000000T', 'SYSTEM0000000033', 'SYSTEM000000000T', '
DECLARE @Status VARCHAR(20)
DECLARE @SpCount VARCHAR(20) -- Specimens
DECLARE @StCount VARCHAR(20) -- Stores
DECLARE @CnCount VARCHAR(20) -- Collections


EXEC usp_Movement_Status_Get ''<#ReportKey>'', @Status OUTPUT
EXEC usp_Movement_MaterialSummary_Get ''<#ReportKey>'', 0, @CnCount OUTPUT
EXEC usp_Movement_MaterialSummary_Get ''<#ReportKey>'', 1, @SpCount OUTPUT
EXEC usp_Movement_MaterialSummary_Get ''<#ReportKey>'', 2, @StCount OUTPUT

SELECT
  M.Number,
  CASE M.Movement_Type 
    WHEN 0 THEN ''Accessioned From''
    WHEN 1 THEN ''Exchanged With''
    WHEN 2 THEN ''Lender''
    WHEN 3 THEN ''Borrower''
    WHEN 5 THEN ''Disposed To''
    WHEN 8 THEN ''Sold To''
    WHEN 9 THEN ''Owner''
  END AS OtherPartyCaption,
  dbo.ufn_GetFormattedName(M.Other_Party_Name_Key) + ISNULL('' ('' + O.Full_Name + '' - '' + ODSrc.Item_Name + '')'', '''') AS OtherParty,
  dbo.ufn_GetFormattedName(M.Staff_Responsible_Name_Key) AS StaffResponsible,
  OD.Item_Name AS Department,
  dbo.ufn_GetDateFromVagueDate(M.Exp_Vague_Date_Start, M.Exp_Vague_Date_End, M.Exp_Vague_Date_Type) AS CompletionDate,
  dbo.ufn_GetDateFromVagueDate(MOO.Vague_Date_Start, MOO.Vague_Date_End, MOO.Vague_Date_Type) AS AccessionDate,
  @Status AS Status,
  M.Notes,
  @CnCount + '' Collection'' + CASE @CnCount WHEN ''1'' THEN '', '' ELSE ''s, '' END +
      @SpCount + '' Specimen'' + CASE @SpCount WHEN ''1'' THEN '', '' ELSE ''s, '' END +
      @StCount + '' Store'' + CASE @StCount WHEN ''1'' THEN '''' ELSE ''s'' END 
  AS Summary,
  MOO.Notes AS NotesDetails
FROM Movement M
INNER JOIN Movement_Direction MD
  ON MD.Movement_Key=M.Movement_Key
  AND MD.Outbound=0
INNER JOIN Movement_Of_Ownership MOO 
  ON MOO.Movement_Direction_Key=MD.Movement_Direction_Key
LEFT JOIN Individual IResp 
  ON IResp.Name_Key=M.Staff_Responsible_Name_Key
LEFT JOIN Organisation_Department OD 
  ON OD.Organisation_Department_Key=IResp.Organisation_Department_Key
LEFT JOIN Individual ISrc 
  ON ISrc.Name_Key=M.Other_Party_Name_Key
LEFT JOIN Organisation_Department ODSrc
  ON ODSrc.Organisation_Department_Key=ISrc.Organisation_Department_Key
LEFT JOIN Organisation O ON O.Name_Key=ODSrc.Name_Key
WHERE M.Movement_Key=''<#ReportKey>''', 'SELECT 1 AS [Count]', 1, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00', 'General');
INSERT INTO [Report_Block_In_Section] ([Report_Block_In_Section_Key], [Report_Block_Key], [Report_Section_Key], [Population_SQL], [Population_SQL_Record_Count], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian], [Title]) VALUES ('SYSTEM000000000U', 'SYSTEM0000000034', 'SYSTEM000000000U', '
SELECT 
		dbo.ufn_GetDateFromVagueDate(MOM.Vague_Date_Start, MOM.Vague_Date_End, MOM.Vague_Date_Type) AS AcqDate,
		CASE 
					WHEN MOM.Contact_Name_Key IS NULL THEN ''''
					ELSE dbo.ufn_GetFormattedName(MOM.Contact_Name_Key) + '', ''
				END +
				ISNULL(dbo.ufn_GetFormattedName(MOM.Receiver_Name_Key),'''') +
				ISNULL('' - '' + ODSrc.Item_Name, '''') AS AcquiredFrom,
		CASE MOM.Completed
			WHEN 1 THEN ''Yes''
			ELSE ''No''
		END AS Completed,
		CT.Item_Name AS Method,
		ISNULL(CAST(TF.Data AS VARCHAR(10)), '''') + CONVERT(VARCHAR(20), Value_Amount) +
			CASE 
				WHEN TF.Data IS NULL THEN ISNULL('' '' + TCurr.Item_Name, '''')
				ELSE ''''
			END
		AS AmountPaid
FROM Movement_Of_Material MOM
INNER JOIN Movement_Direction MD 
		ON MD.Movement_Direction_Key=MOM.Movement_Direction_Key
		AND MD.Outbound=0
LEFT JOIN Individual ISrc 
	ON ISrc.Name_Key=MOM.Receiver_Name_Key
LEFT JOIN Organisation_Department ODSrc
	ON ODSrc.Organisation_Department_Key IN (ISrc.Organisation_Department_Key, MOM.Receiver_Organisation_Department_Key)
LEFT JOIN VW_ConceptTerm CT ON CT.Concept_Key=MOM.Acquisition_Method_Concept_Key
LEFT JOIN Concept CCurr ON CCurr.Concept_Key=MOM.Currency_Concept_Key
LEFT JOIN Term TCurr ON TCurr.Term_Key=CCurr.Term_Key
LEFT JOIN Thesaurus_Fact TF 
		ON TF.Concept_Key=CCurr.Concept_Key
		OR TF.Meaning_Key=CCurr.Meaning_Key
		AND TF.Fact_Type_Concept_Key=''SYSTEM000000009A''
WHERE MD.Movement_Key=''<#ReportKey>''
	', '
SELECT COUNT(MOM.Movement_Of_Material_Key) AS [Count]
FROM Movement_Of_Material MOM
INNER JOIN Movement_Direction MD 
		ON MD.Movement_Direction_Key=MOM.Movement_Direction_Key
		AND MD.Outbound=0
WHERE MD.Movement_Key=''<#ReportKey>''
	', 1, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00', 'Acquisition List');
INSERT INTO [Report_Block_In_Section] ([Report_Block_In_Section_Key], [Report_Block_Key], [Report_Section_Key], [Population_SQL], [Population_SQL_Record_Count], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian], [Title]) VALUES ('SYSTEM000000000V', 'SYSTEM0000000001', 'SYSTEM000000000N', 'SELECT 
  M.Number AS AccNo,
  C.Item_Name AS [Name],
  dbo.ufn_GetFormattedName(Assembler_Name_Key) AS AssembledBy,
  C.Topic,
  CASE 
    WHEN CN.Number IS NULL THEN S.Item_Name
  ELSE
    S.Item_Name + '' - '' + CN.Number
  END AS Store
FROM Collection C
INNER JOIN Collection_Unit CU 
    ON CU.Collection_Unit_Key=C.Collection_Unit_Key
    AND ISNULL(CU.Current_Container_Collection_Unit_Key, '''')=LTRIM(RTRIM(''<#SectionKey>''))
INNER JOIN Collection_Unit_Task CUT ON CUT.Collection_Unit_Key=CU.Collection_Unit_Key
INNER JOIN Conservation_Task CTK
    ON CTK.Conservation_Task_Key=CUT.Conservation_Task_Key
    AND CTK.Conservation_Job_Key=''<#ReportKey>''
LEFT JOIN Store S 
    ON S.Collection_Unit_Key=CU.Current_Container_Collection_Unit_Key
LEFT JOIN 
    (Collection_Unit_Number CN
    INNER JOIN Concept CPT 
        ON CPT.Concept_Key=CN.Type_Concept_Key
        AND CPT.Meaning_Key=''SYSTEM0000000001'')
    ON CN.Collection_Unit_Key=S.Collection_Unit_Key
    AND CN.Preferred=1
LEFT JOIN 
    (MOVEMENT_COLLECTION_UNIT MCU 
        INNER JOIN MOVEMENT_DIRECTION MD 
            ON MCU.Movement_Direction_Key = MD.Movement_Direction_Key 
            AND MD.Outbound = 0
        INNER JOIN MOVEMENT M 
            ON MD.Movement_Key = M.Movement_Key 
            AND (M.Movement_Type = 0 OR M.Movement_Type = 1)) 
    ON CU.Collection_Unit_Key = MCU.Collection_Unit_Key', 'SELECT COUNT(C.Collection_Unit_Key) AS Count
FROM  Collection AS C  
INNER JOIN Collection_Unit AS CU 
    ON CU.Collection_Unit_Key = C.Collection_Unit_Key  
    AND ISNULL(CU.Current_Container_Collection_Unit_Key, '''')=LTRIM(RTRIM(''<#SectionKey>''))
INNER JOIN Collection_Unit_Task CUT ON CUT.Collection_Unit_Key=CU.Collection_Unit_Key
INNER JOIN Conservation_Task CTK
    ON CTK.Conservation_Task_Key=CUT.Conservation_Task_Key
    AND CTK.Conservation_Job_Key=''<#ReportKey>''', 2, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00', 'Collections');
INSERT INTO [Report_Block_In_Section] ([Report_Block_In_Section_Key], [Report_Block_Key], [Report_Section_Key], [Population_SQL], [Population_SQL_Record_Count], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian], [Title]) VALUES ('SYSTEM000000000W', 'SYSTEM0000000010', 'SYSTEM000000000N', 'SELECT
  dbo.ufn_GetRegNumber(S.Collection_Unit_Key) AS RefNo,
  S.Item_Name AS [Name],
  CTS.PlainText AS Type,
  CU.Current_Location_Code AS Store
FROM Store AS S
INNER JOIN Collection_Unit AS CU 
    ON CU.Collection_Unit_Key = S.Collection_Unit_Key
    AND ISNULL(CU.Current_Container_Collection_Unit_Key, '''')=LTRIM(RTRIM(''<#SectionKey>''))
INNER JOIN VW_ConceptTerm AS CTS 
    ON CTS.Concept_Key = S.Store_Type_Concept_Key
INNER JOIN Collection_Unit_Task CUT ON CUT.Collection_Unit_Key=CU.Collection_Unit_Key
INNER JOIN Conservation_Task CTK
    ON CTK.Conservation_Task_Key=CUT.Conservation_Task_Key
    AND CTK.Conservation_Job_Key=''<#ReportKey>''', 'SELECT COUNT(S.Collection_Unit_Key) AS Count
FROM  Store AS S
INNER JOIN Collection_Unit AS CU 
    ON CU.Collection_Unit_Key = S.Collection_Unit_Key  
    AND ISNULL(CU.Current_Container_Collection_Unit_Key, '''')=LTRIM(RTRIM(''<#SectionKey>''))
INNER JOIN Collection_Unit_Task CUT ON CUT.Collection_Unit_Key=CU.Collection_Unit_Key
INNER JOIN Conservation_Task CTK
    ON CTK.Conservation_Task_Key=CUT.Conservation_Task_Key
    AND CTK.Conservation_Job_Key=''<#ReportKey>''', 3, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00', 'Stores');
INSERT INTO [Report_Block_In_Section] ([Report_Block_In_Section_Key], [Report_Block_Key], [Report_Section_Key], [Population_SQL], [Population_SQL_Record_Count], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian], [Title]) VALUES ('SYSTEM000000000X', 'SYSTEM0000000005', 'SYSTEM000000000V', 'SELECT 
  dbo.ufn_GetDateFromVagueDate(F.Vague_Date_Start, F.Vague_Date_End, F.Vague_Date_Type) AS Date,
  dbo.ufn_GetFormattedName(F.Funded_By_Name_Key) AS FundedBy,
  ISNULL(CAST(TF.Data AS VARCHAR(10)), '''') + CONVERT(VARCHAR(20), Amount) +
    CASE 
      WHEN TF.Data IS NULL THEN ISNULL('' '' + TCurr.Item_Name, '''')
    ELSE ''''
  END AS Amount,
  F.Details
FROM Movement_Funding F
LEFT JOIN Concept CCurr 
    ON CCurr.Concept_Key=F.Currency_Concept_Key
    AND CCurr.List_Preferred=1
LEFT JOIN Term TCurr ON TCurr.Term_Key=CCurr.Term_Key
LEFT JOIN Thesaurus_Fact TF 
    ON (TF.Concept_Key=CCurr.Concept_Key
    OR TF.Meaning_Key=CCurr.Meaning_Key)
    AND TF.Fact_Type_Concept_Key=''SYSTEM000000009A''
JOIN Session S ON S.Session_ID=''<#SessionID>''
JOIN [User] U ON U.Name_Key=S.User_Name_Key
WHERE F.Movement_Key=''<#ReportKey>''
AND U.Allow_Finance=1', 'SELECT COUNT(MF.Movement_Funding_Key) AS Count
FROM Movement_Funding MF
JOIN Session S ON S.Session_ID=''<#SessionID>''
JOIN [User] U ON U.Name_Key=S.User_Name_Key
WHERE MF.Movement_Key=''<#ReportKey>''
AND U.Allow_Finance=1', 1, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00', 'Funding List');
INSERT INTO [Report_Block_In_Section] ([Report_Block_In_Section_Key], [Report_Block_Key], [Report_Section_Key], [Population_SQL], [Population_SQL_Record_Count], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian], [Title]) VALUES ('SYSTEM000000000Y', 'SYSTEM0000000009', 'SYSTEM000000000W', 'SELECT
  SU.Collection_Unit_Key,
  CUN.Number AS RegNo,
  CASE 
    WHEN D.Determination_Key IS NULL THEN 
      CASE ITN.Preferred_Name_Italic
        WHEN 1 THEN ''<i>'' + ITN.Preferred_Name + ''</i>''
        ELSE ITN.Preferred_Name
      END
    ELSE DPref.Item_Name
  END AS Determination,
  CType.Item_Name AS Type,
  dbo.ufn_GetFieldCollectors(SU.Collection_Unit_Key) AS FieldCollector,
  LN.Item_Name AS GatheringSite,
  dbo.ufn_GetDateFromVagueDate(S.Vague_Date_Start, S.Vague_Date_End, S.Vague_Date_Type) AS GatheringDate,
  CU.Current_Location_Code AS Store
FROM Movement_Direction MD
INNER JOIN Movement_Collection_Unit MCU 
    ON MCU.Movement_Direction_Key=MD.Movement_Direction_Key
INNER JOIN Specimen_Unit SU 
    ON SU.Collection_Unit_Key=MCU.Collection_Unit_Key
LEFT JOIN Collection_Unit_Number CUN 
    ON CUN.Collection_Unit_Key=SU.Collection_Unit_Key
    AND CUN.Preferred=1
    AND CUN.Type_Concept_Key=''SYSTEM0000000001'' -- Registration Number
LEFT JOIN Determination D 
    ON D.Determination_Key=SU.Preferred_Determination_Key
LEFT JOIN VW_ConceptTermPreferred DPref 
    ON DPref.Concept_Key=D.Concept_Key
LEFT JOIN Taxon_Determination TD
    ON TD.Taxon_Determination_Key=SU.Preferred_Taxon_Determination_Key
LEFT JOIN Index_Taxon_Name ITN 
    ON ITN.Taxon_List_Item_Key=TD.Taxon_List_Item_Key
INNER JOIN VW_ConceptTerm CType 
    ON CType.Concept_Key=SU.Specimen_Type_Concept_Key
LEFT JOIN Specimen_Field_Data SFD 
    ON SFD.Collection_Unit_Key=SU.Collection_Unit_Key
    AND SFD.Gathering_Event=1
LEFT JOIN Taxon_Occurrence XO 
    ON XO.Taxon_Occurrence_Key=SFD.Taxon_Occurrence_Key
LEFT JOIN Occurrence O 
    ON O.Occurrence_Key=SFD.Occurrence_Key
LEFT JOIN [Sample] S 
    ON S.Sample_Key=O.Sample_Key 
    OR S.Sample_Key=XO.Sample_Key
LEFT JOIN Location_Name LN 
    ON LN.Location_Key=S.Location_Key
    AND LN.Preferred=1
INNER JOIN Collection_Unit CU 
    ON CU.Collection_Unit_Key=SU.Collection_Unit_Key
WHERE MD.Movement_Key=''<#ReportKey>''
    AND MD.Outbound=0', 'SELECT COUNT(DISTINCT SU.Collection_Unit_Key) As Count
FROM Movement_Direction MD
INNER JOIN Movement_Collection_Unit MCU 
    ON MCU.Movement_Direction_Key=MD.Movement_Direction_Key
INNER JOIN Specimen_Unit SU 
    ON SU.Collection_Unit_Key=MCU.Collection_Unit_Key
INNER JOIN VW_ConceptTerm CType 
    ON CType.Concept_Key=SU.Specimen_Type_Concept_Key
INNER JOIN Collection_Unit CU 
    ON CU.Collection_Unit_Key=SU.Collection_Unit_Key
WHERE MD.Movement_Key=''<#ReportKey>''
    AND MD.Outbound=0', 1, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00', 'Specimens Registered');
INSERT INTO [Report_Block_In_Section] ([Report_Block_In_Section_Key], [Report_Block_Key], [Report_Section_Key], [Population_SQL], [Population_SQL_Record_Count], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian], [Title]) VALUES ('SYSTEM000000000Z', 'SYSTEM0000000001', 'SYSTEM000000000X', 'SELECT DISTINCT
  C.Collection_Unit_Key,
  M.Number AS AccNo,
  C.Item_Name AS [Name],
  C.Topic,
  dbo.ufn_GetFormattedName(Assembler_Name_Key) AS AssembledBy,
  CU.Current_Location_Code AS Store,
  MCU2.Movement_Collection_Unit_Key
FROM Movement_Direction MD
INNER JOIN Movement_Collection_Unit MCU 
    ON MCU.Movement_Direction_Key=MD.Movement_Direction_Key
INNER JOIN Collection C ON C.Collection_Unit_Key=MCU.Collection_Unit_Key
INNER JOIN Collection_Unit CU
    ON CU.Collection_Unit_Key=C.Collection_Unit_Key
LEFT JOIN (Movement_Collection_Unit MCU2
        INNER JOIN  Movement_Direction MD2 
            ON MCU2.Movement_Direction_Key = MD2.Movement_Direction_Key
            AND MD2.Outbound = 0
        INNER JOIN  Movement M ON MD2.Movement_Key = M.Movement_Key
            AND (M.Movement_Type = 0 OR M.Movement_Type = 1))
    ON CU.Collection_Unit_Key = MCU2.Collection_Unit_Key
WHERE MD.Movement_Key=''<#ReportKey>''
    AND MD.Outbound=0', 'SELECT COUNT(C.Collection_Unit_Key) As Count
FROM Movement_Direction MD
INNER JOIN Movement_Collection_Unit MCU 
    ON MCU.Movement_Direction_Key=MD.Movement_Direction_Key
INNER JOIN Collection C ON C.Collection_Unit_Key=MCU.Collection_Unit_Key
INNER JOIN Collection_Unit CU 
    ON CU.Collection_Unit_Key=C.Collection_Unit_Key
WHERE MD.Movement_Key=''<#ReportKey>''
    AND MD.Outbound=0', 1, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00', 'Collections');
INSERT INTO [Report_Block_In_Section] ([Report_Block_In_Section_Key], [Report_Block_Key], [Report_Section_Key], [Population_SQL], [Population_SQL_Record_Count], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian], [Title]) VALUES ('SYSTEM0000000010', 'SYSTEM0000000010', 'SYSTEM000000000Y', 'SELECT
      dbo.ufn_GetRegNumber(S.Collection_Unit_Key) AS RefNo,
      S.Item_Name AS [Name],
      CTS.PlainText AS Type,
      CUS.Current_Location_Code AS Store
  FROM Movement_Direction MD
  INNER JOIN Movement_Collection_Unit MCU
       ON MCU.Movement_Direction_Key=MD.Movement_Direction_Key
  INNER JOIN Store S ON S.Collection_Unit_Key=MCU.Collection_Unit_Key
  INNER JOIN Collection_Unit AS CUS ON CUS.Collection_Unit_Key = S.Collection_Unit_Key
  INNER JOIN VW_ConceptTerm AS CTS ON CTS.Concept_Key = S.Store_Type_Concept_Key
  WHERE MD.Movement_Key=''<#ReportKey>''
      AND MD.Outbound=0
', 'SELECT COUNT (S.Collection_Unit_Key)
  FROM Movement_Direction MD
  INNER JOIN Movement_Collection_Unit MCU
       ON MCU.Movement_Direction_Key=MD.Movement_Direction_Key
  INNER JOIN Store S ON S.Collection_Unit_Key=MCU.Collection_Unit_Key
  INNER JOIN Collection_Unit AS CUS ON CUS.Collection_Unit_Key = S.Collection_Unit_Key
  INNER JOIN VW_ConceptTerm AS CTS ON CTS.Concept_Key = S.Store_Type_Concept_Key
  WHERE MD.Movement_Key=''<#ReportKey>''
      AND MD.Outbound=0
', 1, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00', 'Stores');
INSERT INTO [Report_Block_In_Section] ([Report_Block_In_Section_Key], [Report_Block_Key], [Report_Section_Key], [Population_SQL], [Population_SQL_Record_Count], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian], [Title]) VALUES ('SYSTEM0000000011', 'SYSTEM0000000011', 'SYSTEM000000000Z', 'SELECT 
    V.Ref_Number as RefNo,
    dbo.ufn_GetDateFromVagueDate(V.Vague_Date_Start, V.Vague_Date_End, V.Vague_Date_Type) as Date,
    CTType.Item_Name AS Type,
    dbo.ufn_GetFormattedName(V.Valued_By_Name_Key) AS ValuedBy,
    ISNULL(CAST(TF.Data AS VARCHAR(10)), '''') + CONVERT(VARCHAR(20), Value_Amount) +
      CASE 
        WHEN TF.Data IS NULL THEN ISNULL('' '' + TCurr.Item_Name, '''')
        ELSE ''''
      END
    AS Value,
    ISNULL (
      dbo.ufn_GetDateFromVagueDate(
      V.Valid_From_Vague_Date_Start, V.Valid_From_Vague_Date_End, V.Valid_From_Vague_Date_Type) + '' - '' +
      dbo.ufn_GetDateFromVagueDate(
      V.Valid_To_Vague_Date_Start, V.Valid_To_Vague_Date_End, V.Valid_To_Vague_Date_Type), 
      dbo.ufn_GetDateFromVagueDate(
      V.Valid_From_Vague_Date_Start, V.Valid_From_Vague_Date_End, V.Valid_From_Vague_Date_Type)
    ) AS [Valid]
FROM Valuation V
INNER JOIN Movement_Valuation MV
    ON MV.Valuation_Key=V.Valuation_Key
    AND MV.Movement_Key=''<#ReportKey>''
INNER JOIN VW_ConceptTerm CTType ON CTType.Concept_Key=V.Type_Concept_Key
LEFT JOIN Concept CCurr ON CCurr.Concept_Key=V.Currency_Concept_Key
LEFT JOIN Term TCurr ON TCurr.Term_Key=CCurr.Term_Key
LEFT JOIN Thesaurus_Fact TF 
    ON TF.Concept_Key=CCurr.Concept_Key
    OR TF.Meaning_Key=CCurr.Meaning_Key
    AND TF.Fact_Type_Concept_Key=''SYSTEM000000009A''
JOIN Session S ON S.Session_ID=''<#SessionID>''
JOIN [User] U ON U.Name_Key=S.User_Name_Key
WHERE U.Allow_Finance=1', 'SELECT COUNT(V.Valuation_Key) As Count
FROM Valuation V
INNER JOIN Movement_Valuation MV
    ON MV.Valuation_Key=V.Valuation_Key
    AND MV.Movement_Key=''<#ReportKey>''
INNER JOIN VW_ConceptTerm CTType ON CTType.Concept_Key=V.Type_Concept_Key
JOIN Session S ON S.Session_ID=''<#SessionID>''
JOIN [User] U ON U.Name_Key=S.User_Name_Key
WHERE U.Allow_Finance=1', 1, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00', 'Valuations');
INSERT INTO [Report_Block_In_Section] ([Report_Block_In_Section_Key], [Report_Block_Key], [Report_Section_Key], [Population_SQL], [Population_SQL_Record_Count], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian], [Title]) VALUES ('SYSTEM0000000012', 'SYSTEM0000000020', 'SYSTEM0000000010', 'SELECT 
    dbo.ufn_RtfToPlainText(R.Title) AS Document,
    dbo.ufn_GetTranslation_String_From_Value(SJ.Original, ''YesNo'') AS Original,
    R.Reference_Type AS Type
FROM Source_Join AS SJ
INNER JOIN Reference AS R
    ON R.Source_Key = SJ.Source_Key
INNER JOIN Source AS S
    ON S.Source_Key = SJ.Source_Key
    AND S.Internal = 1
WHERE SJ.Record_Key = ''<#ReportKey>''
  AND SJ.Table_Name = ''Movement''', 'SELECT Count(R.Source_Key) As Count
FROM Source_Join AS SJ
INNER JOIN Reference AS R
    ON R.Source_Key = SJ.Source_Key
INNER JOIN Source AS S
    ON S.Source_Key = SJ.Source_Key
    AND S.Internal = 1
WHERE SJ.Record_Key = ''<#ReportKey>''
  AND SJ.Table_Name = ''Movement''', 1, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00', 'Sources');
INSERT INTO [Report_Block_In_Section] ([Report_Block_In_Section_Key], [Report_Block_Key], [Report_Section_Key], [Population_SQL], [Population_SQL_Record_Count], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian], [Title]) VALUES ('SYSTEM0000000013', 'SYSTEM0000000021', 'SYSTEM0000000011', 'SELECT SF.[File_Name] AS Item_Name
FROM Source_Join AS SJ
JOIN Source_File AS SF 
    ON SF.Source_Key = SJ.Source_Key
JOIN Source AS S ON S.Source_Key = SJ.Source_Key
WHERE  SJ.Record_Key = ''<#ReportKey>''
    AND S.Internal = 0
    AND Table_Name = ''Movement''', 'SELECT COUNT(SF.Source_Key) As Count
FROM Source_Join AS SJ
JOIN Source_File AS SF 
    ON SF.Source_Key = SJ.Source_Key
JOIN Source AS S ON S.Source_Key = SJ.Source_Key
WHERE  SJ.Record_Key = ''<#ReportKey>''
    AND S.Internal = 0
    AND Table_Name = ''Movement''', 1, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00', 'External References');
INSERT INTO [Report_Block_In_Section] ([Report_Block_In_Section_Key], [Report_Block_Key], [Report_Section_Key], [Population_SQL], [Population_SQL_Record_Count], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian], [Title]) VALUES ('SYSTEM0000000014', 'SYSTEM0000000035', 'SYSTEM0000000012', 'DECLARE @DomainMask INT
SELECT @DomainMask=Domain_Mask
FROM Conservation_Check
WHERE Conservation_Check_Key=''<#ReportKey>''

DECLARE @Domains VARCHAR(1000)

EXEC usp_DomainsForMask_Get @DomainMask, @Domains OUTPUT

SELECT
    CC.Ref_Number AS RefNo,
    dbo.ufn_GetDateFromVagueDate(CC.Vague_Date_Start, CC.Vague_Date_End, CC.Vague_Date_Type) AS Date,
    dbo.ufn_GetFormattedName(CC.Checked_By_Name_Key) + '' - '' + CType.Item_Name AS CheckedByType,
    CCond.Item_Name AS Condition,
    CC.Details,
    @Domains AS Domains
FROM Conservation_Check CC
INNER JOIN VW_ConceptTerm CType 
    ON CType.Concept_Key=CC.Type_Concept_Key
INNER JOIN VW_ConceptTerm CCond 
    ON CCond.Concept_Key=CC.Condition_Concept_Key
WHERE Conservation_Check_Key=''<#ReportKey>''', 'SELECT 1 AS [Count]', 1, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00', 'General');
INSERT INTO [Report_Block_In_Section] ([Report_Block_In_Section_Key], [Report_Block_Key], [Report_Section_Key], [Population_SQL], [Population_SQL_Record_Count], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian], [Title]) VALUES ('SYSTEM0000000015', 'SYSTEM0000000036', 'SYSTEM0000000013', 'SELECT
      dbo.ufn_GetRegNumber(S.Collection_Unit_Key) AS RefNo,
      S.Item_Name AS [Name],
      CTS.PlainText AS Type,
      CUS.Current_Location_Code AS Store,
      CASE CC.Applies_To_Contained_Specimens
        WHEN 1 THEN ''<img src="<#StandardReportTemplatePath>ok.gif">''
        ELSE ''''
      END AS Contents
  FROM Conservation_Check CC
  INNER JOIN Collection_Unit_Check CUC
      ON CUC.Conservation_Check_Key=CC.Conservation_Check_Key
  INNER JOIN Store S
       ON S.Collection_Unit_Key=CUC.Collection_Unit_Key
  INNER JOIN Collection_Unit AS CUS
       ON CUS.Collection_Unit_Key = S.Collection_Unit_Key
  INNER JOIN VW_ConceptTerm AS CTS
       ON CTS.Concept_Key = S.Store_Type_Concept_Key
  WHERE CC.Conservation_Check_Key=''<#ReportKey>''', 'SELECT COUNT(S.Collection_Unit_Key)
  FROM Conservation_Check CC
  INNER JOIN Collection_Unit_Check CUC
      ON CUC.Conservation_Check_Key=CC.Conservation_Check_Key
  INNER JOIN Store S
       ON S.Collection_Unit_Key=CUC.Collection_Unit_Key
  INNER JOIN Collection_Unit AS CUS
       ON CUS.Collection_Unit_Key = S.Collection_Unit_Key
  INNER JOIN VW_ConceptTerm AS CTS
       ON CTS.Concept_Key = S.Store_Type_Concept_Key
  WHERE CC.Conservation_Check_Key=''<#ReportKey>''', 1, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00', 'Stores');
INSERT INTO [Report_Block_In_Section] ([Report_Block_In_Section_Key], [Report_Block_Key], [Report_Section_Key], [Population_SQL], [Population_SQL_Record_Count], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian], [Title]) VALUES ('SYSTEM0000000016', 'SYSTEM0000000037', 'SYSTEM0000000014', 'SELECT DISTINCT
    C.Collection_Unit_Key,
    M.Number AS AccNo,
    C.Item_Name AS [Name],
    C.Topic,
    dbo.ufn_GetFormattedName(Assembler_Name_Key) AS AssembledBy,
    CU.Current_Location_Code AS Store,
    CASE CC.Applies_To_Contained_Specimens
      WHEN 1 THEN ''<img src="<#StandardReportTemplatePath>ok.gif">''
      ELSE ''''
    END AS Contents
FROM Conservation_Check CC
INNER JOIN Collection_Unit_Check CUC
    ON CUC.Conservation_Check_Key=CC.Conservation_Check_Key
INNER JOIN Collection C ON C.Collection_Unit_Key=CUC.Collection_Unit_Key
INNER JOIN Collection_Unit CU
    ON CU.Collection_Unit_Key=C.Collection_Unit_Key
LEFT JOIN (Movement_Collection_Unit MCU2
        INNER JOIN  Movement_Direction MD2 
            ON MCU2.Movement_Direction_Key = MD2.Movement_Direction_Key
            AND MD2.Outbound = 0
        INNER JOIN  Movement M ON MD2.Movement_Key = M.Movement_Key
            AND (M.Movement_Type = 0 OR M.Movement_Type = 1))
    ON CU.Collection_Unit_Key = MCU2.Collection_Unit_Key
WHERE CC.Conservation_Check_Key=''<#ReportKey>''', 'SELECT COUNT(DISTINCT C.Collection_Unit_Key) As Count
FROM Conservation_Check CC
INNER JOIN Collection_Unit_Check CUC
    ON CUC.Conservation_Check_Key=CC.Conservation_Check_Key
INNER JOIN Collection C ON C.Collection_Unit_Key=CUC.Collection_Unit_Key
INNER JOIN Collection_Unit CU
    ON CU.Collection_Unit_Key=C.Collection_Unit_Key
WHERE CC.Conservation_Check_Key=''<#ReportKey>''', 1, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00', 'Collections');
INSERT INTO [Report_Block_In_Section] ([Report_Block_In_Section_Key], [Report_Block_Key], [Report_Section_Key], [Population_SQL], [Population_SQL_Record_Count], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian], [Title]) VALUES ('SYSTEM0000000017', 'SYSTEM0000000009', 'SYSTEM0000000015', 'SELECT DISTINCT
  SU.Collection_Unit_Key,
  CUN.Number AS RegNo,
  CASE 
    WHEN D.Determination_Key IS NULL THEN 
      CASE ITN.Preferred_Name_Italic
        WHEN 1 THEN ''<i>'' + ITN.Preferred_Name + ''</i>''
        ELSE ITN.Preferred_Name
      END
    ELSE DPref.Item_Name
  END AS Determination,
  CType.Item_Name AS Type,
  dbo.ufn_GetFieldCollectors(SU.Collection_Unit_Key) AS FieldCollector,
  LN.Item_Name AS GatheringSite,
  dbo.ufn_GetDateFromVagueDate(S.Vague_Date_Start, S.Vague_Date_End, S.Vague_Date_Type) AS GatheringDate,
  CU.Current_Location_Code AS Store
FROM Conservation_Check CC
INNER JOIN Collection_Unit_Check CUC
    ON CUC.Conservation_Check_Key=CC.Conservation_Check_Key
INNER JOIN Specimen_Unit SU 
    ON SU.Collection_Unit_Key=CUC.Collection_Unit_Key
LEFT JOIN Collection_Unit_Number CUN 
    ON CUN.Collection_Unit_Key=SU.Collection_Unit_Key
    AND CUN.Preferred=1
    AND CUN.Type_Concept_Key=''SYSTEM0000000001'' -- Registration Number
LEFT JOIN Determination D 
    ON D.Determination_Key=SU.Preferred_Determination_Key
LEFT JOIN VW_ConceptTermPreferred DPref 
    ON DPref.Concept_Key=D.Concept_Key
LEFT JOIN Taxon_Determination TD
    ON TD.Taxon_Determination_Key=SU.Preferred_Taxon_Determination_Key
LEFT JOIN Index_Taxon_Name ITN 
    ON ITN.Taxon_List_Item_Key=TD.Taxon_List_Item_Key
INNER JOIN VW_ConceptTerm CType 
    ON CType.Concept_Key=SU.Specimen_Type_Concept_Key
LEFT JOIN Specimen_Field_Data SFD 
    ON SFD.Collection_Unit_Key=SU.Collection_Unit_Key
    AND SFD.Gathering_Event=1
LEFT JOIN Taxon_Occurrence XO 
    ON XO.Taxon_Occurrence_Key=SFD.Taxon_Occurrence_Key
LEFT JOIN Occurrence O 
    ON O.Occurrence_Key=SFD.Occurrence_Key
LEFT JOIN [Sample] S 
    ON S.Sample_Key=O.Sample_Key 
    OR S.Sample_Key=XO.Sample_Key
LEFT JOIN Location_Name LN 
    ON LN.Location_Key=S.Location_Key
    AND LN.Preferred=1
INNER JOIN Collection_Unit CU 
    ON CU.Collection_Unit_Key=SU.Collection_Unit_Key
WHERE CC.Conservation_Check_Key=''<#ReportKey>''', 'SELECT COUNT(DISTINCT SU.Collection_Unit_Key) As Count
FROM Conservation_Check CC
INNER JOIN Collection_Unit_Check CUC
    ON CUC.Conservation_Check_Key=CC.Conservation_Check_Key
INNER JOIN Specimen_Unit SU 
    ON SU.Collection_Unit_Key=CUC.Collection_Unit_Key
INNER JOIN VW_ConceptTerm CType 
    ON CType.Concept_Key=SU.Specimen_Type_Concept_Key
INNER JOIN Collection_Unit CU 
    ON CU.Collection_Unit_Key=SU.Collection_Unit_Key
WHERE CC.Conservation_Check_Key=''<#ReportKey>''', 1, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00', 'Specimens');
INSERT INTO [Report_Block_In_Section] ([Report_Block_In_Section_Key], [Report_Block_Key], [Report_Section_Key], [Population_SQL], [Population_SQL_Record_Count], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian], [Title]) VALUES ('SYSTEM0000000018', 'SYSTEM0000000028', 'SYSTEM0000000016', 'SELECT 
    dbo.ufn_GetDateFromVagueDate(CT.Set_Vague_Date_Start, CT.Set_Vague_Date_End, CT.Set_Vague_Date_Type) AS DateSet,
    Task_Action AS Action,
    CType.Item_Name AS Type,
    CAST(Duration AS VARCHAR(20)) + ISNULL('' '' + CUnit.Item_Name, '''') AS Duration,
    dbo.ufn_GetConservationStatus(CT.Status) AS Status,
    dbo.ufn_GetFormattedName(CT.Identifier_Name_Key) AS IdentifiedBy,
    CASE CT.Priority
      WHEN 0 THEN ''Low''
      WHEN 1 THEN ''Medium''
      WHEN 2 THEN ''High''
      WHEN 3 THEN ''Urgent''
    END AS Priority
FROM Conservation_Task CT
INNER JOIN VW_ConceptTerm CType 
    ON CType.Concept_Key=CT.Type_Concept_Key
LEFT JOIN VW_ConceptTerm CUnit 
    ON CUnit.Concept_Key=CT.Duration_Unit_Concept_Key
WHERE CT.Conservation_Check_Key=''<#ReportKey>''', 'SELECT COUNT(CT.Conservation_Task_Key) As Count
FROM Conservation_Task CT
INNER JOIN VW_ConceptTerm CType 
    ON CType.Concept_Key=CT.Type_Concept_Key
WHERE CT.Conservation_Check_Key=''<#ReportKey>''', 1, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00', 'Tasks Identified');
INSERT INTO [Report_Block_In_Section] ([Report_Block_In_Section_Key], [Report_Block_Key], [Report_Section_Key], [Population_SQL], [Population_SQL_Record_Count], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian], [Title]) VALUES ('SYSTEM0000000019', 'SYSTEM0000000020', 'SYSTEM0000000017', 'SELECT 
    dbo.ufn_RtfToPlainText(R.Title) AS Document,
    dbo.ufn_GetTranslation_String_From_Value(SJ.Original, ''YesNo'') AS Original,
    R.Reference_Type AS Type
FROM Source_Join AS SJ
INNER JOIN Reference AS R
    ON R.Source_Key = SJ.Source_Key
INNER JOIN Source AS S
    ON S.Source_Key = SJ.Source_Key
    AND S.Internal = 1
WHERE SJ.Record_Key = ''<#ReportKey>''
  AND SJ.Table_Name = ''Conservation_Check''', 'SELECT COUNT(S.Source_Key) As Count
FROM Source_Join AS SJ
INNER JOIN Reference AS R
    ON R.Source_Key = SJ.Source_Key
INNER JOIN Source AS S
    ON S.Source_Key = SJ.Source_Key
    AND S.Internal = 1
WHERE SJ.Record_Key = ''<#ReportKey>''
  AND SJ.Table_Name = ''Conservation_Check''', 1, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00', 'Sources');
INSERT INTO [Report_Block_In_Section] ([Report_Block_In_Section_Key], [Report_Block_Key], [Report_Section_Key], [Population_SQL], [Population_SQL_Record_Count], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian], [Title]) VALUES ('SYSTEM000000001A', 'SYSTEM0000000021', 'SYSTEM0000000018', 'SELECT SF.[File_Name] AS Item_Name
FROM Source_Join AS SJ
JOIN Source_File AS SF 
    ON SF.Source_Key = SJ.Source_Key
JOIN Source AS S ON S.Source_Key = SJ.Source_Key
WHERE  SJ.Record_Key = ''<#ReportKey>''
    AND S.Internal = 0
    AND Table_Name = ''Conservation_Check''', 'SELECT COUNT(SF.Source_Key) As Count
FROM Source_Join AS SJ
JOIN Source_File AS SF 
    ON SF.Source_Key = SJ.Source_Key
JOIN Source AS S ON S.Source_Key = SJ.Source_Key
WHERE  SJ.Record_Key = ''<#ReportKey>''
    AND S.Internal = 0
    AND Table_Name = ''Conservation_Check''', 1, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00', 'External References');
INSERT INTO [Report_Block_In_Section] ([Report_Block_In_Section_Key], [Report_Block_Key], [Report_Section_Key], [Population_SQL], [Population_SQL_Record_Count], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian], [Title]) VALUES ('SYSTEM000000001B', 'SYSTEM0000000038', 'SYSTEM0000000019', 'DECLARE @Status VARCHAR(20)
DECLARE @SpCount VARCHAR(20) -- Specimens
DECLARE @StCount VARCHAR(20) -- Stores
DECLARE @CnCount VARCHAR(20) -- Collections

EXEC usp_Movement_Status_Get ''<#ReportKey>'', @Status OUTPUT
EXEC usp_Movement_MaterialSummary_Get ''<#ReportKey>'', 0, @CnCount OUTPUT
EXEC usp_Movement_MaterialSummary_Get ''<#ReportKey>'', 1, @SpCount OUTPUT
EXEC usp_Movement_MaterialSummary_Get ''<#ReportKey>'', 2, @StCount OUTPUT

--GENERAL grid
SELECT
  M.Number,
  CASE M.Movement_Type 
    WHEN 0 THEN ''Accessioned From''
    WHEN 1 THEN ''Exchanged With''
    WHEN 2 THEN ''Lender''
    WHEN 3 THEN ''Borrower''
    WHEN 4 THEN ''Destroyed By''
    WHEN 5 THEN ''Disposed To''
    WHEN 6 THEN ''Internally Transferred By''
    WHEN 7 THEN ''Lost By''
    WHEN 8 THEN ''Sold To''
    WHEN 9 THEN ''Owner''
  END AS Other_Party_Caption,
  dbo.ufn_GetFormattedName(M.Other_Party_Name_Key) + ISNULL('' ('' + O.Full_Name + '' - '' + ODSrc.Item_Name + '')'', '''') AS Other_Party,
  dbo.ufn_GetFormattedName(M.Staff_Responsible_Name_Key) AS Staff_Responsible,
  OD.Item_Name AS Department,
  dbo.ufn_GetDateFromVagueDate(M.Exp_Vague_Date_Start, M.Exp_Vague_Date_End, M.Exp_Vague_Date_Type) AS Completion_Date,
  @Status AS Status,
  M.Notes,
  @CnCount + '' Collection'' + CASE @CnCount WHEN ''1'' THEN '', '' ELSE ''s, '' END +
      @SpCount + '' Specimen'' + CASE @SpCount WHEN ''1'' THEN '', '' ELSE ''s, '' END +
      @StCount + '' Store'' + CASE @StCount WHEN ''1'' THEN '''' ELSE ''s'' END 
  AS Summary
FROM Movement M
LEFT JOIN Individual IResp 
  ON IResp.Name_Key=M.Staff_Responsible_Name_Key
LEFT JOIN Organisation_Department OD 
  ON OD.Organisation_Department_Key=IResp.Organisation_Department_Key
LEFT JOIN Individual ISrc 
  ON ISrc.Name_Key=M.Other_Party_Name_Key
LEFT JOIN Organisation_Department ODSrc
  ON ODSrc.Organisation_Department_Key=ISrc.Organisation_Department_Key
LEFT JOIN Organisation O ON O.Name_Key=ODSrc.Name_Key
WHERE M.Movement_Key=''<#ReportKey>''', 'SELECT 1 AS Count', 1, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00', 'General');
INSERT INTO [Report_Block_In_Section] ([Report_Block_In_Section_Key], [Report_Block_Key], [Report_Section_Key], [Population_SQL], [Population_SQL_Record_Count], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian], [Title]) VALUES ('SYSTEM000000001C', 'SYSTEM0000000009', 'SYSTEM000000001A', 'SELECT DISTINCT
  CUN.Number AS RegNo,
  CASE 
    WHEN D.Determination_Key IS NULL THEN 
      CASE ITN.Preferred_Name_Italic
        WHEN 1 THEN ''<i>'' + ITN.Preferred_Name + ''</i>''
        ELSE ITN.Preferred_Name
      END
    ELSE DPref.Item_Name
  END AS Determination,
  CType.Item_Name AS Type,
  dbo.ufn_GetFieldCollectors(SU.Collection_Unit_Key) AS FieldCollector,
  LN.Item_Name AS GatheringSite,
  dbo.ufn_GetDateFromVagueDate(S.Vague_Date_Start, S.Vague_Date_End, S.Vague_Date_Type) AS GatheringDate,
  SU.Collection_Unit_Key,
  CU.Current_Location_Code AS Store
FROM Movement_Direction MD
INNER JOIN Movement_Collection_Unit MCU 
    ON MCU.Movement_Direction_Key=MD.Movement_Direction_Key
INNER JOIN Specimen_Unit SU 
    ON SU.Collection_Unit_Key=MCU.Collection_Unit_Key
LEFT JOIN Collection_Unit_Number CUN 
    ON CUN.Collection_Unit_Key=SU.Collection_Unit_Key
    AND CUN.Preferred=1
    AND CUN.Type_Concept_Key=''SYSTEM0000000001'' -- Registration Number
LEFT JOIN Determination D 
    ON D.Determination_Key=SU.Preferred_Determination_Key
LEFT JOIN VW_ConceptTermPreferred DPref 
    ON DPref.Concept_Key=D.Concept_Key
LEFT JOIN Taxon_Determination TD
    ON TD.Taxon_Determination_Key=SU.Preferred_Taxon_Determination_Key
LEFT JOIN Index_Taxon_Name ITN 
    ON ITN.Taxon_List_Item_Key=TD.Taxon_List_Item_Key
INNER JOIN VW_ConceptTerm CType 
    ON CType.Concept_Key=SU.Specimen_Type_Concept_Key
LEFT JOIN Specimen_Field_Data SFD 
    ON SFD.Collection_Unit_Key=SU.Collection_Unit_Key
    AND SFD.Gathering_Event=1
LEFT JOIN Taxon_Occurrence XO 
    ON XO.Taxon_Occurrence_Key=SFD.Taxon_Occurrence_Key
LEFT JOIN Occurrence O 
    ON O.Occurrence_Key=SFD.Occurrence_Key
LEFT JOIN [Sample] S 
    ON S.Sample_Key=O.Sample_Key 
    OR S.Sample_Key=XO.Sample_Key
LEFT JOIN Location_Name LN 
    ON LN.Location_Key=S.Location_Key
    AND LN.Preferred=1
INNER JOIN Collection_Unit CU 
    ON CU.Collection_Unit_Key=SU.Collection_Unit_Key
WHERE MD.Movement_Key=''<#ReportKey>''', 'SELECT COUNT(DISTINCT
	SU.Collection_Unit_Key) As Count
FROM Movement_Direction MD
INNER JOIN Movement_Collection_Unit MCU 
		ON MCU.Movement_Direction_Key=MD.Movement_Direction_Key
INNER JOIN Specimen_Unit SU 
		ON SU.Collection_Unit_Key=MCU.Collection_Unit_Key
INNER JOIN VW_ConceptTerm CType 
		ON CType.Concept_Key=SU.Specimen_Type_Concept_Key
INNER JOIN Collection_Unit CU 
		ON CU.Collection_Unit_Key=SU.Collection_Unit_Key
WHERE MD.Movement_Key=''<#ReportKey>''', 1, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00', 'Specimens');
INSERT INTO [Report_Block_In_Section] ([Report_Block_In_Section_Key], [Report_Block_Key], [Report_Section_Key], [Population_SQL], [Population_SQL_Record_Count], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian], [Title]) VALUES ('SYSTEM000000001D', 'SYSTEM0000000001', 'SYSTEM000000001B', 'SELECT DISTINCT
  M.Number AS AccNo,
  C.Item_Name AS [Name],
  C.Topic,
dbo.ufn_GetFormattedName(Assembler_Name_Key) AS AssembledBy,
  CU.Current_Location_Code AS Store,
  C.Collection_Unit_Key,
  MCU2.Movement_Collection_Unit_Key
FROM Movement_Direction MD
INNER JOIN Movement_Collection_Unit MCU 
    ON MCU.Movement_Direction_Key=MD.Movement_Direction_Key
INNER JOIN Collection C ON C.Collection_Unit_Key=MCU.Collection_Unit_Key
INNER JOIN Collection_Unit CU
    ON CU.Collection_Unit_Key=C.Collection_Unit_Key
LEFT JOIN (Movement_Collection_Unit MCU2
        INNER JOIN  Movement_Direction MD2 
            ON MCU2.Movement_Direction_Key = MD2.Movement_Direction_Key
            AND MD2.Outbound = 0
        INNER JOIN  Movement M ON MD2.Movement_Key = M.Movement_Key
            AND (M.Movement_Type = 0 OR M.Movement_Type = 1))
    ON CU.Collection_Unit_Key = MCU2.Collection_Unit_Key
WHERE MD.Movement_Key=''<#ReportKey>''', 'SELECT Count(*) As Count FROM (
SELECT DISTINCT
  C.Collection_Unit_Key,
  MCU2.Movement_Collection_Unit_Key
FROM Movement_Direction MD
INNER JOIN Movement_Collection_Unit MCU 
    ON MCU.Movement_Direction_Key=MD.Movement_Direction_Key
INNER JOIN Collection C ON C.Collection_Unit_Key=MCU.Collection_Unit_Key
INNER JOIN Collection_Unit CU
    ON CU.Collection_Unit_Key=C.Collection_Unit_Key
LEFT JOIN (Movement_Collection_Unit MCU2
        INNER JOIN  Movement_Direction MD2 
            ON MCU2.Movement_Direction_Key = MD2.Movement_Direction_Key
            AND MD2.Outbound = 0
        INNER JOIN  Movement M ON MD2.Movement_Key = M.Movement_Key
            AND (M.Movement_Type = 0 OR M.Movement_Type = 1))
    ON CU.Collection_Unit_Key = MCU2.Collection_Unit_Key
WHERE MD.Movement_Key=''<#ReportKey>'') As TempTable', 1, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00', 'Collections');
INSERT INTO [Report_Block_In_Section] ([Report_Block_In_Section_Key], [Report_Block_Key], [Report_Section_Key], [Population_SQL], [Population_SQL_Record_Count], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian], [Title]) VALUES ('SYSTEM000000001E', 'SYSTEM0000000010', 'SYSTEM000000001C', 'SELECT
    dbo.ufn_GetRegNumber(S.Collection_Unit_Key) AS RefNo,
    S.Item_Name AS [Name],
    CTS.PlainText AS Type,
    CUS.Current_Location_Code AS Store
FROM Movement_Direction MD
INNER JOIN Movement_Collection_Unit MCU 
    ON MCU.Movement_Direction_Key=MD.Movement_Direction_Key
INNER JOIN Store S ON S.Collection_Unit_Key=MCU.Collection_Unit_Key
INNER JOIN Collection_Unit AS CUS ON CUS.Collection_Unit_Key = S.Collection_Unit_Key
INNER JOIN VW_ConceptTerm AS CTS ON CTS.Concept_Key = S.Store_Type_Concept_Key
WHERE MD.Movement_Key=''<#ReportKey>''', 'SELECT COUNT(*) As Count
FROM Movement_Direction MD
INNER JOIN Movement_Collection_Unit MCU 
    ON MCU.Movement_Direction_Key=MD.Movement_Direction_Key
INNER JOIN Store S ON S.Collection_Unit_Key=MCU.Collection_Unit_Key
INNER JOIN Collection_Unit AS CUS ON CUS.Collection_Unit_Key = S.Collection_Unit_Key
INNER JOIN VW_ConceptTerm AS CTS ON CTS.Concept_Key = S.Store_Type_Concept_Key
WHERE MD.Movement_Key=''<#ReportKey>''', 1, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00', 'Stores');
INSERT INTO [Report_Block_In_Section] ([Report_Block_In_Section_Key], [Report_Block_Key], [Report_Section_Key], [Population_SQL], [Population_SQL_Record_Count], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian], [Title]) VALUES ('SYSTEM000000001F', 'SYSTEM0000000020', 'SYSTEM000000001D', 'SELECT 
    dbo.ufn_RtfToPlainText(R.Title) AS Document,
    dbo.ufn_GetTranslation_String_From_Value(SJ.Original, ''YesNo'') AS Original,
    R.Reference_Type AS Type
FROM Source_Join AS SJ
INNER JOIN Reference AS R
    ON R.Source_Key = SJ.Source_Key
INNER JOIN Source AS S
    ON S.Source_Key = SJ.Source_Key
AND S.Internal = 1
WHERE SJ.Record_Key = ''<#ReportKey>''
  AND SJ.Table_Name = ''Movement''', 'SELECT COUNT(*) AS Count
FROM Source_Join AS SJ
INNER JOIN Reference AS R
    ON R.Source_Key = SJ.Source_Key
INNER JOIN Source AS S
    ON S.Source_Key = SJ.Source_Key
AND S.Internal = 1
WHERE SJ.Record_Key = ''<#ReportKey>''
  AND SJ.Table_Name = ''Movement''', 1, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00', 'Sources');
INSERT INTO [Report_Block_In_Section] ([Report_Block_In_Section_Key], [Report_Block_Key], [Report_Section_Key], [Population_SQL], [Population_SQL_Record_Count], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian], [Title]) VALUES ('SYSTEM000000001G', 'SYSTEM0000000021', 'SYSTEM000000001E', 'SELECT SF.[File_Name] AS Item_Name
FROM Source_Join AS SJ
JOIN Source_File AS SF 
    ON SF.Source_Key = SJ.Source_Key
JOIN Source AS S ON S.Source_Key = SJ.Source_Key
WHERE  SJ.Record_Key = ''<#ReportKey>''
    AND S.Internal = 0
    AND Table_Name = ''Movement''', 'SELECT Count(SF.[File_Name]) AS Count
FROM Source_Join AS SJ
JOIN Source_File AS SF 
    ON SF.Source_Key = SJ.Source_Key
JOIN Source AS S ON S.Source_Key = SJ.Source_Key
WHERE  SJ.Record_Key = ''<#ReportKey>''
    AND S.Internal = 0
    AND Table_Name = ''Movement''', 1, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00', 'External References');
INSERT INTO [Report_Block_In_Section] ([Report_Block_In_Section_Key], [Report_Block_Key], [Report_Section_Key], [Population_SQL], [Population_SQL_Record_Count], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian], [Title]) VALUES ('SYSTEM000000001H', 'SYSTEM0000000039', 'SYSTEM000000001F', 'SELECT 1 As Count', 'SELECT 1 As Count', 1, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00', 'Authorisation');
INSERT INTO [Report_Block_In_Section] ([Report_Block_In_Section_Key], [Report_Block_Key], [Report_Section_Key], [Population_SQL], [Population_SQL_Record_Count], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian], [Title]) VALUES ('SYSTEM000000001I', 'SYSTEM000000003A', 'SYSTEM000000001G', 'DECLARE @Status VARCHAR(20)
DECLARE @SpCount VARCHAR(20) -- Specimens
DECLARE @StCount VARCHAR(20) -- Stores
DECLARE @CnCount VARCHAR(20) -- Collections

EXEC usp_Movement_Status_Get ''<#ReportKey>'', @Status OUTPUT
EXEC usp_Movement_MaterialSummary_Get ''<#ReportKey>'', 0, @CnCount OUTPUT
EXEC usp_Movement_MaterialSummary_Get ''<#ReportKey>'', 1, @SpCount OUTPUT
EXEC usp_Movement_MaterialSummary_Get ''<#ReportKey>'', 2, @StCount OUTPUT

--General grid
SELECT
  M.Number,
  CASE M.Movement_Type 
    WHEN 0 THEN ''Accessioned From''
    WHEN 1 THEN ''Exchanged With''
    WHEN 2 THEN ''Lender''
    WHEN 3 THEN ''Borrower''
    WHEN 5 THEN ''Disposed To''
    WHEN 8 THEN ''Sold To''
    WHEN 9 THEN ''Owner''
  END AS Other_Party_Caption,
  dbo.ufn_GetFormattedName(M.Other_Party_Name_Key) + ISNULL('' ('' + O.Full_Name + '' - '' + ODSrc.Item_Name + '')'', '''') AS Other_Party,
  dbo.ufn_GetFormattedName(M.Staff_Responsible_Name_Key) AS Staff_Responsible,
  OD.Item_Name AS Department,
  dbo.ufn_GetDateFromVagueDate(M.Exp_Vague_Date_Start, M.Exp_Vague_Date_End, M.Exp_Vague_Date_Type) AS Completion_Date,
  @Status AS Status,
  M.Notes,
  @CnCount + '' Collection'' + CASE @CnCount WHEN ''1'' THEN '', '' ELSE ''s, '' END +
      @SpCount + '' Specimen'' + CASE @SpCount WHEN ''1'' THEN '', '' ELSE ''s, '' END +
      @StCount + '' Store'' + CASE @StCount WHEN ''1'' THEN '''' ELSE ''s'' END 
  AS Summary
FROM Movement M
LEFT JOIN Individual IResp 
  ON IResp.Name_Key=M.Staff_Responsible_Name_Key
LEFT JOIN Organisation_Department OD 
  ON OD.Organisation_Department_Key=IResp.Organisation_Department_Key
LEFT JOIN Individual ISrc 
  ON ISrc.Name_Key=M.Other_Party_Name_Key
LEFT JOIN Organisation_Department ODSrc
  ON ODSrc.Organisation_Department_Key=ISrc.Organisation_Department_Key
LEFT JOIN Organisation O ON O.Name_Key=ODSrc.Name_Key
WHERE M.Movement_Key=''<#ReportKey>''', 'SELECT 1 AS Count', 1, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00', 'Details');
INSERT INTO [Report_Block_In_Section] ([Report_Block_In_Section_Key], [Report_Block_Key], [Report_Section_Key], [Population_SQL], [Population_SQL_Record_Count], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian], [Title]) VALUES ('SYSTEM000000001J', 'SYSTEM000000003B', 'SYSTEM000000001H', 'SELECT DISTINCT
  SU.Collection_Unit_Key,
  CUN.Number AS RegNo,
  CASE 
    WHEN D.Determination_Key IS NULL THEN 
      CASE ITN.Preferred_Name_Italic
        WHEN 1 THEN ''<i>'' + ITN.Preferred_Name + ''</i>''
        ELSE ITN.Preferred_Name
      END
    ELSE DPref.Item_Name
  END AS Determination,
  CType.Item_Name AS Type,
  dbo.ufn_GetFieldCollectors(SU.Collection_Unit_Key) AS FieldCollector,
  LN.Item_Name AS GatheringSite,
  dbo.ufn_GetDateFromVagueDate(S.Vague_Date_Start, S.Vague_Date_End, S.Vague_Date_Type) AS GatheringDate,
  CU.Current_Location_Code AS Store
FROM Movement_Direction MD
INNER JOIN Movement_Collection_Unit MCU 
    ON MCU.Movement_Direction_Key=MD.Movement_Direction_Key
LEFT JOIN Collection C ON C.Collection_Unit_Key=MCU.Collection_Unit_Key
INNER JOIN Specimen_Unit SU 
    ON SU.Collection_Unit_Key=MCU.Collection_Unit_Key
    OR SU.Parent_Collection_Collection_Unit_Key=C.Collection_Unit_Key
LEFT JOIN Collection_Unit_Number CUN 
    ON CUN.Collection_Unit_Key=SU.Collection_Unit_Key
    AND CUN.Preferred=1
    AND CUN.Type_Concept_Key=''SYSTEM0000000001'' -- Registration Number
LEFT JOIN Determination D 
    ON D.Determination_Key=SU.Preferred_Determination_Key
LEFT JOIN VW_ConceptTermPreferred DPref 
    ON DPref.Concept_Key=D.Concept_Key
LEFT JOIN Taxon_Determination TD
    ON TD.Taxon_Determination_Key=SU.Preferred_Taxon_Determination_Key
LEFT JOIN Index_Taxon_Name ITN 
    ON ITN.Taxon_List_Item_Key=TD.Taxon_List_Item_Key
INNER JOIN VW_ConceptTerm CType 
    ON CType.Concept_Key=SU.Specimen_Type_Concept_Key
LEFT JOIN Specimen_Field_Data SFD 
    ON SFD.Collection_Unit_Key=SU.Collection_Unit_Key
    AND SFD.Gathering_Event=1
LEFT JOIN Taxon_Occurrence XO 
    ON XO.Taxon_Occurrence_Key=SFD.Taxon_Occurrence_Key
LEFT JOIN Occurrence O 
    ON O.Occurrence_Key=SFD.Occurrence_Key
LEFT JOIN [Sample] S 
    ON S.Sample_Key=O.Sample_Key 
    OR S.Sample_Key=XO.Sample_Key
LEFT JOIN Location_Name LN 
    ON LN.Location_Key=S.Location_Key
    AND LN.Preferred=1
INNER JOIN Collection_Unit CU 
    ON CU.Collection_Unit_Key=SU.Collection_Unit_Key
WHERE MD.Movement_Key=''<#ReportKey>''
    AND MD.Outbound=1', 'SELECT COUNT(DISTINCT SU.Collection_Unit_Key) AS Count 
FROM Movement_Direction MD
INNER JOIN Movement_Collection_Unit MCU 
    ON MCU.Movement_Direction_Key=MD.Movement_Direction_Key
LEFT JOIN Collection C ON C.Collection_Unit_Key=MCU.Collection_Unit_Key
INNER JOIN Specimen_Unit SU 
    ON SU.Collection_Unit_Key=MCU.Collection_Unit_Key
    OR SU.Parent_Collection_Collection_Unit_Key=C.Collection_Unit_Key
INNER JOIN VW_ConceptTerm CType 
    ON CType.Concept_Key=SU.Specimen_Type_Concept_Key
INNER JOIN Collection_Unit CU 
    ON CU.Collection_Unit_Key=SU.Collection_Unit_Key
WHERE MD.Movement_Key=''<#ReportKey>''
    AND MD.Outbound=1', 1, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00', 'Specimens');
INSERT INTO [Report_Block_In_Section] ([Report_Block_In_Section_Key], [Report_Block_Key], [Report_Section_Key], [Population_SQL], [Population_SQL_Record_Count], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian], [Title]) VALUES ('SYSTEM000000001K', 'SYSTEM000000003C', 'SYSTEM000000001H', 'SELECT DISTINCT
  C.Collection_Unit_Key,
  MCU2.Accession_Number AS AccNo,
  C.Item_Name AS [Name],
  C.Topic,
  dbo.ufn_GetFormattedName(Assembler_Name_Key) AS AssembledBy,
  CU.Current_Location_Code AS Store
FROM Movement_Direction MD
INNER JOIN Movement_Collection_Unit MCU 
    ON MCU.Movement_Direction_Key=MD.Movement_Direction_Key
INNER JOIN Collection C ON C.Collection_Unit_Key=MCU.Collection_Unit_Key
INNER JOIN Collection_Unit CU
    ON CU.Collection_Unit_Key=C.Collection_Unit_Key
LEFT JOIN vw_CollectionUnitAccessionNumber MCU2
    ON CU.Collection_Unit_Key = MCU2.Collection_Unit_Key
WHERE MD.Movement_Key=''<#ReportKey>''
    AND MD.Outbound=1', 'SELECT COUNT(DISTINCT C.Collection_Unit_Key) AS Count
FROM Movement_Direction MD
INNER JOIN Movement_Collection_Unit MCU 
    ON MCU.Movement_Direction_Key=MD.Movement_Direction_Key
INNER JOIN Collection C ON C.Collection_Unit_Key=MCU.Collection_Unit_Key
INNER JOIN Collection_Unit CU
    ON CU.Collection_Unit_Key=C.Collection_Unit_Key
WHERE MD.Movement_Key=''<#ReportKey>''
    AND MD.Outbound=1', 2, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00', 'Collections');
INSERT INTO [Report_Block_In_Section] ([Report_Block_In_Section_Key], [Report_Block_Key], [Report_Section_Key], [Population_SQL], [Population_SQL_Record_Count], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian], [Title]) VALUES ('SYSTEM000000001L', 'SYSTEM000000003D', 'SYSTEM000000001H', 'SELECT
    dbo.ufn_GetRegNumber(S.Collection_Unit_Key) AS RefNo,
    S.Item_Name AS [Name],
    CTS.PlainText AS Type,
    CUS.Current_Location_Code AS Store
FROM Movement_Direction MD
INNER JOIN Movement_Collection_Unit MCU 
    ON MCU.Movement_Direction_Key=MD.Movement_Direction_Key
INNER JOIN Store S ON S.Collection_Unit_Key=MCU.Collection_Unit_Key
INNER JOIN Collection_Unit AS CUS ON CUS.Collection_Unit_Key = S.Collection_Unit_Key
INNER JOIN VW_ConceptTerm AS CTS ON CTS.Concept_Key = S.Store_Type_Concept_Key
WHERE MD.Movement_Key=''<#ReportKey>''
    AND MD.Outbound=1', 'SELECT COUNT(S.Collection_Unit_Key) As Count
FROM Movement_Direction MD
INNER JOIN Movement_Collection_Unit MCU 
    ON MCU.Movement_Direction_Key=MD.Movement_Direction_Key
INNER JOIN Store S ON S.Collection_Unit_Key=MCU.Collection_Unit_Key
INNER JOIN Collection_Unit AS CUS ON CUS.Collection_Unit_Key = S.Collection_Unit_Key
INNER JOIN VW_ConceptTerm AS CTS ON CTS.Concept_Key = S.Store_Type_Concept_Key
WHERE MD.Movement_Key=''<#ReportKey>''
    AND MD.Outbound=1', 3, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00', 'Stores');
INSERT INTO [Report_Block_In_Section] ([Report_Block_In_Section_Key], [Report_Block_Key], [Report_Section_Key], [Population_SQL], [Population_SQL_Record_Count], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian], [Title]) VALUES ('SYSTEM000000001M', 'SYSTEM000000003B', 'SYSTEM000000001I', 'SELECT DISTINCT
  SU.Collection_Unit_Key,
  CUN.Number AS RegNo,
  CASE 
    WHEN D.Determination_Key IS NULL THEN 
      CASE ITN.Preferred_Name_Italic
        WHEN 1 THEN ''<i>'' + ITN.Preferred_Name + ''</i>''
        ELSE ITN.Preferred_Name
      END
    ELSE DPref.Item_Name
  END AS Determination,
  CType.Item_Name AS Type,
  dbo.ufn_GetFieldCollectors(SU.Collection_Unit_Key) AS FieldCollector,
  LN.Item_Name AS GatheringSite,
  dbo.ufn_GetDateFromVagueDate(S.Vague_Date_Start, S.Vague_Date_End, S.Vague_Date_Type) AS GatheringDate,
  CU.Current_Location_Code AS Store
FROM Movement_Direction MD
INNER JOIN Movement_Collection_Unit MCU 
    ON MCU.Movement_Direction_Key=MD.Movement_Direction_Key
LEFT JOIN Collection C ON C.Collection_Unit_Key=MCU.Collection_Unit_Key
INNER JOIN Specimen_Unit SU 
    ON SU.Collection_Unit_Key=MCU.Collection_Unit_Key
    OR SU.Parent_Collection_Collection_Unit_Key=C.Collection_Unit_Key
LEFT JOIN Collection_Unit_Number CUN 
    ON CUN.Collection_Unit_Key=SU.Collection_Unit_Key
    AND CUN.Preferred=1
    AND CUN.Type_Concept_Key=''SYSTEM0000000001'' -- Registration Number
LEFT JOIN Determination D 
    ON D.Determination_Key=SU.Preferred_Determination_Key
LEFT JOIN VW_ConceptTermPreferred DPref 
    ON DPref.Concept_Key=D.Concept_Key
LEFT JOIN Taxon_Determination TD
    ON TD.Taxon_Determination_Key=SU.Preferred_Taxon_Determination_Key
LEFT JOIN Index_Taxon_Name ITN 
    ON ITN.Taxon_List_Item_Key=TD.Taxon_List_Item_Key
INNER JOIN VW_ConceptTerm CType 
    ON CType.Concept_Key=SU.Specimen_Type_Concept_Key
LEFT JOIN Specimen_Field_Data SFD 
    ON SFD.Collection_Unit_Key=SU.Collection_Unit_Key
    AND SFD.Gathering_Event=1
LEFT JOIN Taxon_Occurrence XO 
    ON XO.Taxon_Occurrence_Key=SFD.Taxon_Occurrence_Key
LEFT JOIN Occurrence O 
    ON O.Occurrence_Key=SFD.Occurrence_Key
LEFT JOIN [Sample] S 
    ON S.Sample_Key=O.Sample_Key 
    OR S.Sample_Key=XO.Sample_Key
LEFT JOIN Location_Name LN 
    ON LN.Location_Key=S.Location_Key
    AND LN.Preferred=1
INNER JOIN Collection_Unit CU 
    ON CU.Collection_Unit_Key=SU.Collection_Unit_Key
WHERE MD.Movement_Key=''<#ReportKey>''
    AND MD.Outbound=0', 'SELECT COUNT(DISTINCT SU.Collection_Unit_Key) AS Count
FROM Movement_Direction MD
INNER JOIN Movement_Collection_Unit MCU 
    ON MCU.Movement_Direction_Key=MD.Movement_Direction_Key
LEFT JOIN Collection C ON C.Collection_Unit_Key=MCU.Collection_Unit_Key
INNER JOIN Specimen_Unit SU 
    ON SU.Collection_Unit_Key=MCU.Collection_Unit_Key
    OR SU.Parent_Collection_Collection_Unit_Key=C.Collection_Unit_Key
INNER JOIN VW_ConceptTerm CType 
    ON CType.Concept_Key=SU.Specimen_Type_Concept_Key
INNER JOIN Collection_Unit CU 
    ON CU.Collection_Unit_Key=SU.Collection_Unit_Key
WHERE MD.Movement_Key=''<#ReportKey>''
    AND MD.Outbound=0', 1, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00', 'Specimens');
INSERT INTO [Report_Block_In_Section] ([Report_Block_In_Section_Key], [Report_Block_Key], [Report_Section_Key], [Population_SQL], [Population_SQL_Record_Count], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian], [Title]) VALUES ('SYSTEM000000001N', 'SYSTEM000000003C', 'SYSTEM000000001I', 'SELECT DISTINCT
    C.Collection_Unit_Key,
    MCU2.Accession_Number AS AccNo,
    C.Item_Name AS [Name],
    C.Topic,
    dbo.ufn_GetFormattedName(Assembler_Name_Key) AS AssembledBy,
    CU.Current_Location_Code AS Store
FROM Movement_Direction MD
INNER JOIN Movement_Collection_Unit MCU 
    ON MCU.Movement_Direction_Key=MD.Movement_Direction_Key
INNER JOIN Collection C ON C.Collection_Unit_Key=MCU.Collection_Unit_Key
INNER JOIN Collection_Unit CU
    ON CU.Collection_Unit_Key=C.Collection_Unit_Key
LEFT JOIN vw_CollectionUnitAccessionNumber MCU2
    ON CU.Collection_Unit_Key = MCU2.Collection_Unit_Key
WHERE MD.Movement_Key=''<#ReportKey>''
    AND MD.Outbound=0', 'SELECT COUNT(*) As Count
FROM Movement_Direction MD
INNER JOIN Movement_Collection_Unit MCU 
    ON MCU.Movement_Direction_Key=MD.Movement_Direction_Key
INNER JOIN Collection C ON C.Collection_Unit_Key=MCU.Collection_Unit_Key
INNER JOIN Collection_Unit CU
    ON CU.Collection_Unit_Key=C.Collection_Unit_Key
WHERE MD.Movement_Key=''<#ReportKey>''
    AND MD.Outbound=0', 2, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00', 'Collections');
INSERT INTO [Report_Block_In_Section] ([Report_Block_In_Section_Key], [Report_Block_Key], [Report_Section_Key], [Population_SQL], [Population_SQL_Record_Count], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian], [Title]) VALUES ('SYSTEM000000001O', 'SYSTEM000000003D', 'SYSTEM000000001I', 'SELECT
    dbo.ufn_GetRegNumber(S.Collection_Unit_Key) AS RefNo,
    S.Item_Name AS [Name],
    CTS.PlainText AS Type,
    CUS.Current_Location_Code AS Store
FROM Movement_Direction MD
INNER JOIN Movement_Collection_Unit MCU 
    ON MCU.Movement_Direction_Key=MD.Movement_Direction_Key
INNER JOIN Store S ON S.Collection_Unit_Key=MCU.Collection_Unit_Key
INNER JOIN Collection_Unit AS CUS ON CUS.Collection_Unit_Key = S.Collection_Unit_Key
INNER JOIN VW_ConceptTerm AS CTS ON CTS.Concept_Key = S.Store_Type_Concept_Key
WHERE MD.Movement_Key=''<#ReportKey>''
    AND MD.Outbound=0', 'SELECT COUNT(*) AS Count
FROM Movement_Direction MD
INNER JOIN Movement_Collection_Unit MCU 
    ON MCU.Movement_Direction_Key=MD.Movement_Direction_Key
INNER JOIN Store S ON S.Collection_Unit_Key=MCU.Collection_Unit_Key
INNER JOIN Collection_Unit AS CUS ON CUS.Collection_Unit_Key = S.Collection_Unit_Key
INNER JOIN VW_ConceptTerm AS CTS ON CTS.Concept_Key = S.Store_Type_Concept_Key
WHERE MD.Movement_Key=''<#ReportKey>''
    AND MD.Outbound=0', 3, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00', 'Stores');
INSERT INTO [Report_Block_In_Section] ([Report_Block_In_Section_Key], [Report_Block_Key], [Report_Section_Key], [Population_SQL], [Population_SQL_Record_Count], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian], [Title]) VALUES ('SYSTEM000000001P', 'SYSTEM000000003E', 'SYSTEM000000001J', 'SELECT 
    V.Ref_Number AS RefNo,
    dbo.ufn_GetDateFromVagueDate(V.Vague_Date_Start, V.Vague_Date_End, V.Vague_Date_Type) as Date,
    CTType.Item_Name AS Type,
    dbo.ufn_GetFormattedName(V.Valued_By_Name_Key) AS ValuedBy,
    ISNULL(CAST(TF.Data AS VARCHAR(10)), '''') + CONVERT(VARCHAR(20), Value_Amount) +
      CASE 
        WHEN TF.Data IS NULL THEN ISNULL('' '' + TCurr.Item_Name, '''')
        ELSE ''''
        END
    AS Value,
    [Description]
FROM Valuation V
INNER JOIN Movement_Valuation MV
    ON MV.Valuation_Key=V.Valuation_Key
    AND MV.Movement_Key=''<#ReportKey>''
LEFT JOIN VW_ConceptTerm CTType ON CTType.Concept_Key=V.Type_Concept_Key
LEFT JOIN Concept CCurr ON CCurr.Concept_Key=V.Currency_Concept_Key
LEFT JOIN Term TCurr ON TCurr.Term_Key=CCurr.Term_Key
LEFT JOIN Thesaurus_Fact TF 
    ON TF.Concept_Key=CCurr.Concept_Key
    OR TF.Meaning_Key=CCurr.Meaning_Key
    AND TF.Fact_Type_Concept_Key=''SYSTEM000000009A''
INNER JOIN Session S ON S.Session_ID = ''<#SessionId>''
INNER JOIN [User] U ON S.User_Name_Key = U.Name_Key
WHERE U.Allow_Finance=1', 'SELECT COUNT(*) AS Count
FROM Valuation V
INNER JOIN Movement_Valuation MV
    ON MV.Valuation_Key=V.Valuation_Key
    AND MV.Movement_Key=''<#ReportKey>''
INNER JOIN Session S ON S.Session_ID = ''<#SessionId>''
INNER JOIN [User] U ON S.User_Name_Key = U.Name_Key
WHERE U.Allow_Finance=1', 1, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00', 'Valuations');
INSERT INTO [Report_Block_In_Section] ([Report_Block_In_Section_Key], [Report_Block_Key], [Report_Section_Key], [Population_SQL], [Population_SQL_Record_Count], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian], [Title]) VALUES ('SYSTEM000000001Q', 'SYSTEM0000000020', 'SYSTEM000000001K', 'SELECT 
    dbo.ufn_RtfToPlainText(R.Title) AS Document,
    dbo.ufn_GetTranslation_String_From_Value(SJ.Original, ''YesNo'') AS Original,
    R.Reference_Type AS Type
FROM Source_Join AS SJ
INNER JOIN Reference AS R
    ON R.Source_Key = SJ.Source_Key
INNER JOIN Source AS S
    ON S.Source_Key = SJ.Source_Key
    AND S.Internal = 1
WHERE SJ.Record_Key = ''<#ReportKey>''
    AND SJ.Table_Name = ''Movement''', 'SELECT COUNT(*) AS Count
FROM Source_Join AS SJ
INNER JOIN Reference AS R
    ON R.Source_Key = SJ.Source_Key
INNER JOIN Source AS S
    ON S.Source_Key = SJ.Source_Key
    AND S.Internal = 1
WHERE SJ.Record_Key = ''<#ReportKey>''
    AND SJ.Table_Name = ''Movement''', 1, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00', 'Sources');
INSERT INTO [Report_Block_In_Section] ([Report_Block_In_Section_Key], [Report_Block_Key], [Report_Section_Key], [Population_SQL], [Population_SQL_Record_Count], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian], [Title]) VALUES ('SYSTEM000000001R', 'SYSTEM0000000021', 'SYSTEM000000001L', 'SELECT SF.[File_Name] AS Item_Name
FROM Source_Join AS SJ
JOIN Source_File AS SF 
    ON SF.Source_Key = SJ.Source_Key
JOIN Source AS S ON S.Source_Key = SJ.Source_Key
WHERE  SJ.Record_Key = ''<#ReportKey>''
    AND S.Internal = 0
    AND Table_Name = ''Movement''', 'SELECT COUNT(*) AS Count
FROM Source_Join AS SJ
JOIN Source_File AS SF 
    ON SF.Source_Key = SJ.Source_Key
JOIN Source AS S ON S.Source_Key = SJ.Source_Key
WHERE  SJ.Record_Key = ''<#ReportKey>''
    AND S.Internal = 0
    AND Table_Name = ''Movement''', 1, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00', 'External References');
INSERT INTO [Report_Block_In_Section] ([Report_Block_In_Section_Key], [Report_Block_Key], [Report_Section_Key], [Population_SQL], [Population_SQL_Record_Count], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian], [Title]) VALUES ('SYSTEM000000001S', 'SYSTEM000000003F', 'SYSTEM000000001M', 'DECLARE @DomainMask INT
SELECT @DomainMask = Domain_Mask 
FROM Collection_Unit
WHERE Collection_Unit_Key=''<#ReportKey>''

DECLARE @Domains VARCHAR(1000)
EXEC usp_DomainsForMask_Get @DomainMask, @Domains OUTPUT

DECLARE @Status VARCHAR(100)
EXEC usp_CollectionUnitStatus_Get ''<#ReportKey>'', @Status OUTPUT

--Retrieve an internationalisable string to display as Unknown
DECLARE @Unknown VARCHAR(100)
SET @Unknown = dbo.ufn_GetTranslation_String_From_Value(0, ''GENERAL'')

--Get counts of contents for the material summary field
DECLARE 
@SpCount INT,
@StCount INT,
@CnCount INT

SELECT @SpCount=COUNT(Collection_Unit_Key)
FROM Specimen_Unit
WHERE Parent_Collection_Collection_Unit_Key=''<#ReportKey>''

SELECT @StCount=COUNT(DISTINCT CU.Current_Container_Collection_Unit_Key)
FROM Specimen_Unit SU
INNER JOIN Collection_Unit CU
    ON CU.Collection_Unit_Key=SU.Collection_Unit_Key
WHERE SU.Parent_Collection_Collection_Unit_Key=''<#ReportKey>'' AND
CU.Current_Container_Collection_Unit_Key IS NOT NULL

SELECT @CnCount=COUNT(Collection_Unit_Key)
FROM Collection
WHERE Parent_Collection_Collection_Unit_Key=''<#ReportKey>''

SELECT
    CUA.Accession_Number AS AccNo,
    C.Item_Name AS [Name],
    CAST(@CnCount AS VARCHAR(20)) + '' Collection'' + CASE @CnCount WHEN 1 THEN '', '' ELSE ''s, '' END +
        CAST(@SpCount AS VARCHAR(20)) + '' Specimen'' + CASE @SpCount WHEN 1 THEN '', '' ELSE ''s, '' END +
        CAST(@StCount AS VARCHAR(20)) + '' Store'' + CASE @StCount WHEN 1 THEN '''' ELSE ''s'' END 
    AS MaterialSummary,
    Topic,
    @Domains AS Domains,
    MGeo.[Text] AS GeoInfo,
    MDesc.[Text] AS [Description],
    ''Collation: '' + ISNULL(dbo.ufn_GetDateFromVagueDate(
        Collation_From_Vague_Date_Start, Collation_From_Vague_Date_End, Collation_From_Vague_Date_Type), @Unknown) + '' to '' + 
        ISNULL(dbo.ufn_GetDateFromVagueDate(
        Collation_To_Vague_Date_Start, Collation_To_Vague_Date_End, Collation_To_Vague_Date_Type), @Unknown) + CHAR(13) + CHAR(10) +
    ''Field Gathering: '' + ISNULL(dbo.ufn_GetDateFromVagueDate(
        Gather_From_Vague_Date_Start, Gather_From_Vague_Date_End, Gather_From_Vague_Date_Type), @Unknown) + '' to '' + 
        ISNULL(dbo.ufn_GetDateFromVagueDate(
        Gather_To_Vague_Date_Start, Gather_To_Vague_Date_End, Gather_To_Vague_Date_Type), @Unknown) + CHAR(13) + CHAR(10)+
    ''Historical or Geological: '' + 
        ISNULL(C.Historical_Period_From, @Unknown) + '' to '' + ISNULL(Historical_Period_To, @Unknown) AS TimePeriod,
    dbo.ufn_GetFormattedName(C.Assembler_Name_Key) AS AssembledBy,
    dbo.ufn_GetFormattedName(CUN.Name_Key) AS Owner,
    ISNULL(S.Item_Name + '' - '', '''') + CU.Current_Location_Code AS Holder,
    @Status AS Status
FROM Collection C
INNER JOIN Collection_Unit CU 
    ON CU.Collection_Unit_Key=C.Collection_Unit_Key
LEFT JOIN VW_CollectionUnitAccessionNumber CUA 
    ON CUA.Collection_Unit_Key=C.Collection_Unit_Key
LEFT JOIN Metadata MDesc
    ON MDesc.Record_Key=C.Collection_Unit_Key
    AND MDesc.Metadata_Type_Key=''SYSTEM0000000003''
LEFT JOIN Metadata MGeo
    ON MGeo.Record_Key=C.Collection_Unit_Key
    AND MGeo.Metadata_Type_Key=''SYSTEM0000000004''
LEFT JOIN Collection_Unit_Name CUN 
    ON CUN.Collection_Unit_Key=C.Collection_Unit_Key
    AND CUN.Relation_Type_Concept_Key=''SYSTEM00000000I7''
LEFT JOIN Store S 
    ON S.Collection_Unit_Key=CU.Current_Container_Collection_Unit_Key
WHERE C.Collection_Unit_Key=''<#ReportKey>''', 'SELECT 1 As Count', 1, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00', 'General');
INSERT INTO [Report_Block_In_Section] ([Report_Block_In_Section_Key], [Report_Block_Key], [Report_Section_Key], [Population_SQL], [Population_SQL_Record_Count], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian], [Title]) VALUES ('SYSTEM000000001T', 'SYSTEM000000003G', 'SYSTEM000000001N', 'SELECT DISTINCT
    C.Collection_Unit_Key,
    MCU2.Accession_Number AS AccNo,
    C.Item_Name AS [Name],
    C.Topic,
    dbo.ufn_GetFormattedName(Assembler_Name_Key) AS AssembledBy,
    CU.Current_Location_Code AS Store
FROM Collection C
INNER JOIN Collection_Unit CU
    ON CU.Collection_Unit_Key=C.Collection_Unit_Key
LEFT JOIN vw_CollectionUnitAccessionNumber MCU2
    ON CU.Collection_Unit_Key = MCU2.Collection_Unit_Key
WHERE C.Parent_Collection_Collection_Unit_Key=''<#ReportKey>''', 'SELECT COUNT(DISTINCT
    C.Collection_Unit_Key) As Count
FROM Collection C
INNER JOIN Collection_Unit CU
    ON CU.Collection_Unit_Key=C.Collection_Unit_Key
WHERE C.Parent_Collection_Collection_Unit_Key=''<#ReportKey>''', 1, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00', 'Includes Collections');
INSERT INTO [Report_Block_In_Section] ([Report_Block_In_Section_Key], [Report_Block_Key], [Report_Section_Key], [Population_SQL], [Population_SQL_Record_Count], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian], [Title]) VALUES ('SYSTEM000000001U', 'SYSTEM000000003H', 'SYSTEM000000001N', 'SELECT DISTINCT
    C.Collection_Unit_Key,
    MCU2.Accession_Number AS AccNo,
    C.Item_Name AS [Name],
    C.Topic,
    dbo.ufn_GetFormattedName(C.Assembler_Name_Key) AS AssembledBy,
    CU.Current_Location_Code AS Store
FROM Collection C
INNER JOIN Collection_Unit CU
    ON CU.Collection_Unit_Key=C.Collection_Unit_Key
LEFT JOIN vw_CollectionUnitAccessionNumber MCU2
    ON CU.Collection_Unit_Key = MCU2.Collection_Unit_Key
INNER JOIN Collection C2 ON C2.Parent_Collection_Collection_Unit_Key = C.Collection_Unit_key
WHERE C2.Collection_Unit_Key=''<#ReportKey>''', 'SELECT COUNT(DISTINCT
    C.Collection_Unit_Key) AS Count
FROM Collection C
INNER JOIN Collection_Unit CU
    ON CU.Collection_Unit_Key=C.Collection_Unit_Key
INNER JOIN Collection C2 ON C2.Parent_Collection_Collection_Unit_Key = C.Collection_Unit_key
WHERE C2.Collection_Unit_Key=''<#ReportKey>''', 2, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00', 'Included in Collections');
INSERT INTO [Report_Block_In_Section] ([Report_Block_In_Section_Key], [Report_Block_Key], [Report_Section_Key], [Population_SQL], [Population_SQL_Record_Count], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian], [Title]) VALUES ('SYSTEM000000001V', 'SYSTEM000000003I', 'SYSTEM000000001N', '-- Other collections
SELECT DISTINCT
    C.Collection_Unit_Key,
    MCU2.Accession_Number AS AccNo,
    C.Item_Name AS [Name],
    C.Topic,
    dbo.ufn_GetFormattedName(C.Assembler_Name_Key) AS AssembledBy,
    CU.Current_Location_Code AS Store
FROM Collection C
INNER JOIN Collection_Unit CU
    ON CU.Collection_Unit_Key=C.Collection_Unit_Key
INNER JOIN (
  Semantic_Relation SR 
  INNER JOIN Thesaurus_Relation_Type TRT
    ON TRT.Semantic_Relation_Key = SR.Semantic_Relation_Key
  INNER JOIN Collection_Unit_Relation CUR
    ON CUR.Thesaurus_Relation_Type_Key = TRT.Thesaurus_Relation_Type_Key
) 
  ON (
    (CUR.To_Collection_Unit_Key = C.Collection_Unit_Key
      AND CUR.From_Collection_Unit_Key = ''<#ReportKey>'') 
    OR
    (CUR.From_Collection_Unit_Key = C.Collection_Unit_Key 
      AND CUR.To_Collection_Unit_Key = ''<#ReportKey>''
      AND SR.Unidirectional = 0)
  )
LEFT JOIN vw_CollectionUnitAccessionNumber MCU2
    ON CU.Collection_Unit_Key = MCU2.Collection_Unit_Key', '-- Other collections
SELECT COUNT(DISTINCT
    C.Collection_Unit_Key) AS Count
FROM Collection C
INNER JOIN Collection_Unit CU
    ON CU.Collection_Unit_Key=C.Collection_Unit_Key
INNER JOIN (
  Semantic_Relation SR 
  INNER JOIN Thesaurus_Relation_Type TRT
    ON TRT.Semantic_Relation_Key = SR.Semantic_Relation_Key
  INNER JOIN Collection_Unit_Relation CUR
    ON CUR.Thesaurus_Relation_Type_Key = TRT.Thesaurus_Relation_Type_Key
) 
  ON (
    (CUR.To_Collection_Unit_Key = C.Collection_Unit_Key
      AND CUR.From_Collection_Unit_Key = ''<#ReportKey>'') 
    OR
    (CUR.From_Collection_Unit_Key = C.Collection_Unit_Key 
      AND CUR.To_Collection_Unit_Key = ''<#ReportKey>''
      AND SR.Unidirectional = 0)
  )', 3, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00', 'Other Collections');
INSERT INTO [Report_Block_In_Section] ([Report_Block_In_Section_Key], [Report_Block_Key], [Report_Section_Key], [Population_SQL], [Population_SQL_Record_Count], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian], [Title]) VALUES ('SYSTEM000000001W', 'SYSTEM0000000010', 'SYSTEM000000001O', '-- Specimen Stores
SELECT DISTINCT
    dbo.ufn_GetRegNumber(S.Collection_Unit_Key) AS RefNo,
    S.Item_Name AS [Name],
    CTS.PlainText AS Type,
    CUS.Current_Location_Code AS Store
FROM Specimen_Unit U
INNER JOIN Collection_Unit CU ON CU.Collection_Unit_Key=U.Collection_Unit_Key
INNER JOIN Store S ON S.Collection_Unit_Key=CU.Current_Container_Collection_Unit_Key
INNER JOIN Collection_Unit AS CUS ON CUS.Collection_Unit_Key = S.Collection_Unit_Key
INNER JOIN VW_ConceptTerm AS CTS ON CTS.Concept_Key = S.Store_Type_Concept_Key
WHERE U.Parent_Collection_Collection_Unit_Key=''<#ReportKey>''', '-- Specimen Stores
SELECT COUNT(DISTINCT S.Collection_Unit_Key) As Count
FROM Specimen_Unit U
INNER JOIN Collection_Unit CU ON CU.Collection_Unit_Key=U.Collection_Unit_Key
INNER JOIN Store S ON S.Collection_Unit_Key=CU.Current_Container_Collection_Unit_Key
INNER JOIN Collection_Unit AS CUS ON CUS.Collection_Unit_Key = S.Collection_Unit_Key
INNER JOIN VW_ConceptTerm AS CTS ON CTS.Concept_Key = S.Store_Type_Concept_Key
WHERE U.Parent_Collection_Collection_Unit_Key=''<#ReportKey>''', 1, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00', 'Specimen Stores');
INSERT INTO [Report_Block_In_Section] ([Report_Block_In_Section_Key], [Report_Block_Key], [Report_Section_Key], [Population_SQL], [Population_SQL_Record_Count], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian], [Title]) VALUES ('SYSTEM000000001X', 'SYSTEM0000000009', 'SYSTEM000000001P', 'SELECT DISTINCT CUN.Number AS RegNo,   CASE WHEN D.Determination_Key IS NULL THEN     CASE ITN.Preferred_Name_Italic WHEN 1 THEN ''<i>'' + ITN.Preferred_Name + ''</i>'' ELSE ITN.Preferred_Name END   ELSE DPref.Item_Name END AS Determination,   CType.Item_Name AS Type, dbo.ufn_GetFieldCollectors(SU.Collection_Unit_Key) AS FieldCollector,   LN.Item_Name AS GatheringSite, dbo.ufn_GetDateFromVagueDate(S.Vague_Date_Start, S.Vague_Date_End, S.Vague_Date_Type) AS GatheringDate,   SU.Collection_Unit_Key, CU.Current_Location_Code AS Store FROM Specimen_Unit SU LEFT JOIN Collection_Unit_Number CUN ON CUN.Collection_Unit_Key=SU.Collection_Unit_Key AND CUN.Preferred=1     AND CUN.Type_Concept_Key=''SYSTEM0000000001'' /* Registration Number */ LEFT JOIN Determination D ON D.Determination_Key=SU.Preferred_Determination_Key LEFT JOIN VW_ConceptTermPreferred DPref ON DPref.Concept_Key=D.Concept_Key LEFT JOIN Taxon_Determination TD ON TD.Taxon_Determination_Key=SU.Preferred_Taxon_Determination_Key LEFT JOIN Index_Taxon_Name ITN ON ITN.Taxon_List_Item_Key=TD.Taxon_List_Item_Key INNER JOIN VW_ConceptTerm CType ON CType.Concept_Key=SU.Specimen_Type_Concept_Key LEFT JOIN Specimen_Field_Data SFD ON SFD.Collection_Unit_Key=SU.Collection_Unit_Key AND SFD.Gathering_Event=1 LEFT JOIN Taxon_Occurrence XO ON XO.Taxon_Occurrence_Key=SFD.Taxon_Occurrence_Key LEFT JOIN Occurrence O ON O.Occurrence_Key=SFD.Occurrence_Key LEFT JOIN [Sample] S ON S.Sample_Key=O.Sample_Key OR S.Sample_Key=XO.Sample_Key LEFT JOIN Location_Name LN ON LN.Location_Key=S.Location_Key AND LN.Preferred=1 INNER JOIN Collection_Unit CU ON CU.Collection_Unit_Key=SU.Collection_Unit_Key WHERE SU.Parent_Collection_Collection_Unit_Key=''<#ReportKey>''', '-- Specimens
SELECT COUNT(DISTINCT
  SU.Collection_Unit_Key) AS Count
FROM Specimen_Unit SU 
INNER JOIN VW_ConceptTerm CType 
    ON CType.Concept_Key=SU.Specimen_Type_Concept_Key
INNER JOIN Collection_Unit CU 
    ON CU.Collection_Unit_Key=SU.Collection_Unit_Key
WHERE SU.Parent_Collection_Collection_Unit_Key=''<#ReportKey>''', 1, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00', 'Specimens');
INSERT INTO [Report_Block_In_Section] ([Report_Block_In_Section_Key], [Report_Block_Key], [Report_Section_Key], [Population_SQL], [Population_SQL_Record_Count], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian], [Title]) VALUES ('SYSTEM000000001Y', 'SYSTEM0000000013', 'SYSTEM000000001Q', '-- Measurements
SELECT
    CUD.Applies_To,
    CTM.Item_Name AS Method_Term,
    CUD.Duration,
    CUD.Accuracy,
    CTP.Item_Name AS Parameter_Term,
    CTU.Item_Name AS Unit_Term,
    CASE
     WHEN CUD.Upper_Value IS NULL THEN CUD.Lower_Value
     ELSE CUD.Lower_Value + '' - '' + CUD.Upper_Value
    END AS Value
  FROM   Collection_Unit_Data CUD
  INNER JOIN  vw_ConceptTerm CTP ON CTP.Concept_Key = CUD.Parameter_Concept_Key
  LEFT JOIN  vw_ConceptTerm CTM ON CTM.Concept_Key = CUD.Method_Concept_Key
  LEFT JOIN  vw_ConceptTerm CTU ON CTU.Concept_Key = CUD.Unit_Concept_Key
  LEFT JOIN  Collection_Unit CU ON CU.Collection_Unit_Key = CUD.Collection_Unit_Key
  WHERE   CUD.Collection_Unit_Key = ''<#ReportKey>''
  AND   CUD.Is_Descriptor = 0', 'SELECT COUNT(DISTINCT CUD.Collection_Unit_Data_Key) AS Count
  FROM   Collection_Unit_Data CUD
  INNER JOIN  vw_ConceptTerm CTP ON CTP.Concept_Key = CUD.Parameter_Concept_Key
  WHERE   CUD.Collection_Unit_Key = ''<#ReportKey>''
  AND   CUD.Is_Descriptor = 0', 1, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00', 'Measurements');
INSERT INTO [Report_Block_In_Section] ([Report_Block_In_Section_Key], [Report_Block_Key], [Report_Section_Key], [Population_SQL], [Population_SQL_Record_Count], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian], [Title]) VALUES ('SYSTEM000000001Z', 'SYSTEM0000000014', 'SYSTEM000000001R', '-- Descriptors
SELECT
   CUD.Collection_Unit_Data_Key AS Item_Key,
   CUD.Applies_To,
   CTM.Item_Name AS Method_Term,
   CUD.Duration,
   CUD.Accuracy,
   CTP.Item_Name AS Parameter_Term,
   CTU.Item_Name AS Unit_Term,
   CUD.Lower_Value AS Value
FROM Collection_Unit_Data CUD
INNER JOIN  vw_ConceptTerm CTP ON CTP.Concept_Key = CUD.Parameter_Concept_Key
LEFT JOIN  vw_ConceptTerm CTM ON CTM.Concept_Key = CUD.Method_Concept_Key
LEFT JOIN  vw_ConceptTerm CTU ON CTU.Concept_Key = CUD.Unit_Concept_Key
LEFT JOIN  Collection_Unit CU ON CU.Collection_Unit_Key = CUD.Collection_Unit_Key
WHERE   CUD.Collection_Unit_Key = ''<#ReportKey>''
AND   CUD.Is_Descriptor = 1', '-- Descriptors
SELECT COUNT(DISTINCT CUD.Collection_Unit_Data_Key) AS Count
FROM Collection_Unit_Data CUD
INNER JOIN  vw_ConceptTerm CTP ON CTP.Concept_Key = CUD.Parameter_Concept_Key
WHERE   CUD.Collection_Unit_Key = ''<#ReportKey>''
AND   CUD.Is_Descriptor = 1', 1, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00', 'Descriptors');
INSERT INTO [Report_Block_In_Section] ([Report_Block_In_Section_Key], [Report_Block_Key], [Report_Section_Key], [Population_SQL], [Population_SQL_Record_Count], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian], [Title]) VALUES ('SYSTEM0000000020', 'SYSTEM0000000020', 'SYSTEM000000001S', '-- Sources
SELECT 
    dbo.ufn_RtfToPlaintext(R.Title) AS Document,
    dbo.ufn_GetTranslation_String_From_Value(SJ.Original, ''YesNo'') AS Original,
    R.Reference_Type AS Type
FROM Source_Join AS SJ
INNER JOIN Reference AS R
    ON R.Source_Key = SJ.Source_Key
INNER JOIN Source AS S
    ON S.Source_Key = SJ.Source_Key
    AND S.Internal = 1
WHERE SJ.Record_Key = ''<#ReportKey>''
    AND SJ.Table_Name = ''Collection''', '-- Sources
SELECT COUNT(*) As Count FROM (
SELECT 
    R.Title AS Document,
    dbo.ufn_GetTranslation_String_From_Value(SJ.Original, ''YesNo'') AS Original,
    R.Reference_Type AS Type
FROM Source_Join AS SJ
INNER JOIN Reference AS R
    ON R.Source_Key = SJ.Source_Key
INNER JOIN Source AS S
    ON S.Source_Key = SJ.Source_Key
    AND S.Internal = 1
WHERE SJ.Record_Key = ''<#ReportKey>''
    AND SJ.Table_Name = ''Collection''
) AS TempTable', 1, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00', 'Sources');
INSERT INTO [Report_Block_In_Section] ([Report_Block_In_Section_Key], [Report_Block_Key], [Report_Section_Key], [Population_SQL], [Population_SQL_Record_Count], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian], [Title]) VALUES ('SYSTEM0000000021', 'SYSTEM0000000021', 'SYSTEM000000001T', 'SELECT SF.[File_Name] AS Item_Name
FROM Source_Join AS SJ
JOIN Source_File AS SF 
    ON SF.Source_Key = SJ.Source_Key
JOIN Source AS S ON S.Source_Key = SJ.Source_Key
WHERE  SJ.Record_Key = ''<#ReportKey>''
    AND S.Internal = 0
    AND Table_Name = ''Collection''', 'SELECT COUNT(SF.[File_Name]) AS Count
FROM Source_Join AS SJ
JOIN Source_File AS SF 
    ON SF.Source_Key = SJ.Source_Key
JOIN Source AS S ON S.Source_Key = SJ.Source_Key
WHERE  SJ.Record_Key = ''<#ReportKey>''
    AND S.Internal = 0
    AND Table_Name = ''Collection''', 1, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00', 'External References');
INSERT INTO [Report_Block_In_Section] ([Report_Block_In_Section_Key], [Report_Block_Key], [Report_Section_Key], [Population_SQL], [Population_SQL_Record_Count], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian], [Title]) VALUES ('SYSTEM0000000022', 'SYSTEM000000003K', 'SYSTEM000000001U', '-- Locality
SET NOCOUNT ON
DECLARE @ResultsTable TABLE (
  OrderValue INTEGER NOT NULL,
  RefNo VARCHAR(30) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
  Type VARCHAR(150) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
  Store VARCHAR(30) COLLATE SQL_Latin1_General_CP1_CI_AS NULL
)

DECLARE @Current_Key CHAR(16)
DECLARE @Count INTEGER
SELECT @Count = 0

SELECT @Current_Key = ''<#ReportKey>''

WHILE (@Current_Key IS NOT NULL AND @Current_Key != '''')
BEGIN
  INSERT INTO @ResultsTable (OrderValue, RefNo, Type, Store)
  SELECT
    @Count AS OrderValue,
    dbo.ufn_GetRegNumber(S.Collection_Unit_Key) AS RefNo,
    CTS.PlainText AS Type,
    CUS.Current_Location_Code AS Store
  FROM Collection_Unit CU
  INNER JOIN Store S ON S.Collection_Unit_Key=CU.Current_Container_Collection_Unit_Key
  INNER JOIN Collection_Unit AS CUS ON CUS.Collection_Unit_Key = S.Collection_Unit_Key
  INNER JOIN VW_ConceptTerm AS CTS ON CTS.Concept_Key = S.Store_Type_Concept_Key
  WHERE CU.Collection_Unit_Key=@Current_Key

  SELECT @Current_Key = CU.Current_Container_Collection_Unit_Key,
    @Count = @Count + 1
  FROM Collection_Unit CU
  WHERE CU.Collection_Unit_Key=@Current_Key
END

SET NOCOUNT OFF

SELECT RefNo, Type, Store FROM @ResultsTable', '-- Locality
SET NOCOUNT ON
DECLARE @ResultsTable TABLE (
  OrderValue INTEGER NOT NULL,
  RefNo VARCHAR(30) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
  Type VARCHAR(150) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
  Store VARCHAR(30) COLLATE SQL_Latin1_General_CP1_CI_AS NULL
)

DECLARE @Current_Key CHAR(16)
DECLARE @Count INTEGER
SELECT @Count = 0

SELECT @Current_Key = ''<#ReportKey>''
WHILE (@Current_Key IS NOT NULL AND @Current_Key != '''')
BEGIN
  INSERT INTO @ResultsTable (OrderValue, RefNo, Type, Store)
  SELECT
    @Count AS OrderValue,
    dbo.ufn_GetRegNumber(S.Collection_Unit_Key) AS RefNo,
    CTS.PlainText AS Type,
    CUS.Current_Location_Code AS Store
  FROM Collection_Unit CU
  INNER JOIN Store S ON S.Collection_Unit_Key=CU.Current_Container_Collection_Unit_Key
  INNER JOIN Collection_Unit AS CUS ON CUS.Collection_Unit_Key = S.Collection_Unit_Key
  INNER JOIN VW_ConceptTerm AS CTS ON CTS.Concept_Key = S.Store_Type_Concept_Key
  WHERE CU.Collection_Unit_Key=@Current_Key

  SELECT @Current_Key = CU.Current_Container_Collection_Unit_Key,
    @Count = @Count + 1
  FROM Collection_Unit CU
  WHERE CU.Collection_Unit_Key=@Current_Key
END
SET NOCOUNT OFF
SELECT COUNT(*) AS Count FROM @ResultsTable', 1, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00', 'Locality');
INSERT INTO [Report_Block_In_Section] ([Report_Block_In_Section_Key], [Report_Block_Key], [Report_Section_Key], [Population_SQL], [Population_SQL_Record_Count], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian], [Title]) VALUES ('SYSTEM0000000023', 'SYSTEM0000000009', 'SYSTEM000000001V', '-- Specimens
SET NOCOUNT ON
DECLARE @ResultsTable TABLE (
  OrderValue INTEGER NOT NULL,
  Collection_Unit_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS  NOT NULL,
  Current_Container_Collection_Unit_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
  Done TINYINT DEFAULT 0
)

DECLARE @Current_Key CHAR(16)
DECLARE @Parent_Key CHAR(16)
DECLARE @Count INTEGER
DECLARE @Done INTEGER
SELECT @Count = 1


INSERT INTO @ResultsTable (OrderValue, Collection_Unit_Key, Current_Container_Collection_Unit_Key, Done)
VALUES (0, ''<#ReportKey>'', NULL, 0)


SELECT @Current_Key = ''<#ReportKey>'', @Parent_Key = NULL

WHILE EXISTS (SELECT * FROM @ResultsTable WHERE Done = 0)
BEGIN
  -- INSERT the children of the current node into the results - if they are not there
  INSERT INTO @ResultsTable (OrderValue, Collection_Unit_Key, Current_Container_Collection_Unit_Key, Done)
  SELECT TOP 1 0, CU.Collection_Unit_Key, CU.Current_Container_Collection_Unit_Key, 0
  FROM Collection_Unit CU
  INNER JOIN Store S ON S.Collection_Unit_Key = CU.Collection_Unit_Key 
  WHERE CU.Current_Container_Collection_Unit_Key = @Current_Key AND CU.Collection_Unit_Key NOT IN (
    SELECT Collection_Unit_Key FROM @ResultsTable
  )
  IF (@@ROWCOUNT > 0)
  BEGIN
  -- If you can :
  --   Go Down 
    SELECT @Parent_Key = @Current_Key, @Current_Key = Collection_Unit_Key
    FROM @ResultsTable WHERE Done = 0 AND Current_Container_Collection_Unit_Key = @Current_Key
    UPDATE @ResultsTable SET OrderValue = @Count WHERE Collection_Unit_Key = @Current_Key
    SELECT @Count = @Count + 1
  END
  ELSE
  BEGIN
  -- If you can''t (this node is done) :
    UPDATE @ResultsTable SET Done = 1 WHERE Collection_Unit_Key = @Current_Key
    SELECT @done = 1
    WHILE (@done = 1 AND @Parent_Key IS NOT NULL)
      SELECT @Current_Key = @Parent_Key, @Parent_Key = Current_Container_Collection_Unit_Key, @done = Done
      FROM @ResultsTable WHERE Collection_Unit_Key = @Parent_Key
  END
END

SET NOCOUNT OFF

SELECT DISTINCT
  R.OrderValue, 
  CUN.Number AS RegNo,
  CASE 
    WHEN D.Determination_Key IS NULL THEN 
      CASE ITN.Preferred_Name_Italic
        WHEN 1 THEN ''<i>'' + ITN.Preferred_Name + ''</i>''
        ELSE ITN.Preferred_Name
      END
    ELSE DPref.Item_Name
  END AS Determination,
  CType.Item_Name AS Type,
  dbo.ufn_GetFieldCollectors(SU.Collection_Unit_Key) AS FieldCollector,
  LN.Item_Name AS GatheringSite,
  dbo.ufn_GetDateFromVagueDate(S.Vague_Date_Start, S.Vague_Date_End, S.Vague_Date_Type) AS GatheringDate,
  SU.Collection_Unit_Key,
  CU.Current_Location_Code AS Store
FROM Specimen_Unit SU 
INNER JOIN Collection_Unit CU ON CU.Collection_Unit_Key = SU.Collection_Unit_Key
INNER JOIN @ResultsTable R ON R.Collection_Unit_Key = CU.Current_Container_Collection_Unit_Key
LEFT JOIN Collection_Unit_Number CUN 
    ON CUN.Collection_Unit_Key=SU.Collection_Unit_Key
    AND CUN.Preferred=1
    AND CUN.Type_Concept_Key=''SYSTEM0000000001'' -- Registration Number
LEFT JOIN Determination D 
    ON D.Determination_Key=SU.Preferred_Determination_Key
LEFT JOIN VW_ConceptTermPreferred DPref 
    ON DPref.Concept_Key=D.Concept_Key
LEFT JOIN Taxon_Determination TD
    ON TD.Taxon_Determination_Key=SU.Preferred_Taxon_Determination_Key
LEFT JOIN Index_Taxon_Name ITN 
    ON ITN.Taxon_List_Item_Key=TD.Taxon_List_Item_Key
INNER JOIN VW_ConceptTerm CType 
    ON CType.Concept_Key=SU.Specimen_Type_Concept_Key
LEFT JOIN Specimen_Field_Data SFD 
    ON SFD.Collection_Unit_Key=SU.Collection_Unit_Key
    AND SFD.Gathering_Event=1
LEFT JOIN Taxon_Occurrence XO 
    ON XO.Taxon_Occurrence_Key=SFD.Taxon_Occurrence_Key
LEFT JOIN Occurrence O 
    ON O.Occurrence_Key=SFD.Occurrence_Key
LEFT JOIN [Sample] S 
    ON S.Sample_Key=O.Sample_Key 
    OR S.Sample_Key=XO.Sample_Key
LEFT JOIN Location_Name LN 
    ON LN.Location_Key=S.Location_Key
    AND LN.Preferred=1', '-- Specimens
SET NOCOUNT ON
DECLARE @ResultsTable TABLE (
  OrderValue INTEGER NOT NULL,
  Collection_Unit_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS  NOT NULL,
  Current_Container_Collection_Unit_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
  Done TINYINT DEFAULT 0
)

DECLARE @Current_Key CHAR(16)
DECLARE @Parent_Key CHAR(16)
DECLARE @Count INTEGER
DECLARE @Done INTEGER
SELECT @Count = 1


INSERT INTO @ResultsTable (OrderValue, Collection_Unit_Key, Current_Container_Collection_Unit_Key, Done)
VALUES (0, ''<#ReportKey>'', NULL, 0)


SELECT @Current_Key = ''<#ReportKey>'', @Parent_Key = NULL

WHILE EXISTS (SELECT * FROM @ResultsTable WHERE Done = 0)
BEGIN
  INSERT INTO @ResultsTable (OrderValue, Collection_Unit_Key, Current_Container_Collection_Unit_Key, Done)
  SELECT TOP 1 0, CU.Collection_Unit_Key, CU.Current_Container_Collection_Unit_Key, 0
  FROM Collection_Unit CU
  INNER JOIN Store S ON S.Collection_Unit_Key = CU.Collection_Unit_Key 
  WHERE CU.Current_Container_Collection_Unit_Key = @Current_Key AND CU.Collection_Unit_Key NOT IN (
    SELECT Collection_Unit_Key FROM @ResultsTable
  )

  IF (@@ROWCOUNT > 0)
  BEGIN
  -- If you can :
  --   Go Down 
    SELECT @Parent_Key = @Current_Key, @Current_Key = Collection_Unit_Key
    FROM @ResultsTable WHERE Done = 0 AND Current_Container_Collection_Unit_Key = @Current_Key
    UPDATE @ResultsTable SET OrderValue = @Count WHERE Collection_Unit_Key = @Current_Key
    SELECT @Count = @Count + 1
  END
  ELSE
  BEGIN
  -- If you can''t (this node is done) :
    UPDATE @ResultsTable SET Done = 1 WHERE Collection_Unit_Key = @Current_Key
    SELECT @done = 1
    WHILE (@done = 1 AND @Parent_Key IS NOT NULL)
      SELECT @Current_Key = @Parent_Key, @Parent_Key = Current_Container_Collection_Unit_Key, @done = Done
      FROM @ResultsTable WHERE Collection_Unit_Key = @Parent_Key
  END
END

SELECT COUNT(*) AS Count FROM (
SELECT DISTINCT
  R.OrderValue, 
  CUN.Number AS RegNo,
  CASE 
    WHEN D.Determination_Key IS NULL THEN 
      CASE ITN.Preferred_Name_Italic
        WHEN 1 THEN ''<i>'' + ITN.Preferred_Name + ''</i>''
        ELSE ITN.Preferred_Name
      END
    ELSE DPref.Item_Name
  END AS Determination,
  CType.Item_Name AS Type,
  dbo.ufn_GetFieldCollectors(SU.Collection_Unit_Key) AS FieldCollector,
  LN.Item_Name AS GatheringSite,
  dbo.ufn_GetDateFromVagueDate(S.Vague_Date_Start, S.Vague_Date_End, S.Vague_Date_Type) AS GatheringDate,
  SU.Collection_Unit_Key,
  CU.Current_Location_Code AS Store
FROM Specimen_Unit SU 
INNER JOIN Collection_Unit CU ON CU.Collection_Unit_Key = SU.Collection_Unit_Key
INNER JOIN @ResultsTable R ON R.Collection_Unit_Key = CU.Current_Container_Collection_Unit_Key
LEFT JOIN Collection_Unit_Number CUN 
    ON CUN.Collection_Unit_Key=SU.Collection_Unit_Key
    AND CUN.Preferred=1
    AND CUN.Type_Concept_Key=''SYSTEM0000000001'' -- Registration Number
LEFT JOIN Determination D 
    ON D.Determination_Key=SU.Preferred_Determination_Key
LEFT JOIN VW_ConceptTermPreferred DPref 
    ON DPref.Concept_Key=D.Concept_Key
LEFT JOIN Taxon_Determination TD
    ON TD.Taxon_Determination_Key=SU.Preferred_Taxon_Determination_Key
LEFT JOIN Index_Taxon_Name ITN 
    ON ITN.Taxon_List_Item_Key=TD.Taxon_List_Item_Key
INNER JOIN VW_ConceptTerm CType 
    ON CType.Concept_Key=SU.Specimen_Type_Concept_Key
LEFT JOIN Specimen_Field_Data SFD 
    ON SFD.Collection_Unit_Key=SU.Collection_Unit_Key
    AND SFD.Gathering_Event=1
LEFT JOIN Taxon_Occurrence XO 
    ON XO.Taxon_Occurrence_Key=SFD.Taxon_Occurrence_Key
LEFT JOIN Occurrence O 
    ON O.Occurrence_Key=SFD.Occurrence_Key
LEFT JOIN [Sample] S 
    ON S.Sample_Key=O.Sample_Key 
    OR S.Sample_Key=XO.Sample_Key
LEFT JOIN Location_Name LN 
    ON LN.Location_Key=S.Location_Key
    AND LN.Preferred=1
) AS TempTable
SET NOCOUNT OFF', 1, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00', 'Specimens');
INSERT INTO [Report_Block_In_Section] ([Report_Block_In_Section_Key], [Report_Block_Key], [Report_Section_Key], [Population_SQL], [Population_SQL_Record_Count], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian], [Title]) VALUES ('SYSTEM0000000024', 'SYSTEM0000000001', 'SYSTEM000000001W', '-- Collections
SELECT DISTINCT
  MCU2.Accession_Number AS AccNo,
  C.Item_Name AS [Name],
  C.Topic,
  dbo.ufn_GetFormattedName(C.Assembler_Name_Key) AS AssembledBy,
  CU.Current_Location_Code AS Store
FROM Collection_Unit CU 
INNER JOIN Collection C ON C.Collection_Unit_Key = CU.Collection_Unit_Key
LEFT JOIN vw_CollectionUnitAccessionNumber MCU2
    ON C.Collection_Unit_Key = MCU2.Collection_Unit_Key
WHERE CU.Current_Container_Collection_Unit_Key = ''<#ReportKey>''', '-- Collections
SELECT COUNT(DISTINCT C.Collection_Unit_Key) AS Count
FROM Collection_Unit CU 
INNER JOIN Collection C ON C.Collection_Unit_Key = CU.Collection_Unit_Key
WHERE CU.Current_Container_Collection_Unit_Key = ''<#ReportKey>''', 1, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00', 'Collections');
INSERT INTO [Report_Block_In_Section] ([Report_Block_In_Section_Key], [Report_Block_Key], [Report_Section_Key], [Population_SQL], [Population_SQL_Record_Count], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian], [Title]) VALUES ('SYSTEM0000000025', 'SYSTEM0000000010', 'SYSTEM000000001X', '-- Stores
SELECT DISTINCT
    dbo.ufn_GetRegNumber(S.Collection_Unit_Key) AS RefNo,
    S.Item_Name AS [Name],
    CTS.PlainText AS Type,
    CUS.Current_Location_Code AS Store
FROM Store S
INNER JOIN Collection_Unit AS CUS ON CUS.Collection_Unit_Key = S.Collection_Unit_Key
INNER JOIN VW_ConceptTerm AS CTS ON CTS.Concept_Key = S.Store_Type_Concept_Key
WHERE CUS.Current_Container_Collection_Unit_Key=''<#ReportKey>''', '-- Stores

SELECT COUNT(*) AS Count
FROM Store S
INNER JOIN Collection_Unit AS CUS ON CUS.Collection_Unit_Key = S.Collection_Unit_Key
INNER JOIN VW_ConceptTerm AS CTS ON CTS.Concept_Key = S.Store_Type_Concept_Key
WHERE CUS.Current_Container_Collection_Unit_Key=''<#ReportKey>''', 1, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00', 'Includes Stores');
INSERT INTO [Report_Block_In_Section] ([Report_Block_In_Section_Key], [Report_Block_Key], [Report_Section_Key], [Population_SQL], [Population_SQL_Record_Count], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian], [Title]) VALUES ('SYSTEM0000000026', 'SYSTEM0000000020', 'SYSTEM000000001Y', '-- Sources
SELECT 
    dbo.ufn_RtfToPlainText(R.Title) AS Document,
    dbo.ufn_GetTranslation_String_From_Value(SJ.Original, ''YesNo'') AS Original,
    R.Reference_Type AS Type
FROM Source_Join AS SJ
INNER JOIN Reference AS R
    ON R.Source_Key = SJ.Source_Key
INNER JOIN Source AS S
    ON S.Source_Key = SJ.Source_Key
    AND S.Internal = 1
WHERE SJ.Record_Key = ''<#ReportKey>''
    AND SJ.Table_Name = ''Store''', '-- Sources
SELECT Count(*) AS Count FROM (
SELECT 
    R.Title AS Document,
    dbo.ufn_GetTranslation_String_From_Value(SJ.Original, ''YesNo'') AS Original,
    R.Reference_Type AS Type
FROM Source_Join AS SJ
INNER JOIN Reference AS R
    ON R.Source_Key = SJ.Source_Key
INNER JOIN Source AS S
    ON S.Source_Key = SJ.Source_Key
    AND S.Internal = 1
WHERE SJ.Record_Key = ''<#ReportKey>''
    AND SJ.Table_Name = ''Store'') AS TempTable', 1, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00', 'Sources');
INSERT INTO [Report_Block_In_Section] ([Report_Block_In_Section_Key], [Report_Block_Key], [Report_Section_Key], [Population_SQL], [Population_SQL_Record_Count], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian], [Title]) VALUES ('SYSTEM0000000027', 'SYSTEM0000000021', 'SYSTEM000000001Z', '--Ext Ref
SELECT SF.[File_Name] AS Item_Name
FROM Source_Join AS SJ
JOIN Source_File AS SF 
    ON SF.Source_Key = SJ.Source_Key
JOIN Source AS S ON S.Source_Key = SJ.Source_Key
WHERE  SJ.Record_Key = ''<#ReportKey>''
    AND S.Internal = 0
    AND Table_Name = ''Store''', '--Ext Ref
SELECT COUNT(SF.[File_Name]) AS Count
FROM Source_Join AS SJ
JOIN Source_File AS SF 
    ON SF.Source_Key = SJ.Source_Key
JOIN Source AS S ON S.Source_Key = SJ.Source_Key
WHERE  SJ.Record_Key = ''<#ReportKey>''
    AND S.Internal = 0
    AND Table_Name = ''Store''', 1, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00', 'External References');
INSERT INTO [Report_Block_In_Section] ([Report_Block_In_Section_Key], [Report_Block_Key], [Report_Section_Key], [Population_SQL], [Population_SQL_Record_Count], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian], [Title]) VALUES ('SYSTEM0000000028', 'SYSTEM000000003L', 'SYSTEM0000000020', 'SELECT TOP 1
  M.Number,
  CASE M.Movement_Type WHEN 2 THEN ''By'' ELSE ''To'' END AS ToOrBy,
  dbo.ufn_GetFormattedName(M.Other_Party_Name_Key) + ISNULL('' ('' + O.Full_Name + '' - '' + ODSrc.Item_Name + '')'', '''') AS LoanedTo,
  dbo.ufn_GetFormattedName(M.Staff_Responsible_Name_Key) AS StaffResponsible,
  OD.Item_Name AS Department,
  dbo.ufn_GetDateFromVagueDate(M.Exp_Vague_Date_Start, M.Exp_Vague_Date_End, M.Exp_Vague_Date_Type) AS ExpReturnDate,
  dbo.ufn_GetDateFromVagueDate(MM.Vague_Date_Start, MM.Vague_Date_End, MM.Vague_Date_Type) AS StartDate,
  case when
    -- Vague dates overlap
    dbo.ufn_DoVagueDatesOverlap(M.Exp_Vague_Date_Start, M.Exp_Vague_Date_End, MM.Vague_Date_Start, MM.Vague_Date_End) = 1 then
      ''-''
  when 
    -- Month2 < Month1
    dbo.FormatDatePart(M.Exp_Vague_Date_Start, M.Exp_Vague_Date_End, M.Exp_Vague_Date_Type, 1) <
    dbo.FormatDatePart(MM.Vague_Date_Start, MM.Vague_Date_End, MM.Vague_Date_Type, 1) then
      -- (Year2 - Year1 - 1) years, 12 - (Month1 - Month2)
      CONVERT(VARCHAR(10), 
        dbo.FormatDatePart(M.Exp_Vague_Date_Start, M.Exp_Vague_Date_End, M.Exp_Vague_Date_Type, 0) -
        dbo.FormatDatePart(MM.Vague_Date_Start, MM.Vague_Date_End, MM.Vague_Date_Type, 0) - 1
      ) + '' year(s) '' + CONVERT(VARCHAR(10), (
        12 - dbo.FormatDatePart(MM.Vague_Date_Start, MM.Vague_Date_End, MM.Vague_Date_Type, 1) +
        dbo.FormatDatePart(M.Exp_Vague_Date_Start, M.Exp_Vague_Date_End, M.Exp_Vague_Date_Type, 1)
      )) + '' month(s)''
  else -- Month2 >= Month1
      -- (Year2 - Year1) years, (Month2 - Month1 + 1) months
      CONVERT(VARCHAR(10), 
        dbo.FormatDatePart(M.Exp_Vague_Date_Start, M.Exp_Vague_Date_End, M.Exp_Vague_Date_Type, 0) -
        dbo.FormatDatePart(MM.Vague_Date_Start, MM.Vague_Date_End, MM.Vague_Date_Type, 0)
      ) + '' year(s) '' + CONVERT(VARCHAR(10), (
        dbo.FormatDatePart(M.Exp_Vague_Date_Start, M.Exp_Vague_Date_End, M.Exp_Vague_Date_Type, 1) -
        dbo.FormatDatePart(MM.Vague_Date_Start, MM.Vague_Date_End, MM.Vague_Date_Type, 1)
      )) + '' month(s)''  
  end AS LoanPeriod
FROM Movement M
LEFT JOIN Movement_Direction MD
  ON M.Movement_Key = MD.Movement_Key
  AND CASE WHEN M.Movement_Type=2 THEN 0 ELSE 1 END = MD.Outbound
LEFT JOIN Movement_Of_Material MM
  ON MD.Movement_Direction_Key = MM.Movement_Direction_Key
LEFT JOIN Individual IResp 
  ON IResp.Name_Key=M.Staff_Responsible_Name_Key
LEFT JOIN Organisation_Department OD 
  ON OD.Organisation_Department_Key=IResp.Organisation_Department_Key
LEFT JOIN Individual ISrc 
  ON ISrc.Name_Key=M.Other_Party_Name_Key
LEFT JOIN Organisation_Department ODSrc
  ON ODSrc.Organisation_Department_Key=ISrc.Organisation_Department_Key
LEFT JOIN Organisation O ON O.Name_Key=ODSrc.Name_Key
WHERE M.Movement_Key=''<#ReportKey>''
ORDER BY MM.Vague_Date_Start ASC', 'SELECT 1 AS Count', 1, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00', 'Details');
INSERT INTO [Report_Block_In_Section] ([Report_Block_In_Section_Key], [Report_Block_Key], [Report_Section_Key], [Population_SQL], [Population_SQL_Record_Count], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian], [Title]) VALUES ('SYSTEM0000000029', 'SYSTEM000000003M', 'SYSTEM0000000021', 'SELECT DISTINCT
  CUN.Number AS RegNo,
  CASE 
    WHEN D.Determination_Key IS NULL THEN 
      CASE ITN.Preferred_Name_Italic
        WHEN 1 THEN ''<i>'' + ITN.Preferred_Name + ''</i>''
        ELSE ITN.Preferred_Name
      END
    ELSE DPref.Item_Name
  END AS Determination,
  CType.Item_Name AS Type,
  dbo.ufn_GetFieldCollectors(SU.Collection_Unit_Key) AS FieldCollector,
  LN.Item_Name AS GatheringSite,
  dbo.ufn_GetDateFromVagueDate(S.Vague_Date_Start, S.Vague_Date_End, S.Vague_Date_Type) AS GatheringDate,
  SU.Collection_Unit_Key,
  CU.Current_Location_Code AS Store
FROM Movement_Direction MD
INNER JOIN Movement_Collection_Unit MCU 
    ON MCU.Movement_Direction_Key=MD.Movement_Direction_Key
INNER JOIN Specimen_Unit SU 
    ON SU.Collection_Unit_Key=MCU.Collection_Unit_Key
    OR SU.Parent_Collection_Collection_Unit_Key=MCU.Collection_Unit_Key
LEFT JOIN Collection_Unit_Number CUN 
    ON CUN.Collection_Unit_Key=SU.Collection_Unit_Key
    AND CUN.Preferred=1
    AND CUN.Type_Concept_Key=''SYSTEM0000000001'' -- Registration Number
LEFT JOIN Determination D 
    ON D.Determination_Key=SU.Preferred_Determination_Key
LEFT JOIN VW_ConceptTermPreferred DPref 
    ON DPref.Concept_Key=D.Concept_Key
LEFT JOIN Taxon_Determination TD
    ON TD.Taxon_Determination_Key=SU.Preferred_Taxon_Determination_Key
LEFT JOIN Index_Taxon_Name ITN 
    ON ITN.Taxon_List_Item_Key=TD.Taxon_List_Item_Key
INNER JOIN VW_ConceptTerm CType 
    ON CType.Concept_Key=SU.Specimen_Type_Concept_Key
LEFT JOIN Specimen_Field_Data SFD 
    ON SFD.Collection_Unit_Key=SU.Collection_Unit_Key
    AND SFD.Gathering_Event=1
LEFT JOIN Taxon_Occurrence XO 
    ON XO.Taxon_Occurrence_Key=SFD.Taxon_Occurrence_Key
LEFT JOIN Occurrence O 
    ON O.Occurrence_Key=SFD.Occurrence_Key
LEFT JOIN [Sample] S 
    ON S.Sample_Key=O.Sample_Key 
    OR S.Sample_Key=XO.Sample_Key
LEFT JOIN Location_Name LN 
    ON LN.Location_Key=S.Location_Key
    AND LN.Preferred=1
INNER JOIN Collection_Unit CU 
    ON CU.Collection_Unit_Key=SU.Collection_Unit_Key
WHERE MD.Movement_Key=''<#ReportKey>''', 'SELECT COUNT(DISTINCT SU.Collection_Unit_Key) AS Count
FROM Movement_Direction MD
INNER JOIN Movement_Collection_Unit MCU 
    ON MCU.Movement_Direction_Key=MD.Movement_Direction_Key
INNER JOIN Specimen_Unit SU 
    ON SU.Collection_Unit_Key=MCU.Collection_Unit_Key
    OR SU.Parent_Collection_Collection_Unit_Key=MCU.Collection_Unit_Key
INNER JOIN VW_ConceptTerm CType 
    ON CType.Concept_Key=SU.Specimen_Type_Concept_Key
INNER JOIN Collection_Unit CU 
    ON CU.Collection_Unit_Key=SU.Collection_Unit_Key
WHERE MD.Movement_Key=''<#ReportKey>''', 1, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00', 'Specimens');
INSERT INTO [Report_Block_In_Section] ([Report_Block_In_Section_Key], [Report_Block_Key], [Report_Section_Key], [Population_SQL], [Population_SQL_Record_Count], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian], [Title]) VALUES ('SYSTEM000000002A', 'SYSTEM000000003N', 'SYSTEM0000000021', 'SELECT DISTINCT
  MCU2.Accession_Number AS AccNo,
  C.Item_Name AS [Name],
  C.Topic,
dbo.ufn_GetFormattedName(Assembler_Name_Key) AS AssembledBy,
  CU.Current_Location_Code AS Store,
  C.Collection_Unit_Key
FROM Movement_Direction MD
INNER JOIN Movement_Collection_Unit MCU 
    ON MCU.Movement_Direction_Key=MD.Movement_Direction_Key
INNER JOIN Collection C ON C.Collection_Unit_Key=MCU.Collection_Unit_Key
INNER JOIN Collection_Unit CU
    ON CU.Collection_Unit_Key=C.Collection_Unit_Key
LEFT JOIN vw_CollectionUnitAccessionNumber MCU2
    ON CU.Collection_Unit_Key = MCU2.Collection_Unit_Key
WHERE MD.Movement_Key=''<#ReportKey>''', 'SELECT COUNT(DISTINCT
  C.Collection_Unit_Key) AS Count
FROM Movement_Direction MD
INNER JOIN Movement_Collection_Unit MCU 
    ON MCU.Movement_Direction_Key=MD.Movement_Direction_Key
INNER JOIN Collection C ON C.Collection_Unit_Key=MCU.Collection_Unit_Key
INNER JOIN Collection_Unit CU
    ON CU.Collection_Unit_Key=C.Collection_Unit_Key
WHERE MD.Movement_Key=''<#ReportKey>''', 2, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00', 'Collections');
INSERT INTO [Report_Block_In_Section] ([Report_Block_In_Section_Key], [Report_Block_Key], [Report_Section_Key], [Population_SQL], [Population_SQL_Record_Count], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian], [Title]) VALUES ('SYSTEM000000002B', 'SYSTEM000000003O', 'SYSTEM0000000021', '-- Stores
SELECT
    dbo.ufn_GetRegNumber(MCU.Collection_Unit_Key) AS RefNo,
    S.Item_Name AS [Name],
    CTS.PlainText AS Type
FROM Movement_Direction MD
INNER JOIN Movement_Collection_Unit MCU 
    ON MCU.Movement_Direction_Key=MD.Movement_Direction_Key
INNER JOIN Store S ON S.Collection_Unit_Key=MCU.Collection_Unit_Key
INNER JOIN Collection_Unit AS CUS ON CUS.Collection_Unit_Key = S.Collection_Unit_Key
INNER JOIN VW_ConceptTerm AS CTS ON CTS.Concept_Key = S.Store_Type_Concept_Key
WHERE MD.Movement_Key=''<#ReportKey>''', '-- Stores
SELECT COUNT(*) AS Count
FROM Movement_Direction MD
INNER JOIN Movement_Collection_Unit MCU 
    ON MCU.Movement_Direction_Key=MD.Movement_Direction_Key
INNER JOIN Store S ON S.Collection_Unit_Key=MCU.Collection_Unit_Key
INNER JOIN Collection_Unit AS CUS ON CUS.Collection_Unit_Key = S.Collection_Unit_Key
INNER JOIN VW_ConceptTerm AS CTS ON CTS.Concept_Key = S.Store_Type_Concept_Key
WHERE MD.Movement_Key=''<#ReportKey>''', 3, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00', 'Stores');
INSERT INTO [Report_Block_In_Section] ([Report_Block_In_Section_Key], [Report_Block_Key], [Report_Section_Key], [Population_SQL], [Population_SQL_Record_Count], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian], [Title]) VALUES ('SYSTEM000000002C', 'SYSTEM000000003E', 'SYSTEM0000000022', 'SELECT 
    V.Ref_Number AS RefNo,
    dbo.ufn_GetDateFromVagueDate(V.Vague_Date_Start, V.Vague_Date_End, V.Vague_Date_Type) as Date,
    CTType.Item_Name AS Type,
    dbo.ufn_GetFormattedName(V.Valued_By_Name_Key) AS ValuedBy,
    ISNULL(CAST(TF.Data AS VARCHAR(10)), '''') + CONVERT(VARCHAR(20), Value_Amount) +
      CASE 
        WHEN TF.Data IS NULL THEN ISNULL('' '' + TCurr.Item_Name, '''')
        ELSE ''''
        END
    AS Value,
    [Description]
FROM Valuation V
INNER JOIN Movement_Valuation MV
    ON MV.Valuation_Key=V.Valuation_Key
    AND MV.Movement_Key=''<#ReportKey>''
INNER JOIN VW_ConceptTerm CTType ON CTType.Concept_Key=V.Type_Concept_Key
LEFT JOIN Concept CCurr ON CCurr.Concept_Key=V.Currency_Concept_Key
LEFT JOIN Term TCurr ON TCurr.Term_Key=CCurr.Term_Key
LEFT JOIN Thesaurus_Fact TF 
    ON TF.Concept_Key=CCurr.Concept_Key
    OR TF.Meaning_Key=CCurr.Meaning_Key
    AND TF.Fact_Type_Concept_Key=''SYSTEM000000009A''
INNER JOIN Session S ON S.Session_ID = ''<#SessionId>''
INNER JOIN [User] U ON S.User_Name_Key = U.Name_Key
WHERE U.Allow_Finance=1', 'SELECT COUNT(*) AS Count 
FROM Valuation V
INNER JOIN Movement_Valuation MV
    ON MV.Valuation_Key=V.Valuation_Key
    AND MV.Movement_Key=''<#ReportKey>''
INNER JOIN VW_ConceptTerm CTType ON CTType.Concept_Key=V.Type_Concept_Key
INNER JOIN Session S ON S.Session_ID = ''<#SessionId>''
INNER JOIN [User] U ON S.User_Name_Key = U.Name_Key
WHERE U.Allow_Finance=1', 1, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00', 'Valuations');
INSERT INTO [Report_Block_In_Section] ([Report_Block_In_Section_Key], [Report_Block_Key], [Report_Section_Key], [Population_SQL], [Population_SQL_Record_Count], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian], [Title]) VALUES ('SYSTEM000000002D', 'SYSTEM000000003P', 'SYSTEM0000000023', 'SELECT 1 AS Count', 'SELECT 1 As Count', 1, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00', 'Agreement');
INSERT INTO [Report_Block_In_Section] ([Report_Block_In_Section_Key], [Report_Block_Key], [Report_Section_Key], [Population_SQL], [Population_SQL_Record_Count], [Sequence], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian], [Title]) VALUES ('SYSTEM000000002E', 'SYSTEM0000000023', 'SYSTEM0000000024', '--Multimedia
SELECT 
  [FILE_NAME]
FROM Source_File
JOIN Source_Join ON Source_file.Source_Key = Source_Join.Source_Key
WHERE Source_Join.Table_Name = ''Specimen_Unit''
  AND Source_File.Preferred = 1
  AND Source_Join.Record_Key = ''<#ReportKey>''', 'SELECT 
 Count([FILE_NAME])
FROM Source_File
JOIN Source_Join ON Source_file.Source_Key = Source_Join.Source_Key
WHERE Source_Join.Table_Name = ''Specimen_Unit''
  AND Source_File.Preferred = 1
  AND Source_Join.Record_Key = ''<#ReportKey>''', 1, 'SYSTEM0000000000', NULL, 1, 'SYSTEM00', 'Multimedia');
INSERT INTO [List_Report] ([List_Report_Key], [Item_Name], [Population_SQL], [Report_Block_Key], [Reported_Table], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000000', 'Accessions List', 'SELECT
  M.Number AS AccNo,
  dbo.ufn_GetDateFromVagueDate(M.Exp_Vague_Date_Start, M.Exp_Vague_Date_End, M.Exp_Vague_Date_Type) AS [Date],
  dbo.ufn_GetFormattedName(M.Other_Party_Name_Key) AS AccessedFrom,
  dbo.ufn_GetFormattedName(M.Staff_Responsible_Name_Key) AS StaffResp,
  dbo.ufn_GetFormattedName(I.Organisation_Department_Key) AS Department
FROM Movement AS M
LEFT JOIN Individual AS I
    ON I.Name_Key = M.Staff_Responsible_Name_Key
WHERE 
  M.Movement_Type = 0
  AND M.Movement_Key IN (<#ReportKeys>)', 'SYSTEM0000000000', 'Movement', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [List_Report] ([List_Report_Key], [Item_Name], [Population_SQL], [Report_Block_Key], [Reported_Table], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000001', 'Collections List', 'SELECT 
  M.Number AS AccNo,
  C.Item_Name AS [Name],
  dbo.ufn_GetFormattedName(Assembler_Name_Key) AS AssembledBy,
  C.Topic,
  CASE 
    WHEN CN.Number IS NULL THEN S.Item_Name
  ELSE
    S.Item_Name + '' - '' + CN.Number
  END AS Store
FROM Collection C
INNER JOIN Collection_Unit CU 
    ON CU.Collection_Unit_Key=C.Collection_Unit_Key
LEFT JOIN Store S 
    ON S.Collection_Unit_Key=CU.Current_Container_Collection_Unit_Key
LEFT JOIN 
    (Collection_Unit_Number CN
    INNER JOIN Concept CPT 
        ON CPT.Concept_Key=CN.Type_Concept_Key
        AND CPT.Meaning_Key=''SYSTEM0000000001'')
    ON CN.Collection_Unit_Key=S.Collection_Unit_Key
    AND CN.Preferred=1
LEFT JOIN 
    (MOVEMENT_COLLECTION_UNIT MCU 
        INNER JOIN MOVEMENT_DIRECTION MD 
            ON MCU.Movement_Direction_Key = MD.Movement_Direction_Key 
            AND MD.Outbound = 0
        INNER JOIN MOVEMENT M 
            ON MD.Movement_Key = M.Movement_Key 
            AND (M.Movement_Type = 0 OR M.Movement_Type = 1)) 
    ON CU.Collection_Unit_Key = MCU.Collection_Unit_Key
WHERE C.Collection_Unit_Key IN (<#ReportKeys>)', 'SYSTEM0000000001', 'Collection', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [List_Report] ([List_Report_Key], [Item_Name], [Population_SQL], [Report_Block_Key], [Reported_Table], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000002', 'Jobs List', 'SET ANSI_NULLS ON
SET ANSI_PADDING ON
SET ANSI_WARNINGS ON
SET CONCAT_NULL_YIELDS_NULL ON
SET QUOTED_IDENTIFIER ON
SET NO_BROWSETABLE OFF

SELECT
  CJ.Job_Number AS JobNo,
  CJ.Item_Name AS [Name],
  dbo.ufn_GetDateFromVagueDate(
    CJ.From_Vague_Date_Start, CJ.From_Vague_Date_End, CJ.From_Vague_Date_Type) + '' - '' +
    dbo.ufn_GetDateFromVagueDate(
    CJ.To_Vague_Date_Start, CJ.To_Vague_Date_End, CJ.To_Vague_Date_Type) AS Dates,
  CAST(Round(CJ.Duration, 2) AS varchar(20)) + '' '' + CTUnit.Item_Name AS Duration,
  CJ.Cost AS MaterialCost,
  dbo.ufn_GetConservationStatus(CJ.Status) AS Status
FROM Conservation_Job AS CJ
LEFT JOIN VW_ConceptTerm CTUnit 
    ON CTUnit.Concept_Key = CJ.Duration_Unit_Concept_Key
WHERE CJ.Conservation_Job_Key IN (<#ReportKeys>)', 'SYSTEM0000000002', 'Conservation_Job', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [List_Report] ([List_Report_Key], [Item_Name], [Population_SQL], [Report_Block_Key], [Reported_Table], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000003', 'Condition Checks List', 'SET ANSI_NULLS ON
SET ANSI_PADDING ON
SET ANSI_WARNINGS ON
SET CONCAT_NULL_YIELDS_NULL ON
SET QUOTED_IDENTIFIER ON
SET NO_BROWSETABLE OFF

SELECT
  CC.Ref_Number AS RefNo,
  CTType.Item_Name AS Type,
  CC.Details,
  CTCondition.Item_Name AS Condition,
  dbo.ufn_GetFormattedName(CC.Checked_By_Name_Key) AS CheckedBy,
  dbo.ufn_GetDateFromVagueDate(
    CC.Vague_Date_Start, CC.Vague_Date_End, CC.Vague_Date_Type) AS [Date]
FROM Conservation_Check AS CC
INNER JOIN VW_ConceptTerm AS CTType 
    ON CTType.Concept_Key = CC.Type_Concept_Key
INNER JOIN VW_ConceptTerm AS CTCondition 
    ON CTCondition.Concept_Key = CC.Condition_Concept_Key
WHERE CC.Conservation_Check_Key IN (<#ReportKeys>)', 'SYSTEM0000000003', 'Conservation_Check', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [List_Report] ([List_Report_Key], [Item_Name], [Population_SQL], [Report_Block_Key], [Reported_Table], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000004', 'Exchanges List', 'SELECT
  M.Number AS RefNo,
  dbo.ufn_GetDateFromVagueDate(
    M.Exp_Vague_Date_Start, M.Exp_Vague_Date_End, M.Exp_Vague_Date_Type) AS [Date],
  dbo.ufn_GetFormattedName(M.Other_Party_Name_Key) AS ExchangedWith,
  dbo.ufn_GetFormattedName(M.Staff_Responsible_Name_Key) AS StaffResp,
  dbo.ufn_GetFormattedName(I.Organisation_Department_Key) AS Department
FROM Movement AS M
LEFT JOIN Individual AS I 
    ON I.Name_Key = M.Staff_Responsible_Name_Key
WHERE M.Movement_Type = 1
    AND M.Movement_Key IN (<#ReportKeys>)', 'SYSTEM0000000004', 'Movement', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [List_Report] ([List_Report_Key], [Item_Name], [Population_SQL], [Report_Block_Key], [Reported_Table], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000005', 'Funding Sources List', 'SELECT
  dbo.ufn_GetDateFromVagueDate(
    F.Vague_Date_Start, F.Vague_Date_End, F.Vague_Date_Type) AS [Date],
  dbo.ufn_GetFormattedName(F.Funded_By_Name_Key) AS FundedBy,
  TF.Data AS CurrencySymbol,
  CAST(ROUND(F.Amount,2) AS Varchar(20)) AS Amount,
  F.Details
FROM Movement_Funding AS F
LEFT JOIN 
    (Concept AS C
        INNER JOIN Thesaurus_Fact AS TF 
            ON TF.Meaning_Key = C.Meaning_Key)
    ON C.Concept_Key = F.Currency_Concept_Key
WHERE F.Movement_Funding_Key IN (<#ReportKeys>)', 'SYSTEM0000000005', 'Movement_Funding', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [List_Report] ([List_Report_Key], [Item_Name], [Population_SQL], [Report_Block_Key], [Reported_Table], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000006', 'Funding Sources List', 'SELECT
  dbo.ufn_GetDateFromVagueDate(
      F.Vague_Date_Start, F.Vague_Date_End, F.Vague_Date_Type) AS [Date],
  dbo.ufn_GetFormattedName(F.Funded_By_Name_Key) AS FundedBy,
  TF.Data AS CurrencySymbol,
  CAST(ROUND(F.Amount, 2) AS varchar(20)) AS Amount,
  F.Details
FROM Conservation_Job_Funding AS F
LEFT JOIN (Concept AS C
    INNER JOIN Thesaurus_Fact AS TF ON TF.Meaning_Key = C.Meaning_Key)
        ON C.Concept_Key = F.Currency_Concept_Key
WHERE F.Conservation_Job_Funding_Key IN (<#ReportKeys>)', 'SYSTEM0000000005', 'Conservation_Job_Funding', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [List_Report] ([List_Report_Key], [Item_Name], [Population_SQL], [Report_Block_Key], [Reported_Table], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000007', 'Loans list', 'SELECT
  M.Number AS RefNo,
  dbo.ufn_GetLoanDates(M.Movement_Key) AS Dates,
  dbo.ufn_GetFormattedName(M.Other_Party_Name_Key) AS LoanedTo,
  dbo.ufn_GetFormattedName(M.Staff_Responsible_Name_Key) AS StaffResp,
  dbo.ufn_GetFormattedName(I.Organisation_Department_Key) AS Department,
  dbo.ufn_GetMovementStatus(M.Movement_Key) AS Status
FROM  Movement AS M
LEFT JOIN Individual AS I ON I.Name_Key = M.Staff_Responsible_Name_Key
WHERE M.Movement_Type IN (2,3)
AND  M.Movement_Key IN (<#ReportKeys>)', 'SYSTEM0000000007', 'Movement', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [List_Report] ([List_Report_Key], [Item_Name], [Population_SQL], [Report_Block_Key], [Reported_Table], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000008', 'Movements List', 'SELECT
  M.Number AS RefNo,
  dbo.ufn_GetDateFromVagueDate(
      M.Exp_Vague_Date_Start, M.Exp_Vague_Date_End, M.Exp_Vague_Date_Type) AS [Date],
  dbo.ufn_GetMovementTypeName(M.Movement_Type) AS Type,
  dbo.ufn_GetFormattedName(M.Other_Party_Name_Key) AS MovedTo,
  dbo.ufn_GetFormattedName(M.Staff_Responsible_Name_Key) AS StaffResp,
  dbo.ufn_GetFormattedName(I.Organisation_Department_Key) AS Department
FROM  Movement AS M
LEFT JOIN Individual AS I 
    ON I.Name_Key = M.Staff_Responsible_Name_Key
WHERE M.Movement_Key IN (<#ReportKeys>)', 'SYSTEM0000000008', 'Movement', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [List_Report] ([List_Report_Key], [Item_Name], [Population_SQL], [Report_Block_Key], [Reported_Table], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM0000000009', 'Specimens List', '
SET ANSI_NULLS ON
SET ANSI_PADDING ON  
SET ANSI_WARNINGS ON  
SET CONCAT_NULL_YIELDS_NULL ON  
SET QUOTED_IDENTIFIER ON  
SET NO_BROWSETABLE OFF    

SELECT
  dbo.ufn_GetRegNumber(SU.Collection_Unit_Key) AS RegNo,
  CASE  
    WHEN SU.Life_Sciences = 0 THEN ISNULL(CT.Item_Name, ''No Determination'')
  ELSE
    ISNULL(dbo.ufn_GetFormattedTaxonNameByParams(
        ITN.Actual_Name, ITN.Actual_Name_Italic, ITN.Common_Name,
        ITN.Common_Name_Italic, ITN.Authority, 1), ''No Determination'')
  END AS Determination,
  CTType.PlainText AS Type,
  dbo.ufn_GetFieldCollectors(SU.Collection_Unit_Key) AS FieldCollector,
  dbo.ufn_GetSpecimenGatheringSite(SU.Collection_Unit_Key) AS GatheringSite,
  dbo.ufn_GetDateFromVagueDate(
      S.Vague_Date_Start, S.Vague_Date_End, S.Vague_Date_Type) AS GatheringDate,
  CU.Current_Location_Code AS Store
FROM  Specimen_Unit AS SU  
INNER JOIN Collection_Unit AS CU ON CU.Collection_Unit_Key = SU.Collection_Unit_Key  
INNER JOIN  VW_ConceptTerm AS CTType ON CTType.Concept_Key = SU.Specimen_Type_Concept_Key  
LEFT JOIN  Determination D ON D.Determination_Key = SU.Preferred_Determination_Key
LEFT JOIN  Occurrence O ON O.Occurrence_Key=D.Determination_Key
LEFT JOIN  VW_ConceptTermPreferred CT ON CT.Concept_Key = D.Concept_Key  
LEFT JOIN  Taxon_Determination TD ON SU.Preferred_Taxon_Determination_Key = TD.Taxon_Determination_Key  
LEFT JOIN  Taxon_Occurrence XO ON XO.Taxon_Occurrence_Key=TD.Taxon_Determination_Key
LEFT JOIN  Index_Taxon_Name ITN ON ITN.Taxon_List_Item_Key = TD.Taxon_List_Item_Key  
LEFT JOIN  [Sample] S ON S.Sample_Key IN (XO.Sample_Key, O.Sample_Key)
WHERE  SU.Collection_Unit_Key IN (<#ReportKeys>)', 'SYSTEM0000000009', 'Specimen_Unit', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [List_Report] ([List_Report_Key], [Item_Name], [Population_SQL], [Report_Block_Key], [Reported_Table], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000000A', 'Stores List', 'SELECT
  dbo.ufn_GetRegNumber(S.Collection_Unit_Key) AS RefNo,
  S.Item_Name AS [Name],
  CTS.PlainText AS Type,
  CU.Current_Location_Code AS Store
FROM Store AS S
INNER JOIN Collection_Unit AS CU 
    ON CU.Collection_Unit_Key = S.Collection_Unit_Key
INNER JOIN VW_ConceptTerm AS CTS 
    ON CTS.Concept_Key = S.Store_Type_Concept_Key
WHERE S.Collection_Unit_Key IN (<#ReportKeys>)', 'SYSTEM0000000010', 'Store', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
INSERT INTO [List_Report] ([List_Report_Key], [Item_Name], [Population_SQL], [Report_Block_Key], [Reported_Table], [Entered_Session_ID], [Changed_Session_ID], [System_Supplied_Data], [Custodian]) VALUES ('SYSTEM000000000B', 'Valuations List', 'SET ANSI_NULLS ON
SET ANSI_PADDING ON
SET ANSI_WARNINGS ON
SET CONCAT_NULL_YIELDS_NULL ON
SET QUOTED_IDENTIFIER ON
SET NO_BROWSETABLE OFF

SELECT
  V.Ref_Number AS RefNo,
  dbo.ufn_GetDateFromVagueDate(
      V.Vague_Date_Start, V.Vague_Date_End, V.Vague_Date_Type) AS [Date],
  CTT.Plaintext AS Type,
  dbo.ufn_GetFormattedName(V.Valued_By_Name_Key) AS ValuedBy,
  ISNULL (
      CAST(TF.Data AS char(1)) + CAST(V.Value_Amount AS varchar(20)), 
      CAST(V.Value_Amount AS varchar(20))
  ) AS Value,
  ISNULL (
      dbo.ufn_GetDateFromVagueDate(
      V.Valid_From_Vague_Date_Start, V.Valid_From_Vague_Date_End, V.Valid_From_Vague_Date_Type) + '' - '' +
      dbo.ufn_GetDateFromVagueDate(
      V.Valid_To_Vague_Date_Start, V.Valid_To_Vague_Date_End, V.Valid_To_Vague_Date_Type), 
      dbo.ufn_GetDateFromVagueDate(
      V.Valid_From_Vague_Date_Start, V.Valid_From_Vague_Date_End, V.Valid_From_Vague_Date_Type)
  ) AS [Valid]
FROM Valuation AS V
LEFT JOIN VW_ConceptTerm CTT 
    ON CTT.Concept_Key = V.Type_Concept_Key
LEFT JOIN (Concept AS C
INNER JOIN Thesaurus_Fact AS TF 
    ON TF.Meaning_Key = C.Meaning_Key)
		ON C.Concept_Key = V.Currency_Concept_Key
WHERE		V.Valuation_Key IN (<#ReportKeys>)', 'SYSTEM0000000011', 'Valuation', 'SYSTEM0000000000', NULL, 1, 'SYSTEM00');
