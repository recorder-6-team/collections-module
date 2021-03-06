IF NOT EXISTS(SELECT * FROM Report_Block_Order WHERE Custodian='SYSTEM00')
BEGIN

INSERT INTO Report_Block_Order(Report_Block_Order_Key, Report_Block_Key, Item_Name, Order_Clause_SQL, Entered_Session_ID, System_Supplied_Data, Custodian)
VALUES ('SYSTEM0000000000', 'SYSTEM0000000001', 'Acc No', 'ORDER BY AccNo', 'SYSTEM0000000000', 1, 'SYSTEM00')

INSERT INTO Report_Block_Order(Report_Block_Order_Key, Report_Block_Key, Item_Name, Order_Clause_SQL, Entered_Session_ID, System_Supplied_Data, Custodian)
VALUES ('SYSTEM0000000001', 'SYSTEM0000000001', 'Name', 'ORDER BY [Name]', 'SYSTEM0000000000', 1, 'SYSTEM00')

INSERT INTO Report_Block_Order(Report_Block_Order_Key, Report_Block_Key, Item_Name, Order_Clause_SQL, Entered_Session_ID, System_Supplied_Data, Custodian)
VALUES ('SYSTEM0000000002', 'SYSTEM0000000001', 'Topic', 'ORDER BY Topic', 'SYSTEM0000000000', 1, 'SYSTEM00')

INSERT INTO Report_Block_Order(Report_Block_Order_Key, Report_Block_Key, Item_Name, Order_Clause_SQL, Entered_Session_ID, System_Supplied_Data, Custodian)
VALUES ('SYSTEM0000000003', 'SYSTEM0000000001', 'Assembled By', 'ORDER BY AssembledBy', 'SYSTEM0000000000', 1, 'SYSTEM00')

INSERT INTO Report_Block_Order(Report_Block_Order_Key, Report_Block_Key, Item_Name, Order_Clause_SQL, Entered_Session_ID, System_Supplied_Data, Custodian)
VALUES ('SYSTEM0000000004', 'SYSTEM0000000001', 'Store', 'ORDER BY Store', 'SYSTEM0000000000', 1, 'SYSTEM00')

INSERT INTO Report_Block_Order(Report_Block_Order_Key, Report_Block_Key, Item_Name, Order_Clause_SQL, Entered_Session_ID, System_Supplied_Data, Custodian)
VALUES ('SYSTEM0000000005', 'SYSTEM0000000013', 'Applies To', 'ORDER BY Applies_To', 'SYSTEM0000000000', 1, 'SYSTEM00')

INSERT INTO Report_Block_Order(Report_Block_Order_Key, Report_Block_Key, Item_Name, Order_Clause_SQL, Entered_Session_ID, System_Supplied_Data, Custodian)
VALUES ('SYSTEM0000000006', 'SYSTEM0000000013', 'Parameter', 'ORDER BY Parameter_Term', 'SYSTEM0000000000', 1, 'SYSTEM00')

INSERT INTO Report_Block_Order(Report_Block_Order_Key, Report_Block_Key, Item_Name, Order_Clause_SQL, Entered_Session_ID, System_Supplied_Data, Custodian)
VALUES ('SYSTEM0000000007', 'SYSTEM0000000013', 'Value', 'ORDER BY Value', 'SYSTEM0000000000', 1, 'SYSTEM00')

INSERT INTO Report_Block_Order(Report_Block_Order_Key, Report_Block_Key, Item_Name, Order_Clause_SQL, Entered_Session_ID, System_Supplied_Data, Custodian)
VALUES ('SYSTEM0000000008', 'SYSTEM0000000013', 'Method', 'ORDER BY Method_Term', 'SYSTEM0000000000', 1, 'SYSTEM00')

INSERT INTO Report_Block_Order(Report_Block_Order_Key, Report_Block_Key, Item_Name, Order_Clause_SQL, Entered_Session_ID, System_Supplied_Data, Custodian)
VALUES ('SYSTEM0000000009', 'SYSTEM0000000014', 'Applies To', 'ORDER BY Applies_To', 'SYSTEM0000000000', 1, 'SYSTEM00')

INSERT INTO Report_Block_Order(Report_Block_Order_Key, Report_Block_Key, Item_Name, Order_Clause_SQL, Entered_Session_ID, System_Supplied_Data, Custodian)
VALUES ('SYSTEM0000000010', 'SYSTEM0000000014', 'Parameter', 'ORDER BY Parameter', 'SYSTEM0000000000', 1, 'SYSTEM00')

INSERT INTO Report_Block_Order(Report_Block_Order_Key, Report_Block_Key, Item_Name, Order_Clause_SQL, Entered_Session_ID, System_Supplied_Data, Custodian)
VALUES ('SYSTEM0000000011', 'SYSTEM0000000014', 'Value', 'ORDER BY Value', 'SYSTEM0000000000', 1, 'SYSTEM00')

INSERT INTO Report_Block_Order(Report_Block_Order_Key, Report_Block_Key, Item_Name, Order_Clause_SQL, Entered_Session_ID, System_Supplied_Data, Custodian)
VALUES ('SYSTEM0000000012', 'SYSTEM0000000014', 'Method', 'ORDER BY Method_Term', 'SYSTEM0000000000', 1, 'SYSTEM00')

INSERT INTO Report_Block_Order(Report_Block_Order_Key, Report_Block_Key, Item_Name, Order_Clause_SQL, Entered_Session_ID, System_Supplied_Data, Custodian)
VALUES ('SYSTEM0000000013', 'SYSTEM0000000015', 'Reg No', 'ORDER BY RegNo', 'SYSTEM0000000000', 1, 'SYSTEM00')

INSERT INTO Report_Block_Order(Report_Block_Order_Key, Report_Block_Key, Item_Name, Order_Clause_SQL, Entered_Session_ID, System_Supplied_Data, Custodian)
VALUES ('SYSTEM0000000014', 'SYSTEM0000000015', 'Determination', 'ORDER BY Determination', 'SYSTEM0000000000', 1, 'SYSTEM00')

INSERT INTO Report_Block_Order(Report_Block_Order_Key, Report_Block_Key, Item_Name, Order_Clause_SQL, Entered_Session_ID, System_Supplied_Data, Custodian)
VALUES ('SYSTEM0000000015', 'SYSTEM0000000015', 'Type', 'ORDER BY Type', 'SYSTEM0000000000', 1, 'SYSTEM00')

INSERT INTO Report_Block_Order(Report_Block_Order_Key, Report_Block_Key, Item_Name, Order_Clause_SQL, Entered_Session_ID, System_Supplied_Data, Custodian)
VALUES ('SYSTEM0000000016', 'SYSTEM0000000015', 'Field Collector', 'ORDER BY FieldCollector', 'SYSTEM0000000000', 1, 'SYSTEM00')

INSERT INTO Report_Block_Order(Report_Block_Order_Key, Report_Block_Key, Item_Name, Order_Clause_SQL, Entered_Session_ID, System_Supplied_Data, Custodian)
VALUES ('SYSTEM0000000017', 'SYSTEM0000000015', 'Gathering Site', 'ORDER BY GatheringSite', 'SYSTEM0000000000', 1, 'SYSTEM00')

INSERT INTO Report_Block_Order(Report_Block_Order_Key, Report_Block_Key, Item_Name, Order_Clause_SQL, Entered_Session_ID, System_Supplied_Data, Custodian)
VALUES ('SYSTEM0000000018', 'SYSTEM0000000015', 'Gathering Date', 'ORDER BY GatheringDate', 'SYSTEM0000000000', 1, 'SYSTEM00')

INSERT INTO Report_Block_Order(Report_Block_Order_Key, Report_Block_Key, Item_Name, Order_Clause_SQL, Entered_Session_ID, System_Supplied_Data, Custodian)
VALUES ('SYSTEM0000000019', 'SYSTEM0000000015', 'Store', 'ORDER BY StoreCode', 'SYSTEM0000000000', 1, 'SYSTEM00')

INSERT INTO Report_Block_Order(Report_Block_Order_Key, Report_Block_Key, Item_Name, Order_Clause_SQL, Entered_Session_ID, System_Supplied_Data, Custodian)
VALUES ('SYSTEM0000000020', 'SYSTEM0000000016', 'Determination', 'ORDER BY Determination', 'SYSTEM0000000000', 1, 'SYSTEM00')

INSERT INTO Report_Block_Order(Report_Block_Order_Key, Report_Block_Key, Item_Name, Order_Clause_SQL, Entered_Session_ID, System_Supplied_Data, Custodian)
VALUES ('SYSTEM0000000021', 'SYSTEM0000000016', 'Determiner', 'ORDER BY Determiner', 'SYSTEM0000000000', 1, 'SYSTEM00')

INSERT INTO Report_Block_Order(Report_Block_Order_Key, Report_Block_Key, Item_Name, Order_Clause_SQL, Entered_Session_ID, System_Supplied_Data, Custodian)
VALUES ('SYSTEM0000000022', 'SYSTEM0000000016', 'Date', 'ORDER BY DetDate', 'SYSTEM0000000000', 1, 'SYSTEM00')

INSERT INTO Report_Block_Order(Report_Block_Order_Key, Report_Block_Key, Item_Name, Order_Clause_SQL, Entered_Session_ID, System_Supplied_Data, Custodian)
VALUES ('SYSTEM0000000023', 'SYSTEM0000000016', 'Type', 'ORDER BY Type', 'SYSTEM0000000000', 1, 'SYSTEM00')

INSERT INTO Report_Block_Order(Report_Block_Order_Key, Report_Block_Key, Item_Name, Order_Clause_SQL, Entered_Session_ID, System_Supplied_Data, Custodian)
VALUES ('SYSTEM0000000024', 'SYSTEM0000000016', 'Confidence', 'ORDER BY Confidence', 'SYSTEM0000000000', 1, 'SYSTEM00')

INSERT INTO Report_Block_Order(Report_Block_Order_Key, Report_Block_Key, Item_Name, Order_Clause_SQL, Entered_Session_ID, System_Supplied_Data, Custodian)
VALUES ('SYSTEM0000000025', 'SYSTEM0000000017', 'Survey Name', 'ORDER BY SurveyName', 'SYSTEM0000000000', 1, 'SYSTEM00')

INSERT INTO Report_Block_Order(Report_Block_Order_Key, Report_Block_Key, Item_Name, Order_Clause_SQL, Entered_Session_ID, System_Supplied_Data, Custodian)
VALUES ('SYSTEM0000000026', 'SYSTEM0000000017', 'Location', 'ORDER BY LocationName', 'SYSTEM0000000000', 1, 'SYSTEM00')

INSERT INTO Report_Block_Order(Report_Block_Order_Key, Report_Block_Key, Item_Name, Order_Clause_SQL, Entered_Session_ID, System_Supplied_Data, Custodian)
VALUES ('SYSTEM0000000027', 'SYSTEM0000000017', 'Collectors', 'ORDER BY FieldCollectors', 'SYSTEM0000000000', 1, 'SYSTEM00')

INSERT INTO Report_Block_Order(Report_Block_Order_Key, Report_Block_Key, Item_Name, Order_Clause_SQL, Entered_Session_ID, System_Supplied_Data, Custodian)
VALUES ('SYSTEM0000000028', 'SYSTEM0000000017', 'Date', 'ORDER BY SampleDate', 'SYSTEM0000000000', 1, 'SYSTEM00')

INSERT INTO Report_Block_Order(Report_Block_Order_Key, Report_Block_Key, Item_Name, Order_Clause_SQL, Entered_Session_ID, System_Supplied_Data, Custodian)
VALUES ('SYSTEM0000000029', 'SYSTEM0000000019', 'Type', 'ORDER BY Type', 'SYSTEM0000000000', 1, 'SYSTEM00')

INSERT INTO Report_Block_Order(Report_Block_Order_Key, Report_Block_Key, Item_Name, Order_Clause_SQL, Entered_Session_ID, System_Supplied_Data, Custodian)
VALUES ('SYSTEM0000000030', 'SYSTEM0000000019', 'Inscription', 'ORDER BY Item_Name', 'SYSTEM0000000000', 1, 'SYSTEM00')

INSERT INTO Report_Block_Order(Report_Block_Order_Key, Report_Block_Key, Item_Name, Order_Clause_SQL, Entered_Session_ID, System_Supplied_Data, Custodian)
VALUES ('SYSTEM0000000031', 'SYSTEM0000000019', 'Author', 'ORDER BY Author', 'SYSTEM0000000000', 1, 'SYSTEM00')

INSERT INTO Report_Block_Order(Report_Block_Order_Key, Report_Block_Key, Item_Name, Order_Clause_SQL, Entered_Session_ID, System_Supplied_Data, Custodian)
VALUES ('SYSTEM0000000032', 'SYSTEM0000000020', 'Document', 'ORDER BY CAST(R.Title AS VARCHAR(100))', 'SYSTEM0000000000', 1, 'SYSTEM00')

INSERT INTO Report_Block_Order(Report_Block_Order_Key, Report_Block_Key, Item_Name, Order_Clause_SQL, Entered_Session_ID, System_Supplied_Data, Custodian)
VALUES ('SYSTEM0000000033', 'SYSTEM0000000020', 'Original', 'ORDER BY Original', 'SYSTEM0000000000', 1, 'SYSTEM00')

INSERT INTO Report_Block_Order(Report_Block_Order_Key, Report_Block_Key, Item_Name, Order_Clause_SQL, Entered_Session_ID, System_Supplied_Data, Custodian)
VALUES ('SYSTEM0000000034', 'SYSTEM0000000020', 'Type', 'ORDER BY Type', 'SYSTEM0000000000', 1, 'SYSTEM00')

INSERT INTO Report_Block_Order(Report_Block_Order_Key, Report_Block_Key, Item_Name, Order_Clause_SQL, Entered_Session_ID, System_Supplied_Data, Custodian)
VALUES ('SYSTEM0000000035', 'SYSTEM0000000021', 'Item_Name', 'ORDER BY Item_Name', 'SYSTEM0000000000', 1, 'SYSTEM00')

END

