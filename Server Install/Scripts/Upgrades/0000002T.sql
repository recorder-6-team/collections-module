IF NOT EXISTS(SELECT 1 FROM Application WHERE Application_Key='SYSTEM0000000000') 
  INSERT INTO Application (Application_Key, Item_Name, Entered_Session_Id, System_Supplied_Data, Custodian)
  VALUES ('SYSTEM0000000000', 'Collections Module Geo Areas', 'SYSTEM0000000000', 1, 'SYSTEM00')

IF NOT EXISTS(SELECT 1 FROM Application_Concept_Group WHERE Application_Key='SYSTEM0000000000') 
  INSERT INTO Application_Concept_Group VALUES ('SYSTEM0000000000', 'SYSTEM000000000X')
