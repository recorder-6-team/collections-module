IF NOT EXISTS(SELECT * FROM SYSCOLUMNS WHERE Name='Position' AND ID=OBJECT_ID('QE_Data_Item'))
	ALTER TABLE dbo.QE_Data_Item ADD
		Position int NULL
GO


IF EXISTS(SELECT 1 FROM Sysconstraints WHERE OBJECT_NAME(ConstID)='IX_QE_Data_Item_Unique')
	ALTER TABLE [dbo].[QE_Data_Item] DROP CONSTRAINT [IX_QE_Data_Item_Unique]
GO

IF NOT EXISTS(SELECT 1 FROM Setting WHERE Name='GeoAreaPK')
	INSERT INTO Setting(Name, Data)
	VALUES('GeoAreaPK', 'SYSTEM000000000X')
GO

IF NOT EXISTS(SELECT 1 FROM Concept_Group WHERE Concept_Group_Key='SYSTEM000000000X')
	INSERT INTO Concept_Group(Concept_Group_Key, Local_Domain_Key, Item_Name, Authority, Entered_Session_Id,
		System_Supplied_Data, Custodian)
	VALUES('SYSTEM000000000X', 'SYSTEM0000000000', 'Geographic Areas', 'System', 'SYSTEM0200000000',
		1, 'SYSTEM02')
GO

IF NOT EXISTS(SELECT 1 FROM QE_Field WHERE QE_Field_Key='SYSTEM000000000V')
	INSERT INTO QE_Field(QE_Field_Key, Item_Name, Data_Type, Field_Name, Table_Name, Template_Type, Default_Size,
		Entered_Session_Id, System_Supplied_Data, Custodian)
	VALUES('SYSTEM000000000V', 'Geographic Areas', 21, 'Survey_Event_Geo_Area_Key', 'Survey_Event_Geo_Area', 7, 70,
		'SYSTEM0000000000', 1, 'SYSTEM00')
GO