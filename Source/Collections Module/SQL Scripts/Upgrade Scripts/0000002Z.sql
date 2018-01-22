DELETE FROM Database_Relationship WHERE Relationship_Key = 'SYSTEM000000000H'
GO

UPDATE Database_Relationship 
SET Follow_Up = 1, Follow_Down=0
WHERE Relationship_Key='SYSTEM000000002Z'
GO

IF NOT EXISTS(SELECT 1 FROM SysColumns WHERE Name='ONE_TO_ONE' AND ID=OBJECT_ID('Database_Relationship'))
ALTER TABLE Database_Relationship 
ADD ONE_TO_ONE BIT NOT NULL DEFAULT 0
GO

UPDATE Database_Relationship
SET ONE_TO_ONE=1
WHERE (Master_Table='Collection_Unit' AND Detail_Table='Specimen_Unit')
OR (Master_Table='Collection_Unit' AND Detail_Table='Collection')
OR (Master_Table='Collection_Unit' AND Detail_Table='Store')
GO