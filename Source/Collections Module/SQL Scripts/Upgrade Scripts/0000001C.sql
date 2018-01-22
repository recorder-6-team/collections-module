DELETE FROM Database_Relationship WHERE Relationship_Key IN ('SYSTEM000000002X', 'SYSTEM000000002Y')

INSERT INTO Database_Relationship (
		Relationship_Key, Relationship_Name, 
		Master_Table, Master_Field, Detail_Table, Detail_Field,
		Follow_Up, Follow_Down)
VALUES (
	'SYSTEM000000002X', 
	'Metadata_TypeMetadata',
	'Metadata_Type', 
	'Metadata_Type_Key', 
	'Metadata', 
	'Metadata_Type_Key', 
	0, 
	1)

INSERT INTO Database_Relationship (
		Relationship_Key, Relationship_Name, 
		Master_Table, Master_Field, Detail_Table, Detail_Field,
		Follow_Up, Follow_Down)
VALUES (
	'SYSTEM000000002Y', 
	'SourceSource_Join',
	'Source', 
	'Source_Key', 
	'Source_Join', 
	'Source_Key', 
	0, 
	1)

