IF NOT EXISTS(SELECT * FROM Database_Relationship WHERE Relationship_Key='SYSTEM000000002Z')
	INSERT INTO Database_Relationship (
		Relationship_Key, Relationship_Name, 
		Master_Table, Master_Field, Detail_Table, Detail_Field,
		Follow_Up, Follow_Down)
	VALUES (
		'SYSTEM000000002Z', 'NameDetermination', 
		'NAME', 'Name_Key', 'Determination', 'Determiner_Name_Key', 0, 1)


IF NOT EXISTS(SELECT * FROM Database_Relationship WHERE Relationship_Key='SYSTEM0000000030')
	INSERT INTO Database_Relationship (
		Relationship_Key, Relationship_Name, 
		Master_Table, Master_Field, Detail_Table, Detail_Field,
		Follow_Up, Follow_Down)
	VALUES (
		'SYSTEM0000000030', 'ThesaurusRelationTypeDomain', 
		'Thesaurus_Relation_Type', 'Thesaurus_Relation_Type_Key', 'Domain', 'Domain_Key', 0, 1)





