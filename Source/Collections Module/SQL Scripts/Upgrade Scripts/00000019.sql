-- Missing relationship for Collections
IF NOT EXISTS(SELECT * FROM Database_Relationship WHERE Relationship_Name = 'Concept_GroupConcept')
	INSERT INTO Database_Relationship (Relationship_Key, Relationship_Name, Master_Table, Master_Field, Detail_Table, Detail_Field, Follow_Up, Follow_Down)
	VALUES ('SYSTEM000000002W', 'Concept_GroupConcept', 'Concept_Group', 'Concept_Group_Key', 'Concept', 'Concept_Group_Key', 1, 1)
