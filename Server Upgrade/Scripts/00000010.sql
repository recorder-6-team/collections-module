UPDATE Database_Relationship
SET Detail_Field='Concept_Group_Version_From'
WHERE Relationship_Key='SYSTEM000000002L'

IF NOT EXISTS(SELECT 1 FROM Database_Relationship WHERE Relationship_Key='SYSTEM000000002V')
INSERT INTO Database_Relationship (Relationship_Key, Relationship_Name, Master_Table, Master_Field, Detail_Table, Detail_Field,
		Follow_Up, Follow_Down)
VALUES('SYSTEM000000002V', 'Concept_Group_VersionConcept_History_To', 
'Concept_Group_Version', 'Concept_Group_Version_Key', 'Concept_History', 
'Concept_Group_Version_To', 1, 0)

UPDATE Database_Relationship
SET Detail_Field='Nomenclatural_Status_Concept_Key'
WHERE Relationship_Key='SYSTEM000000000G'

UPDATE Database_Relationship
SET Master_Table='Thesaurus_Relation_Type'
WHERE Master_Table='Thesaurus Relation_Type'

UPDATE Database_Relationship SET Follow_Up=1 
WHERE Relationship_Key IN ('SYSTEM0000000001', 'SYSTEM0000000005', 'SYSTEM0000000009', 'SYSTEM0000000011')

UPDATE Database_Relationship SET Detail_Field='Store_Type_Concept_Key'
WHERE Relationship_Key='SYSTEM0000000004'





