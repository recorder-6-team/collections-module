-- Fix relationship between Thesaurus_Relationship_Type and Concept_Relation
UPDATE	Database_Relationship
SET	Follow_Down = 0
WHERE	Relationship_Key = 'SYSTEM000000002O'

