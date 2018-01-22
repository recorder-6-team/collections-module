
--Add new column "Internal Use" to specimen unit table

 ALTER TABLE Specimen_Unit ADD Internal_Use BIT NOT NULL 
 Default 'false'  

GO

/*===========================================================================*\
  Insert new specimen type of 'Catelogue Entry'
\*===========================================================================*/

--Create a Specimen Type term
Insert Into Term (Term_Key, Language_Key, Plaintext, Entered_Session_ID, System_Supplied_Data)
Values ('SYSTEM00000005AA', 'en','catalogue entry','SYSTEM0000000000',1)

-- Grab the specimens Concept Key as a parent?
DECLARE @specimensConceptKey CHAR(16)
SELECT TOP(1) @specimensConceptKey = Concept_Key 
FROM	Concept 
WHERE	Published_Term = 'specimens'

-- 'DSS004390000351O'?
--Create a Specimen Type concept
exec usp_ConceptSimple_Insert 
	'SYSTEM00000031AA',
	'SYSTEM00000005AA',
	'SYSTEM000000000J',
	NULL,NULL,1,1,1,NULL,
	@specimensConceptKey,
	NULL,NULL,NULL,NULL,
	'catalogue entry',
	1,NULL,'SYSTEM0000000000',1
GO



/*===========================================================================*\
  For existing specimens with a registration number starting “AA”, 
  or an Inventory Number containing “V”, 
  the specimen type is updated to be marked as “catalogue entry” and "Internal Use".
\*===========================================================================*/
DECLARE @conceptKey VARCHAR(16)
SELECT	@conceptKey = Concept_Key 
FROM	Concept 
WHERE	Published_Term = 'catalogue entry'

update Specimen_Unit --Update Specimen_Unit database table
Set Specimen_Type_Concept_Key = @conceptKey,
	Internal_Use = 'true' -- set the type of the Specimen_Unit to be the new catalogue entry type
	
from Specimen_Unit
      Inner Join collection_unit -- Specimen_Unit is linked to Collection Unit table
            on Specimen_Unit.Collection_Unit_Key = Collection_Unit.Collection_Unit_Key
      Inner Join Collection_Unit_Number -- Collection Unit is linked to Collection Unit Number table
            On Collection_Unit_Number.Collection_Unit_Key = Collection_Unit.Collection_Unit_Key
      Inner Join Concept --Concept is linked to Collection Unit Number table
            On collection_unit_number.type_concept_key = concept.Concept_key 
where -- Only updates specimens which have Inventory Numbers CONTAINING "V" or Registration Numbers STARTING with "AA"
(concept.published_term = 'Inventory Number' AND Collection_Unit_Number.Number like '%V%')
OR
(concept.published_term = 'Registration Number' AND Collection_Unit_Number.Number like 'AA%')

/*===========================================================================*\
 If no preferred number exists for these updated specimens,
 the inventory number is marked as the preferred number (if present).
\*===========================================================================*/

Update collection_unit_number -- Update the Preferred status of a Specimen's Numbering History item to preferred.
            Set Preferred = 1 
From collection_unit_number
      Inner Join concept --Collection Unit Numbers (Numbering History) link to Concepts
            On collection_unit_number.type_concept_key = concept.Concept_key 
      Inner Join collection_unit --Collection Unit Numbers also link to Collection Units
            On collection_unit_number.collection_unit_key = collection_unit.collection_unit_key
      Inner Join specimen_unit SU -- Collection Units link to Specimens
            On SU.collection_unit_key = collection_unit.collection_unit_key
-- Only update to Preferred = 1 if the number is an Inventory Number AND........(see next comment)             
WHERE	concept.published_term = 'Inventory Number' 
	AND	collection_unit_number.preferred = 0
	AND	SU.Specimen_Type_Concept_Key = @conceptKey

--The specimen DOES NOT have any other numbers that are already set to preferred. 
AND SU.collection_unit_key
 
NOT IN

(select specimen_unit.collection_unit_key
from specimen_unit 
      Inner Join collection_unit -- Collection Units link to Speciments
            on specimen_unit.collection_unit_key = collection_unit.collection_unit_key
      Inner Join collection_unit_number --Collection Unit Numbers link to Collection Units
            On collection_unit_number.collection_unit_key = collection_unit.collection_unit_key
      Inner Join concept --Collection Unit Numbers link to Concepts
            On collection_unit_number.type_concept_key = concept.Concept_key 
            --Check for numbers which are Preferred (remembering in this case we are using NOT IN at the start of this section
            --so this means we are actually checking for items without a Preferred number)
      Where collection_unit_number.preferred = 1)


GO
