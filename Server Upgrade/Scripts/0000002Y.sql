-- Remove link from Individual to Determination - this is handled by NAME link
DELETE FROM Database_Relationship WHERE Relationship_Key='SYSTEM000000000H'

-- Follow from determination to name, not other way round
UPDATE Database_Relationship 
SET Follow_Up=1, Follow_Down=0
WHERE Relationship_Key='SYSTEM000000002Z'