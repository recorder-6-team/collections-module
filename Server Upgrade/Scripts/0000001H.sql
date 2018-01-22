
/* 0000001H.sql - Set correct table name for QE field Label */
UPDATE QE_Field SET Table_Name = 'Specimen_Label' WHERE QE_Field_Key = 'SYSTEM000000000F'
