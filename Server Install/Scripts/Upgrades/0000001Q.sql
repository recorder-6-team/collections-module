-- CCN 23 / VI 11152 - Usual store for collection should not be mandatory
ALTER TABLE Collection_Unit
	ALTER COLUMN Usual_Container_Collection_Unit_Key CHAR(16) NULL

ALTER TABLE Collection_Unit
	ALTER COLUMN Usual_Location_Code VARCHAR(30) NULL
