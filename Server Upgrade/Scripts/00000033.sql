-- VI 17294 - CCN69 - Geographic Areas from Subject Area
DECLARE @SubjectAreaKey CHAR(16)
SET @SubjectAreaKey = 'DSS0039400000005'
IF EXISTS (SELECT * FROM Subject_Area WHERE Subject_Area_Key = @SubjectAreaKey)
	UPDATE Subject_Area 
	SET System_Supplied_Data = 1
	WHERE Subject_Area_Key = @SubjectAreaKey 
ELSE
	INSERT Subject_Area (Subject_Area_Key, Item_Name, Entered_Session_ID, System_Supplied_Data, Custodian)
	VALUES (@SubjectAreaKey, 'Geography', 'DSS003940000000Y', 1, 'DSS00394')
GO

DELETE Setting
WHERE [Name] = 'GeoAreaPK'
GO