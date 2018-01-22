-- Indexes to improve deletion performance of a session
IF NOT EXISTS (SELECT * FROM SysIndexes WHERE Name = 'IX_DATA_ROW')
	CREATE INDEX [IX_DATA_ROW] ON [dbo].[QE_Data_Item]([QE_Data_Row_Key]) ON [PRIMARY]
GO

IF NOT EXISTS (SELECT * FROM SysIndexes WHERE Name = 'IX_SESSION')
	CREATE INDEX [IX_SESSION] ON [dbo].[QE_Data_Row]([QE_Session_Key]) ON [PRIMARY]
GO
