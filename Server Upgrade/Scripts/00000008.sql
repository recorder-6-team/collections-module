ALTER TABLE Macro
ALTER COLUMN Macro VARCHAR(200)

ALTER TABLE [dbo].[Macro_Generated_ID] 
	DROP CONSTRAINT PK_Macro_Generated_ID
GO

ALTER TABLE [dbo].[Macro_Generated_ID] WITH NOCHECK ADD 
	CONSTRAINT [PK_Macro_Generated_ID] PRIMARY KEY  CLUSTERED 
	(
		[Number_Type], [Macro_Output]
	)  ON [PRIMARY] 
GO