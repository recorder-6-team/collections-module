CREATE TABLE [dbo].[Survey_Event_Geo_Area](
	[Survey_Event_Geo_Area_Key] [char](16) PRIMARY KEY NOT NULL,
	[Survey_Event_Key] [char](16) NOT NULL,
	[Concept_Key] [char](16) NOT NULL,
	[Entered_Session_ID] [char](16) NOT NULL,
	[System_Supplied_Data] [bit] NOT NULL,
	[Timestamp] [timestamp] NOT NULL,
) 

GO
SET ANSI_PADDING OFF
GO
ALTER TABLE [dbo].[Survey_Event_Geo_Area]  WITH CHECK ADD  CONSTRAINT [FK_Survey_Event_Geo_Area_Concept] FOREIGN KEY([Survey_Event_Geo_Area_Key])
REFERENCES [dbo].[Concept] ([Concept_Key])
GO
ALTER TABLE [dbo].[Survey_Event_Geo_Area] CHECK CONSTRAINT [FK_Survey_Event_Geo_Area_Concept]
GO
ALTER TABLE [dbo].[Survey_Event_Geo_Area]  WITH CHECK ADD  CONSTRAINT [FK_Survey_Event_Geo_Area_SURVEY_EVENT] FOREIGN KEY([Survey_Event_Key])
REFERENCES [dbo].[SURVEY_EVENT] ([SURVEY_EVENT_KEY])
GO
ALTER TABLE [dbo].[Survey_Event_Geo_Area] CHECK CONSTRAINT [FK_Survey_Event_Geo_Area_SURVEY_EVENT]
GO
GRANT SELECT ON [dbo].[Survey_Event_Geo_Area] TO [R2k_ReadOnly]
GO

GRANT SELECT ON [dbo].[Survey_Event_Geo_Area] TO [R2k_RecordCardsOnly]
GO

GRANT SELECT ON [dbo].[Survey_Event_Geo_Area] TO [R2k_AddOnly]
GO

GRANT SELECT, UPDATE, INSERT ON [dbo].[Survey_Event_Geo_Area] TO [R2k_FullEdit]
GO

GRANT SELECT, UPDATE, INSERT ON [dbo].[Survey_Event_Geo_Area] TO [R2k_Administrator]
GO