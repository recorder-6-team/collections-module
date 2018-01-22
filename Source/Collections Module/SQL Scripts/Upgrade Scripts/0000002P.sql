IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[FK_Survey_Event_Geo_Area_Concept]')
	   AND 	  Type = 'F')
ALTER TABLE [dbo].[Survey_Event_Geo_Area] DROP CONSTRAINT [FK_Survey_Event_Geo_Area_Concept]
GO
ALTER TABLE [dbo].[Survey_Event_Geo_Area]  WITH CHECK ADD  CONSTRAINT [FK_Survey_Event_Geo_Area_Concept] FOREIGN KEY([Concept_Key])
REFERENCES [dbo].[Concept] ([Concept_Key])
GO
ALTER TABLE [dbo].[Survey_Event_Geo_Area] CHECK CONSTRAINT [FK_Survey_Event_Geo_Area_Concept]