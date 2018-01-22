/*===========================================================================*\

  New table for Descriptors and Measurements applied to a Location Feature.

  August 2004

\*===========================================================================*/

IF EXISTS (SELECT * FROM dbo.SysObjects WHERE Id = Object_Id(N'[dbo].[Location_Feature_Data]') AND ObjectProperty(Id, N'IsUserTable') = 1)
	DROP TABLE [dbo].[Location_Feature_Data]
GO

CREATE TABLE [dbo].[Location_Feature_Data] (
	[Location_Feature_Data_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Location_Feature_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Applies_To] [varchar] (50) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Method_Concept_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Duration] [varchar] (50) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Accuracy] [varchar] (50) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Parameter_Concept_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Unit_Concept_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Lower_Value] [varchar] (50) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Upper_Value] [varchar] (50) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Is_Descriptor] [bit] NOT NULL ,
	[Entered_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Changed_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Custodian] [char] (8) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Timestamp] [timestamp] NOT NULL 
) ON [PRIMARY]
GO

ALTER TABLE [dbo].[Location_Feature_Data] ADD 
	CONSTRAINT [PK_Location_Feature_Data] PRIMARY KEY  CLUSTERED 
	(
		[Location_Feature_Data_Key]
	)  ON [PRIMARY] 
GO

ALTER TABLE [dbo].[Location_Feature_Data] ADD 
	CONSTRAINT [FK_Location_Feature_Data_LOCATION_FEATURE] FOREIGN KEY 
	(
		[Location_Feature_Key]
	) REFERENCES [dbo].[LOCATION_FEATURE] (
		[LOCATION_FEATURE_KEY]
	) ON DELETE CASCADE 
GO

GRANT SELECT ON [dbo].[Location_Feature_Data] TO [R2k_ReadOnly]
GO

GRANT SELECT ON [dbo].[Location_Feature_Data] TO [R2k_RecordCardsOnly]
GO

GRANT SELECT ON [dbo].[Location_Feature_Data] TO [R2k_AddOnly]
GO

GRANT SELECT, UPDATE, INSERT ON [dbo].[Location_Feature_Data] TO [R2k_FullEdit]
GO

GRANT SELECT, UPDATE, INSERT ON [dbo].[Location_Feature_Data] TO [R2k_Administrator]
GO
