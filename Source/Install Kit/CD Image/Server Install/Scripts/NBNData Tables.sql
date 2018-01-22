/*===========================================================================*\
  Description:	Script to alter some NBNData tables.

  Created:	September 2003

  Last revision information:
    $Revision: 1 $
    $Date: 5/01/06 16:12 $
    $Author: Johnvanbreda $

\*===========================================================================*/

/****** Object:  Table [dbo].[Source_File] ******/
ALTER TABLE [dbo].[Source_File] ADD
	[Title] [varchar] (100) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	[Preferred] [bit] NOT NULL DEFAULT (0),
	[Timestamp] [timestamp] NOT NULL
GO

/****** Object:  Table [dbo].[Taxon_Determination] ******/
ALTER TABLE [dbo].[TAXON_DETERMINATION] ADD
	[Specimen_Collection_Unit_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Nomenclatural_Status_Concept_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Confidence] [tinyint] NOT NULL DEFAULT (0) ,
	[Used_Specimen] [bit] NOT NULL DEFAULT (0) ,
	[Method] [text] COLLATE Latin1_General_CI_AS NULL ,
	[Inferred_Determiner] [tinyint] NOT NULL DEFAULT (0),
	[Timestamp] [timestamp] NOT NULL
GO

ALTER TABLE [dbo].[TAXON_DETERMINATION]
  ALTER COLUMN [Taxon_Occurrence_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL
GO

ALTER TABLE [dbo].[TAXON_DETERMINATION] ADD 
	CONSTRAINT [FK_TAXON_DETERMINATION_Specimen] FOREIGN KEY 
	(
		[Specimen_Collection_Unit_Key]
	) REFERENCES [dbo].[Specimen_Unit] (
		[Collection_Unit_Key]
	)
GO

/****** Object:  Table [dbo].[User] ******/
ALTER TABLE [dbo].[User] ADD
	[Allow_Quick_Entry] [bit] NOT NULL DEFAULT (0),
	[Allow_Quick_Entry_Processing] [bit] NOT NULL DEFAULT (0),
	[Allow_Movement_Edit] [bit] NOT NULL DEFAULT (0),
	[Allow_Finance] [bit] NOT NULL DEFAULT (0)
GO


