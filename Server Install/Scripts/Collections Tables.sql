/****** Object:  Table [dbo].[Collection]    Script Date: 27/08/2004 10:02:23 ******/
if not exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Collection]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
CREATE TABLE [dbo].[Collection] (
	[Collection_Unit_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Parent_Collection_Collection_Unit_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Item_Name] [varchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Assembler_Name_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Topic] [varchar] (200) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Risk_Concept_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Collation_From_Vague_Date_Start] [int] NULL ,
	[Collation_From_Vague_Date_End] [int] NULL ,
	[Collation_From_Vague_Date_Type] [varchar] (2) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Collation_To_Vague_Date_Start] [int] NULL ,
	[Collation_To_Vague_Date_End] [int] NULL ,
	[Collation_To_Vague_Date_Type] [varchar] (2) COLLATE Latin1_General_CI_AS NULL ,
	[Gather_From_Vague_Date_Start] [int] NULL ,
	[Gather_From_Vague_Date_End] [int] NULL ,
	[Gather_From_Vague_Date_Type] [varchar] (2) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Gather_To_Vague_Date_Start] [int] NULL ,
	[Gather_To_Vague_Date_End] [int] NULL ,
	[Gather_To_Vague_Date_Type] [varchar] (2) COLLATE Latin1_General_CI_AS NULL ,
	[Historical_Period_From] [varchar] (30) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Historical_Period_To] [varchar] (30) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Entered_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Changed_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Timestamp] [timestamp] NOT NULL 
) ON [PRIMARY]
GO

/****** Object:  Table [dbo].[Collection_Unit]    Script Date: 27/08/2004 10:02:24 ******/
if not exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Collection_Unit]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
CREATE TABLE [dbo].[Collection_Unit] (
	[Collection_Unit_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Current_Container_Collection_Unit_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Usual_Container_Collection_Unit_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Current_Location_Code] [varchar] (30) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Usual_Location_Code] [varchar] (30) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Domain_Mask] [int] NOT NULL ,
	[Entered_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Changed_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Custodian] [char] (8) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Timestamp] [timestamp] NOT NULL 
) ON [PRIMARY]
GO

/****** Object:  Table [dbo].[Collection_Unit_Check]    Script Date: 27/08/2004 10:02:25 ******/
if not exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Collection_Unit_Check]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
CREATE TABLE [dbo].[Collection_Unit_Check] (
	[Collection_Unit_Check_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Collection_Unit_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Conservation_Check_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Entered_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL 
) ON [PRIMARY]
GO

/****** Object:  Table [dbo].[Collection_Unit_Data]    Script Date: 27/08/2004 10:02:25 ******/
if not exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Collection_Unit_Data]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
CREATE TABLE [dbo].[Collection_Unit_Data] (
	[Collection_Unit_Data_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Collection_Unit_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
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

/****** Object:  Table [dbo].[Collection_Unit_Enquiry]    Script Date: 27/08/2004 10:02:25 ******/
if not exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Collection_Unit_Enquiry]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
CREATE TABLE [dbo].[Collection_Unit_Enquiry] (
	[Collection_Unit_Enquiry_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Collection_Unit_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Enquiry_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Entered_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL 
) ON [PRIMARY]
GO

/****** Object:  Table [dbo].[Collection_Unit_Funding]    Script Date: 27/08/2004 10:02:25 ******/
if not exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Collection_Unit_Funding]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
CREATE TABLE [dbo].[Collection_Unit_Funding] (
	[Collection_Unit_Funding_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Collection_Unit_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Funded_By_Name_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Vague_Date_Start] [int] NULL ,
	[Vague_Date_End] [int] NULL ,
	[Vague_Date_Type] [varchar] (2) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Amount] [money] NOT NULL ,
	[Currency_Concept_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Details] [text] COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Entered_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Changed_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Custodian] [char] (8) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Timestamp] [timestamp] NOT NULL 
) ON [PRIMARY] 
GO

/****** Object:  Table [dbo].[Collection_Unit_History]    Script Date: 27/08/2004 10:02:26 ******/
if not exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Collection_Unit_History]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
CREATE TABLE [dbo].[Collection_Unit_History] (
	[Collection_Unit_History_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Collection_Unit_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Source_Name_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[From_Vague_Date_Start] [int] NULL ,
	[From_Vague_Date_End] [int] NULL ,
	[From_Vague_Date_Type] [varchar] (2) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[To_Vague_Date_Start] [int] NULL ,
	[To_Vague_Date_End] [int] NULL ,
	[To_Vague_Date_Type] [varchar] (2) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Item_Name] [varchar] (100) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Comment] [text] COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Entered_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Changed_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Custodian] [char] (8) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Timestamp] [timestamp] NOT NULL 
) ON [PRIMARY] 
GO

/****** Object:  Table [dbo].[Collection_Unit_Material]    Script Date: 27/08/2004 10:02:26 ******/
if not exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Collection_Unit_Material]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
CREATE TABLE [dbo].[Collection_Unit_Material] (
	[Collection_Unit_Material_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Collection_Unit_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Material_Concept_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Quantity] [varchar] (20) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Unit_Concept_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Entered_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Changed_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Custodian] [char] (8) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Timestamp] [timestamp] NOT NULL 
) ON [PRIMARY]
GO

/****** Object:  Table [dbo].[Collection_Unit_Name]    Script Date: 27/08/2004 10:02:26 ******/
if not exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Collection_Unit_Name]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
CREATE TABLE [dbo].[Collection_Unit_Name] (
	[Collection_Unit_Name_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Collection_Unit_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Name_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Relation_Type_Concept_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Vague_Date_Start] [int] NULL ,
	[Vague_Date_End] [int] NULL ,
	[Vague_Date_Type] [varchar] (2) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Comment] [text] COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Entered_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Changed_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Custodian] [char] (8) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Timestamp] [timestamp] NOT NULL 
) ON [PRIMARY] 
GO

/****** Object:  Table [dbo].[Collection_Unit_Number]    Script Date: 27/08/2004 10:02:26 ******/
if not exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Collection_Unit_Number]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
CREATE TABLE [dbo].[Collection_Unit_Number] (
	[Collection_Unit_Number_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Collection_Unit_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Number] [varchar] (30) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Type_Concept_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Preferred] [bit] NOT NULL ,
	[Notes] [text] COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Entered_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Changed_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Custodian] [char] (8) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Timestamp] [timestamp] NOT NULL 
) ON [PRIMARY] 
GO

/****** Object:  Table [dbo].[Collection_Unit_Process]    Script Date: 27/08/2004 10:02:26 ******/
if not exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Collection_Unit_Process]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
CREATE TABLE [dbo].[Collection_Unit_Process] (
	[Collection_Unit_Process_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Collection_Unit_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Process_Concept_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Description] [text] COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Name_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Vague_Date_Start] [int] NULL ,
	[Vague_Date_End] [int] NULL ,
	[Vague_Date_Type] [varchar] (2) COLLATE Latin1_General_CI_AS NULL ,
	[Inferred_Process] [tinyint] NOT NULL ,
	[Inferred_Description] [tinyint] NOT NULL ,
	[Inferred_Person] [tinyint] NOT NULL ,
	[Inferred_Date] [tinyint] NOT NULL ,
	[Entered_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Changed_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Custodian] [char] (8) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Timestamp] [timestamp] NOT NULL 
) ON [PRIMARY] 
GO

/****** Object:  Table [dbo].[Collection_Unit_Relation]    Script Date: 27/08/2004 10:02:27 ******/
if not exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Collection_Unit_Relation]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
CREATE TABLE [dbo].[Collection_Unit_Relation] (
	[Collection_Unit_Relation_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[From_Collection_Unit_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[To_Collection_Unit_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Thesaurus_Relation_Type_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Inferred_Type] [tinyint] NOT NULL ,
	[Vague_Date_Start] [int] NULL ,
	[Vague_Date_End] [int] NULL ,
	[Vague_Date_Type] [varchar] (2) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Author_Name_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Comment] [text] COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Entered_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Changed_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Custodian] [char] (8) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Timestamp] [timestamp] NOT NULL 
) ON [PRIMARY] 
GO

/****** Object:  Table [dbo].[Collection_Unit_Relation]    Script Date: 27/08/2004 10:02:27 ******/
if not exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Collection_Unit_RelationCollection]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
CREATE TABLE [dbo].[Collection_Unit_Task] (
	[Collection_Unit_Task_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Conservation_Task_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Collection_Unit_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Entered_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Timestamp] [timestamp] NOT NULL 
) ON [PRIMARY]
GO

/****** Object:  Table [dbo].[Collection_Unit_Valuation]    Script Date: 27/08/2004 10:02:27 ******/
if not exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Collection_Unit_Valuation]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
CREATE TABLE [dbo].[Collection_Unit_Valuation] (
	[Collection_Unit_Valuation_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Collection_Unit_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Valuation_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Entered_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Timestamp] [timestamp] NOT NULL 
) ON [PRIMARY]
GO

/****** Object:  Table [dbo].[Concept]    Script Date: 27/08/2004 10:02:27 ******/
if not exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Concept]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
CREATE TABLE [dbo].[Concept] (
	[Concept_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Term_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Concept_Group_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Term_Version_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[List_Preferred] [bit] NOT NULL ,
	[Is_Current] [bit] NOT NULL ,
	[Preferred] [bit] NOT NULL ,
	[Concept_Rank_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Name_Type_Concept_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Meaning_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Author_Copy] [varchar] (100) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Sort_Code] [int] NULL ,
	[List_Code] [varchar] (50) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Entered_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Changed_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[System_Supplied_Data] [bit] NOT NULL ,
	[Custodian] [char] (8) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Timestamp] [timestamp] NOT NULL 
) ON [PRIMARY]
GO

/****** Object:  Table [dbo].[Concept_Designation]    Script Date: 27/08/2004 10:02:28 ******/
if not exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Concept_Designation]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
CREATE TABLE [dbo].[Concept_Designation] (
	[Concept_Designation_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Concept_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Designation_Type_Concept_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[From_Vague_Date_Start] [int] NULL ,
	[From_Vague_Date_End] [int] NULL ,
	[From_Vague_Date_Type] [varchar] (2) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[To_Vague_Date_Start] [int] NULL ,
	[To_Vague_Date_End] [int] NULL ,
	[To_Vague_Date_Type] [varchar] (2) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Entered_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Changed_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[System_Supplied_Data] [bit] NOT NULL ,
	[Custodian] [char] (8) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Timestamp] [timestamp] NOT NULL 
) ON [PRIMARY] 
GO

/****** Object:  Table [dbo].[Concept_Group]    Script Date: 27/08/2004 10:02:28 ******/
if not exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Concept_Group]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
CREATE TABLE [dbo].[Concept_Group] (
	[Concept_Group_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Local_Domain_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Item_Name] [varchar] (100) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Authority] [varchar] (100) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Url] [varchar] (255) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Hierarchy_Relation_Type_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Last_Sequence_Number] [varchar] (8) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Entered_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Changed_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[System_Supplied_Data] [bit] NOT NULL ,
	[Custodian] [char] (8) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Timestamp] [timestamp] NOT NULL 
) ON [PRIMARY]
GO

/****** Object:  Table [dbo].[Concept_Group_Version]    Script Date: 27/08/2004 10:02:28 ******/
if not exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Concept_Group_Version]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
CREATE TABLE [dbo].[Concept_Group_Version] (
	[Concept_Group_Version_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Concept_Group_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Version] [varchar] (100) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Sequence] [int] NOT NULL ,
	[From_Vague_Date_Start] [int] NULL ,
	[From_Vague_Date_End] [int] NULL ,
	[From_Vague_Date_Type] [varchar] (2) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[To_Vague_Date_Start] [int] NULL ,
	[To_Vague_Date_End] [int] NULL ,
	[To_Vague_Date_Type] [varchar] (2) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Acq_Vague_Date_Start] [int] NULL ,
	[Acq_Vague_Date_End] [int] NULL ,
	[Acq_Vague_Date_Type] [varchar] (2) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Url] [varchar] (255) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Entered_Session_Id] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Changed_Session_Id] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[System_Supplied_Data] [bit] NOT NULL ,
	[Custodian] [char] (8) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Timestamp] [timestamp] NOT NULL 
) ON [PRIMARY]
GO

/****** Object:  Table [dbo].[Concept_History]    Script Date: 27/08/2004 10:02:28 ******/
if not exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Concept_History]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
CREATE TABLE [dbo].[Concept_History] (
	[Concept_History_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Concept_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Concept_Group_Version_From] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Concept_Group_Version_To] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[From_Vague_Date_Start] [int] NULL ,
	[From_Vague_Date_End] [int] NULL ,
	[From_Vague_Date_Type] [varchar] (2) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[To_Vague_Date_Start] [int] NULL ,
	[To_Vague_Date_End] [int] NULL ,
	[To_Vague_Date_Type] [varchar] (2) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Entered_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Changed_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[System_Supplied_Data] [bit] NOT NULL ,
	[Custodian] [char] (8) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Timestamp] [timestamp] NOT NULL 
) ON [PRIMARY]
GO

/****** Object:  Table [dbo].[Concept_Lineage]    Script Date: 27/08/2004 10:02:28 ******/
if not exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Concept_Lineage]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
CREATE TABLE [dbo].[Concept_Lineage] (
	[Concept_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Lineage_ID] [int] NOT NULL ,
	[Lineage] [varchar] (900) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Last_Sequence_Number] [varchar] (8) COLLATE SQL_Latin1_General_CP1_CI_AS NULL 
) ON [PRIMARY]
GO

/****** Object:  Table [dbo].[Concept_Rank]    Script Date: 27/08/2004 10:02:29 ******/
if not exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Concept_Rank]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
CREATE TABLE [dbo].[Concept_Rank] (
	[Concept_Rank_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Domain_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Item_Name] [varchar] (100) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Sort_Order] [int] NULL ,
	[Abbreviation] [varchar] (10) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Color_R] [tinyint] NOT NULL ,
	[Color_G] [tinyint] NOT NULL ,
	[Color_B] [tinyint] NOT NULL ,
	[Entered_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Changed_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[System_Supplied_Data] [bit] NOT NULL ,
	[Custodian] [char] (8) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Timestamp] [timestamp] NOT NULL 
) ON [PRIMARY]
GO

/****** Object:  Table [dbo].[Concept_Relation]    Script Date: 27/08/2004 10:02:29 ******/
if not exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Concept_Relation]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
CREATE TABLE [dbo].[Concept_Relation] (
	[Concept_Relation_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[From_Concept_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[To_Concept_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Thesaurus_Relation_Type_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Multiplicity] [float] NULL ,
	[Inherited] [bit] NOT NULL ,
	[Comment] [text] COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Entered_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Changed_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[System_Supplied_Data] [bit] NOT NULL ,
	[Custodian] [char] (8) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Timestamp] [timestamp] NOT NULL 
) ON [PRIMARY] 
GO

/****** Object:  Table [dbo].[Conservation_Check]    Script Date: 27/08/2004 10:02:29 ******/
if not exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Conservation_Check]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
CREATE TABLE [dbo].[Conservation_Check] (
	[Conservation_Check_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Type_Concept_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Ref_Number] [varchar] (20) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Vague_Date_Start] [int] NULL ,
	[Vague_Date_End] [int] NULL ,
	[Vague_Date_Type] [varchar] (2) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Checked_By_Name_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Condition_Concept_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Applies_To_Contained_Specimens] [bit] NOT NULL ,
	[Details] [text] COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Domain_Mask] [int] NOT NULL ,
	[Search_Caption] [nvarchar] (226) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Display_Caption] [nvarchar] (226) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Entered_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Changed_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Custodian] [char] (8) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Timestamp] [timestamp] NOT NULL 
) ON [PRIMARY] 
GO

/****** Object:  Table [dbo].[Conservation_Job]    Script Date: 27/08/2004 10:02:29 ******/
if not exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Conservation_Job]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
CREATE TABLE [dbo].[Conservation_Job] (
	[Conservation_Job_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Item_Name] [varchar] (100) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Job_Number] [int] IDENTITY (1, 1) NOT NULL ,
	[From_Vague_Date_Start] [int] NULL ,
	[From_Vague_Date_End] [int] NULL ,
	[From_Vague_Date_Type] [varchar] (2) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[To_Vague_Date_Start] [int] NULL ,
	[To_Vague_Date_End] [int] NULL ,
	[To_Vague_Date_Type] [varchar] (2) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Duration] [float] NULL ,
	[Duration_Unit_Concept_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Status] [tinyint] NOT NULL ,
	[Details] [text] COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Cost] [money] NULL ,
	[Currency_Concept_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Domain_Mask] [int] NOT NULL ,
	[Display_Caption] [varchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Search_Caption] [varchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Entered_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Changed_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Custodian] [char] (8) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Timestamp] [timestamp] NOT NULL 
) ON [PRIMARY] 
GO

/****** Object:  Table [dbo].[Conservation_Job_Funding]    Script Date: 27/08/2004 10:02:30 ******/
if not exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Conservation_Job_Funding]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
CREATE TABLE [dbo].[Conservation_Job_Funding] (
	[Conservation_Job_Funding_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Conservation_Job_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Funded_By_Name_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Vague_Date_Start] [int] NULL ,
	[Vague_Date_End] [int] NULL ,
	[Vague_Date_Type] [varchar] (2) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Amount] [money] NOT NULL ,
	[Currency_Concept_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Details] [text] COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Entered_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Changed_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Custodian] [char] (8) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Timestamp] [timestamp] NOT NULL 
) ON [PRIMARY] 
GO

/****** Object:  Table [dbo].[Conservation_Job_Material]    Script Date: 27/08/2004 10:02:30 ******/
if not exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Conservation_Job_Material]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
CREATE TABLE [dbo].[Conservation_Job_Material] (
	[Conservation_Job_Material_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Conservation_Job_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Material_Concept_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Quantity] [varchar] (20) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Unit_Concept_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Entered_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Changed_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Custodian] [char] (8) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Timestamp] [timestamp] NOT NULL 
) ON [PRIMARY]
GO

/****** Object:  Table [dbo].[Conservation_Job_Staff]    Script Date: 27/08/2004 10:02:30 ******/
if not exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Conservation_Job_Staff]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
CREATE TABLE [dbo].[Conservation_Job_Staff] (
	[Conservation_Job_Staff_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Conservation_Job_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Name_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Entered_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL 
) ON [PRIMARY]
GO

/****** Object:  Table [dbo].[Conservation_Task]    Script Date: 27/08/2004 10:02:30 ******/
if not exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Conservation_Task]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
CREATE TABLE [dbo].[Conservation_Task] (
	[Conservation_Task_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Conservation_Check_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Conservation_Job_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Set_Vague_Date_Start] [int] NULL ,
	[Set_Vague_Date_End] [int] NULL ,
	[Set_Vague_Date_Type] [varchar] (2) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Status] [tinyint] NOT NULL ,
	[Type_Concept_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Priority] [tinyint] NOT NULL ,
	[Duration] [float] NULL ,
	[Duration_Unit_Concept_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Identifier_Name_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Task_Action] [text] COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Comment] [text] COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Display_Caption] [nvarchar] (103) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Search_Caption] [nvarchar] (103) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Entered_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Changed_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Custodian] [char] (8) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Timestamp] [timestamp] NOT NULL 
) ON [PRIMARY] 
GO

/****** Object:  Table [dbo].[Details_Report]    Script Date: 27/08/2004 10:02:31 ******/
if not exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Details_Report]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
CREATE TABLE [dbo].[Details_Report] (
	[Details_Report_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Item_Name] [varchar] (100) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Reported_Table] [varchar] (100) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Entered_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Changed_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[System_Supplied_Data] [bit] NOT NULL ,
	[Custodian] [char] (8) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Timestamp] [timestamp] NOT NULL 
) ON [PRIMARY]
GO

/****** Object:  Table [dbo].[Determination]    Script Date: 27/08/2004 10:02:31 ******/
if not exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Determination]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
CREATE TABLE [dbo].[Determination] (
	[Determination_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Concept_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Occurrence_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Specimen_Collection_Unit_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Determination_Type_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Nomenclatural_Status_Concept_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Confidence] [tinyint] NOT NULL ,
	[Determiner_Name_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Inferred_Determiner] [tinyint] NOT NULL ,
	[Determiner_Role_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Vague_Date_Start] [int] NULL ,
	[Vague_Date_End] [int] NULL ,
	[Vague_Date_Type] [varchar] (2) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Used_Specimen] [bit] NOT NULL ,
	[Preferred] [bit] NOT NULL ,
	[Method] [text] COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Notes] [text] COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Entered_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Changed_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Custodian] [char] (8) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Timestamp] [timestamp] NOT NULL 
) ON [PRIMARY] 
GO

/****** Object:  Table [dbo].[Domain]    Script Date: 27/08/2004 10:02:31 ******/
if not exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Domain]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
CREATE TABLE [dbo].[Domain] (
	[Domain_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Item_Name] [varchar] (100) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Subject_Area_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Has_Occurrences] [bit] NOT NULL ,
	[Default_Hierarchy_Relation_Type_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Domain_Mask] [int] NULL ,
	[Entered_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Changed_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[System_Supplied_Data] [bit] NOT NULL ,
	[Custodian] [char] (8) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Timestamp] [timestamp] NOT NULL 
) ON [PRIMARY]
GO

/****** Object:  Table [dbo].[Domain_Hyperlink]    Script Date: 27/08/2004 10:02:31 ******/
if not exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Domain_Hyperlink]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
CREATE TABLE [dbo].[Domain_Hyperlink] (
	[Domain_Hyperlink_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Item_Name] [varchar] (100) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Image_File] [varchar] (255) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[URL] [varchar] (255) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Use_Concept_Key] [bit] NULL ,
	[Word_Separator] [varchar] (5) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Local_Domain_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Entered_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Changed_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[System_Supplied_Data] [bit] NOT NULL ,
	[Custodian] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Timestamp] [timestamp] NOT NULL 
) ON [PRIMARY]
GO

/****** Object:  Table [dbo].[Enquiry]    Script Date: 27/08/2004 10:02:32 ******/
if not exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Enquiry]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
CREATE TABLE [dbo].[Enquiry] (
	[Enquiry_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Enquirer_Name_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Vague_Enquirer] [varchar] (100) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Enquiry_Type_Concept_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Enquiry_Method_Concept_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Vague_Date_Start] [int] NULL ,
	[Vague_Date_End] [int] NULL ,
	[Vague_Date_Type] [varchar] (2) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Material_Left] [bit] NOT NULL ,
	[Observation_Planned] [bit] NOT NULL ,
	[Description] [text] COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Answered_By_Name_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Answered] [bit] NOT NULL ,
	[Answered_Vague_Date_Start] [int] NULL ,
	[Answered_Vague_Date_End] [int] NULL ,
	[Answered_Vague_Date_Type] [varchar] (2) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Response] [text] COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Search_Caption] [nvarchar] (203) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Display_Caption] [nvarchar] (203) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Entered_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Changed_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Custodian] [char] (8) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Timestamp] [timestamp] NOT NULL 
) ON [PRIMARY] 
GO

/****** Object:  Table [dbo].[Enquiry_Concept]    Script Date: 27/08/2004 10:02:32 ******/
if not exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Enquiry_Concept]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
CREATE TABLE [dbo].[Enquiry_Concept] (
	[Enquiry_Concept_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Enquiry_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Concept_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Entered_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Timestamp] [timestamp] NOT NULL 
) ON [PRIMARY]
GO

/****** Object:  Table [dbo].[Import_Export_Job]    Script Date: 27/08/2004 10:02:32 ******/
if not exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Import_Export_Job]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
CREATE TABLE [dbo].[Import_Export_Job] (
	[Import_Export_Job_ID] [int] IDENTITY (1, 1) NOT NULL ,
	[Concept_Group_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Status_Message] [varchar] (200) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Total_Records] [int] NOT NULL ,
	[Records_Processed] [int] NOT NULL 
) ON [PRIMARY]
GO

/****** Object:  Table [dbo].[Language]    Script Date: 27/08/2004 10:02:32 ******/
if not exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Language]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
CREATE TABLE [dbo].[Language] (
	[Language_Key] [varchar] (4) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Item_Name] [varchar] (50) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Priority] [smallint] NULL 
) ON [PRIMARY]
GO

/****** Object:  Table [dbo].[List_Report]    Script Date: 27/08/2004 10:02:32 ******/
if not exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[List_Report]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
CREATE TABLE [dbo].[List_Report] (
	[List_Report_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Item_Name] [varchar] (100) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Population_SQL] [text] COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Report_Block_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Reported_Table] [varchar] (100) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Entered_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Changed_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[System_Supplied_Data] [bit] NOT NULL ,
	[Custodian] [char] (8) COLLATE SQL_Latin1_General_CP1_CI_AS NULL 
) ON [PRIMARY] 
GO

/****** Object:  Table [dbo].[Local_Domain]    Script Date: 27/08/2004 10:02:33 ******/
if not exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Local_Domain]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
CREATE TABLE [dbo].[Local_Domain] (
	[Local_Domain_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Item_Name] [varchar] (100) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Domain_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Language_Key] [varchar] (4) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Concept_Group_Label] [varchar] (50) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Entered_Session_Id] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Changed_Session_Id] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[System_Supplied_Data] [bit] NOT NULL ,
	[Custodian] [char] (8) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Timestamp] [timestamp] NOT NULL 
) ON [PRIMARY]
GO

/****** Object:  Table [dbo].[Macro]    Script Date: 27/08/2004 10:02:33 ******/
if not exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Macro]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
CREATE TABLE [dbo].[Macro] (
	[Number_Type] [varchar] (20) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Macro] [varchar] (200) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Macro_ID_Generation] [bit] NOT NULL ,
	[ID_Macro] [varchar] (100) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Last_Global_ID] [int] NOT NULL ,
	[ID_Seed] [int] NOT NULL 
) ON [PRIMARY]
GO

/****** Object:  Table [dbo].[Macro_Generated_ID]    Script Date: 27/08/2004 10:02:33 ******/
if not exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Macro_Generated_ID]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
CREATE TABLE [dbo].[Macro_Generated_ID] (
	[Number_Type] [varchar] (20) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Macro_Output] [varchar] (200) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Last_Macro_ID] [int] NOT NULL 
) ON [PRIMARY]
GO

/****** Object:  Table [dbo].[Meaning]    Script Date: 27/08/2004 10:02:33 ******/
if not exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Meaning]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
CREATE TABLE [dbo].[Meaning] (
	[Meaning_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL 
) ON [PRIMARY]
GO

/****** Object:  Table [dbo].[Meaning_Relation]    Script Date: 27/08/2004 10:02:33 ******/
if not exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Meaning_Relation]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
CREATE TABLE [dbo].[Meaning_Relation] (
	[Meaning_Relation_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[From_Meaning_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[To_Meaning_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[From_Concept_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[To_Concept_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Thesaurus_Relation_Type_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Multiplicity] [float] NULL ,
	[Comment] [text] COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Inherited] [bit] NOT NULL ,
	[Entered_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Changed_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[System_Supplied_Data] [bit] NOT NULL ,
	[Custodian] [char] (8) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Timestamp] [timestamp] NOT NULL 
) ON [PRIMARY] 
GO

/****** Object:  Table [dbo].[Metadata]    Script Date: 27/08/2004 10:02:34 ******/
if not exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Metadata]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
CREATE TABLE [dbo].[Metadata] (
	[Metadata_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Record_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Text] [text] COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Metadata_Type_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Entered_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Changed_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[System_Supplied_Data] [bit] NOT NULL ,
	[Custodian] [char] (8) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Timestamp] [timestamp] NOT NULL 
) ON [PRIMARY] 
GO

/****** Object:  Table [dbo].[Metadata_Type]    Script Date: 27/08/2004 10:02:34 ******/
if not exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Metadata_Type]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
CREATE TABLE [dbo].[Metadata_Type] (
	[Metadata_Type_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Item_Name] [varchar] (50) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Table_Name] [varchar] (50) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Description] [varchar] (100) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Entered_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Changed_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[System_Supplied_Data] [bit] NOT NULL ,
	[Custodian] [char] (8) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Timestamp] [timestamp] NOT NULL 
) ON [PRIMARY]
GO

/****** Object:  Table [dbo].[Movement]    Script Date: 27/08/2004 10:02:34 ******/
if not exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Movement]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
CREATE TABLE [dbo].[Movement] (
	[Movement_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Movement_Type] [tinyint] NOT NULL ,
	[Other_Party_Name_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Staff_Responsible_Name_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Contact_Name_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Exp_Vague_Date_Start] [int] NULL ,
	[Exp_Vague_Date_End] [int] NULL ,
	[Exp_Vague_Date_Type] [varchar] (2) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Number] [varchar] (30) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Notes] [text] COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Display_Caption] [nvarchar] (103) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Search_Caption] [nvarchar] (103) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Entered_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Changed_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Custodian] [char] (8) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Timestamp] [timestamp] NOT NULL 
) ON [PRIMARY] 
GO

/****** Object:  Table [dbo].[Movement_Collection_Unit]    Script Date: 27/08/2004 10:02:34 ******/
if not exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Movement_Collection_Unit]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
CREATE TABLE [dbo].[Movement_Collection_Unit] (
	[Movement_Collection_Unit_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Movement_Direction_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Collection_Unit_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Entered_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL 
) ON [PRIMARY]
GO

/****** Object:  Table [dbo].[Movement_Communication]    Script Date: 27/08/2004 10:02:34 ******/
if not exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Movement_Communication]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
CREATE TABLE [dbo].[Movement_Communication] (
	[Movement_Communication_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Movement_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Sender_Name_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Receiver_Name_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Communication_Type_Concept_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Vague_Date_Start] [int] NULL ,
	[Vague_Date_End] [int] NULL ,
	[Vague_Date_Type] [varchar] (2) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Content] [text] COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[File_Ref] [varchar] (20) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Entered_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Changed_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Custodian] [char] (8) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Timestamp] [timestamp] NOT NULL 
) ON [PRIMARY] 
GO

/****** Object:  Table [dbo].[Movement_Conservation_Check]    Script Date: 27/08/2004 10:02:35 ******/
if not exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Movement_Conservation_Check]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
CREATE TABLE [dbo].[Movement_Conservation_Check] (
	[Movement_Conservation_Check_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Movement_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Conservation_Check_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Entered_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Timestamp] [timestamp] NOT NULL 
) ON [PRIMARY]
GO

/****** Object:  Table [dbo].[Movement_Direction]    Script Date: 27/08/2004 10:02:35 ******/
if not exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Movement_Direction]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
CREATE TABLE [dbo].[Movement_Direction] (
	[Movement_Direction_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Movement_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Receiver_Name_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[OutBound] [bit] NOT NULL ,
	[Entered_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Changed_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Custodian] [char] (8) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Timestamp] [timestamp] NOT NULL 
) ON [PRIMARY]
GO

/****** Object:  Table [dbo].[Movement_Enquiry]    Script Date: 27/08/2004 10:02:35 ******/
if not exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Movement_Enquiry]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
CREATE TABLE [dbo].[Movement_Enquiry] (
	[Movement_Enquiry_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Movement_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Enquiry_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Entered_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL 
) ON [PRIMARY]
GO

/****** Object:  Table [dbo].[Movement_Funding]    Script Date: 27/08/2004 10:02:35 ******/
if not exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Movement_Funding]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
CREATE TABLE [dbo].[Movement_Funding] (
	[Movement_Funding_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Movement_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Funded_By_Name_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Vague_Date_Start] [int] NULL ,
	[Vague_Date_End] [int] NULL ,
	[Vague_Date_Type] [varchar] (2) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Amount] [money] NOT NULL ,
	[Currency_Concept_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Details] [text] COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Entered_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Changed_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Custodian] [char] (8) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Timestamp] [timestamp] NOT NULL 
) ON [PRIMARY] 
GO

/****** Object:  Table [dbo].[Movement_Of_Material]    Script Date: 27/08/2004 10:02:35 ******/
if not exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Movement_Of_Material]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
CREATE TABLE [dbo].[Movement_Of_Material] (
	[Movement_Of_Material_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Movement_Direction_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Contact_Name_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Vague_Date_Start] [int] NULL ,
	[Vague_Date_End] [int] NULL ,
	[Vague_Date_Type] [varchar] (2) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Completed] [bit] NOT NULL ,
	[Receiver_Name_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Receiver_Organisation_Department_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Value_Amount] [money] NULL ,
	[Currency_Concept_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Acquisition_Method_Concept_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Notes] [text] COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Entered_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Changed_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Custodian] [char] (8) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Timestamp] [timestamp] NOT NULL 
) ON [PRIMARY] 
GO

/****** Object:  Table [dbo].[Movement_Of_Material_Exclusion]    Script Date: 27/08/2004 10:02:36 ******/
if not exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Movement_Of_Material_Exclusion]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
CREATE TABLE [dbo].[Movement_Of_Material_Exclusion] (
	[Movement_Of_Material_Exclusion_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Movement_Of_Material_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Collection_Unit_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Entered_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL 
) ON [PRIMARY]
GO

/****** Object:  Table [dbo].[Movement_Of_Ownership]    Script Date: 27/08/2004 10:02:36 ******/
if not exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Movement_Of_Ownership]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
CREATE TABLE [dbo].[Movement_Of_Ownership] (
	[Movement_Of_Ownership_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Movement_Direction_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Contact_Name_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Vague_Date_Start] [int] NULL ,
	[Vague_Date_End] [int] NULL ,
	[Vague_Date_Type] [varchar] (2) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Notes] [text] COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Completed] [bit] NOT NULL ,
	[Entered_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Changed_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Custodian] [char] (8) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Timestamp] [timestamp] NOT NULL 
) ON [PRIMARY] 
GO

/****** Object:  Table [dbo].[Movement_Of_Ownership_Exclusion]    Script Date: 27/08/2004 10:02:36 ******/
if not exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Movement_Of_Ownership_Exclusion]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
CREATE TABLE [dbo].[Movement_Of_Ownership_Exclusion] (
	[Movement_Of_Ownership_Exclusion_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Movement_Of_Ownership_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Collection_Unit_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Entered_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL 
) ON [PRIMARY]
GO

/****** Object:  Table [dbo].[Movement_Valuation]    Script Date: 27/08/2004 10:02:36 ******/
if not exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Movement_Valuation]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
CREATE TABLE [dbo].[Movement_Valuation] (
	[Movement_Valuation_Key] [char] (16) COLLATE Latin1_General_CI_AS NOT NULL ,
	[Movement_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Valuation_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Entered_Session_ID] [char] (16) COLLATE Latin1_General_CI_AS NOT NULL ,
	[Timestamp] [timestamp] NOT NULL 
) ON [PRIMARY]
GO

/****** Object:  Table [dbo].[Occurrence]    Script Date: 27/08/2004 10:02:36 ******/
if not exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Occurrence]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
CREATE TABLE [dbo].[Occurrence] (
	[Occurrence_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Comment] [text] COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Zero_Abundance] [bit] NOT NULL ,
	[Confidential] [bit] NOT NULL ,
	[Verified] [tinyint] NOT NULL ,
	[Checked] [bit] NOT NULL ,
	[Checked_By] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Checked_Date] [datetime] NULL ,
	[Surveyors_Ref] [varchar] (30) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Sample_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Record_Type_Concept_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Entered_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Changed_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Custodian] [char] (8) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Timestamp] [timestamp] NOT NULL 
) ON [PRIMARY] 
GO

/****** Object:  Table [dbo].[Occurrence_Data]    Script Date: 27/08/2004 10:02:37 ******/
if not exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Occurrence_Data]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
CREATE TABLE [dbo].[Occurrence_Data] (
	[Occurrence_Data_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Occurrence_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
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

/****** Object:  Table [dbo].[Occurrence_Relation]    Script Date: 27/08/2004 10:02:37 ******/
if not exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Occurrence_Relation]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
CREATE TABLE [dbo].[Occurrence_Relation] (
	[Occurrence_Relation_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[From_Occurrence_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[To_Occurrence_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Thesaurus_Relation_Type_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Comment] [text] COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Entered_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Changed_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Custodian] [char] (8) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Timestamp] [timestamp] NOT NULL 
) ON [PRIMARY] 
GO

/****** Object:  Table [dbo].[QE_Data_Item]    Script Date: 27/08/2004 10:02:37 ******/
if not exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[QE_Data_Item]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
CREATE TABLE [dbo].[QE_Data_Item] (
	[QE_Data_Item_Key] [int] IDENTITY (1, 1) NOT NULL ,
	[QE_Data_Row_Key] [int] NOT NULL ,
	[QE_Template_Field_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Data_Value] [varchar] (200) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Data_Display] [varchar] (200) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Entered_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Changed_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Timestamp] [timestamp] NOT NULL 
) ON [PRIMARY]
GO

/****** Object:  Table [dbo].[QE_Data_Row]    Script Date: 27/08/2004 10:02:37 ******/
if not exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[QE_Data_Row]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
CREATE TABLE [dbo].[QE_Data_Row] (
	[QE_Data_Row_Key] [int] IDENTITY (1, 1) NOT NULL ,
	[QE_Session_Key] [int] NOT NULL ,
	[General] [bit] NOT NULL ,
	[Validated] [bit] NULL ,
	[Processed] [bit] NOT NULL ,
	[Entered_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Timestamp] [timestamp] NOT NULL 
) ON [PRIMARY]
GO

/****** Object:  Table [dbo].[QE_Field]    Script Date: 27/08/2004 10:02:37 ******/
if not exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[QE_Field]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
CREATE TABLE [dbo].[QE_Field] (
	[QE_Field_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Item_Name] [varchar] (100) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Data_Type] [int] NOT NULL ,
	[Field_Lookup_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Field_Name] [varchar] (50) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Table_Name] [varchar] (50) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Template_Type] [tinyint] NOT NULL ,
	[Default_Size] [int] NOT NULL ,
	[Entered_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Changed_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[System_Supplied_Data] [bit] NOT NULL ,
	[Custodian] [char] (8) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Timestamp] [timestamp] NOT NULL 
) ON [PRIMARY]
GO

/****** Object:  Table [dbo].[QE_Session]    Script Date: 27/08/2004 10:02:38 ******/
if not exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[QE_Session]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
CREATE TABLE [dbo].[QE_Session] (
	[QE_Session_Key] [int] IDENTITY (1, 1) NOT NULL ,
	[QE_Template_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Item_Name] [varchar] (50) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Entered_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Changed_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Timestamp] [timestamp] NOT NULL 
) ON [PRIMARY]
GO

/****** Object:  Table [dbo].[QE_Template]    Script Date: 27/08/2004 10:02:38 ******/
if not exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[QE_Template]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
CREATE TABLE [dbo].[QE_Template] (
	[QE_Template_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Item_Name] [varchar] (100) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Template_Type] [tinyint] NOT NULL ,
	[Subject_Area_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Entered_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Changed_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[System_Supplied_Data] [bit] NOT NULL ,
	[Timestamp] [timestamp] NOT NULL 
) ON [PRIMARY]
GO

/****** Object:  Table [dbo].[QE_Template_Field]    Script Date: 27/08/2004 10:02:38 ******/
if not exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[QE_Template_Field]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
CREATE TABLE [dbo].[QE_Template_Field] (
	[QE_Template_Field_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[QE_Template_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[QE_Field_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Is_Measurement] [bit] NOT NULL ,
	[Measurement_Applies_To] [varchar] (50) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Measurement_Method_Concept_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Measurement_Duration] [varchar] (50) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Measurement_Accuracy] [varchar] (50) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Measurement_Parameter_Concept_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Measurement_Unit_Concept_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Item_Name] [varchar] (100) COLLATE Latin1_General_CI_AS NULL ,
	[General_Tab] [bit] NOT NULL ,
	[Specimen_Tab] [bit] NOT NULL ,
	[Default_Value] [varchar] (200) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Default_Display] [varchar] (200) COLLATE Latin1_General_CI_AS NULL ,
	[Measurement_Is_Specimen] [bit] NULL ,
	[Sequence] [int] NOT NULL ,
	[Entered_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Changed_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[System_Supplied_Data] [bit] NOT NULL ,
	[Timestamp] [timestamp] NOT NULL 
) ON [PRIMARY]
GO

/****** Object:  Table [dbo].[Report_Block]    Script Date: 27/08/2004 10:02:38 ******/
if not exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Report_Block]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
CREATE TABLE [dbo].[Report_Block] (
	[Report_Block_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Title] [varchar] (100) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Header_File] [varchar] (255) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Row_File] [varchar] (255) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Footer_File] [varchar] (255) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Entered_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Changed_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[System_Supplied_Data] [bit] NOT NULL ,
	[Custodian] [char] (8) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Timestamp] [timestamp] NOT NULL 
) ON [PRIMARY]
GO

/****** Object:  Table [dbo].[Report_Block_In_Section]    Script Date: 27/08/2004 10:02:38 ******/
if not exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Report_Block_In_Section]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
CREATE TABLE [dbo].[Report_Block_In_Section] (
	[Report_Block_In_Section_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Report_Block_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Report_Section_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Population_SQL] [text] COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Population_SQL_Record_Count] [text] COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Sequence] [tinyint] NOT NULL ,
	[Entered_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Changed_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[System_Supplied_Data] [bit] NOT NULL ,
	[Custodian] [char] (8) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Timestamp] [timestamp] NOT NULL 
) ON [PRIMARY] 
GO

/****** Object:  Table [dbo].[Report_Block_Order]    Script Date: 27/08/2004 10:02:39 ******/
if not exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Report_Block_Order]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
CREATE TABLE [dbo].[Report_Block_Order] (
	[Report_Block_Order_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Report_Block_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Item_Name] [varchar] (100) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Order_Clause_SQL] [text] COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Entered_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Changed_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[System_Supplied_Data] [bit] NOT NULL ,
	[Custodian] [char] (8) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Timestamp] [timestamp] NOT NULL 
) ON [PRIMARY] 
GO

/****** Object:  Table [dbo].[Report_Section]    Script Date: 27/08/2004 10:02:39 ******/
if not exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Report_Section]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
CREATE TABLE [dbo].[Report_Section] (
	[Report_Section_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Section_List_SQL] [text] COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Item_Name_Macro] [varchar] (100) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Details_Report_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Sequence] [tinyint] NOT NULL ,
	[Entered_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Changed_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[System_Supplied_Data] [bit] NOT NULL ,
	[Custodian] [char] (8) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Timestamp] [timestamp] NOT NULL 
) ON [PRIMARY] 
GO

/****** Object:  Table [dbo].[Semantic_Relation]    Script Date: 27/08/2004 10:02:39 ******/
if not exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Semantic_Relation]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
CREATE TABLE [dbo].[Semantic_Relation] (
	[Semantic_Relation_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Item_Name] [varchar] (100) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Unidirectional] [bit] NOT NULL ,
	[Forward_Equivalence_Possible] [bit] NOT NULL ,
	[Forward_Equivalence_Definite] [bit] NOT NULL ,
	[Reverse_Equivalence_Possible] [bit] NOT NULL ,
	[Reverse_Equivalence_Definite] [bit] NOT NULL ,
	[Proportional_Relationship] [bit] NOT NULL ,
	[Adjacent] [bit] NULL ,
	[Chronological_Overlap] [tinyint] NULL ,
	[Description] [text] COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Entered_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Changed_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[System_Supplied_Data] [bit] NOT NULL ,
	[Custodian] [char] (8) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Timestamp] [timestamp] NOT NULL 
) ON [PRIMARY] 
GO

/****** Object:  Table [dbo].[Session]    Script Date: 27/08/2004 10:02:39 ******/
if not exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Session]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
CREATE TABLE [dbo].[Session] (
	[Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[User_Name_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Date_Time_Start] [datetime] NOT NULL ,
	[Date_Time_End] [datetime] NULL 
) ON [PRIMARY]
GO

/****** Object:  Table [dbo].[Source_Join]    Script Date: 27/08/2004 10:02:39 ******/
if not exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Source_Join]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
CREATE TABLE [dbo].[Source_Join] (
	[Source_Join_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Table_Name] [varchar] (50) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Record_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Source_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Original] [bit] NOT NULL ,
	[Entered_Session_Id] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[System_Supplied_Data] [bit] NOT NULL ,
	[Timestamp] [timestamp] NOT NULL 
) ON [PRIMARY]
GO

/****** Object:  Table [dbo].[Specimen_Field_Data]    Script Date: 27/08/2004 10:02:39 ******/
if not exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Specimen_Field_Data]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
CREATE TABLE [dbo].[Specimen_Field_Data] (
	[Specimen_Field_Data_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Collection_Unit_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Occurrence_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Taxon_Occurrence_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Inferred_Survey] [tinyint] NOT NULL ,
	[Inferred_Location] [tinyint] NOT NULL ,
	[Inferred_Spatial_Ref] [tinyint] NOT NULL ,
	[Inferred_Sample_Type] [tinyint] NOT NULL ,
	[Inferred_Date] [tinyint] NOT NULL ,
	[Inferred_Collectors] [tinyint] NOT NULL ,
	[Gathering_Event] [bit] NOT NULL ,
	[Entered_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Changed_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Custodian] [char] (8) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Timestamp] [timestamp] NOT NULL 
) ON [PRIMARY]
GO

/****** Object:  Table [dbo].[Specimen_Label]    Script Date: 27/08/2004 10:02:40 ******/
if not exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Specimen_Label]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
CREATE TABLE [dbo].[Specimen_Label] (
	[Specimen_Label_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Collection_Unit_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Is_Inscription] [bit] NOT NULL ,
	[Label_Position] [varchar] (100) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Inscription] [ntext] COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Translated] [text] COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Translated_Language_Key] [varchar] (4) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Comments] [text] COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Author_Name_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Inferred_Author] [tinyint] NOT NULL ,
	[Confidence_Concept_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Entered_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Changed_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Custodian] [char] (8) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Timestamp] [timestamp] NOT NULL 
) ON [PRIMARY] 
GO

/****** Object:  Table [dbo].[Specimen_Unit]    Script Date: 27/08/2004 10:02:40 ******/
if not exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Specimen_Unit]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
CREATE TABLE [dbo].[Specimen_Unit] (
	[Collection_Unit_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Parent_Collection_Collection_Unit_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Specimen_Type_Concept_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Confidential] [bit] NOT NULL ,
	[Dangerous] [bit] NOT NULL ,
	[Life_Sciences] [bit] NOT NULL ,
	[Preferred_Determination_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Preferred_Taxon_Determination_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Material_Source_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Entered_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Changed_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Timestamp] [timestamp] NOT NULL 
) ON [PRIMARY]
GO

/****** Object:  Table [dbo].[Store]    Script Date: 27/08/2004 10:02:40 ******/
if not exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Store]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
CREATE TABLE [dbo].[Store] (
	[Collection_Unit_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Item_Name] [varchar] (100) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Store_Type_Concept_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Comment] [text] COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Entered_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Changed_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Timestamp] [timestamp] NOT NULL 
) ON [PRIMARY] 
GO

/****** Object:  Table [dbo].[Subject_Area]    Script Date: 27/08/2004 10:02:40 ******/
if not exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Subject_Area]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
CREATE TABLE [dbo].[Subject_Area] (
	[Subject_Area_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Item_Name] [varchar] (100) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Comment] [text] COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Entered_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Changed_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[System_Supplied_Data] [bit] NOT NULL ,
	[Custodian] [char] (8) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Timestamp] [timestamp] NOT NULL 
) ON [PRIMARY] 
GO

/****** Object:  Table [dbo].[Taxon_Dictionary_Concept_Designation_Mapping]    Script Date: 27/08/2004 10:02:41 ******/
if not exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Taxon_Dictionary_Concept_Designation_Mapping]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
CREATE TABLE [dbo].[Taxon_Dictionary_Concept_Designation_Mapping] (
	[Taxon_Designation_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Concept_Designation_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Source_Join_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL 
) ON [PRIMARY]
GO

/****** Object:  Table [dbo].[Taxon_Dictionary_Concept_Group_Mapping]    Script Date: 27/08/2004 10:02:41 ******/
if not exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Taxon_Dictionary_Concept_Group_Mapping]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
CREATE TABLE [dbo].[Taxon_Dictionary_Concept_Group_Mapping] (
	[Taxon_List_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Concept_Group_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL 
) ON [PRIMARY]
GO

/****** Object:  Table [dbo].[Taxon_Dictionary_Concept_Group_Version_Mapping]    Script Date: 27/08/2004 10:02:41 ******/
if not exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Taxon_Dictionary_Concept_Group_Version_Mapping]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
CREATE TABLE [dbo].[Taxon_Dictionary_Concept_Group_Version_Mapping] (
	[Taxon_List_Version_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Concept_Group_Version_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Source_Join_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL 
) ON [PRIMARY]
GO

/****** Object:  Table [dbo].[Taxon_Dictionary_Concept_Mapping]    Script Date: 27/08/2004 10:02:41 ******/
if not exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Taxon_Dictionary_Concept_Mapping]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
CREATE TABLE [dbo].[Taxon_Dictionary_Concept_Mapping] (
	[Taxon_List_Item_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Concept_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL 
) ON [PRIMARY]
GO

/****** Object:  Table [dbo].[Taxon_Dictionary_Concept_Rank_Mapping]    Script Date: 27/08/2004 10:02:41 ******/
if not exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Taxon_Dictionary_Concept_Rank_Mapping]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
CREATE TABLE [dbo].[Taxon_Dictionary_Concept_Rank_Mapping] (
	[Taxon_Rank_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Concept_Rank_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL 
) ON [PRIMARY]
GO

/****** Object:  Table [dbo].[Taxon_Dictionary_Designation_Type_Mapping]    Script Date: 27/08/2004 10:02:41 ******/
if not exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Taxon_Dictionary_Designation_Type_Mapping]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
CREATE TABLE [dbo].[Taxon_Dictionary_Designation_Type_Mapping] (
	[Taxon_Designation_Type_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Concept_Designation_Type_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL 
) ON [PRIMARY]
GO

/****** Object:  Table [dbo].[Taxon_Dictionary_Meaning_Mapping]    Script Date: 27/08/2004 10:02:41 ******/
if not exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Taxon_Dictionary_Meaning_Mapping]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
CREATE TABLE [dbo].[Taxon_Dictionary_Meaning_Mapping] (
	[Preferred_Name] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Meaning_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL 
) ON [PRIMARY]
GO

/****** Object:  Table [dbo].[Taxon_Dictionary_Name_Type_Mapping]    Script Date: 27/08/2004 10:02:42 ******/
if not exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Taxon_Dictionary_Name_Type_Mapping]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
CREATE TABLE [dbo].[Taxon_Dictionary_Name_Type_Mapping] (
	[Taxon_Name_Type_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Thesaurus_Name_Type_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[System_Supplied_Data] [bit] NOT NULL 
) ON [PRIMARY]
GO

/****** Object:  Table [dbo].[Taxon_Dictionary_Term_Mapping]    Script Date: 27/08/2004 10:02:42 ******/
if not exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Taxon_Dictionary_Term_Mapping]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
CREATE TABLE [dbo].[Taxon_Dictionary_Term_Mapping] (
	[Taxon_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Italic_Font] [bit] NOT NULL ,
	[Term_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL 
) ON [PRIMARY]
GO

/****** Object:  Table [dbo].[Taxon_Dictionary_Term_Sources_Mapping]    Script Date: 27/08/2004 10:02:42 ******/
if not exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Taxon_Dictionary_Term_Sources_Mapping]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
CREATE TABLE [dbo].[Taxon_Dictionary_Term_Sources_Mapping] (
	[Source_Link_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Source_Join_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL 
) ON [PRIMARY]
GO

/****** Object:  Table [dbo].[Taxon_Dictionary_Term_Version_Mapping]    Script Date: 27/08/2004 10:02:42 ******/
if not exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Taxon_Dictionary_Term_Version_Mapping]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
CREATE TABLE [dbo].[Taxon_Dictionary_Term_Version_Mapping] (
	[Taxon_Version_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Term_Version_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Source_Join_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL 
) ON [PRIMARY]
GO

/****** Object:  Table [dbo].[Taxon_Dictionary_Thesaurus_Fact_Mapping]    Script Date: 27/08/2004 10:02:42 ******/
if not exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Taxon_Dictionary_Thesaurus_Fact_Mapping]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
CREATE TABLE [dbo].[Taxon_Dictionary_Thesaurus_Fact_Mapping] (
	[Taxon_Fact_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Thesaurus_Fact_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Source_Join_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL 
) ON [PRIMARY]
GO

/****** Object:  Table [dbo].[Term]    Script Date: 27/08/2004 10:02:42 ******/
if not exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Term]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
CREATE TABLE [dbo].[Term] (
	[Term_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Language_Key] [varchar] (4) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Item_Name] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Plaintext] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AI NOT NULL ,
	[Entered_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Changed_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[System_Supplied_Data] [bit] NOT NULL ,
	[Custodian] [char] (8) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Timestamp] [timestamp] NOT NULL 
) ON [PRIMARY]
GO

/****** Object:  Table [dbo].[Term_Version]    Script Date: 27/08/2004 10:02:42 ******/
if not exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Term_Version]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
CREATE TABLE [dbo].[Term_Version] (
	[Term_Version_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Term_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Version_Label] [varchar] (100) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Author_And_Date] [varchar] (100) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Entered_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Changed_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[System_Supplied_Data] [bit] NOT NULL ,
	[Custodian] [char] (8) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Timestamp] [timestamp] NOT NULL 
) ON [PRIMARY] 
GO

/****** Object:  Table [dbo].[Term_Version_Relation]    Script Date: 27/08/2004 10:02:43 ******/
if not exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Term_Version_Relation]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
CREATE TABLE [dbo].[Term_Version_Relation] (
	[Term_Version_Relation_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[From_Term_Version_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[To_Term_Version_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[From_Concept_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[To_Concept_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Thesaurus_Relation_Type_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Multiplicity] [float] NULL ,
	[Comment] [text] COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Entered_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Changed_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[System_Supplied_Data] [bit] NOT NULL ,
	[Custodian] [char] (8) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Timestamp] [timestamp] NOT NULL 
) ON [PRIMARY] 
GO

/****** Object:  Table [dbo].[Thesaurus_Fact]    Script Date: 27/08/2004 10:02:43 ******/
if not exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Thesaurus_Fact]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
CREATE TABLE [dbo].[Thesaurus_Fact] (
	[Thesaurus_Fact_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Item_Name] [varchar] (100) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Data] [text] COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Meaning_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Concept_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Term_Version_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Related_Term_Versions] [bit] NOT NULL ,
	[Inherited] [bit] NOT NULL ,
	[Language_Key] [varchar] (4) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Fact_Vague_Date_Start] [int] NULL ,
	[Fact_Vague_Date_End] [int] NULL ,
	[Fact_Vague_Date_Type] [varchar] (2) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Fact_Type_Concept_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Entered_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Changed_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[System_Supplied_Data] [bit] NOT NULL ,
	[Custodian] [char] (8) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Timestamp] [timestamp] NOT NULL 
) ON [PRIMARY] 
GO

/****** Object:  Table [dbo].[Thesaurus_Relation_Type]    Script Date: 27/08/2004 10:02:43 ******/
if not exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Thesaurus_Relation_Type]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
CREATE TABLE [dbo].[Thesaurus_Relation_Type] (
	[Thesaurus_Relation_Type_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Semantic_Relation_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Item_Name] [varchar] (100) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Forward_Term] [varchar] (100) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Reverse_Term] [varchar] (100) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Entered_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Changed_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[System_Supplied_Data] [bit] NOT NULL ,
	[Custodian] [char] (8) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Timestamp] [timestamp] NOT NULL 
) ON [PRIMARY]
GO

/****** Object:  Table [dbo].[Thesaurus_Relation_Type_Usage]    Script Date: 27/08/2004 10:02:43 ******/
if not exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Thesaurus_Relation_Type_Usage]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
CREATE TABLE [dbo].[Thesaurus_Relation_Type_Usage] (
	[Thesaurus_Relation_Type_Usage_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Thesaurus_Relation_Type_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Relation_Usage] [tinyint] NOT NULL ,
	[Entered_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Changed_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[System_Supplied_Data] [bit] NOT NULL ,
	[Custodian] [char] (8) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Timestamp] [timestamp] NOT NULL 
) ON [PRIMARY]
GO

/****** Object:  Table [dbo].[User_Domain_Access]    Script Date: 27/08/2004 10:02:44 ******/
if not exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[User_Domain_Access]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
CREATE TABLE [dbo].[User_Domain_Access] (
	[User_Domain_Access_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Name_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Domain_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Allow_Browse] [bit] NOT NULL ,
	[Allow_Quick_Entry] [bit] NOT NULL ,
	[Allow_Add] [bit] NOT NULL ,
	[Allow_Edit] [bit] NOT NULL ,
	[Entered_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Changed_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Timestamp] [timestamp] NOT NULL 
) ON [PRIMARY]
GO

/****** Object:  Table [dbo].[Valuation]    Script Date: 27/08/2004 10:02:44 ******/
if not exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Valuation]') and OBJECTPROPERTY(id, N'IsUserTable') = 1)
CREATE TABLE [dbo].[Valuation] (
	[Valuation_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Vague_Date_Start] [int] NULL ,
	[Vague_Date_End] [int] NULL ,
	[Vague_Date_Type] [varchar] (2) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Ref_Number] [varchar] (20) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Type_Concept_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Valued_By_Name_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Value_Amount] [money] NOT NULL ,
	[Currency_Concept_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Valid_From_Vague_Date_Start] [int] NULL ,
	[Valid_From_Vague_Date_End] [int] NULL ,
	[Valid_From_Vague_Date_Type] [varchar] (2) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Valid_To_Vague_Date_Start] [int] NULL ,
	[Valid_To_Vague_Date_End] [int] NULL ,
	[Valid_To_Vague_Date_Type] [varchar] (2) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Description] [text] COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Display_Caption] [nvarchar] (203) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Search_Caption] [nvarchar] (203) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Entered_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL ,
	[Changed_Session_ID] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Custodian] [char] (8) COLLATE SQL_Latin1_General_CP1_CI_AS NULL ,
	[Timestamp] [timestamp] NOT NULL 
) ON [PRIMARY] 
GO

GRANT  SELECT  ON [dbo].[Collection]  TO [R2k_ReadOnly]
GO

GRANT  SELECT  ON [dbo].[Collection]  TO [R2k_RecordCardsOnly]
GO

GRANT  SELECT  ON [dbo].[Collection]  TO [R2k_AddOnly]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Collection]  TO [R2k_FullEdit]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Collection]  TO [R2k_Administrator]
GO

GRANT  SELECT  ON [dbo].[Collection_Unit]  TO [R2k_ReadOnly]
GO

GRANT  SELECT  ON [dbo].[Collection_Unit]  TO [R2k_RecordCardsOnly]
GO

GRANT  SELECT  ON [dbo].[Collection_Unit]  TO [R2k_AddOnly]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Collection_Unit]  TO [R2k_FullEdit]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Collection_Unit]  TO [R2k_Administrator]
GO

GRANT  SELECT  ON [dbo].[Collection_Unit_Check]  TO [R2k_ReadOnly]
GO

GRANT  SELECT  ON [dbo].[Collection_Unit_Check]  TO [R2k_RecordCardsOnly]
GO

GRANT  SELECT  ON [dbo].[Collection_Unit_Check]  TO [R2k_AddOnly]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Collection_Unit_Check]  TO [R2k_FullEdit]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Collection_Unit_Check]  TO [R2k_Administrator]
GO

GRANT  SELECT  ON [dbo].[Collection_Unit_Data]  TO [R2k_ReadOnly]
GO

GRANT  SELECT  ON [dbo].[Collection_Unit_Data]  TO [R2k_RecordCardsOnly]
GO

GRANT  SELECT  ON [dbo].[Collection_Unit_Data]  TO [R2k_AddOnly]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Collection_Unit_Data]  TO [R2k_FullEdit]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Collection_Unit_Data]  TO [R2k_Administrator]
GO

GRANT  SELECT  ON [dbo].[Collection_Unit_Enquiry]  TO [R2k_ReadOnly]
GO

GRANT  SELECT  ON [dbo].[Collection_Unit_Enquiry]  TO [R2k_RecordCardsOnly]
GO

GRANT  SELECT  ON [dbo].[Collection_Unit_Enquiry]  TO [R2k_AddOnly]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Collection_Unit_Enquiry]  TO [R2k_FullEdit]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Collection_Unit_Enquiry]  TO [R2k_Administrator]
GO

GRANT  SELECT  ON [dbo].[Collection_Unit_Funding]  TO [R2k_ReadOnly]
GO

GRANT  SELECT  ON [dbo].[Collection_Unit_Funding]  TO [R2k_RecordCardsOnly]
GO

GRANT  SELECT  ON [dbo].[Collection_Unit_Funding]  TO [R2k_AddOnly]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Collection_Unit_Funding]  TO [R2k_FullEdit]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Collection_Unit_Funding]  TO [R2k_Administrator]
GO

GRANT  SELECT  ON [dbo].[Collection_Unit_History]  TO [R2k_ReadOnly]
GO

GRANT  SELECT  ON [dbo].[Collection_Unit_History]  TO [R2k_RecordCardsOnly]
GO

GRANT  SELECT  ON [dbo].[Collection_Unit_History]  TO [R2k_AddOnly]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Collection_Unit_History]  TO [R2k_FullEdit]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Collection_Unit_History]  TO [R2k_Administrator]
GO

GRANT  SELECT  ON [dbo].[Collection_Unit_Material]  TO [R2k_ReadOnly]
GO

GRANT  SELECT  ON [dbo].[Collection_Unit_Material]  TO [R2k_RecordCardsOnly]
GO

GRANT  SELECT  ON [dbo].[Collection_Unit_Material]  TO [R2k_AddOnly]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Collection_Unit_Material]  TO [R2k_FullEdit]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Collection_Unit_Material]  TO [R2k_Administrator]
GO

GRANT  SELECT  ON [dbo].[Collection_Unit_Material]  TO [NBNUser]
GO

GRANT  SELECT  ON [dbo].[Collection_Unit_Name]  TO [R2k_ReadOnly]
GO

GRANT  SELECT  ON [dbo].[Collection_Unit_Name]  TO [R2k_RecordCardsOnly]
GO

GRANT  SELECT  ON [dbo].[Collection_Unit_Name]  TO [R2k_AddOnly]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Collection_Unit_Name]  TO [R2k_FullEdit]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Collection_Unit_Name]  TO [R2k_Administrator]
GO

GRANT  SELECT  ON [dbo].[Collection_Unit_Number]  TO [R2k_ReadOnly]
GO

GRANT  SELECT  ON [dbo].[Collection_Unit_Number]  TO [R2k_RecordCardsOnly]
GO

GRANT  SELECT  ON [dbo].[Collection_Unit_Number]  TO [R2k_AddOnly]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Collection_Unit_Number]  TO [R2k_FullEdit]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Collection_Unit_Number]  TO [R2k_Administrator]
GO

GRANT  SELECT  ON [dbo].[Collection_Unit_Process]  TO [R2k_ReadOnly]
GO

GRANT  SELECT  ON [dbo].[Collection_Unit_Process]  TO [R2k_RecordCardsOnly]
GO

GRANT  SELECT  ON [dbo].[Collection_Unit_Process]  TO [R2k_AddOnly]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Collection_Unit_Process]  TO [R2k_FullEdit]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Collection_Unit_Process]  TO [R2k_Administrator]
GO

GRANT  SELECT  ON [dbo].[Collection_Unit_Relation]  TO [R2k_ReadOnly]
GO

GRANT  SELECT  ON [dbo].[Collection_Unit_Relation]  TO [R2k_RecordCardsOnly]
GO

GRANT  SELECT  ON [dbo].[Collection_Unit_Relation]  TO [R2k_AddOnly]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Collection_Unit_Relation]  TO [R2k_FullEdit]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Collection_Unit_Relation]  TO [R2k_Administrator]
GO

GRANT  SELECT  ON [dbo].[Collection_Unit_Task]  TO [R2k_ReadOnly]
GO

GRANT  SELECT  ON [dbo].[Collection_Unit_Task]  TO [R2k_RecordCardsOnly]
GO

GRANT  SELECT  ON [dbo].[Collection_Unit_Task]  TO [R2k_AddOnly]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Collection_Unit_Task]  TO [R2k_FullEdit]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Collection_Unit_Task]  TO [R2k_Administrator]
GO

GRANT  SELECT  ON [dbo].[Collection_Unit_Task]  TO [NBNUser]
GO

GRANT  SELECT  ON [dbo].[Collection_Unit_Valuation]  TO [R2k_ReadOnly]
GO

GRANT  SELECT  ON [dbo].[Collection_Unit_Valuation]  TO [R2k_RecordCardsOnly]
GO

GRANT  SELECT  ON [dbo].[Collection_Unit_Valuation]  TO [R2k_AddOnly]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Collection_Unit_Valuation]  TO [R2k_FullEdit]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Collection_Unit_Valuation]  TO [R2k_Administrator]
GO

GRANT  SELECT  ON [dbo].[Concept]  TO [R2k_ReadOnly]
GO

GRANT  SELECT  ON [dbo].[Concept]  TO [R2k_RecordCardsOnly]
GO

GRANT  SELECT  ON [dbo].[Concept]  TO [R2k_AddOnly]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Concept]  TO [R2k_FullEdit]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Concept]  TO [R2k_Administrator]
GO

GRANT  SELECT  ON [dbo].[Concept_Designation]  TO [R2k_ReadOnly]
GO

GRANT  SELECT  ON [dbo].[Concept_Designation]  TO [R2k_RecordCardsOnly]
GO

GRANT  SELECT  ON [dbo].[Concept_Designation]  TO [R2k_AddOnly]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Concept_Designation]  TO [R2k_FullEdit]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Concept_Designation]  TO [R2k_Administrator]
GO

GRANT  SELECT  ON [dbo].[Concept_Group]  TO [R2k_ReadOnly]
GO

GRANT  SELECT  ON [dbo].[Concept_Group]  TO [R2k_RecordCardsOnly]
GO

GRANT  SELECT  ON [dbo].[Concept_Group]  TO [R2k_AddOnly]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Concept_Group]  TO [R2k_FullEdit]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Concept_Group]  TO [R2k_Administrator]
GO

GRANT  SELECT  ON [dbo].[Concept_Group_Version]  TO [R2k_ReadOnly]
GO

GRANT  SELECT  ON [dbo].[Concept_Group_Version]  TO [R2k_RecordCardsOnly]
GO

GRANT  SELECT  ON [dbo].[Concept_Group_Version]  TO [R2k_AddOnly]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Concept_Group_Version]  TO [R2k_FullEdit]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Concept_Group_Version]  TO [R2k_Administrator]
GO

GRANT  SELECT  ON [dbo].[Concept_History]  TO [R2k_ReadOnly]
GO

GRANT  SELECT  ON [dbo].[Concept_History]  TO [R2k_RecordCardsOnly]
GO

GRANT  SELECT  ON [dbo].[Concept_History]  TO [R2k_AddOnly]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Concept_History]  TO [R2k_FullEdit]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Concept_History]  TO [R2k_Administrator]
GO

GRANT  SELECT  ON [dbo].[Concept_Lineage]  TO [public]
GO

GRANT  SELECT  ON [dbo].[Concept_Lineage]  TO [R2k_ReadOnly]
GO

GRANT  SELECT  ON [dbo].[Concept_Lineage]  TO [R2k_RecordCardsOnly]
GO

GRANT  SELECT  ON [dbo].[Concept_Lineage]  TO [R2k_AddOnly]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Concept_Lineage]  TO [R2k_FullEdit]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Concept_Lineage]  TO [R2k_Administrator]
GO

GRANT  SELECT  ON [dbo].[Concept_Rank]  TO [R2k_ReadOnly]
GO

GRANT  SELECT  ON [dbo].[Concept_Rank]  TO [R2k_RecordCardsOnly]
GO

GRANT  SELECT  ON [dbo].[Concept_Rank]  TO [R2k_AddOnly]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Concept_Rank]  TO [R2k_FullEdit]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Concept_Rank]  TO [R2k_Administrator]
GO

GRANT  SELECT  ON [dbo].[Concept_Relation]  TO [R2k_ReadOnly]
GO

GRANT  SELECT  ON [dbo].[Concept_Relation]  TO [R2k_RecordCardsOnly]
GO

GRANT  SELECT  ON [dbo].[Concept_Relation]  TO [R2k_AddOnly]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Concept_Relation]  TO [R2k_FullEdit]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Concept_Relation]  TO [R2k_Administrator]
GO

GRANT  SELECT  ON [dbo].[Conservation_Check]  TO [R2k_ReadOnly]
GO

GRANT  SELECT  ON [dbo].[Conservation_Check]  TO [R2k_RecordCardsOnly]
GO

GRANT  SELECT  ON [dbo].[Conservation_Check]  TO [R2k_AddOnly]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Conservation_Check]  TO [R2k_FullEdit]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Conservation_Check]  TO [R2k_Administrator]
GO

GRANT  SELECT  ON [dbo].[Conservation_Job]  TO [R2k_ReadOnly]
GO

GRANT  SELECT  ON [dbo].[Conservation_Job]  TO [R2k_RecordCardsOnly]
GO

GRANT  SELECT  ON [dbo].[Conservation_Job]  TO [R2k_AddOnly]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Conservation_Job]  TO [R2k_FullEdit]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Conservation_Job]  TO [R2k_Administrator]
GO

GRANT  SELECT  ON [dbo].[Conservation_Job_Funding]  TO [R2k_ReadOnly]
GO

GRANT  SELECT  ON [dbo].[Conservation_Job_Funding]  TO [R2k_RecordCardsOnly]
GO

GRANT  SELECT  ON [dbo].[Conservation_Job_Funding]  TO [R2k_AddOnly]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Conservation_Job_Funding]  TO [R2k_FullEdit]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Conservation_Job_Funding]  TO [R2k_Administrator]
GO

GRANT  SELECT  ON [dbo].[Conservation_Job_Funding]  TO [NBNUser]
GO

GRANT  SELECT  ON [dbo].[Conservation_Job_Material]  TO [R2k_ReadOnly]
GO

GRANT  SELECT  ON [dbo].[Conservation_Job_Material]  TO [R2k_RecordCardsOnly]
GO

GRANT  SELECT  ON [dbo].[Conservation_Job_Material]  TO [R2k_AddOnly]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Conservation_Job_Material]  TO [R2k_FullEdit]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Conservation_Job_Material]  TO [R2k_Administrator]
GO

GRANT  SELECT  ON [dbo].[Conservation_Job_Material]  TO [NBNUser]
GO

GRANT  SELECT  ON [dbo].[Conservation_Job_Staff]  TO [R2k_ReadOnly]
GO

GRANT  SELECT  ON [dbo].[Conservation_Job_Staff]  TO [R2k_RecordCardsOnly]
GO

GRANT  SELECT  ON [dbo].[Conservation_Job_Staff]  TO [R2k_AddOnly]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Conservation_Job_Staff]  TO [R2k_FullEdit]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Conservation_Job_Staff]  TO [R2k_Administrator]
GO

GRANT  SELECT  ON [dbo].[Conservation_Job_Staff]  TO [NBNUser]
GO

GRANT  SELECT  ON [dbo].[Conservation_Task]  TO [R2k_ReadOnly]
GO

GRANT  SELECT  ON [dbo].[Conservation_Task]  TO [R2k_RecordCardsOnly]
GO

GRANT  SELECT  ON [dbo].[Conservation_Task]  TO [R2k_AddOnly]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Conservation_Task]  TO [R2k_FullEdit]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Conservation_Task]  TO [R2k_Administrator]
GO

GRANT  SELECT  ON [dbo].[Determination]  TO [R2k_ReadOnly]
GO

GRANT  SELECT  ON [dbo].[Determination]  TO [R2k_RecordCardsOnly]
GO

GRANT  SELECT  ON [dbo].[Determination]  TO [R2k_AddOnly]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Determination]  TO [R2k_FullEdit]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Determination]  TO [R2k_Administrator]
GO

GRANT  SELECT  ON [dbo].[Domain]  TO [R2k_ReadOnly]
GO

GRANT  SELECT  ON [dbo].[Domain]  TO [R2k_RecordCardsOnly]
GO

GRANT  SELECT  ON [dbo].[Domain]  TO [R2k_AddOnly]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Domain]  TO [R2k_FullEdit]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Domain]  TO [R2k_Administrator]
GO

GRANT  SELECT  ON [dbo].[Domain_Hyperlink]  TO [R2k_ReadOnly]
GO

GRANT  SELECT  ON [dbo].[Domain_Hyperlink]  TO [R2k_RecordCardsOnly]
GO

GRANT  SELECT  ON [dbo].[Domain_Hyperlink]  TO [R2k_AddOnly]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Domain_Hyperlink]  TO [R2k_FullEdit]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Domain_Hyperlink]  TO [R2k_Administrator]
GO

GRANT  SELECT  ON [dbo].[Enquiry]  TO [R2k_ReadOnly]
GO

GRANT  SELECT  ON [dbo].[Enquiry]  TO [R2k_RecordCardsOnly]
GO

GRANT  SELECT  ON [dbo].[Enquiry]  TO [R2k_AddOnly]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Enquiry]  TO [R2k_FullEdit]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Enquiry]  TO [R2k_Administrator]
GO

GRANT  SELECT  ON [dbo].[Enquiry_Concept]  TO [R2k_ReadOnly]
GO

GRANT  SELECT  ON [dbo].[Enquiry_Concept]  TO [R2k_RecordCardsOnly]
GO

GRANT  SELECT  ON [dbo].[Enquiry_Concept]  TO [R2k_AddOnly]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Enquiry_Concept]  TO [R2k_FullEdit]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Enquiry_Concept]  TO [R2k_Administrator]
GO

GRANT  SELECT  ON [dbo].[Language]  TO [R2k_ReadOnly]
GO

GRANT  SELECT  ON [dbo].[Language]  TO [R2k_RecordCardsOnly]
GO

GRANT  SELECT  ON [dbo].[Language]  TO [R2k_AddOnly]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Language]  TO [R2k_FullEdit]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Language]  TO [R2k_Administrator]
GO

GRANT  SELECT  ON [dbo].[Local_Domain]  TO [R2k_ReadOnly]
GO

GRANT  SELECT  ON [dbo].[Local_Domain]  TO [R2k_RecordCardsOnly]
GO

GRANT  SELECT  ON [dbo].[Local_Domain]  TO [R2k_AddOnly]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Local_Domain]  TO [R2k_FullEdit]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Local_Domain]  TO [R2k_Administrator]
GO

GRANT  SELECT  ON [dbo].[Meaning]  TO [R2k_ReadOnly]
GO

GRANT  SELECT  ON [dbo].[Meaning]  TO [R2k_RecordCardsOnly]
GO

GRANT  SELECT  ON [dbo].[Meaning]  TO [R2k_AddOnly]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Meaning]  TO [R2k_FullEdit]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Meaning]  TO [R2k_Administrator]
GO

GRANT  SELECT  ON [dbo].[Meaning_Relation]  TO [R2k_ReadOnly]
GO

GRANT  SELECT  ON [dbo].[Meaning_Relation]  TO [R2k_RecordCardsOnly]
GO

GRANT  SELECT  ON [dbo].[Meaning_Relation]  TO [R2k_AddOnly]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Meaning_Relation]  TO [R2k_FullEdit]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Meaning_Relation]  TO [R2k_Administrator]
GO

GRANT  SELECT  ON [dbo].[Metadata]  TO [R2k_ReadOnly]
GO

GRANT  SELECT  ON [dbo].[Metadata]  TO [R2k_RecordCardsOnly]
GO

GRANT  SELECT  ON [dbo].[Metadata]  TO [R2k_AddOnly]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Metadata]  TO [R2k_FullEdit]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Metadata]  TO [R2k_Administrator]
GO

GRANT  SELECT  ON [dbo].[Metadata_Type]  TO [R2k_ReadOnly]
GO

GRANT  SELECT  ON [dbo].[Metadata_Type]  TO [R2k_RecordCardsOnly]
GO

GRANT  SELECT  ON [dbo].[Metadata_Type]  TO [R2k_AddOnly]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Metadata_Type]  TO [R2k_FullEdit]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Metadata_Type]  TO [R2k_Administrator]
GO

GRANT  SELECT  ON [dbo].[Movement]  TO [R2k_ReadOnly]
GO

GRANT  SELECT  ON [dbo].[Movement]  TO [R2k_RecordCardsOnly]
GO

GRANT  SELECT  ON [dbo].[Movement]  TO [R2k_AddOnly]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Movement]  TO [R2k_FullEdit]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Movement]  TO [R2k_Administrator]
GO

GRANT  SELECT  ON [dbo].[Movement]  TO [NBNUser]
GO

GRANT  SELECT  ON [dbo].[Movement_Collection_Unit]  TO [R2k_ReadOnly]
GO

GRANT  SELECT  ON [dbo].[Movement_Collection_Unit]  TO [R2k_RecordCardsOnly]
GO

GRANT  SELECT  ON [dbo].[Movement_Collection_Unit]  TO [R2k_AddOnly]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Movement_Collection_Unit]  TO [R2k_FullEdit]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Movement_Collection_Unit]  TO [R2k_Administrator]
GO

GRANT  SELECT  ON [dbo].[Movement_Communication]  TO [R2k_ReadOnly]
GO

GRANT  SELECT  ON [dbo].[Movement_Communication]  TO [R2k_RecordCardsOnly]
GO

GRANT  SELECT  ON [dbo].[Movement_Communication]  TO [R2k_AddOnly]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Movement_Communication]  TO [R2k_FullEdit]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Movement_Communication]  TO [R2k_Administrator]
GO

GRANT  SELECT  ON [dbo].[Movement_Conservation_Check]  TO [R2k_ReadOnly]
GO

GRANT  SELECT  ON [dbo].[Movement_Conservation_Check]  TO [R2k_RecordCardsOnly]
GO

GRANT  SELECT  ON [dbo].[Movement_Conservation_Check]  TO [R2k_AddOnly]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Movement_Conservation_Check]  TO [R2k_FullEdit]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Movement_Conservation_Check]  TO [R2k_Administrator]
GO

GRANT  SELECT  ON [dbo].[Movement_Direction]  TO [R2k_ReadOnly]
GO

GRANT  SELECT  ON [dbo].[Movement_Direction]  TO [R2k_RecordCardsOnly]
GO

GRANT  SELECT  ON [dbo].[Movement_Direction]  TO [R2k_AddOnly]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Movement_Direction]  TO [R2k_FullEdit]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Movement_Direction]  TO [R2k_Administrator]
GO

GRANT  SELECT  ON [dbo].[Movement_Enquiry]  TO [R2k_ReadOnly]
GO

GRANT  SELECT  ON [dbo].[Movement_Enquiry]  TO [R2k_RecordCardsOnly]
GO

GRANT  SELECT  ON [dbo].[Movement_Enquiry]  TO [R2k_AddOnly]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Movement_Enquiry]  TO [R2k_FullEdit]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Movement_Enquiry]  TO [R2k_Administrator]
GO

GRANT  SELECT  ON [dbo].[Movement_Enquiry]  TO [NBNUser]
GO

GRANT  SELECT  ON [dbo].[Movement_Funding]  TO [R2k_ReadOnly]
GO

GRANT  SELECT  ON [dbo].[Movement_Funding]  TO [R2k_RecordCardsOnly]
GO

GRANT  SELECT  ON [dbo].[Movement_Funding]  TO [R2k_AddOnly]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Movement_Funding]  TO [R2k_FullEdit]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Movement_Funding]  TO [R2k_Administrator]
GO

GRANT  SELECT  ON [dbo].[Movement_Funding]  TO [NBNUser]
GO

GRANT  SELECT  ON [dbo].[Movement_Of_Material]  TO [R2k_ReadOnly]
GO

GRANT  SELECT  ON [dbo].[Movement_Of_Material]  TO [R2k_RecordCardsOnly]
GO

GRANT  SELECT  ON [dbo].[Movement_Of_Material]  TO [R2k_AddOnly]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Movement_Of_Material]  TO [R2k_FullEdit]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Movement_Of_Material]  TO [R2k_Administrator]
GO

GRANT  SELECT  ON [dbo].[Movement_Of_Material_Exclusion]  TO [R2k_ReadOnly]
GO

GRANT  SELECT  ON [dbo].[Movement_Of_Material_Exclusion]  TO [R2k_RecordCardsOnly]
GO

GRANT  SELECT  ON [dbo].[Movement_Of_Material_Exclusion]  TO [R2k_AddOnly]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Movement_Of_Material_Exclusion]  TO [R2k_FullEdit]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Movement_Of_Material_Exclusion]  TO [R2k_Administrator]
GO

GRANT  SELECT  ON [dbo].[Movement_Of_Material_Exclusion]  TO [NBNUser]
GO

GRANT  SELECT  ON [dbo].[Movement_Of_Ownership]  TO [R2k_ReadOnly]
GO

GRANT  SELECT  ON [dbo].[Movement_Of_Ownership]  TO [R2k_RecordCardsOnly]
GO

GRANT  SELECT  ON [dbo].[Movement_Of_Ownership]  TO [R2k_AddOnly]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Movement_Of_Ownership]  TO [R2k_FullEdit]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Movement_Of_Ownership]  TO [R2k_Administrator]
GO

GRANT  SELECT  ON [dbo].[Movement_Of_Ownership_Exclusion]  TO [R2k_ReadOnly]
GO

GRANT  SELECT  ON [dbo].[Movement_Of_Ownership_Exclusion]  TO [R2k_RecordCardsOnly]
GO

GRANT  SELECT  ON [dbo].[Movement_Of_Ownership_Exclusion]  TO [R2k_AddOnly]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Movement_Of_Ownership_Exclusion]  TO [R2k_FullEdit]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Movement_Of_Ownership_Exclusion]  TO [R2k_Administrator]
GO

GRANT  SELECT  ON [dbo].[Movement_Valuation]  TO [R2k_ReadOnly]
GO

GRANT  SELECT  ON [dbo].[Movement_Valuation]  TO [R2k_RecordCardsOnly]
GO

GRANT  SELECT  ON [dbo].[Movement_Valuation]  TO [R2k_AddOnly]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Movement_Valuation]  TO [R2k_FullEdit]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Movement_Valuation]  TO [R2k_Administrator]
GO

GRANT  SELECT ,  UPDATE  ON [dbo].[Occurrence]  TO [public]
GO

GRANT  SELECT  ON [dbo].[Occurrence]  TO [R2k_ReadOnly]
GO

GRANT  SELECT  ON [dbo].[Occurrence]  TO [R2k_RecordCardsOnly]
GO

GRANT  SELECT  ON [dbo].[Occurrence]  TO [R2k_AddOnly]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Occurrence]  TO [R2k_FullEdit]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Occurrence]  TO [R2k_Administrator]
GO

GRANT  SELECT  ON [dbo].[Occurrence_Data]  TO [R2k_ReadOnly]
GO

GRANT  SELECT  ON [dbo].[Occurrence_Data]  TO [R2k_RecordCardsOnly]
GO

GRANT  SELECT  ON [dbo].[Occurrence_Data]  TO [R2k_AddOnly]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Occurrence_Data]  TO [R2k_FullEdit]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Occurrence_Data]  TO [R2k_Administrator]
GO

GRANT  SELECT  ON [dbo].[Occurrence_Relation]  TO [R2k_ReadOnly]
GO

GRANT  SELECT  ON [dbo].[Occurrence_Relation]  TO [R2k_RecordCardsOnly]
GO

GRANT  SELECT  ON [dbo].[Occurrence_Relation]  TO [R2k_AddOnly]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Occurrence_Relation]  TO [R2k_FullEdit]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Occurrence_Relation]  TO [R2k_Administrator]
GO

GRANT  SELECT  ON [dbo].[Semantic_Relation]  TO [R2k_ReadOnly]
GO

GRANT  SELECT  ON [dbo].[Semantic_Relation]  TO [R2k_RecordCardsOnly]
GO

GRANT  SELECT  ON [dbo].[Semantic_Relation]  TO [R2k_AddOnly]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Semantic_Relation]  TO [R2k_FullEdit]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Semantic_Relation]  TO [R2k_Administrator]
GO

GRANT  SELECT  ON [dbo].[Session]  TO [R2k_ReadOnly]
GO

GRANT  SELECT  ON [dbo].[Session]  TO [R2k_RecordCardsOnly]
GO

GRANT  SELECT  ON [dbo].[Session]  TO [R2k_AddOnly]
GO

GRANT  SELECT  ON [dbo].[Session]  TO [R2k_FullEdit]
GO

GRANT  SELECT  ON [dbo].[Session]  TO [R2k_Administrator]
GO

GRANT  SELECT  ON [dbo].[Source_Join]  TO [R2k_ReadOnly]
GO

GRANT  SELECT  ON [dbo].[Source_Join]  TO [R2k_RecordCardsOnly]
GO

GRANT  SELECT ,  INSERT  ON [dbo].[Source_Join]  TO [R2k_AddOnly]
GO

GRANT  SELECT ,  UPDATE ,  INSERT ,  DELETE  ON [dbo].[Source_Join]  TO [R2k_FullEdit]
GO

GRANT  SELECT ,  UPDATE ,  INSERT ,  DELETE  ON [dbo].[Source_Join]  TO [R2k_Administrator]
GO

GRANT  SELECT  ON [dbo].[Specimen_Field_Data]  TO [R2k_ReadOnly]
GO

GRANT  SELECT  ON [dbo].[Specimen_Field_Data]  TO [R2k_RecordCardsOnly]
GO

GRANT  SELECT  ON [dbo].[Specimen_Field_Data]  TO [R2k_AddOnly]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Specimen_Field_Data]  TO [R2k_FullEdit]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Specimen_Field_Data]  TO [R2k_Administrator]
GO

GRANT  SELECT  ON [dbo].[Specimen_Label]  TO [R2k_ReadOnly]
GO

GRANT  SELECT  ON [dbo].[Specimen_Label]  TO [R2k_RecordCardsOnly]
GO

GRANT  SELECT  ON [dbo].[Specimen_Label]  TO [R2k_AddOnly]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Specimen_Label]  TO [R2k_FullEdit]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Specimen_Label]  TO [R2k_Administrator]
GO

GRANT  SELECT  ON [dbo].[Specimen_Unit]  TO [R2k_ReadOnly]
GO

GRANT  SELECT  ON [dbo].[Specimen_Unit]  TO [R2k_RecordCardsOnly]
GO

GRANT  SELECT  ON [dbo].[Specimen_Unit]  TO [R2k_AddOnly]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Specimen_Unit]  TO [R2k_FullEdit]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Specimen_Unit]  TO [R2k_Administrator]
GO

GRANT  SELECT  ON [dbo].[Store]  TO [R2k_ReadOnly]
GO

GRANT  SELECT  ON [dbo].[Store]  TO [R2k_RecordCardsOnly]
GO

GRANT  SELECT  ON [dbo].[Store]  TO [R2k_AddOnly]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Store]  TO [R2k_FullEdit]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Store]  TO [R2k_Administrator]
GO

GRANT  SELECT  ON [dbo].[Subject_Area]  TO [R2k_ReadOnly]
GO

GRANT  SELECT  ON [dbo].[Subject_Area]  TO [R2k_RecordCardsOnly]
GO

GRANT  SELECT  ON [dbo].[Subject_Area]  TO [R2k_AddOnly]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Subject_Area]  TO [R2k_FullEdit]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Subject_Area]  TO [R2k_Administrator]
GO

GRANT  SELECT  ON [dbo].[Term]  TO [R2k_ReadOnly]
GO

GRANT  SELECT  ON [dbo].[Term]  TO [R2k_RecordCardsOnly]
GO

GRANT  SELECT  ON [dbo].[Term]  TO [R2k_AddOnly]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Term]  TO [R2k_FullEdit]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Term]  TO [R2k_Administrator]
GO

GRANT  SELECT  ON [dbo].[Term_Version]  TO [R2k_ReadOnly]
GO

GRANT  SELECT  ON [dbo].[Term_Version]  TO [R2k_RecordCardsOnly]
GO

GRANT  SELECT  ON [dbo].[Term_Version]  TO [R2k_AddOnly]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Term_Version]  TO [R2k_FullEdit]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Term_Version]  TO [R2k_Administrator]
GO

GRANT  SELECT  ON [dbo].[Term_Version_Relation]  TO [R2k_ReadOnly]
GO

GRANT  SELECT  ON [dbo].[Term_Version_Relation]  TO [R2k_RecordCardsOnly]
GO

GRANT  SELECT  ON [dbo].[Term_Version_Relation]  TO [R2k_AddOnly]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Term_Version_Relation]  TO [R2k_FullEdit]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Term_Version_Relation]  TO [R2k_Administrator]
GO

GRANT  SELECT  ON [dbo].[Thesaurus_Fact]  TO [R2k_ReadOnly]
GO

GRANT  SELECT  ON [dbo].[Thesaurus_Fact]  TO [R2k_RecordCardsOnly]
GO

GRANT  SELECT  ON [dbo].[Thesaurus_Fact]  TO [R2k_AddOnly]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Thesaurus_Fact]  TO [R2k_FullEdit]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Thesaurus_Fact]  TO [R2k_Administrator]
GO

GRANT  SELECT  ON [dbo].[Thesaurus_Fact]  TO [NBNUser]
GO

GRANT  SELECT  ON [dbo].[Thesaurus_Relation_Type]  TO [R2k_ReadOnly]
GO

GRANT  SELECT  ON [dbo].[Thesaurus_Relation_Type]  TO [R2k_RecordCardsOnly]
GO

GRANT  SELECT  ON [dbo].[Thesaurus_Relation_Type]  TO [R2k_AddOnly]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Thesaurus_Relation_Type]  TO [R2k_FullEdit]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Thesaurus_Relation_Type]  TO [R2k_Administrator]
GO

GRANT  SELECT  ON [dbo].[Thesaurus_Relation_Type_Usage]  TO [R2k_ReadOnly]
GO

GRANT  SELECT  ON [dbo].[Thesaurus_Relation_Type_Usage]  TO [R2k_RecordCardsOnly]
GO

GRANT  SELECT  ON [dbo].[Thesaurus_Relation_Type_Usage]  TO [R2k_AddOnly]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Thesaurus_Relation_Type_Usage]  TO [R2k_FullEdit]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Thesaurus_Relation_Type_Usage]  TO [R2k_Administrator]
GO

GRANT  SELECT  ON [dbo].[User_Domain_Access]  TO [public]
GO

GRANT  SELECT  ON [dbo].[Valuation]  TO [R2k_ReadOnly]
GO

GRANT  SELECT  ON [dbo].[Valuation]  TO [R2k_RecordCardsOnly]
GO

GRANT  SELECT  ON [dbo].[Valuation]  TO [R2k_AddOnly]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Valuation]  TO [R2k_FullEdit]
GO

GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Valuation]  TO [R2k_Administrator]
GO

