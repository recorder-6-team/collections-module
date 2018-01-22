/****** Object:  Index PK_Collection_Unit_Funding on Table [dbo].[Collection_Unit_Funding]    Script Date: 06/01/2006 11:41:13 ******/
if exists (select * from dbo.sysindexes where name = N'PK_Collection_Unit_Funding' and id = object_id(N'[dbo].[Collection_Unit_Funding]'))
drop index [dbo].[Collection_Unit_Funding].[PK_Collection_Unit_Funding]
GO

 CREATE  UNIQUE  CLUSTERED  INDEX [PK_Collection_Unit_Funding] ON [dbo].[Collection_Unit_Funding]([Collection_Unit_Funding_Key]) ON [PRIMARY]
GO

/****** Object:  Index PK_Movement_Conservation_Check on Table [dbo].[Movement_Conservation_Check]    Script Date: 06/01/2006 11:41:13 ******/
if exists (select * from dbo.sysindexes where name = N'PK_Movement_Conservation_Check' and id = object_id(N'[dbo].[Movement_Conservation_Check]'))
drop index [dbo].[Movement_Conservation_Check].[PK_Movement_Conservation_Check]
GO

 CREATE  UNIQUE  CLUSTERED  INDEX [PK_Movement_Conservation_Check] ON [dbo].[Movement_Conservation_Check]([Movement_Conservation_Check_Key]) ON [PRIMARY]
GO