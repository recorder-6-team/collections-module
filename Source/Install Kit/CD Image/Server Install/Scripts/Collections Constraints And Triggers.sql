if not exists (select 1 from SysObjects where name='PK_Collection' AND parent_obj=Object_ID('Collection'))
ALTER TABLE [dbo].[Collection] ADD CONSTRAINT [PK_Collection] PRIMARY KEY  CLUSTERED 
	(
		[Collection_Unit_Key]
	)  ON [PRIMARY] 
GO


if not exists (select 1 from SysObjects where name='PK_Collection_Unit' AND parent_obj=Object_ID('Collection_Unit'))
ALTER TABLE [dbo].[Collection_Unit] ADD CONSTRAINT [PK_Collection_Unit] PRIMARY KEY  CLUSTERED 
	(
		[Collection_Unit_Key]
	)  ON [PRIMARY] 
GO


if not exists (select 1 from SysObjects where name='PK_Collection_Unit_Check' AND parent_obj=Object_ID('Collection_Unit_Check'))
ALTER TABLE [dbo].[Collection_Unit_Check] ADD CONSTRAINT [PK_Collection_Unit_Check] PRIMARY KEY  CLUSTERED 
	(
		[Collection_Unit_Check_Key]
	)  ON [PRIMARY] 
GO


if not exists (select 1 from SysObjects where name='PK_Collection_Unit_Data' AND parent_obj=Object_ID('Collection_Unit_Data'))
ALTER TABLE [dbo].[Collection_Unit_Data] ADD CONSTRAINT [PK_Collection_Unit_Data] PRIMARY KEY  CLUSTERED 
	(
		[Collection_Unit_Data_Key]
	)  ON [PRIMARY] 
GO


if not exists (select 1 from SysObjects where name='PK_Collection_Unit_Enquiry' AND parent_obj=Object_ID('Collection_Unit_Enquiry'))
ALTER TABLE [dbo].[Collection_Unit_Enquiry] ADD CONSTRAINT [PK_Collection_Unit_Enquiry] PRIMARY KEY  CLUSTERED 
	(
		[Collection_Unit_Enquiry_Key]
	)  ON [PRIMARY] 
GO


if not exists (select 1 from SysObjects where name='PK_Collection_Unit_History' AND parent_obj=Object_ID('Collection_Unit_History'))
ALTER TABLE [dbo].[Collection_Unit_History] ADD CONSTRAINT [PK_Collection_Unit_History] PRIMARY KEY  CLUSTERED 
	(
		[Collection_Unit_History_Key]
	)  ON [PRIMARY] 
GO


if not exists (select 1 from SysObjects where name='PK_Specimen_Material' AND parent_obj=Object_ID('Collection_Unit_Material'))
ALTER TABLE [dbo].[Collection_Unit_Material] ADD CONSTRAINT [PK_Specimen_Material] PRIMARY KEY  CLUSTERED 
	(
		[Collection_Unit_Material_Key]
	)  ON [PRIMARY] 
GO


if not exists (select 1 from SysObjects where name='PK_Collection_Unit_Individual' AND parent_obj=Object_ID('Collection_Unit_Name'))
ALTER TABLE [dbo].[Collection_Unit_Name] ADD CONSTRAINT [PK_Collection_Unit_Individual] PRIMARY KEY  CLUSTERED 
	(
		[Collection_Unit_Name_Key]
	)  ON [PRIMARY] 
GO


if not exists (select 1 from SysObjects where name='PK_Collection_Unit_Number' AND parent_obj=Object_ID('Collection_Unit_Number'))
ALTER TABLE [dbo].[Collection_Unit_Number] ADD CONSTRAINT [PK_Collection_Unit_Number] PRIMARY KEY  CLUSTERED 
	(
		[Collection_Unit_Number_Key]
	)  ON [PRIMARY] 
GO


if not exists (select 1 from SysObjects where name='PK_Specimen_Process' AND parent_obj=Object_ID('Collection_Unit_Process'))
ALTER TABLE [dbo].[Collection_Unit_Process] ADD CONSTRAINT [PK_Specimen_Process] PRIMARY KEY  CLUSTERED 
	(
		[Collection_Unit_Process_Key]
	)  ON [PRIMARY] 
GO


if not exists (select 1 from SysObjects where name='PK_Collection_Unit_Relation' AND parent_obj=Object_ID('Collection_Unit_Relation'))
ALTER TABLE [dbo].[Collection_Unit_Relation] ADD CONSTRAINT [PK_Collection_Unit_Relation] PRIMARY KEY  CLUSTERED 
	(
		[Collection_Unit_Relation_Key]
	)  ON [PRIMARY] 
GO


if not exists (select 1 from SysObjects where name='PK_Collection_Unit_Task' AND parent_obj=Object_ID('Collection_Unit_Task'))
ALTER TABLE [dbo].[Collection_Unit_Task] ADD CONSTRAINT [PK_Collection_Unit_Task] PRIMARY KEY  CLUSTERED 
	(
		[Collection_Unit_Task_Key]
	)  ON [PRIMARY] 
GO


if not exists (select 1 from SysObjects where name='PK_Collection_Unit_Valuation' AND parent_obj=Object_ID('Collection_Unit_Valuation'))
ALTER TABLE [dbo].[Collection_Unit_Valuation] ADD CONSTRAINT [PK_Collection_Unit_Valuation] PRIMARY KEY  CLUSTERED 
	(
		[Collection_Unit_Valuation_Key]
	)  ON [PRIMARY] 
GO


if not exists (select 1 from SysObjects where name='PK_Concept' AND parent_obj=Object_ID('Concept'))
ALTER TABLE [dbo].[Concept] ADD CONSTRAINT [PK_Concept] PRIMARY KEY  CLUSTERED 
	(
		[Concept_Key]
	)  ON [PRIMARY] 
GO


if not exists (select 1 from SysObjects where name='PK_Concept_Status' AND parent_obj=Object_ID('Concept_Designation'))
ALTER TABLE [dbo].[Concept_Designation] ADD CONSTRAINT [PK_Concept_Status] PRIMARY KEY  CLUSTERED 
	(
		[Concept_Designation_Key]
	)  ON [PRIMARY] 
GO


if not exists (select 1 from SysObjects where name='PK_Concept_Group' AND parent_obj=Object_ID('Concept_Group'))
ALTER TABLE [dbo].[Concept_Group] ADD CONSTRAINT [PK_Concept_Group] PRIMARY KEY  CLUSTERED 
	(
		[Concept_Group_Key]
	)  ON [PRIMARY] 
GO


if not exists (select 1 from SysObjects where name='PK_Concept_List_Version' AND parent_obj=Object_ID('Concept_Group_Version'))
ALTER TABLE [dbo].[Concept_Group_Version] ADD CONSTRAINT [PK_Concept_List_Version] PRIMARY KEY  CLUSTERED 
	(
		[Concept_Group_Version_Key]
	)  ON [PRIMARY] 
GO


if not exists (select 1 from SysObjects where name='PK_Concept_History' AND parent_obj=Object_ID('Concept_History'))
ALTER TABLE [dbo].[Concept_History] ADD CONSTRAINT [PK_Concept_History] PRIMARY KEY  CLUSTERED 
	(
		[Concept_History_Key]
	)  ON [PRIMARY] 
GO


if not exists (select 1 from SysObjects where name='PK_Concept_Lineage' AND parent_obj=Object_ID('Concept_Lineage'))
ALTER TABLE [dbo].[Concept_Lineage] ADD CONSTRAINT [PK_Concept_Lineage] PRIMARY KEY  NONCLUSTERED 
	(
		[Concept_Key],
		[Lineage_ID]
	)  ON [PRIMARY] 
GO


if not exists (select 1 from SysObjects where name='PK_Concept_Rank' AND parent_obj=Object_ID('Concept_Rank'))
ALTER TABLE [dbo].[Concept_Rank] ADD CONSTRAINT [PK_Concept_Rank] PRIMARY KEY  CLUSTERED 
	(
		[Concept_Rank_Key]
	)  ON [PRIMARY] 
GO


if not exists (select 1 from SysObjects where name='PK_Concept_Relation' AND parent_obj=Object_ID('Concept_Relation'))
ALTER TABLE [dbo].[Concept_Relation] ADD CONSTRAINT [PK_Concept_Relation] PRIMARY KEY  CLUSTERED 
	(
		[Concept_Relation_Key]
	)  ON [PRIMARY] 
GO


if not exists (select 1 from SysObjects where name='PK_Conservation_Check' AND parent_obj=Object_ID('Conservation_Check'))
ALTER TABLE [dbo].[Conservation_Check] ADD CONSTRAINT [PK_Conservation_Check] PRIMARY KEY  CLUSTERED 
	(
		[Conservation_Check_Key]
	)  ON [PRIMARY] 
GO


if not exists (select 1 from SysObjects where name='PK_Job' AND parent_obj=Object_ID('Conservation_Job'))
ALTER TABLE [dbo].[Conservation_Job] ADD CONSTRAINT [PK_Job] PRIMARY KEY  CLUSTERED 
	(
		[Conservation_Job_Key]
	)  ON [PRIMARY] 
GO


if not exists (select 1 from SysObjects where name='PK_Conservation_Job_Funding' AND parent_obj=Object_ID('Conservation_Job_Funding'))
ALTER TABLE [dbo].[Conservation_Job_Funding] ADD CONSTRAINT [PK_Conservation_Job_Funding] PRIMARY KEY  CLUSTERED 
	(
		[Conservation_Job_Funding_Key]
	)  ON [PRIMARY] 
GO


if not exists (select 1 from SysObjects where name='PK_Conservation_Job_Material' AND parent_obj=Object_ID('Conservation_Job_Material'))
ALTER TABLE [dbo].[Conservation_Job_Material] ADD CONSTRAINT [PK_Conservation_Job_Material] PRIMARY KEY  CLUSTERED 
	(
		[Conservation_Job_Material_Key]
	)  ON [PRIMARY] 
GO


if not exists (select 1 from SysObjects where name='PK_Conservation_Job_Conservator' AND parent_obj=Object_ID('Conservation_Job_Staff'))
ALTER TABLE [dbo].[Conservation_Job_Staff] ADD CONSTRAINT [PK_Conservation_Job_Conservator] PRIMARY KEY  CLUSTERED 
	(
		[Conservation_Job_Staff_Key]
	)  ON [PRIMARY] 
GO


if not exists (select 1 from SysObjects where name='PK_Task' AND parent_obj=Object_ID('Conservation_Task'))
ALTER TABLE [dbo].[Conservation_Task] ADD CONSTRAINT [PK_Task] PRIMARY KEY  CLUSTERED 
	(
		[Conservation_Task_Key]
	)  ON [PRIMARY] 
GO


if not exists (select 1 from SysObjects where name='PK_Details_Report' AND parent_obj=Object_ID('Details_Report'))
ALTER TABLE [dbo].[Details_Report] ADD CONSTRAINT [PK_Details_Report] PRIMARY KEY  CLUSTERED 
	(
		[Details_Report_Key]
	)  ON [PRIMARY] 
GO


if not exists (select 1 from SysObjects where name='PK_Determination' AND parent_obj=Object_ID('Determination'))
ALTER TABLE [dbo].[Determination] ADD CONSTRAINT [PK_Determination] PRIMARY KEY  CLUSTERED 
	(
		[Determination_Key]
	)  ON [PRIMARY] 
GO


if not exists (select 1 from SysObjects where name='PK_Domain' AND parent_obj=Object_ID('Domain'))
ALTER TABLE [dbo].[Domain] ADD CONSTRAINT [PK_Domain] PRIMARY KEY  CLUSTERED 
	(
		[Domain_Key]
	)  ON [PRIMARY] 
GO


if not exists (select 1 from SysObjects where name='PK_Domain_Hyperlink' AND parent_obj=Object_ID('Domain_Hyperlink'))
ALTER TABLE [dbo].[Domain_Hyperlink] ADD CONSTRAINT [PK_Domain_Hyperlink] PRIMARY KEY  CLUSTERED 
	(
		[Domain_Hyperlink_Key]
	)  ON [PRIMARY] 
GO


if not exists (select 1 from SysObjects where name='PK_Enquiry' AND parent_obj=Object_ID('Enquiry'))
ALTER TABLE [dbo].[Enquiry] ADD CONSTRAINT [PK_Enquiry] PRIMARY KEY  CLUSTERED 
	(
		[Enquiry_Key]
	)  ON [PRIMARY] 
GO


if not exists (select 1 from SysObjects where name='PK_Enquiry_Concept' AND parent_obj=Object_ID('Enquiry_Concept'))
ALTER TABLE [dbo].[Enquiry_Concept] ADD CONSTRAINT [PK_Enquiry_Concept] PRIMARY KEY  CLUSTERED 
	(
		[Enquiry_Concept_Key]
	)  ON [PRIMARY] 
GO


if not exists (select 1 from SysObjects where name='PK_Import_Export_Job' AND parent_obj=Object_ID('Import_Export_Job'))
ALTER TABLE [dbo].[Import_Export_Job] ADD CONSTRAINT [PK_Import_Export_Job] PRIMARY KEY  CLUSTERED 
	(
		[Import_Export_Job_ID]
	)  ON [PRIMARY] 
GO


if not exists (select 1 from SysObjects where name='PK_Language' AND parent_obj=Object_ID('Language'))
ALTER TABLE [dbo].[Language] ADD CONSTRAINT [PK_Language] PRIMARY KEY  CLUSTERED 
	(
		[Language_Key]
	)  ON [PRIMARY] 
GO


if not exists (select 1 from SysObjects where name='PK_List_Report' AND parent_obj=Object_ID('List_Report'))
ALTER TABLE [dbo].[List_Report] ADD CONSTRAINT [PK_List_Report] PRIMARY KEY  CLUSTERED 
	(
		[List_Report_Key]
	)  ON [PRIMARY] 
GO


if not exists (select 1 from SysObjects where name='PK_Local_Domain' AND parent_obj=Object_ID('Local_Domain'))
ALTER TABLE [dbo].[Local_Domain] ADD CONSTRAINT [PK_Local_Domain] PRIMARY KEY  CLUSTERED 
	(
		[Local_Domain_Key]
	)  ON [PRIMARY] 
GO


if not exists (select 1 from SysObjects where name='PK_Macro' AND parent_obj=Object_ID('Macro'))
ALTER TABLE [dbo].[Macro] ADD CONSTRAINT [PK_Macro] PRIMARY KEY  CLUSTERED 
	(
		[Number_Type]
	)  ON [PRIMARY] 
GO


if not exists (select 1 from SysObjects where name='PK_Macro_Generated_ID' AND parent_obj=Object_ID('Macro_Generated_ID'))
ALTER TABLE [dbo].[Macro_Generated_ID] ADD CONSTRAINT [PK_Macro_Generated_ID] PRIMARY KEY  CLUSTERED 
	(
		[Number_Type],
		[Macro_Output]
	)  ON [PRIMARY] 
GO


if not exists (select 1 from SysObjects where name='PK_Meaning' AND parent_obj=Object_ID('Meaning'))
ALTER TABLE [dbo].[Meaning] ADD CONSTRAINT [PK_Meaning] PRIMARY KEY  CLUSTERED 
	(
		[Meaning_Key]
	)  ON [PRIMARY] 
GO


if not exists (select 1 from SysObjects where name='PK_Meaning_Relation' AND parent_obj=Object_ID('Meaning_Relation'))
ALTER TABLE [dbo].[Meaning_Relation] ADD CONSTRAINT [PK_Meaning_Relation] PRIMARY KEY  CLUSTERED 
	(
		[Meaning_Relation_Key]
	)  ON [PRIMARY] 
GO


if not exists (select 1 from SysObjects where name='PK_Thesaurus_Metadata' AND parent_obj=Object_ID('Metadata'))
ALTER TABLE [dbo].[Metadata] ADD CONSTRAINT [PK_Thesaurus_Metadata] PRIMARY KEY  CLUSTERED 
	(
		[Metadata_Key]
	)  ON [PRIMARY] 
GO


if not exists (select 1 from SysObjects where name='PK_Thesaurus_Metadata_Type' AND parent_obj=Object_ID('Metadata_Type'))
ALTER TABLE [dbo].[Metadata_Type] ADD CONSTRAINT [PK_Thesaurus_Metadata_Type] PRIMARY KEY  CLUSTERED 
	(
		[Metadata_Type_Key]
	)  ON [PRIMARY] 
GO


if not exists (select 1 from SysObjects where name='PK_Movement' AND parent_obj=Object_ID('Movement'))
ALTER TABLE [dbo].[Movement] ADD CONSTRAINT [PK_Movement] PRIMARY KEY  CLUSTERED 
	(
		[Movement_Key]
	)  ON [PRIMARY] 
GO


if not exists (select 1 from SysObjects where name='PK_Movement_Collection_Unit' AND parent_obj=Object_ID('Movement_Collection_Unit'))
ALTER TABLE [dbo].[Movement_Collection_Unit] ADD CONSTRAINT [PK_Movement_Collection_Unit] PRIMARY KEY  CLUSTERED 
	(
		[Movement_Collection_Unit_Key]
	)  ON [PRIMARY] 
GO


if not exists (select 1 from SysObjects where name='PK_Movement_Communication' AND parent_obj=Object_ID('Movement_Communication'))
ALTER TABLE [dbo].[Movement_Communication] ADD CONSTRAINT [PK_Movement_Communication] PRIMARY KEY  CLUSTERED 
	(
		[Movement_Communication_Key]
	)  ON [PRIMARY] 
GO


if not exists (select 1 from SysObjects where name='PK_Movement_Direction' AND parent_obj=Object_ID('Movement_Direction'))
ALTER TABLE [dbo].[Movement_Direction] ADD CONSTRAINT [PK_Movement_Direction] PRIMARY KEY  CLUSTERED 
	(
		[Movement_Direction_Key]
	)  ON [PRIMARY] 
GO


if not exists (select 1 from SysObjects where name='PK_Movement_Enquiry' AND parent_obj=Object_ID('Movement_Enquiry'))
ALTER TABLE [dbo].[Movement_Enquiry] ADD CONSTRAINT [PK_Movement_Enquiry] PRIMARY KEY  CLUSTERED 
	(
		[Movement_Enquiry_Key]
	)  ON [PRIMARY] 
GO


if not exists (select 1 from SysObjects where name='PK_Movement_Funding' AND parent_obj=Object_ID('Movement_Funding'))
ALTER TABLE [dbo].[Movement_Funding] ADD CONSTRAINT [PK_Movement_Funding] PRIMARY KEY  CLUSTERED 
	(
		[Movement_Funding_Key]
	)  ON [PRIMARY] 
GO


if not exists (select 1 from SysObjects where name='PK_Physical_Movement' AND parent_obj=Object_ID('Movement_Of_Material'))
ALTER TABLE [dbo].[Movement_Of_Material] ADD CONSTRAINT [PK_Physical_Movement] PRIMARY KEY  CLUSTERED 
	(
		[Movement_Of_Material_Key]
	)  ON [PRIMARY] 
GO


if not exists (select 1 from SysObjects where name='PK_Movement_Of_Material_Exclusion' AND parent_obj=Object_ID('Movement_Of_Material_Exclusion'))
ALTER TABLE [dbo].[Movement_Of_Material_Exclusion] ADD CONSTRAINT [PK_Movement_Of_Material_Exclusion] PRIMARY KEY  CLUSTERED 
	(
		[Movement_Of_Material_Exclusion_Key]
	)  ON [PRIMARY] 
GO


if not exists (select 1 from SysObjects where name='PK_Ownership_Movement' AND parent_obj=Object_ID('Movement_Of_Ownership'))
ALTER TABLE [dbo].[Movement_Of_Ownership] ADD CONSTRAINT [PK_Ownership_Movement] PRIMARY KEY  CLUSTERED 
	(
		[Movement_Of_Ownership_Key]
	)  ON [PRIMARY] 
GO


if not exists (select 1 from SysObjects where name='PK_Movement_Of_Ownership_Exclusion' AND parent_obj=Object_ID('Movement_Of_Ownership_Exclusion'))
ALTER TABLE [dbo].[Movement_Of_Ownership_Exclusion] ADD CONSTRAINT [PK_Movement_Of_Ownership_Exclusion] PRIMARY KEY  CLUSTERED 
	(
		[Movement_Of_Ownership_Exclusion_Key]
	)  ON [PRIMARY] 
GO


if not exists (select 1 from SysObjects where name='PK_Movement_Valuation' AND parent_obj=Object_ID('Movement_Valuation'))
ALTER TABLE [dbo].[Movement_Valuation] ADD CONSTRAINT [PK_Movement_Valuation] PRIMARY KEY  CLUSTERED 
	(
		[Movement_Valuation_Key]
	)  ON [PRIMARY] 
GO


if not exists (select 1 from SysObjects where name='PK_Occurrence' AND parent_obj=Object_ID('Occurrence'))
ALTER TABLE [dbo].[Occurrence] ADD CONSTRAINT [PK_Occurrence] PRIMARY KEY  CLUSTERED 
	(
		[Occurrence_Key]
	)  ON [PRIMARY] 
GO


if not exists (select 1 from SysObjects where name='PK_Occurrence_Data' AND parent_obj=Object_ID('Occurrence_Data'))
ALTER TABLE [dbo].[Occurrence_Data] ADD CONSTRAINT [PK_Occurrence_Data] PRIMARY KEY  CLUSTERED 
	(
		[Occurrence_Data_Key]
	)  ON [PRIMARY] 
GO


if not exists (select 1 from SysObjects where name='PK_Occurrence_Relation' AND parent_obj=Object_ID('Occurrence_Relation'))
ALTER TABLE [dbo].[Occurrence_Relation] ADD CONSTRAINT [PK_Occurrence_Relation] PRIMARY KEY  CLUSTERED 
	(
		[Occurrence_Relation_Key]
	)  ON [PRIMARY] 
GO


if not exists (select 1 from SysObjects where name='PK_QE_Data_Item' AND parent_obj=Object_ID('QE_Data_Item'))
ALTER TABLE [dbo].[QE_Data_Item] ADD CONSTRAINT [PK_QE_Data_Item] PRIMARY KEY  CLUSTERED 
	(
		[QE_Data_Item_Key]
	)  ON [PRIMARY] 
GO


if not exists (select 1 from SysObjects where name='PK_QE_Data_Row' AND parent_obj=Object_ID('QE_Data_Row'))
ALTER TABLE [dbo].[QE_Data_Row] ADD CONSTRAINT [PK_QE_Data_Row] PRIMARY KEY  CLUSTERED 
	(
		[QE_Data_Row_Key]
	)  ON [PRIMARY] 
GO


if not exists (select 1 from SysObjects where name='PK_QE_Field' AND parent_obj=Object_ID('QE_Field'))
ALTER TABLE [dbo].[QE_Field] ADD CONSTRAINT [PK_QE_Field] PRIMARY KEY  CLUSTERED 
	(
		[QE_Field_Key]
	)  ON [PRIMARY] 
GO


if not exists (select 1 from SysObjects where name='PK_QE_Session' AND parent_obj=Object_ID('QE_Session'))
ALTER TABLE [dbo].[QE_Session] ADD CONSTRAINT [PK_QE_Session] PRIMARY KEY  CLUSTERED 
	(
		[QE_Session_Key]
	)  ON [PRIMARY] 
GO


if not exists (select 1 from SysObjects where name='PK_QE_Template' AND parent_obj=Object_ID('QE_Template'))
ALTER TABLE [dbo].[QE_Template] ADD CONSTRAINT [PK_QE_Template] PRIMARY KEY  CLUSTERED 
	(
		[QE_Template_Key]
	)  ON [PRIMARY] 
GO


if not exists (select 1 from SysObjects where name='PK_QE_Template_Field' AND parent_obj=Object_ID('QE_Template_Field'))
ALTER TABLE [dbo].[QE_Template_Field] ADD CONSTRAINT [PK_QE_Template_Field] PRIMARY KEY  CLUSTERED 
	(
		[QE_Template_Field_Key]
	)  ON [PRIMARY] 
GO


if not exists (select 1 from SysObjects where name='PK_Report_Block' AND parent_obj=Object_ID('Report_Block'))
ALTER TABLE [dbo].[Report_Block] ADD CONSTRAINT [PK_Report_Block] PRIMARY KEY  CLUSTERED 
	(
		[Report_Block_Key]
	)  ON [PRIMARY] 
GO


if not exists (select 1 from SysObjects where name='PK_Report_Block_In_Section' AND parent_obj=Object_ID('Report_Block_In_Section'))
ALTER TABLE [dbo].[Report_Block_In_Section] ADD CONSTRAINT [PK_Report_Block_In_Section] PRIMARY KEY  CLUSTERED 
	(
		[Report_Block_In_Section_Key]
	)  ON [PRIMARY] 
GO


if not exists (select 1 from SysObjects where name='PK_Report_Block_Order' AND parent_obj=Object_ID('Report_Block_Order'))
ALTER TABLE [dbo].[Report_Block_Order] ADD CONSTRAINT [PK_Report_Block_Order] PRIMARY KEY  CLUSTERED 
	(
		[Report_Block_Order_Key]
	)  ON [PRIMARY] 
GO


if not exists (select 1 from SysObjects where name='PK_Report_Section' AND parent_obj=Object_ID('Report_Section'))
ALTER TABLE [dbo].[Report_Section] ADD CONSTRAINT [PK_Report_Section] PRIMARY KEY  CLUSTERED 
	(
		[Report_Section_Key]
	)  ON [PRIMARY] 
GO


if not exists (select 1 from SysObjects where name='PK_Relationship_Type_Semantic' AND parent_obj=Object_ID('Semantic_Relation'))
ALTER TABLE [dbo].[Semantic_Relation] ADD CONSTRAINT [PK_Relationship_Type_Semantic] PRIMARY KEY  CLUSTERED 
	(
		[Semantic_Relation_Key]
	)  ON [PRIMARY] 
GO


if not exists (select 1 from SysObjects where name='PK_Session' AND parent_obj=Object_ID('Session'))
ALTER TABLE [dbo].[Session] ADD CONSTRAINT [PK_Session] PRIMARY KEY  CLUSTERED 
	(
		[Session_ID]
	)  ON [PRIMARY] 
GO


if not exists (select 1 from SysObjects where name='PK_Source_Join' AND parent_obj=Object_ID('Source_Join'))
ALTER TABLE [dbo].[Source_Join] ADD CONSTRAINT [PK_Source_Join] PRIMARY KEY  CLUSTERED 
	(
		[Source_Join_Key]
	)  ON [PRIMARY] 
GO


if not exists (select 1 from SysObjects where name='PK_Specimen_Field_Data' AND parent_obj=Object_ID('Specimen_Field_Data'))
ALTER TABLE [dbo].[Specimen_Field_Data] ADD CONSTRAINT [PK_Specimen_Field_Data] PRIMARY KEY  CLUSTERED 
	(
		[Specimen_Field_Data_Key]
	)  ON [PRIMARY] 
GO


if not exists (select 1 from SysObjects where name='PK_Specimen_Label' AND parent_obj=Object_ID('Specimen_Label'))
ALTER TABLE [dbo].[Specimen_Label] ADD CONSTRAINT [PK_Specimen_Label] PRIMARY KEY  CLUSTERED 
	(
		[Specimen_Label_Key]
	)  ON [PRIMARY] 
GO


if not exists (select 1 from SysObjects where name='PK_Specimen_Unit' AND parent_obj=Object_ID('Specimen_Unit'))
ALTER TABLE [dbo].[Specimen_Unit] ADD CONSTRAINT [PK_Specimen_Unit] PRIMARY KEY  CLUSTERED 
	(
		[Collection_Unit_Key]
	)  ON [PRIMARY] 
GO


if not exists (select 1 from SysObjects where name='PK_Store' AND parent_obj=Object_ID('Store'))
ALTER TABLE [dbo].[Store] ADD CONSTRAINT [PK_Store] PRIMARY KEY  CLUSTERED 
	(
		[Collection_Unit_Key]
	)  ON [PRIMARY] 
GO


if not exists (select 1 from SysObjects where name='PK_Subject_Area' AND parent_obj=Object_ID('Subject_Area'))
ALTER TABLE [dbo].[Subject_Area] ADD CONSTRAINT [PK_Subject_Area] PRIMARY KEY  CLUSTERED 
	(
		[Subject_Area_Key]
	)  ON [PRIMARY] 
GO


if not exists (select 1 from SysObjects where name='PK_Taxon_Dictionary_Concept_Designation_Mapping' AND parent_obj=Object_ID('Taxon_Dictionary_Concept_Designation_Mapping'))
ALTER TABLE [dbo].[Taxon_Dictionary_Concept_Designation_Mapping] ADD CONSTRAINT [PK_Taxon_Dictionary_Concept_Designation_Mapping] PRIMARY KEY  CLUSTERED 
	(
		[Taxon_Designation_Key]
	)  ON [PRIMARY] 
GO


if not exists (select 1 from SysObjects where name='PK_Taxon_Dictionary_Concept_Group_Mapping' AND parent_obj=Object_ID('Taxon_Dictionary_Concept_Group_Mapping'))
ALTER TABLE [dbo].[Taxon_Dictionary_Concept_Group_Mapping] ADD CONSTRAINT [PK_Taxon_Dictionary_Concept_Group_Mapping] PRIMARY KEY  CLUSTERED 
	(
		[Taxon_List_Key]
	)  ON [PRIMARY] 
GO


if not exists (select 1 from SysObjects where name='PK_Taxon_Dictionary_Concept_Group_Version_Mapping' AND parent_obj=Object_ID('Taxon_Dictionary_Concept_Group_Version_Mapping'))
ALTER TABLE [dbo].[Taxon_Dictionary_Concept_Group_Version_Mapping] ADD CONSTRAINT [PK_Taxon_Dictionary_Concept_Group_Version_Mapping] PRIMARY KEY  CLUSTERED 
	(
		[Taxon_List_Version_Key]
	)  ON [PRIMARY] 
GO


if not exists (select 1 from SysObjects where name='PK_Taxon_Dictionary_Concept_Mapping' AND parent_obj=Object_ID('Taxon_Dictionary_Concept_Mapping'))
ALTER TABLE [dbo].[Taxon_Dictionary_Concept_Mapping] ADD CONSTRAINT [PK_Taxon_Dictionary_Concept_Mapping] PRIMARY KEY  CLUSTERED 
	(
		[Taxon_List_Item_Key]
	)  ON [PRIMARY] 
GO


if not exists (select 1 from SysObjects where name='PK_Taxon_Dictionary_Concept_Rank_Mapping' AND parent_obj=Object_ID('Taxon_Dictionary_Concept_Rank_Mapping'))
ALTER TABLE [dbo].[Taxon_Dictionary_Concept_Rank_Mapping] ADD CONSTRAINT [PK_Taxon_Dictionary_Concept_Rank_Mapping] PRIMARY KEY  CLUSTERED 
	(
		[Taxon_Rank_Key]
	)  ON [PRIMARY] 
GO


if not exists (select 1 from SysObjects where name='PK_Taxon_Dictionary_Designation_Type_Mapping' AND parent_obj=Object_ID('Taxon_Dictionary_Designation_Type_Mapping'))
ALTER TABLE [dbo].[Taxon_Dictionary_Designation_Type_Mapping] ADD CONSTRAINT [PK_Taxon_Dictionary_Designation_Type_Mapping] PRIMARY KEY  CLUSTERED 
	(
		[Taxon_Designation_Type_Key]
	)  ON [PRIMARY] 
GO


if not exists (select 1 from SysObjects where name='PK_Taxon_Dictionary_Meaning_Mapping' AND parent_obj=Object_ID('Taxon_Dictionary_Meaning_Mapping'))
ALTER TABLE [dbo].[Taxon_Dictionary_Meaning_Mapping] ADD CONSTRAINT [PK_Taxon_Dictionary_Meaning_Mapping] PRIMARY KEY  CLUSTERED 
	(
		[Preferred_Name]
	)  ON [PRIMARY] 
GO


if not exists (select 1 from SysObjects where name='PK_Taxon_Dictionary_Name_Type_Mapping' AND parent_obj=Object_ID('Taxon_Dictionary_Name_Type_Mapping'))
ALTER TABLE [dbo].[Taxon_Dictionary_Name_Type_Mapping] ADD CONSTRAINT [PK_Taxon_Dictionary_Name_Type_Mapping] PRIMARY KEY  CLUSTERED 
	(
		[Taxon_Name_Type_Key]
	)  ON [PRIMARY] 
GO


if not exists (select 1 from SysObjects where name='PK_Taxon_Dictionary_Term_Sources_Mapping' AND parent_obj=Object_ID('Taxon_Dictionary_Term_Sources_Mapping'))
ALTER TABLE [dbo].[Taxon_Dictionary_Term_Sources_Mapping] ADD CONSTRAINT [PK_Taxon_Dictionary_Term_Sources_Mapping] PRIMARY KEY  CLUSTERED 
	(
		[Source_Link_Key]
	)  ON [PRIMARY] 
GO


if not exists (select 1 from SysObjects where name='PK_Taxon_Dictionary_Term_Version_Mapping' AND parent_obj=Object_ID('Taxon_Dictionary_Term_Version_Mapping'))
ALTER TABLE [dbo].[Taxon_Dictionary_Term_Version_Mapping] ADD CONSTRAINT [PK_Taxon_Dictionary_Term_Version_Mapping] PRIMARY KEY  CLUSTERED 
	(
		[Taxon_Version_Key]
	)  ON [PRIMARY] 
GO


if not exists (select 1 from SysObjects where name='PK_Taxon_Dictionary_Thesaurus_Fact_Mapping' AND parent_obj=Object_ID('Taxon_Dictionary_Thesaurus_Fact_Mapping'))
ALTER TABLE [dbo].[Taxon_Dictionary_Thesaurus_Fact_Mapping] ADD CONSTRAINT [PK_Taxon_Dictionary_Thesaurus_Fact_Mapping] PRIMARY KEY  CLUSTERED 
	(
		[Taxon_Fact_Key]
	)  ON [PRIMARY] 
GO


if not exists (select 1 from SysObjects where name='PK_Term' AND parent_obj=Object_ID('Term'))
ALTER TABLE [dbo].[Term] ADD CONSTRAINT [PK_Term] PRIMARY KEY  CLUSTERED 
	(
		[Term_Key]
	)  ON [PRIMARY] 
GO


if not exists (select 1 from SysObjects where name='PK_Author_And_Version' AND parent_obj=Object_ID('Term_Version'))
ALTER TABLE [dbo].[Term_Version] ADD CONSTRAINT [PK_Author_And_Version] PRIMARY KEY  CLUSTERED 
	(
		[Term_Version_Key]
	)  ON [PRIMARY] 
GO


if not exists (select 1 from SysObjects where name='PK_Term_Version_Relation' AND parent_obj=Object_ID('Term_Version_Relation'))
ALTER TABLE [dbo].[Term_Version_Relation] ADD CONSTRAINT [PK_Term_Version_Relation] PRIMARY KEY  CLUSTERED 
	(
		[Term_Version_Relation_Key]
	)  ON [PRIMARY] 
GO


if not exists (select 1 from SysObjects where name='PK_Thesaurus_Fact' AND parent_obj=Object_ID('Thesaurus_Fact'))
ALTER TABLE [dbo].[Thesaurus_Fact] ADD CONSTRAINT [PK_Thesaurus_Fact] PRIMARY KEY  CLUSTERED 
	(
		[Thesaurus_Fact_Key]
	)  ON [PRIMARY] 
GO


if not exists (select 1 from SysObjects where name='PK_Symantic_Relation_Type' AND parent_obj=Object_ID('Thesaurus_Relation_Type'))
ALTER TABLE [dbo].[Thesaurus_Relation_Type] ADD CONSTRAINT [PK_Symantic_Relation_Type] PRIMARY KEY  CLUSTERED 
	(
		[Thesaurus_Relation_Type_Key]
	)  ON [PRIMARY] 
GO


if not exists (select 1 from SysObjects where name='PK_Thesaurus_Relation_Type_Usage' AND parent_obj=Object_ID('Thesaurus_Relation_Type_Usage'))
ALTER TABLE [dbo].[Thesaurus_Relation_Type_Usage] ADD CONSTRAINT [PK_Thesaurus_Relation_Type_Usage] PRIMARY KEY  CLUSTERED 
	(
		[Thesaurus_Relation_Type_Usage_Key]
	)  ON [PRIMARY] 
GO


if not exists (select 1 from SysObjects where name='PK_User_Domain_Access' AND parent_obj=Object_ID('User_Domain_Access'))
ALTER TABLE [dbo].[User_Domain_Access] ADD CONSTRAINT [PK_User_Domain_Access] PRIMARY KEY  CLUSTERED 
	(
		[User_Domain_Access_Key]
	)  ON [PRIMARY] 
GO


if not exists (select 1 from SysObjects where name='PK_Valuation' AND parent_obj=Object_ID('Valuation'))
ALTER TABLE [dbo].[Valuation] ADD CONSTRAINT [PK_Valuation] PRIMARY KEY  CLUSTERED 
	(
		[Valuation_Key]
	)  ON [PRIMARY] 
GO


if not exists (select 1 from SysObjects where name='FK_Collection_Collection' AND parent_obj=Object_ID('Collection'))
ALTER TABLE [dbo].[Collection] ADD CONSTRAINT [FK_Collection_Collection] FOREIGN KEY 
	(
		[Parent_Collection_Collection_Unit_Key]
	) REFERENCES [Collection] (
		[Collection_Unit_Key]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Collection_Collection_Unit' AND parent_obj=Object_ID('Collection'))
ALTER TABLE [dbo].[Collection] ADD CONSTRAINT [FK_Collection_Collection_Unit] FOREIGN KEY 
	(
		[Collection_Unit_Key]
	) REFERENCES [Collection_Unit] (
		[Collection_Unit_Key]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Collection_Name' AND parent_obj=Object_ID('Collection'))
ALTER TABLE [dbo].[Collection] ADD CONSTRAINT [FK_Collection_Name] FOREIGN KEY 
	(
		[Assembler_Name_Key]
	) REFERENCES [NAME] (
		[NAME_KEY]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Collection_Risk' AND parent_obj=Object_ID('Collection'))
ALTER TABLE [dbo].[Collection] ADD CONSTRAINT [FK_Collection_Risk] FOREIGN KEY 
	(
		[Risk_Concept_Key]
	) REFERENCES [Concept] (
		[Concept_Key]
	)
GO


if not exists (select 1 from dbo.sysindexes where name = N'IX_Collection_Parent' and id = object_id(N'[dbo].[Collection]'))
 CREATE  INDEX [IX_Collection_Parent] ON [dbo].[Collection]([Parent_Collection_Collection_Unit_Key]) ON [PRIMARY]
GO


if not exists (select 1 from SysObjects where name='FK_Collection_Unit_Current_Store' AND parent_obj=Object_ID('Collection_Unit'))
ALTER TABLE [dbo].[Collection_Unit] ADD CONSTRAINT [FK_Collection_Unit_Current_Store] FOREIGN KEY 
	(
		[Current_Container_Collection_Unit_Key]
	) REFERENCES [Collection_Unit] (
		[Collection_Unit_Key]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Collection_Unit_Usual_Store' AND parent_obj=Object_ID('Collection_Unit'))
ALTER TABLE [dbo].[Collection_Unit] ADD CONSTRAINT [FK_Collection_Unit_Usual_Store] FOREIGN KEY 
	(
		[Usual_Container_Collection_Unit_Key]
	) REFERENCES [Collection_Unit] (
		[Collection_Unit_Key]
	)
GO


SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Collection_UnitCustodianInsert]') and OBJECTPROPERTY(id, N'IsTrigger') = 1)
drop trigger [dbo].[Collection_UnitCustodianInsert]
GO


CREATE TRIGGER Collection_UnitCustodianInsert ON dbo.Collection_Unit AFTER INSERT AS UPDATE Collection_Unit SET Collection_Unit.CUSTODIAN = SUBSTRING(Collection_Unit.Collection_Unit_Key, 1, 8) FROM Collection_Unit INNER JOIN INSERTED ON Collection_Unit.Collection_Unit_Key = INSERTED.Collection_Unit_Key WHERE Collection_Unit.CUSTODIAN IS NULL


GO
SET QUOTED_IDENTIFIER OFF 
GO
SET ANSI_NULLS ON 
GO


if not exists (select 1 from SysObjects where name='DF_Collection_Unit_Domain_Bitfield' AND Type='D')
ALTER TABLE [dbo].[Collection_Unit] ADD CONSTRAINT [DF_Collection_Unit_Domain_Bitfield] DEFAULT (0) FOR [Domain_Mask]
if not exists (select 1 from SysObjects where name='FK_Collection_Unit_Check_Collection_Unit' AND parent_obj=Object_ID('Collection_Unit_Check'))
ALTER TABLE [dbo].[Collection_Unit_Check] ADD CONSTRAINT [FK_Collection_Unit_Check_Collection_Unit] FOREIGN KEY 
	(
		[Collection_Unit_Key]
	) REFERENCES [Collection_Unit] (
		[Collection_Unit_Key]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Collection_Unit_Check_Conservation_Check' AND parent_obj=Object_ID('Collection_Unit_Check'))
ALTER TABLE [dbo].[Collection_Unit_Check] ADD CONSTRAINT [FK_Collection_Unit_Check_Conservation_Check] FOREIGN KEY 
	(
		[Conservation_Check_Key]
	) REFERENCES [Conservation_Check] (
		[Conservation_Check_Key]
	)
GO


SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[tr_CollectionUnitCheck_ForDomainMask]') and OBJECTPROPERTY(id, N'IsTrigger') = 1)
drop trigger [dbo].[tr_CollectionUnitCheck_ForDomainMask]
GO



/*===========================================================================*\
  Description:	Update Domain_Mask of all checks linked to changed records in 
		Collection_Unit_Check.

  Type:		AFTER INSERT, UPDATE and DELETE

  Created:	September 2003

  Last revision information:
    $Revision: 2 $
    $Date: 9/01/06 13:42 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE TRIGGER [dbo].[tr_CollectionUnitCheck_ForDomainMask]
ON [dbo].[Collection_Unit_Check]
AFTER INSERT, UPDATE, DELETE
AS 
	DECLARE	@CheckKey char(16)

	/*-------------------------------------------------------------*\
	  Only the Check key is needed to get to the jobs needing 
	  updating. But we need to through all keys, hence the cursor.
	\*-------------------------------------------------------------*/
	DECLARE curChecks CURSOR LOCAL FAST_FORWARD FOR
		SELECT	DISTINCT Conservation_Check_Key
		FROM	Deleted
		WHERE	Conservation_Check_Key NOT IN (SELECT Conservation_Check_Key FROM Inserted)
		UNION
		SELECT	DISTINCT Conservation_Check_Key
		FROM 	Inserted	
	
	/*-------------------------------------------------------------*\
	  Go and do it now.
	\*-------------------------------------------------------------*/
	OPEN curChecks
	FETCH NEXT FROM curChecks INTO @CheckKey
	WHILE @@Fetch_Status = 0
	BEGIN
		EXECUTE	usp_ConservationCheck_Update_DomainMask NULL, @CheckKey
	
		FETCH NEXT FROM curChecks INTO @CheckKey
	END
	
	-- Cleanup
	CLOSE curChecks
	DEALLOCATE curChecks


GO
SET QUOTED_IDENTIFIER OFF 
GO
SET ANSI_NULLS ON 
GO


if not exists (select 1 from SysObjects where name='FK_Collection_Unit_Data_Collection_Unit' AND parent_obj=Object_ID('Collection_Unit_Data'))
ALTER TABLE [dbo].[Collection_Unit_Data] ADD CONSTRAINT [FK_Collection_Unit_Data_Collection_Unit] FOREIGN KEY 
	(
		[Collection_Unit_Key]
	) REFERENCES [Collection_Unit] (
		[Collection_Unit_Key]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Collection_Unit_Data_Method' AND parent_obj=Object_ID('Collection_Unit_Data'))
ALTER TABLE [dbo].[Collection_Unit_Data] ADD CONSTRAINT [FK_Collection_Unit_Data_Method] FOREIGN KEY 
	(
		[Method_Concept_Key]
	) REFERENCES [Concept] (
		[Concept_Key]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Collection_Unit_Data_Parameter' AND parent_obj=Object_ID('Collection_Unit_Data'))
ALTER TABLE [dbo].[Collection_Unit_Data] ADD CONSTRAINT [FK_Collection_Unit_Data_Parameter] FOREIGN KEY 
	(
		[Parameter_Concept_Key]
	) REFERENCES [Concept] (
		[Concept_Key]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Collection_Unit_Data_Unit' AND parent_obj=Object_ID('Collection_Unit_Data'))
ALTER TABLE [dbo].[Collection_Unit_Data] ADD CONSTRAINT [FK_Collection_Unit_Data_Unit] FOREIGN KEY 
	(
		[Unit_Concept_Key]
	) REFERENCES [Concept] (
		[Concept_Key]
	)
GO


SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Collection_Unit_DataCustodianInsert]') and OBJECTPROPERTY(id, N'IsTrigger') = 1)
drop trigger [dbo].[Collection_Unit_DataCustodianInsert]
GO


CREATE TRIGGER Collection_Unit_DataCustodianInsert ON dbo.Collection_Unit_Data AFTER INSERT AS UPDATE Collection_Unit_Data SET Collection_Unit_Data.CUSTODIAN = SUBSTRING(Collection_Unit_Data.Collection_Unit_Data_Key, 1, 8) FROM Collection_Unit_Data INNER JOIN INSERTED ON Collection_Unit_Data.Collection_Unit_Data_Key = INSERTED.Collection_Unit_Data_Key WHERE Collection_Unit_Data.CUSTODIAN IS NULL


GO
SET QUOTED_IDENTIFIER OFF 
GO
SET ANSI_NULLS ON 
GO


if not exists (select 1 from SysObjects where name='FK_Collection_Unit_Enquiry_Collection_Unit' AND parent_obj=Object_ID('Collection_Unit_Enquiry'))
ALTER TABLE [dbo].[Collection_Unit_Enquiry] ADD CONSTRAINT [FK_Collection_Unit_Enquiry_Collection_Unit] FOREIGN KEY 
	(
		[Collection_Unit_Key]
	) REFERENCES [Collection_Unit] (
		[Collection_Unit_Key]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Collection_Unit_Enquiry_Enquiry' AND parent_obj=Object_ID('Collection_Unit_Enquiry'))
ALTER TABLE [dbo].[Collection_Unit_Enquiry] ADD CONSTRAINT [FK_Collection_Unit_Enquiry_Enquiry] FOREIGN KEY 
	(
		[Enquiry_Key]
	) REFERENCES [Enquiry] (
		[Enquiry_Key]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Collection_Unit_Funding_Collection_Unit' AND parent_obj=Object_ID('Collection_Unit_Funding'))
ALTER TABLE [dbo].[Collection_Unit_Funding] ADD CONSTRAINT [FK_Collection_Unit_Funding_Collection_Unit] FOREIGN KEY 
	(
		[Collection_Unit_Key]
	) REFERENCES [Collection_Unit] (
		[Collection_Unit_Key]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Collection_Unit_Funding_Currency' AND parent_obj=Object_ID('Collection_Unit_Funding'))
ALTER TABLE [dbo].[Collection_Unit_Funding] ADD CONSTRAINT [FK_Collection_Unit_Funding_Currency] FOREIGN KEY 
	(
		[Currency_Concept_Key]
	) REFERENCES [Concept] (
		[Concept_Key]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Collection_Unit_Funding_NAME' AND parent_obj=Object_ID('Collection_Unit_Funding'))
ALTER TABLE [dbo].[Collection_Unit_Funding] ADD CONSTRAINT [FK_Collection_Unit_Funding_NAME] FOREIGN KEY 
	(
		[Funded_By_Name_Key]
	) REFERENCES [NAME] (
		[NAME_KEY]
	)
GO


SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Collection_Unit_FundingCustodianInsert]') and OBJECTPROPERTY(id, N'IsTrigger') = 1)
drop trigger [dbo].[Collection_Unit_FundingCustodianInsert]
GO


CREATE TRIGGER Collection_Unit_FundingCustodianInsert ON dbo.Collection_Unit_Funding AFTER INSERT AS UPDATE Collection_Unit_Funding SET Collection_Unit_Funding.CUSTODIAN = SUBSTRING(Collection_Unit_Funding.Collection_Unit_Funding_Key, 1, 8) FROM Collection_Unit_Funding INNER JOIN INSERTED ON Collection_Unit_Funding.Collection_Unit_Funding_Key = INSERTED.Collection_Unit_Funding_Key WHERE Collection_Unit_Funding.CUSTODIAN IS NULL


GO
SET QUOTED_IDENTIFIER OFF 
GO
SET ANSI_NULLS ON 
GO


if not exists (select 1 from SysObjects where name='FK_Collection_Unit_History_Collection_Unit' AND parent_obj=Object_ID('Collection_Unit_History'))
ALTER TABLE [dbo].[Collection_Unit_History] ADD CONSTRAINT [FK_Collection_Unit_History_Collection_Unit] FOREIGN KEY 
	(
		[Collection_Unit_Key]
	) REFERENCES [Collection_Unit] (
		[Collection_Unit_Key]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Collection_Unit_History_Individual' AND parent_obj=Object_ID('Collection_Unit_History'))
ALTER TABLE [dbo].[Collection_Unit_History] ADD CONSTRAINT [FK_Collection_Unit_History_Individual] FOREIGN KEY 
	(
		[Source_Name_Key]
	) REFERENCES [INDIVIDUAL] (
		[NAME_KEY]
	)
GO


SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Collection_Unit_HistoryCustodianInsert]') and OBJECTPROPERTY(id, N'IsTrigger') = 1)
drop trigger [dbo].[Collection_Unit_HistoryCustodianInsert]
GO


CREATE TRIGGER Collection_Unit_HistoryCustodianInsert ON dbo.Collection_Unit_History AFTER INSERT AS UPDATE Collection_Unit_History SET Collection_Unit_History.CUSTODIAN = SUBSTRING(Collection_Unit_History.Collection_Unit_History_Key, 1, 8) FROM Collection_Unit_History INNER JOIN INSERTED ON Collection_Unit_History.Collection_Unit_History_Key = INSERTED.Collection_Unit_History_Key WHERE Collection_Unit_History.CUSTODIAN IS NULL


GO
SET QUOTED_IDENTIFIER OFF 
GO
SET ANSI_NULLS ON 
GO


if not exists (select 1 from SysObjects where name='FK_Collection_Unit_Material_Collection_Unit' AND parent_obj=Object_ID('Collection_Unit_Material'))
ALTER TABLE [dbo].[Collection_Unit_Material] ADD CONSTRAINT [FK_Collection_Unit_Material_Collection_Unit] FOREIGN KEY 
	(
		[Collection_Unit_Key]
	) REFERENCES [Collection_Unit] (
		[Collection_Unit_Key]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Specimen_Material_Material' AND parent_obj=Object_ID('Collection_Unit_Material'))
ALTER TABLE [dbo].[Collection_Unit_Material] ADD CONSTRAINT [FK_Specimen_Material_Material] FOREIGN KEY 
	(
		[Material_Concept_Key]
	) REFERENCES [Concept] (
		[Concept_Key]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Specimen_Material_Unit' AND parent_obj=Object_ID('Collection_Unit_Material'))
ALTER TABLE [dbo].[Collection_Unit_Material] ADD CONSTRAINT [FK_Specimen_Material_Unit] FOREIGN KEY 
	(
		[Unit_Concept_Key]
	) REFERENCES [Concept] (
		[Concept_Key]
	)
GO


SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Collection_Unit_MaterialCustodianInsert]') and OBJECTPROPERTY(id, N'IsTrigger') = 1)
drop trigger [dbo].[Collection_Unit_MaterialCustodianInsert]
GO


 CREATE TRIGGER Collection_Unit_MaterialCustodianInsert ON dbo.Collection_Unit_Material AFTER INSERT AS UPDATE Collection_Unit_Material SET Collection_Unit_Material.CUSTODIAN = SUBSTRING(Collection_Unit_Material.Collection_Unit_Material_Key, 1, 8) FROM Collection_Unit_Material INNER JOIN INSERTED ON Collection_Unit_Material.Collection_Unit_Material_Key = INSERTED.Collection_Unit_Material_Key WHERE Collection_Unit_Material.CUSTODIAN IS NULL

GO
SET QUOTED_IDENTIFIER OFF 
GO
SET ANSI_NULLS ON 
GO


if not exists (select 1 from SysObjects where name='FK_Collection_Unit_Name_Collection_Unit' AND parent_obj=Object_ID('Collection_Unit_Name'))
ALTER TABLE [dbo].[Collection_Unit_Name] ADD CONSTRAINT [FK_Collection_Unit_Name_Collection_Unit] FOREIGN KEY 
	(
		[Collection_Unit_Key]
	) REFERENCES [Collection_Unit] (
		[Collection_Unit_Key]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Collection_Unit_Name_Concept' AND parent_obj=Object_ID('Collection_Unit_Name'))
ALTER TABLE [dbo].[Collection_Unit_Name] ADD CONSTRAINT [FK_Collection_Unit_Name_Concept] FOREIGN KEY 
	(
		[Relation_Type_Concept_Key]
	) REFERENCES [Concept] (
		[Concept_Key]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Collection_Unit_Name_Name' AND parent_obj=Object_ID('Collection_Unit_Name'))
ALTER TABLE [dbo].[Collection_Unit_Name] ADD CONSTRAINT [FK_Collection_Unit_Name_Name] FOREIGN KEY 
	(
		[Name_Key]
	) REFERENCES [NAME] (
		[NAME_KEY]
	)
GO


SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Collection_Unit_NameCustodianInsert]') and OBJECTPROPERTY(id, N'IsTrigger') = 1)
drop trigger [dbo].[Collection_Unit_NameCustodianInsert]
GO


CREATE TRIGGER Collection_Unit_NameCustodianInsert ON dbo.Collection_Unit_Name AFTER INSERT AS UPDATE Collection_Unit_Name SET Collection_Unit_Name.CUSTODIAN = SUBSTRING(Collection_Unit_Name.Collection_Unit_Name_Key, 1, 8) FROM Collection_Unit_Name INNER JOIN INSERTED ON Collection_Unit_Name.Collection_Unit_Name_Key = INSERTED.Collection_Unit_Name_Key WHERE Collection_Unit_Name.CUSTODIAN IS NULL


GO
SET QUOTED_IDENTIFIER OFF 
GO
SET ANSI_NULLS ON 
GO


if not exists (select 1 from SysObjects where name='FK_Collection_Unit_Number_Collection_Unit' AND parent_obj=Object_ID('Collection_Unit_Number'))
ALTER TABLE [dbo].[Collection_Unit_Number] ADD CONSTRAINT [FK_Collection_Unit_Number_Collection_Unit] FOREIGN KEY 
	(
		[Collection_Unit_Key]
	) REFERENCES [Collection_Unit] (
		[Collection_Unit_Key]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Collection_Unit_Number_Type' AND parent_obj=Object_ID('Collection_Unit_Number'))
ALTER TABLE [dbo].[Collection_Unit_Number] ADD CONSTRAINT [FK_Collection_Unit_Number_Type] FOREIGN KEY 
	(
		[Type_Concept_Key]
	) REFERENCES [Concept] (
		[Concept_Key]
	)
GO


SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Collection_Unit_NumberCustodianInsert]') and OBJECTPROPERTY(id, N'IsTrigger') = 1)
drop trigger [dbo].[Collection_Unit_NumberCustodianInsert]
GO


CREATE TRIGGER Collection_Unit_NumberCustodianInsert ON dbo.Collection_Unit_Number AFTER INSERT AS UPDATE Collection_Unit_Number SET Collection_Unit_Number.CUSTODIAN = SUBSTRING(Collection_Unit_Number.Collection_Unit_Number_Key, 1, 8) FROM Collection_Unit_Number INNER JOIN INSERTED ON Collection_Unit_Number.Collection_Unit_Number_Key = INSERTED.Collection_Unit_Number_Key WHERE Collection_Unit_Number.CUSTODIAN IS NULL


GO
SET QUOTED_IDENTIFIER OFF 
GO
SET ANSI_NULLS ON 
GO


if not exists (select 1 from SysObjects where name='FK_Collection_Unit_Process_Collection_Unit' AND parent_obj=Object_ID('Collection_Unit_Process'))
ALTER TABLE [dbo].[Collection_Unit_Process] ADD CONSTRAINT [FK_Collection_Unit_Process_Collection_Unit] FOREIGN KEY 
	(
		[Collection_Unit_Key]
	) REFERENCES [Collection_Unit] (
		[Collection_Unit_Key]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Collection_Unit_Process_Concept' AND parent_obj=Object_ID('Collection_Unit_Process'))
ALTER TABLE [dbo].[Collection_Unit_Process] ADD CONSTRAINT [FK_Collection_Unit_Process_Concept] FOREIGN KEY 
	(
		[Process_Concept_Key]
	) REFERENCES [Concept] (
		[Concept_Key]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Collection_Unit_Process_Individual' AND parent_obj=Object_ID('Collection_Unit_Process'))
ALTER TABLE [dbo].[Collection_Unit_Process] ADD CONSTRAINT [FK_Collection_Unit_Process_Individual] FOREIGN KEY 
	(
		[Name_Key]
	) REFERENCES [INDIVIDUAL] (
		[NAME_KEY]
	)
GO


SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Collection_Unit_ProcessCustodianInsert]') and OBJECTPROPERTY(id, N'IsTrigger') = 1)
drop trigger [dbo].[Collection_Unit_ProcessCustodianInsert]
GO


CREATE TRIGGER Collection_Unit_ProcessCustodianInsert ON dbo.Collection_Unit_Process AFTER INSERT AS UPDATE Collection_Unit_Process SET Collection_Unit_Process.CUSTODIAN = SUBSTRING(Collection_Unit_Process.Collection_Unit_Process_Key, 1, 8) FROM Collection_Unit_Process INNER JOIN INSERTED ON Collection_Unit_Process.Collection_Unit_Process_Key = INSERTED.Collection_Unit_Process_Key WHERE Collection_Unit_Process.CUSTODIAN IS NULL


GO
SET QUOTED_IDENTIFIER OFF 
GO
SET ANSI_NULLS ON 
GO


if not exists (select 1 from SysObjects where name='DF_Collection_Unit_Process_Inferred_Process' AND Type='D')
ALTER TABLE [dbo].[Collection_Unit_Process] ADD CONSTRAINT [DF_Collection_Unit_Process_Inferred_Process] DEFAULT (0) FOR [Inferred_Process]
if not exists (select 1 from SysObjects where name='DF_Collection_Unit_Process_Inferred_Description' AND Type='D')
ALTER TABLE [dbo].[Collection_Unit_Process] ADD CONSTRAINT [DF_Collection_Unit_Process_Inferred_Description] DEFAULT (0) FOR [Inferred_Description]
if not exists (select 1 from SysObjects where name='DF_Collection_Unit_Process_Inferred_Person' AND Type='D')
ALTER TABLE [dbo].[Collection_Unit_Process] ADD CONSTRAINT [DF_Collection_Unit_Process_Inferred_Person] DEFAULT (0) FOR [Inferred_Person]
if not exists (select 1 from SysObjects where name='DF_Collection_Unit_Process_Inferred_Date' AND Type='D')
ALTER TABLE [dbo].[Collection_Unit_Process] ADD CONSTRAINT [DF_Collection_Unit_Process_Inferred_Date] DEFAULT (0) FOR [Inferred_Date]
if not exists (select 1 from SysObjects where name='FK_Collection_Unit_Relation_From_Collection' AND parent_obj=Object_ID('Collection_Unit_Relation'))
ALTER TABLE [dbo].[Collection_Unit_Relation] ADD CONSTRAINT [FK_Collection_Unit_Relation_From_Collection] FOREIGN KEY 
	(
		[From_Collection_Unit_Key]
	) REFERENCES [Collection_Unit] (
		[Collection_Unit_Key]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Collection_Unit_Relation_NAME' AND parent_obj=Object_ID('Collection_Unit_Relation'))
ALTER TABLE [dbo].[Collection_Unit_Relation] ADD CONSTRAINT [FK_Collection_Unit_Relation_NAME] FOREIGN KEY 
	(
		[Author_Name_Key]
	) REFERENCES [NAME] (
		[NAME_KEY]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Collection_Unit_Relation_Thesaurus_Relation_Type' AND parent_obj=Object_ID('Collection_Unit_Relation'))
ALTER TABLE [dbo].[Collection_Unit_Relation] ADD CONSTRAINT [FK_Collection_Unit_Relation_Thesaurus_Relation_Type] FOREIGN KEY 
	(
		[Thesaurus_Relation_Type_Key]
	) REFERENCES [Thesaurus_Relation_Type] (
		[Thesaurus_Relation_Type_Key]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Collection_Unit_Relation_To_Collection' AND parent_obj=Object_ID('Collection_Unit_Relation'))
ALTER TABLE [dbo].[Collection_Unit_Relation] ADD CONSTRAINT [FK_Collection_Unit_Relation_To_Collection] FOREIGN KEY 
	(
		[To_Collection_Unit_Key]
	) REFERENCES [Collection_Unit] (
		[Collection_Unit_Key]
	)
GO


SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Collection_Unit_RelationCustodianInsert]') and OBJECTPROPERTY(id, N'IsTrigger') = 1)
drop trigger [dbo].[Collection_Unit_RelationCustodianInsert]
GO


CREATE TRIGGER Collection_Unit_RelationCustodianInsert ON dbo.Collection_Unit_Relation AFTER INSERT AS UPDATE Collection_Unit_Relation SET Collection_Unit_Relation.CUSTODIAN = SUBSTRING(Collection_Unit_Relation.Collection_Unit_Relation_Key, 1, 8) FROM Collection_Unit_Relation INNER JOIN INSERTED ON Collection_Unit_Relation.Collection_Unit_Relation_Key = INSERTED.Collection_Unit_Relation_Key WHERE Collection_Unit_Relation.CUSTODIAN IS NULL


GO
SET QUOTED_IDENTIFIER OFF 
GO
SET ANSI_NULLS ON 
GO


if not exists (select 1 from SysObjects where name='DF_Collection_Unit_Relation_Inferred_Type' AND Type='D')
ALTER TABLE [dbo].[Collection_Unit_Relation] ADD CONSTRAINT [DF_Collection_Unit_Relation_Inferred_Type] DEFAULT (0) FOR [Inferred_Type]
if not exists (select 1 from SysObjects where name='IX_Collection_Unit_Task_Unique' AND parent_obj=Object_ID('Collection_Unit_Task'))
ALTER TABLE [dbo].[Collection_Unit_Task] ADD CONSTRAINT [IX_Collection_Unit_Task_Unique] UNIQUE  NONCLUSTERED 
	(
		[Conservation_Task_Key],
		[Collection_Unit_Key]
	)  ON [PRIMARY] 
GO


if not exists (select 1 from SysObjects where name='FK_Collection_Unit_Task_Collection_Unit' AND parent_obj=Object_ID('Collection_Unit_Task'))
ALTER TABLE [dbo].[Collection_Unit_Task] ADD CONSTRAINT [FK_Collection_Unit_Task_Collection_Unit] FOREIGN KEY 
	(
		[Collection_Unit_Key]
	) REFERENCES [Collection_Unit] (
		[Collection_Unit_Key]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Collection_Unit_Task_Conservation_Task' AND parent_obj=Object_ID('Collection_Unit_Task'))
ALTER TABLE [dbo].[Collection_Unit_Task] ADD CONSTRAINT [FK_Collection_Unit_Task_Conservation_Task] FOREIGN KEY 
	(
		[Conservation_Task_Key]
	) REFERENCES [Conservation_Task] (
		[Conservation_Task_Key]
	)
GO


if not exists (select 1 from dbo.sysindexes where name = N'IX_Collection_Unit_Task_Unique' and id = object_id(N'[dbo].[Collection_Unit_Task]'))
 CREATE  UNIQUE  INDEX [IX_Collection_Unit_Task_Unique] ON [dbo].[Collection_Unit_Task]([Conservation_Task_Key], [Collection_Unit_Key]) ON [PRIMARY]
GO


if not exists (select 1 from dbo.sysindexes where name = N'IX_Conservation_Task_Key' and id = object_id(N'[dbo].[Collection_Unit_Task]'))
 CREATE  INDEX [IX_Conservation_Task_Key] ON [dbo].[Collection_Unit_Task]([Conservation_Task_Key]) ON [PRIMARY]
GO


if not exists (select 1 from dbo.sysindexes where name = N'IX_Collection_Unit_Key' and id = object_id(N'[dbo].[Collection_Unit_Task]'))
 CREATE  INDEX [IX_Collection_Unit_Key] ON [dbo].[Collection_Unit_Task]([Collection_Unit_Key]) ON [PRIMARY]
GO


SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[tr_CollectionUnitTask_ForDomainMask]') and OBJECTPROPERTY(id, N'IsTrigger') = 1)
drop trigger [dbo].[tr_CollectionUnitTask_ForDomainMask]
GO



/*===========================================================================*\
  Description:	Update Domain_Mask of all jobs linked to changed records in 
		Collection_Unit_Task.

  Type:		AFTER INSERT, UPDATE and DELETE

  Created:	September 2003

  Last revision information:
    $Revision: 2 $
    $Date: 9/01/06 13:42 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE TRIGGER [dbo].[tr_CollectionUnitTask_ForDomainMask]
ON [dbo].[Collection_Unit_Task]
AFTER INSERT, UPDATE, DELETE
AS 
	DECLARE	@TaskKey char(16)

	/*-------------------------------------------------------------*\
	  Only the task key is needed to get to the jobs needing 
	  updating. But we need to through all keys, hence the cursor.
	\*-------------------------------------------------------------*/
	DECLARE curTasks CURSOR LOCAL FAST_FORWARD FOR
		SELECT	DISTINCT Conservation_Task_Key
		FROM	Deleted
		WHERE	Conservation_Task_Key NOT IN (SELECT Conservation_Task_Key FROM Inserted)
		UNION
		SELECT	DISTINCT Conservation_Task_Key
		FROM 	Inserted	
	
	/*-------------------------------------------------------------*\
	  Go and do it now.
	\*-------------------------------------------------------------*/
	OPEN curTasks
	FETCH NEXT FROM curTasks INTO @TaskKey
	WHILE @@Fetch_Status = 0
	BEGIN
		EXECUTE	usp_ConservationJob_Update_DomainMask NULL, @TaskKey
	
		FETCH NEXT FROM curTasks INTO @TaskKey
	END
	
	-- Cleanup
	CLOSE curTasks
	DEALLOCATE curTasks


GO
SET QUOTED_IDENTIFIER OFF 
GO
SET ANSI_NULLS ON 
GO


if not exists (select 1 from SysObjects where name='FK_Collection_Unit_Valuation_Collection_Unit' AND parent_obj=Object_ID('Collection_Unit_Valuation'))
ALTER TABLE [dbo].[Collection_Unit_Valuation] ADD CONSTRAINT [FK_Collection_Unit_Valuation_Collection_Unit] FOREIGN KEY 
	(
		[Collection_Unit_Key]
	) REFERENCES [Collection_Unit] (
		[Collection_Unit_Key]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Collection_Unit_Valuation_Valuation' AND parent_obj=Object_ID('Collection_Unit_Valuation'))
ALTER TABLE [dbo].[Collection_Unit_Valuation] ADD CONSTRAINT [FK_Collection_Unit_Valuation_Valuation] FOREIGN KEY 
	(
		[Valuation_Key]
	) REFERENCES [Valuation] (
		[Valuation_Key]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Concept_Concept_Group' AND parent_obj=Object_ID('Concept'))
ALTER TABLE [dbo].[Concept] ADD CONSTRAINT [FK_Concept_Concept_Group] FOREIGN KEY 
	(
		[Concept_Group_Key]
	) REFERENCES [Concept_Group] (
		[Concept_Group_Key]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Concept_Concept_Name_Type' AND parent_obj=Object_ID('Concept'))
ALTER TABLE [dbo].[Concept] ADD CONSTRAINT [FK_Concept_Concept_Name_Type] FOREIGN KEY 
	(
		[Name_Type_Concept_Key]
	) REFERENCES [Concept] (
		[Concept_Key]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Concept_Concept_Rank' AND parent_obj=Object_ID('Concept'))
ALTER TABLE [dbo].[Concept] ADD CONSTRAINT [FK_Concept_Concept_Rank] FOREIGN KEY 
	(
		[Concept_Rank_Key]
	) REFERENCES [Concept_Rank] (
		[Concept_Rank_Key]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Concept_Meaning' AND parent_obj=Object_ID('Concept'))
ALTER TABLE [dbo].[Concept] ADD CONSTRAINT [FK_Concept_Meaning] FOREIGN KEY 
	(
		[Meaning_Key]
	) REFERENCES [Meaning] (
		[Meaning_Key]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Concept_Term' AND parent_obj=Object_ID('Concept'))
ALTER TABLE [dbo].[Concept] ADD CONSTRAINT [FK_Concept_Term] FOREIGN KEY 
	(
		[Term_Key]
	) REFERENCES [Term] (
		[Term_Key]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Concept_Term_Version' AND parent_obj=Object_ID('Concept'))
ALTER TABLE [dbo].[Concept] ADD CONSTRAINT [FK_Concept_Term_Version] FOREIGN KEY 
	(
		[Term_Version_Key]
	) REFERENCES [Term_Version] (
		[Term_Version_Key]
	)
GO


if not exists (select 1 from dbo.sysindexes where name = N'IX_Term_Key' and id = object_id(N'[dbo].[Concept]'))
 CREATE  INDEX [IX_Term_Key] ON [dbo].[Concept]([Term_Key]) ON [PRIMARY]
GO


if not exists (select 1 from dbo.sysindexes where name = N'IX_Meaning_Key' and id = object_id(N'[dbo].[Concept]'))
 CREATE  INDEX [IX_Meaning_Key] ON [dbo].[Concept]([Meaning_Key]) ON [PRIMARY]
GO


if not exists (select 1 from dbo.sysindexes where name = N'IX_Concept_Group_Key' and id = object_id(N'[dbo].[Concept]'))
 CREATE  INDEX [IX_Concept_Group_Key] ON [dbo].[Concept]([Concept_Group_Key]) ON [PRIMARY]
GO


if not exists (select 1 from dbo.sysindexes where name = N'IX_Term_Version_Key' and id = object_id(N'[dbo].[Concept]'))
 CREATE  INDEX [IX_Term_Version_Key] ON [dbo].[Concept]([Term_Version_Key]) ON [PRIMARY]
GO


SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[ConceptCustodianInsert]') and OBJECTPROPERTY(id, N'IsTrigger') = 1)
drop trigger [dbo].[ConceptCustodianInsert]
GO


CREATE TRIGGER ConceptCustodianInsert ON dbo.Concept AFTER INSERT AS UPDATE Concept SET Concept.CUSTODIAN = SUBSTRING(Concept.Concept_Key, 1, 8) FROM Concept INNER JOIN INSERTED ON Concept.Concept_Key = INSERTED.Concept_Key WHERE Concept.CUSTODIAN IS NULL


GO
SET QUOTED_IDENTIFIER OFF 
GO
SET ANSI_NULLS ON 
GO


SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[tr_Concept_AuthorCopy]') and OBJECTPROPERTY(id, N'IsTrigger') = 1)
drop trigger [dbo].[tr_Concept_AuthorCopy]
GO



/*===========================================================================*\
  Description:	This trigger updates the Author_Copy field in the Concept 
		table when the Author_And_Date field in the Term_Version 
		table is updated

  Created:	Nov 2003

  Last revision information:
    $Revision: 2 $
    $Date: 9/01/06 13:42 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE TRIGGER [dbo].[tr_Concept_AuthorCopy] ON [dbo].[Concept] 
FOR UPDATE, INSERT

AS
	IF UPDATE (Term_Version_Key)
	BEGIN
		UPDATE Concept
		SET Concept.Author_Copy=Term_Version.Author_And_Date
		FROM Concept C 
		INNER JOIN Inserted I ON C.Concept_Key=I.Concept_Key
		LEFT JOIN Term_Version on Term_Version.Term_Version_Key=C.Term_Version_Key
	
		IF @@ERROR <>0
			RAISERROR('Error updating Author_Copy in Concept table',16,1)
	END


GO
SET QUOTED_IDENTIFIER OFF 
GO
SET ANSI_NULLS ON 
GO


SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[tr_Concept_DisplayCaptionUpdates]') and OBJECTPROPERTY(id, N'IsTrigger') = 1)
drop trigger [dbo].[tr_Concept_DisplayCaptionUpdates]
GO


/*===========================================================================*\
  Description:	Update display caption on the following tables when an existing
				concept's term is modified:
				Conservation Check

  Type:		AFTER UPDATE

  Created:	September 2003

  Last revision information:
    $Revision: 2 $
    $Date: 9/01/06 13:42 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE TRIGGER [dbo].[tr_Concept_DisplayCaptionUpdates]
ON dbo.Concept
AFTER UPDATE
AS 
	IF UPDATE(Term_Key)
	BEGIN
		UPDATE Conservation_Check 
		SET Search_Caption=
				dbo.ufn_GetDateFromVagueDate(CC.Vague_Date_Start, CC.Vague_Date_End, CC.Vague_Date_Type)
				+ ' - ' +
				T.Plaintext
				+ ' - ' +
				CC.Ref_Number,
			Display_Caption=
				dbo.ufn_GetDateFromVagueDate(CC.Vague_Date_Start, CC.Vague_Date_End, CC.Vague_Date_Type)
				+ ' - ' +
				T.Item_Name
				+ ' - ' +
				CC.Ref_Number
		FROM Conservation_Check CC
		INNER JOIN Concept C ON C.Concept_Key=CC.Type_Concept_Key
		INNER JOIN Term T ON T.Term_Key=C.Term_Key
		INNER JOIN Inserted I on I.Concept_Key=C.Concept_Key

		UPDATE Enquiry 
		SET Search_Caption=
				dbo.ufn_GetDateFromVagueDate(E.Vague_Date_Start, E.Vague_Date_End, E.Vague_Date_Type)
				+ ' - ' +
				T.Plaintext,
			Display_Caption=
				dbo.ufn_GetDateFromVagueDate(E.Vague_Date_Start, E.Vague_Date_End, E.Vague_Date_Type)
				+ ' - ' +
				T.Item_Name
		FROM Enquiry E
		INNER JOIN Concept C ON C.Concept_Key=E.Enquiry_Type_Concept_Key
		INNER JOIN Term T ON T.Term_Key=C.Term_Key
		INNER JOIN Inserted I on I.Concept_Key=C.Concept_Key

		UPDATE Valuation 
		SET Search_Caption=
				dbo.ufn_GetDateFromVagueDate(V.Vague_Date_Start, V.Vague_Date_End, V.Vague_Date_Type)
				+ ' - ' +
				T.Plaintext,
			Display_Caption=
				dbo.ufn_GetDateFromVagueDate(V.Vague_Date_Start, V.Vague_Date_End, V.Vague_Date_Type)
				+ ' - ' +
				T.Item_Name
		FROM Valuation V
		INNER JOIN Concept C ON C.Concept_Key=V.Type_Concept_Key
		INNER JOIN Term T ON T.Term_Key=C.Term_Key
		INNER JOIN Inserted I on I.Concept_Key=C.Concept_Key

	END


GO
SET QUOTED_IDENTIFIER OFF 
GO
SET ANSI_NULLS ON 
GO


if not exists (select 1 from SysObjects where name='DF_Concept_Is_Current' AND Type='D')
ALTER TABLE [dbo].[Concept] ADD CONSTRAINT [DF_Concept_Is_Current] DEFAULT (1) FOR [Is_Current]
if not exists (select 1 from SysObjects where name='DF_Concept_Preferred' AND Type='D')
ALTER TABLE [dbo].[Concept] ADD CONSTRAINT [DF_Concept_Preferred] DEFAULT (0) FOR [Preferred]
if not exists (select 1 from SysObjects where name='DF_Concept_System_Supplied_Data' AND Type='D')
ALTER TABLE [dbo].[Concept] ADD CONSTRAINT [DF_Concept_System_Supplied_Data] DEFAULT (0) FOR [System_Supplied_Data]
if not exists (select 1 from SysObjects where name='FK_Concept_Designation_Concept' AND parent_obj=Object_ID('Concept_Designation'))
ALTER TABLE [dbo].[Concept_Designation] ADD CONSTRAINT [FK_Concept_Designation_Concept] FOREIGN KEY 
	(
		[Concept_Key]
	) REFERENCES [Concept] (
		[Concept_Key]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Concept_Designation_Type' AND parent_obj=Object_ID('Concept_Designation'))
ALTER TABLE [dbo].[Concept_Designation] ADD CONSTRAINT [FK_Concept_Designation_Type] FOREIGN KEY 
	(
		[Designation_Type_Concept_Key]
	) REFERENCES [Concept] (
		[Concept_Key]
	)
GO


if not exists (select 1 from dbo.sysindexes where name = N'IX_Concept_Key' and id = object_id(N'[dbo].[Concept_Designation]'))
 CREATE  INDEX [IX_Concept_Key] ON [dbo].[Concept_Designation]([Concept_Key]) ON [PRIMARY]
GO


SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Concept_DesignationCustodianInsert]') and OBJECTPROPERTY(id, N'IsTrigger') = 1)
drop trigger [dbo].[Concept_DesignationCustodianInsert]
GO


CREATE TRIGGER Concept_DesignationCustodianInsert ON dbo.Concept_Designation AFTER INSERT AS UPDATE Concept_Designation SET Concept_Designation.CUSTODIAN = SUBSTRING(Concept_Designation.Concept_Designation_Key, 1, 8) FROM Concept_Designation INNER JOIN INSERTED ON Concept_Designation.Concept_Designation_Key = INSERTED.Concept_Designation_Key WHERE Concept_Designation.CUSTODIAN IS NULL


GO
SET QUOTED_IDENTIFIER OFF 
GO
SET ANSI_NULLS ON 
GO


if not exists (select 1 from SysObjects where name='DF_Concept_Status_System_Supplied_Data' AND Type='D')
ALTER TABLE [dbo].[Concept_Designation] ADD CONSTRAINT [DF_Concept_Status_System_Supplied_Data] DEFAULT (0) FOR [System_Supplied_Data]
if not exists (select 1 from SysObjects where name='FK_Concept_Group_Local_Domain' AND parent_obj=Object_ID('Concept_Group'))
ALTER TABLE [dbo].[Concept_Group] ADD CONSTRAINT [FK_Concept_Group_Local_Domain] FOREIGN KEY 
	(
		[Local_Domain_Key]
	) REFERENCES [Local_Domain] (
		[Local_Domain_Key]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Concept_Group_Thesaurus_Relation_Type' AND parent_obj=Object_ID('Concept_Group'))
ALTER TABLE [dbo].[Concept_Group] ADD CONSTRAINT [FK_Concept_Group_Thesaurus_Relation_Type] FOREIGN KEY 
	(
		[Hierarchy_Relation_Type_Key]
	) REFERENCES [Thesaurus_Relation_Type] (
		[Thesaurus_Relation_Type_Key]
	)
GO


if not exists (select 1 from dbo.sysindexes where name = N'IX_Local_Domain_Key' and id = object_id(N'[dbo].[Concept_Group]'))
 CREATE  INDEX [IX_Local_Domain_Key] ON [dbo].[Concept_Group]([Local_Domain_Key]) ON [PRIMARY]
GO


if not exists (select 1 from dbo.sysindexes where name = N'_WA_Sys_Hierarchy_Relation_Type_Key_2276B61A' and id = object_id(N'[dbo].[Concept_Group]'))

SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Concept_GroupCustodianInsert]') and OBJECTPROPERTY(id, N'IsTrigger') = 1)
drop trigger [dbo].[Concept_GroupCustodianInsert]
GO


CREATE TRIGGER Concept_GroupCustodianInsert ON dbo.Concept_Group AFTER INSERT AS UPDATE Concept_Group SET Concept_Group.CUSTODIAN = SUBSTRING(Concept_Group.Concept_Group_Key, 1, 8) FROM Concept_Group INNER JOIN INSERTED ON Concept_Group.Concept_Group_Key = INSERTED.Concept_Group_Key WHERE Concept_Group.CUSTODIAN IS NULL


GO
SET QUOTED_IDENTIFIER OFF 
GO
SET ANSI_NULLS ON 
GO


if not exists (select 1 from SysObjects where name='DF_Concept_Group_System_Supplied_Data' AND Type='D')
ALTER TABLE [dbo].[Concept_Group] ADD CONSTRAINT [DF_Concept_Group_System_Supplied_Data] DEFAULT (0) FOR [System_Supplied_Data]
if not exists (select 1 from SysObjects where name='FK_Concept_Group_Version_Concept_Group' AND parent_obj=Object_ID('Concept_Group_Version'))
ALTER TABLE [dbo].[Concept_Group_Version] ADD CONSTRAINT [FK_Concept_Group_Version_Concept_Group] FOREIGN KEY 
	(
		[Concept_Group_Key]
	) REFERENCES [Concept_Group] (
		[Concept_Group_Key]
	)
GO


if not exists (select 1 from dbo.sysindexes where name = N'IX_Concept_Group_Key' and id = object_id(N'[dbo].[Concept_Group_Version]'))
 CREATE  INDEX [IX_Concept_Group_Key] ON [dbo].[Concept_Group_Version]([Concept_Group_Key]) ON [PRIMARY]
GO


SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Concept_Group_VersionCustodianInsert]') and OBJECTPROPERTY(id, N'IsTrigger') = 1)
drop trigger [dbo].[Concept_Group_VersionCustodianInsert]
GO


CREATE TRIGGER Concept_Group_VersionCustodianInsert ON dbo.Concept_Group_Version AFTER INSERT AS UPDATE Concept_Group_Version SET Concept_Group_Version.CUSTODIAN = SUBSTRING(Concept_Group_Version.Concept_Group_Version_Key, 1, 8) FROM Concept_Group_Version INNER JOIN INSERTED ON Concept_Group_Version.Concept_Group_Version_Key = INSERTED.Concept_Group_Version_Key WHERE Concept_Group_Version.CUSTODIAN IS NULL


GO
SET QUOTED_IDENTIFIER OFF 
GO
SET ANSI_NULLS ON 
GO


if not exists (select 1 from SysObjects where name='DF_Concept_List_Version_System_Supplied_Data' AND Type='D')
ALTER TABLE [dbo].[Concept_Group_Version] ADD CONSTRAINT [DF_Concept_List_Version_System_Supplied_Data] DEFAULT (0) FOR [System_Supplied_Data]
if not exists (select 1 from SysObjects where name='FK_Concept_History_Concept' AND parent_obj=Object_ID('Concept_History'))
ALTER TABLE [dbo].[Concept_History] ADD CONSTRAINT [FK_Concept_History_Concept] FOREIGN KEY 
	(
		[Concept_Key]
	) REFERENCES [Concept] (
		[Concept_Key]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Concept_History_Concept_Group_Version_From' AND parent_obj=Object_ID('Concept_History'))
ALTER TABLE [dbo].[Concept_History] ADD CONSTRAINT [FK_Concept_History_Concept_Group_Version_From] FOREIGN KEY 
	(
		[Concept_Group_Version_From]
	) REFERENCES [Concept_Group_Version] (
		[Concept_Group_Version_Key]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Concept_History_Concept_Group_Version_To' AND parent_obj=Object_ID('Concept_History'))
ALTER TABLE [dbo].[Concept_History] ADD CONSTRAINT [FK_Concept_History_Concept_Group_Version_To] FOREIGN KEY 
	(
		[Concept_Group_Version_To]
	) REFERENCES [Concept_Group_Version] (
		[Concept_Group_Version_Key]
	)
GO


if not exists (select 1 from dbo.sysindexes where name = N'IX_Concept_Key' and id = object_id(N'[dbo].[Concept_History]'))
 CREATE  INDEX [IX_Concept_Key] ON [dbo].[Concept_History]([Concept_Key]) ON [PRIMARY]
GO


if not exists (select 1 from dbo.sysindexes where name = N'_WA_Sys_Concept_Group_Version_To_245EFE8C' and id = object_id(N'[dbo].[Concept_History]'))

SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Concept_HistoryCustodianInsert]') and OBJECTPROPERTY(id, N'IsTrigger') = 1)
drop trigger [dbo].[Concept_HistoryCustodianInsert]
GO


CREATE TRIGGER Concept_HistoryCustodianInsert ON dbo.Concept_History AFTER INSERT AS UPDATE Concept_History SET Concept_History.CUSTODIAN = SUBSTRING(Concept_History.Concept_History_Key, 1, 8) FROM Concept_History INNER JOIN INSERTED ON Concept_History.Concept_History_Key = INSERTED.Concept_History_Key WHERE Concept_History.CUSTODIAN IS NULL


GO
SET QUOTED_IDENTIFIER OFF 
GO
SET ANSI_NULLS ON 
GO


SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[tr_ConceptHistory_IsCurrentUpdate]') and OBJECTPROPERTY(id, N'IsTrigger') = 1)
drop trigger [dbo].[tr_ConceptHistory_IsCurrentUpdate]
GO



/*===========================================================================*\
  Description:	The Concept table has an IsCurrent field that is set 
		depending on the Concept_History records associated with it.
		If a Concept has no associated Concept_History records
		then Is_Current is set to 1. If the Concept is associated
		with a Concept_History record that has no expiry details,
		Is_Current is set to 1. Otherwise, Is_Current is set to 0.

  Type:		AFTER INSERT, UPDATE, DELETE

  Created:	March 2004

  Last revision information:
    $Revision: 2 $
    $Date: 9/01/06 13:42 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE TRIGGER [dbo].[tr_ConceptHistory_IsCurrentUpdate]
ON [dbo].[Concept_History]
AFTER INSERT, UPDATE, DELETE
AS 
	/*--------------------------------------------------------------*\
	  Declare local variables and set @IsCurrent to be 0 as default.
	\*--------------------------------------------------------------*/
	DECLARE @IsCurrent bit,
		@CurrentConceptKey char(16)

	SET @IsCurrent = 0

	/*--------------------------------------------------------------*\
	  Get the Concept_Key for the Concept_History record that has
	  just been updated.
	\*--------------------------------------------------------------*/
	IF UPDATE(Concept_Group_Version_To) OR UPDATE(To_Vague_Date_Type)
		SELECT @CurrentConceptKey = Concept_Key FROM Inserted 

	ELSE IF (SELECT Count(*) FROM Deleted) > 0 
		SELECT @CurrentConceptKey = Concept_Key FROM Deleted 
	
	/*--------------------------------------------------------------*\
	  If there has been a change that requires the IsCurrent field
	  to be altered, then go ahead and make the change.
	\*--------------------------------------------------------------*/	
	IF @CurrentConceptKey IS NOT NULL
	BEGIN
		/*-----------------------------------------------------------------*\
		  If Concept has no associated Concept_History records (i.e. CH 
		  record just deleted).
		\*-----------------------------------------------------------------*/
		SELECT 	@IsCurrent = 	CASE WHEN Count(*) = 0 	THEN 1
								ELSE 0
					END
		FROM 	Concept_History
		WHERE	Concept_Key = @CurrentConceptKey

		/*-----------------------------------------------------------------*\
		  Check to see if Concept is associated with any Concept_History 
		  records that have no expiry date.
		\*-----------------------------------------------------------------*/
		IF @IsCurrent = 0
			SELECT 	@IsCurrent = 	CASE WHEN Count(*) = 0 	THEN 0
									ELSE 1
						END
			FROM 	Concept_History
			WHERE	Concept_Key = @CurrentConceptKey
			AND 	Concept_Group_Version_To IS NULL
			AND 	To_Vague_Date_Type IS NULL

		/*----------------------------------------*\
		  Actually update the Concept record.
		\*----------------------------------------*/
		UPDATE 	Concept
		SET	Is_Current = @IsCurrent
		WHERE	Concept_Key = @CurrentConceptKey
	END


GO
SET QUOTED_IDENTIFIER OFF 
GO
SET ANSI_NULLS ON 
GO


if not exists (select 1 from SysObjects where name='DF_Concept_History_System_Supplied_Data' AND Type='D')
ALTER TABLE [dbo].[Concept_History] ADD CONSTRAINT [DF_Concept_History_System_Supplied_Data] DEFAULT (0) FOR [System_Supplied_Data]
if not exists (select 1 from SysObjects where name='FK_Concept_Lineage_Concept' AND parent_obj=Object_ID('Concept_Lineage'))
ALTER TABLE [dbo].[Concept_Lineage] ADD CONSTRAINT [FK_Concept_Lineage_Concept] FOREIGN KEY 
	(
		[Concept_Key]
	) REFERENCES [Concept] (
		[Concept_Key]
	)
GO


if not exists (select 1 from dbo.sysindexes where name = N'IX_Lineage' and id = object_id(N'[dbo].[Concept_Lineage]'))
 CREATE  CLUSTERED  INDEX [IX_Lineage] ON [dbo].[Concept_Lineage]([Lineage]) ON [PRIMARY]
GO


if not exists (select 1 from dbo.sysindexes where name = N'IX_Concept_Key' and id = object_id(N'[dbo].[Concept_Lineage]'))
 CREATE  INDEX [IX_Concept_Key] ON [dbo].[Concept_Lineage]([Concept_Key]) ON [PRIMARY]
GO


if not exists (select 1 from SysObjects where name='FK_Concept_Rank_Domain' AND parent_obj=Object_ID('Concept_Rank'))
ALTER TABLE [dbo].[Concept_Rank] ADD CONSTRAINT [FK_Concept_Rank_Domain] FOREIGN KEY 
	(
		[Domain_Key]
	) REFERENCES [Domain] (
		[Domain_Key]
	)
GO


SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Concept_RankCustodianInsert]') and OBJECTPROPERTY(id, N'IsTrigger') = 1)
drop trigger [dbo].[Concept_RankCustodianInsert]
GO


CREATE TRIGGER Concept_RankCustodianInsert ON dbo.Concept_Rank AFTER INSERT AS UPDATE Concept_Rank SET Concept_Rank.CUSTODIAN = SUBSTRING(Concept_Rank.Concept_Rank_Key, 1, 8) FROM Concept_Rank INNER JOIN INSERTED ON Concept_Rank.Concept_Rank_Key = INSERTED.Concept_Rank_Key WHERE Concept_Rank.CUSTODIAN IS NULL


GO
SET QUOTED_IDENTIFIER OFF 
GO
SET ANSI_NULLS ON 
GO


if not exists (select 1 from SysObjects where name='DF_Concept_Rank_System_Supplied_Data' AND Type='D')
ALTER TABLE [dbo].[Concept_Rank] ADD CONSTRAINT [DF_Concept_Rank_System_Supplied_Data] DEFAULT (0) FOR [System_Supplied_Data]
if not exists (select 1 from SysObjects where name='FK_Concept_Relation_Concept' AND parent_obj=Object_ID('Concept_Relation'))
ALTER TABLE [dbo].[Concept_Relation] ADD CONSTRAINT [FK_Concept_Relation_Concept] FOREIGN KEY 
	(
		[From_Concept_Key]
	) REFERENCES [Concept] (
		[Concept_Key]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Concept_Relation_Concept1' AND parent_obj=Object_ID('Concept_Relation'))
ALTER TABLE [dbo].[Concept_Relation] ADD CONSTRAINT [FK_Concept_Relation_Concept1] FOREIGN KEY 
	(
		[To_Concept_Key]
	) REFERENCES [Concept] (
		[Concept_Key]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Concept_Relation_Thesaurus_Relation_Type' AND parent_obj=Object_ID('Concept_Relation'))
ALTER TABLE [dbo].[Concept_Relation] ADD CONSTRAINT [FK_Concept_Relation_Thesaurus_Relation_Type] FOREIGN KEY 
	(
		[Thesaurus_Relation_Type_Key]
	) REFERENCES [Thesaurus_Relation_Type] (
		[Thesaurus_Relation_Type_Key]
	)
GO


if not exists (select 1 from dbo.sysindexes where name = N'IX_From_Concept_Key' and id = object_id(N'[dbo].[Concept_Relation]'))
 CREATE  INDEX [IX_From_Concept_Key] ON [dbo].[Concept_Relation]([From_Concept_Key]) ON [PRIMARY]
GO


if not exists (select 1 from dbo.sysindexes where name = N'IX_To_Concept_Key' and id = object_id(N'[dbo].[Concept_Relation]'))
 CREATE  INDEX [IX_To_Concept_Key] ON [dbo].[Concept_Relation]([To_Concept_Key]) ON [PRIMARY]
GO


SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Concept_RelationCustodianInsert]') and OBJECTPROPERTY(id, N'IsTrigger') = 1)
drop trigger [dbo].[Concept_RelationCustodianInsert]
GO


CREATE TRIGGER Concept_RelationCustodianInsert ON dbo.Concept_Relation AFTER INSERT AS UPDATE Concept_Relation SET Concept_Relation.CUSTODIAN = SUBSTRING(Concept_Relation.Concept_Relation_Key, 1, 8) FROM Concept_Relation INNER JOIN INSERTED ON Concept_Relation.Concept_Relation_Key = INSERTED.Concept_Relation_Key WHERE Concept_Relation.CUSTODIAN IS NULL


GO
SET QUOTED_IDENTIFIER OFF 
GO
SET ANSI_NULLS ON 
GO


if not exists (select 1 from SysObjects where name='DF_Concept_Relation_Inherited' AND Type='D')
ALTER TABLE [dbo].[Concept_Relation] ADD CONSTRAINT [DF_Concept_Relation_Inherited] DEFAULT (0) FOR [Inherited]
if not exists (select 1 from SysObjects where name='DF_Concept_Relation_Comment' AND Type='D')
ALTER TABLE [dbo].[Concept_Relation] ADD CONSTRAINT [DF_Concept_Relation_Comment] DEFAULT ('0') FOR [Comment]
if not exists (select 1 from SysObjects where name='FK_Conservation_Check_Concept' AND parent_obj=Object_ID('Conservation_Check'))
ALTER TABLE [dbo].[Conservation_Check] ADD CONSTRAINT [FK_Conservation_Check_Concept] FOREIGN KEY 
	(
		[Condition_Concept_Key]
	) REFERENCES [Concept] (
		[Concept_Key]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Conservation_Check_Name' AND parent_obj=Object_ID('Conservation_Check'))
ALTER TABLE [dbo].[Conservation_Check] ADD CONSTRAINT [FK_Conservation_Check_Name] FOREIGN KEY 
	(
		[Checked_By_Name_Key]
	) REFERENCES [NAME] (
		[NAME_KEY]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Conservation_Check_Type' AND parent_obj=Object_ID('Conservation_Check'))
ALTER TABLE [dbo].[Conservation_Check] ADD CONSTRAINT [FK_Conservation_Check_Type] FOREIGN KEY 
	(
		[Type_Concept_Key]
	) REFERENCES [Concept] (
		[Concept_Key]
	)
GO


if not exists (select 1 from dbo.sysindexes where name = N'IX_Conservation_Check_Display_Caption' and id = object_id(N'[dbo].[Conservation_Check]'))
 CREATE  INDEX [IX_Conservation_Check_Display_Caption] ON [dbo].[Conservation_Check]([Search_Caption]) ON [PRIMARY]
GO


if not exists (select 1 from dbo.sysindexes where name = N'IX_Conservation_Check_Search_Caption' and id = object_id(N'[dbo].[Conservation_Check]'))
 CREATE  INDEX [IX_Conservation_Check_Search_Caption] ON [dbo].[Conservation_Check]([Search_Caption]) ON [PRIMARY]
GO


SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Conservation_CheckCustodianInsert]') and OBJECTPROPERTY(id, N'IsTrigger') = 1)
drop trigger [dbo].[Conservation_CheckCustodianInsert]
GO


CREATE TRIGGER Conservation_CheckCustodianInsert ON dbo.Conservation_Check AFTER INSERT AS UPDATE Conservation_Check SET Conservation_Check.CUSTODIAN = SUBSTRING(Conservation_Check.Conservation_Check_Key, 1, 8) FROM Conservation_Check INNER JOIN INSERTED ON Conservation_Check.Conservation_Check_Key = INSERTED.Conservation_Check_Key WHERE Conservation_Check.CUSTODIAN IS NULL


GO
SET QUOTED_IDENTIFIER OFF 
GO
SET ANSI_NULLS ON 
GO


SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[tr_ConservationCheck_DisplayCaption]') and OBJECTPROPERTY(id, N'IsTrigger') = 1)
drop trigger [dbo].[tr_ConservationCheck_DisplayCaption]
GO


/*===========================================================================*\
  Description:	Update display caption on the Conservation Check table.

  Type:		AFTER INSERT, UPDATE

  Created:	September 2003

  Last revision information:
    $Revision: 2 $
    $Date: 9/01/06 13:42 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE TRIGGER [dbo].[tr_ConservationCheck_DisplayCaption]
ON dbo.Conservation_Check
AFTER INSERT, UPDATE
AS 
	IF UPDATE(Vague_Date_Start) 
			OR UPDATE(Vague_Date_End)
			OR UPDATE(Vague_Date_Type)
			OR UPDATE(Type_Concept_Key)
			OR UPDATE(Ref_Number)
		UPDATE Conservation_Check 
		SET Display_Caption=
			dbo.ufn_GetDateFromVagueDate(CC.Vague_Date_Start, CC.Vague_Date_End, CC.Vague_Date_Type)
			+ ' - ' +
			CT.Item_Name
			+ ' - ' +
			CC.Ref_Number
		FROM Conservation_Check CC
		INNER JOIN Inserted I on I.Conservation_Check_Key=CC.Conservation_Check_Key
		INNER JOIN VW_ConceptTerm CT ON CT.Concept_Key=CC.Type_Concept_Key


GO
SET QUOTED_IDENTIFIER OFF 
GO
SET ANSI_NULLS ON 
GO


SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[tr_ConservationCheck_SearchCaption]') and OBJECTPROPERTY(id, N'IsTrigger') = 1)
drop trigger [dbo].[tr_ConservationCheck_SearchCaption]
GO



/*===========================================================================*\
  Description:	Update search caption on the Conservation Check table.

  Type:		AFTER INSERT, UPDATE

  Created:	September 2003

  Last revision information:
    $Revision: 2 $
    $Date: 9/01/06 13:42 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE TRIGGER [dbo].[tr_ConservationCheck_SearchCaption]
ON [dbo].[Conservation_Check]
AFTER INSERT, UPDATE
AS 
	IF UPDATE(Vague_Date_Start) 
			OR UPDATE(Vague_Date_End)
			OR UPDATE(Vague_Date_Type)
			OR UPDATE(Type_Concept_Key)
			OR UPDATE(Ref_Number)
		UPDATE Conservation_Check 
		SET Search_Caption=
				dbo.ufn_GetDateFromVagueDate(I.Vague_Date_Start, I.Vague_Date_End, I.Vague_Date_Type)
				+ ' - ' +
				CT.Plaintext
				+ ' - ' +
				I.Ref_Number,
			Display_Caption=
				dbo.ufn_GetDateFromVagueDate(I.Vague_Date_Start, I.Vague_Date_End, I.Vague_Date_Type)
				+ ' - ' +
				CT.Item_Name
				+ ' - ' +
				I.Ref_Number
		FROM Conservation_Check CC
		INNER JOIN Inserted I on I.Conservation_Check_Key=CC.Conservation_Check_Key
		INNER JOIN VW_ConceptTerm CT ON CT.Concept_Key=I.Type_Concept_Key	



GO
SET QUOTED_IDENTIFIER OFF 
GO
SET ANSI_NULLS ON 
GO


if not exists (select 1 from SysObjects where name='DF_Conservation_Check_Filter_Bitfield' AND Type='D')
ALTER TABLE [dbo].[Conservation_Check] ADD CONSTRAINT [DF_Conservation_Check_Filter_Bitfield] DEFAULT (0) FOR [Domain_Mask]
if not exists (select 1 from SysObjects where name='FK_Conservation_Job_Currency' AND parent_obj=Object_ID('Conservation_Job'))
ALTER TABLE [dbo].[Conservation_Job] ADD CONSTRAINT [FK_Conservation_Job_Currency] FOREIGN KEY 
	(
		[Currency_Concept_Key]
	) REFERENCES [Concept] (
		[Concept_Key]
	)
GO


if not exists (select 1 from dbo.sysindexes where name = N'IX_Conservation_Job_Display_Caption' and id = object_id(N'[dbo].[Conservation_Job]'))
 CREATE  INDEX [IX_Conservation_Job_Display_Caption] ON [dbo].[Conservation_Job]([Display_Caption]) ON [PRIMARY]
GO


SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Conservation_JobCustodianInsert]') and OBJECTPROPERTY(id, N'IsTrigger') = 1)
drop trigger [dbo].[Conservation_JobCustodianInsert]
GO


CREATE TRIGGER Conservation_JobCustodianInsert ON dbo.Conservation_Job AFTER INSERT AS UPDATE Conservation_Job SET Conservation_Job.CUSTODIAN = SUBSTRING(Conservation_Job.Conservation_Job_Key, 1, 8) FROM Conservation_Job INNER JOIN INSERTED ON Conservation_Job.Conservation_Job_Key = INSERTED.Conservation_Job_Key WHERE Conservation_Job.CUSTODIAN IS NULL


GO
SET QUOTED_IDENTIFIER OFF 
GO
SET ANSI_NULLS ON 
GO


SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[tr_Conservation_Job_SearchCaption]') and OBJECTPROPERTY(id, N'IsTrigger') = 1)
drop trigger [dbo].[tr_Conservation_Job_SearchCaption]
GO


/*===========================================================================*\
  Description:	Update search caption on the tr_Conservation_Job table.

  Type:		AFTER INSERT, UPDATE

  Created:	September 2003

  Last revision information:
    $Revision: 2 $
    $Date: 9/01/06 13:42 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE TRIGGER [dbo].[tr_Conservation_Job_SearchCaption]
ON dbo.Conservation_Job
AFTER INSERT, UPDATE
AS 
	IF UPDATE(From_Vague_Date_Start) 
			OR UPDATE(From_Vague_Date_End)
			OR UPDATE(From_Vague_Date_Type)
			OR UPDATE(Item_Name)
		UPDATE Conservation_Job 
		SET Search_Caption=
				dbo.ufn_GetDateFromVagueDate(I.From_Vague_Date_Start, I.From_Vague_Date_End, I.From_Vague_Date_Type)
				+ ' - ' + I.Item_Name,
			Display_Caption=
				dbo.ufn_GetDateFromVagueDate(I.From_Vague_Date_Start, I.From_Vague_Date_End, I.From_Vague_Date_Type)
				+ ' - ' + I.Item_Name
		FROM Conservation_Job CJ
		INNER JOIN Inserted I on I.Conservation_Job_Key=CJ.Conservation_Job_Key


GO
SET QUOTED_IDENTIFIER OFF 
GO
SET ANSI_NULLS ON 
GO


if not exists (select 1 from SysObjects where name='DF_Conservation_Job_Subject_Area_Bitfield' AND Type='D')
ALTER TABLE [dbo].[Conservation_Job] ADD CONSTRAINT [DF_Conservation_Job_Subject_Area_Bitfield] DEFAULT (0) FOR [Domain_Mask]
if not exists (select 1 from SysObjects where name='FK_Conservation_Job_Funding_Conservation_Job' AND parent_obj=Object_ID('Conservation_Job_Funding'))
ALTER TABLE [dbo].[Conservation_Job_Funding] ADD CONSTRAINT [FK_Conservation_Job_Funding_Conservation_Job] FOREIGN KEY 
	(
		[Conservation_Job_Key]
	) REFERENCES [Conservation_Job] (
		[Conservation_Job_Key]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Conservation_Job_Funding_Currency' AND parent_obj=Object_ID('Conservation_Job_Funding'))
ALTER TABLE [dbo].[Conservation_Job_Funding] ADD CONSTRAINT [FK_Conservation_Job_Funding_Currency] FOREIGN KEY 
	(
		[Currency_Concept_Key]
	) REFERENCES [Concept] (
		[Concept_Key]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Conservation_Job_Funding_Name' AND parent_obj=Object_ID('Conservation_Job_Funding'))
ALTER TABLE [dbo].[Conservation_Job_Funding] ADD CONSTRAINT [FK_Conservation_Job_Funding_Name] FOREIGN KEY 
	(
		[Funded_By_Name_Key]
	) REFERENCES [NAME] (
		[NAME_KEY]
	)
GO


SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Conservation_Job_FundingCustodianInsert]') and OBJECTPROPERTY(id, N'IsTrigger') = 1)
drop trigger [dbo].[Conservation_Job_FundingCustodianInsert]
GO


CREATE TRIGGER Conservation_Job_FundingCustodianInsert ON dbo.Conservation_Job_Funding AFTER INSERT AS UPDATE Conservation_Job_Funding SET Conservation_Job_Funding.CUSTODIAN = SUBSTRING(Conservation_Job_Funding.Conservation_Job_Funding_Key, 1, 8) FROM Conservation_Job_Funding INNER JOIN INSERTED ON Conservation_Job_Funding.Conservation_Job_Funding_Key = INSERTED.Conservation_Job_Funding_Key WHERE Conservation_Job_Funding.CUSTODIAN IS NULL


GO
SET QUOTED_IDENTIFIER OFF 
GO
SET ANSI_NULLS ON 
GO


if not exists (select 1 from SysObjects where name='FK_Conservation_Job_Material_Conservation_Job' AND parent_obj=Object_ID('Conservation_Job_Material'))
ALTER TABLE [dbo].[Conservation_Job_Material] ADD CONSTRAINT [FK_Conservation_Job_Material_Conservation_Job] FOREIGN KEY 
	(
		[Conservation_Job_Key]
	) REFERENCES [Conservation_Job] (
		[Conservation_Job_Key]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Conservation_Job_Material_Material' AND parent_obj=Object_ID('Conservation_Job_Material'))
ALTER TABLE [dbo].[Conservation_Job_Material] ADD CONSTRAINT [FK_Conservation_Job_Material_Material] FOREIGN KEY 
	(
		[Material_Concept_Key]
	) REFERENCES [Concept] (
		[Concept_Key]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Conservation_Job_Material_Unit' AND parent_obj=Object_ID('Conservation_Job_Material'))
ALTER TABLE [dbo].[Conservation_Job_Material] ADD CONSTRAINT [FK_Conservation_Job_Material_Unit] FOREIGN KEY 
	(
		[Unit_Concept_Key]
	) REFERENCES [Concept] (
		[Concept_Key]
	)
GO


SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Conservation_Job_MaterialCustodianInsert]') and OBJECTPROPERTY(id, N'IsTrigger') = 1)
drop trigger [dbo].[Conservation_Job_MaterialCustodianInsert]
GO


CREATE TRIGGER Conservation_Job_MaterialCustodianInsert ON dbo.Conservation_Job_Material AFTER INSERT AS UPDATE Conservation_Job_Material SET Conservation_Job_Material.CUSTODIAN = SUBSTRING(Conservation_Job_Material.Conservation_Job_Material_Key, 1, 8) FROM Conservation_Job_Material INNER JOIN INSERTED ON Conservation_Job_Material.Conservation_Job_Material_Key = INSERTED.Conservation_Job_Material_Key WHERE Conservation_Job_Material.CUSTODIAN IS NULL


GO
SET QUOTED_IDENTIFIER OFF 
GO
SET ANSI_NULLS ON 
GO


if not exists (select 1 from SysObjects where name='FK_Conservation_Job_Staff_Conservation_Job' AND parent_obj=Object_ID('Conservation_Job_Staff'))
ALTER TABLE [dbo].[Conservation_Job_Staff] ADD CONSTRAINT [FK_Conservation_Job_Staff_Conservation_Job] FOREIGN KEY 
	(
		[Conservation_Job_Key]
	) REFERENCES [Conservation_Job] (
		[Conservation_Job_Key]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Conservation_Job_Staff_Individual' AND parent_obj=Object_ID('Conservation_Job_Staff'))
ALTER TABLE [dbo].[Conservation_Job_Staff] ADD CONSTRAINT [FK_Conservation_Job_Staff_Individual] FOREIGN KEY 
	(
		[Name_Key]
	) REFERENCES [INDIVIDUAL] (
		[NAME_KEY]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Conservation_Task_Conservation_Check' AND parent_obj=Object_ID('Conservation_Task'))
ALTER TABLE [dbo].[Conservation_Task] ADD CONSTRAINT [FK_Conservation_Task_Conservation_Check] FOREIGN KEY 
	(
		[Conservation_Check_Key]
	) REFERENCES [Conservation_Check] (
		[Conservation_Check_Key]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Conservation_Task_Conservation_Job' AND parent_obj=Object_ID('Conservation_Task'))
ALTER TABLE [dbo].[Conservation_Task] ADD CONSTRAINT [FK_Conservation_Task_Conservation_Job] FOREIGN KEY 
	(
		[Conservation_Job_Key]
	) REFERENCES [Conservation_Job] (
		[Conservation_Job_Key]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Conservation_Task_Individual' AND parent_obj=Object_ID('Conservation_Task'))
ALTER TABLE [dbo].[Conservation_Task] ADD CONSTRAINT [FK_Conservation_Task_Individual] FOREIGN KEY 
	(
		[Identifier_Name_Key]
	) REFERENCES [INDIVIDUAL] (
		[NAME_KEY]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Conservation_Task_Type' AND parent_obj=Object_ID('Conservation_Task'))
ALTER TABLE [dbo].[Conservation_Task] ADD CONSTRAINT [FK_Conservation_Task_Type] FOREIGN KEY 
	(
		[Type_Concept_Key]
	) REFERENCES [Concept] (
		[Concept_Key]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Conservation_Task_Unit' AND parent_obj=Object_ID('Conservation_Task'))
ALTER TABLE [dbo].[Conservation_Task] ADD CONSTRAINT [FK_Conservation_Task_Unit] FOREIGN KEY 
	(
		[Duration_Unit_Concept_Key]
	) REFERENCES [Concept] (
		[Concept_Key]
	)
GO


if not exists (select 1 from dbo.sysindexes where name = N'IX_Conservation_Task_Display_Caption' and id = object_id(N'[dbo].[Conservation_Task]'))
 CREATE  INDEX [IX_Conservation_Task_Display_Caption] ON [dbo].[Conservation_Task]([Display_Caption]) ON [PRIMARY]
GO


SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Conservation_TaskCustodianInsert]') and OBJECTPROPERTY(id, N'IsTrigger') = 1)
drop trigger [dbo].[Conservation_TaskCustodianInsert]
GO


CREATE TRIGGER Conservation_TaskCustodianInsert ON dbo.Conservation_Task AFTER INSERT AS UPDATE Conservation_Task SET Conservation_Task.CUSTODIAN = SUBSTRING(Conservation_Task.Conservation_Task_Key, 1, 8) FROM Conservation_Task INNER JOIN INSERTED ON Conservation_Task.Conservation_Task_Key = INSERTED.Conservation_Task_Key WHERE Conservation_Task.CUSTODIAN IS NULL


GO
SET QUOTED_IDENTIFIER OFF 
GO
SET ANSI_NULLS ON 
GO


SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[tr_Conservation_Task_SearchCaption]') and OBJECTPROPERTY(id, N'IsTrigger') = 1)
drop trigger [dbo].[tr_Conservation_Task_SearchCaption]
GO



/*===========================================================================*\
  Description:	Update search caption on the Conservation_Task table.

  Type:		AFTER INSERT, UPDATE

  Created:	September 2003

  Last revision information:
    $Revision: 2 $
    $Date: 9/01/06 13:42 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE TRIGGER [dbo].[tr_Conservation_Task_SearchCaption]
ON [dbo].[Conservation_Task]
AFTER INSERT, UPDATE
AS 
	IF UPDATE(Set_Vague_Date_Start) 
			OR UPDATE(Set_Vague_Date_End)
			OR UPDATE(Set_Vague_Date_Type)
			OR UPDATE(Status)
		UPDATE Conservation_Task 
		SET Search_Caption=
				dbo.ufn_GetDateFromVagueDate(I.Set_Vague_Date_Start, I.Set_Vague_Date_End, I.Set_Vague_Date_Type)
				+ ' - ' + dbo.ufn_GetConservationStatus(I.Status),
			Display_Caption=
				dbo.ufn_GetDateFromVagueDate(I.Set_Vague_Date_Start, I.Set_Vague_Date_End, I.Set_Vague_Date_Type)
				+ ' - ' + dbo.ufn_GetConservationStatus(I.Status)
		FROM Conservation_Task CT
		INNER JOIN Inserted I on I.Conservation_Task_Key=CT.Conservation_Task_Key



GO
SET QUOTED_IDENTIFIER OFF 
GO
SET ANSI_NULLS ON 
GO


SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Details_ReportCustodianInsert]') and OBJECTPROPERTY(id, N'IsTrigger') = 1)
drop trigger [dbo].[Details_ReportCustodianInsert]
GO


CREATE TRIGGER Details_ReportCustodianInsert ON dbo.Details_Report AFTER INSERT AS UPDATE Details_Report SET Details_Report.CUSTODIAN = SUBSTRING(Details_Report.Details_Report_Key, 1, 8) FROM Details_Report INNER JOIN INSERTED ON Details_Report.Details_Report_Key = INSERTED.Details_Report_Key WHERE Details_Report.CUSTODIAN IS NULL


GO
SET QUOTED_IDENTIFIER OFF 
GO
SET ANSI_NULLS ON 
GO


if not exists (select 1 from SysObjects where name='DF_Details_Report_System_Supplied_Data' AND Type='D')
ALTER TABLE [dbo].[Details_Report] ADD CONSTRAINT [DF_Details_Report_System_Supplied_Data] DEFAULT (0) FOR [System_Supplied_Data]
if not exists (select 1 from SysObjects where name='FK_Determination_Concept' AND parent_obj=Object_ID('Determination'))
ALTER TABLE [dbo].[Determination] ADD CONSTRAINT [FK_Determination_Concept] FOREIGN KEY 
	(
		[Concept_Key]
	) REFERENCES [Concept] (
		[Concept_Key]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Determination_DETERMINATION_TYPE' AND parent_obj=Object_ID('Determination'))
ALTER TABLE [dbo].[Determination] ADD CONSTRAINT [FK_Determination_DETERMINATION_TYPE] FOREIGN KEY 
	(
		[Determination_Type_Key]
	) REFERENCES [DETERMINATION_TYPE] (
		[DETERMINATION_TYPE_KEY]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Determination_DETERMINER_ROLE' AND parent_obj=Object_ID('Determination'))
ALTER TABLE [dbo].[Determination] ADD CONSTRAINT [FK_Determination_DETERMINER_ROLE] FOREIGN KEY 
	(
		[Determiner_Role_Key]
	) REFERENCES [DETERMINER_ROLE] (
		[DETERMINER_ROLE_KEY]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Determination_INDIVIDUAL' AND parent_obj=Object_ID('Determination'))
ALTER TABLE [dbo].[Determination] ADD CONSTRAINT [FK_Determination_INDIVIDUAL] FOREIGN KEY 
	(
		[Determiner_Name_Key]
	) REFERENCES [INDIVIDUAL] (
		[NAME_KEY]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Determination_Nomenclatural_Status' AND parent_obj=Object_ID('Determination'))
ALTER TABLE [dbo].[Determination] ADD CONSTRAINT [FK_Determination_Nomenclatural_Status] FOREIGN KEY 
	(
		[Nomenclatural_Status_Concept_Key]
	) REFERENCES [Concept] (
		[Concept_Key]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Determination_Occurrence' AND parent_obj=Object_ID('Determination'))
ALTER TABLE [dbo].[Determination] ADD CONSTRAINT [FK_Determination_Occurrence] FOREIGN KEY 
	(
		[Occurrence_Key]
	) REFERENCES [Occurrence] (
		[Occurrence_Key]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Determination_Specimen_Unit' AND parent_obj=Object_ID('Determination'))
ALTER TABLE [dbo].[Determination] ADD CONSTRAINT [FK_Determination_Specimen_Unit] FOREIGN KEY 
	(
		[Specimen_Collection_Unit_Key]
	) REFERENCES [Specimen_Unit] (
		[Collection_Unit_Key]
	)
GO


if not exists (select 1 from dbo.sysindexes where name = N'IX_Determination_Specimen' and id = object_id(N'[dbo].[Determination]'))
 CREATE  INDEX [IX_Determination_Specimen] ON [dbo].[Determination]([Specimen_Collection_Unit_Key]) ON [PRIMARY]
GO


SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[DeterminationCustodianInsert]') and OBJECTPROPERTY(id, N'IsTrigger') = 1)
drop trigger [dbo].[DeterminationCustodianInsert]
GO


CREATE TRIGGER DeterminationCustodianInsert ON dbo.Determination AFTER INSERT AS UPDATE Determination SET Determination.CUSTODIAN = SUBSTRING(Determination.Determination_Key, 1, 8) FROM Determination INNER JOIN INSERTED ON Determination.Determination_Key = INSERTED.Determination_Key WHERE Determination.CUSTODIAN IS NULL


GO
SET QUOTED_IDENTIFIER OFF 
GO
SET ANSI_NULLS ON 
GO


if not exists (select 1 from SysObjects where name='DF_Determination_Confidence' AND Type='D')
ALTER TABLE [dbo].[Determination] ADD CONSTRAINT [DF_Determination_Confidence] DEFAULT (0) FOR [Confidence]
if not exists (select 1 from SysObjects where name='DF_Determination_Inferred_Determiner' AND Type='D')
ALTER TABLE [dbo].[Determination] ADD CONSTRAINT [DF_Determination_Inferred_Determiner] DEFAULT (0) FOR [Inferred_Determiner]
if not exists (select 1 from SysObjects where name='DF_Determination_Preferred' AND Type='D')
ALTER TABLE [dbo].[Determination] ADD CONSTRAINT [DF_Determination_Preferred] DEFAULT (0) FOR [Preferred]
if not exists (select 1 from SysObjects where name='FK_Domain_Subject_Area' AND parent_obj=Object_ID('Domain'))
ALTER TABLE [dbo].[Domain] ADD CONSTRAINT [FK_Domain_Subject_Area] FOREIGN KEY 
	(
		[Subject_Area_Key]
	) REFERENCES [Subject_Area] (
		[Subject_Area_Key]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Domain_Thesaurus_Relation_Type' AND parent_obj=Object_ID('Domain'))
ALTER TABLE [dbo].[Domain] ADD CONSTRAINT [FK_Domain_Thesaurus_Relation_Type] FOREIGN KEY 
	(
		[Default_Hierarchy_Relation_Type_Key]
	) REFERENCES [Thesaurus_Relation_Type] (
		[Thesaurus_Relation_Type_Key]
	)
GO


SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[DomainCustodianInsert]') and OBJECTPROPERTY(id, N'IsTrigger') = 1)
drop trigger [dbo].[DomainCustodianInsert]
GO


CREATE TRIGGER DomainCustodianInsert ON dbo.[Domain] AFTER INSERT AS UPDATE Domain SET Domain.CUSTODIAN = SUBSTRING(Domain.Domain_Key, 1, 8) FROM Domain INNER JOIN INSERTED ON Domain.Domain_Key = INSERTED.Domain_Key WHERE Domain.CUSTODIAN IS NULL


GO
SET QUOTED_IDENTIFIER OFF 
GO
SET ANSI_NULLS ON 
GO


if not exists (select 1 from SysObjects where name='DF_Domain_Has_Occurrences' AND Type='D')
ALTER TABLE [dbo].[Domain] ADD CONSTRAINT [DF_Domain_Has_Occurrences] DEFAULT (0) FOR [Has_Occurrences]
if not exists (select 1 from SysObjects where name='FK_Domain_Hyperlink_Local_Domain' AND parent_obj=Object_ID('Domain_Hyperlink'))
ALTER TABLE [dbo].[Domain_Hyperlink] ADD CONSTRAINT [FK_Domain_Hyperlink_Local_Domain] FOREIGN KEY 
	(
		[Local_Domain_Key]
	) REFERENCES [Local_Domain] (
		[Local_Domain_Key]
	)
GO


SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Domain_HyperlinkCustodianInsert]') and OBJECTPROPERTY(id, N'IsTrigger') = 1)
drop trigger [dbo].[Domain_HyperlinkCustodianInsert]
GO


CREATE TRIGGER Domain_HyperlinkCustodianInsert ON dbo.Domain_Hyperlink AFTER INSERT AS UPDATE Domain_Hyperlink SET Domain_Hyperlink.CUSTODIAN = SUBSTRING(Domain_Hyperlink.Domain_Hyperlink_Key, 1, 8) FROM Domain_Hyperlink INNER JOIN INSERTED ON Domain_Hyperlink.Domain_Hyperlink_Key = INSERTED.Domain_Hyperlink_Key WHERE Domain_Hyperlink.CUSTODIAN IS NULL


GO
SET QUOTED_IDENTIFIER OFF 
GO
SET ANSI_NULLS ON 
GO


if not exists (select 1 from SysObjects where name='DF_Domain_Hyperlink_System_Supplied_Data' AND Type='D')
ALTER TABLE [dbo].[Domain_Hyperlink] ADD CONSTRAINT [DF_Domain_Hyperlink_System_Supplied_Data] DEFAULT (0) FOR [System_Supplied_Data]
if not exists (select 1 from SysObjects where name='FK_Enquiry_Answered_By' AND parent_obj=Object_ID('Enquiry'))
ALTER TABLE [dbo].[Enquiry] ADD CONSTRAINT [FK_Enquiry_Answered_By] FOREIGN KEY 
	(
		[Answered_By_Name_Key]
	) REFERENCES [NAME] (
		[NAME_KEY]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Enquiry_Enquirer' AND parent_obj=Object_ID('Enquiry'))
ALTER TABLE [dbo].[Enquiry] ADD CONSTRAINT [FK_Enquiry_Enquirer] FOREIGN KEY 
	(
		[Enquirer_Name_Key]
	) REFERENCES [INDIVIDUAL] (
		[NAME_KEY]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Enquiry_Method' AND parent_obj=Object_ID('Enquiry'))
ALTER TABLE [dbo].[Enquiry] ADD CONSTRAINT [FK_Enquiry_Method] FOREIGN KEY 
	(
		[Enquiry_Method_Concept_Key]
	) REFERENCES [Concept] (
		[Concept_Key]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Enquiry_Type' AND parent_obj=Object_ID('Enquiry'))
ALTER TABLE [dbo].[Enquiry] ADD CONSTRAINT [FK_Enquiry_Type] FOREIGN KEY 
	(
		[Enquiry_Type_Concept_Key]
	) REFERENCES [Concept] (
		[Concept_Key]
	)
GO


if not exists (select 1 from dbo.sysindexes where name = N'IX_Enquiry_Display_Caption' and id = object_id(N'[dbo].[Enquiry]'))
 CREATE  INDEX [IX_Enquiry_Display_Caption] ON [dbo].[Enquiry]([Display_Caption]) ON [PRIMARY]
GO


SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[EnquiryCustodianInsert]') and OBJECTPROPERTY(id, N'IsTrigger') = 1)
drop trigger [dbo].[EnquiryCustodianInsert]
GO


CREATE TRIGGER EnquiryCustodianInsert ON dbo.Enquiry AFTER INSERT AS UPDATE Enquiry SET Enquiry.CUSTODIAN = SUBSTRING(Enquiry.Enquiry_Key, 1, 8) FROM Enquiry INNER JOIN INSERTED ON Enquiry.Enquiry_Key = INSERTED.Enquiry_Key WHERE Enquiry.CUSTODIAN IS NULL


GO
SET QUOTED_IDENTIFIER OFF 
GO
SET ANSI_NULLS ON 
GO


SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[tr_Enquiry_SearchCaption]') and OBJECTPROPERTY(id, N'IsTrigger') = 1)
drop trigger [dbo].[tr_Enquiry_SearchCaption]
GO



/*===========================================================================*\
  Description:	Update search caption on the Enquiry table.

  Type:		AFTER INSERT, UPDATE

  Created:	September 2003

  Last revision information:
    $Revision: 2 $
    $Date: 9/01/06 13:42 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE TRIGGER [dbo].[tr_Enquiry_SearchCaption]
ON [dbo].[Enquiry]
AFTER INSERT, UPDATE
AS 
	IF UPDATE(Vague_Date_Start) 
			OR UPDATE(Vague_Date_End)
			OR UPDATE(Vague_Date_Type)
			OR UPDATE(Enquiry_Type_Concept_Key)
		UPDATE Enquiry 
		SET Search_Caption=
				dbo.ufn_GetDateFromVagueDate(I.Vague_Date_Start, I.Vague_Date_End, I.Vague_Date_Type)
				+ ' - ' +
				CT.Plaintext,
			Display_Caption=
				dbo.ufn_GetDateFromVagueDate(I.Vague_Date_Start, I.Vague_Date_End, I.Vague_Date_Type)
				+ ' - ' +
				CT.Item_Name
		FROM Enquiry E
		INNER JOIN Inserted I on I.Enquiry_Key=E.Enquiry_Key
		INNER JOIN VW_ConceptTerm CT ON CT.Concept_Key=I.Enquiry_Type_Concept_Key	



GO
SET QUOTED_IDENTIFIER OFF 
GO
SET ANSI_NULLS ON 
GO


if not exists (select 1 from SysObjects where name='IX_Enquiry_Concept_Unique' AND parent_obj=Object_ID('Enquiry_Concept'))
ALTER TABLE [dbo].[Enquiry_Concept] ADD CONSTRAINT [IX_Enquiry_Concept_Unique] UNIQUE  NONCLUSTERED 
	(
		[Enquiry_Key],
		[Concept_Key]
	)  ON [PRIMARY] 
GO


if not exists (select 1 from SysObjects where name='FK_Enquiry_Concept_Concept' AND parent_obj=Object_ID('Enquiry_Concept'))
ALTER TABLE [dbo].[Enquiry_Concept] ADD CONSTRAINT [FK_Enquiry_Concept_Concept] FOREIGN KEY 
	(
		[Concept_Key]
	) REFERENCES [Concept] (
		[Concept_Key]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Enquiry_Concept_Enquiry' AND parent_obj=Object_ID('Enquiry_Concept'))
ALTER TABLE [dbo].[Enquiry_Concept] ADD CONSTRAINT [FK_Enquiry_Concept_Enquiry] FOREIGN KEY 
	(
		[Enquiry_Key]
	) REFERENCES [Enquiry] (
		[Enquiry_Key]
	)
GO


if not exists (select 1 from dbo.sysindexes where name = N'IX_Enquiry_Concept_Unique' and id = object_id(N'[dbo].[Enquiry_Concept]'))
 CREATE  UNIQUE  INDEX [IX_Enquiry_Concept_Unique] ON [dbo].[Enquiry_Concept]([Enquiry_Key], [Concept_Key]) ON [PRIMARY]
GO


if not exists (select 1 from dbo.sysindexes where name = N'IX_Enquiry_Key' and id = object_id(N'[dbo].[Enquiry_Concept]'))
 CREATE  INDEX [IX_Enquiry_Key] ON [dbo].[Enquiry_Concept]([Enquiry_Key]) ON [PRIMARY]
GO


if not exists (select 1 from dbo.sysindexes where name = N'IX_Concept_Key' and id = object_id(N'[dbo].[Enquiry_Concept]'))
 CREATE  INDEX [IX_Concept_Key] ON [dbo].[Enquiry_Concept]([Concept_Key]) ON [PRIMARY]
GO


if not exists (select 1 from SysObjects where name='FK_Import_Export_Job_Concept_Group' AND parent_obj=Object_ID('Import_Export_Job'))
ALTER TABLE [dbo].[Import_Export_Job] ADD CONSTRAINT [FK_Import_Export_Job_Concept_Group] FOREIGN KEY 
	(
		[Concept_Group_Key]
	) REFERENCES [Concept_Group] (
		[Concept_Group_Key]
	)
GO


if not exists (select 1 from dbo.sysindexes where name = N'_WA_Sys_Item_Name_34956655' and id = object_id(N'[dbo].[Language]'))

if not exists (select 1 from SysObjects where name='FK_List_Report_Report_Block' AND parent_obj=Object_ID('List_Report'))
ALTER TABLE [dbo].[List_Report] ADD CONSTRAINT [FK_List_Report_Report_Block] FOREIGN KEY 
	(
		[Report_Block_Key]
	) REFERENCES [Report_Block] (
		[Report_Block_Key]
	)
GO


if not exists (select 1 from dbo.sysindexes where name = N'_WA_Sys_Report_Block_Key_35898A8E' and id = object_id(N'[dbo].[List_Report]'))

SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[List_ReportCustodianInsert]') and OBJECTPROPERTY(id, N'IsTrigger') = 1)
drop trigger [dbo].[List_ReportCustodianInsert]
GO


CREATE TRIGGER List_ReportCustodianInsert ON dbo.List_Report AFTER INSERT AS UPDATE List_Report SET List_Report.CUSTODIAN = SUBSTRING(List_Report.List_Report_Key, 1, 8) FROM List_Report INNER JOIN INSERTED ON List_Report.List_Report_Key = INSERTED.List_Report_Key WHERE List_Report.CUSTODIAN IS NULL


GO
SET QUOTED_IDENTIFIER OFF 
GO
SET ANSI_NULLS ON 
GO


if not exists (select 1 from SysObjects where name='DF_List_Report_System_Supplied_Data' AND Type='D')
ALTER TABLE [dbo].[List_Report] ADD CONSTRAINT [DF_List_Report_System_Supplied_Data] DEFAULT (0) FOR [System_Supplied_Data]
if not exists (select 1 from SysObjects where name='FK_Local_Domain_Domain' AND parent_obj=Object_ID('Local_Domain'))
ALTER TABLE [dbo].[Local_Domain] ADD CONSTRAINT [FK_Local_Domain_Domain] FOREIGN KEY 
	(
		[Domain_Key]
	) REFERENCES [Domain] (
		[Domain_Key]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Local_Domain_Language' AND parent_obj=Object_ID('Local_Domain'))
ALTER TABLE [dbo].[Local_Domain] ADD CONSTRAINT [FK_Local_Domain_Language] FOREIGN KEY 
	(
		[Language_Key]
	) REFERENCES [Language] (
		[Language_Key]
	)
GO


if not exists (select 1 from dbo.sysindexes where name = N'IX_Local_Domain_Domain_Key' and id = object_id(N'[dbo].[Local_Domain]'))
 CREATE  INDEX [IX_Local_Domain_Domain_Key] ON [dbo].[Local_Domain]([Domain_Key]) ON [PRIMARY]
GO


SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Local_DomainCustodianInsert]') and OBJECTPROPERTY(id, N'IsTrigger') = 1)
drop trigger [dbo].[Local_DomainCustodianInsert]
GO


CREATE TRIGGER Local_DomainCustodianInsert ON dbo.Local_Domain AFTER INSERT AS UPDATE Local_Domain SET Local_Domain.CUSTODIAN = SUBSTRING(Local_Domain.Local_Domain_Key, 1, 8) FROM Local_Domain INNER JOIN INSERTED ON Local_Domain.Local_Domain_Key = INSERTED.Local_Domain_Key WHERE Local_Domain.CUSTODIAN IS NULL


GO
SET QUOTED_IDENTIFIER OFF 
GO
SET ANSI_NULLS ON 
GO


if not exists (select 1 from SysObjects where name='DF_Local_Domain_Concept_Group_Label' AND Type='D')
ALTER TABLE [dbo].[Local_Domain] ADD CONSTRAINT [DF_Local_Domain_Concept_Group_Label] DEFAULT ('Concept Group') FOR [Concept_Group_Label]
if not exists (select 1 from SysObjects where name='DF_Local_Domain_System_Supplied_Data' AND Type='D')
ALTER TABLE [dbo].[Local_Domain] ADD CONSTRAINT [DF_Local_Domain_System_Supplied_Data] DEFAULT (0) FOR [System_Supplied_Data]
if not exists (select 1 from SysObjects where name='DF__Macro__ID_Seed__6EDA43DF' AND Type='D')
ALTER TABLE [dbo].[Macro] ADD CONSTRAINT [DF__Macro__ID_Seed__6EDA43DF] DEFAULT (1) FOR [ID_Seed]
SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Meaning_RelationCustodianInsert]') and OBJECTPROPERTY(id, N'IsTrigger') = 1)
drop trigger [dbo].[Meaning_RelationCustodianInsert]
GO


CREATE TRIGGER Meaning_RelationCustodianInsert ON dbo.Meaning_Relation AFTER INSERT AS UPDATE Meaning_Relation SET Meaning_Relation.CUSTODIAN = SUBSTRING(Meaning_Relation.Meaning_Relation_Key, 1, 8) FROM Meaning_Relation INNER JOIN INSERTED ON Meaning_Relation.Meaning_Relation_Key = INSERTED.Meaning_Relation_Key WHERE Meaning_Relation.CUSTODIAN IS NULL


GO
SET QUOTED_IDENTIFIER OFF 
GO
SET ANSI_NULLS ON 
GO


if not exists (select 1 from SysObjects where name='DF_Meaning_Relation_Inherited' AND Type='D')
ALTER TABLE [dbo].[Meaning_Relation] ADD CONSTRAINT [DF_Meaning_Relation_Inherited] DEFAULT (0) FOR [Inherited]
if not exists (select 1 from SysObjects where name='DF_Meaning_Relation_System_Supplied_Data' AND Type='D')
ALTER TABLE [dbo].[Meaning_Relation] ADD CONSTRAINT [DF_Meaning_Relation_System_Supplied_Data] DEFAULT (0) FOR [System_Supplied_Data]
if not exists (select 1 from SysObjects where name='FK_Metadata_Metadata_Type' AND parent_obj=Object_ID('Metadata'))
ALTER TABLE [dbo].[Metadata] ADD CONSTRAINT [FK_Metadata_Metadata_Type] FOREIGN KEY 
	(
		[Metadata_Type_Key]
	) REFERENCES [Metadata_Type] (
		[Metadata_Type_Key]
	)
GO


if not exists (select 1 from dbo.sysindexes where name = N'IX_Metadata_Record_Key' and id = object_id(N'[dbo].[Metadata]'))
 CREATE  INDEX [IX_Metadata_Record_Key] ON [dbo].[Metadata]([Record_Key]) ON [PRIMARY]
GO


if not exists (select 1 from dbo.sysindexes where name = N'IX_Metadata_Type_Key' and id = object_id(N'[dbo].[Metadata]'))
 CREATE  INDEX [IX_Metadata_Type_Key] ON [dbo].[Metadata]([Metadata_Type_Key]) ON [PRIMARY]
GO


SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[MetadataCustodianInsert]') and OBJECTPROPERTY(id, N'IsTrigger') = 1)
drop trigger [dbo].[MetadataCustodianInsert]
GO


CREATE TRIGGER MetadataCustodianInsert ON dbo.Metadata AFTER INSERT AS UPDATE Metadata SET Metadata.CUSTODIAN = SUBSTRING(Metadata.Metadata_Key, 1, 8) FROM Metadata INNER JOIN INSERTED ON Metadata.Metadata_Key = INSERTED.Metadata_Key WHERE Metadata.CUSTODIAN IS NULL


GO
SET QUOTED_IDENTIFIER OFF 
GO
SET ANSI_NULLS ON 
GO


if not exists (select 1 from SysObjects where name='DF_Thesaurus_Metadata_System_Supplied_Data' AND Type='D')
ALTER TABLE [dbo].[Metadata] ADD CONSTRAINT [DF_Thesaurus_Metadata_System_Supplied_Data] DEFAULT (0) FOR [System_Supplied_Data]
if not exists (select 1 from SysObjects where name='IX_Metadata_Type_Unique' AND parent_obj=Object_ID('Metadata_Type'))
ALTER TABLE [dbo].[Metadata_Type] ADD CONSTRAINT [IX_Metadata_Type_Unique] UNIQUE  NONCLUSTERED 
	(
		[Item_Name],
		[Table_Name]
	)  ON [PRIMARY] 
GO


if not exists (select 1 from dbo.sysindexes where name = N'IX_Metadata_Type_Unique' and id = object_id(N'[dbo].[Metadata_Type]'))
 CREATE  UNIQUE  INDEX [IX_Metadata_Type_Unique] ON [dbo].[Metadata_Type]([Item_Name], [Table_Name]) ON [PRIMARY]
GO


if not exists (select 1 from dbo.sysindexes where name = N'IX_Metadata_Type_Table' and id = object_id(N'[dbo].[Metadata_Type]'))
 CREATE  INDEX [IX_Metadata_Type_Table] ON [dbo].[Metadata_Type]([Table_Name]) ON [PRIMARY]
GO


SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Metadata_TypeCustodianInsert]') and OBJECTPROPERTY(id, N'IsTrigger') = 1)
drop trigger [dbo].[Metadata_TypeCustodianInsert]
GO


CREATE TRIGGER Metadata_TypeCustodianInsert ON dbo.Metadata_Type AFTER INSERT AS UPDATE Metadata_Type SET Metadata_Type.CUSTODIAN = SUBSTRING(Metadata_Type.Metadata_Type_Key, 1, 8) FROM Metadata_Type INNER JOIN INSERTED ON Metadata_Type.Metadata_Type_Key = INSERTED.Metadata_Type_Key WHERE Metadata_Type.CUSTODIAN IS NULL


GO
SET QUOTED_IDENTIFIER OFF 
GO
SET ANSI_NULLS ON 
GO


if not exists (select 1 from SysObjects where name='DF_Thesaurus_Metadata_Type_System_Supplied_Data' AND Type='D')
ALTER TABLE [dbo].[Metadata_Type] ADD CONSTRAINT [DF_Thesaurus_Metadata_Type_System_Supplied_Data] DEFAULT (0) FOR [System_Supplied_Data]
if not exists (select 1 from SysObjects where name='FK_Movement_Contact' AND parent_obj=Object_ID('Movement'))
ALTER TABLE [dbo].[Movement] ADD CONSTRAINT [FK_Movement_Contact] FOREIGN KEY 
	(
		[Contact_Name_Key]
	) REFERENCES [INDIVIDUAL] (
		[NAME_KEY]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Movement_Other_Party' AND parent_obj=Object_ID('Movement'))
ALTER TABLE [dbo].[Movement] ADD CONSTRAINT [FK_Movement_Other_Party] FOREIGN KEY 
	(
		[Other_Party_Name_Key]
	) REFERENCES [NAME] (
		[NAME_KEY]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Movement_Staff_Responsible' AND parent_obj=Object_ID('Movement'))
ALTER TABLE [dbo].[Movement] ADD CONSTRAINT [FK_Movement_Staff_Responsible] FOREIGN KEY 
	(
		[Staff_Responsible_Name_Key]
	) REFERENCES [INDIVIDUAL] (
		[NAME_KEY]
	)
GO


if not exists (select 1 from dbo.sysindexes where name = N'IX_Movement_Display_Caption' and id = object_id(N'[dbo].[Movement]'))
 CREATE  INDEX [IX_Movement_Display_Caption] ON [dbo].[Movement]([Display_Caption]) ON [PRIMARY]
GO


SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[MovementCustodianInsert]') and OBJECTPROPERTY(id, N'IsTrigger') = 1)
drop trigger [dbo].[MovementCustodianInsert]
GO


CREATE TRIGGER MovementCustodianInsert ON dbo.Movement AFTER INSERT AS UPDATE Movement SET Movement.CUSTODIAN = SUBSTRING(Movement.Movement_Key, 1, 8) FROM Movement INNER JOIN INSERTED ON Movement.Movement_Key = INSERTED.Movement_Key WHERE Movement.CUSTODIAN IS NULL


GO
SET QUOTED_IDENTIFIER OFF 
GO
SET ANSI_NULLS ON 
GO


SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[tr_Movement_SearchCaption]') and OBJECTPROPERTY(id, N'IsTrigger') = 1)
drop trigger [dbo].[tr_Movement_SearchCaption]
GO



/*===========================================================================*\
  Description:	Update search caption on the Movement table.

  Type:		AFTER INSERT, UPDATE

  Created:	September 2003

  Last revision information:
    $Revision: 2 $
    $Date: 9/01/06 13:42 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE TRIGGER [dbo].[tr_Movement_SearchCaption]
ON [dbo].[Movement]
AFTER INSERT, UPDATE
AS 

	IF UPDATE(Exp_Vague_Date_Start) 
			OR UPDATE(Exp_Vague_Date_End)
			OR UPDATE(Exp_Vague_Date_Type)
			OR UPDATE(Number)
	BEGIN
			UPDATE Movement 
			SET Search_Caption=
					dbo.ufn_GetMovementTypeName(I.Movement_Type) + ' (' + I.Number +  ') - ' +
					dbo.ufn_GetDateFromVagueDate(I.Exp_Vague_Date_Start, I.Exp_Vague_Date_End, I.Exp_Vague_Date_Type),
				Display_Caption=
					dbo.ufn_GetMovementTypeName(I.Movement_Type) + ' (' + I.Number +  ') - ' +
					dbo.ufn_GetDateFromVagueDate(I.Exp_Vague_Date_Start, I.Exp_Vague_Date_End, I.Exp_Vague_Date_Type)
			FROM Movement M
			INNER JOIN Inserted I on I.Movement_Key=M.Movement_Key
			WHERE I.Movement_Type IN (0, 1) 

			UPDATE Movement 
			SET Search_Caption=
					dbo.ufn_GetMovementTypeName(I.Movement_Type) + ' - ' +
					dbo.ufn_GetDateFromVagueDate(I.Exp_Vague_Date_Start, I.Exp_Vague_Date_End, I.Exp_Vague_Date_Type),
				Display_Caption=
					dbo.ufn_GetMovementTypeName(I.Movement_Type) + ' - ' +
					dbo.ufn_GetDateFromVagueDate(I.Exp_Vague_Date_Start, I.Exp_Vague_Date_End, I.Exp_Vague_Date_Type)
			FROM Movement M
			INNER JOIN Inserted I on I.Movement_Key=M.Movement_Key
			WHERE I.Movement_Type NOT IN (0, 1) 
	END





GO
SET QUOTED_IDENTIFIER OFF 
GO
SET ANSI_NULLS ON 
GO


if not exists (select 1 from SysObjects where name='FK_Movement_Collection_Unit_Collection_Unit' AND parent_obj=Object_ID('Movement_Collection_Unit'))
ALTER TABLE [dbo].[Movement_Collection_Unit] ADD CONSTRAINT [FK_Movement_Collection_Unit_Collection_Unit] FOREIGN KEY 
	(
		[Collection_Unit_Key]
	) REFERENCES [Collection_Unit] (
		[Collection_Unit_Key]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Movement_Collection_Unit_Movement_Direction' AND parent_obj=Object_ID('Movement_Collection_Unit'))
ALTER TABLE [dbo].[Movement_Collection_Unit] ADD CONSTRAINT [FK_Movement_Collection_Unit_Movement_Direction] FOREIGN KEY 
	(
		[Movement_Direction_Key]
	) REFERENCES [Movement_Direction] (
		[Movement_Direction_Key]
	)
GO


SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[tr_MovementCollectionUnit_DeleteCleanup]') and OBJECTPROPERTY(id, N'IsTrigger') = 1)
drop trigger [dbo].[tr_MovementCollectionUnit_DeleteCleanup]
GO



/*===========================================================================*\
  Description:	The purpose of this trigger is to keep the Movement_Of_Material_Exclusion 
		and Movement_Of_Ownership_Exclusion tables consistent with the 
		Movement_Collection_Unit table. If a record is removed from the 
		latter, the corresponding record in the former tables must be 
		removed (if there is one).

  Created:	July 2003

  Last revision information:
    $Revision: 2 $
    $Date: 9/01/06 13:42 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE TRIGGER [dbo].[tr_MovementCollectionUnit_DeleteCleanup] ON [dbo].[Movement_Collection_Unit] 
FOR DELETE 
AS

	DELETE FROM Movement_Of_Material_Exclusion
	WHERE Movement_Of_Material_Exclusion_Key IN
		(SELECT MME.Movement_Of_Material_Exclusion_Key
		FROM Movement_Of_Material_Exclusion MME
		INNER JOIN Movement_Of_Material MM ON MME.Movement_Of_Material_Key = MM.Movement_Of_Material_Key
		INNER JOIN Movement_Direction MD ON MM.Movement_Direction_Key = MD.Movement_Direction_Key
		INNER JOIN Deleted D ON MD.Movement_Direction_Key = D.Movement_Direction_Key
		WHERE D.Collection_Unit_Key=MME.Collection_Unit_Key)
	
	-- THis section keeps the Movement_Of_Ownership_Exclusion up to date
	DELETE FROM Movement_Of_Ownership_Exclusion
	WHERE Movement_Of_Ownership_Exclusion_Key IN
		(SELECT MME.Movement_Of_Ownership_Exclusion_Key
		FROM Movement_Of_Ownership_Exclusion MME
		INNER JOIN Movement_Of_Ownership MO ON MME.Movement_Of_Ownership_Key = MO.Movement_Of_Ownership_Key
		INNER JOIN Movement_Direction MD ON MO.Movement_Direction_Key = MD.Movement_Direction_Key
		INNER JOIN Deleted D ON MD.Movement_Direction_Key = D.Movement_Direction_Key
		WHERE D.Collection_Unit_Key=MME.Collection_Unit_Key)
	


GO
SET QUOTED_IDENTIFIER OFF 
GO
SET ANSI_NULLS ON 
GO


if not exists (select 1 from SysObjects where name='FK_Movement_Communication_Movement' AND parent_obj=Object_ID('Movement_Communication'))
ALTER TABLE [dbo].[Movement_Communication] ADD CONSTRAINT [FK_Movement_Communication_Movement] FOREIGN KEY 
	(
		[Movement_Key]
	) REFERENCES [Movement] (
		[Movement_Key]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Movement_Communication_Receiver' AND parent_obj=Object_ID('Movement_Communication'))
ALTER TABLE [dbo].[Movement_Communication] ADD CONSTRAINT [FK_Movement_Communication_Receiver] FOREIGN KEY 
	(
		[Receiver_Name_Key]
	) REFERENCES [NAME] (
		[NAME_KEY]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Movement_Communication_Sender' AND parent_obj=Object_ID('Movement_Communication'))
ALTER TABLE [dbo].[Movement_Communication] ADD CONSTRAINT [FK_Movement_Communication_Sender] FOREIGN KEY 
	(
		[Sender_Name_Key]
	) REFERENCES [NAME] (
		[NAME_KEY]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Movement_Communication_Type' AND parent_obj=Object_ID('Movement_Communication'))
ALTER TABLE [dbo].[Movement_Communication] ADD CONSTRAINT [FK_Movement_Communication_Type] FOREIGN KEY 
	(
		[Communication_Type_Concept_Key]
	) REFERENCES [Concept] (
		[Concept_Key]
	)
GO


SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Movement_CommunicationCustodianInsert]') and OBJECTPROPERTY(id, N'IsTrigger') = 1)
drop trigger [dbo].[Movement_CommunicationCustodianInsert]
GO


CREATE TRIGGER Movement_CommunicationCustodianInsert ON dbo.Movement_Communication AFTER INSERT AS UPDATE Movement_Communication SET Movement_Communication.CUSTODIAN = SUBSTRING(Movement_Communication.Movement_Communication_Key, 1, 8) FROM Movement_Communication INNER JOIN INSERTED ON Movement_Communication.Movement_Communication_Key = INSERTED.Movement_Communication_Key WHERE Movement_Communication.CUSTODIAN IS NULL


GO
SET QUOTED_IDENTIFIER OFF 
GO
SET ANSI_NULLS ON 
GO


if not exists (select 1 from SysObjects where name='FK_Movement_Conservation_Check_Conservation_Check' AND parent_obj=Object_ID('Movement_Conservation_Check'))
ALTER TABLE [dbo].[Movement_Conservation_Check] ADD CONSTRAINT [FK_Movement_Conservation_Check_Conservation_Check] FOREIGN KEY 
	(
		[Conservation_Check_Key]
	) REFERENCES [Conservation_Check] (
		[Conservation_Check_Key]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Movement_Conservation_Check_Movement' AND parent_obj=Object_ID('Movement_Conservation_Check'))
ALTER TABLE [dbo].[Movement_Conservation_Check] ADD CONSTRAINT [FK_Movement_Conservation_Check_Movement] FOREIGN KEY 
	(
		[Movement_Key]
	) REFERENCES [Movement] (
		[Movement_Key]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Movement_Direction_Movement' AND parent_obj=Object_ID('Movement_Direction'))
ALTER TABLE [dbo].[Movement_Direction] ADD CONSTRAINT [FK_Movement_Direction_Movement] FOREIGN KEY 
	(
		[Movement_Key]
	) REFERENCES [Movement] (
		[Movement_Key]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Movement_Direction_NAME' AND parent_obj=Object_ID('Movement_Direction'))
ALTER TABLE [dbo].[Movement_Direction] ADD CONSTRAINT [FK_Movement_Direction_NAME] FOREIGN KEY 
	(
		[Receiver_Name_Key]
	) REFERENCES [NAME] (
		[NAME_KEY]
	)
GO


SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Movement_DirectionCustodianInsert]') and OBJECTPROPERTY(id, N'IsTrigger') = 1)
drop trigger [dbo].[Movement_DirectionCustodianInsert]
GO


CREATE TRIGGER Movement_DirectionCustodianInsert ON dbo.Movement_Direction AFTER INSERT AS UPDATE Movement_Direction SET Movement_Direction.CUSTODIAN = SUBSTRING(Movement_Direction.Movement_Direction_Key, 1, 8) FROM Movement_Direction INNER JOIN INSERTED ON Movement_Direction.Movement_Direction_Key = INSERTED.Movement_Direction_Key WHERE Movement_Direction.CUSTODIAN IS NULL


GO
SET QUOTED_IDENTIFIER OFF 
GO
SET ANSI_NULLS ON 
GO


if not exists (select 1 from SysObjects where name='FK_Movement_Enquiry_Enquiry' AND parent_obj=Object_ID('Movement_Enquiry'))
ALTER TABLE [dbo].[Movement_Enquiry] ADD CONSTRAINT [FK_Movement_Enquiry_Enquiry] FOREIGN KEY 
	(
		[Enquiry_Key]
	) REFERENCES [Enquiry] (
		[Enquiry_Key]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Movement_Enquiry_Movement' AND parent_obj=Object_ID('Movement_Enquiry'))
ALTER TABLE [dbo].[Movement_Enquiry] ADD CONSTRAINT [FK_Movement_Enquiry_Movement] FOREIGN KEY 
	(
		[Movement_Key]
	) REFERENCES [Movement] (
		[Movement_Key]
	)
GO


if not exists (select 1 from dbo.sysindexes where name = N'IX_Movement_Enquiry' and id = object_id(N'[dbo].[Movement_Enquiry]'))
 CREATE  INDEX [IX_Movement_Enquiry] ON [dbo].[Movement_Enquiry]([Movement_Key]) ON [PRIMARY]
GO


if not exists (select 1 from SysObjects where name='FK_Movement_Funding_Currency' AND parent_obj=Object_ID('Movement_Funding'))
ALTER TABLE [dbo].[Movement_Funding] ADD CONSTRAINT [FK_Movement_Funding_Currency] FOREIGN KEY 
	(
		[Currency_Concept_Key]
	) REFERENCES [Concept] (
		[Concept_Key]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Movement_Funding_Movement' AND parent_obj=Object_ID('Movement_Funding'))
ALTER TABLE [dbo].[Movement_Funding] ADD CONSTRAINT [FK_Movement_Funding_Movement] FOREIGN KEY 
	(
		[Movement_Key]
	) REFERENCES [Movement] (
		[Movement_Key]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Movement_Funding_NAME' AND parent_obj=Object_ID('Movement_Funding'))
ALTER TABLE [dbo].[Movement_Funding] ADD CONSTRAINT [FK_Movement_Funding_NAME] FOREIGN KEY 
	(
		[Funded_By_Name_Key]
	) REFERENCES [NAME] (
		[NAME_KEY]
	)
GO


SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Movement_FundingCustodianInsert]') and OBJECTPROPERTY(id, N'IsTrigger') = 1)
drop trigger [dbo].[Movement_FundingCustodianInsert]
GO


CREATE TRIGGER Movement_FundingCustodianInsert ON dbo.Movement_Funding AFTER INSERT AS UPDATE Movement_Funding SET Movement_Funding.CUSTODIAN = SUBSTRING(Movement_Funding.Movement_Funding_Key, 1, 8) FROM Movement_Funding INNER JOIN INSERTED ON Movement_Funding.Movement_Funding_Key = INSERTED.Movement_Funding_Key WHERE Movement_Funding.CUSTODIAN IS NULL


GO
SET QUOTED_IDENTIFIER OFF 
GO
SET ANSI_NULLS ON 
GO


if not exists (select 1 from SysObjects where name='FK_Movement_Of_Material_Acquisition_Method' AND parent_obj=Object_ID('Movement_Of_Material'))
ALTER TABLE [dbo].[Movement_Of_Material] ADD CONSTRAINT [FK_Movement_Of_Material_Acquisition_Method] FOREIGN KEY 
	(
		[Acquisition_Method_Concept_Key]
	) REFERENCES [Concept] (
		[Concept_Key]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Movement_Of_Material_Currency' AND parent_obj=Object_ID('Movement_Of_Material'))
ALTER TABLE [dbo].[Movement_Of_Material] ADD CONSTRAINT [FK_Movement_Of_Material_Currency] FOREIGN KEY 
	(
		[Currency_Concept_Key]
	) REFERENCES [Concept] (
		[Concept_Key]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Movement_Of_Material_Movement_Direction' AND parent_obj=Object_ID('Movement_Of_Material'))
ALTER TABLE [dbo].[Movement_Of_Material] ADD CONSTRAINT [FK_Movement_Of_Material_Movement_Direction] FOREIGN KEY 
	(
		[Movement_Direction_Key]
	) REFERENCES [Movement_Direction] (
		[Movement_Direction_Key]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Movement_Of_Material_Person' AND parent_obj=Object_ID('Movement_Of_Material'))
ALTER TABLE [dbo].[Movement_Of_Material] ADD CONSTRAINT [FK_Movement_Of_Material_Person] FOREIGN KEY 
	(
		[Contact_Name_Key]
	) REFERENCES [INDIVIDUAL] (
		[NAME_KEY]
	)
GO


SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Movement_Of_MaterialCustodianInsert]') and OBJECTPROPERTY(id, N'IsTrigger') = 1)
drop trigger [dbo].[Movement_Of_MaterialCustodianInsert]
GO


CREATE TRIGGER Movement_Of_MaterialCustodianInsert ON dbo.Movement_Of_Material AFTER INSERT AS UPDATE Movement_Of_Material SET Movement_Of_Material.CUSTODIAN = SUBSTRING(Movement_Of_Material.Movement_Of_Material_Key, 1, 8) FROM Movement_Of_Material INNER JOIN INSERTED ON Movement_Of_Material.Movement_Of_Material_Key = INSERTED.Movement_Of_Material_Key WHERE Movement_Of_Material.CUSTODIAN IS NULL


GO
SET QUOTED_IDENTIFIER OFF 
GO
SET ANSI_NULLS ON 
GO


if not exists (select 1 from SysObjects where name='DF_Movement_Of_Material_Completed' AND Type='D')
ALTER TABLE [dbo].[Movement_Of_Material] ADD CONSTRAINT [DF_Movement_Of_Material_Completed] DEFAULT (0) FOR [Completed]
if not exists (select 1 from SysObjects where name='FK_Movement_Of_Material_Exclusion_Collection_Unit' AND parent_obj=Object_ID('Movement_Of_Material_Exclusion'))
ALTER TABLE [dbo].[Movement_Of_Material_Exclusion] ADD CONSTRAINT [FK_Movement_Of_Material_Exclusion_Collection_Unit] FOREIGN KEY 
	(
		[Collection_Unit_Key]
	) REFERENCES [Collection_Unit] (
		[Collection_Unit_Key]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Movement_Of_Material_Exclusion_Movement_Of_Material' AND parent_obj=Object_ID('Movement_Of_Material_Exclusion'))
ALTER TABLE [dbo].[Movement_Of_Material_Exclusion] ADD CONSTRAINT [FK_Movement_Of_Material_Exclusion_Movement_Of_Material] FOREIGN KEY 
	(
		[Movement_Of_Material_Key]
	) REFERENCES [Movement_Of_Material] (
		[Movement_Of_Material_Key]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Movement_Of_Ownership_Contact' AND parent_obj=Object_ID('Movement_Of_Ownership'))
ALTER TABLE [dbo].[Movement_Of_Ownership] ADD CONSTRAINT [FK_Movement_Of_Ownership_Contact] FOREIGN KEY 
	(
		[Contact_Name_Key]
	) REFERENCES [INDIVIDUAL] (
		[NAME_KEY]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Movement_Of_Ownership_Movement_Direction' AND parent_obj=Object_ID('Movement_Of_Ownership'))
ALTER TABLE [dbo].[Movement_Of_Ownership] ADD CONSTRAINT [FK_Movement_Of_Ownership_Movement_Direction] FOREIGN KEY 
	(
		[Movement_Direction_Key]
	) REFERENCES [Movement_Direction] (
		[Movement_Direction_Key]
	)
GO


SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Movement_Of_OwnershipCustodianInsert]') and OBJECTPROPERTY(id, N'IsTrigger') = 1)
drop trigger [dbo].[Movement_Of_OwnershipCustodianInsert]
GO


CREATE TRIGGER Movement_Of_OwnershipCustodianInsert ON dbo.Movement_Of_Ownership AFTER INSERT AS UPDATE Movement_Of_Ownership SET Movement_Of_Ownership.CUSTODIAN = SUBSTRING(Movement_Of_Ownership.Movement_Of_Ownership_Key, 1, 8) FROM Movement_Of_Ownership INNER JOIN INSERTED ON Movement_Of_Ownership.Movement_Of_Ownership_Key = INSERTED.Movement_Of_Ownership_Key WHERE Movement_Of_Ownership.CUSTODIAN IS NULL


GO
SET QUOTED_IDENTIFIER OFF 
GO
SET ANSI_NULLS ON 
GO


if not exists (select 1 from SysObjects where name='DF_Movement_Of_Ownership_Completed' AND Type='D')
ALTER TABLE [dbo].[Movement_Of_Ownership] ADD CONSTRAINT [DF_Movement_Of_Ownership_Completed] DEFAULT (0) FOR [Completed]
if not exists (select 1 from SysObjects where name='FK_Movement_Of_Ownership_Exclusion_Collection_Unit' AND parent_obj=Object_ID('Movement_Of_Ownership_Exclusion'))
ALTER TABLE [dbo].[Movement_Of_Ownership_Exclusion] ADD CONSTRAINT [FK_Movement_Of_Ownership_Exclusion_Collection_Unit] FOREIGN KEY 
	(
		[Collection_Unit_Key]
	) REFERENCES [Collection_Unit] (
		[Collection_Unit_Key]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Movement_Of_Ownership_Exclusion_Movement_Of_Ownership' AND parent_obj=Object_ID('Movement_Of_Ownership_Exclusion'))
ALTER TABLE [dbo].[Movement_Of_Ownership_Exclusion] ADD CONSTRAINT [FK_Movement_Of_Ownership_Exclusion_Movement_Of_Ownership] FOREIGN KEY 
	(
		[Movement_Of_Ownership_Key]
	) REFERENCES [Movement_Of_Ownership] (
		[Movement_Of_Ownership_Key]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Movement_Valuation_Movement' AND parent_obj=Object_ID('Movement_Valuation'))
ALTER TABLE [dbo].[Movement_Valuation] ADD CONSTRAINT [FK_Movement_Valuation_Movement] FOREIGN KEY 
	(
		[Movement_Key]
	) REFERENCES [Movement] (
		[Movement_Key]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Movement_Valuation_Valuation' AND parent_obj=Object_ID('Movement_Valuation'))
ALTER TABLE [dbo].[Movement_Valuation] ADD CONSTRAINT [FK_Movement_Valuation_Valuation] FOREIGN KEY 
	(
		[Valuation_Key]
	) REFERENCES [Valuation] (
		[Valuation_Key]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Occurrence_Record_Type' AND parent_obj=Object_ID('Occurrence'))
ALTER TABLE [dbo].[Occurrence] ADD CONSTRAINT [FK_Occurrence_Record_Type] FOREIGN KEY 
	(
		[Record_Type_Concept_Key]
	) REFERENCES [Concept] (
		[Concept_Key]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Occurrence_Sample' AND parent_obj=Object_ID('Occurrence'))
ALTER TABLE [dbo].[Occurrence] ADD CONSTRAINT [FK_Occurrence_Sample] FOREIGN KEY 
	(
		[Sample_Key]
	) REFERENCES [SAMPLE] (
		[SAMPLE_KEY]
	)
GO


if not exists (select 1 from dbo.sysindexes where name = N'IX_Occurrence_Sample' and id = object_id(N'[dbo].[Occurrence]'))
 CREATE  INDEX [IX_Occurrence_Sample] ON [dbo].[Occurrence]([Sample_Key]) ON [PRIMARY]
GO


SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[OccurrenceCustodianInsert]') and OBJECTPROPERTY(id, N'IsTrigger') = 1)
drop trigger [dbo].[OccurrenceCustodianInsert]
GO


CREATE TRIGGER OccurrenceCustodianInsert ON dbo.Occurrence AFTER INSERT AS UPDATE Occurrence SET Occurrence.CUSTODIAN = SUBSTRING(Occurrence.Occurrence_Key, 1, 8) FROM Occurrence INNER JOIN INSERTED ON Occurrence.Occurrence_Key = INSERTED.Occurrence_Key WHERE Occurrence.CUSTODIAN IS NULL


GO
SET QUOTED_IDENTIFIER OFF 
GO
SET ANSI_NULLS ON 
GO


if not exists (select 1 from SysObjects where name='DF_Occurrence_Zero_Abundance' AND Type='D')
ALTER TABLE [dbo].[Occurrence] ADD CONSTRAINT [DF_Occurrence_Zero_Abundance] DEFAULT (0) FOR [Zero_Abundance]
if not exists (select 1 from SysObjects where name='DF_Occurrence_Confidential' AND Type='D')
ALTER TABLE [dbo].[Occurrence] ADD CONSTRAINT [DF_Occurrence_Confidential] DEFAULT (0) FOR [Confidential]
if not exists (select 1 from SysObjects where name='DF_Occurrence_Verified' AND Type='D')
ALTER TABLE [dbo].[Occurrence] ADD CONSTRAINT [DF_Occurrence_Verified] DEFAULT (0) FOR [Verified]
if not exists (select 1 from SysObjects where name='DF_Occurrence_Checked' AND Type='D')
ALTER TABLE [dbo].[Occurrence] ADD CONSTRAINT [DF_Occurrence_Checked] DEFAULT (0) FOR [Checked]
if not exists (select 1 from SysObjects where name='FK_Occurrence_Data_Method' AND parent_obj=Object_ID('Occurrence_Data'))
ALTER TABLE [dbo].[Occurrence_Data] ADD CONSTRAINT [FK_Occurrence_Data_Method] FOREIGN KEY 
	(
		[Method_Concept_Key]
	) REFERENCES [Concept] (
		[Concept_Key]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Occurrence_Data_Occurrence' AND parent_obj=Object_ID('Occurrence_Data'))
ALTER TABLE [dbo].[Occurrence_Data] ADD CONSTRAINT [FK_Occurrence_Data_Occurrence] FOREIGN KEY 
	(
		[Occurrence_Key]
	) REFERENCES [Occurrence] (
		[Occurrence_Key]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Occurrence_Data_Parameter' AND parent_obj=Object_ID('Occurrence_Data'))
ALTER TABLE [dbo].[Occurrence_Data] ADD CONSTRAINT [FK_Occurrence_Data_Parameter] FOREIGN KEY 
	(
		[Parameter_Concept_Key]
	) REFERENCES [Concept] (
		[Concept_Key]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Occurrence_Data_Unit' AND parent_obj=Object_ID('Occurrence_Data'))
ALTER TABLE [dbo].[Occurrence_Data] ADD CONSTRAINT [FK_Occurrence_Data_Unit] FOREIGN KEY 
	(
		[Unit_Concept_Key]
	) REFERENCES [Concept] (
		[Concept_Key]
	)
GO


if not exists (select 1 from dbo.sysindexes where name = N'IX_Occurrence_Data_Occurrence' and id = object_id(N'[dbo].[Occurrence_Data]'))
 CREATE  INDEX [IX_Occurrence_Data_Occurrence] ON [dbo].[Occurrence_Data]([Occurrence_Key]) ON [PRIMARY]
GO


SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Occurrence_DataCustodianInsert]') and OBJECTPROPERTY(id, N'IsTrigger') = 1)
drop trigger [dbo].[Occurrence_DataCustodianInsert]
GO


CREATE TRIGGER Occurrence_DataCustodianInsert ON dbo.Occurrence_Data AFTER INSERT AS UPDATE Occurrence_Data SET Occurrence_Data.CUSTODIAN = SUBSTRING(Occurrence_Data.Occurrence_Data_Key, 1, 8) FROM Occurrence_Data INNER JOIN INSERTED ON Occurrence_Data.Occurrence_Data_Key = INSERTED.Occurrence_Data_Key WHERE Occurrence_Data.CUSTODIAN IS NULL


GO
SET QUOTED_IDENTIFIER OFF 
GO
SET ANSI_NULLS ON 
GO


if not exists (select 1 from SysObjects where name='FK_Occurrence_Relation_Occurrence_From' AND parent_obj=Object_ID('Occurrence_Relation'))
ALTER TABLE [dbo].[Occurrence_Relation] ADD CONSTRAINT [FK_Occurrence_Relation_Occurrence_From] FOREIGN KEY 
	(
		[From_Occurrence_Key]
	) REFERENCES [Occurrence] (
		[Occurrence_Key]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Occurrence_Relation_Occurrence_To' AND parent_obj=Object_ID('Occurrence_Relation'))
ALTER TABLE [dbo].[Occurrence_Relation] ADD CONSTRAINT [FK_Occurrence_Relation_Occurrence_To] FOREIGN KEY 
	(
		[To_Occurrence_Key]
	) REFERENCES [Occurrence] (
		[Occurrence_Key]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Occurrence_Relation_Thesaurus_Relation_Type' AND parent_obj=Object_ID('Occurrence_Relation'))
ALTER TABLE [dbo].[Occurrence_Relation] ADD CONSTRAINT [FK_Occurrence_Relation_Thesaurus_Relation_Type] FOREIGN KEY 
	(
		[Thesaurus_Relation_Type_Key]
	) REFERENCES [Thesaurus_Relation_Type] (
		[Thesaurus_Relation_Type_Key]
	)
GO


SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Occurrence_RelationCustodianInsert]') and OBJECTPROPERTY(id, N'IsTrigger') = 1)
drop trigger [dbo].[Occurrence_RelationCustodianInsert]
GO


CREATE TRIGGER Occurrence_RelationCustodianInsert ON dbo.Occurrence_Relation AFTER INSERT AS UPDATE Occurrence_Relation SET Occurrence_Relation.CUSTODIAN = SUBSTRING(Occurrence_Relation.Occurrence_Relation_Key, 1, 8) FROM Occurrence_Relation INNER JOIN INSERTED ON Occurrence_Relation.Occurrence_Relation_Key = INSERTED.Occurrence_Relation_Key WHERE Occurrence_Relation.CUSTODIAN IS NULL


GO
SET QUOTED_IDENTIFIER OFF 
GO
SET ANSI_NULLS ON 
GO


if not exists (select 1 from SysObjects where name='IX_QE_Data_Item_Unique' AND parent_obj=Object_ID('QE_Data_Item'))
ALTER TABLE [dbo].[QE_Data_Item] ADD CONSTRAINT [IX_QE_Data_Item_Unique] UNIQUE  NONCLUSTERED 
	(
		[QE_Data_Row_Key],
		[QE_Template_Field_Key]
	)  ON [PRIMARY] 
GO


if not exists (select 1 from SysObjects where name='FK_QE_Data_Item_QE_Data_Row' AND parent_obj=Object_ID('QE_Data_Item'))
ALTER TABLE [dbo].[QE_Data_Item] ADD CONSTRAINT [FK_QE_Data_Item_QE_Data_Row] FOREIGN KEY 
	(
		[QE_Data_Row_Key]
	) REFERENCES [QE_Data_Row] (
		[QE_Data_Row_Key]
	)
GO


if not exists (select 1 from SysObjects where name='FK_QE_Data_Item_QE_Template_Field' AND parent_obj=Object_ID('QE_Data_Item'))
ALTER TABLE [dbo].[QE_Data_Item] ADD CONSTRAINT [FK_QE_Data_Item_QE_Template_Field] FOREIGN KEY 
	(
		[QE_Template_Field_Key]
	) REFERENCES [QE_Template_Field] (
		[QE_Template_Field_Key]
	)
GO


if not exists (select 1 from dbo.sysindexes where name = N'IX_QE_Data_Item_Unique' and id = object_id(N'[dbo].[QE_Data_Item]'))
 CREATE  UNIQUE  INDEX [IX_QE_Data_Item_Unique] ON [dbo].[QE_Data_Item]([QE_Data_Row_Key], [QE_Template_Field_Key]) ON [PRIMARY]
GO


if not exists (select 1 from dbo.sysindexes where name = N'IX_DATA_ROW' and id = object_id(N'[dbo].[QE_Data_Item]'))
 CREATE  INDEX [IX_DATA_ROW] ON [dbo].[QE_Data_Item]([QE_Data_Row_Key]) ON [PRIMARY]
GO


if not exists (select 1 from SysObjects where name='FK_QE_Data_Row_QE_Session' AND parent_obj=Object_ID('QE_Data_Row'))
ALTER TABLE [dbo].[QE_Data_Row] ADD CONSTRAINT [FK_QE_Data_Row_QE_Session] FOREIGN KEY 
	(
		[QE_Session_Key]
	) REFERENCES [QE_Session] (
		[QE_Session_Key]
	)
GO


if not exists (select 1 from dbo.sysindexes where name = N'IX_SESSION' and id = object_id(N'[dbo].[QE_Data_Row]'))
 CREATE  INDEX [IX_SESSION] ON [dbo].[QE_Data_Row]([QE_Session_Key]) ON [PRIMARY]
GO


if not exists (select 1 from SysObjects where name='DF_QE_Data_Row_General' AND Type='D')
ALTER TABLE [dbo].[QE_Data_Row] ADD CONSTRAINT [DF_QE_Data_Row_General] DEFAULT (0) FOR [General]
SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[QE_FieldCustodianInsert]') and OBJECTPROPERTY(id, N'IsTrigger') = 1)
drop trigger [dbo].[QE_FieldCustodianInsert]
GO


CREATE TRIGGER QE_FieldCustodianInsert ON dbo.QE_Field AFTER INSERT AS UPDATE QE_Field SET QE_Field.CUSTODIAN = SUBSTRING(QE_Field.QE_Field_Key, 1, 8) FROM QE_Field INNER JOIN INSERTED ON QE_Field.QE_Field_Key = INSERTED.QE_Field_Key WHERE QE_Field.CUSTODIAN IS NULL


GO
SET QUOTED_IDENTIFIER OFF 
GO
SET ANSI_NULLS ON 
GO


if not exists (select 1 from SysObjects where name='DF_QE_Field_Data_Type' AND Type='D')
ALTER TABLE [dbo].[QE_Field] ADD CONSTRAINT [DF_QE_Field_Data_Type] DEFAULT (0) FOR [Data_Type]
if not exists (select 1 from SysObjects where name='FK_QE_Session_QE_Template' AND parent_obj=Object_ID('QE_Session'))
ALTER TABLE [dbo].[QE_Session] ADD CONSTRAINT [FK_QE_Session_QE_Template] FOREIGN KEY 
	(
		[QE_Template_Key]
	) REFERENCES [QE_Template] (
		[QE_Template_Key]
	)
GO


if not exists (select 1 from SysObjects where name='FK_QE_Template_Subject_Area' AND parent_obj=Object_ID('QE_Template'))
ALTER TABLE [dbo].[QE_Template] ADD CONSTRAINT [FK_QE_Template_Subject_Area] FOREIGN KEY 
	(
		[Subject_Area_Key]
	) REFERENCES [Subject_Area] (
		[Subject_Area_Key]
	)
GO


if not exists (select 1 from SysObjects where name='FK_QE_Template_Field_Method' AND parent_obj=Object_ID('QE_Template_Field'))
ALTER TABLE [dbo].[QE_Template_Field] ADD CONSTRAINT [FK_QE_Template_Field_Method] FOREIGN KEY 
	(
		[Measurement_Method_Concept_Key]
	) REFERENCES [Concept] (
		[Concept_Key]
	)
GO


if not exists (select 1 from SysObjects where name='FK_QE_Template_Field_Parameter' AND parent_obj=Object_ID('QE_Template_Field'))
ALTER TABLE [dbo].[QE_Template_Field] ADD CONSTRAINT [FK_QE_Template_Field_Parameter] FOREIGN KEY 
	(
		[Measurement_Parameter_Concept_Key]
	) REFERENCES [Concept] (
		[Concept_Key]
	)
GO


if not exists (select 1 from SysObjects where name='FK_QE_Template_Field_QE_Field' AND parent_obj=Object_ID('QE_Template_Field'))
ALTER TABLE [dbo].[QE_Template_Field] ADD CONSTRAINT [FK_QE_Template_Field_QE_Field] FOREIGN KEY 
	(
		[QE_Field_Key]
	) REFERENCES [QE_Field] (
		[QE_Field_Key]
	)
GO


if not exists (select 1 from SysObjects where name='FK_QE_Template_Field_QE_Template' AND parent_obj=Object_ID('QE_Template_Field'))
ALTER TABLE [dbo].[QE_Template_Field] ADD CONSTRAINT [FK_QE_Template_Field_QE_Template] FOREIGN KEY 
	(
		[QE_Template_Key]
	) REFERENCES [QE_Template] (
		[QE_Template_Key]
	)
GO


if not exists (select 1 from SysObjects where name='FK_QE_Template_Field_Unit' AND parent_obj=Object_ID('QE_Template_Field'))
ALTER TABLE [dbo].[QE_Template_Field] ADD CONSTRAINT [FK_QE_Template_Field_Unit] FOREIGN KEY 
	(
		[Measurement_Unit_Concept_Key]
	) REFERENCES [Concept] (
		[Concept_Key]
	)
GO


if not exists (select 1 from SysObjects where name='CK_QE_Template_Field' AND Type='C' AND parent_obj=Object_ID('QE_Template_Field'))
ALTER TABLE [dbo].[QE_Template_Field] ADD CONSTRAINT [CK_QE_Template_Field] CHECK ([General_Tab] = 1 or [Specimen_Tab] = 1)
SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Report_BlockCustodianInsert]') and OBJECTPROPERTY(id, N'IsTrigger') = 1)
drop trigger [dbo].[Report_BlockCustodianInsert]
GO


CREATE TRIGGER Report_BlockCustodianInsert ON dbo.Report_Block AFTER INSERT AS UPDATE Report_Block SET Report_Block.CUSTODIAN = SUBSTRING(Report_Block.Report_Block_Key, 1, 8) FROM Report_Block INNER JOIN INSERTED ON Report_Block.Report_Block_Key = INSERTED.Report_Block_Key WHERE Report_Block.CUSTODIAN IS NULL


GO
SET QUOTED_IDENTIFIER OFF 
GO
SET ANSI_NULLS ON 
GO


if not exists (select 1 from SysObjects where name='DF_Report_Block_System_Supplied_Data' AND Type='D')
ALTER TABLE [dbo].[Report_Block] ADD CONSTRAINT [DF_Report_Block_System_Supplied_Data] DEFAULT (0) FOR [System_Supplied_Data]
if not exists (select 1 from SysObjects where name='FK_Report_Block_In_Section_Report_Block' AND parent_obj=Object_ID('Report_Block_In_Section'))
ALTER TABLE [dbo].[Report_Block_In_Section] ADD CONSTRAINT [FK_Report_Block_In_Section_Report_Block] FOREIGN KEY 
	(
		[Report_Block_Key]
	) REFERENCES [Report_Block] (
		[Report_Block_Key]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Report_Block_In_Section_Report_Section' AND parent_obj=Object_ID('Report_Block_In_Section'))
ALTER TABLE [dbo].[Report_Block_In_Section] ADD CONSTRAINT [FK_Report_Block_In_Section_Report_Section] FOREIGN KEY 
	(
		[Report_Section_Key]
	) REFERENCES [Report_Section] (
		[Report_Section_Key]
	)
GO


if not exists (select 1 from dbo.sysindexes where name = N'_WA_Sys_Report_Section_Key_5225C93C' and id = object_id(N'[dbo].[Report_Block_In_Section]'))

SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Report_Block_In_SectionCustodianInsert]') and OBJECTPROPERTY(id, N'IsTrigger') = 1)
drop trigger [dbo].[Report_Block_In_SectionCustodianInsert]
GO


CREATE TRIGGER Report_Block_In_SectionCustodianInsert ON dbo.Report_Block_In_Section AFTER INSERT AS UPDATE Report_Block_In_Section SET Report_Block_In_Section.CUSTODIAN = SUBSTRING(Report_Block_In_Section.Report_Block_In_Section_Key, 1, 8) FROM Report_Block_In_Section INNER JOIN INSERTED ON Report_Block_In_Section.Report_Block_In_Section_Key = INSERTED.Report_Block_In_Section_Key WHERE Report_Block_In_Section.CUSTODIAN IS NULL


GO
SET QUOTED_IDENTIFIER OFF 
GO
SET ANSI_NULLS ON 
GO


if not exists (select 1 from SysObjects where name='DF_Report_Block_In_Section_System_Supplied_Data' AND Type='D')
ALTER TABLE [dbo].[Report_Block_In_Section] ADD CONSTRAINT [DF_Report_Block_In_Section_System_Supplied_Data] DEFAULT (0) FOR [System_Supplied_Data]
if not exists (select 1 from SysObjects where name='FK_Report_Block_Order_Report_Block' AND parent_obj=Object_ID('Report_Block_Order'))
ALTER TABLE [dbo].[Report_Block_Order] ADD CONSTRAINT [FK_Report_Block_Order_Report_Block] FOREIGN KEY 
	(
		[Report_Block_Key]
	) REFERENCES [Report_Block] (
		[Report_Block_Key]
	)
GO


if not exists (select 1 from dbo.sysindexes where name = N'_WA_Sys_Report_Block_Key_5319ED75' and id = object_id(N'[dbo].[Report_Block_Order]'))

SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Report_Block_OrderCustodianInsert]') and OBJECTPROPERTY(id, N'IsTrigger') = 1)
drop trigger [dbo].[Report_Block_OrderCustodianInsert]
GO


CREATE TRIGGER Report_Block_OrderCustodianInsert ON dbo.Report_Block_Order AFTER INSERT AS UPDATE Report_Block_Order SET Report_Block_Order.CUSTODIAN = SUBSTRING(Report_Block_Order.Report_Block_Order_Key, 1, 8) FROM Report_Block_Order INNER JOIN INSERTED ON Report_Block_Order.Report_Block_Order_Key = INSERTED.Report_Block_Order_Key WHERE Report_Block_Order.CUSTODIAN IS NULL


GO
SET QUOTED_IDENTIFIER OFF 
GO
SET ANSI_NULLS ON 
GO


if not exists (select 1 from SysObjects where name='DF_Report_Block_Order_System_Supplied_Dat' AND Type='D')
ALTER TABLE [dbo].[Report_Block_Order] ADD CONSTRAINT [DF_Report_Block_Order_System_Supplied_Dat] DEFAULT (0) FOR [System_Supplied_Data]
if not exists (select 1 from SysObjects where name='FK_Report_Section_Details_Report' AND parent_obj=Object_ID('Report_Section'))
ALTER TABLE [dbo].[Report_Section] ADD CONSTRAINT [FK_Report_Section_Details_Report] FOREIGN KEY 
	(
		[Details_Report_Key]
	) REFERENCES [Details_Report] (
		[Details_Report_Key]
	)
GO


if not exists (select 1 from dbo.sysindexes where name = N'_WA_Sys_Details_Report_Key_540E11AE' and id = object_id(N'[dbo].[Report_Section]'))

SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Report_SectionCustodianInsert]') and OBJECTPROPERTY(id, N'IsTrigger') = 1)
drop trigger [dbo].[Report_SectionCustodianInsert]
GO


CREATE TRIGGER Report_SectionCustodianInsert ON dbo.Report_Section AFTER INSERT AS UPDATE Report_Section SET Report_Section.CUSTODIAN = SUBSTRING(Report_Section.Report_Section_Key, 1, 8) FROM Report_Section INNER JOIN INSERTED ON Report_Section.Report_Section_Key = INSERTED.Report_Section_Key WHERE Report_Section.CUSTODIAN IS NULL


GO
SET QUOTED_IDENTIFIER OFF 
GO
SET ANSI_NULLS ON 
GO


if not exists (select 1 from SysObjects where name='DF_Report_Section_System_Supplied_Data' AND Type='D')
ALTER TABLE [dbo].[Report_Section] ADD CONSTRAINT [DF_Report_Section_System_Supplied_Data] DEFAULT (0) FOR [System_Supplied_Data]
SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Semantic_RelationCustodianInsert]') and OBJECTPROPERTY(id, N'IsTrigger') = 1)
drop trigger [dbo].[Semantic_RelationCustodianInsert]
GO


CREATE TRIGGER Semantic_RelationCustodianInsert ON dbo.Semantic_Relation AFTER INSERT AS UPDATE Semantic_Relation SET Semantic_Relation.CUSTODIAN = SUBSTRING(Semantic_Relation.Semantic_Relation_Key, 1, 8) FROM Semantic_Relation INNER JOIN INSERTED ON Semantic_Relation.Semantic_Relation_Key = INSERTED.Semantic_Relation_Key WHERE Semantic_Relation.CUSTODIAN IS NULL


GO
SET QUOTED_IDENTIFIER OFF 
GO
SET ANSI_NULLS ON 
GO


if not exists (select 1 from SysObjects where name='CK_Semantic_Relation_Chronological_Overlap' AND Type='C' AND parent_obj=Object_ID('Semantic_Relation'))
ALTER TABLE [dbo].[Semantic_Relation] ADD CONSTRAINT [CK_Semantic_Relation_Chronological_Overlap] CHECK ([Chronological_Overlap] = 9 or ([Chronological_Overlap] = 8 or ([Chronological_Overlap] = 7 or ([Chronological_Overlap] = 6 or ([Chronological_Overlap] = 5 or ([Chronological_Overlap] = 4 or ([Chronological_Overlap] = 3 or ([Chronological_Overlap] = 2 or [Chronological_Overlap] = 1))))))) or [Chronological_Overlap] = 0 or [Chronological_Overlap] is null)
if not exists (select 1 from SysObjects where name='DF_Semantic_Relation_Unidirectional' AND Type='D')
ALTER TABLE [dbo].[Semantic_Relation] ADD CONSTRAINT [DF_Semantic_Relation_Unidirectional] DEFAULT (0) FOR [Unidirectional]
if not exists (select 1 from SysObjects where name='DF_Session_Date_Time_Start' AND Type='D')
ALTER TABLE [dbo].[Session] ADD CONSTRAINT [DF_Session_Date_Time_Start] DEFAULT (getdate()) FOR [Date_Time_Start]
if not exists (select 1 from SysObjects where name='FK_Source_Join_Source' AND parent_obj=Object_ID('Source_Join'))
ALTER TABLE [dbo].[Source_Join] ADD CONSTRAINT [FK_Source_Join_Source] FOREIGN KEY 
	(
		[Source_Key]
	) REFERENCES [SOURCE] (
		[SOURCE_KEY]
	)
GO


if not exists (select 1 from dbo.sysindexes where name = N'IX_Source_Join_Record' and id = object_id(N'[dbo].[Source_Join]'))
 CREATE  INDEX [IX_Source_Join_Record] ON [dbo].[Source_Join]([Record_Key]) ON [PRIMARY]
GO


if not exists (select 1 from SysObjects where name='DF_Source_Join_Original' AND Type='D')
ALTER TABLE [dbo].[Source_Join] ADD CONSTRAINT [DF_Source_Join_Original] DEFAULT (0) FOR [Original]
if not exists (select 1 from SysObjects where name='IX_Unique_Specimen_Occurrence' AND parent_obj=Object_ID('Specimen_Field_Data'))
ALTER TABLE [dbo].[Specimen_Field_Data] ADD CONSTRAINT [IX_Unique_Specimen_Occurrence] UNIQUE  NONCLUSTERED 
	(
		[Collection_Unit_Key],
		[Occurrence_Key],
		[Taxon_Occurrence_Key]
	)  ON [PRIMARY] 
GO


if not exists (select 1 from SysObjects where name='FK_Specimen_Field_Data_Occurrence' AND parent_obj=Object_ID('Specimen_Field_Data'))
ALTER TABLE [dbo].[Specimen_Field_Data] ADD CONSTRAINT [FK_Specimen_Field_Data_Occurrence] FOREIGN KEY 
	(
		[Occurrence_Key]
	) REFERENCES [Occurrence] (
		[Occurrence_Key]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Specimen_Field_Data_Specimen' AND parent_obj=Object_ID('Specimen_Field_Data'))
ALTER TABLE [dbo].[Specimen_Field_Data] ADD CONSTRAINT [FK_Specimen_Field_Data_Specimen] FOREIGN KEY 
	(
		[Collection_Unit_Key]
	) REFERENCES [Specimen_Unit] (
		[Collection_Unit_Key]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Specimen_Field_Data_Taxon_Occurrence' AND parent_obj=Object_ID('Specimen_Field_Data'))
ALTER TABLE [dbo].[Specimen_Field_Data] ADD CONSTRAINT [FK_Specimen_Field_Data_Taxon_Occurrence] FOREIGN KEY 
	(
		[Taxon_Occurrence_Key]
	) REFERENCES [TAXON_OCCURRENCE] (
		[TAXON_OCCURRENCE_KEY]
	)
GO


if not exists (select 1 from dbo.sysindexes where name = N'IX_Unique_Specimen_Occurrence' and id = object_id(N'[dbo].[Specimen_Field_Data]'))
 CREATE  UNIQUE  INDEX [IX_Unique_Specimen_Occurrence] ON [dbo].[Specimen_Field_Data]([Collection_Unit_Key], [Occurrence_Key], [Taxon_Occurrence_Key]) ON [PRIMARY]
GO


if not exists (select 1 from dbo.sysindexes where name = N'IX_Specimen_Key' and id = object_id(N'[dbo].[Specimen_Field_Data]'))
 CREATE  INDEX [IX_Specimen_Key] ON [dbo].[Specimen_Field_Data]([Collection_Unit_Key]) ON [PRIMARY]
GO


SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Specimen_Field_DataCustodianInsert]') and OBJECTPROPERTY(id, N'IsTrigger') = 1)
drop trigger [dbo].[Specimen_Field_DataCustodianInsert]
GO


CREATE TRIGGER Specimen_Field_DataCustodianInsert ON dbo.Specimen_Field_Data AFTER INSERT AS UPDATE Specimen_Field_Data SET Specimen_Field_Data.CUSTODIAN = SUBSTRING(Specimen_Field_Data.Specimen_Field_Data_Key, 1, 8) FROM Specimen_Field_Data INNER JOIN INSERTED ON Specimen_Field_Data.Specimen_Field_Data_Key = INSERTED.Specimen_Field_Data_Key WHERE Specimen_Field_Data.CUSTODIAN IS NULL


GO
SET QUOTED_IDENTIFIER OFF 
GO
SET ANSI_NULLS ON 
GO


SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[tr_SpecimenFieldData_SingleGatheringEventPerSpecimen]') and OBJECTPROPERTY(id, N'IsTrigger') = 1)
drop trigger [dbo].[tr_SpecimenFieldData_SingleGatheringEventPerSpecimen]
GO



/*===========================================================================*\
  Description:	Update Gathering_Event so that Specimens can have no more than one
				gathering event.

  Type:		AFTER INSERT, UPDATE

  Created:	October 2003

  Last revision information:
    $Revision: 2 $
    $Date: 9/01/06 13:42 $
    $Author: Johnvanbreda $

\*===========================================================================*/

CREATE TRIGGER [tr_SpecimenFieldData_SingleGatheringEventPerSpecimen] ON [dbo].[SPECIMEN_FIELD_DATA] 
FOR INSERT, UPDATE
AS

DECLARE @CurrentSFDKey CHAR(16)
DECLARE @GatheringEvent BIT
DECLARE @CurrentSUKey CHAR(16)

SELECT @CurrentSFDKey = Specimen_Field_Data_Key, 
	@GatheringEvent = Gathering_Event, 
	@CurrentSUKey = Collection_Unit_Key 
FROM INSERTED

IF @GatheringEvent = 1
	UPDATE SPECIMEN_FIELD_DATA
	SET Gathering_Event = 0
	WHERE Collection_Unit_Key = @CurrentSUKey
		AND Specimen_Field_Data_Key <> @CurrentSFDKey


GO
SET QUOTED_IDENTIFIER OFF 
GO
SET ANSI_NULLS ON 
GO


if not exists (select 1 from SysObjects where name='DF_Specimen_Field_Data_Inferred_Survey_Organiser' AND Type='D')
ALTER TABLE [dbo].[Specimen_Field_Data] ADD CONSTRAINT [DF_Specimen_Field_Data_Inferred_Survey_Organiser] DEFAULT (0) FOR [Inferred_Survey]
if not exists (select 1 from SysObjects where name='DF_Specimen_Field_Data_Inferred_Location' AND Type='D')
ALTER TABLE [dbo].[Specimen_Field_Data] ADD CONSTRAINT [DF_Specimen_Field_Data_Inferred_Location] DEFAULT (0) FOR [Inferred_Location]
if not exists (select 1 from SysObjects where name='DF_Specimen_Field_Data_Inferred_Spatial_Ref' AND Type='D')
ALTER TABLE [dbo].[Specimen_Field_Data] ADD CONSTRAINT [DF_Specimen_Field_Data_Inferred_Spatial_Ref] DEFAULT (0) FOR [Inferred_Spatial_Ref]
if not exists (select 1 from SysObjects where name='DF_Specimen_Field_Data_Inferred_Sample_Type' AND Type='D')
ALTER TABLE [dbo].[Specimen_Field_Data] ADD CONSTRAINT [DF_Specimen_Field_Data_Inferred_Sample_Type] DEFAULT (0) FOR [Inferred_Sample_Type]
if not exists (select 1 from SysObjects where name='DF_Specimen_Field_Data_Inferred_Date' AND Type='D')
ALTER TABLE [dbo].[Specimen_Field_Data] ADD CONSTRAINT [DF_Specimen_Field_Data_Inferred_Date] DEFAULT (0) FOR [Inferred_Date]
if not exists (select 1 from SysObjects where name='DF_Specimen_Field_Data_Inferred_Collectors' AND Type='D')
ALTER TABLE [dbo].[Specimen_Field_Data] ADD CONSTRAINT [DF_Specimen_Field_Data_Inferred_Collectors] DEFAULT (0) FOR [Inferred_Collectors]
if not exists (select 1 from SysObjects where name='FK_Specimen_Label_Confidence' AND parent_obj=Object_ID('Specimen_Label'))
ALTER TABLE [dbo].[Specimen_Label] ADD CONSTRAINT [FK_Specimen_Label_Confidence] FOREIGN KEY 
	(
		[Confidence_Concept_Key]
	) REFERENCES [Concept] (
		[Concept_Key]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Specimen_Label_Individual' AND parent_obj=Object_ID('Specimen_Label'))
ALTER TABLE [dbo].[Specimen_Label] ADD CONSTRAINT [FK_Specimen_Label_Individual] FOREIGN KEY 
	(
		[Author_Name_Key]
	) REFERENCES [INDIVIDUAL] (
		[NAME_KEY]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Specimen_Label_Language' AND parent_obj=Object_ID('Specimen_Label'))
ALTER TABLE [dbo].[Specimen_Label] ADD CONSTRAINT [FK_Specimen_Label_Language] FOREIGN KEY 
	(
		[Translated_Language_Key]
	) REFERENCES [Language] (
		[Language_Key]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Specimen_Label_Specimen_Unit' AND parent_obj=Object_ID('Specimen_Label'))
ALTER TABLE [dbo].[Specimen_Label] ADD CONSTRAINT [FK_Specimen_Label_Specimen_Unit] FOREIGN KEY 
	(
		[Collection_Unit_Key]
	) REFERENCES [Specimen_Unit] (
		[Collection_Unit_Key]
	)
GO


SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Specimen_LabelCustodianInsert]') and OBJECTPROPERTY(id, N'IsTrigger') = 1)
drop trigger [dbo].[Specimen_LabelCustodianInsert]
GO


CREATE TRIGGER Specimen_LabelCustodianInsert ON dbo.Specimen_Label AFTER INSERT AS UPDATE Specimen_Label SET Specimen_Label.CUSTODIAN = SUBSTRING(Specimen_Label.Specimen_Label_Key, 1, 8) FROM Specimen_Label INNER JOIN INSERTED ON Specimen_Label.Specimen_Label_Key = INSERTED.Specimen_Label_Key WHERE Specimen_Label.CUSTODIAN IS NULL


GO
SET QUOTED_IDENTIFIER OFF 
GO
SET ANSI_NULLS ON 
GO


if not exists (select 1 from SysObjects where name='DF_Specimen_Label_Inferred_Author' AND Type='D')
ALTER TABLE [dbo].[Specimen_Label] ADD CONSTRAINT [DF_Specimen_Label_Inferred_Author] DEFAULT (0) FOR [Inferred_Author]
if not exists (select 1 from SysObjects where name='FK_Specimen_Collection' AND parent_obj=Object_ID('Specimen_Unit'))
ALTER TABLE [dbo].[Specimen_Unit] ADD CONSTRAINT [FK_Specimen_Collection] FOREIGN KEY 
	(
		[Parent_Collection_Collection_Unit_Key]
	) REFERENCES [Collection] (
		[Collection_Unit_Key]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Specimen_Collection_Unit' AND parent_obj=Object_ID('Specimen_Unit'))
ALTER TABLE [dbo].[Specimen_Unit] ADD CONSTRAINT [FK_Specimen_Collection_Unit] FOREIGN KEY 
	(
		[Collection_Unit_Key]
	) REFERENCES [Collection_Unit] (
		[Collection_Unit_Key]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Specimen_Concept' AND parent_obj=Object_ID('Specimen_Unit'))
ALTER TABLE [dbo].[Specimen_Unit] ADD CONSTRAINT [FK_Specimen_Concept] FOREIGN KEY 
	(
		[Specimen_Type_Concept_Key]
	) REFERENCES [Concept] (
		[Concept_Key]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Specimen_Unit_Determination' AND parent_obj=Object_ID('Specimen_Unit'))
ALTER TABLE [dbo].[Specimen_Unit] ADD CONSTRAINT [FK_Specimen_Unit_Determination] FOREIGN KEY 
	(
		[Preferred_Determination_Key]
	) REFERENCES [Determination] (
		[Determination_Key]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Specimen_Unit_REFERENCE' AND parent_obj=Object_ID('Specimen_Unit'))
ALTER TABLE [dbo].[Specimen_Unit] ADD CONSTRAINT [FK_Specimen_Unit_REFERENCE] FOREIGN KEY 
	(
		[Material_Source_Key]
	) REFERENCES [REFERENCE] (
		[SOURCE_KEY]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Specimen_Unit_TAXON_DETERMINATION' AND parent_obj=Object_ID('Specimen_Unit'))
ALTER TABLE [dbo].[Specimen_Unit] ADD CONSTRAINT [FK_Specimen_Unit_TAXON_DETERMINATION] FOREIGN KEY 
	(
		[Preferred_Taxon_Determination_Key]
	) REFERENCES [TAXON_DETERMINATION] (
		[TAXON_DETERMINATION_KEY]
	)
GO


if not exists (select 1 from dbo.sysindexes where name = N'IX_Specimen_Collection' and id = object_id(N'[dbo].[Specimen_Unit]'))
 CREATE  INDEX [IX_Specimen_Collection] ON [dbo].[Specimen_Unit]([Parent_Collection_Collection_Unit_Key]) ON [PRIMARY]
GO


if not exists (select 1 from SysObjects where name='DF_Specimen_Confidential' AND Type='D')
ALTER TABLE [dbo].[Specimen_Unit] ADD CONSTRAINT [DF_Specimen_Confidential] DEFAULT (0) FOR [Confidential]
if not exists (select 1 from SysObjects where name='DF_Specimen_Unit_Dangerous' AND Type='D')
ALTER TABLE [dbo].[Specimen_Unit] ADD CONSTRAINT [DF_Specimen_Unit_Dangerous] DEFAULT (0) FOR [Dangerous]
if not exists (select 1 from SysObjects where name='FK_Store_Collection_Unit' AND parent_obj=Object_ID('Store'))
ALTER TABLE [dbo].[Store] ADD CONSTRAINT [FK_Store_Collection_Unit] FOREIGN KEY 
	(
		[Collection_Unit_Key]
	) REFERENCES [Collection_Unit] (
		[Collection_Unit_Key]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Store_Concept' AND parent_obj=Object_ID('Store'))
ALTER TABLE [dbo].[Store] ADD CONSTRAINT [FK_Store_Concept] FOREIGN KEY 
	(
		[Store_Type_Concept_Key]
	) REFERENCES [Concept] (
		[Concept_Key]
	)
GO


SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Subject_AreaCustodianInsert]') and OBJECTPROPERTY(id, N'IsTrigger') = 1)
drop trigger [dbo].[Subject_AreaCustodianInsert]
GO


CREATE TRIGGER Subject_AreaCustodianInsert ON dbo.Subject_Area AFTER INSERT AS UPDATE Subject_Area SET Subject_Area.CUSTODIAN = SUBSTRING(Subject_Area.Subject_Area_Key, 1, 8) FROM Subject_Area INNER JOIN INSERTED ON Subject_Area.Subject_Area_Key = INSERTED.Subject_Area_Key WHERE Subject_Area.CUSTODIAN IS NULL


GO
SET QUOTED_IDENTIFIER OFF 
GO
SET ANSI_NULLS ON 
GO


if not exists (select 1 from SysObjects where name='FK_Taxon_Dictionary_Concept_Designation_Mapping_Concept_Designation' AND parent_obj=Object_ID('Taxon_Dictionary_Concept_Designation_Mapping'))
ALTER TABLE [dbo].[Taxon_Dictionary_Concept_Designation_Mapping] ADD CONSTRAINT [FK_Taxon_Dictionary_Concept_Designation_Mapping_Concept_Designation] FOREIGN KEY 
	(
		[Concept_Designation_Key]
	) REFERENCES [Concept_Designation] (
		[Concept_Designation_Key]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Taxon_Dictionary_Concept_Designation_Mapping_Source_Join' AND parent_obj=Object_ID('Taxon_Dictionary_Concept_Designation_Mapping'))
ALTER TABLE [dbo].[Taxon_Dictionary_Concept_Designation_Mapping] ADD CONSTRAINT [FK_Taxon_Dictionary_Concept_Designation_Mapping_Source_Join] FOREIGN KEY 
	(
		[Source_Join_Key]
	) REFERENCES [Source_Join] (
		[Source_Join_Key]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Taxon_Dictionary_Concept_Designation_Mapping_TAXON_DESIGNATION' AND parent_obj=Object_ID('Taxon_Dictionary_Concept_Designation_Mapping'))
ALTER TABLE [dbo].[Taxon_Dictionary_Concept_Designation_Mapping] ADD CONSTRAINT [FK_Taxon_Dictionary_Concept_Designation_Mapping_TAXON_DESIGNATION] FOREIGN KEY 
	(
		[Taxon_Designation_Key]
	) REFERENCES [TAXON_DESIGNATION] (
		[TAXON_DESIGNATION_KEY]
	)
GO


if not exists (select 1 from dbo.sysindexes where name = N'IX_Concept_Designation_Key' and id = object_id(N'[dbo].[Taxon_Dictionary_Concept_Designation_Mapping]'))
 CREATE  INDEX [IX_Concept_Designation_Key] ON [dbo].[Taxon_Dictionary_Concept_Designation_Mapping]([Concept_Designation_Key]) ON [PRIMARY]
GO


if not exists (select 1 from SysObjects where name='FK_Taxon_Dictionary_Concept_Group_Mapping_Concept_Group' AND parent_obj=Object_ID('Taxon_Dictionary_Concept_Group_Mapping'))
ALTER TABLE [dbo].[Taxon_Dictionary_Concept_Group_Mapping] ADD CONSTRAINT [FK_Taxon_Dictionary_Concept_Group_Mapping_Concept_Group] FOREIGN KEY 
	(
		[Concept_Group_Key]
	) REFERENCES [Concept_Group] (
		[Concept_Group_Key]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Taxon_Dictionary_Concept_Group_Mapping_TAXON_LIST' AND parent_obj=Object_ID('Taxon_Dictionary_Concept_Group_Mapping'))
ALTER TABLE [dbo].[Taxon_Dictionary_Concept_Group_Mapping] ADD CONSTRAINT [FK_Taxon_Dictionary_Concept_Group_Mapping_TAXON_LIST] FOREIGN KEY 
	(
		[Taxon_List_Key]
	) REFERENCES [TAXON_LIST] (
		[TAXON_LIST_KEY]
	)
GO


if not exists (select 1 from dbo.sysindexes where name = N'IX_Concept_Group_Key' and id = object_id(N'[dbo].[Taxon_Dictionary_Concept_Group_Mapping]'))
 CREATE  INDEX [IX_Concept_Group_Key] ON [dbo].[Taxon_Dictionary_Concept_Group_Mapping]([Concept_Group_Key]) ON [PRIMARY]
GO


if not exists (select 1 from SysObjects where name='FK_Taxon_Dictionary_Concept_Group_Version_Mapping_Concept_Group_Version' AND parent_obj=Object_ID('Taxon_Dictionary_Concept_Group_Version_Mapping'))
ALTER TABLE [dbo].[Taxon_Dictionary_Concept_Group_Version_Mapping] ADD CONSTRAINT [FK_Taxon_Dictionary_Concept_Group_Version_Mapping_Concept_Group_Version] FOREIGN KEY 
	(
		[Concept_Group_Version_Key]
	) REFERENCES [Concept_Group_Version] (
		[Concept_Group_Version_Key]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Taxon_Dictionary_Concept_Group_Version_Mapping_Source_Join' AND parent_obj=Object_ID('Taxon_Dictionary_Concept_Group_Version_Mapping'))
ALTER TABLE [dbo].[Taxon_Dictionary_Concept_Group_Version_Mapping] ADD CONSTRAINT [FK_Taxon_Dictionary_Concept_Group_Version_Mapping_Source_Join] FOREIGN KEY 
	(
		[Source_Join_Key]
	) REFERENCES [Source_Join] (
		[Source_Join_Key]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Taxon_Dictionary_Concept_Group_Version_Mapping_TAXON_LIST_VERSION' AND parent_obj=Object_ID('Taxon_Dictionary_Concept_Group_Version_Mapping'))
ALTER TABLE [dbo].[Taxon_Dictionary_Concept_Group_Version_Mapping] ADD CONSTRAINT [FK_Taxon_Dictionary_Concept_Group_Version_Mapping_TAXON_LIST_VERSION] FOREIGN KEY 
	(
		[Taxon_List_Version_Key]
	) REFERENCES [TAXON_LIST_VERSION] (
		[TAXON_LIST_VERSION_KEY]
	)
GO


if not exists (select 1 from dbo.sysindexes where name = N'IX_Concept_Group_Version_Key' and id = object_id(N'[dbo].[Taxon_Dictionary_Concept_Group_Version_Mapping]'))
 CREATE  INDEX [IX_Concept_Group_Version_Key] ON [dbo].[Taxon_Dictionary_Concept_Group_Version_Mapping]([Concept_Group_Version_Key]) ON [PRIMARY]
GO


if not exists (select 1 from SysObjects where name='FK_Taxon_Dictionary_Concept_Mapping_Concept' AND parent_obj=Object_ID('Taxon_Dictionary_Concept_Mapping'))
ALTER TABLE [dbo].[Taxon_Dictionary_Concept_Mapping] ADD CONSTRAINT [FK_Taxon_Dictionary_Concept_Mapping_Concept] FOREIGN KEY 
	(
		[Concept_Key]
	) REFERENCES [Concept] (
		[Concept_Key]
	) ON DELETE CASCADE 
GO


if not exists (select 1 from SysObjects where name='FK_Taxon_Dictionary_Concept_Mapping_TAXON_LIST_ITEM' AND parent_obj=Object_ID('Taxon_Dictionary_Concept_Mapping'))
ALTER TABLE [dbo].[Taxon_Dictionary_Concept_Mapping] ADD CONSTRAINT [FK_Taxon_Dictionary_Concept_Mapping_TAXON_LIST_ITEM] FOREIGN KEY 
	(
		[Taxon_List_Item_Key]
	) REFERENCES [TAXON_LIST_ITEM] (
		[TAXON_LIST_ITEM_KEY]
	)
GO


if not exists (select 1 from dbo.sysindexes where name = N'IX_Concept_Key' and id = object_id(N'[dbo].[Taxon_Dictionary_Concept_Mapping]'))
 CREATE  UNIQUE  INDEX [IX_Concept_Key] ON [dbo].[Taxon_Dictionary_Concept_Mapping]([Concept_Key]) ON [PRIMARY]
GO


if not exists (select 1 from SysObjects where name='FK_Taxon_Dictionary_Concept_Rank_Mapping_Concept_Rank' AND parent_obj=Object_ID('Taxon_Dictionary_Concept_Rank_Mapping'))
ALTER TABLE [dbo].[Taxon_Dictionary_Concept_Rank_Mapping] ADD CONSTRAINT [FK_Taxon_Dictionary_Concept_Rank_Mapping_Concept_Rank] FOREIGN KEY 
	(
		[Concept_Rank_Key]
	) REFERENCES [Concept_Rank] (
		[Concept_Rank_Key]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Taxon_Dictionary_Concept_Rank_Mapping_TAXON_RANK' AND parent_obj=Object_ID('Taxon_Dictionary_Concept_Rank_Mapping'))
ALTER TABLE [dbo].[Taxon_Dictionary_Concept_Rank_Mapping] ADD CONSTRAINT [FK_Taxon_Dictionary_Concept_Rank_Mapping_TAXON_RANK] FOREIGN KEY 
	(
		[Taxon_Rank_Key]
	) REFERENCES [TAXON_RANK] (
		[TAXON_RANK_KEY]
	)
GO


if not exists (select 1 from dbo.sysindexes where name = N'IX_Concept_Rank_Key' and id = object_id(N'[dbo].[Taxon_Dictionary_Concept_Rank_Mapping]'))
 CREATE  INDEX [IX_Concept_Rank_Key] ON [dbo].[Taxon_Dictionary_Concept_Rank_Mapping]([Concept_Rank_Key]) ON [PRIMARY]
GO


if not exists (select 1 from SysObjects where name='FK_Taxon_Dictionary_Designation_Type_Mapping_Concept' AND parent_obj=Object_ID('Taxon_Dictionary_Designation_Type_Mapping'))
ALTER TABLE [dbo].[Taxon_Dictionary_Designation_Type_Mapping] ADD CONSTRAINT [FK_Taxon_Dictionary_Designation_Type_Mapping_Concept] FOREIGN KEY 
	(
		[Concept_Designation_Type_Key]
	) REFERENCES [Concept] (
		[Concept_Key]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Taxon_Dictionary_Designation_Type_Mapping_TAXON_DESIGNATION_TYPE' AND parent_obj=Object_ID('Taxon_Dictionary_Designation_Type_Mapping'))
ALTER TABLE [dbo].[Taxon_Dictionary_Designation_Type_Mapping] ADD CONSTRAINT [FK_Taxon_Dictionary_Designation_Type_Mapping_TAXON_DESIGNATION_TYPE] FOREIGN KEY 
	(
		[Taxon_Designation_Type_Key]
	) REFERENCES [TAXON_DESIGNATION_TYPE] (
		[TAXON_DESIGNATION_TYPE_KEY]
	)
GO


if not exists (select 1 from dbo.sysindexes where name = N'IX_Concept_Designation_Type_Key' and id = object_id(N'[dbo].[Taxon_Dictionary_Designation_Type_Mapping]'))
 CREATE  INDEX [IX_Concept_Designation_Type_Key] ON [dbo].[Taxon_Dictionary_Designation_Type_Mapping]([Concept_Designation_Type_Key]) ON [PRIMARY]
GO


if not exists (select 1 from SysObjects where name='FK_Taxon_Dictionary_Meaning_Mapping_Meaning' AND parent_obj=Object_ID('Taxon_Dictionary_Meaning_Mapping'))
ALTER TABLE [dbo].[Taxon_Dictionary_Meaning_Mapping] ADD CONSTRAINT [FK_Taxon_Dictionary_Meaning_Mapping_Meaning] FOREIGN KEY 
	(
		[Meaning_Key]
	) REFERENCES [Meaning] (
		[Meaning_Key]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Taxon_Dictionary_Meaning_Mapping_TAXON_LIST_ITEM' AND parent_obj=Object_ID('Taxon_Dictionary_Meaning_Mapping'))
ALTER TABLE [dbo].[Taxon_Dictionary_Meaning_Mapping] ADD CONSTRAINT [FK_Taxon_Dictionary_Meaning_Mapping_TAXON_LIST_ITEM] FOREIGN KEY 
	(
		[Preferred_Name]
	) REFERENCES [TAXON_LIST_ITEM] (
		[TAXON_LIST_ITEM_KEY]
	)
GO


if not exists (select 1 from dbo.sysindexes where name = N'IX_Meaning_Key' and id = object_id(N'[dbo].[Taxon_Dictionary_Meaning_Mapping]'))
 CREATE  INDEX [IX_Meaning_Key] ON [dbo].[Taxon_Dictionary_Meaning_Mapping]([Meaning_Key]) ON [PRIMARY]
GO


if not exists (select 1 from SysObjects where name='FK_Taxon_Dictionary_Name_Type_Mapping_Concept' AND parent_obj=Object_ID('Taxon_Dictionary_Name_Type_Mapping'))
ALTER TABLE [dbo].[Taxon_Dictionary_Name_Type_Mapping] ADD CONSTRAINT [FK_Taxon_Dictionary_Name_Type_Mapping_Concept] FOREIGN KEY 
	(
		[Thesaurus_Name_Type_Key]
	) REFERENCES [Concept] (
		[Concept_Key]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Taxon_Dictionary_Name_Type_Mapping_TAXON_NAME_TYPE' AND parent_obj=Object_ID('Taxon_Dictionary_Name_Type_Mapping'))
ALTER TABLE [dbo].[Taxon_Dictionary_Name_Type_Mapping] ADD CONSTRAINT [FK_Taxon_Dictionary_Name_Type_Mapping_TAXON_NAME_TYPE] FOREIGN KEY 
	(
		[Taxon_Name_Type_Key]
	) REFERENCES [TAXON_NAME_TYPE] (
		[TAXON_NAME_TYPE_KEY]
	)
GO


if not exists (select 1 from dbo.sysindexes where name = N'IX_Thesaurus_Name_Type_Key' and id = object_id(N'[dbo].[Taxon_Dictionary_Name_Type_Mapping]'))
 CREATE  INDEX [IX_Thesaurus_Name_Type_Key] ON [dbo].[Taxon_Dictionary_Name_Type_Mapping]([Thesaurus_Name_Type_Key]) ON [PRIMARY]
GO


if not exists (select 1 from SysObjects where name='FK_Taxon_Dictionary_Term_Mapping_TAXON' AND parent_obj=Object_ID('Taxon_Dictionary_Term_Mapping'))
ALTER TABLE [dbo].[Taxon_Dictionary_Term_Mapping] ADD CONSTRAINT [FK_Taxon_Dictionary_Term_Mapping_TAXON] FOREIGN KEY 
	(
		[Taxon_Key]
	) REFERENCES [TAXON] (
		[TAXON_KEY]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Taxon_Dictionary_Term_Mapping_Term' AND parent_obj=Object_ID('Taxon_Dictionary_Term_Mapping'))
ALTER TABLE [dbo].[Taxon_Dictionary_Term_Mapping] ADD CONSTRAINT [FK_Taxon_Dictionary_Term_Mapping_Term] FOREIGN KEY 
	(
		[Term_Key]
	) REFERENCES [Term] (
		[Term_Key]
	) ON DELETE CASCADE 
GO


if not exists (select 1 from dbo.sysindexes where name = N'IX_Taxon_Key' and id = object_id(N'[dbo].[Taxon_Dictionary_Term_Mapping]'))
 CREATE  INDEX [IX_Taxon_Key] ON [dbo].[Taxon_Dictionary_Term_Mapping]([Taxon_Key]) ON [PRIMARY]
GO


if not exists (select 1 from dbo.sysindexes where name = N'IX_Term_Key' and id = object_id(N'[dbo].[Taxon_Dictionary_Term_Mapping]'))
 CREATE  INDEX [IX_Term_Key] ON [dbo].[Taxon_Dictionary_Term_Mapping]([Term_Key]) ON [PRIMARY]
GO


if not exists (select 1 from SysObjects where name='FK_Taxon_Dictionary_Term_Sources_Mapping_Source_Join' AND parent_obj=Object_ID('Taxon_Dictionary_Term_Sources_Mapping'))
ALTER TABLE [dbo].[Taxon_Dictionary_Term_Sources_Mapping] ADD CONSTRAINT [FK_Taxon_Dictionary_Term_Sources_Mapping_Source_Join] FOREIGN KEY 
	(
		[Source_Join_Key]
	) REFERENCES [Source_Join] (
		[Source_Join_Key]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Taxon_Dictionary_Term_Sources_Mapping_TAXON_SOURCES' AND parent_obj=Object_ID('Taxon_Dictionary_Term_Sources_Mapping'))
ALTER TABLE [dbo].[Taxon_Dictionary_Term_Sources_Mapping] ADD CONSTRAINT [FK_Taxon_Dictionary_Term_Sources_Mapping_TAXON_SOURCES] FOREIGN KEY 
	(
		[Source_Link_Key]
	) REFERENCES [TAXON_SOURCES] (
		[SOURCE_LINK_KEY]
	)
GO


if not exists (select 1 from dbo.sysindexes where name = N'IX_Source_Join_Key' and id = object_id(N'[dbo].[Taxon_Dictionary_Term_Sources_Mapping]'))
 CREATE  UNIQUE  INDEX [IX_Source_Join_Key] ON [dbo].[Taxon_Dictionary_Term_Sources_Mapping]([Source_Join_Key]) ON [PRIMARY]
GO


if not exists (select 1 from SysObjects where name='FK_Taxon_Dictionary_Term_Version_Mapping_Source_Join' AND parent_obj=Object_ID('Taxon_Dictionary_Term_Version_Mapping'))
ALTER TABLE [dbo].[Taxon_Dictionary_Term_Version_Mapping] ADD CONSTRAINT [FK_Taxon_Dictionary_Term_Version_Mapping_Source_Join] FOREIGN KEY 
	(
		[Source_Join_Key]
	) REFERENCES [Source_Join] (
		[Source_Join_Key]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Taxon_Dictionary_Term_Version_Mapping_TAXON_VERSION' AND parent_obj=Object_ID('Taxon_Dictionary_Term_Version_Mapping'))
ALTER TABLE [dbo].[Taxon_Dictionary_Term_Version_Mapping] ADD CONSTRAINT [FK_Taxon_Dictionary_Term_Version_Mapping_TAXON_VERSION] FOREIGN KEY 
	(
		[Taxon_Version_Key]
	) REFERENCES [TAXON_VERSION] (
		[TAXON_VERSION_KEY]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Taxon_Dictionary_Term_Version_Mapping_Term_Version' AND parent_obj=Object_ID('Taxon_Dictionary_Term_Version_Mapping'))
ALTER TABLE [dbo].[Taxon_Dictionary_Term_Version_Mapping] ADD CONSTRAINT [FK_Taxon_Dictionary_Term_Version_Mapping_Term_Version] FOREIGN KEY 
	(
		[Term_Version_Key]
	) REFERENCES [Term_Version] (
		[Term_Version_Key]
	) ON DELETE CASCADE 
GO


if not exists (select 1 from dbo.sysindexes where name = N'IX_Term_Version_Key' and id = object_id(N'[dbo].[Taxon_Dictionary_Term_Version_Mapping]'))
 CREATE  UNIQUE  INDEX [IX_Term_Version_Key] ON [dbo].[Taxon_Dictionary_Term_Version_Mapping]([Term_Version_Key]) ON [PRIMARY]
GO


if not exists (select 1 from SysObjects where name='FK_Taxon_Dictionary_Thesaurus_Fact_Mapping_Source_Join' AND parent_obj=Object_ID('Taxon_Dictionary_Thesaurus_Fact_Mapping'))
ALTER TABLE [dbo].[Taxon_Dictionary_Thesaurus_Fact_Mapping] ADD CONSTRAINT [FK_Taxon_Dictionary_Thesaurus_Fact_Mapping_Source_Join] FOREIGN KEY 
	(
		[Source_Join_Key]
	) REFERENCES [Source_Join] (
		[Source_Join_Key]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Taxon_Dictionary_Thesaurus_Fact_Mapping_TAXON_FACT' AND parent_obj=Object_ID('Taxon_Dictionary_Thesaurus_Fact_Mapping'))
ALTER TABLE [dbo].[Taxon_Dictionary_Thesaurus_Fact_Mapping] ADD CONSTRAINT [FK_Taxon_Dictionary_Thesaurus_Fact_Mapping_TAXON_FACT] FOREIGN KEY 
	(
		[Taxon_Fact_Key]
	) REFERENCES [TAXON_FACT] (
		[TAXON_FACT_KEY]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Taxon_Dictionary_Thesaurus_Fact_Mapping_Thesaurus_Fact' AND parent_obj=Object_ID('Taxon_Dictionary_Thesaurus_Fact_Mapping'))
ALTER TABLE [dbo].[Taxon_Dictionary_Thesaurus_Fact_Mapping] ADD CONSTRAINT [FK_Taxon_Dictionary_Thesaurus_Fact_Mapping_Thesaurus_Fact] FOREIGN KEY 
	(
		[Thesaurus_Fact_Key]
	) REFERENCES [Thesaurus_Fact] (
		[Thesaurus_Fact_Key]
	) ON DELETE CASCADE 
GO


if not exists (select 1 from dbo.sysindexes where name = N'IX_Thesaurus_Fact_Key' and id = object_id(N'[dbo].[Taxon_Dictionary_Thesaurus_Fact_Mapping]'))
 CREATE  UNIQUE  INDEX [IX_Thesaurus_Fact_Key] ON [dbo].[Taxon_Dictionary_Thesaurus_Fact_Mapping]([Thesaurus_Fact_Key]) ON [PRIMARY]
GO


if not exists (select 1 from SysObjects where name='IX_Term_Unique' AND parent_obj=Object_ID('Term'))
ALTER TABLE [dbo].[Term] ADD CONSTRAINT [IX_Term_Unique] UNIQUE  NONCLUSTERED 
	(
		[Language_Key],
		[Item_Name]
	)  ON [PRIMARY] 
GO


if not exists (select 1 from dbo.sysindexes where name = N'IX_Term_Unique' and id = object_id(N'[dbo].[Term]'))
 CREATE  UNIQUE  INDEX [IX_Term_Unique] ON [dbo].[Term]([Language_Key], [Item_Name]) ON [PRIMARY]
GO


if not exists (select 1 from dbo.sysindexes where name = N'IX_PlainText' and id = object_id(N'[dbo].[Term]'))
 CREATE  INDEX [IX_PlainText] ON [dbo].[Term]([Plaintext]) ON [PRIMARY]
GO


SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[TermCustodianInsert]') and OBJECTPROPERTY(id, N'IsTrigger') = 1)
drop trigger [dbo].[TermCustodianInsert]
GO


CREATE TRIGGER TermCustodianInsert ON dbo.Term AFTER INSERT AS UPDATE Term SET Term.CUSTODIAN = SUBSTRING(Term.Term_Key, 1, 8) FROM Term INNER JOIN INSERTED ON Term.Term_Key = INSERTED.Term_Key WHERE Term.CUSTODIAN IS NULL


GO
SET QUOTED_IDENTIFIER OFF 
GO
SET ANSI_NULLS ON 
GO


SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[tr_Term_DisplayCaptionUpdates]') and OBJECTPROPERTY(id, N'IsTrigger') = 1)
drop trigger [dbo].[tr_Term_DisplayCaptionUpdates]
GO


/*===========================================================================*\
  Description:	Update display caption on the following tables when an existing
				term is modified:
				Conservation Check

  Type:		AFTER UPDATE

  Created:	September 2003

  Last revision information:
    $Revision: 2 $
    $Date: 9/01/06 13:42 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE TRIGGER [dbo].[tr_Term_DisplayCaptionUpdates]
ON dbo.Term
AFTER UPDATE
AS 
	IF UPDATE(Item_Name) OR UPDATE(Plaintext)
	BEGIN
		UPDATE Conservation_Check 
		SET Search_Caption=
				dbo.ufn_GetDateFromVagueDate(CC.Vague_Date_Start, CC.Vague_Date_End, CC.Vague_Date_Type)
				+ ' - ' +
				T.Plaintext
				+ ' - ' +
				CC.Ref_Number,
			Display_Caption=
				dbo.ufn_GetDateFromVagueDate(CC.Vague_Date_Start, CC.Vague_Date_End, CC.Vague_Date_Type)
				+ ' - ' +
				T.Item_Name
				+ ' - ' +
				CC.Ref_Number
		FROM Conservation_Check CC
		INNER JOIN Concept C ON C.Concept_Key=CC.Type_Concept_Key
		INNER JOIN Term T ON T.Term_Key=C.Term_Key
		INNER JOIN Inserted I on T.Term_Key=I.Term_Key

		UPDATE Enquiry 
		SET Search_Caption=
				dbo.ufn_GetDateFromVagueDate(E.Vague_Date_Start, E.Vague_Date_End, E.Vague_Date_Type)
				+ ' - ' +
				T.Plaintext,
			Display_Caption=
				dbo.ufn_GetDateFromVagueDate(E.Vague_Date_Start, E.Vague_Date_End, E.Vague_Date_Type)
				+ ' - ' +
				T.Item_Name
		FROM Enquiry E
		INNER JOIN Concept C ON C.Concept_Key=E.Enquiry_Type_Concept_Key
		INNER JOIN Term T ON T.Term_Key=C.Term_Key
		INNER JOIN Inserted I on T.Term_Key=I.Term_Key

		UPDATE Valuation 
		SET Search_Caption=
				dbo.ufn_GetDateFromVagueDate(V.Vague_Date_Start, V.Vague_Date_End, V.Vague_Date_Type)
				+ ' - ' +
				T.Plaintext,
			Display_Caption=
				dbo.ufn_GetDateFromVagueDate(V.Vague_Date_Start, V.Vague_Date_End, V.Vague_Date_Type)
				+ ' - ' +
				T.Item_Name
		FROM Valuation V
		INNER JOIN Concept C ON C.Concept_Key=V.Type_Concept_Key
		INNER JOIN Term T ON T.Term_Key=C.Term_Key
		INNER JOIN Inserted I on T.Term_Key=I.Term_Key

	END


GO
SET QUOTED_IDENTIFIER OFF 
GO
SET ANSI_NULLS ON 
GO


if not exists (select 1 from SysObjects where name='DF_Term_System_Supplied_Data' AND Type='D')
ALTER TABLE [dbo].[Term] ADD CONSTRAINT [DF_Term_System_Supplied_Data] DEFAULT (0) FOR [System_Supplied_Data]
if not exists (select 1 from SysObjects where name='FK_Term_Version_Term' AND parent_obj=Object_ID('Term_Version'))
ALTER TABLE [dbo].[Term_Version] ADD CONSTRAINT [FK_Term_Version_Term] FOREIGN KEY 
	(
		[Term_Key]
	) REFERENCES [Term] (
		[Term_Key]
	)
GO


SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Term_VersionCustodianInsert]') and OBJECTPROPERTY(id, N'IsTrigger') = 1)
drop trigger [dbo].[Term_VersionCustodianInsert]
GO


 CREATE TRIGGER Term_VersionCustodianInsert ON dbo.Term_Version AFTER INSERT AS UPDATE Term_Version SET Term_Version.CUSTODIAN = SUBSTRING(Term_Version.Term_Version_Key, 1, 8) FROM Term_Version INNER JOIN INSERTED ON Term_Version.Term_Version_Key = INSERTED.Term_Version_Key WHERE Term_Version.CUSTODIAN IS NULL

GO
SET QUOTED_IDENTIFIER OFF 
GO
SET ANSI_NULLS ON 
GO


SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[tr_TermVersion_AuthorCopy]') and OBJECTPROPERTY(id, N'IsTrigger') = 1)
drop trigger [dbo].[tr_TermVersion_AuthorCopy]
GO



/*===========================================================================*\
  Description:	This trigger updates the Author_Copy field in the Concept 
		table when the Author_And_Date field in the Term_Version 
		table is updated

  Created:	Nov 2003

  Last revision information:
    $Revision: 2 $
    $Date: 9/01/06 13:42 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE TRIGGER [dbo].[tr_TermVersion_AuthorCopy] ON [dbo].[Term_Version] 
FOR UPDATE, INSERT

AS
	IF UPDATE (Author_And_Date)
	BEGIN
		UPDATE Concept
		SET Concept.Author_Copy=I.Author_And_Date
		FROM Concept INNER JOIN Inserted I
		ON Concept.Term_Version_Key=I.Term_Version_Key
	
		IF @@ERROR <>0
			RAISERROR('Error updating Author_Copy in Concept table',16,1)
	END


GO
SET QUOTED_IDENTIFIER OFF 
GO
SET ANSI_NULLS ON 
GO


if not exists (select 1 from SysObjects where name='DF_Author_And_Version_System_Supplied_Data' AND Type='D')
ALTER TABLE [dbo].[Term_Version] ADD CONSTRAINT [DF_Author_And_Version_System_Supplied_Data] DEFAULT (0) FOR [System_Supplied_Data]
if not exists (select 1 from SysObjects where name='FK_Term_Version_Relation_Term_Version_From' AND parent_obj=Object_ID('Term_Version_Relation'))
ALTER TABLE [dbo].[Term_Version_Relation] ADD CONSTRAINT [FK_Term_Version_Relation_Term_Version_From] FOREIGN KEY 
	(
		[From_Term_Version_Key]
	) REFERENCES [Term_Version] (
		[Term_Version_Key]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Term_Version_Relation_Term_Version_To' AND parent_obj=Object_ID('Term_Version_Relation'))
ALTER TABLE [dbo].[Term_Version_Relation] ADD CONSTRAINT [FK_Term_Version_Relation_Term_Version_To] FOREIGN KEY 
	(
		[To_Term_Version_Key]
	) REFERENCES [Term_Version] (
		[Term_Version_Key]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Term_Version_Relation_Thesaurus_Relation_Type' AND parent_obj=Object_ID('Term_Version_Relation'))
ALTER TABLE [dbo].[Term_Version_Relation] ADD CONSTRAINT [FK_Term_Version_Relation_Thesaurus_Relation_Type] FOREIGN KEY 
	(
		[Thesaurus_Relation_Type_Key]
	) REFERENCES [Thesaurus_Relation_Type] (
		[Thesaurus_Relation_Type_Key]
	)
GO


if not exists (select 1 from dbo.sysindexes where name = N'IX_Term_Version_Relation_From' and id = object_id(N'[dbo].[Term_Version_Relation]'))
 CREATE  INDEX [IX_Term_Version_Relation_From] ON [dbo].[Term_Version_Relation]([From_Term_Version_Key]) ON [PRIMARY]
GO


if not exists (select 1 from dbo.sysindexes where name = N'IX_Term_Version_Relation_To' and id = object_id(N'[dbo].[Term_Version_Relation]'))
 CREATE  INDEX [IX_Term_Version_Relation_To] ON [dbo].[Term_Version_Relation]([Term_Version_Relation_Key], [To_Term_Version_Key]) ON [PRIMARY]
GO


SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Thesaurus_FactCustodianInsert]') and OBJECTPROPERTY(id, N'IsTrigger') = 1)
drop trigger [dbo].[Thesaurus_FactCustodianInsert]
GO


CREATE TRIGGER Thesaurus_FactCustodianInsert ON dbo.Thesaurus_Fact AFTER INSERT AS UPDATE Thesaurus_Fact SET Thesaurus_Fact.CUSTODIAN = SUBSTRING(Thesaurus_Fact.Thesaurus_Fact_Key, 1, 8) FROM Thesaurus_Fact INNER JOIN INSERTED ON Thesaurus_Fact.Thesaurus_Fact_Key = INSERTED.Thesaurus_Fact_Key WHERE Thesaurus_Fact.CUSTODIAN IS NULL


GO
SET QUOTED_IDENTIFIER OFF 
GO
SET ANSI_NULLS ON 
GO


if not exists (select 1 from SysObjects where name='DF_Thesaurus_Fact_Related_Term_Versions' AND Type='D')
ALTER TABLE [dbo].[Thesaurus_Fact] ADD CONSTRAINT [DF_Thesaurus_Fact_Related_Term_Versions] DEFAULT (0) FOR [Related_Term_Versions]
if not exists (select 1 from SysObjects where name='DF_Thesaurus_Fact_Inherited' AND Type='D')
ALTER TABLE [dbo].[Thesaurus_Fact] ADD CONSTRAINT [DF_Thesaurus_Fact_Inherited] DEFAULT (0) FOR [Inherited]
if not exists (select 1 from SysObjects where name='FK_Thesaurus_Relation_Type_Semantic_Relation' AND parent_obj=Object_ID('Thesaurus_Relation_Type'))
ALTER TABLE [dbo].[Thesaurus_Relation_Type] ADD CONSTRAINT [FK_Thesaurus_Relation_Type_Semantic_Relation] FOREIGN KEY 
	(
		[Semantic_Relation_Key]
	) REFERENCES [Semantic_Relation] (
		[Semantic_Relation_Key]
	)
GO


if not exists (select 1 from dbo.sysindexes where name = N'_WA_Sys_Semantic_Relation_Key_6BE59B3F' and id = object_id(N'[dbo].[Thesaurus_Relation_Type]'))

SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Thesaurus_Relation_TypeCustodianInsert]') and OBJECTPROPERTY(id, N'IsTrigger') = 1)
drop trigger [dbo].[Thesaurus_Relation_TypeCustodianInsert]
GO


CREATE TRIGGER Thesaurus_Relation_TypeCustodianInsert ON dbo.Thesaurus_Relation_Type AFTER INSERT AS UPDATE Thesaurus_Relation_Type SET Thesaurus_Relation_Type.CUSTODIAN = SUBSTRING(Thesaurus_Relation_Type.Thesaurus_Relation_Type_Key, 1, 8) FROM Thesaurus_Relation_Type INNER JOIN INSERTED ON Thesaurus_Relation_Type.Thesaurus_Relation_Type_Key = INSERTED.Thesaurus_Relation_Type_Key WHERE Thesaurus_Relation_Type.CUSTODIAN IS NULL


GO
SET QUOTED_IDENTIFIER OFF 
GO
SET ANSI_NULLS ON 
GO


if not exists (select 1 from SysObjects where name='FK_Thesaurus_Relation_Type_Usage_Thesaurus_Relation_Type' AND parent_obj=Object_ID('Thesaurus_Relation_Type_Usage'))
ALTER TABLE [dbo].[Thesaurus_Relation_Type_Usage] ADD CONSTRAINT [FK_Thesaurus_Relation_Type_Usage_Thesaurus_Relation_Type] FOREIGN KEY 
	(
		[Thesaurus_Relation_Type_Key]
	) REFERENCES [Thesaurus_Relation_Type] (
		[Thesaurus_Relation_Type_Key]
	)
GO


SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[Thesaurus_Relation_Type_UsageCustodianInsert]') and OBJECTPROPERTY(id, N'IsTrigger') = 1)
drop trigger [dbo].[Thesaurus_Relation_Type_UsageCustodianInsert]
GO


CREATE TRIGGER Thesaurus_Relation_Type_UsageCustodianInsert ON dbo.Thesaurus_Relation_Type_Usage AFTER INSERT AS UPDATE Thesaurus_Relation_Type_Usage SET Thesaurus_Relation_Type_Usage.CUSTODIAN = SUBSTRING(Thesaurus_Relation_Type_Usage.Thesaurus_Relation_Type_Usage_Key, 1, 8) FROM Thesaurus_Relation_Type_Usage INNER JOIN INSERTED ON Thesaurus_Relation_Type_Usage.Thesaurus_Relation_Type_Usage_Key = INSERTED.Thesaurus_Relation_Type_Usage_Key WHERE Thesaurus_Relation_Type_Usage.CUSTODIAN IS NULL


GO
SET QUOTED_IDENTIFIER OFF 
GO
SET ANSI_NULLS ON 
GO


if not exists (select 1 from SysObjects where name='FK_User_Domain_Access_Domain' AND parent_obj=Object_ID('User_Domain_Access'))
ALTER TABLE [dbo].[User_Domain_Access] ADD CONSTRAINT [FK_User_Domain_Access_Domain] FOREIGN KEY 
	(
		[Domain_Key]
	) REFERENCES [Domain] (
		[Domain_Key]
	)
GO


if not exists (select 1 from SysObjects where name='FK_User_Domain_Access_USER' AND parent_obj=Object_ID('User_Domain_Access'))
ALTER TABLE [dbo].[User_Domain_Access] ADD CONSTRAINT [FK_User_Domain_Access_USER] FOREIGN KEY 
	(
		[Name_Key]
	) REFERENCES [USER] (
		[NAME_KEY]
	) ON DELETE CASCADE 
GO


if not exists (select 1 from SysObjects where name='DF_User_Domain_Access_Allow_Browse' AND Type='D')
ALTER TABLE [dbo].[User_Domain_Access] ADD CONSTRAINT [DF_User_Domain_Access_Allow_Browse] DEFAULT (0) FOR [Allow_Browse]
if not exists (select 1 from SysObjects where name='DF_User_Domain_Access_Allow_Quick_Entry' AND Type='D')
ALTER TABLE [dbo].[User_Domain_Access] ADD CONSTRAINT [DF_User_Domain_Access_Allow_Quick_Entry] DEFAULT (0) FOR [Allow_Quick_Entry]
if not exists (select 1 from SysObjects where name='DF_User_Domain_Access_Allow_Add' AND Type='D')
ALTER TABLE [dbo].[User_Domain_Access] ADD CONSTRAINT [DF_User_Domain_Access_Allow_Add] DEFAULT (0) FOR [Allow_Add]
if not exists (select 1 from SysObjects where name='DF_User_Domain_Access_Allow_Edito' AND Type='D')
ALTER TABLE [dbo].[User_Domain_Access] ADD CONSTRAINT [DF_User_Domain_Access_Allow_Edito] DEFAULT (0) FOR [Allow_Edit]
if not exists (select 1 from SysObjects where name='FK_Valuation_Concept' AND parent_obj=Object_ID('Valuation'))
ALTER TABLE [dbo].[Valuation] ADD CONSTRAINT [FK_Valuation_Concept] FOREIGN KEY 
	(
		[Currency_Concept_Key]
	) REFERENCES [Concept] (
		[Concept_Key]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Valuation_Type' AND parent_obj=Object_ID('Valuation'))
ALTER TABLE [dbo].[Valuation] ADD CONSTRAINT [FK_Valuation_Type] FOREIGN KEY 
	(
		[Type_Concept_Key]
	) REFERENCES [Concept] (
		[Concept_Key]
	)
GO


if not exists (select 1 from SysObjects where name='FK_Valuation_Valuer' AND parent_obj=Object_ID('Valuation'))
ALTER TABLE [dbo].[Valuation] ADD CONSTRAINT [FK_Valuation_Valuer] FOREIGN KEY 
	(
		[Valued_By_Name_Key]
	) REFERENCES [NAME] (
		[NAME_KEY]
	)
GO


if not exists (select 1 from dbo.sysindexes where name = N'IX_Valuation_Display_Caption' and id = object_id(N'[dbo].[Valuation]'))
 CREATE  INDEX [IX_Valuation_Display_Caption] ON [dbo].[Valuation]([Display_Caption]) ON [PRIMARY]
GO


SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[tr_Valuation_SearchCaption]') and OBJECTPROPERTY(id, N'IsTrigger') = 1)
drop trigger [dbo].[tr_Valuation_SearchCaption]
GO



/*===========================================================================*\
  Description:	Update search caption on the Valuation table.

  Type:		AFTER INSERT, UPDATE

  Created:	September 2003

  Last revision information:
    $Revision: 2 $
    $Date: 9/01/06 13:42 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE TRIGGER [dbo].[tr_Valuation_SearchCaption]
ON [dbo].[Valuation]
AFTER INSERT, UPDATE
AS 
	IF UPDATE(Vague_Date_Start) 
			OR UPDATE(Vague_Date_End)
			OR UPDATE(Vague_Date_Type)
			OR UPDATE(Type_Concept_Key)
		UPDATE Valuation 
		SET Search_Caption=
				dbo.ufn_GetDateFromVagueDate(I.Vague_Date_Start, I.Vague_Date_End, I.Vague_Date_Type)
				+ ISNULL(' - ' + CT.Plaintext, ''),
			Display_Caption=
				dbo.ufn_GetDateFromVagueDate(I.Vague_Date_Start, I.Vague_Date_End, I.Vague_Date_Type)
				+ ISNULL(' - ' + CT.Item_Name, '')
		FROM Valuation V
		INNER JOIN Inserted I on I.Valuation_Key=V.Valuation_Key
		LEFT JOIN VW_ConceptTerm CT ON CT.Concept_Key=I.Type_Concept_Key	



GO
SET QUOTED_IDENTIFIER OFF 
GO
SET ANSI_NULLS ON 
GO


SET QUOTED_IDENTIFIER ON 
GO
SET ANSI_NULLS ON 
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[ValuationCustodianInsert]') and OBJECTPROPERTY(id, N'IsTrigger') = 1)
drop trigger [dbo].[ValuationCustodianInsert]
GO


CREATE TRIGGER ValuationCustodianInsert ON dbo.Valuation AFTER INSERT AS UPDATE Valuation SET Valuation.CUSTODIAN = SUBSTRING(Valuation.Valuation_Key, 1, 8) FROM Valuation INNER JOIN INSERTED ON Valuation.Valuation_Key = INSERTED.Valuation_Key WHERE Valuation.CUSTODIAN IS NULL


GO
SET QUOTED_IDENTIFIER OFF 
GO
SET ANSI_NULLS ON 
GO


