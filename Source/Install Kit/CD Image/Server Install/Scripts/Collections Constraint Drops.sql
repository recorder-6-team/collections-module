if exists (select 1 from SysObjects where name='FK_Collection_Collection' AND parent_obj=Object_ID('Collection'))
ALTER TABLE [dbo].[Collection] DROP CONSTRAINT [FK_Collection_Collection]

GO


if exists (select 1 from SysObjects where name='FK_Collection_Collection_Unit' AND parent_obj=Object_ID('Collection'))
ALTER TABLE [dbo].[Collection] DROP CONSTRAINT [FK_Collection_Collection_Unit]

GO


if exists (select 1 from SysObjects where name='FK_Collection_Name' AND parent_obj=Object_ID('Collection'))
ALTER TABLE [dbo].[Collection] DROP CONSTRAINT [FK_Collection_Name]

GO


if exists (select 1 from SysObjects where name='FK_Collection_Risk' AND parent_obj=Object_ID('Collection'))
ALTER TABLE [dbo].[Collection] DROP CONSTRAINT [FK_Collection_Risk]

GO


if exists (select 1 from SysObjects where name='FK_Collection_Unit_Current_Store' AND parent_obj=Object_ID('Collection_Unit'))
ALTER TABLE [dbo].[Collection_Unit] DROP CONSTRAINT [FK_Collection_Unit_Current_Store]

GO


if exists (select 1 from SysObjects where name='FK_Collection_Unit_Usual_Store' AND parent_obj=Object_ID('Collection_Unit'))
ALTER TABLE [dbo].[Collection_Unit] DROP CONSTRAINT [FK_Collection_Unit_Usual_Store]

GO


if exists (select 1 from SysObjects where name='FK_Collection_Unit_Check_Collection_Unit' AND parent_obj=Object_ID('Collection_Unit_Check'))
ALTER TABLE [dbo].[Collection_Unit_Check] DROP CONSTRAINT [FK_Collection_Unit_Check_Collection_Unit]

GO


if exists (select 1 from SysObjects where name='FK_Collection_Unit_Check_Conservation_Check' AND parent_obj=Object_ID('Collection_Unit_Check'))
ALTER TABLE [dbo].[Collection_Unit_Check] DROP CONSTRAINT [FK_Collection_Unit_Check_Conservation_Check]

GO


if exists (select 1 from SysObjects where name='FK_Collection_Unit_Data_Collection_Unit' AND parent_obj=Object_ID('Collection_Unit_Data'))
ALTER TABLE [dbo].[Collection_Unit_Data] DROP CONSTRAINT [FK_Collection_Unit_Data_Collection_Unit]

GO


if exists (select 1 from SysObjects where name='FK_Collection_Unit_Data_Method' AND parent_obj=Object_ID('Collection_Unit_Data'))
ALTER TABLE [dbo].[Collection_Unit_Data] DROP CONSTRAINT [FK_Collection_Unit_Data_Method]

GO


if exists (select 1 from SysObjects where name='FK_Collection_Unit_Data_Parameter' AND parent_obj=Object_ID('Collection_Unit_Data'))
ALTER TABLE [dbo].[Collection_Unit_Data] DROP CONSTRAINT [FK_Collection_Unit_Data_Parameter]

GO


if exists (select 1 from SysObjects where name='FK_Collection_Unit_Data_Unit' AND parent_obj=Object_ID('Collection_Unit_Data'))
ALTER TABLE [dbo].[Collection_Unit_Data] DROP CONSTRAINT [FK_Collection_Unit_Data_Unit]

GO


if exists (select 1 from SysObjects where name='FK_Collection_Unit_Enquiry_Collection_Unit' AND parent_obj=Object_ID('Collection_Unit_Enquiry'))
ALTER TABLE [dbo].[Collection_Unit_Enquiry] DROP CONSTRAINT [FK_Collection_Unit_Enquiry_Collection_Unit]

GO


if exists (select 1 from SysObjects where name='FK_Collection_Unit_Enquiry_Enquiry' AND parent_obj=Object_ID('Collection_Unit_Enquiry'))
ALTER TABLE [dbo].[Collection_Unit_Enquiry] DROP CONSTRAINT [FK_Collection_Unit_Enquiry_Enquiry]

GO


if exists (select 1 from SysObjects where name='FK_Collection_Unit_Funding_Collection_Unit' AND parent_obj=Object_ID('Collection_Unit_Funding'))
ALTER TABLE [dbo].[Collection_Unit_Funding] DROP CONSTRAINT [FK_Collection_Unit_Funding_Collection_Unit]

GO


if exists (select 1 from SysObjects where name='FK_Collection_Unit_Funding_Currency' AND parent_obj=Object_ID('Collection_Unit_Funding'))
ALTER TABLE [dbo].[Collection_Unit_Funding] DROP CONSTRAINT [FK_Collection_Unit_Funding_Currency]

GO


if exists (select 1 from SysObjects where name='FK_Collection_Unit_Funding_NAME' AND parent_obj=Object_ID('Collection_Unit_Funding'))
ALTER TABLE [dbo].[Collection_Unit_Funding] DROP CONSTRAINT [FK_Collection_Unit_Funding_NAME]

GO


if exists (select 1 from SysObjects where name='FK_Collection_Unit_History_Collection_Unit' AND parent_obj=Object_ID('Collection_Unit_History'))
ALTER TABLE [dbo].[Collection_Unit_History] DROP CONSTRAINT [FK_Collection_Unit_History_Collection_Unit]

GO


if exists (select 1 from SysObjects where name='FK_Collection_Unit_History_Individual' AND parent_obj=Object_ID('Collection_Unit_History'))
ALTER TABLE [dbo].[Collection_Unit_History] DROP CONSTRAINT [FK_Collection_Unit_History_Individual]

GO


if exists (select 1 from SysObjects where name='FK_Collection_Unit_Material_Collection_Unit' AND parent_obj=Object_ID('Collection_Unit_Material'))
ALTER TABLE [dbo].[Collection_Unit_Material] DROP CONSTRAINT [FK_Collection_Unit_Material_Collection_Unit]

GO


if exists (select 1 from SysObjects where name='FK_Specimen_Material_Material' AND parent_obj=Object_ID('Collection_Unit_Material'))
ALTER TABLE [dbo].[Collection_Unit_Material] DROP CONSTRAINT [FK_Specimen_Material_Material]

GO


if exists (select 1 from SysObjects where name='FK_Specimen_Material_Unit' AND parent_obj=Object_ID('Collection_Unit_Material'))
ALTER TABLE [dbo].[Collection_Unit_Material] DROP CONSTRAINT [FK_Specimen_Material_Unit]

GO


if exists (select 1 from SysObjects where name='FK_Collection_Unit_Name_Collection_Unit' AND parent_obj=Object_ID('Collection_Unit_Name'))
ALTER TABLE [dbo].[Collection_Unit_Name] DROP CONSTRAINT [FK_Collection_Unit_Name_Collection_Unit]

GO


if exists (select 1 from SysObjects where name='FK_Collection_Unit_Name_Concept' AND parent_obj=Object_ID('Collection_Unit_Name'))
ALTER TABLE [dbo].[Collection_Unit_Name] DROP CONSTRAINT [FK_Collection_Unit_Name_Concept]

GO


if exists (select 1 from SysObjects where name='FK_Collection_Unit_Name_Name' AND parent_obj=Object_ID('Collection_Unit_Name'))
ALTER TABLE [dbo].[Collection_Unit_Name] DROP CONSTRAINT [FK_Collection_Unit_Name_Name]

GO


if exists (select 1 from SysObjects where name='FK_Collection_Unit_Number_Collection_Unit' AND parent_obj=Object_ID('Collection_Unit_Number'))
ALTER TABLE [dbo].[Collection_Unit_Number] DROP CONSTRAINT [FK_Collection_Unit_Number_Collection_Unit]

GO


if exists (select 1 from SysObjects where name='FK_Collection_Unit_Number_Type' AND parent_obj=Object_ID('Collection_Unit_Number'))
ALTER TABLE [dbo].[Collection_Unit_Number] DROP CONSTRAINT [FK_Collection_Unit_Number_Type]

GO


if exists (select 1 from SysObjects where name='FK_Collection_Unit_Process_Collection_Unit' AND parent_obj=Object_ID('Collection_Unit_Process'))
ALTER TABLE [dbo].[Collection_Unit_Process] DROP CONSTRAINT [FK_Collection_Unit_Process_Collection_Unit]

GO


if exists (select 1 from SysObjects where name='FK_Collection_Unit_Process_Concept' AND parent_obj=Object_ID('Collection_Unit_Process'))
ALTER TABLE [dbo].[Collection_Unit_Process] DROP CONSTRAINT [FK_Collection_Unit_Process_Concept]

GO


if exists (select 1 from SysObjects where name='FK_Collection_Unit_Process_Individual' AND parent_obj=Object_ID('Collection_Unit_Process'))
ALTER TABLE [dbo].[Collection_Unit_Process] DROP CONSTRAINT [FK_Collection_Unit_Process_Individual]

GO


if exists (select 1 from SysObjects where name='FK_Collection_Unit_Relation_From_Collection' AND parent_obj=Object_ID('Collection_Unit_Relation'))
ALTER TABLE [dbo].[Collection_Unit_Relation] DROP CONSTRAINT [FK_Collection_Unit_Relation_From_Collection]

GO


if exists (select 1 from SysObjects where name='FK_Collection_Unit_Relation_NAME' AND parent_obj=Object_ID('Collection_Unit_Relation'))
ALTER TABLE [dbo].[Collection_Unit_Relation] DROP CONSTRAINT [FK_Collection_Unit_Relation_NAME]

GO


if exists (select 1 from SysObjects where name='FK_Collection_Unit_Relation_Thesaurus_Relation_Type' AND parent_obj=Object_ID('Collection_Unit_Relation'))
ALTER TABLE [dbo].[Collection_Unit_Relation] DROP CONSTRAINT [FK_Collection_Unit_Relation_Thesaurus_Relation_Type]

GO


if exists (select 1 from SysObjects where name='FK_Collection_Unit_Relation_To_Collection' AND parent_obj=Object_ID('Collection_Unit_Relation'))
ALTER TABLE [dbo].[Collection_Unit_Relation] DROP CONSTRAINT [FK_Collection_Unit_Relation_To_Collection]

GO


if exists (select 1 from SysObjects where name='IX_Collection_Unit_Task_Unique' AND parent_obj=Object_ID('Collection_Unit_Task'))
ALTER TABLE [dbo].[Collection_Unit_Task] DROP CONSTRAINT [IX_Collection_Unit_Task_Unique]

GO


if exists (select 1 from SysObjects where name='FK_Collection_Unit_Task_Collection_Unit' AND parent_obj=Object_ID('Collection_Unit_Task'))
ALTER TABLE [dbo].[Collection_Unit_Task] DROP CONSTRAINT [FK_Collection_Unit_Task_Collection_Unit]

GO


if exists (select 1 from SysObjects where name='FK_Collection_Unit_Task_Conservation_Task' AND parent_obj=Object_ID('Collection_Unit_Task'))
ALTER TABLE [dbo].[Collection_Unit_Task] DROP CONSTRAINT [FK_Collection_Unit_Task_Conservation_Task]

GO


if exists (select 1 from SysObjects where name='FK_Collection_Unit_Valuation_Collection_Unit' AND parent_obj=Object_ID('Collection_Unit_Valuation'))
ALTER TABLE [dbo].[Collection_Unit_Valuation] DROP CONSTRAINT [FK_Collection_Unit_Valuation_Collection_Unit]

GO


if exists (select 1 from SysObjects where name='FK_Collection_Unit_Valuation_Valuation' AND parent_obj=Object_ID('Collection_Unit_Valuation'))
ALTER TABLE [dbo].[Collection_Unit_Valuation] DROP CONSTRAINT [FK_Collection_Unit_Valuation_Valuation]

GO


if exists (select 1 from SysObjects where name='FK_Concept_Concept_Group' AND parent_obj=Object_ID('Concept'))
ALTER TABLE [dbo].[Concept] DROP CONSTRAINT [FK_Concept_Concept_Group]

GO


if exists (select 1 from SysObjects where name='FK_Concept_Concept_Name_Type' AND parent_obj=Object_ID('Concept'))
ALTER TABLE [dbo].[Concept] DROP CONSTRAINT [FK_Concept_Concept_Name_Type]

GO


if exists (select 1 from SysObjects where name='FK_Concept_Concept_Rank' AND parent_obj=Object_ID('Concept'))
ALTER TABLE [dbo].[Concept] DROP CONSTRAINT [FK_Concept_Concept_Rank]

GO


if exists (select 1 from SysObjects where name='FK_Concept_Meaning' AND parent_obj=Object_ID('Concept'))
ALTER TABLE [dbo].[Concept] DROP CONSTRAINT [FK_Concept_Meaning]

GO


if exists (select 1 from SysObjects where name='FK_Concept_Term' AND parent_obj=Object_ID('Concept'))
ALTER TABLE [dbo].[Concept] DROP CONSTRAINT [FK_Concept_Term]

GO


if exists (select 1 from SysObjects where name='FK_Concept_Term_Version' AND parent_obj=Object_ID('Concept'))
ALTER TABLE [dbo].[Concept] DROP CONSTRAINT [FK_Concept_Term_Version]

GO


if exists (select 1 from SysObjects where name='FK_Concept_Designation_Concept' AND parent_obj=Object_ID('Concept_Designation'))
ALTER TABLE [dbo].[Concept_Designation] DROP CONSTRAINT [FK_Concept_Designation_Concept]

GO


if exists (select 1 from SysObjects where name='FK_Concept_Designation_Type' AND parent_obj=Object_ID('Concept_Designation'))
ALTER TABLE [dbo].[Concept_Designation] DROP CONSTRAINT [FK_Concept_Designation_Type]

GO


if exists (select 1 from SysObjects where name='FK_Concept_Group_Local_Domain' AND parent_obj=Object_ID('Concept_Group'))
ALTER TABLE [dbo].[Concept_Group] DROP CONSTRAINT [FK_Concept_Group_Local_Domain]

GO


if exists (select 1 from SysObjects where name='FK_Concept_Group_Thesaurus_Relation_Type' AND parent_obj=Object_ID('Concept_Group'))
ALTER TABLE [dbo].[Concept_Group] DROP CONSTRAINT [FK_Concept_Group_Thesaurus_Relation_Type]

GO


if exists (select 1 from SysObjects where name='FK_Concept_Group_Version_Concept_Group' AND parent_obj=Object_ID('Concept_Group_Version'))
ALTER TABLE [dbo].[Concept_Group_Version] DROP CONSTRAINT [FK_Concept_Group_Version_Concept_Group]

GO


if exists (select 1 from SysObjects where name='FK_Concept_History_Concept' AND parent_obj=Object_ID('Concept_History'))
ALTER TABLE [dbo].[Concept_History] DROP CONSTRAINT [FK_Concept_History_Concept]

GO


if exists (select 1 from SysObjects where name='FK_Concept_History_Concept_Group_Version_From' AND parent_obj=Object_ID('Concept_History'))
ALTER TABLE [dbo].[Concept_History] DROP CONSTRAINT [FK_Concept_History_Concept_Group_Version_From]

GO


if exists (select 1 from SysObjects where name='FK_Concept_History_Concept_Group_Version_To' AND parent_obj=Object_ID('Concept_History'))
ALTER TABLE [dbo].[Concept_History] DROP CONSTRAINT [FK_Concept_History_Concept_Group_Version_To]

GO


if exists (select 1 from SysObjects where name='FK_Concept_Lineage_Concept' AND parent_obj=Object_ID('Concept_Lineage'))
ALTER TABLE [dbo].[Concept_Lineage] DROP CONSTRAINT [FK_Concept_Lineage_Concept]

GO


if exists (select 1 from SysObjects where name='FK_Concept_Rank_Domain' AND parent_obj=Object_ID('Concept_Rank'))
ALTER TABLE [dbo].[Concept_Rank] DROP CONSTRAINT [FK_Concept_Rank_Domain]

GO


if exists (select 1 from SysObjects where name='FK_Concept_Relation_Concept' AND parent_obj=Object_ID('Concept_Relation'))
ALTER TABLE [dbo].[Concept_Relation] DROP CONSTRAINT [FK_Concept_Relation_Concept]

GO


if exists (select 1 from SysObjects where name='FK_Concept_Relation_Concept1' AND parent_obj=Object_ID('Concept_Relation'))
ALTER TABLE [dbo].[Concept_Relation] DROP CONSTRAINT [FK_Concept_Relation_Concept1]

GO


if exists (select 1 from SysObjects where name='FK_Concept_Relation_Thesaurus_Relation_Type' AND parent_obj=Object_ID('Concept_Relation'))
ALTER TABLE [dbo].[Concept_Relation] DROP CONSTRAINT [FK_Concept_Relation_Thesaurus_Relation_Type]

GO


if exists (select 1 from SysObjects where name='FK_Conservation_Check_Concept' AND parent_obj=Object_ID('Conservation_Check'))
ALTER TABLE [dbo].[Conservation_Check] DROP CONSTRAINT [FK_Conservation_Check_Concept]

GO


if exists (select 1 from SysObjects where name='FK_Conservation_Check_Name' AND parent_obj=Object_ID('Conservation_Check'))
ALTER TABLE [dbo].[Conservation_Check] DROP CONSTRAINT [FK_Conservation_Check_Name]

GO


if exists (select 1 from SysObjects where name='FK_Conservation_Check_Type' AND parent_obj=Object_ID('Conservation_Check'))
ALTER TABLE [dbo].[Conservation_Check] DROP CONSTRAINT [FK_Conservation_Check_Type]

GO


if exists (select 1 from SysObjects where name='FK_Conservation_Job_Currency' AND parent_obj=Object_ID('Conservation_Job'))
ALTER TABLE [dbo].[Conservation_Job] DROP CONSTRAINT [FK_Conservation_Job_Currency]

GO


if exists (select 1 from SysObjects where name='FK_Conservation_Job_Funding_Conservation_Job' AND parent_obj=Object_ID('Conservation_Job_Funding'))
ALTER TABLE [dbo].[Conservation_Job_Funding] DROP CONSTRAINT [FK_Conservation_Job_Funding_Conservation_Job]

GO


if exists (select 1 from SysObjects where name='FK_Conservation_Job_Funding_Currency' AND parent_obj=Object_ID('Conservation_Job_Funding'))
ALTER TABLE [dbo].[Conservation_Job_Funding] DROP CONSTRAINT [FK_Conservation_Job_Funding_Currency]

GO


if exists (select 1 from SysObjects where name='FK_Conservation_Job_Funding_Name' AND parent_obj=Object_ID('Conservation_Job_Funding'))
ALTER TABLE [dbo].[Conservation_Job_Funding] DROP CONSTRAINT [FK_Conservation_Job_Funding_Name]

GO


if exists (select 1 from SysObjects where name='FK_Conservation_Job_Material_Conservation_Job' AND parent_obj=Object_ID('Conservation_Job_Material'))
ALTER TABLE [dbo].[Conservation_Job_Material] DROP CONSTRAINT [FK_Conservation_Job_Material_Conservation_Job]

GO


if exists (select 1 from SysObjects where name='FK_Conservation_Job_Material_Material' AND parent_obj=Object_ID('Conservation_Job_Material'))
ALTER TABLE [dbo].[Conservation_Job_Material] DROP CONSTRAINT [FK_Conservation_Job_Material_Material]

GO


if exists (select 1 from SysObjects where name='FK_Conservation_Job_Material_Unit' AND parent_obj=Object_ID('Conservation_Job_Material'))
ALTER TABLE [dbo].[Conservation_Job_Material] DROP CONSTRAINT [FK_Conservation_Job_Material_Unit]

GO


if exists (select 1 from SysObjects where name='FK_Conservation_Job_Staff_Conservation_Job' AND parent_obj=Object_ID('Conservation_Job_Staff'))
ALTER TABLE [dbo].[Conservation_Job_Staff] DROP CONSTRAINT [FK_Conservation_Job_Staff_Conservation_Job]

GO


if exists (select 1 from SysObjects where name='FK_Conservation_Job_Staff_Individual' AND parent_obj=Object_ID('Conservation_Job_Staff'))
ALTER TABLE [dbo].[Conservation_Job_Staff] DROP CONSTRAINT [FK_Conservation_Job_Staff_Individual]

GO


if exists (select 1 from SysObjects where name='FK_Conservation_Task_Conservation_Check' AND parent_obj=Object_ID('Conservation_Task'))
ALTER TABLE [dbo].[Conservation_Task] DROP CONSTRAINT [FK_Conservation_Task_Conservation_Check]

GO


if exists (select 1 from SysObjects where name='FK_Conservation_Task_Conservation_Job' AND parent_obj=Object_ID('Conservation_Task'))
ALTER TABLE [dbo].[Conservation_Task] DROP CONSTRAINT [FK_Conservation_Task_Conservation_Job]

GO


if exists (select 1 from SysObjects where name='FK_Conservation_Task_Individual' AND parent_obj=Object_ID('Conservation_Task'))
ALTER TABLE [dbo].[Conservation_Task] DROP CONSTRAINT [FK_Conservation_Task_Individual]

GO


if exists (select 1 from SysObjects where name='FK_Conservation_Task_Type' AND parent_obj=Object_ID('Conservation_Task'))
ALTER TABLE [dbo].[Conservation_Task] DROP CONSTRAINT [FK_Conservation_Task_Type]

GO


if exists (select 1 from SysObjects where name='FK_Conservation_Task_Unit' AND parent_obj=Object_ID('Conservation_Task'))
ALTER TABLE [dbo].[Conservation_Task] DROP CONSTRAINT [FK_Conservation_Task_Unit]

GO


if exists (select 1 from SysObjects where name='FK_Determination_Concept' AND parent_obj=Object_ID('Determination'))
ALTER TABLE [dbo].[Determination] DROP CONSTRAINT [FK_Determination_Concept]

GO


if exists (select 1 from SysObjects where name='FK_Determination_DETERMINATION_TYPE' AND parent_obj=Object_ID('Determination'))
ALTER TABLE [dbo].[Determination] DROP CONSTRAINT [FK_Determination_DETERMINATION_TYPE]

GO


if exists (select 1 from SysObjects where name='FK_Determination_DETERMINER_ROLE' AND parent_obj=Object_ID('Determination'))
ALTER TABLE [dbo].[Determination] DROP CONSTRAINT [FK_Determination_DETERMINER_ROLE]

GO


if exists (select 1 from SysObjects where name='FK_Determination_INDIVIDUAL' AND parent_obj=Object_ID('Determination'))
ALTER TABLE [dbo].[Determination] DROP CONSTRAINT [FK_Determination_INDIVIDUAL]

GO


if exists (select 1 from SysObjects where name='FK_Determination_Nomenclatural_Status' AND parent_obj=Object_ID('Determination'))
ALTER TABLE [dbo].[Determination] DROP CONSTRAINT [FK_Determination_Nomenclatural_Status]

GO


if exists (select 1 from SysObjects where name='FK_Determination_Occurrence' AND parent_obj=Object_ID('Determination'))
ALTER TABLE [dbo].[Determination] DROP CONSTRAINT [FK_Determination_Occurrence]

GO


if exists (select 1 from SysObjects where name='FK_Determination_Specimen_Unit' AND parent_obj=Object_ID('Determination'))
ALTER TABLE [dbo].[Determination] DROP CONSTRAINT [FK_Determination_Specimen_Unit]

GO


if exists (select 1 from SysObjects where name='FK_Domain_Subject_Area' AND parent_obj=Object_ID('Domain'))
ALTER TABLE [dbo].[Domain] DROP CONSTRAINT [FK_Domain_Subject_Area]

GO


if exists (select 1 from SysObjects where name='FK_Domain_Thesaurus_Relation_Type' AND parent_obj=Object_ID('Domain'))
ALTER TABLE [dbo].[Domain] DROP CONSTRAINT [FK_Domain_Thesaurus_Relation_Type]

GO


if exists (select 1 from SysObjects where name='FK_Domain_Hyperlink_Local_Domain' AND parent_obj=Object_ID('Domain_Hyperlink'))
ALTER TABLE [dbo].[Domain_Hyperlink] DROP CONSTRAINT [FK_Domain_Hyperlink_Local_Domain]

GO


if exists (select 1 from SysObjects where name='FK_Enquiry_Answered_By' AND parent_obj=Object_ID('Enquiry'))
ALTER TABLE [dbo].[Enquiry] DROP CONSTRAINT [FK_Enquiry_Answered_By]

GO


if exists (select 1 from SysObjects where name='FK_Enquiry_Enquirer' AND parent_obj=Object_ID('Enquiry'))
ALTER TABLE [dbo].[Enquiry] DROP CONSTRAINT [FK_Enquiry_Enquirer]

GO


if exists (select 1 from SysObjects where name='FK_Enquiry_Method' AND parent_obj=Object_ID('Enquiry'))
ALTER TABLE [dbo].[Enquiry] DROP CONSTRAINT [FK_Enquiry_Method]

GO


if exists (select 1 from SysObjects where name='FK_Enquiry_Type' AND parent_obj=Object_ID('Enquiry'))
ALTER TABLE [dbo].[Enquiry] DROP CONSTRAINT [FK_Enquiry_Type]

GO


if exists (select 1 from SysObjects where name='IX_Enquiry_Concept_Unique' AND parent_obj=Object_ID('Enquiry_Concept'))
ALTER TABLE [dbo].[Enquiry_Concept] DROP CONSTRAINT [IX_Enquiry_Concept_Unique]

GO


if exists (select 1 from SysObjects where name='FK_Enquiry_Concept_Concept' AND parent_obj=Object_ID('Enquiry_Concept'))
ALTER TABLE [dbo].[Enquiry_Concept] DROP CONSTRAINT [FK_Enquiry_Concept_Concept]

GO


if exists (select 1 from SysObjects where name='FK_Enquiry_Concept_Enquiry' AND parent_obj=Object_ID('Enquiry_Concept'))
ALTER TABLE [dbo].[Enquiry_Concept] DROP CONSTRAINT [FK_Enquiry_Concept_Enquiry]

GO


if exists (select 1 from SysObjects where name='FK_Import_Export_Job_Concept_Group' AND parent_obj=Object_ID('Import_Export_Job'))
ALTER TABLE [dbo].[Import_Export_Job] DROP CONSTRAINT [FK_Import_Export_Job_Concept_Group]

GO


if exists (select 1 from SysObjects where name='FK_List_Report_Report_Block' AND parent_obj=Object_ID('List_Report'))
ALTER TABLE [dbo].[List_Report] DROP CONSTRAINT [FK_List_Report_Report_Block]

GO


if exists (select 1 from SysObjects where name='FK_Local_Domain_Domain' AND parent_obj=Object_ID('Local_Domain'))
ALTER TABLE [dbo].[Local_Domain] DROP CONSTRAINT [FK_Local_Domain_Domain]

GO


if exists (select 1 from SysObjects where name='FK_Local_Domain_Language' AND parent_obj=Object_ID('Local_Domain'))
ALTER TABLE [dbo].[Local_Domain] DROP CONSTRAINT [FK_Local_Domain_Language]

GO


if exists (select 1 from SysObjects where name='FK_Metadata_Metadata_Type' AND parent_obj=Object_ID('Metadata'))
ALTER TABLE [dbo].[Metadata] DROP CONSTRAINT [FK_Metadata_Metadata_Type]

GO


if exists (select 1 from SysObjects where name='IX_Metadata_Type_Unique' AND parent_obj=Object_ID('Metadata_Type'))
ALTER TABLE [dbo].[Metadata_Type] DROP CONSTRAINT [IX_Metadata_Type_Unique]

GO


if exists (select 1 from SysObjects where name='FK_Movement_Contact' AND parent_obj=Object_ID('Movement'))
ALTER TABLE [dbo].[Movement] DROP CONSTRAINT [FK_Movement_Contact]

GO


if exists (select 1 from SysObjects where name='FK_Movement_Other_Party' AND parent_obj=Object_ID('Movement'))
ALTER TABLE [dbo].[Movement] DROP CONSTRAINT [FK_Movement_Other_Party]

GO


if exists (select 1 from SysObjects where name='FK_Movement_Staff_Responsible' AND parent_obj=Object_ID('Movement'))
ALTER TABLE [dbo].[Movement] DROP CONSTRAINT [FK_Movement_Staff_Responsible]

GO


if exists (select 1 from SysObjects where name='FK_Movement_Collection_Unit_Collection_Unit' AND parent_obj=Object_ID('Movement_Collection_Unit'))
ALTER TABLE [dbo].[Movement_Collection_Unit] DROP CONSTRAINT [FK_Movement_Collection_Unit_Collection_Unit]

GO


if exists (select 1 from SysObjects where name='FK_Movement_Collection_Unit_Movement_Direction' AND parent_obj=Object_ID('Movement_Collection_Unit'))
ALTER TABLE [dbo].[Movement_Collection_Unit] DROP CONSTRAINT [FK_Movement_Collection_Unit_Movement_Direction]

GO


if exists (select 1 from SysObjects where name='FK_Movement_Communication_Movement' AND parent_obj=Object_ID('Movement_Communication'))
ALTER TABLE [dbo].[Movement_Communication] DROP CONSTRAINT [FK_Movement_Communication_Movement]

GO


if exists (select 1 from SysObjects where name='FK_Movement_Communication_Receiver' AND parent_obj=Object_ID('Movement_Communication'))
ALTER TABLE [dbo].[Movement_Communication] DROP CONSTRAINT [FK_Movement_Communication_Receiver]

GO


if exists (select 1 from SysObjects where name='FK_Movement_Communication_Sender' AND parent_obj=Object_ID('Movement_Communication'))
ALTER TABLE [dbo].[Movement_Communication] DROP CONSTRAINT [FK_Movement_Communication_Sender]

GO


if exists (select 1 from SysObjects where name='FK_Movement_Communication_Type' AND parent_obj=Object_ID('Movement_Communication'))
ALTER TABLE [dbo].[Movement_Communication] DROP CONSTRAINT [FK_Movement_Communication_Type]

GO


if exists (select 1 from SysObjects where name='FK_Movement_Conservation_Check_Conservation_Check' AND parent_obj=Object_ID('Movement_Conservation_Check'))
ALTER TABLE [dbo].[Movement_Conservation_Check] DROP CONSTRAINT [FK_Movement_Conservation_Check_Conservation_Check]

GO


if exists (select 1 from SysObjects where name='FK_Movement_Conservation_Check_Movement' AND parent_obj=Object_ID('Movement_Conservation_Check'))
ALTER TABLE [dbo].[Movement_Conservation_Check] DROP CONSTRAINT [FK_Movement_Conservation_Check_Movement]

GO


if exists (select 1 from SysObjects where name='FK_Movement_Direction_Movement' AND parent_obj=Object_ID('Movement_Direction'))
ALTER TABLE [dbo].[Movement_Direction] DROP CONSTRAINT [FK_Movement_Direction_Movement]

GO


if exists (select 1 from SysObjects where name='FK_Movement_Direction_NAME' AND parent_obj=Object_ID('Movement_Direction'))
ALTER TABLE [dbo].[Movement_Direction] DROP CONSTRAINT [FK_Movement_Direction_NAME]

GO


if exists (select 1 from SysObjects where name='FK_Movement_Enquiry_Enquiry' AND parent_obj=Object_ID('Movement_Enquiry'))
ALTER TABLE [dbo].[Movement_Enquiry] DROP CONSTRAINT [FK_Movement_Enquiry_Enquiry]

GO


if exists (select 1 from SysObjects where name='FK_Movement_Enquiry_Movement' AND parent_obj=Object_ID('Movement_Enquiry'))
ALTER TABLE [dbo].[Movement_Enquiry] DROP CONSTRAINT [FK_Movement_Enquiry_Movement]

GO


if exists (select 1 from SysObjects where name='FK_Movement_Funding_Currency' AND parent_obj=Object_ID('Movement_Funding'))
ALTER TABLE [dbo].[Movement_Funding] DROP CONSTRAINT [FK_Movement_Funding_Currency]

GO


if exists (select 1 from SysObjects where name='FK_Movement_Funding_Movement' AND parent_obj=Object_ID('Movement_Funding'))
ALTER TABLE [dbo].[Movement_Funding] DROP CONSTRAINT [FK_Movement_Funding_Movement]

GO


if exists (select 1 from SysObjects where name='FK_Movement_Funding_NAME' AND parent_obj=Object_ID('Movement_Funding'))
ALTER TABLE [dbo].[Movement_Funding] DROP CONSTRAINT [FK_Movement_Funding_NAME]

GO


if exists (select 1 from SysObjects where name='FK_Movement_Of_Material_Acquisition_Method' AND parent_obj=Object_ID('Movement_Of_Material'))
ALTER TABLE [dbo].[Movement_Of_Material] DROP CONSTRAINT [FK_Movement_Of_Material_Acquisition_Method]

GO


if exists (select 1 from SysObjects where name='FK_Movement_Of_Material_Currency' AND parent_obj=Object_ID('Movement_Of_Material'))
ALTER TABLE [dbo].[Movement_Of_Material] DROP CONSTRAINT [FK_Movement_Of_Material_Currency]

GO


if exists (select 1 from SysObjects where name='FK_Movement_Of_Material_Movement_Direction' AND parent_obj=Object_ID('Movement_Of_Material'))
ALTER TABLE [dbo].[Movement_Of_Material] DROP CONSTRAINT [FK_Movement_Of_Material_Movement_Direction]

GO


if exists (select 1 from SysObjects where name='FK_Movement_Of_Material_Person' AND parent_obj=Object_ID('Movement_Of_Material'))
ALTER TABLE [dbo].[Movement_Of_Material] DROP CONSTRAINT [FK_Movement_Of_Material_Person]

GO


if exists (select 1 from SysObjects where name='FK_Movement_Of_Material_Exclusion_Collection_Unit' AND parent_obj=Object_ID('Movement_Of_Material_Exclusion'))
ALTER TABLE [dbo].[Movement_Of_Material_Exclusion] DROP CONSTRAINT [FK_Movement_Of_Material_Exclusion_Collection_Unit]

GO


if exists (select 1 from SysObjects where name='FK_Movement_Of_Material_Exclusion_Movement_Of_Material' AND parent_obj=Object_ID('Movement_Of_Material_Exclusion'))
ALTER TABLE [dbo].[Movement_Of_Material_Exclusion] DROP CONSTRAINT [FK_Movement_Of_Material_Exclusion_Movement_Of_Material]

GO


if exists (select 1 from SysObjects where name='FK_Movement_Of_Ownership_Contact' AND parent_obj=Object_ID('Movement_Of_Ownership'))
ALTER TABLE [dbo].[Movement_Of_Ownership] DROP CONSTRAINT [FK_Movement_Of_Ownership_Contact]

GO


if exists (select 1 from SysObjects where name='FK_Movement_Of_Ownership_Movement_Direction' AND parent_obj=Object_ID('Movement_Of_Ownership'))
ALTER TABLE [dbo].[Movement_Of_Ownership] DROP CONSTRAINT [FK_Movement_Of_Ownership_Movement_Direction]

GO


if exists (select 1 from SysObjects where name='FK_Movement_Of_Ownership_Exclusion_Collection_Unit' AND parent_obj=Object_ID('Movement_Of_Ownership_Exclusion'))
ALTER TABLE [dbo].[Movement_Of_Ownership_Exclusion] DROP CONSTRAINT [FK_Movement_Of_Ownership_Exclusion_Collection_Unit]

GO


if exists (select 1 from SysObjects where name='FK_Movement_Of_Ownership_Exclusion_Movement_Of_Ownership' AND parent_obj=Object_ID('Movement_Of_Ownership_Exclusion'))
ALTER TABLE [dbo].[Movement_Of_Ownership_Exclusion] DROP CONSTRAINT [FK_Movement_Of_Ownership_Exclusion_Movement_Of_Ownership]

GO


if exists (select 1 from SysObjects where name='FK_Movement_Valuation_Movement' AND parent_obj=Object_ID('Movement_Valuation'))
ALTER TABLE [dbo].[Movement_Valuation] DROP CONSTRAINT [FK_Movement_Valuation_Movement]

GO


if exists (select 1 from SysObjects where name='FK_Movement_Valuation_Valuation' AND parent_obj=Object_ID('Movement_Valuation'))
ALTER TABLE [dbo].[Movement_Valuation] DROP CONSTRAINT [FK_Movement_Valuation_Valuation]

GO


if exists (select 1 from SysObjects where name='FK_Occurrence_Record_Type' AND parent_obj=Object_ID('Occurrence'))
ALTER TABLE [dbo].[Occurrence] DROP CONSTRAINT [FK_Occurrence_Record_Type]

GO


if exists (select 1 from SysObjects where name='FK_Occurrence_Sample' AND parent_obj=Object_ID('Occurrence'))
ALTER TABLE [dbo].[Occurrence] DROP CONSTRAINT [FK_Occurrence_Sample]

GO


if exists (select 1 from SysObjects where name='FK_Occurrence_Data_Method' AND parent_obj=Object_ID('Occurrence_Data'))
ALTER TABLE [dbo].[Occurrence_Data] DROP CONSTRAINT [FK_Occurrence_Data_Method]

GO


if exists (select 1 from SysObjects where name='FK_Occurrence_Data_Occurrence' AND parent_obj=Object_ID('Occurrence_Data'))
ALTER TABLE [dbo].[Occurrence_Data] DROP CONSTRAINT [FK_Occurrence_Data_Occurrence]

GO


if exists (select 1 from SysObjects where name='FK_Occurrence_Data_Parameter' AND parent_obj=Object_ID('Occurrence_Data'))
ALTER TABLE [dbo].[Occurrence_Data] DROP CONSTRAINT [FK_Occurrence_Data_Parameter]

GO


if exists (select 1 from SysObjects where name='FK_Occurrence_Data_Unit' AND parent_obj=Object_ID('Occurrence_Data'))
ALTER TABLE [dbo].[Occurrence_Data] DROP CONSTRAINT [FK_Occurrence_Data_Unit]

GO


if exists (select 1 from SysObjects where name='FK_Occurrence_Relation_Occurrence_From' AND parent_obj=Object_ID('Occurrence_Relation'))
ALTER TABLE [dbo].[Occurrence_Relation] DROP CONSTRAINT [FK_Occurrence_Relation_Occurrence_From]

GO


if exists (select 1 from SysObjects where name='FK_Occurrence_Relation_Occurrence_To' AND parent_obj=Object_ID('Occurrence_Relation'))
ALTER TABLE [dbo].[Occurrence_Relation] DROP CONSTRAINT [FK_Occurrence_Relation_Occurrence_To]

GO


if exists (select 1 from SysObjects where name='FK_Occurrence_Relation_Thesaurus_Relation_Type' AND parent_obj=Object_ID('Occurrence_Relation'))
ALTER TABLE [dbo].[Occurrence_Relation] DROP CONSTRAINT [FK_Occurrence_Relation_Thesaurus_Relation_Type]

GO


if exists (select 1 from SysObjects where name='IX_QE_Data_Item_Unique' AND parent_obj=Object_ID('QE_Data_Item'))
ALTER TABLE [dbo].[QE_Data_Item] DROP CONSTRAINT [IX_QE_Data_Item_Unique]

GO


if exists (select 1 from SysObjects where name='FK_QE_Data_Item_QE_Data_Row' AND parent_obj=Object_ID('QE_Data_Item'))
ALTER TABLE [dbo].[QE_Data_Item] DROP CONSTRAINT [FK_QE_Data_Item_QE_Data_Row]

GO


if exists (select 1 from SysObjects where name='FK_QE_Data_Item_QE_Template_Field' AND parent_obj=Object_ID('QE_Data_Item'))
ALTER TABLE [dbo].[QE_Data_Item] DROP CONSTRAINT [FK_QE_Data_Item_QE_Template_Field]

GO


if exists (select 1 from SysObjects where name='FK_QE_Data_Row_QE_Session' AND parent_obj=Object_ID('QE_Data_Row'))
ALTER TABLE [dbo].[QE_Data_Row] DROP CONSTRAINT [FK_QE_Data_Row_QE_Session]

GO


if exists (select 1 from SysObjects where name='FK_QE_Session_QE_Template' AND parent_obj=Object_ID('QE_Session'))
ALTER TABLE [dbo].[QE_Session] DROP CONSTRAINT [FK_QE_Session_QE_Template]

GO


if exists (select 1 from SysObjects where name='FK_QE_Template_Subject_Area' AND parent_obj=Object_ID('QE_Template'))
ALTER TABLE [dbo].[QE_Template] DROP CONSTRAINT [FK_QE_Template_Subject_Area]

GO


if exists (select 1 from SysObjects where name='FK_QE_Template_Field_Method' AND parent_obj=Object_ID('QE_Template_Field'))
ALTER TABLE [dbo].[QE_Template_Field] DROP CONSTRAINT [FK_QE_Template_Field_Method]

GO


if exists (select 1 from SysObjects where name='FK_QE_Template_Field_Parameter' AND parent_obj=Object_ID('QE_Template_Field'))
ALTER TABLE [dbo].[QE_Template_Field] DROP CONSTRAINT [FK_QE_Template_Field_Parameter]

GO


if exists (select 1 from SysObjects where name='FK_QE_Template_Field_QE_Field' AND parent_obj=Object_ID('QE_Template_Field'))
ALTER TABLE [dbo].[QE_Template_Field] DROP CONSTRAINT [FK_QE_Template_Field_QE_Field]

GO


if exists (select 1 from SysObjects where name='FK_QE_Template_Field_QE_Template' AND parent_obj=Object_ID('QE_Template_Field'))
ALTER TABLE [dbo].[QE_Template_Field] DROP CONSTRAINT [FK_QE_Template_Field_QE_Template]

GO


if exists (select 1 from SysObjects where name='FK_QE_Template_Field_Unit' AND parent_obj=Object_ID('QE_Template_Field'))
ALTER TABLE [dbo].[QE_Template_Field] DROP CONSTRAINT [FK_QE_Template_Field_Unit]

GO


if exists (select 1 from SysObjects where name='FK_Report_Block_In_Section_Report_Block' AND parent_obj=Object_ID('Report_Block_In_Section'))
ALTER TABLE [dbo].[Report_Block_In_Section] DROP CONSTRAINT [FK_Report_Block_In_Section_Report_Block]

GO


if exists (select 1 from SysObjects where name='FK_Report_Block_In_Section_Report_Section' AND parent_obj=Object_ID('Report_Block_In_Section'))
ALTER TABLE [dbo].[Report_Block_In_Section] DROP CONSTRAINT [FK_Report_Block_In_Section_Report_Section]

GO


if exists (select 1 from SysObjects where name='FK_Report_Block_Order_Report_Block' AND parent_obj=Object_ID('Report_Block_Order'))
ALTER TABLE [dbo].[Report_Block_Order] DROP CONSTRAINT [FK_Report_Block_Order_Report_Block]

GO


if exists (select 1 from SysObjects where name='FK_Report_Section_Details_Report' AND parent_obj=Object_ID('Report_Section'))
ALTER TABLE [dbo].[Report_Section] DROP CONSTRAINT [FK_Report_Section_Details_Report]

GO


if exists (select 1 from SysObjects where name='FK_Source_Join_Source' AND parent_obj=Object_ID('Source_Join'))
ALTER TABLE [dbo].[Source_Join] DROP CONSTRAINT [FK_Source_Join_Source]

GO


if exists (select 1 from SysObjects where name='IX_Unique_Specimen_Occurrence' AND parent_obj=Object_ID('Specimen_Field_Data'))
ALTER TABLE [dbo].[Specimen_Field_Data] DROP CONSTRAINT [IX_Unique_Specimen_Occurrence]

GO


if exists (select 1 from SysObjects where name='FK_Specimen_Field_Data_Occurrence' AND parent_obj=Object_ID('Specimen_Field_Data'))
ALTER TABLE [dbo].[Specimen_Field_Data] DROP CONSTRAINT [FK_Specimen_Field_Data_Occurrence]

GO


if exists (select 1 from SysObjects where name='FK_Specimen_Field_Data_Specimen' AND parent_obj=Object_ID('Specimen_Field_Data'))
ALTER TABLE [dbo].[Specimen_Field_Data] DROP CONSTRAINT [FK_Specimen_Field_Data_Specimen]

GO


if exists (select 1 from SysObjects where name='FK_Specimen_Field_Data_Taxon_Occurrence' AND parent_obj=Object_ID('Specimen_Field_Data'))
ALTER TABLE [dbo].[Specimen_Field_Data] DROP CONSTRAINT [FK_Specimen_Field_Data_Taxon_Occurrence]

GO


if exists (select 1 from SysObjects where name='FK_Specimen_Label_Confidence' AND parent_obj=Object_ID('Specimen_Label'))
ALTER TABLE [dbo].[Specimen_Label] DROP CONSTRAINT [FK_Specimen_Label_Confidence]

GO


if exists (select 1 from SysObjects where name='FK_Specimen_Label_Individual' AND parent_obj=Object_ID('Specimen_Label'))
ALTER TABLE [dbo].[Specimen_Label] DROP CONSTRAINT [FK_Specimen_Label_Individual]

GO


if exists (select 1 from SysObjects where name='FK_Specimen_Label_Language' AND parent_obj=Object_ID('Specimen_Label'))
ALTER TABLE [dbo].[Specimen_Label] DROP CONSTRAINT [FK_Specimen_Label_Language]

GO


if exists (select 1 from SysObjects where name='FK_Specimen_Label_Specimen_Unit' AND parent_obj=Object_ID('Specimen_Label'))
ALTER TABLE [dbo].[Specimen_Label] DROP CONSTRAINT [FK_Specimen_Label_Specimen_Unit]

GO


if exists (select 1 from SysObjects where name='FK_Specimen_Collection' AND parent_obj=Object_ID('Specimen_Unit'))
ALTER TABLE [dbo].[Specimen_Unit] DROP CONSTRAINT [FK_Specimen_Collection]

GO


if exists (select 1 from SysObjects where name='FK_Specimen_Collection_Unit' AND parent_obj=Object_ID('Specimen_Unit'))
ALTER TABLE [dbo].[Specimen_Unit] DROP CONSTRAINT [FK_Specimen_Collection_Unit]

GO


if exists (select 1 from SysObjects where name='FK_Specimen_Concept' AND parent_obj=Object_ID('Specimen_Unit'))
ALTER TABLE [dbo].[Specimen_Unit] DROP CONSTRAINT [FK_Specimen_Concept]

GO


if exists (select 1 from SysObjects where name='FK_Specimen_Unit_Determination' AND parent_obj=Object_ID('Specimen_Unit'))
ALTER TABLE [dbo].[Specimen_Unit] DROP CONSTRAINT [FK_Specimen_Unit_Determination]

GO


if exists (select 1 from SysObjects where name='FK_Specimen_Unit_REFERENCE' AND parent_obj=Object_ID('Specimen_Unit'))
ALTER TABLE [dbo].[Specimen_Unit] DROP CONSTRAINT [FK_Specimen_Unit_REFERENCE]

GO


if exists (select 1 from SysObjects where name='FK_Specimen_Unit_TAXON_DETERMINATION' AND parent_obj=Object_ID('Specimen_Unit'))
ALTER TABLE [dbo].[Specimen_Unit] DROP CONSTRAINT [FK_Specimen_Unit_TAXON_DETERMINATION]

GO


if exists (select 1 from SysObjects where name='FK_Store_Collection_Unit' AND parent_obj=Object_ID('Store'))
ALTER TABLE [dbo].[Store] DROP CONSTRAINT [FK_Store_Collection_Unit]

GO


if exists (select 1 from SysObjects where name='FK_Store_Concept' AND parent_obj=Object_ID('Store'))
ALTER TABLE [dbo].[Store] DROP CONSTRAINT [FK_Store_Concept]

GO


if exists (select 1 from SysObjects where name='FK_Taxon_Dictionary_Concept_Designation_Mapping_Concept_Designation' AND parent_obj=Object_ID('Taxon_Dictionary_Concept_Designation_Mapping'))
ALTER TABLE [dbo].[Taxon_Dictionary_Concept_Designation_Mapping] DROP CONSTRAINT [FK_Taxon_Dictionary_Concept_Designation_Mapping_Concept_Designation]

GO


if exists (select 1 from SysObjects where name='FK_Taxon_Dictionary_Concept_Designation_Mapping_Source_Join' AND parent_obj=Object_ID('Taxon_Dictionary_Concept_Designation_Mapping'))
ALTER TABLE [dbo].[Taxon_Dictionary_Concept_Designation_Mapping] DROP CONSTRAINT [FK_Taxon_Dictionary_Concept_Designation_Mapping_Source_Join]

GO


if exists (select 1 from SysObjects where name='FK_Taxon_Dictionary_Concept_Designation_Mapping_TAXON_DESIGNATION' AND parent_obj=Object_ID('Taxon_Dictionary_Concept_Designation_Mapping'))
ALTER TABLE [dbo].[Taxon_Dictionary_Concept_Designation_Mapping] DROP CONSTRAINT [FK_Taxon_Dictionary_Concept_Designation_Mapping_TAXON_DESIGNATION]

GO


if exists (select 1 from SysObjects where name='FK_Taxon_Dictionary_Concept_Group_Mapping_Concept_Group' AND parent_obj=Object_ID('Taxon_Dictionary_Concept_Group_Mapping'))
ALTER TABLE [dbo].[Taxon_Dictionary_Concept_Group_Mapping] DROP CONSTRAINT [FK_Taxon_Dictionary_Concept_Group_Mapping_Concept_Group]

GO


if exists (select 1 from SysObjects where name='FK_Taxon_Dictionary_Concept_Group_Mapping_TAXON_LIST' AND parent_obj=Object_ID('Taxon_Dictionary_Concept_Group_Mapping'))
ALTER TABLE [dbo].[Taxon_Dictionary_Concept_Group_Mapping] DROP CONSTRAINT [FK_Taxon_Dictionary_Concept_Group_Mapping_TAXON_LIST]

GO


if exists (select 1 from SysObjects where name='FK_Taxon_Dictionary_Concept_Group_Version_Mapping_Concept_Group_Version' AND parent_obj=Object_ID('Taxon_Dictionary_Concept_Group_Version_Mapping'))
ALTER TABLE [dbo].[Taxon_Dictionary_Concept_Group_Version_Mapping] DROP CONSTRAINT [FK_Taxon_Dictionary_Concept_Group_Version_Mapping_Concept_Group_Version]

GO


if exists (select 1 from SysObjects where name='FK_Taxon_Dictionary_Concept_Group_Version_Mapping_Source_Join' AND parent_obj=Object_ID('Taxon_Dictionary_Concept_Group_Version_Mapping'))
ALTER TABLE [dbo].[Taxon_Dictionary_Concept_Group_Version_Mapping] DROP CONSTRAINT [FK_Taxon_Dictionary_Concept_Group_Version_Mapping_Source_Join]

GO


if exists (select 1 from SysObjects where name='FK_Taxon_Dictionary_Concept_Group_Version_Mapping_TAXON_LIST_VERSION' AND parent_obj=Object_ID('Taxon_Dictionary_Concept_Group_Version_Mapping'))
ALTER TABLE [dbo].[Taxon_Dictionary_Concept_Group_Version_Mapping] DROP CONSTRAINT [FK_Taxon_Dictionary_Concept_Group_Version_Mapping_TAXON_LIST_VERSION]

GO


if exists (select 1 from SysObjects where name='FK_Taxon_Dictionary_Concept_Mapping_Concept' AND parent_obj=Object_ID('Taxon_Dictionary_Concept_Mapping'))
ALTER TABLE [dbo].[Taxon_Dictionary_Concept_Mapping] DROP CONSTRAINT [FK_Taxon_Dictionary_Concept_Mapping_Concept]

GO


if exists (select 1 from SysObjects where name='FK_Taxon_Dictionary_Concept_Mapping_TAXON_LIST_ITEM' AND parent_obj=Object_ID('Taxon_Dictionary_Concept_Mapping'))
ALTER TABLE [dbo].[Taxon_Dictionary_Concept_Mapping] DROP CONSTRAINT [FK_Taxon_Dictionary_Concept_Mapping_TAXON_LIST_ITEM]

GO


if exists (select 1 from SysObjects where name='FK_Taxon_Dictionary_Concept_Rank_Mapping_Concept_Rank' AND parent_obj=Object_ID('Taxon_Dictionary_Concept_Rank_Mapping'))
ALTER TABLE [dbo].[Taxon_Dictionary_Concept_Rank_Mapping] DROP CONSTRAINT [FK_Taxon_Dictionary_Concept_Rank_Mapping_Concept_Rank]

GO


if exists (select 1 from SysObjects where name='FK_Taxon_Dictionary_Concept_Rank_Mapping_TAXON_RANK' AND parent_obj=Object_ID('Taxon_Dictionary_Concept_Rank_Mapping'))
ALTER TABLE [dbo].[Taxon_Dictionary_Concept_Rank_Mapping] DROP CONSTRAINT [FK_Taxon_Dictionary_Concept_Rank_Mapping_TAXON_RANK]

GO


if exists (select 1 from SysObjects where name='FK_Taxon_Dictionary_Designation_Type_Mapping_Concept' AND parent_obj=Object_ID('Taxon_Dictionary_Designation_Type_Mapping'))
ALTER TABLE [dbo].[Taxon_Dictionary_Designation_Type_Mapping] DROP CONSTRAINT [FK_Taxon_Dictionary_Designation_Type_Mapping_Concept]

GO


if exists (select 1 from SysObjects where name='FK_Taxon_Dictionary_Designation_Type_Mapping_TAXON_DESIGNATION_TYPE' AND parent_obj=Object_ID('Taxon_Dictionary_Designation_Type_Mapping'))
ALTER TABLE [dbo].[Taxon_Dictionary_Designation_Type_Mapping] DROP CONSTRAINT [FK_Taxon_Dictionary_Designation_Type_Mapping_TAXON_DESIGNATION_TYPE]

GO


if exists (select 1 from SysObjects where name='FK_Taxon_Dictionary_Meaning_Mapping_Meaning' AND parent_obj=Object_ID('Taxon_Dictionary_Meaning_Mapping'))
ALTER TABLE [dbo].[Taxon_Dictionary_Meaning_Mapping] DROP CONSTRAINT [FK_Taxon_Dictionary_Meaning_Mapping_Meaning]

GO


if exists (select 1 from SysObjects where name='FK_Taxon_Dictionary_Meaning_Mapping_TAXON_LIST_ITEM' AND parent_obj=Object_ID('Taxon_Dictionary_Meaning_Mapping'))
ALTER TABLE [dbo].[Taxon_Dictionary_Meaning_Mapping] DROP CONSTRAINT [FK_Taxon_Dictionary_Meaning_Mapping_TAXON_LIST_ITEM]

GO


if exists (select 1 from SysObjects where name='FK_Taxon_Dictionary_Name_Type_Mapping_Concept' AND parent_obj=Object_ID('Taxon_Dictionary_Name_Type_Mapping'))
ALTER TABLE [dbo].[Taxon_Dictionary_Name_Type_Mapping] DROP CONSTRAINT [FK_Taxon_Dictionary_Name_Type_Mapping_Concept]

GO


if exists (select 1 from SysObjects where name='FK_Taxon_Dictionary_Name_Type_Mapping_TAXON_NAME_TYPE' AND parent_obj=Object_ID('Taxon_Dictionary_Name_Type_Mapping'))
ALTER TABLE [dbo].[Taxon_Dictionary_Name_Type_Mapping] DROP CONSTRAINT [FK_Taxon_Dictionary_Name_Type_Mapping_TAXON_NAME_TYPE]

GO


if exists (select 1 from SysObjects where name='FK_Taxon_Dictionary_Term_Mapping_TAXON' AND parent_obj=Object_ID('Taxon_Dictionary_Term_Mapping'))
ALTER TABLE [dbo].[Taxon_Dictionary_Term_Mapping] DROP CONSTRAINT [FK_Taxon_Dictionary_Term_Mapping_TAXON]

GO


if exists (select 1 from SysObjects where name='FK_Taxon_Dictionary_Term_Mapping_Term' AND parent_obj=Object_ID('Taxon_Dictionary_Term_Mapping'))
ALTER TABLE [dbo].[Taxon_Dictionary_Term_Mapping] DROP CONSTRAINT [FK_Taxon_Dictionary_Term_Mapping_Term]

GO


if exists (select 1 from SysObjects where name='FK_Taxon_Dictionary_Term_Sources_Mapping_Source_Join' AND parent_obj=Object_ID('Taxon_Dictionary_Term_Sources_Mapping'))
ALTER TABLE [dbo].[Taxon_Dictionary_Term_Sources_Mapping] DROP CONSTRAINT [FK_Taxon_Dictionary_Term_Sources_Mapping_Source_Join]

GO


if exists (select 1 from SysObjects where name='FK_Taxon_Dictionary_Term_Sources_Mapping_TAXON_SOURCES' AND parent_obj=Object_ID('Taxon_Dictionary_Term_Sources_Mapping'))
ALTER TABLE [dbo].[Taxon_Dictionary_Term_Sources_Mapping] DROP CONSTRAINT [FK_Taxon_Dictionary_Term_Sources_Mapping_TAXON_SOURCES]

GO


if exists (select 1 from SysObjects where name='FK_Taxon_Dictionary_Term_Version_Mapping_Source_Join' AND parent_obj=Object_ID('Taxon_Dictionary_Term_Version_Mapping'))
ALTER TABLE [dbo].[Taxon_Dictionary_Term_Version_Mapping] DROP CONSTRAINT [FK_Taxon_Dictionary_Term_Version_Mapping_Source_Join]

GO


if exists (select 1 from SysObjects where name='FK_Taxon_Dictionary_Term_Version_Mapping_TAXON_VERSION' AND parent_obj=Object_ID('Taxon_Dictionary_Term_Version_Mapping'))
ALTER TABLE [dbo].[Taxon_Dictionary_Term_Version_Mapping] DROP CONSTRAINT [FK_Taxon_Dictionary_Term_Version_Mapping_TAXON_VERSION]

GO


if exists (select 1 from SysObjects where name='FK_Taxon_Dictionary_Term_Version_Mapping_Term_Version' AND parent_obj=Object_ID('Taxon_Dictionary_Term_Version_Mapping'))
ALTER TABLE [dbo].[Taxon_Dictionary_Term_Version_Mapping] DROP CONSTRAINT [FK_Taxon_Dictionary_Term_Version_Mapping_Term_Version]

GO


if exists (select 1 from SysObjects where name='FK_Taxon_Dictionary_Thesaurus_Fact_Mapping_Source_Join' AND parent_obj=Object_ID('Taxon_Dictionary_Thesaurus_Fact_Mapping'))
ALTER TABLE [dbo].[Taxon_Dictionary_Thesaurus_Fact_Mapping] DROP CONSTRAINT [FK_Taxon_Dictionary_Thesaurus_Fact_Mapping_Source_Join]

GO


if exists (select 1 from SysObjects where name='FK_Taxon_Dictionary_Thesaurus_Fact_Mapping_TAXON_FACT' AND parent_obj=Object_ID('Taxon_Dictionary_Thesaurus_Fact_Mapping'))
ALTER TABLE [dbo].[Taxon_Dictionary_Thesaurus_Fact_Mapping] DROP CONSTRAINT [FK_Taxon_Dictionary_Thesaurus_Fact_Mapping_TAXON_FACT]

GO


if exists (select 1 from SysObjects where name='FK_Taxon_Dictionary_Thesaurus_Fact_Mapping_Thesaurus_Fact' AND parent_obj=Object_ID('Taxon_Dictionary_Thesaurus_Fact_Mapping'))
ALTER TABLE [dbo].[Taxon_Dictionary_Thesaurus_Fact_Mapping] DROP CONSTRAINT [FK_Taxon_Dictionary_Thesaurus_Fact_Mapping_Thesaurus_Fact]

GO


if exists (select 1 from SysObjects where name='IX_Term_Unique' AND parent_obj=Object_ID('Term'))
ALTER TABLE [dbo].[Term] DROP CONSTRAINT [IX_Term_Unique]

GO


if exists (select 1 from SysObjects where name='FK_Term_Version_Term' AND parent_obj=Object_ID('Term_Version'))
ALTER TABLE [dbo].[Term_Version] DROP CONSTRAINT [FK_Term_Version_Term]

GO


if exists (select 1 from SysObjects where name='FK_Term_Version_Relation_Term_Version_From' AND parent_obj=Object_ID('Term_Version_Relation'))
ALTER TABLE [dbo].[Term_Version_Relation] DROP CONSTRAINT [FK_Term_Version_Relation_Term_Version_From]

GO


if exists (select 1 from SysObjects where name='FK_Term_Version_Relation_Term_Version_To' AND parent_obj=Object_ID('Term_Version_Relation'))
ALTER TABLE [dbo].[Term_Version_Relation] DROP CONSTRAINT [FK_Term_Version_Relation_Term_Version_To]

GO


if exists (select 1 from SysObjects where name='FK_Term_Version_Relation_Thesaurus_Relation_Type' AND parent_obj=Object_ID('Term_Version_Relation'))
ALTER TABLE [dbo].[Term_Version_Relation] DROP CONSTRAINT [FK_Term_Version_Relation_Thesaurus_Relation_Type]

GO


if exists (select 1 from SysObjects where name='FK_Thesaurus_Relation_Type_Semantic_Relation' AND parent_obj=Object_ID('Thesaurus_Relation_Type'))
ALTER TABLE [dbo].[Thesaurus_Relation_Type] DROP CONSTRAINT [FK_Thesaurus_Relation_Type_Semantic_Relation]

GO


if exists (select 1 from SysObjects where name='FK_Thesaurus_Relation_Type_Usage_Thesaurus_Relation_Type' AND parent_obj=Object_ID('Thesaurus_Relation_Type_Usage'))
ALTER TABLE [dbo].[Thesaurus_Relation_Type_Usage] DROP CONSTRAINT [FK_Thesaurus_Relation_Type_Usage_Thesaurus_Relation_Type]

GO


if exists (select 1 from SysObjects where name='FK_User_Domain_Access_Domain' AND parent_obj=Object_ID('User_Domain_Access'))
ALTER TABLE [dbo].[User_Domain_Access] DROP CONSTRAINT [FK_User_Domain_Access_Domain]

GO


if exists (select 1 from SysObjects where name='FK_User_Domain_Access_USER' AND parent_obj=Object_ID('User_Domain_Access'))
ALTER TABLE [dbo].[User_Domain_Access] DROP CONSTRAINT [FK_User_Domain_Access_USER]

GO


if exists (select 1 from SysObjects where name='FK_Valuation_Concept' AND parent_obj=Object_ID('Valuation'))
ALTER TABLE [dbo].[Valuation] DROP CONSTRAINT [FK_Valuation_Concept]

GO


if exists (select 1 from SysObjects where name='FK_Valuation_Type' AND parent_obj=Object_ID('Valuation'))
ALTER TABLE [dbo].[Valuation] DROP CONSTRAINT [FK_Valuation_Type]

GO


if exists (select 1 from SysObjects where name='FK_Valuation_Valuer' AND parent_obj=Object_ID('Valuation'))
ALTER TABLE [dbo].[Valuation] DROP CONSTRAINT [FK_Valuation_Valuer]

GO


