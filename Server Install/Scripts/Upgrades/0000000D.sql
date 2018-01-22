/*===========================================================================*\
  Description:	This upgrade script is used to update the permissions 
		of the tables.

  Created:	May 2004

  Last revision information:
    $Revision: 1 $
    $Date: 11/05/04 12:23 $
    $Author: Anthonysimpson $

\*===========================================================================*/
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
GRANT  SELECT  ON [dbo].[Source_Join]  TO [R2k_ReadOnly]
GO
GRANT  SELECT  ON [dbo].[Source_Join]  TO [R2k_RecordCardsOnly]
GO
GRANT  SELECT  ON [dbo].[Source_Join]  TO [R2k_AddOnly]
GO
GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Source_Join]  TO [R2k_FullEdit]
GO
GRANT  SELECT ,  UPDATE ,  INSERT  ON [dbo].[Source_Join]  TO [R2k_Administrator]
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

