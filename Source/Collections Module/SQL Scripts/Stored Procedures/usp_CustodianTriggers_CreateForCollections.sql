/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_CustodianTriggers_CreateForCollections]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_CustodianTriggers_CreateForCollections]
GO

/*===========================================================================*\
  Description:	This procedure creates the triggers required to maintain the 
		custodian field for each table in the Collections.
		Calls spCreateCustodianTrigger for each table needing a trigger.

  Parameters:	<none>

  Created:	August 2003

  Last revision information:
    $Revision: 4 $
    $Date: 12/11/03 13:47 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_CustodianTriggers_CreateForCollections]
AS
BEGIN
	SET NOCOUNT ON

	EXECUTE spCreateCustodianTrigger 'Collection_Unit', 'Collection_Unit_Key'
	EXECUTE spCreateCustodianTrigger 'Collection_Unit_Data', 'Collection_Unit_Data_Key'
	EXECUTE spCreateCustodianTrigger 'Collection_Unit_Funding', 'Collection_Unit_Funding_Key'
	EXECUTE spCreateCustodianTrigger 'Collection_Unit_History', 'Collection_Unit_History_Key'
	EXECUTE spCreateCustodianTrigger 'Collection_Unit_Material', 'Collection_Unit_Material_Key'
	EXECUTE spCreateCustodianTrigger 'Collection_Unit_Name', 'Collection_Unit_Name_Key'
	EXECUTE spCreateCustodianTrigger 'Collection_Unit_Number', 'Collection_Unit_Number_Key'
	EXECUTE spCreateCustodianTrigger 'Collection_Unit_Process', 'Collection_Unit_Process_Key'
	EXECUTE spCreateCustodianTrigger 'Collection_Unit_Relation', 'Collection_Unit_Relation_Key'
	EXECUTE spCreateCustodianTrigger 'Concept', 'Concept_Key'
	EXECUTE spCreateCustodianTrigger 'Concept_Designation', 'Concept_Designation_Key'
	EXECUTE spCreateCustodianTrigger 'Concept_Group', 'Concept_Group_Key'
	EXECUTE spCreateCustodianTrigger 'Concept_Group_Version', 'Concept_Group_Version_Key'
	EXECUTE spCreateCustodianTrigger 'Concept_History', 'Concept_History_Key'
	EXECUTE spCreateCustodianTrigger 'Concept_Rank', 'Concept_Rank_Key'
	EXECUTE spCreateCustodianTrigger 'Concept_Relation', 'Concept_Relation_Key'
	EXECUTE spCreateCustodianTrigger 'Conservation_Check', 'Conservation_Check_Key'
	EXECUTE spCreateCustodianTrigger 'Conservation_Job', 'Conservation_Job_Key'
	EXECUTE spCreateCustodianTrigger 'Conservation_Job_Funding', 'Conservation_Job_Funding_Key'
	EXECUTE spCreateCustodianTrigger 'Conservation_Job_Material', 'Conservation_Job_Material_Key'
	EXECUTE spCreateCustodianTrigger 'Conservation_Task', 'Conservation_Task_Key'
	EXECUTE spCreateCustodianTrigger 'Currency', 'Currency_Key'
	EXECUTE spCreateCustodianTrigger 'Details_Report', 'Details_Report_Key'
	EXECUTE spCreateCustodianTrigger 'Determination', 'Determination_Key'
	EXECUTE spCreateCustodianTrigger 'Domain', 'Domain_Key'
	EXECUTE spCreateCustodianTrigger 'Domain_Hyperlink', 'Domain_Hyperlink_Key'
	EXECUTE spCreateCustodianTrigger 'Enquiry', 'Enquiry_Key'
	EXECUTE spCreateCustodianTrigger 'List_Report', 'List_Report_Key'
	EXECUTE spCreateCustodianTrigger 'Local_Domain', 'Local_Domain_Key'
	EXECUTE spCreateCustodianTrigger 'Meaning_Relation', 'Meaning_Relation_Key'
	EXECUTE spCreateCustodianTrigger 'Metadata', 'Metadata_Key'
	EXECUTE spCreateCustodianTrigger 'Metadata_Type', 'Metadata_Type_Key'
	EXECUTE spCreateCustodianTrigger 'Movement', 'Movement_Key'
	EXECUTE spCreateCustodianTrigger 'Movement_Communication', 'Movement_Communication_Key'
	EXECUTE spCreateCustodianTrigger 'Movement_Direction', 'Movement_Direction_Key'
	EXECUTE spCreateCustodianTrigger 'Movement_Funding', 'Movement_Funding_Key'
	EXECUTE spCreateCustodianTrigger 'Movement_Of_Material', 'Movement_Of_Material_Key'
	EXECUTE spCreateCustodianTrigger 'Movement_Of_Ownership', 'Movement_Of_Ownership_Key'
	EXECUTE spCreateCustodianTrigger 'Occurrence', 'Occurrence_Key'
	EXECUTE spCreateCustodianTrigger 'Occurrence_Data', 'Occurrence_Data_Key'
	EXECUTE spCreateCustodianTrigger 'Occurrence_Relation', 'Occurrence_Relation_Key'
	EXECUTE spCreateCustodianTrigger 'Organisation_Department', 'Organisation_Department_Key'
	EXECUTE spCreateCustodianTrigger 'QE_Field', 'QE_Field_Key'
	EXECUTE spCreateCustodianTrigger 'Report_Block', 'Report_Block_Key'
	EXECUTE spCreateCustodianTrigger 'Report_Block_In_Report', 'Report_Block_In_Report_Key'
	EXECUTE spCreateCustodianTrigger 'Report_Block_In_Section', 'Report_Block_In_Section_Key'
	EXECUTE spCreateCustodianTrigger 'Report_Block_Order', 'Report_Block_Order_Key'
	EXECUTE spCreateCustodianTrigger 'Report_Section', 'Report_Section_Key'
	EXECUTE spCreateCustodianTrigger 'Semantic_Relation', 'Semantic_Relation_Key'
	EXECUTE spCreateCustodianTrigger 'Specimen_Field_Data', 'Specimen_Field_Data_Key'
	EXECUTE spCreateCustodianTrigger 'Specimen_Label', 'Specimen_Label_Key'
	EXECUTE spCreateCustodianTrigger 'Subject_Area', 'Subject_Area_Key'
	EXECUTE spCreateCustodianTrigger 'Term', 'Term_Key'
	EXECUTE spCreateCustodianTrigger 'Term_Version', 'Term_Version_Key'
	EXECUTE spCreateCustodianTrigger 'Thesaurus_Fact', 'Thesaurus_Fact_Key'
	EXECUTE spCreateCustodianTrigger 'Thesaurus_Relation_Type', 'Thesaurus_Relation_Type_Key'
	EXECUTE spCreateCustodianTrigger 'Thesaurus_Relation_Type_Usage', 'Thesaurus_Relation_Type_Usage_Key'
	EXECUTE spCreateCustodianTrigger 'Valuation', 'Valuation_Key'
END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_CustodianTriggers_CreateForCollections') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_CustodianTriggers_CreateForCollections'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_CustodianTriggers_CreateForCollections TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_CustodianTriggers_CreateForCollections TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_CustodianTriggers_CreateForCollections TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_CustodianTriggers_CreateForCollections TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_CustodianTriggers_CreateForCollections TO [Dev - JNCC SQL]
END

GO