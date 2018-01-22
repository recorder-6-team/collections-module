/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptReplace_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptReplace_Update]
GO

/*===========================================================================*\
  Description:	Takes in two Concept keys - the key to be replaced and the
				key that will replace it. The tables to be updated have been
				chosen by using the Diagram tool in Enterprise manager, adding
				the Concept table, then choosing 'Add Related Tables'. All
				tables that have a foreign key to the Concept table will need
				to be updated.

  Parameters:	@Key					Concept Key to be replaced
				@NewConceptKey			Concept Key that will replace the old Concept Key
				@DeleteConceptRelations	If this flag is set, rather than update the
										Concept_Relation records, they are deleted
				@RecordsAffected		Output - this is necessary or the app will think
										the update has failed if the last update in the
										proc doesn't update any records.

  Created:		September 2004

  Last revision information:
    $Revision: 4 $
    $Date: 24/09/04 13:37 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptReplace_Update]
	@Key char(16),
	@NewConceptKey char(16),
	@DeleteConceptRelations bit = 0,
	@RecordsAffected int = 1 output

AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION
		
		UPDATE 	Determination
		SET		Concept_Key = @NewConceptKey
		WHERE	Concept_Key = @Key 

		IF @@Error <> 0 GOTO RollbackAndExit

		UPDATE 	Determination
		SET		Nomenclatural_Status_Concept_Key = @NewConceptKey
		WHERE	Nomenclatural_Status_Concept_Key = @Key 

		IF @@Error <> 0 GOTO RollbackAndExit

		UPDATE	Occurrence
		SET		Record_Type_Concept_Key = @NewConceptKey
		WHERE	Record_Type_Concept_Key = @Key 

		IF @@Error <> 0 GOTO RollbackAndExit

		UPDATE	Occurrence_Data
		SET		Method_Concept_Key = @NewConceptKey
		WHERE	Method_Concept_Key = @Key 

		IF @@Error <> 0 GOTO RollbackAndExit

		UPDATE	Occurrence_Data
		SET		Parameter_Concept_Key = @NewConceptKey
		WHERE	Parameter_Concept_Key = @Key 

		IF @@Error <> 0 GOTO RollbackAndExit

		UPDATE	Occurrence_Data
		SET		Unit_Concept_Key = @NewConceptKey
		WHERE	Unit_Concept_Key = @Key 

		IF @@Error <> 0 GOTO RollbackAndExit

		UPDATE	Collection_Unit_Funding
		SET		Currency_Concept_Key = @NewConceptKey
		WHERE	Currency_Concept_Key = @Key 

		IF @@Error <> 0 GOTO RollbackAndExit

		UPDATE	Store
		SET		Store_Type_Concept_Key = @NewConceptKey
		WHERE	Store_Type_Concept_Key = @Key 

		IF @@Error <> 0 GOTO RollbackAndExit  

		UPDATE	Taxon_Dictionary_Name_Type_Mapping
		SET		Thesaurus_Name_Type_Key = @NewConceptKey
		WHERE	Thesaurus_Name_Type_Key = @Key 

		IF @@Error <> 0 GOTO RollbackAndExit   

		UPDATE	QE_Template_Field
		SET		Measurement_Method_Concept_Key = @NewConceptKey
		WHERE	Measurement_Method_Concept_Key = @Key 

		IF @@Error <> 0 GOTO RollbackAndExit

		UPDATE	QE_Template_Field
		SET		Measurement_Parameter_Concept_Key = @NewConceptKey
		WHERE	Measurement_Parameter_Concept_Key = @Key 

		IF @@Error <> 0 GOTO RollbackAndExit

		UPDATE	QE_Template_Field
		SET		Measurement_Unit_Concept_Key = @NewConceptKey
		WHERE	Measurement_Unit_Concept_Key = @Key 

		IF @@Error <> 0 GOTO RollbackAndExit

		UPDATE	Collection_Unit_Data
		SET 	Method_Concept_Key = @NewConceptKey
		WHERE	Method_Concept_Key = @Key 

		IF @@Error <> 0 GOTO RollbackAndExit

		UPDATE	Collection_Unit_Data
		SET 	Parameter_Concept_Key = @NewConceptKey
		WHERE	Parameter_Concept_Key = @Key 

		IF @@Error <> 0 GOTO RollbackAndExit

		UPDATE	Collection_Unit_Data
		SET 	Unit_Concept_Key = @NewConceptKey
		WHERE	Unit_Concept_Key = @Key 

		IF @@Error <> 0 GOTO RollbackAndExit

		IF NOT EXISTS (SELECT * FROM Taxon_Dictionary_Concept_Mapping WHERE	Concept_Key = @Key)
			UPDATE	Taxon_Dictionary_Concept_Mapping
			SET 	Concept_Key = @NewConceptKey
			WHERE	Concept_Key = @Key 

		IF @@Error <> 0 GOTO RollbackAndExit

		IF (@DeleteConceptRelations = 1)
			DELETE	Concept_Relation
			WHERE	(From_Concept_Key = @Key)
			OR		(To_Concept_Key = @Key)
		ELSE
			UPDATE	Concept_Relation
			SET 	From_Concept_Key = @NewConceptKey
			WHERE	From_Concept_Key = @Key 

		IF @@Error <> 0 GOTO RollbackAndExit

		UPDATE	Concept_Relation
		SET 	To_Concept_Key = @NewConceptKey
		WHERE	To_Concept_Key = @Key 

		IF @@Error <> 0 GOTO RollbackAndExit

		UPDATE	Concept_History
		SET		Concept_Key = @NewConceptKey
		WHERE	Concept_Key = @Key 

		IF @@Error <> 0 GOTO RollbackAndExit

		UPDATE	Conservation_Check
		SET		Type_Concept_Key = @NewConceptKey
		WHERE	Type_Concept_Key = @Key 

		IF @@Error <> 0 GOTO RollbackAndExit

		UPDATE	Conservation_Check
		SET		Condition_Concept_Key = @NewConceptKey
		WHERE	Condition_Concept_Key = @Key 

		IF @@Error <> 0 GOTO RollbackAndExit

		UPDATE	Conservation_Task
		SET		Type_Concept_Key = @NewConceptKey
		WHERE	Type_Concept_Key = @Key 

		IF @@Error <> 0 GOTO RollbackAndExit 

		UPDATE	Conservation_Task
		SET		Duration_Unit_Concept_Key = @NewConceptKey
		WHERE	Duration_Unit_Concept_Key = @Key 

		IF @@Error <> 0 GOTO RollbackAndExit 

		UPDATE	Conservation_Job
		SET		Duration_Unit_Concept_Key = @NewConceptKey
		WHERE	Duration_Unit_Concept_Key = @Key 

		IF @@Error <> 0 GOTO RollbackAndExit 	

		UPDATE	Conservation_Job
		SET		Currency_Concept_Key = @NewConceptKey
		WHERE	Currency_Concept_Key = @Key 

		IF @@Error <> 0 GOTO RollbackAndExit

		UPDATE	Conservation_Job_Material
		SET		Material_Concept_Key = @NewConceptKey
		WHERE	Material_Concept_Key = @Key 

		IF @@Error <> 0 GOTO RollbackAndExit  	

		UPDATE	Conservation_Job_Material
		SET		Unit_Concept_Key = @NewConceptKey
		WHERE	Unit_Concept_Key = @Key 

		IF @@Error <> 0 GOTO RollbackAndExit 

		UPDATE	Conservation_Job_Funding
		SET		Currency_Concept_Key = @NewConceptKey
		WHERE	Currency_Concept_Key = @Key 

		IF @@Error <> 0 GOTO RollbackAndExit 

		UPDATE	Movement_Communication
		SET		Communication_Type_Concept_Key = @NewConceptKey
		WHERE	Communication_Type_Concept_Key = @Key 

		IF @@Error <> 0 GOTO RollbackAndExit 		

		UPDATE	Collection_Unit_Process
		SET		Process_Concept_Key = @NewConceptKey
		WHERE	Process_Concept_Key = @Key 

		IF @@Error <> 0 GOTO RollbackAndExit 	

		UPDATE	Taxon_Dictionary_Designation_Type_Mapping
		SET		Concept_Designation_Type_Key = @NewConceptKey
		WHERE	Concept_Designation_Type_Key = @Key  

		IF @@Error <> 0 GOTO RollbackAndExit	

		UPDATE	Movement_Of_Material
		SET		Currency_Concept_Key = @NewConceptKey
		WHERE	Currency_Concept_Key = @Key 

		IF @@Error <> 0 GOTO RollbackAndExit 	

		UPDATE	Movement_Of_Material
		SET		Acquisition_Method_Concept_Key = @NewConceptKey
		WHERE	Acquisition_Method_Concept_Key = @Key 

		IF @@Error <> 0 GOTO RollbackAndExit 	

		UPDATE	Valuation
		SET		Type_Concept_Key = @NewConceptKey
		WHERE	Type_Concept_Key = @Key 

		IF @@Error <> 0 GOTO RollbackAndExit 

		UPDATE	Valuation
		SET		Currency_Concept_Key = @NewConceptKey
		WHERE	Currency_Concept_Key = @Key 

		IF @@Error <> 0 GOTO RollbackAndExit 

		UPDATE	Movement_Funding
		SET		Currency_Concept_Key = @NewConceptKey
		WHERE	Currency_Concept_Key = @Key 

		IF @@Error <> 0 GOTO RollbackAndExit 

		UPDATE	Enquiry
		SET		Enquiry_Type_Concept_Key = @NewConceptKey
		WHERE	Enquiry_Type_Concept_Key = @Key 

		IF @@Error <> 0 GOTO RollbackAndExit   

		UPDATE	Enquiry
		SET		Enquiry_Method_Concept_Key = @NewConceptKey
		WHERE	Enquiry_Method_Concept_Key = @Key 

		IF @@Error <> 0 GOTO RollbackAndExit  

		UPDATE	Enquiry_Concept
		SET		Concept_Key = @NewConceptKey
		WHERE	Concept_Key = @Key 

		IF @@Error <> 0 GOTO RollbackAndExit  

		UPDATE	Collection_Unit_Name
		SET		Relation_Type_Concept_Key = @NewConceptKey
		WHERE	Relation_Type_Concept_Key = @Key 

		IF @@Error <> 0 GOTO RollbackAndExit   

		UPDATE	Collection_Unit_Number
		SET		Type_Concept_Key = @NewConceptKey
		WHERE	Type_Concept_Key = @Key 

		IF @@Error <> 0 GOTO RollbackAndExit  		

		UPDATE	Collection_Unit_Material
		SET		Material_Concept_Key = @NewConceptKey
		WHERE	Material_Concept_Key = @Key 

		IF @@Error <> 0 GOTO RollbackAndExit  

		UPDATE	Collection_Unit_Material
		SET		Unit_Concept_Key = @NewConceptKey
		WHERE	Unit_Concept_Key = @Key 

		IF @@Error <> 0 GOTO RollbackAndExit 

		UPDATE	Concept_Designation
		SET		Concept_Key = @NewConceptKey
		WHERE	Concept_Key = @Key 

		IF @@Error <> 0 GOTO RollbackAndExit 

		UPDATE	Concept_Designation
		SET		Designation_Type_Concept_Key = @NewConceptKey
		WHERE	Designation_Type_Concept_Key = @Key 

		IF @@Error <> 0 GOTO RollbackAndExit 

		UPDATE	Specimen_Label
		SET		Confidence_Concept_Key = @NewConceptKey
		WHERE	Confidence_Concept_Key = @Key  

		IF @@Error <> 0 GOTO RollbackAndExit

		UPDATE	Collection
		SET		Risk_Concept_Key = @NewConceptKey
		WHERE	Risk_Concept_Key = @Key  

		IF @@Error <> 0 GOTO RollbackAndExit 

		UPDATE	Specimen_Unit 
		SET		Specimen_Type_Concept_Key = @NewConceptKey
		WHERE	Specimen_Type_Concept_Key = @Key 

		IF @@Error <> 0 GOTO RollbackAndExit  

		UPDATE 	Taxon_Determination
		SET		Nomenclatural_Status_Concept_Key = @NewConceptKey
		WHERE	Nomenclatural_Status_Concept_Key = @Key 

		IF @@Error <> 0 GOTO RollbackAndExit 

		UPDATE	Thesaurus_Fact
		SET		Concept_Key = @NewConceptKey
		WHERE	Concept_Key = @Key 
 
		IF @@Error <> 0 GOTO RollbackAndExit  

		SET @RecordsAffected = 1		

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	SET @RecordsAffected = 0		
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptReplace_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptReplace_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptReplace_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptReplace_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptReplace_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptReplace_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_ConceptReplace_Update TO [Dev - JNCC SQL]
END
GO