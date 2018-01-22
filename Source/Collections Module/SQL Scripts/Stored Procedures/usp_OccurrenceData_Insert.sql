/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_OccurrenceData_Insert') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_OccurrenceData_Insert]
GO

/*===========================================================================*\
  Description: Inserts a record into Occurrence_Data.

  Parameters:  Fields of Occurrence_Data

  Created:     September 2003

  Last revision information:
    $Revision: 5 $
    $Date: 4/08/04 16:25 $
    $Author: Ericsalmon $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_OccurrenceData_Insert]
	-- Required by both Measurements and Descriptors updates.
	@Key char(16) OUTPUT,
       	@AppliesTo varchar(50),
       	@ParameterConceptKey char(16),
	@Value varchar(50),	-- Used for Descriptors and as Lower_Value for Measurements
	@IsDescriptor bit,
	@SessionID char(16),
	-- Only required for the Measurements update.
	@UpperValue varchar(50) = NULL,
	@OccurrenceKey char(16) = NULL,
	@MethodConceptKey char(16) = NULL,
	@Duration varchar(50) = NULL,
	@Accuracy varchar(50) = NULL,
	@UnitConceptKey char(16) = NULL
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION

		EXECUTE spNextKey 'Occurrence_Data', @Key OUTPUT

		/*----------------------------------------------------------------------------------*\
		  If we are inserting measurement data, more information needs to inserted than if 
		  we are inserting descriptor data. Hence, there are two different insert statements.
		\*----------------------------------------------------------------------------------*/
		IF @IsDescriptor = 1
			INSERT INTO Occurrence_Data (
				Occurrence_Data_Key, Occurrence_Key, Applies_To, Parameter_Concept_Key, 
				Lower_Value, Is_Descriptor, Entered_Session_ID
			) VALUES (
				@Key, @OccurrenceKey, @AppliesTo, @ParameterConceptKey, IsNull(@Value, ' '), 
				1, @SessionID
			)
		ELSE
			INSERT INTO Occurrence_Data (
				Occurrence_Data_Key, Occurrence_Key, Applies_To, Method_Concept_Key, Duration,
				Accuracy, Parameter_Concept_Key, Unit_Concept_Key, Lower_Value, Upper_Value,
				Is_Descriptor, Entered_Session_ID
			) VALUES (
				@Key, @OccurrenceKey, @AppliesTo, @MethodConceptKey, @Duration,
				@Accuracy, @ParameterConceptKey, @UnitConceptKey, IsNull(@Value, ' '), @UpperValue,
				0, @SessionID
			)

		IF @@Error <> 0 GOTO RollbackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_OccurrenceData_Insert') AND SysStat & 0xf = 4)
BEGIN
	PRINT 'Setting up security on procedure usp_OccurrenceData_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_OccurrenceData_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_OccurrenceData_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_OccurrenceData_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_OccurrenceData_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
		GRANT EXECUTE ON dbo.usp_OccurrenceData_Insert TO [Dev - JNCC SQL]
END
GO