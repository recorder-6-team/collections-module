/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_OccurrenceData_Update') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_OccurrenceData_Update]
GO

/*===========================================================================*\
  Description:	Updates a record in the Occurrence_Data table.
		The Occurrence_Data table hold descriptor and measurement
		information.

  Parameters:	@Key
		@CollectionUnitKey
		@AppliesTo
		@MethodConceptKey
		@Duration
		@Accuracy
		@ParameterConceptKey
		@UnitConceptKey
		@Value
		@UpperValue
		@IsDescriptor
		@SessionID
		@Timestamp

  Created:	November 2003

  Last revision information:
    $Revision: 6 $
    $Date: 3/02/09 9:58 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_OccurrenceData_Update]
	-- Required by both Measurements and Descriptors updates.
	@Key char(16),
	@IsDescriptor bit,
	@ParameterConceptKey char(16),
	@AppliesTo varchar(50),
	@Value varchar(50),	-- Used for Descriptors and as Lower_Value for Measurements
	@SessionID char(16),
	@Timestamp timestamp,

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

	/*----------------------------------------------------------------------------------*\
	  If we are updating measurement data, more information needs to changed than if
	  we are inserting descriptor data. Hence, there are two different update statements.
	\*----------------------------------------------------------------------------------*/
	BEGIN TRANSACTION

		IF @IsDescriptor = 1	
			-- Updating a descriptor.
			UPDATE	Occurrence_Data
			SET	Applies_To = @AppliesTo,
				Parameter_Concept_Key = @ParameterConceptKey,
				Lower_Value = IsNull(@Value, ' '),
				Is_Descriptor = @IsDescriptor,
				Changed_Session_ID = @SessionID
			WHERE	Occurrence_Data_Key = @Key
			AND		[Timestamp] = @Timestamp
	
		ELSE		
			-- Updating a measurement.
			UPDATE	Occurrence_Data
			SET	Applies_To = @AppliesTo,
				Parameter_Concept_Key = @ParameterConceptKey,
				Lower_Value = IsNull(@Value, ' '),
				Is_Descriptor = @IsDescriptor,
				Changed_Session_ID = @SessionID,
				Occurrence_Key = @OccurrenceKey,
				Method_Concept_Key = @MethodConceptKey,
				Duration = @Duration,
				Accuracy = @Accuracy,
				Unit_Concept_Key = @UnitConceptKey,
				Upper_Value = @UpperValue
			WHERE	Occurrence_Data_Key = @Key
			AND		[Timestamp] = @Timestamp

		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Occurrence_Data WHERE Occurrence_Data_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_OccurrenceData_Update') AND SysStat & 0xf = 4)
BEGIN
	PRINT 'Setting up security on procedure usp_OccurrenceData_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_OccurrenceData_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_OccurrenceData_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_OccurrenceData_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_OccurrenceData_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
		GRANT EXECUTE ON dbo.usp_OccurrenceData_Update TO [Dev - JNCC SQL]
END
GO