/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_FieldData_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_FieldData_Update]
GO

/*===========================================================================*\
  Description:	Updates tables when information in the field data frame is
		changed.

  Parameters:	@Key 
		@SurveyEventKey 
	        @SurveyKey 
	        @LocationKey 
		@LocationName
		@SampleKey 
	        @SpatialRefQualifier 
	        @SpatialRef 
	        @GatheringMethod 
	        @VagueDateStart 
	        @VagueDateEnd 
	        @VagueDateType
		@CollectionUnitKey 
		@OccurrenceKey 
		@TaxonOccurrenceKey 
		@InferredSurvey 
		@InferredLocation 
		@InferredSpatialRef 
		@InferredSampleType 
		@InferredDate 
		@InferredCollectors 
		@GatheringEvent
		@SessionID 
		@Timestamp

  Created:	October 2003

  Last revision information:
    $Revision: 8 $
    $Date: 3/02/09 9:29 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_FieldData_Update]
	@Key char(16),
	@SurveyEventKey char(16) = NULL,
	@SurveyKey char(16),
	@LocationKey char(16),
	@LocationName varchar(100) = NULL,
	@SampleKey char(16),
	@SpatialRefQualifier varchar(20),
	@SpatialRef varchar(40),
	@GatheringMethod char(16),
	@VagueDateStart int,
	@VagueDateEnd int,
	@VagueDateType varchar(2) = NULL,
	@CollectionUnitKey char(16),
	@OccurrenceKey char(16),
	@TaxonOccurrenceKey char(16),
	@InferredSurvey tinyint,
	@InferredLocation tinyint,
	@InferredSpatialRef tinyint,
	@InferredSampleType tinyint,
	@InferredDate tinyint,
	@InferredCollectors tinyint,
	@GatheringEvent bit,
	@SessionID char(16),
	@Timestamp timestamp
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION
		
		UPDATE 	[Sample]
		SET 	Location_Key = @LocationKey,
			Vague_Date_Start = @VagueDateStart, 
			Vague_Date_End = @VagueDateEnd, 
			Vague_Date_Type = IsNull(@VagueDateType, 'U'),
			Spatial_Ref_Qualifier = @SpatialRefQualifier,
			Spatial_Ref = @SpatialRef,
			Sample_Type_Key = @GatheringMethod,
			Location_Name = @LocationName
		WHERE	Sample_Key = @SampleKey

		IF @@Error <> 0 GOTO RollbackAndExit

		-- Update the Survey Event table so that is points to the
		-- correct Survey record.

		IF @SurveyEventKey = NULL 
			SET @SurveyEventKey = (SELECT	Survey_Event_Key
						FROM	[Sample]
						WHERE	Sample_Key = @SampleKey)
		UPDATE	Survey_Event
		SET	Survey_Key = @SurveyKey
		WHERE	Survey_Event_Key = @SurveyEventKey

		IF @@Error <> 0 GOTO RollbackAndExit

		UPDATE 	Specimen_Field_Data
		SET	Gathering_Event = @GatheringEvent,
			Collection_Unit_Key = @CollectionUnitKey,
			Occurrence_Key = @OccurrenceKey,
			Taxon_Occurrence_Key = @TaxonOccurrenceKey,
			Inferred_Survey = @InferredSurvey,
			Inferred_Location = @InferredLocation,
			Inferred_Spatial_Ref = @InferredSpatialRef,
			Inferred_Sample_Type = @InferredSampleType,
			Inferred_Date = @InferredDate,
			Inferred_Collectors = @InferredCollectors			
		WHERE	Specimen_Field_Data_Key = @Key
		AND		[Timestamp] = @Timestamp
		
		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Specimen_Field_Data WHERE Specimen_Field_Data_Key = @Key)
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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_FieldData_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_FieldData_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_FieldData_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_FieldData_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_FieldData_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_FieldData_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_FieldData_Update TO [Dev - JNCC SQL]
END
GO