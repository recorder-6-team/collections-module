/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_FieldData_Insert]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_FieldData_Insert]
GO

/*===========================================================================*\
  Description:	Inserts a record into the Specimen_Field_Data table, and
		updates records in others. This stored proc. differs from 
		usp_SpecimenFieldData insert because this updates records in 
		the Sample and Survey_Event tables.

  Parameters:	@Key OUTPUT
		@SurveyEventKey 
	        @SurveyKey 
	        @LocationKey 
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

  Created:	January 2004

  Last revision information:
    $Revision: 5 $
    $Date: 30/09/05 14:03 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_FieldData_Insert]
	@Key char(16) OUTPUT,
	@SurveyEventKey char(16) = NULL,
	@SurveyKey char(16),
	@LocationKey char(16),
	@LocationName varchar(100) = NULL,
	@SampleKey char(16),
	@SpatialRefQualifier varchar(20),
	@SpatialRef varchar(40),
	@Lat float,
	@Long float,
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
	@SessionID char(16)
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF
	SET ANSI_NULLS ON

	BEGIN TRANSACTION
		
		UPDATE 	[Sample]
		SET 	Location_Key = @LocationKey,
			Vague_Date_Start = @VagueDateStart, 
			Vague_Date_End = @VagueDateEnd, 
			Vague_Date_Type = IsNull(@VagueDateType, 'U'),
			Spatial_Ref_Qualifier = @SpatialRefQualifier,
			Spatial_Ref = @SpatialRef,
			Sample_Type_Key = @GatheringMethod,
			Lat = @Lat,
			Long = @Long,
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

		EXECUTE	spNextKey 'Specimen_Field_Data', @Key OUTPUT

		INSERT INTO Specimen_Field_Data (
			Specimen_Field_Data_Key, 
			Collection_Unit_Key, 
			Occurrence_Key, 
			Taxon_Occurrence_Key,
			Inferred_Survey, 
			Inferred_Location, 
			Inferred_Spatial_Ref, 
			Inferred_Sample_Type,
			Inferred_Date, 
			Inferred_Collectors, 
			Gathering_Event, 
			Entered_Session_ID
		) VALUES (
			@Key, 
			@CollectionUnitKey, 
			@OccurrenceKey, 
			@TaxonOccurrenceKey,
			@InferredSurvey, 
			@InferredLocation, 
			@InferredSpatialRef,
			@InferredSampleType, 
			@InferredDate, 
			@InferredCollectors,
			@GatheringEvent, 
			@SessionID
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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_FieldData_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_FieldData_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_FieldData_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_FieldData_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_FieldData_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_FieldData_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_FieldData_Insert TO [Dev - JNCC SQL]
END
GO