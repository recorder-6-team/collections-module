/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_SpecimenFieldData_Insert]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_SpecimenFieldData_Insert]
GO

/*===========================================================================*\
  Description:	Inserts a record in Specimen_Field_Data

  Parameters:	@CollectionUnitKey
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
		@Key			OUTPUT

  Created:	November 2003

  Last revision information:
    $Revision: 3 $
    $Date: 6/09/07 15:26 $
    $Author: Davidkelly $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SpecimenFieldData_Insert]
	@CollectionUnitKey char(16),
	@OccurrenceKey char(16),
	@TaxonOccurrenceKey char(16),
	@InferredSurvey tinyint,
	@InferredLocation tinyint,
	@InferredSpatialRef tinyint,
	@InferredSampleType tinyint,
	@InferredDate tinyint,
	@InferredCollectors tinyint,
	@InferredDeterminers tinyint,
	@GatheringEvent bit,
	@SessionID char(16),
	@Key char(16) OUTPUT
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	EXECUTE	spNextKey 'Specimen_Field_Data', @Key OUTPUT

	BEGIN TRANSACTION

		INSERT INTO Specimen_Field_Data (
			Specimen_Field_Data_Key, Collection_Unit_Key, Occurrence_Key, Taxon_Occurrence_Key,
			Inferred_Survey, Inferred_Location, Inferred_Spatial_Ref, Inferred_Sample_Type,
			Inferred_Date, Inferred_Collectors, Inferred_Determiners, Gathering_Event, Entered_Session_ID
		) VALUES (
			@Key, @CollectionUnitKey, @OccurrenceKey, @TaxonOccurrenceKey,
			@InferredSurvey, @InferredLocation, @InferredSpatialRef,
			@InferredSampleType, @InferredDate, @InferredCollectors,
			@InferredDeterminers, @GatheringEvent, @SessionID
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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SpecimenFieldData_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SpecimenFieldData_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SpecimenFieldData_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SpecimenFieldData_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SpecimenFieldData_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SpecimenFieldData_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SpecimenFieldData_Insert TO [Dev - JNCC SQL]
END
GO