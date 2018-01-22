/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Sample_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Sample_Update]
GO

/*===========================================================================*\
  Description:	Updates a record into the Sample table

  Parameters:	@Key
				@ConservationCheckKey 
				@ConservationJobKey
				@SetVagueDateStart
				@SetVagueDateEnd 
				@SetVagueDateType
				@Status
				@TypeConceptKey
				@Priority
				@Duration
				@DurationUnitConceptKey 
				@IdentifierNameKey
				@TaskAction
				@Comment
				@SessionID
				@Timestamp 

  Created:	August 2004

  Last revision information:
    $Revision: 1 $
    $Date: 23/09/04 15:11 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Sample_Update]
	@Key char(16),
	@VagueDateStart int,
	@VagueDateEnd int,
	@VagueDateType varchar(2) = NULL,
	@SpatialRef varchar(40),
	@SpatialRefSystem varchar(4),
	@Lat float,
	@Long float,
	@SpatialRefQualifier varchar(20),
	@SampleTypeKey char(16) = NULL,
	@LocationKey char(16),
	@SurveyEventKey char(16),
	@Comment text,
	@ChangedBy char(16),
	@LocationName varchar(100)
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION
		
		UPDATE 	[Sample]
		SET 	Vague_Date_Start = @VagueDateStart, 
				Vague_Date_End = @VagueDateEnd, 
				Vague_Date_Type = @VagueDateType,
				Spatial_Ref = @SpatialRef, 
				Spatial_Ref_System = @SpatialRefSystem, 
				Spatial_Ref_Qualifier = @SpatialRefQualifier,
				Lat = @Lat, 
				Long = @Long, 
				Location_Key = @LocationKey,
				Sample_Type_Key = @SampleTypeKey,
				Survey_Event_Key= @SurveyEventKey,
				Comment = @Comment,
				Location_Name = @LocationName,
				Changed_By = @ChangedBy,
				Changed_Date = GetDate()
		WHERE	Sample_Key = @Key

		IF @@Error <> 0 GOTO RollbackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Sample_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Sample_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        GRANT EXECUTE ON dbo.usp_Sample_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Sample_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Sample_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Sample_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_Sample_Update TO [Dev - JNCC SQL]
END
GO