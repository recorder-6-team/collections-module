/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_SurveyEvent_Insert]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_SurveyEvent_Insert]
GO

/*===========================================================================*\
  Description:	
  Parameters:	

  Created:	July 2003

  Last revision information:
    $Revision: 6 $
    $Date: 14/04/04 10:39 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SurveyEvent_Insert]
	@Key char(16) OUTPUT,
	@VagueDateStart int,
	@VagueDateEnd int,
	@VagueDateType varchar(2) = NULL,
	@SpatialRef varchar(40),
	@SpatialRefSystem varchar(4),
	@Lat float,
	@Long float,
	@SpatialRefQualifier varchar(20),
	@LocationKey char(16),
	@SurveyKey char(16),
	@Comment text,
	@EnteredBy char(16),
	@LocationName varchar(100)
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF
	
	EXECUTE spNextKey 'Survey_Event', @Key OUTPUT

	IF @SpatialRef IS NULL 
		SELECT 
			@SpatialRef=Spatial_Ref,
			@SpatialRefSystem=Spatial_Ref_System,
			@SpatialRefQualifier=Spatial_Ref_Qualifier,
			@Lat=Lat,
			@Long=Long
		FROM Location
		WHERE Location_Key=@LocationKey

	BEGIN TRANSACTION	
		INSERT INTO Survey_Event (
			Survey_Event_Key,
			Vague_Date_Start,
			Vague_Date_End,
			Vague_Date_Type,
			Spatial_Ref,
			Spatial_Ref_System,
			Lat,
			Long,
			Spatial_Ref_Qualifier,
			Location_Key,
			Survey_Key,
			Comment,
			Entered_By,
			Location_Name
		) VALUES (
			@Key,
			@VagueDateStart,
			@VagueDateEnd,
			IsNull(@VagueDateType, 'U'),
			@SpatialRef,
			@SpatialRefSystem,
			@Lat,
			@Long,
			@SpatialRefQualifier,
			@LocationKey,
			@SurveyKey,
			@Comment,
			@EnteredBy,
			@LocationName
		)

		IF @@Error <> 0 GOTO RollBackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit:
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SurveyEvent_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SurveyEvent_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SurveyEvent_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SurveyEvent_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SurveyEvent_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SurveyEvent_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SurveyEvent_Insert TO [Dev - JNCC SQL]
END
GO