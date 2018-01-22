/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Sample_Insert]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Sample_Insert]
GO
/*===========================================================================*\
  Description:	Inserts a record into the Sample table.
  Parameters:	

  Created:	July 2003

  Last revision information:
    $Revision: 5 $
    $Date: 14/04/04 10:39 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Sample_Insert]
	@Key char(16) OUTPUT,
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
	@EnteredBy char(16),
	@LocationName varchar(100)
	
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF
	
	/*-------------------------------------------------------------*\
	  Get a new key.
	\*-------------------------------------------------------------*/
	EXECUTE spNextKey 'Sample', @Key OUTPUT
	
  	/*-------------------------------------------------------------*\
	  If there is a location key, but no spatial ref, use the 
       	  survey event's
	\*-------------------------------------------------------------*/
	IF @SpatialRef IS NULL 
		SELECT 
			@SpatialRef=Spatial_Ref,
			@SpatialRefSystem=Spatial_Ref_System,
			@SpatialRefQualifier=Spatial_Ref_Qualifier,
			@Lat=Lat,
			@Long=Long
		FROM Survey_Event
		WHERE Survey_Event_Key=@SurveyEventKey

  	/*-------------------------------------------------------------*\
		Perform main insert
	\*-------------------------------------------------------------*/
	INSERT INTO Sample
		(Sample_Key,
		Vague_Date_Start,
		Vague_Date_End,
		Vague_Date_Type,
		Spatial_Ref,
		Spatial_Ref_System,
		Lat,
		Long,
		Spatial_Ref_Qualifier,
		Sample_Type_Key,
		Location_Key,
		Survey_Event_Key,
		Comment,
		Entered_By,
		Location_Name)
	VALUES
		(@Key,
		@VagueDateStart,
		@VagueDateEnd,
		IsNull(@VagueDateType, 'U'),
		@SpatialRef,
		@SpatialRefSystem,
		@Lat,
		@Long,
		@SpatialRefQualifier,
		IsNull(@SampleTypeKey, 'SYSTEM0000000000'),
		@LocationKey,
		@SurveyEventKey,
		@Comment,
		@EnteredBy,
		@LocationName)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Sample_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Sample_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Sample_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Sample_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Sample_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Sample_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Sample_Insert TO [Dev - JNCC SQL]
END

GO