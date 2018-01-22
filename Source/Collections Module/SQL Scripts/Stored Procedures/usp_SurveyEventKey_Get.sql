/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SurveyEventKey_Get') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_SurveyEventKey_Get]
GO
/*===========================================================================*\
  Description: 	Returns a survey event key based on properties of the survey 
	       	event

  Created:	September 2003

  Last revision information:
    $Revision: 9 $
    $Date: 4/08/04 11:51 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SurveyEventKey_Get]
	@Key char(16) OUTPUT,
	@VagueDateStart int,
	@VagueDateEnd int,
	@VagueDateType varchar(2),
	@SpatialRef varchar(40),
	@SpatialRefSystem varchar(4),
	@Lat float,
	@Long float,
	@SpatialRefQualifier varchar(20),
	@LocationKey char(16),
	@SurveyKey char(16),
	@LocationName varchar(100)
	
AS
	/*----------------------------------------------------------------------------*\
	  Match exact list of Field Collectors if #TempFieldCollectors has been 
	  created by Collections Browser.
	\*----------------------------------------------------------------------------*/
	IF object_id('tempdb..#TempFieldCollectors') IS NOT NULL
	BEGIN
		/*-----------------------------------------------------------------*\
		  Create a table variable to store the Survey_Event match keys.
		\*-----------------------------------------------------------------*/
		DECLARE @tableSurveyEventMatches TABLE (
			Survey_Event_Key char(16) COLLATE SQL_Latin1_General_CP1_CI_AS
		)
	
		DECLARE @CurrentFieldCollector char(16)
	
		/*------------------------------------------------------------------*\
		  Create the cursor.
		\*------------------------------------------------------------------*/
		DECLARE curFieldCollectors CURSOR LOCAL FAST_FORWARD FOR
			SELECT 	Name_Key
			FROM	#TempFieldCollectors
	
		OPEN	curFieldCollectors

		/*-------------------------------------------------------*\
		  Give @CurrentFieldCollector its first value.
		\*-------------------------------------------------------*/	
		FETCH NEXT
		FROM	curFieldCollectors
		INTO	@CurrentFieldCollector

		/*-------------------------------------------------------*\
		  Start looping through the field collectors.
		\*-------------------------------------------------------*/		
		WHILE @@Fetch_Status = 0
		BEGIN
			/*------------------------------------------------------------------------*\
			  If the there are no records in @tableSurveyEventMatches, insert all of
			  the Survey_Event records for the first Field Collector in the table.
			\*------------------------------------------------------------------------*/
			IF NOT EXISTS (SELECT * FROM @tableSurveyEventMatches)
			BEGIN
				INSERT INTO @tableSurveyEventMatches (
					Survey_Event_Key
				) 
				SELECT 	Survey_Event_Key
				FROM	Survey_Event_Recorder
				WHERE	Name_Key = @CurrentFieldCollector
			END
			ELSE
			/*------------------------------------------------------------------------*\
			  As there are records in @tableSurveyEventMatches, we can now start
			  removing records that aren't matched.
			\*------------------------------------------------------------------------*/ 
			BEGIN
				-- Delete non matches
				DELETE		@tableSurveyEventMatches
				FROM		@tableSurveyEventMatches AS SEM
				LEFT JOIN	Survey_Event_Recorder AS SER ON SER.Survey_Event_Key = SEM.Survey_Event_Key
									AND SER.Name_Key = @CurrentFieldCollector
				WHERE		SER.Survey_Event_Key IS NULL
			END

			/*---------------------------------------------------------------------*\
			  Get next field collector and put value into @CurrentFieldCollector.
			\*---------------------------------------------------------------------*/		
			FETCH NEXT
			FROM	curFieldCollectors
			INTO	@CurrentFieldCollector
		END
	
		CLOSE curFieldCollectors
		DEALLOCATE curFieldCollectors

		/*---------------------------------------------------------------------*\
		  Now match on Survey, Location, Date and Sample Recorders.
		\*---------------------------------------------------------------------*/		
		SELECT 	@Key = SE.Survey_Event_Key 
		FROM 	Survey_Event AS SE
		JOIN 	@tableSurveyEventMatches AS SEM ON SEM.Survey_Event_Key = SE.Survey_Event_Key
		WHERE	Survey_Key = @SurveyKey
		AND	((Vague_Date_Start = @VagueDateStart AND Vague_Date_End = @VagueDateEnd AND Vague_Date_Type = @VagueDateType) 
			OR 
			(Vague_Date_Type = 'U' AND (@VagueDateType='U' OR @VagueDateType IS NULL)))
		AND	(Location_key = @LocationKey
			OR
			(@LocationKey IS NULL AND Spatial_Ref = @SpatialRef AND Spatial_Ref_System = @SpatialRefSystem)
			OR
			(@LocationKey IS NULL AND @SpatialRef IS NULL AND Location_Name = @LocationName))
	END
	ELSE
		/*---------------------------------------------------------------------*\
		  This matches on Survey, Location and Date. I left this here in case
		  it is required somewhere else and #TempFieldCollectors hasn't been
		  created by the application.
		\*---------------------------------------------------------------------*/	
		SELECT 	@Key = SE.Survey_Event_Key
		FROM 	Survey_Event AS SE
		WHERE	Survey_Key = @SurveyKey
		AND	((Vague_Date_Start = @VagueDateStart AND Vague_Date_End = @VagueDateEnd AND Vague_Date_Type = @VagueDateType) 
			OR 
			(Vague_Date_Type = 'U' AND (@VagueDateType='U' OR @VagueDateType IS NULL)))
		AND	(Location_key = @LocationKey
			OR
			(@LocationKey IS NULL AND Spatial_Ref = @SpatialRef AND Spatial_Ref_System = @SpatialRefSystem)
			OR
			(@LocationKey IS NULL AND @SpatialRef IS NULL AND Location_Name = @LocationName))
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SurveyEventKey_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SurveyEventKey_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SurveyEventKey_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SurveyEventKey_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SurveyEventKey_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_SurveyEventKey_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SurveyEventKey_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SurveyEventKey_Get TO [Dev - JNCC SQL]
END
GO