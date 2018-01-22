/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Match_Exists_ForSample]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Match_Exists_ForSample]
GO

/*===========================================================================*\
  Description:	Checks for an existing sample inside the survey which matches 
		the spatial reference, sample type, date and the exact list 
		of sample recorders, and the sample or survey event points 
		to the entered location.  

  Parameters:	@MatchExists 
		@SpatialRef 
		@SpatialRefQualifier
		@SampleTypeKey 
		@VagueDateStart 
		@VagueDateEnd
		@VagueDateType 
		@LocationKey
		@SampleRecorderKeys

  Created:	October 2003

  Last revision information:
    $Revision: 2 $
    $Date: 12/11/03 14:48 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Match_Exists_ForSample]
	@MatchExists bit OUTPUT,
	@SpatialRef varchar(20),
	@SpatialRefQualifier varchar(20),
	@SampleTypeKey char(16),
	@VagueDateStart int,
	@VagueDateEnd int,
	@VagueDateType varchar(2),
	@LocationKey char(16),
	@SampleRecorderKeys varchar(4096),
	@SurveyKey char(16)
AS

-- Not finished. 

SET NOCOUNT ON

	CREATE TABLE #SampleRecorderKeyTable (
		[Key] char(16)
	)

	DECLARE @TotalSampleRecorderKeys int
	DECLARE @ICounter int
	DECLARE @TempKey char(16)

	SET @ICounter = 0
	SET @TotalSampleRecorderKeys = len(@SampleRecorderKeys) / 16

	WHILE @ICounter < @TotalSampleRecorderKeys
	BEGIN 
		SET @TempKey = RTRIM(LEFT(@SampleRecorderKeys, CHARINDEX(';', @SampleRecorderKeys)-1))
		SET @SampleRecorderKeys = LTRIM(RIGHT(@SampleRecorderKeys, len(@SampleRecorderKeys) - CHARINDEX(';', @SampleRecorderKeys)))
	
		INSERT INTO #SampleRecorderKeyTable
			([Key])
		VALUES (@TempKey)

		SET @ICounter = @ICounter + 1
	END

	SELECT 	Sample_Key
	FROM 	Sample_Recorder




/*	SELECT *
	FROM		[Sample] AS S
	INNER JOIN	Survey_Event AS SE ON SE.Survey_Event_Key = S.Survey_Event_Key
	INNER JOIN	Survey AS SY on SY.Survey_Key = SE.Survey_Key
*/



SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Match_Exists_ForSample') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Match_Exists_ForSample'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Match_Exists_ForSample TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Match_Exists_ForSample TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Match_Exists_ForSample TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Match_Exists_ForSample TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Match_Exists_ForSample TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Match_Exists_ForSample TO [Dev - JNCC SQL]
END

GO