/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SampleKey_Get') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_SampleKey_Get]
GO
/*===========================================================================*\
  Description:	
  Parameters:	

  Created:	July 2003

  Last revision information:
    $Revision: 11 $
    $Date: 13/10/04 12:19 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SampleKey_Get]
	@Key char(16) OUTPUT,
	@VagueDateStart int,
	@VagueDateEnd int,
	@VagueDateType varchar(2),
	@SpatialRef varchar(40),
	@SpatialRefSystem varchar(4),
	@Lat float,
	@Long float,
	@SpatialRefQualifier varchar(20),
	@SampleTypeKey char(16),
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
		-- Work out checksum of names to look for.
		DECLARE @ChkSm int
		SELECT @ChkSm = CheckSum_Agg(Checksum(Name_Key)) FROM #TempFieldCollectors

		-- Create a table variable to store the samples keys.
		DECLARE @Samples TABLE (Sample_Key char(16) COLLATE SQL_Latin1_General_CP1_CI_AS) 

		-- Select only the samples with ALL the names
		INSERT INTO @Samples (Sample_Key)
		SELECT DISTINCT S.Sample_Key
		FROM 	[Sample] S 
		JOIN 	Sample_Recorder SR ON SR.Sample_Key = S.Sample_Key 
		JOIN 	Survey_Event_Recorder SER ON SER.SE_Recorder_Key = SR.SE_Recorder_Key
		GROUP BY S.Sample_Key
		HAVING CheckSum_Agg(Checksum(SER.Name_Key)) = @ChkSm

		-- Now match on Spatial Ref, Sample Type, Date and Sample Recorders (through @Samples).
		SELECT 	@Key = S.Sample_Key
		FROM 	[Sample] AS S
		JOIN	@Samples AS SA ON S.Sample_Key = SA.Sample_Key
		JOIN 	Survey_Event AS SE ON SE.Survey_Event_Key = S.Survey_Event_Key
		WHERE	SE.Survey_Key = @SurveyKey
		AND	S.Sample_Type_Key = @SampleTypeKey
		AND	((S.Vague_Date_Start = @VagueDateStart AND S.Vague_Date_End = @VagueDateEnd AND S.Vague_Date_Type = @VagueDateType)
			OR 
			(S.Vague_Date_Type = 'U' AND (@VagueDateType='U' OR @VagueDateType IS NULL)))
		AND	IsNull(S.Location_Key, '') = IsNull(@LocationKey, '')
		AND	IsNull(S.Spatial_Ref, '') = IsNull(@SpatialRef, '')
		AND	IsNull(S.Spatial_Ref_System, '') = IsNull(@SpatialRefSystem, '')
		AND	IsNull(S.Location_Name, '') = IsNull(@LocationName, '')
	END
	ELSE
		/*---------------------------------------------------------------------*\
		  This matches on Spatial Ref, Sample Type and Date. I left this here  
		  in case it is required somewhere else and #TempFieldCollectors hasn't 
		  been created by the application.
		\*---------------------------------------------------------------------*/	
		SELECT 	@Key = Sample_Key
		FROM 	[Sample] AS S
		JOIN 	Survey_Event AS SE ON SE.Survey_Event_Key = S.Survey_Event_Key
		WHERE	SE.Survey_Key = @SurveyKey
		AND	S.Sample_Type_Key = @SampleTypeKey
		AND	((S.Vague_Date_Start = @VagueDateStart AND S.Vague_Date_End = @VagueDateEnd AND S.Vague_Date_Type = @VagueDateType)
			OR 
			(S.Vague_Date_Type = 'U' AND (@VagueDateType='U' OR @VagueDateType IS NULL)))
		AND	IsNull(S.Location_Key, '') = IsNull(@LocationKey, '')
		AND	IsNull(S.Spatial_Ref, '') = IsNull(@SpatialRef, '')
		AND	IsNull(S.Spatial_Ref_System, '') = IsNull(@SpatialRefSystem, '')
		AND	IsNull(S.Location_Name, '') = IsNull(@LocationName, '')
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SampleKey_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SampleKey_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SampleKey_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SampleKey_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SampleKey_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_SampleKey_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SampleKey_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SampleKey_Get TO [Dev - JNCC SQL]
END
GO