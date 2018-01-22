If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_SurveyEventKey_Get_ForFieldData]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_SurveyEventKey_Get_ForFieldData]
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

CREATE PROCEDURE [dbo].usp_SurveyEventKey_Get_ForFieldData 
	@Key char(16),
	@SurveyEventKey char(16) OUTPUT

AS

--  DESCRIPTION
--  Insert a record into Survey_Event_Geo_Area
--
--	PARAMETERS
--	NAME				DESCRIPTION
--	@Key				The Specimen_Field_Data_Key of the record
--  @SurveyEventKey		The Survey_Event_Key of the record
--
--
--  AUTHOR:				David Kelly, Dorset Software
--  CREATED:			2007-09-07
--


	-- Required to be able to get number of changed records.
	SET NOCOUNT ON

	SELECT 	@SurveyEventKey = S.Survey_Event_Key
	FROM
	Specimen_Field_Data SFD			
	LEFT JOIN 	Taxon_Occurrence XO ON XO.Taxon_Occurrence_Key=SFD.Taxon_Occurrence_Key
	LEFT JOIN 	Occurrence O ON O.Occurrence_Key=SFD.Occurrence_Key
	INNER JOIN 	[Sample] S ON S.Sample_Key=XO.Sample_Key OR S.Sample_Key=O.Sample_Key
	WHERE	SFD.Specimen_Field_Data_Key = @Key


GO
/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SurveyEventKey_Get_ForFieldData') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SurveyEventKey_Get_ForFieldData'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SurveyEventKey_Get_ForFieldData TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SurveyEventKey_Get_ForFieldData TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SurveyEventKey_Get_ForFieldData TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_SurveyEventKey_Get_ForFieldData TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SurveyEventKey_Get_ForFieldData TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SurveyEventKey_Get_ForFieldData TO [Dev - JNCC SQL]
END

GO


