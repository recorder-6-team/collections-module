If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_SurveyEventGeoArea_Select_ForFieldData]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_SurveyEventGeoArea_Select_ForFieldData]
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

CREATE PROCEDURE [dbo].usp_SurveyEventGeoArea_Select_ForFieldData 
@Key CHAR(16)

AS

--  DESCRIPTION
--  Returns all the Geo Areas associated with the survey event.
--
--	PARAMETERS
--	NAME				DESCRIPTION
--	@Key				They Survey_Event_Key to search on
--
--
--  AUTHOR:				David Kelly, Dorset Software
--  CREATED:			2007-09-07
--
SET NOCOUNT ON

SELECT
	T.Plaintext,
	T.Term_Key,
	SEGA.Survey_Event_Geo_Area_Key,
	SEGA.Survey_Event_Key,
	C1.Concept_Key,
	SFD.Custodian,
	SFD.[Timestamp]
FROM
	Specimen_Field_Data SFD			
	LEFT JOIN 	Taxon_Occurrence XO ON XO.Taxon_Occurrence_Key=SFD.Taxon_Occurrence_Key
	LEFT JOIN 	Occurrence O ON O.Occurrence_Key=SFD.Occurrence_Key
	INNER JOIN 	[Sample] S ON S.Sample_Key=XO.Sample_Key OR S.Sample_Key=O.Sample_Key
	INNER JOIN Survey_Event SE ON SE.Survey_Event_Key = S.Survey_Event_Key
	INNER JOIN Survey_Event_Geo_Area SEGA ON SEGA.Survey_Event_Key = SE.Survey_Event_Key
	INNER JOIN Concept C ON C.Concept_Key = SEGA.Concept_Key
	INNER JOIN Concept C1 ON C1.Meaning_Key = C.Meaning_Key AND C1.List_Preferred = 1
	INNER JOIN Term T ON T.Term_Key = C1.Term_Key

WHERE
	SFD.Specimen_Field_Data_Key = @Key
ORDER BY
	C1.Sort_Code, T.Plaintext

GO
/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SurveyEventGeoArea_Select_ForFieldData') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SurveyEventGeoArea_Select_ForFieldData'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SurveyEventGeoArea_Select_ForFieldData TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SurveyEventGeoArea_Select_ForFieldData TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SurveyEventGeoArea_Select_ForFieldData TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_SurveyEventGeoArea_Select_ForFieldData TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SurveyEventGeoArea_Select_ForFieldData TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SurveyEventGeoArea_Select_ForFieldData TO [Dev - JNCC SQL]
END

GO


