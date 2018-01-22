If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_SurveyEventGeoArea_Select]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_SurveyEventGeoArea_Select]
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

CREATE PROCEDURE [dbo].usp_SurveyEventGeoArea_Select 
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
	SEGA.Concept_Key,
	SE.Custodian,
	SEGA.[Timestamp]
FROM
	Term T
	JOIN Concept C ON C.Term_Key = T.Term_Key
	JOIN Concept C1 ON C1.Meaning_Key = C.Meaning_Key AND C.List_Preferred = 1
	JOIN Survey_Event_Geo_Area SEGA ON SEGA.Concept_Key = C1.Concept_Key
	JOIN Survey_Event SE ON SE.Survey_Event_Key = SEGA.Survey_Event_Key
WHERE
	SEGA.Survey_Event_Key = @Key
ORDER BY
	C1.Sort_Code, T.Plaintext

GO
/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SurveyEventGeoArea_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SurveyEventGeoArea_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SurveyEventGeoArea_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SurveyEventGeoArea_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SurveyEventGeoArea_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_SurveyEventGeoArea_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SurveyEventGeoArea_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SurveyEventGeoArea_Select TO [Dev - JNCC SQL]
END

GO


