/*------------------------------------------------------------------------------*\
  Various changes required to fix the Reports
\*------------------------------------------------------------------------------*/

--==========================================================================================
update report_block_in_section
set population_sql = 'SET NOCOUNT ON
DECLARE @Key CHAR(16)

SET @Key = ''<#ReportKey>''

SELECT DISTINCT
 	SUR.Item_Name AS SurveyName,
	LN.Item_Name AS LocationName,
	dbo.ufn_GetFieldCollectors(@Key) AS FieldCollectors,
	dbo.ufn_GetDateFromVagueDate(S.Vague_Date_Start, S.Vague_Date_End, S.Vague_Date_Type) AS SampleDate
FROM
	Specimen_Unit SU
	INNER JOIN 
		Specimen_Field_Data SFD 
	ON SU.Collection_Unit_Key = SFD.Collection_Unit_Key
		AND SU.Collection_Unit_Key = @Key
		AND SFD.Gathering_Event = 1
	LEFT JOIN 
		Occurrence O 
	ON SFD.Occurrence_Key = O.Occurrence_Key
	LEFT JOIN
		Taxon_Occurrence XO
	ON SFD.Taxon_Occurrence_Key = XO.Taxon_Occurrence_Key
	INNER JOIN 
		[Sample] S
	ON ((O.Sample_Key = S.Sample_Key) OR (XO.Sample_Key = S.Sample_Key))
	INNER JOIN
		Survey_Event SE
	ON S.Survey_Event_Key = SE.Survey_Event_Key
	INNER JOIN
		Survey SUR
	ON SE.Survey_Key = SUR.Survey_Key
	LEFT JOIN
		Location_Name LN
	ON S.Location_Key = LN.Location_Key
'
where report_block_in_section_key = 'system0000000005'

GO
--==========================================================================================
update report_block_in_section
set population_sql_record_count = 'SET NOCOUNT ON
DECLARE @Key CHAR(16)

SET @Key = ''<#ReportKey>''

SELECT COUNT(DISTINCT S.Sample_Key) AS [Count]
FROM
	Specimen_Unit SU
	INNER JOIN 
		Specimen_Field_Data SFD 
	ON SU.Collection_Unit_Key = SFD.Collection_Unit_Key
		AND SU.Collection_Unit_Key = @Key
		AND SFD.Gathering_Event = 1
	LEFT JOIN 
		Occurrence O 
	ON SFD.Occurrence_Key = O.Occurrence_Key
	LEFT JOIN
		Taxon_Occurrence XO
	ON SFD.Taxon_Occurrence_Key = XO.Taxon_Occurrence_Key
	INNER JOIN 
		[Sample] S
	ON ((O.Sample_Key = S.Sample_Key) OR (XO.Sample_Key = S.Sample_Key))
'
where report_block_in_section_key = 'system0000000005'
GO

/*===========================================================================*\
  Drop function before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_GetTranslation_String_From_Value]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	DROP FUNCTION ufn_GetTranslation_String_From_Value
GO

/*===========================================================================*\
  Description:	Converts the input index into an internationalised string (used application-wide).

  Parameters:	@Index		-Input index value
				@Context	-Context which the value is to be interpreted

  Created:	12 Jan 2004

  Last revision information:
    $Revision: 1 $
    $Date: 7/05/04 14:53 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE FUNCTION [dbo].[ufn_GetTranslation_String_From_Value]
(
@Index INT,
@Context VARCHAR(30)
)
RETURNS varchar(50)

AS
BEGIN
DECLARE @ReturnValue VARCHAR(50)
SET @Context = UPPER(@Context)

SET @ReturnValue = 
CASE @Context
WHEN 'YESNO' THEN --General Use
	CASE @Index
		WHEN 0 THEN 'No'
		WHEN 1 THEN 'Yes'
	END
WHEN 'CONFIDENCE' THEN --Used for specimen determinations
	CASE @Index
		WHEN 0 THEN 'Uncertain Confidence'
		WHEN 1 THEN 'No Confidence'
		WHEN 2 THEN 'Low Confidence'
		WHEN 3 THEN 'Medium Confidence'
		WHEN 4 THEN 'High Confidence'
	END
WHEN 'LABELINSCRIPTION' THEN --Used for specimen labels and inscriptions
	CASE @Index
		WHEN 0 THEN 'Label'
		WHEN 1 THEN 'Inscription'
	END
END

RETURN @ReturnValue

END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_GetTranslation_String_From_Value]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	BEGIN
    	PRINT 'Setting up security on function ufn_GetTranslation_String_From_Value'
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
	        	GRANT EXECUTE ON dbo.ufn_GetTranslation_String_From_Value TO [R2k_AddOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
			GRANT EXECUTE ON dbo.ufn_GetTranslation_String_From_Value TO [R2k_Administrator]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
			GRANT EXECUTE ON dbo.ufn_GetTranslation_String_From_Value TO [R2k_FullEdit]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
			GRANT EXECUTE ON dbo.ufn_GetTranslation_String_From_Value TO [R2k_ReadOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
			GRANT EXECUTE ON dbo.ufn_GetTranslation_String_From_Value TO [R2k_RecordCardsOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
	        	GRANT EXECUTE ON dbo.ufn_GetTranslation_String_From_Value TO [Dev - JNCC SQL]
	END
GO
