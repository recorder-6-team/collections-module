/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_SampleRecorders_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_SampleRecorders_Select]
GO

/*===========================================================================*\
  Description:	Returns a list of field collectors which is shown in the
		Field Data frame.

  Parameters:	@Key	Specimen_Field_Data key

  Created:	October 2003

  Last revision information:
    $Revision: 3 $
    $Date: 21/01/04 11:58 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SampleRecorders_Select]
	@Key char(16)
AS

SET NOCOUNT ON

	SELECT 		SER.Name_Key,
			SER.SE_Recorder_Key,
			SER.Survey_Event_Key,
			dbo.ufn_GetFormattedName(SER.Name_Key) AS Name,
			SFD.Custodian,
			SFD.[Timestamp],
			S.Sample_Key,
			XO.Taxon_Occurrence_Key,
			O.Occurrence_Key
	FROM		Specimen_Field_Data SFD			
	LEFT JOIN 	Taxon_Occurrence XO ON XO.Taxon_Occurrence_Key=SFD.Taxon_Occurrence_Key
	LEFT JOIN 	Occurrence O ON O.Occurrence_Key=SFD.Occurrence_Key
	INNER JOIN 	[Sample] S ON S.Sample_Key=XO.Sample_Key OR S.Sample_Key=O.Sample_Key
	INNER JOIN	Sample_Recorder SR ON SR.Sample_Key = S.Sample_Key
	INNER JOIN	Survey_Event_Recorder SER ON SER.SE_Recorder_Key = SR.SE_Recorder_Key
	WHERE		SFD.Specimen_Field_Data_Key = @Key

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SampleRecorders_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SampleRecorders_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SampleRecorders_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SampleRecorders_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SampleRecorders_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_SampleRecorders_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SampleRecorders_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SampleRecorders_Select TO [Dev - JNCC SQL]
END

GO