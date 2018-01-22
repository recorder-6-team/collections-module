/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Individual_Select_ForSpecimenGeneral]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Individual_Select_ForSpecimenGeneral]
GO

/*===========================================================================*\
  Description:	Returns a field gatherers who are relevent to a specimen.

  Parameters:	@Key	Collection Unit Key

  Created:	Setember 2003

  Last revision information:
    $Revision: 3 $
    $Date: 2/12/03 12:07 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Individual_Select_ForSpecimenGeneral]
	@Key char(16)
AS

SET NOCOUNT ON

	SELECT		IsNull (dbo.ufn_GetFormattedName(SER.Name_Key) + ' - ' + RR.Short_Name, 
				dbo.ufn_GetFormattedName(SER.Name_Key)) AS GathererName

	FROM		Specimen_Field_Data AS SFD

	LEFT JOIN	Occurrence AS O ON O.Occurrence_Key = SFD.Occurrence_Key
	LEFT JOIN	Taxon_Occurrence AS XO ON XO.Taxon_Occurrence_Key = SFD.Taxon_Occurrence_Key
	INNER JOIN	Sample_Recorder AS SR ON (SR.Sample_Key = O.Sample_Key OR SR.Sample_Key = XO.Sample_Key)
	INNER JOIN	Survey_Event_Recorder AS SER ON SER.SE_Recorder_Key = SR.SE_Recorder_Key
	LEFT JOIN	Recorder_Role AS RR ON RR.Recorder_Role_Key = SER.Recorder_Role_Key

	WHERE 		SFD.Collection_Unit_Key = @Key 
	AND 		SFD.Gathering_Event = 1

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Individual_Select_ForSpecimenGeneral') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Individual_Select_ForSpecimenGeneral'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Individual_Select_ForSpecimenGeneral TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Individual_Select_ForSpecimenGeneral TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Individual_Select_ForSpecimenGeneral TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Individual_Select_ForSpecimenGeneral TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Individual_Select_ForSpecimenGeneral TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Individual_Select_ForSpecimenGeneral TO [Dev - JNCC SQL]
END

GO