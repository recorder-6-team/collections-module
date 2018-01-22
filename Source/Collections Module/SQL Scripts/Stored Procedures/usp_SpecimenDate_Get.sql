/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_SpecimenDate_Get]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_SpecimenDate_Get]
GO

/*===========================================================================*\
  Description:	Returns the value of Date from the Sample table.

  Parameters:	@Key			Collection unit key
		@ShortDateFormat	Required Date Format
		@Date			OUTPUT

  Created:	September 2003

  Last revision information:
    $Revision: 3 $
    $Date: 9/01/04 11:53 $
    $Author: Bencollier $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SpecimenDate_Get]
	@Key char(16),
	@ShortDateFormat varchar(12),
	@Date varchar(50) OUTPUT
AS

	IF EXISTS(SELECT * FROM Specimen_Field_Data WHERE Collection_Unit_Key = @Key AND Gathering_Event = 1)
		SELECT		@Date = dbo.ufn_GetDateFromVagueDate
					(S.Vague_Date_Start, S.Vague_Date_End, S.Vague_Date_Type)
	
		FROM		Specimen_Field_Data AS SFD
	
		INNER JOIN	Occurrence As O ON O.Occurrence_Key = SFD.Occurrence_Key
		INNER JOIN	Sample AS S ON S.Sample_Key = O.Sample_Key

		WHERE		SFD.Collection_Unit_Key = @Key

	ELSE
		SET @Date = 'Unknown'

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SpecimenDate_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SpecimenDate_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SpecimenDate_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SpecimenDate_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SpecimenDate_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_SpecimenDate_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SpecimenDate_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SpecimenDate_Get TO [Dev - JNCC SQL]
END

GO