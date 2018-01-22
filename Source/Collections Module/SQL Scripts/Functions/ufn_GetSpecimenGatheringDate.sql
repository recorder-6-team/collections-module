/*===========================================================================*\
  Drop function before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_GetSpecimenGatheringDate]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	DROP FUNCTION ufn_GetSpecimenGatheringDate
GO

/*===========================================================================*\
  Description:	For a Collection_Unit_Key, the location is returned.

  Parameters:	@Key 	Collection_Unit_Key

  Created:	December 2003

  Last revision information:
    $Revision: 2 $
    $Date: 6/05/04 15:00 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE FUNCTION [dbo].[ufn_GetSpecimenGatheringDate]
	(@Key char(16))
RETURNS varchar(12)
AS
BEGIN
	DECLARE	@Date varchar(12)

	IF EXISTS(SELECT * FROM Specimen_Field_Data WHERE Collection_Unit_Key = @Key AND Gathering_Event = 1)
		SELECT		@Date = dbo.ufn_GetDateFromVagueDate
					(S.Vague_Date_Start, S.Vague_Date_End, S.Vague_Date_Type)
	
		FROM		Specimen_Field_Data AS SFD
	
		INNER JOIN	Occurrence As O ON O.Occurrence_Key = SFD.Occurrence_Key
		INNER JOIN	Sample AS S ON S.Sample_Key = O.Sample_Key

		WHERE		SFD.Collection_Unit_Key = @Key

	ELSE
		SET @Date = 'Unknown'

	RETURN @Date
END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_GetSpecimenGatheringDate]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	BEGIN
    	PRINT 'Setting up security on function ufn_GetSpecimenGatheringDate'
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
	        	GRANT EXECUTE ON dbo.ufn_GetSpecimenGatheringDate TO [R2k_AddOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
			GRANT EXECUTE ON dbo.ufn_GetSpecimenGatheringDate TO [R2k_Administrator]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
			GRANT EXECUTE ON dbo.ufn_GetSpecimenGatheringDate TO [R2k_FullEdit]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
			GRANT EXECUTE ON dbo.ufn_GetSpecimenGatheringDate TO [R2k_ReadOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
			GRANT EXECUTE ON dbo.ufn_GetSpecimenGatheringDate TO [R2k_RecordCardsOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
	        	GRANT EXECUTE ON dbo.ufn_GetSpecimenGatheringDate TO [Dev - JNCC SQL]
	END
GO
