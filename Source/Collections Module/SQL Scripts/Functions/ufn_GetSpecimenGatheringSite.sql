/*===========================================================================*\
  Drop function before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_GetSpecimenGatheringSite]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	DROP FUNCTION ufn_GetSpecimenGatheringSite
GO

/*===========================================================================*\
  Description:	For a Collection_Unit_Key, the gathering site is returned.

  Parameters:	@Key 	Collection_Unit_Key

  Created:	December 2003

  Last revision information:
    $Revision: 2 $
    $Date: 6/05/04 15:00 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE FUNCTION [dbo].[ufn_GetSpecimenGatheringSite]
	(@Key char(16))
RETURNS varchar(100)
AS
BEGIN
	DECLARE	@SpecimenLocation varchar(100)

	IF EXISTS(SELECT * FROM Specimen_Field_Data WHERE Collection_Unit_Key = @Key AND Gathering_Event = 1)
		SELECT		@SpecimenLocation = LN.Item_Name
	
		FROM		Specimen_Field_Data AS SFD
	
		LEFT JOIN	Taxon_Occurrence AS XO ON XO.Taxon_Occurrence_Key = SFD.Taxon_Occurrence_Key
		LEFT JOIN	Occurrence AS O ON O.Occurrence_Key = SFD.Occurrence_Key
		INNER JOIN	[Sample] AS S ON (S.Sample_Key = O.Sample_Key OR S.Sample_Key = XO.Sample_Key)
		INNER JOIN	Location_Name AS LN ON LN.Location_Key = S.Location_Key

		WHERE		SFD.Collection_Unit_Key = @Key AND LN.Preferred = 1
	ELSE
		SET @SpecimenLocation = 'Unknown'

	RETURN @SpecimenLocation
END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_GetSpecimenGatheringSite]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	BEGIN
    	PRINT 'Setting up security on function ufn_GetSpecimenGatheringSite'
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
	        	GRANT EXECUTE ON dbo.ufn_GetSpecimenGatheringSite TO [R2k_AddOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
			GRANT EXECUTE ON dbo.ufn_GetSpecimenGatheringSite TO [R2k_Administrator]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
			GRANT EXECUTE ON dbo.ufn_GetSpecimenGatheringSite TO [R2k_FullEdit]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
			GRANT EXECUTE ON dbo.ufn_GetSpecimenGatheringSite TO [R2k_ReadOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
			GRANT EXECUTE ON dbo.ufn_GetSpecimenGatheringSite TO [R2k_RecordCardsOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
	        	GRANT EXECUTE ON dbo.ufn_GetSpecimenGatheringSite TO [Dev - JNCC SQL]
	END
GO