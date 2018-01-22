/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_SpecimenLocation_Get]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_SpecimenLocation_Get]
GO

/*===========================================================================*\
  Description:	Returns the value of Item_Name from the Domain table.

  Parameters:	@Key	Collection unit key
		@Name	OUTPUT

  Created:	September 2003

  Last revision information:
    $Revision: 3 $
    $Date: 3/12/03 9:46 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SpecimenLocation_Get]
	@Key char(16),
	@Name varchar(100) OUTPUT
AS

	IF EXISTS(SELECT * FROM Specimen_Field_Data WHERE Collection_Unit_Key = @Key AND Gathering_Event = 1)
		SELECT		@Name = LN.Item_Name
	
		FROM		Specimen_Field_Data AS SFD
	
		LEFT JOIN	Taxon_Occurrence AS XO ON XO.Taxon_Occurrence_Key = SFD.Taxon_Occurrence_Key
		LEFT JOIN	Occurrence AS O ON O.Occurrence_Key = SFD.Occurrence_Key
		INNER JOIN	[Sample] AS S ON (S.Sample_Key = O.Sample_Key OR S.Sample_Key = XO.Sample_Key)
		INNER JOIN	Location_Name AS LN ON LN.Location_Key = S.Location_Key

		WHERE		SFD.Collection_Unit_Key = @Key AND LN.Preferred = 1

	ELSE
		SET @Name = 'Unknown'

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SpecimenLocation_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SpecimenLocation_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SpecimenLocation_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SpecimenLocation_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SpecimenLocation_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_SpecimenLocation_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SpecimenLocation_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SpecimenLocation_Get TO [Dev - JNCC SQL]
END

GO