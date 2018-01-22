If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Specimen_HasDetermination_Get]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Specimen_HasDetermination_Get]
GO

CREATE PROCEDURE [dbo].[usp_Specimen_HasDetermination_Get] 
@SpecimenUnitKey CHAR(16),
@HasDetermination BIT OUTPUT

AS

--  DESCRIPTION
--  Returns whether or not a specimen has a determination
--
--	PARAMETERS
--	NAME					DESCRIPTION
--	@SpecimenUnitKey		Specimen Collection_Unit_Key
--	@HasDetermination		Output
--
--
--  AUTHOR:				Ben Collier, Dorset Software
--  CREATED:			16/02/2004
--

SELECT @HasDetermination = 
CASE 
	WHEN Preferred_Determination_Key IS NOT NULL THEN 1 
	WHEN Preferred_Taxon_Determination_Key IS NOT NULL THEN 1
	ELSE 0 END
FROM
	Specimen_Unit
WHERE Collection_Unit_Key = @SpecimenUnitKey

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Specimen_HasDetermination_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Specimen_HasDetermination_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Specimen_HasDetermination_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimen_HasDetermination_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimen_HasDetermination_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Specimen_HasDetermination_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimen_HasDetermination_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Specimen_HasDetermination_Get TO [Dev - JNCC SQL]
END

GO