If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_SpecimenIsLifeSciences_ByKey_Get]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_SpecimenIsLifeSciences_ByKey_Get]
GO

CREATE PROCEDURE [dbo].[usp_SpecimenIsLifeSciences_ByKey_Get] 
@Key CHAR(16),
@LifeSciences BIT OUTPUT

AS

--  DESCRIPTION
--  Returns whether a Specimen is Life_Sciences or not
--
--  PARAMETERS
--	NAME				DESCRIPTION
--	@Key				Specimen Key
--
--
--  AUTHOR:				Ben Collier, Dorset Software
--  CREATED:			2003-10-17
--
SET NOCOUNT ON

SELECT @LifeSciences = Life_Sciences 
FROM SPECIMEN_UNIT
WHERE Collection_Unit_Key = @Key

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SpecimenIsLifeSciences_ByKey_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SpecimenIsLifeSciences_ByKey_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SpecimenIsLifeSciences_ByKey_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SpecimenIsLifeSciences_ByKey_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SpecimenIsLifeSciences_ByKey_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_SpecimenIsLifeSciences_ByKey_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SpecimenIsLifeSciences_ByKey_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SpecimenIsLifeSciences_ByKey_Get TO [Dev - JNCC SQL]
END

GO