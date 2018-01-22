If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_FieldData_DomainMask_Get]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_FieldData_DomainMask_Get]
GO

CREATE PROCEDURE [dbo].[usp_FieldData_DomainMask_Get] 
@Key CHAR(16),
@DomainMask INT OUTPUT

AS

--  DESCRIPTION
--  Returns the domain mask of a given Field Data record
--
--  PARAMETERS
--  NAME				DESCRIPTION
--	@Key				Specimen_Field_Data_Key
--
--
--  AUTHOR:     		Ben Collier, Dorset Software
--  CREATED:    		03/02/2004
--

SET NOCOUNT ON

SELECT @DomainMask = Domain_Mask
FROM Specimen_Field_Data SFD
INNER JOIN Collection_Unit CU ON SFD.Collection_Unit_Key = CU.Collection_Unit_Key
WHERE Specimen_Field_Data_Key = @Key

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_FieldData_DomainMask_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_FieldData_DomainMask_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_FieldData_DomainMask_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_FieldData_DomainMask_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_FieldData_DomainMask_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_FieldData_DomainMask_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_FieldData_DomainMask_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_FieldData_DomainMask_Get TO [Dev - JNCC SQL]
END

GO