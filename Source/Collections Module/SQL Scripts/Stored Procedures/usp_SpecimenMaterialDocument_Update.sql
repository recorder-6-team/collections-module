/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_SpecimenMaterialDocument_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_SpecimenMaterialDocument_Update]
GO

/*===========================================================================*\
  Description:	Sets a specimen's associated Reference.

  Parameters:	@Key	-Collection_Unit_Key
				@Source_Key				-Material_Source_Key

  Created:	27/01/2004

  Last revision information:
    $Revision: 1 $
    $Date: 28/01/04 18:30 $
    $Author: Bencollier $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SpecimenMaterialDocument_Update]
	@Key CHAR(16),
	@Source_Key CHAR(16)
AS

SET NOCOUNT ON
	UPDATE Specimen_Unit
		SET Material_Source_Key = @Source_Key
	WHERE Collection_Unit_Key = @Key
		
SET NOCOUNT OFF

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SpecimenMaterialDocument_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SpecimenMaterialDocument_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SpecimenMaterialDocument_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SpecimenMaterialDocument_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SpecimenMaterialDocument_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_SpecimenMaterialDocument_Update TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SpecimenMaterialDocument_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SpecimenMaterialDocument_Update TO [Dev - JNCC SQL]
END

GO