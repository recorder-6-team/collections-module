If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_CollectionUnitName_Exists_Get]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_CollectionUnitName_Exists_Get]
GO

CREATE PROCEDURE [dbo].[usp_CollectionUnitName_Exists_Get] 
@CollectionUnitKey CHAR(16),
@NameKey CHAR(16),
@Exists BIT OUTPUT

AS

--  DESCRIPTION
--  Determines if a Collection_Unit_Name record exists for a given Collection_Unit_Key and Name_Key
--
--	PARAMETERS
--	NAME				DESCRIPTION
--	@CollectionUnitKey 	Collection_Unit_Key
--	@NameKey			Name_Key
--	@Exists				OUTPUT Param
--
--
--  AUTHOR:				Ben Collier, Dorset Software
--  CREATED:			2003-08-12
--
SET NOCOUNT ON

IF EXISTS(SELECT * FROM Collection_Unit_Name WHERE Collection_Unit_Key = @CollectionUnitKey AND Name_Key = @NameKey)
	SET @Exists = 1
ELSE
	SET @Exists = 0
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_CollectionUnitName_Exists_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_CollectionUnitName_Exists_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitName_Exists_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_CollectionUnitName_Exists_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_CollectionUnitName_Exists_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_CollectionUnitName_Exists_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_CollectionUnitName_Exists_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitName_Exists_Get TO [Dev - JNCC SQL]
END

GO