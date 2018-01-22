If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Movement_DomainMask_Get]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Movement_DomainMask_Get]
GO

CREATE PROCEDURE [dbo].[usp_Movement_DomainMask_Get] 
@Key CHAR(16),
@DomainMask INT OUTPUT

AS

--  DESCRIPTION
--  Returns the domain mask of a given movement
--
--  PARAMETERS
--  NAME				DESCRIPTION
--	@Key				Movement_Key
--
--
--  AUTHOR:     		Ben Collier, Dorset Software
--  CREATED:    		03/02/2004
--

SET NOCOUNT ON


DECLARE @ItemDomain INT

SET @DomainMask = 0

DECLARE csr CURSOR FOR
SELECT DISTINCT CU.Domain_Mask
FROM Movement M
INNER JOIN Movement_Direction MD
ON M.Movement_Key = MD.Movement_Key 
LEFT JOIN Movement_Collection_Unit MCU
ON MD.Movement_Direction_Key = MCU.Movement_Direction_Key
LEFT JOIN Collection_Unit CU
ON MCU.Collection_Unit_Key = CU.Collection_Unit_Key
WHERE M.Movement_Key = @Key

OPEN csr

WHILE 1=1
BEGIN
	FETCH NEXT FROM csr INTO @ItemDomain
	IF @@FETCH_STATUS<>0
		BREAK
	SET @DomainMask = @DomainMask | @ItemDomain
END

CLOSE csr
DEALLOCATE csr

--If no CollectionUnits are linked then return 0, an unset domain
SELECT @DomainMask = ISNULL(@DomainMask, 0)

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Movement_DomainMask_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Movement_DomainMask_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Movement_DomainMask_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Movement_DomainMask_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Movement_DomainMask_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Movement_DomainMask_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Movement_DomainMask_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Movement_DomainMask_Get TO [Dev - JNCC SQL]
END

GO