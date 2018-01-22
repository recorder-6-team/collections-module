If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_ConservationJob_DomainMask_Get]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_ConservationJob_DomainMask_Get]
GO

CREATE PROCEDURE [dbo].[usp_ConservationJob_DomainMask_Get] 
@Key CHAR(16),
@DomainMask INT OUTPUT

AS

--  DESCRIPTION
--  Returns the domain mask of a given Conservation Job
--
--  PARAMETERS
--  NAME				DESCRIPTION
--	@Key				Conservation_Job_Key
--
--
--  AUTHOR:     		Ben Collier, Dorset Software
--  CREATED:    		03/02/2004
--

SET NOCOUNT ON

SELECT @DomainMask = Domain_Mask
FROM Conservation_Job
WHERE Conservation_Job_Key = @Key

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConservationJob_DomainMask_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConservationJob_DomainMask_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConservationJob_DomainMask_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConservationJob_DomainMask_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConservationJob_DomainMask_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ConservationJob_DomainMask_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConservationJob_DomainMask_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConservationJob_DomainMask_Get TO [Dev - JNCC SQL]
END

GO