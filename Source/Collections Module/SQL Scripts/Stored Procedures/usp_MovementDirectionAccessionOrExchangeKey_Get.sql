/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_MovementDirectionAccessionOrExchangeKey_Get]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_MovementDirectionAccessionOrExchangeKey_Get]
GO
/*===========================================================================*\
  Description: Gets a MovementKey	
  Parameters:	

  Created:	September 2003

  Last revision information:
    $Revision: 2 $
    $Date: 12/11/03 14:48 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_MovementDirectionAccessionOrExchangeKey_Get]
	@Key char(16) OUTPUT,
	@MovementNumber varchar(30)
	
AS
	Select @Key = Movement_Direction_Key
	from Movement inner join Movement_Direction 
	on Movement.Movement_Key = Movement_Direction.Movement_Key 
	where 
	(Number = @MovementNumber)
	and (Movement_Type in (0,1))
	and Outbound = 0
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_MovementDirectionAccessionOrExchangeKey_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_MovementDirectionAccessionOrExchangeKey_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_MovementDirectionAccessionOrExchangeKey_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_MovementDirectionAccessionOrExchangeKey_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_MovementDirectionAccessionOrExchangeKey_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_MovementDirectionAccessionOrExchangeKey_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_MovementDirectionAccessionOrExchangeKey_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_MovementDirectionAccessionOrExchangeKey_Get TO [Dev - JNCC SQL]
END

GO