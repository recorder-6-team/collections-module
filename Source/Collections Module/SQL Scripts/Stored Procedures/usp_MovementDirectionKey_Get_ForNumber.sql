/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_MovementDirectionKey_Get_ForNumber]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_MovementDirectionKey_Get_ForNumber]
GO
/*===========================================================================*\
  Description: Gets a MovementKey	
  Parameters:	

  Created:	September 2003

  Last revision information:
    $Revision: 1 $
    $Date: 24/02/04 15:17 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_MovementDirectionKey_Get_ForNumber]
	@Key char(16) OUTPUT,
	@MovementNumber varchar(30)
	
AS
	SELECT @Key = MD.Movement_Direction_Key
	FROM Movement 
	INNER JOIN Movement_Direction MD ON Movement.Movement_Key = MD.Movement_Key 
	INNER JOIN Movement_Of_Material MM ON MM.Movement_Direction_Key=MD.Movement_Direction_Key
	WHERE (Number = @MovementNumber)
	AND Outbound = 0
GO


/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_MovementDirectionKey_Get_ForNumber') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_MovementDirectionKey_Get_ForNumber'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_MovementDirectionKey_Get_ForNumber TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_MovementDirectionKey_Get_ForNumber TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_MovementDirectionKey_Get_ForNumber TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_MovementDirectionKey_Get_ForNumber TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_MovementDirectionKey_Get_ForNumber TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_MovementDirectionKey_Get_ForNumber TO [Dev - JNCC SQL]
END

GO