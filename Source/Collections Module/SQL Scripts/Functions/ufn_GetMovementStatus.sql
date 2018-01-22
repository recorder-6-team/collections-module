/*===========================================================================*\
  Drop function before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_GetMovementStatus]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	DROP FUNCTION ufn_GetMovementStatus
GO

/*===========================================================================*\
  Description:	Returns movement status for a movement

  Parameters:	@Key		- movement key

  Created:	Aug 2004

  Last revision information:
    $Revision: 1 $
    $Date: 19/08/04 11:31 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE FUNCTION dbo.ufn_GetMovementStatus(
  @Key CHAR(16)
)
RETURNS varchar(50)

AS
BEGIN

	DECLARE @TotalComplete int
	DECLARE @TotalIncomplete int
	DECLARE @Status VARCHAR(50)
	
	/*==============================================*\
  	   Count how many have been completed.
	\*==============================================*/
	SELECT @TotalComplete = Count(*)
	FROM Movement_Of_Ownership M
	INNER JOIN Movement_Direction MD ON MD.Movement_Direction_Key=M.Movement_Direction_Key
	WHERE MD.Movement_Key=@Key
	AND Completed=1
	
	SELECT @TotalComplete = @TotalComplete + Count(*)
	FROM Movement_Of_Material M
	INNER JOIN Movement_Direction MD ON MD.Movement_Direction_Key=M.Movement_Direction_Key
	WHERE MD.Movement_Key=@Key
	AND Completed=1

	/*==============================================*\
  	   Count how many have not been completed.
	\*==============================================*/
	SELECT @TotalIncomplete = Count(*)
	FROM Movement_Of_Ownership M
	INNER JOIN Movement_Direction MD ON MD.Movement_Direction_Key=M.Movement_Direction_Key
	WHERE MD.Movement_Key=@Key
	AND Completed=0
	
	SELECT @TotalIncomplete = @TotalIncomplete + Count(*)
	FROM Movement_Of_Material M
	INNER JOIN Movement_Direction MD ON MD.Movement_Direction_Key=M.Movement_Direction_Key
	WHERE MD.Movement_Key=@Key
	AND Completed=0

	/*===============================*\
  	   Determine the status.
	\*===============================*/
	IF @TotalIncomplete = 0 AND @TotalComplete <> 0
		SET @Status = 'Completed'
	ELSE IF @TotalIncomplete <> 0 AND @TotalComplete = 0
		SET @Status = 'Not started'
	ELSE
		SET @Status = 'Incomplete'

	RETURN @Status

END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_GetMovementStatus]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	BEGIN
    	PRINT 'Setting up security on function ufn_GetMovementStatus'
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
	        	GRANT EXECUTE ON dbo.ufn_GetMovementStatus TO [R2k_AddOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
			GRANT EXECUTE ON dbo.ufn_GetMovementStatus TO [R2k_Administrator]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
			GRANT EXECUTE ON dbo.ufn_GetMovementStatus TO [R2k_FullEdit]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
			GRANT EXECUTE ON dbo.ufn_GetMovementStatus TO [R2k_ReadOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
			GRANT EXECUTE ON dbo.ufn_GetMovementStatus TO [R2k_RecordCardsOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
	        	GRANT EXECUTE ON dbo.ufn_GetMovementStatus TO [Dev - JNCC SQL]
	END
GO