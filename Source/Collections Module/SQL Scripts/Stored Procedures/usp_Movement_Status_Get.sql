/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Movement_Status_Get]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Movement_Status_Get]
GO

/*===========================================================================*\
  Description:	Returns status for FrameMovementGeneral.

  Parameters:	@Key		Movement key
		@Status		Returns the status of the Movement. This can
				be Completed, Incomplete or Not Started.

  Created:	September 2003

  Last revision information:
    $Revision: 2 $
    $Date: 12/11/03 14:48 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Movement_Status_Get]
	@Key char(16),
	@Status varchar(20) output
AS

SET NOCOUNT ON

	DECLARE @TotalComplete int
	DECLARE @TotalIncomplete int
	
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

SET NOCOUNT OFF

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Movement_Status_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Movement_Status_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Movement_Status_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Movement_Status_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Movement_Status_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Movement_Status_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Movement_Status_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Movement_Status_Get TO [Dev - JNCC SQL]
END

GO