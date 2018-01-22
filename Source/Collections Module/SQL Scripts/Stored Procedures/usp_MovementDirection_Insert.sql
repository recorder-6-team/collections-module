/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_MovementDirection_Insert]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_MovementDirection_Insert]
GO
/*===========================================================================*\
  Description:	
  Parameters:	

  Created:	July 2003

  Last revision information:
    $Revision: 3 $
    $Date: 8/12/03 11:15 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_MovementDirection_Insert]
	@Key char(16) output,
	@MovementKey char(16),
	@Outbound bit,
	@SessionID char(16)
	
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF
	
	EXECUTE spNextKey 'Movement_Direction', @Key OUTPUT
	
	BEGIN TRANSACTION
		INSERT INTO Movement_Direction (
			Movement_Direction_Key,
			Movement_Key,
			Outbound,
			Entered_Session_ID
		) VALUES (
			@Key,
			@MovementKey,
			@Outbound,
			@SessionID
		)

		IF @@Error <> 0 GOTO RollBackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit:
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_MovementDirection_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_MovementDirection_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_MovementDirection_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_MovementDirection_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_MovementDirection_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_MovementDirection_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_MovementDirection_Insert TO [Dev - JNCC SQL]
END
GO