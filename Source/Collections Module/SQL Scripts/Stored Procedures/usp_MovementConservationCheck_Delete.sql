/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_MovementConservationCheck_Delete]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_MovementConservationCheck_Delete]
GO

/*===========================================================================*\
  Description:	Delete a record from the Movement_Conservation_Check table.

  Parameters:	@Key		Movement_Conservation_Check key.

  Created:	April 2004

  Last revision information:
    $Revision: 1 $
    $Date: 13/04/04 15:46 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_MovementConservationCheck_Delete]
	@Key char(16)
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION

		DELETE	Movement_Conservation_Check
		WHERE	Movement_Conservation_Check_Key = @Key

		IF @@Error <> 0 GOTO RollbackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_MovementConservationCheck_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_MovementConservationCheck_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_MovementConservationCheck_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_MovementConservationCheck_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_MovementConservationCheck_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_MovementConservationCheck_Delete TO [Dev - JNCC SQL]
END
GO