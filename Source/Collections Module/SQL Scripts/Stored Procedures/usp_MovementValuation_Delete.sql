/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_MovementValuation_Delete]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_MovementValuation_Delete]
GO

/*===========================================================================*\
  Description:	Delete a record from the Movement_Valuation table.

  Parameters:	@Key		Movement_Valuation key.

  Created:	April 2004

  Last revision information:
    $Revision: 1 $
    $Date: 13/04/04 15:46 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_MovementValuation_Delete]
	@Key char(16)
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION

		DELETE	Movement_Valuation
		WHERE	Movement_Valuation_Key = @Key

		IF @@Error <> 0 GOTO RollbackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_MovementValuation_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_MovementValuation_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_MovementValuation_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_MovementValuation_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_MovementValuation_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_MovementValuation_Delete TO [Dev - JNCC SQL]
END
GO