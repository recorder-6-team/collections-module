/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_MovementMaterialDetail_Delete]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_MovementMaterialDetail_Delete]
GO

/*===========================================================================*\
  Description:	Delete a record from the Movement_Of_Material table.

  Parameters:	@Key
		@Timestamp

  Created:	December 2003

  Last revision information:
    $Revision: 3 $
    $Date: 3/02/09 9:49 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_MovementMaterialDetail_Delete]
	@Key char(16),
	@Timestamp timestamp
AS
	SET NOCOUNT OFF

	BEGIN TRANSACTION

		-- Delete record from Movement_Of_Material table.
		DELETE	Movement_Of_Material
		WHERE	Movement_Of_Material_Key = @Key
		AND		[Timestamp] = @Timestamp
		
		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Movement_Of_Material WHERE Movement_Of_Material_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_MovementMaterialDetail_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_MovementMaterialDetail_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_MovementMaterialDetail_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_MovementMaterialDetail_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_MovementMaterialDetail_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_MovementMaterialDetail_Delete TO [Dev - JNCC SQL]
END
GO