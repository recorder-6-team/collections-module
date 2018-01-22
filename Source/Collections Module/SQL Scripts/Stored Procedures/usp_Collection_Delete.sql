/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Collection_Delete]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Collection_Delete]
GO

/*===========================================================================*\
  Description:	Delete a record from the Collection table.
		Its mask should already have been dealt with and be 0. If not, 
		something wrong	probably happened somewhere else.

  Parameters:	@Key

  Created:	July 2003

  Last revision information:
    $Revision: 8 $
    $Date: 2/02/09 17:07 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Collection_Delete]
	@Key char(16),
	@Timestamp timestamp
AS
	BEGIN TRANSACTION

		-- Delete record from Collection table.
		DELETE	Collection
		WHERE	Collection_Unit_Key = @Key
	
		IF @@Error <> 0 GOTO RollbackAndExit

		DELETE	Specimen_Unit
		WHERE	Collection_Unit_Key = @Key

		IF @@Error <> 0 GOTO RollbackAndExit

		-- Delete record from Collection_Unit table.
		DELETE	Collection_Unit
		WHERE	Collection_Unit_Key = @Key
		AND		@Timestamp = [Timestamp]

		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Collection_Unit WHERE Collection_Unit_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

		IF @@Error <> 0 GOTO RollbackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Collection_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Collection_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Collection_Delete TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Collection_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Collection_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Collection_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Collection_Delete TO [Dev - JNCC SQL]
END
GO