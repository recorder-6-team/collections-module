/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Funding_Delete]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Funding_Delete]
GO

/*===========================================================================*\
  Description:	Delete a record from either the Conservation_Job_Funding table
		or the Movement_Funding table.

  Parameters:	@Key
		@IsMovement
		@Timestamp

  Created:	December 2003

  Last revision information:
    $Revision: 3 $
    $Date: 3/02/09 9:31 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Funding_Delete]
	@Key char(16),
	@IsMovement bit,
	@Timestamp timestamp
AS
	SET NOCOUNT OFF

	BEGIN TRANSACTION
		
		DECLARE @Error int
		DECLARE @RecordsAffected int

		IF @IsMovement = 1 
		BEGIN
			-- Delete record from Movement_Funding table.
			DELETE	Movement_Funding
			WHERE	Movement_Funding_Key = @Key
			AND		[Timestamp] = @Timestamp

			SELECT @Error = @@Error, @RecordsAffected = @@Rowcount
		
			IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Movement_Funding WHERE Movement_Funding_Key = @Key)
			BEGIN
				RAISERROR('Record updated by another user', 16, 1)
				GOTO RollbackAndExit
			END
		END ELSE BEGIN
			-- Delete record from Conservation_Job_Funding table.
			DELETE	Conservation_Job_Funding
			WHERE	Conservation_Job_Funding_Key = @Key
			AND		[Timestamp] = @Timestamp

			SELECT @Error = @@Error, @RecordsAffected = @@Rowcount
		
			IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Conservation_Job_Funding WHERE Conservation_Job_Funding_Key = @Key)
			BEGIN
				RAISERROR('Record updated by another user', 16, 1)
				GOTO RollbackAndExit
			END
		END
	
		IF @Error <> 0 GOTO RollbackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Funding_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Funding_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Funding_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Funding_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Funding_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Funding_Delete TO [Dev - JNCC SQL]
END
GO