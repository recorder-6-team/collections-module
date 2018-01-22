/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_LocalDomain_Delete]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_LocalDomain_Delete]
GO

/*===========================================================================*\
  Description:	Deletes a Local_Domain record.

  Parameters:	@Key	Local_Domain_Key
				@Timestamp

  Created:	December 2003

  Last revision information:
    $Revision: 8 $
    $Date: 3/02/09 9:35 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_LocalDomain_Delete]
	@Key char(16),
	@Timestamp timestamp = NULL
AS

SET NOCOUNT ON

	BEGIN TRANSACTION
	
		DELETE 
		FROM 	Local_Domain
		WHERE	Local_Domain_Key = @Key
		AND		([Timestamp] = @Timestamp OR (@Timestamp IS NULL))

		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Local_Domain WHERE Local_Domain_Key = @Key)
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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_LocalDomain_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_LocalDomain_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_LocalDomain_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_LocalDomain_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_LocalDomain_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_LocalDomain_Delete TO [Dev - JNCC SQL]
END
GO