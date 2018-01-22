/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_UserDomainAccess_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_UserDomainAccess_Update]
GO

/*===========================================================================*\
  Description:	Updates a record in the User_Domain_Access table.

  Parameters:	@Key		User_Domain_Access_Key
		@Browse
		@QuickEntry
		@Add
		@Edit
		@Timestamp
		@SessionID

  Created:	December 2003

  Last revision information:
    $Revision: 3 $
    $Date: 3/02/09 10:55 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_UserDomainAccess_Update]
	@Key char(16),
	@Browse bit,
	@QuickEntry bit,
	@Add bit,
	@Edit bit,
	@Timestamp timestamp,
	@SessionID char(16)
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION

		UPDATE 	User_Domain_Access
		SET	Allow_Browse = @Browse,
			Allow_Quick_Entry = @QuickEntry,
			Allow_Add = @Add,
			Allow_Edit = @Edit,
			Changed_Session_ID = @SessionID
		WHERE	User_Domain_Access_Key = @Key
		AND		[Timestamp] = @Timestamp

		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM User_Domain_Access WHERE User_Domain_Access_Key = @Key)
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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_UserDomainAccess_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_UserDomainAccess_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_UserDomainAccess_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_UserDomainAccess_Update TO [Dev - JNCC SQL]
END
GO