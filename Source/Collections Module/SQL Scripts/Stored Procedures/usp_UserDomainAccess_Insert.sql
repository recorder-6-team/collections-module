/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_UserDomainAccess_Insert]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_UserDomainAccess_Insert]
GO

/*===========================================================================*\
  Description:	Inserts a record in the User_Domain_Access table.

  Parameters:	@Key		OUTPUT
		@NameKey	User's name key
		@DomainKey
		@Browse
		@QuickEntry
		@Add
		@Edit
		@SessionID

  Created:	December 2003

  Last revision information:
    $Revision: 1 $
    $Date: 5/12/03 16:58 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_UserDomainAccess_Insert]
	@Key char(16) OUTPUT,
	@NameKey char(16),
	@DomainKey char(16),
	@Browse bit,
	@QuickEntry bit,
	@Add bit,
	@Edit bit,
	@SessionID char(16)
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	EXEC spNextKey 'User_Domain_Access', @Key OUTPUT

	BEGIN TRANSACTION

		INSERT 	User_Domain_Access (
			User_Domain_Access_Key, Name_Key, Domain_Key, 
			Allow_Browse, Allow_Quick_Entry, Allow_Add, Allow_Edit,
			Entered_Session_ID
		) VALUES (
			@Key, @NameKey, @DomainKey,
			@Browse, @QuickEntry, @Add, @Edit,
			@SessionID
		)

		IF @@Error <> 0 GOTO RollbackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_UserDomainAccess_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_UserDomainAccess_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_UserDomainAccess_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_UserDomainAccess_Insert TO [Dev - JNCC SQL]
END
GO