/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_User_Update_ForSystemAccess]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_User_Update_ForSystemAccess]
GO

/*===========================================================================*\
  Description:	Updates a record in the User table, for System Access 
		permissions only.

  Parameters:	@NameKey
		@QuickEntry
		@Movements
		@ProcessQuickEntry
		@Finance

  Created:	December 2003

  Last revision information:
    $Revision: 1 $
    $Date: 5/12/03 16:58 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_User_Update_ForSystemAccess]
	@NameKey char(16),
	@QuickEntry bit,
	@Movements bit,
	@ProcessQuickEntry bit,
	@Finance bit
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION

		UPDATE 	[User]
		SET	Allow_Quick_Entry = @QuickEntry,
			Allow_Quick_Entry_Processing = @ProcessQuickEntry,
			Allow_Movement_Edit = @Movements,
			Allow_Finance = @Finance
		WHERE	Name_Key = @NameKey

		IF @@Error <> 0 GOTO RollBackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_User_Update_ForSystemAccess') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_User_Update_ForSystemAccess'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_User_Update_ForSystemAccess TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_User_Update_ForSystemAccess TO [Dev - JNCC SQL]
END
GO