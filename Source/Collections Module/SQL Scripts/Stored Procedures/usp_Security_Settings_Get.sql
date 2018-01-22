If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Security_Settings_Get]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Security_Settings_Get]
GO

CREATE PROCEDURE [dbo].[usp_Security_Settings_Get] 
@UserID CHAR(16)

AS

--  DESCRIPTION
--  Returns User Security options from the User table
--
--  PARAMETERS
--	NAME				DESCRIPTION
--	@UserID				ID/Name_Key of User
--
--
--  AUTHOR:				Ben Collier, Dorset Software
--  CREATED:			2003-10-10
--
SET NOCOUNT ON

SELECT [SECURITY_LEVEL], [FIRST_LOGIN], [ALLOW_QUICK_ENTRY], [ALLOW_QUICK_ENTRY_PROCESSING], 
		[ALLOW_MOVEMENT_EDIT], [ALLOW_FINANCE], [SECURITY_LEVEL]
FROM [USER] U
WHERE U.Name_Key = @UserID

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Security_Settings_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Security_Settings_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Security_Settings_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Security_Settings_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Security_Settings_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Security_Settings_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Security_Settings_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Security_Settings_Get TO [Dev - JNCC SQL]
END

GO