If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Get_User_Allow_Finance]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Get_User_Allow_Finance]
GO

CREATE PROCEDURE [dbo].[usp_Get_User_Allow_Finance] 
@UserID CHAR(16),
@UserAllowFinance BIT OUTPUT

AS

--  DESCRIPTION
--  Returns Allow_Finance value from User table
--
--  PARAMETERS
--	NAME				DESCRIPTION
--
--
--  AUTHOR:				Ben Collier, Dorset Software
--  CREATED:			2003-10-10
--
SET NOCOUNT ON

SELECT @UserAllowFinance = Allow_Finance
FROM [USER] U
WHERE U.Name_Key = @UserID

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Get_User_Allow_Finance') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Get_User_Allow_Finance'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Get_User_Allow_Finance TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Get_User_Allow_Finance TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Get_User_Allow_Finance TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Get_User_Allow_Finance TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Get_User_Allow_Finance TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Get_User_Allow_Finance TO [Dev - JNCC SQL]
END

GO