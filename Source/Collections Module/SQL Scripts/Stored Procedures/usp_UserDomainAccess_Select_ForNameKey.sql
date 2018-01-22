/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_UserDomainAccess_Select_ForNameKey]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_UserDomainAccess_Select_ForNameKey]
GO

/*===========================================================================*\
  Description:	Returns all users, and the formatted user name.

  Parameters:	<none>

  Created:	December 2003

  Last revision information:
    $Revision: 1 $
    $Date: 5/12/03 16:58 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_UserDomainAccess_Select_ForNameKey]
	@NameKey char(16)
AS
SET NOCOUNT ON

	SELECT 	D.Item_Name AS Domain_Name, UDA.*
	FROM	User_Domain_Access UDA
	JOIN	Domain D ON D.Domain_Key = UDA.Domain_Key
	WHERE	Name_Key = @NameKey

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_UserDomainAccess_Select_ForNameKey') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_UserDomainAccess_Select_ForNameKey'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_UserDomainAccess_Select_ForNameKey TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_UserDomainAccess_Select_ForNameKey TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_UserDomainAccess_Select_ForNameKey TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_UserDomainAccess_Select_ForNameKey TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_UserDomainAccess_Select_ForNameKey TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_UserDomainAccess_Select_ForNameKey TO [Dev - JNCC SQL]
END
GO