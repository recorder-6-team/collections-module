If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_UserDomainAccess_Get]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_UserDomainAccess_Get]
GO

/*===========================================================================*\
  Description:	Returns all domains and security settings for a given user.

  Parameters:
	@UserID		User for which settings are to be returned.

  Created:	February 2004

  Last revision information:
    $Revision: 3 $
    $Date: 18/05/04 11:18 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_UserDomainAccess_Get] 
	@UserID CHAR(16)
AS

	DECLARE @AddDomainMask INT
	DECLARE @EditDomainMask INT
	
	SET NOCOUNT ON
	
	-- Use DISTINCT in the SUM(), in case some domains have the same mask!
	SELECT 		@AddDomainMask = SUM(DISTINCT D.Domain_Mask)
	FROM 		User_Domain_Access UDA
	INNER JOIN	Domain D ON UDA.Domain_Key = D.Domain_Key AND D.Has_Occurrences = 1
	WHERE 		UDA.Name_Key = @UserID
	AND 		UDA.Allow_Add = 1
	
	-- Use DISTINCT in the SUM(), in case some domains have the same mask!
	SELECT 		@EditDomainMask = SUM(DISTINCT D.Domain_Mask)
	FROM 		User_Domain_Access UDA
	INNER JOIN	Domain D ON UDA.Domain_Key = D.Domain_Key AND D.Has_Occurrences = 1
	WHERE 		UDA.Name_Key = @UserID
	AND 		UDA.Allow_Edit = 1
	
	SET @AddDomainMask = ISNULL(@AddDomainMask, 0)
	SET @EditDomainMask = ISNULL(@EditDomainMask, 0)
	
	--Return results
	SELECT @AddDomainMask AS Add_Domain_Mask, @EditDomainMask AS Edit_Domain_Mask
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_UserDomainAccess_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_UserDomainAccess_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_UserDomainAccess_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_UserDomainAccess_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_UserDomainAccess_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_UserDomainAccess_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_UserDomainAccess_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_UserDomainAccess_Get TO [Dev - JNCC SQL]
END
GO