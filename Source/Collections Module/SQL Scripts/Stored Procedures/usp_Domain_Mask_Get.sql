If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Domain_Mask_Get]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Domain_Mask_Get]
GO

/*===========================================================================*\
  Description:	Returns a given user's domain mask

  Parameters:
	@Name_Key	Name_Key of user to calculate domain mask for
	@UserDomainMask	User's Domain Mask. Returns 0 if User has no domains assigned.

  Created:	January 2004

  Last revision information:
    $Revision: 3 $
    $Date: 18/05/04 11:18 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Domain_Mask_Get] 
	@Name_Key CHAR(16),
	@UserDomainMask INT OUTPUT
AS

	SET NOCOUNT ON
	-- Use DISTINCT in the SUM(), in case some domains have the same mask!
	SELECT 		@UserDomainMask = SUM(DISTINCT D.Domain_Mask)
	FROM 		User_Domain_Access UDA
	INNER JOIN	Domain D ON UDA.Domain_Key = D.Domain_Key AND D.Has_Occurrences = 1
	WHERE 		UDA.Allow_Browse = 1
	AND 		UDA.Name_Key = @Name_Key
	
	--Return a domain mask of 0 (no domains) if no records exist
	SET @UserDomainMask = ISNULL(@UserDomainMask, 0)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Domain_Mask_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Domain_Mask_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Domain_Mask_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Domain_Mask_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Domain_Mask_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Domain_Mask_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Domain_Mask_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Domain_Mask_Get TO [Dev - JNCC SQL]
END
GO