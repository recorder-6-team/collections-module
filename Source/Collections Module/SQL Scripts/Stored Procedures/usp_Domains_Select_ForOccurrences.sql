/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Domains_Select_ForOccurrences') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_Domains_Select_ForOccurrences]
GO

/*===========================================================================*\
  Description:	Returns a list of domain names and keys.

  Parameters:	
	@NameKey	Currently logged in user.

  Created:	October 2003

  Last revision information:
    $Revision: 4 $
    $Date: 24/06/05 13:55 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Domains_Select_ForOccurrences]
	@NameKey CHAR(16),
	@UserDomainMask INT
AS

SET NOCOUNT ON

	SELECT 	D.Domain_Key, D.Item_Name
	FROM	Domain D
	JOIN	User_Domain_Access UDA ON UDA.Domain_Key = D.Domain_Key AND UDA.Allow_Add = 1
	WHERE	D.Has_Occurrences = 1
	AND	UDA.Name_Key = @NameKey
	AND 	D.Domain_Mask & @UserDomainMask > 0
	ORDER BY D.Item_Name

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Domains_Select_ForOccurrences') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Domains_Select_ForOccurrences'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Domains_Select_ForOccurrences TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Domains_Select_ForOccurrences TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Domains_Select_ForOccurrences TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Domains_Select_ForOccurrences TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Domains_Select_ForOccurrences TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Domains_Select_ForOccurrences TO [Dev - JNCC SQL]
END
GO