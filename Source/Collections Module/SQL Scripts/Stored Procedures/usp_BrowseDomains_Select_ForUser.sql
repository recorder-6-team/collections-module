/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_BrowseDomains_Select_ForUser]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_BrowseDomains_Select_ForUser]
GO

/*===========================================================================*\
  Description:	Returns a list of domains the specified user is allowed to 
		browse.

  Parameters:	@NameKey

  Created:	December 2003

  Last revision information:
    $Revision: 2 $
    $Date: 24/02/04 15:07 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_BrowseDomains_Select_ForUser]
	@NameKey char(16)
AS
SET NOCOUNT ON

	SELECT	D.Item_Name,
		D.Domain_Mask
	FROM	Domain D
	JOIN	User_Domain_Access UDA ON UDA.Domain_Key = D.Domain_Key AND UDA.Allow_Browse = 1
	WHERE	UDA.Name_Key = @NameKey
	AND	D.Has_Occurrences = 1
	AND	D.Domain_Mask IS NOT NULL

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_BrowseDomains_Select_ForUser') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_BrowseDomains_Select_ForUser'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_BrowseDomains_Select_ForUser TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_BrowseDomains_Select_ForUser TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_BrowseDomains_Select_ForUser TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_BrowseDomains_Select_ForUser TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_BrowseDomains_Select_ForUser TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_BrowseDomains_Select_ForUser TO [Dev - JNCC SQL]
END
GO
