/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_TermVersions_Select_ForTerm]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_TermVersions_Select_ForTerm]
GO

/*===========================================================================*\
  Description:	Returns all of the Term Version records that use a given
		Term ket

  Parameters:	@Key	Term key

  Created:	February 2004

  Last revision information:
    $Revision: 2 $
    $Date: 2/04/04 14:04 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_TermVersions_Select_ForTerm]
	@Key char(16)
AS

SET NOCOUNT ON

	SELECT 
		Term_Version_Key AS Item_Key,
		IsNull(Version_Label + ' (' + Author_And_Date + ')', IsNull(Version_Label, Author_And_Date)) AS Item_Name
	FROM	Term_Version
	WHERE 	Term_Key = @Key

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TermVersions_Select_ForTerm') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_TermVersions_Select_ForTerm'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_TermVersions_Select_ForTerm TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_TermVersions_Select_ForTerm TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_TermVersions_Select_ForTerm TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_TermVersions_Select_ForTerm TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_TermVersions_Select_ForTerm TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_TermVersions_Select_ForTerm TO [Dev - JNCC SQL]
END

GO