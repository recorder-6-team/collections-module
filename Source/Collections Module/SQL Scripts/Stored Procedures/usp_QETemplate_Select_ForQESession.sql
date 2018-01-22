If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_QETemplate_Select_ForQESession]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_QETemplate_Select_ForQESession]
GO
    
/*===========================================================================*\
  Description:	

  Parameters:	

  Created:	August 2003

  Last revision information:
    $Revision: 3 $
    $Date: 20/02/04 15:21 $
    $Author: Ericsalmon $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_QETemplate_Select_ForQESession]
  @QESessionKey as int
 AS

SET NOCOUNT ON
Select QE_Template.QE_Template_Key, QE_Template.Item_Name as Template_Name, Template_Type,
	QE_Session.Item_Name as Session_Name 
	from QE_Template inner join
	QE_Session on QE_Template.QE_Template_Key
	= QE_Session.QE_Template_Key
	where QE_Session_Key = @QESessionKey
GO 

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_QETemplate_Select_ForQESession') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_QETemplate_Select_ForQESession'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_QETemplate_Select_ForQESession TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_QETemplate_Select_ForQESession TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_QETemplate_Select_ForQESession TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_QETemplate_Select_ForQESession TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_QETemplate_Select_ForQESession TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_QETemplate_Select_ForQESession TO [Dev - JNCC SQL]
END

GO



