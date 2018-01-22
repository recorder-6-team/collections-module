/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_QETemplates_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_QETemplates_Select]
GO

/*===========================================================================*\
  Description:	Returns the list of quick entry templates available.

  Created:	October 2003

  Last revision information:
    $Revision: 4 $
    $Date: 20/02/04 16:10 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_QETemplates_Select]

 AS

SET NOCOUNT ON

SELECT DISTINCT
	QT.QE_Template_Key, 
	QT.Item_Name, 
	QT.Template_Type, 
	QT.Subject_Area_Key, 
	QT.[timestamp], 
	case when QE_Session_Key is null then 1 else 0 end as CanDelete 
FROM QE_Template QT 
LEFT JOIN QE_Session QS ON QS.QE_Template_Key=QT.QE_Template_Key
ORDER BY QT.Item_Name

GO 

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_QETemplates_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_QETemplates_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_QETemplates_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_QETemplates_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_QETemplates_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_QETemplates_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_QETemplates_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_QETemplates_Select TO [Dev - JNCC SQL]
END

GO