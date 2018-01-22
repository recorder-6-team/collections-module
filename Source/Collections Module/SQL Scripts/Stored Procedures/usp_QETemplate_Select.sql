/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_QETemplate_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_QETemplate_Select]
GO


/*===========================================================================*\
  Description:	Selects the details for a template

  Parameters:	@Key - QE_Template_Key

  Created:	Jan 2004

  Last revision information:
    $Revision: 1 $
    $Date: 28/01/04 9:32 $
    $Author: Johnvanbreda $

\*===========================================================================*/    
CREATE PROCEDURE [dbo].[usp_QETemplate_Select]
  @Key as Char(16)
 AS

SELECT DISTINCT
	QT.QE_Template_Key, 
	QT.Item_Name, 
	QT.Template_Type, 
	QT.Subject_Area_Key, 
	QT.[timestamp], 
	CASE WHEN QE_Session_Key IS NULL THEN 1 ELSE 0 END AS CanDelete 
FROM QE_Template QT 
LEFT JOIN QE_Session QS ON QS.QE_Template_Key=QT.QE_Template_Key
WHERE QT.QE_Template_Key=@Key

GO 

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_QETemplate_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_QETemplate_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_QETemplate_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_QETemplate_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_QETemplate_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_QETemplate_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_QETemplate_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_QETemplate_Select TO [Dev - JNCC SQL]
END

GO
