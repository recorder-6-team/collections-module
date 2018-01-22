/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_QESession_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_QESession_Select]
GO
    

/*===========================================================================*\
  Description:	Returns details of a single quick entry session

  Parameters:	
		@QESessionKey - @QE_SEssion_Key

  Created:	August 2003

  Last revision information:
    $Revision: 3 $
    $Date: 20/02/04 16:10 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_QESession_Select]
  @QESessionKey as int
 AS

SET NOCOUNT ON
SELECT 
		QT.QE_Template_Key, 
		QT.Item_Name as Template_Name, 
		QT.Template_Type, 
		QT.Subject_Area_Key,
		QS.Item_Name AS Session_Name,
		QS.Timestamp
FROM QE_Template QT 
INNER JOIN QE_Session QS ON QT.QE_Template_Key = QS.QE_Template_Key
WHERE QS.QE_Session_Key = @QESessionKey
	
GO 

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_QESession_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_QESession_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_QESession_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_QESession_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_QESession_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_QESession_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_QESession_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_QESession_Select TO [Dev - JNCC SQL]
END

GO


