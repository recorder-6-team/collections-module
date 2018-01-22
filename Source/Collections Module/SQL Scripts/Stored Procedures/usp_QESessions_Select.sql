/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_QESessions_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_QESessions_Select]
GO

/*===========================================================================*\
  Description:	Returns the list of quick entry sessions available.

  Parameters:	@UserDomainMask

  Created:	October 2003

  Last revision information:
    $Revision: 3 $
    $Date: 12/11/03 16:39 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_QESessions_Select]
	@UserDomainMask int

 AS

SET NOCOUNT ON

SELECT DISTINCT
		QS.QE_Session_Key,
		QS.Item_Name
FROM QE_Session QS
		INNER JOIN QE_Template QT ON QT.QE_Template_Key=QS.QE_Template_Key
		INNER JOIN Domain D ON (D.Subject_Area_Key=QT.Subject_Area_Key
				AND D.Domain_Mask & @UserDomainMask > 0) OR 
				QT.Subject_Area_Key IS NULL
ORDER BY QS.Item_Name

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_QESessions_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_QESessions_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_QESessions_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_QESessions_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_QESessions_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_QESessions_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_QESessions_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_QESessions_Select TO [Dev - JNCC SQL]
END

GO
