/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_QESessionCount_Get]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_QESessionCount_Get]
GO

/*===========================================================================*\
  Description:	Returns the count of quick entry sessions available.

  Parameters:	@Count	OUTPUT
							@UserDomainMask

  Created:	September 2003

  Last revision information:
    $Revision: 3 $
    $Date: 16/02/04 7:58 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_QESessionCount_Get]
	@Count int OUTPUT,
	@UserDomainMask int

AS

  SELECT @Count=COUNT(DISTINCT QE_Session_Key) 
  FROM QE_Session QS
	INNER JOIN QE_Template QT ON QT.QE_Template_Key=QS.QE_Template_Key
	INNER JOIN Domain D ON (D.Subject_Area_Key=QT.Subject_Area_Key
		AND D.Domain_Mask & @UserDomainMask > 0)
		OR QT.Subject_Area_Key IS NULL
	
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_QESessionCount_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_QESessionCount_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_QESessionCount_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_QESessionCount_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_QESessionCount_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_QESessionCount_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_QESessionCount_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_QESessionCount_Get TO [Dev - JNCC SQL]
END

GO