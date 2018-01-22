/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_QESessionFormCount_Get]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_QESessionFormCount_Get]
GO

/*===========================================================================*\
  Description:	Returns the count of quick entry sessions, for a given template,
				available.

  Parameters:	@Count	OUTPUT
				@UserDomainMask

  Created:	March 2014

  Last revision information:
    $Revision: 1 $
    $Date: 18/03/14 13:53 $
    $Author: Christopherknight $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_QESessionFormCount_Get]
	@Count int OUTPUT,
    @QE_Template_Key char(16),
	@UserDomainMask int

AS

  SELECT @Count=COUNT(DISTINCT QE_Session_Key) 
  FROM QE_Session QS
	INNER JOIN QE_Template QT ON QT.QE_Template_Key=QS.QE_Template_Key
	INNER JOIN Domain D ON (D.Subject_Area_Key=QT.Subject_Area_Key
		AND D.Domain_Mask & @UserDomainMask > 0)
		OR QT.Subject_Area_Key IS NULL
  WHERE QT.QE_Template_Key = @QE_Template_Key
	
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_QESessionFormCount_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_QESessionFormCount_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        GRANT EXECUTE ON dbo.usp_QESessionFormCount_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_QESessionFormCount_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_QESessionFormCount_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_QESessionFormCount_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_QESessionFormCount_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_QESessionFormCount_Get TO [Dev - JNCC SQL]
END

GO