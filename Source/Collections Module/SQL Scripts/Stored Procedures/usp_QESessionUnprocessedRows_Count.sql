/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_QESessionUnprocessedRows_Count]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_QESessionUnprocessedRows_Count]
GO

/*===========================================================================*\
  Description:	Returns the count of unprocessed rows in a session

  Parameters:	@Key 		QE_Session_Key
							@Count	Output

  Created:	October 2003

  Last revision information:
    $Revision: 2 $
    $Date: 12/11/03 16:39 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_QESessionUnprocessedRows_Count]
	@Key CHAR(16),
	@Count INT OUTPUT

 AS

SET NOCOUNT ON

SELECT @Count=COUNT(*)
FROM QE_Data_Row
WHERE QE_Session_Key=@Key

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_QESessionUnprocessedRows_Count') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_QESessionUnprocessedRows_Count'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_QESessionUnprocessedRows_Count TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_QESessionUnprocessedRows_Count TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_QESessionUnprocessedRows_Count TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_QESessionUnprocessedRows_Count TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_QESessionUnprocessedRows_Count TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_QESessionUnprocessedRows_Count TO [Dev - JNCC SQL]
END

GO
