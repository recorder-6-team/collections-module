If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_QESession_Update]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_QESession_Update]
GO
    
/*===========================================================================*\
  Description:	

  Parameters:	

  Created:	August 2003

  Last revision information:
    $Revision: 7 $
    $Date: 5/07/16 9:28 $
    $Author: Christopherknight $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_QESession_Update]
  @QESessionKey as int,
  @QESessionName as varchar(50),
  @Timestamp as timestamp,
  @SessionID as char(16)
AS

SET NOCOUNT OFF

Update QE_Session
	set Item_Name = @QESessionName,
	Changed_Session_ID = @SessionID
	where 
	QE_Session_Key = @QESessionKey 
	AND ([Timestamp] = @Timestamp
		OR Changed_Session_ID = @SessionID
		OR (Changed_Session_ID IS NULL
			AND Entered_Session_ID = @SessionID))

	IF @@Rowcount = 0 AND EXISTS(SELECT 1 FROM QE_Session WHERE QE_Session_Key = @QESessionKey)
		RAISERROR('Record updated by another user', 16, 1)

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_QESession_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_QESession_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_QESession_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_QESession_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_QESession_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_QESession_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_QESession_Update TO [Dev - JNCC SQL]
END

GO