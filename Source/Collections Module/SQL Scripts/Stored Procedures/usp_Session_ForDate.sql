/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_Session_ForDate]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_Session_ForDate]
GO

/*===========================================================================*\
  Description:	Obtain a session identifier for specified user and date.

				Used when converting ENTERED_BY, ENTRY_DATE etc. from
				dictionaries to avoid generating many duplicate sessions.

  Parameters:	@UserKey				User key
				@Date					Date
				@SessionID				Session identifier (on exit)

  Created:		Nov 2003

  Last revision information:
	$Revision: 6 $
	$Date: 12/05/04 9:57 $
	$Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Session_ForDate]
	@UserKey			CHAR(16),
	@Date				DATETIME,
	@SessionID			CHAR(16)	OUTPUT
AS
	SET NOCOUNT ON

	/* remove any time component */
	SELECT		@Date				=	CONVERT(VARCHAR, @Date, 113)

	SELECT		@SessionID			=	Session_ID
	FROM		Session
	WHERE		User_Name_Key		=	@UserKey
	AND			Date_Time_Start		=	@Date
	AND			Date_Time_End		=	@Date

	IF @@ROWCOUNT = 0
	BEGIN
		EXECUTE		spNextKey	'Session',
								@SessionID	OUTPUT
		IF @@ERROR <> 0 GOTO fail

		INSERT		Session
					(Session_ID,
					User_Name_Key,
					Date_Time_Start,
					Date_Time_End)
		VALUES		(@SessionID,
					@UserKey,
					@Date,
					@Date)

		IF @@ERROR <> 0 GOTO fail
	END

	RETURN

fail:
	RAISERROR ('usp_Session_ForDate failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Session_ForDate') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Session_ForDate'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Session_ForDate TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Session_ForDate TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Session_ForDate TO [Dev - JNCC SQL]
END
GO
