/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_SurveyEventRecorder_Insert]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_SurveyEventRecorder_Insert]
GO
/*===========================================================================*\
  Description:	

  Parameters:	

  Created:	July 2003

  Last revision information:
    $Revision: 3 $
    $Date: 8/12/03 11:52 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SurveyEventRecorder_Insert]
	@Key char(16) OUTPUT,
	@NameKey char(16),
	@SurveyEventKey char(16),
	@RecorderRoleKey char(16),
	@EnteredBy char(16)
	
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	--find out whether this survey Event recorder alreadly exists
	SET @Key = NULL
	SELECT	@Key = SE_Recorder_Key 
	FROM 	Survey_Event_Recorder
	WHERE	Name_Key = @NameKey
	AND	Survey_Event_Key = @SurveyEventKey
	
	IF @Key IS NULL
	BEGIN
		BEGIN TRANSACTION
			EXECUTE spNextKey 'Survey_Event_Recorder', @Key OUTPUT

			INSERT INTO Survey_Event_Recorder (
				SE_Recorder_Key,
				Name_Key,
				Survey_Event_Key,
				Recorder_Role_Key,
				Entered_By
			) VALUES (
				@Key,
				@NameKey,
				@SurveyEventKey,
				@RecorderRoleKey,
				@EnteredBy
			)

			IF @@Error <> 0 GOTO RollBackAndExit

		COMMIT TRANSACTION
	END

	RETURN 0

RollBackAndExit:
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SurveyEventRecorder_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SurveyEventRecorder_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SurveyEventRecorder_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SurveyEventRecorder_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SurveyEventRecorder_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SurveyEventRecorder_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SurveyEventRecorder_Insert TO [Dev - JNCC SQL]
END
GO