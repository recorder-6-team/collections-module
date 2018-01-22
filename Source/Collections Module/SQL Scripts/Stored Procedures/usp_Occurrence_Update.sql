/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Occurrence_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Occurrence_Update]
GO

/*===========================================================================*\
  Description:	Updates an occurrence record.

  Parameters:	@Key
		@SurveyorsRef
		@RecordTypeKey
		@Comment
		@Confidential
		@Checked
		@CheckedBy
		@CheckedDate
		@TimeStamp
		@SessionID

  Created:	October 2003

  Last revision information:
    $Revision: 4 $
    $Date: 3/02/09 9:55 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Occurrence_Update]
	@Key char(16),
	@SurveyorsRef varchar(30),
	@RecordTypeKey char(16),
	@Comment text,
	@Confidential bit,
	@Checked bit,
	@CheckedBy char(16),
	@CheckedDate datetime,
	@TimeStamp timestamp,
	@SessionID char(16)
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION
	
		UPDATE	Occurrence
		SET	Surveyors_Ref = @SurveyorsRef,
			Record_Type_Concept_Key  = @RecordTypeKey,
			Comment = @Comment,
			Confidential = @Confidential,
			Checked = @Checked, 
			Checked_By = @CheckedBy,
			Checked_Date = @CheckedDate
		WHERE	Occurrence_Key = @Key
		AND		[Timestamp] = @Timestamp

		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Occurrence WHERE Occurrence_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Occurrence_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Occurrence_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Occurrence_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Occurrence_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Occurrence_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Occurrence_Update TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Occurrence_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Occurrence_Update TO [Dev - JNCC SQL]
END
GO