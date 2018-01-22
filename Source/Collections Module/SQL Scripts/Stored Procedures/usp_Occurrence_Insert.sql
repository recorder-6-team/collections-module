/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Occurrence_Insert]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Occurrence_Insert]
GO

/*===========================================================================*\
  Description:	Insert a new record in Occurrence table.

  Parameters:	@Key 		OUTPUT
		@SampleKey
		@SurveyorsRef
		@RecordTypeKey
		@Comment
		@Confidential
		@Checked
		@CheckedBy
		@CheckedDate
		@SessionID

  Created:	July 2003

  Last revision information:
    $Revision: 4 $
    $Date: 21/01/04 11:58 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Occurrence_Insert]
	@Key char(16) OUTPUT,
	@SampleKey char(16),
	@SurveyorsRef varchar(30) = NULL,
	@RecordTypeKey char(16) = NULL,
	@Comment text = NULL,
	@Confidential bit = NULL,
	@Checked bit = NULL,
	@CheckedBy char(16) = NULL,
	@CheckedDate datetime = NULL,
	@SessionID char(16)
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF
	
	EXECUTE spNextKey 'Occurrence', @Key OUTPUT
	
	BEGIN TRANSACTION
	
		INSERT INTO Occurrence (
			Occurrence_Key, Sample_Key, Surveyors_Ref, Record_Type_Concept_Key, Comment, Confidential, 
			Checked, Checked_By, Checked_Date, Entered_Session_ID
		) VALUES (
			@Key, @SampleKey, @SurveyorsRef, @RecordTypeKey, @Comment, IsNull(@Confidential, 0), 
			IsNull(@Checked, 0), @CheckedBy, @CheckedDate, @SessionID
		)
		IF @@Error <> 0 GOTO RollbackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Occurrence_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Occurrence_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Occurrence_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Occurrence_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Occurrence_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Occurrence_Insert TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Occurrence_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Occurrence_Insert TO [Dev - JNCC SQL]
END
GO