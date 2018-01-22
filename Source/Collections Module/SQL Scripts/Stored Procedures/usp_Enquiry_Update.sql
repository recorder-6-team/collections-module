/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Enquiry_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Enquiry_Update]
GO

/*===========================================================================*\
  Description:	Updates a record in the Enquiry table

  Parameters:	@Key
		@EnquirerNameKey 
		@VagueEnquirer
		@EnquiryTypeConceptKey
		@EnquiryMethodConceptKey
		@VagueDateStart
		@VagueDateEnd
		@VagueDateType
		@MaterialLeft
		@ObservationPlanned
		@Description
		@AnsweredByNameKey
		@Answered
		@AnsweredVagueDateStart
		@AnsweredVagueDateEnd
		@AnsweredVagueDateType
		@SessionID
		@Timestamp

  Created:	October 2003

  Last revision information:
    $Revision: 5 $
    $Date: 3/02/09 9:27 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Enquiry_Update]
	@Key char(16) OUTPUT,
	@EnquirerNameKey char(16),
	@VagueEnquirer varchar(100),
	@EnquiryTypeConceptKey char(16),
	@EnquiryMethodConceptKey char(16),
	@VagueDateStart int,
	@VagueDateEnd int,
	@VagueDateType varchar(2),
	@MaterialLeft bit,
	@ObservationPlanned bit,
	@Description text,
	@AnsweredByNameKey char(16),
	@Answered bit,
	@AnsweredVagueDateStart int,
	@AnsweredVagueDateEnd int,
	@AnsweredVagueDateType varchar(2),
	@SessionID char(16),
	@Timestamp timestamp
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION

		/*-------------------------------------------------------------*\
		  Update in Enquiry.
		\*-------------------------------------------------------------*/
		UPDATE 	Enquiry
		SET	Enquirer_Name_Key = @EnquirerNameKey, 
			Vague_Enquirer = @VagueEnquirer, 
			Enquiry_Type_Concept_Key = @EnquiryTypeConceptKey,
			Enquiry_Method_Concept_Key = @EnquiryMethodConceptKey, 
			Vague_Date_Start = @VagueDateStart, 
			Vague_Date_End = @VagueDateEnd, 
			Vague_Date_Type = @VagueDateType,
			Material_Left = @MaterialLeft,
			Observation_Planned = @ObservationPlanned, 
			Description = @Description, 
			Answered_By_Name_Key = @AnsweredByNameKey,
			Answered = @Answered, 
			Answered_Vague_Date_Start = @AnsweredVagueDateStart, 
			Answered_Vague_Date_End = @AnsweredVagueDateEnd, 
			Answered_Vague_Date_Type = @AnsweredVagueDateType,
			Entered_Session_ID = @SessionID
		WHERE	Enquiry_Key = @Key
		AND		[Timestamp] = @Timestamp

		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Enquiry WHERE Enquiry_Key = @Key)
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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Enquiry_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Enquiry_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Enquiry_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Enquiry_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Enquiry_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Enquiry_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Enquiry_Update TO [Dev - JNCC SQL]
END
GO