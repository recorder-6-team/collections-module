/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Enquiry_Insert]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Enquiry_Insert]
GO

/*===========================================================================*\
  Description:	Inserts a record into the Enquiry table

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

  Created:	October 2003

  Last revision information:
    $Revision: 3 $
    $Date: 8/12/03 10:48 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Enquiry_Insert]
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
	@SessionID char(16)
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	EXECUTE spNextKey 'Enquiry', @Key OUTPUT

	BEGIN TRANSACTION

		/*-------------------------------------------------------------*\
		  Insert in Enquiry.
		\*-------------------------------------------------------------*/
		INSERT INTO Enquiry (
			Enquiry_Key, Enquirer_Name_Key, Vague_Enquirer, Enquiry_Type_Concept_Key,
			Enquiry_Method_Concept_Key, Vague_Date_Start, Vague_Date_End, Vague_Date_Type,
			Material_Left, Observation_Planned, Description, Answered_By_Name_Key,
			Answered, Answered_Vague_Date_Start, Answered_Vague_Date_End, Answered_Vague_Date_Type,
			Entered_Session_ID
		) VALUES (
			@Key, @EnquirerNameKey, @VagueEnquirer, @EnquiryTypeConceptKey,
			@EnquiryMethodConceptKey, @VagueDateStart, @VagueDateEnd, @VagueDateType,
			@MaterialLeft, @ObservationPlanned, @Description, @AnsweredByNameKey,
			@Answered, @AnsweredVagueDateStart, @AnsweredVagueDateEnd, @AnsweredVagueDateType,
			@SessionID
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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Enquiry_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Enquiry_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Enquiry_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Enquiry_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Enquiry_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Enquiry_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Enquiry_Insert TO [Dev - JNCC SQL]
END
GO