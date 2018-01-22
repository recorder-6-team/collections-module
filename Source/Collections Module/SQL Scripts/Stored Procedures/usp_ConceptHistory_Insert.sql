/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptHistory_Insert]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptHistory_Insert]
GO

/*===========================================================================*\
  Description:	Inserts a record into the Concept_History table

  Parameters:	@Key
		@ConservationCheckKey 
		@ConservationJobKey 
		@SetVagueDateStart
		@SetVagueDateEnd
		@SetVagueDateType
		@Status
		@TypeConceptKey
		@Priority
		@Duration
		@DurationUnitConceptKey
		@IdentifierNameKey
		@TaskAction 
		@Comment
		@SessionID
	

  Created:	September 2003

  Last revision information:
    $Revision: 4 $
    $Date: 1/04/04 11:21 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptHistory_Insert]
	@Key char(16) OUTPUT,
	@ConceptKey char(16),
	@ConceptGroupVersionFromKey char(16) = NULL,
	@ConceptGroupVersionToKey char(16) = NULL,
	@FromVagueDateStart int = NULL,
	@FromVagueDateEnd int = NULL,
	@FromVagueDateType varchar(2) = NULL,
	@ToVagueDateStart int = NULL,
	@ToVagueDateEnd int = NULL,
	@ToVagueDateType varchar(2) = NULL,
	@SessionID char(16),
	@SystemSuppliedData bit = NULL
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	EXECUTE spNextKey 'Concept_History', @Key OUTPUT

	BEGIN TRANSACTION

		/*-------------------------------------------------------------*\
		  Insert in Concept_History.
		\*-------------------------------------------------------------*/
		INSERT INTO Concept_History (
			Concept_History_Key,
			Concept_Key,
			Concept_Group_Version_From,
			Concept_Group_Version_To,
			From_Vague_Date_Start,
			From_Vague_Date_End,
			From_Vague_Date_Type,
			To_Vague_Date_Start,
			To_Vague_Date_End,
			To_Vague_Date_Type,
			Entered_Session_ID,
			System_Supplied_Data
		) VALUES (
			@Key, 
			@ConceptKey,
			@ConceptGroupVersionFromKey,
			@ConceptGroupVersionToKey,
			@FromVagueDateStart,
			@FromVagueDateEnd,
			@FromVagueDateType,
			@ToVagueDateStart,
			@ToVagueDateEnd,
			@ToVagueDateType,
			@SessionID,
			IsNull(@SystemSuppliedData, 0)
		)	
		IF @@Error <> 0 GOTO RollbackAndExit

	--Ensure Concept's current status is set correctly.
		EXEC usp_Concept_UpdateIsCurrent @ConceptKey
		IF @@Error <> 0 GOTO RollbackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptHistory_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptHistory_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptHistory_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptHistory_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptHistory_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptHistory_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptHistory_Insert TO [Dev - JNCC SQL]
END
GO