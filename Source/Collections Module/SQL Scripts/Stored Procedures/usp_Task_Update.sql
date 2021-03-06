/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Task_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Task_Update]
GO

/*===========================================================================*\
  Description:	Updates a record into the Conservation_Task table

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
		@Timestamp 

  Created:	September 2003

  Last revision information:
    $Revision: 7 $
    $Date: 3/02/09 10:43 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Task_Update]
	@Key char(16),
	@ConservationCheckKey char(16), 
	@ConservationJobKey char(16),
	@SetVagueDateStart int, 
	@SetVagueDateEnd int, 
	@SetVagueDateType varchar(2),
	@Status tinyint, 
	@TypeConceptKey char(16), 
	@Priority tinyint, 
	@Duration float, 
	@DurationUnitConceptKey char(16),
	@IdentifierNameKey char(16), 
	@TaskAction text, 
	@Comment text, 
	@SessionID char(16), 
	@Timestamp timestamp

AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION
		
		UPDATE 	Conservation_Task
		SET 	Conservation_Check_Key = @ConservationCheckKey, 
			Conservation_Job_Key = @ConservationJobKey,
			Set_Vague_Date_Start = @SetVagueDateStart, 
			Set_Vague_Date_End = @SetVagueDateEnd, 
			Set_Vague_Date_Type = @SetVagueDateType,
			Status = @Status, 
			Type_Concept_Key = @TypeConceptKey, 
			Priority = @Priority, 
			Duration = @Duration, 
			Duration_Unit_Concept_Key = @DurationUnitConceptKey,
			Identifier_Name_Key = @IdentifierNameKey, 
			Task_Action = @TaskAction, 
			Comment = @Comment, 
			Changed_Session_ID = @SessionID

		WHERE	Conservation_Task_Key = @Key
		AND		[Timestamp] = @Timestamp

		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Conservation_Task WHERE Conservation_Task_Key = @Key)
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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Task_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Task_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Task_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Task_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Task_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Task_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Task_Update TO [Dev - JNCC SQL]
END
GO