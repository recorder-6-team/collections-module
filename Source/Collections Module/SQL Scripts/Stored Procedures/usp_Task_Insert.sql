/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Task_Insert]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Task_Insert]
GO

/*===========================================================================*\
  Description:	Inserts a record into the Conservation_Task table

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
    $Revision: 6 $
    $Date: 22/03/04 9:48 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Task_Insert]
	@Key char(16) OUTPUT,
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
	@SessionID char(16)
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	EXECUTE spNextKey 'Conservation_Task', @Key OUTPUT

	BEGIN TRANSACTION

		/*-------------------------------------------------------------*\
		  Insert in Conservation_Task.
		\*-------------------------------------------------------------*/
		INSERT INTO Conservation_Task (
			Conservation_Task_Key, Conservation_Check_Key, Conservation_Job_Key,
			Set_Vague_Date_Start, Set_Vague_Date_End, Set_Vague_Date_Type,
			Status, Type_Concept_Key, Priority, Duration, Duration_Unit_Concept_Key,
			Identifier_Name_Key, Task_Action, Comment, Entered_Session_ID
			
		) VALUES (
			@Key, @ConservationCheckKey, @ConservationJobKey,
			@SetVagueDateStart, @SetVagueDateEnd, @SetVagueDateType,
			@Status, @TypeConceptKey, @Priority, @Duration, @DurationUnitConceptKey,
			@IdentifierNameKey, @TaskAction, @Comment, @SessionID
		)
		IF @@Error <> 0 GOTO RollbackAndExit

		/*---------------------------------------------------------------------------------*\
		  If the Conservation Check has just one task linked to it, we will want to duplicate 
		  records from the Collection_Unit_Check table into the Collection_Unit_Table.
		  Here we are going to find out how many Tasks are linked to the Conservation_Check.
		  If it is just one, then we have stored this task key in @TaskKey and can run
		  usp_CollectionUnitTask_Insert_ForTaskLinkCondCheck which will duplicate the 
		  records for us.
		\*---------------------------------------------------------------------------------*/

		DECLARE @TaskKey char(16)

		SELECT 	@TaskKey = Conservation_Task_Key
		FROM	Conservation_Task
		WHERE	Conservation_Check_Key = @ConservationCheckKey

		IF @@RowCount = 1
		BEGIN
			EXEC usp_CollectionUnitTask_Insert_ForTaskLinkCondCheck @TaskKey, @ConservationCheckKey, @SessionID
		END


	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Task_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Task_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Task_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Task_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Task_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Task_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Task_Insert TO [Dev - JNCC SQL]
END
GO