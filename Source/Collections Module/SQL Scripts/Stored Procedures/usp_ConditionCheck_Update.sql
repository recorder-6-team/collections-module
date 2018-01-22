/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConditionCheck_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConditionCheck_Update]
GO

/*===========================================================================*\
  Description:	Updates a record into the Conservation_Check table

  Parameters:	@Key
		@TypeConceptKey
		@RefNumber
		@VagueDateStart
		@VagueDateEnd
		@VagueDateType
		@CheckedByNameKey
		@ConditionConceptKey
		@AppliesToContainedSpecimens
		@Details
		@SessionID
		@Timestamp

  Created:	September 2003

  Last revision information:
    $Revision: 7 $
    $Date: 3/02/09 9:12 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConditionCheck_Update]
	@Key char(16),
	@TypeConceptKey char(16),
	@RefNumber varchar(20),
	@VagueDateStart int,
	@VagueDateEnd int,
	@VagueDateType varchar(2),
	@CheckedByNameKey char(16),
	@ConditionConceptKey char(16),
	@AppliesToContainedSpecimens bit,
	@Details text,
	@SessionID char(16),
	@Timestamp timestamp
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION
		
		UPDATE 	Conservation_Check
		SET 	Type_Concept_Key = @TypeConceptKey,
			Ref_Number = @RefNumber,
			Vague_Date_Start = @VagueDateStart,
			Vague_Date_End = @VagueDateEnd,
			Checked_By_Name_Key = @CheckedByNameKey,
			Condition_Concept_Key = @ConditionConceptKey,
			Applies_To_Contained_Specimens = @AppliesToContainedSpecimens,
			Details = @Details,
			Changed_Session_ID = @SessionID

		WHERE	Conservation_Check_Key = @Key
		AND		[Timestamp] = @Timestamp

		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Conservation_Check WHERE Conservation_Check_Key = @Key)
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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConditionCheck_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConditionCheck_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConditionCheck_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConditionCheck_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConditionCheck_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConditionCheck_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConditionCheck_Update TO [Dev - JNCC SQL]
END
GO