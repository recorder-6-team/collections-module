/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConditionCheck_Insert]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConditionCheck_Insert]
GO

/*===========================================================================*\
  Description:	Inserts a record into the Conservation_Check table

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

  Created:	September 2003

  Last revision information:
    $Revision: 4 $
    $Date: 8/12/03 10:38 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConditionCheck_Insert]
	@Key char(16) OUTPUT,
	@TypeConceptKey char(16),
	@RefNumber varchar(20),
	@VagueDateStart int,
	@VagueDateEnd int,
	@VagueDateType varchar(2),
	@CheckedByNameKey char(16),
	@ConditionConceptKey char(16),
	@AppliesToContainedSpecimens bit,
	@Details text,
	@SessionID char(16)
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	EXECUTE spNextKey 'Conservation_Check', @Key OUTPUT

	BEGIN TRANSACTION

		/*-------------------------------------------------------------*\
		  Insert in Conservation_Check.
		\*-------------------------------------------------------------*/
		INSERT INTO Conservation_Check (
			Conservation_Check_Key, Type_Concept_Key, Ref_Number, Vague_Date_Start,
			Vague_Date_End, Vague_Date_Type, Checked_By_Name_Key, 
			Condition_Concept_Key, Applies_To_Contained_Specimens, Details,
			Entered_Session_ID
		) VALUES (
			@Key, @TypeConceptKey, @RefNumber, @VagueDateStart, @VagueDateEnd,
			@VagueDateType, @CheckedByNameKey, @ConditionConceptKey, 
			@AppliesToContainedSpecimens, @Details, @SessionID
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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConditionCheck_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConditionCheck_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConditionCheck_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConditionCheck_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConditionCheck_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConditionCheck_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConditionCheck_Insert TO [Dev - JNCC SQL]
END
GO