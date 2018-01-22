/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_CollectionUnitProcess_Insert]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_CollectionUnitProcess_Insert]
GO

/*===========================================================================*\
  Description:	Inserts a record into the Collection_Unit_Process table.

  Parameters:	@Key 
		@CollectionUnitKey 
		@ProcessConceptKey 
		@Description
		@NameKey
		@VagueDateStart
		@VagueDateEnd
		@VagueDateType
		@InferredProcess
		@InferredDescription
		@InferredPerson
		@InferredDate 
		@SessionID

  Created:	October 2003

  Last revision information:
    $Revision: 3 $
    $Date: 5/12/03 17:25 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_CollectionUnitProcess_Insert]
	@Key char(16) OUTPUT,
	@CollectionUnitKey char(16),
	@ProcessConceptKey char(16),
	@Description text,
	@NameKey char(16),
	@VagueDateStart int,
	@VagueDateEnd int,
	@VagueDateType varchar(2),
	@InferredProcess tinyint,
	@InferredDescription tinyint,
	@InferredPerson tinyint,
	@InferredDate tinyint,
	@SessionID char(16)
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	EXECUTE spNextKey 'Collection_Unit_Process', @Key OUTPUT

	BEGIN TRANSACTION

		INSERT INTO Collection_Unit_Process (
			Collection_Unit_Process_Key, Collection_Unit_Key, Process_Concept_Key,
			Description, Name_Key, Vague_Date_Start, Vague_Date_End, 
			Vague_Date_Type, Inferred_Process, Inferred_Description,
			Inferred_Person, Inferred_Date, Entered_Session_ID
		) VALUES (
			@Key, @CollectionUnitKey, @ProcessConceptKey,
			@Description, @NameKey, @VagueDateStart, @VagueDateEnd, 
			@VagueDateType, @InferredProcess, @InferredDescription,
			@InferredPerson, @InferredDate, @SessionID
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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_CollectionUnitProcess_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_CollectionUnitProcess_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitProcess_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_CollectionUnitProcess_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_CollectionUnitProcess_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_CollectionUnitProcess_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitProcess_Insert TO [Dev - JNCC SQL]
END
GO