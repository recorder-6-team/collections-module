/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_CollectionUnitProcess_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_CollectionUnitProcess_Update]
GO

/*===========================================================================*\
  Description:	Updates a record in the Collection_Unit_Process table.

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
    $Revision: 5 $
    $Date: 2/02/09 17:29 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_CollectionUnitProcess_Update]
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
	@SessionID char(16),
	@Timestamp timestamp
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION

		UPDATE 	Collection_Unit_Process
		SET 	Collection_Unit_Key = @CollectionUnitKey,
			Process_Concept_Key = @ProcessConceptKey,
			Description = @Description,
			Name_Key = @NameKey,
			Vague_Date_Start = @VagueDateStart,
			Vague_Date_End = @VagueDateEnd,
			Vague_Date_Type = @VagueDateType,
			Inferred_Process = @InferredProcess,
			Inferred_Description = @InferredDescription,
			Inferred_Person = @InferredPerson,
			Inferred_Date = @InferredDate,
			Changed_Session_ID = @SessionID
		WHERE	Collection_Unit_Process_Key = @Key
		AND		[Timestamp] = @Timestamp

		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Collection_Unit_Process WHERE Collection_Unit_Process_Key = @Key)
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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_CollectionUnitProcess_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_CollectionUnitProcess_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitProcess_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_CollectionUnitProcess_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_CollectionUnitProcess_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_CollectionUnitProcess_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitProcess_Update TO [Dev - JNCC SQL]
END
GO