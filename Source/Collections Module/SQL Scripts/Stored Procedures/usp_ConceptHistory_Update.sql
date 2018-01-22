/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptHistory_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptHistory_Update]
GO

/*===========================================================================*\
  Description:	Updates a record in the Concept_History table

  Parameters:	@Key
		@ConceptKey 
		@ConceptGroupVersionFromKey
		@ConceptGroupVersionToKey 
		@FromVagueDateStart
		@FromVagueDateEnd 
		@FromVagueDateType
		@ToVagueDateStart 
		@ToVagueDateEnd
		@ToVagueDateType
		@SessionID 
		@Timestamp 

  Created:	December 2003

  Last revision information:
    $Revision: 4 $
    $Date: 2/02/09 17:55 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptHistory_Update]
	@Key char(16),
	@ConceptKey char(16),
	@ConceptGroupVersionFromKey char(16),
	@ConceptGroupVersionToKey char(16),
	@FromVagueDateStart int,
	@FromVagueDateEnd int,
	@FromVagueDateType varchar(2),
	@ToVagueDateStart int,
	@ToVagueDateEnd int,
	@ToVagueDateType varchar(2),
	@SessionID char(16),
	@Timestamp timestamp
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION
		
		UPDATE 	Concept_History
		SET 	Concept_Key = @ConceptKey,
				Concept_Group_Version_From = @ConceptGroupVersionFromKey,
				Concept_Group_Version_To = @ConceptGroupVersionToKey,
				From_Vague_Date_Start = @FromVagueDateStart,
				From_Vague_Date_End = @FromVagueDateEnd,
				From_Vague_Date_Type = @FromVagueDateType,
				To_Vague_Date_Start = @ToVagueDateStart,
				To_Vague_Date_End = @ToVagueDateEnd,
				To_Vague_Date_Type = @ToVagueDateType,
				Changed_Session_ID = @SessionID
		WHERE	Concept_History_Key = @Key
		AND		[Timestamp] = @Timestamp

		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Concept_History WHERE Concept_History_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptHistory_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptHistory_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptHistory_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptHistory_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptHistory_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptHistory_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptHistory_Update TO [Dev - JNCC SQL]
END
GO