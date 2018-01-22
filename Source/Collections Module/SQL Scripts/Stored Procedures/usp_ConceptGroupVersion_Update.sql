/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptGroupVersion_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptGroupVersion_Update]
GO

/*===========================================================================*\
  Description:	Updates a record in the Concept_Group_Version table.

  Parameters:	@Key (output),	
		@ConceptGroupKey 
		@Version 
		@FromVagueDateStart (optional)
		@FromVagueDateEnd (optional)
		@FromVagueDateType (optional)
		@ToVagueDateStart (optional)
		@ToVagueDateEnd (optional)
		@ToVagueDateType (optional)
		@AcqVagueDateStart (optional)
		@AcqVagueDateEnd (optional)
		@AcqVagueDateType (optional)
		@URL (optional)
		@SessionID
		@SystemSuppliedData bit (optional),
		@Timestamp

  Created:	November 2003

  Last revision information:
    $Revision: 5 $
    $Date: 2/02/09 17:46 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptGroupVersion_Update]
	@Key char(16) OUTPUT,	
	@ConceptGroupKey char(16),
	@Version varchar(100),
	@FromVagueDateStart int = NULL,
	@FromVagueDateEnd int = NULL,
	@FromVagueDateType varchar(2) = NULL,
	@ToVagueDateStart int = NULL,
	@ToVagueDateEnd int = NULL,
	@ToVagueDateType varchar(2) = NULL,	
	@AcqVagueDateStart int = NULL,
	@AcqVagueDateEnd int = NULL,
	@AcqVagueDateType varchar(2) = NULL,
	@URL varchar(255) = NULL,
	@SessionID char(16),
	@Timestamp timestamp	
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	DECLARE @SequenceNumber int

	BEGIN TRANSACTION

		UPDATE 	Concept_Group_Version
		SET 	Concept_Group_Key = @ConceptGroupKey, 
			Version = @Version, 
			From_Vague_Date_Start = @FromVagueDateStart, 
			From_Vague_Date_End = @FromVagueDateEnd, 
			From_Vague_Date_Type = IsNull(@FromVagueDateType, 'U'), 
			To_Vague_Date_Start = @ToVagueDateStart, 
			To_Vague_Date_End = @ToVagueDateEnd, 
			To_Vague_Date_Type = @ToVagueDateType, 
			Acq_Vague_Date_Start = @AcqVagueDateStart, 
			Acq_Vague_Date_End = @AcqVagueDateEnd, 
			Acq_Vague_Date_Type = IsNull(@AcqVagueDateType, 'U'),
			URL = @URL, 
			Changed_Session_ID = @SessionID

		WHERE	Concept_Group_Version_Key = @Key
		AND		[Timestamp] = @Timestamp

		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Concept_Group_Version WHERE Concept_Group_Version_Key = @Key)
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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptGroupVersion_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptGroupVersion_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptGroupVersion_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptGroupVersion_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptGroupVersion_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptGroupVersion_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptGroupVersion_Update TO [Dev - JNCC SQL]
END
GO