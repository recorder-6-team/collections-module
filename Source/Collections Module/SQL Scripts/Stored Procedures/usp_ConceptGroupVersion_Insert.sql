/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptGroupVersion_Insert]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptGroupVersion_Insert]
GO

/*===========================================================================*\
  Description:	Adds a record to the ConceptGroupVersion table.

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
		@SystemSuppliedData bit (optional)

  Created:	November 2003

  Last revision information:
    $Revision: 6 $
    $Date: 1/04/04 11:08 $
    $Author: Johnvanbreda $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_ConceptGroupVersion_Insert]
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
	@SystemSuppliedData bit = NULL
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	DECLARE @SequenceNumber int

	BEGIN TRANSACTION

		/*-------------------------------------------------------------*\
		  Calculate a new sequence number for the concept group version
		  record that is to be added.
		\*-------------------------------------------------------------*/		
		SELECT 	@SequenceNumber = Max([Sequence]) + 1
		FROM	Concept_Group_Version
		WHERE	Concept_Group_Key = @ConceptGroupKey

		-- If Null is returned, the sequence number should be 1
		SET @SequenceNumber = IsNull(@SequenceNumber, 1)

		/*-------------------------------------------------------------*\
		  Insert in Concept_Group_Version.
		\*-------------------------------------------------------------*/
		EXECUTE spNextKey 'Concept_Group_Version', @Key OUTPUT		

		INSERT INTO Concept_Group_Version (
			Concept_Group_Version_Key, 
			Concept_Group_Key, 
			Version,
			[Sequence], 
			From_Vague_Date_Start,
			From_Vague_Date_End,
			From_Vague_Date_Type,
			To_Vague_Date_Start,
			To_Vague_Date_End,
			To_Vague_Date_Type,	
			Acq_Vague_Date_Start,
			Acq_Vague_Date_End,
			Acq_Vague_Date_Type,	
			URL,
			Entered_Session_ID,
			System_Supplied_Data
		) VALUES (
			@Key,
			@ConceptGroupKey,
			@Version,
			@SequenceNumber,
			@FromVagueDateStart,
			@FromVagueDateEnd,
			IsNull(@FromVagueDateType, 'U'),
			@ToVagueDateStart,
			@ToVagueDateEnd,
			@ToVagueDateType,	
			@AcqVagueDateStart,
			@AcqVagueDateEnd,
			IsNull(@AcqVagueDateType, 'U'),	
			@URL,
			@SessionID,
			IsNull(@SystemSuppliedData, 0)
		)
		IF @@Error <> 0 GOTO RollbackAndExit

		/*------------------------------------------------------------------*\
		  When a concept group version is added to a concept group for the 
			first time, all of the existing concepts should have a version 
			history record added for the new version. See IR 4918
		\*------------------------------------------------------------------*/
		IF @SequenceNumber=1
		BEGIN
			DECLARE @CurrentConceptKey char(16),
				@NewConceptHistoryKey char(16)
		
			-- Get all the Concepts in the Concept Group
			DECLARE curConcepts CURSOR LOCAL FAST_FORWARD FOR
				SELECT	C.Concept_Key
				FROM	Concept AS C
				WHERE	Concept_Group_Key = @ConceptGroupKey
		
			OPEN curConcepts
		
			FETCH NEXT
			FROM	curConcepts
			INTO	@CurrentConceptKey
		
			WHILE @@Fetch_Status = 0
			BEGIN
				IF NOT EXISTS (SELECT 1 FROM Concept_History WHERE Concept_Key=@CurrentConceptKey)
					EXEC usp_ConceptHistory_Insert 
						@Key = 'Unwanted Key',
						@ConceptKey = @CurrentConceptKey,
						@ConceptGroupVersionFromKey = @Key,
						@SessionID = @SessionID
		
				IF @@Error <> 0 GOTO RollbackAndExit
		
				FETCH NEXT
				FROM	curConcepts
				INTO	@CurrentConceptKey
			END
			CLOSE	curConcepts
			DEALLOCATE curConcepts
		END
		ELSE BEGIN
			/*------------------------------------------------------------------*\
			  When adding further concept group versions, make sure all old 
				concepts now expire if they were set to expire in the last version
			\*------------------------------------------------------------------*/
			UPDATE C
			SET Is_Current=0
			FROM Concept C
			INNER  JOIN Concept_History	CH ON CH.Concept_Key=C.Concept_Key
			INNER JOIN Concept_Group_Version CGVExpire ON CGVExpire.Concept_Group_Version_Key=CH.Concept_Group_Version_To
				AND CGVExpire.Sequence = @SequenceNumber-1
			WHERE C.Concept_Group_Key=@ConceptGroupKey
		END
		
	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptGroupVersion_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptGroupVersion_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptGroupVersion_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptGroupVersion_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptGroupVersion_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptGroupVersion_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptGroupVersion_Insert TO [Dev - JNCC SQL]
END
GO