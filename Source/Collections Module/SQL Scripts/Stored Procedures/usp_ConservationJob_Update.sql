/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConservationJob_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConservationJob_Update]
GO

/*===========================================================================*\
  Description:	Updates a record into the Conservation Job table

  Parameters:	@Key

		@Timestamp

  Created:	September 2003

  Last revision information:
    $Revision: 8 $
    $Date: 3/02/09 9:15 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConservationJob_Update]
	@Key CHAR(16),
	@ItemName VARCHAR(100),
	@FromVagueDateStart INT,
	@FromVagueDateEnd INT,
	@FromVagueDateType VARCHAR(2) = NULL,
	@ToVagueDateStart INT,
	@ToVagueDateEnd INT,
	@ToVagueDateType VARCHAR(2),
	@Duration FLOAT,
	@DurationType CHAR(16),
	@Status TINYINT,
	@CostAmount MONEY,
	@Currency CHAR(16),
	@Details TEXT,
	@SessionID CHAR(16),
	@Timestamp timestamp,
	@RecordsAffected int output

AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION
	
		DECLARE	@Error int
		
		UPDATE 	Conservation_Job
		SET 	Item_Name = @ItemName,
			From_Vague_Date_Start = @FromVagueDateStart,
			From_Vague_Date_End = @FromVagueDateEnd,
			From_Vague_Date_Type = IsNull(@FromVagueDateType, 'U'),
			To_Vague_Date_Start = @ToVagueDateStart,
			To_Vague_Date_End = @ToVagueDateEnd,
			To_Vague_Date_Type = @ToVagueDateType,
			Duration = @Duration,
			Duration_Unit_Concept_Key = @DurationType,
			Status = @Status,
			Cost = @CostAmount,
			Currency_Concept_Key = @Currency,
			Details = @Details,
			Changed_Session_ID = @SessionID
		WHERE	Conservation_Job_Key = @Key
		AND		[Timestamp] = @Timestamp

		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Conservation_Job WHERE Conservation_Job_Key = @Key)
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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConservationJob_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConservationJob_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConservationJob_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConservationJob_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConservationJob_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConservationJob_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConservationJob_Update TO [Dev - JNCC SQL]
END
GO