/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConservationJob_Insert]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConservationJob_Insert]
GO
/*===========================================================================*\
  Description:	Insert a record into the Conservation Job table.
  Parameters:	@Key 
		@ItemName
		@FromVagueDateStart
		@FromVagueDateEnd
		@FromVagueDateType
		@ToVagueDateStart
		@ToVagueDateEnd 
		@ToVagueDateType
		@Duration
		@DurationType
		@Status
		@CostAmount
		@Currency
		@Details 
		@SessionID

  Created:	September 2003

  Last revision information:
    $Revision: 5 $
    $Date: 3/02/04 10:19 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConservationJob_Insert]
	@Key CHAR(16) OUTPUT,
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
	@SessionID char(16)
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF
	
	EXECUTE spNextKey 'Conservation_Job', @Key OUTPUT

	BEGIN TRANSACTION	
		INSERT INTO Conservation_Job (
			Conservation_Job_Key, Item_Name,
			From_Vague_Date_Start, From_Vague_Date_End, From_Vague_Date_Type,
			To_Vague_Date_Start, To_Vague_Date_End, To_Vague_Date_Type,
			Duration, Duration_Unit_Concept_Key, Status, Cost, Currency_Concept_Key, Details,	
			Entered_Session_ID
		) VALUES (
			@Key, @ItemName,
			@FromVagueDateStart, @FromVagueDateEnd, IsNull(@FromVagueDateType, 'U'),
			@ToVagueDateStart, @ToVagueDateEnd, @ToVagueDateType,
			@Duration, @DurationType, @Status, @CostAmount, @Currency, @Details,
			@SessionID
		)

		IF @@Error <> 0 GOTO RollBackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit:
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConservationJob_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConservationJob_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConservationJob_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConservationJob_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConservationJob_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConservationJob_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConservationJob_Insert TO [Dev - JNCC SQL]
END
GO