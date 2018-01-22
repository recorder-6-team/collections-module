/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Valuation_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Valuation_Update]
GO

/*===========================================================================*\
  Description:	Updates a record in the Valuation table

  Parameters:	@Key
		@VagueDateStart
		@VagueDateEnd
		@VagueDateType
		@RefNumber
		@TypeConceptKey
		@ValuedByNameKey
		@ValueAmount
		@CurrencyConceptKey
		@ValidFromVagueDateStart
		@ValidFromVagueDateEnd 
		@ValidFromVagueDateType
		@ValidToVagueDateStart
		@ValidToVagueDateEnd 
		@ValidToVagueDateType
		@Description
		@SessionID 
		@Timestamp

  Created:	October 2003

  Last revision information:
    $Revision: 5 $
    $Date: 3/02/09 10:56 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Valuation_Update]
	@Key char(16) OUTPUT,
	@VagueDateStart int, 
	@VagueDateEnd int, 
	@VagueDateType varchar(2),
	@RefNumber varchar(30),
	@TypeConceptKey char(16),
	@ValuedByNameKey char(16),
	@ValueAmount money,
	@CurrencyConceptKey char(16),
	@ValidFromVagueDateStart int,
	@ValidFromVagueDateEnd int,
	@ValidFromVagueDateType varchar(2),
	@ValidToVagueDateStart int,
	@ValidToVagueDateEnd int,
	@ValidToVagueDateType varchar(2),
	@Description text,
	@SessionID char(16),
	@Timestamp timestamp

AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION

		UPDATE 	Valuation
		SET 	Vague_Date_Start = @VagueDateStart,
			Vague_Date_End = @VagueDateEnd,
			Vague_Date_Type = @VagueDateType,
			Ref_Number = @RefNumber,
			Type_Concept_Key = @TypeConceptKey,
			Valued_By_Name_Key = @ValuedByNameKey,
			Value_Amount = @ValueAmount,		
			Currency_Concept_Key = @CurrencyConceptKey,
			Valid_From_Vague_Date_Start = @ValidFromVagueDateStart, 
			Valid_From_Vague_Date_End = @ValidFromVagueDateEnd,
			Valid_From_Vague_Date_Type = @ValidFromVagueDateType, 
			Valid_To_Vague_Date_Start = @ValidToVagueDateStart,
			Valid_To_Vague_Date_End = @ValidToVagueDateEnd, 
			Valid_To_Vague_Date_Type = @ValidToVagueDateType,
			Description = @Description, 
			Entered_Session_ID = @SessionID
		WHERE	Valuation_Key = @Key
		AND		[Timestamp] = @Timestamp

		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Valuation WHERE Valuation_Key = @Key)
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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Valuation_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Valuation_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Valuation_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Valuation_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Valuation_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Valuation_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Valuation_Update TO [Dev - JNCC SQL]
END
GO