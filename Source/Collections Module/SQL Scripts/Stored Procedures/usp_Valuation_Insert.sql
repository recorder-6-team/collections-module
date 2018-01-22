/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Valuation_Insert]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Valuation_Insert]
GO

/*===========================================================================*\
  Description:	Inserts a record into the Valuation table

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

  Created:	September 2003

  Last revision information:
    $Revision: 2 $
    $Date: 12/11/03 16:40 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Valuation_Insert]
	@Key char(16) OUTPUT,
	@ParentKey char(16),
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
	@SessionID char(16)

AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	/*-------------------------------------------------------------*\
	  Get a new key.
	\*-------------------------------------------------------------*/
	EXECUTE spNextKey 'Valuation', @Key OUTPUT

	/*-------------------------------------------------------------*\
	  Wrap everything in one transaction.
	\*-------------------------------------------------------------*/
	BEGIN TRANSACTION

		/*-------------------------------------------------------------*\
		  Insert in Valuation.
		\*-------------------------------------------------------------*/
		INSERT INTO Valuation (
			Valuation_Key, Vague_Date_Start, Vague_Date_End, 
			Vague_Date_Type, Ref_Number, Type_Concept_Key, 
			Valued_By_Name_Key, Value_Amount, Currency_Concept_Key,
			Valid_From_Vague_Date_Start, Valid_From_Vague_Date_End,
			Valid_From_Vague_Date_Type, Valid_To_Vague_Date_Start,
			Valid_To_Vague_Date_End, Valid_To_Vague_Date_Type,
			Description, Entered_Session_ID
		) VALUES (
			@Key, @VagueDateStart, @VagueDateEnd, 
			@VagueDateType, @RefNumber, @TypeConceptKey, 
			@ValuedByNameKey, @ValueAmount, @CurrencyConceptKey,
			@ValidFromVagueDateStart, @ValidFromVagueDateEnd,
			@ValidFromVagueDateType, @ValidToVagueDateStart,
			@ValidToVagueDateEnd, @ValidToVagueDateType,
			@Description, @SessionID
		)
		IF @@Error <> 0 GOTO RollbackAndExit

	/*-------------------------------------------------------------*\
	  All went well, so commit.
	\*-------------------------------------------------------------*/
	COMMIT TRANSACTION

RollBackAndExit: 
	/*-------------------------------------------------------------*\
	  Cancel any changes, or left-overs might mess up some tables.
	\*-------------------------------------------------------------*/
	IF @@TranCount > 0 ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Valuation_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Valuation_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Valuation_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Valuation_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Valuation_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Valuation_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Valuation_Insert TO [Dev - JNCC SQL]
END

GO
			