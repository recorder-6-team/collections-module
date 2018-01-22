/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Funding_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Funding_Update]
GO

/*===========================================================================*\
  Description:	Updates a record in the correct Funding table

  Parameters:	@Key 
		@ParentKey 
		@FundedByNameKey 
		@VagueDateStart 
		@VagueDateEnd 
		@VagueDateType 
		@Amount 
		@CurrencyConceptKey 
		@Details 
		@SessionID 
		@Timestamp 
		@IsMovement 

  Created:	September 2003

  Last revision information:
    $Revision: 5 $
    $Date: 3/02/09 9:33 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Funding_Update]
	@Key CHAR(16),
	@ParentKey CHAR(16),
	@FundedByNameKey CHAR(16),
	@VagueDateStart INT,
	@VagueDateEnd INT,
	@VagueDateType VARCHAR(2),
	@Amount MONEY,
	@CurrencyConceptKey CHAR(16),
	@Details TEXT,
	@SessionID CHAR(16),
	@Timestamp timestamp,
	@IsMovement BIT
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION
		
		DECLARE @Error int
		DECLARE @RecordsAffected int

		IF @IsMovement = 1
		BEGIN
			UPDATE 	Movement_Funding
			SET 	Movement_Key = @ParentKey,
				Funded_By_Name_Key = @FundedByNameKey,
				Vague_Date_Start = @VagueDateStart,
				Vague_Date_End = @VagueDateEnd,
				Vague_Date_Type = @VagueDateType,
				Amount = @Amount,
				Currency_Concept_Key = @CurrencyConceptKey,
				Details = @Details,
				Changed_Session_ID = @SessionID
			WHERE	Movement_Funding_Key = @Key
			AND		[Timestamp] = @Timestamp

			SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

			IF @Error <> 0 GOTO RollbackAndExit 
		
			IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Movement_Funding WHERE Movement_Funding_Key = @Key)
			BEGIN
				RAISERROR('Record updated by another user', 16, 1)
				GOTO RollbackAndExit
			END

		END ELSE BEGIN
			UPDATE 	Conservation_Job_Funding
			SET 	Conservation_Job_Key = @ParentKey,
				Funded_By_Name_Key = @FundedByNameKey,
				Vague_Date_Start = @VagueDateStart,
				Vague_Date_End = @VagueDateEnd,
				Vague_Date_Type = @VagueDateType,
				Amount = @Amount,
				Currency_Concept_Key = @CurrencyConceptKey,
				Details = @Details,
				Changed_Session_ID = @SessionID
			WHERE	Conservation_Job_Funding_Key = @Key
			AND		[Timestamp] = @Timestamp

			SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

			IF @Error <> 0 GOTO RollbackAndExit 
		
			IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Conservation_Job_Funding WHERE Conservation_Job_Funding_Key = @Key)
			BEGIN
				RAISERROR('Record updated by another user', 16, 1)
				GOTO RollbackAndExit
			END
		END

		IF @@Error <> 0 GOTO RollbackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO
			
/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Funding_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Funding_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Funding_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Funding_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Funding_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Funding_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Funding_Update TO [Dev - JNCC SQL]
END
GO