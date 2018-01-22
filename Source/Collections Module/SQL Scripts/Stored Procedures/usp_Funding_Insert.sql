/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Funding_Insert]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Funding_Insert]
GO

/*===========================================================================*\
  Description:	Inserts a record into the Funding table
  Parameters:	@Key
		@ParentKey 
		@FundedByNameKey,
		@VagueDateStart 
		@VagueDateEnd 
		@VagueDateType
		@Amount
		@CurrencyConceptKey 
		@Details 
		@SessionID 
		@IsMovement

  Created:	September 2003

  Last revision information:
    $Revision: 4 $
    $Date: 8/12/03 10:54 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Funding_Insert]
	@Key CHAR(16) OUTPUT,
	@ParentKey CHAR(16),
	@FundedByNameKey CHAR(16),
	@VagueDateStart INT,
	@VagueDateEnd INT,
	@VagueDateType VARCHAR(2),
	@Amount MONEY,
	@CurrencyConceptKey CHAR(16),
	@Details TEXT,
	@SessionID CHAR(16),
	@IsMovement BIT
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION

		IF @IsMovement = 1 BEGIN	
			EXECUTE spNextKey 'Movement_Funding', @Key OUTPUT

			INSERT INTO Movement_Funding (
				Movement_Funding_Key,
				Movement_Key,
				Funded_By_Name_Key,
				Vague_Date_Start,
				Vague_Date_End,
				Vague_Date_Type,
				Amount,
				Currency_Concept_Key,
				Details,
				Entered_Session_ID
			) VALUES (
				@Key,
				@ParentKey,
				@FundedByNameKey,
				@VagueDateStart,
				@VagueDateEnd,
				@VagueDateType,
				@Amount,
				@CurrencyConceptKey,
				@Details,
				@SessionID
			)
		END ELSE BEGIN
			EXECUTE spNextKey 'Conservation_Job_Funding', @Key OUTPUT

			INSERT INTO Conservation_Job_Funding (
				Conservation_Job_Funding_Key,
				Conservation_Job_Key,
				Funded_By_Name_Key,
				Vague_Date_Start,
				Vague_Date_End,
				Vague_Date_Type,
				Amount,
				Currency_Concept_Key,
				Details,
				Entered_Session_ID
			) VALUES (
				@Key,
				@ParentKey,
				@FundedByNameKey,
				@VagueDateStart,
				@VagueDateEnd,
				@VagueDateType,
				@Amount,
				@CurrencyConceptKey,
				@Details,
				@SessionID
			)
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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Funding_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Funding_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Funding_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Funding_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Funding_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Funding_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Funding_Insert TO [Dev - JNCC SQL]
END
GO