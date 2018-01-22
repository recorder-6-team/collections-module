/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Valuation_Duplicate]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Valuation_Duplicate]
GO

/*===========================================================================*\
  Description:	Duplicates a Valuation record except for the Value itself.

  Parameters:	@OriginalKey

  Created:	January 2004

  Last revision information:
    $Revision: 2 $
    $Date: 1/03/04 9:39 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Valuation_Duplicate]
	@NewKey char(16) OUTPUT,
	@Key char(16),
	@SessionID char(16),
	@RecordsAffected int OUTPUT
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF
	
	DECLARE @Error int

	/*-------------------------------------------------------------*\
	  Get a new key.
	\*-------------------------------------------------------------*/
	EXECUTE spNextKey 'Valuation', @NewKey OUTPUT

	/*-------------------------------------------------------------*\
	  Wrap everything in one transaction.
	\*-------------------------------------------------------------*/
	BEGIN TRANSACTION

		/*-------------------------------------------------------------*\
		  Insert in Valuation.
		\*-------------------------------------------------------------*/
		INSERT INTO Valuation (
			Valuation_Key, 
			Vague_Date_Start, 
			Vague_Date_End, 
			Vague_Date_Type, 
			Ref_Number, 
			Type_Concept_Key, 
			Valued_By_Name_Key, 
			Value_Amount, 
			Currency_Concept_Key,
			Valid_From_Vague_Date_Start, 
			Valid_From_Vague_Date_End,
			Valid_From_Vague_Date_Type, 
			Valid_To_Vague_Date_Start,
			Valid_To_Vague_Date_End, 
			Valid_To_Vague_Date_Type,
			[Description], 
			Display_Caption,
			Search_Caption,
			Entered_Session_ID
		) 
		SELECT 	
		 	@NewKey, 
			Vague_Date_Start, 
			Vague_Date_End, 
			Vague_Date_Type, 
			Ref_Number, 
			Type_Concept_Key, 
			Valued_By_Name_Key, 
			0, 
			Currency_Concept_Key,
			Valid_From_Vague_Date_Start, 
			Valid_From_Vague_Date_End,
			Valid_From_Vague_Date_Type, 
			Valid_To_Vague_Date_Start,
			Valid_To_Vague_Date_End, 
			Valid_To_Vague_Date_Type,
			[Description], 
			Display_Caption,
			Search_Caption,
			@SessionID
		FROM	Valuation
		WHERE	Valuation_Key = @Key

		SELECT	@RecordsAffected = @@RowCount,
			@Error = @@Error

		IF @Error <> 0 GOTO RollbackAndExit

		/*-------------------------------------------------------------*\
		  Duplicate any links to sources.
		\*-------------------------------------------------------------*/
		DECLARE	@OriginalSourceJoinKey char(16),
			@NewSourceJoinKey char(16)

		DECLARE curSources CURSOR LOCAL FAST_FORWARD FOR
			SELECT	Source_Join_Key
			FROM	Source_Join
			WHERE	Record_Key = @Key
			AND	Table_Name = 'Valuation'

		OPEN curSources

		FETCH NEXT
		FROM 	curSources
		INTO	@OriginalSourceJoinKey

		WHILE @@Fetch_Status = 0
		BEGIN
			EXECUTE spNextKey 'Source_Join', @NewSourceJoinKey OUTPUT

			INSERT INTO Source_Join (
				Source_Join_Key,
				Table_Name,
				Record_Key,
				Source_Key,
				Original,
				Entered_Session_ID,
				System_Supplied_Data
			) SELECT
				@NewSourceJoinKey,
				Table_Name,
				@NewKey,
				Source_Key,
				Original,
				@SessionID,
				0
			FROM 	Source_Join
			WHERE	Source_Join_Key = @OriginalSourceJoinKey
		
			IF @Error <> 0 GOTO RollbackAndExit
		
			FETCH NEXT
			FROM 	curSources
			INTO	@OriginalSourceJoinKey
		END

		CLOSE curSources
		DEALLOCATE curSources

	/*-------------------------------------------------------------*\
	  All went well, so commit.
	\*-------------------------------------------------------------*/
	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Valuation_Duplicate') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Valuation_Duplicate'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Valuation_Duplicate TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Valuation_Duplicate TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Valuation_Duplicate TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Valuation_Duplicate TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Valuation_Duplicate TO [Dev - JNCC SQL]
END

GO
			