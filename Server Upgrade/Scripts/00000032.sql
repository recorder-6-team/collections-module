-- CCN 72 - VI 17295
DECLARE @TermKey CHAR(16),
		@MeaningKey CHAR(16),
		@ConceptKey CHAR(16),
		@ConceptHistoryKey CHAR(16)

BEGIN TRANSACTION

	SET @TermKey = 'SYSTEM00000004BS'
	SET @MeaningKey = 'SYSTEM00000021ZU'
	SET @ConceptKey = 'SYSTEM000000220H'
	SET @ConceptHistoryKey = 'SYSTEM0000009T6H'

	IF NOT EXISTS (SELECT * FROM Term WHERE Term_Key = @TermKey)
	BEGIN
		-- Create the Term record
		INSERT INTO Term (
			Term_Key,
			Language_Key,
			Item_Name,
			Plaintext,
			Entered_Session_ID,
			System_Supplied_Data,
			Custodian
		) VALUES (
			@TermKey,
			'en',
			'Presence Check',
			'Presence Check',
			'SYSTEM0000000000',
			1,
			'SYSTEM00'
		)
		IF @@Error <> 0 GOTO RollbackAndExit
	END

	IF NOT EXISTS (SELECT * FROM Meaning WHERE Meaning_Key = @MeaningKey)
	BEGIN
		-- Create the Meaning Record
		INSERT INTO Meaning (
			Meaning_Key
		) VALUES (
			@MeaningKey
		)
		IF @@Error <> 0 GOTO RollbackAndExit
	END

	IF NOT EXISTS (SELECT * FROM Concept WHERE Concept_Key = @ConceptKey)
	BEGIN
		-- Create the Concept record
		INSERT INTO Concept (
			Concept_Key, 
			Term_Key, 
			Concept_Group_Key, 
			List_Preferred, 
			Is_Current, 
			Preferred, 
			Name_Type_Concept_Key, 
			Meaning_Key, 
			Entered_Session_ID, 
			System_Supplied_Data,
			Custodian
		) VALUES (
			@ConceptKey, 
			@TermKey, 
			'SYSTEM0000000006', 
			1, 
			1, 
			1, 
			'SYSTEM000000000L', 
			@MeaningKey, 
			'SYSTEM0000000000', 
			1,
			'SYSTEM00'
		)
		IF @@Error <> 0 GOTO RollbackAndExit
	END

	IF NOT EXISTS (SELECT * FROM Concept_History WHERE Concept_History_Key = @ConceptHistoryKey)
	BEGIN
		-- Create the concept history record
		INSERT INTO Concept_History (
			Concept_History_Key,
			Concept_Key,
			Concept_Group_Version_From,
			Entered_Session_ID,
			System_Supplied_Data,
			Custodian
		) VALUES (
			@ConceptHistoryKey, 
			@ConceptKey,
			'SYSTEM0000000006',
			'SYSTEM0000000000',
			1,
			'SYSTEM00'
		)	
		IF @@Error <> 0 GOTO RollbackAndExit
	END

	SET @TermKey = 'SYSTEM00000004BT'
	SET @MeaningKey = 'SYSTEM00000021ZV'
	SET @ConceptKey = 'SYSTEM000000220I'
	SET @ConceptHistoryKey = 'SYSTEM0000009T6I'

	IF NOT EXISTS (SELECT * FROM Term WHERE Term_Key = @TermKey)
	BEGIN
		-- Create the Term record
		INSERT INTO Term (
			Term_Key,
			Language_Key,
			Item_Name,
			Plaintext,
			Entered_Session_ID,
			System_Supplied_Data,
			Custodian
		) VALUES (
			@TermKey,
			'en',
			'Present',
			'Present',
			'SYSTEM0000000000',
			1,
			'SYSTEM00'
		)
		IF @@Error <> 0 GOTO RollbackAndExit
	END

	IF NOT EXISTS (SELECT * FROM Meaning WHERE Meaning_Key = @MeaningKey)
	BEGIN
		-- Create the Meaning Record
		INSERT INTO Meaning (
			Meaning_Key
		) VALUES (
			@MeaningKey
		)
		IF @@Error <> 0 GOTO RollbackAndExit
	END

	IF NOT EXISTS (SELECT * FROM Concept WHERE Concept_Key = @ConceptKey)
	BEGIN
		-- Create the Concept record
		INSERT INTO Concept (
			Concept_Key, 
			Term_Key, 
			Concept_Group_Key, 
			List_Preferred, 
			Is_Current, 
			Preferred, 
			Name_Type_Concept_Key, 
			Meaning_Key, 
			Entered_Session_ID, 
			System_Supplied_Data,
			Custodian
		) VALUES (
			@ConceptKey, 
			@TermKey, 
			'SYSTEM0000000007', 
			1, 
			1, 
			1, 
			'SYSTEM000000000L', 
			@MeaningKey, 
			'SYSTEM0000000000', 
			1,
			'SYSTEM00'
		)
		IF @@Error <> 0 GOTO RollbackAndExit
	END

	IF NOT EXISTS (SELECT * FROM Concept_History WHERE Concept_History_Key = @ConceptHistoryKey)
	BEGIN
		-- Create the concept history record
		INSERT INTO Concept_History (
			Concept_History_Key,
			Concept_Key,
			Concept_Group_Version_From,
			Entered_Session_ID,
			System_Supplied_Data,
			Custodian
		) VALUES (
			@ConceptHistoryKey, 
			@ConceptKey,
			'SYSTEM0000000007',
			'SYSTEM0000000000',
			1,
			'SYSTEM00'
		)	
		IF @@Error <> 0 GOTO RollbackAndExit
	END

	SET @TermKey = 'SYSTEM00000004BU'
	SET @MeaningKey = 'SYSTEM00000021ZW'
	SET @ConceptKey = 'SYSTEM000000220J'
	SET @ConceptHistoryKey = 'SYSTEM0000009T6J'

	IF NOT EXISTS (SELECT * FROM Term WHERE Term_Key = @TermKey)
	BEGIN
		-- Create the Term record
		INSERT INTO Term (
			Term_Key,
			Language_Key,
			Item_Name,
			Plaintext,
			Entered_Session_ID,
			System_Supplied_Data,
			Custodian
		) VALUES (
			@TermKey,
			'en',
			'Not Found',
			'Not Found',
			'SYSTEM0000000000',
			1,
			'SYSTEM00'
		)
		IF @@Error <> 0 GOTO RollbackAndExit
	END

	IF NOT EXISTS (SELECT * FROM Meaning WHERE Meaning_Key = @MeaningKey)
	BEGIN
		-- Create the Meaning Record
		INSERT INTO Meaning (
			Meaning_Key
		) VALUES (
			@MeaningKey
		)
		IF @@Error <> 0 GOTO RollbackAndExit
	END

	IF NOT EXISTS (SELECT * FROM Concept WHERE Concept_Key = @ConceptKey)
	BEGIN
		-- Create the Concept record
		INSERT INTO Concept (
			Concept_Key, 
			Term_Key, 
			Concept_Group_Key, 
			List_Preferred, 
			Is_Current, 
			Preferred, 
			Name_Type_Concept_Key, 
			Meaning_Key, 
			Entered_Session_ID, 
			System_Supplied_Data,
			Custodian
		) VALUES (
			@ConceptKey, 
			@TermKey, 
			'SYSTEM0000000007', 
			1, 
			1, 
			1, 
			'SYSTEM000000000L', 
			@MeaningKey, 
			'SYSTEM0000000000', 
			1,
			'SYSTEM00'
		)
		IF @@Error <> 0 GOTO RollbackAndExit
	END

	IF NOT EXISTS (SELECT * FROM Concept_History WHERE Concept_History_Key = @ConceptHistoryKey)
	BEGIN
		-- Create the concept history record
		INSERT INTO Concept_History (
			Concept_History_Key,
			Concept_Key,
			Concept_Group_Version_From,
			Entered_Session_ID,
			System_Supplied_Data,
			Custodian
		) VALUES (
			@ConceptHistoryKey, 
			@ConceptKey,
			'SYSTEM0000000007',
			'SYSTEM0000000000',
			1,
			'SYSTEM00'
		)	
		IF @@Error <> 0 GOTO RollbackAndExit
	END

COMMIT TRANSACTION
GOTO Finish

RollBackAndExit: 
	ROLLBACK TRANSACTION

Finish:
GO

