/*===========================================================================*\
  Description:	Script to insert system supplied Concept called 'Field Record'

  Created:	March 2004

  Last revision information:
    $Revision: 2 $
    $Date: 23/09/08 14:08 $
    $Author: Ericsalmon $

\*===========================================================================*/

DECLARE @ConceptGroupKey char(16),
	@TermName nvarchar(100),
	@PlainText nvarchar(100),
	@LanguageKey varchar(4),
	@SessionID char(16),
	@NameTypeConceptKey char(16),
	@Key char(16),
	@NewTermKey char(16), 
	@NewMeaningKey char(16),
	@ConceptGroupVersionKey char(16),
	@SystemSuppliedData bit

SELECT	@ConceptGroupKey = 'SYSTEM000000005T',
	@TermName = 'Field Record',
	@PlainText = 'Field Record',
	@LanguageKey = 'en',
	@SessionID = 'SYSTEM0000000000',
	@NameTypeConceptKey = 'SYSTEM0000000000',
	@SystemSuppliedData = 1,
	@Key = 'SYSTEM000000220G',
	@NewTermKey = 'SYSTEM00000004BR',
	@NewMeaningKey = 'SYSTEM00000021ZT'
	

BEGIN TRANSACTION

	IF NOT EXISTS(SELECT * FROM Concept_Group WHERE Concept_Group_Key = @ConceptGroupKey)
	BEGIN
		RAISERROR('Invalid CONCEPT_GROUP_KEY, Concept Group does not exist yet.', 16, 1)
		GOTO RollbackAndExit
	END
	ELSE
	BEGIN
		/*-------------------------------------------------------------*\
		  Create the new term.
		\*-------------------------------------------------------------*/
		IF NOT EXISTS(SELECT * FROM Term WHERE Term_Key = @NewTermKey)
			INSERT INTO Term (
				Term_Key, 
				Language_Key, 
				Item_Name, 
				Plaintext, 
				Entered_Session_ID, 
				System_Supplied_Data
			) VALUES (
				@NewTermKey, 
				@LanguageKey, 
				@TermName, 
				@PlainText, 
				@SessionID, 
				@SystemSuppliedData
			)
		IF @@Error <> 0 GOTO RollbackAndExit

		/*-------------------------------------------------------------*\
		  Create new Meaning.
		\*-------------------------------------------------------------*/
		IF NOT EXISTS(SELECT * FROM Meaning WHERE Meaning_Key = @NewMeaningKey)
			INSERT INTO Meaning (
				Meaning_Key
			) VALUES (
				@NewMeaningKey
			)
			IF @@Error <> 0 GOTO RollbackAndExit

		/*-------------------------------------------------------------*\
		  Create new Concept.
		\*-------------------------------------------------------------*/
		IF NOT EXISTS(SELECT * FROM Concept WHERE Concept_Key = @Key)
			INSERT INTO Concept (
				Concept_Key, 
				Term_Key, 
				Concept_Group_Key, 
				List_Preferred, 
				Preferred, 
				Is_Current, 
				Name_Type_Concept_Key, 
				Meaning_Key, 
				Entered_Session_ID, 
				System_Supplied_Data
			) VALUES (
				@Key, 
				@NewTermKey, 
				@ConceptGroupKey, 
				1, 
				1, 
				1, 
				@NameTypeConceptKey, 
				@NewMeaningKey, 
				@SessionID, 
				@SystemSuppliedData
			)
		IF @@Error <> 0 GOTO RollbackAndExit

		/*-------------------------------------------------------------*\
		  Don't need to insert a Concept History record because the
		  Concept Group we are adding to has no Concept_Group_Version.
		\*-------------------------------------------------------------*/

		/*-------------------------------------------------------------*\
		  Create Concept_Lineage
		\*-------------------------------------------------------------*/
		EXECUTE		usp_ConceptLineage_NewConcept	@Key
		IF @@ERROR <> 0 GOTO RollbackAndExit
	END

	COMMIT TRANSACTION
	RETURN

RollBackAndExit: 
	ROLLBACK TRANSACTION
	RAISERROR ('usp_Concept_Insert failed', 16, 1)
GO

