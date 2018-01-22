/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Concept_Insert]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Concept_Insert]
GO

/*===========================================================================*\
  Description:	Inserts a new concept and term if necessary.

  Parameters:	@Key	
		@ConceptGroupKey
		@TermName
		@PlainText 
		@LanguageKey 
		@SessionID 
		@NameTypeConceptKey 
		@IsSystem bit
		@PublishedTerm
		@AutomaticPublishedTerm
		@TermVersionKey

  Created:	August 2003

  Last revision information:
    $Revision: 20 $
    $Date: 26/08/11 14:50 $
    $Author: Jamesbichard $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_Concept_Insert]
	@Key char(16) OUTPUT,
	@ConceptGroupKey char(16),
	@TermName nvarchar(100),
	@PlainText nvarchar(100) = NULL,
	@LanguageKey varchar(4) = NULL,
	@SessionID char(16),
	@NameTypeConceptKey char(16) = NULL,
	@IsSystem bit = NULL,
	@PublishedTerm NVARCHAR(450) = NULL,
	@AutomaticPublishedTerm BIT = NULL,
	@TermVersionKey CHAR(16) = NULL
AS
	SET NOCOUNT ON
	SET ANSI_NULLS ON
	SET ANSI_PADDING ON
	SET ANSI_WARNINGS ON
	SET CONCAT_NULL_YIELDS_NULL ON
	SET QUOTED_IDENTIFIER ON
	SET NO_BROWSETABLE OFF
	SET XACT_ABORT ON

	BEGIN TRANSACTION

	DECLARE @NewTermKey char(16), 
		@NewConceptHistoryKey char(16),
		@SiteID char(8),
		@ConceptGroupVersionKey char(16),
		@SystemSuppliedData bit,
		@NewMeaningKey char(16)

	IF @AutomaticPublishedTerm IS NULL
		SET @AutomaticPublishedTerm = 1

	IF @IsSystem = 1
	BEGIN
		SET @SiteID = 'SYSTEM00'
		SET @SystemSuppliedData = 1
	END
	ELSE
	BEGIN
		SELECT @SiteID = Data FROM Setting WHERE [Name] = 'SiteID'
		SET @SystemSuppliedData = 0
	END

	IF NOT EXISTS(SELECT * FROM Concept_Group WHERE Concept_Group_Key = @ConceptGroupKey)
	BEGIN
		RAISERROR('Invalid CONCEPT_GROUP_KEY, Concept Group does not exist', 16, 1)
		GOTO RollbackAndExit
	END
	ELSE
	BEGIN
		-- if no NameTypeConcept Key, use default one.
		IF @NameTypeConceptKey IS NULL SET @NameTypeConceptKey = 'SYSTEM00000000AN'

		-- if plaintext is null, set it equal to TermName.
		IF @PlainText IS NULL SET @PlainText = @TermName
		
		IF @PublishedTerm IS NULL SET @PublishedTerm = @TermName
		
		-- If Language not specified, get the one with Priority of 1.
		IF @LanguageKey IS NULL
			SELECT	@LanguageKey = Language_Key
			FROM	Language
			WHERE	Priority = 1

		/*-------------------------------------------------------------*\
		  Find out if new Term is required and create if needed.
		\*-------------------------------------------------------------*/
		SELECT 	@NewTermKey = Term_Key 
		FROM 	Term 
		WHERE 	Plaintext = dbo.ufn_RemoveHtmlMarkup(@TermName)
		AND 	Language_Key = @LanguageKey

		IF @NewTermKey IS NULL
		BEGIN
			EXEC spNextKey 'Term', @NewTermKey OUTPUT, @SiteID
			IF @@Error <> 0 GOTO RollbackAndExit
			INSERT INTO Term (
				Term_Key, Language_Key, Plaintext, Entered_Session_ID, 
				System_Supplied_Data
			) VALUES (
				@NewTermKey, @LanguageKey, @PlainText, @SessionID, @SystemSuppliedData
			)
			IF @@Error <> 0 GOTO RollbackAndExit
		END
		
		/*-------------------------------------------------------------*\
		  Create a Term_Version if an existing key wasn't supplied...
		\*-------------------------------------------------------------*/
		IF @TermVersionKey IS NULL
		BEGIN
			EXECUTE spNextKey 'Term_Version', @TermVersionKey OUTPUT
			
			IF @@Error <> 0 GOTO RollbackAndExit
			
			INSERT INTO	dbo.Term_Version
						(
							Term_Version_Key,
							Term_Key,
							Version_Label,
							Author_And_Date,
							Entered_Session_ID,
							Changed_Session_ID,
							Custodian
						)
			VALUES		(
							@TermVersionKey,
							@NewTermKey,
							NULL,
							NULL,
							@SessionID,
							NULL,
							NULL
						)
		END
		
		/*-------------------------------------------------------------*\
		  Create new Concept and Meaning.
		\*-------------------------------------------------------------*/
		EXEC spNextKey 'Concept', @Key OUTPUT, @SiteID
		IF @@Error <> 0 GOTO RollbackAndExit

		EXEC spNextKey 'Meaning', @NewMeaningKey OUTPUT, @SiteID
		IF @@Error <> 0 GOTO RollbackAndExit

		INSERT INTO [dbo].[Meaning] (Meaning_Key) VALUES (@NewMeaningKey)
		IF @@Error <> 0 GOTO RollbackAndExit

		-- Now insert the Concept.
		INSERT INTO Concept (
			Concept_Key, Term_Key, Term_Version_Key, Concept_Group_Key, List_Preferred, Preferred, Is_Current, 
			Name_Type_Concept_Key, Meaning_Key, Entered_Session_ID, System_Supplied_Data,
			Published_Term, Automatic_Published_Term
		) VALUES (
			@Key, @NewTermKey, @TermVersionKey, @ConceptGroupKey, 1, 1, 1, 
			@NameTypeConceptKey, @NewMeaningKey, @SessionID, @SystemSuppliedData,
			@PublishedTerm, @AutomaticPublishedTerm
		)
		IF @@Error <> 0 GOTO RollbackAndExit

		/*-------------------------------------------------------------*\
		  Create new Concept_History.
		\*-------------------------------------------------------------*/
		EXEC spNextKey 'Concept_History', @NewConceptHistoryKey OUTPUT, @SiteID
		IF @@Error <> 0 GOTO RollbackAndExit
		INSERT INTO Concept_History (
			Concept_History_Key, Concept_Key, Concept_Group_Version_From,
			Entered_Session_ID, System_Supplied_Data
		)
			SELECT TOP 1 	@NewConceptHistoryKey, @Key, Concept_Group_Version_Key ,
					@SessionID, @SystemSuppliedData
			FROM 		Concept_Group_Version
			WHERE 		Concept_Group_Key = @ConceptGroupKey
			ORDER BY 	From_Vague_Date_Start DESC
		IF @@Error <> 0 GOTO RollbackAndExit

		/*-------------------------------------------------------------*\
		  Create Concept_Lineage
		\*-------------------------------------------------------------*/
		EXECUTE		usp_ConceptLineage_NewConcept	@Key
		IF @@ERROR <> 0 GOTO RollbackAndExit
	END

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_Concept_Insert failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Concept_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Concept_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concept_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concept_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Concept_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Concept_Insert TO [Dev - JNCC SQL]
END
GO