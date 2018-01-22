/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_MeaningsShareTermsClone_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_MeaningsShareTermsClone_Update]
GO

/*===========================================================================*\
  Description:	When a relationship is saved that creates a new link between 
		2 meanings within a single concept group and the relationship 
		indicates at least some overlap of meaning, if a there is a 
		single term that is in both groups of terms associated with 
		each meaning, then certain actions take place. This proc.
		allows the app. to decide whether any action should occur 
		by the count of the number of records returned.

  Parameters:	@FromConceptKey
		@ToConceptKey
		@ThesaurusRelationTypeKey

  Created:	February 2004

  Last revision information:
    $Revision: 1 $
    $Date: 26/02/04 17:23 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_MeaningsShareTermsClone_Update]
	@FromConceptKey char(16),
	@ToConceptKey char(16),
	@SessionID char(16),
	@Unidirectional bit	
AS
	
SET NOCOUNT ON

	DECLARE	@ConceptKeyForCursor char(16),
		@MeaningKey2 char(16),
		@TermVersionKey2 char(16),
		@MeaningKey1 char(16),
		@TermVersionKey1 char(16)

	DECLARE @Key char(16),
		@ConceptGroupKey char(16),
		@TermVersionKey char(16),
		@TermKey char(16),
		@ListPreferred bit,
		@IsCurrent bit,
		@Preferred bit,
		@ConceptRankKey char(16),
		@NameTypeConceptKey char(16),
		@MeaningKey char(16),
		@AuthorCopy varchar(100),
		@SortCode int,
		@ListCode varchar(50),
		@SystemSuppliedData bit

	BEGIN TRANSACTION
		/*--------------------------------------------------------*\
		  Set up both cursors here before the data is altered.
		\*--------------------------------------------------------*/ 		
		DECLARE curTerms1 CURSOR LOCAL STATIC FOR
			SELECT 		C1M.Concept_Key
			FROM		Concept AS C1
			INNER JOIN	Concept AS C1M ON C1.Meaning_Key = C1M.Meaning_Key 
			WHERE		C1.Concept_Key = @FromConceptKey
			AND		C1.Concept_Group_Key = C1M.Concept_Group_Key
			AND		C1.Concept_Key <> C1M.Concept_Key
			AND		C1.Term_Key <> C1M.Term_Key

		OPEN curTerms1
		IF @@Error <> 0 GOTO RollbackAndExit

		IF @Unidirectional = 0
		BEGIN
			DECLARE curTerms2 CURSOR LOCAL STATIC FOR
				SELECT 		C2M.Concept_Key
				FROM		Concept AS C2
				INNER JOIN	Concept AS C2M ON C2.Meaning_Key = C2M.Meaning_Key 
				WHERE		C2.Concept_Key = @ToConceptKey
				AND		C2.Concept_Group_Key = C2M.Concept_Group_Key
				AND		C2.Concept_Key <> C2M.Concept_Key
				AND		C2.Term_Key <> C2M.Term_Key
	
			OPEN curTerms2
			IF @@Error <> 0 GOTO RollbackAndExit
		END

		/*--------------------------------------------------------*\
		  Duplicate the From terms as concepts with those terms, 
		  but the To meaning and Term_Version label.
		\*--------------------------------------------------------*/ 
		SELECT 	@MeaningKey2 = Meaning_Key,
			@TermVersionKey2 = Term_Version_Key
		FROM	Concept
		WHERE	Concept_Key = @ToConceptKey
	
		IF @@Error <> 0 GOTO RollbackAndExit

		FETCH NEXT
		FROM	curTerms1
		INTO	@ConceptKeyForCursor

		IF @@Error <> 0 GOTO RollbackAndExit

		WHILE @@Fetch_Status = 0
		BEGIN
			SELECT	@ConceptGroupKey = Concept_Group_Key,
				@TermVersionKey = Term_Version_Key,
				@TermKey = Term_Key,
				@ListPreferred = List_Preferred,
				@IsCurrent = Is_Current,
				@Preferred = Preferred,
				@ConceptRankKey = Concept_Rank_Key,
				@NameTypeConceptKey = Name_Type_Concept_Key,
				@MeaningKey = Meaning_Key,
				@AuthorCopy = Author_Copy,
				@SortCode = Sort_Code,
				@ListCode = List_Code
			FROM	Concept
			WHERE	Concept_Key = @ConceptKeyForCursor
	
			EXECUTE spNextKey 'Concept', @Key OUTPUT
			IF @@Error <> 0 GOTO RollbackAndExit
			
			INSERT INTO Concept (
				Concept_Key,
				Meaning_Key,
				Concept_Group_Key,
				Term_Key,
				Term_Version_Key,
				List_Preferred,
				Is_Current,
				Preferred,
				Concept_Rank_Key,
				Name_Type_Concept_Key,
				Author_Copy,
				Sort_Code,
				List_Code,
				Entered_Session_ID
			) VALUES (
				@Key,
				@MeaningKey2,
				@ConceptGroupKey,
				@TermKey,
				@TermVersionKey2,
				0,
				@IsCurrent,
				@Preferred,
				@ConceptRankKey,
				@NameTypeConceptKey,
				@AuthorCopy,
				@SortCode,
				@ListCode,
				@SessionID
			)
			IF @@Error <> 0 GOTO RollbackAndExit

			FETCH NEXT
			FROM	curTerms1
			INTO	@ConceptKeyForCursor

			IF @@Error <> 0 GOTO RollbackAndExit
		END
		
		CLOSE curTerms1
		DEALLOCATE curTerms1	
		
		IF @@Error <> 0 GOTO RollbackAndExit

		IF @Unidirectional = 0
		BEGIN
			/*--------------------------------------------------------*\
			  Duplicate the To terms as concepts with those terms, 
			  but the From meaning and Term_Version label.
			\*--------------------------------------------------------*/ 
			SELECT 	@MeaningKey1 = Meaning_Key,
				@TermVersionKey1 = Term_Version_Key
			FROM	Concept
			WHERE	Concept_Key = @FromConceptKey
		
			IF @@Error <> 0 GOTO RollbackAndExit
	
			FETCH NEXT
			FROM	curTerms2
			INTO	@ConceptKeyForCursor
	
			IF @@Error <> 0 GOTO RollbackAndExit
	
			WHILE @@Fetch_Status = 0
			BEGIN
				SELECT	@ConceptGroupKey = Concept_Group_Key,
					@TermVersionKey = Term_Version_Key,
					@TermKey = Term_Key,
					@ListPreferred = List_Preferred,
					@IsCurrent = Is_Current,
					@Preferred = Preferred,
					@ConceptRankKey = Concept_Rank_Key,
					@NameTypeConceptKey = Name_Type_Concept_Key,
					@MeaningKey = Meaning_Key,
					@AuthorCopy = Author_Copy,
					@SortCode = Sort_Code,
					@ListCode = List_Code
				FROM	Concept
				WHERE	Concept_Key = @ConceptKeyForCursor
		
				EXECUTE spNextKey 'Concept', @Key OUTPUT
				IF @@Error <> 0 GOTO RollbackAndExit
	
				INSERT INTO Concept (
					Concept_Key,
					Meaning_Key,
					Concept_Group_Key,
					Term_Key,
					Term_Version_Key,
					List_Preferred,
					Is_Current,
					Preferred,
					Concept_Rank_Key,
					Name_Type_Concept_Key,
					Author_Copy,
					Sort_Code,
					List_Code,
					Entered_Session_ID
				) VALUES (
					@Key,
					@MeaningKey1,
					@ConceptGroupKey,
					@TermKey,
					@TermVersionKey1,
					0,
					@IsCurrent,
					@Preferred,
					@ConceptRankKey,
					@NameTypeConceptKey,
					@AuthorCopy,
					@SortCode,
					@ListCode,
					@SessionID
				)
			
				IF @@Error <> 0 GOTO RollbackAndExit
	
				FETCH NEXT
				FROM	curTerms2
				INTO	@ConceptKeyForCursor
	
				IF @@Error <> 0 GOTO RollbackAndExit
			END
			
			CLOSE curTerms2
			DEALLOCATE curTerms2		
	
			IF @@Error <> 0 GOTO RollbackAndExit
		END

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO	


SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_MeaningsShareTermsClone_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_MeaningsShareTermsClone_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_MeaningsShareTermsClone_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_MeaningsShareTermsClone_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_MeaningsShareTermsClone_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_MeaningsShareTermsClone_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_MeaningsShareTermsClone_Update TO [Dev - JNCC SQL]
END

GO
			