/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptSimple_Update') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_ConceptSimple_Update]
GO

/*===========================================================================*\
  Description:	Updates a record in the concept table.

  Parameters:	@Key
		@TermKey 
		@ConceptGroupKey 
		@Preferred
		@ConceptRankKey 
		@NameTypeConceptKey
		@SortCode 
		@ListCode 
		@SessionID 
		@RecordsAffected 
		@Timestamp 

  Created:	December 2003

  Last revision information:
    $Revision: 14 $
    $Date: 5/10/12 9:22 $
    $Author: Alexanderpadley $

\*===========================================================================*/
CREATE PROCEDURE [usp_ConceptSimple_Update]
	@Key char(16),
	@TermKey char(16),
	@AuthorAndDate varchar(100) = NULL,
	@Attributes varchar(100) = NULL,
	@NewTermVersionNeeded bit = 0,
	@ConceptGroupKey char(16),
	@ListPreferred bit = NULL,
	@Preferred bit,
	@ConceptRankKey char(16),
	@NameTypeConceptKey char(16) = NULL,
	@SortCode int,
	@ListCode varchar(50),
	@PublishedTerm nvarchar(450) = NULL,
	@AutomaticPublishedTerm bit,
	@TermGeneratorKey char(16),
	@SessionID char(16),
	@RecordsAffected int OUTPUT,
	@Timestamp timestamp,
	@AcknowledgeUpdate bit
AS
SET NOCOUNT OFF
	
	BEGIN TRANSACTION
		-- Check record has not been updated by another user
		IF EXISTS( SELECT * 
					FROM Concept
					WHERE Concept_Key = @Key and @timestamp = Timestamp)
		BEGIN
			-- VI 13430 - CCN178 - TSEQUAL and stored procs
			IF @RecordsAffected = 0 AND EXISTS (
				SELECT Concept_Key FROM Concept WHERE Concept_Key = @Key
			)
			BEGIN
				RAISERROR('Record updated by another user', 16, 1)
			END
		END

		/*--------------------------------------------------*\
			Update/create new term version
		\*--------------------------------------------------*/
		DECLARE @TermVersionKey CHAR(16)

		SELECT @TermVersionKey = Term_Version_Key
		FROM Concept
		WHERE Concept_Key = @Key

		IF @TermVersionKey IS NULL OR @NewTermVersionNeeded = 1
		BEGIN
			EXEC usp_TermVersion_Insert
				@Key = @TermVersionKey OUTPUT,
				@ConceptKey = @Key,
				@VersionLabel = @Attributes,
				@AuthorAndDate = @AuthorAndDate,
				@SessionID = @SessionID
			IF @@Error <> 0 GOTO RollbackAndExit
		END
		ELSE
		BEGIN
			EXEC usp_TermVersion_Update
				@Key = @TermVersionKey,
				@ConceptKey = @Key,
				@VersionLabel = @Attributes,
				@AuthorAndDate = @AuthorAndDate,
				@SessionID = @SessionID	

			IF @@Error <> 0 GOTO RollbackAndExit	

			-- Term version may have changed, so make sure we have the right key
			SELECT @TermVersionKey = Term_Version_Key
			FROM Concept
			WHERE Concept_Key = @Key 	
		END		
		
		/*-------------------*\
		  Update the Concept.
		\*-------------------*/
		DECLARE @old_concept_group_key CHAR(16),
			@old_list_preferred BIT,
			@OldConceptRankKey char(16),
			@error INT

		DECLARE @Plaintext VARCHAR(100)

		SELECT @Plaintext = Plaintext
		FROM Term
		WHERE Term_Key = @TermKey
		
		UPDATE	Concept
		SET 	@old_concept_group_key = Concept_Group_Key,
			@old_list_preferred = List_Preferred,
			@OldConceptRankKey = Concept_Rank_Key,
			List_Preferred = IsNull(@ListPreferred, List_Preferred),
			Concept_Group_Key = @ConceptGroupKey,
			Term_Key = @TermKey,
			Term_Version_Key = @TermVersionKey,
			Concept_Rank_Key = @ConceptRankKey,
			Preferred = @Preferred,
			Name_Type_Concept_Key = @NameTypeConceptKey,
			Sort_Code = @SortCode,
			List_Code = @ListCode,
			Published_Term = ISNULL(@PublishedTerm, @Plaintext),
			Automatic_Published_Term = @AutomaticPublishedTerm,
			Term_Generator_Key = @TermGeneratorKey,
			Changed_Session_ID = @SessionID,
			AcknowledgeUpdate = @AcknowledgeUpdate			
		WHERE	Concept_Key = @Key

		SELECT	@error = @@ERROR,
			@RecordsAffected = @@ROWCOUNT

		IF @error <> 0 GOTO RollbackAndExit		

		/*----------------------------------------------------------------------------*\
		  Make corresponding changes in Concept_Lineage
		\*----------------------------------------------------------------------------*/
		EXECUTE	usp_ConceptLineage_ConceptUpdated	@Key,
								@old_concept_group_key,
								@old_list_preferred
		IF @@ERROR <> 0 GOTO RollbackAndExit
		
		/*----------------------------------------------------------------------------*\
		  If @Preferred = 1, then make sure the updated concept is the 
		  only Preferred synonym with the same language key and name type concept key.
		\*----------------------------------------------------------------------------*/
		IF @Preferred = 1 
			UPDATE		CSynonyms
			SET		Preferred = 0
			FROM 		Concept AS CSource
			INNER JOIN	Term 	AS TSource 	ON TSource.Term_Key = CSource.Term_Key
			INNER JOIN	Concept AS CSynonyms 	ON CSynonyms.Meaning_Key = CSource.Meaning_Key
								AND CSynonyms.Name_Type_Concept_Key = CSource.Name_Type_Concept_Key
								AND CSynonyms.Concept_Key <> CSource.Concept_Key
			INNER JOIN	Term 	AS TSynonyms 	ON TSynonyms.Term_Key = CSynonyms.Term_Key
								AND TSynonyms.Language_Key = TSource.Language_Key
			WHERE		CSource.Concept_Key = @Key

		IF @@Error <> 0 GOTO RollbackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit:
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_ConceptSimple_Update failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptSimple_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptSimple_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptSimple_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptSimple_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptSimple_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptSimple_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptSimple_Update TO [Dev - JNCC SQL]
END
GO