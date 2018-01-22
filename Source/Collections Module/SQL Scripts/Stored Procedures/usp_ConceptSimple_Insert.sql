/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptSimple_Insert]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptSimple_Insert]
GO

/*===========================================================================*\
  Description:	Inserts a new concept and term (if necessary).

  Parameters:	@Key OUTPUT,
		@TermKey 
		@ConceptGroupKey 
		@TermVersionKey 
		@ListPreferred 
		@IsCurrent
		@Preferred 
		@ConceptRankKey 
		@NameTypeConceptKey 
		@MeaningKey 
		@AuthorCopy 
		@SortCode 
		@ListCode
		@SessionID 
		@SystemSuppliedData 

  Created:	December 2003

  Last revision information:
    $Revision: 11 $
    $Date: 5/10/12 9:22 $
    $Author: Alexanderpadley $

\*===========================================================================*/

CREATE PROCEDURE [usp_ConceptSimple_Insert]
	@Key char(16) OUTPUT,
	@TermKey char(16),
	@ConceptGroupKey char(16),
	@AuthorAndDate varchar(100) = NULL,
	@Attributes varchar(100) = NULL,
	@ListPreferred bit = NULL,
	@IsCurrent bit = NULL,
	@Preferred bit = NULL,
	@ConceptRankKey char(16) = NULL,
	@NameTypeConceptKey char(16) = NULL,
	@MeaningKey char(16) = NULL,
	@AuthorCopy varchar(100) = NULL,
	@SortCode int = NULL,
	@ListCode varchar(50) = NULL,
	@PublishedTerm nvarchar(450),
	@AutomaticPublishedTerm bit = 1,
	@TermGeneratorKey char(16),
	@SessionID char(16),
	@SystemSuppliedData bit = NULL
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
		DECLARE @NewMeaningKey char(16),
				@TermVersionKey char(16)
		
		SET @ListPreferred = IsNull(@ListPreferred, 1)
	
		/*-------------------------------------------------------------*\
		  If we don't have a meaning key, create one.
		\*-------------------------------------------------------------*/
		IF @MeaningKey IS NULL
		BEGIN
			EXEC spNextKey 'Meaning', @NewMeaningKey OUTPUT--, @SiteID
			IF @@ERROR <> 0 GOTO RollbackAndExit
			
			INSERT INTO Meaning (
				Meaning_Key
			) VALUES (
				@NewMeaningKey
			)
			IF @@Error <> 0 GOTO RollbackAndExit
		END
		ELSE
			SET @NewMeaningKey = @MeaningKey

		/*-------------------------------------------------------------*\
			Create new term version
		\*-------------------------------------------------------------*/

		EXEC usp_TermVersion_Insert
			@Key = @TermVersionKey OUTPUT,
			@ConceptKey = '',
			@TermKey = @TermKey,
			@VersionLabel = @Attributes,
			@AuthorAndDate = @AuthorAndDate,
			@SessionID = @SessionID,
			@SystemSuppliedData = @SystemSuppliedData
		IF @@Error <> 0 GOTO RollbackAndExit		
	
		/*-------------------------------------------------------------*\
		  Create new Concept.
		\*-------------------------------------------------------------*/
		EXEC spNextKey 'Concept', @Key OUTPUT
		IF @@ERROR <> 0 GOTO RollbackAndExit

		DECLARE @Plaintext VARCHAR(100)

		SELECT @Plaintext = Plaintext
		FROM Term
		WHERE Term_Key = @TermKey
		
		INSERT INTO Concept (
			Concept_Key, Term_Key, Concept_Group_Key, List_Preferred, 
			Is_Current, Preferred, Concept_Rank_Key, Name_Type_Concept_Key, 
			Meaning_Key, Author_Copy, Sort_Code, List_Code,
			Published_Term, Automatic_Published_Term, Term_Generator_Key,
			Entered_Session_ID, System_Supplied_Data, Term_Version_Key, AcknowledgeUpdate
		) VALUES (
			@Key, @TermKey, @ConceptGroupKey, @ListPreferred, 
			IsNull(@IsCurrent, 1), IsNull(@Preferred, 0), @ConceptRankKey,
			@NameTypeConceptKey, @NewMeaningKey, @AuthorCopy, @SortCode,
			@ListCode, ISNULL(@PublishedTerm, @Plaintext), @AutomaticPublishedTerm,
			@TermGeneratorKey, @SessionID, 0, @TermVersionKey, 1
		)
		IF @@Error <> 0 GOTO RollbackAndExit
	
		/*-------------------------------------------------------------*\
		  Create Concept_Lineage.
		\*-------------------------------------------------------------*/
		EXECUTE		usp_ConceptLineage_NewConcept	@Key
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
	RAISERROR ('usp_ConceptSimple_Insert failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptSimple_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptSimple_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
			GRANT EXECUTE ON dbo.usp_ConceptSimple_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptSimple_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptSimple_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptSimple_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptSimple_Insert TO [Dev - JNCC SQL]
END
GO