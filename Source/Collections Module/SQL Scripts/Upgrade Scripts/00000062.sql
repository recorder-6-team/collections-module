SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

/*============================================================================*\
	Add Acknowledge Update column to Concept table
\*============================================================================*/

IF NOT EXISTS (SELECT * FROM SYS.COLUMNS WHERE Name = N'AcknowledgeUpdate' AND Object_ID = Object_ID('Concept'))
BEGIN
	ALTER TABLE Concept
	ADD AcknowledgeUpdate BIT NOT NULL DEFAULT 1
END
GO

/*============================================================================*\
	Alter Concept Insert and Update Procedures
\*============================================================================*/
IF OBJECT_ID(N'usp_ConceptSimple_Insert') IS NOT NULL
	DROP PROCEDURE usp_ConceptSimple_Insert
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
    $Revision: 2 $
    $Date: 5/10/12 11:17 $
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

IF OBJECT_ID(N'usp_ConceptSimple_Update') IS NOT NULL
	DROP PROCEDURE usp_ConceptSimple_Update
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
		@AcknowledgeUpdate

  Created:	December 2003

  Last revision information:
    $Revision: 2 $
    $Date: 5/10/12 11:17 $
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

/*===========================================================================*\
  Description:	Returns fields from the Concept table.

  Parameters:	@ConceptKey

  Created:	December 2003

  Last revision information:
    $Revision: 2 $
    $Date: 5/10/12 11:17 $
    $Author: Alexanderpadley $

\*===========================================================================*/
ALTER PROCEDURE [dbo].[usp_Concept_Select]
	@ConceptKey char(16)
AS
	SET NOCOUNT ON
	SET ANSI_NULLS ON
	SET ANSI_PADDING ON
	SET ANSI_WARNINGS ON
	SET CONCAT_NULL_YIELDS_NULL ON
	SET QUOTED_IDENTIFIER ON
	SET NO_BROWSETABLE OFF

	SELECT	
			C.Term_Key AS Item_Key,
			C.Published_Term AS Item_Name,
			TL.Plaintext,
			TV.Version_Label,
			TV.Author_And_Date,
			Lower(TL.Language_Key) AS Language_Key,
			L.Language_Key + ' - ' + L.Item_Name AS Language_Name,
			C.Concept_Group_Key,
			CG.Hierarchy_Relation_Type_Key,
			C.Term_Version_Key,	
			C.List_Preferred,
			C.Is_Current,
			C.Preferred,
			C.Concept_Rank_Key,
			CR.Item_Name AS Concept_Rank_Name,
			C.Name_Type_Concept_Key,
			CT.Item_Name AS Name_Type_Concept_Name,
			C.Meaning_Key,
			C.Author_Copy,
			C.Sort_Code,
			C.List_Code,
			C.Automatic_Published_Term,
			C.Term_Generator_Key,
			TG.Item_Name AS Term_Generator_Name,
			CASE WHEN CRel.Concept_Relation_Key IS NULL THEN 0 ELSE 1 END AS HasChildren,
			C.System_Supplied_Data,
			C.Entered_Session_ID,
			C.[Timestamp],
			C.AcknowledgeUpdate
	FROM 		Concept AS C
	INNER JOIN	Term AS TL ON TL.Term_Key = C.Term_Key
	INNER JOIN	Language AS L ON L.Language_Key = TL.Language_Key
	LEFT JOIN	Concept_Rank AS CR ON CR.Concept_Rank_Key = C.Concept_Rank_Key
	INNER JOIN	VW_ConceptTerm AS CT ON CT.Concept_Key = C.Name_Type_Concept_Key
	INNER JOIN	Concept_Group AS CG ON CG.Concept_Group_Key = C.Concept_Group_Key
	LEFT JOIN 	Concept_Relation CRel ON CRel.From_Concept_Key = C.Concept_Key
       				     	     AND CRel.Thesaurus_Relation_Type_Key = CG.Hierarchy_Relation_Type_Key
	LEFT JOIN	Term_Version TV ON TV.Term_Version_Key = C.Term_Version_Key
	LEFT JOIN	Term_Generator TG ON TG.Term_Generator_Key = C.Term_Generator_Key
	WHERE		C.Concept_Key = @ConceptKey
GO