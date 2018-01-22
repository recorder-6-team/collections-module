/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ThesaurusRelationType_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ThesaurusRelationType_Update]
GO

/*===========================================================================*\
  Description:	Updates a record in the Thesaurus_Relation_Type table.

  Parameters:	@Key	(Thesaurus_Relation_Type_Key)
		@SemanticRelationKey 
		@ItemName
		@ForwardTerm 
		@ReverseTerm 
		@SessionID 
		@Timestamp
		@Meaning (optional)
		@Concept (optional) 
		@TermVersion (optional)
		@Occurrence (optional) 
		@CollectionUnit (optional) 

  Created:	December 2003

  Last revision information:
    $Revision: 5 $
    $Date: 3/02/09 10:54 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ThesaurusRelationType_Update]
	@Key char(16),
	@SemanticRelationKey char(16),
	@ItemName varchar(100),
	@ForwardTerm varchar(100),
	@ReverseTerm varchar(100),
	@SessionID char(16),
	@Timestamp timestamp,
	@Meaning bit = NULL,
	@Concept bit = NULL,
	@TermVersion bit = NULL,
	@Occurrence bit = NULL,
	@CollectionUnit bit = NULL			
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION

		UPDATE	Thesaurus_Relation_Type
		SET	Semantic_Relation_Key = @SemanticRelationKey,
			Item_Name = @ItemName,
			Forward_Term = @ForwardTerm,
			Reverse_Term = @ReverseTerm,
			Changed_Session_ID = @SessionID
		WHERE	Thesaurus_Relation_Type_Key = @Key
		AND		[Timestamp] = @Timestamp

		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Thesaurus_Relation_Type WHERE Thesaurus_Relation_Type_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

		IF @Meaning = 1 EXEC usp_ThesaurusRelationTypeUsage_Insert @Key, 1, @SessionID
		ELSE EXEC usp_ThesaurusRelationTypeUsage_Delete @Key, 1
		IF @Concept = 1 EXEC usp_ThesaurusRelationTypeUsage_Insert @Key, 2, @SessionID
		ELSE EXEC usp_ThesaurusRelationTypeUsage_Delete @Key, 2
		IF @TermVersion = 1 EXEC usp_ThesaurusRelationTypeUsage_Insert @Key, 3, @SessionID
		ELSE EXEC usp_ThesaurusRelationTypeUsage_Delete @Key, 3
		IF @Occurrence = 1 EXEC usp_ThesaurusRelationTypeUsage_Insert @Key, 4, @SessionID
		ELSE EXEC usp_ThesaurusRelationTypeUsage_Delete @Key, 4
		IF @CollectionUnit = 1 EXEC usp_ThesaurusRelationTypeUsage_Insert @Key, 5, @SessionID
		ELSE EXEC usp_ThesaurusRelationTypeUsage_Delete @Key, 5

		IF @@Error <> 0 GOTO RollbackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ThesaurusRelationType_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ThesaurusRelationType_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ThesaurusRelationType_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ThesaurusRelationType_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ThesaurusRelationType_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ThesaurusRelationType_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ThesaurusRelationType_Update TO [Dev - JNCC SQL]
END
GO