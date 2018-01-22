/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ThesaurusRelationType_Insert]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ThesaurusRelationType_Insert]
GO

/*===========================================================================*\
  Description:	Inserts a record into the Thesaurus_Relation_Type table.

  Parameters:	@SemanticRelationKey 
		@ItemName
		@ForwardTerm 
		@ReverseTerm 
		@SessionID 
		@SystemSuppliedData (optional)
		@Meaning (optional)
		@Concept (optional) 
		@TermVersion (optional) 
		@Occurrence (optional) 
		@CollectionUnit (optional) 

  Created:	December 2003

  Last revision information:
    $Revision: 3 $
    $Date: 8/12/03 12:08 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ThesaurusRelationType_Insert]
	@Key char(16) OUTPUT,
	@SemanticRelationKey char(16),
	@ItemName varchar(100),
	@ForwardTerm varchar(100),
	@ReverseTerm varchar(100),
	@SessionID char(16),
	@SystemSuppliedData bit = NULL,
	@Meaning bit = NULL,
	@Concept bit = NULL,
	@TermVersion bit = NULL,
	@Occurrence bit = NULL,
	@CollectionUnit bit = NULL	
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	EXECUTE spNextKey 'Thesaurus_Relation_Type', @Key OUTPUT

	BEGIN TRANSACTION

		/*-------------------------------------------------------------*\
		  Inserts a record into the Thesaurus_Relation_Type table.
		\*-------------------------------------------------------------*/
		INSERT INTO Thesaurus_Relation_Type (
			Thesaurus_Relation_Type_Key,
			Semantic_Relation_Key,
			Item_Name,
			Forward_Term,
			Reverse_Term,
			Entered_Session_ID,
			System_Supplied_Data
		) VALUES (
			@Key,
			@SemanticRelationKey,
			@ItemName,
			@ForwardTerm,
			@ReverseTerm,
			@SessionID,
			ISNULL(@SystemSuppliedData, 0)
		)
		IF @@Error <> 0 GOTO RollbackAndExit

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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ThesaurusRelationType_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ThesaurusRelationType_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ThesaurusRelationType_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ThesaurusRelationType_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ThesaurusRelationType_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ThesaurusRelationType_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ThesaurusRelationType_Insert TO [Dev - JNCC SQL]
END
GO