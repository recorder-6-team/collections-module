/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_SemanticRelation_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_SemanticRelation_Update]
GO

/*===========================================================================*\
  Description:	Updates a record in the Semantic_Relation table

  Parameters:	@Key,
		@ItemName 
		@Unidirectional 
		@ForwardEquivalencePossible
		@ForwardEquivalenceDefinite
		@ReverseEquivalencePossible 
		@ReverseEquivalenceDefinite
		@ProportionalRelationship
		@Adjacent 
		@Description 
		@SessionID 
		@Timestamp

  Created:	December 2003

  Last revision information:
    $Revision: 5 $
    $Date: 3/02/09 10:25 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SemanticRelation_Update]
	@Key char(16),
	@ItemName varchar(100),
	@Unidirectional bit,
	@ForwardEquivalencePossible bit,
	@ForwardEquivalenceDefinite bit,
	@ReverseEquivalencePossible bit,
	@ReverseEquivalenceDefinite bit,
	@ProportionalRelationship bit = NULL,
	@ChronologicalOverlap int = NULL,
	@Adjacent bit,
	@Description text,
	@SessionID char(16),
	@Timestamp timestamp

AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	/*-------------------------------------------------------------*\
	  Wrap everything in one transaction.
	\*-------------------------------------------------------------*/
	BEGIN TRANSACTION

		/*-------------------------------------------------------------*\
		  Update in Semantic_Relation.
		\*-------------------------------------------------------------*/
		UPDATE 	Semantic_Relation 
		SET	Item_Name = @ItemName, 
			Unidirectional = @Unidirectional, 
			Forward_Equivalence_Possible = @ForwardEquivalencePossible,
			Forward_Equivalence_Definite = @ForwardEquivalenceDefinite,
			Reverse_Equivalence_Possible = @ReverseEquivalencePossible,
			Reverse_Equivalence_Definite = @ReverseEquivalenceDefinite,
			Proportional_Relationship = IsNull(@ProportionalRelationship, 0),
			Chronological_Overlap = @ChronologicalOverlap,
			Adjacent = @Adjacent,
			[Description] = @Description,
			Entered_Session_ID = @SessionID

		WHERE	Semantic_Relation_Key = @Key
		AND		[Timestamp] = @Timestamp

		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Semantic_Relation WHERE Semantic_Relation_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

	/*-------------------------------------------------------------*\
	  All went well, so commit.
	\*-------------------------------------------------------------*/
	COMMIT TRANSACTION

RETURN 0

RollBackAndExit: 
	/*-------------------------------------------------------------*\
	  Cancel any changes, or left-overs might mess up some tables.
	\*-------------------------------------------------------------*/
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SemanticRelation_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SemanticRelation_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SemanticRelation_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SemanticRelation_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SemanticRelation_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SemanticRelation_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SemanticRelation_Update TO [Dev - JNCC SQL]
END

GO