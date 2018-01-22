/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_SemanticRelation_Insert]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_SemanticRelation_Insert]
GO

/*===========================================================================*\
  Description:	Inserts a record into the Semantic_Relation table

  Parameters:	@Key (output),
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
		@SystemSuppliedData (optional)

  Created:	December 2003

  Last revision information:
    $Revision: 3 $
    $Date: 10/02/04 15:49 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SemanticRelation_Insert]
	@Key char(16) OUTPUT,
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
	@SystemSuppliedData bit = NULL	

AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	/*-------------------------------------------------------------*\
	  Get a new key.
	\*-------------------------------------------------------------*/
	EXECUTE spNextKey 'Semantic_Relation', @Key OUTPUT

	/*-------------------------------------------------------------*\
	  Wrap everything in one transaction.
	\*-------------------------------------------------------------*/
	BEGIN TRANSACTION

		/*-------------------------------------------------------------*\
		  Insert in Semantic_Relation.
		\*-------------------------------------------------------------*/
		INSERT INTO Semantic_Relation (
			Semantic_Relation_Key, 
			Item_Name, 
			Unidirectional, 
			Forward_Equivalence_Possible,
			Forward_Equivalence_Definite,
			Reverse_Equivalence_Possible,
			Reverse_Equivalence_Definite,
			Proportional_Relationship,
			Chronological_Overlap,
			Adjacent,
			[Description],
			Entered_Session_ID,
			System_Supplied_Data
		) VALUES (
			@Key, 
			@ItemName, 
			@Unidirectional, 
			@ForwardEquivalencePossible,
			@ForwardEquivalenceDefinite,
			@ReverseEquivalencePossible,
			@ReverseEquivalenceDefinite,
			IsNull(@ProportionalRelationship, 0),
			@ChronologicalOverlap,
			@Adjacent,
			@Description,
			@SessionID,
			IsNull(@SystemSuppliedData, 0)
		)
		IF @@Error <> 0 GOTO RollbackAndExit

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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SemanticRelation_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SemanticRelation_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SemanticRelation_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SemanticRelation_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SemanticRelation_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SemanticRelation_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SemanticRelation_Insert TO [Dev - JNCC SQL]
END

GO