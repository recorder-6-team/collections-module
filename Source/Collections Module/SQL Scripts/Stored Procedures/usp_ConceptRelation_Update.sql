/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptRelation_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptRelation_Update]
GO

/*===========================================================================*\
  Description:	Updates a record in the Concept_Relation table.

  Parameters:	@Key
		@FromConceptKey
		@ToConceptKey
		@ThesaurusRelationTypeKey
		@Multiplicity
		@Inherited
		@Comment
		@SessionID
		@RecordsAffected
		@Timestamp

  Created:	December 2003

  Last revision information:
    $Revision: 8 $
    $Date: 2/02/09 17:59 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptRelation_Update]
	@Key char(16),
	@FromConceptKey char(16),
	@ToConceptKey char(16),
	@ThesaurusRelationTypeKey char(16),
	@Multiplicity float = NULL,
	@Inherited bit = NULL,
	@Comment text = NULL,
	@SessionID char(16),
	@RecordsAffected INT OUTPUT,
	@Timestamp timestamp
AS
	SET NOCOUNT ON

	/*-------------------------------------------------------------*\
	  Wrap everything in one transaction.
	\*-------------------------------------------------------------*/
	BEGIN TRANSACTION

		/*-------------------------------------------------------------*\
		  Update record in Concept_Relation.
		\*-------------------------------------------------------------*/
		DECLARE		@old_from_concept_key		CHAR(16),
					@old_to_concept_key			CHAR(16),
					@old_relation_type_key		CHAR(16),
					@error						INT

		UPDATE	Concept_Relation
		SET @old_from_concept_key = From_Concept_Key,
			From_Concept_Key = @FromConceptKey,
			@old_to_concept_key = To_Concept_Key,
			To_Concept_Key = @ToConceptKey,
			@old_relation_type_key = Thesaurus_Relation_Type_Key,
			Thesaurus_Relation_Type_Key = @ThesaurusRelationTypeKey,
			Multiplicity = @Multiplicity,
			Inherited = IsNull(@Inherited, 0),
			Comment = @Comment,
			Changed_Session_ID = @SessionID
		WHERE	Concept_Relation_Key = @Key
		AND		[Timestamp] = @Timestamp

		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Concept_Relation WHERE Concept_Relation_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

		/*-------------------------------------------------------------*\
		  Make corresponding changes in Concept_Lineage
		\*-------------------------------------------------------------*/
		EXECUTE		usp_ConceptLineage_UpdateRelation	@Key,
														@old_from_concept_key,
														@old_to_concept_key,
														@old_relation_type_key
		IF @@ERROR <> 0 GOTO RollbackAndExit

	/*-------------------------------------------------------------*\
	  All went well, so commit.
	\*-------------------------------------------------------------*/
	COMMIT TRANSACTION

RETURN 0

RollBackAndExit: 
	/*-------------------------------------------------------------*\
	  Cancel any changes, or left-overs might mess up some tables.
	\*-------------------------------------------------------------*/
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_ConceptRelation_Update failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptRelation_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptRelation_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptRelation_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptRelation_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptRelation_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptRelation_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptRelation_Update TO [Dev - JNCC SQL]
END

GO
			