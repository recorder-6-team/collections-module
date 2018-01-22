/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptRelation_Update_ForCut]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptRelation_Update_ForCut]
GO

/*===========================================================================*\
  Description:	Updates a record in the Concept_Relation table.

  Parameters:	@OldParentConceptKey
				@NewParentConceptKey
				@ToConceptKey
				@RecordsAffected

  Created:	January 2004

  Last revision information:
    $Revision: 3 $
    $Date: 12/01/04 10:00 $
    $Author: Andrewkemp $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptRelation_Update_ForCut]
	@OldParentConceptKey char(16),
	@NewParentConceptKey char(16),
	@ToConceptKey char(16),
	@RecordsAffected INT OUTPUT
AS
	SET NOCOUNT ON

	/*-------------------------------------------------------------*\
	  Wrap everything in one transaction.
	\*-------------------------------------------------------------*/
	BEGIN TRANSACTION

		DECLARE		@relations	TABLE (
						Concept_Relation_Key	CHAR(16)	PRIMARY KEY,
			   			Relation_Type_Key		CHAR(16))

		INSERT		@relations
		SELECT		Concept_Relation_Key,
					Thesaurus_Relation_Type_Key
		FROM		Concept_Relation
		WHERE		From_Concept_Key			=	@OldParentConceptKey
		AND			To_Concept_Key				=	@ToConceptKey

		IF @@ERROR <> 0 GOTO RollbackAndExit

		/*-------------------------------------------------------------*\
		  Update record in Concept_Relation.
		\*-------------------------------------------------------------*/
		DECLARE		@error		INT

		UPDATE	Concept_Relation
		SET	From_Concept_Key = @NewParentConceptKey
		FROM	Concept_Relation
		WHERE	From_Concept_Key = @OldParentConceptKey
		AND	To_Concept_Key = @ToConceptKey

		SELECT		@error				=	@@ERROR,
					@RecordsAffected	=	@@ROWCOUNT

		IF @error <> 0 GOTO RollbackAndExit

		/*-------------------------------------------------------------*\
		  Make corresponding changes in Concept_Lineage
		\*-------------------------------------------------------------*/
		DECLARE		@relation_key		CHAR(16),
					@relation_type_key	CHAR(16)

		DECLARE		relations			CURSOR LOCAL FAST_FORWARD FOR
		SELECT		Concept_Relation_Key,
					Relation_Type_Key
		FROM		@relations

		OPEN		relations

		WHILE 1 = 1
		BEGIN
			FETCH		relations
			INTO		@relation_key,
						@relation_type_key

			IF @@FETCH_STATUS <> 0 BREAK

			EXECUTE		usp_ConceptLineage_UpdateRelation	@relation_key,
															@OldParentConceptKey,
															@ToConceptKey,
															@relation_type_key
			IF @@ERROR <> 0 GOTO CloseRollbackAndExit
		END

		CLOSE		relations

	/*-------------------------------------------------------------*\
	  All went well, so commit.
	\*-------------------------------------------------------------*/
	COMMIT TRANSACTION
	RETURN 0

CloseRollBackAndExit:
	CLOSE		relations

RollBackAndExit:
	/*-------------------------------------------------------------*\
	  Cancel any changes, or left-overs might mess up some tables.
	\*-------------------------------------------------------------*/
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_ConceptRelation_Update_ForCut failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptRelation_Update_ForCut') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptRelation_Update_ForCut'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptRelation_Update_ForCut TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptRelation_Update_ForCut TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptRelation_Update_ForCut TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptRelation_Update_ForCut TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptRelation_Update_ForCut TO [Dev - JNCC SQL]
END

GO
			