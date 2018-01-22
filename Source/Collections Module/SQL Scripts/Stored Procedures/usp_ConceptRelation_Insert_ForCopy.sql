/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptRelation_Insert_ForCopy]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptRelation_Insert_ForCopy]
GO

/*===========================================================================*\
  Description:	When a concept with child nodes is copied to the clipboard
		and then pasted, the duplicated node should have the same 
		children as the original node that was copied. This proc.
		replicates the ConceptRelation records that give the 
		original concept the links to its children.

  Parameters:	

  Created:	February 2004

  Last revision information:
    $Revision: 2 $
    $Date: 24/02/04 15:26 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptRelation_Insert_ForCopy]
	@OriginalConceptKey char(16),
	@PastedConceptKey char(16),
	@SessionID char(16)
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	DECLARE @OriginalConceptRelationKey char(16),
		@NewConceptRelationKey char(16)

	DECLARE curRelations CURSOR LOCAL FAST_FORWARD FOR
		SELECT	Concept_Relation_Key
		FROM	Concept_Relation
		WHERE	From_Concept_Key = @OriginalConceptKey

	/*-------------------------------------------------------------*\
	  Wrap everything in one transaction.
	\*-------------------------------------------------------------*/
	BEGIN TRANSACTION

		OPEN curRelations

		FETCH NEXT 
		FROM	curRelations
		INTO	@OriginalConceptRelationKey
	
		WHILE @@Fetch_Status = 0
		BEGIN
			EXECUTE spNextKey 'Concept_Relation', @NewConceptRelationKey OUTPUT
			
			INSERT INTO Concept_Relation (
				Concept_Relation_Key,
				From_Concept_key,
				To_Concept_Key,
				Thesaurus_Relation_Type_Key,
				Multiplicity,
				Inherited,
				Comment,
				System_Supplied_Data,
				Entered_Session_ID
			) SELECT @NewConceptRelationKey,
				@PastedConceptKey,
				To_Concept_Key,
				Thesaurus_Relation_Type_Key,
				Multiplicity,
				Inherited,
				Comment,
				System_Supplied_Data,
				@SessionID
			FROM Concept_Relation 
			WHERE Concept_Relation_Key = @OriginalConceptRelationKey

			IF @@ERROR <> 0 GOTO RollbackAndExit	

			FETCH NEXT 
			FROM	curRelations
			INTO 	@OriginalConceptRelationKey	
		END

		CLOSE curRelations
		DEALLOCATE curRelations

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit:
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptRelation_Insert_ForCopy') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptRelation_Insert_ForCopy'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptRelation_Insert_ForCopy TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptRelation_Insert_ForCopy TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptRelation_Insert_ForCopy TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptRelation_Insert_ForCopy TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptRelation_Insert_ForCopy TO [Dev - JNCC SQL]
END

GO
			