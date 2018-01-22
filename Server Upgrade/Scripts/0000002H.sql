If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Concept_RecursionCheck_Get]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Concept_RecursionCheck_Get]
GO

/*===========================================================================*\
  Description:  Checks that the user isn't trying to create a circular
        Concept_Relation.

  Parameters:   @PotentialChildKey - key of dragged node.
                @PotentialParentKey - key of target node.
                @RecursionExists - if cycle exists (i.e. a problem) return 1
                        else return 0 (i.e. OK)

  Created:  March 2004

  Last revision information:
    $Revision: 1 $
    $Date: 26/11/07 12:26 $
    $Author: Ericsalmon $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_Concept_RecursionCheck_Get] 
	@PotentialChildKey 	CHAR(16),
	@PotentialParentKey CHAR(16),
	@RecursionExists 	BIT OUTPUT
AS
    SET NOCOUNT ON

    IF @PotentialChildKey = @PotentialParentKey
        SET	@RecursionExists    =   1
    ELSE
    BEGIN
        SELECT 		@RecursionExists = MAX(CASE WHEN LP.Lineage LIKE LC.Lineage + '\%' THEN 1 ELSE 0 END)
        FROM 		Concept_Lineage LC
		JOIN		Concept			CC 	ON	CC.Concept_Key			=	LC.Concept_Key
        CROSS JOIN 	Concept_Lineage LP
		JOIN		Concept			CP	ON	CP.Concept_Key			= 	LP.Concept_Key
										AND	CP.Concept_Group_Key	= 	CC.Concept_Group_Key
        WHERE 		LC.Concept_Key	=	@PotentialChildKey
        AND 		LP.Concept_Key 	= 	@PotentialParentKey

        IF @RecursionExists IS NULL
            SET	@RecursionExists    =   0
    END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_RecursionCheck_Get') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_Concept_RecursionCheck_Get'
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
            GRANT EXECUTE ON dbo.usp_Concept_RecursionCheck_Get TO [R2k_AddOnly]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
        GRANT EXECUTE ON dbo.usp_Concept_RecursionCheck_Get TO [R2k_Administrator]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
        GRANT EXECUTE ON dbo.usp_Concept_RecursionCheck_Get TO [R2k_FullEdit]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
        GRANT EXECUTE ON dbo.usp_Concept_RecursionCheck_Get TO [R2k_ReadOnly]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
        GRANT EXECUTE ON dbo.usp_Concept_RecursionCheck_Get TO [R2k_RecordCardsOnly]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_Concept_RecursionCheck_Get TO [Dev - JNCC SQL]
END

GO


/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptRelation_Insert]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptRelation_Insert]
GO

/*===========================================================================*\
  Description:	Inserts a record into the Concept_Relation table

  Parameters:	@FromConceptKey
		@ToConceptKey
		@ThesaurusRelationTypeKey
		@Multiplicity
		@Inherited
		@Comment
		@SessionID
		@SystemSuppliedData

  Created:	December 2003

  Last revision information:
    $Revision: 1 $
    $Date: 26/11/07 12:26 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptRelation_Insert]
	@Key char(16) OUTPUT,
	@FromConceptKey char(16),
	@ToConceptKey char(16),
	@ThesaurusRelationTypeKey char(16) = NULL,
	@Multiplicity float = NULL,
	@Inherited bit = NULL,
	@Comment text = NULL,
	@SessionID char(16),
	@SystemSuppliedData bit = NULL

AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	DECLARE @ParentRelationTypeKey CHAR(16)

	/*-------------------------------------------------------------*\
	  Get a new key.
	\*-------------------------------------------------------------*/
	EXECUTE spNextKey 'Concept_Relation', @Key OUTPUT
	IF @@ERROR <> 0 GOTO RollBackAndExit

	/*-------------------------------------------------------------*\
	  Wrap everything in one transaction.
	\*-------------------------------------------------------------*/
	BEGIN TRANSACTION
		/*-------------------------------------------------------------*\
		  If the app. doesn't pass in a @ThesaurusRelationTypeKey then
		  the Hierarchy_Relation_Type_Key for the Concept Group of the
		  parent is used.
		\*-------------------------------------------------------------*/		
		SELECT 		@ParentRelationTypeKey = CG.Hierarchy_Relation_Type_Key
		FROM		Concept_Group AS CG
		INNER JOIN	Concept AS C ON C.Concept_Group_Key = CG.Concept_Group_Key
		WHERE		C.Concept_Key = @FromConceptKey

		IF @ThesaurusRelationTypeKey IS NULL 
			SET @ThesaurusRelationTypeKey = @ParentRelationTypeKey

		/*-------------------------------------------------------------*\
			Validate to ensure we are not trying to create a cycle in the 
			concept group hierarchy
		\*-------------------------------------------------------------*/				
		IF @ThesaurusRelationTypeKey = @ParentRelationTypeKey
		BEGIN
			DECLARE @Check	BIT
			EXECUTE	usp_Concept_RecursionCheck_Get	@ToConceptKey, @FromConceptKey, @Check OUTPUT

			IF @Check = 1
				RAISERROR ('Cyclical relationship', 16, 1)

			IF @@Error <> 0 GOTO RollbackAndExit
		END

		/*-------------------------------------------------------------*\
		  Insert in Concept_Relation.
		\*-------------------------------------------------------------*/
		INSERT INTO Concept_Relation (
			Concept_Relation_Key,
			From_Concept_Key,
			To_Concept_Key,
			Thesaurus_Relation_Type_Key,
			Inherited,
			Multiplicity,
			Comment,
			Entered_Session_ID,
			System_Supplied_Data
		) VALUES (
			@Key,
			@FromConceptKey,
			@ToConceptKey,
			@ThesaurusRelationTypeKey,
			IsNull(@Inherited, 0),
			@Multiplicity,
			@Comment,
			@SessionID,
			IsNull(@SystemSuppliedData, 0)
		)
		IF @@Error <> 0 GOTO RollbackAndExit

		/*-------------------------------------------------------------*\
		  Make corresponding changes to lineage table if this is a 
			parent relationship
		\*-------------------------------------------------------------*/
		IF @ThesaurusRelationTypeKey=@ParentRelationTypeKey
		BEGIN
			EXECUTE		usp_ConceptLineage_NewRelation	@Key
			IF @@ERROR <> 0 GOTO RollbackAndExit
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
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_ConceptRelation_Insert failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptRelation_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptRelation_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptRelation_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptRelation_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptRelation_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptRelation_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptRelation_Insert TO [Dev - JNCC SQL]
END

GO


/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_Paste') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_Concept_Paste]
GO

/*===========================================================================*\
  Description:	Pastes a concept from one position to another

  Parameters:	@DestConceptKey CHAR(16) - output param = key of newly pasted concept

  Created:	Aug 2004

  Last revision information:
    $Revision: 1 $
    $Date: 26/11/07 12:26 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Concept_Paste]
	@ConceptKey 			CHAR(16),
	@DestConceptGroupKey 	CHAR(16),
	@DestParentConceptKey 	CHAR(16),
	@IsCut 					BIT,
	@SessionID 				CHAR(16),
	@SystemSuppliedData 	BIT			=	0,
	@DestConceptKey 		CHAR(16) 	OUTPUT
AS

BEGIN TRANSACTION
	/*-------------------------------------------------------------*\
		Prepare things for the operation
	\*-------------------------------------------------------------*/

	--Enforce a value in @SystemSuppliedData as the default value 
	--doesn't seem to work every time
	IF @SystemSuppliedData IS NULL
		SET @SystemSuppliedData=0

	DECLARE @SrcConceptGroupKey CHAR(16),
			@Lineage 			VARCHAR(900),
			@OldRelationTypeKey CHAR(16),
			@NewRelationTypeKey CHAR(16),
			@Key 				CHAR(16),
			@DummyKey			CHAR(16),
			@RelationshipAdded	BIT

	SET		@RelationshipAdded	=	0

	SELECT 	@SrcConceptGroupKey = Concept_Group_Key
	FROM 	Concept
	WHERE 	Concept_Key = @ConceptKey
	IF @@Error <> 0 GOTO RollbackAndExit

	--Find the source concept group's hierarchy relationship
	SELECT 	@OldRelationTypeKey = CG.Hierarchy_Relation_Type_Key
	FROM 	Concept C
	JOIN 	Concept_Group CG ON CG.Concept_Group_Key = C.Concept_Group_Key
	WHERE 	C.Concept_Key = @ConceptKey
	IF @@Error <> 0 GOTO RollbackAndExit

	--Find the dest concept group's hierarchy relationship
	IF @DestParentConceptKey IS NULL 
		SET @NewRelationTypeKey=@OldRelationTypeKey
	ELSE
	BEGIN
		SELECT	@NewRelationTypeKey = CG.Hierarchy_Relation_Type_Key
		FROM 	Concept C
		JOIN 	Concept_Group CG ON CG.Concept_Group_Key = C.Concept_Group_Key
		WHERE 	C.Concept_Key = @DestParentConceptKey
	END

	/*-------------------------------------------------------------*\
		Perform the cut operation
	\*-------------------------------------------------------------*/
	IF @IsCut = 1 
	BEGIN
		DECLARE @OldKey CHAR(16)

		SET @DestConceptKey = @ConceptKey
	
		--Prepare to delete subtree of lineage
		SELECT 	@Lineage = Lineage
		FROM 	Concept_Lineage
		WHERE 	Concept_Key = @ConceptKey
		IF @@Error <> 0 GOTO RollbackAndExit
	
		IF @DestParentConceptKey IS NULL
		BEGIN
			--Delete source's parent relationship(s)
			DECLARE @KeyToDel 	CHAR(16)
			DECLARE @Timestamp 	TIMESTAMP
	
			DECLARE csr CURSOR STATIC LOCAL FOR
				SELECT 	Concept_Relation_Key, Timestamp
				FROM 	Concept_Relation 
				WHERE 	To_Concept_Key				=	@ConceptKey
				AND 	Thesaurus_Relation_Type_Key	=	@OldRelationTypeKey
	
			OPEN csr
			
			FETCH NEXT FROM csr INTO @KeyToDel, @Timestamp
			WHILE @@FETCH_STATUS = 0
			BEGIN
				EXEC usp_ConceptRelation_Delete @KeyToDel, @Timestamp
				IF @@Error <> 0 GOTO RollbackAndExit
				FETCH NEXT FROM csr INTO @KeyToDel, @Timestamp
			END
		END
		ELSE
		BEGIN
			--Update source's parent relationship to point to new parent key
			IF EXISTS(	SELECT 	1 
						FROM 	Concept_Relation 
						WHERE 	To_Concept_Key				=	@ConceptKey
						AND 	Thesaurus_Relation_Type_Key	=	@OldRelationTypeKey)
			BEGIN
				SELECT 	@OldKey	=	From_Concept_Key, 
						@Key	=	Concept_Relation_Key
				FROM 	Concept_Relation
				WHERE 	To_Concept_Key				=	@ConceptKey
				AND 	Thesaurus_Relation_Type_Key	=	@OldRelationTypeKey
				IF @@Error <> 0 GOTO RollbackAndExit
	
				UPDATE 	Concept_Relation 
				SET 	From_Concept_Key			=	@DestParentConceptKey,
						Changed_Session_ID			=	@SessionID,
						Thesaurus_Relation_Type_Key	=	@NewRelationTypeKey
				WHERE 	To_Concept_Key				=	@ConceptKey
				AND 	Thesaurus_Relation_Type_Key	=	@OldRelationTypeKey
				IF @@Error <> 0 GOTO RollbackAndExit
	
				EXECUTE	usp_ConceptLineage_UpdateRelation	
						@Key,
						@OldKey,
						@ConceptKey,
						@OldRelationTypeKey
				IF @@Error <> 0 GOTO RollbackAndExit
			END
			ELSE
			BEGIN
				EXECUTE	usp_ConceptRelation_Insert
						@DummyKey,
						@DestParentConceptKey,
						@ConceptKey,
						@OldRelationTypeKey,
						NULL,
						NULL,
						NULL,
						@SessionID, 
						@SystemSuppliedData
				IF @@Error <> 0 GOTO RollbackAndExit

				SET	@RelationshipAdded = 1
			END
		END
	
		IF @SrcConceptGroupKey <> @DestConceptGroupKey 
		BEGIN
			-- Update concept group for source concepts to new group, but not the synonyms, hence using ChildrenOnly.
			UPDATE 	CChild
			SET 	Concept_Group_Key = @DestConceptGroupKey
			FROM 	VW_ConceptChildrenOnly 	CC 
			JOIN 	Concept 				CChild 	ON 	CChild.Concept_Key			=	CC.Child_Concept_Key
													AND CChild.Concept_Group_Key	=	@SrcConceptGroupKey
			WHERE 	CC.Parent_Concept_Key	=	@ConceptKey
			IF @@Error <> 0 GOTO RollbackAndExit

			-- And now do the Concept.
			UPDATE	Concept
			SET		Concept_Group_Key	=	@DestConceptGroupKey
			WHERE	Concept_Key			= 	@ConceptKey
			IF @@Error <> 0 GOTO RollbackAndExit

			-- Cut/Paste to top level causes usp_ConceptRelation_Delete to create a lineage for orphaned concept.
			-- And it causes extra records in ConceptLineage that are no good. Lineage will be properly recreated further down anyway.
			EXECUTE usp_ConceptLineage_DeleteConcept @ConceptKey
		END
	
		-- Actually delete the old lineage information
		IF @OldKey <> @DestParentConceptKey
		BEGIN
			EXECUTE usp_ConceptLineage_DeleteSubtree @SrcConceptGroupKey, @Lineage
			IF @@Error <> 0 GOTO RollbackAndExit
		END
	END
	ELSE
	/*-------------------------------------------------------------*\
		or copy operation
	\*-------------------------------------------------------------*/
	BEGIN
		--Whole branch being copied into a the concept group, so find all concepts and clone them
		DECLARE @ChildConceptKey CHAR(16)
	
		--Create a local table to hold key mappings
		DECLARE @ConceptMapping TABLE (
			Src_Concept_Key 	CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS PRIMARY KEY,
			Dest_Concept_Key	CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS
		)
	
		--Clone the source concepts, updating concept group key
		DECLARE csr CURSOR STATIC LOCAL FOR
			SELECT 	CChild.Concept_Key
			FROM 	VW_ConceptChildrenOnly 	CC 
			JOIN 	Concept 				CChild	ON 	CChild.Concept_Key 			= CC.Child_Concept_Key
													AND CChild.Concept_Group_Key 	= @SrcConceptGroupKey
			WHERE 	CC.Parent_Concept_Key 	= @ConceptKey
			-- Add the copied concept, instead of duplicating code to clone it separatly
			UNION
			SELECT	@ConceptKey
	
		OPEN csr
		FETCH NEXT FROM csr INTO @ChildConceptKey
		WHILE @@FETCH_STATUS = 0
		BEGIN
			EXECUTE spNextKey 'Concept', @Key OUTPUT
			IF @@Error <> 0 GOTO RollBackAndExit
			
			-- When cloning the actual selected concept, remember the new concept key
			IF @ChildConceptKey = @ConceptKey 
				SET @DestConceptKey = @Key
	
			-- Rememer mappings so we can update relationships later
			INSERT INTO @ConceptMapping VALUES (@ChildConceptKey, @Key)
			IF @@Error <> 0 GOTO RollBackAndExit
	
			-- Clone the concept
			INSERT INTO Concept (
				Concept_Key, Term_Key, Concept_Group_Key, Term_Version_Key, List_Preferred, 
				Is_Current, Preferred, Concept_Rank_Key, Name_Type_Concept_Key, Meaning_Key,
				Author_Copy, Sort_Code, List_Code, Entered_Session_ID, System_Supplied_Data, Custodian
			)
			SELECT 	@Key, Term_Key, @DestConceptGroupKey, Term_Version_Key, List_Preferred, 
				Is_Current, Preferred, Concept_Rank_Key, Name_Type_Concept_Key, Meaning_Key,
				Author_Copy, Sort_Code, List_Code, @SessionID, @SystemSuppliedData, LEFT(@Key, 8)
			FROM 	Concept 
			WHERE 	Concept_Key = @ChildConceptKey
			IF @@Error <> 0 GOTO RollBackAndExit
	
			FETCH NEXT FROM csr INTO @ChildConceptKey
		END
	
		CLOSE csr
		DEALLOCATE csr
	
		/*-------------------------------------------------------------*\
			Clone the hierarchical relationships within the copied branch
				of concepts
		\*-------------------------------------------------------------*/
		DECLARE @SrcKey		CHAR(16), 
				@DestKey	CHAR(16)
	
		--Declare a temp table with same structure as concept relation that 
		--we can populate with dummy primary keys, then update later
		SELECT TOP 0 * INTO #TempRel FROM Concept_Relation
		IF @@Error <> 0 GOTO RollbackAndExit
	
		DECLARE cmap CURSOR STATIC LOCAL FOR
			--Note we are cloning parent relationships within the branch, so 
			--exclude the top node
			SELECT * FROM @ConceptMapping WHERE Dest_Concept_Key <> @DestConceptKey
		
		OPEN cmap
		
		FETCH NEXT FROM cmap INTO @SrcKey, @DestKey
		WHILE @@FETCH_STATUS=0
		BEGIN
			INSERT INTO #TempRel (
				Concept_Relation_Key,
				From_Concept_Key,
				To_Concept_Key,
				Thesaurus_Relation_Type_Key,
				Multiplicity,
				Inherited,
				Comment,
				Entered_Session_ID,
				System_Supplied_Data,
				Custodian
			)
			SELECT 		CR.Concept_Relation_Key, -- Will be replaced later
						ISNULL(CM.Dest_Concept_Key, CR.From_Concept_Key),
						@DestKey,
						Thesaurus_Relation_Type_Key,
						Multiplicity,
						Inherited,
						Comment,
						@SessionID,
						@SystemSuppliedData,
						LEFT(@DestKey, 8)
			FROM 		Concept_Relation 	CR
			LEFT JOIN 	@ConceptMapping 	CM 	ON 	CM.Src_Concept_Key	=	CR.From_Concept_Key
			WHERE 		CR.To_Concept_Key				=	@SrcKey
			AND 		CR.Thesaurus_Relation_Type_Key	=	@OldRelationTypeKey

			IF @@Error <> 0 GOTO RollbackAndExit
	
			FETCH NEXT FROM cmap INTO @SrcKey, @DestKey
		END
		
		CLOSE cmap
		DEALLOCATE cmap 
	
		--Now we have a table of concept relationships to insert, but we must update the keys first
		DECLARE crel CURSOR LOCAL FOR
			SELECT Concept_Relation_Key FROM #TempRel
		
		OPEN crel
		
		FETCH NEXT FROM crel INTO @SrcKey
		
		WHILE @@FETCH_STATUS=0
		BEGIN
			EXECUTE spNextKey 'Concept_Relation', @DestKey OUTPUT
			IF @@Error <> 0 GOTO RollbackAndExit
			
			UPDATE 	#TempRel
			SET 	Concept_Relation_Key = @DestKey
			WHERE CURRENT OF crel

			IF @@Error <> 0 GOTO RollbackAndExit
	
			FETCH NEXT FROM crel INTO @SrcKey		
		END
	
		CLOSE crel
		DEALLOCATE crel
	
		--Copy the relationships into the concept relation table
		INSERT INTO Concept_Relation (
			Concept_Relation_Key,
			From_Concept_Key,
			To_Concept_Key,
			Thesaurus_Relation_Type_Key,
			Multiplicity,
			Inherited,
			Comment,
			Entered_Session_ID,
			System_Supplied_Data,
			Custodian
		)
		SELECT 	Concept_Relation_Key,
				From_Concept_Key,
				To_Concept_Key,
				Thesaurus_Relation_Type_Key,
				Multiplicity,
				Inherited,
				Comment,
				Entered_Session_ID,
				System_Supplied_Data,
				Custodian 
		FROM 	#TempRel
		IF @@Error <> 0 GOTO RollbackAndExit
	
		DROP TABLE #TempRel
	END

	/*-------------------------------------------------------------*\
	 Join the copied branch of concepts to the destination concept.
	 This also fixes up the lineage.
	\*-------------------------------------------------------------*/
	IF 	(@DestParentConceptKey IS NOT NULL) 
	AND ((@SrcConceptGroupKey <> @DestConceptGroupKey) OR (@IsCut = 0)) 
	AND (@RelationshipAdded = 0)	-- If already added (see cut handling), don't do it again.
	BEGIN
		EXECUTE usp_ConceptRelation_Insert
				@DummyKey,
				@DestParentConceptKey,
				@DestConceptKey,
				@OldRelationTypeKey,
				NULL,
				NULL,
				NULL,
				@SessionID, 
				@SystemSuppliedData
		IF @@Error <> 0 GOTO RollbackAndExit
	END
	ELSE 
	BEGIN
		IF @DestParentConceptKey IS NULL
			EXECUTE usp_ConceptLineage_CreateSubtree @DestConceptKey, ''
	END

	COMMIT TRANSACTION

	RETURN

RollBackAndExit: 
	IF @@TranCount > 0 ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_Paste') AND SysStat & 0xf = 4)
BEGIN
	PRINT 'Setting up security on procedure usp_Concept_Paste'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concept_Paste TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concept_Paste TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
		GRANT EXECUTE ON dbo.usp_Concept_Paste TO [Dev - JNCC SQL]
END
GO

