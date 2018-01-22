SET QUOTED_IDENTIFIER ON
GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Collections_Select_ForSearchByMetadata]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Collections_Select_ForSearchByMetadata]
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

CREATE PROCEDURE [dbo].usp_Collections_Select_ForSearchByMetadata 
@UserDomainMask INT,
@SessionID CHAR(16),
@SortOrderIndex TINYINT,
@SearchText VARCHAR(100),
@MetaDataType VARCHAR(100)

AS

--  DESCRIPTION
--  Returns Collections based on the search parameter for the specified type of metadata
--
--	PARAMETERS
--	NAME				DESCRIPTION
--	@UserDomainMask		User's Domain Mask restricting which records may be returned
--	@SessionID 			User's SessionID
--	@SortOrderIndex		Index determining Sort Order
--	@SearchText			Text to be used for search
--  @MetaDataType       The type of metadata to be searching on
--
--
--  AUTHOR:				David Kelly, Dorset Software
--  CREATED:			2007-09-04
--
SET NOCOUNT ON

DECLARE @MetaDataTypeKey CHAR(16)
SET @MetaDataTypeKey = (SELECT MetaData_Type_Key From Metadata_Type WHERE Item_Name = @MetaDataType AND Table_Name = 'Collection')

IF @SortOrderIndex = 0
	SELECT C.Collection_Unit_Key AS Item_Key, C.Item_Name, M.Number, MT.Text AS Hint
	FROM 
		(COLLECTION C
		INNER JOIN
   		    COLLECTION_UNIT CU 
	   	ON C.Collection_Unit_Key = CU.Collection_Unit_Key
    	   	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
				OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
		)
		LEFT JOIN
			(MOVEMENT_COLLECTION_UNIT MCU
			INNER JOIN
				MOVEMENT_DIRECTION MD
			ON MCU.Movement_Direction_Key = MD.Movement_Direction_Key 
				AND (MD.Outbound = 0)
			INNER JOIN
				MOVEMENT M
			ON MD.Movement_Key = M.Movement_Key AND (M.Movement_Type = 0 OR M.Movement_Type = 1)
			LEFT JOIN
				(CONCEPT CON
				INNER JOIN 
					TERM T
				ON CON.Term_Key = T.Term_Key)
			ON CON.Concept_Key = 'SYSTEM0000000006') --ACCESSION NUMBER
		ON CU.Collection_Unit_Key = MCU.Collection_Unit_Key

		INNER JOIN
			METADATA MT
		ON C.Collection_Unit_Key = MT.Record_Key
			AND MT.MetaData_Type_Key = @MetaDataTypeKey
			AND MT.TEXT LIKE @SearchText + '%'
	ORDER BY Collation_From_Vague_Date_Start DESC, Collation_From_Vague_Date_End DESC, C.Item_Name, M.Number

ELSE IF @SortOrderIndex = 1
	SELECT C.Collection_Unit_Key AS Item_Key, C.Item_Name, M.Number, MT.Text AS Hint
	FROM 
		(COLLECTION C
		INNER JOIN
   		    COLLECTION_UNIT CU 
	   	ON C.Collection_Unit_Key = CU.Collection_Unit_Key
    	   	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
				OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
		)
		LEFT JOIN
			(MOVEMENT_COLLECTION_UNIT MCU
			INNER JOIN
				MOVEMENT_DIRECTION MD
			ON MCU.Movement_Direction_Key = MD.Movement_Direction_Key 
				AND (MD.Outbound = 0)
			INNER JOIN
				MOVEMENT M
			ON MD.Movement_Key = M.Movement_Key AND (M.Movement_Type = 0 OR M.Movement_Type = 1)
			LEFT JOIN
				(CONCEPT CON
				INNER JOIN 
					TERM T
				ON CON.Term_Key = T.Term_Key)
			ON CON.Concept_Key = 'SYSTEM0000000006') --ACCESSION NUMBER
		ON CU.Collection_Unit_Key = MCU.Collection_Unit_Key

		INNER JOIN
			METADATA MT
		ON C.Collection_Unit_Key = MT.Record_Key
			AND MT.MetaData_Type_Key = @MetaDataTypeKey
			AND MT.TEXT LIKE @SearchText + '%'
	ORDER BY C.Item_Name, M.Number

ELSE IF @SortOrderIndex = 2
	SELECT C.Collection_Unit_Key AS Item_Key, C.Item_Name, M.Number, MT.Text AS Hint
	FROM 
		(COLLECTION C
		INNER JOIN
   		    COLLECTION_UNIT CU 
	   	ON C.Collection_Unit_Key = CU.Collection_Unit_Key
    	   	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
				OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
		)
		LEFT JOIN
			(MOVEMENT_COLLECTION_UNIT MCU
			INNER JOIN
				MOVEMENT_DIRECTION MD
			ON MCU.Movement_Direction_Key = MD.Movement_Direction_Key 
				AND (MD.Outbound = 0)
			INNER JOIN
				MOVEMENT M
			ON MD.Movement_Key = M.Movement_Key AND (M.Movement_Type = 0 OR M.Movement_Type = 1)
			LEFT JOIN
				(CONCEPT CON
				INNER JOIN 
					TERM T
				ON CON.Term_Key = T.Term_Key)
			ON CON.Concept_Key = 'SYSTEM0000000006') --ACCESSION NUMBER
		ON CU.Collection_Unit_Key = MCU.Collection_Unit_Key

		INNER JOIN
			METADATA MT
		ON C.Collection_Unit_Key = MT.Record_Key
			AND MT.MetaData_Type_Key = @MetaDataTypeKey
			AND MT.TEXT LIKE @SearchText + '%'
	ORDER BY M.Number

GO
/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Collections_Select_ForSearchByMetadata') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Collections_Select_ForSearchByMetadata'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Collections_Select_ForSearchByMetadata TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Collections_Select_ForSearchByMetadata TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Collections_Select_ForSearchByMetadata TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Collections_Select_ForSearchByMetadata TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Collections_Select_ForSearchByMetadata TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Collections_Select_ForSearchByMetadata TO [Dev - JNCC SQL]
END

GO



/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_CollectionUnitNumber_Insert]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_CollectionUnitNumber_Insert]
GO
/*===========================================================================*\
  Description:	Inserts a record into the Collection Unit Number table.
  Parameters:	@Key 
		@CollectionUnitKey 
		@Number 
		@TypeConceptKey 
		@Preferred
		@Notes 
		@SessionID 

  Created:	July 2003

  Last revision information:
    $Revision: 5 $
    $Date: 2/02/09 16:56 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_CollectionUnitNumber_Insert]
	@Key char(16) OUTPUT,
	@CollectionUnitKey char(16),
	@Number varchar(30),
	@TypeConceptKey char(16),
	@Preferred bit,
	@Notes text,
	@SessionID char(16)
	
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF
	
	EXECUTE spNextKey 'Collection_Unit_Number', @Key OUTPUT

	BEGIN TRANSACTION
		-- Make sure every other Collection_Unit_Number record for this Collection unit record and 
		-- this this type are set to not preferred.
		IF @Preferred = 1 BEGIN
			UPDATE 	Collection_Unit_Number
			SET 	Preferred = 0
			WHERE 	Collection_Unit_Key = @CollectionUnitKey
		
			IF @@Error <> 0 GOTO RollbackAndExit
		END
	
		INSERT INTO Collection_Unit_Number (
			Collection_Unit_Number_Key,
			Collection_Unit_Key,
			Number,
			Type_Concept_Key,
			Preferred,
			Notes,
			Entered_Session_ID
		) VALUES (
			@Key,
			@CollectionUnitKey,
			@Number,
			@TypeConceptKey,
			@Preferred,
			@Notes,
			@SessionID )

		IF @@Error <> 0 GOTO RollbackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_CollectionUnitNumber_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_CollectionUnitNumber_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitNumber_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_CollectionUnitNumber_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_CollectionUnitNumber_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_CollectionUnitNumber_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitNumber_Insert TO [Dev - JNCC SQL]
END
GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_ConceptKey_Get_ForTermKey]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_ConceptKey_Get_ForTermKey]
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

CREATE PROCEDURE [dbo].usp_ConceptKey_Get_ForTermKey 
	@Key char(16),
	@ConceptKey char(16) OUTPUT

AS

--  DESCRIPTION
--  Insert a record into Survey_Event_Geo_Area
--
--	PARAMETERS
--	NAME				DESCRIPTION
--	@Key				The Term_Key of the concept
--  @SurveyEventKey		The Concept_Key of the concept
--
--
--  AUTHOR:				David Kelly, Dorset Software
--  CREATED:			2007-09-07
--


	-- Required to be able to get number of changed records.
	SET NOCOUNT ON

	SELECT 	@ConceptKey = Concept_Key
	FROM
	Concept
	WHERE	Term_Key = @Key


GO
/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptKey_Get_ForTermKey') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptKey_Get_ForTermKey'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptKey_Get_ForTermKey TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptKey_Get_ForTermKey TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptKey_Get_ForTermKey TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ConceptKey_Get_ForTermKey TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptKey_Get_ForTermKey TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptKey_Get_ForTermKey TO [Dev - JNCC SQL]
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
    $Revision: 5 $
    $Date: 2/02/09 16:56 $
    $Author: Pauldavies $

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
			IF EXISTS(
					SELECT 1
					FROM Concept_Lineage CLParent
					INNER JOIN Concept_Lineage CLChild ON CLParent.Lineage LIKE CLChild.Lineage + '\%'
							OR CLParent.Lineage = CLChild.Lineage		
					WHERE CLParent.Concept_Key=@FromConceptKey
					AND CLChild.Concept_Key=@ToConceptKey
					)
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
    $Revision: 5 $
    $Date: 2/02/09 16:56 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Concept_Paste]
	@ConceptKey CHAR(16),
	@DestConceptGroupKey CHAR(16),
	@DestParentConceptKey CHAR(16),
	@IsCut BIT,
	@SessionID CHAR(16),
	@SystemSuppliedData BIT=0,
	@DestConceptKey CHAR(16) OUTPUT
AS

BEGIN TRANSACTION

	/*-------------------------------------------------------------*\
		Prepare things for the operation
	\*-------------------------------------------------------------*/

	--Enforce a value in @SystemSuppliedData as the default value 
	--doesn't seem to work every time
	IF @SystemSuppliedData IS NULL
		SET @SystemSuppliedData=0

	DECLARE @SrcConceptGroupKey CHAR(16)
	DECLARE @Lineage VARCHAR(900)
	DECLARE @OldRelationTypeKey CHAR(16)
	DECLARE @NewRelationTypeKey CHAR(16)
	DECLARE @Key CHAR(16)


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
	Perform the cut or copy operation
\*-------------------------------------------------------------*/
IF @IsCut=1 
BEGIN
	SET @DestConceptKey=@ConceptKey

	--Prepare to delete subtree of lineage
	SELECT @Lineage=Lineage
	FROM Concept_Lineage
	WHERE Concept_Key=@ConceptKey
	IF @@Error <> 0 GOTO RollbackAndExit

	IF @DestParentConceptKey IS NULL
	BEGIN
		--Delete source's parent relationship(s)
		DECLARE @KeyToDel CHAR(16)
		DECLARE @Timestamp TIMESTAMP

		DECLARE csr CURSOR STATIC LOCAL FOR
			SELECT Concept_Relation_Key, Timestamp
			FROM Concept_Relation 
			WHERE To_Concept_Key=@ConceptKey
			AND Thesaurus_Relation_Type_Key=@OldRelationTypeKey

		OPEN csr
		
		WHILE 1=1
		BEGIN
			FETCH NEXT FROM csr INTO @KeyToDel, @Timestamp

			IF @@FETCH_STATUS<>0 
				BREAK
			
			EXEC usp_ConceptRelation_Delete @KeyToDel, @Timestamp
			IF @@Error <> 0 GOTO RollbackAndExit
		END
	END
	ELSE
	BEGIN
		--Update source's parent relationship to point to new parent key
		IF EXISTS(SELECT 1 FROM Concept_Relation 
					WHERE To_Concept_Key=@ConceptKey
					AND Thesaurus_Relation_Type_Key=@OldRelationTypeKey)
		BEGIN
			DECLARE @OldKey CHAR(16)
			SELECT @OldKey=From_Concept_Key, @Key=Concept_Relation_Key
			FROM Concept_Relation
			WHERE To_Concept_Key=@ConceptKey
				AND Thesaurus_Relation_Type_Key=@OldRelationTypeKey
			IF @@Error <> 0 GOTO RollbackAndExit

			UPDATE Concept_Relation 
			SET From_Concept_Key=@DestParentConceptKey,
				Changed_Session_ID=@SessionID,
				Thesaurus_Relation_Type_Key=@NewRelationTypeKey
			WHERE To_Concept_Key=@ConceptKey
				AND Thesaurus_Relation_Type_Key=@OldRelationTypeKey
			IF @@Error <> 0 GOTO RollbackAndExit

			EXECUTE		usp_ConceptLineage_UpdateRelation	
					@Key,
					@OldKey,
					@ConceptKey,
					@OldRelationTypeKey
			IF @@Error <> 0 GOTO RollbackAndExit
		END
		ELSE
		BEGIN
			EXECUTE spNextKey 'Concept_Relation', @Key OUTPUT	
		
			EXEC usp_ConceptRelation_Insert
				@Key,
				@DestParentConceptKey,
				@ConceptKey,
				@OldRelationTypeKey,
				NULL,
				NULL,
				NULL,
				@SessionID, 
				@SystemSuppliedData
			IF @@Error <> 0 GOTO RollbackAndExit
		END
	END

	IF @SrcConceptGroupKey<>@DestConceptGroupKey 
	BEGIN
		--Update concept group for source concepts to new group
		UPDATE CChild
		SET Concept_Group_Key = @DestConceptGroupKey
		FROM VW_ConceptChildren CC 
		INNER JOIN Concept CChild ON CChild.Concept_Key=CC.Child_Concept_Key
			AND CChild.Concept_Group_Key=@SrcConceptGroupKey
		WHERE CC.Parent_Concept_Key=@ConceptKey
		IF @@Error <> 0 GOTO RollbackAndExit
	END

	-- Actually delete the old lineage information	
	IF @OldKey<>@DestParentConceptKey
	BEGIN
		EXEC usp_ConceptLineage_DeleteSubtree @SrcConceptGroupKey, @Lineage
		IF @@Error <> 0 GOTO RollbackAndExit
	END
END
ELSE
BEGIN
	--Whole branch being copied into a the concept group, so find all concepts and clone them
	DECLARE @ChildConceptKey CHAR(16)

	--Create a local table to hold key mappings
	DECLARE @ConceptMapping TABLE (
		Src_Concept_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS PRIMARY KEY,
		Dest_Concept_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS
	)

	--Clone the source concepts, updating concept group key
	DECLARE csr CURSOR STATIC LOCAL FOR
		SELECT 	CChild.Concept_Key
		FROM 	VW_ConceptChildrenOnly CC 
		JOIN 	Concept CChild	ON CChild.Concept_Key = CC.Child_Concept_Key
					AND CChild.Concept_Group_Key = @SrcConceptGroupKey
		WHERE 	CC.Parent_Concept_Key = @ConceptKey
		-- Add the copied concept, instead of duplicating code to clone it separatly
		UNION
		SELECT	@ConceptKey

	OPEN csr
	FETCH NEXT FROM csr INTO @ChildConceptKey
	WHILE @@FETCH_STATUS=0
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
	DECLARE @SrcKey CHAR(16), @DestKey CHAR(16)

	--Declare a temp table with same structure as concept relation that 
	--we can populate with dummy primary keys, then update later
	SELECT TOP 0 * INTO #TempRel FROM Concept_Relation
	IF @@Error <> 0 GOTO RollbackAndExit

	DECLARE cmap CURSOR STATIC LOCAL FOR
		--Note we are cloning parent relationships within the branch, so 
		--exclude the top node
		SELECT * FROM @ConceptMapping WHERE Dest_Concept_Key<>@DestConceptKey
	
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
		SELECT 
			CR.Concept_Relation_Key, -- Will be replaced later
			ISNULL(CM.Dest_Concept_Key, CR.From_Concept_Key),
			@DestKey,
			Thesaurus_Relation_Type_Key,
			Multiplicity,
			Inherited,
			Comment,
			@SessionID,
			@SystemSuppliedData,
			Left(@DestKey, 8)
		FROM Concept_Relation CR
		LEFT JOIN @ConceptMapping CM ON CM.Src_Concept_Key=CR.From_Concept_Key
		WHERE CR.To_Concept_Key=@SrcKey
		AND CR.Thesaurus_Relation_Type_Key=@OldRelationTypeKey
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
		
		UPDATE #TempRel
		SET Concept_Relation_Key=@DestKey
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
		SELECT 
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
		FROM #TempRel
	IF @@Error <> 0 GOTO RollbackAndExit

	DROP TABLE #TempRel
END

	/*-------------------------------------------------------------*\
	 Join the copied branch of concepts to the destination concept.
	 This also fixes up the lineage.
	\*-------------------------------------------------------------*/
	IF (@DestParentConceptKey IS NOT NULL) AND ((@SrcConceptGroupKey<>@DestConceptGroupKey) OR (@IsCut=0))
	BEGIN
		EXECUTE spNextKey 'Concept_Relation', @Key OUTPUT	

		EXEC usp_ConceptRelation_Insert
			@Key,
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
	ELSE BEGIN
		IF @DestParentConceptKey IS NULL
		EXEC usp_ConceptLineage_CreateSubtree @DestConceptKey, ''
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

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Concept_RecursionCheck_Get]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Concept_RecursionCheck_Get]
GO

CREATE PROCEDURE [dbo].[usp_Concept_RecursionCheck_Get] 
@PotentialChildKey CHAR(16),
@PotentialParentKey CHAR(16),
@RecursionExists BIT OUTPUT
AS

/*===========================================================================*\
  Description:  Checks that the user isn't trying to create a circular
        Concept_Relation.

  Parameters:   @PotentialChildKey - key of dragged node.
                @PotentialParentKey - key of target node.
                @RecursionExists - if cycle exists (i.e. a problem) return 1
                        else return 0 (i.e. OK)

  Created:  March 2004

  Last revision information:
    $Revision: 5 $
    $Date: 2/02/09 16:56 $
    $Author: Pauldavies $

\*===========================================================================*/

    SET NOCOUNT ON

    IF @PotentialChildKey = @PotentialParentKey
        SET         @RecursionExists    =   1
    ELSE
    BEGIN
        SELECT @RecursionExists = MAX(CASE WHEN LP.Lineage LIKE LC.Lineage + '\%' THEN 1 ELSE 0 END)
        FROM Concept_Lineage LC
        CROSS JOIN Concept_Lineage LP
        WHERE LC.Concept_Key=@PotentialChildKey
        AND LP.Concept_Key = @PotentialParentKey

        IF @RecursionExists IS NULL
            SET         @RecursionExists    =   0
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

IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_EnquiryStats_Select') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_EnquiryStats_Select]
GO

/*===========================================================================*\
  Description:	Returns the number of enquiries against the departments they were made by.

  Parameters:	
	@VagueDateStart 	Start date to filter records by
	@VagueDateEnd		End date to filter records by

  Created:	January 2003

  Last revision information:
    $Revision: 5 $
    $Date: 2/02/09 16:56 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_EnquiryStats_Select] 
	@VagueDateStart INT,
	@VagueDateEnd INT
AS
SET NOCOUNT ON

	IF @VagueDateStart = -1 SET @VagueDateStart = NULL
	IF @VagueDateEnd = -1 SET @VagueDateEnd = NULL

	/*---------------------------------------------*\
	  Get all enquiry types to build a total column
	\*---------------------------------------------*/
	DECLARE	@Sums varchar(2000),
		@Names varchar(1000)

	-- Default is to have at least a Total for all types.
	SET	@Sums = 'Cast(Count(E.Enquiry_Key) AS varchar(100))'
	SET	@Names = 'Total'

	DECLARE	@ETypeKey char(16), 
		@EType varchar(100)

	DECLARE curEnquiryType CURSOR FOR
		SELECT Concept_Key, PlainText FROM vw_ConceptTerm WHERE Concept_Group_Key = 'SYSTEM000000000A'
	OPEN curEnquiryType
	FETCH NEXT FROM curEnquiryType INTO @ETypeKey, @EType
	WHILE (@@Fetch_Status = 0) BEGIN
		-- Build up the SQL for the calculated value
		SET @Sums = @Sums + '+'',''+' +
			'Cast(Sum(CASE WHEN Enquiry_Type_Concept_Key = ''' + @ETypeKey + ''' THEN 1 ELSE 0 END) AS varchar(100))'
		-- Build up the column name out of all enquiry types.
		SET @Names = @Names + ',' + @EType

		FETCH NEXT FROM curEnquiryType INTO @ETypeKey, @EType
	END
	CLOSE curEnquiryType
	DEALLOCATE curEnquiryType

	/*---------------------------------------------*\
	  And run the query. Has to be a dynamic query.
	\*---------------------------------------------*/
	EXECUTE(
	-- Use NULL to have types as first record in results
	'SELECT 	NULL AS Department, ''' + @names + ''' AS Types ' +
	'UNION ' +
	'SELECT 	OD.Item_Name, ' + @Sums + 
	'FROM		Organisation_Department OD ' +
	'LEFT JOIN 	(Individual I JOIN Enquiry E ON I.Name_Key = E.Answered_By_Name_Key) ' +
	'		ON OD.Organisation_Department_Key = I.Organisation_Department_Key ' +
	'LEFT JOIN	vw_ConceptTerm C ON C.Concept_Key = E.Enquiry_Type_Concept_Key ' +
	'GROUP BY OD.Item_Name ' +
	'ORDER BY "Department"'
	)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_EnquiryStats_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_EnquiryStats_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_EnquiryStats_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_EnquiryStats_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_EnquiryStats_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_EnquiryStats_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_EnquiryStats_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_EnquiryStats_Select TO [Dev - JNCC SQL]
END
GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_MetadataType_Select]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_MetadataType_Select]
GO

/*===========================================================================*\
  Description:	Returns field data from Recorder for a specified Specimen

  Parameters:	
	@TableName	The name of the table

  Created:	September 2007

  Last revision information:
    $Revision: 5 $
    $Date: 2/02/09 16:56 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_MetadataType_Select] 
	@TableName VARCHAR(50)
AS
SELECT
	Metadata_Type_Key,
	Item_Name,
	Table_Name,
	Description,
	Custodian,
	[Timestamp]
FROM Metadata_Type
WHERE Table_Name = @TableName
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_MetadataType_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_MetadataType_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_MetadataType_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_MetadataType_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_MetadataType_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_MetadataType_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_MetadataType_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_MetadataType_Select TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_QEDataItem_Insert_ForMultiValues]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_QEDataItem_Insert_ForMultiValues]
GO

/*===========================================================================*\
  Description:	Inserts a record into the QE_Data_Item table

  Parameters:	@Key	Data Item key

  Created:	October 2007

  Last revision information:
    $Revision: 5 $
    $Date: 2/02/09 16:56 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_QEDataItem_Insert_ForMultiValues]
	@DataRowKey INT,
	@TemplateFieldKey CHAR(16),
	@DataValue as VARCHAR(200),
	@DataDisplay as VARCHAR(200),
	@Position INT,
	@SessionID as CHAR(16)
AS
INSERT INTO QE_Data_Item (QE_Data_Row_Key, QE_Template_Field_Key,
	Data_Value, Data_Display, Position, Entered_Session_ID)
VALUES(@DataRowKey, @TemplateFieldKey, @DataValue, @DataDisplay, @Position, @SessionID)

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_QEDataItem_Insert_ForMultiValues') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_QEDataItem_Insert_ForMultiValues'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_QEDataItem_Insert_ForMultiValues TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_QEDataItem_Insert_ForMultiValues TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_QEDataItem_Insert_ForMultiValues TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_QEDataItem_Insert_ForMultiValues TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_QEDataItem_Insert_ForMultiValues TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_QEDataItem_Insert_ForMultiValues TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_QEDataItem_NumberofValues]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_QEDataItem_NumberofValues]
GO

/*===========================================================================*\
  Description:	Inserts a record into the QE_Data_Item table

  Parameters:	@Key	Data Item key

  Created:	October 2007

  Last revision information:
    $Revision: 5 $
    $Date: 2/02/09 16:56 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_QEDataItem_NumberofValues]
	@Key CHAR(16),
	@Count INT OUTPUT
AS
SELECT 
	@Count = count(*)
from
QE_Data_Item
where QE_Template_Field_Key = @Key

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_QEDataItem_NumberofValues') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_QEDataItem_NumberofValues'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_QEDataItem_NumberofValues TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_QEDataItem_NumberofValues TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_QEDataItem_NumberofValues TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_QEDataItem_NumberofValues TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_QEDataItem_NumberofValues TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_QEDataItem_NumberofValues TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_QEDataItem_Select_ForTemplateField]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_QEDataItem_Select_ForTemplateField]
GO

/*===========================================================================*\
  Description:	Selects all the data from the QE_Data_Item table for
				the specified template field.

  Parameters:	@Key	Template Field key

  Created:	October 2007

  Last revision information:
    $Revision: 5 $
    $Date: 2/02/09 16:56 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_QEDataItem_Select_ForTemplateField]
	@Key char(16)
AS
	SELECT 	QE_Data_Item_Key,
			QE_Data_Row_Key,
			QE_Template_Field_Key,
			Data_Value,
			Data_Display,
			Position,
			[Timestamp],
			NULL as Custodian
	FROM QE_Data_Item
	WHERE QE_Template_Field_Key = @Key
	ORDER BY Position
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_QEDataItem_Select_ForTemplateField') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_QEDataItem_Select_ForTemplateField'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_QEDataItem_Select_ForTemplateField TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_QEDataItem_Select_ForTemplateField TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_QEDataItem_Select_ForTemplateField TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_QEDataItem_Select_ForTemplateField TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_QEDataItem_Select_ForTemplateField TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_QEDataItem_Select_ForTemplateField TO [Dev - JNCC SQL]
END

GO

IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_QETemplateField_Insert') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_QETemplateField_Insert]
GO
    
/*===========================================================================*\
  Description:	Inserts a record in QE_Template_Field table

  Parameters:	Table's fields.

  Created:	August 2003

  Last revision information:
    $Revision: 5 $
    $Date: 2/02/09 16:56 $
    $Author: Pauldavies $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_QETemplateField_Insert]
	@Key 				CHAR(16) OUTPUT,
	@QETemplateKey 			CHAR(16),
	@QEFieldKey 			CHAR(16) = NULL,
	@GeneralTab 			BIT,
	@SpecimenTab 			BIT,
	@ItemName 			VARCHAR(100),
	@DefaultValue 			VARCHAR(200),
	@DefaultDisplay 		VARCHAR(200),
	@SessionID 			VARCHAR(16),
	@IsCustom 			TINYINT,
	@MeasurementAppliesTo 		VARCHAR(50) = NULL,
	@MeasurementMethodConceptKey 	CHAR(16) = NULL,
	@MeasurementDuration 		VARCHAR(50) = NULL,
	@MeasurementAccuracy 		VARCHAR(50) = NULL,
	@MeasurementParameterConceptKey CHAR(16) = NULL,
	@MeasurementunitConceptKey 	CHAR(16) = NULL,
	@MeasurementIsSpecimen 		BIT = NULL,
	@TaxonMeasurementQualifierKey	CHAR(16) = NULL,
	@TaxonMeasurementUnitKey	CHAR(16) = NULL,
	@MeasurementIsTaxonData		BIT = NULL,
	@Sequence 			INT = 0,
	@NumberTypeConceptKey		CHAR(16) = NULL,
	@NumberPreferred			BIT = NULL,
	@Hidden				BIT,
	@Locked				BIT,
	@MetadataTypeKey	CHAR(16) = NULL
AS

SET NOCOUNT OFF

	EXEC spNextKey 'QE_Template_Field', @Key OUTPUT

	INSERT INTO QE_Template_Field (
		QE_Template_Field_Key, QE_Template_Key, QE_Field_Key, General_Tab, Specimen_Tab,
		Item_Name, Default_Value, Default_Display, Entered_Session_ID, System_Supplied_Data, Is_Custom,
		Measurement_Applies_To, Measurement_Method_Concept_Key, Measurement_Duration, Measurement_Accuracy,
		Measurement_Parameter_Concept_Key, Measurement_Unit_Concept_Key, Measurement_Is_Specimen,
		Measurement_Is_TaxonData, Taxon_Measurement_Qualifier_Key, Taxon_Measurement_Unit_Key, [Sequence],
		Number_Type_Concept_Key, Number_Preferred, Hidden, Locked, Metadata_Type_Key
	) VALUES (
		@Key, @QETemplateKey, @QEFieldKey, @GeneralTab, @SpecimenTab,
		@ItemName, @DefaultValue, @DefaultDisplay, @SessionID, 0, @IsCustom,
		@MeasurementAppliesTo, @MeasurementMethodConceptKey, @MeasurementDuration, @MeasurementAccuracy,
		@MeasurementParameterConceptKey, @MeasurementUnitConceptKey, @MeasurementIsSpecimen, 
		@MeasurementIsTaxonData, @TaxonMeasurementQualifierKey, @TaxonMeasurementUnitKey, @Sequence,
		@NumberTypeConceptKey, @NumberPreferred, @Hidden, @Locked, @MetadataTypeKey
	)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_QETemplateField_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_QETemplateField_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_QETemplateField_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_QETemplateField_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_QETemplateField_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_QETemplateField_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_QETemplateField_Insert TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_QETemplateField_Select_ForTemplate') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_QETemplateField_Select_ForTemplate]
GO

/*===========================================================================*\
  Description:	Selects the fields for a template

  Parameters:	@QETemplateKey - QE_Template_Key
		@TemplateType - see template type field description

  Created:	Jan 2004

  Last revision information:
    $Revision: 5 $
    $Date: 2/02/09 16:56 $
    $Author: Pauldavies $

\*===========================================================================*/    
CREATE PROCEDURE [dbo].[usp_QETemplateField_Select_ForTemplate]
	@QETemplateKey 	char(16),
	@TemplateType 	tinyint
AS

SET NOCOUNT ON

	SELECT  	F.Item_Name AS 'Field', 
			General_Tab,
		 	Specimen_Tab,
			TF.Item_Name AS 'Alternative_Name', 
			Default_Value, 
			F.Default_Size,
			TF.QE_Template_Field_Key, 
			TF.Timestamp,
			Data_Type, 
			F.QE_Field_Key,
			F.Field_Lookup_Key,
			Default_Display,
			Is_Custom,
			NULL AS Measurement_Applies_To,			-- So we get columns named
			NULL AS Measurement_Method_Concept_Key,
			NULL AS Measurement_Duration,
			NULL AS Measurement_Accuracy,
			NULL AS Measurement_Parameter_Concept_Key,
			NULL AS Measurement_Unit_Concept_Key,
			NULL AS Measurement_Is_Specimen,
			NULL AS Measurement_Is_TaxonData,
			NULL AS Taxon_Measurement_Qualifier_Key,
			NULL AS Taxon_Measurement_Unit_Key,
			Field_Name,
			Table_Name,
			isnull(TF.Sequence, (select count(*) from QE_Template_Field)),
			NULL AS Number_Type_Concept_Key,
			NULL AS Number_Preferred,
			Hidden,
			Locked,
			NULL AS Metadata_Type_Key 
		
	FROM  		QE_Field F 
	LEFT JOIN 	QE_Template_Field TF ON F.QE_Field_Key = TF.QE_Field_Key AND @QETemplateKey = TF.QE_Template_Key AND Is_Custom = 0
	WHERE 		(Template_Type & @TemplateType) <> 0

	UNION -- Measurements for Thesaurus determinations

	SELECT 	CT.Plaintext + ' (' + D.Item_Name + ')' COLLATE SQL_Latin1_General_CP1_CI_AS, 
		General_Tab,
		Specimen_Tab, 
		F.Item_Name, 
		Default_Value,
		20, 
		QE_Template_Field_Key, 
		F.Timestamp,
		0, 
		NULL,
		'',
		Default_Display,
	 	Is_Custom,
		Measurement_Applies_To,
		Measurement_Method_Concept_Key,
		Measurement_Duration,
		Measurement_Accuracy,
		Measurement_Parameter_Concept_Key,
		Measurement_Unit_Concept_Key,
		Measurement_Is_Specimen,
		Measurement_Is_TaxonData,
		Taxon_Measurement_Qualifier_Key,
		Taxon_Measurement_Unit_Key,
		NULL,
		NULL,
		isnull(F.Sequence, (select count(*) from QE_Template_Field)),
		NULL AS Number_Type_Concept_Key,
		NULL AS Number_Preferred,
		Hidden,
		Locked,
		NULL AS Metadata_Type_Key

	FROM	QE_Template_Field F
	JOIN	vw_ConceptTerm CT ON Concept_Key = F.Measurement_Parameter_Concept_Key
	JOIN 	Concept_Group CG ON CT.Concept_Group_Key = CG.Concept_Group_Key
	JOIN 	Local_Domain LD ON LD.Local_Domain_Key = CG.Local_Domain_Key
	JOIN 	Domain D ON D.Domain_Key = LD.Domain_Key AND (D.Has_Occurrences = 1 OR D.Domain_Key = 'SYSTEM00000000')
	WHERE 	Is_Custom = 1 AND QE_Template_Key = @QETemplateKey AND Measurement_Is_TaxonData = 0

	UNION -- Measurements for Taxon  determinations

	SELECT 	MT.Short_Name + ' of ' + MQ.Short_Name + ' (' + MU.Short_Name + ')' COLLATE SQL_Latin1_General_CP1_CI_AS, 
		General_Tab,
		Specimen_Tab, 
		F.Item_Name, 
		Default_Value,
		20, 
		QE_Template_Field_Key, 
		F.Timestamp,
		0, 
		NULL,
		'',
		Default_Display,
	 	Is_Custom,
		Measurement_Applies_To,
		NULL,	-- Irrelevant for Taxon
		NULL,	-- Irrelevant for Taxon
		Measurement_Accuracy,
		Measurement_Parameter_Concept_Key,
		Measurement_Unit_Concept_Key,
		Measurement_Is_Specimen,
		Measurement_Is_TaxonData,
		Taxon_Measurement_Qualifier_Key,
		Taxon_Measurement_Unit_Key,
		NULL,
		NULL,
		isnull(F.Sequence, (select count(*) from QE_Template_Field)),
		NULL AS Number_Type_Concept_Key,
		NULL AS Number_Preferred,
		Hidden,
		Locked,
		NULL AS Metadata_Type_Key

	FROM	QE_Template_Field F
	JOIN	Measurement_Qualifier MQ ON MQ.Measurement_Qualifier_Key = Taxon_Measurement_Qualifier_Key
	JOIN	Measurement_Unit MU ON MU.Measurement_Unit_Key = Taxon_Measurement_Unit_Key 
	JOIN	Measurement_Type MT ON MT.Measurement_Type_Key = MU.Measurement_Type_Key
	WHERE 	Is_Custom = 1 AND QE_Template_Key = @QETemplateKey AND Measurement_Is_Specimen = 0 AND Measurement_Is_TaxonData = 1

	UNION --Numbers

	SELECT 	CT.Plaintext COLLATE SQL_Latin1_General_CP1_CI_AS, 
		General_Tab,
		Specimen_Tab, 
		F.Item_Name, 
		Default_Value,
		20, 
		QE_Template_Field_Key, 
		F.Timestamp,
		0, 
		NULL,
		'',
		Default_Display,
	 	Is_Custom,
		NULL,
		NULL,
		NULL,
		NULL,
		NULL,
		NULL,
		NULL,
		NULL,
		NULL,
		NULL,
		NULL,
		NULL,
		isnull(F.Sequence, (select count(*) from QE_Template_Field)),
		F.Number_Type_Concept_Key,
		F.Number_Preferred,
		Hidden,
		Locked,
		NULL AS Metadata_Type_Key

	FROM	QE_Template_Field F
	JOIN	vw_ConceptTerm CT ON Concept_Key = F.Number_Type_Concept_Key
	WHERE 	Is_Custom = 2 AND QE_Template_Key = @QETemplateKey

	UNION --Metadata

	SELECT 	MT.Table_Name + ' ' + MT.Item_Name COLLATE SQL_Latin1_General_CP1_CI_AS, 
		General_Tab,
		Specimen_Tab, 
		F.Item_Name, 
		Default_Value,
		20, 
		QE_Template_Field_Key, 
		F.Timestamp,
		0, 
		NULL,
		'',
		Default_Display,
	 	Is_Custom,
		NULL,
		NULL,
		NULL,
		NULL,
		NULL,
		NULL,
		NULL,
		NULL,
		NULL,
		NULL,
		NULL,
		NULL,
		isnull(F.Sequence, (select count(*) from QE_Template_Field)),
		NULL AS Number_Type_Concept_Key,
		NULL AS Number_Preferred,
		Hidden,
		Locked,
		F.Metadata_Type_Key

	FROM	QE_Template_Field F
	JOIN	MetaData_Type MT ON MT.MetaData_Type_Key = F.Metadata_Type_Key
	WHERE 	Is_Custom = 3 AND QE_Template_Key = @QETemplateKey

	ORDER BY 26
GO 

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_QETemplateField_Select_ForTemplate') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_QETemplateField_Select_ForTemplate'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_QETemplateField_Select_ForTemplate TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_QETemplateField_Select_ForTemplate TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_QETemplateField_Select_ForTemplate TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_QETemplateField_Select_ForTemplate TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_QETemplateField_Select_ForTemplate TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_QETemplateField_Select_ForTemplate TO [Dev - JNCC SQL]
END
GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_QETemplateField_Update]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_QETemplateField_Update]
GO
    
/*===========================================================================*\
  Description:	

  Parameters:	

  Created:	August 2003

  Last revision information:
    $Revision: 5 $
    $Date: 2/02/09 16:56 $
    $Author: Pauldavies $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_QETemplateField_Update]
 	@Key as char(16),
	@Timestamp as timestamp,
	@GeneralTab as bit,
	@SpecimenTab as bit,
	@ItemName as varchar(100),
	@DefaultValue as varchar(200),
	@DefaultDisplay as varchar(200),
	@SessionID as varchar(16),
	@Sequence int,
	@NumberTypeConceptKey as varchar(16),
	@NumberPreferred as bit,
	@Hidden as bit,
	@Locked as bit,
	@MetadataTypeKey as varchar(16)
AS

Set Nocount off

update QE_Template_Field
	set 
	General_Tab = @GeneralTab,
	Specimen_Tab = @SpecimenTab,
	Item_Name = @ItemName,
	Default_Value = @DefaultValue,
	Default_Display = @DefaultDisplay,
	Changed_Session_ID= @SessionID,
	Sequence = @Sequence,
	Number_Type_Concept_Key = @NumberTypeConceptKey,
	Number_Preferred = @NumberPreferred,
	Hidden = @Hidden,
	Locked = @Locked,
	Metadata_Type_Key = @MetadataTypeKey
	where QE_Template_Field_Key= @Key and
	(timestamp = @Timestamp)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_QETemplateField_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_QETemplateField_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_QETemplateField_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_QETemplateField_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_QETemplateField_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_QETemplateField_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_QETemplateField_Update TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_SpecimenFieldData_Insert]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_SpecimenFieldData_Insert]
GO

/*===========================================================================*\
  Description:	Inserts a record in Specimen_Field_Data

  Parameters:	@CollectionUnitKey
		@OccurrenceKey
		@TaxonOccurrenceKey
		@InferredSurvey
		@InferredLocation
		@InferredSpatialRef
		@InferredSampleType
		@InferredDate
		@InferredCollectors
		@GatheringEvent
		@SessionID
		@Key			OUTPUT

  Created:	November 2003

  Last revision information:
    $Revision: 5 $
    $Date: 2/02/09 16:56 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SpecimenFieldData_Insert]
	@CollectionUnitKey char(16),
	@OccurrenceKey char(16),
	@TaxonOccurrenceKey char(16),
	@InferredSurvey tinyint,
	@InferredLocation tinyint,
	@InferredSpatialRef tinyint,
	@InferredSampleType tinyint,
	@InferredDate tinyint,
	@InferredCollectors tinyint,
	@InferredDeterminers tinyint,
	@GatheringEvent bit,
	@SessionID char(16),
	@Key char(16) OUTPUT
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	EXECUTE	spNextKey 'Specimen_Field_Data', @Key OUTPUT

	BEGIN TRANSACTION

		INSERT INTO Specimen_Field_Data (
			Specimen_Field_Data_Key, Collection_Unit_Key, Occurrence_Key, Taxon_Occurrence_Key,
			Inferred_Survey, Inferred_Location, Inferred_Spatial_Ref, Inferred_Sample_Type,
			Inferred_Date, Inferred_Collectors, Inferred_Determiners, Gathering_Event, Entered_Session_ID
		) VALUES (
			@Key, @CollectionUnitKey, @OccurrenceKey, @TaxonOccurrenceKey,
			@InferredSurvey, @InferredLocation, @InferredSpatialRef,
			@InferredSampleType, @InferredDate, @InferredCollectors,
			@InferredDeterminers, @GatheringEvent, @SessionID
		)

		IF @@Error <> 0 GOTO RollbackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SpecimenFieldData_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SpecimenFieldData_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SpecimenFieldData_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SpecimenFieldData_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SpecimenFieldData_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SpecimenFieldData_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SpecimenFieldData_Insert TO [Dev - JNCC SQL]
END
GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Specimens_Select_ForSearchByMetadata]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Specimens_Select_ForSearchByMetadata]
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Description:
	Returns Specimens data based on the search parameter for the specified type of metadata.

  Parameters:
	@UserDomainMask		User's Domain Mask restricting which records may be returned
	@SessionID 		User's SessionID
	@ShowCommonNames	Specifies whether or not Common Names should be shown
	@SearchText		Text to be searched on
	@SortOrderIndex		Index determining Sort Order
	@MetaDataType	The type of metadata to search on

  Created:
	September 2007
  Author:
	David Kelly, Dorset Software

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_Specimens_Select_ForSearchByMetadata] 
	@UserDomainMask INT,
	@SessionID CHAR(16),
	@ShowCommonNames BIT,
	@SearchText VARCHAR(100),
	@SortOrderIndex TINYINT,
	@MetaDataType VARCHAR(100)
AS
	SET NOCOUNT ON
	-- Set of options for better use of vw_ConceptTerm.
	SET ANSI_NULLS ON
	SET ANSI_PADDING ON
	SET ANSI_WARNINGS ON
	SET ARITHABORT ON
	SET CONCAT_NULL_YIELDS_NULL ON
	SET QUOTED_IDENTIFIER ON
	SET NO_BROWSETABLE OFF

	DECLARE @MetaDataTypeKey CHAR(16)
	SET @MetaDataTypeKey = (SELECT MetaData_Type_Key From Metadata_Type WHERE Item_Name = @MetaDataType AND Table_Name = 'Specimen')


	--Use temp table to build output, so silly data errors such as duplicate accessions
	-- don't duplicate in the list
	DECLARE @SpecimensSearch TABLE
	(
		[Item_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[Det_Item_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[Det_Item_Name] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[Item_Name] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[Life_Sciences] [bit] NULL,
		[Number] [varchar] (30) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[Hint] [text] COLLATE SQL_Latin1_General_CP1_CI_AS NULL 
	)

	INSERT INTO @SpecimensSearch (Item_Key, Life_Sciences, Hint) 
	SELECT 	SU.Collection_Unit_Key, SU.Life_Sciences, M.Text
	FROM 	Specimen_Unit SU
	JOIN 	Metadata M ON M.Record_Key=SU.Collection_Unit_Key
			AND M.Metadata_Type_Key=@MetaDataTypeKey
	JOIN 	Collection_Unit CU ON SU.Collection_Unit_Key = CU.Collection_Unit_Key 
	 		AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
			OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
	WHERE 	M.Text LIKE @SearchText + '%'


	UPDATE 	@SpecimensSearch
	SET	Det_Item_Key=CPref.Concept_Key,
		Item_Name = TPref.Item_Name,
		Number=CUN.Number,
		Det_Item_Name=TDet.Plaintext
	FROM @SpecimensSearch SU
	LEFT JOIN Collection_Unit_Number CUN ON SU.Item_key = CUN.Collection_Unit_Key 
			AND CUN.Type_Concept_Key = 'SYSTEM0000000001' AND CUN.Preferred=1
	INNER JOIN VW_SpecimenDetsEarth SDE ON SDE.Collection_Unit_Key=SU.Item_Key
		AND SDE.Preferred_Determination_Key=SDE.Determination_Key
	INNER JOIN Concept C ON C.Concept_Key=SDE.Concept_Key
	INNER JOIN Term TDet ON TDet.Term_Key=C.Term_Key
	INNER JOIN Concept CPref ON CPref.Meaning_Key=C.Meaning_Key
			AND CPref.List_Preferred=1
			AND CPref.Concept_Group_Key=C.Concept_Group_Key
	INNER JOIN Term TPref ON TPref.Term_Key=CPref.Term_Key


	UPDATE @SpecimensSearch
	SET	Det_Item_Key = SDL.Taxon_List_Item_Key,
		Item_Name = dbo.ufn_GetFormattedTaxonNameByParams(
				Preferred_Name,
				Preferred_Name_Italic,
				Common_Name,
				Common_Name_Italic,
				NULL,
				@ShowCommonNames),
		Number = CUN.Number,
		Det_Item_Name = ITN.Actual_Name
	FROM 	@SpecimensSearch SU
	LEFT JOIN Collection_Unit_Number CUN ON SU.Item_key = CUN.Collection_Unit_Key 
			AND CUN.Type_Concept_Key = 'SYSTEM0000000001' AND CUN.Preferred = 1
	INNER JOIN VW_SpecimenDetsLife SDL ON SDL.Collection_Unit_Key = SU.Item_Key
			AND SDL.Preferred_Taxon_Determination_Key = SDL.Taxon_Determination_Key
	INNER JOIN Index_Taxon_Name ITN	ON ITN.Taxon_List_Item_Key = SDL.Taxon_List_Item_Key

	-- Select table and sort appropriately
	IF @SortOrderIndex = 0
		SELECT * FROM @SpecimensSearch
		ORDER BY Det_Item_Name, Number
	ELSE IF @SortOrderIndex = 1
		SELECT * FROM @SpecimensSearch
		ORDER BY Number, Item_Name
GO
/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Specimens_Select_ForSearchByMetadata') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Specimens_Select_ForSearchByMetadata'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByMetadata TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByMetadata TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByMetadata TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByMetadata TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByMetadata TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByMetadata TO [Dev - JNCC SQL]
END

GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_StoreHierarchy_Select_ForSearchByMetadata]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_StoreHierarchy_Select_ForSearchByMetadata]
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

CREATE PROCEDURE [dbo].[usp_StoreHierarchy_Select_ForSearchByMetadata] 
@UserDomainMask INT,
@SessionID CHAR(16),
@SortOrderIndex TINYINT,
@SearchText VARCHAR(30),
@MetaDataType VARCHAR(100)

AS

--  DESCRIPTION
--  Returns a store hierarchy based on the search parameter for the specified type of metadata
--
--	PARAMETERS
--	NAME				DESCRIPTION
--	@UserDomainMask		User's Domain Mask restricting which records may be returned
--	@SessionID 			User's SessionID
--	@SortOrderIndex		Index determining Sort Order
--	@SearchText			Text to be used for search
--  @MetaDataType       The type of metadata to be searching on
--
--
--  AUTHOR:				David Kelly, Dorset Software
--  CREATED:			2007-09-04
--

	SET NOCOUNT ON
	DECLARE @MetaDataTypeKey CHAR(16)
	SET @MetaDataTypeKey = (SELECT MetaData_Type_Key From Metadata_Type WHERE Item_Name = @MetaDataType AND Table_Name = 'Store')

	
	-- Create a table variable to put the initial results into. We use the results in this
	-- table to work out whether there are any stores contained within it.
	DECLARE @StoreHierarchy TABLE
	(
		[Collection_Unit_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
		[Item_Name] [varchar] (150), 
		[Number] [varchar] (30) NULL,
		[Bottom_Level] [bit] NOT NULL,
		[Current_Location_Code] [varchar] (30) NULL
	)
	
	-- Insert the initials results into the table variable.
	INSERT INTO @StoreHierarchy (Collection_Unit_Key, Item_Name, Number, Bottom_Level, Current_Location_Code) 
	SELECT 		S.Collection_Unit_Key AS Item_Key, 
				S.Item_Name + IsNull(' - ' + CU.Current_Location_Code, IsNull(' - ' + CU.Usual_Location_Code, '')) AS Item_Name,  
				Number,
				0,
				CU.Current_Location_Code
	FROM 		Store AS S
	INNER JOIN	Collection_Unit AS CU ON CU.Collection_Unit_Key = S.Collection_Unit_Key
	    			AND ((CU.Domain_Mask & @UserDomainMask > 0) 
						OR (CU.Entered_Session_ID = @SessionID) 
						OR (CU.Changed_Session_ID = @SessionID) 
						OR (CU.Domain_Mask = 0))
	LEFT JOIN	Collection_Unit_Number AS CUN ON CUN.Collection_Unit_Key = S.Collection_Unit_Key 
					AND CUN.Type_Concept_Key = 'SYSTEM0000000001'  --REGISTRATION NUMBER
					AND CUN.Preferred = 1
	INNER JOIN METADATA MT ON S.Collection_Unit_Key = MT.Record_Key
					AND MT.MetaData_Type_Key = @MetaDataTypeKey
					AND MT.TEXT LIKE @SearchText + '%'
	WHERE		CU.Current_Container_Collection_Unit_Key IS NULL
	
	-- Work out whether the top level node is also the bottom level node.
	UPDATE		SH
	SET			SH.Bottom_Level = CASE WHEN S.Collection_Unit_Key IS NULL THEN 1 ELSE 0 END
	FROM		@StoreHierarchy AS SH
	LEFT JOIN 	Collection_Unit CU ON CU.Current_Container_Collection_Unit_Key = SH.Collection_Unit_Key 
					AND ((CU.Domain_Mask & @UserDomainMask > 0) 
						OR (CU.Entered_Session_ID = @SessionID) 
						OR (CU.Changed_Session_ID = @SessionID) 
						OR (CU.Domain_Mask = 0))
				-- Need to do a join onto Store because other Collection_Units could have their current container
				-- as this store. However, we are only interested in the Stores at the moment.
	LEFT JOIN 	Store AS S ON S.Collection_Unit_Key = CU.Collection_Unit_Key
	
	-- Select the results for the result set.
	SELECT		Collection_Unit_Key AS Item_Key, 
				Collection_Unit_Key AS Join_Key, 
				Item_Name, 
				Number, 
				Bottom_Level
	FROM		@StoreHierarchy
	ORDER BY 	CASE @SortOrderIndex WHEN 0 THEN Item_Name 
									 WHEN 1 THEN Number
									 WHEN 2 THEN Current_Location_Code
				END,
				CASE @SortOrderIndex WHEN 0 THEN Number
									 WHEN 1 THEN Item_Name
									 WHEN 2 THEN Item_Name
				END,
				CASE @SortOrderIndex WHEN 2 THEN Number
				END

GO
/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_StoreHierarchy_Select_ForSearchByMetadata') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_StoreHierarchy_Select_ForSearchByMetadata'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_StoreHierarchy_Select_ForSearchByMetadata TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_StoreHierarchy_Select_ForSearchByMetadata TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_StoreHierarchy_Select_ForSearchByMetadata TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_StoreHierarchy_Select_ForSearchByMetadata TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_StoreHierarchy_Select_ForSearchByMetadata TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_StoreHierarchy_Select_ForSearchByMetadata TO [Dev - JNCC SQL]
END

GO


If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Stores_Select_ForSearchByMetadata]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Stores_Select_ForSearchByMetadata]
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

CREATE PROCEDURE [dbo].usp_Stores_Select_ForSearchByMetadata 
@UserDomainMask INT,
@SessionID CHAR(16),
@SortOrderIndex TINYINT,
@SearchText VARCHAR(30),
@MetaDataType VARCHAR(100)

AS

--  DESCRIPTION
--  Returns Stores based on the search parameter for the specified type of metadata
--
--	PARAMETERS
--	NAME				DESCRIPTION
--	@UserDomainMask		User's Domain Mask restricting which records may be returned
--	@SessionID 			User's SessionID
--	@SortOrderIndex		Index determining Sort Order
--	@SearchText			Text to be used for search
--  @MetaDataType       The type of metadata to be searching on
--
--
--  AUTHOR:				David Kelly, Dorset Software
--  CREATED:			2007-09-04
--
SET NOCOUNT ON

DECLARE @MetaDataTypeKey CHAR(16)
SET @MetaDataTypeKey = (SELECT MetaData_Type_Key From Metadata_Type WHERE Item_Name = @MetaDataType AND Table_Name = 'Store')

IF @SortOrderIndex = 0
	SELECT S.Collection_Unit_Key AS Item_Key, 	
				S.Item_Name + IsNull(' - ' + CU.Current_Location_Code, IsNull(' - ' + CU.Usual_Location_Code, '')) AS Item_Name, 
				Number, S.Item_Name AS Hint
	FROM 
	STORE S
	    INNER JOIN
	   	    COLLECTION_UNIT CU 
	    ON S.Collection_Unit_Key = CU.Collection_Unit_Key
	       	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
				OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
		LEFT JOIN 
			COLLECTION_UNIT_NUMBER CUN 
		ON S.Collection_Unit_Key = CUN.Collection_Unit_Key 
			AND CUN.Type_Concept_Key = 'SYSTEM0000000001'  --REGISTRATION NUMBER
			AND CUN.Preferred = 1
		INNER JOIN
			METADATA MT
		ON S.Collection_Unit_Key = MT.Record_Key
			AND MT.MetaData_Type_Key = @MetaDataTypeKey
			AND MT.TEXT LIKE @SearchText + '%'
	ORDER BY S.Item_Name, Number
ELSE IF @SortOrderIndex = 1
	SELECT S.Collection_Unit_Key AS Item_Key, 	
				S.Item_Name + IsNull(' - ' + CU.Current_Location_Code, IsNull(' - ' + CU.Usual_Location_Code, '')) AS Item_Name, 
				Number, Number AS Hint
	FROM 
	STORE S
	    INNER JOIN
	   	    COLLECTION_UNIT CU 
	    ON S.Collection_Unit_Key = CU.Collection_Unit_Key
	       	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
				OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
		LEFT JOIN 
			COLLECTION_UNIT_NUMBER CUN 
		ON S.Collection_Unit_Key = CUN.Collection_Unit_Key 
			AND CUN.Type_Concept_Key = 'SYSTEM0000000001'  --REGISTRATION NUMBER
			AND CUN.Preferred = 1
		INNER JOIN
			METADATA MT
		ON S.Collection_Unit_Key = MT.Record_Key
			AND MT.MetaData_Type_Key = @MetaDataTypeKey
			AND MT.TEXT LIKE @SearchText + '%'
	ORDER BY Number, S.Item_Name
ELSE IF @SortOrderIndex = 2
	SELECT S.Collection_Unit_Key AS Item_Key, S.Item_Name, 	
				S.Item_Name + IsNull(' - ' + CU.Current_Location_Code, IsNull(' - ' + CU.Usual_Location_Code, '')) AS Item_Name, 
				CU.Current_Location_Code AS Hint
	FROM 
	STORE S
	    INNER JOIN
	   	    COLLECTION_UNIT CU 
	    ON S.Collection_Unit_Key = CU.Collection_Unit_Key
	       	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
				OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
		LEFT JOIN 
			COLLECTION_UNIT_NUMBER CUN 
		ON S.Collection_Unit_Key = CUN.Collection_Unit_Key 
			AND CUN.Type_Concept_Key = 'SYSTEM0000000001'  --REGISTRATION NUMBER
			AND CUN.Preferred = 1
		INNER JOIN
			METADATA MT
		ON S.Collection_Unit_Key = MT.Record_Key
			AND MT.MetaData_Type_Key = @MetaDataTypeKey
			AND MT.TEXT LIKE @SearchText + '%'
	ORDER BY Current_Location_Code, S.Item_Name, Number

GO
/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Stores_Select_ForSearchByMetadata') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Stores_Select_ForSearchByMetadata'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Stores_Select_ForSearchByMetadata TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Stores_Select_ForSearchByMetadata TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Stores_Select_ForSearchByMetadata TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Stores_Select_ForSearchByMetadata TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Stores_Select_ForSearchByMetadata TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Stores_Select_ForSearchByMetadata TO [Dev - JNCC SQL]
END

GO

IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Stores_Select_ForTopLevel]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
    DROP PROCEDURE [dbo].[usp_Stores_Select_ForTopLevel]
GO


/*===========================================================================*\
  Description:    Returns top level Stores data to the CollectionsBrowser.

  Parameters:
    @Key                Optional Key. When specified, only the single top level record is returned with that key
    @UserDomainMask     User's Domain Mask restricting which records may be returned
    @SessionID          User's SessionID
    @SortOrderIndex     Index determining Sort Order

  Created:    2003-08-15

  Last revision information:
    $Revision: 5 $
    $Date: 2/02/09 16:56 $
    $Author: Pauldavies $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_Stores_Select_ForTopLevel] 
    @UserDomainMask INT,
    @SessionID      CHAR(16),
    @SortOrderIndex TINYINT,
    @Key            CHAR(16) = NULL
AS
    SET NOCOUNT ON

    -- Create  a table to hold the items we are looking for
    DECLARE @Search TABLE (ItemKey CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS PRIMARY KEY)

    IF @Key IS NOT NULL
        INSERT INTO @Search VALUES (@Key)
    ELSE IF object_id('tempdb..#TempFilter') IS NOT NULL
        INSERT INTO @Search SELECT DISTINCT ItemKey FROM #TempFilter
    ELSE
        INSERT INTO @Search SELECT Collection_Unit_Key FROM Store

    DECLARE @Results	TABLE (
            Item_Key    CHAR(16)        NOT NULL,
            Item_Name   VARCHAR(160)    NOT NULL,
            Hint        VARCHAR(30)     NULL,
            Number      VARCHAR(30)     NULL
    )

    INSERT INTO @Results
    SELECT      S.Collection_Unit_Key,     
                S.Item_Name + IsNull(' - ' + CU.Current_Location_Code, IsNull(' - ' + CU.Usual_Location_Code, '')), 
                CU.Current_Location_Code,
                Number
    FROM        Store                   S
    INNER JOIN  Collection_Unit         CU  ON  CU.Collection_Unit_Key              =   S.Collection_Unit_Key
                                            AND ((CU.Domain_Mask & @UserDomainMask  >   0) 
                                            OR   (CU.Entered_Session_ID             =   @SessionID) 
                                            OR   (CU.Changed_Session_ID             =   @SessionID) 
                                            OR   (CU.Domain_Mask                    =   0))
    LEFT JOIN   Collection_Unit_Number  CUN ON  CUN.Collection_Unit_Key             =   S.Collection_Unit_Key 
                                            AND CUN.Preferred                       =   1
                                            -- Because of historical data, there could be multiple preferred.
                                            -- Only want one though.
                                            AND CUN.Collection_Unit_Number_Key      =   (
                                                SELECT  TOP 1 CUN2.Collection_Unit_Number_Key 
                                                FROM    Collection_Unit_Number CUN2 
                                                WHERE   CUN2.Collection_Unit_Key    =   CUN.Collection_Unit_Key 
                                                AND     CUN2.Preferred              =   1
                                            )
    INNER JOIN  @Search                 SR  ON     SR.ItemKey                       =   CU.Collection_Unit_Key


    IF @SortOrderIndex = 0
        SELECT      *
        FROM        @Results
        ORDER BY    Item_Name, Number
    ELSE
    IF @SortOrderIndex = 1
        SELECT      *
        FROM        @Results
        ORDER BY    Number, Item_Name
    ELSE
    IF @SortOrderIndex = 2
        SELECT      *
        FROM        @Results
        ORDER BY    Hint, Item_Name, Number
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Stores_Select_ForTopLevel') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_Stores_Select_ForTopLevel'
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        GRANT EXECUTE ON dbo.usp_Stores_Select_ForTopLevel TO [R2k_AddOnly]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
        GRANT EXECUTE ON dbo.usp_Stores_Select_ForTopLevel TO [R2k_Administrator]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
        GRANT EXECUTE ON dbo.usp_Stores_Select_ForTopLevel TO [R2k_FullEdit]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
        GRANT EXECUTE ON dbo.usp_Stores_Select_ForTopLevel TO [R2k_ReadOnly]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
        GRANT EXECUTE ON dbo.usp_Stores_Select_ForTopLevel TO [R2k_RecordCardsOnly]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_Stores_Select_ForTopLevel TO [Dev - JNCC SQL]
END

GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_SurveyEventGeoArea_Delete]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_SurveyEventGeoArea_Delete]
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

CREATE PROCEDURE [dbo].usp_SurveyEventGeoArea_Delete 
@SurveyEventGeoAreaKey CHAR(16)

AS

--  DESCRIPTION
--  Insert a record into Survey_Event_Geo_Area
--
--	PARAMETERS
--	NAME					DESCRIPTION
--	@SurveyEventGeoAreaKey	The Survey_Event_Geo_Area_Key of the record to be deleted
--
--
--  AUTHOR:				David Kelly, Dorset Software
--  CREATED:			2007-09-07
--

	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION

		DELETE	Survey_Event_Geo_Area
		WHERE	Survey_Event_Geo_Area_Key = @SurveyEventGeoAreaKey

		IF @@Error <> 0 GOTO RollbackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION

GO
/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SurveyEventGeoArea_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SurveyEventGeoArea_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SurveyEventGeoArea_Delete TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SurveyEventGeoArea_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SurveyEventGeoArea_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_SurveyEventGeoArea_Delete TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SurveyEventGeoArea_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SurveyEventGeoArea_Delete TO [Dev - JNCC SQL]
END

GO



If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_SurveyEventGeoArea_Insert]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_SurveyEventGeoArea_Insert]
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

CREATE PROCEDURE [dbo].usp_SurveyEventGeoArea_Insert 
@Key CHAR(16) OUTPUT,
@SurveyEventKey CHAR(16),
@ConceptKey CHAR(16),
@EnteredBy CHAR(16)

AS

--  DESCRIPTION
--  Insert a record into Survey_Event_Geo_Area
--
--	PARAMETERS
--	NAME				DESCRIPTION
--	@Key				The Survey_Event_Geo_Area_Key of the new record
--  @SurveyEventKey		The Survey_Event_Key of the new record
--	@Concept_Key		The Concept_Key of the new record
--	@EnteredBy			The user who enetered the record
--
--
--  AUTHOR:				David Kelly, Dorset Software
--  CREATED:			2007-09-07
--


	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	--find out whether this Survey Event Geo Area alreadly exists
	SET @Key = NULL
	SELECT	@Key = Survey_Event_Geo_Area_Key 
	FROM 	Survey_Event_Geo_Area
	WHERE	Concept_Key = @ConceptKey
	AND	Survey_Event_Key = @SurveyEventKey
	
	IF @Key IS NULL
	BEGIN
		BEGIN TRANSACTION
			EXECUTE spNextKey 'Survey_Event_Geo_Area', @Key OUTPUT

			INSERT INTO Survey_Event_GeoArea (
				Survey_Event_Geo_Area_Key,
				Survey_Event_Key,
				Concept_Key,
				Entered_By
			) VALUES (
				@Key,
				@SurveyEventKey,
				@ConceptKey,
				@EnteredBy
			)

			IF @@Error <> 0 GOTO RollBackAndExit

		COMMIT TRANSACTION
	END

	RETURN 0

RollBackAndExit:
	ROLLBACK TRANSACTION

GO
/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SurveyEventGeoArea_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SurveyEventGeoArea_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SurveyEventGeoArea_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SurveyEventGeoArea_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SurveyEventGeoArea_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_SurveyEventGeoArea_Insert TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SurveyEventGeoArea_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SurveyEventGeoArea_Insert TO [Dev - JNCC SQL]
END

GO



If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_SurveyEventGeoArea_Select]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_SurveyEventGeoArea_Select]
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

CREATE PROCEDURE [dbo].usp_SurveyEventGeoArea_Select 
@Key CHAR(16)

AS

--  DESCRIPTION
--  Returns all the Geo Areas associated with the survey event.
--
--	PARAMETERS
--	NAME				DESCRIPTION
--	@Key				They Survey_Event_Key to search on
--
--
--  AUTHOR:				David Kelly, Dorset Software
--  CREATED:			2007-09-07
--
SET NOCOUNT ON

SELECT
	T.Plaintext,
	T.Term_Key,
	SEGA.Survey_Event_Geo_Area_Key,
	SEGA.Survey_Event_Key,
	SEGA.Concept_Key,
	SE.Custodian,
	SEGA.[Timestamp]
FROM
	Term T
	JOIN Concept C ON C.Term_Key = T.Term_Key
	JOIN Concept C1 ON C1.Meaning_Key = C.Meaning_Key AND C.List_Preferred = 1
	JOIN Survey_Event_Geo_Area SEGA ON SEGA.Concept_Key = C1.Concept_Key
	JOIN Survey_Event SE ON SE.Survey_Event_Key = SEGA.Survey_Event_Key
WHERE
	SEGA.Survey_Event_Key = @Key
ORDER BY
	C1.Sort_Code, T.Plaintext

GO
/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SurveyEventGeoArea_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SurveyEventGeoArea_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SurveyEventGeoArea_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SurveyEventGeoArea_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SurveyEventGeoArea_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_SurveyEventGeoArea_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SurveyEventGeoArea_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SurveyEventGeoArea_Select TO [Dev - JNCC SQL]
END

GO



If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_SurveyEventGeoArea_Select_ForFieldData]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_SurveyEventGeoArea_Select_ForFieldData]
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

CREATE PROCEDURE [dbo].usp_SurveyEventGeoArea_Select_ForFieldData 
@Key CHAR(16)

AS

--  DESCRIPTION
--  Returns all the Geo Areas associated with the survey event.
--
--	PARAMETERS
--	NAME				DESCRIPTION
--	@Key				They Survey_Event_Key to search on
--
--
--  AUTHOR:				David Kelly, Dorset Software
--  CREATED:			2007-09-07
--
SET NOCOUNT ON

SELECT
	T.Plaintext,
	T.Term_Key,
	SEGA.Survey_Event_Geo_Area_Key,
	SEGA.Survey_Event_Key,
	SEGA.Concept_Key,
	SFD.Custodian,
	SFD.[Timestamp]
FROM
	Specimen_Field_Data SFD			
	LEFT JOIN 	Taxon_Occurrence XO ON XO.Taxon_Occurrence_Key=SFD.Taxon_Occurrence_Key
	LEFT JOIN 	Occurrence O ON O.Occurrence_Key=SFD.Occurrence_Key
	INNER JOIN 	[Sample] S ON S.Sample_Key=XO.Sample_Key OR S.Sample_Key=O.Sample_Key
	INNER JOIN Survey_Event SE ON SE.Survey_Event_Key = S.Survey_Event_Key
	INNER JOIN Survey_Event_Geo_Area SEGA ON SEGA.Survey_Event_Key = SE.Survey_Event_Key
	INNER JOIN Concept C ON C.Concept_Key = SEGA.Concept_Key
	INNER JOIN Concept C1 ON C1.Meaning_Key = C.Meaning_Key AND C1.List_Preferred = 1
	INNER JOIN Term T ON T.Term_Key = C1.Term_Key

WHERE
	SFD.Specimen_Field_Data_Key = @Key
ORDER BY
	C1.Sort_Code, T.Plaintext

GO
/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SurveyEventGeoArea_Select_ForFieldData') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SurveyEventGeoArea_Select_ForFieldData'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SurveyEventGeoArea_Select_ForFieldData TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SurveyEventGeoArea_Select_ForFieldData TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SurveyEventGeoArea_Select_ForFieldData TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_SurveyEventGeoArea_Select_ForFieldData TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SurveyEventGeoArea_Select_ForFieldData TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SurveyEventGeoArea_Select_ForFieldData TO [Dev - JNCC SQL]
END

GO




If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_SurveyEventKey_Get_ForFieldData]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_SurveyEventKey_Get_ForFieldData]
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

CREATE PROCEDURE [dbo].usp_SurveyEventKey_Get_ForFieldData 
	@Key char(16),
	@SurveyEventKey char(16) OUTPUT

AS

--  DESCRIPTION
--  Insert a record into Survey_Event_Geo_Area
--
--	PARAMETERS
--	NAME				DESCRIPTION
--	@Key				The Specimen_Field_Data_Key of the record
--  @SurveyEventKey		The Survey_Event_Key of the record
--
--
--  AUTHOR:				David Kelly, Dorset Software
--  CREATED:			2007-09-07
--


	-- Required to be able to get number of changed records.
	SET NOCOUNT ON

	SELECT 	@SurveyEventKey = S.Survey_Event_Key
	FROM
	Specimen_Field_Data SFD			
	LEFT JOIN 	Taxon_Occurrence XO ON XO.Taxon_Occurrence_Key=SFD.Taxon_Occurrence_Key
	LEFT JOIN 	Occurrence O ON O.Occurrence_Key=SFD.Occurrence_Key
	INNER JOIN 	[Sample] S ON S.Sample_Key=XO.Sample_Key OR S.Sample_Key=O.Sample_Key
	WHERE	SFD.Specimen_Field_Data_Key = @Key


GO
/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SurveyEventKey_Get_ForFieldData') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SurveyEventKey_Get_ForFieldData'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SurveyEventKey_Get_ForFieldData TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SurveyEventKey_Get_ForFieldData TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SurveyEventKey_Get_ForFieldData TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_SurveyEventKey_Get_ForFieldData TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SurveyEventKey_Get_ForFieldData TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SurveyEventKey_Get_ForFieldData TO [Dev - JNCC SQL]
END

GO



/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_TaxonListItem_ImportConceptGroup]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_TaxonListItem_ImportConceptGroup]
GO

/*===========================================================================*\
  Description:	Import taxon list items corresponding to the concepts in a
				concept group.

  Parameters:	@job_id					Job identifier

  Created:		Dec 2003

  Last revision information:
	$Revision: 5 $
	$Date: 2/02/09 16:56 $
	$Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_TaxonListItem_ImportConceptGroup]
	@job_id				INT
AS
	SET NOCOUNT ON

	DECLARE     @concept_group_key		CHAR(16),
				@concept_key			CHAR(16),
				@taxon_list_item_key	CHAR(16),
				@term_key				CHAR(16),
				@term_version_key		CHAR(16),
				@list_preferred			BIT,
				@meaning_key			CHAR(16),
				@taxon_key				CHAR(16),
				@taxon_version_key		CHAR(16),
				@preferred_name			CHAR(16),
				@sort_code				INT,
				@taxon_rank_key			CHAR(16),
				@ins_user_key			CHAR(16),
				@ins_date				SMALLDATETIME,
				@upd_user_key			CHAR(16),
				@upd_date				SMALLDATETIME,
				@system					BIT,
				@from_list_version_key	CHAR(16),
				@to_list_version_key	CHAR(16),
				@is_new					BIT

	/* determine parameters of job */
	SELECT      @concept_group_key			=	j.Concept_Group_Key
	FROM		Import_Export_Job			AS	j
	WHERE		j.Import_Export_Job_ID		=	@job_id

	IF @@ROWCOUNT = 0
	BEGIN
		RAISERROR ('Job does not exist or has not been configured', 16, 1)
		GOTO fail
	END

	EXECUTE		usp_Import_Export_Job_UpdateStatus	@job_id,
													'Exporting concepts'
	IF @@ERROR <> 0 GOTO fail

	DECLARE		concepts	CURSOR FAST_FORWARD LOCAL FOR
	SELECT		c.Concept_Key,
				c.Term_Key,
				c.Term_Version_Key,
				c.List_Preferred,
				c.Meaning_Key,
				c.Sort_Code,
				crm.Taxon_Rank_Key,
				es.User_Name_Key,
				CONVERT(SMALLDATETIME,
						CONVERT(VARCHAR, es.Date_Time_Start, 112)),
				cs.User_Name_Key,
				CONVERT(SMALLDATETIME,
						CONVERT(VARCHAR, cs.Date_Time_Start, 112)),
				c.System_Supplied_Data
	FROM		Concept									AS	c
	INNER JOIN	Taxon_Dictionary_Concept_Rank_Mapping	AS	crm
	ON			crm.Concept_Rank_Key					=	c.Concept_Rank_Key
	INNER JOIN	Session									AS	es
	ON			es.Session_ID							=	c.Entered_Session_ID
	LEFT JOIN	Session									AS	cs
	ON			cs.Session_ID							=	c.Changed_Session_ID
	WHERE		c.Concept_Group_Key						=	@concept_group_key
	ORDER BY	c.List_Preferred DESC	/* i.e. preferred names first */

	OPEN		concepts

	WHILE 1 = 1
	BEGIN
		FETCH		concepts
		INTO		@concept_key,
					@term_key,
					@term_version_key,
					@list_preferred,
					@meaning_key,
					@sort_code,
					@taxon_rank_key,
					@ins_user_key,
					@ins_date,
					@upd_user_key,
					@upd_date,
					@system

		IF @@FETCH_STATUS <> 0 BREAK

		BEGIN TRANSACTION

		SET ROWCOUNT 1

		SELECT      @from_list_version_key							=	gvm.Taxon_List_Version_Key
		FROM		Concept_History									AS	h
		INNER JOIN	Concept_Group_Version							AS	gv
		ON			gv.Concept_Group_Version_Key					=	h.Concept_Group_Version_From
		INNER JOIN	Taxon_Dictionary_Concept_Group_Version_Mapping	AS	gvm
		ON			gvm.Concept_Group_Version_Key					=	gv.Concept_Group_Version_Key
		WHERE		h.Concept_Key									=	@concept_key
		ORDER BY	gv.Sequence

		--If the from list version is null (because the thesaurus allows no history), 
		--then use the first version available	
		SET ROWCOUNT 1
		
		IF @from_list_version_key IS NULL
			SELECT @from_list_version_key = Taxon_List_Version_Key
			FROM Concept c
			INNER JOIN Taxon_Dictionary_Concept_Group_Mapping cgm ON cgm.Concept_Group_Key=c.Concept_Group_Key
			INNER JOIN Taxon_List_Version tlv on tlv.Taxon_list_Key=cgm.Taxon_List_Key
			WHERE c.Concept_Key=@concept_Key
			ORDER BY tlv.Version ASC

		/* we do the term and term version mappings inside the 'SET ROWCOUNT 1'
		 * because a single term may map onto multiple taxa; we need to choose
		 * exactly one, preferabyly one that is already correctly mapper
		 */
		SELECT		@Taxon_Key=tm.Taxon_Key
		FROM		Taxon_Dictionary_Term_Mapping tm
		LEFT JOIN Taxon_Version tv ON tv.Taxon_Key=tm.Taxon_Key
		LEFT JOIN Taxon_Dictionary_Term_Version_Mapping tvm 
			ON tvm.Taxon_Version_Key=tv.Taxon_Version_Key
			AND tvm.Term_Version_key=@term_version_key
		WHERE		tm.Term_Key						=	@term_key
		ORDER BY tvm.Term_Version_Key DESC

		IF @@ROWCOUNT = 0 GOTO skip_item

		SET ROWCOUNT 0

		SELECT		@to_list_version_key							=	gvm.Taxon_List_Version_Key
		FROM		Concept_History									AS	h
		INNER JOIN	Concept_Group_Version							AS	gv
		ON			gv.Concept_Group_Version_Key					=	h.Concept_Group_Version_To
		INNER JOIN	Taxon_Dictionary_Concept_Group_Version_Mapping	AS	gvm
		ON			gvm.Concept_Group_Version_Key					=	gv.Concept_Group_Version_Key
		WHERE		h.Concept_Key									=	@concept_key
		ORDER BY	gv.Sequence DESC

		IF @@ROWCOUNT = 0
		BEGIN
			SET			@to_list_version_key	=	NULL
		END

		IF @term_version_key IS NULL
		BEGIN
			SET			@taxon_version_key	=	NULL
		END
		ELSE
		BEGIN
			SELECT		@taxon_version_key						=	m.Taxon_Version_Key
			FROM		Taxon_Dictionary_Term_Version_Mapping	AS	m
			INNER JOIN	TAXON_VERSION							AS	tv
			ON			tv.TAXON_VERSION_KEY					=	m.Taxon_Version_Key
			WHERE		m.Term_Version_Key						=	@term_version_key
			AND			tv.TAXON_KEY							=	@taxon_key

			IF @@ROWCOUNT = 0 GOTO skip_item				
		END

		SET ROWCOUNT 0

		/* check for existing mapping */
		SELECT		@taxon_list_item_key				=	Taxon_List_Item_Key
		FROM		Taxon_Dictionary_Concept_Mapping
		WHERE		Concept_Key							=	@concept_key

		SET			@is_new		=	CASE WHEN @@ROWCOUNT = 0 THEN 1 ELSE 0 END

		IF @is_new = 1
		BEGIN
			EXECUTE		spNextKey	'TAXON_LIST_ITEM',
									@taxon_list_item_key	OUTPUT
			IF @@ERROR <> 0 GOTO fail_from_cursor
		END

		/* work out preferred name */
		IF @list_preferred = 1
		BEGIN
			SET			@preferred_name		=	@taxon_list_item_key
		END
		ELSE
		BEGIN
			SET			@preferred_name						=	NULL

			SET ROWCOUNT 1

			SELECT		@preferred_name						=	m.Taxon_List_Item_Key
			FROM		Concept								AS	c
			INNER JOIN 	Taxon_Dictionary_Concept_Mapping	AS	m
			ON			m.Concept_Key						=	c.Concept_Key
			WHERE		c.Concept_Group_Key					=	@concept_group_key
			AND			c.Meaning_Key						=	@meaning_key
			AND			c.List_Preferred					=	1

			SET ROWCOUNT 0
		END

		IF @preferred_name IS NOT NULL AND @taxon_version_key IS NULL
		BEGIN
			/* create a minimal taxon version */
			EXECUTE		spNextKey	'TAXON_VERSION',
									@taxon_version_key	OUTPUT
			IF @@ERROR <> 0 GOTO fail_from_cursor

			INSERT		TAXON_VERSION (
						TAXON_VERSION_KEY,
						TAXON_KEY,
						ENTERED_BY,
						ENTRY_DATE,
						SYSTEM_SUPPLIED_DATA)
			VALUES		(@taxon_version_key,
						@taxon_key,
						@ins_user_key,
						@ins_date,
						@system)

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END

		IF @is_new = 0
		BEGIN
			IF @preferred_name IS NULL
			BEGIN
				/* concept is not selectable; remove any list items */
				DELETE		Taxon_Dictionary_Concept_Mapping
				WHERE		TAXON_LIST_ITEM_KEY		=	@taxon_list_item_key
				
				DELETE		Taxon_Common_Name
				WHERE		TAXON_LIST_ITEM_KEY		=	@taxon_list_item_key

				DELETE		Index_Taxon_Name
				WHERE		TAXON_LIST_ITEM_KEY		=	@taxon_list_item_key

				DELETE		Index_Taxon_Synonym
				WHERE		TAXON_LIST_ITEM_KEY		=	@taxon_list_item_key
				OR			Synonym_List_Item_Key	=	@taxon_list_item_key

				DELETE		Index_Taxon_Group
				WHERE		TAXON_LIST_ITEM_KEY		=	@taxon_list_item_key
				OR			Contained_List_Item_Key	=	@taxon_list_item_key

				DELETE		TAXON_LIST_ITEM
				WHERE		TAXON_LIST_ITEM_KEY		=	@taxon_list_item_key

				IF @@ERROR <> 0 GOTO fail_from_cursor
			END
			ELSE
			BEGIN
				/* update taxon list item */
				UPDATE		TAXON_LIST_ITEM
				SET			TAXON_VERSION_KEY		=	@taxon_version_key,
							TAXON_LIST_VERSION_KEY	=	@from_list_version_key,
							TAXON_LIST_VERSION_TO	=	@to_list_version_key,
							PREFERRED_NAME			=	@preferred_name,
							SORT_CODE				=	@sort_code,
							TAXON_RANK_KEY			=	@taxon_rank_key,
							ENTERED_BY				=	@ins_user_key,
							ENTRY_DATE				=	@ins_date,
							CHANGED_BY				=	@upd_user_key,
							CHANGED_DATE			=	@upd_date,
							SYSTEM_SUPPLIED_DATA	=	@system
				WHERE		TAXON_LIST_ITEM_KEY		=	@taxon_list_item_key

				IF @@ERROR <> 0 GOTO fail_from_cursor
			END
		END
		ELSE
		BEGIN
			/* create taxon list item */
			INSERT		TAXON_LIST_ITEM (
						TAXON_LIST_ITEM_KEY,
						TAXON_VERSION_KEY,
						TAXON_LIST_VERSION_KEY,
						TAXON_LIST_VERSION_TO,
						PREFERRED_NAME,
						SORT_CODE,
						TAXON_RANK_KEY,
						ENTERED_BY,
						ENTRY_DATE,
						CHANGED_BY,
						CHANGED_DATE,
						SYSTEM_SUPPLIED_DATA)
			VALUES		(@taxon_list_item_key,
						@taxon_version_key,
						@from_list_version_key,
						@to_list_version_key,
						@preferred_name,
						@sort_code,
						@taxon_rank_key,
						@ins_user_key,
						@ins_date,
						@upd_user_key,
						@upd_date,
						@system)

			IF @@ERROR <> 0 GOTO fail_from_cursor

			/* record mapping */
			INSERT		Taxon_Dictionary_Concept_Mapping (
						Taxon_List_Item_Key,
						Concept_key)
			VALUES		(@taxon_list_item_key,
						@concept_key)

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END
		
skip_item:
		EXECUTE		usp_Import_Export_Job_RecordProcessed	@job_id
		IF @@ERROR <> 0 GOTO fail_from_cursor

		COMMIT TRANSACTION
	END

	CLOSE		concepts
	DEALLOCATE	concepts
	RETURN

fail_from_cursor:
	CLOSE		concepts
	DEALLOCATE	concepts

fail:
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_TaxonListItem_ImportConceptGroup failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TaxonListItem_ImportConceptGroup') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_TaxonListItem_ImportConceptGroup'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_TaxonListItem_ImportConceptGroup TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_TaxonListItem_ImportConceptGroup TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_TaxonListItem_ImportConceptGroup TO [Dev - JNCC SQL]
END
GO

