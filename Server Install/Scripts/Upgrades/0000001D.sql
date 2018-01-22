SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptRanks_FixDomain]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptRanks_FixDomain]
GO

/*===========================================================================*\
  Description:	When pasting concepts into a concept group, ensure that the
		ranks are also copied

  Parameters:	@FixDomainKey - domain key which should be checked for fixing
							@SessionID
							@SystemSuppliedData - both used only if new ranks must be created.

  Created:	July 2004

  Last revision information:
    $Revision: 3 $
    $Date: 2/02/09 16:47 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptRanks_FixDomain]
	@FixDomainKey CHAR(16),	
	@SessionID CHAR(16),
	@SystemSuppliedData bit
AS

BEGIN TRANSACTION

-- cursor variables
DECLARE	@ConceptRankKey CHAR(16)

DECLARE @ExistingRankKey CHAR(16)
DECLARE @NewRankKey CHAR(16)

DECLARE csr CURSOR FOR
	SELECT distinct
		C.Concept_Rank_Key
	FROM Concept c
	INNER JOIN Concept_Rank cr ON cr.Concept_Rank_Key=c.Concept_Rank_Key
	INNER JOIN concept_group cg ON cg.Concept_Group_Key=c.Concept_Group_Key
	INNER JOIN Local_Domain ld ON ld.Local_Domain_Key=cg.Local_Domain_Key
	WHERE ld.Domain_Key<>cr.Domain_Key
	AND LD.Domain_Key=@FixDomainKey
OPEN csr

WHILE (1=1)
BEGIN

	FETCH NEXT FROM csr 
	INTO @ConceptRankKey

	IF @@FETCH_STATUS <>0 BREAK

	EXECUTE spNextKey 'Concept_Rank', @NewRankKey OUTPUT

	INSERT INTO Concept_Rank (
				Concept_Rank_Key,
				Domain_Key,
				Item_Name, 
				Sort_Order, 
				Abbreviation, 
				Color_R, 
				Color_G, 
				Color_B, 
				Entered_Session_ID,
				System_Supplied_Data,
				Custodian)
			SELECT 
				@NewRankKey, 
				@FixDomainKey, 
				Item_Name, 
				Sort_Order, 
				Abbreviation, 
				Color_R, 
				Color_G, 
				Color_B, 
				@SessionID,
				@SystemSuppliedData,
				LEFT(@NewRankKey, 8)
			FROM Concept_Rank
			WHERE Concept_Rank_Key=@ConceptRankKey
	IF @@Error<>0 GOTO Rollbackandexit

	UPDATE C
	SET C.Concept_Rank_Key=@NewRankKey
	FROM Concept C
	INNER JOIN Concept_Group CG ON CG.Concept_Group_Key=C.Concept_Group_Key
	INNER JOIN Local_Domain LD ON LD.Local_Domain_Key=CG.Local_Domain_Key
	WHERE C.Concept_Rank_Key=@ConceptRankKey
	AND LD.Domain_Key=@FixDomainKey

END


CLOSE csr
DEALLOCATE csr
   


COMMIT TRANSACTION
RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptRanks_FixDomain') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptRanks_FixDomain'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptRanks_FixDomain TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptRanks_FixDomain TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptRanks_FixDomain TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptRanks_FixDomain TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptRanks_FixDomain TO [Dev - JNCC SQL]
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
    $Revision: 3 $
    $Date: 2/02/09 16:47 $
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
	EXEC usp_ConceptLineage_DeleteSubtree @SrcConceptGroupKey, @Lineage
	IF @@Error <> 0 GOTO RollbackAndExit
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

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_FieldData_Insert]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_FieldData_Insert]
GO

/*===========================================================================*\
  Description:	Inserts a record into the Specimen_Field_Data table, and
		updates records in others. This stored proc. differs from 
		usp_SpecimenFieldData insert because this updates records in 
		the Sample and Survey_Event tables.

  Parameters:	@Key OUTPUT
		@SurveyEventKey 
	        @SurveyKey 
	        @LocationKey 
		@SampleKey 
	        @SpatialRefQualifier 
	        @SpatialRef 
	        @GatheringMethod 
	        @VagueDateStart 
	        @VagueDateEnd 
	        @VagueDateType
		@CollectionUnitKey 
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

  Created:	January 2004

  Last revision information:
    $Revision: 3 $
    $Date: 2/02/09 16:47 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_FieldData_Insert]
	@Key char(16) OUTPUT,
	@SurveyEventKey char(16) = NULL,
	@SurveyKey char(16),
	@LocationKey char(16),
	@LocationName varchar(100) = NULL,
	@SampleKey char(16),
	@SpatialRefQualifier varchar(20),
	@SpatialRef varchar(40),
	@Lat float,
	@Long float,
        @GatheringMethod char(16),
        @VagueDateStart int,
        @VagueDateEnd int,
        @VagueDateType varchar(2) = NULL,
	@CollectionUnitKey char(16),
	@OccurrenceKey char(16),
	@TaxonOccurrenceKey char(16),
	@InferredSurvey tinyint,
	@InferredLocation tinyint,
	@InferredSpatialRef tinyint,
	@InferredSampleType tinyint,
	@InferredDate tinyint,
	@InferredCollectors tinyint,
	@GatheringEvent bit,
	@SessionID char(16)
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF
	SET ANSI_NULLS ON

	BEGIN TRANSACTION
		
		UPDATE 	[Sample]
		SET 	Location_Key = @LocationKey,
			Vague_Date_Start = @VagueDateStart, 
			Vague_Date_End = @VagueDateEnd, 
			Vague_Date_Type = IsNull(@VagueDateType, 'U'),
			Spatial_Ref_Qualifier = @SpatialRefQualifier,
			Spatial_Ref = @SpatialRef,
			Sample_Type_Key = @GatheringMethod,
			Lat = @Lat,
			Long = @Long,
			Location_Name = @LocationName
		WHERE	Sample_Key = @SampleKey

		IF @@Error <> 0 GOTO RollbackAndExit

		-- Update the Survey Event table so that is points to the
		-- correct Survey record.

		IF @SurveyEventKey = NULL 
			SET @SurveyEventKey = (SELECT	Survey_Event_Key
						FROM	[Sample]
						WHERE	Sample_Key = @SampleKey)
		UPDATE	Survey_Event
		SET	Survey_Key = @SurveyKey
		WHERE	Survey_Event_Key = @SurveyEventKey

		IF @@Error <> 0 GOTO RollbackAndExit

		EXECUTE	spNextKey 'Specimen_Field_Data', @Key OUTPUT

		INSERT INTO Specimen_Field_Data (
			Specimen_Field_Data_Key, 
			Collection_Unit_Key, 
			Occurrence_Key, 
			Taxon_Occurrence_Key,
			Inferred_Survey, 
			Inferred_Location, 
			Inferred_Spatial_Ref, 
			Inferred_Sample_Type,
			Inferred_Date, 
			Inferred_Collectors, 
			Gathering_Event, 
			Entered_Session_ID
		) VALUES (
			@Key, 
			@CollectionUnitKey, 
			@OccurrenceKey, 
			@TaxonOccurrenceKey,
			@InferredSurvey, 
			@InferredLocation, 
			@InferredSpatialRef,
			@InferredSampleType, 
			@InferredDate, 
			@InferredCollectors,
			@GatheringEvent, 
			@SessionID
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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_FieldData_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_FieldData_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_FieldData_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_FieldData_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_FieldData_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_FieldData_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_FieldData_Insert TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_FieldData_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_FieldData_Select]
GO

/*===========================================================================*\
  Description:	Returns data for the Field Data frame.

  Parameters:	@Key	Collection key

  Created:	October 2003

  Last revision information:
    $Revision: 3 $
    $Date: 2/02/09 16:47 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_FieldData_Select]
	@Key char(16)
AS

SET NOCOUNT ON

	SELECT	SV.Survey_Key,
		SV.Item_Name + ' - ' + dbo.ufn_GetFormattedName(SV.Run_By) AS Survey_Name,
		SV.Run_By, 
		SFD.Inferred_Survey,
		S.Sample_Key,
		SE.Survey_Event_Key,
		S.Location_Key,
		LN.Item_Name as Location,
		S.Location_Name,
		CASE WHEN S.Location_Key IS NULL THEN S.Spatial_Ref_System ELSE L.Spatial_Ref_System END AS Spatial_Ref_System,
		SFD.Inferred_Location,
		S.Spatial_Ref_Qualifier,
		S.Spatial_Ref,
		S.Lat, 
		S.Long,
		SFD.Inferred_Spatial_Ref,
		S.Sample_Type_Key,
		ST.Short_Name AS Sample_Type,
		S.Vague_Date_Start,
		S.Vague_Date_End,
		S.Vague_Date_Type,
		SFD.Taxon_Occurrence_Key,
		SFD.Occurrence_Key,
		SFD.Inferred_Sample_Type,
		SFD.Inferred_Date,
		SFD.Inferred_Collectors,
		SFD.Gathering_Event,
	  	SFD.Custodian,
		SFD.[Timestamp]
	FROM Specimen_Field_Data SFD
	LEFT JOIN Taxon_Occurrence XO ON XO.Taxon_Occurrence_Key=SFD.Taxon_Occurrence_Key
	LEFT JOIN Occurrence O ON O.Occurrence_Key=SFD.Occurrence_Key
	INNER JOIN [Sample] S ON S.Sample_Key=XO.Sample_Key OR S.Sample_Key=O.Sample_Key
	INNER JOIN Survey_Event SE ON SE.Survey_Event_Key=S.Survey_Event_Key
	INNER JOIN Survey SV ON SV.Survey_Key=SE.Survey_Key
	LEFT JOIN Location AS L ON L.Location_Key = S.Location_Key
	LEFT JOIN Location_Name LN ON LN.Location_Key=S.Location_Key
				AND LN.Preferred = 1
	INNER JOIN Sample_Type ST ON ST.Sample_Type_Key=S.Sample_Type_Key
	WHERE SFD.Specimen_Field_Data_Key = @Key

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_FieldData_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_FieldData_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_FieldData_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_FieldData_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_FieldData_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_FieldData_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_FieldData_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_FieldData_Select TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_FieldData_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_FieldData_Update]
GO

/*===========================================================================*\
  Description:	Updates tables when information in the field data frame is
		changed.

  Parameters:	@Key 
		@SurveyEventKey 
	        @SurveyKey 
	        @LocationKey 
		@LocationName
		@SampleKey 
	        @SpatialRefQualifier 
	        @SpatialRef 
	        @GatheringMethod 
	        @VagueDateStart 
	        @VagueDateEnd 
	        @VagueDateType
		@CollectionUnitKey 
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
		@Timestamp

  Created:	October 2003

  Last revision information:
    $Revision: 3 $
    $Date: 2/02/09 16:47 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_FieldData_Update]
	@Key char(16),
	@SurveyEventKey char(16) = NULL,
	@SurveyKey char(16),
	@LocationKey char(16),
	@LocationName varchar(100) = NULL,
	@SampleKey char(16),
	@SpatialRefQualifier varchar(20),
	@SpatialRef varchar(40),
	@GatheringMethod char(16),
	@VagueDateStart int,
	@VagueDateEnd int,
	@VagueDateType varchar(2) = NULL,
	@CollectionUnitKey char(16),
	@OccurrenceKey char(16),
	@TaxonOccurrenceKey char(16),
	@InferredSurvey tinyint,
	@InferredLocation tinyint,
	@InferredSpatialRef tinyint,
	@InferredSampleType tinyint,
	@InferredDate tinyint,
	@InferredCollectors tinyint,
	@GatheringEvent bit,
	@SessionID char(16),
	@Timestamp timestamp
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION
		
		UPDATE 	[Sample]
		SET 	Location_Key = @LocationKey,
			Vague_Date_Start = @VagueDateStart, 
			Vague_Date_End = @VagueDateEnd, 
			Vague_Date_Type = IsNull(@VagueDateType, 'U'),
			Spatial_Ref_Qualifier = @SpatialRefQualifier,
			Spatial_Ref = @SpatialRef,
			Sample_Type_Key = @GatheringMethod,
			Location_Name = @LocationName
		WHERE	Sample_Key = @SampleKey

		IF @@Error <> 0 GOTO RollbackAndExit

		-- Update the Survey Event table so that is points to the
		-- correct Survey record.

		IF @SurveyEventKey = NULL 
			SET @SurveyEventKey = (SELECT	Survey_Event_Key
						FROM	[Sample]
						WHERE	Sample_Key = @SampleKey)
		UPDATE	Survey_Event
		SET	Survey_Key = @SurveyKey
		WHERE	Survey_Event_Key = @SurveyEventKey

		IF @@Error <> 0 GOTO RollbackAndExit

		UPDATE 	Specimen_Field_Data
		SET	Gathering_Event = @GatheringEvent,
			Collection_Unit_Key = @CollectionUnitKey,
			Occurrence_Key = @OccurrenceKey,
			Taxon_Occurrence_Key = @TaxonOccurrenceKey,
			Inferred_Survey = @InferredSurvey,
			Inferred_Location = @InferredLocation,
			Inferred_Spatial_Ref = @InferredSpatialRef,
			Inferred_Sample_Type = @InferredSampleType,
			Inferred_Date = @InferredDate,
			Inferred_Collectors = @InferredCollectors			
		WHERE	Specimen_Field_Data_Key = @Key
		AND	(@Timestamp = Timestamp)
		
		IF @@Error <> 0 GOTO RollbackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_FieldData_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_FieldData_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_FieldData_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_FieldData_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_FieldData_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_FieldData_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_FieldData_Update TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_TaxonList_ImportConceptGroup]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_TaxonList_ImportConceptGroup]
GO

/*===========================================================================*\
  Description:	Import a concept group as a taxon list.

  Parameters:   @job_id					Job identifier
				@taxon_list_key			Taxon list key
				@concept_group_key		Concept group key

  Created:		Dec 2003

  Last revision information:
	$Revision: 3 $
	$Date: 2/02/09 16:47 $
	$Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_TaxonList_ImportConceptGroup]
	@job_id				INT,
	@taxon_list_key		CHAR(16),
	@concept_group_key	CHAR(16),
	@SessionID	CHAR(16)
AS
	SET NOCOUNT ON

	DECLARE		@existing_list_key		CHAR(16)

	SELECT		@existing_list_key						=	Taxon_List_Key
	FROM		Taxon_Dictionary_Concept_Group_Mapping
	WHERE		Concept_Group_Key						=	@concept_group_key

	IF @@ROWCOUNT = 0
	BEGIN
		/* record mapping */
		INSERT		Taxon_Dictionary_Concept_Group_Mapping (
					Taxon_List_Key,
					Concept_Group_Key)
		VALUES		(@taxon_list_key,
					@concept_group_key)

		IF @@ERROR <> 0 RETURN
	END
	ELSE IF @existing_list_key <> @taxon_list_key
	BEGIN
		RAISERROR (
			'Concept group has previously been imported into a different taxon list',
			16,
			1)
		RETURN
	END

	/* Calculate size of job */
	DECLARE		@record_count			INT

	SELECT		@record_count			=	COUNT(*)
	FROM		Concept_Group_Version
	WHERE		Concept_Group_Key		=	@concept_group_key

	SELECT		@record_count							=	@record_count * 3
															+ COUNT(DISTINCT c.Name_Type_Concept_Key)
															+ COUNT(DISTINCT c.Term_Key)
															+ COUNT(DISTINCT c.Term_Version_Key)
															+ COUNT(DISTINCT j.Source_Join_Key)
															+ COUNT(DISTINCT c.Concept_Rank_Key)
															+ COUNT(DISTINCT c.Concept_Key)
															+ COUNT(DISTINCT d.Designation_Type_Concept_Key)
															+ COUNT(DISTINCT d.Concept_Designation_Key)
															+ COUNT(DISTINCT f.Thesaurus_Fact_Key
																	+ vm.Term_Version_Key)
	FROM		Concept									AS	c
	LEFT JOIN	Source_Join								AS	j
	ON			j.Record_Key							=	c.Term_Key
	AND			j.Table_Name							=	'Term'
	LEFT JOIN	Concept_Designation						AS	d
	ON			d.Concept_Key							=	c.Concept_Key
	LEFT JOIN	Thesaurus_Fact							AS	f
	ON			f.Meaning_Key							=	c.Meaning_Key
	LEFT JOIN	Taxon_Dictionary_Term_Version_Mapping	AS	vm
	ON			vm.Term_Version_Key						=	c.Term_Version_Key
	WHERE		c.Concept_Group_Key						=	@concept_group_key
	AND j.Source_Join_Key IS NOT NULL

	EXECUTE		usp_Import_Export_Job_Configure		@job_id,
													@concept_group_key,
													@record_count
	IF @@ERROR <> 0 RETURN
	
	/* import versions */
	EXECUTE		usp_TaxonListVersion_ImportConceptGroup		@job_id
	IF @@ERROR <> 0 RETURN

	/* import name types */
	EXECUTE		usp_TaxonNameType_ImportConceptGroup	@job_id
	IF @@ERROR <> 0 RETURN

	/* import taxa */
	EXECUTE		usp_Taxon_ImportConceptGroup	@job_id
	IF @@ERROR <> 0 RETURN

	/* import taxon versions */
	EXECUTE		usp_TaxonVersion_ImportConceptGroup		@job_id, @SessionID
	IF @@ERROR <> 0 RETURN

	/* import taxon/source relationships */
	EXECUTE		usp_TaxonSources_ImportConceptGroup		@job_id
	IF @@ERROR <> 0 RETURN

	/* import taxon ranks */
	EXECUTE		usp_TaxonRank_ImportConceptGroup	@job_id
	IF @@ERROR <> 0 RETURN

	/* import taxon list items */
	EXECUTE		usp_TaxonListItem_ImportConceptGroup	@job_id
	IF @@ERROR <> 0 RETURN

	/* import hierarchical relationships */
	EXECUTE		usp_TaxonListItem_ImportRelationships	@job_id
	IF @@ERROR <> 0 RETURN

	/* import designation types */
	EXECUTE		usp_TaxonDesignationType_ImportConceptGroup		@job_id
	IF @@ERROR <> 0 RETURN

	/* import designations */
	EXECUTE		usp_TaxonDesignation_ImportConceptGroup		@job_id
	IF @@ERROR <> 0 RETURN

	/* import facts */
	EXECUTE		usp_TaxonFact_ImportConceptGroup	@job_id
	IF @@ERROR <> 0 RETURN 

	EXECUTE		usp_Import_Export_Job_UpdateStatus	@job_id,
													'Updating Taxon Names Index...'
	/* Discard Index_Taxon_Name records for the concept group */
	DELETE ITN
	FROM Index_Taxon_Name ITN
	INNER JOIN Taxon_Dictionary_Concept_Mapping TDM ON TDM.Taxon_List_Item_Key=ITN.Taxon_List_Item_Key
	INNER JOIN Concept C1 ON C1.Concept_Key=TDM.Concept_Key
			AND C1.Concept_Group_Key=@Concept_Group_Key
	
	/* Rebuild Index_Taxon_Name for the concept group */
	INSERT INTO Index_Taxon_Name (Taxon_List_Item_Key, Taxon_List_Version_Key,
	 Actual_Name, Actual_Name_Italic, Common_Name, Common_Name_Italic, 
	  Preferred_Name, Preferred_Name_Italic, Abbreviation, Authority, System_Supplied_Data )
	SELECT TLI.Taxon_List_Item_Key, TLI.Taxon_List_Version_Key, 
	  T.Item_Name, CASE WHEN TR3.List_Font_Italic = 1 AND T.Language = 'La' THEN 1 ELSE 0 END, 
	  T2.Item_Name, CASE WHEN TR3.List_Font_Italic = 1 AND T2.Language = 'La' THEN 1 ELSE 0 END, 
	  T3.Item_Name, CASE WHEN TR3.List_Font_Italic = 1 AND T3.Language = 'La' THEN 1 ELSE 0 END, 
	  T.Abbreviation, T.Authority, 1 
	FROM ((((((((Taxon_List_Item AS TLI 
	INNER JOIN Taxon_Dictionary_Concept_Mapping TDM ON TDM.Taxon_List_Item_Key=TLI.Taxon_List_Item_Key
	INNER JOIN Concept C1 ON C1.Concept_Key=TDM.Concept_Key
			AND C1.Concept_Group_Key=@Concept_Group_Key
	LEFT JOIN Taxon_version AS TV ON TV.Taxon_Version_Key = TLI.Taxon_Version_Key) 
	LEFT JOIN Taxon AS T ON T.Taxon_Key = TV.Taxon_Key) 
	LEFT JOIN Taxon_Common_Name AS TCN ON TCN.Taxon_List_Item_Key = TLI.Taxon_List_Item_Key) 
	LEFT JOIN Taxon_Version AS TV2 ON TV2.Taxon_Version_Key = TCN.Taxon_Version_Key) 
	LEFT JOIN Taxon AS T2 ON T2.Taxon_Key = TV2.Taxon_Key) 
	LEFT JOIN Taxon_List_Item AS TLI3 ON TLI3.Taxon_List_Item_Key = TLI.Preferred_Name) 
	LEFT JOIN Taxon_Rank AS TR3 ON TR3.Taxon_Rank_Key = TLI3.Taxon_Rank_Key) 
	LEFT JOIN Taxon_Version AS TV3 ON TV3.Taxon_Version_Key = TLI3.Taxon_Version_Key) 
	LEFT JOIN Taxon AS T3 ON T3.Taxon_Key = TV3.Taxon_Key 
	WHERE TLI.Taxon_List_Version_To IS NULL

	UPDATE Import_Export_Job
	SET Records_Processed = Records_Processed + @@ROWCOUNT
	WHERE Import_Export_Job_ID = @job_id
	
	EXECUTE		usp_Import_Export_Job_UpdateStatus	@job_id,
													'Exporting Taxon Common Names...'	

	/* Create a local table containing the taxon common name data */
	DECLARE @TaxonCommonName TABLE (
		Taxon_List_Item_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS PRIMARY KEY,
		Taxon_Version_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS
	)

	/* Get the Taxon_List_Item_Keys first, as there may be several Taxon_Version_Keys for some, 
	  and that would break the primary key constraint. */
	INSERT INTO @TaxonCommonName
	SELECT DISTINCT TDM1.Taxon_List_Item_Key, NULL
	FROM 	Taxon_Dictionary_Concept_Mapping TDM1 
	JOIN 	Concept C1 
			ON C1.Concept_Key = TDM1.Concept_Key
			AND C1.Concept_Group_Key = @Concept_Group_Key

	/* Now get a Taxon_Version_Key for each Taxon_List_Item_Key found, it'll use just one, thus 
	  being ok with the primary key constraint.  */
	UPDATE 	TCNTemp
	SET 	Taxon_Version_Key = TLI.Taxon_Version_Key
	FROM 	@TaxonCommonName TCNTemp
	INNER JOIN Taxon_Dictionary_Concept_Mapping TDM1 ON TDM1.Taxon_List_Item_Key = TCNTemp.Taxon_List_Item_Key
	INNER JOIN Concept C1 ON C1.Concept_Key=TDM1.Concept_Key
			AND C1.Concept_Group_Key=@Concept_Group_Key
	LEFT JOIN (
			Concept C2 
			INNER JOIN Term T ON T.Term_Key=C2.Term_Key
			INNER JOIN Language L ON L.Language_Key=T.Language_Key AND L.Priority=1
		) ON C2.Meaning_Key=C1.Meaning_Key
			AND C2.Preferred=1
			AND C2.Name_Type_Concept_Key='SYSTEM000000000L'
	LEFT JOIN Concept C3 ON C3.Meaning_Key=C1.Meaning_Key
		AND C3.List_Preferred=1
		AND C3.Concept_Group_Key=C1.Concept_Group_Key
	INNER JOIN Taxon_Dictionary_Concept_Mapping TDM2 ON TDM2.Concept_Key=ISNULL(C2.Concept_Key, C3.Concept_Key)
	INNER JOIN Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key=TDM2.Taxon_List_Item_Key

	UPDATE Import_Export_Job
	SET Records_Processed = Records_Processed + @@ROWCOUNT
	WHERE Import_Export_Job_ID = @job_id

	/* Update existing taxon common name records that are out of date */
	UPDATE TCN
	SET Taxon_Version_Key=TCNTmp.Taxon_Version_Key
	FROM @TaxonCommonName TCNTmp
	INNER JOIN Taxon_Common_Name TCN ON TCN.Taxon_List_Item_Key=TCNTmp.Taxon_List_Item_Key
	WHERE TCN.Taxon_Version_Key=TCNTmp.Taxon_Version_Key

	/* For new Taxon_Common_Name records, if no common name in the Thesaurus then
	link to itself */
	UPDATE TCNTmp
	SET TCNTmp.Taxon_Version_Key=TLI.Taxon_Version_Key
	FROM @TaxonCommonName TCNTmp
	INNER JOIN Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key=TCNTmp.Taxon_List_Item_Key
	WHERE TCNTmp.Taxon_Version_Key IS NULL
		
	/* Insert any new required taxon common name records */
	INSERT INTO Taxon_Common_Name
	SELECT DISTINCT TCNTmp.Taxon_List_Item_Key, TCNTmp.Taxon_Version_Key
	FROM @TaxonCommonName TCNTmp
	LEFT JOIN Taxon_Common_Name TCN ON TCN.Taxon_List_Item_Key=TCNTmp.Taxon_List_Item_Key
	WHERE TCN.Taxon_List_Item_Key IS NULL

	UPDATE Import_Export_Job
	SET Records_Processed = Records_Processed + @@ROWCOUNT
	WHERE Import_Export_Job_ID = @job_id
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TaxonList_ImportConceptGroup') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_TaxonList_ImportConceptGroup'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_TaxonList_ImportConceptGroup TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_TaxonList_ImportConceptGroup TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_TaxonList_ImportConceptGroup TO [Dev - JNCC SQL]
END
GO

