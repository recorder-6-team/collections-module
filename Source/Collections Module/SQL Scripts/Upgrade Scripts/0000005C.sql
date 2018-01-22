/*===========================================================================*\
  Drop function before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'dbo.ufn_GetConceptAncestorPath')
	   AND    Type IN ('FN', 'IF', 'TF'))
	DROP FUNCTION dbo.ufn_GetConceptAncestorPath
GO

/*===========================================================================*\
  Description:	Returns the hierarchy of a specified concept

  Parameters:	@Concept_Key		The key of the concept for which the ancestor
									hierarchy is return

  Created:	May 2011

  Last revision information:
    $Revision: 1 $
    $Date: 31/08/11 11:15 $
    $Author: Jamesbichard $

\*===========================================================================*/
CREATE FUNCTION dbo.ufn_GetConceptAncestorPath (
	@ItemName varchar(100),
	@Concept_Key char(16)
)
RETURNS varchar(5000)
AS
BEGIN
	DECLARE @lineageId INT,
			@HierarchyPath varchar(5000),
			@CurrentLineage varchar(1000),
			@current_ancestor varchar(200)

	SET @HierarchyPath = ''
	
	DECLARE conceptLineageId CURSOR LOCAL FAST_FORWARD FOR
	SELECT lineage_id 
	FROM concept_lineage 
	WHERE concept_key = @Concept_Key

	OPEN conceptLineageId

	WHILE 1 = 1
	BEGIN

		FETCH conceptLineageId
		INTO @lineageId

		IF @@FETCH_STATUS <> 0 BREAK

		SET @CurrentLineage = ''

		DECLARE		ancestors	CURSOR LOCAL FAST_FORWARD FOR
		select crelated.published_term 
		from concept c 
		left join concept_lineage cl on cl.concept_key = c.concept_key
		inner join (
			select cl1.lineage, cl1.lineage_id, c.concept_group_key, c.published_term
			from concept_lineage cl1
			inner join concept c on c.concept_key = cl1.concept_key ) as crelated
			on cl.lineage LIKE crelated.lineage + '\%' 
				and c.concept_group_key = crelated.concept_group_key
		where c.concept_key = @Concept_Key and cl.lineage_id = @lineageId
		order by crelated.lineage

		OPEN		ancestors

		WHILE 1 = 1
		BEGIN
			FETCH		ancestors
			INTO		@current_ancestor

			IF @@FETCH_STATUS <> 0 BREAK
	
			IF LEN(@CurrentLineage) = 0
			BEGIN
				SET @CurrentLineage = @ItemName + ': '
			END

			SET @CurrentLineage = @CurrentLineage + @current_ancestor + ' - '
		END
	
		CLOSE		ancestors
		DEALLOCATE	ancestors

		IF LEN(@CurrentLineage) > 0 
		BEGIN
			SELECT @CurrentLineage = SUBSTRING(@CurrentLineage, 0, LEN(@CurrentLineage) - 1)								 
		END ELSE
		BEGIN
			SELECT @CurrentLineage = @ItemName	
		END

		SET @HierarchyPath = @HierarchyPath + @CurrentLineage + '**'
	END
	
	CLOSE conceptLineageId
	DEALLOCATE conceptLineageId

	IF LEN(@HierarchyPath) > 0
	BEGIN
		SET @HierarchyPath = SUBSTRING(@HierarchyPath, 0, LEN(@HierarchyPath) - 1)
	END
	ELSE
	BEGIN
		SET @HierarchyPath = @ItemName
	END

	RETURN @HierarchyPath
END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'dbo.ufn_GetConceptAncestorPath')
	   AND    Type IN ('FN', 'IF', 'TF'))
	BEGIN
    	PRINT 'Setting up security on function dbo.ufn_GetConceptAncestorPath'
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
	        	GRANT EXECUTE ON dbo.ufn_GetConceptAncestorPath TO [R2k_AddOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
			GRANT EXECUTE ON dbo.ufn_GetConceptAncestorPath TO [R2k_Administrator]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
			GRANT EXECUTE ON dbo.ufn_GetConceptAncestorPath TO [R2k_FullEdit]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
			GRANT EXECUTE ON dbo.ufn_GetConceptAncestorPath TO [R2k_ReadOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
			GRANT EXECUTE ON dbo.ufn_GetConceptAncestorPath TO [R2k_RecordCardsOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
	        	GRANT EXECUTE ON dbo.ufn_GetConceptAncestorPath TO [Dev - JNCC SQL]
	END
GO
SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

/*============================================================================*\
	Drop function before re-creating.
\*============================================================================*/
IF OBJECT_ID(N'dbo.ufn_GetTermGenerator') IS NOT NULL
	DROP FUNCTION dbo.ufn_GetTermGenerator
GO

/*============================================================================*\
	Description:
		Loops through concept hierarchy, concept group, local domain and
		domain to get the term generator key for a concept.

	Created: July 2011

	Last revision information:
		$Revision: 1 $
		$Date: 31/08/11 11:15 $
		$Author: Jamesbichard $
\*============================================================================*/

CREATE FUNCTION dbo.ufn_GetTermGenerator(
	@Key CHAR(16),
	@IsConceptGroupKey BIT
)
RETURNS CHAR(16)
AS
BEGIN
	DECLARE @TermGeneratorKey CHAR(16)
	DECLARE @ConceptGroupKey CHAR(16)
	DECLARE @LocalDomainKey CHAR(16)
	DECLARE @DomainKey CHAR(16)

	IF @IsConceptGroupKey = 0
	BEGIN

		--Check if concept has a defined published term rule
		SELECT 
			@TermGeneratorKey = Term_Generator_Key, 
			@ConceptGroupKey = Concept_Group_Key
		FROM Concept
		WHERE Concept_Key = @Key

		--If rule is undefined, look recursively for a parent rule
		IF @TermGeneratorKey IS NULL
		BEGIN
			DECLARE	@RelationKey CHAR(16)
			DECLARE	@ParentKey CHAR(16)

			-- Get the hierarchy type from the concept group itself.
			SELECT	@RelationKey 	= 	Hierarchy_Relation_Type_Key
			FROM	Concept_Group	CG
			JOIN	Concept			C	ON	C.Concept_Group_Key = CG.Concept_Group_Key
			WHERE	Concept_Key		= 	@Key

			SELECT @ParentKey = (
				SELECT TOP(1) C.Concept_Key
				FROM 		Concept_Relation	AS	CR1
				INNER JOIN 	Concept				AS	C
				ON			C.Concept_Key		=	CR1.From_Concept_Key
				INNER JOIN 	Term				AS	T
				ON			T.Term_Key			=	C.Term_Key
				LEFT JOIN 	(Concept_Relation CR2 
							INNER JOIN Concept	AS C2 
							ON	C2.Concept_Key			= CR2.From_Concept_Key
								AND C2.List_Preferred	= 1
								AND C2.Is_Current		= 1)
				ON			CR2.To_Concept_Key				= C.Concept_Key
					AND		CR2.Thesaurus_Relation_Type_Key = @RelationKey
				WHERE 		CR1.To_Concept_Key				= @Key
					AND 	CR1.Thesaurus_Relation_Type_Key = @RelationKey
					AND 	C.List_Preferred	= 1
					AND 	C.Is_Current		= 1
			)
			
			IF @ParentKey IS NOT NULL
			BEGIN
				RETURN dbo.ufn_GetTermGenerator(@ParentKey, 0)	
			END
			ELSE
			BEGIN
				RETURN dbo.ufn_GetTermGenerator(@ConceptGroupKey, 1)
			END		
		END

	END
	ELSE
	BEGIN
		--If no more parents, look for concept group rule
		SELECT @TermGeneratorKey = Term_Generator_Key,
				@LocalDomainKey = Local_Domain_Key
		FROM Concept_Group	
		WHERE Concept_Group_Key = @Key	
		
		--If rule is still undefined, look for local domain rule
		IF @TermGeneratorKey IS NULL
		BEGIN
			SELECT @TermGeneratorKey = Term_Generator_Key,
					@DomainKey = Domain_Key
			FROM Local_Domain
			WHERE Local_Domain_Key = @LocalDomainKey

			--If rule is still undefined, look for domain rule
			IF @TermGeneratorKey IS NULL
			BEGIN
				SELECT @TermGeneratorKey = Term_Generator_Key
				FROM Domain
				WHERE Domain_Key = @DomainKey					
			END
		END
	END

	--If term generator is still undefined, use the default
	IF @TermGeneratorKey IS NULL
	BEGIN
		SELECT @TermGeneratorKey = Term_Generator_Key
		FROM Term_Generator
		WHERE Item_Name = 'System default rule'
	END

	RETURN @TermGeneratorKey

END
GO

/*============================================================================*\
	Grant permissions.
\*============================================================================*/
PRINT 'Setting up security on function ufn_GetTermGenerator'

IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
    GRANT EXECUTE ON dbo.ufn_GetTermGenerator TO R2k_AddOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
	GRANT EXECUTE ON dbo.ufn_GetTermGenerator TO R2k_Administrator
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
	GRANT EXECUTE ON dbo.ufn_GetTermGenerator TO R2k_FullEdit
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
	GRANT EXECUTE ON dbo.ufn_GetTermGenerator TO R2k_ReadOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
	GRANT EXECUTE ON dbo.ufn_GetTermGenerator TO R2k_RecordCardsOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
    GRANT EXECUTE ON dbo.ufn_GetTermGenerator TO "Dev - JNCC SQL"
GO
/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptGroup_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptGroup_Update]
GO

/*===========================================================================*\
  Description:	Updates a record in the Concept_Group table.

  Parameters:	@Key 
		@URL
		@SessionID
		@ConceptGroupName 
		@Authority 
		@HierarchyRelationTypeKey 
		@LocalDomainKey
		@RecordsAffected
		@Timestamp 

  Created:	November 2003

  Last revision information:
	$Revision: 1 $
	$Date: 31/08/11 11:15 $
	$Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptGroup_Update]
	@Key char(16),
	@LocalDomainKey char(16),
	@ConceptGroupName varchar(100),
	@Authority varchar(100) = NULL,
	@URL varchar(255) = NULL,
	@HierarchyRelationTypeKey char(16) = NULL,
	@TermGeneratorKey char(16),
	@SessionID char(16),
	@UpdateDescendents bit,
	@Timestamp timestamp,
	@RecordsAffected int output
AS
	SET NOCOUNT OFF

	DECLARE	@hierarchy_changed BIT,
		@error INT	

	IF @UpdateDescendents IS NULL SET @UpdateDescendents = 1

	IF @UpdateDescendents = 0
	BEGIN
		DISABLE TRIGGER tr_ConceptGroup_PublishedTermFields ON Concept_Group
	END

	BEGIN TRANSACTION

		/* has hierarchical relation changed? */
		SELECT	@hierarchy_changed = 	CASE WHEN @HierarchyRelationTypeKey = Hierarchy_Relation_Type_Key
							THEN 0
							ELSE 1
						END
		FROM	Concept_Group
		WHERE	Concept_Group_Key = @Key

		UPDATE 	Concept_Group
		SET	Local_Domain_Key = @LocalDomainKey,
			Item_Name = @ConceptGroupName,
			Authority = @Authority,
			URL = @URL,
			Hierarchy_Relation_Type_Key = @HierarchyRelationTypeKey,
			Term_Generator_Key = @TermGeneratorKey,
			Changed_Session_ID = @SessionID
		WHERE	Concept_Group_Key = @Key
		AND		[Timestamp] = @Timestamp

		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Concept_Group WHERE Concept_Group_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

		/* regenerate concept lineage if hierarchy changed */
		IF @hierarchy_changed = 1
		BEGIN
			EXECUTE	usp_ConceptLineage_GenerateForGroup @concept_group_key = @Key
			IF @@ERROR <> 0 GOTO RollbackAndExit
		END;
	
		ENABLE TRIGGER tr_ConceptGroup_PublishedTermFields ON Concept_Group
	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit:
	ENABLE TRIGGER tr_ConceptGroup_PublishedTermFields ON Concept_Group
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_ConceptGroup_Update failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptGroup_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptGroup_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptGroup_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptGroup_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptGroup_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptGroup_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptGroup_Update TO [Dev - JNCC SQL]
END

GO	
/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].usp_Concept_Select_ForCaptionUpdate')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].usp_Concept_Select_ForCaptionUpdate
GO

/*===========================================================================*\
  Description:	For a given concept, returns a list of all concepts whose
				captions may need updating. Includes all child concepts, as
				well as any concepts which share the given concept's term
				or term version.


  Created:	August 2011

  Last revision information:
    $Revision: 1 $
    $Date: 31/08/11 11:15 $
    $Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].usp_Concept_Select_ForCaptionUpdate
	@ConceptKey char(16)
AS
	SELECT relc.Concept_Key
	FROM Concept c
	INNER JOIN Concept relc on relc.Term_Key = c.Term_Key 
		OR relc.Term_Version_Key = c.Term_Version_Key
	WHERE c.Concept_Key = @ConceptKey
	
	UNION 

	-- Get child concepts
	SELECT crelated.concept_key 
	FROM concept c 
	LEFT JOIN concept_lineage cl on cl.concept_key = c.concept_key
	INNER JOIN (
		SELECT 
			cl1.lineage, 
			cl1.lineage_id, 
			c1.concept_group_key, 
			c1.concept_key
		FROM concept_lineage cl1
		INNER JOIN concept c1 on c1.concept_key = cl1.concept_key ) as crelated
		ON crelated.lineage LIKE cl.lineage + '\%' 
			AND c.concept_group_key = crelated.concept_group_key
	WHERE c.concept_key = @ConceptKey
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_Select_ForCaptionUpdate') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Concept_Select_ForCaptionUpdate'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Concept_Select_ForCaptionUpdate TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForCaptionUpdate TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForCaptionUpdate TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForCaptionUpdate TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForCaptionUpdate TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Concept_Select_ForCaptionUpdate TO [Dev - JNCC SQL]
END

GO
/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Domain_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Domain_Update]
GO

/*===========================================================================*\
  Description:	Updates a record in the Domain table

  Parameters:	@Key 
		@ItemName
		@SubjectAreaKey
		@HasOccurrences 
		@DefaultHierarchyRelationTypeKey 
		@DomainMask
		@SessionID
		@Timestamp

  Created:	November 2003

  Last revision information:
    $Revision: 1 $
    $Date: 31/08/11 11:15 $
    $Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Domain_Update]
	@Key char(16),
	@ItemName varchar(100),
	@SubjectAreaKey char(16),
	@HasOccurrences bit,
	@DefaultHierarchyRelationTypeKey char(16),
	@DomainMask int,
	@TermGeneratorKey char(16),
	@UpdateDescendents bit,
	@SessionID char(16),
	@Timestamp timestamp
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	IF @UpdateDescendents IS NULL SET @UpdateDescendents = 1

	IF @UpdateDescendents = 0
	BEGIN
		DISABLE TRIGGER tr_Domain_PublishedTermFields ON Domain
	END

	BEGIN TRANSACTION
		
		
		UPDATE 	Domain
		SET 	Item_Name = @ItemName, 
			Subject_Area_Key = @SubjectAreaKey,
			Has_Occurrences = @HasOccurrences,
			Default_Hierarchy_Relation_Type_Key = @DefaultHierarchyRelationTypeKey,
			Domain_Mask = @DomainMask,
			Changed_Session_ID = @SessionID,
			Term_Generator_Key = @TermGeneratorKey
		WHERE	Domain_Key = @Key
		AND		[Timestamp] = @Timestamp

		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Domain WHERE Domain_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END;

		ENABLE TRIGGER tr_Domain_PublishedTermFields ON Domain		
	COMMIT TRANSACTION
	
	RETURN 0

RollBackAndExit:
		ENABLE TRIGGER tr_Domain_PublishedTermFields ON Domain 
	ROLLBACK TRANSACTION	
GO


/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Domain_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Domain_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Domain_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Domain_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Domain_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Domain_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Domain_Update TO [Dev - JNCC SQL]
END
GO
/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_LocalDomain_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_LocalDomain_Update]
GO

/*===========================================================================*\
  Description:	Updates a record in the Local Domain table

  Parameters:	@Key 
		@ItemName 
		@LanguageKey 
		@ConceptGroupLabel
		@SessionID 
		@Timestamp 

  Created:	November 2003

  Last revision information:
    $Revision: 1 $
    $Date: 31/08/11 11:15 $
    $Author: Jamesbichard $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_LocalDomain_Update]
	@Key char(16),
	@ItemName varchar(100),
	@LanguageKey varchar(4),
	@ConceptGroupLabel varchar(50),
	@TermGeneratorKey char(16),
	@UpdateDescendents bit,
	@SessionID char(16),
	@Timestamp timestamp
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	IF @UpdateDescendents IS NULL SET @UpdateDescendents = 1

	IF @UpdateDescendents = 0
	BEGIN
		DISABLE TRIGGER tr_LocalDomain_PublishedTermFields ON Local_Domain
	END

	BEGIN TRANSACTION
		
		UPDATE 	Local_Domain
		SET 	Item_Name = @ItemName, 
			Language_Key = @LanguageKey,
			Concept_Group_Label = @ConceptGroupLabel,
			Changed_Session_ID = @SessionID,
			Term_Generator_Key = @TermGeneratorKey
		WHERE	Local_Domain_Key = @Key
		AND		[Timestamp] = @Timestamp

		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Local_Domain WHERE Local_Domain_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END;

		ENABLE TRIGGER tr_LocalDomain_PublishedTermFields ON Local_Domain
	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ENABLE TRIGGER tr_LocalDomain_PublishedTermFields ON Local_Domain
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_LocalDomain_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_LocalDomain_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_LocalDomain_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_LocalDomain_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_LocalDomain_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_LocalDomain_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_LocalDomain_Update TO [Dev - JNCC SQL]
END
GO
/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_SpecimensCollected_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_SpecimensCollected_Select]
GO

/*===========================================================================*\
  Description:	Returns the list of all preferred determinations and the
		preferred number for the specimens linked to the currently
		selected collection. Returns the list of all determinations for 
		specimen top level node.

  Parameters:	@Key			Key of the event recorder person
		@CollectionUnitKey	Specimen or Collection Unit key
		@KeyIsSpecimen		Bit saying whether the key is a specimen

  Created:	October 2003

  Last revision information:
    $Revision: 1 $
    $Date: 31/08/11 11:15 $
    $Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SpecimensCollected_Select]
	@Key char(16),
	@CollectionUnitKey char(16),
	@UserDomainMask int,
	@SessionID char(16),
	@ShowCommonNames bit,
	@KeyIsSpecimen bit
AS

SET NOCOUNT ON

IF @KeyIsSpecimen = 1
	SELECT DISTINCT
			SU.Collection_Unit_Key AS Item_Key,
			CASE WHEN SU.Life_Sciences = 0
			THEN CTP.Item_Name COLLATE SQL_Latin1_General_CP1_CI_AS
			ELSE dbo.ufn_GetFormattedTaxonNameByParams(
								Preferred_Name,
								0,
								Common_Name,
								0,
								NULL,
								@ShowCommonNames)
			END + CASE WHEN CUN.Number IS NULL THEN '' ELSE ' - ' + CUN.Number END AS Item_Name
	FROM		Individual AS I
	INNER JOIN	Survey_Event_Recorder AS SER ON SER.Name_Key = I.Name_Key
	INNER JOIN	Sample_Recorder AS SR ON SR.SE_Recorder_Key = SER.SE_Recorder_Key
	INNER JOIN	[Sample] AS S ON S.Sample_Key = SR.Sample_Key
	LEFT JOIN	Occurrence AS O ON O.Sample_Key = S.Sample_Key
	LEFT JOIN	Taxon_Occurrence AS XO ON XO.Sample_Key = S.Sample_Key
	INNER JOIN	Specimen_Field_Data AS SFD ON (SFD.Occurrence_Key = O.Occurrence_Key
							OR SFD.Taxon_Occurrence_Key = XO.Taxon_Occurrence_Key)
						AND SFD.Gathering_Event = 1
	INNER JOIN	Specimen_Unit AS SU ON SU.Collection_Unit_Key = SFD.Collection_Unit_Key
					AND SU.Collection_Unit_Key = @CollectionUnitKey
	INNER JOIN	Collection_Unit AS CU ON CU.Collection_Unit_Key = SU.Collection_Unit_Key 
					AND ((CU.Domain_Mask & @UserDomainMask > 0) 
						OR (CU.Entered_Session_ID = @SessionID)
						OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
					
	LEFT JOIN	(Determination AS D
	INNER JOIN	VW_ConceptTermPreferred AS CTP ON CTP.Concept_Key = D.Concept_Key)
			ON D.Determination_Key = SU.Preferred_Determination_Key
	LEFT JOIN 	(Taxon_Determination AS TD 
				INNER JOIN 	Index_Taxon_Name ITN ON ITN.Taxon_List_Item_Key = TD.Taxon_List_Item_Key)
			ON TD.Taxon_Determination_Key = SU.Preferred_Taxon_Determination_Key  
	LEFT JOIN 	Collection_Unit_Number CUN ON SU.Collection_Unit_key = CUN.Collection_Unit_Key 
					 AND CUN.Preferred = 1
	
	WHERE		I.Name_Key = @Key
	ORDER BY	Item_Name
ELSE
	SELECT DISTINCT
			SU.Collection_Unit_Key AS Item_Key,
			CASE WHEN SU.Life_Sciences = 0
			THEN CTP.PlainText COLLATE SQL_Latin1_General_CP1_CI_AS
			ELSE dbo.ufn_GetFormattedTaxonNameByParams(
								Preferred_Name,
								0,
								Common_Name,
								0,
								NULL,
								@ShowCommonNames)
			END + CASE WHEN CUN.Number IS NULL THEN '' ELSE ' - ' + CUN.Number END AS Item_Name
	FROM		Individual AS I
	INNER JOIN	Survey_Event_Recorder AS SER ON SER.Name_Key = I.Name_Key
	INNER JOIN	Sample_Recorder AS SR ON SR.SE_Recorder_Key = SER.SE_Recorder_Key
	INNER JOIN	[Sample] AS S ON S.Sample_Key = SR.Sample_Key
	LEFT JOIN	Occurrence AS O ON O.Sample_Key = S.Sample_Key
	LEFT JOIN	Taxon_Occurrence AS XO ON XO.Sample_Key = S.Sample_Key
	INNER JOIN	Specimen_Field_Data AS SFD ON (SFD.Occurrence_Key = O.Occurrence_Key
							OR SFD.Taxon_Occurrence_Key = XO.Taxon_Occurrence_Key)
						AND SFD.Gathering_Event = 1
	INNER JOIN	Specimen_Unit AS SU ON SU.Collection_Unit_Key = SFD.Collection_Unit_Key
					AND SU.Parent_Collection_Collection_Unit_Key = @CollectionUnitKey
	INNER JOIN	Collection_Unit AS CU ON CU.Collection_Unit_Key = SU.Collection_Unit_Key 
					AND ((CU.Domain_Mask & @UserDomainMask > 0) 
						OR (CU.Entered_Session_ID = @SessionID)
						OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
	LEFT JOIN	(Determination AS D
				INNER JOIN	VW_ConceptTermPreferred AS CTP ON CTP.Concept_Key = D.Concept_Key)
			ON D.Determination_Key = SU.Preferred_Determination_Key
	LEFT JOIN 	(Taxon_Determination AS TD 
				INNER JOIN 	Index_Taxon_Name ITN ON ITN.Taxon_List_Item_Key = TD.Taxon_List_Item_Key)
			ON TD.Taxon_Determination_Key = SU.Preferred_Taxon_Determination_Key  
	LEFT JOIN 	Collection_Unit_Number CUN ON SU.Collection_Unit_key = CUN.Collection_Unit_Key 
					 AND CUN.Preferred = 1
	
	WHERE		I.Name_Key = @Key
	ORDER BY	Item_Name
SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SpecimensCollected_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SpecimensCollected_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SpecimensCollected_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SpecimensCollected_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SpecimensCollected_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_SpecimensCollected_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SpecimensCollected_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SpecimensCollected_Select TO [Dev - JNCC SQL]
END

GO
/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_SpecimensDetermined_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_SpecimensDetermined_Select]
GO

/*===========================================================================*\
  Description:	Returns the list of all preferred determinations and the
		preferred number for the specimens linked to the currently
		selected collection. Returns the list of all determinations for
		specimen top level node.

  Parameters:	@Key			Key of the determiner.
		@CollectionUnitKey	Key of the collection unit
		@KeyIsSpecimen		Bit saying whether the key is a specimen

  Created:	October 2003

  Last revision information:
    $Revision: 1 $
    $Date: 31/08/11 11:15 $
    $Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SpecimensDetermined_Select]
	@Key char(16),
	@CollectionUnitKey char(16),
	@UserDomainMask int,
	@SessionID char(16),
	@ShowCommonNames bit,
	@KeyIsSpecimen bit
AS

SET NOCOUNT ON

IF @KeyIsSpecimen = 1
BEGIN
	SELECT DISTINCT
			SU.Collection_Unit_Key AS Item_Key, 
			CASE SU.Life_Sciences 
				WHEN 0 THEN 
					CTPref.Item_Name COLLATE SQL_Latin1_General_CP1_CI_AS
				ELSE dbo.ufn_GetFormattedTaxonNameByParams(
								ITN.Preferred_Name,
								0,
								ITN.Common_Name,
								0,
								NULL,
								@ShowCommonNames)
			END + 
			CASE 
				WHEN CUN.Number IS NULL THEN '' 
				ELSE ' - ' + CUN.Number 
			END AS Item_Name
	FROM 		Specimen_Unit AS SU
	INNER JOIN	Collection_Unit AS CU ON CU.Collection_Unit_Key = SU.Collection_Unit_Key	
					AND ((CU.Domain_Mask & @UserDomainMask > 0) 
						OR (CU.Entered_Session_ID = @SessionID) 
						OR (CU.Changed_Session_ID = @SessionID) 
						OR (CU.Domain_Mask = 0))
	LEFT JOIN	Taxon_Determination AS TD ON TD.Taxon_Determination_Key = SU.Preferred_Taxon_Determination_Key
	LEFT JOIN	Determination AS D ON D.Determination_Key = SU.Preferred_Determination_Key
	INNER JOIN	Individual AS I ON (I.Name_Key = TD.Determiner
						OR I.Name_Key = D.Determiner_Name_Key)
					AND I.Name_Key = @Key
	LEFT JOIN 	VW_ConceptTermPreferred CTPref ON CTPref.Concept_Key=D.Concept_Key
	LEFT JOIN 	Index_Taxon_Name ITN ON ITN.Taxon_List_Item_Key=TD.Taxon_List_Item_Key
	LEFT JOIN 	Collection_Unit_Number CUN ON SU.Collection_Unit_key = CUN.Collection_Unit_Key 
			 			AND CUN.Preferred = 1
	WHERE 		SU.Collection_unit_key = @CollectionUnitKey
	ORDER BY 	Item_Name
END
ELSE
BEGIN
	SELECT DISTINCT
			SU.Collection_Unit_Key AS Item_Key, 
			CASE SU.Life_Sciences 
				WHEN 0 THEN 
					CTPref.PlainText COLLATE SQL_Latin1_General_CP1_CI_AS
				ELSE dbo.ufn_GetFormattedTaxonNameByParams(
								ITN.Preferred_Name,
								0,
								ITN.Common_Name,
								0,
								NULL,
								@ShowCommonNames)
			END + 
			CASE 
				WHEN CUN.Number IS NULL THEN '' 
				ELSE ' - ' + CUN.Number 
			END AS Item_Name
	FROM 		Specimen_Unit AS SU
	INNER JOIN	Collection_Unit AS CU ON CU.Collection_Unit_Key = SU.Collection_Unit_Key	
					AND ((CU.Domain_Mask & @UserDomainMask > 0) 
						OR (CU.Entered_Session_ID = @SessionID) 
						OR (CU.Changed_Session_ID = @SessionID) 
						OR (CU.Domain_Mask = 0))
	LEFT JOIN	Taxon_Determination AS TD ON TD.Taxon_Determination_Key = SU.Preferred_Taxon_Determination_Key
	LEFT JOIN	Determination AS D ON D.Determination_Key = SU.Preferred_Determination_Key
	INNER JOIN	Individual AS I ON (I.Name_Key = TD.Determiner
						OR I.Name_Key = D.Determiner_Name_Key)
					AND I.Name_Key = @Key
	LEFT JOIN 	VW_ConceptTermPreferred CTPref ON CTPref.Concept_Key=D.Concept_Key
	LEFT JOIN 	Index_Taxon_Name ITN ON ITN.Taxon_List_Item_Key=TD.Taxon_List_Item_Key
	LEFT JOIN 	Collection_Unit_Number CUN ON SU.Collection_Unit_key = CUN.Collection_Unit_Key 
			 			AND CUN.Preferred = 1
	WHERE 		SU.Parent_Collection_Collection_unit_key = @CollectionUnitKey
	ORDER BY 	Item_Name
END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SpecimensDetermined_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SpecimensDetermined_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SpecimensDetermined_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SpecimensDetermined_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SpecimensDetermined_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_SpecimensDetermined_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SpecimensDetermined_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SpecimensDetermined_Select TO [Dev - JNCC SQL]
END

GO
