--The following rebuilds the lineage for four concept groups which are known
--to be faulty in the data provided by LNMNH.
exec usp_ConceptLineage_GenerateForGroup null, 'DSS0043900000005'
exec usp_ConceptLineage_GenerateForGroup null, 'DSS0043900000008'
exec usp_ConceptLineage_GenerateForGroup null, 'DSS004390000000G'
exec usp_ConceptLineage_GenerateForGroup null, 'DSS0045300000009'

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
    $Revision: 2 $
    $Date: 9/06/11 15:44 $
    $Author: Jamesbichard $

\*===========================================================================*/
CREATE FUNCTION dbo.ufn_GetConceptAncestorPath (
	@ItemName varchar(100),
	@Concept_Key char(16)
)
RETURNS varchar(200)
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

		SET @CurrentLineage = @ItemName + ': '

		DECLARE		ancestors	CURSOR LOCAL FAST_FORWARD FOR
		select t.item_name 
		from concept c 
		left join concept_lineage cl on cl.concept_key = c.concept_key
		inner join (
			select cl1.lineage, cl1.lineage_id, c.concept_group_key, c.term_key
			from concept_lineage cl1
			inner join concept c on c.concept_key = cl1.concept_key ) as crelated
			on cl.lineage_id = crelated.lineage_id
				and cl.lineage LIKE crelated.lineage + '\%' 
				and c.concept_group_key = crelated.concept_group_key
		inner join term t on t.term_key = crelated.term_key
		where c.concept_key = @Concept_Key and cl.lineage_id = @lineageId
		order by crelated.lineage

		OPEN		ancestors

		WHILE 1 = 1
		BEGIN
			FETCH		ancestors
			INTO		@current_ancestor

			IF @@FETCH_STATUS <> 0 BREAK

			SET @CurrentLineage = @CurrentLineage + @current_ancestor + ' - '
		END
	
		CLOSE		ancestors
		DEALLOCATE	ancestors

		SELECT @CurrentLineage = SUBSTRING(@CurrentLineage, 0, LEN(@CurrentLineage) - 2)
								 

		SET @HierarchyPath = @HierarchyPath + @CurrentLineage + '**'
	END
	
	CLOSE conceptLineageId
	DEALLOCATE conceptLineageId

	IF LEN(@HierarchyPath) > 0
	BEGIN
		SET @HierarchyPath = SUBSTRING(@HierarchyPath, 0, LEN(@HierarchyPath) - 2)
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

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_DeterminationsRecorder_Select_ForSpecimen]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_DeterminationsRecorder_Select_ForSpecimen]
GO

CREATE PROCEDURE [dbo].[usp_DeterminationsRecorder_Select_ForSpecimen] 
@ParentKey CHAR(16),
@ShowCommonNames BIT

AS

--  DESCRIPTION
--  Returns Determinations (Recorder) for a specified Specimen
--
--  PARAMETERS
--  NAME				DESCRIPTION
--	@ParentKey 			Only the records associated with the parent key are returned
--	@ShowCommonNames	Whether or not Common Names should be shown
--
--
--  AUTHOR:     		Ben Collier, Dorset Software
--  CREATED:    		2003-08-26
--
SET NOCOUNT ON
SELECT 
	TD.Taxon_Determination_Key AS Item_Key, 
	dbo.ufn_GetFormattedTaxonNameByParams(ITN.Actual_Name, ITN.Actual_Name_Italic, ITN.Common_Name, 
			ITN.Common_Name_Italic, ITN.Authority, @ShowCommonNames) AS Item_Name,
	CR.Concept_Rank_Key, Color_R, Color_G, Color_B,	
	dbo.ufn_GetConceptAncestorPath(tl.Item_Name, TCM.Concept_Key) AS Hint
	
FROM 
SPECIMEN_UNIT SU
	INNER JOIN 
		(TAXON_DETERMINATION TD
		INNER JOIN
			INDEX_TAXON_NAME ITN
		ON TD.Taxon_List_Item_Key = ITN.Taxon_List_Item_Key
		LEFT JOIN  
			Taxon_Dictionary_Concept_Mapping  TCM
        ON          TCM.Taxon_List_Item_Key               =   TD.Taxon_List_Item_Key
		INNER JOIN
			TAXON_LIST_Item TLI
		ON TD.Taxon_List_Item_Key = TLI.Taxon_List_Item_Key
		INNER JOIN
			TAXON_LIST_VERSION TLV
		ON TLI.Taxon_List_Version_Key = TLV.Taxon_List_Version_Key
		LEFT JOIN
			TAXON_LIST TL
		ON TLV.Taxon_List_Key = TL.Taxon_List_Key
		LEFT JOIN
			Concept C
		ON TD.Nomenclatural_Status_Concept_Key = C.Concept_Key
		LEFT JOIN
			CONCEPT_RANK CR
		ON C.Concept_Rank_Key = CR.Concept_Rank_Key)
	ON SU.Collection_Unit_Key = TD.Specimen_Collection_Unit_Key AND SU.Collection_Unit_Key = @ParentKey
ORDER BY Item_Name

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_DeterminationsRecorder_Select_ForSpecimen') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_DeterminationsRecorder_Select_ForSpecimen'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_DeterminationsRecorder_Select_ForSpecimen TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_DeterminationsRecorder_Select_ForSpecimen TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_DeterminationsRecorder_Select_ForSpecimen TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_DeterminationsRecorder_Select_ForSpecimen TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_DeterminationsRecorder_Select_ForSpecimen TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_DeterminationsRecorder_Select_ForSpecimen TO [Dev - JNCC SQL]
END

GO


SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Determinations_Select_ForSpecimen]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Determinations_Select_ForSpecimen]
GO

CREATE PROCEDURE [dbo].[usp_Determinations_Select_ForSpecimen] 
@ParentKey CHAR(16)

AS

--  DESCRIPTION
--  Returns Determinations (Non-Recorder) for a specified Specimen
--
--  PARAMETERS
--  NAME			DESCRIPTION
--	@ParentKey 		Only the records associated with the parent key are returned
--
--
--  AUTHOR:     		Ben Collier, Dorset Software
--  CREATED:    		2003-08-26
--
SET NOCOUNT ON
SELECT 
	D.Determination_Key AS Item_Key, 
	CT.Item_Name AS Item_Name, 
	CR.Concept_Rank_Key, 
	Color_R, 
	Color_G, 
	Color_B,
	dbo.ufn_GetConceptAncestorPath(CG.Item_Name, D.Concept_Key) AS Hint
FROM
	DETERMINATION D 
	INNER JOIN VW_ConceptTerm CT ON D.Concept_Key = CT.Concept_Key
	INNER JOIN	Concept_Group CG ON	CT.Concept_Group_Key = CG.Concept_Group_Key
	LEFT JOIN Concept_Rank CR ON CT.Concept_Rank_Key = CR.Concept_Rank_Key
	LEFT JOIN Concept_Lineage CL on D.Concept_Key = CL.Concept_Key
WHERE D.Specimen_Collection_Unit_Key=@ParentKey

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Determinations_Select_ForSpecimen') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Determinations_Select_ForSpecimen'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Determinations_Select_ForSpecimen TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Determinations_Select_ForSpecimen TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Determinations_Select_ForSpecimen TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Determinations_Select_ForSpecimen TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Determinations_Select_ForSpecimen TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Determinations_Select_ForSpecimen TO [Dev - JNCC SQL]
END

GO

SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

/*============================================================================*\
  Drop procedure before re-creating.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_Determinations_Select_ForSpecimenAndDomain') IS NOT NULL
    DROP PROCEDURE dbo.usp_Determinations_Select_ForSpecimenAndDomain
GO

/*============================================================================*\
  Description:  Returns determinations for a particular specimen and domain
                mask.

  Parameters:   @SpecimenCollectionUnitKey  Identifies the specimen
                @DomainMask                 The domain mask

  Created:      November 2009

  Last revision information:
    $Revision: 2 $
    $Date: 9/06/11 15:44 $
    $Author: Jamesbichard $
\*============================================================================*/
CREATE PROCEDURE dbo.usp_Determinations_Select_ForSpecimenAndDomain
    @SpecimenCollectionUnitKey          CHAR(16),
    @DomainMask                         INT,
    @LifeSciences                       BIT,
    @ShowCommonNames                    BIT
AS
    SET NOCOUNT ON

    IF @LifeSciences = 0
    BEGIN
        SELECT      e.Determination_Key                 AS  Item_Key,   
                    t.Item_Name                         AS  Item_Name,   
                    r.Concept_Rank_Key,   
                    r.Color_R,   
                    r.Color_G,   
                    r.Color_B,
					dbo.ufn_GetConceptAncestorPath(g.Item_Name, t.Concept_Key)
														AS	Hint   
        FROM        Determination                       AS  e
        INNER JOIN  VW_ConceptTerm                      AS  t
        ON          t.Concept_Key                       =   e.Concept_Key  
        INNER JOIN  Concept_Group                       AS  g
        ON          g.Concept_Group_Key                 =   t.Concept_Group_Key
        INNER JOIN  Local_Domain                        AS  l
        ON          l.Local_Domain_Key                  =   g.Local_Domain_Key
        INNER JOIN  Domain                              AS  d
        ON          d.Domain_Key                        =   l.Domain_Key
        LEFT JOIN   Concept_Rank                        AS  r
        ON          r.Concept_Rank_Key                  =   t.Concept_Rank_Key
		LEFT JOIN	Concept_Lineage						AS	cl
		ON			cl.Concept_Key						=	t.Concept_Key  
        WHERE       e.Specimen_Collection_Unit_Key      =   @SpecimenCollectionUnitKey
        AND         d.Domain_Mask & @DomainMask         <>  0
        ORDER BY    t.Item_Name
    END
    ELSE
    BEGIN
        SELECT      e.Taxon_Determination_Key           AS  Item_Key,   
                    dbo.ufn_GetFormattedTaxonNameByParams(
                            n.Actual_Name,
                            n.Actual_Name_Italic,
                            n.Common_Name,
                            n.Common_Name_Italic,
                            n.Authority,
                            @ShowCommonNames)           AS  Item_Name,  
                    r.Concept_Rank_Key,
                    r.Color_R,
                    r.Color_G,
                    r.Color_B,
					dbo.ufn_GetConceptAncestorPath(g.Item_Name,	m.Concept_Key)
														AS Hint
        FROM        Specimen_Unit                       AS  u  
        INNER JOIN  TAXON_DETERMINATION                 AS  e  
        ON          e.Specimen_Collection_Unit_Key      =   u.Collection_Unit_Key
        INNER JOIN  INDEX_TAXON_NAME                    AS  n  
        ON          n.Taxon_List_Item_Key               =   e.Taxon_List_Item_Key  
        INNER JOIN  Taxon_Dictionary_Concept_Mapping    AS  m
        ON          m.Taxon_List_Item_Key               =   e.Taxon_List_Item_Key  
        INNER JOIN  Concept                             AS  c
        ON          c.Concept_Key                       =   m.Concept_Key  
        INNER JOIN  Concept_Group                       AS  g
        ON          g.Concept_Group_Key                 =   c.Concept_Group_Key  
        INNER JOIN  Local_Domain                        AS  l
        ON          l.Local_Domain_Key                  =   g.Local_Domain_Key
        INNER JOIN  Domain                              AS  d
        ON          d.Domain_Key                        =   l.Domain_Key
        LEFT JOIN   Concept                             AS  sc  
        ON          sc.Concept_Key                      =   e.Nomenclatural_Status_Concept_Key
        LEFT JOIN   Concept_Rank                        AS  r  
        ON          r.Concept_Rank_Key                  =   sc.Concept_Rank_Key
		LEFT JOIN	Concept_Lineage						AS	cl
		ON			cl.Concept_Key						=	c.Concept_Key
        WHERE       u.Collection_Unit_Key               =   @SpecimenCollectionUnitKey
        AND         d.Domain_Mask & @DomainMask         <>  0
        ORDER BY    Item_Name
    END
GO

/*============================================================================*\
  Grant permissions.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_Determinations_Select_ForSpecimenAndDomain') IS NOT NULL
BEGIN
    PRINT 'Setting up security on procedure usp_Determinations_Select_ForSpecimenAndDomain'
    IF EXISTS (SELECT * FROM sysusers WHERE NAME = 'R2k_AddOnly')
        GRANT EXECUTE ON dbo.usp_Determinations_Select_ForSpecimenAndDomain TO R2k_AddOnly
    IF EXISTS (SELECT * FROM sysusers WHERE NAME = 'R2k_Administrator')
        GRANT EXECUTE ON dbo.usp_Determinations_Select_ForSpecimenAndDomain TO R2k_Administrator
    IF EXISTS (SELECT * FROM sysusers WHERE NAME = 'R2k_FullEdit')
        GRANT EXECUTE ON dbo.usp_Determinations_Select_ForSpecimenAndDomain TO R2k_FullEdit
    IF EXISTS (SELECT * FROM sysusers WHERE NAME = 'R2k_ReadOnly')
        GRANT EXECUTE ON dbo.usp_Determinations_Select_ForSpecimenAndDomain TO R2k_ReadOnly
    IF EXISTS (SELECT * FROM sysusers WHERE NAME = 'R2k_RecordCardsOnly')
        GRANT EXECUTE ON dbo.usp_Determinations_Select_ForSpecimenAndDomain TO R2k_RecordCardsOnly
END
GO

IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_PotentialSynonyms_Select_ForMerge]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
	DROP PROCEDURE [dbo].[usp_PotentialSynonyms_Select_ForMerge]
GO

/*===========================================================================*\
  Description:	
	Returns concepts that are potential synonyms.

  Parameters:	
  @ConceptGroupKey 
	When specified, only the Concepts in the group are scanned. Otherwise,
	the whole thesaurus is scanned.

  Created:	December 2006

  Last revision information:
    $Revision: 2 $
    $Date: 9/06/11 15:44 $
    $Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_PotentialSynonyms_Select_ForMerge] 
	@ConceptGroupKey		CHAR(16),
	@ConceptGroupSearchKey	CHAR(16),
	@PreferredSynonymGroup	CHAR(16),
	@MaxRowCount			INT,
	@Timestamp				TIMESTAMP,
	@SessionId				CHAR(16)
AS
	SET NOCOUNT ON
	
	SET ROWCOUNT @MaxRowCount

	SELECT			DISTINCT
					CSource.Concept_Key			AS	SourceConceptKey,
					CSource.Meaning_Key			AS	SourceMeaningKey,
					CG.Item_Name				AS	SourceGroup,
					ISNULL
					(
						TVSource.Author_And_Date,
						''
					)							AS	SourceAuthority,
					TSource.Item_Name			AS	SourceConcept,
					TSource.PlainText,			-- There for the ORDER BY
					CPotSyn.Concept_Key			AS	SynonymConceptKey,
					CPotSyn.Meaning_Key			AS	SynonymMeaningKey,
					CGPotSyn.Item_Name			AS	SynonymGroup,
					ISNULL
					(
						TVPotSyn.Author_And_Date,
						''
					)							AS	SynonymAuthority,
					TPotSyn.Item_Name			AS	SynonymConcept,
					TPotentials.Item_Name		AS	SharedTerm,
					CSource.Custodian,
					CSource.Timestamp,
					CASE 
						WHEN CPotSyn.Concept_Group_Key = @PreferredSynonymGroup
							THEN 1
						ELSE 0
					END AS InPreferredGroup
	FROM			Concept						AS	CSource
	--Consider all synonyms of the source concept
	INNER JOIN		Concept						AS	CSynonyms 		
	ON				CSynonyms.Meaning_Key		=	CSource.Meaning_Key
	INNER JOIN		Term						AS	TSource
	ON				TSource.Term_Key			=	CSource.Term_Key
	INNER JOIN		Term						AS	TSynonyms
	ON				TSynonyms.Term_Key			=	CSynonyms.Term_Key
	INNER JOIN		Term						AS	TPotentials
	ON				TPotentials.Plaintext		=	TSynonyms.Plaintext
	AND				TPotentials.Language_Key	=	TSynonyms.Language_Key
	--Join on all concepts whose term matches the term of a synonym of
	--the original concepts
	INNER JOIN		Concept						AS	CPotentials
	ON				CPotentials.Term_Key		=	TPotentials.Term_Key
	--Get all synonyms of the concepts which match the term of one of the
	--synonyms of the original concept. These will be the potential synonyms
	INNER JOIN		Concept						AS	CPotSyn
	ON				CPotSyn.Meaning_Key			=	CPotentials.Meaning_Key
	INNER JOIN		Term						AS	TPotSyn
	ON				TPotSyn.Term_Key			=	CPotSyn.Term_Key
	--Exclude any concepts that match by term but which are already synonyms
	LEFT JOIN		Concept						AS	CExclude
	ON				CExclude.Concept_Key		=	CPotentials.Concept_Key
	AND				CExclude.Meaning_Key		=	CSource.Meaning_Key
	--Exclude any concepts that match by term but whose meaning is already
	--marked as an homonym of the source concept meaning
	LEFT JOIN		Homonym_Pair				AS	H
	ON				(H.Meaning_Key_1			=	CPotentials.Meaning_Key
		AND			H.Meaning_Key_2				=	CSource.Meaning_Key)
	OR				(H.Meaning_Key_1			=	CSource.Meaning_Key
		AND			H.Meaning_Key_2				=	CPotentials.Meaning_Key)
	INNER JOIN		Concept_Group				AS	CG
	ON				CG.Concept_Group_Key		=	CSource.Concept_Group_Key
	INNER JOIN		Concept_Group				AS	CGPotSyn
	ON				CGPotSyn.Concept_Group_Key	=	CPotSyn.Concept_Group_Key
	LEFT JOIN		dbo.Term_Version			AS	TVSource
	ON				TVSource.Term_Version_Key	=	CSource.Term_Version_Key
	LEFT JOIN		dbo.Term_Version			AS	TVPotSyn
	ON				TVPotSyn.Term_Version_Key	=	CPotSyn.Term_Version_Key
	WHERE			(@ConceptGroupKey IS NULL
	OR				CSource.Concept_Group_Key	=	@ConceptGroupKey)
	AND				(@ConceptGroupSearchKey IS NULL
	OR				CPotSyn.Concept_Group_Key	=	@ConceptGroupSearchKey)
	AND				CExclude.Concept_Key IS NULL
	AND				H.Meaning_Key_1 IS NULL
	AND				CSource.List_Preferred		=	1
	AND				CPotSyn.List_Preferred		=	1
	AND				CSource.[Timestamp]			>	ISNULL(@Timestamp, 0)
	AND				(@SessionId IS NULL
	OR				CSource.Entered_Session_Id	=	@SessionId)
	--following condition ensures that if two potential synonyms are in the same
	--list, the pair is only returned once
	AND				(((@ConceptGroupKey IS NOT NULL OR @ConceptGroupSearchKey IS NOT NULL)
					AND (CSource.Concept_Group_Key <> CPotSyn.Concept_Group_Key
						OR	CSource.Concept_Key > CPotSyn.Concept_Key))
		OR			(@ConceptGroupKey IS NULL AND @ConceptGroupSearchKey IS NULL
					AND CSource.Concept_Key > CPotSyn.Concept_Key))
	ORDER BY		TSource.PlainText

	
	SET ROWCOUNT 0
	SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_PotentialSynonyms_Select_ForMerge') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_PotentialSynonyms_Select_ForMerge'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_PotentialSynonyms_Select_ForMerge TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_PotentialSynonyms_Select_ForMerge TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_PotentialSynonyms_Select_ForMerge TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_PotentialSynonyms_Select_ForMerge TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_PotentialSynonyms_Select_ForMerge TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_PotentialSynonyms_Select_ForMerge TO [Dev - JNCC SQL]
END
