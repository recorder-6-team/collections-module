SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

/*============================================================================*\
	Drop function before re-creating.
\*============================================================================*/
IF OBJECT_ID(N'dbo.ufn_GeneratePublishedTermUsingDefaultRule') IS NOT NULL
	DROP FUNCTION dbo.ufn_GeneratePublishedTermUsingDefaultRule
GO

/*============================================================================*\
	Description:
		The default function used to calculate a full published term based on
		the supplied concept information. Simply returns the given plaintext.

	Created: July 2011

	Last revision information:
		$Revision: 5 $
		$Date: 17/08/11 15:40 $
		$Author: Jamesbichard $
\*============================================================================*/

CREATE FUNCTION dbo.ufn_GeneratePublishedTermUsingDefaultRule
(
	@Plaintext NVARCHAR(150),
	@AuthorAndDate VARCHAR(100),
	@Attributes VARCHAR(100),
	@RankKey CHAR(16),
	@ParentConceptKey CHAR(16)
)
RETURNS NVARCHAR(256)
AS
BEGIN
	RETURN @Plaintext
END
GO

/*============================================================================*\
	Grant permissions.
\*============================================================================*/
PRINT 'Setting up security on function ufn_GeneratePublishedTermUsingDefaultRule'

IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
    GRANT EXECUTE ON dbo.ufn_GeneratePublishedTermUsingDefaultRule TO R2k_AddOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
	GRANT EXECUTE ON dbo.ufn_GeneratePublishedTermUsingDefaultRule TO R2k_Administrator
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
	GRANT EXECUTE ON dbo.ufn_GeneratePublishedTermUsingDefaultRule TO R2k_FullEdit
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
	GRANT EXECUTE ON dbo.ufn_GeneratePublishedTermUsingDefaultRule TO R2k_ReadOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
	GRANT EXECUTE ON dbo.ufn_GeneratePublishedTermUsingDefaultRule TO R2k_RecordCardsOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
    GRANT EXECUTE ON dbo.ufn_GeneratePublishedTermUsingDefaultRule TO "Dev - JNCC SQL"
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
		$Revision: 5 $
		$Date: 17/08/11 15:40 $
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
		WHERE Concept_Group_Key = @ConceptGroupKey	
		
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
	   WHERE  Id = Object_Id(N'[dbo].[usp_AllSynonyms_Select_ForConcept]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_AllSynonyms_Select_ForConcept]
GO

/*===========================================================================*\
  Description:	Returns List Synonyms

  Parameters:	@Key	Concept_Key

  Created:	December 2003

  Last revision information:
    $Revision: 5 $
    $Date: 17/08/11 15:40 $
    $Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_AllSynonyms_Select_ForConcept]
	@Key char(16)
AS

SET NOCOUNT ON

	/*=============================*\
	  Get all known synonyms.
	\*=============================*/
	SELECT 		CAllKnownSynonyms.Concept_Key AS Item_Key,
				CAllKnownSynonyms.Published_Term + ' (' + CG.Item_Name + ')'	AS	Item_Name,
				CAllKnownSynonyms.Concept_Group_Key 
	FROM 		Concept AS CSource
	INNER JOIN	Concept AS CAllKnownSynonyms 	ON CAllKnownSynonyms.Meaning_Key = CSource.Meaning_Key 
							AND CAllKnownSynonyms.Concept_Key <> @Key
	INNER JOIN	Concept_Group AS CG 		ON CG.Concept_Group_Key = CAllKnownSynonyms.Concept_Group_Key
	WHERE 		CSource.Concept_key = @Key

	ORDER BY 	Item_Name

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_AllSynonyms_Select_ForConcept') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_AllSynonyms_Select_ForConcept'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_AllSynonyms_Select_ForConcept TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_AllSynonyms_Select_ForConcept TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_AllSynonyms_Select_ForConcept TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_AllSynonyms_Select_ForConcept TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_AllSynonyms_Select_ForConcept TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_AllSynonyms_Select_ForConcept TO [Dev - JNCC SQL]
END
GO
/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Collection_Select') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_Collection_Select]
GO

/*===========================================================================*\
  Description:	Returns a collection record.

  Parameters:	@Key	Collection key

  Created:	August 2003

  Last revision information:
    $Revision: 5 $
    $Date: 17/08/11 15:40 $
    $Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Collection_Select]
	@Key char(16)
AS

SET NOCOUNT ON

	SELECT	CU.Collection_Unit_Key, 
		C.Item_Name,
		C.Parent_Collection_Collection_Unit_Key,
		C.Assembler_Name_Key, 
		dbo.ufn_GetFormattedName(C.Assembler_Name_Key) AS Assembler_Name,
		C.Topic, 
		CUN.Name_Key AS Owner_Name_Key,
		CU.Current_Container_Collection_Unit_Key,
		SC.Item_Name + ISNULL(' - ' + CSC.Current_Location_Code, ISNULL(' - ' + CSC.Usual_Location_Code, '')) AS Current_Location_Name,
		CU.Current_Location_Code,
		CU.Usual_Container_Collection_Unit_Key, 
		SU.Item_Name + ISNULL(' - ' + CSU.Current_Location_Code, ISNULL(' - ' + CSU.Usual_Location_Code, '')) AS Usual_Location_Name,
		CU.Usual_Location_Code,
		CU.Domain_Mask,
		C.Risk_Concept_Key,
		CP.Published_Term AS Risk_Name,
		C.Timestamp AS Collection_Timestamp,
		CU.Timestamp AS Collection_Unit_Timestamp

	FROM		Collection C
	INNER JOIN 	Collection_Unit CU ON CU.Collection_Unit_Key = C.Collection_Unit_Key
	LEFT JOIN 	Store SC ON SC.Collection_Unit_Key = CU.Current_Container_Collection_Unit_Key
	LEFT JOIN	Collection_Unit CSC ON CSC.Collection_Unit_Key = SC.Collection_Unit_Key 

	LEFT JOIN 	Store SU ON SU.Collection_Unit_Key = CU.Usual_Container_Collection_Unit_Key
	LEFT JOIN	Collection_Unit CSU ON CSU.Collection_Unit_Key = SU.Collection_Unit_Key

	LEFT JOIN 	Collection_Unit_Name CUN ON CUN.Collection_Unit_Key = C.Collection_Unit_Key
						AND CUN.Relation_Type_Concept_Key = 'SYSTEM00000000I7' --Owner concept key
	LEFT JOIN 	Concept CP ON CP.Concept_Key = C.Risk_Concept_Key
	WHERE	C.Collection_Unit_Key = @Key

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Collection_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Collection_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Collection_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Collection_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Collection_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Collection_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Collection_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Collection_Select TO [Dev - JNCC SQL]
END
GO
/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_CommonName_Get]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_CommonName_Get]
GO

/*===========================================================================*\
  Description: Returns the common name for a concept

  Parameters:	@ParentConceptKey
							@HierarchyRelationTypeKey - relationship type used to populate
							hierarchy.

  Created:	August 2003

  Last revision information:
    $Revision: 5 $
    $Date: 17/08/11 15:40 $
    $Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_CommonName_Get]
	@ConceptKey char(16),
	@CommonName varchar(150) output
AS

SELECT Top 1 	@CommonName = C2.Published_Term
FROM 		Concept C1
INNER JOIN 	Concept C2 	on C2.Meaning_Key=C1.Meaning_Key
    				AND C2.Name_Type_Concept_Key='SYSTEM000000000L'
				AND C2.Preferred = 1
INNER JOIN 	Term T 		on T.Term_Key=C2.Term_Key
INNER JOIN 	Language L 	on L.Language_Key=T.Language_Key
    				AND L.Priority=1
WHERE 		C1.Concept_Key = @ConceptKey

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_CommonName_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_CommonName_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_CommonName_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_CommonName_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_CommonName_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_CommonName_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_CommonName_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_CommonName_Get TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptAncestors_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptAncestors_Select]
GO

/*===========================================================================*\
  Description: Returns the ancestor hierarchy for a concept

  Parameters:	@ConceptKey - conecpt to find ancestors for

  Created:	August 2003

  Last revision information:
    $Revision: 5 $
    $Date: 17/08/11 15:40 $
    $Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptAncestors_Select]
	@ConceptKey varchar(100),
  @LineageID integer,
  @HierarchyRelationTypeKey char(16)
AS

DECLARE @Lineage varchar(2000)
DECLARE @CharPos integer
DECLARE @ConceptGroupKey char(16)


SET NOCOUNT ON

--select the lineage and concept group
SELECT @Lineage = CL.Lineage, @ConceptGroupKey = C.Concept_Group_Key
FROM Concept_Lineage CL
INNER JOIN Concept C on C.Concept_Key=CL.Concept_Key
WHERE CL.Concept_Key=@ConceptKey
AND CL.Lineage_ID=@LineageID

--Create an output table
CREATE TABLE #Output (
  Concept_Key char(16),
  Item_Name varchar(100),
  HasChildren bit,
  Concept_Rank_Key char(16)
)

SET @CharPos=1

--Find each ancestor, start at top of tree and work down
WHILE @CharPos<LEN(@Lineage)
BEGIN
  IF SUBSTRING(@Lineage, @CharPos, 1)='\'
	  INSERT INTO #Output
	    SELECT DISTINCT C.Concept_Key, C.Published_Term	AS	Item_Name,
        CASE WHEN CR.Concept_Relation_Key IS NULL THEN 0 ELSE 1 END,
				C.Concept_Rank_Key
			FROM Concept C
	        INNER JOIN Concept_Lineage CL ON CL.Concept_Key=C.Concept_Key
  			LEFT JOIN Concept_Relation CR ON CR.From_Concept_Key=C.Concept_Key
		  WHERE C.Concept_Group_Key=@ConceptGroupKey
	    AND CL.Lineage=Left(@Lineage, @CharPos-1)
 	    AND CR.Thesaurus_Relation_Type_Key=@HierarchyRelationTypeKey
  SET @CharPos=@CharPos+1
END

SELECT * FROM #Output

DROP TABLE #Output

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptAncestors_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptAncestors_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptAncestors_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptAncestors_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptAncestors_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ConceptAncestors_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptAncestors_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptAncestors_Select TO [Dev - JNCC SQL]
END

GO

SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/

IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptCount_CopyCheck') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_ConceptCount_CopyCheck]
GO

/*===========================================================================*\
  Description:	Given a concept, this returns the number of children of the 
				concept, including the concept itself, which are already in the
				concept's concept group. 

  Parameters:	@Count - The number of the concept's children already in the
				concept's group
				@ParentKey - The concept key of the concept in question

  Created:	October 2010

\*===========================================================================*/

CREATE PROCEDURE dbo.usp_ConceptCount_CopyCheck (
	@Count INT = 0 OUTPUT,
	@ParentKey CHAR(16),
	@ConceptGroupKey CHAR(16)
)
AS

SET NOCOUNT ON

CREATE TABLE #TargetConcepts (
	Concept_Key CHAR(16),
	Item_Name NVARCHAR(150)
)

CREATE TABLE #CopiedConcepts (
	ConceptKey CHAR(16),
	ItemName NVARCHAR(300),
	SortCode INT,
	HasChildren BIT,
	RANK CHAR(16)
)

DECLARE	@RelationKey CHAR(16)

-- Get the hierarchy type from the concept group itself.
SELECT	@RelationKey 	 = 	Hierarchy_Relation_Type_Key
FROM	Concept_Group	CG
JOIN	Concept			C	ON	C.Concept_Group_Key = CG.Concept_Group_Key
WHERE	Concept_Key		= 	@ParentKey

INSERT INTO #TargetConcepts
EXEC usp_Concept_Select_ForConceptGroup @ConceptGroupKey

INSERT INTO #CopiedConcepts
SELECT 
	C.Concept_Key as ConceptKey,
	C.Published_Term as ItemName,
	C.Sort_Code as SortCode,
	1 as HasChildren,
	C.Concept_Rank_Key
FROM Concept C
WHERE Concept_Key = @ParentKey

-- Repeat until no more concepts with child concepts.
WHILE EXISTS(SELECT * FROM #CopiedConcepts WHERE HasChildren = 1)
BEGIN
	-- Get first concept with child concepts.
	SELECT 	@ParentKey 	= ConceptKey
	FROM	#CopiedConcepts
	WHERE	HasChildren = 1

	-- Get the child concepts in.
	INSERT INTO #CopiedConcepts
	EXECUTE usp_Concept_Select_ForParent @ParentKey, @RelationKey
	
	-- Update flag to indicate child concepts are done for this one.
	UPDATE 	#CopiedConcepts
	SET		HasChildren = 0
	WHERE	ConceptKey 	= @ParentKey
END

SELECT @Count = COUNT(TC.Item_Name)
FROM #CopiedConcepts CC
INNER JOIN	#TargetConcepts TC
	ON		CC.ItemName = TC.Item_Name

DROP TABLE #TargetConcepts
DROP TABLE #CopiedConcepts
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptCount_CopyCheck') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptCount_CopyCheck'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptCount_CopyCheck TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptCount_CopyCheck TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptCount_CopyCheck TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ConceptCount_CopyCheck TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptCount_CopyCheck TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON usp_ConceptCount_CopyCheck TO [Dev - JNCC SQL]
END
GO
/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_ConceptFullySpecifiedWithCG_Get]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_ConceptFullySpecifiedWithCG_Get]
GO


/*===========================================================================*\
  Description:	Returns a fully specified concept for a concept_key.  
		E.g. Name + Author (list preferred name + author).

  Parameters:	@Key

  Created:	2003-08-24

  Last revision information:
    $Revision: 5 $
    $Date: 17/08/11 15:40 $
    $Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptFullySpecifiedWithCG_Get] 
@ItemKey char(16),
@Output varchar(500) output

AS

SET NOCOUNT ON

-- Get selected term
DECLARE @ListPreferred bit

SELECT @Output = C.Published_Term,
  @ListPreferred = C.List_Preferred
FROM Concept C
WHERE C.Concept_Key=@ItemKey


-- If this is not the list preferred, then find that and append it to the output
IF @ListPreferred=0 BEGIN
	SELECT @Output = @Output + C2.Published_Term
	FROM Concept C1
  INNER JOIN Concept C2 on C2.Meaning_Key=C1.Meaning_Key
	WHERE C1.Concept_Key=@ItemKey
    AND C2.Concept_Group_Key=C1.Concept_Group_Key
    AND C2.List_Preferred=1  
END

-- Add the concept group name
SELECT @Output = @Output + ' - ' + CG.Item_Name
FROM Concept C 
INNER JOIN Concept_Group CG
ON CG.Concept_Group_Key=C.Concept_Group_Key
WHERE C.Concept_Key=@ItemKey

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptFullySpecifiedWithCG_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptFullySpecifiedWithCG_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptFullySpecifiedWithCG_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptFullySpecifiedWithCG_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptFullySpecifiedWithCG_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ConceptFullySpecifiedWithCG_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptFullySpecifiedWithCG_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptFullySpecifiedWithCG_Get TO [Dev - JNCC SQL]
END

GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_ConceptFullySpecified_Select]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_ConceptFullySpecified_Select]
GO

CREATE PROCEDURE [dbo].[usp_ConceptFullySpecified_Select] 
@ItemKey char(16),
@Output varchar(500) output

AS

--  DESCRIPTION
--  Returns a fully specified concept for a concept_key.  E.g. Name + Author (list preferred name + author).
--
--  PARAMETERS
--  NAME					DESCRIPTION
--	@ItemKey 			Key of the concept
--
--
--  CREATED:    		2003-08-24
--
SET NOCOUNT ON

-- Get selected term
DECLARE @ListPreferred bit

SELECT @Output = C.Published_Term,
  @ListPreferred = C.List_Preferred
FROM Concept C
WHERE C.Concept_Key=@ItemKey


-- If this is not the list preferred, then find that and append it to the output
IF @ListPreferred=0 BEGIN
	SELECT @Output = @Output + C2.Published_Term
	FROM Concept C1
  INNER JOIN Concept C2 on C2.Meaning_Key=C1.Meaning_Key
	WHERE C1.Concept_Key=@ItemKey
    AND C2.Concept_Group_Key=C1.Concept_Group_Key
    AND C2.List_Preferred=1  
END


GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptFullySpecified_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptFullySpecified_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptFullySpecified_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptFullySpecified_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptFullySpecified_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ConceptFullySpecified_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptFullySpecified_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptFullySpecified_Select TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptGroup_Insert]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptGroup_Insert]
GO

/*===========================================================================*\
  Description:	Adds a Concept Group by adding records to the Concept Group
		and Concept Group Version tables.

  Parameters:	@Key 
		@URL
		@SessionID
		@ConceptGroupName 
		@Authority 
		@HierarchyRelationTypeKey 
		@LocalDomainKey 
		@FromVagueDateStart 
		@FromVagueDateEnd
		@FromVagueDateType 
		@ToVagueDateStart 
		@ToVagueDateEnd 
		@ToVagueDateType
		@AcqVagueDateStart 
		@AcqVagueDateEnd 
		@AcqVagueDateType
		@Version

  Created:	November 2003

  Last revision information:
    $Revision: 5 $
    $Date: 17/08/11 15:40 $
    $Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptGroup_Insert]
	@Key char(16) OUTPUT,	-- New Concept_Group_Key
	-- For both tables
	@URL varchar(255) = NULL,
	@SessionID char(16),
	
	-- For Concept_Group table
	@ConceptGroupName varchar(100),
	@Authority varchar(100) = NULL,
	@HierarchyRelationTypeKey char(16) = NULL,
	@LocalDomainKey char(16),
	@TermGeneratorKey char(16),
	
	-- For Concept_Group_Version table
	@FromVagueDateStart int = NULL,
	@FromVagueDateEnd int = NULL,
	@FromVagueDateType varchar(2) = NULL,
	@ToVagueDateStart int = NULL,
	@ToVagueDateEnd int = NULL,
	@ToVagueDateType varchar(2) = NULL,	
	@AcqVagueDateStart int = NULL,
	@AcqVagueDateEnd int = NULL,
	@AcqVagueDateType varchar(2) = NULL,
	@Version varchar(100) = NULL
	
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION

		/*-------------------------------------------------------------*\
		  Insert in Concept_Group.
		\*-------------------------------------------------------------*/
		EXECUTE spNextKey 'Concept_Group', @Key OUTPUT		

		INSERT INTO Concept_Group (
			Concept_Group_Key, 
			Local_Domain_Key, 
			Item_Name, 
			Authority,
			URL,
			Hierarchy_Relation_Type_Key, 
			Term_Generator_Key,
			Entered_Session_ID, 
			System_Supplied_Data
		) VALUES (
			@Key, 
			@LocalDomainKey, 
			@ConceptGroupName, 
			@Authority, 
			@URL,
			@HierarchyRelationTypeKey, 
			@TermGeneratorKey,
			@SessionID, 
			0
		)
		IF @@Error <> 0 GOTO RollbackAndExit

		-- Only if @Version contains a string do we want to add a Concept_Group_Version record here.
		-- A Concept_Group record needs to have at least on Concept_Group_Version record. However,
		-- if no value for @Version is supplied, it is likely that the Concept_Group_Version record
		-- is being added elsewhere because the key of the new record is required.
		IF @Version IS NOT NULL
		BEGIN
			/*-------------------------------------------------------------*\
			  Insert in Concept_Group_Version.
			\*-------------------------------------------------------------*/
			DECLARE @ConceptGroupVersionKey char(16) 
			EXECUTE spNextKey 'Concept_Group_Version', @ConceptGroupVersionKey OUTPUT		
			
			INSERT INTO Concept_Group_Version (
				Concept_Group_Version_Key, 
				Concept_Group_Key, 
				Version,
				[Sequence], 
				From_Vague_Date_Start,
				From_Vague_Date_End,
				From_Vague_Date_Type,
				To_Vague_Date_Start,
				To_Vague_Date_End,
				To_Vague_Date_Type,	
				Acq_Vague_Date_Start,
				Acq_Vague_Date_End,
				Acq_Vague_Date_Type,	
				URL,
				Entered_Session_ID,
				System_Supplied_Data
			) VALUES (
				@ConceptGroupVersionKey,
				@Key,
				@Version,
				1,
				@FromVagueDateStart,
				@FromVagueDateEnd,
				IsNull(@FromVagueDateType, 'U'),
				@ToVagueDateStart,
				@ToVagueDateEnd,
				IsNull(@ToVagueDateType, 'U'),	
				@AcqVagueDateStart,
				@AcqVagueDateEnd,
				IsNull(@AcqVagueDateType, 'U'),
				@URL,
				@SessionID,
				0
			)
			IF @@Error <> 0 GOTO RollbackAndExit
		END

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptGroup_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptGroup_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptGroup_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptGroup_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptGroup_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptGroup_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptGroup_Insert TO [Dev - JNCC SQL]
END
GO	
/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptGroup_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptGroup_Select]
GO

/*===========================================================================*\
  Description:	Returns data from the Concept Group table.

  Parameters:	@Key	

  Created:	November 2003

  Last revision information:
    $Revision: 5 $
    $Date: 17/08/11 15:40 $
    $Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptGroup_Select]
	@Key char(16)
AS

SET NOCOUNT ON

	SELECT
			CG.Concept_Group_Key, 		
			CG.Local_Domain_Key,
			CG.Item_Name,
			CG.Authority,
			CG.Url,
			CG.Hierarchy_Relation_Type_Key,
			TRT.Item_Name AS Hierarchy_Relation_Type_Name,
			CG.Term_Generator_Key,
			TG.Item_Name as Term_Generator_Name,
			CG.Entered_Session_ID,
			CG.Changed_Session_ID,
			CG.System_Supplied_Data,
			CG.Custodian,
			CG.[Timestamp],
			CASE WHEN COUNT(CGQC.Checked_Date_Time) > 0 THEN 1
			ELSE 0 END AS HasHistory
	FROM		Concept_Group AS CG
	LEFT JOIN	Thesaurus_Relation_Type AS TRT ON TRT.Thesaurus_Relation_Type_Key = CG.Hierarchy_Relation_Type_Key
	LEFT JOIN	Concept_Group_Quality_Check AS CGQC ON CGQC.Concept_Group_Key = CG.Concept_Group_Key
	LEFT JOIN	Term_Generator TG ON TG.Term_Generator_Key = CG.Term_Generator_Key
	WHERE		CG.Concept_Group_Key = @Key 
	GROUP BY
			CG.Concept_Group_Key, 		
			CG.Local_Domain_Key,
			CG.Item_Name,
			CG.Authority,
			CG.Url,
			CG.Hierarchy_Relation_Type_Key,
			CG.Term_Generator_Key,
			TG.Item_Name,
			TRT.Item_Name,
			CG.Entered_Session_ID,
			CG.Changed_Session_ID,
			CG.System_Supplied_Data,
			CG.Custodian,
			CG.[Timestamp]

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptGroup_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptGroup_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptGroup_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptGroup_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptGroup_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ConceptGroup_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptGroup_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptGroup_Select TO [Dev - JNCC SQL]
END

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
	$Revision: 5 $
	$Date: 17/08/11 15:40 $
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
	@Timestamp timestamp,
	@RecordsAffected int output
AS
	SET NOCOUNT OFF

	DECLARE	@hierarchy_changed BIT,
		@error INT	

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
		END

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit:
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
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptKey_Get_ForConceptGroupAndItemName]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptKey_Get_ForConceptGroupAndItemName]
GO

/*===========================================================================*\
  Description:	Retrieves the concept key when given the concept title and a
		concept group key.

  Parameters:	@ConceptGroupKey
		@PlainText - The concept title.
		@ConceptKey - The concept key is outputted.

  Created:	October 2003

  Last revision information:
    $Revision: 5 $
    $Date: 17/08/11 15:40 $
    $Author: Jamesbichard $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_ConceptKey_Get_ForConceptGroupAndItemName]
	@ConceptGroupKey char(16),
	@PlainText varchar(100),
	@ConceptKey char(16) OUTPUT
AS

SET NOCOUNT ON

	SELECT 		@ConceptKey = C.Concept_Key 
	FROM 		Concept C 
	INNER JOIN 	Term T ON T.Term_Key = C.Term_Key 
	WHERE 		(C.Concept_Group_Key = @ConceptGroupKey)
	AND 		(C.List_Preferred = 1)
	AND 		(C.Is_Current = 1)
	AND 		PlainText = @PlainText
	ORDER BY 	C.Sort_Code, T.Plaintext
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptKey_Get_ForConceptGroupAndItemName') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptKey_Get_ForConceptGroupAndItemName'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptKey_Get_ForConceptGroupAndItemName TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptKey_Get_ForConceptGroupAndItemName TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptKey_Get_ForConceptGroupAndItemName TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ConceptKey_Get_ForConceptGroupAndItemName TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptKey_Get_ForConceptGroupAndItemName TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptKey_Get_ForConceptGroupAndItemName TO [Dev - JNCC SQL]
END

GO
/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptListPreferred_Select_ForConcept]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptListPreferred_Select_ForConcept]
GO

/*===========================================================================*\
  Description:	Retrieves details of the list preferred concept using any concept
								key as input.  Input could be a synonym, but proper name will be 
								returned.

  Parameters:	@ConceptGroupKey - key of the concept group
							@ConceptKey - concept key

  Created:	August 2003

  Last revision information:
    $Revision: 5 $
    $Date: 17/08/11 15:40 $
    $Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptListPreferred_Select_ForConcept]
	@ConceptGroupKey char(16),
  @ConceptKey varchar(16)
AS


SELECT CPref.Concept_Key, CPref.Published_Term	AS	Item_Name, CPref.Concept_Rank_Key, 
  CASE WHEN CR.Concept_Relation_Key IS NULL THEN 0 ELSE 1 END AS HasChildren
FROM Concept C
  INNER JOIN Concept CPref on CPref.Meaning_Key=C.Meaning_Key
  LEFT JOIN Concept_Relation CR on CR.From_Concept_Key=CPref.Concept_Key
WHERE C.Concept_Key=@ConceptKey
  AND CPref.List_Preferred=1
  AND CPref.Concept_Group_Key=@ConceptGroupKey
	AND CPref.Is_Current=1


GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptListPreferred_Select_ForConcept') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptListPreferred_Select_ForConcept'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptListPreferred_Select_ForConcept TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptListPreferred_Select_ForConcept TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptListPreferred_Select_ForConcept TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ConceptListPreferred_Select_ForConcept TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptListPreferred_Select_ForConcept TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptListPreferred_Select_ForConcept TO [Dev - JNCC SQL]
END

GO


/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptRelations_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptRelations_Select]
GO

/*===========================================================================*\
  Description:	Retrieves a list of all the related concepts for a concept.
	              Includes concept and meaning relations, synonyms and term versions.

  Parameters:	@ConceptKey	
 	            @IncludeSynonyms 

  Created:	September 2003

  Last revision information:
    $Revision: 5 $
    $Date: 17/08/11 15:40 $
    $Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptRelations_Select]
	@ConceptKey char(16),
  @IncludeSynonyms bit
AS

SET NOCOUNT ON

--Create a temp table to store the output
DECLARE @Output TABLE (
  Relationship_Name varchar(100),
  Concept_Key char(16),
  Plaintext nvarchar(150),
  Item_Name nvarchar(150),
  Author_And_Date varchar(100),
  Concept_Group_Name varchar(100),
  Details text
)   

--Create a temp table to store the concept keys we are scanning
DECLARE @ConceptKeys TABLE (
  Concept_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS PRIMARY KEY,
	Ancestor BIT   -- is this a concept in the lineage above the selected concept?
)

--Find the current concept group.  The output only lists the concept group if different to the current item
DECLARE @ConceptGroupKey char(16)
SELECT @ConceptGroupKey=Concept_Group_Key FROM Concept WHERE Concept_Key=@ConceptKey

--Store the appropriate concept key
INSERT INTO @ConceptKeys (Concept_Key, Ancestor) 
VALUES (@ConceptKey, 0)

/*===========================================================================*\	
	Retrieve the list of lineage concept keys that we need to look at for 
	inherited relationships
\*===========================================================================*/
	DECLARE @LineageID integer
	DECLARE @Lineage varchar(2000)
	DECLARE @CharPos integer

	--First find all the parent lineages
  DECLARE Lineages_Cursor CURSOR LOCAL FORWARD_ONLY FOR
    SELECT Lineage_ID, Lineage FROM Concept_Lineage WHERE Concept_Key=@ConceptKey

	OPEN Lineages_Cursor

  FETCH NEXT FROM Lineages_Cursor INTO @LineageID, @Lineage
  
  WHILE @@FETCH_STATUS=0
	BEGIN
		
		SET @CharPos=1
		
		--Find each ancestor, start at top of tree and work down
		WHILE @CharPos<LEN(@Lineage)
		BEGIN
		  IF SUBSTRING(@Lineage, @CharPos, 1)='\'
			  INSERT INTO @ConceptKeys (Concept_Key, Ancestor)
			    SELECT DISTINCT C.Concept_Key, 1
					FROM Concept C
		      INNER JOIN Concept_Lineage CL ON CL.Concept_Key=C.Concept_Key
					LEFT JOIN @ConceptKeys CK ON CK.Concept_Key=C.Concept_Key
				  WHERE C.Concept_Group_Key=@ConceptGroupKey
			    AND CL.Lineage=Left(@Lineage, @CharPos-1)
					AND CK.Concept_Key IS NULL
		  SET @CharPos=@CharPos+1
		END

    FETCH NEXT FROM Lineages_Cursor INTO @LineageID, @Lineage

	END

	CLOSE Lineages_Cursor
	DEALLOCATE Lineages_Cursor

--Now get the synonyms we need to scan for
IF @IncludeSynonyms=1
  INSERT INTO @ConceptKeys (Concept_Key, Ancestor)
      SELECT C2.Concept_Key, CK1.Ancestor
      FROM @ConceptKeys CK1
			INNER JOIN Concept C1 ON C1.Concept_Key=CK1.Concept_Key
      INNER JOIN Concept C2 ON C2.Meaning_Key=C1.Meaning_Key
			-- Join to ensure no duplicates
			LEFT JOIN @ConceptKeys CK2 ON CK2.Concept_Key=C2.Concept_Key
			WHERE CK2.Concept_Key IS NULL

--Insert forward terms for concept relations.  The odd nesting of queries
--allows distinct to be used before the text field (comment) is obtained)
--Note that concept relations cannot be inherited.
INSERT INTO @Output (Relationship_Name, Concept_Key, Plaintext, 
      Item_Name, Author_And_Date, Concept_Group_Name, Details)
	SELECT 
		Data.Forward_Term, 
		Data.Concept_Key, 
		Data.Plaintext, 
		Data.Item_Name, 
		Data.Author_Copy, 
		Data.Concept_Group_Name,
		CR.Comment
	FROM (
		SELECT DISTINCT CR.Concept_Relation_Key, TRT.Forward_Term, C.Concept_Key, T.Plaintext, 
	      C.Published_Term AS	Item_Name, C.Author_Copy, 
	      CASE WHEN CG.Concept_Group_Key=@ConceptGroupKey THEN NULL ELSE CG.Item_Name END AS Concept_Group_Name
		FROM Concept_Relation CR
	    	INNER JOIN Concept C ON C.Concept_Key=CR.To_Concept_Key
	    	INNER JOIN Term T ON T.Term_Key=C.Term_Key
				INNER JOIN Thesaurus_Relation_Type TRT ON TRT.Thesaurus_Relation_Type_Key=CR.Thesaurus_Relation_Type_Key
			  INNER JOIN Concept_Group CG ON CG.Concept_Group_Key=C.Concept_Group_Key
				INNER JOIN @ConceptKeys CK ON CK.Concept_Key=CR.From_Concept_Key
						AND (CK.Ancestor=0 OR CR.Inherited=1)
	) Data INNER JOIN Concept_Relation CR ON CR.Concept_Relation_Key=Data.Concept_Relation_Key
  ORDER BY PlainText, Author_Copy


--Insert reverse terms for concept relations
INSERT INTO @Output (Relationship_Name, Concept_Key, Plaintext, 
      Item_Name, Author_And_Date, Concept_Group_Name, Details)
	SELECT 
		Data.Reverse_Term, 
		Data.Concept_Key, 
		Data.Plaintext, 
		Data.Item_Name, 
		Data.Author_Copy, 
		Data.Concept_Group_Name,
		CR.Comment
	FROM (
		SELECT DISTINCT CR.Concept_Relation_Key, TRT.Reverse_Term, C.Concept_Key, T.Plaintext, 
	      C.Published_Term	AS	Item_Name, C.Author_Copy, 
	      CASE WHEN CG.Concept_Group_Key=@ConceptGroupKey THEN NULL ELSE CG.Item_Name END AS Concept_Group_Name
		FROM Concept_Relation CR
	    	INNER JOIN Concept C ON C.Concept_Key=CR.From_Concept_Key
	    	INNER JOIN Term T ON T.Term_Key=C.Term_Key
				INNER JOIN Thesaurus_Relation_Type TRT ON TRT.Thesaurus_Relation_Type_Key=CR.Thesaurus_Relation_Type_Key
			  INNER JOIN Concept_Group CG ON CG.Concept_Group_Key=C.Concept_Group_Key
				INNER JOIN @ConceptKeys CK ON CK.Concept_Key=CR.To_Concept_Key
						AND (CK.Ancestor=0 OR CR.Inherited=1)
	) Data INNER JOIN Concept_Relation CR ON CR.Concept_Relation_Key=Data.Concept_Relation_Key
  ORDER BY PlainText, Author_Copy


--Insert forward terms for meaning relations
INSERT INTO @Output (Relationship_Name, Concept_Key, Plaintext, 
      Item_Name, Author_And_Date, Concept_Group_Name, Details)
	SELECT 
		Data.Forward_Term, 
		Data.Concept_Key, 
		Data.Plaintext, 
		Data.Item_Name, 
		Data.Author_Copy, 
		Data.Concept_Group_Name,
		MR.Comment
	FROM (
		SELECT DISTINCT MR.Meaning_Relation_Key, TRT.Forward_Term, C2.Concept_Key, T.Plaintext, 
	      C2.Published_Term	AS	Item_Name, C2.Author_Copy, 
	      CASE WHEN CG.Concept_Group_Key=@ConceptGroupKey THEN NULL ELSE CG.Item_Name END AS Concept_Group_Name
		FROM Concept C1
	      INNER JOIN Meaning_Relation MR ON MR.From_Meaning_Key=C1.Meaning_Key
				INNER JOIN Concept C2 ON C2.Meaning_Key=MR.To_Meaning_Key
					AND C2.List_Preferred=1
	    	INNER JOIN Term T ON T.Term_Key=C2.Term_Key
				INNER JOIN Thesaurus_Relation_Type TRT ON TRT.Thesaurus_Relation_Type_Key=MR.Thesaurus_Relation_Type_Key
			  INNER JOIN Concept_Group CG ON CG.Concept_Group_Key=C2.Concept_Group_Key
				INNER JOIN @ConceptKeys CK ON CK.Concept_Key=C1.Concept_Key
						AND (CK.Ancestor=0 OR MR.Inherited=1)
	) Data INNER JOIN Meaning_Relation MR ON MR.Meaning_Relation_Key=Data.Meaning_Relation_Key
  ORDER BY Plaintext, Author_Copy


--Insert reverse terms for meaning relations
INSERT INTO @Output (Relationship_Name, Concept_Key, Plaintext, 
      Item_Name, Author_And_Date, Concept_Group_Name, Details)
	SELECT 
		Data.Reverse_Term, 
		Data.Concept_Key, 
		Data.Plaintext, 
		Data.Item_Name, 
		Data.Author_Copy, 
		Data.Concept_Group_Name,
		MR.Comment
	FROM (
		SELECT DISTINCT MR.Meaning_Relation_Key, TRT.Reverse_Term, C2.Concept_Key, T.Plaintext, 
	      C2.Published_Term	AS	Item_Name, C2.Author_Copy, 
	      CASE WHEN CG.Concept_Group_Key=@ConceptGroupKey THEN NULL ELSE CG.Item_Name END AS Concept_Group_Name
		FROM Concept C1
	      INNER JOIN Meaning_Relation MR ON MR.To_Meaning_Key=C1.Meaning_Key
				INNER JOIN Concept C2 ON C2.Meaning_Key=MR.From_Meaning_Key
					AND C2.List_Preferred=1
	    	INNER JOIN Term T ON T.Term_Key=C2.Term_Key
				INNER JOIN Thesaurus_Relation_Type TRT ON TRT.Thesaurus_Relation_Type_Key=MR.Thesaurus_Relation_Type_Key
			  INNER JOIN Concept_Group CG ON CG.Concept_Group_Key=C2.Concept_Group_Key
				INNER JOIN @ConceptKeys CK ON CK.Concept_Key=C1.Concept_Key
						AND (CK.Ancestor=0 OR MR.Inherited=1)
	) Data INNER JOIN Meaning_Relation MR ON MR.Meaning_Relation_Key=Data.Meaning_Relation_Key
  ORDER BY Plaintext, Author_Copy


--Insert all known synonyms
INSERT INTO @Output (Relationship_Name, Concept_Key, Plaintext, 
      Item_Name, Author_And_Date, Concept_Group_Name)
	SELECT '#Synonyms', C2.Concept_Key, T.Plaintext, C2.Published_Term, C2.Author_Copy, 
      CASE WHEN CG.Concept_Group_Key=@ConceptGroupKey THEN NULL ELSE CG.Item_Name END 
	FROM Concept C1
	    INNER JOIN Concept C2 ON C2.Meaning_Key=C1.Meaning_Key
			INNER JOIN Term T ON T.Term_Key=C2.Term_Key
			INNER JOIN Concept_Group CG on CG.Concept_Group_Key=C2.Concept_Group_Key
  WHERE C1.Concept_Key=@ConceptKey
  ORDER BY T.Plaintext, C2.Author_Copy


--Table to hold all term versions
DECLARE @TermVersions TABLE (
		Concept_Key char(16) COLLATE SQL_Latin1_General_CP1_CI_AS PRIMARY KEY,
		Term_Version_Key char(16) COLLATE SQL_Latin1_General_CP1_CI_AS,
		Sort_Order INT
)

--Insert the selected concept into the version list
INSERT INTO @TermVersions(Concept_Key, Term_Version_Key, Sort_Order) 
		SELECT C.Concept_Key, C.Term_Version_Key, C.Sort_Code
		FROM Concept C
				INNER JOIN Term_Version TV ON TV.Term_Version_Key=C.Term_Version_Key
		WHERE C.Concept_Key=@ConceptKey

--Recursively find all the versions, first get the nearest set of versions
INSERT INTO @TermVersions(Concept_Key, Term_Version_Key, Sort_Order)
		SELECT TVR.From_Concept_Key, TVR.From_Term_Version_Key, TV.Sort_Order-1
		FROM @TermVersions TV
				INNER JOIN Term_Version_Relation TVR ON TVR.To_Term_Version_Key=TV.Term_Version_Key
				INNER JOIN Thesaurus_Relation_Type TRT ON TRT.Thesaurus_Relation_Type_Key=TVR.Thesaurus_Relation_Type_Key
				INNER JOIN Semantic_Relation SR ON SR.Semantic_Relation_Key=TRT.Semantic_Relation_Key
						AND SR.Reverse_Equivalence_Possible=1
 				--Join to exclude items already in the list
				LEFT JOIN @TermVersions TVExist ON TVExist.Term_Version_Key=TVR.From_Term_Version_Key
		WHERE TVExist.Concept_Key IS NULL
		UNION
		SELECT TVR.To_Concept_Key, TVR.To_Term_Version_Key, TV.Sort_Order+1
		FROM @TermVersions TV
				INNER JOIN Term_Version_Relation TVR ON TVR.From_Term_Version_Key=TV.Term_Version_Key
				INNER JOIN Thesaurus_Relation_Type TRT ON TRT.Thesaurus_Relation_Type_Key=TVR.Thesaurus_Relation_Type_Key
				INNER JOIN Semantic_Relation SR ON SR.Semantic_Relation_Key=TRT.Semantic_Relation_Key
						AND SR.Forward_Equivalence_Possible=1
 				--Join to exclude items already in the list
				LEFT JOIN @TermVersions TVExist ON TVExist.Term_Version_Key=TVR.To_Term_Version_Key
		WHERE TVExist.Concept_Key IS NULL

--Now recurse to find all other versions
WHILE @@ROWCOUNT<>0
	INSERT INTO @TermVersions(Concept_Key, Term_Version_Key, Sort_Order)
			SELECT TVR.From_Concept_Key, TVR.From_Term_Version_Key, TV.Sort_Order-1
			FROM @TermVersions TV
					INNER JOIN Term_Version_Relation TVR ON TVR.To_Term_Version_Key=TV.Term_Version_Key
					INNER JOIN Thesaurus_Relation_Type TRT ON TRT.Thesaurus_Relation_Type_Key=TVR.Thesaurus_Relation_Type_Key
					INNER JOIN Semantic_Relation SR ON SR.Semantic_Relation_Key=TRT.Semantic_Relation_Key
							AND SR.Reverse_Equivalence_Possible=1
	 				--Join to exclude items already in the list
					LEFT JOIN @TermVersions TVExist ON TVExist.Term_Version_Key=TVR.From_Term_Version_Key
			WHERE TVExist.Concept_Key IS NULL
			UNION
			SELECT TVR.To_Concept_Key, TVR.To_Term_Version_Key, TV.Sort_Order+1
			FROM @TermVersions TV
					INNER JOIN Term_Version_Relation TVR ON TVR.From_Term_Version_Key=TV.Term_Version_Key
					INNER JOIN Thesaurus_Relation_Type TRT ON TRT.Thesaurus_Relation_Type_Key=TVR.Thesaurus_Relation_Type_Key
					INNER JOIN Semantic_Relation SR ON SR.Semantic_Relation_Key=TRT.Semantic_Relation_Key
							AND SR.Forward_Equivalence_Possible=1
	 				--Join to exclude items already in the list
					LEFT JOIN @TermVersions TVExist ON TVExist.Term_Version_Key=TVR.To_Term_Version_Key
			WHERE TVExist.Concept_Key IS NULL

	
INSERT INTO @Output (Relationship_Name, Concept_Key, Plaintext, 
    Item_Name, Author_And_Date, Concept_Group_Name)
SELECT '#TermVersions', 
		C.Concept_Key,
		T.Plaintext,
		C.Published_Term,
		CASE WHEN TV.Version_Label IS NULL THEN 
			C.Author_Copy
		ELSE
			C.Author_Copy + ' (' + TV.Version_Label + ')'
		END,
    CASE WHEN CG.Concept_Group_Key=@ConceptGroupKey THEN NULL ELSE CG.Item_Name END 
FROM Concept C
		INNER JOIN Term T ON T.Term_Key=C.Term_Key
		INNER JOIN Concept_Group CG on CG.Concept_Group_Key=C.Concept_Group_Key
		INNER JOIN Term_Version TV on TV.Term_Version_Key=C.Term_Version_Key
		INNER JOIN @TermVersions TVLIST ON TVLIST.Concept_Key=C.Concept_Key
ORDER BY TVLIST.Sort_Order ASC


--Select the output
SELECT * FROM @Output

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptRelations_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptRelations_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptRelations_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptRelations_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptRelations_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ConceptRelations_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptRelations_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptRelations_Select TO [Dev - JNCC SQL]
END

GO
/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptSimple_Insert]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptSimple_Insert]
GO

/*===========================================================================*\
  Description:	Inserts a new concept and term (if necessary).

  Parameters:	@Key OUTPUT,
		@TermKey 
		@ConceptGroupKey 
		@TermVersionKey 
		@ListPreferred 
		@IsCurrent
		@Preferred 
		@ConceptRankKey 
		@NameTypeConceptKey 
		@MeaningKey 
		@AuthorCopy 
		@SortCode 
		@ListCode
		@SessionID 
		@SystemSuppliedData 

  Created:	December 2003

  Last revision information:
    $Revision: 5 $
    $Date: 17/08/11 15:40 $
    $Author: Jamesbichard $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_ConceptSimple_Insert]
	@Key char(16) OUTPUT,
	@TermKey char(16),
	@ConceptGroupKey char(16),
	@AuthorAndDate varchar(100) = NULL,
	@Attributes varchar(100) = NULL,
	@ListPreferred bit = NULL,
	@IsCurrent bit = NULL,
	@Preferred bit = NULL,
	@ConceptRankKey char(16) = NULL,
	@NameTypeConceptKey char(16) = NULL,
	@MeaningKey char(16) = NULL,
	@AuthorCopy varchar(100) = NULL,
	@SortCode int = NULL,
	@ListCode varchar(50) = NULL,
	@PublishedTerm nvarchar(450),
	@AutomaticPublishedTerm bit = 0,
	@TermGeneratorKey char(16),
	@SessionID char(16),
	@SystemSuppliedData bit = NULL
AS
	SET NOCOUNT ON
	SET ANSI_NULLS ON
	SET ANSI_PADDING ON
	SET ANSI_WARNINGS ON
	SET CONCAT_NULL_YIELDS_NULL ON
	SET QUOTED_IDENTIFIER ON
	SET NO_BROWSETABLE OFF
	SET XACT_ABORT ON

	BEGIN TRANSACTION
		DECLARE @NewMeaningKey char(16),
				@TermVersionKey char(16)
		
		SET @ListPreferred = IsNull(@ListPreferred, 1)
	
		/*-------------------------------------------------------------*\
		  If we don't have a meaning key, create one.
		\*-------------------------------------------------------------*/
		IF @MeaningKey IS NULL
		BEGIN
			EXEC spNextKey 'Meaning', @NewMeaningKey OUTPUT--, @SiteID
			IF @@ERROR <> 0 GOTO RollbackAndExit
			
			INSERT INTO Meaning (
				Meaning_Key
			) VALUES (
				@NewMeaningKey
			)
			IF @@Error <> 0 GOTO RollbackAndExit
		END
		ELSE
			SET @NewMeaningKey = @MeaningKey

		/*-------------------------------------------------------------*\
			Create new term version
		\*-------------------------------------------------------------*/

		EXEC usp_TermVersion_Insert
			@Key = @TermVersionKey OUTPUT,
			@ConceptKey = '',
			@TermKey = @TermKey,
			@VersionLabel = @Attributes,
			@AuthorAndDate = @AuthorAndDate,
			@SessionID = @SessionID,
			@SystemSuppliedData = @SystemSuppliedData
		IF @@Error <> 0 GOTO RollbackAndExit		
	
		/*-------------------------------------------------------------*\
		  Create new Concept.
		\*-------------------------------------------------------------*/
		EXEC spNextKey 'Concept', @Key OUTPUT
		IF @@ERROR <> 0 GOTO RollbackAndExit

		DECLARE @Plaintext VARCHAR(100)

		SELECT @Plaintext = Plaintext
		FROM Term
		WHERE Term_Key = @TermKey
		
		INSERT INTO Concept (
			Concept_Key, Term_Key, Concept_Group_Key, List_Preferred, 
			Is_Current, Preferred, Concept_Rank_Key, Name_Type_Concept_Key, 
			Meaning_Key, Author_Copy, Sort_Code, List_Code,
			Published_Term, Automatic_Published_Term, Term_Generator_Key,
			Entered_Session_ID, System_Supplied_Data, Term_Version_Key
		) VALUES (
			@Key, @TermKey, @ConceptGroupKey, @ListPreferred, 
			IsNull(@IsCurrent, 1), IsNull(@Preferred, 0), @ConceptRankKey,
			@NameTypeConceptKey, @NewMeaningKey, @AuthorCopy, @SortCode,
			@ListCode, ISNULL(@PublishedTerm, @Plaintext), @AutomaticPublishedTerm,
			@TermGeneratorKey, @SessionID, 0, @TermVersionKey
		)
		IF @@Error <> 0 GOTO RollbackAndExit
	
		/*-------------------------------------------------------------*\
		  Create Concept_Lineage.
		\*-------------------------------------------------------------*/
		EXECUTE		usp_ConceptLineage_NewConcept	@Key
		IF @@ERROR <> 0 GOTO RollbackAndExit
		
		/*----------------------------------------------------------------------------*\
		  If @Preferred = 1, then make sure the updated concept is the 
		  only Preferred synonym with the same language key and name type concept key.
		\*----------------------------------------------------------------------------*/
		IF @Preferred = 1 
			UPDATE		CSynonyms
			SET		Preferred = 0
			FROM 		Concept AS CSource
			INNER JOIN	Term 	AS TSource 	ON TSource.Term_Key = CSource.Term_Key
			INNER JOIN	Concept AS CSynonyms 	ON CSynonyms.Meaning_Key = CSource.Meaning_Key
								AND CSynonyms.Name_Type_Concept_Key = CSource.Name_Type_Concept_Key
								AND CSynonyms.Concept_Key <> CSource.Concept_Key
			INNER JOIN	Term 	AS TSynonyms 	ON TSynonyms.Term_Key = CSynonyms.Term_Key
								AND TSynonyms.Language_Key = TSource.Language_Key
			WHERE		CSource.Concept_Key = @Key

		IF @@Error <> 0 GOTO RollbackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit:
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_ConceptSimple_Insert failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptSimple_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptSimple_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
			GRANT EXECUTE ON dbo.usp_ConceptSimple_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptSimple_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptSimple_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptSimple_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptSimple_Insert TO [Dev - JNCC SQL]
END
GO
/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptSimple_Update') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_ConceptSimple_Update]
GO

/*===========================================================================*\
  Description:	Updates a record in the concept table.

  Parameters:	@Key
		@TermKey 
		@ConceptGroupKey 
		@Preferred
		@ConceptRankKey 
		@NameTypeConceptKey
		@SortCode 
		@ListCode 
		@SessionID 
		@RecordsAffected 
		@Timestamp 

  Created:	December 2003

  Last revision information:
    $Revision: 5 $
    $Date: 17/08/11 15:40 $
    $Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptSimple_Update]
	@Key char(16),
	@TermKey char(16),
	@AuthorAndDate varchar(100) = NULL,
	@Attributes varchar(100) = NULL,
	@NewTermVersionNeeded bit = 0,
	@ConceptGroupKey char(16),
	@ListPreferred bit = NULL,
	@Preferred bit,
	@ConceptRankKey char(16),
	@NameTypeConceptKey char(16) = NULL,
	@SortCode int,
	@ListCode varchar(50),
	@PublishedTerm nvarchar(450) = NULL,
	@AutomaticPublishedTerm bit,
	@TermGeneratorKey char(16),
	@SessionID char(16),
	@RecordsAffected int OUTPUT,
	@Timestamp timestamp
AS
SET NOCOUNT OFF
	
	BEGIN TRANSACTION
		-- Check record has not been updated by another user
		IF EXISTS( SELECT * 
					FROM Concept
					WHERE Concept_Key = @Key and @timestamp = Timestamp)
		BEGIN
			-- VI 13430 - CCN178 - TSEQUAL and stored procs
			IF @RecordsAffected = 0 AND EXISTS (
				SELECT Concept_Key FROM Concept WHERE Concept_Key = @Key
			)
			BEGIN
				RAISERROR('Record updated by another user', 16, 1)
			END
		END

		/*--------------------------------------------------*\
			Update/create new term version
		\*--------------------------------------------------*/
		DECLARE @TermVersionKey CHAR(16)

		SELECT @TermVersionKey = Term_Version_Key
		FROM Concept
		WHERE Concept_Key = @Key

		IF @TermVersionKey IS NULL OR @NewTermVersionNeeded = 1
		BEGIN
			EXEC usp_TermVersion_Insert
				@Key = @TermVersionKey OUTPUT,
				@ConceptKey = @Key,
				@VersionLabel = @Attributes,
				@AuthorAndDate = @AuthorAndDate,
				@SessionID = @SessionID
			IF @@Error <> 0 GOTO RollbackAndExit
		END
		ELSE
		BEGIN
			EXEC usp_TermVersion_Update
				@Key = @TermVersionKey,
				@ConceptKey = @Key,
				@VersionLabel = @Attributes,
				@AuthorAndDate = @AuthorAndDate,
				@SessionID = @SessionID	

			IF @@Error <> 0 GOTO RollbackAndExit		
		END		
		
		/*-------------------*\
		  Update the Concept.
		\*-------------------*/
		DECLARE @old_concept_group_key CHAR(16),
			@old_list_preferred BIT,
			@OldConceptRankKey char(16),
			@error INT

		DECLARE @Plaintext VARCHAR(100)

		SELECT @Plaintext = Plaintext
		FROM Term
		WHERE Term_Key = @TermKey
		
		UPDATE	Concept
		SET 	@old_concept_group_key = Concept_Group_Key,
			@old_list_preferred = List_Preferred,
			@OldConceptRankKey = Concept_Rank_Key,
			List_Preferred = IsNull(@ListPreferred, List_Preferred),
			Concept_Group_Key = @ConceptGroupKey,
			Term_Key = @TermKey,
			Term_Version_Key = @TermVersionKey,
			Concept_Rank_Key = @ConceptRankKey,
			Preferred = @Preferred,
			Name_Type_Concept_Key = @NameTypeConceptKey,
			Sort_Code = @SortCode,
			List_Code = @ListCode,
			Published_Term = ISNULL(@PublishedTerm, @Plaintext),
			Automatic_Published_Term = @AutomaticPublishedTerm,
			Term_Generator_Key = @TermGeneratorKey,
			Changed_Session_ID = @SessionID			
		WHERE	Concept_Key = @Key

		SELECT	@error = @@ERROR,
			@RecordsAffected = @@ROWCOUNT

		IF @error <> 0 GOTO RollbackAndExit		

		/*----------------------------------------------------------------------------*\
		  Make corresponding changes in Concept_Lineage
		\*----------------------------------------------------------------------------*/
		EXECUTE	usp_ConceptLineage_ConceptUpdated	@Key,
								@old_concept_group_key,
								@old_list_preferred
		IF @@ERROR <> 0 GOTO RollbackAndExit
		
		/*----------------------------------------------------------------------------*\
		  If @Preferred = 1, then make sure the updated concept is the 
		  only Preferred synonym with the same language key and name type concept key.
		\*----------------------------------------------------------------------------*/
		IF @Preferred = 1 
			UPDATE		CSynonyms
			SET		Preferred = 0
			FROM 		Concept AS CSource
			INNER JOIN	Term 	AS TSource 	ON TSource.Term_Key = CSource.Term_Key
			INNER JOIN	Concept AS CSynonyms 	ON CSynonyms.Meaning_Key = CSource.Meaning_Key
								AND CSynonyms.Name_Type_Concept_Key = CSource.Name_Type_Concept_Key
								AND CSynonyms.Concept_Key <> CSource.Concept_Key
			INNER JOIN	Term 	AS TSynonyms 	ON TSynonyms.Term_Key = CSynonyms.Term_Key
								AND TSynonyms.Language_Key = TSource.Language_Key
			WHERE		CSource.Concept_Key = @Key

		IF @@Error <> 0 GOTO RollbackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit:
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_ConceptSimple_Update failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptSimple_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptSimple_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptSimple_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptSimple_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptSimple_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptSimple_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptSimple_Update TO [Dev - JNCC SQL]
END
GO
/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptTermAndAuthor_Get]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptTermAndAuthor_Get]
GO

/*===========================================================================*\
  Description: Returns the published term of a Concept.

  Parameters:	@ConceptKey	

  Created:	August 2003

  Last revision information:
    $Revision: 5 $
    $Date: 17/08/11 15:40 $
    $Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptTermAndAuthor_Get]
	@ConceptKey varchar(100),
    @ConceptTermAndAuthor varchar(505) output

AS

  SELECT @ConceptTermAndAuthor =	Published_Term
  FROM Concept
  WHERE Concept_Key = @ConceptKey

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptTermAndAuthor_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptTermAndAuthor_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptTermAndAuthor_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptTermAndAuthor_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptTermAndAuthor_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ConceptTermAndAuthor_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptTermAndAuthor_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptTermAndAuthor_Get TO [Dev - JNCC SQL]
END

GO
/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptVersionListPreferred_Select_ForConcept]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptVersionListPreferred_Select_ForConcept]
GO

/*===========================================================================*\
  Description:	Retrieves details of the list preferred concept using any concept
								key as input.  Input could be a synonym, but proper name will be 
								returned.  Preferred name is picked from the supplied list version

  Parameters:	@ConceptGroupVersionKey - key of the concept group version to obtain 
																				name
							@ConceptKey - concept key

  Created:	August 2003

  Last revision information:
    $Revision: 5 $
    $Date: 17/08/11 15:40 $
    $Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptVersionListPreferred_Select_ForConcept]
	@ConceptGroupVersionKey char(16),
  @ConceptKey varchar(16)
AS


SELECT CPref.Concept_Key, CPref.Published_Term AS Item_Name, CPref.Concept_Rank_Key, 
  CASE WHEN (CGV1Child.Concept_Group_Version_Key IS NULL OR CGV1Child.Sequence<=CGVChild.Sequence)
      AND (CGV2Child.Concept_Group_Version_Key IS NULL OR CGV2Child.Sequence>=CGVChild.Sequence) 
      AND CChild.Concept_Key IS NOT NULL THEN
    1
  ELSE
    0 
  END AS HasChildren
FROM Concept C
  INNER JOIN Concept CPref on CPref.Meaning_Key=C.Meaning_Key
  LEFT JOIN Concept_Relation CR on CR.From_Concept_Key=CPref.Concept_Key
  INNER JOIN Concept_Group_Version CGV on CGV.Concept_Group_Key=CPref.Concept_Group_Key
      AND CGV.Concept_Group_Version_Key=@ConceptGroupVersionKey
  LEFT JOIN Concept_History CH on CH.Concept_Key=CPref.Concept_Key
  LEFT JOIN Concept_Group_Version CGV1 ON CGV1.Concept_Group_Version_Key=CH.Concept_Group_Version_From
  LEFT JOIN Concept_Group_Version CGV2 ON CGV2.Concept_Group_Version_Key=CH.Concept_Group_Version_To
  --And ensure the children are checked in the correct concept group version
  LEFT JOIN Concept CChild on CChild.Concept_Key=CR.To_Concept_Key
  LEFT JOIN Concept_Group_Version CGVChild on CGVChild.Concept_Group_Key=CChild.Concept_Group_Key
      AND CGVChild.Concept_Group_Version_Key=@ConceptGroupVersionKey
  LEFT JOIN Concept_History CHChild on CHChild.Concept_Key=CChild.Concept_Key
  LEFT JOIN Concept_Group_Version CGV1Child ON CGV1Child.Concept_Group_Version_Key=CHChild.Concept_Group_Version_From
  LEFT JOIN Concept_Group_Version CGV2Child ON CGV2Child.Concept_Group_Version_Key=CHChild.Concept_Group_Version_To
WHERE C.Concept_Key=@ConceptKey
  AND CPref.List_Preferred=1
  AND (CGV1.Concept_Group_Version_Key IS NULL OR CGV1.Sequence<=CGV.Sequence)
  AND (CGV2.Concept_Group_Version_Key IS NULL OR CGV2.Sequence>=CGV.Sequence)


GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptVersionListPreferred_Select_ForConcept') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptVersionListPreferred_Select_ForConcept'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptVersionListPreferred_Select_ForConcept TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptVersionListPreferred_Select_ForConcept TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptVersionListPreferred_Select_ForConcept TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ConceptVersionListPreferred_Select_ForConcept TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptVersionListPreferred_Select_ForConcept TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptVersionListPreferred_Select_ForConcept TO [Dev - JNCC SQL]
END

GO


/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Concept_Delete]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Concept_Delete]
GO

/*===========================================================================*\
  Description:	Delete a record from the Concept tables. Also deletes records
		from other tables where necessary.  If @DeleteUnlinkedSynonyms
		is 1, then removes any non-list preferred concepts from the 
		same concept group.

  Parameters:	@Key		Concept key.
				@Timestamp

  Created:	December 2003

  Last revision information:
    $Revision: 5 $
    $Date: 17/08/11 15:40 $
    $Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Concept_Delete]
	@Key char(16),
	@Timestamp timestamp = NULL,
	@SyncTaxonDict bit = 0,
	@DeleteUnlinkedSynonyms bit = 0
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	DECLARE @MeaningKey char(16),
			@TermKey char(16),
			@TermVersionKey char(16),
			@ConceptsSharingMeaningKeyCount int,
			@ConceptsSharingTermKeyCount int,
			@ConceptsSharingTermVersionKeyCount int,
			@OriginalTimestamp timestamp,
			@error				INT,
			@RecordsAffected	INT

	-- Store the Meaning, Term and Term Version keys because the concept record
	-- needs to be deleted before these other records can be, due to referential
	-- integrity.
	SELECT	@MeaningKey = Meaning_Key,
			@TermKey = Term_Key,
			@TermVersionKey = Term_Version_Key,
			@OriginalTimestamp = [Timestamp]
	FROM 	Concept
	WHERE	Concept_Key = @Key

	-- Count the number of concepts that use this meaning key.
	SELECT 		@ConceptsSharingMeaningKeyCount = Count(C2.Concept_Key)
	FROM		Concept AS C1
	INNER JOIN	Concept AS C2 ON C2.Meaning_Key = C1.Meaning_Key
	WHERE		C1.Concept_Key = @Key

	-- Count the number of concepts that use the same term key as the concept we want to delete.
	SELECT 		@ConceptsSharingTermKeyCount = Count(DISTINCT C2.Concept_Key)
	FROM		Concept AS C1
	INNER JOIN	Concept AS C2 ON C2.Term_Key = C1.Term_Key
	WHERE		C1.Concept_Key = @Key

	-- Count the number of concepts that use the same term version key as the concept we want to delete.
	SELECT 		@ConceptsSharingTermVersionKeyCount = Count(DISTINCT C2.Concept_Key)
	FROM		Concept AS C1
	INNER JOIN	Concept AS C2 ON C2.Term_Version_Key = C1.Term_Version_Key
	WHERE		C1.Concept_Key = @Key


	BEGIN TRANSACTION
		/*============================================================*\
		  See if the user wants any associated taxon dictionary
		  records be deleted with the concept.
		\*============================================================*/
		IF @SyncTaxonDict = 1 
		BEGIN
			/*--------------------------------------------------------*\
			  Check that the Taxon_List_Item table exists before
			  attempting any of this deletion. In the future, the 
			  Thesaurus module could be installed without the Taxon
			  tables, so would go wrong if we tried to delete from
			  non-existant tables.			
			\*--------------------------------------------------------*/
			IF EXISTS (SELECT *
		   			FROM   SysObjects 
					WHERE  Id = Object_Id(N'[dbo].[Taxon_List_Item]')
					AND 	  Type = 'U')
			BEGIN
				-- Get the Taxon List Item Key for the current Concept
				DECLARE @TaxonListItemKey char(16)
	
				SELECT 	@TaxonListItemKey = Taxon_List_Item_Key
				FROM	Taxon_Dictionary_Concept_Mapping
				WHERE	Concept_Key = @Key

				/*--------------------------------------------------------*\
				  Delete the records related to the Taxon_List_Item table
				\*--------------------------------------------------------*/
				DELETE 	Taxon_Dictionary_Concept_Mapping
				WHERE	Taxon_List_Item_Key = @TaxonListItemKey
				AND	Concept_Key = @Key

				IF @@Error <> 0 GOTO RollbackAndExit

				DELETE	Taxon_Common_Name
				WHERE	Taxon_List_Item_Key = @TaxonListItemKey

				IF @@Error <> 0 GOTO RollbackAndExit

				DELETE	Index_Taxon_Synonym
				WHERE	Taxon_List_Item_Key = @TaxonListItemKey
				OR	Synonym_List_Item_Key = @TaxonListItemKey

				IF @@Error <> 0 GOTO RollbackAndExit

				DELETE	Export_Filter_Taxon
				WHERE	Taxon_List_Item_Key = @TaxonListItemKey

				IF @@Error <> 0 GOTO RollbackAndExit

				DELETE	Index_Taxon_Group
				WHERE	Taxon_List_Item_Key = @TaxonListItemKey
				OR	Contained_List_Item_Key = @TaxonListItemKey

				IF @@Error <> 0 GOTO RollbackAndExit

				DELETE	Taxon_Designation
				WHERE	Taxon_List_Item_Key = @TaxonListItemKey

				IF @@Error <> 0 GOTO RollbackAndExit

				DELETE	Taxon_User_Name
				WHERE	Taxon_List_Item_Key = @TaxonListItemKey

				IF @@Error <> 0 GOTO RollbackAndExit

				DELETE	Index_Taxon_Name
				WHERE	Taxon_List_Item_Key = @TaxonListItemKey

				IF @@Error <> 0 GOTO RollbackAndExit

				DELETE	Nameserver
				WHERE	Recommended_Taxon_List_Item_Key = @TaxonListItemKey

				IF @@Error <> 0 GOTO RollbackAndExit

				-- If one Concept shares the Term, attempt to delete the equivalent Taxon.
				IF @ConceptsSharingTermKeyCount = 1
				BEGIN
					DECLARE @TaxonKey char(16)

					-- Get the key of the equivalent Taxon
					SELECT 	@TaxonKey = Taxon_Key
					FROM	Taxon_Dictionary_Term_Mapping
					WHERE	Term_Key = @TermKey

							-- Only delete if there are no Taxon_Version records using the Taxon
					IF NOT EXISTS(SELECT 	*
									FROM 	Taxon_Version
									WHERE	Taxon_Key = @TaxonKey)
					BEGIN
						DELETE SF
						FROM Source_File SF
						INNER JOIN Taxon_Sources TS ON TS.Source_Key=SF.Source_Key
						WHERE TS.Taxon_Key=@TaxonKey
		
						DELETE Taxon_Sources
						WHERE Taxon_Key=@TaxonKey
					
						DELETE	Taxon
						WHERE	Taxon_Key = @TaxonKey
					END
				END

				/*-----------------------------------------------------------------*\
				  It is possible that this delete will fail. e.g. If the TLI record
				  is referred to in the Taxon_Determination table, or a row in 
				  the TLI table has its Parent set to the record we are attempting
				  to delete. This will cause it to go to the RollbackAndExit method,
				  where the user can be asked if they want to replace the concept
				  with another (4.2.17.18). Before deleting the TLI records, we
				  need to remove the Taxon_Dictionary_Meaning_Mapping records.
				\*-----------------------------------------------------------------*/ 
				DELETE	Taxon_Dictionary_Meaning_Mapping
				WHERE	Preferred_Name = @TaxonListItemKey

				IF @@Error <> 0 GOTO RollbackAndExit

				DELETE 	Taxon_List_Item
				WHERE	Taxon_List_Item_Key = @TaxonListItemKey

				IF @@Error <> 0 GOTO RollbackAndExit
			END
		END			

		/*====================================*\
		  Delete the synonyms that are no longer
		  required.
		\*====================================*/
		IF @DeleteUnlinkedSynonyms=1
		BEGIN
			DECLARE @SynConceptKey CHAR(16)
			
			DECLARE csr CURSOR FOR
				SELECT CSyn.Concept_Key
				FROM Concept C
				INNER JOIN Concept CSyn 
					ON CSyn.Meaning_Key=C.Meaning_Key
					AND CSyn.Concept_Group_Key=C.Concept_Group_Key
					AND CSyn.List_Preferred=0
					AND C.Concept_Key=@Key
			
			OPEN csr
			WHILE (1=1)
			BEGIN
				FETCH NEXT FROM csr INTO @SynConceptKey

				IF @@FETCH_STATUS <> 0
					BREAK

				-- Recurse to remove synonym concepts
				EXEC usp_Concept_Delete @SynConceptKey
			END
			CLOSE csr
			DEALLOCATE csr
		END
	
		/*====================================*\
		  Delete the records.
		\*====================================*/
		--Delete the concept's search terms
		DELETE	Search_Term
		WHERE	Concept_Key = @Key
			
		IF @@Error <> 0 GOTO RollbackAndExit

		-- Delete the Concept_History record.
		DELETE	Concept_History
		WHERE	Concept_Key = @Key

		IF @@Error <> 0 GOTO RollbackAndExit

		/*-------------------------------------------------------*\
		  Delete the relation records which refer to the concept.
		\*-------------------------------------------------------*/
		DELETE	Concept_Relation
		WHERE	To_Concept_Key = @Key
		OR	From_Concept_Key = @Key

		IF @@Error <> 0 GOTO RollbackAndExit

		DELETE	Meaning_Relation
		WHERE	To_Concept_Key = @Key
		OR	From_Concept_Key = @Key

		IF @@Error <> 0 GOTO RollbackAndExit

		DELETE	Term_Version_Relation
		WHERE	To_Concept_Key = @Key
		OR	From_Concept_Key = @Key

		IF @@Error <> 0 GOTO RollbackAndExit

		/*-------------------------------------------------------*\
		  Delete the Enquiry_Concept records because otherwise
		  the deletion will fail because it says other records
		  link to the Concept. Enquiries cannot be viewed in the
		  Thesaurus Editor it would appear at a casual glance
		  that nothing is actually linked to the concept. 
		  So best to just delete the Enquiry_Concept join records.
		\*-------------------------------------------------------*/
		IF EXISTS (SELECT * FROM dbo.sysobjects WHERE id = object_id(N'[dbo].[Enquiry_Concept]') 
					AND OBJECTPROPERTY(id, N'IsUserTable') = 1)
			DELETE	Enquiry_Concept
			WHERE	Concept_Key = @Key

		IF @@Error <> 0 GOTO RollbackAndExit

		-- Delete the Concept_Lineage records.
		IF EXISTS (SELECT 1 FROM Concept WHERE Concept_Key = @Key)
		BEGIN
			EXECUTE		usp_ConceptLineage_DeleteConcept	@Key
			IF @@ERROR <> 0 GOTO RollbackAndExit
		END

		/*-------------------------------------------------------*\
			Delete the concept's designation records (and related)
		\*-------------------------------------------------------*/
		IF EXISTS (SELECT * FROM dbo.sysobjects WHERE id = object_id(N'[dbo].[Taxon_Dictionary_Concept_Designation_Mapping]') 
					AND OBJECTPROPERTY(id, N'IsUserTable') = 1)
			DELETE DM
			FROM Taxon_Dictionary_Concept_Designation_Mapping DM
			INNER JOIN Concept_Designation CD ON CD.Concept_Designation_Key=DM.Concept_Designation_Key
			WHERE CD.Concept_Key=@Key

		IF @@Error <> 0 GOTO RollbackAndExit		

		IF EXISTS (SELECT * FROM dbo.sysobjects WHERE id = object_id(N'[dbo].[Source_Join]') 
					AND OBJECTPROPERTY(id, N'IsUserTable') = 1)
		BEGIN
			--Delete the source files
			DELETE SF
			FROM Source_File SF
			INNER JOIN Source_Join SJ
					ON SJ.Table_Name='Concept_Designation'
			INNER JOIN Concept_Designation CD
					ON CD.Concept_Designation_Key=SJ.Record_Key
					AND CD.Concept_Key=@Key
	
			IF @@Error <> 0 GOTO RollbackAndExit
		
			--Now delete the source joins
			DELETE SJ
			FROM Source_Join SJ
			INNER JOIN Concept_Designation CD
					ON CD.Concept_Designation_Key=SJ.Record_Key
					AND CD.Concept_Key=@Key
			WHERE SJ.Table_Name='Concept_Designation'

			IF @@Error <> 0 GOTO RollbackAndExit

			--Delete the source files for the main concept
			DELETE SF
			FROM Source_File SF
			INNER JOIN Source_Join SJ
					ON SJ.Table_Name='Concept' AND SJ.Record_Key=@Key

			IF @@Error <> 0 GOTO RollbackAndExit

			--Now delete the source joins
			DELETE SJ
			FROM Source_Join SJ
			WHERE SJ.Table_Name='Concept' AND SJ.Record_Key=@Key

			IF @@Error <> 0 GOTO RollbackAndExit
		END

		DELETE 
		FROM Concept_Designation
		WHERE Concept_Key=@Key

		/*-------------------------------------------------------*\
			 Delete the Concept record. Have to check timestamp passed into the proc
			 against the timestamp the Concept had before any of its related records
			 were deleted. This is because deleting the records above may cause
			 triggers to be fired. Deleting the record in Concept_History will fire
			 a trigger that updates the current Concept, causing its timestamp to 
			 change.
		\*-------------------------------------------------------*/

		DELETE	Concept
		WHERE	Concept_Key = @Key
		AND		(@Timestamp = @OriginalTimestamp OR @Timestamp IS NULL)

		-- VI 13430 - CCN178 - TSEQUAL and stored procs
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Concept WHERE Concept_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

		IF @RecordsAffected = 0 AND EXISTS (
			SELECT Concept_Key FROM Concept WHERE Concept_Key = @Key
		)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
		END

		-- Delete the Meaning record if only one Concept uses that Meaning key. Also
		-- delete any Homonym_Pair records with this meaning key.
		IF @ConceptsSharingMeaningKeyCount = 1 
		BEGIN
			DELETE	Homonym_Pair
			WHERE	Meaning_Key_1 = @MeaningKey
			OR		Meaning_Key_2 = @MeaningKey

			DELETE	Taxon_Dictionary_Meaning_Mapping
			WHERE	Meaning_Key = @MeaningKey

			DELETE 	Meaning
			WHERE	Meaning_Key = @MeaningKey
		END

		IF @@Error <> 0 GOTO RollbackAndExit

		-- Delete the Term Version record if only one Concept uses that Term Version key.
		IF @ConceptsSharingTermVersionKeyCount = 1
			DELETE	Term_Version
			WHERE	Term_Version_Key = @TermVersionKey

		IF @@Error <> 0 GOTO RollbackAndExit

		-- Delete the Term record if only one Concept uses that Term key.
		IF @ConceptsSharingTermKeyCount = 1
			IF NOT EXISTS(SELECT * FROM Term_Version WHERE Term_Key = @TermKey)	
				DELETE	Term
				WHERE	Term_Key = @TermKey

		IF @@Error <> 0 GOTO RollbackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
	RAISERROR ('usp_Concept_Delete failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Concept_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concept_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concept_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Concept_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Concept_Delete TO [Dev - JNCC SQL]
END
GO
/*===========================================================================*\
  Drop function before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].usp_Concept_GeneratePublishedTerm'))
    DROP PROCEDURE [dbo].usp_Concept_GeneratePublishedTerm
GO

/*===========================================================================*\
  Description:	A wrapper to call the function in the term generator
				table for generating published term

  Parameters:	 

  Created:	August 2011

  Last revision information:
    $Revision: 5 $
    $Date: 17/08/11 15:40 $
    $Author: Jamesbichard $

\*===========================================================================*/

CREATE PROCEDURE dbo.usp_Concept_GeneratePublishedTerm  
	@Plaintext VARCHAR(100),
	@AuthorAndDate VARCHAR(100),
	@Attributes VARCHAR(100),
	@RankKey CHAR(16),
	@ParentKey CHAR(16), 
	@TermGeneratorKey CHAR(16),
	@HierarchyRelationTypeKey CHAR(16)
AS
BEGIN
	DECLARE @PublishedTermFunction NVARCHAR(257)

	SELECT @PublishedTermFunction = Published_Term_Function
	FROM Term_Generator
	WHERE Term_Generator_Key = @TermGeneratorKey

	IF @HierarchyRelationTypeKey IS NULL
	BEGIN
		SELECT @Plaintext
	END
	ELSE
	BEGIN
		DECLARE @sql NVARCHAR(4000)
		DECLARE @params NVARCHAR(500)

		SELECT @sql = '
			SELECT ' + @PublishedTermFunction + '(@Plaintext, @AuthorAndDate, @Attributes, @RankKey, @ParentKey)'
		
		SET @params = '@Plaintext NVARCHAR(150), ' +
					'@AuthorAndDate VARCHAR(100), @Attributes VARCHAR(100), ' +
					'@RankKey CHAR(16), @ParentKey CHAR(16)'

		EXEC sp_executesql @sql, @params, 
				@Plaintext = @Plaintext,
				@AuthorAndDate = @AuthorAndDate,
				@Attributes = @Attributes,
				@RankKey = @RankKey,
				@ParentKey = @ParentKey
	END

END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_GeneratePublishedTerm') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Concept_GeneratePublishedTerm'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
			GRANT EXECUTE ON dbo.usp_Concept_GeneratePublishedTerm TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concept_GeneratePublishedTerm TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concept_GeneratePublishedTerm TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Concept_GeneratePublishedTerm TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Concept_GeneratePublishedTerm TO [Dev - JNCC SQL]
END
GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Concept_Get]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Concept_Get]
GO

CREATE PROCEDURE [dbo].[usp_Concept_Get] 
@Key CHAR(16),
@GetPublishedTerm BIT = 0,
@Caption VARCHAR(150) OUTPUT

AS

--  DESCRIPTION
--  Returns a Concept caption given the concept key
--
--  PARAMETERS
--	NAME				DESCRIPTION
--	@Key				Key of record to be returned
--	@Caption			Output caption
--
--
--  AUTHOR:				Ben Collier, Dorset Software
--  CREATED:			2003-11-03
--
SET NOCOUNT ON

SELECT @Caption = CASE
					WHEN @GetPublishedTerm = 0 THEN PlainText
					ELSE Item_Name
				  END
FROM
VW_CONCEPTTERM
WHERE Concept_Key = @Key

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Concept_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Concept_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concept_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concept_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Concept_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Concept_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Concept_Get TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_Concept_ImportTaxonDesignationTypes]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_Concept_ImportTaxonDesignationTypes]
GO

/*===========================================================================*\
  Description:	Import concepts corresponding to the taxon designation types
				used in the specified taxon list.

  Parameters:	@job_id					Job identifier

  Created:		Dec 2003

  Last revision information:
	$Revision: 5 $
	$Date: 17/08/11 15:40 $
	$Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Concept_ImportTaxonDesignationTypes]
	@job_id					INT
AS
	SET NOCOUNT ON
	SET ANSI_NULLS ON

	DECLARE     @taxon_list_key					CHAR(16),
				@taxon_designation_type_key		CHAR(16),
				@item_name						NVARCHAR(300),
				@ins_user_key					CHAR(16),
				@ins_date						DATETIME,
				@ins_session_id					CHAR(16),
				@upd_user_key					CHAR(16),
				@upd_date						DATETIME,
				@upd_session_id					CHAR(16),
				@system							BIT,
				@concept_designation_type_key	CHAR(16),
				@term_key						CHAR(16),
				@meaning_key					CHAR(16),
				@concept_history_key			CHAR(16)

	/* determine parameters of job */
	SELECT		@taxon_list_key							=	m.Taxon_List_Key
	FROM		Import_Export_Job						AS	j
	INNER JOIN	Taxon_Dictionary_Concept_Group_Mapping	AS	m
	ON			m.Concept_Group_Key						=	j.Concept_Group_Key
	WHERE		j.Import_Export_Job_ID					=	@job_id

	IF @@ROWCOUNT = 0
	BEGIN
		RAISERROR ('Job does not exist or has not been configured', 16, 1)
		RETURN
	END

	EXECUTE		usp_Import_Export_Job_UpdateStatus	@job_id,
													'Importing designation types'
	IF @@ERROR <> 0 RETURN

	DECLARE		types	CURSOR LOCAL FAST_FORWARD FOR
	SELECT DISTINCT
				tdt.TAXON_DESIGNATION_TYPE_KEY,
				ISNULL(tdt.LONG_NAME,
					   tdt.SHORT_NAME),
				tdt.ENTERED_BY,
				tdt.ENTRY_DATE,
				tdt.CHANGED_BY,
				tdt.CHANGED_DATE,
				tdt.SYSTEM_SUPPLIED_DATA
	FROM		TAXON_LIST_VERSION				AS	tlv
	INNER JOIN	TAXON_LIST_ITEM					AS	tli
	ON			tli.TAXON_LIST_VERSION_KEY		=	tlv.TAXON_LIST_VERSION_KEY
	INNER JOIN	TAXON_DESIGNATION				AS	td
	ON			td.TAXON_LIST_ITEM_KEY			=	tli.TAXON_LIST_ITEM_KEY
	INNER JOIN	TAXON_DESIGNATION_TYPE			AS	tdt
	ON			tdt.TAXON_DESIGNATION_TYPE_KEY	=	td.TAXON_DESIGNATION_TYPE_KEY
	WHERE		tlv.TAXON_LIST_KEY				=	@taxon_list_key

	OPEN		types

	WHILE 1 = 1
	BEGIN
		FETCH		types
		INTO		@taxon_designation_type_key,
					@item_name,
					@ins_user_key,
					@ins_date,
					@upd_user_key,
					@upd_date,
					@system

		IF @@FETCH_STATUS <> 0 BREAK

		BEGIN TRANSACTION

		/* obtain session identifiers */
		EXECUTE		usp_Session_ForDate		@ins_user_key,
											@ins_date,
											@ins_session_id		OUTPUT
		IF @@ERROR <> 0 GOTO fail_from_cursor

		IF @upd_user_key IS NULL OR @upd_date IS NULL
		BEGIN
			SET			@upd_session_id		=	NULL
		END
		ELSE
		BEGIN
			EXECUTE		usp_Session_ForDate		@upd_user_key,
												@upd_date,
												@upd_session_id		OUTPUT
			IF @@ERROR <> 0 GOTO fail_from_cursor
		END

		SELECT		@concept_designation_type_key				=	tdm.Concept_Designation_Type_Key,
					@term_key									=	c.Term_Key
		FROM		Taxon_Dictionary_Designation_Type_Mapping	AS	tdm
		INNER JOIN	Concept										AS	c
		ON			c.Concept_Key								=	tdm.Concept_Designation_Type_Key
		WHERE		tdm.Taxon_Designation_Type_Key				=	@taxon_designation_type_key

		IF @@ROWCOUNT > 0
		BEGIN
			/* designation type has previously been imported */
			IF NOT EXISTS (	SELECT		1
							FROM		Term
							WHERE		Term_Key		=	@term_key
							AND			Language_Key	=	'en'
							AND			Plaintext		=	dbo.ufn_RemoveHtmlMarkup(@item_name) )
			BEGIN
				/* term has changed */
				IF EXISTS (	SELECT		1
							FROM		Concept
							WHERE		Term_Key			=	@term_key
							AND			Concept_Group_Key	<>	'SYSTEM000000000T' )
				BEGIN
					/* term is linked outside this concept group; create
					 * a new term instead of updating the existing one */
					EXECUTE		spNextKey	'Term',
											@term_key	OUTPUT
					IF @@ERROR <> 0 GOTO fail_from_cursor

					INSERT		Term (
								Term_Key,
								Language_Key,
								Plaintext,
								Entered_Session_ID,
								Changed_Session_ID,
								System_Supplied_Data)
					VALUES		(@term_key,
								'en',
								@item_name,
								@ins_session_id,
								@upd_session_id,
								@system)

					IF @@ERROR <> 0 GOTO fail_from_cursor
				END
				ELSE
				BEGIN
					/* term only linked within this concept group */
					DECLARE		@cur_term_key		CHAR(16)

					SELECT		@cur_term_key	=	Term_Key
					FROM		Term
					WHERE		Language_Key	=	'en'
					AND			Plaintext		=	dbo.ufn_RemoveHtmlMarkup(@item_name)

					IF @@ROWCOUNT = 0
					BEGIN
						/* term can simply be updated */
						UPDATE		Term
						SET			Language_Key	=	'en',
									Plaintext		=	dbo.ufn_RemoveHtmlMarkup(@item_name)
						WHERE		Term_Key		=	@term_key

						IF @@ERROR <> 0 GOTO fail_from_cursor
					END
					ELSE
					BEGIN
						/* term cannot be updated; there is an existing
						 * term with the same name which we will link to
						 * instead */
						DELETE		Term
						WHERE		Term_Key			=	@term_key

						IF @@ERROR <> 0 GOTO fail_from_cursor

						SET			@term_key			=	@cur_term_key
					END
				END
			END

			UPDATE		Concept
			SET			Term_Key				=	@term_key,
						Entered_Session_ID		=	@ins_session_id,
						Changed_Session_ID		=	@upd_session_id,
						System_Supplied_Data	=	@system
			WHERE		Concept_Key				=	@concept_designation_type_key

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END
		ELSE
		BEGIN
			/* find/create term */
			SELECT		@term_key		=	Term_Key
			FROM		Term
			WHERE		Language_Key	=	'en'
			AND			Plaintext		=	dbo.ufn_RemoveHtmlMarkup(@item_name)

			IF @@ROWCOUNT = 0
			BEGIN
				EXECUTE		spNextKey	'Term',
										@term_key	OUTPUT
				IF @@ERROR <> 0 GOTO fail_from_cursor

				INSERT		Term (
							Term_Key,
							Language_Key,
							Plaintext,
							Entered_Session_ID,
							Changed_Session_ID,
							System_Supplied_Data)
				VALUES		(@term_key,
							'en',
							@item_name,
							@ins_session_id,
							@upd_session_id,
							@system)

				IF @@ERROR <> 0 GOTO fail_from_cursor
			END

			/* create Meaning */
			EXECUTE		spNextKey	'Meaning',
									@meaning_key	OUTPUT
			IF @@ERROR <> 0 GOTO fail_from_cursor

			INSERT		Meaning (
						Meaning_Key)
			VALUES		(@meaning_key)

			IF @@ERROR <> 0 GOTO fail_from_cursor

			/* create Concept */
			EXECUTE		spNextKey	'Concept',
									@concept_designation_type_key	OUTPUT
			IF @@ERROR <> 0 GOTO fail_from_cursor

			INSERT		Concept (
						Concept_Key,
						Term_Key,
						Concept_Group_Key,
						List_Preferred,
						Is_Current,
						Preferred,
						Name_Type_Concept_Key,
						Meaning_Key,
						Entered_Session_ID,
						Changed_Session_ID,
						System_Supplied_Data,
						Published_Term,
						Automatic_Published_Term)
			VALUES 		(@concept_designation_type_key,
						@term_key,
						'SYSTEM000000000T', /* "Concept Designation Types" group */
						1,
						1,
						1,
						'SYSTEM0000000000', /* "Formal" -- meaningless, but
												we need a value here */
						@meaning_key,
						@ins_session_id,
						@upd_session_id,
						@system,
						@item_name,
						1)

			IF @@ERROR <> 0 GOTO fail_from_cursor

			/* create concept history */
			EXECUTE		spNextKey	'Concept_History',
									@concept_history_key	OUTPUT
			IF @@ERROR <> 0 GOTO fail_from_cursor

			INSERT		Concept_History (
						Concept_History_Key,
						Concept_Key,
						Concept_Group_Version_From,
						Entered_Session_ID,
						Changed_Session_ID,
						System_Supplied_Data)
			VALUES		(@concept_history_key,
						@concept_designation_type_key,
						'SYSTEM000000000T', /* "Concept Designation Types" version */
						@ins_session_id,
						@upd_session_id,
						@system)

			IF @@ERROR <> 0 GOTO fail_from_cursor

			/* record taxon designation type mapping */
			INSERT		Taxon_Dictionary_Designation_Type_Mapping (
						Taxon_Designation_Type_Key,
						Concept_Designation_Type_Key)
			VALUES		(@taxon_designation_type_key,
						@concept_designation_type_key)

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END

		EXECUTE		usp_Import_Export_Job_RecordProcessed	@job_id
		IF @@ERROR <> 0 GOTO fail_from_cursor

		COMMIT TRANSACTION
	END

	CLOSE		types
	DEALLOCATE	types
	RETURN

fail_from_cursor:
	CLOSE		types
	DEALLOCATE	types

fail:
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_Concept_ImportTaxonDesignationTypes failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_ImportTaxonDesignationTypes') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Concept_ImportTaxonDesignationTypes'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concept_ImportTaxonDesignationTypes TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concept_ImportTaxonDesignationTypes TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Concept_ImportTaxonDesignationTypes TO [Dev - JNCC SQL]
END
GO
/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_Concept_ImportTaxonList]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_Concept_ImportTaxonList]
GO

/*===========================================================================*\
  Description:	Import concepts corresponding to the contents of a taxon list.

  Parameters:	@job_id					Job identifier

  Created:		Nov 2003

  Last revision information:
	$Revision: 5 $
	$Date: 17/08/11 15:40 $
	$Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Concept_ImportTaxonList]
	@job_id				INT
AS
	SET NOCOUNT ON
	SET ANSI_NULLS ON

	DECLARE     @taxon_list_key				CHAR(16),
				@concept_group_key			CHAR(16),
				@taxon_list_item_key		CHAR(16),
				@taxon_version_key			CHAR(16),
				@term_key					CHAR(16),
				@term_version_key			CHAR(16),
				@list_preferred				BIT,
				@is_current					BIT,
				@is_preferred				BIT,
				@taxon_rank_key				CHAR(16),
				@rank_uses_italics			BIT,
				@concept_rank_key			CHAR(16),
				@name_type_concept_key		CHAR(16),
				@sort_code					INT,
				@ins_user_key				CHAR(16),
				@ins_date					SMALLDATETIME,
				@ins_session_id				CHAR(16),
				@upd_user_key				CHAR(16),
				@upd_date					SMALLDATETIME,
				@upd_session_id				CHAR(16),
				@system						BIT,
				@preferred_name				CHAR(16),
				@taxon_list_version_from	CHAR(16),
				@taxon_list_version_to		CHAR(16),	
				@concept_group_version_from	CHAR(16),
				@concept_group_version_to	CHAR(16),
				@meaning_key				CHAR(16),
				@concept_key				CHAR(16),
				@domain_key					CHAR(16)

	/* determine parameters of job */
	SELECT		@taxon_list_key							=	m.Taxon_List_Key,
				@concept_group_key						=	m.Concept_Group_Key
	FROM		Import_Export_Job						AS	j
	INNER JOIN	Taxon_Dictionary_Concept_Group_Mapping	AS	m
	ON			m.Concept_Group_Key						=	j.Concept_Group_Key
	WHERE		j.Import_Export_Job_ID					=	@job_id

	SELECT		@domain_key								=	LD.Domain_Key
	FROM		Concept_Group CG
	INNER JOIN	Local_Domain LD 
	ON LD.Local_Domain_Key								=	CG.Local_Domain_Key
	WHERE		CG.Concept_Group_Key					=	@concept_group_key

	IF @@ROWCOUNT = 0
	BEGIN
		RAISERROR ('Job does not exist or has not been configured', 16, 1)
		RETURN
	END

	EXECUTE		usp_Import_Export_Job_UpdateStatus	@job_id,
													'Importing concepts'
	IF @@ERROR <> 0 RETURN

	/* remove current lineage data */
	DELETE		l
	FROM		Concept					AS	c
	INNER JOIN	Concept_Lineage			AS	l
	ON			l.Concept_Key			=	c.Concept_Key
	WHERE		c.Concept_Group_Key		=	@concept_group_key

	IF @@ERROR <> 0 RETURN

	DECLARE		@items	TABLE (
				Taxon_List_Item_Key	CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS,
				Rank_Uses_Italic	BIT)

	INSERT		@items
	SELECT      tli.TAXON_LIST_ITEM_KEY,
				tr.LIST_FONT_ITALIC
	FROM        TAXON_LIST_VERSION				AS	tlv
	INNER JOIN	TAXON_LIST_ITEM					AS	tli
	ON			tli.TAXON_LIST_VERSION_KEY		=	tlv.TAXON_LIST_VERSION_KEY
	INNER JOIN	TAXON_RANK						AS	tr
	ON			tr.TAXON_RANK_KEY				=	tli.TAXON_RANK_KEY
	WHERE		tlv.TAXON_LIST_KEY				=	@taxon_list_key

	DECLARE		items		CURSOR FAST_FORWARD LOCAL FOR
	SELECT		tli.TAXON_LIST_ITEM_KEY,
				tli.TAXON_VERSION_KEY,
				CASE WHEN tli.TAXON_LIST_ITEM_KEY = tli.PREFERRED_NAME
					THEN 1	/* list preferred */
					ELSE 0
				END,
				CASE WHEN tli.TAXON_LIST_VERSION_TO IS NULL
					THEN 1	/* current */
					ELSE 0
				END,
				tli.TAXON_RANK_KEY,
				itm.Rank_Uses_Italic,
				tli.SORT_CODE,
				tli.ENTERED_BY,
				tli.ENTRY_DATE,
				tli.CHANGED_BY,
				tli.CHANGED_DATE,
				tli.SYSTEM_SUPPLIED_DATA,
				tli.PREFERRED_NAME,
				tli.TAXON_LIST_VERSION_KEY,
				tli.TAXON_LIST_VERSION_TO
	FROM		@items							AS	itm
	INNER JOIN	TAXON_LIST_ITEM					AS	tli
	ON			tli.TAXON_LIST_ITEM_KEY			=	itm.TAXON_LIST_ITEM_KEY

	OPEN        items

	WHILE 1 = 1
	BEGIN
		FETCH		items
		INTO		@taxon_list_item_key,
					@taxon_version_key,
					@list_preferred,
					@is_current,
					@taxon_rank_key,
					@rank_uses_italics,
					@sort_code,
					@ins_user_key,
					@ins_date,
					@upd_user_key,
					@upd_date,
					@system,
					@preferred_name,
					@taxon_list_version_from,
					@taxon_list_version_to

		IF @@FETCH_STATUS <> 0 BREAK

		BEGIN TRANSACTION

		/* perform mappings */
		SELECT		@term_version_key						=	Term_Version_Key
		FROM		Taxon_Dictionary_Term_Version_Mapping
		WHERE		Taxon_Version_Key						=	@taxon_version_key

		IF @@ROWCOUNT = 0 GOTO skip_item

		SELECT		@term_key								=	tm.Term_Key,
					@name_type_concept_key					=	ntm.Thesaurus_Name_Type_Key
		FROM		TAXON_VERSION							AS	tv
		INNER JOIN	TAXON									AS	tx
		ON			tx.TAXON_KEY							=	tv.TAXON_KEY
		INNER JOIN	Taxon_Dictionary_Term_Mapping			AS	tm
		ON			tm.Taxon_Key							=	tx.TAXON_KEY
		AND			tm.Italic_Font							=	CASE WHEN tx.Language = 'La'
																	 AND @rank_uses_italics = 1
																	THEN 1
																	ELSE 0
																END
		INNER JOIN	Taxon_Dictionary_Name_Type_Mapping		AS	ntm
		ON			ntm.Taxon_Name_Type_Key					=	tx.TAXON_NAME_TYPE_KEY
		WHERE		tv.TAXON_VERSION_KEY					=	@taxon_version_key

		IF @@ROWCOUNT = 0 GOTO skip_item

		SELECT		@concept_rank_key						=	M.Concept_Rank_Key
		FROM		Taxon_Dictionary_Concept_Rank_Mapping M
		INNER JOIN	Concept_Rank CR 
					ON CR.Concept_Rank_Key					=	M.Concept_Rank_Key
					AND	CR.Domain_Key						=	@domain_key
		WHERE		M.Taxon_Rank_Key						=	@taxon_rank_key

		IF @@ROWCOUNT = 0 GOTO skip_item

		IF @list_preferred = 1
			SET			@is_preferred		=	1
		ELSE
		BEGIN
			SELECT		@is_preferred 		=	CASE WHEN TAXON_VERSION_KEY = @taxon_version_key
													THEN 1
													ELSE 0
												END
			FROM		TAXON_COMMON_NAME
			WHERE		TAXON_LIST_ITEM_KEY	=	@taxon_list_item_key
		END

		SELECT      @concept_group_version_from						=	Concept_Group_Version_Key
		FROM		Taxon_Dictionary_Concept_Group_Version_Mapping
		WHERE		Taxon_List_Version_Key							=   @taxon_list_version_from

		IF @@ROWCOUNT = 0 GOTO skip_item

		SELECT		@concept_group_version_to						=	Concept_Group_Version_Key
		FROM		Taxon_Dictionary_Concept_Group_Version_Mapping
		WHERE		Taxon_List_Version_Key							=	@taxon_list_version_to

		IF @@ROWCOUNT = 0
		BEGIN
			SET			@concept_group_version_to	=	NULL
		END

		/* obtain meaning key */
		SELECT		@meaning_key						=	Meaning_Key
		FROM        Taxon_Dictionary_Meaning_Mapping
		WHERE		Preferred_Name						=	@preferred_name

		IF @@ROWCOUNT = 0
		BEGIN
			/* look for meaning assigned to synonyms of @preferred_name from
			 * some other taxon list */
			SELECT		@meaning_key						=	tdm.Meaning_Key
			FROM		INDEX_TAXON_SYNONYM					AS	its
			INNER JOIN	Taxon_Dictionary_Meaning_Mapping	AS	tdm
			ON			tdm.Preferred_Name					=	its.SYNONYM_LIST_ITEM_KEY
			WHERE		its.TAXON_LIST_ITEM_KEY				=	@preferred_name

			IF @@ROWCOUNT = 0
			BEGIN
				/* create new meaning */
				EXECUTE		spNextKey	'Meaning',
										@meaning_key	OUTPUT
				IF @@ERROR <> 0 GOTO fail_from_cursor

				INSERT		Meaning (
							Meaning_Key)
				VALUES		(@meaning_key)

				IF @@ERROR <> 0 GOTO fail_from_cursor
			END

			INSERT		Taxon_Dictionary_Meaning_Mapping (
						Preferred_Name,
						Meaning_Key)
			VALUES		(@preferred_name,
						@meaning_key)

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END

		IF @meaning_key IS NOT NULL
				/* meaning not explicitly mapped to null,
				 * so we can import item */
		BEGIN
			/* obtain session identifiers */
			EXECUTE		usp_Session_ForDate		@ins_user_key,
												@ins_date,
												@ins_session_id		OUTPUT
			IF @@ERROR <> 0 GOTO fail_from_cursor

			IF @upd_user_key IS NULL OR @upd_date IS NULL
			BEGIN
				SET			@upd_session_id		=	NULL
			END
			ELSE
			BEGIN
				EXECUTE		usp_Session_ForDate		@upd_user_key,
													@upd_date,
													@upd_session_id		OUTPUT
				IF @@ERROR <> 0 GOTO fail_from_cursor
			END

			SELECT      @concept_key						=	Concept_Key
			FROM		Taxon_Dictionary_Concept_Mapping
			WHERE		Taxon_List_Item_Key					=	@taxon_list_item_key

			IF @@ROWCOUNT > 0
			BEGIN
				DECLARE		@old_group_key			CHAR(16),
							@was_list_preferred		BIT
							
				/* update concept */
				UPDATE		Concept
				SET         @old_group_key				=	Concept_Group_Key,
							@was_list_preferred			=	List_Preferred,
							Term_Key					=	@term_key,
							Concept_Group_Key			=	@concept_group_key,
							Term_Version_Key			=	@term_version_key,
							List_Preferred				=	@list_preferred,
							Is_Current					=	@is_current,
							Preferred					=	@is_preferred,
							Concept_Rank_Key			=	@concept_rank_key,
							Name_Type_Concept_Key		=   @name_type_concept_key,
							Meaning_Key					=	@meaning_key,
							Sort_Code					=	@sort_code,
							Entered_Session_ID			=	@ins_session_id,
							Changed_Session_ID			=	@upd_session_id,
							System_Supplied_Data		=	@system
				WHERE		Concept_Key					=	@concept_key

				IF @@ERROR <> 0 GOTO fail_from_cursor

				/* re-create concept history */
				DELETE		Concept_History
				WHERE		Concept_Key					=	@concept_key

				IF @@ERROR <> 0 GOTO fail_from_cursor

				EXECUTE		usp_ConceptHistory_Insert_Imported	@concept_key,
																@concept_group_version_from,
																@concept_group_version_to
				IF @@ERROR <> 0 GOTO fail_from_cursor
			END
			ELSE
			BEGIN
				/* create concept */
				EXECUTE		spNextKey	'Concept',
										@concept_key	OUTPUT
				IF @@ERROR <> 0 GOTO fail_from_cursor

				INSERT		Concept
							(Concept_Key,
							Term_Key,
							Concept_Group_Key,
							Term_Version_Key,
							List_Preferred,
							Is_Current,
							Preferred,
							Concept_Rank_Key,
							Name_Type_Concept_Key,
							Meaning_Key,
							Sort_Code,
							Entered_Session_ID,
							Changed_Session_ID,
							System_Supplied_Data,
							Published_Term,
							Automatic_Published_Term)
				VALUES		(@concept_key,
							@term_key,
							@concept_group_key,
							@term_version_key,
							@list_preferred,
							@is_current,
							@is_preferred,
							@concept_rank_key,
							@name_type_concept_key,
							@meaning_key,
							@sort_code,
							@ins_session_id,
							@upd_session_id,
							@system,
							@preferred_name,
							1)

				IF @@ERROR <> 0 GOTO fail_from_cursor

				/* create concept history */
				EXECUTE		usp_ConceptHistory_Insert_Imported	@concept_key,
																@concept_group_version_from,
																@concept_group_version_to
				IF @@ERROR <> 0 GOTO fail_from_cursor

				/* record mapping */
				INSERT		Taxon_Dictionary_Concept_Mapping
							(Taxon_List_Item_Key,
							Concept_Key)
				VALUES		(@taxon_list_item_key,
							@concept_key)

				IF @@ERROR <> 0 GOTO fail_from_cursor
			END
		END
		
skip_item:
		EXECUTE		usp_Import_Export_Job_RecordProcessed	@job_id
		IF @@ERROR <> 0 GOTO fail_from_cursor

		COMMIT TRANSACTION
	END

	CLOSE		items
	DEALLOCATE	items
	RETURN

fail_from_cursor:
	CLOSE		items
	DEALLOCATE	items

fail:
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_Concept_ImportTaxonList failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_ImportTaxonList') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Concept_ImportTaxonList'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concept_ImportTaxonList TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concept_ImportTaxonList TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Concept_ImportTaxonList TO [Dev - JNCC SQL]
END
GO
/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_Concept_ImportTaxonNameTypes]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_Concept_ImportTaxonNameTypes]
GO

/*===========================================================================*\
  Description:	Import concepts corresponding to the taxon name types used in
				the specified taxon list.

  Parameters:   @job_id					Job identifier

  Created:		Nov 2003

  Last revision information:
	$Revision: 5 $
	$Date: 17/08/11 15:40 $
	$Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Concept_ImportTaxonNameTypes]
	@job_id					INT
AS
	SET NOCOUNT ON
	SET ANSI_NULLS ON

	DECLARE		@taxon_list_key				CHAR(16),
				@taxon_name_type_key		CHAR(16),
				@item_name					VARCHAR(100),
				@author_and_date			VARCHAR(100),
				@ins_user_key				CHAR(16),
				@ins_date					SMALLDATETIME,
				@ins_session_id				CHAR(16),
				@system						BIT,
				@thesaurus_name_type_key	CHAR(16),
				@system_mapping				BIT,
				@term_key					CHAR(16),
				@term_version_key			CHAR(16),
				@meaning_key				CHAR(16),
				@concept_history_key		CHAR(16)

	/* determine parameters of job */
	SELECT		@taxon_list_key							=	m.Taxon_List_Key
	FROM		Import_Export_Job						AS	j
	INNER JOIN	Taxon_Dictionary_Concept_Group_Mapping	AS	m
	ON			m.Concept_Group_Key						=	j.Concept_Group_Key
	WHERE		j.Import_Export_Job_ID					=	@job_id

	IF @@ROWCOUNT = 0
	BEGIN
		RAISERROR ('Job does not exist or has not been configured', 16, 1)
		RETURN
	END

	EXECUTE		usp_Import_Export_Job_UpdateStatus	@job_id,
													'Importing name types'
	IF @@ERROR <> 0 RETURN

	DECLARE     @versions   TABLE ( Taxon_Version_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS)

	INSERT      @versions
	SELECT      tli.TAXON_VERSION_KEY
	FROM		TAXON_LIST_VERSION				AS	tlv
	INNER JOIN	TAXON_LIST_ITEM					AS	tli
	ON			tli.TAXON_LIST_VERSION_KEY		=	tlv.TAXON_LIST_VERSION_KEY
	WHERE		tlv.TAXON_LIST_KEY				=	@taxon_list_key

	DECLARE		name_types	CURSOR LOCAL FAST_FORWARD FOR
	SELECT DISTINCT
				tnt.TAXON_NAME_TYPE_KEY,
				tnt.SHORT_NAME,
				tnt.AUTHORITY,
				tnt.ENTERED_BY,
				tnt.ENTRY_DATE,
				tnt.SYSTEM_SUPPLIED_DATA
	FROM        @versions                       AS  v0
	INNER JOIN	TAXON_VERSION					AS	tv
	ON			tv.TAXON_VERSION_KEY			=	v0.TAXON_VERSION_KEY
	INNER JOIN	TAXON							AS	tx
	ON			tx.TAXON_KEY					=	tv.TAXON_KEY
	INNER JOIN	TAXON_NAME_TYPE					AS	tnt
	ON			tnt.TAXON_NAME_TYPE_KEY			=	tx.TAXON_NAME_TYPE_KEY

	OPEN        name_types

	WHILE 1 = 1
	BEGIN
		FETCH		name_types
		INTO		@taxon_name_type_key,
					@item_name,
					@author_and_date,
					@ins_user_key,
					@ins_date,
					@system
					
		IF @@FETCH_STATUS <> 0 BREAK

		BEGIN TRANSACTION

		SELECT		@thesaurus_name_type_key			=   Thesaurus_Name_Type_Key,
					@system_mapping						=	System_Supplied_Data
		FROM		Taxon_Dictionary_Name_Type_Mapping
		WHERE       Taxon_Name_Type_Key					=	@taxon_name_type_key

		IF @@ROWCOUNT > 0
		BEGIN
			/* name type has previously been imported */
			IF @system_mapping = 0
			BEGIN
				SELECT		@term_key			=	Term_Key,
							@term_version_key	=	Term_Version_Key
				FROM		Concept
				WHERE		Concept_Key			=	@thesaurus_name_type_key

				IF NOT EXISTS (	SELECT		1
								FROM		Term
								WHERE		Term_Key		=	@term_key
								AND			Language_Key	=	'en'
								AND			Plaintext		=	dbo.ufn_RemoveHtmlMarkup(@item_name) )
				BEGIN
					/* term has changed */
					IF EXISTS (	SELECT		1
								FROM		Concept
								WHERE		Term_Key			=	@term_key
								AND			Concept_Group_Key	<>	'SYSTEM000000000M' )
					BEGIN
						/* term is linked outside this concept group; create
						 * a new term instead of updating the existing one */
						EXECUTE		spNextKey	'Term',
												@term_key	OUTPUT
						IF @@ERROR <> 0 GOTO fail_from_cursor

						INSERT		Term (
									Term_Key,
									Language_Key,
									Plaintext,
									Entered_Session_ID,
									System_Supplied_Data)
						VALUES		(@term_key,
									'en',
									@item_name,
									@ins_session_id,
									@system)

						IF @@ERROR <> 0 GOTO fail_from_cursor

						EXECUTE		spNextKey		'Term_Version',
													@term_version_key	OUTPUT
						IF @@ERROR <> 0 GOTO fail_from_cursor

						INSERT		Term_Version (
									Term_Version_Key,
									Term_Key,
									Author_And_Date,
									Entered_Session_ID,
									System_Supplied_Data)
						VALUES		(@term_version_key,
									@term_key,
									@author_and_date,
									@ins_session_id,
									@system)

						IF @@ERROR <> 0 GOTO fail_from_cursor									

						UPDATE		Concept
						SET			Term_Key		=	@term_key
						WHERE		Concept_Key		=	@thesaurus_name_type_key

						IF @@ERROR <> 0 GOTO fail_from_cursor
					END
					ELSE
					BEGIN
						/* term only linked within this concept group */
						DECLARE		@cur_term_key		CHAR(16)

						SELECT		@cur_term_key	=	Term_Key
						FROM		Term
						WHERE		Language_Key	=	'en'
						AND			Plaintext		=	dbo.ufn_RemoveHtmlMarkup(@item_name)

						IF @@ROWCOUNT = 0
						BEGIN
							/* term can simply be updated */
							UPDATE		Term
							SET			Language_Key	=	'en',
										Plaintext		=	dbo.ufn_RemoveHtmlMarkup(@item_name)
							WHERE		Term_Key		=	@term_key

							IF @@ERROR <> 0 GOTO fail_from_cursor

							UPDATE		Term_Version
							SET			Author_And_Date		=	@author_and_date
							WHERE		Term_Version_Key	=	@term_version_key

							IF @@ERROR <> 0 GOTO fail_from_cursor
						END
						ELSE
						BEGIN
							/* term cannot be updated; there is an existing
							 * term with the same name which we will link to
							 * instead */
							EXECUTE		spNextKey	'Term_Version',
													@term_version_key	OUTPUT
							IF @@ERROR <> 0 GOTO fail_from_cursor

							INSERT		Term_Version (
										Term_Version_Key,
										Term_Key,
										Author_And_Date,
										Entered_Session_ID,
										System_Supplied_Data)
							VALUES		(@term_version_key,
										@cur_term_key,
										@author_and_date,
										@ins_session_id,
										@system)

							IF @@error <> 0 GOTO fail_from_cursor

							UPDATE		Concept
							SET			Term_Key			=	@cur_term_key,
										Term_Version_Key	=	@term_version_key
							WHERE		Term_Key			=	@term_key

							IF @@ERROR <> 0 GOTO fail_from_cursor

							DELETE		Term
							WHERE		Term_Key			=	@term_key

							IF @@ERROR <> 0 GOTO fail_from_cursor
						END
					END
				END
			END
		END
		ELSE
		BEGIN
			/* obtain session identifier */
			EXECUTE		usp_Session_ForDate		@ins_user_key,
												@ins_date,
												@ins_session_id		OUTPUT
			IF @@ERROR <> 0 GOTO fail_from_cursor

			/* find/create term */
			SELECT		@term_key		=	Term_Key
			FROM		Term
			WHERE		Language_Key	=	'en'
			AND			Plaintext		=	dbo.ufn_RemoveHtmlMarkup(@item_name)

			IF @@ROWCOUNT = 0
			BEGIN
				EXECUTE		spNextKey	'Term',
										@term_key	OUTPUT
				IF @@ERROR <> 0 GOTO fail_from_cursor

				INSERT		Term (
							Term_Key,
							Language_Key,
							Plaintext,
							Entered_Session_ID,
							System_Supplied_Data)
				VALUES		(@term_key,
							'en',
							@item_name,
							@ins_session_id,
							@system)

				IF @@ERROR <> 0 GOTO fail_from_cursor
			END

			EXECUTE		spNextKey	'Term_Version',
									@term_version_key	OUTPUT
			IF @@ERROR <> 0 GOTO fail_from_cursor

			INSERT		Term_Version (
						Term_Version_Key,
						Term_Key,
						Author_And_Date,
						Entered_Session_ID,
						System_Supplied_Data)
			VALUES		(@term_version_key,
						@term_key,
						@author_and_date,
						@ins_session_id,
						@system)

			IF @@ERROR <> 0 GOTO fail_from_cursor

			/* create Meaning */
			EXECUTE		spNextKey	'Meaning',
									@meaning_key	OUTPUT
			IF @@ERROR <> 0 GOTO fail_from_cursor

			INSERT		Meaning (
						Meaning_Key)
			VALUES		(@meaning_key)

			IF @@ERROR <> 0 GOTO fail_from_cursor

			/* create Concept */
			EXECUTE		spNextKey	'Concept',
									@thesaurus_name_type_key	OUTPUT
			IF @@ERROR <> 0 GOTO fail_from_cursor

			INSERT		Concept (
						Concept_Key,
						Term_Key,
						Concept_Group_Key,
						Term_Version_Key,
						List_Preferred,
						Is_Current,
						Preferred,
						Name_Type_Concept_Key,
						Meaning_Key,
						Entered_Session_ID,
						System_Supplied_Data,
						Published_Term,
						Automatic_Published_Term)
			VALUES 		(@thesaurus_name_type_key,
						@term_key,
						'SYSTEM000000000M', /* "Thesaurus Name Types" group */
						@term_version_key,
						1,
						1,
						1,
						'SYSTEM0000000000', /* "Formal" -- meaningless, but
												we need a value here */
						@meaning_key,
						@ins_session_id,
						@system,
						@item_name,
						1)

			IF @@ERROR <> 0 GOTO fail_from_cursor

			/* create concept history */
			EXECUTE		spNextKey	'Concept_History',
									@concept_history_key	OUTPUT
			IF @@ERROR <> 0 GOTO fail_from_cursor

			INSERT		Concept_History (
						Concept_History_Key,
						Concept_Key,
						Concept_Group_Version_From,
						Entered_Session_ID,
						System_Supplied_Data)
			VALUES		(@concept_history_key,
						@thesaurus_name_type_key,
						'SYSTEM000000000M', /* "Thesaurus Name Types" version */
						@ins_session_id,
						@system)

			IF @@ERROR <> 0 GOTO fail_from_cursor

			/* record taxon name type mapping */
			INSERT		Taxon_Dictionary_Name_Type_Mapping (
						Taxon_Name_Type_Key,
						Thesaurus_Name_Type_Key,
						System_Supplied_Data)
			VALUES		(@taxon_name_type_key,
						@thesaurus_name_type_key,
						0)

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END

		EXECUTE		usp_Import_Export_Job_RecordProcessed	@job_id
		IF @@ERROR <> 0 GOTO fail_from_cursor

		COMMIT TRANSACTION
	END

	CLOSE		name_types
	DEALLOCATE	name_types
	RETURN

fail_from_cursor:
	CLOSE		name_types
	DEALLOCATE	name_types

fail:
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_Concept_ImportTaxonNameTypes failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_ImportTaxonNameTypes') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Concept_ImportTaxonNameTypes'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concept_ImportTaxonNameTypes TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concept_ImportTaxonNameTypes TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Concept_ImportTaxonNameTypes TO [Dev - JNCC SQL]
END
GO
/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Concept_Insert]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Concept_Insert]
GO

/*===========================================================================*\
  Description:	Inserts a new concept and term if necessary.

  Parameters:	@Key	
		@ConceptGroupKey
		@TermName
		@PlainText 
		@LanguageKey 
		@SessionID 
		@NameTypeConceptKey 
		@IsSystem bit
		@PublishedTerm
		@AutomaticPublishedTerm
		@TermVersionKey

  Created:	August 2003

  Last revision information:
    $Revision: 5 $
    $Date: 17/08/11 15:40 $
    $Author: Jamesbichard $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_Concept_Insert]
	@Key char(16) OUTPUT,
	@ConceptGroupKey char(16),
	@TermName nvarchar(100),
	@PlainText nvarchar(100) = NULL,
	@LanguageKey varchar(4) = NULL,
	@SessionID char(16),
	@NameTypeConceptKey char(16) = NULL,
	@IsSystem bit = NULL,
	@PublishedTerm NVARCHAR(450) = NULL,
	@AutomaticPublishedTerm BIT = 1,
	@TermVersionKey CHAR(16) = NULL
AS
	SET NOCOUNT ON
	SET ANSI_NULLS ON
	SET ANSI_PADDING ON
	SET ANSI_WARNINGS ON
	SET CONCAT_NULL_YIELDS_NULL ON
	SET QUOTED_IDENTIFIER ON
	SET NO_BROWSETABLE OFF
	SET XACT_ABORT ON

	BEGIN TRANSACTION

	DECLARE @NewTermKey char(16), 
		@NewConceptHistoryKey char(16),
		@SiteID char(8),
		@ConceptGroupVersionKey char(16),
		@SystemSuppliedData bit,
		@NewMeaningKey char(16)

	IF @IsSystem = 1
	BEGIN
		SET @SiteID = 'SYSTEM00'
		SET @SystemSuppliedData = 1
	END
	ELSE
	BEGIN
		SELECT @SiteID = Data FROM Setting WHERE [Name] = 'SiteID'
		SET @SystemSuppliedData = 0
	END

	IF NOT EXISTS(SELECT * FROM Concept_Group WHERE Concept_Group_Key = @ConceptGroupKey)
	BEGIN
		RAISERROR('Invalid CONCEPT_GROUP_KEY, Concept Group does not exist', 16, 1)
		GOTO RollbackAndExit
	END
	ELSE
	BEGIN
		-- if no NameTypeConcept Key, use default one.
		IF @NameTypeConceptKey IS NULL SET @NameTypeConceptKey = 'SYSTEM00000000AN'

		-- if plaintext is null, set it equal to TermName.
		IF @PlainText IS NULL SET @PlainText = @TermName
		
		IF @PublishedTerm IS NULL SET @PublishedTerm = @TermName
		
		-- If Language not specified, get the one with Priority of 1.
		IF @LanguageKey IS NULL
			SELECT	@LanguageKey = Language_Key
			FROM	Language
			WHERE	Priority = 1

		/*-------------------------------------------------------------*\
		  Find out if new Term is required and create if needed.
		\*-------------------------------------------------------------*/
		SELECT 	@NewTermKey = Term_Key 
		FROM 	Term 
		WHERE 	Plaintext = dbo.ufn_RemoveHtmlMarkup(@TermName)
		AND 	Language_Key = @LanguageKey

		IF @NewTermKey IS NULL
		BEGIN
			EXEC spNextKey 'Term', @NewTermKey OUTPUT, @SiteID
			IF @@Error <> 0 GOTO RollbackAndExit
			INSERT INTO Term (
				Term_Key, Language_Key, Plaintext, Entered_Session_ID, 
				System_Supplied_Data
			) VALUES (
				@NewTermKey, @LanguageKey, @PlainText, @SessionID, @SystemSuppliedData
			)
			IF @@Error <> 0 GOTO RollbackAndExit
		END
		
		/*-------------------------------------------------------------*\
		  Create a Term_Version if an existing key wasn't supplied...
		\*-------------------------------------------------------------*/
		IF @TermVersionKey IS NULL
		BEGIN
			EXECUTE spNextKey 'Term_Version', @TermVersionKey OUTPUT
			
			IF @@Error <> 0 GOTO RollbackAndExit
			
			INSERT INTO	dbo.Term_Version
						(
							Term_Version_Key,
							Term_Key,
							Version_Label,
							Author_And_Date,
							Entered_Session_ID,
							Changed_Session_ID,
							Custodian
						)
			VALUES		(
							@TermVersionKey,
							@NewTermKey,
							NULL,
							NULL,
							@SessionID,
							NULL,
							NULL
						)
		END
		
		/*-------------------------------------------------------------*\
		  Create new Concept and Meaning.
		\*-------------------------------------------------------------*/
		EXEC spNextKey 'Concept', @Key OUTPUT, @SiteID
		IF @@Error <> 0 GOTO RollbackAndExit

		EXEC spNextKey 'Meaning', @NewMeaningKey OUTPUT, @SiteID
		IF @@Error <> 0 GOTO RollbackAndExit

		INSERT INTO [dbo].[Meaning] (Meaning_Key) VALUES (@NewMeaningKey)
		IF @@Error <> 0 GOTO RollbackAndExit

		-- Now insert the Concept.
		INSERT INTO Concept (
			Concept_Key, Term_Key, Term_Version_Key, Concept_Group_Key, List_Preferred, Preferred, Is_Current, 
			Name_Type_Concept_Key, Meaning_Key, Entered_Session_ID, System_Supplied_Data,
			Published_Term, Automatic_Published_Term
		) VALUES (
			@Key, @NewTermKey, @TermVersionKey, @ConceptGroupKey, 1, 1, 1, 
			@NameTypeConceptKey, @NewMeaningKey, @SessionID, @SystemSuppliedData,
			@PublishedTerm, @AutomaticPublishedTerm
		)
		IF @@Error <> 0 GOTO RollbackAndExit

		/*-------------------------------------------------------------*\
		  Create new Concept_History.
		\*-------------------------------------------------------------*/
		EXEC spNextKey 'Concept_History', @NewConceptHistoryKey OUTPUT, @SiteID
		IF @@Error <> 0 GOTO RollbackAndExit
		INSERT INTO Concept_History (
			Concept_History_Key, Concept_Key, Concept_Group_Version_From,
			Entered_Session_ID, System_Supplied_Data
		)
			SELECT TOP 1 	@NewConceptHistoryKey, @Key, Concept_Group_Version_Key ,
					@SessionID, @SystemSuppliedData
			FROM 		Concept_Group_Version
			WHERE 		Concept_Group_Key = @ConceptGroupKey
			ORDER BY 	From_Vague_Date_Start DESC
		IF @@Error <> 0 GOTO RollbackAndExit

		/*-------------------------------------------------------------*\
		  Create Concept_Lineage
		\*-------------------------------------------------------------*/
		EXECUTE		usp_ConceptLineage_NewConcept	@Key
		IF @@ERROR <> 0 GOTO RollbackAndExit
	END

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_Concept_Insert failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Concept_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Concept_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concept_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concept_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Concept_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Concept_Insert TO [Dev - JNCC SQL]
END
GO
/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = Object_Id(N'[dbo].[usp_Concept_InsertSpreadsheetRow]') AND Type = 'P')
	DROP PROCEDURE [dbo].[usp_Concept_InsertSpreadsheetRow]
GO

/*===========================================================================*\
  Description:	Insert a Concept record, plus related records as required,
				based on data extracted from a spreadsheet.

  Parameters:   @SessionID							Session key
				@concept_group_key					Concept group key
				@author								Name of author
				@child_of							Parent concept key
				@citation_date						Citation date
                @rank           	    		    Rank (abbreviation or name)
				@fact_#?_title						Name of fact 		(*5)
				@fact_#?_type						Name of fact type 	(*5)
				@fact_#?_description				Fact data 			(*5)
				@designation_#?_status				Status of concept designation	(*2)
				@designation_#?_start_date_start	Start date of designation		(*2)
				@designation_#?_start_date_end
				@designation_#?_start_date_type
				@designation_#?_end_date_start		End date of designation			(*2)
				@designation_#?_end_date_end
				@designation_#?_end_date_type
				@list_code							Concept list code
				@name_type							Name of name type
				@sort_code							Concept sort code
				@synonym_of							Synonym concept key
				@language							Name of term language
				@language_key						Term language key
				@term_name							Term name
				@concept_key						[on exit] New concept key

  Created:		Jan 2004

  Last revision information:
	$Revision: 5 $
	$Date: 17/08/11 15:40 $
	$Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Concept_InsertSpreadsheetRow]
	@SessionID							CHAR(16),
	@concept_group_key					CHAR(16),
	@author								VARCHAR(100),
	@child_of							CHAR(16),
	@citation_date						VARCHAR(100),
    @rank           				    VARCHAR(100),
	@fact_#1_title						VARCHAR(100),
	@fact_#1_type						NVARCHAR(150),
	@fact_#1_description				TEXT,
	@fact_#2_title						VARCHAR(100),
	@fact_#2_type						NVARCHAR(150),
	@fact_#2_description				TEXT,
	@fact_#3_title						VARCHAR(100),
	@fact_#3_type						NVARCHAR(150),
	@fact_#3_description				TEXT,
	@fact_#4_title						VARCHAR(100),
	@fact_#4_type						NVARCHAR(150),
	@fact_#4_description				TEXT,
	@fact_#5_title						VARCHAR(100),
	@fact_#5_type						NVARCHAR(150),
	@fact_#5_description				TEXT,
	@designation_#1_status				NVARCHAR(150),
	@designation_#1_start_date_start	INT,
	@designation_#1_start_date_end		INT,
	@designation_#1_start_date_type		VARCHAR(2),
	@designation_#1_end_date_start		INT,
	@designation_#1_end_date_end		INT,
	@designation_#1_end_date_type		VARCHAR(2),
	@designation_#2_status				NVARCHAR(150),
	@designation_#2_start_date_start	INT,
	@designation_#2_start_date_end		INT,
	@designation_#2_start_date_type		VARCHAR(2),
	@designation_#2_end_date_start		INT,
	@designation_#2_end_date_end		INT,
	@designation_#2_end_date_type		VARCHAR(2),
	@list_code							VARCHAR(50),
	@name_type							NVARCHAR(300),
	@sort_code							INT,
	@synonym_of							CHAR(16),
	@language							VARCHAR(50),
	@language_key						VARCHAR(4),
	@term_name							NVARCHAR(300),
	@concept_key						CHAR(16)	OUTPUT
AS
	SET NOCOUNT ON
	SET ARITHABORT ON

	/*--------------------------------------------------------------------------------*\ 
		Check parameters.
	\*--------------------------------------------------------------------------------*/
	IF @term_name IS NULL
	BEGIN
		RAISERROR ('Term name must be specified.', 16, 1)
		RETURN
	END

	IF NOT @language_key IS NULL
	BEGIN
		IF NOT EXISTS (	SELECT		1
						FROM		Language
						WHERE		Language_Key	=	@language_key )
		BEGIN
			RAISERROR ('Specified language does not exist.', 16, 1)
			RETURN
		END
	END
	ELSE
	BEGIN
		IF @language IS NULL
		BEGIN
			RAISERROR ('Language or Language Key must be specified.', 16, 1)
			RETURN
		END

		SELECT		@language_key	=	Language_Key
		FROM		Language
		WHERE		Item_Name		=	@language

		IF @@ROWCOUNT = 0
		BEGIN
			RAISERROR ('Specified language is not recognised.', 16, 1)
			RETURN
		END
	END

	DECLARE		@term_key			CHAR(16),
				@term_version_key	CHAR(16),
				@name_type_key		CHAR(16),
				@meaning_key		CHAR(16),
                @domain_key         CHAR(16),
                @rank_key           CHAR(16)

	BEGIN TRANSACTION

	/*--------------------------------------------------------------------------------*\ 
		Work out the term.
	\*--------------------------------------------------------------------------------*/
	SELECT		@term_key		=	Term_Key
	FROM		Term
	WHERE		Language_Key	=	@language_key
	AND			Plaintext		=	dbo.ufn_RemoveHtmlMarkup(LTRIM(RTRIM(@term_name)))

	IF @@ROWCOUNT = 0
	BEGIN
		EXECUTE		spNextKey	'Term',
								@term_key	OUTPUT
		IF @@ERROR <> 0 GOTO fail

		INSERT		Term (
					Term_Key,
					Language_Key,
					Plaintext,
					Entered_Session_ID)
		VALUES		(@term_key,
					@language_key,
					LTRIM(RTRIM(dbo.ufn_RemoveHtmlMarkup(@term_name))),
					@SessionID)
		IF @@ERROR <> 0 GOTO fail
	END

	/*--------------------------------------------------------------------------------*\ 
		Create term version.
	\*--------------------------------------------------------------------------------*/
	EXECUTE		spNextKey	'Term_Version',
							@term_version_key	OUTPUT
	IF @@ERROR <> 0 GOTO fail

	INSERT		Term_Version (
				Term_Version_Key,
				Term_Key,
				Author_And_Date,
				Entered_Session_ID)
	VALUES		(@term_version_key,
				@term_key,
				ISNULL(@author + ' ', '') + ISNULL(@citation_date, ''),
				@SessionID)
	IF @@ERROR <> 0 GOTO fail
	
	/*--------------------------------------------------------------------------------*\ 
		Work out the meaning.
	\*--------------------------------------------------------------------------------*/
	IF @synonym_of IS NOT NULL
	BEGIN
		SELECT		@meaning_key	=	Meaning_Key
		FROM		Concept
		WHERE		Concept_Key		=	@synonym_of

		IF @@ROWCOUNT = 0
		BEGIN
			RAISERROR ('Synonym does not exist.', 16, 1)
			GOTO fail
		END
	END
	ELSE
	BEGIN
		EXECUTE		spNextKey	'Meaning',
								@meaning_key	OUTPUT
		IF @@ERROR <> 0 GOTO fail

		INSERT		Meaning (
					Meaning_Key)
		VALUES		(@meaning_key)

		IF @@ERROR <> 0 GOTO fail
	END

	/*--------------------------------------------------------------------------------*\ 
		Work out the name type.
	\*--------------------------------------------------------------------------------*/
	IF @name_type IS NULL
	BEGIN
		SET			@name_type_key	=	'SYSTEM00000000AN' /* 'Unknown' */
	END
	ELSE
	BEGIN
		SELECT		@name_type_key		=	c.Concept_Key
		FROM		Concept				AS	c
		INNER JOIN	Term				AS	t
		ON			t.Term_Key			=	c.Term_Key
		WHERE		c.Concept_Group_Key	=	'SYSTEM000000000M'
		AND			t.Language_Key		=	'en'
		AND			t.Plaintext			=	dbo.ufn_RemoveHtmlMarkup(@name_type)

		
		IF @@ROWCOUNT = 0
		BEGIN
			EXECUTE		usp_Concept_Insert	@name_type_key	OUTPUT,
											'SYSTEM000000000M',
											@name_type,
											@name_type,
											'en',
											@SessionID,
											'SYSTEM00000000AN'
			IF @@ERROR <> 0 GOTO fail
		END
	END

	/*--------------------------------------------------------------------------------*\ 
		Work out the rank.
	\*--------------------------------------------------------------------------------*/
    IF @rank IS NOT NULL
    BEGIN
        EXECUTE     usp_ConceptGroup_GetDomain  @concept_group_key,
                                                @domain_key         OUTPUT
        IF @@ERROR <> 0 GOTO fail

        SELECT      @rank_key       =   Concept_Rank_Key
        FROM        Concept_Rank
        WHERE       Domain_Key      =   @domain_key
        AND         Abbreviation    =   @rank

        IF @rank_key IS NULL
        BEGIN
            SELECT      @rank_key       =   Concept_Rank_Key
            FROM        Concept_Rank
            WHERE       Domain_Key      =   @domain_key
            AND         Item_Name       =   @rank
        END

        IF @rank_key IS NULL
        BEGIN
            EXECUTE     spNextKey   'Concept_Rank',
                                    @rank_key       OUTPUT
            IF @@ERROR <> 0 GOTO fail

            INSERT      Concept_Rank (
                        Concept_Rank_Key,
                        Domain_Key,
                        Item_Name,
                        Abbreviation,
                        Color_R,
                        Color_G,
                        Color_B,
                        Entered_Session_ID)
            VALUES      (@rank_key,
                        @domain_key,
                        @rank,
                        @rank,
                        0,
                        0,
                        0,
                        @SessionID)
            IF @@ERROR <> 0 GOTO fail
        END
    END

	/*--------------------------------------------------------------------------------*\ 
		Create concept.
	\*--------------------------------------------------------------------------------*/
		
	EXECUTE		spNextKey	'Concept',
							@concept_key	OUTPUT
	IF @@ERROR <> 0 GOTO fail

	INSERT		Concept (
				Concept_Key,
				Term_Key,
				Concept_Group_Key,
				Term_Version_Key,
				List_Preferred,
				Preferred,
                Concept_Rank_Key,
				Name_Type_Concept_Key,
				Meaning_Key,
				Sort_Code,
				List_Code,
				Entered_Session_ID,
				Published_Term,
				Automatic_Published_Term)
	SELECT		@concept_key,
				@term_key,
				@concept_group_key,
				@term_version_key,
				CASE WHEN @synonym_of IS NULL THEN 1 ELSE 0 END,
				CASE
					WHEN @synonym_of IS NULL THEN 1
					WHEN EXISTS (	SELECT		1
									FROM		Concept			AS	c
									INNER JOIN	Term			AS	t
									ON			t.Term_Key		=	c.Term_Key
									WHERE		c.Meaning_Key	=	@meaning_key
									AND			t.Language_Key	=	@language_key)
						THEN 0
					ELSE 1
				END,
                @rank_key,
				@name_type_key,
				@meaning_key,
				@sort_code,
				@list_code,
				@SessionID,
				@term_name,
				1
	IF @@ERROR <> 0 GOTO fail

	/*--------------------------------------------------------------------------------*\ 
		Update the lineage.
	\*--------------------------------------------------------------------------------*/
	EXECUTE		usp_ConceptLineage_NewConcept	@concept_key
	IF @@ERROR <> 0 GOTO fail

	/*--------------------------------------------------------------------------------*\ 
		Create the parent-child relationship.
	\*--------------------------------------------------------------------------------*/
	IF @child_of IS NOT NULL
	BEGIN
		DECLARE		@relation_key		CHAR(16),
					@relation_type_key	CHAR(16)

		SELECT		@relation_type_key	=	Hierarchy_Relation_Type_Key
		FROM		Concept_Group
		WHERE		Concept_Group_Key	=	@concept_group_key

		EXECUTE		usp_ConceptRelation_Insert	@relation_key	OUTPUT,
												@child_of,
												@concept_key,
												@relation_type_key,
												@SessionID = @SessionID
		IF @@ERROR <> 0 GOTO fail
	END

	/*--------------------------------------------------------------------------------*\ 
		Create up to 5 facts.
	\*--------------------------------------------------------------------------------*/
	DECLARE	@Counter 			TINYINT,
			@fact_title			VARCHAR(100),
			@fact_type			NVARCHAR(150),
			@fact_description	VARCHAR(8000)
	DECLARE	@fact_type_key		CHAR(16),
			@fact_key			CHAR(16)
	SET		@Counter = 1

	WHILE @Counter <= 5 BEGIN
		-- Depending on counter, assign param values to holding vars
		IF @Counter = 1
			SELECT	@fact_title			= @fact_#1_title, 
					@fact_type			= @fact_#1_type,
					@fact_description	= @fact_#1_description
		ELSE
		IF @Counter = 2
			SELECT	@fact_title			= @fact_#2_title, 
					@fact_type			= @fact_#2_type,
					@fact_description	= @fact_#2_description
		ELSE
		IF @Counter = 3
			SELECT	@fact_title			= @fact_#3_title, 
					@fact_type			= @fact_#3_type,
					@fact_description	= @fact_#3_description
		ELSE
		IF @Counter = 4
			SELECT	@fact_title			= @fact_#4_title, 
					@fact_type			= @fact_#4_type,
					@fact_description	= @fact_#4_description
		ELSE
		IF @Counter = 5
			SELECT	@fact_title			= @fact_#5_title, 
					@fact_type			= @fact_#5_type,
					@fact_description	= @fact_#5_description

		-- Update now for next time round.
		SET @Counter = @Counter + 1

		-- Now proceed with the inserts
		IF @fact_description IS NOT NULL
		BEGIN
	
			IF @fact_type IS NULL
			BEGIN
				SET			@fact_type_key	=	'SYSTEM00000002NO' /* HTML */
			END
			ELSE
			BEGIN
				SELECT		@fact_type_key		=	c.Concept_Key
				FROM		Concept				AS	c
				INNER JOIN	Term				AS	t
				ON			t.Term_Key			=	c.Term_Key
				WHERE		c.Concept_Group_Key	=	'SYSTEM000000000L'
				AND			t.Language_Key		=	'en'
				AND			t.Plaintext			=	dbo.ufn_RemoveHtmlMarkup(@fact_type)
	
				IF @@ROWCOUNT = 0
				BEGIN
					EXECUTE		usp_Concept_Insert	@fact_type_key	OUTPUT,
													'SYSTEM000000000L',
													@fact_type,
													@fact_type,
													'en',
													@SessionID,
													'SYSTEM00000000AN'
					IF @@ERROR <> 0 GOTO fail
				END
			END
	
			EXECUTE		spNextKey	'Thesaurus_Fact',
									@fact_key			OUTPUT
			INSERT		Thesaurus_Fact (
						Thesaurus_Fact_Key,
						Item_Name,
						Data,
						Meaning_Key,
						Language_Key,
						Fact_Vague_Date_Type,
						Fact_Type_Concept_Key,
						Entered_Session_ID,
						System_Supplied_Data)
			VALUES		(@fact_key,
						ISNULL(@fact_title, 'Fact'),
						@fact_description,
						@meaning_key,
						'en',
						'U',
						@fact_type_key,
						@SessionID,
						0)
			IF @@ERROR <> 0 GOTO fail
		END
	END

	/*--------------------------------------------------------------------------------*\ 
		Create up to 2 designations.
	\*--------------------------------------------------------------------------------*/
	DECLARE @designation_Status				NVARCHAR(150),
			@designation_Start_Date_Start	INT,
			@designation_Start_Date_End		INT,
			@designation_Start_Date_Type	VARCHAR(2),
			@designation_End_Date_Start		INT,
			@designation_End_Date_End		INT,
			@designation_End_Date_Type		VARCHAR(2)
	DECLARE	@designation_key				CHAR(16),
			@designation_type_concept_key	CHAR(16),
			@designation_concept_group_key	CHAR(16),
			@designation_term_key			CHAR(16)
	SET		@Counter = 1

	WHILE @Counter <= 2 BEGIN
		SELECT	@designation_key				= NULL,
				@designation_type_concept_key	= NULL,
				@designation_concept_group_key	= NULL

		IF @Counter = 1
			SELECT	@designation_status 			= @designation_#1_status,
					@designation_start_date_start	= @designation_#1_start_date_start,
					@designation_start_date_end		= @designation_#1_start_date_end,
					@designation_start_date_type	= @designation_#1_start_date_type,
					@designation_end_date_start		= @designation_#1_end_date_start,
					@designation_end_date_end		= @designation_#1_end_date_end,
					@designation_end_date_type		= @designation_#1_end_date_type
		ELSE
			SELECT	@designation_status 			= @designation_#2_status,
					@designation_start_date_start	= @designation_#2_start_date_start,
					@designation_start_date_end		= @designation_#2_start_date_end,
					@designation_start_date_type	= @designation_#2_start_date_type,
					@designation_end_date_start		= @designation_#2_end_date_start,
					@designation_end_date_end		= @designation_#2_end_date_end,
					@designation_end_date_type		= @designation_#2_end_date_type

		IF @designation_status IS NOT NULL BEGIN
			-- Locate the Concept Designation Type concept group. Create new one if required.
			EXECUTE		usp_ConceptDesignationsAvailable_Get	@concept_group_key,
																@designation_concept_group_key OUTPUT
			IF @designation_concept_group_key IS NULL BEGIN
				EXECUTE	spNextKey 'Concept_Group', @designation_concept_group_key OUTPUT

				INSERT INTO Concept_Group (
							Concept_Group_Key,
							Local_Domain_Key,
							Item_Name,
							Entered_Session_Id)
				SELECT		@designation_concept_group_key,
							Local_Domain_Key,
							'Concept Designation Types',
							@SessionId
				FROM		Concept_Group
				WHERE		Concept_Group_Key = @concept_group_key

				IF @@ERROR <> 0 GOTO fail
			END

			-- Status has to be added as concept if not found.
			SELECT	@designation_type_concept_key = concept_Key
			FROM	vw_ConceptTerm
			WHERE	Item_Name 			= @designation_status
			AND		Concept_Group_Key 	= @designation_concept_group_key

			IF @designation_type_concept_key IS NULL BEGIN
				-- No history for this concept, so can't use usp_Concept_Insert directly.
				EXECUTE	usp_Term_Insert
						@Key				= @designation_term_key OUTPUT,
						@LanguageKey 		= 'en',
						@Plaintext 			= @designation_status,
						@SessionID 			= @SessionID,
						@SystemSuppliedData = 0
				IF @@ERROR <> 0 GOTO fail

				EXECUTE	usp_ConceptSimple_Insert 
						@Key 					= @designation_type_concept_key OUTPUT,
						@TermKey 				= @designation_term_key,
						@ConceptGroupKey 		= @designation_concept_group_key,
						@Preferred 				= 1,
						@NameTypeConceptKey		= 'SYSTEM0000000000', -- Formal name type
						@SessionID 				= @SessionID,
						@SystemSuppliedData		= 0,
						@AutomaticPublishedTerm	= 1
				IF @@ERROR <> 0 GOTO fail
			END

			-- Now add the concept designation record.
			EXECUTE		spNextKey	'Concept_Designation',
									@designation_key		OUTPUT
			IF @@ERROR <> 0 GOTO fail

			INSERT INTO Concept_Designation (
						Concept_Designation_Key,
						Concept_Key,
						Designation_Type_Concept_Key,
						From_Vague_Date_Start,
						From_Vague_Date_End,
						From_Vague_Date_Type,
						To_Vague_Date_Start,
						To_Vague_Date_End,
						To_Vague_Date_Type,
						Entered_Session_ID)
			VALUES 		(@designation_Key,
						@concept_Key,
						@designation_type_concept_key,
						@designation_start_date_start,
						@designation_start_date_end,
						@designation_start_date_type,
						@designation_end_date_start,
						@designation_end_date_end,
						@designation_end_date_type,
						@SessionID)

			IF @@ERROR <> 0 GOTO fail
		END

		SET @Counter = @Counter + 1
	END

	COMMIT TRANSACTION
	RETURN

fail:
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_Concept_InsertSpreadsheetRow failed', 16, 1)

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_InsertSpreadsheetRow') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_Concept_InsertSpreadsheetRow'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concept_InsertSpreadsheetRow TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concept_InsertSpreadsheetRow TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
       	GRANT EXECUTE ON dbo.usp_Concept_InsertSpreadsheetRow TO [Dev - JNCC SQL]
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
    $Date: 17/08/11 15:40 $
    $Author: Jamesbichard $

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
				Author_Copy, Sort_Code, List_Code, Published_Term, Automatic_Published_Term,
				Term_Generator_Key, Entered_Session_ID, System_Supplied_Data, Custodian
			)
			SELECT 	@Key, Term_Key, @DestConceptGroupKey, Term_Version_Key, List_Preferred, 
				Is_Current, Preferred, Concept_Rank_Key, Name_Type_Concept_Key, Meaning_Key,
				Author_Copy, Sort_Code, List_Code, Published_Term, Automatic_Published_Term,
				Term_Generator_Key, @SessionID, @SystemSuppliedData, LEFT(@Key, 8)
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
/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Concept_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Concept_Select]
GO

/*===========================================================================*\
  Description:	Returns fields from the Concept table.

  Parameters:	@ConceptKey

  Created:	December 2003

  Last revision information:
    $Revision: 5 $
    $Date: 17/08/11 15:40 $
    $Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Concept_Select]
	@ConceptKey char(16)
AS
	SET NOCOUNT ON
	SET ANSI_NULLS ON
	SET ANSI_PADDING ON
	SET ANSI_WARNINGS ON
	SET CONCAT_NULL_YIELDS_NULL ON
	SET QUOTED_IDENTIFIER ON
	SET NO_BROWSETABLE OFF

	SELECT	
			C.Term_Key AS Item_Key,
			C.Published_Term AS Item_Name,
			TL.Plaintext,
			TV.Version_Label,
			TV.Author_And_Date,
			Lower(TL.Language_Key) AS Language_Key,
			L.Language_Key + ' - ' + L.Item_Name AS Language_Name,
			C.Concept_Group_Key,
			CG.Hierarchy_Relation_Type_Key,
			C.Term_Version_Key,	
			C.List_Preferred,
			C.Is_Current,
			C.Preferred,
			C.Concept_Rank_Key,
			CR.Item_Name AS Concept_Rank_Name,
			C.Name_Type_Concept_Key,
			CT.Item_Name AS Name_Type_Concept_Name,
			C.Meaning_Key,
			C.Author_Copy,
			C.Sort_Code,
			C.List_Code,
			C.Automatic_Published_Term,
			C.Term_Generator_Key,
			TG.Item_Name AS Term_Generator_Name,
			CASE WHEN CRel.Concept_Relation_Key IS NULL THEN 0 ELSE 1 END AS HasChildren,
			C.System_Supplied_Data,
			C.Entered_Session_ID,
			C.[Timestamp]
	FROM 		Concept AS C
	INNER JOIN	Term AS TL ON TL.Term_Key = C.Term_Key
	INNER JOIN	Language AS L ON L.Language_Key = TL.Language_Key
	LEFT JOIN	Concept_Rank AS CR ON CR.Concept_Rank_Key = C.Concept_Rank_Key
	INNER JOIN	VW_ConceptTerm AS CT ON CT.Concept_Key = C.Name_Type_Concept_Key
	INNER JOIN	Concept_Group AS CG ON CG.Concept_Group_Key = C.Concept_Group_Key
	LEFT JOIN 	Concept_Relation CRel ON CRel.From_Concept_Key = C.Concept_Key
       				     	     AND CRel.Thesaurus_Relation_Type_Key = CG.Hierarchy_Relation_Type_Key
	LEFT JOIN	Term_Version TV ON TV.Term_Version_Key = C.Term_Version_Key
	LEFT JOIN	Term_Generator TG ON TG.Term_Generator_Key = C.Term_Generator_Key
	WHERE		C.Concept_Key = @ConceptKey
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Concept_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Concept_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concept_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concept_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Concept_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Concept_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Concept_Select TO [Dev - JNCC SQL]
END

GO

SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Concept_Select_ForChild]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Concept_Select_ForChild]
GO

/*===========================================================================*\
  Description:	
		Selects the parent of the given concept. 

  Parameters:
		@ChildConceptKey
		@HierarchyRelationTypeKey - relationship type used to populate
						hierarchy.	

  Created:	September 2010

  Last revision information:
    $Revision: 5 $
    $Date: 17/08/11 15:40 $
    $Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Concept_Select_ForChild]
	@ChildConceptKey			VARCHAR(100),
  	@HierarchyRelationTypeKey	CHAR(16)
AS
	SELECT DISTINCT 
				C.Concept_Key, 
				C.Published_Term	AS	Item_Name, 
				C.Sort_Code, 
  				CASE 
					WHEN CR2.Concept_Relation_Key IS NULL 
					THEN 0 
					ELSE 1 
				END AS HasParents,
  				C.Concept_Rank_Key
	FROM 		Concept_Relation	AS	CR1
	INNER JOIN 	Concept				AS	C
	ON			C.Concept_Key		=	CR1.From_Concept_Key
	LEFT JOIN 	(Concept_Relation CR2 
				INNER JOIN Concept	AS C2 
				ON	C2.Concept_Key			= CR2.From_Concept_Key
					AND C2.List_Preferred	= 1
					AND C2.Is_Current		= 1)
	ON			CR2.To_Concept_Key				= C.Concept_Key
		AND		CR2.Thesaurus_Relation_Type_Key = @HierarchyRelationTypeKey
	WHERE 		CR1.To_Concept_Key				= @ChildConceptKey
		AND 	CR1.Thesaurus_Relation_Type_Key = @HierarchyRelationTypeKey
		AND 	C.List_Preferred	= 1
		AND 	C.Is_Current		= 1
	ORDER BY 	C.Sort_Code, 
				C.Published_Term
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_Select_ForChild') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Concept_Select_ForChild'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Concept_Select_ForChild TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForChild TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForChild TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForChild TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Concept_Select_ForChild TO [Dev - JNCC SQL]
END
GO
/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
       FROM   SysObjects 
       WHERE  Id = Object_Id(N'[dbo].[usp_Concept_Select_ForConceptGroupSearch]')
       AND    Type = 'P')
    DROP PROCEDURE [dbo].[usp_Concept_Select_ForConceptGroupSearch]
GO

/*===========================================================================*\
  Description:  Retrieves a list of concepts that match a search string, in a 
                                specified concept group.

  Parameters:   @ConceptGroup - key of the concept group
                            @SearchText - search text

  Created:  August 2003

  Last revision information:
    $Revision: 5 $
    $Date: 17/08/11 15:40 $
    $Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Concept_Select_ForConceptGroupSearch]
	@SearchKey char(16),
	@SearchText varchar(100),
	@SearchSize int = NULL
AS

SET ANSI_NULLS ON
SET ANSI_PADDING ON
SET ANSI_WARNINGS ON
SET ARITHABORT ON
SET CONCAT_NULL_YIELDS_NULL ON
SET QUOTED_IDENTIFIER ON
SET NO_BROWSETABLE OFF

    SET @SearchSize =   ISNULL(@SearchSize, 0)

    SET ROWCOUNT @SearchSize

	SELECT DISTINCT CT.Concept_Key as Item_Key,
	  CT.Item_Name AS DisplayTerm,
	  CASE WHEN CT.Author_Copy IS NULL THEN
		ISNULL(ST.Plaintext, CT.Plaintext)
	  ELSE
		ISNULL(ST.Plaintext, CT.Plaintext) + ' ' + CT.Author_Copy COLLATE SQL_Latin1_General_CP1_CI_AI
	  END AS SearchTerm,
	  CT.Author_copy,
	  CT.Concept_Rank_Key
	FROM VW_ConceptTerm CT
	LEFT JOIN Search_Term ST on ST.Concept_Key = CT.Concept_Key
	WHERE CT.Concept_Group_Key = @SearchKey
	AND (ST.Plaintext like @SearchText + '%' 
	OR CT.Author_Copy like @SearchText + '%')
	AND CT.Is_Current = 1
	ORDER BY SearchTerm, Author_Copy

    SET ROWCOUNT 0    
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_Select_ForConceptGroupSearch') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_Concept_Select_ForConceptGroupSearch'
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        GRANT EXECUTE ON dbo.usp_Concept_Select_ForConceptGroupSearch TO [R2k_AddOnly]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
        GRANT EXECUTE ON dbo.usp_Concept_Select_ForConceptGroupSearch TO [R2k_Administrator]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
        GRANT EXECUTE ON dbo.usp_Concept_Select_ForConceptGroupSearch TO [R2k_FullEdit]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
        GRANT EXECUTE ON dbo.usp_Concept_Select_ForConceptGroupSearch TO [R2k_ReadOnly]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
        GRANT EXECUTE ON dbo.usp_Concept_Select_ForConceptGroupSearch TO [R2k_RecordCardsOnly]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_Concept_Select_ForConceptGroupSearch TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Concept_Select_ForConceptGroupVersionSearch]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Concept_Select_ForConceptGroupVersionSearch]
GO

/*===========================================================================*\
  Description:	Retrieves a list of concepts that match a search string, in a 
 								specified concept group version.

  Parameters:	@SearchKey - key of the concept group version
							@SearchText - search text

  Created:	August 2003

  Last revision information:
    $Revision: 5 $
    $Date: 17/08/11 15:40 $
    $Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Concept_Select_ForConceptGroupVersionSearch]
	@SearchKey char(16),
  @SearchText varchar(100)
AS

SET ANSI_NULLS ON
SET ANSI_PADDING ON
SET ANSI_WARNINGS ON
SET ARITHABORT ON
SET CONCAT_NULL_YIELDS_NULL ON
SET QUOTED_IDENTIFIER ON
SET NO_BROWSETABLE OFF


SELECT DISTINCT CT.Concept_Key as Item_Key, 
  CT.Item_Name AS DisplayTerm, 
  CASE WHEN CT.Author_Copy IS NULL THEN
    ISNULL(ST.Plaintext, CT.Plaintext)
  ELSE
    ISNULL(ST.Plaintext, CT.Plaintext) + ' ' + CT.Author_Copy COLLATE SQL_Latin1_General_CP1_CI_AI  
  END AS SearchTerm, 
  CT.Author_copy,
  CT.Concept_Rank_Key
FROM VW_ConceptTerm CT
  INNER JOIN Concept_Group_Version CGV on CGV.Concept_Group_Key=CT.Concept_Group_Key
      AND CGV.Concept_Group_Version_Key=@SearchKey
  LEFT JOIN Search_Term ST on ST.Concept_Key = CT.Concept_Key
  LEFT JOIN Concept_History CH on CH.Concept_Key=CT.Concept_Key
  LEFT JOIN Concept_Group_Version CGV1 ON CGV1.Concept_Group_Version_Key=CH.Concept_Group_Version_From
  LEFT JOIN Concept_Group_Version CGV2 ON CGV2.Concept_Group_Version_Key=CH.Concept_Group_Version_To
WHERE (ST.Plaintext like @SearchText + '%'
  OR CT.Author_Copy like @SearchText + '%')
  AND (CGV1.Concept_Group_Version_Key IS NULL OR CGV1.Sequence<=CGV.Sequence)
  AND (CGV2.Concept_Group_Version_Key IS NULL OR CGV2.Sequence>=CGV.Sequence)
ORDER BY CT.SearchTerm, CT.Author_Copy
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_Select_ForConceptGroupVersionSearch') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Concept_Select_ForConceptGroupVersionSearch'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Concept_Select_ForConceptGroupVersionSearch TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForConceptGroupVersionSearch TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForConceptGroupVersionSearch TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForConceptGroupVersionSearch TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForConceptGroupVersionSearch TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Concept_Select_ForConceptGroupVersionSearch TO [Dev - JNCC SQL]
END

GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Concept_Select_ForGroup]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Concept_Select_ForGroup]
GO
    
/*===========================================================================*\
  Description:	Returns all concept keys relating to the group

  Parameters:	@Concept_Group_Key  Key of the Concept group whose members we're
		retrieving

  Created:	August 2003

  Last revision information:
    $Revision: 5 $
    $Date: 17/08/11 15:40 $
    $Author: Jamesbichard $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_Concept_Select_ForGroup]
	@Concept_Group_Key as Char(16)
 AS

SET NOCOUNT ON

SELECT C.Concept_Key, T.PlainText FROM Concept C 
                       INNER JOIN Term T ON T.Term_Key = C.Term_Key 
                       WHERE (C.Concept_Group_Key = @Concept_Group_Key)
                       AND (C.List_Preferred = 1)
                       AND (C.Is_Current = 1)
                       ORDER BY C.Sort_Code, T.Plaintext

go

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_Select_ForGroup') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Concept_Select_ForGroup'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Concept_Select_ForGroup TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForGroup TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForGroup TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForGroup TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForGroup TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Concept_Select_ForGroup TO [Dev - JNCC SQL]
END

GO


If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Concept_Select_ForGroupAndItemName]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Concept_Select_ForGroupAndItemName]
GO
    
/*===========================================================================*\
  Description:	Returns a concept key matching the plain text

  Parameters:	@Concept_Group_Key  Key of the Concept group whose members we're retrieving
		@Key#               Keys to match

  Created:	August 2003

  Last revision information:
    $Revision: 5 $
    $Date: 17/08/11 15:40 $
    $Author: Jamesbichard $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_Concept_Select_ForGroupAndItemName]
	@Concept_Group_Key as Char(16),
	@PlainTExt as nVarchar(100)
	
 AS

SET NOCOUNT ON
SELECT C.Concept_Key FROM Concept C 
                       INNER JOIN Term T ON T.Term_Key = C.Term_Key 
                       WHERE (C.Concept_Group_Key = @Concept_Group_Key)
                       AND (C.List_Preferred = 1)
                       AND (C.Is_Current = 1)
		       and PlainText = @PlainText
                       ORDER BY C.Sort_Code, T.Plaintext
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_Select_ForGroupAndItemName') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Concept_Select_ForGroupAndItemName'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Concept_Select_ForGroupAndItemName TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForGroupAndItemName TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForGroupAndItemName TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForGroupAndItemName TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForGroupAndItemName TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Concept_Select_ForGroupAndItemName TO [Dev - JNCC SQL]
END
GO
/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Concept_Select_ForKeyListAndGroup]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Concept_Select_ForKeyListAndGroup]
GO
    
/*===========================================================================*\
  Description:	Returns all concept keys relating to the group and in a list of keys.

  Parameters:	@Concept_Group_Key	Key of the Concept group whose members we're retrieving
		@Key#			Keys to match

  Created:	August 2003

  Last revision information:
    $Revision: 5 $
    $Date: 17/08/11 15:40 $
    $Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Concept_Select_ForKeyListAndGroup]
	@Concept_Group_Key1 as Char(16),
	@Concept_Group_Key2 as Char(16) = NULL,
	@Key1 as Char(16),
	@Key2 as Char(16) = NULL,
	@Key3 as Char(16) = NULL,
	@Key4 as Char(16) = NULL,
	@Key5 as Char(16) = NULL,
	@Key6 as Char(16) = NULL,
	@Key7 as Char(16) = NULL,
	@Key8 as Char(16) = NULL,
	@Key9 as Char(16) = NULL,
	@Key10 as Char(16) = NULL
AS
	SET NOCOUNT ON

	SELECT		C.Concept_Key, T.PlainText
	FROM		Concept C 
	INNER JOIN	Term T ON T.Term_Key = C.Term_Key 
	WHERE 		(C.Concept_Group_Key = @Concept_Group_Key1
	OR		 C.Concept_Group_Key = @Concept_Group_Key2)
	AND		C.List_Preferred = 1
	AND		C.Is_Current = 1
	AND		Concept_Key IN (@Key1, @Key2, @Key3, @Key4, @Key5, @Key6, @Key7, @Key8,@Key9, @Key10)
	ORDER BY	C.Sort_Code, T.Plaintext
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_Select_ForKeyListAndGroup') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Concept_Select_ForKeyListAndGroup'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Concept_Select_ForKeyListAndGroup TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForKeyListAndGroup TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForKeyListAndGroup TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForKeyListAndGroup TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForKeyListAndGroup TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Concept_Select_ForKeyListAndGroup TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Concept_Select_ForParent]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Concept_Select_ForParent]
GO

/*===========================================================================*\
  Description: Returns a list of concepts that are with the supplied parent	

  Parameters:	@ParentConceptKey
		@HierarchyRelationTypeKey - relationship type used to populate
						hierarchy.

  Created:	August 2003

  Last revision information:
    $Revision: 5 $
    $Date: 17/08/11 15:40 $
    $Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Concept_Select_ForParent]
	@ParentConceptKey varchar(100),
  	@HierarchyRelationTypeKey char(16)
AS

SELECT distinct C.Concept_Key, 
		C.Published_Term	AS	Item_Name, 
		C.Sort_Code, 
  		CASE WHEN CR2.Concept_Relation_Key IS NULL THEN 0 
							   ELSE 1 
		END AS HasChildren,
  		C.Concept_Rank_Key
FROM 		Concept_Relation CR1
INNER JOIN 	Concept C ON C.Concept_Key = CR1.To_Concept_Key
INNER JOIN 	Term T ON T.Term_Key = C.Term_Key
LEFT JOIN 	(Concept_Relation CR2 
			INNER JOIN Concept AS C2 ON C2.Concept_Key = CR2.To_Concept_Key
						AND C2.List_Preferred = 1
						AND C2.Is_Current = 1)
		ON CR2.From_Concept_Key = C.Concept_Key
       		AND CR2.Thesaurus_Relation_Type_Key = @HierarchyRelationTypeKey
WHERE 		CR1.From_Concept_Key = @ParentConceptKey
AND 		CR1.Thesaurus_Relation_Type_Key = @HierarchyRelationTypeKey
AND 		C.List_Preferred = 1
AND 		C.Is_Current = 1
ORDER BY 	C.Sort_Code, 
			Item_Name

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_Select_ForParent') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Concept_Select_ForParent'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Concept_Select_ForParent TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForParent TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForParent TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForParent TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForParent TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Concept_Select_ForParent TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
       FROM   SysObjects 
       WHERE  Id = Object_Id(N'[dbo].[usp_Concept_Select_ForSubjectAreaSearch]')
       AND    Type = 'P')
    DROP PROCEDURE [dbo].[usp_Concept_Select_ForSubjectAreaSearch]
GO

/*===========================================================================*\
  Description:  Retrieves a list of concepts that match a search string, in a 
                                specified subject area.

  Parameters:   @SearchKey - key of the subject area
                            @SearchText - search text

  Created:  August 2003

  Last revision information:
    $Revision: 5 $
    $Date: 17/08/11 15:40 $
    $Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Concept_Select_ForSubjectAreaSearch]
	@SearchKey char(16),
	@SearchText varchar(100),
	@SearchSize int = NULL
AS

SET ANSI_NULLS ON
SET ANSI_PADDING ON
SET ANSI_WARNINGS ON
SET ARITHABORT ON
SET CONCAT_NULL_YIELDS_NULL ON
SET QUOTED_IDENTIFIER ON
SET NO_BROWSETABLE OFF

    SET @SearchSize =   ISNULL(@SearchSize, 0)

    SET ROWCOUNT @SearchSize

	SELECT CT.Concept_Key as Item_Key,
	  CT.Item_Name AS DisplayTerm,
	  CASE WHEN Author_Copy IS NULL THEN
		ISNULL(ST.Plaintext, CT.Plaintext)
	  ELSE
		ISNULL(ST.Plaintext, CT.Plaintext) + ' ' + Author_Copy COLLATE SQL_Latin1_General_CP1_CI_AI
	  END AS SearchTerm,
	  CT.Author_copy,
	  CT.Concept_Rank_Key
	FROM VW_ConceptTerm CT
	INNER JOIN Concept_Group CG ON CT.Concept_Group_Key = CG.Concept_Group_Key
	INNER JOIN Local_Domain LD ON CG.Local_Domain_Key = LD.Local_Domain_Key
	INNER JOIN Domain D ON LD.Domain_Key = D.Domain_Key
	LEFT JOIN Search_Term ST ON ST.Concept_Key = CT.Concept_Key
	WHERE D.Subject_Area_Key = @SearchKey
	AND (ST.Plaintext like @SearchText + '%'
	OR CT.Author_Copy like @SearchText + '%')
	AND CT.Is_Current = 1
	ORDER BY CT.SearchTerm, CT.Author_Copy

    SET ROWCOUNT 0    
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_Select_ForSubjectAreaSearch') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_Concept_Select_ForSubjectAreaSearch'
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        GRANT EXECUTE ON dbo.usp_Concept_Select_ForSubjectAreaSearch TO [R2k_AddOnly]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
        GRANT EXECUTE ON dbo.usp_Concept_Select_ForSubjectAreaSearch TO [R2k_Administrator]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
        GRANT EXECUTE ON dbo.usp_Concept_Select_ForSubjectAreaSearch TO [R2k_FullEdit]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
        GRANT EXECUTE ON dbo.usp_Concept_Select_ForSubjectAreaSearch TO [R2k_ReadOnly]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
        GRANT EXECUTE ON dbo.usp_Concept_Select_ForSubjectAreaSearch TO [R2k_RecordCardsOnly]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_Concept_Select_ForSubjectAreaSearch TO [Dev - JNCC SQL]
END

GO

SET ANSI_NULLS ON
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].usp_Concept_Select_ForTermVersion')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].usp_Concept_Select_ForTermVersion
GO

/*===========================================================================*\
  Description:	Returns concepts with a specified term version.

  Parameters: @TermVersionKey  - term version to search for
				@ConceptKey - optional parameter to exclude specified concept	

  Created:	August 2011

  Last revision information:
    $Revision: 5 $
    $Date: 17/08/11 15:40 $
    $Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].usp_Concept_Select_ForTermVersion
	@TermVersionKey char(16),
	@ConceptKey char(16) = NULL
AS
	SELECT Concept_Key
	FROM Concept
	WHERE Term_Version_Key = @TermVersionKey
	AND (Concept_Key <> @ConceptKey OR @ConceptKey = null)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_Select_ForTermVersion') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Concept_Select_ForTermVersion'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Concept_Select_ForTermVersion TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForTermVersion TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForTermVersion TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForTermVersion TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForTermVersion TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Concept_Select_ForTermVersion TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'dbo.usp_Concept_Select_Homonyms')
	   AND    Type = 'P')
	DROP PROCEDURE dbo.usp_Concept_Select_Homonyms
GO

/*===========================================================================*\
  Description:	Returns all homonyms of the specified concept.

  Parameters:   
	@concept_key	Concept key

  Created:	May 2011

  Last revision information:
	$Revision: 5 $
	$Date: 17/08/11 15:40 $
	$Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE dbo.usp_Concept_Select_Homonyms
	@Key		CHAR(16)
AS
	SET NOCOUNT ON

	select 
		h.Concept_Key as Item_Key,
		ct.Item_Name + ' (' + cg.Item_Name + ')' as Item_Name
	from		concept c
	inner join	homonym_pair hp
		on		hp.Meaning_Key_1 = c.Meaning_Key or hp.Meaning_Key_2 = c.Meaning_Key
	inner join	concept h
		on		(hp.Meaning_Key_1 = h.Meaning_Key or hp.Meaning_Key_2 = h.Meaning_Key)
		and		(h.Meaning_Key <> c.Meaning_Key)
	inner join 	VW_ConceptTerm ct
	 	on ct.Concept_Key = h.Concept_Key
	left join	Term_Version tv 	
		on tv.Term_Version_Key = h.Term_Version_Key
	inner join	Concept_Group cg
		on cg.Concept_Group_Key = h.Concept_Group_Key
	where c.concept_key = @Key
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_Select_Homonyms') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Concept_Select_Homonyms'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concept_Select_Homonyms TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concept_Select_Homonyms TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Concept_Select_Homonyms TO [Dev - JNCC SQL]
END
GO
/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_Concept_Select_Imported]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_Concept_Select_Imported]
GO

/*===========================================================================*\
  Description:	List-preferred concepts from the specified group that have
				Timestamp later than the given value.

				Note that @session_id should *not* be named @SessionID so
				that dmGeneral does not automagically supply a value when
				we don't want it to.

  Parameters:   @concept_group_key		Concept group key
				@timestamp				Timestamp
				@session_id				[optional] If specified, restrict
										records to those inserted in that
										session

  Created:		Jan 2004

  Last revision information:
	$Revision: 5 $
	$Date: 17/08/11 15:40 $
	$Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Concept_Select_Imported]
	@concept_group_key	CHAR(16),
	@timestamp			TIMESTAMP,
	@session_id			CHAR(16)	=	NULL
AS
	SET NOCOUNT ON

	IF @session_id IS NULL
	BEGIN
		SELECT		c.Concept_Key,
					c.Timestamp,
					c.Published_Term		AS	Item_Name
		FROM		Concept					AS	c
		INNER JOIN	Term					AS	t
		ON			t.Term_Key				=	c.Term_Key
		WHERE		c.Concept_Group_Key		=	@concept_group_key
		AND			c.Timestamp				>	ISNULL(@timestamp, 0)
		AND			c.List_Preferred		=	1
	END
	ELSE
	BEGIN
		SELECT		c.Concept_Key,
					c.Timestamp,
					c.Published_Term		AS	Item_Name
		FROM		Concept					AS	c
		INNER JOIN	Term					AS	t
		ON			t.Term_Key				=	c.Term_Key
		WHERE		c.Concept_Group_Key		=	@concept_group_key
		AND			c.Entered_Session_ID	=	@session_id
		AND			c.Timestamp				>	ISNULL(@timestamp, 0)
		AND			c.List_Preferred		=	1
	END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_Select_Imported') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Concept_Select_Imported'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concept_Select_Imported TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concept_Select_Imported TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Concept_Select_Imported TO [Dev - JNCC SQL]
END
GO
/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_Concept_Select_Synonyms]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_Concept_Select_Synonyms]
GO

/*===========================================================================*\
  Description:	Names of synonyms of the specified concept.

  Parameters:   
	@concept_key	Concept key

  Created:	Jan 2004

  Last revision information:
	$Revision: 5 $
	$Date: 17/08/11 15:40 $
	$Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Concept_Select_Synonyms]
	@concept_key		CHAR(16)
AS
	SET NOCOUNT ON

	SELECT			DISTINCT
					s.Concept_Key,
					s.Published_Term	AS	Item_Name,
					ISNULL
					(
						TV.Author_And_Date,
						''
					)						AS		Authority, 
					t.PlainText,
					g.Item_Name				AS		Group_Name
	
	FROM			Concept					AS		c
	INNER JOIN		Concept					AS		s
	ON				s.Meaning_Key			=		c.Meaning_Key
	
	INNER JOIN		Term					AS		t
	ON				t.Term_Key				=		s.Term_Key
	

	INNER JOIN		dbo.Term_Version		AS		TV
	ON				TV.Term_Version_Key		=		s.Term_Version_Key

	INNER JOIN		Concept_Group			AS 		g
	ON				g.Concept_Group_Key		=		s.Concept_Group_Key
	WHERE			c.Concept_Key			=		@concept_key
	AND				s.Concept_Key			<>		@concept_key
	ORDER BY		g.Item_Name
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_Select_Synonyms') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Concept_Select_Synonyms'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concept_Select_Synonyms TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concept_Select_Synonyms TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Concept_Select_Synonyms TO [Dev - JNCC SQL]
END
GO
/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_UpdateAutomaticTerms') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].usp_Concept_UpdateAutomaticTerms
GO

/*===========================================================================*\
  Description:	Update published terms (if required) and search terms for a 
				given concept

  Parameters:	

  Created:	August 2011

  Last revision information:
    $Revision: 5 $
    $Date: 17/08/11 15:40 $
    $Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].usp_Concept_UpdateAutomaticTerms
	@ConceptKey 			CHAR(16),
	@AutomaticUpdate		BIT
AS
	DECLARE @TermGeneratorKey CHAR(16)
	SELECT @TermGeneratorKey = dbo.ufn_GetTermGenerator(@ConceptKey, 0)

	IF @AutomaticUpdate = 1
	BEGIN
		DECLARE @Plaintext VARCHAR(100),
				@ParentKey CHAR(16),
				@HierarchyRelationTypeKey CHAR(16),
				@AuthorAndDate VARCHAR(100),
				@Attributes VARCHAR(100),
				@ConceptRankKey CHAR(16),
				@PublishedTerm NVARCHAR(450)

		SELECT @Plaintext = t.Plaintext,
				@AuthorAndDate = tv.Author_And_Date,
				@Attributes = tv.Version_Label,	
				@ConceptRankKey = c.Concept_Rank_Key,
				@HierarchyRelationTypeKey = cg.Hierarchy_Relation_Type_Key
		FROM Concept c
		INNER JOIN Term t ON c.Term_Key = t.Term_Key
		LEFT JOIN Term_Version tv ON c.Term_Version_Key = tv.Term_Version_Key
		INNER JOIN Concept_Group cg ON c.Concept_Group_Key = cg.Concept_Group_Key
		WHERE c.Concept_Key = @ConceptKey
	
		CREATE TABLE #PublishedTerm (
			Published_Term NVARCHAR(450)
		)
		
		INSERT INTO #PublishedTerm
		EXEC usp_Concept_GeneratePublishedTerm 
			@Plaintext,
			@AuthorAndDate,
			@Attributes,	
			@ConceptRankKey,
			NULL,
			@TermGeneratorKey,
			@HierarchyRelationTypeKey

		SELECT @PublishedTerm = Published_Term
		FROM #PublishedTerm
	
		DROP TABLE #PublishedTerm

		UPDATE Concept
		SET Published_Term = @PublishedTerm
		WHERE Concept_Key = @ConceptKey
	END

	EXEC usp_SearchTerm_DeleteOldTerms @ConceptKey
	EXEC usp_SearchTerm_Generate @ConceptKey, @TermGeneratorKey

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/

	PRINT 'Setting up security on procedure usp_Concept_UpdateAutomaticTerms'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_Concept_UpdateAutomaticTerms TO R2k_AddOnly
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concept_UpdateAutomaticTerms TO R2k_Administrator
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concept_UpdateAutomaticTerms TO R2k_FullEdit
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Concept_UpdateAutomaticTerms TO R2k_ReadOnly
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Concept_UpdateAutomaticTerms TO R2k_RecordCardsOnly
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
		GRANT EXECUTE ON dbo.usp_Concept_UpdateAutomaticTerms TO "Dev - JNCC SQL"

GO
/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].usp_Concept_UpdateDescendentTerms')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].usp_Concept_UpdateDescendentTerms
GO


/*===========================================================================*\
  Description:	Update published and search terms for descendants of a concept

  Parameters:	

  Created:	August 2011

  Last revision information:
    $Revision: 5 $
    $Date: 17/08/11 15:40 $
    $Author: Jamesbichard $

\*===========================================================================*/

CREATE PROCEDURE dbo.usp_Concept_UpdateDescendentTerms
	@ConceptKey char(16) = null,
	@ConceptGroupKey char(16) = null,
	@LocalDomainKey char(16) = null,
	@DomainKey char(16) = null
AS
	SET NOCOUNT ON

	DECLARE @AutomaticPublishedTerm BIT

	IF @ConceptKey IS NOT NULL
	BEGIN
		-- get concept descendents from the concept lineage table
		DECLARE descendents CURSOR FAST_FORWARD LOCAL FOR
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
		ORDER BY crelated.lineage

		OPEN descendents

		WHILE 1 = 1
		BEGIN
			FETCH descendents
			INTO @ConceptKey

			IF @@FETCH_STATUS <> 0 BREAK

			SELECT @AutomaticPublishedTerm = Automatic_Published_Term
			FROM Concept
			WHERE Concept_Key = @ConceptKey

			EXEC usp_Concept_UpdateAutomaticTerms @ConceptKey, @AutomaticPublishedTerm
		END

		CLOSE descendents	
		DEALLOCATE descendents
	END
	ELSE
	BEGIN
		DECLARE descendents CURSOR FAST_FORWARD LOCAL FOR
		SELECT 
			c.Concept_Key
		FROM Concept c
		INNER JOIN Concept_Group cg ON cg.Concept_Group_Key = c.Concept_Group_Key
		INNER JOIN Local_Domain ld ON ld.Local_Domain_Key = cg.Local_Domain_Key
		INNER JOIN Domain d ON d.Domain_Key = ld.Domain_Key	
		WHERE (cg.Concept_Group_Key = @ConceptGroupKey OR @ConceptGroupKey IS NULL)
		AND (ld.Local_Domain_Key = @LocalDomainKey OR @LocalDomainKey IS NULL)
		AND (d.Domain_Key = @DomainKey OR @DomainKey IS NULL)

		OPEN descendents

		WHILE 1 = 1
		BEGIN
			FETCH descendents
			INTO @ConceptKey

			IF @@FETCH_STATUS <> 0 BREAK

			SELECT @AutomaticPublishedTerm = Automatic_Published_Term
			FROM Concept
			WHERE Concept_Key = @ConceptKey

			EXEC usp_Concept_UpdateAutomaticTerms @ConceptKey, @AutomaticPublishedTerm
		END
		
		CLOSE descendents	
		DEALLOCATE descendents
	END

	
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_UpdateDescendentTerms') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Concept_UpdateDescendentTerms'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
			GRANT EXECUTE ON dbo.usp_Concept_UpdateDescendentTerms TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concept_UpdateDescendentTerms TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concept_UpdateDescendentTerms TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Concept_UpdateDescendentTerms TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Concept_UpdateDescendentTerms TO [Dev - JNCC SQL]
END
GO
/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].usp_Concept_UpdatePublishedTerm')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].usp_Concept_UpdatePublishedTerm
GO

/*===========================================================================*\
  Description:	Returns all concepts which belong to the domain/local domain/
				concept group specified, or descend from the concept specified

  Parameters:	

  Created:	August 2011

  Last revision information:
    $Revision: 5 $
    $Date: 17/08/11 15:40 $
    $Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].usp_Concept_UpdatePublishedTerm
	@ConceptKey char(16),
	@PublishedTerm nvarchar(450)
AS
	UPDATE Concept
	SET Published_Term = @PublishedTerm
	WHERE Concept_Key = @ConceptKey	
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_UpdatePublishedTerm') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Concept_UpdatePublishedTerm'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Concept_UpdatePublishedTerm TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concept_UpdatePublishedTerm TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concept_UpdatePublishedTerm TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Concept_UpdatePublishedTerm TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Concept_UpdatePublishedTerm TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Concept_UpdatePublishedTerm TO [Dev - JNCC SQL]
END

GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_ConditionChecks_Select_ForCollectionUnit]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_ConditionChecks_Select_ForCollectionUnit]
GO

/*===========================================================================*\
  Description:	Returns Condition Checks for a specified Collection.

  Parameters:
	@ParentKey 	When specified, only the records associated with the parent key are returned
	@UserDomainMask	User's Domain Mask restricting which records may be returned
	@SessionID 	User's SessionID
	@SortOrderIndex	Index determining Sort Order

  Created:	August 2003

  Last revision information:
    $Revision: 5 $
    $Date: 17/08/11 15:40 $
    $Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConditionChecks_Select_ForCollectionUnit] 
	@UserDomainMask INT,
	@SessionID CHAR(16),
	@ParentKey CHAR(16),
	@SortOrderIndex TINYINT
AS

SET NOCOUNT ON

	SELECT 		CUC.Conservation_Check_Key AS Item_Key, CC.Display_Caption,
			CUC.Collection_Unit_Check_Key AS Join_Key

	FROM 		Collection_Unit_Check CUC
	INNER JOIN 	Conservation_Check CC
		ON CUC.Conservation_Check_Key = CC.Conservation_Check_Key 
		AND CUC.Collection_Unit_Key = @ParentKey
		AND ((CC.Domain_Mask & @UserDomainMask > 0) OR (CC.Entered_Session_ID = @SessionID) 
			OR (CC.Changed_Session_ID = @SessionID) OR (CC.Domain_Mask = 0))

	INNER JOIN 	Concept C ON CC.Type_Concept_Key = C.Concept_Key
	INNER JOIN 	Term T ON C.Term_Key = T.Term_Key

	ORDER BY 
		-- 0: CC.Vague_Date_Start DESC, CC.Vague_Date_End DESC, Item_Name, CC.Ref_Number
		-- 1: CC.Ref_Number
		CASE @SortOrderIndex WHEN 0 THEN CC.Vague_Date_Start ELSE NULL END DESC, 
		CASE @SortOrderIndex WHEN 0 THEN CC.Vague_Date_End ELSE NULL END DESC, 
		CASE @SortOrderIndex WHEN 0 THEN T.Plaintext ELSE NULL END,
		CC.Ref_Number
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConditionChecks_Select_ForCollectionUnit') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConditionChecks_Select_ForCollectionUnit'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConditionChecks_Select_ForCollectionUnit TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConditionChecks_Select_ForCollectionUnit TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConditionChecks_Select_ForCollectionUnit TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ConditionChecks_Select_ForCollectionUnit TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConditionChecks_Select_ForCollectionUnit TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConditionChecks_Select_ForCollectionUnit TO [Dev - JNCC SQL]
END
GO
If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_ConditionChecks_Select_ForMovement]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_ConditionChecks_Select_ForMovement]
GO

/*===========================================================================*\
  Description:	Returns Condition Checks data to the CollectionsBrowser for a given Movement.

  Parameters:	
	@ParentKey 	When specified, only the records associated with the parent key are returned
	@SortOrderIndex	Index determining Sort Order

  Created:	August 2003

  Last revision information:
    $Revision: 5 $
    $Date: 17/08/11 15:40 $
    $Author: Jamesbichard $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_ConditionChecks_Select_ForMovement] 
	@ParentKey CHAR(16),
	@SortOrderIndex TINYINT
AS

SET NOCOUNT ON

IF @SortOrderIndex = 0
	SELECT 		CC.Conservation_Check_Key AS Item_Key, CC.Display_Caption, MCC.Movement_Conservation_Check_Key AS Join_Key
	FROM 		Movement_Conservation_Check MCC
	INNER JOIN	Conservation_Check CC ON MCC.Conservation_Check_Key = CC.Conservation_Check_Key AND MCC.Movement_Key = @ParentKey
	INNER JOIN 	Concept C ON CC.Type_Concept_Key = C.Concept_Key
	INNER JOIN 	Term T ON C.Term_Key = T.Term_Key
	ORDER BY 	CC.Vague_Date_Start DESC, CC.Vague_Date_End DESC, T.Plaintext, CC.Ref_Number
ELSE 
IF @SortOrderIndex = 1
	SELECT 		CC.Conservation_Check_Key AS Item_Key, CC.Display_Caption, MCC.Movement_Conservation_Check_Key AS Join_Key
	FROM 		Movement_Conservation_Check MCC
	INNER JOIN	Conservation_Check CC ON MCC.Conservation_Check_Key = CC.Conservation_Check_Key AND MCC.Movement_Key = @ParentKey
	ORDER BY 	CC.Ref_Number
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConditionChecks_Select_ForMovement') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConditionChecks_Select_ForMovement'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConditionChecks_Select_ForMovement TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConditionChecks_Select_ForMovement TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConditionChecks_Select_ForMovement TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ConditionChecks_Select_ForMovement TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConditionChecks_Select_ForMovement TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConditionChecks_Select_ForMovement TO [Dev - JNCC SQL]
END

GO
If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_ConditionChecks_Select_ForSearchByCheckedBy]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_ConditionChecks_Select_ForSearchByCheckedBy]
GO

CREATE PROCEDURE [dbo].[usp_ConditionChecks_Select_ForSearchByCheckedBy] 
@UserDomainMask INT,
@SessionID CHAR(16),
@SearchText VARCHAR(100),
@SortOrderIndex TINYINT

AS

--  DESCRIPTION
--  Returns Condition Checks data based on the search parameter for Checked By From
--
--  PARAMETERS
--	NAME				DESCRIPTION
--	@UserDomainMask		User's Domain Mask restricting which records may be returned
--	@SessionID 			User's SessionID
--	@SearchText			Text to be used for search
--	@SortOrderIndex		Index determining Sort Order
--
--
--  AUTHOR:				Ben Collier, Dorset Software
--  CREATED:			2003-09-15
--
SET NOCOUNT ON

IF @SortOrderIndex = 0
	SELECT CC.Conservation_Check_Key AS Item_Key, CC.Display_Caption,
		dbo.ufn_GetFormattedIndividualByParams(I.Title, I.Initials, I.Forename, I.Surname) AS Hint
	
	FROM 
	CONSERVATION_CHECK CC
		INNER JOIN 
			CONCEPT C
		ON CC.Type_Concept_Key = C.Concept_Key
			AND ((CC.Domain_Mask & @UserDomainMask > 0) OR (CC.Entered_Session_ID = @SessionID) 
				OR (CC.Changed_Session_ID = @SessionID) OR (CC.Domain_Mask = 0))
			INNER JOIN
				INDIVIDUAL I
			ON CC.Checked_By_Name_Key = I.Name_Key
				AND dbo.ufn_GetFormattedIndividualByParams(I.Title, I.Initials, I.Forename, I.Surname) LIKE @SearchText + '%'
		INNER JOIN 
			TERM T
		ON C.Term_Key = T.Term_Key
	ORDER BY CC.Vague_Date_Start DESC, CC.Vague_Date_End DESC, T.Plaintext, CC.Ref_Number
ELSE IF @SortOrderIndex = 1
	SELECT CC.Conservation_Check_Key AS Item_Key, CC.Display_Caption,
		dbo.ufn_GetFormattedIndividualByParams(I.Title, I.Initials, I.Forename, I.Surname) AS Hint
	
	FROM 
	CONSERVATION_CHECK CC
		INNER JOIN 
			CONCEPT C
		ON CC.Type_Concept_Key = C.Concept_Key
			AND ((CC.Domain_Mask & @UserDomainMask > 0) OR (CC.Entered_Session_ID = @SessionID) 
				OR (CC.Changed_Session_ID = @SessionID) OR (CC.Domain_Mask = 0))
			INNER JOIN
				INDIVIDUAL I
			ON CC.Checked_By_Name_Key = I.Name_Key
				AND dbo.ufn_GetFormattedIndividualByParams(I.Title, I.Initials, I.Forename, I.Surname) LIKE @SearchText + '%'
		INNER JOIN 
			TERM T
		ON C.Term_Key = T.Term_Key
	ORDER BY CC.Ref_Number

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConditionChecks_Select_ForSearchByCheckedBy') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConditionChecks_Select_ForSearchByCheckedBy'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConditionChecks_Select_ForSearchByCheckedBy TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConditionChecks_Select_ForSearchByCheckedBy TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConditionChecks_Select_ForSearchByCheckedBy TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ConditionChecks_Select_ForSearchByCheckedBy TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConditionChecks_Select_ForSearchByCheckedBy TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConditionChecks_Select_ForSearchByCheckedBy TO [Dev - JNCC SQL]
END

GO
If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_ConditionChecks_Select_ForTask]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_ConditionChecks_Select_ForTask]
GO

/*===========================================================================*\
  Description:	Returns Condition Checks data to the CollectionsBrowser for a given Tasks unit.

  Parameters:
	@UserDomainMask		User's Domain Mask restricting which records may be returned
	@SessionID 		User's SessionID
	@ParentKey 		When specified, only the records associated with the parent key are returned
	@SortOrderIndex		Index determining Sort Order

  Created:	August 2003

  Last revision information:
    $Revision: 5 $
    $Date: 17/08/11 15:40 $
    $Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConditionChecks_Select_ForTask] 
	@UserDomainMask INT,
	@SessionID CHAR(16),
	@ParentKey CHAR(16),
	@SortOrderIndex TINYINT
AS

SET NOCOUNT ON

	SELECT	 	CC.Conservation_Check_Key AS Item_Key, CC.Display_Caption,
			CC.Conservation_Check_Key AS Join_Key

	FROM 		Conservation_Task CT
	INNER JOIN 	Conservation_Check CC
		ON CT.Conservation_Check_Key = CC.Conservation_Check_Key 
		AND CT.Conservation_Task_Key = @ParentKey
		AND ((CC.Domain_Mask & @UserDomainMask > 0) OR (CC.Entered_Session_ID = @SessionID) 
			OR (CC.Changed_Session_ID = @SessionID) OR (CC.Domain_Mask = 0))

	INNER JOIN 	Concept C ON CC.Type_Concept_Key = C.Concept_Key
	INNER JOIN 	Term T ON C.Term_Key = T.Term_Key

	ORDER BY 
		-- 0: CC.Vague_Date_Start DESC, CC.Vague_Date_End DESC, Item_Name, CC.Ref_Number
		-- 1: CC.Ref_Number
		CASE @SortOrderIndex WHEN 0 THEN CC.Vague_Date_Start ELSE NULL END DESC, 
		CASE @SortOrderIndex WHEN 0 THEN CC.Vague_Date_End ELSE NULL END DESC, 
		CASE @SortOrderIndex WHEN 0 THEN T.Plaintext ELSE NULL END, 
		CC.Ref_Number
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConditionChecks_Select_ForTask') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConditionChecks_Select_ForTask'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConditionChecks_Select_ForTask TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConditionChecks_Select_ForTask TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConditionChecks_Select_ForTask TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ConditionChecks_Select_ForTask TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConditionChecks_Select_ForTask TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConditionChecks_Select_ForTask TO [Dev - JNCC SQL]
END
GO
If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_ConditionChecks_Select_ForTopLevel]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_ConditionChecks_Select_ForTopLevel]
GO

CREATE PROCEDURE [dbo].[usp_ConditionChecks_Select_ForTopLevel] 
@UserDomainMask INT,
@SessionID CHAR(16),
@SortOrderIndex TINYINT,
@Key CHAR(16) = NULL

AS

--  DESCRIPTION
--  Returns top level Condition Checks data to the CollectionsBrowser
--
--  PARAMETERS
--	NAME				DESCRIPTION
--	@Key 				Optional Key. When specified, only the single top level record is returned with that key
--	@UserDomainMask		User's Domain Mask restricting which records may be returned
--	@SessionID 			User's SessionID
--	@SortOrderIndex		Index determining Sort Order
--
--
--  AUTHOR:     		Ben Collier, Dorset Software
--  CREATED:    		2003-08-18
--
SET NOCOUNT ON

-- Create  a table to hold the items we are looking for
DECLARE @Search TABLE (ItemKey CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS PRIMARY KEY)

IF @Key IS NOT NULL
	INSERT INTO @Search VALUES (@Key)
ELSE IF object_id('tempdb..#TempFilter') is not null
	INSERT INTO @Search SELECT DISTINCT ItemKey FROM #TempFilter
ELSE
	INSERT INTO @Search SELECT Conservation_Check_Key FROM Conservation_Check

IF @SortOrderIndex = 0
BEGIN
	SELECT CC.Conservation_Check_Key AS Item_Key, CC.Display_Caption
	FROM 
	CONSERVATION_CHECK CC
		INNER JOIN 
			CONCEPT C
		ON CC.Type_Concept_Key = C.Concept_Key
			AND ((CC.Domain_Mask & @UserDomainMask > 0) OR (CC.Entered_Session_ID = @SessionID) 
				OR (CC.Changed_Session_ID = @SessionID) OR (CC.Domain_Mask = 0))
		INNER JOIN TERM T	ON C.Term_Key = T.Term_Key
		INNER JOIN @Search S ON S.ItemKey=CC.Conservation_Check_Key
	ORDER BY CC.Vague_Date_Start DESC, CC.Vague_Date_End DESC, T.Plaintext, CC.Ref_Number
END
ELSE IF @SortOrderIndex = 1
BEGIN
	SELECT CC.Conservation_Check_Key AS Item_Key, CC.Display_Caption
	FROM 
	CONSERVATION_CHECK CC
	INNER JOIN 
		CONCEPT C
	ON CC.Type_Concept_Key = C.Concept_Key
		AND ((CC.Domain_Mask & @UserDomainMask > 0) OR (CC.Entered_Session_ID = @SessionID) 
			OR (CC.Changed_Session_ID = @SessionID) OR (CC.Domain_Mask = 0))
	INNER JOIN TERM T	ON C.Term_Key = T.Term_Key
	INNER JOIN @Search S ON S.ItemKey=CC.Conservation_Check_Key
	ORDER BY CC.Ref_Number
END

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConditionChecks_Select_ForTopLevel') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConditionChecks_Select_ForTopLevel'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConditionChecks_Select_ForTopLevel TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConditionChecks_Select_ForTopLevel TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConditionChecks_Select_ForTopLevel TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ConditionChecks_Select_ForTopLevel TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConditionChecks_Select_ForTopLevel TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConditionChecks_Select_ForTopLevel TO [Dev - JNCC SQL]
END

GO
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_DeterminationsEarthSciences_Select_ForSearch') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_DeterminationsEarthSciences_Select_ForSearch]
GO

/*===========================================================================*\
  Description:	Returns Concept_Key and DisplayTerm when search characters are 
		entered.

  Parameters:	@SearchText
		@UserDomainMask

  Created:	October 2003

  Last revision information:
	$Revision: 5 $
	$Date: 17/08/11 15:40 $
	$Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_DeterminationsEarthSciences_Select_ForSearch] 
	@SearchText VARCHAR(100),
	@UserDomainMask INT
AS

	SET NOCOUNT ON

	SELECT 		VW.Concept_Key AS Item_Key,
			VW.Item_Name + ' - ' + CG.Item_Name AS DisplayTerm,
			VW.Item_Name + ' - ' + CG.Item_Name AS SearchTerm, 
			CG.Item_Name,
			VW.List_Preferred

	FROM		VW_ConceptTerm AS VW 
	INNER JOIN 	Concept_Group CG ON CG.Concept_Group_Key = VW.Concept_Group_Key
	INNER JOIN 	Local_Domain LD ON LD.Local_Domain_Key = CG.Local_Domain_Key
	INNER JOIN 	Domain D ON D.Domain_Key = LD.Domain_Key
			AND ((D.Domain_Mask & @UserDomainMask > 0) OR (D.Domain_Mask = 0))
			AND D.Has_Occurrences = 1
	-- Join to find out which concepts are mapped to taxa
	LEFT JOIN	Taxon_Dictionary_Concept_Mapping TDCM ON TDCM.Concept_Key = VW.Concept_Key
	LEFT JOIN	Search_Term ST ON ST.Concept_Key = VW.Concept_Key
	WHERE 		ST.PlainText LIKE @SearchText + '%'
	-- And filter out all concepts that are mapped to any taxon.
	AND		TDCM.Concept_Key IS NULL
	
	ORDER BY DisplayTerm
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_DeterminationsEarthSciences_Select_ForSearch') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_DeterminationsEarthSciences_Select_ForSearch'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_DeterminationsEarthSciences_Select_ForSearch TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_DeterminationsEarthSciences_Select_ForSearch TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_DeterminationsEarthSciences_Select_ForSearch TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_DeterminationsEarthSciences_Select_ForSearch TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_DeterminationsEarthSciences_Select_ForSearch TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_DeterminationsEarthSciences_Select_ForSearch TO [Dev - JNCC SQL]
END
GO
If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Determinations_Select_ForSearch]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Determinations_Select_ForSearch]
GO

CREATE PROCEDURE [dbo].[usp_Determinations_Select_ForSearch] 
@SearchText VARCHAR(100)

AS
--
--  DESCRIPTION
--  Returns Concept_Key and DisplayTerm when search characters are entered.
--
--	PARAMETERS
--	NAME			DESCRIPTION
--	@SearchText 		Search text used to find collections.
--
--  AUTHOR:			Anthony Simpson, Dorset Software
--  CREATED:			2003-10-20

SET NOCOUNT ON

	SELECT 		D.Concept_Key AS Item_Key, VW.Item_Name AS DisplayTerm, VW.Item_Name AS SearchTerm
	FROM		Determination AS D
	LEFT JOIN	Search_Term ST ON ST.Concept_Key = D.Concept_Key
	INNER JOIN	VW_ConceptTerm AS VW ON VW.Concept_Key = D.Concept_Key
	AND 		ST.Plaintext LIKE @SearchText + '%'
	
	UNION
	
	SELECT 		TD.Taxon_List_Item_Key AS Item_Key, ITN.Preferred_Name AS DisplayTerm, ITN.Preferred_Name AS SearchTerm
	FROM		Taxon_Determination AS TD
	INNER JOIN	Index_Taxon_Name AS ITN ON ITN.Taxon_List_Item_Key = TD.Taxon_List_Item_Key
	AND 		ITN.Preferred_Name LIKE @SearchText + '%'

	ORDER BY DisplayTerm
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Determinations_Select_ForSearch') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Determinations_Select_ForSearch'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Determinations_Select_ForSearch TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Determinations_Select_ForSearch TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Determinations_Select_ForSearch TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Determinations_Select_ForSearch TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Determinations_Select_ForSearch TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Determinations_Select_ForSearch TO [Dev - JNCC SQL]
END

GO
/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Domain_Insert]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Domain_Insert]
GO

/*===========================================================================*\
  Description:	Inserts a Domain record

  Parameters:	@Key 
		@ItemName 
		@SubjectAreaKey 
		@HasOccurrences 
		@DefaultHierarchyRelationTypeKey 
		@DomainMask
		@EnteredSessionID 
		@SystemSuppliedData 

  Created:	Oct 2003

  Last revision information:
    $Revision: 5 $
    $Date: 17/08/11 15:40 $
    $Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Domain_Insert]
	@Key char(16) OUTPUT,
	@ItemName varchar(100),
	@SubjectAreaKey char(16),
	@HasOccurrences bit,
	@DefaultHierarchyRelationTypeKey char(16),
	@DomainMask int,
	@TermGeneratorKey char(16),
	@SessionID char(16),
	@SystemSuppliedData bit = 0
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	EXECUTE spNextKey 'Domain', @Key OUTPUT

	BEGIN TRANSACTION
	
		INSERT INTO Domain (
			Domain_Key,
			Item_Name,
			Subject_Area_Key,
			Has_Occurrences,
			Default_Hierarchy_Relation_Type_Key,
			Domain_Mask,
			Term_Generator_Key,
			Entered_Session_ID,
			System_Supplied_Data
		) VALUES (
			@Key,
			@ItemName,
			@SubjectAreaKey,
			@HasOccurrences,
			@DefaultHierarchyRelationTypeKey,
			@DomainMask,
			@TermGeneratorKey,
			@SessionID,
			@SystemSuppliedData
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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Domain_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Domain_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Domain_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Domain_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Domain_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Domain_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Domain_Insert TO [Dev - JNCC SQL]
END
GO
/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Domain_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Domain_Select]
GO

/*===========================================================================*\
  Description:	Returns data for the Domain General tab page in the 
		Thesaurus Editor.

  Parameters:	@Key	

  Created:	November 2003

  Last revision information:
    $Revision: 5 $
    $Date: 17/08/11 15:40 $
    $Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Domain_Select]
	@Key char(16)
AS

SET NOCOUNT ON

	SELECT 		
			D.Item_Name, 
			D.Subject_Area_Key,
			D.Has_Occurrences,
			D.Default_Hierarchy_Relation_Type_Key,
			TRT.Item_Name AS Default_Hierarchy_Relation_Type_Name,
			D.Domain_Mask,
			D.Entered_Session_ID,
			D.Changed_Session_ID,
			D.System_Supplied_Data,
			D.Custodian,
			D.[Timestamp],
			D.Term_Generator_Key,
			TG.Item_Name as Term_Generator_Name
	FROM		Domain AS D
	LEFT JOIN	Thesaurus_Relation_Type AS TRT ON TRT.Thesaurus_Relation_Type_Key = D.Default_Hierarchy_Relation_Type_Key 
	LEFT JOIN	Term_Generator TG ON TG.Term_Generator_Key = D.Term_Generator_Key
	WHERE		D.Domain_Key = @Key

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Domain_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Domain_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Domain_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Domain_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Domain_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Domain_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Domain_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Domain_Select TO [Dev - JNCC SQL]
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
    $Revision: 5 $
    $Date: 17/08/11 15:40 $
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
	@SessionID char(16),
	@Timestamp timestamp
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

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
		END

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_DuplicateTerms_Merge]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
	DROP PROCEDURE [dbo].[usp_DuplicateTerms_Merge]
GO

/*===========================================================================*\
  Description:	
	Reassign records linked to @OldTermKey to @NewTermKey before deleting
	the @OldTermKey.

  Parameters:	
	@NewTermKey	Specify the key of the term to keep.
	@OldTermKey	Specify the key of the term to delete.

  Created:	
	January 2006

  Last revision information:
    $Revision: 5 $
    $Date: 17/08/11 15:40 $
    $Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_DuplicateTerms_Merge]
	@NewTermKey	CHAR(16),
	@OldTermKey	CHAR(16)
AS
	IF @NewTermKey <> @OldTermKey
	BEGIN
		SET NOCOUNT ON 

		-- Change over 
		UPDATE	Taxon_Dictionary_Term_Mapping
		SET	Term_Key = @NewTermKey
		WHERE	Term_Key = @OldTermKey

		UPDATE	Concept
		SET	Term_Key = @NewTermKey
		WHERE	Term_Key = @OldTermKey	

		/*========================================================*\
			Update term version records
		\*========================================================*/
		DECLARE @CurrentKey CHAR(16), @DuplicateKey CHAR(16), @UpdateTermKey BIT
		
		-- Update/delete term versions that use the old term key
		WHILE 7 = (6 + 1)
		BEGIN			
			-- Get the first term version which uses the old term key
			SELECT @CurrentKey = Term_Version_Key
			FROM Term_Version
			WHERE Term_Key = @OldTermKey

			-- If there are no more term versions using the old key, there is nothing to do
			IF @@RowCount = 0 BREAK

			-- Flags whether the current term version should be updated or deleted
			SET @UpdateTermKey = 0

			-- Check if we have already got a term version with the new term key which has
			-- the same authority and version label as the current term version. We also don't
			-- want the duplicate to be mapped to a taxon version - the only term versions we
			-- will remove are unmapped, and we don't want any linked concepts to suddenly get
			-- a term version which is mapped.		
			SELECT @DuplicateKey = tvduplicate.Term_Version_Key
			FROM Term_Version tvduplicate
			INNER JOIN Term_Version tvcurrent 
				ON ISNULL(tvduplicate.Version_Label, '') = ISNULL(tvcurrent.Version_Label, '')
				AND	ISNULL(tvduplicate.Author_And_Date, '') = ISNULL(tvcurrent.Author_And_Date, '')
			LEFT JOIN Taxon_Dictionary_Term_Version_Mapping tvm
				ON tvm.Term_Version_Key = tvduplicate.Term_Version_Key
			WHERE tvcurrent.Term_Version_Key = @CurrentKey AND tvduplicate.Term_Key = @NewTermKey
				AND tvm.Term_Version_Key IS NULL

			IF @@RowCount > 0
			BEGIN
				-- If we do, only keep the term version if it is already mapped to a taxon
				-- version (VI 24145). Otherwise remove the term version
				IF EXISTS (
					SELECT * FROM Taxon_Dictionary_Term_Version_Mapping
					WHERE Term_Version_Key = @CurrentKey)
				BEGIN
					SET @UpdateTermKey = 1
				END
			END
			ELSE
			BEGIN
				-- If the term version will not duplicate an existing term version when updated,
				-- update it.
				SET @UpdateTermKey = 1
			END
			
			-- Update term version to new term key/delete term version as required. In either
			-- case, the current term version will not be considered in the next iteration of the
			-- while loop since it will either no longer have the old term key or it will have
			-- been deleted.
			IF @UpdateTermKey = 1
			BEGIN
				UPDATE Term_Version
				SET Term_Key = @NewTermKey
				WHERE Term_Version_Key = @CurrentKey
			END
			ELSE
			BEGIN
				UPDATE Concept	
				SET Term_Version_Key = @DuplicateKey
				WHERE Term_Version_Key = @CurrentKey

				UPDATE Thesaurus_Fact	
				SET Term_Version_Key = @DuplicateKey
				WHERE Term_Version_Key = @CurrentKey

				DELETE FROM Term_Version
				WHERE Term_Version_Key = @CurrentKey
			END		
		END

		-- And finally remove the unnecessary leftover term.
		DELETE	Term
		WHERE	Term_Key = @OldTermKey

		SET NOCOUNT ON
	END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_DuplicateTerms_Merge') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_DuplicateTerms_Merge'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_DuplicateTerms_Merge TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_DuplicateTerms_Merge TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_DuplicateTerms_Merge TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_DuplicateTerms_Merge TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_DuplicateTerms_Merge TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_DuplicateTerms_Merge TO [Dev - JNCC SQL]
END

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_GetPreferredConceptsForGroup]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_GetPreferredConceptsForGroup]
GO

/*===========================================================================*\
  Description:	Returns all preferred concepts and their name for a given
		concept group,

  Parameters:	@ConceptGroupKey

  Created:	August 2003

  Last revision information:
    $Revision: 5 $
    $Date: 17/08/11 15:40 $
    $Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_GetPreferredConceptsForGroup]
	@ConceptGroupKey int
AS

	SELECT C.Concept_Key, C.Published_Term AS Item_Name FROM Concept C
    INNER JOIN Term T ON T.Term_Key = C.Term_Key
	    WHERE C.Concept_Group_Key = @ConceptGroupKey
        AND C.List_Preferred = 1
	AND C.Is_Current = 1
	ORDER BY C.Sort_Code, T.Plaintext
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_GetPreferredConceptsForGroup') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_GetPreferredConceptsForGroup'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_GetPreferredConceptsForGroup TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_GetPreferredConceptsForGroup TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_GetPreferredConceptsForGroup TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_GetPreferredConceptsForGroup TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_GetPreferredConceptsForGroup TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_GetPreferredConceptsForGroup TO [Dev - JNCC SQL]
END

GO
/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ListSynonyms_Select_ForConcept]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ListSynonyms_Select_ForConcept]
GO

/*===========================================================================*\
  Description:	Returns List Synonyms

  Parameters:	@Key	Concept_Key

  Created:	December 2003

  Last revision information:
    $Revision: 5 $
    $Date: 17/08/11 15:40 $
    $Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ListSynonyms_Select_ForConcept]
	@Key char(16)
AS

SET NOCOUNT ON

	/*=============================*\
	  Get the list synonyms.
	\*=============================*/
	SELECT 		CListSynonyms.Concept_Key AS Item_Key,
			CListSynonyms.Published_Term AS Item_Name,
			T.Language_Key,
			L.Item_Name AS Language,
			CListSynonyms.Custodian,
			S.User_Name_Key AS Entered_By
	FROM 		Concept AS CSource
	INNER JOIN	Concept AS CListSynonyms 	ON CListSynonyms.Meaning_Key = CSource.Meaning_Key 
							AND CListSynonyms.Concept_Group_Key = CSource.Concept_Group_Key
							AND CListSynonyms.Concept_Key <> @Key
	INNER JOIN	Term AS T 			ON T.Term_Key = CListSynonyms.Term_Key
	INNER JOIN 	Language AS L			ON L.Language_Key=T.Language_Key
	LEFT JOIN		Term_Version AS TV 		ON TV.Term_Version_Key = CListSynonyms.Term_Version_Key
	LEFT JOIN Session S ON S.Session_ID=CListSynonyms.Entered_Session_ID
	WHERE 		CSource.Concept_key = @Key

	ORDER BY 	T.Plaintext

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ListSynonyms_Select_ForConcept') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ListSynonyms_Select_ForConcept'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ListSynonyms_Select_ForConcept TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ListSynonyms_Select_ForConcept TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ListSynonyms_Select_ForConcept TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ListSynonyms_Select_ForConcept TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ListSynonyms_Select_ForConcept TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ListSynonyms_Select_ForConcept TO [Dev - JNCC SQL]
END

GO
/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_LocalDomain_Insert]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_LocalDomain_Insert]
GO
/*===========================================================================*\
  Description:	Inserts a Local_Domain record
  Parameters:	@Key 
		@ItemName 
		@DomainKey 
		@LanguageKey 
		@ConceptGroupLabel 
		@SessionID 
		@SystemSuppliedData (Optional)

  Created:	Oct 2003

  Last revision information:
    $Revision: 5 $
    $Date: 17/08/11 15:40 $
    $Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_LocalDomain_Insert]
	@Key char(16) OUTPUT,
	@ItemName varchar(100),
	@DomainKey char(16),
	@LanguageKey varchar(4),
	@ConceptGroupLabel varchar(50),
	@TermGeneratorKey char(16),
	@SessionID char(16),
	@SystemSuppliedData bit = 0
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	EXECUTE spNextKey 'Local_Domain', @Key OUTPUT

	BEGIN TRANSACTION

		INSERT INTO Local_Domain (
			Local_Domain_Key,
			Item_Name,
			Domain_Key,
			Language_Key,
			Concept_Group_Label,
			Term_Generator_Key,
			Entered_Session_ID,
			System_Supplied_Data
		) VALUES (
			@Key,
			@ItemName,
			@DomainKey,
			@LanguageKey,
			CASE WHEN @ConceptGroupLabel IS NULL THEN 'Concept Group' ELSE @ConceptGroupLabel END,
			@TermGeneratorKey,
			@SessionID,
			@SystemSuppliedData 
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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_LocalDomain_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_LocalDomain_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_LocalDomain_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_LocalDomain_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_LocalDomain_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_LocalDomain_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_LocalDomain_Insert TO [Dev - JNCC SQL]
END
GO
/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_LocalDomain_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_LocalDomain_Select]
GO

/*===========================================================================*\
  Description:	Returns data for the Local Domain General tab page in the 
		Thesaurus Editor.

  Parameters:	@Key	

  Created:	November 2003

  Last revision information:
    $Revision: 5 $
    $Date: 17/08/11 15:40 $
    $Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_LocalDomain_Select]
	@Key char(16)
AS

SET NOCOUNT ON

	SELECT 		LD.Item_Name, 
			LD.Domain_Key,
			LD.Language_Key,
			L.Item_Name AS Language_Name,
			LD.Concept_Group_Label,
			LD.Entered_Session_ID,
			LD.Changed_Session_ID,
			LD.Term_Generator_Key,
			TG.Item_Name as Term_Generator_Name,
			LD.System_Supplied_Data,
			LD.Custodian,
			LD.[Timestamp]
	FROM		Local_Domain AS LD
	INNER JOIN	Language AS L ON L.Language_Key = LD.Language_Key
	LEFT JOIN	Term_Generator TG ON TG.Term_Generator_Key = LD.Term_Generator_Key
	WHERE		LD.Local_Domain_Key = @Key

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_LocalDomain_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_LocalDomain_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_LocalDomain_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_LocalDomain_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_LocalDomain_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_LocalDomain_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_LocalDomain_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_LocalDomain_Select TO [Dev - JNCC SQL]
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
    $Revision: 5 $
    $Date: 17/08/11 15:40 $
    $Author: Jamesbichard $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_LocalDomain_Update]
	@Key char(16),
	@ItemName varchar(100),
	@LanguageKey varchar(4),
	@ConceptGroupLabel varchar(50),
	@TermGeneratorKey char(16),
	@SessionID char(16),
	@Timestamp timestamp
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

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
		END

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
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
If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_NumberHistory_Select_ForCollectionUnit]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_NumberHistory_Select_ForCollectionUnit]
GO

/*===========================================================================*\
  Description:	Returns Numbering History for a specified Collection

  Parameters:	
	@ParentKey	When specified, only the records associated with the parent key are returned

  Created:	August 2003

  Last revision information:
    $Revision: 5 $
    $Date: 17/08/11 15:40 $
    $Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_NumberHistory_Select_ForCollectionUnit] 
	@ParentKey CHAR(16)
AS

SET NOCOUNT ON

	SELECT 		CUN.Collection_Unit_Number_Key AS Item_Key, C.Published_Term AS Item_Name, CUN.Number, 
			CUN.Preferred AS xPreferred, 0 AS 'ISAccession', CUN.Collection_Unit_Number_Key AS Join_Key

	FROM 		Collection_Unit_Number CUN
	INNER JOIN 	Concept C ON CUN.Type_Concept_Key = C.Concept_Key AND (CUN.Collection_Unit_Key = @ParentKey)
	
	UNION

	SELECT 		M.Movement_Key AS Item_Key, C.Published_Term AS Item_Name, M.Number, 
			1 AS xPreferred, 1 AS 'ISAccession', MCU.Movement_Collection_Unit_Key AS Join_Key

	FROM 		Movement_Collection_Unit MCU
	INNER JOIN	Movement_Direction MD
		ON MCU.Movement_Direction_Key = MD.Movement_Direction_Key 
		AND (MCU.Collection_Unit_Key = @ParentKey)
		AND (MD.Outbound = 0)

	INNER JOIN	Movement M ON MD.Movement_Key = M.Movement_Key AND M.Movement_Type IN (0, 1)
	LEFT JOIN	Concept C ON C.Concept_Key = 'SYSTEM0000000006' --ACCESSION NUMBER
	
	ORDER BY 	xPreferred DESC, IsAccession, Item_Name
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_NumberHistory_Select_ForCollectionUnit') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_NumberHistory_Select_ForCollectionUnit'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_NumberHistory_Select_ForCollectionUnit TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_NumberHistory_Select_ForCollectionUnit TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_NumberHistory_Select_ForCollectionUnit TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_NumberHistory_Select_ForCollectionUnit TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_NumberHistory_Select_ForCollectionUnit TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_NumberHistory_Select_ForCollectionUnit TO [Dev - JNCC SQL]
END

GO
/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Occurrences_Select_ForSearch]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Occurrences_Select_ForSearch]
GO

/*===========================================================================*\
  Description:	Returns a list of Occurrences

  Parameters:	@SearchText

  Created:	November 2003

  Last revision information:
    $Revision: 5 $
    $Date: 17/08/11 15:40 $
    $Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Occurrences_Select_ForSearch]
	@SearchText varchar(150)
AS

SET NOCOUNT ON

	SELECT DISTINCT O.Occurrence_Key AS [Item_Key],
			CT.Item_Name + ' - ' +
			dbo.ufn_GetDateFromVagueDate(S.Vague_Date_Start, S.Vague_Date_End, S.Vague_Date_Type) +
			' - ' + 
			CASE 
				WHEN LN.Item_Name IS NULL THEN
					CASE WHEN S.Spatial_Ref IS NULL THEN '' ELSE S.Spatial_Ref END
				ELSE LN.Item_Name + ' (' + 
					CASE 
						WHEN S.Spatial_Ref IS NULL THEN L.Spatial_Ref
						ELSE S.Spatial_Ref END +
					')'
			END
			AS SearchTerm,
			CT.Item_Name + ' - ' +
			dbo.ufn_GetDateFromVagueDate(S.Vague_Date_Start, S.Vague_Date_End, S.Vague_Date_Type) +
			' - ' + 
			CASE 
				WHEN LN.Item_Name IS NULL THEN
					CASE WHEN S.Spatial_Ref IS NULL THEN '' ELSE S.Spatial_Ref END
				ELSE LN.Item_Name + ' (' + 
					CASE 
						WHEN S.Spatial_Ref IS NULL THEN L.Spatial_Ref
						ELSE S.Spatial_Ref END +
					')'
			END
			AS DisplayTerm
	FROM		Occurrence O
	INNER JOIN	Determination D ON O.Occurrence_Key = D.Occurrence_Key 
	INNER JOIN	vw_ConceptTermPreferred CT ON D.Concept_Key = CT.Concept_Key
	LEFT JOIN	Search_Term ST ON ST.Concept_Key = CT.Concept_Key
	INNER JOIN 	Sample S ON S.Sample_Key = O.Sample_Key
	LEFT JOIN	Location L ON L.Location_Key = S.Location_Key 
	LEFT JOIN	Location_Name LN ON LN.Location_Key = L.Location_Key AND LN.Preferred = 1
	WHERE		ST.PlainText LIKE @SearchText + '%'
	ORDER BY	SearchTerm

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Occurrences_Select_ForSearch') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Occurrences_Select_ForSearch'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Occurrences_Select_ForSearch TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Occurrences_Select_ForSearch TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Occurrences_Select_ForSearch TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Occurrences_Select_ForSearch TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Occurrences_Select_ForSearch TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Occurrences_Select_ForSearch TO [Dev - JNCC SQL]
END

GO
/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Parents_Select_ForConcept]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Parents_Select_ForConcept]
GO

/*===========================================================================*\
  Description:	Returns parent concepts for a given concept key.

  Parameters:	@Key	Concept_Key

  Created:	January 2004

  Last revision information:
    $Revision: 5 $
    $Date: 17/08/11 15:40 $
    $Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Parents_Select_ForConcept]
	@Key char(16)
AS

SET NOCOUNT ON

	/*===================================================================*\
	  Get the Default_Hierarchy_Relation_Type_Key for the domain.
	\*===================================================================*/
	DECLARE @Hierarchy_Relation_Type_Key char(16)

	SELECT 		@Hierarchy_Relation_Type_Key = CG.Hierarchy_Relation_Type_Key
	FROM		Concept_Group AS CG
	INNER JOIN	Concept AS C ON C.Concept_Group_Key = CG.Concept_Group_Key
	WHERE		C.Concept_Key = @Key

	/*================*\
	  Get the parents.
	\*================*/
	SELECT DISTINCT	C.Concept_Key AS Item_Key,
			C.Published_Term AS Item_Name
	FROM Concept CChild
	INNER JOIN Concept CChildSyn ON CChildSyn.Meaning_Key=CChild.Meaning_Key
			AND CChildSyn.Concept_Group_Key=CChild.Concept_Group_Key
	INNER JOIN Concept_Relation AS CR ON CR.To_Concept_Key=CChildSyn.Concept_Key
	INNER JOIN	Concept AS C ON C.Concept_Key = CR.From_Concept_Key
	WHERE		CChild.Concept_Key = @Key
	AND		CR.Thesaurus_Relation_Type_Key = @Hierarchy_Relation_Type_Key

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Parents_Select_ForConcept') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Parents_Select_ForConcept'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Parents_Select_ForConcept TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Parents_Select_ForConcept TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Parents_Select_ForConcept TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Parents_Select_ForConcept TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Parents_Select_ForConcept TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Parents_Select_ForConcept TO [Dev - JNCC SQL]
END

GO
If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_PeopleOrganisationsOther_Select_ForCollectionUnit]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_PeopleOrganisationsOther_Select_ForCollectionUnit]
GO

/*===========================================================================*\
  Description:	Returns Related Names for a specified Collection

  Parameters:	
	@ParentKey	When specified, only the records associated with the parent key are returned

  Created:	August 2003

  Last revision information:
    $Revision: 5 $
    $Date: 17/08/11 15:40 $
    $Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_PeopleOrganisationsOther_Select_ForCollectionUnit] 
@ParentKey CHAR(16)

AS

SET NOCOUNT ON

	SELECT 		N.Name_Key AS Rec_Item_Key, CUN.Collection_Unit_Name_Key AS Item_Key, N.Name_Key AS Join_Key,
			CASE 	WHEN N.Organisation = 0 THEN dbo.ufn_GetFormattedIndividualByParams(I.Title, I.Initials, I.Forename, I.Surname) + ' - ' + C.Published_Term
				ELSE 
					CASE 	WHEN O.Acronym IS NULL THEN O.Full_Name + ' - ' + C.Published_Term
						ELSE O.Acronym + ', ' + O.Full_Name + ' - ' + C.Published_Term
					END
			END AS Item_Name

	FROM 		Collection_Unit_Name CUN
	INNER JOIN	[Name] N ON CUN.Name_Key = N.Name_Key AND CUN.Collection_Unit_Key = @ParentKey
	INNER JOIN	Concept C ON CUN.Relation_Type_Concept_KEY = C.CONCEPT_KEY
	LEFT JOIN	Individual I ON N.Name_Key = I.Name_Key
	LEFT JOIN	Organisation O ON N.Name_Key = O.Name_Key

	WHERE 		(I.Name_Key IS NOT NULL) OR (O.Name_Key IS NOT NULL)

	ORDER BY 	Item_Name

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_PeopleOrganisationsOther_Select_ForCollectionUnit') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_PeopleOrganisationsOther_Select_ForCollectionUnit'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_PeopleOrganisationsOther_Select_ForCollectionUnit TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_PeopleOrganisationsOther_Select_ForCollectionUnit TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_PeopleOrganisationsOther_Select_ForCollectionUnit TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_PeopleOrganisationsOther_Select_ForCollectionUnit TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_PeopleOrganisationsOther_Select_ForCollectionUnit TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_PeopleOrganisationsOther_Select_ForCollectionUnit TO [Dev - JNCC SQL]
END

GO
SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

/*============================================================================*\
  Drop stored proc before re-creating.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_PotentialSynonyms_Select_ForImportedConcept') IS NOT NULL
	DROP PROCEDURE dbo.usp_PotentialSynonyms_Select_ForImportedConcept
GO

/*============================================================================*\
  Description:  List-preferred potential synonyms of the specified concept.

  Parameters:   @concept_key            Concept key

  Created:      Jan 2004

  Last revision information:
	$Revision: 5 $
	$Date: 17/08/11 15:40 $
	$Author: Jamesbichard $
\*============================================================================*/
CREATE PROCEDURE dbo.usp_PotentialSynonyms_Select_ForImportedConcept
	@concept_key        CHAR(16)
AS
	SET NOCOUNT ON

	DECLARE     @src_meaning_key    CHAR(16),
				@src_group_key      CHAR(16)

	SELECT      @src_meaning_key    =   Meaning_Key,
				@src_group_key      =   Concept_Group_Key
	FROM        Concept
	WHERE       Concept_Key         =   @concept_key

	/* work out all current synonyms of the concept */
	DECLARE     @current_synonyms   TABLE (
		Language_Key        CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS,
		Plaintext           NVARCHAR(300) COLLATE SQL_Latin1_General_CP1_CI_AI
		PRIMARY KEY (Language_Key, Plaintext))

	INSERT      @current_synonyms (
				Language_Key,
				Plaintext)
	SELECT DISTINCT
				t.Language_Key,
				t.Plaintext
	FROM        Concept             AS  c
	INNER JOIN  Term                AS  t
	ON          t.Term_Key          =   c.Term_Key
	WHERE       c.Meaning_Key       =   @src_meaning_key

	IF @@ERROR <> 0 RETURN        

	/* work out all list-preferred potential synonyms */
	DECLARE     @potential  TABLE (
			Concept_Key         CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS PRIMARY KEY,
			Concept_Group_Key   CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS,
			Meaning_Key         CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS,
			Term_Key            CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS,
			Author_Copy         VARCHAR(100) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
			Different_Group     BIT,
			Session_Start       DATETIME)

	INSERT      @potential (
				Concept_Key,
				Concept_Group_Key,
				Meaning_Key,
				Term_Key,
				Author_Copy,
				Different_Group,
				Session_Start)
	SELECT DISTINCT
				psyn.Concept_Key,
				psyn.Concept_Group_Key,
				psyn.Meaning_Key,
				psyn.Term_Key,
				psyn.Author_Copy,
				CASE psyn.Concept_Group_Key
					WHEN @src_group_key THEN 0
					ELSE 1
				END,
				s.Date_Time_Start
	FROM        @current_synonyms   AS  curr
	INNER JOIN  Term                AS  tpot WITH (INDEX (IX_Plaintext))
	ON          tpot.Plaintext      =   curr.Plaintext
	AND         tpot.Language_Key   =   curr.Language_Key
	INNER JOIN  Concept             AS  pot
	ON          pot.Term_Key        =   tpot.Term_Key
	INNER JOIN  Concept             AS  psyn
	ON          psyn.Meaning_Key    =   pot.Meaning_Key
	AND         psyn.List_Preferred =   1
	INNER JOIN  Session             AS  s
	ON          s.Session_ID        =   psyn.Entered_Session_ID
	WHERE       pot.Meaning_Key     <>  @src_meaning_key  /* not currently a synonym */

	IF @@ERROR <> 0 RETURN

	/* select most recently entered list-preferred concept for each
	 * potential synonym */
	SELECT      p.Concept_Key       AS  Item_Key,
				c.Published_Term	AS	Item_Name,
				g.Concept_Group_Key	AS	Group_Key,
				g.Item_Name         AS  Group_Name,
				g.Authority
	FROM        @potential          AS  p
	INNER JOIN	dbo.Concept			AS	c
	ON			p.Concept_Key		=	c.Concept_Key
	INNER JOIN  Concept_Group       AS  g
	ON          g.Concept_Group_Key =   p.Concept_Group_Key
	WHERE       p.Session_Start     =   (   SELECT      TOP 1 Session_Start
											FROM        @potential      AS  p2
											WHERE       p2.Meaning_Key  =   p.Meaning_Key
											ORDER BY    p2.Different_Group,
														p2.Session_Start DESC)
GO

/*============================================================================*\
  Grant permissions.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_PotentialSynonyms_Select_ForImportedConcept') IS NOT NULL
BEGIN
	PRINT 'Setting up security on procedure usp_PotentialSynonyms_Select_ForImportedConcept'
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_AddOnly')
			GRANT EXECUTE ON dbo.usp_PotentialSynonyms_Select_ForImportedConcept TO R2k_AddOnly
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_PotentialSynonyms_Select_ForImportedConcept TO R2k_Administrator
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_PotentialSynonyms_Select_ForImportedConcept TO R2k_FullEdit
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_PotentialSynonyms_Select_ForImportedConcept TO R2k_ReadOnly
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_PotentialSynonyms_Select_ForImportedConcept TO R2k_RecordCardsOnly
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
    $Revision: 5 $
    $Date: 17/08/11 15:40 $
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
					CSource.Published_Term		AS	SourceConcept,
					TSource.PlainText,			-- There for the ORDER BY
					CPotSyn.Concept_Key			AS	SynonymConceptKey,
					CPotSyn.Meaning_Key			AS	SynonymMeaningKey,
					CGPotSyn.Item_Name			AS	SynonymGroup,
					ISNULL
					(
						TVPotSyn.Author_And_Date,
						''
					)							AS	SynonymAuthority,
					CPotSyn.Published_Term		AS	SynonymConcept,
					CPotentials.Published_Term	AS	SharedTerm,
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

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Processes_Select_ForCollectionUnit]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Processes_Select_ForCollectionUnit]
GO

CREATE PROCEDURE [dbo].[usp_Processes_Select_ForCollectionUnit] 
@ParentKey CHAR(16)

AS
--  DESCRIPTION
--  Returns Processes data to the CollectionsBrowser for a given Collection Unit
--
--  PARAMETERS
--	NAME				DESCRIPTION
--	@ParentKey 			When specified, only the records associated with the parent key are returned
--
--
--  AUTHOR:     		Ben Collier, Dorset Software
--  CREATED:    		2003-08-25
--
SET NOCOUNT ON

SELECT DISTINCT Collection_Unit_Process_Key AS Item_Key, C.Published_Term AS Item_Name
FROM 
	Collection_Unit_Process CUP
	INNER JOIN 
		CONCEPT C
	ON CUP.Process_Concept_Key = C.Concept_Key AND CUP.Collection_Unit_Key = @ParentKey
ORDER BY Item_Name

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Processes_Select_ForCollectionUnit') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Processes_Select_ForCollectionUnit'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Processes_Select_ForCollectionUnit TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Processes_Select_ForCollectionUnit TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Processes_Select_ForCollectionUnit TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Processes_Select_ForCollectionUnit TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Processes_Select_ForCollectionUnit TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Processes_Select_ForCollectionUnit TO [Dev - JNCC SQL]
END

GO
SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

/*============================================================================*\
	Drop stored proc before re-creating.
\*============================================================================*/
IF OBJECT_ID(N'dbo.usp_SearchTerm_Delete') IS NOT NULL
	DROP PROCEDURE dbo.usp_SearchTerm_Delete
GO

/*============================================================================*\
	Description:
		Delete a search term

	Parameters:
		

	Created: August 2011

	Last revision information:
		$Revision: 5 $
		$Date: 17/08/11 15:40 $
		$Author: Jamesbichard $
\*============================================================================*/

CREATE PROCEDURE dbo.usp_SearchTerm_Delete
	@Key char(16)
AS
	DELETE Search_Term
	WHERE Search_Term_Key = @Key
GO

/*============================================================================*\
	Grant permissions.
\*============================================================================*/
PRINT 'Setting up security on procedure usp_SearchTerm_Delete'

IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
    GRANT EXECUTE ON dbo.usp_SearchTerm_Delete TO R2k_AddOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
	GRANT EXECUTE ON dbo.usp_SearchTerm_Delete TO R2k_Administrator
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
	GRANT EXECUTE ON dbo.usp_SearchTerm_Delete TO R2k_FullEdit
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
	GRANT EXECUTE ON dbo.usp_SearchTerm_Delete TO R2k_ReadOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
	GRANT EXECUTE ON dbo.usp_SearchTerm_Delete TO R2k_RecordCardsOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
    GRANT EXECUTE ON dbo.usp_SearchTerm_Delete TO "Dev - JNCC SQL"
GO
SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

/*============================================================================*\
	Drop stored proc before re-creating.
\*============================================================================*/
IF OBJECT_ID(N'dbo.usp_SearchTerm_DeleteOldTerms') IS NOT NULL
	DROP PROCEDURE dbo.usp_SearchTerm_DeleteOldTerms
GO

/*============================================================================*\
	Description:
		Clears out old system generated search terms before inserting new ones

	Parameters:
		

	Created: August 2011

	Last revision information:
		$Revision: 5 $
		$Date: 17/08/11 15:40 $
		$Author: Jamesbichard $
\*============================================================================*/

CREATE PROCEDURE dbo.usp_SearchTerm_DeleteOldTerms
	@ConceptKey char(16)
AS
	SET NOCOUNT ON

	DELETE FROM Search_Term
	WHERE Concept_Key = @ConceptKey
	AND System_Generated = 1

GO

/*============================================================================*\
	Grant permissions.
\*============================================================================*/
PRINT 'Setting up security on procedure usp_SearchTerm_DeleteOldTerms'

IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
    GRANT EXECUTE ON dbo.usp_SearchTerm_DeleteOldTerms TO R2k_AddOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
	GRANT EXECUTE ON dbo.usp_SearchTerm_DeleteOldTerms TO R2k_Administrator
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
	GRANT EXECUTE ON dbo.usp_SearchTerm_DeleteOldTerms TO R2k_FullEdit
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
	GRANT EXECUTE ON dbo.usp_SearchTerm_DeleteOldTerms TO R2k_ReadOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
	GRANT EXECUTE ON dbo.usp_SearchTerm_DeleteOldTerms TO R2k_RecordCardsOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
    GRANT EXECUTE ON dbo.usp_SearchTerm_DeleteOldTerms TO "Dev - JNCC SQL"
GO
/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].usp_SearchTerm_Generate')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].usp_SearchTerm_Generate
GO

/*===========================================================================*\
  Description:	Generate search terms for a concept

  Parameters:	

  Created:	August 2011

  Last revision information:
    $Revision: 5 $
    $Date: 17/08/11 15:40 $
    $Author: Jamesbichard $

\*===========================================================================*/

CREATE PROCEDURE dbo.usp_SearchTerm_Generate 
	@ConceptKey CHAR(16), 
	@TermGeneratorKey CHAR(16)
AS
	DECLARE @SearchTermsProc NVARCHAR(257)

	SELECT @SearchTermsProc = Search_Term_Procedure
	FROM Term_Generator
	WHERE Term_Generator_Key = @TermGeneratorKey

	DECLARE @Plaintext NVARCHAR(150),
			@AuthorAndDate VARCHAR(100),
			@Attributes VARCHAR(100),
			@RankKey CHAR(16),
			@ParentKey CHAR(16),
			@PublishedTerm NVARCHAR(450),
			@HierarchyRelationTypeKey CHAR(16)
	SELECT 
		@Plaintext = t.Plaintext,
		@AuthorAndDate = tv.Author_And_Date,
		@Attributes = tv.Version_Label,
		@RankKey = c.Concept_Rank_Key,
		@PublishedTerm = c.Published_Term
	FROM Concept c
	LEFT JOIN Term_Version tv ON tv.Term_Version_Key = c.Term_Version_Key
	INNER JOIN Term t ON t.Term_Key = c.Term_Key
	WHERE c.Concept_Key	= @ConceptKey

	SELECT @HierarchyRelationTypeKey = cg.Hierarchy_Relation_Type_Key
	FROM Concept_Group cg
	INNER JOIN Concept c ON c.Concept_Group_Key = cg.Concept_Group_Key
	WHERE c.Concept_Key = @ConceptKey

	IF @HierarchyRelationTypeKey IS NOT NULL
	BEGIN
		CREATE TABLE #ParentKeys (
			Parent_Key CHAR(16),
			Item_Name VARCHAR(100),
			Sort_Code INT,
			HasParents BIT,
			Rank_Key CHAR(16)
		)

		INSERT INTO #ParentKeys
		EXEC usp_Concept_Select_ForChild @ConceptKey, @HierarchyRelationTypeKey
	
		SELECT @ParentKey = Parent_Key
		FROM #ParentKeys
	
		DROP TABLE #ParentKeys
	END

	DECLARE @sql NVARCHAR(4000)
	DECLARE @params NVARCHAR(500)

	SELECT @sql = '
		CREATE TABLE #SearchTerms (
			Term NVARCHAR(450))

		INSERT INTO #SearchTerms
		EXEC ' + @SearchTermsProc + ' @Plaintext, @AuthorAndDate, ' + 
				'@Attributes, @RankKey, ' +
				'@ParentKey, @PublishedTerm
		
		DECLARE @term NVARCHAR(450),
				@searchTermKey CHAR(16)
		DECLARE terms CURSOR LOCAL FAST_FORWARD FOR
		SELECT * FROM #SearchTerms
		
		OPEN terms

		WHILE 1 = 1
		BEGIN
			FETCH		terms
			INTO        @term
	
			IF @@FETCH_STATUS <> 0 BREAK
			EXEC usp_SearchTerm_Insert @searchTermKey, ''' + @ConceptKey + ''', 1, @term
		END

		CLOSE terms
		DEALLOCATE terms
		DROP TABLE #SearchTerms'

	SET @params = '@Plaintext NVARCHAR(150), @PublishedTerm NVARCHAR(450), ' +
					'@AuthorAndDate VARCHAR(100), @Attributes VARCHAR(100), ' +
					'@RankKey CHAR(16), @ParentKey CHAR(16)'

	EXEC sp_executesql @sql, @params, 
			@Plaintext = @Plaintext,
			@PublishedTerm = @PublishedTerm,
			@AuthorAndDate = @AuthorAndDate,
			@Attributes = @Attributes,
			@RankKey = @RankKey,
			@ParentKey = @ParentKey
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SearchTerm_Generate') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SearchTerm_Generate'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
			GRANT EXECUTE ON dbo.usp_SearchTerm_Generate TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SearchTerm_Generate TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SearchTerm_Generate TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SearchTerm_Generate TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SearchTerm_Generate TO [Dev - JNCC SQL]
END
GO

SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

/*============================================================================*\
	Drop stored proc before re-creating.
\*============================================================================*/
IF OBJECT_ID(N'dbo.usp_SearchTerm_Insert') IS NOT NULL
	DROP PROCEDURE dbo.usp_SearchTerm_Insert
GO

/*============================================================================*\
	Description:
		Insert search term

	Parameters:
		

	Created: August 2011

	Last revision information:
		$Revision: 5 $
		$Date: 17/08/11 15:40 $
		$Author: Jamesbichard $
\*============================================================================*/

CREATE PROCEDURE dbo.usp_SearchTerm_Insert
	@Key char(16) OUTPUT,
	@ConceptKey char(16),
	@SystemGenerated bit,
	@Plaintext nvarchar(450)
AS
	SET NOCOUNT ON
	
	EXECUTE spNextKey 'Search_Term', @Key OUTPUT

	INSERT INTO Search_Term(Search_Term_Key, Concept_Key, Plaintext, System_Generated)
	VALUES (@Key, @ConceptKey, @Plaintext, @SystemGenerated)
GO

/*============================================================================*\
	Grant permissions.
\*============================================================================*/
PRINT 'Setting up security on procedure usp_SearchTerm_Insert'

IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
    GRANT EXECUTE ON dbo.usp_SearchTerm_Insert TO R2k_AddOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
	GRANT EXECUTE ON dbo.usp_SearchTerm_Insert TO R2k_Administrator
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
	GRANT EXECUTE ON dbo.usp_SearchTerm_Insert TO R2k_FullEdit
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
	GRANT EXECUTE ON dbo.usp_SearchTerm_Insert TO R2k_ReadOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
	GRANT EXECUTE ON dbo.usp_SearchTerm_Insert TO R2k_RecordCardsOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
    GRANT EXECUTE ON dbo.usp_SearchTerm_Insert TO "Dev - JNCC SQL"
GO
SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

/*============================================================================*\
	Drop stored proc before re-creating.
\*============================================================================*/
IF OBJECT_ID(N'dbo.usp_SearchTerm_Select') IS NOT NULL
	DROP PROCEDURE dbo.usp_SearchTerm_Select
GO

/*============================================================================*\
	Description:
		Select search terms

	Parameters:
		

	Created: August 2011

	Last revision information:
		$Revision: 5 $
		$Date: 17/08/11 15:40 $
		$Author: Jamesbichard $
\*============================================================================*/

CREATE PROCEDURE dbo.usp_SearchTerm_Select
	@ConceptKey char(16),
	@SystemGenerated bit = null
AS
	SELECT 
		Search_Term_Key, 
		Plaintext,
		null as Custodian,
		null as Timestamp
	FROM Search_Term
	WHERE Concept_Key = @ConceptKey
	AND (System_Generated = @SystemGenerated OR @SystemGenerated IS NULL)
GO

/*============================================================================*\
	Grant permissions.
\*============================================================================*/
PRINT 'Setting up security on procedure usp_SearchTerm_Select'

IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
    GRANT EXECUTE ON dbo.usp_SearchTerm_Select TO R2k_AddOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
	GRANT EXECUTE ON dbo.usp_SearchTerm_Select TO R2k_Administrator
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
	GRANT EXECUTE ON dbo.usp_SearchTerm_Select TO R2k_FullEdit
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
	GRANT EXECUTE ON dbo.usp_SearchTerm_Select TO R2k_ReadOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
	GRANT EXECUTE ON dbo.usp_SearchTerm_Select TO R2k_RecordCardsOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
    GRANT EXECUTE ON dbo.usp_SearchTerm_Select TO "Dev - JNCC SQL"
GO
SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

/*============================================================================*\
	Drop stored proc before re-creating.
\*============================================================================*/
IF OBJECT_ID(N'dbo.usp_SearchTerm_Update') IS NOT NULL
	DROP PROCEDURE dbo.usp_SearchTerm_Update
GO

/*============================================================================*\
	Description:
		Updates search term

	Parameters:
		

	Created: August 2011

	Last revision information:
		$Revision: 5 $
		$Date: 17/08/11 15:40 $
		$Author: Jamesbichard $
\*============================================================================*/

CREATE PROCEDURE dbo.usp_SearchTerm_Update
	@Key char(16),
	@Plaintext nvarchar(450)
AS
	SET NOCOUNT ON
	
	UPDATE Search_Term
	SET Plaintext = @Plaintext
	WHERE Search_Term_Key = @Key
GO

/*============================================================================*\
	Grant permissions.
\*============================================================================*/
PRINT 'Setting up security on procedure usp_SearchTerm_Update'

IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
    GRANT EXECUTE ON dbo.usp_SearchTerm_Update TO R2k_AddOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
	GRANT EXECUTE ON dbo.usp_SearchTerm_Update TO R2k_Administrator
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
	GRANT EXECUTE ON dbo.usp_SearchTerm_Update TO R2k_FullEdit
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
	GRANT EXECUTE ON dbo.usp_SearchTerm_Update TO R2k_ReadOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
	GRANT EXECUTE ON dbo.usp_SearchTerm_Update TO R2k_RecordCardsOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
    GRANT EXECUTE ON dbo.usp_SearchTerm_Update TO "Dev - JNCC SQL"
GO
SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

/*============================================================================*\
	Drop stored proc before re-creating.
\*============================================================================*/
IF OBJECT_ID(N'dbo.usp_Search_Term_GenerateTermsUsingDefaultRule') IS NOT NULL
	DROP PROCEDURE dbo.usp_Search_Term_GenerateTermsUsingDefaultRule
GO

/*============================================================================*\
	Description:
		The default stored procedure used to generate the search terms for a
		concept.

	Parameters:
		@Plaintext:			The base term.
		@AuthorAndDate:		The author and date.
		@Attributes:		Additional attributes.
		@RankKey:			Identifies the rank of the concept.
		@ParentConceptKey:	Identifies the parent concept, if any.
		@PublishedTerm:		The published term.

	Created: July 2011

	Last revision information:
		$Revision: 5 $
		$Date: 17/08/11 15:40 $
		$Author: Jamesbichard $
\*============================================================================*/

CREATE PROCEDURE dbo.usp_Search_Term_GenerateTermsUsingDefaultRule
	@Plaintext NVARCHAR(150),
	@AuthorAndDate VARCHAR(100),
	@Attributes VARCHAR(100),
	@RankKey CHAR(16),
	@ParentConceptKey CHAR(16),
	@PublishedTerm NVARCHAR(256)
AS
	SET NOCOUNT ON
	
	DECLARE @SearchTerm TABLE
	(
		Search_Term NVARCHAR(450)
	)

	INSERT INTO	@SearchTerm
	SELECT		@Plaintext

	IF @PublishedTerm <> @Plaintext
	BEGIN
		INSERT INTO	@SearchTerm
		SELECT		dbo.ufn_RemoveHtmlMarkup(@PublishedTerm)
	END
	
	IF @AuthorAndDate IS NOT NULL AND LEN(@AuthorAndDate) > 0
	BEGIN
		INSERT INTO	@SearchTerm
		SELECT		@Plaintext + ' ' + @AuthorAndDate
	END

	IF @Attributes IS NOT NULL AND LEN(@Attributes) > 0
	BEGIN
		INSERT INTO	@SearchTerm
		SELECT		@Plaintext + ' ' + @Attributes
	END

	IF @AuthorAndDate IS NOT NULL AND LEN(@AuthorAndDate) > 0 
		AND @Attributes IS NOT NULL AND LEN(@Attributes) > 0
	BEGIN
		INSERT INTO	@SearchTerm
		SELECT		@Plaintext + ' ' + @AuthorAndDate + ' ' + @Attributes
	END

	SELECT		Search_Term
	FROM		@SearchTerm
GO

/*============================================================================*\
	Grant permissions.
\*============================================================================*/
PRINT 'Setting up security on procedure usp_Search_Term_GenerateTermsUsingDefaultRule'

IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
    GRANT EXECUTE ON dbo.usp_Search_Term_GenerateTermsUsingDefaultRule TO R2k_AddOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
	GRANT EXECUTE ON dbo.usp_Search_Term_GenerateTermsUsingDefaultRule TO R2k_Administrator
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
	GRANT EXECUTE ON dbo.usp_Search_Term_GenerateTermsUsingDefaultRule TO R2k_FullEdit
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
	GRANT EXECUTE ON dbo.usp_Search_Term_GenerateTermsUsingDefaultRule TO R2k_ReadOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
	GRANT EXECUTE ON dbo.usp_Search_Term_GenerateTermsUsingDefaultRule TO R2k_RecordCardsOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
    GRANT EXECUTE ON dbo.usp_Search_Term_GenerateTermsUsingDefaultRule TO "Dev - JNCC SQL"
GO
SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

/*============================================================================*\
  Drop stored proc before re-creating.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_Specimens_Select_ForCollection') IS NOT NULL
	DROP PROCEDURE dbo.usp_Specimens_Select_ForCollection
GO

/*============================================================================*\
  Description:	Returns specimens' data to the Collections Browser for a
				specified collection.

  Parameters:	@UserDomainMask			User's Domain Mask restricting which
										records may be returned
				@SessionID				User's SessionID
				@ParentKey				Identifies the collection
				@ShowCommonNames		Specifies whether Common Names are shown
				@ShowOriginalSpecimenNames
										0 => preferred names are shown
										1 => names originally entered are shown
				@SortOrderIndex			0 => results sorted by Item_Name, Number
										1 => results sorted by Number, Item_Name

  Created:		August 2003

  Last revision information:
	$Revision: 5 $
	$Date: 17/08/11 15:40 $
	$Author: Jamesbichard $
\*============================================================================*/
CREATE PROCEDURE dbo.usp_Specimens_Select_ForCollection
	@UserDomainMask 					INT,
	@SessionID 							CHAR(16),
	@ParentKey 							CHAR(16),
	@ShowCommonNames 					BIT,
	@ShowOriginalSpecimenNames			BIT,
	@SortOrderIndex 					TINYINT
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

	-- Use temp table to build output, so silly data errors such as duplicate accessions
	-- don't duplicate in the list
	DECLARE @SpecimensSearch TABLE
	(
		Item_Key		CHAR(16) 		COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		Join_Key		CHAR(16) 		COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		Det_Item_Key	CHAR(16) 		COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		Det_Item_Name	NVARCHAR(150) 	COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		Item_Name		NVARCHAR(150) 	COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		Life_Sciences	BIT NULL,
		Number			VARCHAR(30) 	COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		Hint			NVARCHAR(150) 	COLLATE SQL_Latin1_General_CP1_CI_AS NULL 
	)

	INSERT INTO	@SpecimensSearch (Item_Key, Join_Key, Life_Sciences) 
	SELECT 	DISTINCT SU.Collection_Unit_Key, SU.Collection_Unit_Key, SU.Life_Sciences
	FROM 	Specimen_Unit 	SU
	JOIN 	Collection_Unit CU 	ON 	SU.Collection_Unit_Key = CU.Collection_Unit_Key
								AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
								OR 	(CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
	WHERE 	SU.Parent_Collection_Collection_Unit_Key = @ParentKey

	UPDATE		@SpecimensSearch
	SET			Number = CUN.Number
	FROM 		@SpecimensSearch 		SU
	LEFT JOIN 	Collection_Unit_Number 	CUN ON 	SU.Item_key 	= CUN.Collection_Unit_Key 
											AND CUN.Preferred	= 1

	UPDATE 	@SpecimensSearch
	SET		Det_Item_Key	= CPref.Concept_Key,
			Item_Name 		= CASE @ShowOriginalSpecimenNames
								WHEN 1 THEN C.Published_Term
								ELSE CPref.Published_Term END,
			Det_Item_Name	= TDet.Plaintext
	FROM 	@SpecimensSearch 		SU
	JOIN 	VW_SpecimenDetsEarth 	SDE 	ON 	SDE.Collection_Unit_Key			= SU.Item_Key
											AND SDE.Preferred_Determination_Key	= SDE.Determination_Key
	JOIN 	Concept 				C 		ON 	C.Concept_Key					= SDE.Concept_Key
	JOIN 	Term 					TDet 	ON 	TDet.Term_Key					= C.Term_Key
	JOIN 	Concept 				CPref 	ON 	CPref.Meaning_Key				= C.Meaning_Key
											AND CPref.List_Preferred			= 1
											AND CPref.Concept_Group_Key			= C.Concept_Group_Key
	
	UPDATE 	@SpecimensSearch
	SET		Det_Item_Key	= SDL.Taxon_List_Item_Key,
			Item_Name 		= dbo.ufn_GetFormattedTaxonNameByParams(
				CASE @ShowOriginalSpecimenNames
					WHEN 1 THEN Actual_Name
					ELSE Preferred_Name END,
				CASE @ShowOriginalSpecimenNames
					WHEN 1 THEN Actual_Name_Italic
					ELSE Preferred_Name_Italic END,
				Common_Name,
				Common_Name_Italic,
				NULL,
				@ShowCommonNames),
			Det_Item_Name	= ITN.Actual_Name
	FROM 	@SpecimensSearch 	SU
	JOIN 	VW_SpecimenDetsLife SDL ON 	SDL.Collection_Unit_Key					= SU.Item_Key
									AND SDL.Preferred_Taxon_Determination_Key 	= SDL.Taxon_Determination_Key
	JOIN 	Index_Taxon_Name 	ITN	ON 	ITN.Taxon_List_Item_Key					= SDL.Taxon_List_Item_Key

	-- Select table and sort appropriately
	IF @SortOrderIndex = 0
		SELECT * FROM @SpecimensSearch
		ORDER BY Item_Name, Number
	ELSE 
	IF @SortOrderIndex = 1
		SELECT * FROM @SpecimensSearch
		ORDER BY Number, Item_Name
GO

/*============================================================================*\
  Grant permissions.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_Specimens_Select_ForCollection') IS NOT NULL
BEGIN
	PRINT 'Setting up security on procedure usp_Specimens_Select_ForCollection'
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForCollection TO R2k_AddOnly
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForCollection TO R2k_Administrator
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForCollection TO R2k_FullEdit
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForCollection TO R2k_ReadOnly
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForCollection TO R2k_RecordCardsOnly
END
GO
SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

/*============================================================================*\
  Drop stored proc before re-creating.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_Specimens_Select_ForConditionCheck') IS NOT NULL
	DROP PROCEDURE dbo.usp_Specimens_Select_ForConditionCheck
GO

/*===========================================================================*\
  Description:	Returns specimens associated with a specified condition check

  Parameters:	@UserDomainMask			User's Domain Mask restricting which
										records may be returned
				@SessionID 				User's SessionID
				@ParentKey 				Identifies the condition check
				@ShowCommonNames		Specifies whether Common Names are shown
				@ShowOriginalSpecimenNames
										0 => preferred names are shown
										1 => names originally entered are shown
				@SortOrderIndex			0 => results sorted by Item_Name, Number
										1 => results sorted by Number, Item_Name

  Created:		August 2003

  Last revision information:
    $Revision: 5 $
    $Date: 17/08/11 15:40 $
    $Author: Jamesbichard $
\*===========================================================================*/
CREATE PROCEDURE dbo.usp_Specimens_Select_ForConditionCheck
	@UserDomainMask						INT,
	@SessionID							CHAR(16),
	@ParentKey							CHAR(16),
	@ShowCommonNames					BIT,
	@ShowOriginalSpecimenNames			BIT,
	@SortOrderIndex						TINYINT
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

	DECLARE @SpecimensSearch TABLE
	(
		Item_Key		CHAR(16) 		COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		Join_Key		CHAR(16) 		COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		Det_Item_Key	CHAR(16) 		COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		Det_Item_Name	NVARCHAR(150) 	COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		Item_Name		NVARCHAR(150) 	COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		Life_Sciences	BIT NULL,
		Number			VARCHAR(30) 	COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		Hint			NVARCHAR(150) 	COLLATE SQL_Latin1_General_CP1_CI_AS NULL 
	)
	
	INSERT INTO	@SpecimensSearch (
				Item_Key,
				Join_Key,
				Life_Sciences)
	SELECT		s.Collection_Unit_Key,
				k.Collection_Unit_Check_Key,
				s.Life_Sciences
	FROM 		Collection_Unit_Check				AS	k
	INNER JOIN	Specimen_Unit						AS	s
	ON			s.Collection_Unit_Key				=	k.Collection_Unit_Key
	INNER JOIN	Collection_Unit						AS	cu
	ON			cu.Collection_Unit_Key				=	s.Collection_Unit_Key
	WHERE		k.Conservation_Check_Key			=	@ParentKey
	AND			(cu.Domain_Mask & @UserDomainMask	>	0
	OR			cu.Entered_Session_ID				=	@SessionID
	OR			cu.Changed_Session_ID				=	@SessionID
	OR			cu.Domain_Mask						=	0)
	
	UPDATE		@SpecimensSearch
	SET			Number = CUN.Number
	FROM 		@SpecimensSearch 		SU
	LEFT JOIN 	Collection_Unit_Number 	CUN ON 	SU.Item_key 	= CUN.Collection_Unit_Key 
											AND CUN.Preferred	= 1

	UPDATE 	@SpecimensSearch
	SET		Det_Item_Key	= CPref.Concept_Key,
			Item_Name 		= CASE @ShowOriginalSpecimenNames
								WHEN 1 THEN C.Published_Term
								ELSE CPref.Published_Term END,
			Det_Item_Name	= TDet.Plaintext
	FROM 	@SpecimensSearch 		SU
	JOIN 	VW_SpecimenDetsEarth 	SDE 	ON 	SDE.Collection_Unit_Key			= SU.Item_Key
											AND SDE.Preferred_Determination_Key	= SDE.Determination_Key
	JOIN 	Concept 				C 		ON 	C.Concept_Key					= SDE.Concept_Key
	JOIN 	Term 					TDet 	ON 	TDet.Term_Key					= C.Term_Key
	JOIN 	Concept 				CPref 	ON 	CPref.Meaning_Key				= C.Meaning_Key
											AND CPref.List_Preferred			= 1
											AND CPref.Concept_Group_Key			= C.Concept_Group_Key
	
	UPDATE 	@SpecimensSearch
	SET		Det_Item_Key	= SDL.Taxon_List_Item_Key,
			Item_Name 		= dbo.ufn_GetFormattedTaxonNameByParams(
				CASE @ShowOriginalSpecimenNames
					WHEN 1 THEN Actual_Name
					ELSE Preferred_Name END,
				CASE @ShowOriginalSpecimenNames
					WHEN 1 THEN Actual_Name_Italic
					ELSE Preferred_Name_Italic END,
				Common_Name,
				Common_Name_Italic,
				NULL,
				@ShowCommonNames),
			Det_Item_Name	= ITN.Actual_Name
	FROM 	@SpecimensSearch 	SU
	JOIN 	VW_SpecimenDetsLife SDL ON 	SDL.Collection_Unit_Key					= SU.Item_Key
									AND SDL.Preferred_Taxon_Determination_Key 	= SDL.Taxon_Determination_Key
	JOIN 	Index_Taxon_Name 	ITN	ON 	ITN.Taxon_List_Item_Key					= SDL.Taxon_List_Item_Key

	-- Select table and sort appropriately
	IF @SortOrderIndex = 0
		SELECT * FROM @SpecimensSearch
		ORDER BY Item_Name, Number
	ELSE 
	IF @SortOrderIndex = 1
		SELECT * FROM @SpecimensSearch
		ORDER BY Number, Item_Name
GO

/*============================================================================*\
  Grant permissions.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_Specimens_Select_ForConditionCheck') IS NOT NULL
BEGIN
	PRINT 'Setting up security on procedure usp_Specimens_Select_ForConditionCheck'
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForConditionCheck TO R2k_AddOnly
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForConditionCheck TO R2k_Administrator
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForConditionCheck TO R2k_FullEdit
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForConditionCheck TO R2k_ReadOnly
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForConditionCheck TO R2k_RecordCardsOnly
END
GO
SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

/*============================================================================*\
  Drop stored proc before re-creating.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_Specimens_Select_ForEnquiry') IS NOT NULL
	DROP PROCEDURE dbo.usp_Specimens_Select_ForEnquiry
GO

/*============================================================================*\
  Description:	Returns specimens for a specified enquiry.

  Parameters:	@UserDomainMask			User's Domain Mask restricting which
										records may be returned
				@SessionID 				User's SessionID
				@ParentKey 				Identifies the enquiry
				@ShowCommonNames		Specifies whether Common Names are shown
				@ShowOriginalSpecimenNames
										0 => preferred names are shown
										1 => names originally entered are shown
				@SortOrderIndex			0 => results sorted by Item_Name, Number
										1 => results sorted by Number, Item_Name

  Created:		August 2003

  Last revision information:
	$Revision: 5 $
	$Date: 17/08/11 15:40 $
	$Author: Jamesbichard $
\*============================================================================*/
CREATE PROCEDURE dbo.usp_Specimens_Select_ForEnquiry
	@UserDomainMask						INT,
	@SessionID							CHAR(16),
	@ParentKey							CHAR(16),
	@ShowCommonNames					BIT,
	@ShowOriginalSpecimenNames			BIT,
	@SortOrderIndex						TINYINT
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

	DECLARE @SpecimensSearch TABLE
	(
		Item_Key		CHAR(16) 		COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		Join_Key		CHAR(16) 		COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		Det_Item_Key	CHAR(16) 		COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		Det_Item_Name	NVARCHAR(150) 	COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		Item_Name		NVARCHAR(150) 	COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		Life_Sciences	BIT NULL,
		Number			VARCHAR(30) 	COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		Hint			NVARCHAR(150) 	COLLATE SQL_Latin1_General_CP1_CI_AS NULL 
	)
	
	INSERT INTO	@SpecimensSearch (
				Item_Key,
				Join_Key,
				Life_Sciences)
	SELECT		s.Collection_Unit_Key,
				q.Collection_Unit_Enquiry_Key,
				s.Life_Sciences
	FROM 		Collection_Unit_Enquiry				AS	q
	INNER JOIN	Specimen_Unit						AS	s
	ON			s.Collection_Unit_Key				=	q.Collection_Unit_Key
	INNER JOIN	Collection_Unit						AS	cu
	ON			cu.Collection_Unit_Key				=	s.Collection_Unit_Key
	WHERE		q.Enquiry_Key						=	@ParentKey
	AND			(cu.Domain_Mask & @UserDomainMask	>	0
	OR			cu.Entered_Session_ID				=	@SessionID
	OR			cu.Changed_Session_ID				=	@SessionID
	OR			cu.Domain_Mask						=	0)
	
	UPDATE		@SpecimensSearch
	SET			Number = CUN.Number
	FROM 		@SpecimensSearch 		SU
	LEFT JOIN 	Collection_Unit_Number 	CUN ON 	SU.Item_key 	= CUN.Collection_Unit_Key 
											AND CUN.Preferred	= 1

	UPDATE 	@SpecimensSearch
	SET		Det_Item_Key	= CPref.Concept_Key,
			Item_Name 		= CASE @ShowOriginalSpecimenNames
								WHEN 1 THEN C.Published_Term
								ELSE CPref.Published_Term END,
			Det_Item_Name	= TDet.Plaintext
	FROM 	@SpecimensSearch 		SU
	JOIN 	VW_SpecimenDetsEarth 	SDE 	ON 	SDE.Collection_Unit_Key			= SU.Item_Key
											AND SDE.Preferred_Determination_Key	= SDE.Determination_Key
	JOIN 	Concept 				C 		ON 	C.Concept_Key					= SDE.Concept_Key
	JOIN 	Term 					TDet 	ON 	TDet.Term_Key					= C.Term_Key
	JOIN 	Concept 				CPref 	ON 	CPref.Meaning_Key				= C.Meaning_Key
											AND CPref.List_Preferred			= 1
											AND CPref.Concept_Group_Key			= C.Concept_Group_Key
	
	UPDATE 	@SpecimensSearch
	SET		Det_Item_Key	= SDL.Taxon_List_Item_Key,
			Item_Name 		= dbo.ufn_GetFormattedTaxonNameByParams(
				CASE @ShowOriginalSpecimenNames
					WHEN 1 THEN Actual_Name
					ELSE Preferred_Name END,
				CASE @ShowOriginalSpecimenNames
					WHEN 1 THEN Actual_Name_Italic
					ELSE Preferred_Name_Italic END,
				Common_Name,
				Common_Name_Italic,
				NULL,
				@ShowCommonNames),
			Det_Item_Name	= ITN.Actual_Name
	FROM 	@SpecimensSearch 	SU
	JOIN 	VW_SpecimenDetsLife SDL ON 	SDL.Collection_Unit_Key					= SU.Item_Key
									AND SDL.Preferred_Taxon_Determination_Key 	= SDL.Taxon_Determination_Key
	JOIN 	Index_Taxon_Name 	ITN	ON 	ITN.Taxon_List_Item_Key					= SDL.Taxon_List_Item_Key

	-- Select table and sort appropriately
	IF @SortOrderIndex = 0
		SELECT * FROM @SpecimensSearch
		ORDER BY Item_Name, Number
	ELSE 
	IF @SortOrderIndex = 1
		SELECT * FROM @SpecimensSearch
		ORDER BY Number, Item_Name
GO

/*============================================================================*\
  Grant permissions.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_Specimens_Select_ForEnquiry') IS NOT NULL
BEGIN
	PRINT 'Setting up security on procedure usp_Specimens_Select_ForEnquiry'
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForEnquiry TO R2k_AddOnly
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForEnquiry TO R2k_Administrator
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForEnquiry TO R2k_FullEdit
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForEnquiry TO R2k_ReadOnly
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForEnquiry TO R2k_RecordCardsOnly
END
GO
SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

/*============================================================================*\
  Drop stored proc before re-creating.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_Specimens_Select_ForJob') IS NOT NULL
	DROP PROCEDURE dbo.usp_Specimens_Select_ForJob
GO

/*============================================================================*\
  Description:	Returns specimens associated with a specified job.

  Parameters:	@UserDomainMask			User's Domain Mask restricting which
										records may be returned
				@SessionID 				User's SessionID
				@ParentKey 				Identifies the job
				@ShowCommonNames		Specifies whether Common Names are shown
				@ShowOriginalSpecimenNames
										0 => preferred names are shown
										1 => names originally entered are shown
				@SortOrderIndex			0 => results sorted by Item_Name, Number
										1 => results sorted by Number, Item_Name

  Created:		August 2003

  Last revision information:
    $Revision: 5 $
    $Date: 17/08/11 15:40 $
    $Author: Jamesbichard $
\*============================================================================*/
CREATE PROCEDURE dbo.usp_Specimens_Select_ForJob
	@UserDomainMask						INT,
	@SessionID							CHAR(16),
	@ParentKey							CHAR(16),
	@ShowCommonNames					BIT,
	@ShowOriginalSpecimenNames			BIT,
	@SortOrderIndex						TINYINT
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

	DECLARE @SpecimensSearch TABLE
	(
		Item_Key		CHAR(16) 		COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		Join_Key		CHAR(16) 		COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		Det_Item_Key	CHAR(16) 		COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		PlainText		NVARCHAR(150) 	COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		Item_Name		NVARCHAR(150) 	COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		Life_Sciences	BIT NULL,
		Number			VARCHAR(30) 	COLLATE SQL_Latin1_General_CP1_CI_AS NULL 
	)
	
	INSERT INTO	@SpecimensSearch (
				Item_Key,
				Join_Key,
				Life_Sciences)
	SELECT		s.Collection_Unit_Key,
				t.Collection_Unit_Task_Key,
				s.Life_Sciences
	FROM 		Conservation_Task					AS	ct
	INNER JOIN	Collection_Unit_Task				AS	t
	ON			t.Conservation_Task_Key				=	ct.Conservation_Task_Key
	INNER JOIN	Specimen_Unit						AS	s
	ON			s.Collection_Unit_Key				=	t.Collection_Unit_Key
	INNER JOIN	Collection_Unit						AS	cu
	ON			cu.Collection_Unit_Key				=	s.Collection_Unit_Key
	WHERE		ct.Conservation_Job_Key				=	@ParentKey
	AND			(cu.Domain_Mask & @UserDomainMask	>	0
	OR			cu.Entered_Session_ID				=	@SessionID
	OR			cu.Changed_Session_ID				=	@SessionID
	OR			cu.Domain_Mask						=	0)
	
	UPDATE		@SpecimensSearch
	SET			Number = CUN.Number
	FROM 		@SpecimensSearch 		SU
	LEFT JOIN 	Collection_Unit_Number 	CUN ON 	SU.Item_key 	= CUN.Collection_Unit_Key 
											AND CUN.Preferred	= 1

	UPDATE 	@SpecimensSearch
	SET		Det_Item_Key	= CPref.Concept_Key,
			Item_Name 		= CASE @ShowOriginalSpecimenNames
								WHEN 1 THEN C.Published_Term
								ELSE CPref.Published_Term END,
			PlainText		= TDet.Plaintext
	FROM 	@SpecimensSearch 		SU
	JOIN 	VW_SpecimenDetsEarth 	SDE 	ON 	SDE.Collection_Unit_Key			= SU.Item_Key
											AND SDE.Preferred_Determination_Key	= SDE.Determination_Key
	JOIN 	Concept 				C 		ON 	C.Concept_Key					= SDE.Concept_Key
	JOIN 	Term 					TDet 	ON 	TDet.Term_Key					= C.Term_Key
	JOIN 	Concept 				CPref 	ON 	CPref.Meaning_Key				= C.Meaning_Key
											AND CPref.List_Preferred			= 1
											AND CPref.Concept_Group_Key			= C.Concept_Group_Key
	
	UPDATE 	@SpecimensSearch
	SET		Det_Item_Key	= SDL.Taxon_List_Item_Key,
			Item_Name 		= dbo.ufn_GetFormattedTaxonNameByParams(
				CASE @ShowOriginalSpecimenNames
					WHEN 1 THEN Actual_Name
					ELSE Preferred_Name END,
				CASE @ShowOriginalSpecimenNames
					WHEN 1 THEN Actual_Name_Italic
					ELSE Preferred_Name_Italic END,
				Common_Name,
				Common_Name_Italic,
				NULL,
				@ShowCommonNames),
			PlainText		= ITN.Actual_Name
	FROM 	@SpecimensSearch 	SU
	JOIN 	VW_SpecimenDetsLife SDL ON 	SDL.Collection_Unit_Key					= SU.Item_Key
									AND SDL.Preferred_Taxon_Determination_Key 	= SDL.Taxon_Determination_Key
	JOIN 	Index_Taxon_Name 	ITN	ON 	ITN.Taxon_List_Item_Key					= SDL.Taxon_List_Item_Key

	-- Select table and sort appropriately
	IF @SortOrderIndex = 0
		SELECT * FROM @SpecimensSearch
		ORDER BY Item_Name, Number
	ELSE 
	IF @SortOrderIndex = 1
		SELECT * FROM @SpecimensSearch
		ORDER BY Number, Item_Name
GO

/*============================================================================*\
  Grant permissions.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_Specimens_Select_ForJob') IS NOT NULL
BEGIN
	PRINT 'Setting up security on procedure usp_Specimens_Select_ForJob'
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForJob TO R2k_AddOnly
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForJob TO R2k_Administrator
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForJob TO R2k_FullEdit
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForJob TO R2k_ReadOnly
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForJob TO R2k_RecordCardsOnly
END
GO
SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

/*============================================================================*\
  Drop stored proc before re-creating.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_Specimens_Select_ForLinkedOther') IS NOT NULL
	DROP PROCEDURE dbo.usp_Specimens_Select_ForLinkedOther
GO

/*============================================================================*\
  Description:	Returns specimens that are related to a particular specimen.

  Parameters:	@UserDomainMask			User's Domain Mask restricting which
										records may be returned
				@SessionID 				User's SessionID
				@ParentKey 				Identifies the specimen
				@ShowCommonNames		Specifies whether Common Names are shown
				@ShowOriginalSpecimenNames
										0 => preferred names are shown
										1 => names originally entered are shown
				@SortOrderIndex			0 => results sorted by Item_Name, Number
										1 => results sorted by Number, Item_Name

  Created:		August 2003

  Last revision information:
    $Revision: 5 $
    $Date: 17/08/11 15:40 $
    $Author: Jamesbichard $
\*============================================================================*/
CREATE PROCEDURE dbo.usp_Specimens_Select_ForLinkedOther
	@UserDomainMask						INT,
	@SessionID							CHAR(16),
	@ParentKey							CHAR(16),
	@ShowCommonNames					BIT,
	@ShowOriginalSpecimenNames			BIT,
	@SortOrderIndex						TINYINT
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

	DECLARE @SpecimensSearch TABLE
	(
		Item_Key			CHAR(16) 		COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
		Join_Key			CHAR(16) 		COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
		Hyperlink_Item_Key	CHAR(16) 		COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
		Drag_Drop_Item_Key	CHAR(16) 		COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
		Det_Item_Key		CHAR(16) 		COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		Item_Name			NVARCHAR(150) 	COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		Life_Sciences		BIT NULL,
		Number				VARCHAR(30) 	COLLATE SQL_Latin1_General_CP1_CI_AS NULL 
	)
	
	INSERT INTO	@SpecimensSearch (
				Item_Key,
				Join_Key,
				Hyperlink_Item_Key,
				Drag_Drop_Item_Key,
				Life_Sciences)
	SELECT		cr.Collection_Unit_Relation_Key,
				cr.Collection_Unit_Relation_Key,
				s.Collection_Unit_Key,
				s.Collection_Unit_Key,
				s.Life_Sciences
	FROM 		Collection_Unit_Relation			AS	cr
	INNER JOIN	Thesaurus_Relation_Type				AS	rt
	ON			rt.Thesaurus_Relation_Type_Key		=	cr.Thesaurus_Relation_Type_Key
	INNER JOIN	Semantic_Relation					AS	sr
	ON			sr.Semantic_Relation_Key			=	rt.Semantic_Relation_Key
	INNER JOIN	Specimen_Unit						AS	s
	ON			s.Collection_Unit_Key				=	CASE WHEN cr.From_Collection_Unit_Key = @ParentKey
															THEN cr.To_Collection_Unit_Key
															ELSE cr.From_Collection_Unit_Key
														END
	INNER JOIN	Collection_Unit						AS	cu
	ON			cu.Collection_Unit_Key				=	s.Collection_Unit_Key
	WHERE		(cr.From_Collection_Unit_Key		=	@ParentKey
	OR			(sr.Unidirectional					=	0
	AND			cr.To_Collection_Unit_Key			=	@ParentKey))
	AND			(cu.Domain_Mask & @UserDomainMask	>	0
	OR			cu.Entered_Session_ID				=	@SessionID
	OR			cu.Changed_Session_ID				=	@SessionID
	OR			cu.Domain_Mask						=	0)
	
	UPDATE		@SpecimensSearch
	SET			Number = CUN.Number
	FROM 		@SpecimensSearch 		SU
	LEFT JOIN 	Collection_Unit_Number 	CUN ON 	SU.Hyperlink_Item_Key	= CUN.Collection_Unit_Key 
											AND CUN.Preferred	= 1

	UPDATE 	@SpecimensSearch
	SET		Det_Item_Key	= CPref.Concept_Key,
			Item_Name 		= CASE @ShowOriginalSpecimenNames
								WHEN 1 THEN C.Published_Term
								ELSE CPref.Published_Term END
	FROM 	@SpecimensSearch 		SU
	JOIN 	VW_SpecimenDetsEarth 	SDE 	ON 	SDE.Collection_Unit_Key			= SU.Hyperlink_Item_Key
											AND SDE.Preferred_Determination_Key	= SDE.Determination_Key
	JOIN 	Concept 				C 		ON 	C.Concept_Key					= SDE.Concept_Key
	JOIN 	Term 					TDet 	ON 	TDet.Term_Key					= C.Term_Key
	JOIN 	Concept 				CPref 	ON 	CPref.Meaning_Key				= C.Meaning_Key
											AND CPref.List_Preferred			= 1
											AND CPref.Concept_Group_Key			= C.Concept_Group_Key
	
	UPDATE 	@SpecimensSearch
	SET		Det_Item_Key	= SDL.Taxon_List_Item_Key,
			Item_Name 		= dbo.ufn_GetFormattedTaxonNameByParams(
				CASE @ShowOriginalSpecimenNames
					WHEN 1 THEN Actual_Name
					ELSE Preferred_Name END,
				CASE @ShowOriginalSpecimenNames
					WHEN 1 THEN Actual_Name_Italic
					ELSE Preferred_Name_Italic END,
				Common_Name,
				Common_Name_Italic,
				NULL,
				@ShowCommonNames)
	FROM 	@SpecimensSearch 	SU
	JOIN 	VW_SpecimenDetsLife SDL ON 	SDL.Collection_Unit_Key					= SU.Hyperlink_Item_Key
									AND SDL.Preferred_Taxon_Determination_Key 	= SDL.Taxon_Determination_Key
	JOIN 	Index_Taxon_Name 	ITN	ON 	ITN.Taxon_List_Item_Key					= SDL.Taxon_List_Item_Key

	-- Select table and sort appropriately
	IF @SortOrderIndex = 0
		SELECT * FROM @SpecimensSearch
		ORDER BY Item_Name, Number
	ELSE 
	IF @SortOrderIndex = 1
		SELECT * FROM @SpecimensSearch
		ORDER BY Number, Item_Name
GO

/*============================================================================*\
  Grant permissions.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_Specimens_Select_ForLinkedOther') IS NOT NULL
BEGIN
	PRINT 'Setting up security on procedure usp_Specimens_Select_ForLinkedOther'
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForLinkedOther TO R2k_AddOnly
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForLinkedOther TO R2k_Administrator
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForLinkedOther TO R2k_FullEdit
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForLinkedOther TO R2k_ReadOnly
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForLinkedOther TO R2k_RecordCardsOnly
END
GO
SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

/*============================================================================*\
  Drop stored proc before re-creating.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_Specimens_Select_ForMovement') IS NOT NULL
	DROP PROCEDURE dbo.usp_Specimens_Select_ForMovement
GO

/*============================================================================*\
  Description:	Returns specimens for a specified movement.

  Parameters:	@UserDomainMask			User's Domain Mask restricting which
										records may be returned
				@SessionID 				User's SessionID
				@ParentKey 				Identifies the movement
				@ShowCommonNames		Specifies whether Common Names are shown
				@ShowOriginalSpecimenNames
										0 => preferred names are shown
										1 => names originally entered are shown
				@SortOrderIndex			0 => results sorted by Item_Name, Number
										1 => results sorted by Number, Item_Name

  Created:		August 2003

  Last revision information:
	$Revision: 5 $
	$Date: 17/08/11 15:40 $
	$Author: Jamesbichard $
\*============================================================================*/
CREATE PROCEDURE dbo.usp_Specimens_Select_ForMovement
	@UserDomainMask						INT,
	@SessionID							CHAR(16),
	@ParentKey							CHAR(16),
	@ShowCommonNames					BIT,
	@ShowOriginalSpecimenNames			BIT,
	@SortOrderIndex						TINYINT
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

	DECLARE @SpecimensSearch TABLE
	(
		Item_Key		CHAR(16) 		COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		Join_Key		CHAR(16) 		COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		Det_Item_Key	CHAR(16) 		COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		Det_Item_Name	NVARCHAR(150) 	COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		Item_Name		NVARCHAR(150) 	COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		Life_Sciences	BIT NULL,
		Number			VARCHAR(30) 	COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		Hint			NVARCHAR(150) 	COLLATE SQL_Latin1_General_CP1_CI_AS NULL 
	)
	
	INSERT INTO	@SpecimensSearch (
				Item_Key,
				Join_Key,
				Life_Sciences)
	SELECT		s.Collection_Unit_Key,
				mcu.Movement_Collection_Unit_Key,
				s.Life_Sciences
	FROM 		Movement							AS	m
	INNER JOIN	Movement_Direction					AS	d
	ON			d.Movement_Key						=	m.Movement_Key
	INNER JOIN	Movement_Collection_Unit			AS	mcu
	ON			mcu.Movement_Direction_Key			=	d.Movement_Direction_Key
	INNER JOIN	Specimen_Unit						AS	s
	ON			s.Collection_Unit_Key				=	mcu.Collection_Unit_Key
	INNER JOIN	Collection_Unit						AS	cu
	ON			cu.Collection_Unit_Key				=	s.Collection_Unit_Key
	WHERE		m.Movement_Key						=	@ParentKey
	AND			(cu.Domain_Mask & @UserDomainMask	>	0
	OR			cu.Entered_Session_ID				=	@SessionID
	OR			cu.Changed_Session_ID				=	@SessionID
	OR			cu.Domain_Mask						=	0)
	
	UPDATE		@SpecimensSearch
	SET			Number = CUN.Number
	FROM 		@SpecimensSearch 		SU
	LEFT JOIN 	Collection_Unit_Number 	CUN ON 	SU.Item_key 	= CUN.Collection_Unit_Key 
											AND CUN.Preferred	= 1

	UPDATE 	@SpecimensSearch
	SET		Det_Item_Key	= CPref.Concept_Key,
			Item_Name 		= CASE @ShowOriginalSpecimenNames
								WHEN 1 THEN C.Published_Term
								ELSE CPref.Published_Term END,
			Det_Item_Name	= TDet.Plaintext
	FROM 	@SpecimensSearch 		SU
	JOIN 	VW_SpecimenDetsEarth 	SDE 	ON 	SDE.Collection_Unit_Key			= SU.Item_Key
											AND SDE.Preferred_Determination_Key	= SDE.Determination_Key
	JOIN 	Concept 				C 		ON 	C.Concept_Key					= SDE.Concept_Key
	JOIN 	Term 					TDet 	ON 	TDet.Term_Key					= C.Term_Key
	JOIN 	Concept 				CPref 	ON 	CPref.Meaning_Key				= C.Meaning_Key
											AND CPref.List_Preferred			= 1
											AND CPref.Concept_Group_Key			= C.Concept_Group_Key
	
	UPDATE 	@SpecimensSearch
	SET		Det_Item_Key	= SDL.Taxon_List_Item_Key,
			Item_Name 		= dbo.ufn_GetFormattedTaxonNameByParams(
				CASE @ShowOriginalSpecimenNames
					WHEN 1 THEN Actual_Name
					ELSE Preferred_Name END,
				CASE @ShowOriginalSpecimenNames
					WHEN 1 THEN Actual_Name_Italic
					ELSE Preferred_Name_Italic END,
				Common_Name,
				Common_Name_Italic,
				NULL,
				@ShowCommonNames),
			Det_Item_Name	= ITN.Actual_Name
	FROM 	@SpecimensSearch 	SU
	JOIN 	VW_SpecimenDetsLife SDL ON 	SDL.Collection_Unit_Key					= SU.Item_Key
									AND SDL.Preferred_Taxon_Determination_Key 	= SDL.Taxon_Determination_Key
	JOIN 	Index_Taxon_Name 	ITN	ON 	ITN.Taxon_List_Item_Key					= SDL.Taxon_List_Item_Key

	-- Select table and sort appropriately
	IF @SortOrderIndex = 0
		SELECT * FROM @SpecimensSearch
		ORDER BY Item_Name, Number
	ELSE 
	IF @SortOrderIndex = 1
		SELECT * FROM @SpecimensSearch
		ORDER BY Number, Item_Name
GO

/*============================================================================*\
  Grant permissions.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_Specimens_Select_ForMovement') IS NOT NULL
BEGIN
	PRINT 'Setting up security on procedure usp_Specimens_Select_ForMovement'
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForMovement TO R2k_AddOnly
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForMovement TO R2k_Administrator
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForMovement TO R2k_FullEdit
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForMovement TO R2k_ReadOnly
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForMovement TO R2k_RecordCardsOnly
END
GO
SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

/*============================================================================*\
  Drop stored proc before re-creating.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_Specimens_Select_ForMovementIn') IS NOT NULL
	DROP PROCEDURE dbo.usp_Specimens_Select_ForMovementIn
GO

/*============================================================================*\
 Description:	Returns specimens for a specified movement.

  Parameters:	@UserDomainMask			User's Domain Mask restricting which
										records may be returned
				@SessionID 				User's SessionID
				@ParentKey 				Identifies the movement
				@ShowCommonNames		Specifies whether Common Names are shown
				@ShowOriginalSpecimenNames
										0 => preferred names are shown
										1 => names originally entered are shown
				@SortOrderIndex			0 => results sorted by Item_Name, Number
										1 => results sorted by Number, Item_Name

  Created:		August 2003

  Last revision information:
    $Revision: 5 $
    $Date: 17/08/11 15:40 $
    $Author: Jamesbichard $
\*============================================================================*/
CREATE PROCEDURE dbo.usp_Specimens_Select_ForMovementIn
	@UserDomainMask						INT,
	@SessionID							CHAR(16),
	@ParentKey							CHAR(16),
	@ShowCommonNames					BIT,
	@ShowOriginalSpecimenNames			BIT,
	@SortOrderIndex						TINYINT
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

	DECLARE @SpecimensSearch TABLE
	(
		Item_Key		CHAR(16) 		COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		Join_Key		CHAR(16) 		COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		Det_Item_Key	CHAR(16) 		COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		Det_Item_Name	NVARCHAR(150) 	COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		Item_Name		NVARCHAR(150) 	COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		Life_Sciences	BIT NULL,
		Number			VARCHAR(30) 	COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		Hint			NVARCHAR(150) 	COLLATE SQL_Latin1_General_CP1_CI_AS NULL 
	)
	
	INSERT INTO	@SpecimensSearch (
				Item_Key,
				Join_Key,
				Life_Sciences)
	SELECT		s.Collection_Unit_Key,
				mcu.Movement_Collection_Unit_Key,
				s.Life_Sciences
	FROM 		Movement							AS	m
	INNER JOIN	Movement_Direction					AS	d
	ON			d.Movement_Key						=	m.Movement_Key
	INNER JOIN	Movement_Collection_Unit			AS	mcu
	ON			mcu.Movement_Direction_Key			=	d.Movement_Direction_Key
	INNER JOIN	Specimen_Unit						AS	s
	ON			s.Collection_Unit_Key				=	mcu.Collection_Unit_Key
	INNER JOIN	Collection_Unit						AS	cu
	ON			cu.Collection_Unit_Key				=	s.Collection_Unit_Key
	WHERE		m.Movement_Key						=	@ParentKey
	AND			d.Outbound							=	0
	AND			(cu.Domain_Mask & @UserDomainMask	>	0
	OR			cu.Entered_Session_ID				=	@SessionID
	OR			cu.Changed_Session_ID				=	@SessionID
	OR			cu.Domain_Mask						=	0)
	
	UPDATE		@SpecimensSearch
	SET			Number = CUN.Number
	FROM 		@SpecimensSearch 		SU
	LEFT JOIN 	Collection_Unit_Number 	CUN ON 	SU.Item_key 	= CUN.Collection_Unit_Key 
											AND CUN.Preferred	= 1

	UPDATE 	@SpecimensSearch
	SET		Det_Item_Key	= CPref.Concept_Key,
			Item_Name 		= CASE @ShowOriginalSpecimenNames
								WHEN 1 THEN C.Published_Term
								ELSE CPref.Published_Term END,
			Det_Item_Name	= TDet.Plaintext
	FROM 	@SpecimensSearch 		SU
	JOIN 	VW_SpecimenDetsEarth 	SDE 	ON 	SDE.Collection_Unit_Key			= SU.Item_Key
											AND SDE.Preferred_Determination_Key	= SDE.Determination_Key
	JOIN 	Concept 				C 		ON 	C.Concept_Key					= SDE.Concept_Key
	JOIN 	Term 					TDet 	ON 	TDet.Term_Key					= C.Term_Key
	JOIN 	Concept 				CPref 	ON 	CPref.Meaning_Key				= C.Meaning_Key
											AND CPref.List_Preferred			= 1
											AND CPref.Concept_Group_Key			= C.Concept_Group_Key
	
	UPDATE 	@SpecimensSearch
	SET		Det_Item_Key	= SDL.Taxon_List_Item_Key,
			Item_Name 		= dbo.ufn_GetFormattedTaxonNameByParams(
				CASE @ShowOriginalSpecimenNames
					WHEN 1 THEN Actual_Name
					ELSE Preferred_Name END,
				CASE @ShowOriginalSpecimenNames
					WHEN 1 THEN Actual_Name_Italic
					ELSE Preferred_Name_Italic END,
				Common_Name,
				Common_Name_Italic,
				NULL,
				@ShowCommonNames),
			Det_Item_Name	= ITN.Actual_Name
	FROM 	@SpecimensSearch 	SU
	JOIN 	VW_SpecimenDetsLife SDL ON 	SDL.Collection_Unit_Key					= SU.Item_Key
									AND SDL.Preferred_Taxon_Determination_Key 	= SDL.Taxon_Determination_Key
	JOIN 	Index_Taxon_Name 	ITN	ON 	ITN.Taxon_List_Item_Key					= SDL.Taxon_List_Item_Key

	-- Select table and sort appropriately
	IF @SortOrderIndex = 0
		SELECT * FROM @SpecimensSearch
		ORDER BY Item_Name, Number
	ELSE 
	IF @SortOrderIndex = 1
		SELECT * FROM @SpecimensSearch
		ORDER BY Number, Item_Name
GO

/*============================================================================*\
  Grant permissions.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_Specimens_Select_ForMovementIn') IS NOT NULL
BEGIN
	PRINT 'Setting up security on procedure usp_Specimens_Select_ForMovementIn'
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForMovementIn TO R2k_AddOnly
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForMovementIn TO R2k_Administrator
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForMovementIn TO R2k_FullEdit
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForMovementIn TO R2k_ReadOnly
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForMovementIn TO R2k_RecordCardsOnly
END
GO
SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

/*============================================================================*\
  Drop stored proc before re-creating.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_Specimens_Select_ForMovementOut') IS NOT NULL
	DROP PROCEDURE dbo.usp_Specimens_Select_ForMovementOut
GO

/*============================================================================*\
 Description:	Returns specimens for a specified movement.

  Parameters:	@UserDomainMask			User's Domain Mask restricting which
										records may be returned
				@SessionID 				User's SessionID
				@ParentKey 				Identifies the movement
				@ShowCommonNames		Specifies whether Common Names are shown
				@ShowOriginalSpecimenNames
										0 => preferred names are shown
										1 => names originally entered are shown
				@SortOrderIndex			0 => results sorted by Item_Name, Number
										1 => results sorted by Number, Item_Name

  Created:		August 2003

  Last revision information:
    $Revision: 5 $
    $Date: 17/08/11 15:40 $
    $Author: Jamesbichard $
\*============================================================================*/
CREATE PROCEDURE dbo.usp_Specimens_Select_ForMovementOut
	@UserDomainMask						INT,
	@SessionID							CHAR(16),
	@ParentKey							CHAR(16),
	@ShowCommonNames					BIT,
	@ShowOriginalSpecimenNames			BIT,
	@SortOrderIndex						TINYINT
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

	DECLARE @SpecimensSearch TABLE
	(
		Item_Key		CHAR(16) 		COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		Join_Key		CHAR(16) 		COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		Det_Item_Key	CHAR(16) 		COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		Det_Item_Name	NVARCHAR(150) 	COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		Item_Name		NVARCHAR(150) 	COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		Life_Sciences	BIT NULL,
		Number			VARCHAR(30) 	COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		Hint			NVARCHAR(150) 	COLLATE SQL_Latin1_General_CP1_CI_AS NULL 
	)
	
	INSERT INTO	@SpecimensSearch (
				Item_Key,
				Join_Key,
				Life_Sciences)
	SELECT		s.Collection_Unit_Key,
				mcu.Movement_Collection_Unit_Key,
				s.Life_Sciences
	FROM 		Movement							AS	m
	INNER JOIN	Movement_Direction					AS	d
	ON			d.Movement_Key						=	m.Movement_Key
	INNER JOIN	Movement_Collection_Unit			AS	mcu
	ON			mcu.Movement_Direction_Key			=	d.Movement_Direction_Key
	INNER JOIN	Specimen_Unit						AS	s
	ON			s.Collection_Unit_Key				=	mcu.Collection_Unit_Key
	INNER JOIN	Collection_Unit						AS	cu
	ON			cu.Collection_Unit_Key				=	s.Collection_Unit_Key
	WHERE		m.Movement_Key						=	@ParentKey
	AND			d.Outbound							=	1
	AND			(cu.Domain_Mask & @UserDomainMask	>	0
	OR			cu.Entered_Session_ID				=	@SessionID
	OR			cu.Changed_Session_ID				=	@SessionID
	OR			cu.Domain_Mask						=	0)
	
	UPDATE		@SpecimensSearch
	SET			Number = CUN.Number
	FROM 		@SpecimensSearch 		SU
	LEFT JOIN 	Collection_Unit_Number 	CUN ON 	SU.Item_key 	= CUN.Collection_Unit_Key 
											AND CUN.Preferred	= 1

	UPDATE 	@SpecimensSearch
	SET		Det_Item_Key	= CPref.Concept_Key,
			Item_Name 		= CASE @ShowOriginalSpecimenNames
								WHEN 1 THEN C.Published_Term
								ELSE CPref.Published_Term END,
			Det_Item_Name	= TDet.Plaintext
	FROM 	@SpecimensSearch 		SU
	JOIN 	VW_SpecimenDetsEarth 	SDE 	ON 	SDE.Collection_Unit_Key			= SU.Item_Key
											AND SDE.Preferred_Determination_Key	= SDE.Determination_Key
	JOIN 	Concept 				C 		ON 	C.Concept_Key					= SDE.Concept_Key
	JOIN 	Term 					TDet 	ON 	TDet.Term_Key					= C.Term_Key
	JOIN 	Concept 				CPref 	ON 	CPref.Meaning_Key				= C.Meaning_Key
											AND CPref.List_Preferred			= 1
											AND CPref.Concept_Group_Key			= C.Concept_Group_Key
	
	UPDATE 	@SpecimensSearch
	SET		Det_Item_Key	= SDL.Taxon_List_Item_Key,
			Item_Name 		= dbo.ufn_GetFormattedTaxonNameByParams(
				CASE @ShowOriginalSpecimenNames
					WHEN 1 THEN Actual_Name
					ELSE Preferred_Name END,
				CASE @ShowOriginalSpecimenNames
					WHEN 1 THEN Actual_Name_Italic
					ELSE Preferred_Name_Italic END,
				Common_Name,
				Common_Name_Italic,
				NULL,
				@ShowCommonNames),
			Det_Item_Name	= ITN.Actual_Name
	FROM 	@SpecimensSearch 	SU
	JOIN 	VW_SpecimenDetsLife SDL ON 	SDL.Collection_Unit_Key					= SU.Item_Key
									AND SDL.Preferred_Taxon_Determination_Key 	= SDL.Taxon_Determination_Key
	JOIN 	Index_Taxon_Name 	ITN	ON 	ITN.Taxon_List_Item_Key					= SDL.Taxon_List_Item_Key

	-- Select table and sort appropriately
	IF @SortOrderIndex = 0
		SELECT * FROM @SpecimensSearch
		ORDER BY Item_Name, Number
	ELSE 
	IF @SortOrderIndex = 1
		SELECT * FROM @SpecimensSearch
		ORDER BY Number, Item_Name
GO

/*============================================================================*\
  Grant permissions.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_Specimens_Select_ForMovementOut') IS NOT NULL
BEGIN
	PRINT 'Setting up security on procedure usp_Specimens_Select_ForMovementOut'
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForMovementOut TO R2k_AddOnly
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForMovementOut TO R2k_Administrator
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForMovementOut TO R2k_FullEdit
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForMovementOut TO R2k_ReadOnly
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForMovementOut TO R2k_RecordCardsOnly
END
GO
/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Specimens_Select_ForSearch]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Specimens_Select_ForSearch]
GO

/*===========================================================================*\
  Description:	Returns Collection_Unit_Key and DisplayTerm when search characters 
		are entered. The Specimen_Unit table does not have a Display_Caption 
		or Search_Caption field, so the caption must be constructed through 
		joins to other tables.

  Parameters:	@UserDomainMask		User's Domain Mask restricting which records may be returned.
		@SessionID 		User's SessionID.
		@SearchText 		Search text used to find collections.

  Created:	September 2003

  Last revision information:
    $Revision: 5 $
    $Date: 17/08/11 15:40 $
    $Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Specimens_Select_ForSearch] 
	@UserDomainMask int,
	@SessionID char(16),
	@ShowCommonNames BIT,
	@SearchText varchar(100)
AS

SET NOCOUNT ON


--Use temp table to build output, so silly data errors such as duplicate accessions
-- don't duplicate in the list
DECLARE @SpecimensSearch TABLE
(
	[Item_Key] [char] (16)				COLLATE database_default NULL,
	[DisplayTerm] [nvarchar] (150)		COLLATE database_default NULL,
	[SearchTerm] [nvarchar] (150)		COLLATE database_default NULL,
	[Life_Sciences] [bit] NULL
)

--Find all specimens with a determination match
INSERT INTO 
	@SpecimensSearch (Item_Key, Life_Sciences, SearchTerm, DisplayTerm) 
SELECT DISTINCT 
	SU.Collection_Unit_Key					COLLATE database_default, 
	SU.Life_Sciences,
	CASE Su.Life_Sciences 
		WHEN 0 THEN TSearch.Plaintext
		ELSE ITN.Actual_Name				
	END COLLATE database_default AS SearchTerm,
	CASE Su.Life_Sciences 
		WHEN 0 THEN CSearch.Published_Term		
		ELSE CASE ITN.Actual_Name_Italic	
			WHEN 1 THEN '<i>' + ITN.Actual_Name + '</i>' 
			ELSE ITN.Actual_Name			
		END
	END COLLATE database_default AS DisplayTerm
	
FROM SPECIMEN_UNIT SU
INNER JOIN Collection_Unit CU ON SU.Collection_Unit_Key = CU.Collection_Unit_Key 
 	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
	OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
LEFT JOIN VW_SpecimenDetsEarth SDE ON SU.Collection_Unit_Key = SDE.Collection_Unit_Key
LEFT JOIN Concept C ON SDE.Concept_Key = C.Concept_Key
LEFT JOIN Concept CSearch ON CSearch.Meaning_Key=C.Meaning_Key
LEFT JOIN Search_Term ST ON ST.Concept_Key = CSearch.Concept_Key
LEFT JOIN Term TSearch ON TSearch.Term_Key=CSearch.Term_Key
LEFT JOIN VW_SpecimenDetsLife SDL ON SU.Collection_Unit_Key = SDL.Collection_Unit_Key
LEFT JOIN Index_Taxon_Synonym ITS ON ITS.Taxon_List_Item_Key=SDL.Taxon_List_Item_Key
LEFT JOIN INDEX_TAXON_NAME ITN	ON ITS.Synonym_List_Item_Key = ITN.Taxon_List_Item_Key
WHERE 
	(ST.Plaintext LIKE @SearchText + '%' AND SU.Life_Sciences=0) 
	OR 
	(ITN.Actual_Name LIKE @SearchText + '%' AND SU.Life_Sciences=1)

-- Update the number in case there are 2 registrations for a specimen, so we don't duplicate
-- the rows in the output results.
UPDATE @SpecimensSearch
SET 
		SearchTerm = SearchTerm + ' - ' + CUN.Number,
		DisplayTerm = DisplayTerm + ' - ' + CUN.Number
FROM @SpecimensSearch SU
INNER JOIN Collection_Unit_Number CUN ON SU.Item_key = CUN.Collection_Unit_Key 	AND CUN.Preferred = 1

-- Select table and sort appropriately
SELECT * from @SpecimensSearch
ORDER BY SearchTerm

GO




/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Specimens_Select_ForSearch') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_Specimens_Select_ForSearch'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
       	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearch TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearch TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearch TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearch TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearch TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
       	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearch TO [Dev - JNCC SQL]
END
GO
SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

/*============================================================================*\
  Drop stored proc before re-creating.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_Specimens_Select_ForSearchByAnyDetermination') IS NOT NULL
	DROP PROCEDURE dbo.usp_Specimens_Select_ForSearchByAnyDetermination
GO

/*============================================================================*\
  Description:	Returns specimens based on any of their determinations.

  Parameters:
				@UserDomainMask			User's Domain Mask restricting which
										records may be returned
				@SessionID 				User's SessionID
				@ShowCommonNames		Specifies whether Common Names are shown
				@ShowOriginalSpecimenNames
										0 => preferred names are shown
										1 => names originally entered are shown
				@SearchText				Text to search for
				@SortOrderIndex			0 => results sorted by Item_Name, Number
										1 => results sorted by Number, Item_Name

  Created:		October 2003

  Last revision information:
    $Revision: 5 $
    $Date: 17/08/11 15:40 $
    $Author: Jamesbichard $
\*============================================================================*/
CREATE PROCEDURE dbo.usp_Specimens_Select_ForSearchByAnyDetermination
	@UserDomainMask						INT,
	@SessionID							CHAR(16),
	@ShowCommonNames					BIT,
	@ShowOriginalSpecimenNames			BIT,
	@SearchText							VARCHAR(150),
	@SortOrderIndex						TINYINT
AS
	SET NOCOUNT ON

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
		[Hint] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL 
	)

	--Find all specimens with a determination match
	INSERT INTO @SpecimensSearch (Item_Key, Life_Sciences) 
	SELECT DISTINCT SU.Collection_Unit_Key, SU.Life_Sciences
	FROM 	SPECIMEN_UNIT SU
	JOIN 	Collection_Unit CU ON SU.Collection_Unit_Key = CU.Collection_Unit_Key 
		 	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
			OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
	JOIN 	VW_SpecimenDetsEarth SDE ON SU.Collection_Unit_Key = SDE.Collection_Unit_Key
	JOIN 	Concept C ON SDE.Concept_Key = C.Concept_Key
	JOIN 	Concept CSearch ON CSearch.Meaning_Key=C.Meaning_Key
	LEFT JOIN Search_Term ST ON ST.Concept_Key = CSearch.Concept_Key
	WHERE 	(ST.Plaintext LIKE @SearchText + '%'
			 AND SU.Life_Sciences=0) 

	INSERT INTO @SpecimensSearch (Item_Key, Life_Sciences) 
	SELECT DISTINCT SU.Collection_Unit_Key, SU.Life_Sciences
	FROM 	SPECIMEN_UNIT SU
	JOIN 	Collection_Unit CU ON SU.Collection_Unit_Key = CU.Collection_Unit_Key 
	 		AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
			OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
	JOIN 	VW_SpecimenDetsLife SDL ON SU.Collection_Unit_Key = SDL.Collection_Unit_Key
	JOIN 	Index_Taxon_Synonym ITS ON ITS.Taxon_List_Item_Key=SDL.Taxon_List_Item_Key
	JOIN 	INDEX_TAXON_NAME ITN	ON ITS.Synonym_List_Item_Key = ITN.Taxon_List_Item_Key
	WHERE 	(ITN.Actual_Name LIKE @SearchText + '%' AND SU.Life_Sciences=1)

	UPDATE 		@SpecimensSearch
	SET			Number = CUN.Number
	FROM 		@SpecimensSearch 		SU
	LEFT JOIN 	Collection_Unit_Number 	CUN ON 	SU.Item_key 	= CUN.Collection_Unit_Key 
											AND CUN.Preferred	= 1

	UPDATE @SpecimensSearch
	SET	Det_Item_Key=CPref.Concept_Key,
		Item_Name = CASE @ShowOriginalSpecimenNames
						WHEN 1 THEN C.Published_Term
						ELSE CPref.Published_Term END,
		Det_Item_Name=TDet.Plaintext,
		Hint=TDet.Plaintext
	FROM @SpecimensSearch SU
	INNER JOIN VW_SpecimenDetsEarth SDE ON SDE.Collection_Unit_Key=SU.Item_Key
		AND SDE.Preferred_Determination_Key=SDE.Determination_Key
	INNER JOIN Concept C ON C.Concept_Key=SDE.Concept_Key
	INNER JOIN Term TDet ON TDet.Term_Key=C.Term_Key
	INNER JOIN Concept CPref ON CPref.Meaning_Key=C.Meaning_Key
		AND CPref.List_Preferred=1
		AND CPref.Concept_Group_Key=C.Concept_Group_Key
	
	UPDATE @SpecimensSearch
	SET	Det_Item_Key=SDL.Taxon_List_Item_Key,
		Item_Name = dbo.ufn_GetFormattedTaxonNameByParams(
				CASE @ShowOriginalSpecimenNames
					WHEN 1 THEN Actual_Name
					ELSE Preferred_Name END,
				CASE @ShowOriginalSpecimenNames
					WHEN 1 THEN Actual_Name_Italic
					ELSE Preferred_Name_Italic END,
				Common_Name,
				Common_Name_Italic,
				null,
				@ShowCommonNames),
		Det_Item_Name=ITN.Actual_Name,
		Hint=ITN.Actual_Name
	FROM @SpecimensSearch SU
	INNER JOIN VW_SpecimenDetsLife SDL ON SDL.Collection_Unit_Key=SU.Item_Key
			AND SDL.Preferred_Taxon_Determination_Key = SDL.Taxon_Determination_Key
	INNER JOIN Index_Taxon_Name ITN	ON ITN.Taxon_List_Item_Key=SDL.Taxon_List_Item_Key

	-- Select table and sort appropriately
	IF @SortOrderIndex = 0
		SELECT * FROM @SpecimensSearch
		ORDER BY Item_Name, Number
	ELSE IF @SortOrderIndex = 1
		SELECT * FROM @SpecimensSearch
		ORDER BY Number, Item_Name
GO

/*============================================================================*\
  Grant permissions.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_Specimens_Select_ForSearchByAnyDetermination') IS NOT NULL
BEGIN
	PRINT 'Setting up security on procedure usp_Specimens_Select_ForSearchByAnyDetermination'
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByAnyDetermination TO R2k_AddOnly
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByAnyDetermination TO R2k_Administrator
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByAnyDetermination TO R2k_FullEdit
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByAnyDetermination TO R2k_ReadOnly
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByAnyDetermination TO R2k_RecordCardsOnly
END
GO
SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

/*============================================================================*\
  Drop stored proc before re-creating.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_Specimens_Select_ForSearchByAnyNumber') IS NOT NULL
	DROP PROCEDURE dbo.usp_Specimens_Select_ForSearchByAnyNumber
GO

/*=============================================================================*\
  Description:	Returns specimens based on their numbers.

  Parameters:	@UserDomainMask			User's Domain Mask restricting which
										records may be returned
				@SessionID 				User's SessionID
				@ShowCommonNames		Specifies whether Common Names are shown
				@ShowOriginalSpecimenNames
										0 => preferred names are shown
										1 => names originally entered are shown
				@SearchText				Text to search for
				@SortOrderIndex			0 => results sorted by Item_Name, Number
										1 => results sorted by Number, Item_Name

  Created:		October 2003

  Last revision information:
    $Revision: 5 $
    $Date: 17/08/11 15:40 $
    $Author: Jamesbichard $
\*============================================================================*/
CREATE PROCEDURE dbo.usp_Specimens_Select_ForSearchByAnyNumber
	@UserDomainMask						INT,
	@SessionID							CHAR(16),
	@ShowCommonNames					BIT,
	@ShowOriginalSpecimenNames			BIT,
	@SearchText							VARCHAR(30),
	@SortOrderIndex						TINYINT
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
		[Hint] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL 
	)

	INSERT INTO @SpecimensSearch (Item_Key, Life_Sciences, Hint) 
	SELECT DISTINCT SU.Collection_Unit_Key, SU.Life_Sciences, CUN.Number
	FROM 	Specimen_Unit SU
	JOIN 	Collection_Unit CU ON SU.Collection_Unit_Key = CU.Collection_Unit_Key 
	   		AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
			OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
	JOIN 	Collection_Unit_Number CUN ON SU.Collection_Unit_key = CUN.Collection_Unit_Key 
			AND CUN.Number LIKE @SearchText + '%'

	UPDATE 		@SpecimensSearch
	SET			Number = CUN.Number
	FROM 		@SpecimensSearch 		SU
	LEFT JOIN 	Collection_Unit_Number 	CUN 
				ON SU.Item_key = CUN.Collection_Unit_Key 
				AND CUN.Preferred=1

	UPDATE @SpecimensSearch
	SET	Det_Item_Key=CPref.Concept_Key,
		Item_Name = CASE @ShowOriginalSpecimenNames
						WHEN 1 THEN C.Published_Term
						ELSE CPref.Published_Term END,
		Det_Item_Name=TDet.Plaintext
	FROM @SpecimensSearch SU
	INNER JOIN VW_SpecimenDetsEarth SDE ON SDE.Collection_Unit_Key=SU.Item_Key
		AND SDE.Preferred_Determination_Key=SDE.Determination_Key
	INNER JOIN Concept C ON C.Concept_Key=SDE.Concept_Key
	INNER JOIN Term TDet ON TDet.Term_Key=C.Term_Key
	INNER JOIN Concept CPref ON CPref.Meaning_Key=C.Meaning_Key
		AND CPref.List_Preferred=1
		AND CPref.Concept_Group_Key=C.Concept_Group_Key
	
	UPDATE @SpecimensSearch
	SET	Det_Item_Key=SDL.Taxon_List_Item_Key,
		Item_Name = dbo.ufn_GetFormattedTaxonNameByParams(
				CASE @ShowOriginalSpecimenNames
					WHEN 1 THEN Actual_Name
					ELSE Preferred_Name END,
				CASE @ShowOriginalSpecimenNames
					WHEN 1 THEN Actual_Name_Italic
					ELSE Preferred_Name_Italic END,
				Common_Name,
				Common_Name_Italic,
				null,
				@ShowCommonNames),
		Det_Item_Name=ITN.Actual_Name
	FROM @SpecimensSearch SU
	INNER JOIN VW_SpecimenDetsLife SDL ON SDL.Collection_Unit_Key=SU.Item_Key
			AND SDL.Preferred_Taxon_Determination_Key = SDL.Taxon_Determination_Key
	INNER JOIN Index_Taxon_Name ITN	ON ITN.Taxon_List_Item_Key=SDL.Taxon_List_Item_Key

	-- Select table and sort appropriately
	IF @SortOrderIndex = 0
		SELECT * FROM @SpecimensSearch
		ORDER BY Item_Name, Number
	ELSE IF @SortOrderIndex = 1
		SELECT * FROM @SpecimensSearch
		ORDER BY Number, Item_Name
GO

/*============================================================================*\
  Grant permissions.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_Specimens_Select_ForSearchByAnyNumber') IS NOT NULL
BEGIN
	PRINT 'Setting up security on procedure usp_Specimens_Select_ForSearchByAnyNumber'
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByAnyNumber TO R2k_AddOnly
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByAnyNumber TO R2k_Administrator
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByAnyNumber TO R2k_FullEdit
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByAnyNumber TO R2k_ReadOnly
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByAnyNumber TO R2k_RecordCardsOnly
END
GO
If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Specimens_Select_ForSearchByDescription]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Specimens_Select_ForSearchByDescription]
GO

/*===========================================================================*\
  Description:
	Returns Specimens data based on a search using the Description parameter.

  Parameters:
	@UserDomainMask		User's Domain Mask restricting which records may be returned
	@SessionID 		User's SessionID
	@ShowCommonNames	Specifies whether or not Common Names should be shown
	@SearchText		Text to be searched on
	@SortOrderIndex		Index determining Sort Order

  Created:
	October 2003

  Last revision information:
    $Revision: 5 $
    $Date: 17/08/11 15:40 $
    $Author: Jamesbichard $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_Specimens_Select_ForSearchByDescription] 
	@UserDomainMask INT,
	@SessionID CHAR(16),
	@ShowCommonNames BIT,
	@SearchText VARCHAR(100),
	@SortOrderIndex TINYINT
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
			AND M.Metadata_Type_Key='SYSTEM0000000006'
	JOIN 	Collection_Unit CU ON SU.Collection_Unit_Key = CU.Collection_Unit_Key 
	 		AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
			OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
	WHERE 	M.Text LIKE @SearchText + '%'

	UPDATE		@SpecimensSearch
	SET			Number = CUN.Number
	FROM 		@SpecimensSearch 		SU
	LEFT JOIN 	Collection_Unit_Number 	CUN ON 	SU.Item_key 	= CUN.Collection_Unit_Key 
											AND CUN.Preferred	= 1

	UPDATE @SpecimensSearch
	SET	Det_Item_Key=CPref.Concept_Key,
		Item_Name = CPref.Published_Term,
		Det_Item_Name=TDet.Plaintext
	FROM @SpecimensSearch SU
	INNER JOIN VW_SpecimenDetsEarth SDE ON SDE.Collection_Unit_Key=SU.Item_Key
	INNER JOIN Concept C ON C.Concept_Key=SDE.Concept_Key
	INNER JOIN Term TDet ON TDet.Term_Key=C.Term_Key
	INNER JOIN Concept CPref ON CPref.Meaning_Key=C.Meaning_Key
		AND CPref.List_Preferred=1
		AND CPref.Concept_Group_Key=C.Concept_Group_Key
	
	UPDATE @SpecimensSearch
	SET	Det_Item_Key=SDL.Taxon_List_Item_Key,
		Item_Name = dbo.ufn_GetFormattedTaxonNameByParams(
				Preferred_Name,
				Preferred_Name_Italic,
				Common_Name,
				Common_Name_Italic,
				null,
				@ShowCommonNames),
		Det_Item_Name=ITN.Actual_Name
	FROM @SpecimensSearch SU
	INNER JOIN VW_SpecimenDetsLife SDL ON SDL.Collection_Unit_Key=SU.Item_Key
			AND SDL.Preferred_Taxon_Determination_Key = SDL.Taxon_Determination_Key
	INNER JOIN Index_Taxon_Name ITN	ON ITN.Taxon_List_Item_Key=SDL.Taxon_List_Item_Key

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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Specimens_Select_ForSearchByDescription') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_Specimens_Select_ForSearchByDescription'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
       	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByDescription TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByDescription TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByDescription TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByDescription TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByDescription TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
       	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByDescription TO [Dev - JNCC SQL]
END
GO
SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

/*============================================================================*\
  Drop stored proc before re-creating.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_Specimens_Select_ForSearchByDeterminationInGroup') IS NOT NULL
	DROP PROCEDURE dbo.usp_Specimens_Select_ForSearchByDeterminationInGroup
GO

/*============================================================================*\
  Description:	Returns specimens that have determinations whose concepts are
				hierarchically contained by concepts that match the search text.

  Parameters:	@UserDomainMask			User's Domain Mask restricting which
										records may be returned
				@SessionID 				User's SessionID
				@ShowCommonNames		Specifies whether Common Names are shown
				@ShowOriginalSpecimenNames
										0 => preferred names are shown
										1 => names originally entered are shown
				@SearchText				Text to search for
				@SortOrderIndex			0 => results sorted by Item_Name, Number
										1 => results sorted by Number, Item_Name

  Created:		October 2003

  Last revision information:
    $Revision: 5 $
    $Date: 17/08/11 15:40 $
    $Author: Jamesbichard $
\*============================================================================*/
CREATE PROCEDURE dbo.usp_Specimens_Select_ForSearchByDeterminationInGroup
	@UserDomainMask						INT,
	@SessionID							CHAR(16),
	@ShowCommonNames					BIT,
	@ShowOriginalSpecimenNames			BIT,
	@SearchText							VARCHAR(150),
	@SortOrderIndex						TINYINT
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
		[Hint] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL 
	)

	--Create a temp table to hold the meanings of the contents of the groups that match the search
	DECLARE @SearchLineage TABLE (
		Child_Meaning_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS,
		PlainText varchar(150) COLLATE SQL_Latin1_General_CP1_CI_AS
	)

	INSERT INTO 	@SearchLineage
	SELECT DISTINCT CChild.Meaning_Key, CSearch.PlainText
	FROM 		vw_ConceptTerm CSearch
	INNER JOIN 	Concept_Group CG ON CG.Concept_Group_Key = CSearch.Concept_Group_Key
	INNER JOIN 	Local_Domain LD ON LD.Local_Domain_Key = CG.Local_Domain_Key
	INNER JOIN 	Domain D ON D.Domain_Key = LD.Domain_Key AND D.Has_Occurrences = 1
	INNER JOIN 	Concept CSynSearch ON CSynSearch.Meaning_Key = CSearch.Meaning_Key
	INNER JOIN 	Concept_Lineage CL ON CL.Concept_Key = CSynSearch.Concept_Key
	INNER JOIN 	Concept_Lineage CLChild ON CLChild.Lineage LIKE CL.Lineage + '\%'
	INNER JOIN 	Concept CChild ON CChild.Concept_Key = CLChild.Concept_Key AND (CChild.Concept_Group_Key = CSynSearch.Concept_Group_Key)
	LEFT JOIN	Search_Term ST ON ST.Concept_Key = CSearch.Concept_Key
	WHERE 		ST.Plaintext LIKE @SearchText + '%'

	INSERT INTO 	@SpecimensSearch (Item_Key, Life_Sciences, Hint) 
	SELECT DISTINCT SDE.Collection_Unit_Key, 0 AS Life_Sciences, SL.Plaintext
	FROM 		@SearchLineage SL
	INNER JOIN 	vw_ConceptTerm CChildSyn ON CChildSyn.Meaning_Key = SL.Child_Meaning_Key
	INNER JOIN 	vw_SpecimenDetsEarth SDE ON SDE.Concept_Key = CChildSyn.Concept_Key
	INNER JOIN 	Collection_Unit CU ON CU.Collection_Unit_Key = SDE.Collection_Unit_Key	
	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
	
	INSERT INTO 	@SpecimensSearch (Item_Key, Life_Sciences, Hint) 
	SELECT DISTINCT SDL.Collection_Unit_Key AS Item_Key, 1 AS Life_Sciences, ITNSearch.Actual_Name
	FROM 		Index_Taxon_Name ITNSearch
	INNER JOIN 	Index_Taxon_Synonym ITSSearch ON ITSSearch.Taxon_List_Item_Key = ITNSearch.Taxon_List_Item_Key
	INNER JOIN 	Index_Taxon_Group ITG ON ITG.Taxon_List_Item_Key = ITSSearch.Synonym_List_Item_Key
	INNER JOIN 	Index_Taxon_Synonym ITSSyn ON ITSSyn.Taxon_List_Item_Key = ITG.Contained_List_Item_Key
	INNER JOIN 	Index_Taxon_Name ITNSyn ON ITNSyn.Taxon_List_Item_Key = ITSSyn.Synonym_List_Item_Key
	INNER JOIN 	vw_SpecimenDetsLife SDL ON SDL.Taxon_List_Item_Key = ITNSyn.Taxon_List_Item_Key
	INNER JOIN 	Collection_Unit CU ON CU.Collection_Unit_Key = SDL.Collection_Unit_Key
		AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
	WHERE 		ITNSearch.Actual_Name LIKE @SearchText + '%'

	UPDATE		@SpecimensSearch
	SET			Number = CUN.Number
	FROM 		@SpecimensSearch 		SU
	LEFT JOIN 	Collection_Unit_Number 	CUN ON 	SU.Item_key 	= CUN.Collection_Unit_Key 
											AND CUN.Preferred	= 1

	UPDATE 	@SpecimensSearch
	SET	Det_Item_Key=CPref.Concept_Key,
		Item_Name = CASE @ShowOriginalSpecimenNames
						WHEN 1 THEN C.Published_Term
						ELSE CPref.Published_Term END,
		Det_Item_Name=TDet.Plaintext 
	FROM 		@SpecimensSearch SU
	INNER JOIN 	VW_SpecimenDetsEarth SDE ON SDE.Collection_Unit_Key=SU.Item_Key
		AND SDE.Preferred_Determination_Key=SDE.Determination_Key
	INNER JOIN 	Concept C ON C.Concept_Key=SDE.Concept_Key
	INNER JOIN 	Term TDet ON TDet.Term_Key=C.Term_Key
	INNER JOIN 	Concept CPref ON CPref.Meaning_Key = C.Meaning_Key AND CPref.List_Preferred = 1 AND CPref.Concept_Group_Key = C.Concept_Group_Key
	
	UPDATE 	@SpecimensSearch
	SET	Det_Item_Key=SDL.Taxon_List_Item_Key,
		Item_Name = dbo.ufn_GetFormattedTaxonNameByParams(
					CASE @ShowOriginalSpecimenNames
						WHEN 1 THEN Actual_Name
						ELSE Preferred_Name END,
					CASE @ShowOriginalSpecimenNames
						WHEN 1 THEN Actual_Name_Italic
						ELSE Preferred_Name_Italic END,
					Common_Name,
					Common_Name_Italic,
					NULL,
					@ShowCommonNames),
		Det_Item_Name=ITN.Actual_Name
	FROM 		@SpecimensSearch SU
	INNER JOIN 	VW_SpecimenDetsLife SDL ON SU.Item_Key = SDL.Collection_Unit_Key
	INNER JOIN 	Index_Taxon_Name ITN	ON ITN.Taxon_List_Item_Key=SDL.Taxon_List_Item_Key

	-- Select table and sort appropriately
	IF @SortOrderIndex = 0
		SELECT * FROM @SpecimensSearch
		ORDER BY Item_Name, Number
	ELSE IF @SortOrderIndex = 1
		SELECT * FROM @SpecimensSearch
		ORDER BY Number, Item_Name
GO

/*============================================================================*\
  Grant permissions.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_Specimens_Select_ForSearchByDeterminationInGroup') IS NOT NULL
BEGIN
	PRINT 'Setting up security on procedure usp_Specimens_Select_ForSearchByDeterminationInGroup'
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByDeterminationInGroup TO R2k_AddOnly
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByDeterminationInGroup TO R2k_Administrator
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByDeterminationInGroup TO R2k_FullEdit
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByDeterminationInGroup TO R2k_ReadOnly
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByDeterminationInGroup TO R2k_RecordCardsOnly
END
GO
SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

/*============================================================================*\
  Drop stored proc before re-creating.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_Specimens_Select_ForSearchByGatheringDate') IS NOT NULL
	DROP PROCEDURE dbo.usp_Specimens_Select_ForSearchByGatheringDate
GO

/*============================================================================*\
  Description:	Returns specimens based on their gathering date.

  Parameters:	@UserDomainMask			User's Domain Mask restricting which
										records may be returned
				@SessionID 				User's SessionID
				@ShowCommonNames		Specifies whether Common Names are shown
				@ShowOriginalSpecimenNames
										0 => preferred names are shown
										1 => names originally entered are shown
				@SearchText				Text to search for
				@SortOrderIndex			0 => results sorted by Item_Name, Number
										1 => results sorted by Number, Item_Name

  Created:		October 2003

  Last revision information:
    $Revision: 5 $
    $Date: 17/08/11 15:40 $
    $Author: Jamesbichard $
\*============================================================================*/
CREATE PROCEDURE dbo.usp_Specimens_Select_ForSearchByGatheringDate
	@UserDomainMask						INT,
	@SessionID							CHAR(16),
	@ShowCommonNames					BIT,
	@ShowOriginalSpecimenNames			BIT,
	@SearchText							VARCHAR(50),
	@SortOrderIndex						TINYINT
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
		[Hint] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL 
	)

	INSERT INTO @SpecimensSearch (Item_Key, Life_Sciences, Hint) 
	SELECT DISTINCT 
		SU.Collection_Unit_Key, 
		SU.Life_Sciences, 
		dbo.ufn_GetDateFromVagueDate(S.Vague_Date_Start, S.Vague_Date_End, S.Vague_Date_Type)
	FROM Specimen_Unit SU
	INNER JOIN Collection_Unit CU ON SU.Collection_Unit_Key = CU.Collection_Unit_Key 
	 	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
		OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
	INNER JOIN Specimen_Field_Data SFD ON SFD.Collection_Unit_Key = SU.Collection_Unit_Key
		AND SFD.Gathering_Event=1
	LEFT JOIN Occurrence O ON O.Occurrence_Key = SFD.Occurrence_Key
	LEFT JOIN Taxon_Occurrence XO ON XO.Taxon_Occurrence_Key=SFD.Taxon_Occurrence_Key
	INNER JOIN [Sample] S ON S.Sample_Key=O.Sample_Key OR S.Sample_Key = XO.Sample_Key
	WHERE dbo.ufn_CBWrapperForDoVagueDatesOverlap(@SearchText, S.Vague_Date_Start, S.Vague_Date_End) = 1

	UPDATE		@SpecimensSearch
	SET			Number = CUN.Number
	FROM 		@SpecimensSearch 		SU
	LEFT JOIN 	Collection_Unit_Number 	CUN ON 	SU.Item_key 	= CUN.Collection_Unit_Key 
											AND CUN.Preferred	= 1

	UPDATE @SpecimensSearch
	SET	Det_Item_Key=CPref.Concept_Key,
		Item_Name = CASE @ShowOriginalSpecimenNames
						WHEN 1 THEN C.Published_Term
						ELSE CPref.Published_Term END,
		Det_Item_Name=TDet.Plaintext
	FROM @SpecimensSearch SU
	INNER JOIN VW_SpecimenDetsEarth SDE ON SDE.Collection_Unit_Key=SU.Item_Key
		AND SDE.Preferred_Determination_Key=SDE.Determination_Key
	INNER JOIN Concept C ON C.Concept_Key=SDE.Concept_Key
	INNER JOIN Term TDet ON TDet.Term_Key=C.Term_Key
	INNER JOIN Concept CPref ON CPref.Meaning_Key=C.Meaning_Key
		AND CPref.List_Preferred=1
		AND CPref.Concept_Group_Key=C.Concept_Group_Key
	
	UPDATE @SpecimensSearch
	SET	Det_Item_Key=SDL.Taxon_List_Item_Key,
		Item_Name = dbo.ufn_GetFormattedTaxonNameByParams(
				CASE @ShowOriginalSpecimenNames
					WHEN 1 THEN Actual_Name
					ELSE Preferred_Name END,
				CASE @ShowOriginalSpecimenNames
					WHEN 1 THEN Actual_Name_Italic
					ELSE Preferred_Name_Italic END,
				Common_Name,
				Common_Name_Italic,
				null,
				@ShowCommonNames),
		Det_Item_Name=ITN.Actual_Name,
		Hint=ITN.Actual_Name
	FROM @SpecimensSearch SU
	INNER JOIN VW_SpecimenDetsLife SDL ON SDL.Collection_Unit_Key=SU.Item_Key
			AND SDL.Preferred_Taxon_Determination_Key = SDL.Taxon_Determination_Key
	INNER JOIN Index_Taxon_Name ITN	ON ITN.Taxon_List_Item_Key=SDL.Taxon_List_Item_Key

	-- Select table and sort appropriately
	IF @SortOrderIndex = 0
		SELECT * FROM @SpecimensSearch
		ORDER BY Item_Name, Number
	ELSE IF @SortOrderIndex = 1
		SELECT * FROM @SpecimensSearch
		ORDER BY Number, Item_Name
GO

/*============================================================================*\
  Grant permissions.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_Specimens_Select_ForSearchByGatheringDate') IS NOT NULL
BEGIN
	PRINT 'Setting up security on procedure usp_Specimens_Select_ForSearchByGatheringDate'
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByGatheringDate TO R2k_AddOnly
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByGatheringDate TO R2k_Administrator
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByGatheringDate TO R2k_FullEdit
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByGatheringDate TO R2k_ReadOnly
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByGatheringDate TO R2k_RecordCardsOnly
END
GO
SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

/*============================================================================*\
  Drop stored proc before re-creating.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_Specimens_Select_ForSearchByGatheringLocation') IS NOT NULL
	DROP PROCEDURE dbo.usp_Specimens_Select_ForSearchByGatheringLocation
GO

/*============================================================================*\
  Description:	Returns specimens based on their gathering location.

  Parameters:	@UserDomainMask			User's Domain Mask restricting which
										records may be returned
				@SessionID 				User's SessionID
				@ShowCommonNames		Specifies whether Common Names are shown
				@ShowOriginalSpecimenNames
										0 => preferred names are shown
										1 => names originally entered are shown
				@SearchText				Text to search for
				@SortOrderIndex			0 => results sorted by Item_Name, Number
										1 => results sorted by Number, Item_Name

  Created:		October 2003

  Last revision information:
    $Revision: 5 $
    $Date: 17/08/11 15:40 $
    $Author: Jamesbichard $
\*============================================================================*/
CREATE PROCEDURE dbo.usp_Specimens_Select_ForSearchByGatheringLocation
	@UserDomainMask						INT,
	@SessionID							CHAR(16),
	@ShowCommonNames					BIT,
	@ShowOriginalSpecimenNames			BIT,
	@SearchText							VARCHAR(100),
	@SortOrderIndex						TINYINT
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
		[Hint] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL 
	)

	INSERT INTO @SpecimensSearch (Item_Key, Life_Sciences, Hint) 
	SELECT 	DISTINCT SU.Collection_Unit_Key, SU.Life_Sciences, LN.Item_Name
	FROM 	Specimen_Unit SU
	INNER JOIN Collection_Unit CU ON SU.Collection_Unit_Key = CU.Collection_Unit_Key 
	 	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
		OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
	INNER JOIN Specimen_Field_Data SFD ON SFD.Collection_Unit_Key = SU.Collection_Unit_Key
		AND SFD.Gathering_Event=1
	LEFT JOIN Occurrence O ON O.Occurrence_Key = SFD.Occurrence_Key
	LEFT JOIN Taxon_Occurrence XO ON XO.Taxon_Occurrence_Key=SFD.Taxon_Occurrence_Key
	INNER JOIN [Sample] S ON S.Sample_Key=O.Sample_Key OR S.Sample_Key = XO.Sample_Key
	INNER JOIN [Survey_Event] SE ON SE.Survey_Event_Key=S.Survey_Event_Key
	INNER JOIN Location_Name LN ON LN.Location_Key=S.Location_Key OR LN.Location_KEY=SE.Location_Key
	WHERE LN.Item_Name LIKE @SearchText + '%'

	UPDATE		@SpecimensSearch
	SET			Number = CUN.Number
	FROM 		@SpecimensSearch 		SU
	LEFT JOIN 	Collection_Unit_Number 	CUN ON 	SU.Item_key 	= CUN.Collection_Unit_Key 
											AND CUN.Preferred	= 1

	UPDATE @SpecimensSearch
	SET	Det_Item_Key=CPref.Concept_Key,
		Item_Name = CASE @ShowOriginalSpecimenNames
						WHEN 1 THEN C.Published_Term
						ELSE CPref.Published_Term END,
		Det_Item_Name=TDet.Plaintext,
		Hint=TDet.Plaintext
	FROM @SpecimensSearch SU
	INNER JOIN VW_SpecimenDetsEarth SDE ON SDE.Collection_Unit_Key=SU.Item_Key
		AND SDE.Preferred_Determination_Key=SDE.Determination_Key
	INNER JOIN Concept C ON C.Concept_Key=SDE.Concept_Key
	INNER JOIN Term TDet ON TDet.Term_Key=C.Term_Key
	INNER JOIN Concept CPref ON CPref.Meaning_Key=C.Meaning_Key
		AND CPref.List_Preferred=1
		AND CPref.Concept_Group_Key=C.Concept_Group_Key
	

	UPDATE @SpecimensSearch
	SET	Det_Item_Key=SDL.Taxon_List_Item_Key,
		Item_Name = dbo.ufn_GetFormattedTaxonNameByParams(
				CASE @ShowOriginalSpecimenNames
					WHEN 1 THEN Actual_Name
					ELSE Preferred_Name END,
				CASE @ShowOriginalSpecimenNames
					WHEN 1 THEN Actual_Name_Italic
					ELSE Preferred_Name_Italic END,
				Common_Name,
				Common_Name_Italic,
				NULL,
				@ShowCommonNames),
		Det_Item_Name=ITN.Actual_Name,
		Hint=ITN.Actual_Name
	FROM @SpecimensSearch SU
	INNER JOIN VW_SpecimenDetsLife SDL ON SDL.Collection_Unit_Key=SU.Item_Key
			AND SDL.Preferred_Taxon_Determination_Key = SDL.Taxon_Determination_Key
	INNER JOIN Index_Taxon_Name ITN	ON ITN.Taxon_List_Item_Key=SDL.Taxon_List_Item_Key

	-- Select table and sort appropriately
	IF @SortOrderIndex = 0
		SELECT * FROM @SpecimensSearch
		ORDER BY Item_Name, Number
	ELSE IF @SortOrderIndex = 1
		SELECT * FROM @SpecimensSearch
		ORDER BY Number, Item_Name
GO

/*============================================================================*\
  Grant permissions.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_Specimens_Select_ForSearchByGatheringLocation') IS NOT NULL
BEGIN
	PRINT 'Setting up security on procedure usp_Specimens_Select_ForSearchByGatheringLocation'
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByGatheringLocation TO R2k_AddOnly
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByGatheringLocation TO R2k_Administrator
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByGatheringLocation TO R2k_FullEdit
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByGatheringLocation TO R2k_ReadOnly
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByGatheringLocation TO R2k_RecordCardsOnly
END
GO
If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Specimens_Select_ForSearchByGeographicInformation]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Specimens_Select_ForSearchByGeographicInformation]
GO

/*===========================================================================*\
  Description:
	Returns Specimens data based on the search parameter for Geographic Information.

  Parameters:
	@UserDomainMask		User's Domain Mask restricting which records may be returned
	@SessionID 		User's SessionID
	@ShowCommonNames	Specifies whether or not Common Names should be shown
	@SearchText		Text to be searched on
	@SortOrderIndex		Index determining Sort Order

  Created:
	October 2003

  Last revision information:
    $Revision: 5 $
    $Date: 17/08/11 15:40 $
    $Author: Jamesbichard $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_Specimens_Select_ForSearchByGeographicInformation] 
	@UserDomainMask INT,
	@SessionID CHAR(16),
	@ShowCommonNames BIT,
	@SearchText VARCHAR(100),
	@SortOrderIndex TINYINT
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
			AND M.Metadata_Type_Key='SYSTEM0000000005'
	JOIN 	Collection_Unit CU ON SU.Collection_Unit_Key = CU.Collection_Unit_Key 
	 		AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
			OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
	WHERE 	M.Text LIKE @SearchText + '%'

	UPDATE		@SpecimensSearch
	SET			Number = CUN.Number
	FROM 		@SpecimensSearch 		SU
	LEFT JOIN 	Collection_Unit_Number 	CUN ON 	SU.Item_key 	= CUN.Collection_Unit_Key 
											AND CUN.Preferred	= 1

	UPDATE 	@SpecimensSearch
	SET	Det_Item_Key=CPref.Concept_Key,
		Item_Name = CPref.Published_Term,
		Det_Item_Name=TDet.Plaintext
	FROM @SpecimensSearch SU
	INNER JOIN VW_SpecimenDetsEarth SDE ON SDE.Collection_Unit_Key=SU.Item_Key
		AND SDE.Preferred_Determination_Key=SDE.Determination_Key
	INNER JOIN Concept C ON C.Concept_Key=SDE.Concept_Key
	INNER JOIN Term TDet ON TDet.Term_Key=C.Term_Key
	INNER JOIN Concept CPref ON CPref.Meaning_Key=C.Meaning_Key
			AND CPref.List_Preferred=1
			AND CPref.Concept_Group_Key=C.Concept_Group_Key
	

	UPDATE @SpecimensSearch
	SET	Det_Item_Key = SDL.Taxon_List_Item_Key,
		Item_Name = dbo.ufn_GetFormattedTaxonNameByParams(
				Preferred_Name,
				Preferred_Name_Italic,
				Common_Name,
				Common_Name_Italic,
				NULL,
				@ShowCommonNames),
		Det_Item_Name = ITN.Actual_Name
	FROM 	@SpecimensSearch SU
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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Specimens_Select_ForSearchByGeographicInformation') AND SysStat & 0xf = 4)
BEGIN
   	PRINT 'Setting up security on procedure usp_Specimens_Select_ForSearchByGeographicInformation'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
       	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByGeographicInformation TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByGeographicInformation TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByGeographicInformation TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByGeographicInformation TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByGeographicInformation TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
       	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByGeographicInformation TO [Dev - JNCC SQL]
END
GO
SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

/*============================================================================*\
  Drop stored proc before re-creating.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_Specimens_Select_ForSearchByMetadata') IS NOT NULL
	DROP PROCEDURE dbo.usp_Specimens_Select_ForSearchByMetadata
GO

/*============================================================================*\
  Description:	Returns specimens based on their metadata of the specified type.

  Parameters:	@UserDomainMask			User's Domain Mask restricting which
										records may be returned
				@SessionID 				User's SessionID
				@ShowCommonNames		Specifies whether Common Names are shown
				@ShowOriginalSpecimenNames
										0 => preferred names are shown
										1 => names originally entered are shown
				@SearchText				Text to search for
				@SortOrderIndex			0 => results sorted by Item_Name, Number
										1 => results sorted by Number, Item_Name
				@MetaDataType			The type of metadata to search on

  Created:		September 2007

  Last revision information:
    $Revision: 5 $
    $Date: 17/08/11 15:40 $
    $Author: Jamesbichard $
\*============================================================================*/
CREATE PROCEDURE dbo.usp_Specimens_Select_ForSearchByMetadata
	@UserDomainMask						INT,
	@SessionID							CHAR(16),
	@ShowCommonNames					BIT,
	@ShowOriginalSpecimenNames			BIT,
	@SearchText							VARCHAR(100),
	@SortOrderIndex						TINYINT,
	@MetaDataType						VARCHAR(100)
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

	UPDATE		@SpecimensSearch
	SET			Number = CUN.Number
	FROM 		@SpecimensSearch 		SU
	LEFT JOIN 	Collection_Unit_Number 	CUN ON 	SU.Item_key 	= CUN.Collection_Unit_Key 
											AND CUN.Preferred	= 1

	UPDATE 	@SpecimensSearch
	SET	Det_Item_Key=CPref.Concept_Key,
		Item_Name = CASE @ShowOriginalSpecimenNames
						WHEN 1 THEN C.Published_Term
						ELSE CPref.Published_Term END,
		Det_Item_Name=TDet.Plaintext
	FROM @SpecimensSearch SU
	INNER JOIN VW_SpecimenDetsEarth SDE ON SDE.Collection_Unit_Key=SU.Item_Key
		AND SDE.Preferred_Determination_Key=SDE.Determination_Key
	INNER JOIN Concept C ON C.Concept_Key=SDE.Concept_Key
	INNER JOIN Term TDet ON TDet.Term_Key=C.Term_Key
	INNER JOIN Concept CPref ON CPref.Meaning_Key=C.Meaning_Key
			AND CPref.List_Preferred=1
			AND CPref.Concept_Group_Key=C.Concept_Group_Key
	
	UPDATE @SpecimensSearch
	SET	Det_Item_Key = SDL.Taxon_List_Item_Key,
		Item_Name = dbo.ufn_GetFormattedTaxonNameByParams(
				CASE @ShowOriginalSpecimenNames
					WHEN 1 THEN Actual_Name
					ELSE Preferred_Name END,
				CASE @ShowOriginalSpecimenNames
					WHEN 1 THEN Actual_Name_Italic
					ELSE Preferred_Name_Italic END,
				Common_Name,
				Common_Name_Italic,
				NULL,
				@ShowCommonNames),
		Det_Item_Name = ITN.Actual_Name
	FROM 	@SpecimensSearch SU
	INNER JOIN VW_SpecimenDetsLife SDL ON SDL.Collection_Unit_Key = SU.Item_Key
			AND SDL.Preferred_Taxon_Determination_Key = SDL.Taxon_Determination_Key
	INNER JOIN Index_Taxon_Name ITN	ON ITN.Taxon_List_Item_Key = SDL.Taxon_List_Item_Key

	-- Select table and sort appropriately
	IF @SortOrderIndex = 0
		SELECT * FROM @SpecimensSearch
		ORDER BY Item_Name, Number
	ELSE IF @SortOrderIndex = 1
		SELECT * FROM @SpecimensSearch
		ORDER BY Number, Item_Name
GO

/*============================================================================*\
  Grant permissions.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_Specimens_Select_ForSearchByMetadata') IS NOT NULL
BEGIN
	PRINT 'Setting up security on procedure usp_Specimens_Select_ForSearchByMetadata'
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByMetadata TO R2k_AddOnly
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByMetadata TO R2k_Administrator
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByMetadata TO R2k_FullEdit
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByMetadata TO R2k_ReadOnly
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByMetadata TO R2k_RecordCardsOnly
END
GO
If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Specimens_Select_ForSearchByNomenclaturalStatus]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Specimens_Select_ForSearchByNomenclaturalStatus]
GO

/*===========================================================================*\
  Description:
	Returns Specimens data based on the search parameter for Nomenclatural Status.

  Parameters:	@UserDomainMask		User's Domain Mask restricting which
									records may be returned
				@SessionID 			User's SessionID
				@ShowCommonNames	Specifies whether or not Common Names
									should be shown
				@ShowOriginalSpecimenNames
									0 => preferred names are shown
									1 => names originally entered are shown
				@SearchText			Text to be searched on
				@SortOrderIndex		Index determining Sort Order

  Created:
	November 2009

  Last revision information:
    $Revision: 5 $
    $Date: 17/08/11 15:40 $
    $Author: Jamesbichard $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_Specimens_Select_ForSearchByNomenclaturalStatus] 
	@UserDomainMask				INT,
	@SessionID					CHAR(16),
	@ShowCommonNames			BIT,
	@ShowOriginalSpecimenNames	BIT,
	@SearchText					VARCHAR(50),
	@SortOrderIndex				TINYINT
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
		[Hint] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL 
	)

	INSERT INTO		@SpecimensSearch
					(
						Item_Key,
						Life_Sciences
					) 
	SELECT			DISTINCT
					SU.Collection_Unit_Key,
					SU.Life_Sciences
	FROM			dbo.VW_ConceptTerm				AS		CT
	LEFT JOIN		dbo.Search_Term					AS		ST
	ON				ST.Concept_Key					=		CT.Concept_Key
	INNER JOIN		dbo.Determination				AS		D
	ON				CT.Concept_Key					=		D.Nomenclatural_Status_Concept_Key
	INNER JOIN		dbo.Specimen_Unit				AS		SU
	ON				D.Specimen_Collection_Unit_Key	=		SU.Collection_Unit_Key
	INNER JOIN		Collection_Unit CU 
	ON				SU.Collection_Unit_Key = CU.Collection_Unit_Key 
				 	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
					OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
	WHERE			ST.Plaintext					LIKE	@SearchText + '%'

	UPDATE			@SpecimensSearch
	SET				Number							=		CUN.Number
	FROM 			@SpecimensSearch				AS		SU
	LEFT JOIN		Collection_Unit_Number			AS		CUN
	ON				SU.Item_key						=		CUN.Collection_Unit_Key 
	AND				CUN.Preferred					=		1
	
	UPDATE			@SpecimensSearch
	SET				Det_Item_Key					=		CPref.Concept_Key,
					Item_Name						=		CASE @ShowOriginalSpecimenNames
																WHEN 1
																THEN C.Published_Term
																ELSE CPref.Published_Term
															END,
					Det_Item_Name					=		TDet.Plaintext
	FROM			@SpecimensSearch				AS		SU
	INNER JOIN		VW_SpecimenDetsEarth			AS		SDE
	ON				SDE.Collection_Unit_Key			=		SU.Item_Key
	AND				SDE.Preferred_Determination_Key	=		SDE.Determination_Key
	INNER JOIN		Concept							AS		C
	ON				C.Concept_Key					=		SDE.Concept_Key
	INNER JOIN		Term							AS		TDet
	ON				TDet.Term_Key					=		C.Term_Key
	INNER JOIN		Concept							AS		CPref
	ON				CPref.Meaning_Key				=		C.Meaning_Key
	AND				CPref.List_Preferred			=		1
	AND				CPref.Concept_Group_Key			=		C.Concept_Group_Key
	
	UPDATE			@SpecimensSearch
	SET				Det_Item_Key					=		SDL.Taxon_List_Item_Key,
					Item_Name						=		dbo.ufn_GetFormattedTaxonNameByParams(
																	CASE @ShowOriginalSpecimenNames
																		WHEN 1
																		THEN Actual_Name
																		ELSE Preferred_Name
																	END,
																	CASE @ShowOriginalSpecimenNames
																		WHEN 1
																		THEN Actual_Name_Italic
																		ELSE Preferred_Name_Italic
																	END,
																	Common_Name,
																	Common_Name_Italic,
																	null,
																	@ShowCommonNames),
					Det_Item_Name					=		ITN.Actual_Name,
					Hint							=		ITN.Actual_Name
	FROM			@SpecimensSearch				AS		SU
	INNER JOIN		VW_SpecimenDetsLife				AS		SDL
	ON				SDL.Collection_Unit_Key			=		SU.Item_Key
	AND				SDL.Taxon_Determination_Key		=		SDL.Preferred_Taxon_Determination_Key
	INNER JOIN		Index_Taxon_Name				AS		ITN
	ON				ITN.Taxon_List_Item_Key			=		SDL.Taxon_List_Item_Key

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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Specimens_Select_ForSearchByNomenclaturalStatus') AND SysStat & 0xf = 4)
BEGIN
   	PRINT 'Setting up security on procedure usp_Specimens_Select_ForSearchByNomenclaturalStatus'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
       	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByNomenclaturalStatus TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByNomenclaturalStatus TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByNomenclaturalStatus TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByNomenclaturalStatus TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByNomenclaturalStatus TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
       	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByNomenclaturalStatus TO [Dev - JNCC SQL]
END
GO
SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

/*============================================================================*\
  Drop stored proc before re-creating.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_Specimens_Select_ForSearchByObservationLocation') IS NOT NULL
	DROP PROCEDURE dbo.usp_Specimens_Select_ForSearchByObservationLocation
GO

/*============================================================================*\
  Description:	Returns specimens based on their observation location.

  Parameters:	@UserDomainMask			User's Domain Mask restricting which
										records may be returned
				@SessionID 				User's SessionID
				@ShowCommonNames		Specifies whether Common Names are shown
				@ShowOriginalSpecimenNames
										0 => preferred names are shown
										1 => names originally entered are shown
				@SearchText				Text to search for
				@SortOrderIndex			0 => results sorted by Item_Name, Number
										1 => results sorted by Number, Item_Name

  Created:		September 2010

  Last revision information:
    $Revision: 5 $
    $Date: 17/08/11 15:40 $
    $Author: Jamesbichard $
\*============================================================================*/
CREATE PROCEDURE dbo.usp_Specimens_Select_ForSearchByObservationLocation
	@UserDomainMask						INT,
	@SessionID							CHAR(16),
	@ShowCommonNames					BIT,
	@ShowOriginalSpecimenNames			BIT,
	@SearchText							VARCHAR(50),
	@SortOrderIndex						TINYINT
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
		[Hint] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL 
	)

	INSERT INTO		@SpecimensSearch 
					(
						Item_Key, 
						Life_Sciences
					) 
	SELECT			DISTINCT 
					SU.Collection_Unit_Key, 
					SU.Life_Sciences
	FROM			Specimen_Unit SU
	INNER JOIN		Collection_Unit CU 
	ON				SU.Collection_Unit_Key	=		CU.Collection_Unit_Key 
	 	AND			((CU.Domain_Mask & @UserDomainMask > 0)		OR (CU.Entered_Session_ID = @SessionID) 
		OR			(CU.Changed_Session_ID	=		@SessionID) OR (CU.Domain_Mask = 0))
	INNER JOIN		Specimen_Field_Data		AS		SFD 
	ON				SFD.Collection_Unit_Key =		SU.Collection_Unit_Key
	LEFT JOIN		Occurrence				AS		O	
	ON				O.Occurrence_Key		=		SFD.Occurrence_Key
	LEFT JOIN		Taxon_Occurrence		AS		XO
	ON				XO.Taxon_Occurrence_Key	=		SFD.Taxon_Occurrence_Key
	INNER JOIN		[Sample]				AS		S
	ON				S.Sample_Key			=		O.Sample_Key 
		OR			S.Sample_Key			=		XO.Sample_Key
	LEFT JOIN		Metadata				AS		M
	ON				SU.collection_unit_key	=		M.Record_Key
	LEFT JOIN		Metadata_Type			AS		MT
	ON				M.metadata_type_key		=		MT.metadata_type_key
	AND				MT.Item_Name			=		'Provenance'
	WHERE			(ISNULL(S.Location_Name, '')	LIKE	@SearchText + '%'
		OR			ISNULL(M.Text, '')				LIKE	@SearchText + '%')		

	UPDATE			@SpecimensSearch
	SET				Number							=		CUN.Number
	FROM 			@SpecimensSearch				AS		SU
	LEFT JOIN		Collection_Unit_Number			AS		CUN
	ON				SU.Item_key						=		CUN.Collection_Unit_Key 
	AND				CUN.Preferred					=		1
	
	UPDATE			@SpecimensSearch
	SET				Det_Item_Key					=		CPref.Concept_Key,
					Item_Name						=		CASE @ShowOriginalSpecimenNames
																WHEN 1
																THEN C.Published_Term
																ELSE CPref.Published_Term
															END,
					Det_Item_Name					=		TDet.Plaintext
	FROM			@SpecimensSearch				AS		SU
	INNER JOIN		VW_SpecimenDetsEarth			AS		SDE
	ON				SDE.Collection_Unit_Key			=		SU.Item_Key
	AND				SDE.Preferred_Determination_Key	=		SDE.Determination_Key
	INNER JOIN		Concept							AS		C
	ON				C.Concept_Key					=		SDE.Concept_Key
	INNER JOIN		Term							AS		TDet
	ON				TDet.Term_Key					=		C.Term_Key
	INNER JOIN		Concept							AS		CPref
	ON				CPref.Meaning_Key				=		C.Meaning_Key
	AND				CPref.List_Preferred			=		1
	AND				CPref.Concept_Group_Key			=		C.Concept_Group_Key
	
	UPDATE			@SpecimensSearch
	SET				Det_Item_Key					=		SDL.Taxon_List_Item_Key,
					Item_Name						=		dbo.ufn_GetFormattedTaxonNameByParams(
																	CASE @ShowOriginalSpecimenNames
																		WHEN 1
																		THEN Actual_Name
																		ELSE Preferred_Name
																	END,
																	CASE @ShowOriginalSpecimenNames
																		WHEN 1
																		THEN Actual_Name_Italic
																		ELSE Preferred_Name_Italic
																	END,
																	Common_Name,
																	Common_Name_Italic,
																	null,
																	@ShowCommonNames),
					Det_Item_Name					=		ITN.Actual_Name,
					Hint							=		ITN.Actual_Name
	FROM			@SpecimensSearch				AS		SU
	INNER JOIN		VW_SpecimenDetsLife				AS		SDL
	ON				SDL.Collection_Unit_Key			=		SU.Item_Key
	AND				SDL.Taxon_Determination_Key		=		SDL.Preferred_Taxon_Determination_Key
	INNER JOIN		Index_Taxon_Name				AS		ITN
	ON				ITN.Taxon_List_Item_Key			=		SDL.Taxon_List_Item_Key

	-- Select table and sort appropriately
	IF @SortOrderIndex = 0
		SELECT * FROM @SpecimensSearch
		ORDER BY Det_Item_Name, Number
	ELSE IF @SortOrderIndex = 1
		SELECT * FROM @SpecimensSearch
		ORDER BY Number, Item_Name
GO

/*============================================================================*\
  Grant permissions.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_Specimens_Select_ForSearchByObservationLocation') IS NOT NULL
BEGIN
	PRINT 'Setting up security on procedure usp_Specimens_Select_ForSearchByObservationLocation'
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByObservationLocation TO R2k_AddOnly
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByObservationLocation TO R2k_Administrator
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByObservationLocation TO R2k_FullEdit
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByObservationLocation TO R2k_ReadOnly
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByGatheringDate TO R2k_RecordCardsOnly
END
GO
SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

/*============================================================================*\
  Drop stored proc before re-creating.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_Specimens_Select_ForSearchByPreferredAccNumber') IS NOT NULL
	DROP PROCEDURE dbo.usp_Specimens_Select_ForSearchByPreferredAccNumber
GO

/*============================================================================*\
  Description:	Returns specimens based on their preferred accession numbers.

  Parameters:	@UserDomainMask			User's Domain Mask restricting which
										records may be returned
				@SessionID 				User's SessionID
				@ShowCommonNames		Specifies whether Common Names are shown
				@ShowOriginalSpecimenNames
										0 => preferred names are shown
										1 => names originally entered are shown
				@SearchText				Text to search for
				@SortOrderIndex			0 => results sorted by Item_Name, Number
										1 => results sorted by Number, Item_Name

  Created:		October 2003

  Last revision information:
    $Revision: 5 $
    $Date: 17/08/11 15:40 $
    $Author: Jamesbichard $
\*============================================================================*/
CREATE PROCEDURE dbo.usp_Specimens_Select_ForSearchByPreferredAccNumber
	@UserDomainMask						INT,
	@SessionID							CHAR(16),
	@ShowCommonNames					BIT,
	@ShowOriginalSpecimenNames			BIT,
	@SearchText							VARCHAR(30),
	@SortOrderIndex						TINYINT
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
		[Hint] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL 
	)

	INSERT INTO @SpecimensSearch (Item_Key, Life_Sciences, Hint) 
	SELECT DISTINCT SU.Collection_Unit_Key, SU.Life_Sciences, M.Number
	FROM SPECIMEN_UNIT SU
	INNER JOIN Collection_Unit CU ON SU.Collection_Unit_Key = CU.Collection_Unit_Key 
	 	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
		OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
	INNER JOIN Movement_Collection_Unit MCU ON MCU.Collection_Unit_Key = SU.Collection_Unit_Key
	INNER JOIN Movement_Direction MD ON MD.Movement_Direction_Key=MCU.Movement_Direction_Key
		AND MD.Outbound=0
	INNER JOIN Movement_Of_Ownership MOE ON MOE.Movement_Direction_Key=MD.Movement_Direction_Key
	LEFT JOIN Movement_Of_Ownership_Exclusion MOEE 
		ON MOEE.Movement_Of_Ownership_Key=MOE.Movement_Of_Ownership_Key
		AND SU.Collection_Unit_Key=MOEE.Collection_Unit_Key
	INNER JOIN Movement M ON M.Movement_Key=MD.Movement_Key
		AND M.Movement_Type IN (0,1)
	WHERE M.Number LIKE @SearchText + '%'
	AND MOEE.Movement_Of_Ownership_Exclusion_Key IS NULL

	UPDATE		@SpecimensSearch
	SET			Number = CUN.Number
	FROM 		@SpecimensSearch 		SU
	LEFT JOIN 	Collection_Unit_Number 	CUN ON 	SU.Item_key 	= CUN.Collection_Unit_Key 
											AND CUN.Preferred	= 1

	UPDATE @SpecimensSearch
	SET	Det_Item_Key=CPref.Concept_Key,
		Item_Name = CASE @ShowOriginalSpecimenNames
						WHEN 1 THEN C.Published_Term
						ELSE CPref.Published_Term END,
		Det_Item_Name=TDet.Plaintext
	FROM @SpecimensSearch SU
	INNER JOIN VW_SpecimenDetsEarth SDE ON SDE.Collection_Unit_Key=SU.Item_Key
		AND SDE.Preferred_Determination_Key=SDE.Determination_Key
	INNER JOIN Concept C ON C.Concept_Key=SDE.Concept_Key
	INNER JOIN Term TDet ON TDet.Term_Key=C.Term_Key
	INNER JOIN Concept CPref ON CPref.Meaning_Key=C.Meaning_Key
		AND CPref.List_Preferred=1
		AND CPref.Concept_Group_Key=C.Concept_Group_Key
	
	UPDATE @SpecimensSearch
	SET	Det_Item_Key=SDL.Taxon_List_Item_Key,
		Item_Name = dbo.ufn_GetFormattedTaxonNameByParams(
				CASE @ShowOriginalSpecimenNames
					WHEN 1 THEN Actual_Name
					ELSE Preferred_Name END,
				CASE @ShowOriginalSpecimenNames
					WHEN 1 THEN Actual_Name_Italic
					ELSE Preferred_Name_Italic END,
				Common_Name,
				Common_Name_Italic,
				null,
				@ShowCommonNames),
		Det_Item_Name=ITN.Actual_Name
	FROM @SpecimensSearch SU
	INNER JOIN VW_SpecimenDetsLife SDL ON SDL.Collection_Unit_Key=SU.Item_Key
			AND SDL.Preferred_Taxon_Determination_Key = SDL.Taxon_Determination_Key
	INNER JOIN Index_Taxon_Name ITN	ON ITN.Taxon_List_Item_Key=SDL.Taxon_List_Item_Key

	-- Select table and sort appropriately
	IF @SortOrderIndex = 0
		SELECT * FROM @SpecimensSearch
		ORDER BY Item_Name, Number
	ELSE IF @SortOrderIndex = 1
		SELECT * FROM @SpecimensSearch
		ORDER BY Number, Item_Name
GO

/*============================================================================*\
  Grant permissions.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_Specimens_Select_ForSearchByPreferredAccNumber') IS NOT NULL
BEGIN
	PRINT 'Setting up security on procedure usp_Specimens_Select_ForSearchByPreferredAccNumber'
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByPreferredAccNumber TO R2k_AddOnly
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByPreferredAccNumber TO R2k_Administrator
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByPreferredAccNumber TO R2k_FullEdit
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByPreferredAccNumber TO R2k_ReadOnly
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByPreferredAccNumber TO R2k_RecordCardsOnly
END
GO
SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

/*============================================================================*\
  Drop stored proc before re-creating.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_Specimens_Select_ForSearchByPreferredDetermination') IS NOT NULL
	DROP PROCEDURE dbo.usp_Specimens_Select_ForSearchByPreferredDetermination
GO

/*============================================================================*\
  Description:	Returns specimens based on their preferred determinations.

  Parameters:	@UserDomainMask			User's Domain Mask restricting which
										records may be returned
				@SessionID 				User's SessionID
				@ShowCommonNames		Specifies whether Common Names are shown
				@ShowOriginalSpecimenNames
										0 => preferred names are shown
										1 => names originally entered are shown
				@SearchText				Text to search for
				@SortOrderIndex			0 => results sorted by Item_Name, Number
										1 => results sorted by Number, Item_Name

  Created:		October 2003

  Last revision information:
    $Revision: 5 $
    $Date: 17/08/11 15:40 $
    $Author: Jamesbichard $
\*============================================================================*/
CREATE PROCEDURE dbo.usp_Specimens_Select_ForSearchByPreferredDetermination
	@UserDomainMask						INT,
	@SessionID							CHAR(16),
	@ShowCommonNames					BIT,
	@ShowOriginalSpecimenNames			BIT,
	@SearchText							VARCHAR(150),
	@SortOrderIndex						TINYINT
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
		[Hint] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL 
	)

	--Find all specimens with a determination match
	INSERT INTO @SpecimensSearch (Item_Key, Life_Sciences) 
	SELECT DISTINCT SU.Collection_Unit_Key, SU.Life_Sciences
	FROM 	SPECIMEN_UNIT SU
	JOIN 	Collection_Unit CU ON SU.Collection_Unit_Key = CU.Collection_Unit_Key 
	 		AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
			OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
	JOIN 	VW_SpecimenDetsEarth SDE ON (SU.Collection_Unit_Key = SDE.Collection_Unit_Key)
			AND (SDE.Preferred_Determination_Key=SDE.Determination_Key)
	JOIN 	Concept C ON SDE.Concept_Key = C.Concept_Key
	JOIN 	Concept CSearch ON CSearch.Meaning_Key=C.Meaning_Key
	JOIN 	Term TSearch ON TSearch.Term_Key=CSearch.Term_Key
	LEFT JOIN Search_Term ST ON ST.Concept_Key = CSearch.Concept_Key
	WHERE (ST.Plaintext LIKE @SearchText + '%' 
			AND SU.Life_Sciences=0) 

	INSERT INTO @SpecimensSearch (Item_Key, Life_Sciences) 
	SELECT DISTINCT SU.Collection_Unit_Key, SU.Life_Sciences
	FROM 	SPECIMEN_UNIT SU
	JOIN 	Collection_Unit CU ON SU.Collection_Unit_Key = CU.Collection_Unit_Key 
	 		AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
			OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
	JOIN 	VW_SpecimenDetsLife SDL ON SU.Collection_Unit_Key = SDL.Collection_Unit_Key
			AND (SDL.Preferred_Taxon_Determination_Key=SDL.Taxon_Determination_Key)
	JOIN 	Index_Taxon_Synonym ITS ON ITS.Taxon_List_Item_Key=SDL.Taxon_List_Item_Key
	JOIN 	INDEX_TAXON_NAME ITN	ON ITS.Synonym_List_Item_Key = ITN.Taxon_List_Item_Key
	WHERE 	(ITN.Actual_Name LIKE @SearchText + '%' AND SU.Life_Sciences=1)

	UPDATE		@SpecimensSearch
	SET			Number = CUN.Number
	FROM 		@SpecimensSearch 		SU
	LEFT JOIN 	Collection_Unit_Number 	CUN ON 	SU.Item_key 	= CUN.Collection_Unit_Key 
											AND CUN.Preferred	= 1

	UPDATE @SpecimensSearch
	SET	Det_Item_Key=CPref.Concept_Key,
		Item_Name = CASE @ShowOriginalSpecimenNames
						WHEN 1 THEN C.Published_Term
						ELSE CPref.Published_Term END,
		Det_Item_Name=TDet.Plaintext,
		Hint=TDet.Plaintext
	FROM @SpecimensSearch SU
	INNER JOIN VW_SpecimenDetsEarth SDE ON SDE.Collection_Unit_Key=SU.Item_Key
		AND SDE.Preferred_Determination_Key=SDE.Determination_Key
	INNER JOIN Concept C ON C.Concept_Key=SDE.Concept_Key
	INNER JOIN Term TDet ON TDet.Term_Key=C.Term_Key
	INNER JOIN Concept CPref ON CPref.Meaning_Key=C.Meaning_Key
		AND CPref.List_Preferred=1
		AND CPref.Concept_Group_Key=C.Concept_Group_Key
	
	UPDATE @SpecimensSearch
	SET	Det_Item_Key=SDL.Taxon_List_Item_Key,
		Item_Name = dbo.ufn_GetFormattedTaxonNameByParams(
				CASE @ShowOriginalSpecimenNames
					WHEN 1 THEN Actual_Name
					ELSE Preferred_Name END,
				CASE @ShowOriginalSpecimenNames
					WHEN 1 THEN Actual_Name_Italic
					ELSE Preferred_Name_Italic END,
				Common_Name,
				Common_Name_Italic,
				null,
				@ShowCommonNames),
		Det_Item_Name=ITN.Actual_Name,
		Hint=ITN.Actual_Name
	FROM @SpecimensSearch SU
	INNER JOIN VW_SpecimenDetsLife SDL ON SDL.Collection_Unit_Key=SU.Item_Key
			AND SDL.Preferred_Taxon_Determination_Key = SDL.Taxon_Determination_Key
	INNER JOIN Index_Taxon_Name ITN	ON ITN.Taxon_List_Item_Key=SDL.Taxon_List_Item_Key

	-- Select table and sort appropriately
	IF @SortOrderIndex = 0
		SELECT * FROM @SpecimensSearch
		ORDER BY Item_Name, Number
	ELSE IF @SortOrderIndex = 1
		SELECT * FROM @SpecimensSearch
		ORDER BY Number, Item_Name
GO

/*============================================================================*\
  Grant permissions.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_Specimens_Select_ForSearchByPreferredDetermination') IS NOT NULL
BEGIN
	PRINT 'Setting up security on procedure usp_Specimens_Select_ForSearchByPreferredDetermination'
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByPreferredDetermination TO R2k_AddOnly
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByPreferredDetermination TO R2k_Administrator
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByPreferredDetermination TO R2k_FullEdit
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByPreferredDetermination TO R2k_ReadOnly
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByPreferredDetermination TO R2k_RecordCardsOnly
END
GO
SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

/*============================================================================*\
  Drop stored proc before re-creating.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_Specimens_Select_ForSearchByPreferredNumber') IS NOT NULL
	DROP PROCEDURE dbo.usp_Specimens_Select_ForSearchByPreferredNumber
GO

/*============================================================================*\
  Description:	Returns specimens based on their preferred number.

  Parameters:	@UserDomainMask			User's Domain Mask restricting which
										records may be returned
				@SessionID 				User's SessionID
				@ShowCommonNames		Specifies whether Common Names are shown
				@ShowOriginalSpecimenNames
										0 => preferred names are shown
										1 => names originally entered are shown
				@SearchText				Text to search for
				@SortOrderIndex			0 => results sorted by Item_Name, Number
										1 => results sorted by Number, Item_Name

  Created:		October 2003

  Last revision information:
    $Revision: 5 $
    $Date: 17/08/11 15:40 $
    $Author: Jamesbichard $
\*============================================================================*/
CREATE PROCEDURE dbo.usp_Specimens_Select_ForSearchByPreferredNumber
	@UserDomainMask						INT,
	@SessionID							CHAR(16),
	@ShowCommonNames					BIT,
	@ShowOriginalSpecimenNames			BIT,
	@SearchText							VARCHAR(30),
	@SortOrderIndex						TINYINT
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
		[Hint] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL 
	)

	INSERT INTO @SpecimensSearch (Item_Key, Life_Sciences, Hint) 
	SELECT DISTINCT SU.Collection_Unit_Key, SU.Life_Sciences, CUN.Number
	FROM 	Specimen_Unit SU
	JOIN 	Collection_Unit CU ON SU.Collection_Unit_Key = CU.Collection_Unit_Key 
	   		AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
			OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
	JOIN 	Collection_Unit_Number CUN ON SU.Collection_Unit_key = CUN.Collection_Unit_Key 
			AND CUN.Preferred = 1
			AND CUN.Number LIKE @SearchText + '%'

	UPDATE		@SpecimensSearch
	SET			Number = CUN.Number
	FROM 		@SpecimensSearch 		SU
	LEFT JOIN 	Collection_Unit_Number 	CUN ON 	SU.Item_key 	= CUN.Collection_Unit_Key 
											AND CUN.Preferred	= 1

	UPDATE @SpecimensSearch
	SET	Det_Item_Key=CPref.Concept_Key,
		Item_Name = CASE @ShowOriginalSpecimenNames
						WHEN 1 THEN C.Published_Term
						ELSE CPref.Published_Term END,
		Det_Item_Name=TDet.Plaintext
	FROM @SpecimensSearch SU
	INNER JOIN VW_SpecimenDetsEarth SDE ON SDE.Collection_Unit_Key=SU.Item_Key
		AND SDE.Preferred_Determination_Key=SDE.Determination_Key
	INNER JOIN Concept C ON C.Concept_Key=SDE.Concept_Key
	INNER JOIN Term TDet ON TDet.Term_Key=C.Term_Key
	INNER JOIN Concept CPref ON CPref.Meaning_Key=C.Meaning_Key
		AND CPref.List_Preferred=1
		AND CPref.Concept_Group_Key=C.Concept_Group_Key
	
	UPDATE @SpecimensSearch
	SET	Det_Item_Key=SDL.Taxon_List_Item_Key,
		Item_Name = dbo.ufn_GetFormattedTaxonNameByParams(
				CASE @ShowOriginalSpecimenNames
					WHEN 1 THEN Actual_Name
					ELSE Preferred_Name END,
				CASE @ShowOriginalSpecimenNames
					WHEN 1 THEN Actual_Name_Italic
					ELSE Preferred_Name_Italic END,
				Common_Name,
				Common_Name_Italic,
				null,
				@ShowCommonNames),
		Det_Item_Name=ITN.Actual_Name
	FROM @SpecimensSearch SU
	INNER JOIN VW_SpecimenDetsLife SDL ON SDL.Collection_Unit_Key=SU.Item_Key
			AND SDL.Preferred_Taxon_Determination_Key = SDL.Taxon_Determination_Key
	INNER JOIN Index_Taxon_Name ITN	ON ITN.Taxon_List_Item_Key=SDL.Taxon_List_Item_Key

	-- Select table and sort appropriately
	IF @SortOrderIndex = 0
		SELECT * FROM @SpecimensSearch
		ORDER BY Item_Name, Number
	ELSE IF @SortOrderIndex = 1
		SELECT * FROM @SpecimensSearch
		ORDER BY Number, Item_Name
GO

/*============================================================================*\
  Grant permissions.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_Specimens_Select_ForSearchByPreferredNumber') IS NOT NULL
BEGIN
	PRINT 'Setting up security on procedure usp_Specimens_Select_ForSearchByPreferredNumber'
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByPreferredNumber TO R2k_AddOnly
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByPreferredNumber TO R2k_Administrator
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByPreferredNumber TO R2k_FullEdit
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByPreferredNumber TO R2k_ReadOnly
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByPreferredNumber TO R2k_RecordCardsOnly
END
GO
SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

/*============================================================================*\
  Drop stored proc before re-creating.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_Specimens_Select_ForSearchByType') IS NOT NULL
	DROP PROCEDURE dbo.usp_Specimens_Select_ForSearchByType
GO

/*============================================================================*\
  Description:	Returns specimens based on their specimen type.

  Parameters:	@UserDomainMask			User's Domain Mask restricting which
										records may be returned
				@SessionID 				User's SessionID
				@ShowCommonNames		Specifies whether Common Names are shown
				@ShowOriginalSpecimenNames
										0 => preferred names are shown
										1 => names originally entered are shown
				@SearchText				Text to search for
				@SortOrderIndex			0 => results sorted by Item_Name, Number
										1 => results sorted by Number, Item_Name

  Created:		October 2003

  Last revision information:
    $Revision: 5 $
    $Date: 17/08/11 15:40 $
    $Author: Jamesbichard $
\*============================================================================*/
CREATE PROCEDURE dbo.usp_Specimens_Select_ForSearchByType
	@UserDomainMask						INT,
	@SessionID							CHAR(16),
	@ShowCommonNames					BIT,
	@ShowOriginalSpecimenNames			BIT,
	@SearchText							VARCHAR(150),
	@SortOrderIndex						TINYINT
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
		[Hint] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL 
	)

	INSERT INTO @SpecimensSearch (Item_Key, Life_Sciences, Hint) 
	SELECT DISTINCT SU.Collection_Unit_Key, SU.Life_Sciences, T.Plaintext
	FROM 		SPECIMEN_UNIT SU
	JOIN 		Collection_Unit CU ON SU.Collection_Unit_Key = CU.Collection_Unit_Key 
	 			AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
				OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
	JOIN 		Concept C ON C.Concept_Key = SU.Specimen_Type_Concept_Key
	JOIN 		Concept CSyn ON CSyn.Meaning_Key=C.Meaning_Key
	JOIN 		Term T ON T.Term_Key=CSyn.Term_Key
	LEFT JOIN	Search_Term ST ON ST.Concept_Key = CSyn.Concept_Key
	WHERE 		ST.Plaintext LIKE @Searchtext + '%'

	UPDATE		@SpecimensSearch
	SET			Number = CUN.Number
	FROM 		@SpecimensSearch 		SU
	LEFT JOIN 	Collection_Unit_Number 	CUN ON 	SU.Item_key 	= CUN.Collection_Unit_Key 
											AND CUN.Preferred	= 1

	UPDATE @SpecimensSearch
	SET	Det_Item_Key=CPref.Concept_Key,
		Item_Name = CASE @ShowOriginalSpecimenNames
						WHEN 1 THEN C.Published_Term
						ELSE CPref.Published_Term END,
		Det_Item_Name=TDet.Plaintext
	FROM @SpecimensSearch SU
	INNER JOIN VW_SpecimenDetsEarth SDE ON SDE.Collection_Unit_Key=SU.Item_Key
		AND SDE.Preferred_Determination_Key=SDE.Determination_Key
	INNER JOIN Concept C ON C.Concept_Key=SDE.Concept_Key
	INNER JOIN Term TDet ON TDet.Term_Key=C.Term_Key
	INNER JOIN Concept CPref ON CPref.Meaning_Key=C.Meaning_Key
		AND CPref.List_Preferred=1
		AND CPref.Concept_Group_Key=C.Concept_Group_Key
	
	UPDATE @SpecimensSearch
	SET	Det_Item_Key=SDL.Taxon_List_Item_Key,
		Item_Name = dbo.ufn_GetFormattedTaxonNameByParams(
				CASE @ShowOriginalSpecimenNames
					WHEN 1 THEN Actual_Name
					ELSE Preferred_Name END,
				CASE @ShowOriginalSpecimenNames
					WHEN 1 THEN Actual_Name_Italic
					ELSE Preferred_Name_Italic END,
				Common_Name,
				Common_Name_Italic,
				null,
				@ShowCommonNames),
		Det_Item_Name=ITN.Actual_Name
	FROM @SpecimensSearch SU
	INNER JOIN VW_SpecimenDetsLife SDL ON SDL.Collection_Unit_Key=SU.Item_Key
			AND SDL.Preferred_Taxon_Determination_Key = SDL.Taxon_Determination_Key
	INNER JOIN Index_Taxon_Name ITN	ON ITN.Taxon_List_Item_Key=SDL.Taxon_List_Item_Key

	-- Select table and sort appropriately
	IF @SortOrderIndex = 0
		SELECT * FROM @SpecimensSearch
		ORDER BY Item_Name, Number
	ELSE IF @SortOrderIndex = 1
		SELECT * FROM @SpecimensSearch
		ORDER BY Number, Item_Name
GO

/*============================================================================*\
  Grant permissions.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_Specimens_Select_ForSearchByType') IS NOT NULL
BEGIN
	PRINT 'Setting up security on procedure usp_Specimens_Select_ForSearchByType'
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByType TO R2k_AddOnly
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByType TO R2k_Administrator
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByType TO R2k_FullEdit
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByType TO R2k_ReadOnly
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByType TO R2k_RecordCardsOnly
END
GO
SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

/*============================================================================*\
  Drop stored proc before re-creating.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_Specimens_Select_ForStore') IS NOT NULL
	DROP PROCEDURE dbo.usp_Specimens_Select_ForStore
GO

/*============================================================================*\
  Description:	Returns specimens' data to the Collections Browser for a
				specified store.

  Parameters:	@ParentKey				Identifies the store
				@UserDomainMask			User's domain mask restricting which
										records may be returned
				@SessionID 				User's SessionID
				@ShowCommonNames		Specifies whether Common Names are shown
				@ShowOriginalSpecimenNames
										0 => preferred names are shown
										1 => names originally entered are shown
				@SortOrderIndex			0 => results sorted by Item_Name, Number
										1 => results sorted by Number, Item_Name

  Created:		October 2003

  Last revision information:
	$Revision: 5 $
	$Date: 17/08/11 15:40 $
	$Author: Jamesbichard $
\*============================================================================*/
CREATE PROCEDURE dbo.usp_Specimens_Select_ForStore
	@UserDomainMask 					INT,
	@SessionID 							CHAR(16),
	@ParentKey 							CHAR(16),
	@ShowCommonNames 					BIT,
	@ShowOriginalSpecimenNames			BIT,
	@SortOrderIndex 					TINYINT
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


	-- Use temp table to build output, so silly data errors such as duplicate accessions
	-- don't duplicate in the list
	DECLARE @SpecimensSearch TABLE
	(
		Item_Key		CHAR(16) 		COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		Join_Key		CHAR(16) 		COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		Det_Item_Key	CHAR(16) 		COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		Det_Item_Name	NVARCHAR(150) 	COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		Item_Name		NVARCHAR(150) 	COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		Life_Sciences	BIT NULL,
		Number			VARCHAR(30) 	COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		Hint			NVARCHAR(150) 	COLLATE SQL_Latin1_General_CP1_CI_AS NULL 
	)

	INSERT INTO @SpecimensSearch (Item_Key, Join_Key, Life_Sciences) 
	SELECT 	DISTINCT SU.Collection_Unit_Key, SU.Collection_Unit_Key, SU.Life_Sciences
	FROM 	Specimen_Unit 	SU
	JOIN 	Collection_Unit CU 	ON 	SU.Collection_Unit_Key = CU.Collection_Unit_Key 
								AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
								OR 	(CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
	WHERE 	CU.Current_Container_Collection_Unit_Key = @ParentKey



	UPDATE		@SpecimensSearch
	SET			Number = CUN.Number
	FROM 		@SpecimensSearch 		SU
	LEFT JOIN 	Collection_Unit_Number 	CUN ON 	SU.Item_key 	= CUN.Collection_Unit_Key 
											AND CUN.Preferred	= 1

	UPDATE 	@SpecimensSearch
	SET		Det_Item_Key	= CPref.Concept_Key,
			Item_Name 		= CASE @ShowOriginalSpecimenNames
								WHEN 1 THEN C.Published_Term
								ELSE CPref.Published_Term END,
			Det_Item_Name	= TDet.Plaintext
	FROM 	@SpecimensSearch 		SU
	JOIN 	VW_SpecimenDetsEarth 	SDE 	ON 	SDE.Collection_Unit_Key			= SU.Item_Key
											AND SDE.Preferred_Determination_Key	= SDE.Determination_Key
	JOIN 	Concept 				C 		ON 	C.Concept_Key					= SDE.Concept_Key
	JOIN 	Term 					TDet 	ON 	TDet.Term_Key					= C.Term_Key
	JOIN 	Concept 				CPref 	ON 	CPref.Meaning_Key				= C.Meaning_Key
											AND CPref.List_Preferred			= 1
											AND CPref.Concept_Group_Key			= C.Concept_Group_Key
	
	UPDATE 	@SpecimensSearch
	SET		Det_Item_Key	= SDL.Taxon_List_Item_Key,
			Item_Name 		= dbo.ufn_GetFormattedTaxonNameByParams(
				CASE @ShowOriginalSpecimenNames
					WHEN 1 THEN Actual_Name
					ELSE Preferred_Name END,
				CASE @ShowOriginalSpecimenNames
					WHEN 1 THEN Actual_Name_Italic
					ELSE Preferred_Name_Italic END,
				Common_Name,
				Common_Name_Italic,
				NULL,
				@ShowCommonNames),
			Det_Item_Name	= ITN.Actual_Name
	FROM 	@SpecimensSearch 	SU
	JOIN 	VW_SpecimenDetsLife SDL ON 	SDL.Collection_Unit_Key					= SU.Item_Key
									AND SDL.Preferred_Taxon_Determination_Key 	= SDL.Taxon_Determination_Key
	JOIN 	Index_Taxon_Name 	ITN	ON 	ITN.Taxon_List_Item_Key					= SDL.Taxon_List_Item_Key

	-- Select table and sort appropriately
	IF @SortOrderIndex = 0
		SELECT * FROM @SpecimensSearch
		ORDER BY Item_Name, Number
	ELSE 
	IF @SortOrderIndex = 1
		SELECT * FROM @SpecimensSearch
		ORDER BY Number, Item_Name
GO

/*============================================================================*\
  Grant permissions.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_Specimens_Select_ForStore') IS NOT NULL
BEGIN
	PRINT 'Setting up security on procedure usp_Specimens_Select_ForStore'
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForStore TO R2k_AddOnly
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForStore TO R2k_Administrator
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForStore TO R2k_FullEdit
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForStore TO R2k_ReadOnly
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForStore TO R2k_RecordCardsOnly
END
GO
SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

/*============================================================================*\
  Drop stored proc before re-creating.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_Specimens_Select_ForTopLevel') IS NOT NULL
	DROP PROCEDURE dbo.usp_Specimens_Select_ForTopLevel
GO

/*============================================================================*\
  Description:	Returns top level specimens' data to the Collections Browser.

  Parameters:	@UserDomainMask			User's Domain Mask restricting which
										records may be returned
				@SessionID 				User's SessionID
				@ShowCommonNames		Specifies whether Common Names are shown
				@ShowOriginalSpecimenNames
										0 => preferred names are shown
										1 => names originally entered are shown
				@SortOrderIndex			0 => results sorted by Item_Name, Number
										1 => results sorted by Number, Item_Name
				@Key 					[Optional] If specified, identifies
										a specimen which will be the only record
										returned.
  Created:
	August 2003

  Last revision information:
	$Revision: 5 $
	$Date: 17/08/11 15:40 $
	$Author: Jamesbichard $
\*============================================================================*/
CREATE PROCEDURE dbo.usp_Specimens_Select_ForTopLevel
	@UserDomainMask						BIGINT,
	@SessionID							CHAR(16),
	@ShowCommonNames					BIT,
	@ShowOriginalSpecimenNames			BIT,
	@SortOrderIndex						TINYINT,
	@Key								CHAR(16) = NULL
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

	-- Use temp table to build output, so silly data errors such as duplicate accessions
	-- don't duplicate in the list. A temporary table has been used rather than a
	-- table variable since it stores a large number of records.
	CREATE TABLE #Search
	(
		-- defined as primary key to create an index on this column
		[Item_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS PRIMARY KEY,
		[Det_Item_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[Det_Item_Name] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[Item_Name] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[Life_Sciences] [bit] NULL,
		[Number] [varchar] (30) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[Hint] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL 
	)

	IF @Key IS NULL
		IF object_id('tempdb..#TempFilter') IS NOT NULL
			-- Display data for a list of keys in the #TempFilter table
			INSERT INTO #Search (Item_Key, Life_Sciences) 
			SELECT DISTINCT SU.Collection_Unit_Key, SU.Life_Sciences
			FROM 		Specimen_Unit 	SU
			INNER JOIN 	Collection_Unit CU 	ON 	SU.Collection_Unit_Key = CU.Collection_Unit_Key 
											AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
											OR 	(CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
			INNER JOIN 	#TempFilter 		ON 	#TempFilter.ItemKey=CU.Collection_Unit_Key
		ELSE
			INSERT INTO #Search (Item_Key, Life_Sciences) 
			SELECT DISTINCT SU.Collection_Unit_Key, SU.Life_Sciences
			FROM 		Specimen_Unit 	SU
			INNER JOIN 	Collection_Unit CU 	ON 	SU.Collection_Unit_Key = CU.Collection_Unit_Key 
											AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
											OR 	(CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
	ELSE
		-- Display data for a single key
		INSERT INTO #Search (Item_Key, Life_Sciences) 
		SELECT DISTINCT SU.Collection_Unit_Key, SU.Life_Sciences
		FROM 		Specimen_Unit 	SU
		INNER JOIN 	Collection_Unit CU 	ON 	SU.Collection_Unit_Key = CU.Collection_Unit_Key 
										AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
										OR 	(CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
		WHERE SU.Collection_Unit_Key = @Key

	UPDATE		#Search
	SET			Number = CUN.Number
	FROM 		#Search 				SU
	LEFT JOIN 	Collection_Unit_Number 	CUN ON 	SU.Item_key 	= CUN.Collection_Unit_Key 
											AND CUN.Preferred	= 1

	UPDATE 	#Search
	SET		Det_Item_Key = CPref.Concept_Key,
			Item_Name = CASE @ShowOriginalSpecimenNames
							WHEN 1 THEN C.Published_Term
							ELSE CPref.Published_Term END,
			Det_Item_Name = TDet.Plaintext
	FROM #Search SU
	INNER JOIN VW_SpecimenDetsEarth SDE ON SDE.Collection_Unit_Key = SU.Item_Key
			AND SDE.Preferred_Determination_key	= SDE.Determination_Key
	INNER JOIN Concept C ON C.Concept_Key=SDE.Concept_Key
	INNER JOIN Term TDet ON TDet.Term_Key=C.Term_Key
	INNER JOIN Concept CPref ON CPref.Meaning_Key=C.Meaning_Key
			AND CPref.List_Preferred=1
			AND CPref.Concept_Group_Key=C.Concept_Group_Key
	
	UPDATE #Search
	SET		Det_Item_Key=SDL.Taxon_List_Item_Key,
			Item_Name =
						dbo.ufn_GetFormattedTaxonNameByParams(
							CASE @ShowOriginalSpecimenNames
								WHEN 1 THEN Actual_Name
								ELSE Preferred_Name END,
							CASE @ShowOriginalSpecimenNames
								WHEN 1 THEN Actual_Name_Italic
								ELSE Preferred_Name_Italic END,
							Common_Name,
							Common_Name_Italic,
							NULL,
							@ShowCommonNames),
			Det_Item_Name=ITN.Actual_Name
	FROM #Search SU
	INNER JOIN VW_SpecimenDetsLife SDL ON SDL.Collection_Unit_Key=SU.Item_Key
		AND SDL.Preferred_Taxon_Determination_key=SDL.Taxon_Determination_Key
	INNER JOIN Index_Taxon_Name ITN	ON ITN.Taxon_List_Item_Key=SDL.Taxon_List_Item_Key

	-- Select table and sort appropriately
	IF @SortOrderIndex = 0
		SELECT * FROM #Search
		ORDER BY Det_Item_Name, Number
	ELSE IF @SortOrderIndex = 1
		SELECT * FROM #Search
		ORDER BY Number, Item_Name

	DROP TABLE #Search
GO

/*============================================================================*\
  Grant permissions.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_Specimens_Select_ForTopLevel') IS NOT NULL
BEGIN
	PRINT 'Setting up security on procedure usp_Specimens_Select_ForTopLevel'
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForTopLevel TO R2k_AddOnly
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForTopLevel TO R2k_Administrator
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForTopLevel TO R2k_FullEdit
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForTopLevel TO R2k_ReadOnly
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForTopLevel TO R2k_RecordCardsOnly
END
GO
SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

/*============================================================================*\
  Drop stored proc before re-creating.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_Specimens_Select_ForValuation') IS NOT NULL
	DROP PROCEDURE dbo.usp_Specimens_Select_ForValuation
GO

/*============================================================================*\
  Description:	Returns specimens' data for a specified valuation.

  Parameters:	@UserDomainMask			User's Domain Mask restricting which
										records may be returned
				@SessionID				User's SessionID
				@ParentKey				Identifies the valuation
				@UserID					Name_Key of current user
				@ShowCommonNames		Specifies whether Common Names are shown
				@ShowOriginalSpecimenNames
										0 => preferred names are shown
										1 => names originally entered are shown
				@SortOrderIndex			0 => results sorted by Item_Name, Number
										1 => results sorted by Number, Item_Name

  Created:		August 2003

  Last revision information:
    $Revision: 5 $
    $Date: 17/08/11 15:40 $
    $Author: Jamesbichard $
\*============================================================================*/
CREATE PROCEDURE dbo.usp_Specimens_Select_ForValuation
	@UserDomainMask						INT,
	@SessionID							CHAR(16),
	@ParentKey							CHAR(16),
	@UserID								CHAR(16),
	@ShowCommonNames					BIT,
	@ShowOriginalSpecimenNames			BIT,
	@SortOrderIndex						TINYINT
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

	DECLARE @SpecimensSearch TABLE
	(
		Item_Key		CHAR(16) 		COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		Join_Key		CHAR(16) 		COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		Det_Item_Key	CHAR(16) 		COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		Det_Item_Name	NVARCHAR(150) 	COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		Item_Name		NVARCHAR(150) 	COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		Life_Sciences	BIT NULL,
		Number			VARCHAR(30) 	COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		Hint			NVARCHAR(150) 	COLLATE SQL_Latin1_General_CP1_CI_AS NULL 
	)

	INSERT INTO	@SpecimensSearch (
				Item_Key,
				Join_Key,
				Life_Sciences)
	SELECT		s.Collection_Unit_Key,
				v.Collection_Unit_Valuation_Key,
				s.Life_Sciences
	FROM 		Collection_Unit_Valuation			AS	v
	INNER JOIN	"User"								AS	u
	ON			u.Name_Key							=	@UserID 
	INNER JOIN	Specimen_Unit						AS	s
	ON			s.Collection_Unit_Key				=	v.Collection_Unit_Key
	INNER JOIN	Collection_Unit						AS	cu
	ON			cu.Collection_Unit_Key				=	s.Collection_Unit_Key
	WHERE		v.Valuation_Key						=	@ParentKey
	AND			u.Allow_Finance						=	1
	AND			(cu.Domain_Mask & @UserDomainMask	>	0
	OR			cu.Entered_Session_ID				=	@SessionID
	OR			cu.Changed_Session_ID				=	@SessionID
	OR			cu.Domain_Mask						=	0)
	
	UPDATE		@SpecimensSearch
	SET			Number = CUN.Number
	FROM 		@SpecimensSearch 		SU
	LEFT JOIN 	Collection_Unit_Number 	CUN ON 	SU.Item_key 	= CUN.Collection_Unit_Key 
											AND CUN.Preferred	= 1

	UPDATE 	@SpecimensSearch
	SET		Det_Item_Key	= CPref.Concept_Key,
			Item_Name 		= CASE @ShowOriginalSpecimenNames
								WHEN 1 THEN C.Published_Term
								ELSE CPref.Published_Term END,
			Det_Item_Name	= TDet.Plaintext
	FROM 	@SpecimensSearch 		SU
	JOIN 	VW_SpecimenDetsEarth 	SDE 	ON 	SDE.Collection_Unit_Key			= SU.Item_Key
											AND SDE.Preferred_Determination_Key	= SDE.Determination_Key
	JOIN 	Concept 				C 		ON 	C.Concept_Key					= SDE.Concept_Key
	JOIN 	Term 					TDet 	ON 	TDet.Term_Key					= C.Term_Key
	JOIN 	Concept 				CPref 	ON 	CPref.Meaning_Key				= C.Meaning_Key
											AND CPref.List_Preferred			= 1
											AND CPref.Concept_Group_Key			= C.Concept_Group_Key
	
	UPDATE 	@SpecimensSearch
	SET		Det_Item_Key	= SDL.Taxon_List_Item_Key,
			Item_Name 		= dbo.ufn_GetFormattedTaxonNameByParams(
				CASE @ShowOriginalSpecimenNames
					WHEN 1 THEN Actual_Name
					ELSE Preferred_Name END,
				CASE @ShowOriginalSpecimenNames
					WHEN 1 THEN Actual_Name_Italic
					ELSE Preferred_Name_Italic END,
				Common_Name,
				Common_Name_Italic,
				NULL,
				@ShowCommonNames),
			Det_Item_Name	= ITN.Actual_Name
	FROM 	@SpecimensSearch 	SU
	JOIN 	VW_SpecimenDetsLife SDL ON 	SDL.Collection_Unit_Key					= SU.Item_Key
									AND SDL.Preferred_Taxon_Determination_Key 	= SDL.Taxon_Determination_Key
	JOIN 	Index_Taxon_Name 	ITN	ON 	ITN.Taxon_List_Item_Key					= SDL.Taxon_List_Item_Key

	-- Select table and sort appropriately
	IF @SortOrderIndex = 0
		SELECT * FROM @SpecimensSearch
		ORDER BY Item_Name, Number
	ELSE 
	IF @SortOrderIndex = 1
		SELECT * FROM @SpecimensSearch
		ORDER BY Number, Item_Name
GO

/*============================================================================*\
  Grant permissions.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_Specimens_Select_ForValuation') IS NOT NULL
BEGIN
   	PRINT 'Setting up security on procedure usp_Specimens_Select_ForValuation'
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_AddOnly')
       	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForValuation TO R2k_AddOnly
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForValuation TO R2k_Administrator
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForValuation TO R2k_FullEdit
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForValuation TO R2k_ReadOnly
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForValuation TO R2k_RecordCardsOnly
END
GO
If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Specimen_GetFinderData]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Specimen_GetFinderData]
GO
    
/*===========================================================================*\
  Description:	Gets all the data needed to add dropped specimens to one of the
				specimen finder grids
  Parameters:	 

  Created:	October 2007

  Last revision information:
    $Revision: 5 $
    $Date: 17/08/11 15:40 $
    $Author: Jamesbichard $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_Specimen_GetFinderData]
  @CollectionUnitKey  Char(16),
  @UserDomainMask Int,
  @ShowCommonNames Bit,
  @SessionID      Char(16)
 AS

-- Because of multiple preferred list synoynms - i.e. Amber and Ambre - different language but both preferred. Just select the first.
SELECT		TOP 1
			SU.Collection_Unit_Key					AS		Item_Key, 
			CASE 
				WHEN C.Concept_Key IS NOT NULL
				THEN C.Concept_Key 
				WHEN ITN.Taxon_List_Item_Key IS NOT NULL
				THEN ITN.Taxon_List_Item_Key 
			END										AS		Det_Item_Key, 
			CASE 
				WHEN SU.Life_Sciences = 0
				THEN CP.Published_Term 
				ELSE dbo.ufn_GetFormattedTaxonNameByParams(
						ITN.Actual_Name,
						ITN.Actual_Name_Italic,
						ITN.Common_Name, 
						ITN.Common_Name_Italic,
						ITN.Authority,
						@ShowCommonNames)
			END										AS		Item_Name, 
			SU.Life_Sciences, 
			dbo.ufn_GetPrefNumber(
					SU.Collection_Unit_Key)			AS		Number, 
			CU.Current_Location_Code, 
			SU.Specimen_Type_Concept_Key, 
			STCP.Published_Term						AS		Specimen_Type,
			C.List_Code, 
			DM.Item_Name							AS		Domain_Name 
FROM		SPECIMEN_UNIT							AS		SU 
INNER JOIN	COLLECTION_UNIT							AS		CU
ON			SU.Collection_Unit_Key					=		CU.Collection_Unit_Key 
AND			((CU.Domain_Mask & @UserDomainMask > 0)
OR			(CU.Domain_Mask = 0)
OR			(CU.Entered_Session_ID = @SessionID)
OR			(CU.Changed_Session_ID = @SessionID)) 
LEFT JOIN	COLLECTION_UNIT_NUMBER					AS		CUN
ON			SU.Collection_Unit_key					=		CUN.Collection_Unit_Key 
AND			CUN.Preferred							=		1 
LEFT JOIN	DETERMINATION							AS		D
ON			SU.Preferred_Determination_Key			=		D.Determination_Key 
LEFT JOIN	Concept									AS		C
ON			D.Concept_Key							=		C.Concept_Key 
LEFT JOIN	Concept									AS		CP
ON			CP.Meaning_Key							=		C.Meaning_Key
AND			CP.Concept_Group_Key					=		C.Concept_Group_Key
AND			CP.List_Preferred						=		1 
LEFT JOIN	TAXON_DETERMINATION						AS		TD
ON			SU.Preferred_Taxon_Determination_Key	=		TD.Taxon_Determination_Key 
LEFT JOIN	INDEX_TAXON_NAME						AS		ITN
ON			TD.Taxon_List_Item_Key					=		ITN.Taxon_List_Item_Key 
LEFT JOIN	Concept_Group							AS		CG
ON			C.Concept_Group_Key						=		CG.Concept_Group_Key 
LEFT JOIN	Local_Domain							AS		LD
ON			CG.Local_Domain_Key						=		LD.Local_Domain_Key 
LEFT JOIN	Domain									AS		DM
ON			LD.Domain_Key							=		DM.Domain_Key
INNER JOIN	Concept									AS		STC
ON			SU.Specimen_Type_Concept_Key			=		STC.Concept_Key 
LEFT JOIN	Concept									AS		STCP
ON			STCP.Meaning_Key						=		STC.Meaning_Key 
AND			STCP.Concept_Group_Key					=		STC.Concept_Group_Key
AND			STCP.List_Preferred						=		1 
WHERE		SU.Collection_Unit_Key					=		@CollectionUnitKey
ORDER BY	3 
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Specimen_GetFinderData') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Specimen_GetFinderData'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Specimen_GetFinderData TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimen_GetFinderData TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimen_GetFinderData TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimen_GetFinderData TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Specimen_GetFinderData TO [Dev - JNCC SQL]
END

GO
/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Specimen_Select') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_Specimen_Select]
GO

/*===========================================================================*\
  Description:	Returns data for the Specimen General frame.

  Parameters:	@Key		Specimen Collection Unit key
		@IsLifeScience	Whether we should be using the Taxon_Determination
				or Taxon tables.

  Created:	Setember 2003

  Last revision information:
    $Revision: 5 $
    $Date: 17/08/11 15:40 $
    $Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Specimen_Select]
	@Key char(16),
	@IsLifeScience bit
AS

SET NOCOUNT ON

	IF @IsLifeScience = 1
	BEGIN
		SELECT		CU.Collection_Unit_Key,
				ITN.Taxon_List_Item_Key AS Term_Key,
				CU.Current_Location_Code,
				CU.Usual_Location_Code,
				CU.Domain_Mask,
				SU.Dangerous,
				SU.Confidential,
				SU.Parent_Collection_Collection_Unit_Key AS Parent_Unit_Key,
				Coll.Item_Name AS ParentCollectionCollectionUnitName,
				CU.Current_Container_Collection_Unit_Key,
				CU.Usual_Container_Collection_Unit_Key,
				SCU.Item_Name + ISNULL(' - ' + CSC.Current_Location_Code, ISNULL(' - ' + CSC.Usual_Location_Code, '')) AS Current_Location_Name,
				SUS.Item_Name + ISNULL(' - ' + CSU.Current_Location_Code, ISNULL(' - ' + CSU.Usual_Location_Code, '')) AS Usual_Location_Name,
				CASE 	WHEN ITN.Preferred_Name_Italic = 1 THEN '<i>' + ITN.Preferred_Name + '</i>'
					ELSE ITN.Preferred_Name 
				END AS Item_Name,
				Specimen_Type_Concept_Key,
				CTType.PlainText AS Type,
				SU.Timestamp AS SUTimeStamp,
				CU.Timestamp AS CUTimeStamp,
				SU.Checked

		FROM		Collection_Unit AS CU
		INNER JOIN	Specimen_Unit AS SU ON SU.Collection_Unit_Key = CU.Collection_Unit_Key	
		INNER JOIN	Collection AS Coll On Coll.Collection_Unit_Key = SU.Parent_Collection_Collection_Unit_Key
		LEFT JOIN	Taxon_Determination AS TD ON TD.Taxon_Determination_Key = SU.Preferred_Taxon_Determination_Key
		LEFT JOIN	Index_Taxon_Name AS ITN ON ITN.Taxon_List_Item_Key = TD.Taxon_List_Item_Key
	
		INNER JOIN 	VW_ConceptTerm AS CTType ON Specimen_Type_Concept_Key = CTType.Concept_Key

		LEFT JOIN 	Store SCU ON SCU.Collection_Unit_Key = CU.Current_Container_Collection_Unit_Key
		LEFT JOIN	Collection_Unit CSC ON CSC.Collection_Unit_Key = SCU.Collection_Unit_Key 
		LEFT JOIN 	Store SUS ON SUS.Collection_Unit_Key = CU.Usual_Container_Collection_Unit_Key
		LEFT JOIN	Collection_Unit CSU ON CSU.Collection_Unit_Key = SUS.Collection_Unit_Key
	
		WHERE SU.Collection_Unit_Key = @Key
	END
	ELSE
	BEGIN
		SELECT		CU.Collection_Unit_Key,
				C.Term_Key,
				CU.Current_Location_Code,
				CU.Usual_Location_Code,
				CU.Domain_Mask,
				SU.Dangerous,
				SU.Confidential,
				SU.Parent_Collection_Collection_Unit_Key AS Parent_Unit_Key,
				Coll.Item_Name AS ParentCollectionCollectionUnitName,
				CU.Current_Container_Collection_Unit_Key,
				CU.Usual_Container_Collection_Unit_Key,
				SCU.Item_Name + ISNULL(' - ' + CSC.Current_Location_Code, ISNULL(' - ' + CSC.Usual_Location_Code, '')) AS Current_Location_Name,
				SUS.Item_Name + ISNULL(' - ' + CSU.Current_Location_Code, ISNULL(' - ' + CSU.Usual_Location_Code, '')) AS Usual_Location_Name,
				C.Published_Term	AS	Item_Name,
				Specimen_Type_Concept_Key,
				CTType.PlainText AS Type,
				SU.Timestamp AS SUTimeStamp,
				CU.Timestamp AS CUTimeStamp,
				SU.Checked
	
		FROM		Collection_Unit AS CU
		INNER JOIN	Specimen_Unit AS SU ON SU.Collection_Unit_Key = CU.Collection_Unit_Key
		INNER JOIN	Collection AS Coll On Coll.Collection_Unit_Key = SU.Parent_Collection_Collection_Unit_Key
		LEFT JOIN	Determination AS D ON D.Determination_Key = SU.Preferred_Determination_Key
		LEFT JOIN	Concept AS C ON C.Concept_Key = D.Concept_Key
		
		INNER JOIN 	VW_ConceptTerm AS CTType ON Specimen_Type_Concept_Key = CTType.Concept_Key
		LEFT JOIN 	Store SCU ON SCU.Collection_Unit_Key = CU.Current_Container_Collection_Unit_Key
		LEFT JOIN	Collection_Unit CSC ON CSC.Collection_Unit_Key = SCU.Collection_Unit_Key 
		LEFT JOIN 	Store SUS ON SUS.Collection_Unit_Key = CU.Usual_Container_Collection_Unit_Key
		LEFT JOIN	Collection_Unit CSU ON CSU.Collection_Unit_Key = SUS.Collection_Unit_Key
	
		WHERE SU.Collection_Unit_Key = @Key
	END

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Specimen_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Specimen_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Specimen_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimen_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimen_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Specimen_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimen_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Specimen_Select TO [Dev - JNCC SQL]
END
GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Tasks_Select_ForSearchByIncompleteAndUrgent]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Tasks_Select_ForSearchByIncompleteAndUrgent]
GO

CREATE PROCEDURE [dbo].[usp_Tasks_Select_ForSearchByIncompleteAndUrgent] 
@UserDomainMask INT,
@SessionID CHAR(16),
@SortOrderIndex TINYINT

AS

--  DESCRIPTION
--  Returns Tasks data based on the search parameter for Status
--
--  PARAMETERS
--	NAME				DESCRIPTION
--	@UserDomainMask		User's Domain Mask restricting which records may be returned
--	@SessionID 			User's SessionID
--	@SortOrderIndex		Index determining Sort Order
--
--
--  AUTHOR:				Ben Collier, Dorset Software
--  CREATED:			2003-09-15
--
SET NOCOUNT ON

IF @SortOrderIndex = 0
	SELECT CT.Conservation_Task_Key AS Item_Key, CT.Display_Caption, C.Published_Term AS Hint
	FROM 
	CONSERVATION_TASK CT
		INNER JOIN
			CONSERVATION_CHECK CC
		ON CT.Conservation_Check_Key = CC.Conservation_Check_Key 
			AND ((CC.Domain_Mask & @UserDomainMask > 0) OR (CC.Entered_Session_ID = @SessionID) 
				OR (CC.Changed_Session_ID = @SessionID) OR (CC.Domain_Mask = 0))
			AND CT.Priority = 3
			AND CT.Status IN (0, 1) -- Pending or Open
		INNER JOIN 
			CONCEPT C
		ON CT.Type_Concept_Key = C.Concept_Key
	ORDER BY CT.Set_Vague_Date_Start DESC, CT.Set_Vague_Date_End DESC, CT.Set_Vague_Date_Type, 
			dbo.ufn_GetConservationStatus(Status)

ELSE IF @SortOrderIndex = 1
	SELECT CT.Conservation_Task_Key AS Item_Key, CT.Display_Caption, C.Published_Term AS Hint
	FROM 
	CONSERVATION_TASK CT
		INNER JOIN
			CONSERVATION_CHECK CC
		ON CT.Conservation_Check_Key = CC.Conservation_Check_Key 
			AND ((CC.Domain_Mask & @UserDomainMask > 0) OR (CC.Entered_Session_ID = @SessionID) 
				OR (CC.Changed_Session_ID = @SessionID) OR (CC.Domain_Mask = 0))
			AND CT.Priority = 3
			AND CT.Status IN (0, 1) -- Pending or Open
		INNER JOIN 
			CONCEPT C
		ON CT.Type_Concept_Key = C.Concept_Key
	ORDER BY dbo.ufn_GetConservationStatus(Status), CT.Set_Vague_Date_Start DESC, CT.Set_Vague_Date_End DESC, CT.Set_Vague_Date_Type
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Tasks_Select_ForSearchByIncompleteAndUrgent') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Tasks_Select_ForSearchByIncompleteAndUrgent'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Tasks_Select_ForSearchByIncompleteAndUrgent TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Tasks_Select_ForSearchByIncompleteAndUrgent TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Tasks_Select_ForSearchByIncompleteAndUrgent TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Tasks_Select_ForSearchByIncompleteAndUrgent TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Tasks_Select_ForSearchByIncompleteAndUrgent TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Tasks_Select_ForSearchByIncompleteAndUrgent TO [Dev - JNCC SQL]
END

GO
If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Tasks_Select_ForSearchByType]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Tasks_Select_ForSearchByType]
GO

CREATE PROCEDURE [dbo].[usp_Tasks_Select_ForSearchByType] 
@UserDomainMask INT,
@SessionID CHAR(16),
@SearchText VARCHAR(150),
@SortOrderIndex TINYINT

AS

--  DESCRIPTION
--  Returns Tasks data based on the search parameter for Type
--
--  PARAMETERS
--	NAME				DESCRIPTION
--	@SearchText			Text to be used for search
--	@UserDomainMask		User's Domain Mask restricting which records may be returned
--	@SessionID 			User's SessionID
--	@SortOrderIndex		Index determining Sort Order
--
--
--  AUTHOR:				Ben Collier, Dorset Software
--  CREATED:			2003-09-15
--
SET NOCOUNT ON

IF @SortOrderIndex = 0
	SELECT CT.Conservation_Task_Key AS Item_Key, CT.Display_Caption, C.Published_Term AS Hint
	FROM 
	CONSERVATION_TASK CT
		INNER JOIN
			CONSERVATION_CHECK CC
		ON CT.Conservation_Check_Key = CC.Conservation_Check_Key 
			AND ((CC.Domain_Mask & @UserDomainMask > 0) OR (CC.Entered_Session_ID = @SessionID) 
				OR (CC.Changed_Session_ID = @SessionID) OR (CC.Domain_Mask = 0))
		INNER JOIN 
			CONCEPT C
		ON CT.Type_Concept_Key = C.Concept_Key
		INNER JOIN 
			dbo.Search_Term AS ST
		ON C.Concept_Key = ST.Concept_Key
			AND ST.Plaintext LIKE @SearchText + '%'
	ORDER BY CT.Set_Vague_Date_Start DESC, CT.Set_Vague_Date_End DESC, CT.Set_Vague_Date_Type, 
		dbo.ufn_GetConservationStatus(Status)

ELSE IF @SortOrderIndex = 1
	SELECT CT.Conservation_Task_Key AS Item_Key, CT.Display_Caption, C.Published_Term AS Hint
	FROM 
	CONSERVATION_TASK CT
		INNER JOIN
			CONSERVATION_CHECK CC
		ON CT.Conservation_Check_Key = CC.Conservation_Check_Key 
			AND ((CC.Domain_Mask & @UserDomainMask > 0) OR (CC.Entered_Session_ID = @SessionID) 
				OR (CC.Changed_Session_ID = @SessionID) OR (CC.Domain_Mask = 0))
		INNER JOIN 
			CONCEPT C
		ON CT.Type_Concept_Key = C.Concept_Key
		INNER JOIN 
			dbo.Search_Term AS ST
		ON C.Concept_Key = ST.Concept_Key
			AND ST.Plaintext LIKE @SearchText + '%'
	ORDER BY dbo.ufn_GetConservationStatus(Status),
		CT.Set_Vague_Date_Start DESC, CT.Set_Vague_Date_End DESC, CT.Set_Vague_Date_Type

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Tasks_Select_ForSearchByType') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Tasks_Select_ForSearchByType'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Tasks_Select_ForSearchByType TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Tasks_Select_ForSearchByType TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Tasks_Select_ForSearchByType TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Tasks_Select_ForSearchByType TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Tasks_Select_ForSearchByType TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Tasks_Select_ForSearchByType TO [Dev - JNCC SQL]
END

GO
/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_TaxonDesignationType_ImportConceptGroup]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_TaxonDesignationType_ImportConceptGroup]
GO

/*===========================================================================*\
  Description:	Import taxon designation types corresponding to the types
  				used in the specified taxon list.

  Parameters:	@job_id					Job identifier

  Created:		Dec 2003

  Last revision information:
	$Revision: 5 $
	$Date: 17/08/11 15:40 $
	$Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_TaxonDesignationType_ImportConceptGroup]
	@job_id					INT
AS
	SET NOCOUNT ON

	DECLARE     @concept_group_key				CHAR(16),
				@concept_key					CHAR(16),
				@taxon_designation_type_key		CHAR(16),
				@short_name						VARCHAR(40),
				@ins_user_key					CHAR(16),
				@ins_date						SMALLDATETIME,
				@upd_user_key					CHAR(16),
				@upd_date						SMALLDATETIME,
				@system							BIT

	/* determine parameters of job */
	SELECT		@concept_group_key				=	j.Concept_Group_Key
	FROM		Import_Export_Job				AS	j
	WHERE		j.Import_Export_Job_ID			=	@job_id

	IF @@ROWCOUNT = 0
	BEGIN
		RAISERROR ('Job does not exist or has not been configured', 16, 1)
		GOTO fail
	END

	EXECUTE		usp_Import_Export_Job_UpdateStatus	@job_id,
													'Importing designation types'
	IF @@ERROR <> 0 GOTO fail

	DECLARE		types	CURSOR LOCAL FAST_FORWARD FOR
	SELECT DISTINCT
				dt.Concept_Key,
				dt.Published_Term	AS	Item_Name,
				es.User_Name_Key,
				CONVERT(SMALLDATETIME,
						CONVERT(VARCHAR, es.Date_Time_Start, 112)),
				cs.User_Name_Key,
				CONVERT(SMALLDATETIME,
						CONVERT(VARCHAR, cs.Date_Time_Start, 112)),
				dt.System_Supplied_Data
	FROM		Concept							AS	c
	INNER JOIN	Concept_Designation				AS	d
	ON			d.Concept_Key					=	c.Concept_Key
	INNER JOIN	Concept							AS  dt
	ON			dt.Concept_Key					=	d.Designation_Type_Concept_Key
	INNER JOIN	Session							AS	es
	ON			es.Session_ID					=	dt.Entered_Session_ID
	LEFT JOIN	Session							AS	cs
	ON			cs.Session_ID					=	dt.Changed_Session_ID
	WHERE		c.Concept_Group_Key				=	@concept_group_key

	OPEN		types

	WHILE 1 = 1
	BEGIN
		FETCH		types
		INTO		@concept_key,
					@short_name,
					@ins_user_key,
					@ins_date,
					@upd_user_key,
					@upd_date,
					@system

		IF @@FETCH_STATUS <> 0 BREAK

		BEGIN TRANSACTION

		SELECT		@taxon_designation_type_key					=	Taxon_Designation_Type_Key
		FROM		Taxon_Dictionary_Designation_Type_Mapping
		WHERE		Concept_Designation_Type_Key				=	@concept_key

		IF @@ROWCOUNT > 0
		BEGIN
			/* update designation type */
			UPDATE		TAXON_DESIGNATION_TYPE
			SET			SHORT_NAME					=	@short_name,
						ENTERED_BY					=	@ins_user_key,
						ENTRY_DATE					=	@ins_date,
						CHANGED_BY					=	@upd_user_key,
						CHANGED_DATE				=	@upd_date,
						SYSTEM_SUPPLIED_DATA		=	@system
			WHERE		TAXON_DESIGNATION_TYPE_KEY	=	@taxon_designation_type_key

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END
		ELSE
		BEGIN
			/* create designation type */
			EXECUTE		spNextKey	'TAXON_DESIGNATION_TYPE',
									@taxon_designation_type_key		OUTPUT
			IF @@ERROR <> 0 GOTO fail_from_cursor

			INSERT		TAXON_DESIGNATION_TYPE (
						TAXON_DESIGNATION_TYPE_KEY,
						SHORT_NAME,
						ENTERED_BY,
						ENTRY_DATE,
						CHANGED_BY,
						CHANGED_DATE,
						SYSTEM_SUPPLIED_DATA)
			VALUES		(@taxon_designation_type_key,
						@short_name,
						@ins_user_key,
						@ins_date,
						@upd_user_key,
						@upd_date,
						@system)

			IF @@ERROR <> 0 GOTO fail_from_cursor

			/* record mapping */
			INSERT		Taxon_Dictionary_Designation_Type_Mapping (
						Taxon_Designation_Type_Key,
						Concept_Designation_Type_Key)
			VALUES		(@taxon_designation_type_key,
						@concept_key)

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END

		EXECUTE		usp_Import_Export_Job_RecordProcessed	@job_id
		IF @@ERROR <> 0 GOTO fail_from_cursor

		COMMIT TRANSACTION
	END

	CLOSE		types
	DEALLOCATE	types
	RETURN

fail_from_cursor:
	CLOSE		types
	DEALLOCATE	types

fail:
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_TaxonDesignationType_ImportConceptGroup failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TaxonDesignationType_ImportConceptGroup') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_TaxonDesignationType_ImportConceptGroup'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_TaxonDesignationType_ImportConceptGroup TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_TaxonDesignationType_ImportConceptGroup TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_TaxonDesignationType_ImportConceptGroup TO [Dev - JNCC SQL]
END
GO
/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_TaxonListVersion_ImportConceptGroup]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_TaxonListVersion_ImportConceptGroup]
GO

/*===========================================================================*\
  Description:	Import taxon list versions corresponding to the versions
				of a concept group.

  Parameters:	@job_id					Job identifier

  Created:		Dec 2003

  Last revision information:
	$Revision: 5 $
	$Date: 17/08/11 15:40 $
	$Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_TaxonListVersion_ImportConceptGroup]
	@job_id					INT
AS
	SET NOCOUNT ON

	DECLARE		@concept_group_key				CHAR(16),
				@taxon_list_key					CHAR(16),
				@concept_group_version_key		CHAR(16),
				@version						INT,
				@authority						VARCHAR(50),
				@vague_date_start				INT,
				@vague_date_end					INT,
				@vague_date_type				VARCHAR(2),
				@entered_by						CHAR(16),
				@entry_date						SMALLDATETIME,
				@changed_by						CHAR(16),
				@changed_date					SMALLDATETIME,
				@system							BIT,
				@taxon_list_version_key			CHAR(16),
				@source_key						CHAR(16),
				@source_join_key				CHAR(16)

	/* determine parameters of job */
	SELECT      @concept_group_key						=	m.Concept_Group_Key,
				@taxon_list_key							=	m.Taxon_List_Key
	FROM		Import_Export_Job						AS	j
	INNER JOIN	Taxon_Dictionary_Concept_Group_Mapping	AS	m
	ON			m.Concept_Group_Key						=	j.Concept_Group_Key
	WHERE		j.Import_Export_Job_ID					=	@job_id

	IF @@ROWCOUNT = 0
	BEGIN
		RAISERROR ('Job does not exist or has not been configured', 16, 1)
		GOTO fail
	END

	EXECUTE		usp_Import_Export_Job_UpdateStatus	@job_id,
													'Exporting concept group versions'
	IF @@ERROR <> 0 GOTO fail

	DECLARE		versions	CURSOR LOCAL FAST_FORWARD FOR
	SELECT		cgv.Concept_Group_Version_Key,
				cgv.Sequence,
				CAST(cg.Authority AS VARCHAR(50)),
				cgv.From_Vague_Date_Start,
				cgv.From_Vague_Date_End,
				cgv.From_Vague_Date_Type,
				es.User_Name_Key,
				CONVERT(SMALLDATETIME,
						CONVERT(VARCHAR, es.Date_Time_Start, 112)),
				cs.User_Name_Key,
				CONVERT(SMALLDATETIME,
						CONVERT(VARCHAR, cs.Date_Time_End, 112)),
				cgv.System_Supplied_Data
	FROM		Concept_Group_Version		AS	cgv
	INNER JOIN	Concept_Group				AS	cg
	ON			cgv.Concept_Group_Key		=	cg.Concept_Group_Key
	INNER JOIN	Session						AS	es
	ON			es.Session_ID				=	cgv.Entered_Session_ID
	LEFT JOIN	Session						AS	cs
	ON			cs.Session_ID				=	cgv.Changed_Session_ID
	WHERE		cgv.Concept_Group_Key		=	@concept_group_key

	OPEN		versions

	WHILE 1 = 1
	BEGIN
		FETCH		versions
		INTO		@concept_group_version_key,
					@version,
					@authority,
					@vague_date_start,
					@vague_date_end,
					@vague_date_type,
					@entered_by,
					@entry_date,
					@changed_by,
					@changed_date,
					@system

		IF @@FETCH_STATUS <> 0 BREAK

		BEGIN TRANSACTION

		SELECT      @taxon_list_version_key							=	NULL,
					@source_join_key					   			=	NULL,
					@source_key										=	NULL

		SELECT		@taxon_list_version_key							=	m.Taxon_List_Version_Key,
					@source_key										=	j.Source_Key
		FROM		Taxon_Dictionary_Concept_Group_Version_Mapping	AS	m
		LEFT JOIN	Source_Join										AS	j
		ON			j.Source_Join_Key								=	m.Source_Join_Key
		WHERE		m.Concept_Group_Version_Key						=	@concept_group_version_key

		IF @source_key IS NULL
		BEGIN
			/* there is no existing mapping for the source join; pick an
			 * arbitrary join record (if there are any) and make this the
			 * mapped join.
			 */
			SELECT		@source_join_key	=	Source_Join_Key,
						@source_key			=	Source_Key
			FROM		Source_Join
			WHERE		Record_Key			=	@concept_group_version_key
			AND			Table_Name			=	'Concept_Group_Version'
			ORDER BY	Source_Join_Key
		END

		IF @taxon_list_version_key IS NOT NULL
		BEGIN
			/* update taxon list version */
			UPDATE		TAXON_LIST_VERSION
			SET			VERSION					=	@version,
						AUTHORITY				=	@authority,
						VAGUE_DATE_START		=	@vague_date_start,
						VAGUE_DATE_END			=	@vague_date_end,
						VAGUE_DATE_TYPE			=	@vague_date_type,
						SOURCE_KEY				=	@source_key,
						ENTERED_BY				=	@entered_by,
						ENTRY_DATE				=	@entry_date,
						CHANGED_BY				=	@changed_by,
						CHANGED_DATE			=	@changed_date,
						SYSTEM_SUPPLIED_DATA	=	@system
			WHERE		TAXON_LIST_VERSION_KEY	=	@taxon_list_version_key

			IF @@ERROR <> 0 GOTO fail_from_cursor

			IF @source_join_key IS NOT NULL
			BEGIN
				UPDATE		Taxon_Dictionary_Concept_Group_Version_Mapping
				SET			Source_Join_Key									=	@source_join_key
				WHERE		Taxon_List_Version_Key							=	@taxon_list_version_key

				IF @@ERROR <> 0 GOTO fail_from_cursor
			END
		END
		ELSE
		BEGIN
			/* create taxon list version */
			EXECUTE		spNextKey	'TAXON_LIST_VERSION',
									@taxon_list_version_key		OUTPUT
			IF @@ERROR <> 0 GOTO fail_from_cursor

			INSERT		TAXON_LIST_VERSION (
						TAXON_LIST_VERSION_KEY,
						TAXON_LIST_KEY,
						VERSION,
						AUTHORITY,
						VAGUE_DATE_START,
						VAGUE_DATE_END,
						VAGUE_DATE_TYPE,
						SOURCE_KEY,
						ENTERED_BY,
						ENTRY_DATE,
						CHANGED_BY,
						CHANGED_DATE,
						SYSTEM_SUPPLIED_DATA)
			VALUES		(@taxon_list_version_key,
						@taxon_list_key,
						@version,
						@authority,
						@vague_date_start,
						@vague_date_end,
						@vague_date_type,
						@source_key,
						@entered_by,
						@entry_date,
						@changed_by,
						@changed_date,
						@system)

			IF @@ERROR <> 0 GOTO fail_from_cursor

			/* record mapping */
			INSERT		Taxon_Dictionary_Concept_Group_Version_Mapping (
						Taxon_List_Version_Key,
						Concept_Group_Version_Key,
						Source_Join_Key)
			VALUES		(@taxon_list_version_key,
						@concept_group_version_key,
						@source_join_key)

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END

		EXECUTE		usp_Import_Export_Job_RecordProcessed	@job_id
		IF @@ERROR <> 0 GOTO fail_from_cursor
		
		COMMIT TRANSACTION
	END

	CLOSE		versions
	DEALLOCATE	versions
	RETURN

fail_from_cursor:
	CLOSE		versions
	DEALLOCATE	versions

fail:
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_TaxonListVersion_ImportConceptGroup failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TaxonListVersion_ImportConceptGroup') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_TaxonListVersion_ImportConceptGroup'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_TaxonListVersion_ImportConceptGroup TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_TaxonListVersion_ImportConceptGroup TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_TaxonListVersion_ImportConceptGroup TO [Dev - JNCC SQL]
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
	$Revision: 5 $
	$Date: 17/08/11 15:40 $
	$Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_TaxonList_ImportConceptGroup]
	@job_id				INT,
	@Taxon_List_Key		CHAR(16),
	@concept_group_Key	CHAR(16),
	@SessionID	CHAR(16)
AS

SET NOCOUNT ON

	DECLARE		@existing_list_key					CHAR(16),
				@taxon_version_Wellformed			Char(1),
				@taxon_version_Incorrectformed		Char(1),
				@taxon_version_Unverified			Char(1),
				@taxon_version_status_recommended	Char(1),
				@taxon_version_status_synonym		Char(1),
				@taxon_type_scientific 				Char(1),
				@taxon_type_vernacular  			Char(1),
				@taxon_name_type_key_formal			CHAR(16),
				@record_count						INT,
				@taxon_list_type_key				CHAR(16),
				@taxon_list_name					VARCHAR(100),
				@authority							VARCHAR(50),
				@user_name_key						CHAR(16),
				@taxon_list_version_key				CHAR(16)
		
	/* determine default parameters */
	SET			@taxon_version_wellformed			=	'W'	
	SET			@taxon_version_incorrectformed		=	'I'
	SET			@taxon_version_unverified			=	'U'			
    SET			@taxon_version_status_recommended	=	'R'		
	SET			@taxon_version_status_synonym		=	'S'
	SET			@taxon_type_scientific				=	'S'
	SET			@taxon_type_vernacular				=	'V'
	SET			@taxon_name_type_key_formal			=	'MNHSYS0000000001'

	SELECT @taxon_list_name = Item_Name, @authority = authority
	FROM Concept_Group
	WHERE Concept_Group_Key = @Concept_Group_Key

	--If @Taxon_List_Key is null, create a new taxon list with the same name
	--as the exported concept group
	IF @Taxon_List_Key IS NULL
	BEGIN
		EXEC dbo.spNextKey 'TAXON_LIST', @Taxon_List_Key OUTPUT
		EXEC dbo.spNextKey 'TAXON_LIST_VERSION', @taxon_list_version_key OUTPUT

		SELECT @taxon_list_type_key = Taxon_List_Type_Key
		FROM Taxon_List_Type
		WHERE Short_Name = 'Checklist'

		SELECT @user_name_key = User_Name_Key
		FROM Session
		WHERE Session_ID = @SessionID		

		INSERT INTO Taxon_List (
			taxon_list_key,
			item_name,
			taxon_list_type_key,
			entered_by,
			authority)
		VALUES (
			@taxon_list_key,
			@taxon_list_name,
			@taxon_list_type_key,
			@user_name_key,
			@authority)
	
		INSERT INTO Taxon_List_Version (
			taxon_list_version_key,
			taxon_list_key,
			entered_by)
		VALUES (
			@taxon_list_version_key,
			@taxon_list_key,
			@user_name_key)

		IF @@ERROR <> 0 RETURN
	END
	ELSE
	BEGIN
		UPDATE Taxon_List
		SET Authority = @Authority
		WHERE Taxon_List_Key = @taxon_list_key
	END

	SET ROWCOUNT 0

	SELECT		@existing_list_key		=	Taxon_List_Key
	FROM		Taxon_Dictionary_Concept_Group_Mapping
	WHERE		Concept_Group_Key		=	@concept_group_Key

	IF @@ROWCOUNT = 0
	BEGIN
		/* record mapping */
		INSERT		Taxon_Dictionary_Concept_Group_Mapping (
					Taxon_List_Key,
					Concept_Group_Key)
		VALUES		(@Taxon_List_Key,
					@concept_group_Key)

		IF @@ERROR <> 0 RETURN
	END
	ELSE IF @existing_List_Key <> @Taxon_List_Key
	BEGIN
		RAISERROR (
			'Concept group has previously been imported into a different Taxon List',
			16,
			1)
		RETURN
	END

	

	/* Calculate size of job */
	SELECT		@record_count			=	COUNT(*)
	FROM		Concept_Group_Version
	WHERE		Concept_Group_Key		=	@concept_group_Key

	/* COUNT(column) generates a warning if NULLs are encountered. And this ends up coming out as
	   an error after the import. Not good. So turn it off to avoid that situation. */
	SET ANSI_WARNINGS OFF  ---VI 17676

	SET	@record_count = @record_count * 3 + (
		SELECT		COUNT(DISTINCT c.Name_Type_Concept_Key)
					+ COUNT(DISTINCT c.Term_Key)
					+ COUNT(DISTINCT c.Term_Version_Key)
					+ COUNT(DISTINCT j.Source_Join_Key)
					+ COUNT(DISTINCT c.Concept_Rank_Key)
					+ COUNT(DISTINCT c.Concept_Key)
					+ COUNT(DISTINCT d.Designation_Type_Concept_Key)
					+ COUNT(DISTINCT d.Concept_Designation_Key)
					+ COUNT(DISTINCT f.Thesaurus_Fact_Key + vm.Term_Version_Key)
		FROM		Concept									AS	c
		LEFT JOIN	Source_Join								AS	j	ON	j.Record_Key		=	c.Term_Key
																	AND	j.Table_Name		=	'Term'
		LEFT JOIN	Concept_Designation						AS	d   ON	d.Concept_Key		=	c.Concept_Key
		LEFT JOIN	Thesaurus_Fact							AS	f	ON	f.Meaning_Key		=	c.Meaning_Key
		LEFT JOIN	Taxon_Dictionary_Term_Version_Mapping	AS	vm	ON	vm.Term_Version_Key	=	c.Term_Version_Key
		WHERE		c.Concept_Group_Key						=	@concept_group_Key  )

	SET ANSI_WARNINGS ON

	EXECUTE		usp_Import_Export_Job_Configure		@job_id,
													@concept_group_Key,
													@record_count
	IF @@ERROR <> 0 RETURN

	/* import Versions */
	EXECUTE		usp_TaxonListVersion_ImportConceptGroup		@job_id
	IF @@ERROR <> 0 RETURN

	/* import name types */
	EXECUTE		usp_TaxonNameType_ImportConceptGroup	@job_id
	IF @@ERROR <> 0 RETURN

	/* import taxa */
	EXECUTE		usp_Taxon_ImportConceptGroup	@job_id
	IF @@ERROR <> 0 RETURN

	/* import Taxon Versions */
	EXECUTE		usp_TaxonVersion_ImportConceptGroup		@job_id, @SessionID
	IF @@ERROR <> 0 RETURN

	/* import Taxon/source relationships */
	EXECUTE		usp_TaxonSources_ImportConceptGroup		@job_id
	IF @@ERROR <> 0 RETURN

	/* import Taxon ranks */
	EXECUTE		usp_TaxonRank_ImportConceptGroup	@job_id
	IF @@ERROR <> 0 RETURN

	/* import Taxon List Items */
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
			AND C1.Concept_Group_Key=@concept_group_Key

	/* Discard NameServer records for the concept group */
	DELETE		NS
	FROM		NameServer NS
	INNER JOIN	Taxon_Dictionary_Term_Version_Mapping TDM
										ON TDM.Taxon_Version_Key		=	NS.INPUT_Taxon_Version_Key
	INNER JOIN	Term_Version		TV	ON TV.Term_Version_Key			=	TDM.Term_Version_Key
	INNER JOIN	Concept				C1	ON C1.Term_Key					=	TV.Term_Key
										AND	C1.Concept_Group_Key		=	@concept_group_Key

	/* Rebuild Index_Taxon_Name for the concept group */
	INSERT INTO Index_Taxon_Name (Taxon_List_Item_Key, Taxon_List_Version_Key,
	 Actual_Name, Actual_Name_Italic, Common_Name, Common_Name_Italic, 
	  Preferred_Name, Preferred_Name_Italic, Abbreviation, Authority, System_Supplied_Data,
	  Recommended_Taxon_List_Item_Key)
	SELECT TLI.Taxon_List_Item_Key, TLI.Taxon_List_Version_Key, 
	  T.Item_Name, CASE WHEN TR3.List_Font_Italic = 1 AND T.Language = 'La' THEN 1 ELSE 0 END, 
	  T2.Item_Name, CASE WHEN TR3.List_Font_Italic = 1 AND T2.Language = 'La' THEN 1 ELSE 0 END, 
	  T3.Item_Name, CASE WHEN TR3.List_Font_Italic = 1 AND T3.Language = 'La' THEN 1 ELSE 0 END, 
	  T.Abbreviation, T.Authority, 1, TDM2.Taxon_List_Item_Key 
	FROM ((((((((Taxon_List_Item AS TLI 
	INNER JOIN Taxon_Dictionary_Concept_Mapping TDM ON TDM.Taxon_List_Item_Key=TLI.Taxon_List_Item_Key
	INNER JOIN Concept C1 ON C1.Concept_Key=TDM.Concept_Key
			AND C1.Concept_Group_Key=@concept_group_Key
	LEFT JOIN Taxon_Dictionary_Concept_Mapping TDM2 ON TDM2.Concept_Key = C1.Meaning_Key
	LEFT JOIN Taxon_Version AS TV ON TV.Taxon_Version_Key = TLI.Taxon_Version_Key) 
	LEFT JOIN Taxon AS T ON T.Taxon_Key = TV.Taxon_Key) 
	LEFT JOIN Taxon_Common_Name AS TCN ON TCN.Taxon_List_Item_Key = TLI.Taxon_List_Item_Key) 
	LEFT JOIN Taxon_Version AS TV2 ON TV2.Taxon_Version_Key = TCN.Taxon_Version_Key) 
	LEFT JOIN Taxon AS T2 ON T2.Taxon_Key = TV2.Taxon_Key) 
	LEFT JOIN Taxon_List_Item AS TLI3 ON TLI3.Taxon_List_Item_Key = TLI.Preferred_Name) 
	LEFT JOIN Taxon_Rank AS TR3 ON TR3.Taxon_Rank_Key = TLI3.Taxon_Rank_Key) 
	LEFT JOIN Taxon_Version AS TV3 ON TV3.Taxon_Version_Key = TLI3.Taxon_Version_Key) 
	LEFT JOIN Taxon AS T3 ON T3.Taxon_Key = TV3.Taxon_Key 
	WHERE TLI.Taxon_List_Version_To IS NULL
	
	CREATE TABLE #NameServer (
			Taxon_List_Item_Key				CHAR(16)	COLLATE database_default,
			Input_Taxon_Version_Key			CHAR(16)	COLLATE database_default,
			Taxon_Version_FORM				CHAR		COLLATE database_default,
			Taxon_Version_STATUS			CHAR		COLLATE database_default,
			Taxon_Type						CHAR		COLLATE database_default,
			Recommended_Taxon_Version_Key	CHAR(16)	COLLATE database_default,
			Recommended_Taxon_List_Item_Key	CHAR(16)	COLLATE database_default,
			Entered_By						CHAR(16)	COLLATE database_default,
			Entry_Date						DATETIME,
			Changed_By						CHAR(16)	COLLATE database_default,
			Changed_Date					DATETIME	)

	--Get the taxon list item preferred synonyms for the taxon list items related 
	--to the export concept group. Use preferred lists to determine which synonym
	--is chosen.
	declare @bound int
	select @bound = Count(*) from taxon_list

	--Create a temporary table containing each taxon list item for the export group,
	--and for each item the list-preferred synonyms for each preferred list (as
	--recorded in Concept_Group_Preferred_Taxon_List). If there is no synonym in a
	--list, the original item is recorded, and the priority is set so that this will
	--be the lowest priority item.
	select distinct 
		tli.taxon_list_item_key,
		case
			when (syn.concept_key is not null and cmsyn.concept_key is not null)
				then cmsyn.taxon_list_item_key
			else tli.taxon_list_item_key
		end as synonym_taxon_list_item_key,
		case
			when (syn.concept_key is not null and cmsyn.concept_key is not null)
				then ptl.priority
			else @bound
		end as "priority"
	into #PreferredListSynonyms
	from taxon_list_item tli
	inner join taxon_dictionary_concept_mapping cm
		on cm.taxon_list_item_key = tli.taxon_list_item_key
	inner join concept c
		on c.concept_key = cm.concept_key
	left join (
		concept_group_preferred_taxon_list ptl
			inner join taxon_dictionary_concept_group_mapping cgm
				on cgm.taxon_list_key = ptl.taxon_list_key
			)
		on ptl.concept_group_key = c.concept_group_key 
	left join concept syn on syn.concept_group_key = cgm.concept_group_key
	and	syn.meaning_key = c.meaning_key
			and syn.list_preferred = 1
	left join taxon_dictionary_concept_mapping cmsyn
		on syn.concept_key = cmsyn.concept_key

	where c.concept_group_key = @concept_group_key

	--Create a new temporary table to store the overall preferred synonym for each
	--list item. This is the synonym in #PreferredListSynonyms with the lowest value
	--for priority. If there are more than one such synonyms, the maximum value of
	--the synonym key is arbitrarily chosen.
	select 
		ps.taxon_list_item_key,
		max(ps.synonym_taxon_list_item_key) as synonym_taxon_list_item_key,
		tli.taxon_version_key as synonym_taxon_version_key
	into #PreferredListPreferredSynonyms
	from #PreferredListSynonyms ps
	inner join (
			select taxon_list_item_key, min(priority) as priority
			from #PreferredListSynonyms
			group by taxon_list_item_key) as ps1
		on ps1.taxon_list_item_key = ps.taxon_list_item_key
		and ps1.priority = ps.priority
	inner join taxon_list_item tli 
		on tli.taxon_list_item_key = ps.synonym_taxon_list_item_key
	group by 
		ps.taxon_list_item_key, 
		ps.priority, 
		tli.taxon_version_key

	--	Populates the temporary table of potential new Nameserver objects.
	INSERT  INTO #NameServer (
			Taxon_List_Item_Key,	
			Input_Taxon_Version_Key,
			Taxon_Version_FORM,
			Taxon_Version_STATUS,
			Taxon_Type,
			Recommended_Taxon_Version_Key,
			Recommended_Taxon_List_Item_Key,
			Entered_By,
			Entry_Date,
			Changed_By,
			Changed_Date )
	SELECT DISTINCT TLI.Taxon_List_Item_Key,
			TLI.Taxon_Version_Key,
			CASE WHEN TV1.Validation_Level = 0 THEN @taxon_version_wellformed
				 WHEN TV1.Validation_Level = 3 THEN @taxon_version_incorrectformed 
				 ELSE @taxon_version_unverified 
			END,
			CASE WHEN TV1.Taxon_Version_Key is NULL THEN @taxon_version_unverified 
				 WHEN TV1.Taxon_Version_Key  = TLI.Taxon_Version_Key THEN @taxon_version_status_recommended 
				 ELSE @taxon_version_status_synonym  
			END,
			CASE WHEN TNT.TAXON_NAME_TYPE_KEY = @taxon_name_type_key_formal THEN @taxon_type_scientific 
				 ELSE @taxon_type_vernacular 
			END,  
			PS.Synonym_Taxon_Version_Key,
			PS.Synonym_Taxon_List_Item_Key,
			TLI.ENTERED_BY,
			TLI.ENTRY_DATE,
			TLI.CHANGED_BY,
			TLI.CHANGED_DATE		
	FROM		Taxon_List_Item		TLI 
	INNER JOIN	#PreferredListPreferredSynonyms PS
											ON	PS.Taxon_List_Item_Key		=	TLI.Taxon_List_Item_Key
	LEFT JOIN	Taxon_List_Item		TLI1	ON	TLI1.Taxon_List_Item_Key	=	TLI.Preferred_Name
	LEFT JOIN	Taxon_Version		TV1		ON	TV1.Taxon_Version_Key		=	TLI1.Taxon_Version_Key 
	INNER JOIN	TAXON				TX		ON	TX.TAXON_KEY				=	TV1.TAXON_KEY
	INNER JOIN	TAXON_NAME_TYPE		TNT		ON	TNT.TAXON_NAME_TYPE_KEY		=	TX.TAXON_NAME_TYPE_KEY
	INNER JOIN	Taxon_List_Version	TLV		ON	TLV.Taxon_List_Version_Key	=	TLI.Taxon_List_Version_Key
											AND TLV.Taxon_List_Key			=	@Taxon_List_Key
	LEFT JOIN	NameServer			NS		ON	NS.Input_Taxon_Version_Key	=	TLI.Taxon_Version_Key
	WHERE		TLI.Taxon_List_Version_To	IS NULL
			AND	NS.Input_Taxon_Version_Key	IS NULL

	drop table #PreferredListSynonyms
	drop table #PreferredListPreferredSynonyms

	-- Removes any NameServer records relating to Taxon_List_Items with the same Input_Taxon_Version_Key
	-- as another Taxon_List_Item from a more recent version.
	DELETE		NS
	FROM		#NameServer			NS
	INNER JOIN	Taxon_List_Item		TLI
			ON	NS.Recommended_Taxon_List_Item_Key	=	TLI.Taxon_List_Item_Key
	INNER JOIN	Taxon_List_Version	TLV
			ON	TLI.Taxon_List_Version_Key			=	TLV.Taxon_List_Version_Key
	INNER JOIN	#NameServer			NS2
			ON	NS.Input_Taxon_Version_Key			=	NS2.Input_Taxon_Version_Key
			AND	NS.Recommended_Taxon_List_Item_Key	<>	NS2.Recommended_Taxon_List_Item_Key
	INNER JOIN	Taxon_List_Item		TLI2
			ON	NS2.Recommended_Taxon_List_Item_Key	=	TLI2.Taxon_List_Item_Key
	INNER JOIN	Taxon_List_Version	TLV2
			ON	TLI2.Taxon_List_Version_Key			=	TLV2.Taxon_List_Version_Key
			AND	TLV2.Version						>	TLV.Version

	-- Actually inserts the records into the nameserver. The join ensures that if there are still
	-- duplicate Input_Taxon_Version_Keys, a single one will be chosen so as not to conflict with
	-- the unique constant on the primary key.
	INSERT INTO	NameServer	(
				Input_Taxon_Version_Key,
				Taxon_Version_FORM,
				Taxon_Version_STATUS,
				Taxon_Type,
				Recommended_Taxon_Version_Key,
				Recommended_Taxon_List_Item_Key,
				Entered_By,
				Entry_Date,
				Changed_By,
				Changed_Date	)
	SELECT		NS.Input_Taxon_Version_Key,
				NS.Taxon_Version_FORM,
				NS.Taxon_Version_STATUS,
				NS.Taxon_Type,
				NS.Recommended_Taxon_Version_Key,
				NS.Recommended_Taxon_List_Item_Key,
				NS.Entered_By,
				NS.Entry_Date,
				NS.Changed_By,
				NS.Changed_Date	
	FROM		#NameServer									NS
	INNER JOIN	(	SELECT		Input_Taxon_Version_Key,
								MAX(Taxon_List_Item_Key) AS Taxon_List_Item_Key 
					FROM		#NameServer 
					GROUP BY	Input_Taxon_Version_Key	)	NS2
		ON		NS2.Taxon_List_Item_Key		=	NS.Taxon_List_Item_Key
		AND		NS2.Input_Taxon_Version_Key	=	NS.Input_Taxon_Version_Key
	
	DROP TABLE	#NameServer

	/* Update to include the Has_Children field */
	UPDATE ITN
	SET Has_Children=1
	FROM Index_Taxon_Name ITN
	INNER JOIN Taxon_List_Item TLIChild ON TLIChild.Parent=ITN.Taxon_List_Item_Key
	INNER JOIN Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key=ITN.Taxon_List_Version_Key
		AND TLV.Taxon_List_Key=@Taxon_List_Key

	EXEC usp_IndexTaxonName_ApplyNameServer_SingleList @Taxon_List_Key

	UPDATE Import_Export_Job
	SET Records_Processed = Records_Processed + @@ROWCOUNT
	WHERE Import_Export_Job_ID = @job_id

	EXECUTE		usp_Import_Export_Job_UpdateStatus	@job_id,
													'Exporting Taxon Common Names...'	

	/* Create a local table containing the Taxon common name data */
	DECLARE @TaxonCommonName TABLE (
		Taxon_List_Item_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS PRIMARY Key,
		Taxon_Version_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS
	)

	/* Get the Taxon_List_Item_Keys first, as there may be several Taxon_Version_Keys for some, 
	  and that would break the primary Key constraint. */
	INSERT INTO @TaxonCommonName
	SELECT DISTINCT TDM1.Taxon_List_Item_Key, NULL
	FROM 	Taxon_Dictionary_Concept_Mapping TDM1 
	JOIN 	Concept C1 
			ON C1.Concept_Key = TDM1.Concept_Key
			AND C1.Concept_Group_Key = @concept_group_Key

	/* Now get a Taxon_Version_Key for each Taxon_List_Item_Key found, it'll use just one, thus 
	  being ok with the primary Key constraint.  */
	UPDATE 	TCNTemp
	SET 	Taxon_Version_Key = TLI.Taxon_Version_Key
	FROM 	@TaxonCommonName TCNTemp
	INNER JOIN Taxon_Dictionary_Concept_Mapping TDM1 ON TDM1.Taxon_List_Item_Key = TCNTemp.Taxon_List_Item_Key
	INNER JOIN Concept C1 ON C1.Concept_Key=TDM1.Concept_Key
			AND C1.Concept_Group_Key=@concept_group_Key
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

	/* Update existing Taxon common name records that are out of date */
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
		
	/* Insert any new required Taxon common name records */
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
/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_TaxonVersion_ImportConceptGroup]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_TaxonVersion_ImportConceptGroup]
GO

/*===========================================================================*\
  Description:	Import taxon versions corresponding to items in a concept
				group.

  Parameters:	@job_id					Job identifier

  Created:		Dec 2003

  Last revision information:
	$Revision: 5 $
	$Date: 17/08/11 15:40 $
	$Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_TaxonVersion_ImportConceptGroup]
	@job_id					INT,
	@SessionID			CHAR(16)
AS
	SET NOCOUNT ON

 DECLARE     @concept_group_key			CHAR(16),
				@term_version_key			CHAR(16),
				@taxon_key					CHAR(16),
				@attribute					VARCHAR(10),
				@entered_by					CHAR(16),
				@entry_date					SMALLDATETIME,
				@changed_by					CHAR(16),
				@changed_date				SMALLDATETIME,
				@system						BIT,
				@taxon_version_key			CHAR(16),
				@source_key					CHAR(16),
				@source_join_key			CHAR(16),
				@prior_term_version_key		CHAR(16),
				@concept_key	CHAR(16),
				@term_key		CHAR(16)


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
													'Exporting term versions'
	IF @@ERROR <> 0 GOTO fail
 

	DECLARE		versions	CURSOR LOCAL FAST_FORWARD FOR
	SELECT DISTINCT
				tv.Term_Version_Key,
				tm.Taxon_Key,
				tv.Version_Label,
				es.User_Name_Key,
				CONVERT(SMALLDATETIME,
						CONVERT(VARCHAR, es.Date_Time_Start, 112)),
				cs.User_Name_Key,
				CONVERT(SMALLDATETIME,
						CONVERT(VARCHAR, cs.Date_Time_Start, 112)),
				ISNULL(tv.System_Supplied_Data, c.System_Supplied_Data),
				c.Concept_Key,
				tm.Term_key
	FROM		Concept							AS	c
	LEFT JOIN	Term_Version					AS	tv
	ON			tv.Term_Version_Key				=	c.Term_Version_Key
	INNER JOIN	Taxon_Dictionary_Term_Mapping	AS	tm
	ON			tm.Term_Key						=	c.Term_Key
	INNER JOIN	Session							AS	es
	ON			es.Session_ID					=	 ISNULL(tv.Entered_Session_ID, @SessionID)
	LEFT JOIN	Session							AS	cs
	ON			cs.Session_ID					=	tv.Changed_Session_ID
	WHERE		c.Concept_Group_Key				=	@concept_group_key
	ORDER BY	tv.Term_Version_Key

	SET			@prior_term_version_key			=	''

	OPEN		versions

	WHILE 1 = 1
	BEGIN
		FETCH		versions
		INTO		@term_version_key,
					@taxon_key,
					@attribute,
					@entered_by,
					@entry_date,
					@changed_by,
					@changed_date,
					@system,
					@concept_key,
					@term_key

		IF @@FETCH_STATUS <> 0 BREAK

		BEGIN TRANSACTION

		SELECT      @taxon_version_key						=	NULL,
					@source_join_key						=	NULL,
					@source_key								=	NULL

		SELECT		@taxon_version_key						=	m.Taxon_Version_Key,
					@source_key								=	j.Source_Key
		FROM		Taxon_Dictionary_Term_Version_Mapping	AS	m
		LEFT JOIN	Source_Join								AS	j
		ON			j.Source_Join_Key						=	m.Source_Join_Key
		WHERE		m.Term_Version_Key						=	@term_version_key

		IF @source_key IS NULL
		BEGIN
			/* there is no existing mapping for the source join; pick an
			 * arbitrary join record (if there are any) and make this the
			 * mapped join.
			 */
			SELECT		@source_join_key	=	Source_Join_Key,
						@source_key			=	Source_Key
			FROM		Source_Join
			WHERE		Record_Key			=	@term_version_key
			AND			Table_Name			=	'Term_Version'
			ORDER BY	Source_Join_Key
		END

		IF @taxon_version_key IS NOT NULL
		BEGIN
			/* update taxon version */
			UPDATE		TAXON_VERSION
			SET			TAXON_KEY				=	@taxon_key,
						ATTRIBUTE				=	@attribute,
						SOURCE_KEY				=	@source_key,
						ENTERED_BY				=	@entered_by,
						ENTRY_DATE				=	@entry_date,
						CHANGED_BY				=	@changed_by,
						CHANGED_DATE			=	@changed_date,
						SYSTEM_SUPPLIED_DATA   	=	@system
			WHERE		TAXON_VERSION_KEY		=	@taxon_version_key

			IF @@ERROR <> 0 GOTO fail_from_cursor

			IF @source_join_key IS NOT NULL
			BEGIN
				UPDATE		Taxon_Dictionary_Term_Version_Mapping
				SET			Source_Join_Key							=	@source_join_key
				WHERE		Taxon_Version_Key						=	@taxon_version_key

				IF @@ERROR <> 0 GOTO fail_from_cursor
			END
		END
		ELSE
		BEGIN
			/* create taxon version */
			EXECUTE		spNextKey		'TAXON_VERSION',
										@taxon_version_key	OUTPUT
			IF @@ERROR <> 0 GOTO fail_from_cursor

			INSERT		TAXON_VERSION (
						TAXON_VERSION_KEY,
						TAXON_KEY,
						ATTRIBUTE,
						UK_NATIVE,
						SOURCE_KEY,
						ENTERED_BY,
						ENTRY_DATE,
						CHANGED_BY,
						CHANGED_DATE,
						SYSTEM_SUPPLIED_DATA)
			VALUES		(@taxon_version_key,
						@taxon_key,
						@attribute,
						0,
						@source_key,
						@entered_by,
						@entry_date,
						@changed_by,
						@changed_date,
						@system)

			IF @@ERROR <> 0 GOTO fail_from_cursor

			/* record mapping */
			INSERT		Taxon_Dictionary_Term_Version_Mapping (
						Taxon_Version_Key,
						Term_Version_Key,
						Source_Join_Key)
			VALUES		(@taxon_version_key,
						@term_version_key,
						@source_join_key)

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END

		IF @term_version_key <> @prior_term_version_key
		BEGIN
			/* Use of @prior_term_version_key is a hack for the case where
			 * a single Term corresponds to multiple Taxon records; we don't
			 * increment the progress count until all the taxa have been
			 * considered.
			 */
			SET			@prior_term_version_key		=	@term_version_key

		END

		COMMIT TRANSACTION
	END

	CLOSE		versions
	DEALLOCATE	versions
	RETURN

fail_from_cursor:
	CLOSE		versions
	DEALLOCATE	versions

fail:
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_TaxonVersion_ImportConceptGroup failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TaxonVersion_ImportConceptGroup') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_TaxonVersion_ImportConceptGroup'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_TaxonVersion_ImportConceptGroup TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_TaxonVersion_ImportConceptGroup TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_TaxonVersion_ImportConceptGroup TO [Dev - JNCC SQL]
END
GO
/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_Taxon_ImportConceptGroup]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_Taxon_ImportConceptGroup]
GO

/*===========================================================================*\
  Description:	Import taxa corresponding to terms in a concept group.

  Parameters:	@job_id					Job identifier

  Created:		Dec 2003

  Last revision information:
	$Revision: 5 $
	$Date: 17/08/11 15:40 $
	$Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Taxon_ImportConceptGroup]
	@job_id				CHAR(16)
AS
	SET NOCOUNT ON

	DECLARE     @concept_group_key		CHAR(16),
				@term_key				CHAR(16),
				@italic					BIT,
				@item_name				VARCHAR(60),
				@authority				VARCHAR(65),
				@language				VARCHAR(2),
				@taxon_name_type_key	CHAR(16),
				@entered_by				CHAR(16),
				@entry_date				SMALLDATETIME,
				@changed_by				CHAR(16),
				@changed_date			SMALLDATETIME,
				@system					BIT,
				@Taxon_Key				CHAR(16)

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
													'Exporting terms'
	IF @@ERROR <> 0 GOTO fail

	DECLARE		terms	CURSOR LOCAL FAST_FORWARD FOR
	SELECT DISTINCT
				t.Term_Key,
				CASE WHEN PATINDEX('%<i>%', C.Published_Term) <> 0
					THEN 1
					ELSE 0
				END,
				t.Plaintext COLLATE SQL_Latin1_General_CP1_CI_AS,
				tv.Author_And_Date,
				t.Language_Key,
				tnt.Taxon_Name_Type_Key,
				ISNULL(es.User_Name_Key, 'NBNSYS0000000004') AS User_Name_Key,
				ISNULL(CONVERT(SMALLDATETIME,
						CONVERT(VARCHAR, es.Date_Time_Start, 112)), GetDate()),
				cs.User_Name_Key,
				CONVERT(SMALLDATETIME,
						CONVERT(VARCHAR, cs.Date_Time_Start, 112)),
				t.System_Supplied_Data
	FROM		Concept								AS	c
	INNER JOIN	Term								AS	t
	ON			t.Term_Key							=	c.Term_Key
	LEFT JOIN	Term_Version						AS	tv
	ON			tv.Term_Version_Key					=	c.Term_Version_Key
	INNER JOIN	Taxon_Dictionary_Name_Type_Mapping	AS	tnt
	ON			tnt.Thesaurus_Name_Type_Key			=	c.Name_Type_Concept_Key
	LEFT JOIN	Session								AS	es
	ON			es.Session_ID						=	t.Entered_Session_ID
	LEFT JOIN	Session								AS	cs
	ON			cs.Session_ID						=	t.Changed_Session_ID
	WHERE		c.Concept_Group_Key					=	@concept_group_key

	OPEN		terms

	WHILE 1 = 1
	BEGIN
		FETCH		terms
		INTO		@term_key,
					@italic,
					@item_name,
					@authority,
					@language,
					@taxon_name_type_key,
					@entered_by,
					@entry_date,
					@changed_by,
					@changed_date,
					@system

		IF @@FETCH_STATUS <> 0 BREAK

		BEGIN TRANSACTION

		SELECT		@taxon_key						=	tdm.Taxon_Key
		FROM		Taxon_Dictionary_Term_Mapping	AS	tdm
		INNER JOIN	TAXON							AS	tx
		ON			tx.TAXON_KEY					=	tdm.Taxon_Key
		WHERE		tdm.Term_Key					=	@term_key
		AND			tx.TAXON_NAME_TYPE_KEY			=	@taxon_name_type_key

		IF @@ROWCOUNT > 0
		BEGIN
			/* update taxon */
			UPDATE		TAXON
			SET			ITEM_NAME					=	@item_name,
						AUTHORITY					=	@authority,
						LANGUAGE					=	@language,
						ENTERED_BY					=	@entered_by,
						ENTRY_DATE					=	@entry_date,
						CHANGED_BY					=	@changed_by,
						CHANGED_DATE				=	@changed_date,
						SYSTEM_SUPPLIED_DATA		=	@system
			WHERE		TAXON_KEY					=	@taxon_key

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END
		ELSE
		BEGIN
			/* create new taxon */
			EXECUTE		spNextKey	'TAXON',
									@taxon_key	OUTPUT
			IF @@ERROR <> 0 GOTO fail_from_cursor

			INSERT		TAXON (
						TAXON_KEY,
						ITEM_NAME,
						AUTHORITY,
						LANGUAGE,
						TAXON_NAME_TYPE_KEY,
						ENTERED_BY,
						ENTRY_DATE,
						CHANGED_BY,
						CHANGED_DATE,
						SYSTEM_SUPPLIED_DATA)
			VALUES		(@taxon_key,
						@item_name,
						@authority,
						@language,
						@taxon_name_type_key,
						@entered_by,
						@entry_date,
						@changed_by,
						@changed_date,
						@system)

			IF @@ERROR <> 0 GOTO fail_from_cursor

			/* record mapping */
			INSERT		Taxon_Dictionary_Term_Mapping (
						Taxon_Key,
						Italic_Font,
						Term_Key)
			VALUES		(@taxon_key,
						@italic,
						@term_key)

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END

		EXECUTE		usp_Import_Export_Job_RecordProcessed	@job_id
		IF @@ERROR <> 0 GOTO fail_from_cursor

		COMMIT TRANSACTION
	END

	CLOSE		terms
	DEALLOCATE	terms
	RETURN

fail_from_cursor:
	CLOSE		terms
	DEALLOCATE	terms

fail:
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_Taxon_ImportConceptGroup failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Taxon_ImportConceptGroup') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Taxon_ImportConceptGroup'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Taxon_ImportConceptGroup TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Taxon_ImportConceptGroup TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Taxon_ImportConceptGroup TO [Dev - JNCC SQL]
END
GO
SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

/*============================================================================*\
	Drop stored proc before re-creating.
\*============================================================================*/
IF OBJECT_ID(N'dbo.usp_TermGenerator_GetRules') IS NOT NULL
	DROP PROCEDURE dbo.usp_TermGenerator_GetRules
GO

/*============================================================================*\
	Description:
		Returns all of the term generators in the system.

	Created: July 2011

	Last revision information:
		$Revision: 5 $
		$Date: 17/08/11 15:40 $
		$Author: Jamesbichard $
\*============================================================================*/

CREATE PROCEDURE dbo.usp_TermGenerator_GetRules
AS
	SELECT
		Term_Generator_Key,
		Item_Name
	FROM dbo.Term_Generator
	ORDER BY Item_Name
		
GO

/*============================================================================*\
	Grant permissions.
\*============================================================================*/
PRINT 'Setting up security on procedure usp_TermGenerator_GetRules'

IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
    GRANT EXECUTE ON dbo.usp_TermGenerator_GetRules TO R2k_AddOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
	GRANT EXECUTE ON dbo.usp_TermGenerator_GetRules TO R2k_Administrator
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
	GRANT EXECUTE ON dbo.usp_TermGenerator_GetRules TO R2k_FullEdit
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
	GRANT EXECUTE ON dbo.usp_TermGenerator_GetRules TO R2k_ReadOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
	GRANT EXECUTE ON dbo.usp_TermGenerator_GetRules TO R2k_RecordCardsOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
    GRANT EXECUTE ON dbo.usp_TermGenerator_GetRules TO "Dev - JNCC SQL"
GO
SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

/*============================================================================*\
	Drop function before re-creating.
\*============================================================================*/
IF OBJECT_ID(N'dbo.usp_TermGenerator_Select') IS NOT NULL
	DROP PROCEDURE dbo.usp_TermGenerator_Select
GO

/*============================================================================*\
	Description:
		Returns published term function for specified key. Key can directly be
		a term generator key, or can be a concept key, for which the 
		associated term generator key is obtained.

	Created: July 2011

	Last revision information:
		$Revision: 5 $
		$Date: 17/08/11 15:40 $
		$Author: Jamesbichard $
\*============================================================================*/

CREATE PROCEDURE dbo.usp_TermGenerator_Select(
	@Key CHAR(16)
)
AS
BEGIN

	SELECT Published_Term_Function, Search_Term_Procedure
	FROM Term_Generator
	WHERE Term_Generator_Key = @Key
END
GO

/*============================================================================*\
	Grant permissions.
\*============================================================================*/
PRINT 'Setting up security on function usp_TermGenerator_Select'

IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
    GRANT EXECUTE ON dbo.usp_TermGenerator_Select TO R2k_AddOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
	GRANT EXECUTE ON dbo.usp_TermGenerator_Select TO R2k_Administrator
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
	GRANT EXECUTE ON dbo.usp_TermGenerator_Select TO R2k_FullEdit
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
	GRANT EXECUTE ON dbo.usp_TermGenerator_Select TO R2k_ReadOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
	GRANT EXECUTE ON dbo.usp_TermGenerator_Select TO R2k_RecordCardsOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
    GRANT EXECUTE ON dbo.usp_TermGenerator_Select TO "Dev - JNCC SQL"
GO
/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_TermHTMLNames_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_TermHTMLNames_Select]
GO



/*===========================================================================*\
  Description:	Returns multiple recordsets suitable for populating the HTML
			details of a concept for the name and synonyms.  This is separated from 
			the rest of the details so that it can be displayed first whilst the 
			rest of the HTML is loaded.
			The following recordsets are returned:
				Title
				List Synonyms
				All Known Synonyms

  Parameters:	@Key	Collection key

  Created:	August 2003

  Last revision information:
    $Revision: 5 $
    $Date: 17/08/11 15:40 $
    $Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_TermHTMLNames_Select]
	@ConceptKey char(16)
AS

SET NOCOUNT ON

/*===========================================================================*\	
  Recordset for HTML Title (name + author).  Single record.
\*===========================================================================*/
	SELECT Item_Name	AS		ItemName
	FROM VW_ConceptTerm
	WHERE Concept_Key=@ConceptKey


/*===========================================================================*\	
  Recordset for list synonyms, ordered in language priority then alphabetical
\*===========================================================================*/
	SELECT 
		C2.Published_Term AS ItemName,
		CASE WHEN L.Priority IS NULL THEN 0x7FFF ELSE L.Priority END AS Seq, T.Plaintext
	FROM Concept C1
		INNER JOIN Concept C2 on C2.Meaning_Key=C1.Meaning_Key
				AND C2.Concept_Group_Key=C1.Concept_Group_Key
		INNER JOIN Term T on T.Term_Key=C2.Term_Key
		INNER JOIN Language L on L.Language_Key=T.Language_Key
	WHERE C1.Concept_Key=@ConceptKey
		AND C2.Concept_Key<>@ConceptKey
	ORDER BY Seq, C2.Preferred, T.Plaintext


/*===========================================================================*\	
  Recordset for all synonyms, ordered in language priority then alphabetical
\*===========================================================================*/
	SELECT DISTINCT
		C2.Published_Term AS ItemName,
		CASE WHEN L.Priority IS NULL THEN 0x7FFF ELSE L.Priority END AS Seq, T.Plaintext
	FROM Concept C1
		INNER JOIN Concept C2 on C2.Meaning_Key=C1.Meaning_Key
		INNER JOIN Term T on T.Term_Key=C2.Term_Key
		INNER JOIN Language L on L.Language_Key=T.Language_Key
	WHERE C1.Concept_Key=@ConceptKey
	ORDER BY Seq, T.Plaintext
	
	SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TermHTMLNames_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_TermHTMLNames_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_TermHTMLNames_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_TermHTMLNames_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_TermHTMLNames_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_TermHTMLNames_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_TermHTMLNames_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_TermHTMLNames_Select TO [Dev - JNCC SQL]
END

GO
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_TermName_Get]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_TermName_Get]
GO


/*===========================================================================*\
  Description:	Gets the term plaintext given a term key.

				NOTE: Obsolete because captions should now use concept published
				term.

  Parameters:	@Key
		@Caption OUTPUT	

  Created:	December 2003

  Last revision information:
    $Revision: 5 $
    $Date: 17/08/11 15:40 $
    $Author: Jamesbichard $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_TermName_Get] 
@Key CHAR(16),
@Caption VARCHAR(100) OUTPUT

AS
	SET NOCOUNT ON

	SELECT		@Caption = Plaintext
	FROM		Term
	WHERE		Term_Key = @Key

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TermName_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_TermName_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_TermName_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_TermName_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_TermName_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_TermName_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_TermName_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_TermName_Get TO [Dev - JNCC SQL]
END

GO
If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Terms_Select_ForEnquiry]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Terms_Select_ForEnquiry]
GO

/*===========================================================================*\
  Description:	Returns Terms data to the CollectionsBrowser for a given Enquiry.

  Parameters:	
	@ParentKey 	Only the records associated with the parent key are returned

  Created:	August 2003

  Last revision information:
    $Revision: 5 $
    $Date: 17/08/11 15:40 $
    $Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Terms_Select_ForEnquiry] 
	@ParentKey CHAR(16)
AS

SET NOCOUNT ON

	SELECT 		C.Concept_Key AS Item_Key, C.Published_Term AS Item_Name, EC.Enquiry_Concept_Key AS Join_Key

	FROM 		Enquiry_Concept EC
	INNER JOIN 	Concept C ON EC.Concept_Key = C.Concept_Key AND EC.Enquiry_Key = @ParentKey
	ORDER BY 	C.Published_Term
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Terms_Select_ForEnquiry') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Terms_Select_ForEnquiry'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Terms_Select_ForEnquiry TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Terms_Select_ForEnquiry TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Terms_Select_ForEnquiry TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Terms_Select_ForEnquiry TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Terms_Select_ForEnquiry TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Terms_Select_ForEnquiry TO [Dev - JNCC SQL]
END
GO
If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Terms_Select_ForSearch]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Terms_Select_ForSearch]
GO

/*===========================================================================*\
  Description: 	Search proc for Term table.

  Parameters:	@SearchText
		@SearchKey	Language_key

  Created:	December 2003

  Last revision information:
    $Revision: 5 $
    $Date: 17/08/11 15:40 $
    $Author: Jamesbichard $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_Terms_Select_ForSearch] 
	@SearchText varchar(100),
	@SearchKey varchar(4) = NULL
AS

SET NOCOUNT ON

	-- N.B. This sp used to return Term.Item_Name as the display name. This field
	-- is now obsolete, and replaced by Concept.Published_Term. However, since this
	-- search has nothing to do with concepts, there is no choice but to display
	-- Term.Plaintext.

	IF @SearchKey IS NOT NULL 
		SELECT 
				T.Term_Key AS Item_Key,
				Plaintext AS DisplayTerm,
				Plaintext AS SearchTerm,
				Language_Key
		FROM		Term AS T
		WHERE		PlainText LIKE @SearchText + '%' 
		AND 		Language_Key = @SearchKey
		ORDER BY 	Plaintext
	ELSE
		SELECT 
				T.Term_Key AS Item_Key,
				Plaintext AS DisplayTerm,
				Plaintext AS SearchTerm,
				Language_Key
		FROM		Term AS T
		WHERE		PlainText LIKE @SearchText + '%' 
		ORDER BY 	Plaintext
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Terms_Select_ForSearch') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Terms_Select_ForSearch'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Terms_Select_ForSearch TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Terms_Select_ForSearch TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Terms_Select_ForSearch TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Terms_Select_ForSearch TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Terms_Select_ForSearch TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Terms_Select_ForSearch TO [Dev - JNCC SQL]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'BasicRecorderAccess')
        	GRANT EXECUTE ON dbo.usp_Terms_Select_ForSearch TO [BasicRecorderAccess]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_TermVersion_Insert]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_TermVersion_Insert]
GO

/*===========================================================================*\
  Description:	Inserts a record into the Term_Version table

  Parameters:	@Key (Term_Version_Key)	OUTPUT
		@ConceptKey
		@VersionLabel
		@AuthorAndDate
		@SessionID
		@SyncTaxonDict
		@SystemSuppliedData
		
  Created:	December 2003

  Last revision information:
    $Revision: 5 $
    $Date: 17/08/11 15:40 $
    $Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_TermVersion_Insert]
	@Key char(16) OUTPUT,
	@ConceptKey char(16),
	@TermKey char(16) = null,
	@VersionLabel varchar(100),
	@AuthorAndDate varchar(100),
	@SessionID char(16),
	@SyncTaxonDict bit = 0,
	@SystemSuppliedData bit = NULL
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION

		IF @TermKey IS NULL
		BEGIN
			SELECT 	@TermKey = Term_Key
			FROM	Concept
			WHERE	Concept_Key = @ConceptKey
		END


		EXECUTE spNextKey 'Term_Version', @Key OUTPUT
		
		INSERT INTO Term_Version (
			Term_Version_Key,
			Term_Key,
			Version_Label,
			Author_And_Date,
			Entered_Session_ID,
			System_Supplied_Data			
		) VALUES (
			@Key, 	
			@TermKey,
			@VersionLabel,
			@AuthorAndDate,
			@SessionID,
			IsNull(@SystemSuppliedData, 0)
		)
		IF @@Error <> 0 GOTO RollbackAndExit

		/*-------------------------------------------------------------*\
		  Update Concept to point to new version
		\*-------------------------------------------------------------*/
		UPDATE Concept
		SET Term_Version_Key=@Key
		WHERE Concept_Key=@ConceptKey

		IF @@Error <> 0 GOTO RollbackAndExit

		/*-------------------------------------------------------------*\
			Keep mapping for version, so that deletion synching can work
		\*-------------------------------------------------------------*/
		IF (@SyncTaxonDict = 1) AND EXISTS (SELECT *	FROM	SysObjects 
						WHERE	Id = Object_Id(N'[dbo].[Taxon_Version]') AND Type = 'U')
		BEGIN
			DECLARE @TaxonVersionKey CHAR(16)
			SELECT @TaxonVersionKey=TLI.Taxon_Version_Key
			FROM Concept C
			INNER JOIN Taxon_Dictionary_Concept_Mapping CM ON CM.Concept_Key=C.Concept_Key
			INNER JOIN Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key=CM.Taxon_List_Item_Key

			IF NOT @TaxonVersionKey IS NULL
			BEGIN

				IF EXISTS(SELECT 1 FROM Taxon_Dictionary_Term_Version_Mapping WHERE Taxon_Version_Key=@TaxonVersionKey)
					UPDATE Taxon_Dictionary_Term_Version_Mapping 
					SET Term_Version_Key=@Key
					WHERE Taxon_Version_Key=@TaxonVersionKey
				ELSE
				INSERT INTO Taxon_Dictionary_Term_Version_Mapping VALUES(@TaxonVersionKey, @Key, NULL)

				IF @@Error <> 0 GOTO RollbackAndExit

			END

			IF @@Error <> 0 GOTO RollbackAndExit

		END

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TermVersion_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_TermVersion_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_TermVersion_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_TermVersion_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_TermVersion_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_TermVersion_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_TermVersion_Insert TO [Dev - JNCC SQL]
END
GO
/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_TermVersion_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_TermVersion_Update]
GO

/*===========================================================================*\
  Description:	Updates a record in the Term_Version table. However, if
				(@VersionLabel IS NULL AND @AuthorAndDate IS NULL) OR 
				(@VersionLabel = '' AND @AuthorAndDate = '') then
				the Term_Version record will be deleted. This method is used
				to delete Term Versions, rather than 'usp_TermVersion_Delete'
				because it is more intelligent using this method, than brute
				force deletion.

  Parameters:	@Key (Term_Version_Key)
				@ConceptKey 
				@VersionLabel
				@AuthorAndDate
				@SessionID 
				@Timestamp 
				@SyncTaxonDict
				@RecordsAffected OUTPUT

  Created:	December 2003

  Last revision information:
    $Revision: 5 $
    $Date: 17/08/11 15:40 $
    $Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_TermVersion_Update]
	@Key char(16),
	@ConceptKey char(16),
	@TermKey char(16) = NULL,
	@VersionLabel varchar(100),
	@AuthorAndDate varchar(100),
	@SessionID char(16),
	@Timestamp timestamp = NULL,
	@SyncTaxonDict bit = 0,
	@RecordsAffected int =1 OUTPUT
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION

		DECLARE @Error int

		IF @TermKey IS NULL
		BEGIN
			SELECT 	@TermKey = Term_Key
			FROM	Concept
			WHERE	Concept_Key = @ConceptKey
		END

		IF @@Error <> 0 GOTO RollbackAndExit
		
		UPDATE 	Term_Version
		SET 	Term_Key = @TermKey,
				Version_Label = @VersionLabel,
				Author_And_Date = @AuthorAndDate,
				Changed_Session_ID = @SessionID
		WHERE	Term_Version_Key = @Key
		AND		([Timestamp] = @Timestamp OR @Timestamp IS NULL)

		SELECT	@RecordsAffected = @@RowCount,
			@Error = @@Error

		IF @Error <> 0 GOTO RollbackAndExit 

		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Term_Version WHERE Term_Version_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

		/*-------------------------------------------------------------*\
		  Update Concept to point to new version
		\*-------------------------------------------------------------*/
		UPDATE 	Concept
		SET 	Term_Version_Key = @Key
		WHERE 	Concept_Key = @ConceptKey

		IF @@Error <> 0 GOTO RollbackAndExit	
		
	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TermVersion_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_TermVersion_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_TermVersion_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_TermVersion_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_TermVersion_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_TermVersion_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_TermVersion_Update TO [Dev - JNCC SQL]
END
GO
/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_Term_ImportTaxonList]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_Term_ImportTaxonList]
GO

/*===========================================================================*\
  Description:	Import terms corresponding to items in a taxon list.

  Parameters:   @job_id					Job identifier

  Created:		Nov 2003

  Last revision information:
	$Revision: 5 $
	$Date: 17/08/11 15:40 $
	$Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Term_ImportTaxonList]
	@job_id				INT
AS
	SET NOCOUNT ON
	SET ANSI_NULLS ON

	DECLARE     @concept_group_key	CHAR(16),
				@taxon_list_key		CHAR(16),
				@taxon_key			CHAR(16),
				@term_key			CHAR(16),
				@item_name      	VARCHAR(60),
				@language			VARCHAR(2),
				@ins_user_key		CHAR(16),
				@ins_date			SMALLDATETIME,
				@ins_session_id		CHAR(16),
				@upd_user_key		CHAR(16),
				@upd_date			SMALLDATETIME,
				@upd_session_id		CHAR(16),
				@system				BIT,
				@italic				BIT,
				@plaintext			NVARCHAR(300),
				@create_term		BIT

	/* determine parameters of job */
	SELECT		@concept_group_key						=	m.Concept_Group_Key,
				@taxon_list_key							=	m.Taxon_List_Key
	FROM		Import_Export_Job						AS	j
	INNER JOIN	Taxon_Dictionary_Concept_Group_Mapping	AS	m
	ON			m.Concept_Group_Key						=	j.Concept_Group_Key
	WHERE		j.Import_Export_Job_ID					=	@job_id

	IF @@ROWCOUNT = 0
	BEGIN
		RAISERROR ('Job does not exist or has not been configured', 16, 1)
		RETURN
	END

	EXECUTE		usp_Import_Export_Job_UpdateStatus	@job_id,
													'Importing terms'
	IF @@ERROR <> 0 RETURN

	DECLARE		@versions	TABLE (
				Taxon_Version_Key	CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS,
				List_Font_Italic	BIT)

	INSERT		@versions
    SELECT      tli.TAXON_VERSION_KEY,
                tr.List_Font_Italic
	FROM        TAXON_LIST_VERSION				AS	tlv
	INNER JOIN	TAXON_LIST_ITEM					AS	tli
	ON			tli.TAXON_LIST_VERSION_KEY		=	tlv.TAXON_LIST_VERSION_KEY
	INNER JOIN	TAXON_RANK						AS	tr
	ON			tr.TAXON_RANK_KEY				=	tli.TAXON_RANK_KEY
	WHERE		tlv.TAXON_LIST_KEY				=	@taxon_list_key

	DECLARE		terms	CURSOR LOCAL FAST_FORWARD FOR
	SELECT DISTINCT
				t.TAXON_KEY,
				t.ITEM_NAME,
				t.LANGUAGE,
				t.ENTERED_BY,
				t.ENTRY_DATE,
				t.CHANGED_BY,
				t.CHANGED_DATE,
				t.SYSTEM_SUPPLIED_DATA,
				CASE WHEN t.LANGUAGE = 'La'
					 AND v0.LIST_FONT_ITALIC = 1
					THEN 1
					ELSE 0
				END
	FROM		@versions							AS	v0
	INNER JOIN	TAXON_VERSION						AS	tv
	ON			tv.TAXON_VERSION_KEY				=	v0.TAXON_VERSION_KEY
	INNER JOIN	TAXON								AS	t
	ON			t.TAXON_KEY							=	tv.TAXON_KEY

	OPEN		terms

	WHILE 1 = 1
	BEGIN
		FETCH		terms
		INTO		@taxon_key,
					@plaintext,
					@language,
					@ins_user_key,
					@ins_date,
					@upd_user_key,
					@upd_date,
					@system,
					@italic

		IF @@FETCH_STATUS <> 0 BREAK

		SET			@item_name		=	@plaintext

		BEGIN TRANSACTION										

		/* obtain session identifiers */
		EXECUTE		usp_Session_ForDate		@ins_user_key,
											@ins_date,
											@ins_session_id		OUTPUT
		IF @@ERROR <> 0 GOTO fail_from_cursor

		IF @upd_user_key IS NULL OR @upd_date IS NULL
		BEGIN
			SET			@upd_session_id		=	NULL
		END
		ELSE
		BEGIN
			EXECUTE		usp_Session_ForDate		@upd_user_key,
												@upd_date,
												@upd_session_id		OUTPUT
			IF @@ERROR <> 0 GOTO fail_from_cursor
		END

		/* check for existing mapping */
		SELECT		@term_key						=	Term_Key
		FROM		Taxon_Dictionary_Term_Mapping
		WHERE		Taxon_Key						=	@taxon_key
		AND			Italic_Font						=	@italic

		SELECT		@create_term	=	CASE WHEN @@ROWCOUNT = 0
											THEN 1
											ELSE 0
										END

		IF @create_term = 0
		BEGIN
			IF NOT EXISTS (	SELECT		1
							FROM		Term
							WHERE		Term_Key				=	@term_key
							AND			Language_Key			=	@language
							AND			Plaintext				=	@plaintext )
			BEGIN
				/* term has been modified */
				IF EXISTS (	SELECT		1
							FROM		Concept
							WHERE		Term_Key			=	@term_key
							AND			Concept_Group_Key	<>	@concept_group_key )
				BEGIN
					/* term is linked outside this concept group */
					SET			@create_term	=	1
				END
				ELSE
				BEGIN
					/* term linked only within this concept group */
					DECLARE		@new_term_key	CHAR(16)

					SELECT		@new_term_key	=	Term_Key
					FROM		Term
					WHERE		Language_Key	=	@language
					AND			Plaintext		=	@plaintext
					AND			Term_Key		<>	@term_key

					IF @@ROWCOUNT = 0
					BEGIN
						/* update the current term */
						UPDATE		Term
						SET         Language_Key		=	@language,
									Plaintext			=	@plaintext,
									Changed_Session_ID	=	@upd_session_id
						WHERE		Term_Key			=	@term_key

						IF @@ERROR <> 0 GOTO fail_from_cursor
					END
					ELSE
					BEGIN
						-- Create a placeholder term version for the concept
						-- whose term is to be deleted...
						
						DECLARE @TermVersionKey INT
						
						EXECUTE spNextKey 'Term_Version', @TermVersionKey OUTPUT
						
						INSERT INTO	dbo.Term_Version
									(
										Term_Version_Key,
										Term_Key,
										Version_Label,
										Author_And_Date,
										Entered_Session_ID,
										Changed_Session_ID,
										Custodian
									)
						VALUES		(
										@TermVersionKey,
										@term_key,
										NULL,
										NULL,
										@ins_session_id,
										@upd_session_id,
										NULL
									)
						
						/* remove current term */
						UPDATE		Concept
						SET			Term_Key			=	@new_term_key,
									Term_Version_Key	=	@TermVersionKey
						WHERE		Term_Key			=	@term_key

						IF @@ERROR <> 0 GOTO fail_from_cursor

						DELETE		Term_Version
						WHERE		Term_Key		=	@term_key

						IF @@ERROR <> 0 GOTO fail_from_cursor
						
						DELETE		Term
						WHERE		Term_Key		=	@term_key

						IF @@ERROR <> 0 GOTO fail_from_cursor

						/* link to the existing term that already
						 * has the new details */
						INSERT		Taxon_Dictionary_Term_Mapping (
									Taxon_Key,
									Italic_Font,
									Term_Key)
						VALUES		(@taxon_key,
									@italic,
									@new_term_key)

						IF @@ERROR <> 0 GOTO fail_from_cursor
					END
				END
			END /* term has been modified */
		END /* if @create_term = 0 */

		IF @create_term = 1
		BEGIN
			/* check for existing term that could be used */
			SELECT		@term_key		=	Term_Key
			FROM		Term
			WHERE		Language_Key	=	@language
			AND			Plaintext		=	@plaintext

			IF @@ROWCOUNT > 0
			BEGIN
				/* map taxon onto the existing term */
				INSERT		Taxon_Dictionary_Term_Mapping (
							Taxon_Key,
							Italic_Font,
							Term_Key)
				VALUES		(@taxon_key,
							@italic,
							@term_key)

				IF @@ERROR <> 0 GOTO fail_from_cursor
			END
			ELSE
			BEGIN
				/* create term */
				EXECUTE		spNextKey	'Term',
										@term_key	OUTPUT
				IF @@ERROR <> 0 GOTO fail_from_cursor

				INSERT		Term (
							Term_Key,
							Language_Key,
							Plaintext,
							Entered_Session_ID,
							Changed_Session_ID,
							System_Supplied_Data)
				VALUES		(@term_key,
							@language,
							@plaintext,
							@ins_session_id,
							@upd_session_id,
							@system)

				IF @@ERROR <> 0 GOTO fail_from_cursor

				/* record mapping */
				INSERT		Taxon_Dictionary_Term_Mapping
							(Taxon_Key,
							Italic_Font,
							Term_Key)
				VALUES		(@taxon_key,
							@italic,
							@term_key)

				IF @@ERROR <> 0 GOTO fail_from_cursor
			END
		END

		EXECUTE		usp_Import_Export_Job_RecordProcessed	@job_id
		IF @@ERROR <> 0 GOTO fail_from_cursor

		COMMIT TRANSACTION
	END

	CLOSE		terms
	DEALLOCATE	terms
	RETURN

fail_from_cursor:
	CLOSE		terms
	DEALLOCATE	terms

fail:
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_Term_ImportTaxonList failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Term_ImportTaxonList') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Term_ImportTaxonList'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Term_ImportTaxonList TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Term_ImportTaxonList TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Term_ImportTaxonList TO [Dev - JNCC SQL]
END
GO
/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Term_Insert]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Term_Insert]
GO

/*===========================================================================*\
  Description:	Inserts a Term record

  Parameters:	@Key
		@LanguageKey 
		@Plaintext 
		@SessionID
		@SystemSuppliedData 

  Created:	January 2004

  Last revision information:
    $Revision: 5 $
    $Date: 17/08/11 15:40 $
    $Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Term_Insert]
	@Key char(16) OUTPUT,
	@LanguageKey varchar(4),
	@Plaintext nvarchar(150) = NULL,
	@SessionID char(16),
	@SystemSuppliedData bit = NULL
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	EXECUTE spNextKey 'Term', @Key OUTPUT

	BEGIN TRANSACTION
	
		INSERT INTO Term (
			Term_Key,
			Language_Key,
			Plaintext,
			Entered_Session_ID,
			System_Supplied_Data
		) VALUES (
			@Key,
			@LanguageKey,
			LTRIM(RTRIM(@Plaintext)),
			@SessionID,
			IsNull(@SystemSuppliedData, 0)
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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Term_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Term_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Term_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Term_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Term_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Term_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Term_Insert TO [Dev - JNCC SQL]
END
GO
/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Term_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Term_Select]
GO

/*===========================================================================*\
  Description:	Inserts a Term record

  Parameters:	
		@LanguageKey 
		@Plaintext 

  Created:	July 2011

  Last revision information:
    $Revision: 5 $
    $Date: 17/08/11 15:40 $
    $Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Term_Select]
	@LanguageKey varchar(4),
	@Plaintext nvarchar(150)
AS	
	SELECT Term_Key
	FROM Term 
	WHERE Plaintext = @Plaintext and Language_Key = @LanguageKey
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.[usp_Term_Select]') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Term_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.[usp_Term_Select] TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.[usp_Term_Select] TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.[usp_Term_Select] TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.[usp_Term_Select] TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.[usp_Term_Select] TO [Dev - JNCC SQL]
END
GO
/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Term_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Term_Update]
GO

/*===========================================================================*\
  Description:	Updates a record in the Term table

  Parameters:	@Key 
		@LanguageKey 
		@Plaintext 
		@SessionID 

  Created:	January 2004

  Last revision information:
    $Revision: 5 $
    $Date: 17/08/11 15:40 $
    $Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Term_Update]
	@Key char(16),
	@LanguageKey varchar(4),
	@Plaintext nvarchar(150),
	@RecordsAffected int OUTPUT,
	@SessionID char(16)
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION
		
		UPDATE 	Term
		SET 	Language_Key = @LanguageKey,
				Plaintext = @Plaintext,
			Changed_Session_ID = @SessionID
		WHERE	Term_Key = @Key

		SET @RecordsAffected = @@Rowcount

		IF @@Error <> 0 GOTO RollbackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Term_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Term_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Term_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Term_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Term_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Term_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Term_Update TO [Dev - JNCC SQL]
END
GO
/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ThesaurusFactTypes_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ThesaurusFactTypes_Select]
GO

/*===========================================================================*\
  Description:	Returns data from the Thesaurus Fact Types concept group.

  Created:	December 2003

  Last revision information:
    $Revision: 5 $
    $Date: 17/08/11 15:40 $
    $Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ThesaurusFactTypes_Select]
AS

SET NOCOUNT ON

	SELECT 		C.Concept_Key,
			C.Meaning_Key,
			C.Published_Term AS Item_Name
	FROM		Concept AS C
	WHERE 		Concept_Group_Key = 'SYSTEM000000000L'

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ThesaurusFactTypes_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ThesaurusFactTypes_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ThesaurusFactTypes_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ThesaurusFactTypes_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ThesaurusFactTypes_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ThesaurusFactTypes_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ThesaurusFactTypes_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ThesaurusFactTypes_Select TO [Dev - JNCC SQL]
END

GO
/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ThesaurusFact_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ThesaurusFact_Update]
GO

/*===========================================================================*\
  Description:	Updates a record in the Thesaurus_Fact table

  Parameters:	@Key	Thesaurus_Fact_Key

  Created:	December 2003

  Last revision information:
    $Revision: 5 $
    $Date: 17/08/11 15:40 $
    $Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ThesaurusFact_Update]
	@Key char(16),
	@ItemName varchar(100),
	@Data text,
	@MeaningKey char(16),
	@ConceptKey char(16),
	@TermVersionKey char(16),
	@RelatedTermVersions bit,
	@Inherited bit,
	@LanguageKey varchar(4),
	@FactTypeMeaningKey char(16),
	@FactTypeMeaningName varchar(100),
	@FactVagueDateStart int,
	@FactVagueDateEnd int,
	@FactVagueDateType varchar(2) = NULL,
	@SessionID char(16), 
	@SystemSuppliedData bit = NULL,
	@Timestamp timestamp
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	/*-------------------------------------------------------------*\
	  Wrap everything in one transaction.
	\*-------------------------------------------------------------*/
	BEGIN TRANSACTION

		DECLARE @FactTypeConceptKey char(16)

		-- The combo box stores the meaning key and the published term. This meaning key
		-- needs to be converted to a concept key. Because many concepts can
		-- share the same meaning key, we have to use the meaning key and the published term.
		SELECT 		@FactTypeConceptKey = Concept_Key
		FROM 		Concept AS C
		WHERE 		C.Meaning_Key = @FactTypeMeaningKey
		AND 		C.Published_Term = @FactTypeMeaningName

		UPDATE	Thesaurus_Fact
		SET	Item_Name = @ItemName,
			Data = @Data,
			Meaning_Key = @MeaningKey,
			Concept_Key = @ConceptKey,
			Term_Version_Key = @TermVersionKey,
			Related_Term_Versions = @RelatedTermVersions,
			Inherited = @Inherited,
			Language_Key = @LanguageKey,
			Changed_Session_ID = @SessionID,
			Fact_Vague_Date_Start = @FactVagueDateStart,
			Fact_Vague_Date_End = @FactVagueDateEnd,
			Fact_Vague_Date_Type = IsNull(@FactVagueDateType, 'U'),
			Fact_Type_Concept_Key = @FactTypeConceptKey,
			System_Supplied_Data = IsNull(@SystemSuppliedData, 0)
		WHERE	Thesaurus_Fact_Key = @Key
		AND		@Timestamp = Timestamp

		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Thesaurus_Fact WHERE Thesaurus_Fact_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

	COMMIT TRANSACTION

	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ThesaurusFact_Update') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_ThesaurusFact_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
       	GRANT EXECUTE ON dbo.usp_ThesaurusFact_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ThesaurusFact_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ThesaurusFact_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ThesaurusFact_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
       	GRANT EXECUTE ON dbo.usp_ThesaurusFact_Update TO [Dev - JNCC SQL]
END
GO
