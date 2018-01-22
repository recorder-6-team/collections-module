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