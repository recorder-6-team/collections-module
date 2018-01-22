IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Concept_Select_AllChildConcepts]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
	DROP PROCEDURE [dbo].[usp_Concept_Select_AllChildConcepts]
GO

/*===========================================================================*\
  Description:	Returns All concepts  below a given concept.

  Parameters:
	@Key	Concept key

  Created:	August 2008

  Last revision information:
    $Revision: 1 $
    $Date: 26/08/08 9:49 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Concept_Select_AllChildConcepts] 
	@ParentKey	CHAR(16)
AS

	SET NOCOUNT ON

	DECLARE	@RelationKey CHAR(16)

	-- Get the hierarchy type from the concept group itself.
	SELECT	@RelationKey 	= 	Hierarchy_Relation_Type_Key
	FROM	Concept_Group	CG
	JOIN	Concept			C	ON	C.Concept_Group_Key = CG.Concept_Group_Key
	WHERE	Concept_Key		= 	@ParentKey

	CREATE TABLE #ConceptKeys (
		ConceptKey	CHAR(16),
		ItemName	NVARCHAR(300),
		SortCode	INT,
		HasChildren	BIT,
		Rank		CHAR(16)	
	)

	-- Get the ball rolling with the first level of chil concepts.
	INSERT INTO #ConceptKeys 
	EXECUTE usp_Concept_Select_ForParent @ParentKey, @RelationKey

	-- Repeat until no more concepts with child concepts.
	WHILE EXISTS(SELECT * FROM #ConceptKeys WHERE HasChildren = 1)
	BEGIN
		-- Get first concept with child concepts.
		SELECT 	@ParentKey 	= ConceptKey
		FROM	#ConceptKeys
		WHERE	HasChildren = 1
	
		-- Get the child concepts in.
		INSERT INTO #ConceptKeys
		EXECUTE usp_Concept_Select_ForParent @ParentKey, @RelationKey
	
		-- Update flag to indicate child concepts are done for this one.
		UPDATE 	#ConceptKeys
		SET		HasChildren = 0
		WHERE	ConceptKey 	= @ParentKey
	END

	SELECT * FROM #ConceptKeys

	DROP TABLE #ConceptKeys
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_Select_AllChildConcepts') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_Concept_Select_AllChildConcepts'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
       	GRANT EXECUTE ON dbo.usp_Concept_Select_AllChildConcepts TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concept_Select_AllChildConcepts TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concept_Select_AllChildConcepts TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Concept_Select_AllChildConcepts TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Concept_Select_AllChildConcepts TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
       	GRANT EXECUTE ON dbo.usp_Concept_Select_AllChildConcepts TO [Dev - JNCC SQL]
END
GO