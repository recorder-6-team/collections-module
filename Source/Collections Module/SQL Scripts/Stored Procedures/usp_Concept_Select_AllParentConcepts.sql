SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Concept_Select_AllParentConcepts]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Concept_Select_AllParentConcepts]
GO

/*===========================================================================*\
  Description:	
		Selects all the ancestors of the given concept, with the closest
		ancestors listed first. 

  Parameters:
		@ChildConceptKey

  Created:	September 2010

  Last revision information:
    $Revision: 2 $
    $Date: 8/09/10 11:07 $
    $Author: Robertjohnson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Concept_Select_AllParentConcepts]
	@ConceptKey	CHAR(16)
AS

	SET NOCOUNT ON

	DECLARE	@RelationKey CHAR(16)

	-- Get the hierarchy type from the concept group itself.
	SELECT	@RelationKey 	= 	Hierarchy_Relation_Type_Key
	FROM	Concept_Group	CG
	JOIN	Concept			C	ON	C.Concept_Group_Key = CG.Concept_Group_Key
	WHERE	Concept_Key		= 	@ConceptKey

	DECLARE @ConceptKeys TABLE(
		ConceptKey	CHAR(16),
		ItemName	NVARCHAR(300),
		SortCode	INT,
		HasParents	BIT,
		Rank		CHAR(16)	
	)

	-- Get the ball rolling with the first level of parent concepts.
	INSERT INTO @ConceptKeys 
	EXECUTE usp_Concept_Select_ForChild @ConceptKey, @RelationKey

	-- Repeat until no more concepts with parent concepts.
	WHILE EXISTS(SELECT * FROM @ConceptKeys WHERE HasParents = 1)
	BEGIN
		-- Get first concept with parent concepts.
		SELECT 	@ConceptKey 	= ConceptKey
		FROM	@ConceptKeys
		WHERE	HasParents = 1
	
		-- Get the parent concepts in.
		INSERT INTO @ConceptKeys
		EXECUTE usp_Concept_Select_ForChild @ConceptKey, @RelationKey
	
		-- Update flag to indicate parent concepts are done for this one.
		UPDATE 	@ConceptKeys
		SET		HasParents = 0
		WHERE	ConceptKey 	= @ConceptKey
	END

	SELECT * FROM @ConceptKeys
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_Select_AllParentConcepts') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Concept_Select_AllParentConcepts'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Concept_Select_AllParentConcepts TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concept_Select_AllParentConcepts TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concept_Select_AllParentConcepts TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Concept_Select_AllParentConcepts TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Concept_Select_AllParentConcepts TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Concept_Select_AllParentConcepts TO [Dev - JNCC SQL]
END
GO