/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ThesaurusRelationTypes_Select_WithEquivalenceBits]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ThesaurusRelationTypes_Select_WithEquivalenceBits]
GO

/*===========================================================================*\
  Description:	Returns relationship types that satisfy the desired semantic
		relationship.

  Parameters:	@ForwardEquivalencePossible
		@ForwardEquivalenceDefinite
		@ReverseEquivalencePossible
		@ReverseEquivalenceDefinite

  Created:	November 2003

  Last revision information:
    $Revision: 1 $
    $Date: 28/11/03 11:02 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ThesaurusRelationTypes_Select_WithEquivalenceBits]
	@ForwardEquivalencePossible bit,
	@ForwardEquivalenceDefinite bit,
	@ReverseEquivalencePossible bit,
	@ReverseEquivalenceDefinite bit
AS

SET NOCOUNT ON

	SELECT		TRT.Thesaurus_Relation_Type_Key,
			TRT.Item_Name
	FROM		Thesaurus_Relation_Type AS TRT
	INNER JOIN	Semantic_Relation AS SR ON SR.Semantic_Relation_Key = TRT.Semantic_Relation_Key
	WHERE		Forward_Equivalence_Possible = @ForwardEquivalencePossible
	AND		Forward_Equivalence_Definite = @ForwardEquivalenceDefinite
	AND		Reverse_Equivalence_Possible = @ReverseEquivalencePossible
	AND		Reverse_Equivalence_Definite = @ReverseEquivalenceDefinite
		
SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ThesaurusRelationTypes_Select_WithEquivalenceBits') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ThesaurusRelationTypes_Select_WithEquivalenceBits'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ThesaurusRelationTypes_Select_WithEquivalenceBits TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ThesaurusRelationTypes_Select_WithEquivalenceBits TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ThesaurusRelationTypes_Select_WithEquivalenceBits TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ThesaurusRelationTypes_Select_WithEquivalenceBits TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ThesaurusRelationTypes_Select_WithEquivalenceBits TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ThesaurusRelationTypes_Select_WithEquivalenceBits TO [Dev - JNCC SQL]
END

GO