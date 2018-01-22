/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_TermVersionRelation_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_TermVersionRelation_Select]
GO

/*===========================================================================*\
  Description:	Returns data from the Term_Version_Relation table

  Parameters:	@Key		Term_Version_Relation_Key
		@ConceptKey 

  Created:	December 2003

  Last revision information:
    $Revision: 4 $
    $Date: 25/02/04 14:30 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_TermVersionRelation_Select]
	@Key char(16),
	@ConceptKey char(16)
AS

SET NOCOUNT ON

	SELECT 	
			CASE @ConceptKey WHEN TVR.From_Concept_Key THEN TVR.To_Concept_Key
					 WHEN TVR.To_Concept_Key   THEN TVR.From_Concept_Key
			END AS Item_Key,			
			CASE @ConceptKey WHEN TVR.From_Concept_Key THEN CT_To.Item_Name
					 WHEN TVR.To_Concept_Key   THEN CT_From.Item_Name
			END AS Item_Name,	
			CASE @ConceptKey WHEN TVR.From_Concept_Key THEN TVR.To_Term_Version_Key
					 WHEN TVR.To_Concept_Key   THEN TVR.From_Term_Version_Key
			END AS Meaning_Key,	
			TVR.Thesaurus_Relation_Type_Key,
			CASE @ConceptKey WHEN TVR.From_Concept_Key THEN TRT.Forward_Term
					 WHEN TVR.To_Concept_Key   THEN TRT.Reverse_Term
			END AS Thesaurus_Relation_Type_Name,	
			SR.Unidirectional,
			TVR.Multiplicity,
			TVR.Comment,
			0 AS Inherited,
			TVR.[Timestamp]
	FROM		Term_Version_Relation AS TVR
	INNER JOIN	Thesaurus_Relation_Type AS TRT ON TRT.Thesaurus_Relation_Type_Key = TVR.Thesaurus_Relation_Type_Key
	INNER JOIN	Semantic_Relation AS SR ON SR.Semantic_Relation_Key = TRT.Semantic_Relation_Key
	LEFT JOIN	VW_ConceptTerm AS CT_To   ON CT_To.Concept_Key   = TVR.To_Concept_Key
	LEFT JOIN	VW_ConceptTerm AS CT_From ON CT_From.Concept_Key = TVR.From_Concept_Key

	WHERE 		TVR.Term_Version_Relation_Key = @Key

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TermVersionRelation_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_TermVersionRelation_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_TermVersionRelation_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_TermVersionRelation_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_TermVersionRelation_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_TermVersionRelation_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_TermVersionRelation_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_TermVersionRelation_Select TO [Dev - JNCC SQL]
END

GO