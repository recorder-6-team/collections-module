/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_MeaningRelation_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_MeaningRelation_Select]
GO

/*===========================================================================*\
  Description:	Returns data from the Meaning_Relation table

  Parameters:	@Key		Meaning_Relation_Key
		@ConceptKey 

  Created:	December 2003

  Last revision information:
    $Revision: 4 $
    $Date: 25/02/04 14:30 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_MeaningRelation_Select]
	@Key char(16),		-- Meaning_Relation_Key
	@ConceptKey char(16)	-- Concept_Key
AS

SET NOCOUNT ON

	SET ANSI_NULLS ON
	SET ANSI_PADDING ON
	SET ANSI_WARNINGS ON
	SET CONCAT_NULL_YIELDS_NULL ON
	SET QUOTED_IDENTIFIER ON
	SET NO_BROWSETABLE OFF

	SELECT 	
			CASE @ConceptKey WHEN MR.From_Concept_Key THEN MR.To_Concept_Key
					 WHEN MR.To_Concept_Key   THEN MR.From_Concept_Key
			END AS Item_Key,			
			CASE @ConceptKey WHEN MR.From_Concept_Key THEN CT_To.Item_Name
					 WHEN MR.To_Concept_Key   THEN CT_From.Item_Name
			END AS Item_Name,	
			CASE @ConceptKey WHEN MR.From_Concept_Key THEN MR.To_Meaning_Key
					 WHEN MR.To_Concept_Key   THEN MR.From_Meaning_Key
			END AS Meaning_Key,	
			MR.Thesaurus_Relation_Type_Key,
			CASE @ConceptKey WHEN MR.From_Concept_Key THEN TRT.Forward_Term
					 WHEN MR.To_Concept_Key   THEN TRT.Reverse_Term
			END AS Thesaurus_Relation_Type_Name,	
			SR.Unidirectional,
			MR.Multiplicity,
			MR.Inherited,
			MR.Comment,
			MR.[Timestamp]
	FROM		Meaning_Relation AS MR
	INNER JOIN	Thesaurus_Relation_Type AS TRT ON TRT.Thesaurus_Relation_Type_Key = MR.Thesaurus_Relation_Type_Key
	INNER JOIN	Semantic_Relation AS SR ON SR.Semantic_Relation_Key = TRT.Semantic_Relation_Key
	LEFT JOIN	VW_ConceptTerm AS CT_To   ON CT_To.Concept_Key   = MR.To_Concept_Key
	LEFT JOIN	VW_ConceptTerm AS CT_From ON CT_From.Concept_Key = MR.From_Concept_Key

	WHERE 		MR.Meaning_Relation_Key = @Key

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_MeaningRelation_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_MeaningRelation_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_MeaningRelation_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_MeaningRelation_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_MeaningRelation_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_MeaningRelation_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_MeaningRelation_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_MeaningRelation_Select TO [Dev - JNCC SQL]
END

GO