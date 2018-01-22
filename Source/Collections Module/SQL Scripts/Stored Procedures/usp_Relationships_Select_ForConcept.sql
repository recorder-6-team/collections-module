/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Relationships_Select_ForConcept]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Relationships_Select_ForConcept]
GO

/*===========================================================================*\
  Description:	Returns data from the Thesaurus_Fact table

  Parameters:	@Key	Concept_Key

  Created:	December 2003

  Last revision information:
    $Revision: 6 $
    $Date: 6/04/04 12:24 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Relationships_Select_ForConcept]
	@Key char(16)	-- Concept's key
AS

-- Applies To: 		ConceptRelation: 0
--			MeaningRelation: 1
--			TermVersionRelation: 2

	SET NOCOUNT ON
	SET ANSI_NULLS ON
	SET ANSI_PADDING ON
	SET ANSI_WARNINGS ON
	SET CONCAT_NULL_YIELDS_NULL ON
	SET QUOTED_IDENTIFIER ON
	SET NO_BROWSETABLE OFF

	DECLARE @MeaningKey char(16)
	DECLARE @TermVersionKey char(16)

	-- Get the meaning key
	SELECT		@MeaningKey = Meaning_Key,
			@TermVersionKey = Term_Version_Key
	FROM		Concept
	WHERE		Concept_Key = @Key

	-- Get the records needed to be the nodes
	/*-----------------*\
	  Concept_Relation
	\*-----------------*/
	SELECT 		CR.Concept_Relation_Key AS Item_Key,
			CR.To_Concept_Key AS To_Key,
			TRT.Forward_Term + ' ' + CT.Item_Name AS Item_Name,
			0 AS Applies_To,
			'Forward' AS Direction
	FROM 		Concept_Relation AS CR
	INNER JOIN	Thesaurus_Relation_Type AS TRT ON TRT.Thesaurus_Relation_Type_Key = CR.Thesaurus_Relation_Type_Key
	INNER JOIN	VW_ConceptTerm AS CT ON CT.Concept_Key = CR.To_Concept_Key
	WHERE		CR.From_Concept_Key = @Key

	UNION

	SELECT 		CR.Concept_Relation_Key AS Item_Key,
			CR.From_Concept_Key AS To_Key,
			ISNULL(TRT.Reverse_Term + ' ', '') + CT.Item_Name AS Item_Name,
			0 AS Applies_To,
			'Reverse' AS Direction
	FROM 		Concept_Relation AS CR
	INNER JOIN	Thesaurus_Relation_Type AS TRT ON TRT.Thesaurus_Relation_Type_Key = CR.Thesaurus_Relation_Type_Key
	INNER JOIN Semantic_Relation SR ON SR.Semantic_Relation_Key=TRT.Semantic_Relation_Key
			AND SR.Unidirectional=0
	INNER JOIN	VW_ConceptTerm AS CT ON CT.Concept_Key = CR.From_Concept_Key
	WHERE		CR.To_Concept_Key = @Key


	UNION

	/*-----------------*\
	  Meaning_Relation
	\*-----------------*/
		SELECT 		MR.Meaning_Relation_Key AS Item_Key,
			C2.Concept_key,
			IsNull(TRT.Forward_Term + ' ' + CT1.Item_Name, TRT.Forward_Term) +  
			CASE WHEN C1.Concept_Key <> C2.Concept_Key THEN ' (Synonym - ' + CT2.Item_Name + ')'
								   ELSE ''
			END AS Item_Name,			
			1 AS Applies_To,
			'Forward' AS Direction
	FROM 		Meaning_Relation AS MR
	INNER JOIN	Thesaurus_Relation_Type AS TRT ON TRT.Thesaurus_Relation_Type_Key = MR.Thesaurus_Relation_Type_Key
			-- So all Concepts with same meaning key are shown
	INNER JOIN	(Concept AS C1 
				INNER JOIN Concept AS C2 on C2.Meaning_Key = C1.Meaning_Key
				INNER JOIN VW_ConceptTerm as CT1 ON CT1.Concept_Key = C1.Concept_Key
				INNER JOIN VW_ConceptTerm as CT2 ON CT2.Concept_Key = C2.Concept_Key)
			ON C1.Concept_Key = MR.To_Concept_Key
	WHERE		MR.From_Meaning_Key = @MeaningKey
	
	UNION

	SELECT 		MR.Meaning_Relation_Key AS Item_Key,
			C2.Concept_Key,
			IsNull(TRT.Reverse_Term + ' ' + CT1.Item_Name, ISNULL(TRT.Reverse_Term, 'Unknown')) + 
			CASE WHEN C1.Concept_Key <> c2.Concept_Key THEN ' (Synonym - ' + CT2.Item_Name + ')'
								   ELSE ''
			END AS Item_Name,
			1 AS Applies_To,
			'Reverse' AS Direction
	FROM 		Meaning_Relation AS MR
	INNER JOIN	Thesaurus_Relation_Type AS TRT ON TRT.Thesaurus_Relation_Type_Key = MR.Thesaurus_Relation_Type_Key
	INNER JOIN Semantic_Relation SR ON SR.Semantic_Relation_Key=TRT.Semantic_Relation_Key
			AND SR.Unidirectional=0
			-- So all Concepts with same meaning key are shown
	LEFT JOIN	(Concept AS C1 
				INNER JOIN Concept AS C2 on C2.Meaning_Key = C1.Meaning_Key
				INNER JOIN VW_ConceptTerm as CT1 ON CT1.Concept_Key = C1.Concept_Key
				INNER JOIN VW_ConceptTerm as CT2 ON CT2.Concept_Key = C2.Concept_Key)
			ON C1.Concept_Key = MR.From_Concept_Key
	WHERE		MR.To_Meaning_Key = @MeaningKey

	UNION

	/*----------------------*\
	  Term_Version_Relation
	\*----------------------*/
	SELECT 		TVR.Term_Version_Relation_Key AS Item_Key,
			TVR.To_Concept_Key AS To_Key,
			IsNull(TRT.Forward_Term + ' ' + CT.Item_Name, TRT.Forward_Term) AS Item_Name,
			2 AS Applies_To,
			'Forward' AS Direction
	FROM 		Term_Version_Relation AS TVR
	INNER JOIN	Thesaurus_Relation_Type AS TRT ON TRT.Thesaurus_Relation_Type_Key = TVR.Thesaurus_Relation_Type_Key
	LEFT JOIN	VW_ConceptTerm AS CT ON CT.Concept_Key = TVR.To_Concept_Key
	WHERE		TVR.From_Term_Version_Key = @TermVersionKey

	UNION

	SELECT 		TVR.Term_Version_Relation_Key AS Item_Key,
			TVR.From_Concept_Key AS To_Key,
			IsNull(TRT.Reverse_Term + ' ' + CT.Item_Name, ISNULL(TRT.Reverse_Term, 'Unknown')) AS Item_Name,
			2 AS Applies_To,
			'Reverse' AS Direction
	FROM 		Term_Version_Relation AS TVR
	INNER JOIN	Thesaurus_Relation_Type AS TRT ON TRT.Thesaurus_Relation_Type_Key = TVR.Thesaurus_Relation_Type_Key
	INNER JOIN Semantic_Relation SR ON SR.Semantic_Relation_Key=TRT.Semantic_Relation_Key
			AND SR.Unidirectional=0
	LEFT JOIN	VW_ConceptTerm AS CT ON CT.Concept_Key = TVR.From_Concept_Key
	WHERE		TVR.To_Term_Version_Key = @TermVersionKey
	ORDER BY	Item_Key, Item_Name


SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Relationships_Select_ForConcept') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Relationships_Select_ForConcept'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Relationships_Select_ForConcept TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Relationships_Select_ForConcept TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Relationships_Select_ForConcept TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Relationships_Select_ForConcept TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Relationships_Select_ForConcept TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Relationships_Select_ForConcept TO [Dev - JNCC SQL]
END

GO