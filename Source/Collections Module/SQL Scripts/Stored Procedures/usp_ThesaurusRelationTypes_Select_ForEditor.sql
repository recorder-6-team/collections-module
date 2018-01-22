/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ThesaurusRelationTypes_Select_ForEditor]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ThesaurusRelationTypes_Select_ForEditor]
GO

/*===========================================================================*\
  Description:	Selects a record from the Thesaurus_Relation_Type table
		for the Thesaurus Editor.

  Created:	December 2003

  Last revision information:
    $Revision: 1 $
    $Date: 4/12/03 17:17 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ThesaurusRelationTypes_Select_ForEditor]
	@Key char(16)
AS

SET NOCOUNT ON
	
	SELECT		
			TRT.Item_Name,
			TRT.Semantic_Relation_Key,
			SR.Item_Name AS Semantic_Relation_Name,
			TRT.Forward_Term,
			TRT.Reverse_Term,
			TRTU1.Thesaurus_Relation_Type_Usage_Key AS Meaning,
			TRTU2.Thesaurus_Relation_Type_Usage_Key AS Concept,
			TRTU3.Thesaurus_Relation_Type_Usage_Key AS Term_Version,
			TRTU4.Thesaurus_Relation_Type_Usage_Key AS Occurrence,
			TRTU5.Thesaurus_Relation_Type_Usage_Key AS Collection_Unit,
			TRT.[Timestamp]
	FROM		Thesaurus_Relation_Type AS TRT
	INNER JOIN	Semantic_Relation AS SR ON SR.Semantic_Relation_Key = TRT.Semantic_Relation_Key
	LEFT JOIN	Thesaurus_Relation_Type_Usage AS TRTU1 ON TRTU1.Thesaurus_Relation_Type_Key = TRT.Thesaurus_Relation_Type_Key
								AND TRTU1.Relation_Usage = 1
	LEFT JOIN	Thesaurus_Relation_Type_Usage AS TRTU2 ON TRTU2.Thesaurus_Relation_Type_Key = TRT.Thesaurus_Relation_Type_Key
								AND TRTU2.Relation_Usage = 2
	LEFT JOIN	Thesaurus_Relation_Type_Usage AS TRTU3 ON TRTU3.Thesaurus_Relation_Type_Key = TRT.Thesaurus_Relation_Type_Key
								AND TRTU3.Relation_Usage = 3
	LEFT JOIN	Thesaurus_Relation_Type_Usage AS TRTU4 ON TRTU4.Thesaurus_Relation_Type_Key = TRT.Thesaurus_Relation_Type_Key
								AND TRTU4.Relation_Usage = 4
	LEFT JOIN	Thesaurus_Relation_Type_Usage AS TRTU5 ON TRTU5.Thesaurus_Relation_Type_Key = TRT.Thesaurus_Relation_Type_Key
								AND TRTU5.Relation_Usage = 5
	WHERE 		TRT.Thesaurus_Relation_Type_Key = @Key
			


SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ThesaurusRelationTypes_Select_ForEditor') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ThesaurusRelationTypes_Select_ForEditor'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ThesaurusRelationTypes_Select_ForEditor TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ThesaurusRelationTypes_Select_ForEditor TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ThesaurusRelationTypes_Select_ForEditor TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ThesaurusRelationTypes_Select_ForEditor TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ThesaurusRelationTypes_Select_ForEditor TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ThesaurusRelationTypes_Select_ForEditor TO [Dev - JNCC SQL]
END

GO