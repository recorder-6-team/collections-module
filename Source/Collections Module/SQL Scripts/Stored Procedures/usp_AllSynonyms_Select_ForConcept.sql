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
    $Revision: 7 $
    $Date: 3/08/11 11:33 $
    $Author: Simonlewis $

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