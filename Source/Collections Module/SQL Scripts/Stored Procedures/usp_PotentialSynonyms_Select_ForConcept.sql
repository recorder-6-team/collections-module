/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_PotentialSynonyms_Select_ForConcept]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_PotentialSynonyms_Select_ForConcept]
GO

/*===========================================================================*\
  Description:	Returns List Synonyms

  Parameters:	@Key	Concept_Key

  Created:	December 2003

  Last revision information:
    $Revision: 10 $
    $Date: 18/11/11 10:43 $
    $Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_PotentialSynonyms_Select_ForConcept]
	@Key char(16)
AS

SET NOCOUNT ON
SET ANSI_NULLS ON
SET ANSI_PADDING ON
SET ANSI_WARNINGS ON
SET CONCAT_NULL_YIELDS_NULL ON
SET QUOTED_IDENTIFIER ON
SET NO_BROWSETABLE OFF

	/*=============================*\
	  Actually get the results set.
	\*=============================*/
	SELECT DISTINCT
			CPotentials.Concept_Key AS Item_Key, 
			CT.Item_Name + ' (' + CG.Item_Name + ')' AS Item_Name
	FROM 		Concept AS CSource
	INNER JOIN 	Concept AS CSynonyms 		ON CSynonyms.Meaning_Key = CSource.Meaning_Key
	INNER JOIn	Term AS TSource			ON TSource.Term_Key = CSource.Term_Key
	INNER JOIN 	Term AS TSynonyms 		ON TSynonyms.Term_Key = CSynonyms.Term_Key
	INNER JOIN 	Term AS TPotentials 		ON TPotentials.Plaintext = TSynonyms.Plaintext
							AND TPotentials.Language_Key = TSource.Language_Key
	INNER JOIN 	Concept AS CPotentials 		ON CPotentials.Term_Key = TPotentials.Term_Key
	LEFT JOIN 	Concept AS CExclude		ON CExclude.Concept_Key = CPotentials.Concept_Key
							AND CExclude.Meaning_Key = CSource.Meaning_Key
	LEFT JOIN Homonym_Pair AS H	ON (H.Meaning_Key_1	= CPotentials.Meaning_Key
								AND	H.Meaning_Key_2	= CSource.Meaning_Key)
								OR (H.Meaning_Key_1 = CSource.Meaning_Key
								AND	H.Meaning_Key_2 = CPotentials.Meaning_Key)
	INNER JOIN 	VW_ConceptTerm AS CT 	ON CT.Concept_Key = CPotentials.Concept_Key
							AND CT.Concept_Key <> CSynonyms.Concept_Key
	LEFT JOIN	Term_Version AS TV 		ON TV.Term_Version_Key = CPotentials.Term_Version_Key
	INNER JOIN	Concept_Group AS CG 		ON CG.Concept_Group_Key = CPotentials.Concept_Group_Key
	WHERE 		CSource.Concept_Key = @Key
	AND 		CExclude.Concept_Key IS NULL
	AND			H.Meaning_Key_1 IS NULL	
SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_PotentialSynonyms_Select_ForConcept') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_PotentialSynonyms_Select_ForConcept'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_PotentialSynonyms_Select_ForConcept TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_PotentialSynonyms_Select_ForConcept TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_PotentialSynonyms_Select_ForConcept TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_PotentialSynonyms_Select_ForConcept TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_PotentialSynonyms_Select_ForConcept TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_PotentialSynonyms_Select_ForConcept TO [Dev - JNCC SQL]
END

GO