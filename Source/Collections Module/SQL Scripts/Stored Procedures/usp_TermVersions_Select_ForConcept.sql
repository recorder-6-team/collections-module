/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_TermVersions_Select_ForConcept]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_TermVersions_Select_ForConcept]
GO

/*===========================================================================*\
  Description:	Returns Term_Version records for a given concept key.

  Parameters:	@Key	Concept_Key

  Created:	December 2003

  Last revision information:
    $Revision: 3 $
    $Date: 16/02/04 15:49 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_TermVersions_Select_ForConcept]
	@Key CHAR(16),
	@ThisTermVersionLabel VARCHAR(100),
	@VersionLabel VARCHAR(100)
AS

SET NOCOUNT ON

DECLARE @TermVersionKey char(16)

	/*------------------------------------------*\
	  Get the Term Version Key for the concept.
	\*------------------------------------------*/
	SELECT 		@TermVersionKey = Term_Version_Key
	FROM		Concept
	WHERE		Concept_Key = @Key
	
	/*======================*\
	  Build the result set.
	\*======================*/
	/*--------------------------------------*\
	  Get the value for 'This Term Version'
	\*--------------------------------------*/
	SELECT 		TV.Term_Version_Key AS Item_Key, 
			@ThisTermVersionLabel + IsNull(' ' + TV.Version_Label, '') + IsNull(' (' + TV.Author_And_Date + ')', '') AS Item_Name,  
			1 AS This_Term_Version
	FROM		Concept AS C
	LEFT JOIN	Term_Version AS TV ON TV.Term_Version_Key = C.Term_Version_Key
	WHERE		C.Concept_Key = @Key
	
	/*-------------------------------------------------------*\
	  Get the Term Version the Concept's TV is related 'to'.
	\*-------------------------------------------------------*/
	UNION
	SELECT DISTINCT	TVOut.Term_Version_Key, 
			@VersionLabel + IsNull(TVOut.Version_Label, '') + IsNull(' (' + TVOut.Author_And_Date + ')', '') AS Item_Name,  
			0 AS This_Term_Version
	FROM 		Term_Version AS TV
	INNER JOIN 	Term_Version_Relation AS TVR1 	ON TVR1.From_Term_Version_Key = TV.Term_Version_Key
	INNER JOIN 	Thesaurus_Relation_Type AS TRT 	ON TRT.Thesaurus_Relation_Type_Key = TVR1.Thesaurus_Relation_Type_Key
	INNER JOIN 	Semantic_Relation AS SR 	ON SR.Semantic_Relation_Key = TRT.Semantic_Relation_Key
							AND SR.Forward_Equivalence_Possible = 1
							AND SR.Forward_Equivalence_Definite = 0
							AND SR.Reverse_Equivalence_Possible = 1
							AND SR.Reverse_Equivalence_Definite = 0
	INNER JOIN 	Term_Version AS TVOut 		ON TVOut.Term_Version_Key = TVR1.To_Term_Version_Key 
	WHERE 		TV.Term_Version_Key = @TermVersionKey
	
	/*-------------------------------------------------------*\
	  Get the Term Version the Concept's TV is related 'from'.
	\*-------------------------------------------------------*/	
	UNION
	SELECT DISTINCT	TVOut.Term_Version_Key, 
			@VersionLabel + IsNull(TVOut.Version_Label, '') + IsNull(' (' + TVOut.Author_And_Date + ')', '') AS Item_Name,  
			0 AS This_Term_Version
	FROM 		Term_Version AS TV
	INNER JOIN 	Term_Version_Relation AS TVR2 	ON TVR2.To_Term_Version_Key = TV.Term_Version_Key
	INNER JOIN 	Thesaurus_Relation_Type AS TRT 	ON TRT.Thesaurus_Relation_Type_Key = TVR2.Thesaurus_Relation_Type_Key
	INNER JOIN 	Semantic_Relation AS SR 	ON SR.Semantic_Relation_Key = TRT.Semantic_Relation_Key
							AND SR.Forward_Equivalence_Possible = 1
							AND SR.Forward_Equivalence_Definite = 0
							AND SR.Reverse_Equivalence_Possible = 1
							AND SR.Reverse_Equivalence_Definite = 0
	INNER JOIN 	Term_Version AS TVOut 		ON TVOut.Term_Version_Key = TVR2.From_Term_Version_Key
	WHERE 		TV.Term_Version_Key = @TermVersionKey

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TermVersions_Select_ForConcept') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_TermVersions_Select_ForConcept'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_TermVersions_Select_ForConcept TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_TermVersions_Select_ForConcept TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_TermVersions_Select_ForConcept TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_TermVersions_Select_ForConcept TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_TermVersions_Select_ForConcept TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_TermVersions_Select_ForConcept TO [Dev - JNCC SQL]
END

GO