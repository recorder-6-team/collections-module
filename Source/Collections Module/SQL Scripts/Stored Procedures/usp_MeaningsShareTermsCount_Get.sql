/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_MeaningsShareTermsCount_Get]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_MeaningsShareTermsCount_Get]
GO

/*===========================================================================*\
  Description:	When a relationship is saved that creates a new link between 
		2 meanings within a single concept group and the relationship 
		indicates at least some overlap of meaning, if a there is a 
		single term that is in both groups of terms associated with 
		each meaning, then certain actions take place. This proc.
		allows the app. to decide whether any action should occur 
		by the count of the number of records returned.

  Parameters:	@FromConceptKey
		@ToConceptKey
		@ThesaurusRelationTypeKey

  Created:	February 2004

  Last revision information:
    $Revision: 1 $
    $Date: 26/02/04 17:23 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_MeaningsShareTermsCount_Get]
	@FromConceptKey char(16),
	@ToConceptKey char(16),
	@ThesaurusRelationTypeKey char(16),
	@Matches int output
AS
	
SET NOCOUNT ON

	IF EXISTS( 	
		SELECT 		*
		FROM 		Thesaurus_Relation_Type AS TF
		INNER JOIN	Semantic_Relation AS SR ON SR.Semantic_Relation_Key = TF.Semantic_Relation_Key
							AND SR.Forward_Equivalence_Possible = 1
							AND SR.Reverse_Equivalence_Possible = 1
		WHERE		Thesaurus_Relation_Type_Key = @ThesaurusRelationTypeKey)
	BEGIN
		-- C1M is for Concepts sharing the same meaning as concept C1
		-- C2M is for Concepts sharing the same meaning as concept C2
		SELECT @Matches = Count(*)
		FROM 		Concept AS C1M 
		INNER JOIN	Concept AS C1 ON C1.Meaning_Key = C1M.Meaning_Key
		INNER JOIN 	Concept AS C2M ON C1M.Term_Key = C2M.Term_Key
		INNER JOIN	Concept AS C2 ON C2.Meaning_Key = C2M.Meaning_Key
		WHERE 		C1.Concept_Key = @FromConceptKey
		AND 		C2.Concept_Key = @ToConceptKey
		AND		C1.Concept_Group_Key = C2.Concept_Group_Key
	END	
	ELSE
		SELECT @Matches = 0

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_MeaningsShareTermsCount_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_MeaningsShareTermsCount_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_MeaningsShareTermsCount_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_MeaningsShareTermsCount_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_MeaningsShareTermsCount_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_MeaningsShareTermsCount_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_MeaningsShareTermsCount_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_MeaningsShareTermsCount_Get TO [Dev - JNCC SQL]
END

GO
			