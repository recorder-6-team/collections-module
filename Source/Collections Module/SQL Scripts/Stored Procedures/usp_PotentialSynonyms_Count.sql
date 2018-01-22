IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].usp_PotentialSynonyms_Count') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
	DROP PROCEDURE [dbo].usp_PotentialSynonyms_Count
GO

/*===========================================================================*\
  Description:	
	Returns the total number of concepts that are potential synonyms.

  Parameters:	
  @ConceptGroupKey 
	When specified, only the Concepts in the group are scanned. Otherwise,
	the whole thesaurus is scanned.

  Created:	May 2011

  Last revision information:
    $Revision: 1 $
    $Date: 25/05/11 9:19 $
    $Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].usp_PotentialSynonyms_Count 
	@ConceptGroupKey		CHAR(16),
	@RowCount				INT OUTPUT
AS
	SELECT DISTINCT @RowCount = COUNT(*)

	FROM			Concept						AS	CSource
	--Consider all synonyms of the source concept
	INNER JOIN		Concept						AS	CSynonyms 		
	ON				CSynonyms.Meaning_Key		=	CSource.Meaning_Key
	INNER JOIN		Term						AS	TSynonyms
	ON				TSynonyms.Term_Key			=	CSynonyms.Term_Key
	INNER JOIN		Term						AS	TPotentials
	ON				TPotentials.Plaintext		=	TSynonyms.Plaintext
	AND				TPotentials.Language_Key	=	TSynonyms.Language_Key
	--Join on all concepts whose term matches the term of a synonym of
	--the original concepts
	INNER JOIN		Concept						AS	CPotentials
	ON				CPotentials.Term_Key		=	TPotentials.Term_Key
	--Get all synonyms of the concepts which match the term of one of the
	--synonyms of the original concept. These will be the potential synonyms
	INNER JOIN		Concept						AS	CPotSyn
	ON				CPotSyn.Meaning_Key			=	CPotentials.Meaning_Key
	--Exclude any concepts that match by term but which are already synonyms
	LEFT JOIN		Concept						AS	CExclude
	ON				CExclude.Concept_Key		=	CPotentials.Concept_Key
	AND				CExclude.Meaning_Key		=	CSource.Meaning_Key
	--Exclude any concepts that match by term but whose meaning is already
	--marked as an homonym of the source concept meaning
	LEFT JOIN		Homonym_Pair				AS	H
	ON				(H.Meaning_Key_1			=	CPotentials.Meaning_Key
		AND			H.Meaning_Key_2				=	CSource.Meaning_Key)
	OR				(H.Meaning_Key_1			=	CSource.Meaning_Key
		AND			H.Meaning_Key_2				=	CPotentials.Meaning_Key)
	WHERE			CSource.Concept_Group_Key	=	@ConceptGroupKey
	AND				CExclude.Concept_Key IS NULL
	AND				H.Meaning_Key_1 IS NULL
	AND				CSource.List_Preferred		=	1
	AND				CPotSyn.List_Preferred		=	1
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_PotentialSynonyms_Count') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_PotentialSynonyms_Count'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_PotentialSynonyms_Count TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_PotentialSynonyms_Count TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_PotentialSynonyms_Count TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_PotentialSynonyms_Count TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_PotentialSynonyms_Count TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_PotentialSynonyms_Count TO [Dev - JNCC SQL]
END
