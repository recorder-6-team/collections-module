IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_PotentialSynonyms_Select_ForMerge]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
	DROP PROCEDURE [dbo].[usp_PotentialSynonyms_Select_ForMerge]
GO

/*===========================================================================*\
  Description:	
	Returns concepts that are potential synonyms.

  Parameters:	
  @ConceptGroupKey 
	When specified, only the Concepts in the group are scanned. Otherwise,
	the whole thesaurus is scanned.

  Created:	December 2006

  Last revision information:
    $Revision: 7 $
    $Date: 3/08/11 15:58 $
    $Author: Simonlewis $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_PotentialSynonyms_Select_ForMerge] 
	@ConceptGroupKey		CHAR(16),
	@ConceptGroupSearchKey	CHAR(16),
	@PreferredSynonymGroup	CHAR(16),
	@MaxRowCount			INT,
	@Timestamp				TIMESTAMP,
	@SessionId				CHAR(16)
AS
	SET NOCOUNT ON
	
	SET ROWCOUNT @MaxRowCount

	SELECT			DISTINCT
					CSource.Concept_Key			AS	SourceConceptKey,
					CSource.Meaning_Key			AS	SourceMeaningKey,
					CG.Item_Name				AS	SourceGroup,
					ISNULL
					(
						TVSource.Author_And_Date,
						''
					)							AS	SourceAuthority,
					CSource.Published_Term		AS	SourceConcept,
					TSource.PlainText,			-- There for the ORDER BY
					CPotSyn.Concept_Key			AS	SynonymConceptKey,
					CPotSyn.Meaning_Key			AS	SynonymMeaningKey,
					CGPotSyn.Item_Name			AS	SynonymGroup,
					ISNULL
					(
						TVPotSyn.Author_And_Date,
						''
					)							AS	SynonymAuthority,
					CPotSyn.Published_Term		AS	SynonymConcept,
					CPotentials.Published_Term	AS	SharedTerm,
					CSource.Custodian,
					CSource.Timestamp,
					CASE 
						WHEN CPotSyn.Concept_Group_Key = @PreferredSynonymGroup
							THEN 1
						ELSE 0
					END AS InPreferredGroup
	FROM			Concept						AS	CSource
	--Consider all synonyms of the source concept
	INNER JOIN		Concept						AS	CSynonyms 		
	ON				CSynonyms.Meaning_Key		=	CSource.Meaning_Key
	INNER JOIN		Term						AS	TSource
	ON				TSource.Term_Key			=	CSource.Term_Key
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
	INNER JOIN		Term						AS	TPotSyn
	ON				TPotSyn.Term_Key			=	CPotSyn.Term_Key
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
	INNER JOIN		Concept_Group				AS	CG
	ON				CG.Concept_Group_Key		=	CSource.Concept_Group_Key
	INNER JOIN		Concept_Group				AS	CGPotSyn
	ON				CGPotSyn.Concept_Group_Key	=	CPotSyn.Concept_Group_Key
	LEFT JOIN		dbo.Term_Version			AS	TVSource
	ON				TVSource.Term_Version_Key	=	CSource.Term_Version_Key
	LEFT JOIN		dbo.Term_Version			AS	TVPotSyn
	ON				TVPotSyn.Term_Version_Key	=	CPotSyn.Term_Version_Key
	WHERE			(@ConceptGroupKey IS NULL
	OR				CSource.Concept_Group_Key	=	@ConceptGroupKey)
	AND				(@ConceptGroupSearchKey IS NULL
	OR				CPotSyn.Concept_Group_Key	=	@ConceptGroupSearchKey)
	AND				CExclude.Concept_Key IS NULL
	AND				H.Meaning_Key_1 IS NULL
	AND				CSource.List_Preferred		=	1
	AND				CPotSyn.List_Preferred		=	1
	AND				CSource.[Timestamp]			>	ISNULL(@Timestamp, 0)
	AND				(@SessionId IS NULL
	OR				CSource.Entered_Session_Id	=	@SessionId)
	--following condition ensures that if two potential synonyms are in the same
	--list, the pair is only returned once
	AND				(((@ConceptGroupKey IS NOT NULL OR @ConceptGroupSearchKey IS NOT NULL)
					AND (CSource.Concept_Group_Key <> CPotSyn.Concept_Group_Key
						OR	CSource.Concept_Key > CPotSyn.Concept_Key))
		OR			(@ConceptGroupKey IS NULL AND @ConceptGroupSearchKey IS NULL
					AND CSource.Concept_Key > CPotSyn.Concept_Key))
	ORDER BY		TSource.PlainText

	
	SET ROWCOUNT 0
	SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_PotentialSynonyms_Select_ForMerge') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_PotentialSynonyms_Select_ForMerge'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_PotentialSynonyms_Select_ForMerge TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_PotentialSynonyms_Select_ForMerge TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_PotentialSynonyms_Select_ForMerge TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_PotentialSynonyms_Select_ForMerge TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_PotentialSynonyms_Select_ForMerge TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_PotentialSynonyms_Select_ForMerge TO [Dev - JNCC SQL]
END
