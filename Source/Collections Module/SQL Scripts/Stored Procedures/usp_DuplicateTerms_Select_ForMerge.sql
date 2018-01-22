IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_DuplicateTerms_Select_ForMerge]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
	DROP PROCEDURE [dbo].[usp_DuplicateTerms_Select_ForMerge]
GO

/*===========================================================================*\
  Description:	Returns Collection Units for a specified movement.

  Parameters:	
  @ConceptGroupKey 
	When specified, only the Concepts in the group are scanned. Otherwise,
	the whole thesaurus is scanned.

  Created:	December 2006

  Last revision information:
    $Revision: 3 $
    $Date: 12/02/10 9:13 $
    $Author: Simonlewis $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_DuplicateTerms_Select_ForMerge]
	@ConceptGroupKey	CHAR(16),
	@MaxRowCount		INT
AS
	SET NOCOUNT ON

	SET ROWCOUNT @MaxRowCount

	SELECT		DISTINCT
				T1.PlainText			AS	Term,
				T1.Term_Key 			AS	TermKey1, 
				T2.Term_Key 			AS	TermKey2, 
				CG1.Item_Name 			AS	Group1,
				ISNULL
				(
					TV1.Author_And_Date,
					''
				)						AS	Authority1,
				C1Pref.Concept_Key		AS	PreferredKey1,
				C1Pref.PlainText 		AS	Preferred1,
				CG2.Item_Name 			AS	Group2,
				ISNULL
				(
					TV2.Author_And_Date,
					''
				)						AS	Authority2, 
				C2Pref.Concept_Key		AS	PreferredKey2,
				C2Pref.PlainText 		AS	Preferred2,
				T1.Custodian,
				T1.Timestamp

	FROM		Term T1
	-- Find different term with same text
	JOIN		Term T2					ON	T2.PlainText = T1.PlainText
	AND			T2.Language_Key			=	T1.Language_Key
	AND			T2.Term_Key				<>	T1.Term_Key
	-- Find Concept using the term
	JOIN		Concept					AS	C1
	ON			C1.Term_Key				=	T1.Term_Key
	-- Find synonym that is list preferred
	JOIN		VW_ConceptTerm			AS	C1Pref
	ON			C1Pref.Meaning_Key		=	C1.Meaning_Key
	AND			C1Pref.List_Preferred	=	1
	-- Get group name
	JOIN		Concept_Group			AS	CG1
	ON			CG1.Concept_Group_Key	=	C1Pref.Concept_Group_Key
	-- Find other concept using same term
	JOIN		Concept					AS	C2
	ON			C2.Term_Key				=	T2.Term_Key
	-- Find synonym that is list preferred
	JOIN		VW_ConceptTerm			AS	C2Pref
	ON			C2Pref.Meaning_Key		=	C2.Meaning_Key
	AND			C2Pref.List_Preferred	=	1
	-- Get group name
	JOIN		Concept_Group			AS	CG2
	ON			CG2.Concept_Group_Key	=	C2Pref.Concept_Group_Key
	
	LEFT JOIN	dbo.Term_Version		AS		TV1
	ON			TV1.Term_Version_Key	=		C1.Term_Version_Key
	LEFT JOIN	dbo.Term_Version		AS		TV2
	ON			TV2.Term_Version_Key	=		C2.Term_Version_Key

	WHERE		(@ConceptGroupKey IS NULL
	OR			C1.Concept_Group_Key	=	@ConceptGroupKey)
	-- Only want different concepts sharing term.
	AND			C1Pref.Concept_Key		<>	C2Pref.Concept_Key
	ORDER BY	T1.PlainText,
				CG1.Item_Name,
				C1Pref.PlainText,
				C2Pref.PlainText

	SET ROWCOUNT 0

	SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_DuplicateTerms_Select_ForMerge') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_DuplicateTerms_Select_ForMerge'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_DuplicateTerms_Select_ForMerge TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_DuplicateTerms_Select_ForMerge TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_DuplicateTerms_Select_ForMerge TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_DuplicateTerms_Select_ForMerge TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_DuplicateTerms_Select_ForMerge TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_DuplicateTerms_Select_ForMerge TO [Dev - JNCC SQL]
END
