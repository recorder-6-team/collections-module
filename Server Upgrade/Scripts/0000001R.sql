If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_FieldData_Select_ForSpecimen]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_FieldData_Select_ForSpecimen]
GO

/*===========================================================================*\
  Description:	Returns field data from Recorder for a specified Specimen

  Parameters:	
	@ParentKey Only the records associated with the parent key are returned

  Created:	August 2003

  Last revision information:
    $Revision: 4 $
    $Date: 2/02/07 15:49 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_FieldData_Select_ForSpecimen] 
	@ParentKey CHAR(16)
AS

SET NOCOUNT ON

	SELECT 		SFD.Specimen_Field_Data_Key AS Item_Key, SFD.Specimen_Field_Data_Key AS Join_Key,
			CASE 	WHEN S1.Sample_Key IS NOT NULL THEN S1.Sample_Key 
				WHEN S2.Sample_Key IS NOT NULL THEN S2.Sample_Key 
			END AS Hyperlink_Item_Key,
			CASE	WHEN S1.Sample_Key IS NOT NULL THEN S1.Sample_Key 
				WHEN S2.Sample_Key IS NOT NULL THEN S2.Sample_Key
			END AS Drag_Drop_Item_Key, 
			CASE 	WHEN S1.Sample_Key IS NOT NULL THEN S1.Vague_Date_Start
				WHEN S2.Sample_Key IS NOT NULL THEN S2.Vague_Date_Start
			END AS Vague_Date_Start, 
			CASE 	WHEN S1.Sample_Key IS NOT NULL THEN S1.Vague_Date_End
				WHEN S2.Sample_Key IS NOT NULL THEN S2.Vague_Date_End
			END AS Vague_Date_End, 
			CASE 	WHEN S1.Sample_Key IS NOT NULL THEN S1.Vague_Date_Type
				WHEN S2.Sample_Key IS NOT NULL THEN S2.Vague_Date_Type
			END AS Vague_Date_Type,
			CASE 	WHEN S1.Sample_Key IS NOT NULL THEN 
					CASE 	WHEN L1.Location_Name_Key IS NOT NULL THEN L1.Item_Name
						ELSE S1.Location_Name
					END
				WHEN S2.Sample_Key IS NOT NULL THEN 
					CASE 	WHEN L2.Location_Name_Key IS NOT NULL THEN L2.Item_Name
						ELSE S2.Location_Name
					END
			END AS LocationName

	FROM 		Specimen_Unit SU
	INNER JOIN 	Specimen_Field_Data SFD ON SU.Collection_Unit_Key = SFD.Collection_Unit_Key
	LEFT  JOIN 	(Occurrence O 
			INNER JOIN [Sample] S1 ON O.Sample_Key = S1.Sample_Key
			LEFT  JOIN [Location_Name] L1 ON L1.Location_Key = S1.Location_Key AND L1.Preferred = 1
			) 
		ON SFD.Occurrence_Key = O.Occurrence_Key 
	LEFT  JOIN	(Taxon_Occurrence XO 
			INNER JOIN [Sample] S2 ON XO.Sample_Key = S2.Sample_Key
			LEFT  JOIN [Location_Name] L2 ON L2.Location_Key = S2.Location_Key AND L2.Preferred = 1
			) 
		ON SFD.Taxon_Occurrence_Key = XO.Taxon_Occurrence_Key

	WHERE	SU.Collection_Unit_Key = @ParentKey
	AND	(S1.Sample_Key IS NOT NULL OR S2.Sample_Key IS NOT NULL)

	ORDER BY Vague_Date_Start DESC, Vague_Date_End DESC, Vague_Date_Type
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_FieldData_Select_ForSpecimen') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_FieldData_Select_ForSpecimen'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_FieldData_Select_ForSpecimen TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_FieldData_Select_ForSpecimen TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_FieldData_Select_ForSpecimen TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_FieldData_Select_ForSpecimen TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_FieldData_Select_ForSpecimen TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_FieldData_Select_ForSpecimen TO [Dev - JNCC SQL]
END
GO


/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_Concept_Select_Synonyms]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_Concept_Select_Synonyms]
GO

/*===========================================================================*\
  Description:	Names of synonyms of the specified concept.

  Parameters:   
	@concept_key	Concept key

  Created:	Jan 2004

  Last revision information:
	$Revision: 4 $
	$Date: 2/02/07 15:49 $
	$Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Concept_Select_Synonyms]
	@concept_key		CHAR(16)
AS
	SET NOCOUNT ON

	SELECT DISTINCT	t.Item_Name, 
			t.PlainText,
			g.Item_Name AS Group_Name
	FROM		Concept			AS	c
	INNER JOIN	Concept			AS	s
	ON		s.Meaning_Key		=	c.Meaning_Key
	INNER JOIN	Term			AS	t
	ON		t.Term_Key		=	s.Term_Key
	INNER JOIN	Concept_Group		AS 	g
	ON		g.Concept_Group_Key	=	s.Concept_Group_Key
	WHERE		c.Concept_Key		=	@concept_key
	AND		s.Concept_Key		<>	@concept_key
	ORDER BY	g.Item_Name
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_Select_Synonyms') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Concept_Select_Synonyms'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concept_Select_Synonyms TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concept_Select_Synonyms TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Concept_Select_Synonyms TO [Dev - JNCC SQL]
END
GO


IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_DuplicateTerms_Merge]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
	DROP PROCEDURE [dbo].[usp_DuplicateTerms_Merge]
GO

/*===========================================================================*\
  Description:	
	Reassign records linked to @OldTermKey to @NewTermKey before deleting
	the @OldTermKey.

  Parameters:	
	@NewTermKey	Specify the key of the term to keep.
	@OldTermKey	Specify the key of the term to delete.

  Created:	
	January 2006

  Last revision information:
    $Revision: 4 $
    $Date: 2/02/07 15:49 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_DuplicateTerms_Merge]
	@NewTermKey	CHAR(16),
	@OldTermKey	CHAR(16)
AS
	SET NOCOUNT ON 

	-- Change over 
	UPDATE	Taxon_Dictionary_Term_Mapping
	SET	Term_Key = @NewTermKey
	WHERE	Term_Key = @OldTermKey

	UPDATE	Concept
	SET	Term_Key = @NewTermKey
	WHERE	Term_Key = @OldTermKey

	-- Move records missing for new term from soon-to-be-deleted old term.
	UPDATE	Term_Version
	SET	Term_Key = @NewTermKey
	WHERE	Term_Version_Key IN (
		SELECT 		TV1.Term_Version_Key 
		FROM 		Term_Version TV1
		LEFT JOIN	Term_Version TV2	ON 	ISNULL(TV2.Version_Label, '') = ISNULL(TV1.Version_Label, '')
							AND	ISNULL(TV2.Author_And_Date, '') = ISNULL(TV1.Author_And_Date, '')
							AND	TV2.Term_Key <> TV1.Term_Key
		WHERE	TV2.Term_Version_Key IS NULL
		AND	TV1.Term_Key = @OldTermKey
	)

	-- And get rid of the rest.
	DELETE 	Term_Version
	WHERE 	Term_Key = @OldTermKey

	-- And finally remove the unnecessary leftover term.
	DELETE	Term
	WHERE	Term_Key = @OldTermKey

	SET NOCOUNT ON
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_DuplicateTerms_Merge') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_DuplicateTerms_Merge'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_DuplicateTerms_Merge TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_DuplicateTerms_Merge TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_DuplicateTerms_Merge TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_DuplicateTerms_Merge TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_DuplicateTerms_Merge TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_DuplicateTerms_Merge TO [Dev - JNCC SQL]
END
GO


IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_DuplicateSynonymTerms_Merge]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
	DROP PROCEDURE [dbo].[usp_DuplicateSynonymTerms_Merge]
GO

/*===========================================================================*\
  Description:	
	De-duplicates terms found in synonyms of given concept.

  Parameters:	
	@ConceptKey

  Created:	
	January 2006

  Last revision information:
    $Revision: 4 $
    $Date: 2/02/07 15:49 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_DuplicateSynonymTerms_Merge]
	@ConceptKey	CHAR(16)
AS
	SET NOCOUNT ON

	-- Temp table used a a work area to de-duplicate terms.
	DECLARE @T TABLE (
		PlainText	VARCHAR(50),
		ConceptKey	CHAR(16),
		TermKey		CHAR(16),
		LanguageKey	VARCHAR(4)
	)

	-- Get all synonyms for given concept into a temp table. Also return the TermKey.
	INSERT INTO @T
	SELECT DISTINCT	
		T.PlainText,
		S.Concept_Key,
		S.Term_Key,
		T.Language_Key
	FROM	VW_ConceptTerm C
	JOIN	Concept S	ON 	S.Meaning_Key  = C.Meaning_Key
	JOIN	Term T		ON 	T.Term_Key     = S.Term_Key
	WHERE	C.Concept_Key = @ConceptKey

	DECLARE	@NewTermKey CHAR(16), 
		@OldTermKey CHAR(16)

	-- Repeat until all duplicates handled.
	WHILE 1 = 1 BEGIN
		SELECT 	@NewTermKey = T1.TermKey, @OldTermKey = T2.TermKey
		FROM 	@T T1
		JOIN	@T T2 	ON 	T2.PlainText   =  T1.PlainText
				AND	T2.LanguageKey =  T1.LanguageKey
				AND	T2.TermKey     <> T1.TermKey

		-- If nothing selected, nothing more to do.
		IF @@ROWCOUNT = 0 BREAK

		-- Merge terms.
		EXECUTE	usp_DuplicateTerms_Merge @NewTermKey, @OldTermKey

		-- Update to change replaced key with kept key.
		UPDATE	@T SET TermKey = @NewTermKey WHERE TermKey = @OldTermKey
	END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_DuplicateSynonymTerms_Merge') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_DuplicateSynonymTerms_Merge'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_DuplicateSynonymTerms_Merge TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_DuplicateSynonymTerms_Merge TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_DuplicateSynonymTerms_Merge TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_DuplicateSynonymTerms_Merge TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_DuplicateSynonymTerms_Merge TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_DuplicateSynonymTerms_Merge TO [Dev - JNCC SQL]
END
GO


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
    $Revision: 4 $
    $Date: 2/02/07 15:49 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_DuplicateTerms_Select_ForMerge]
	@ConceptGroupKey	CHAR(16),
	@MaxRowCount		INT
AS
	SET NOCOUNT ON

	SET ROWCOUNT @MaxRowCount

	SELECT	DISTINCT
		T1.PlainText		AS Term,
		T1.Term_Key 		AS TermKey1, 
		T2.Term_Key 		AS TermKey2, 
		CG1.Item_Name 		AS Group1, 
		C1Pref.Concept_Key	AS PreferredKey1,
		C1Pref.PlainText 	AS Preferred1,
		CG2.Item_Name 		AS Group2, 
		C2Pref.Concept_Key	AS PreferredKey2,
		C2Pref.PlainText 	AS Preferred2,
		T1.Custodian,
		T1.Timestamp

	FROM	Term T1
	-- Find different term with same text
	JOIN	Term T2 		ON 	T2.PlainText = T1.PlainText
					AND	T2.Language_Key = T1.Language_Key
					AND	T2.Term_Key <> T1.Term_Key
	-- Find Concept using the term
	JOIN	Concept	C1		ON 	C1.Term_Key = T1.Term_Key
	-- Find synonym that is list preferred
	JOIN	VW_ConceptTerm C1Pref	ON 	C1Pref.Meaning_Key = C1.Meaning_Key
					AND	C1Pref.List_Preferred = 1
	-- Get group name
	JOIN	Concept_Group CG1	ON	CG1.Concept_Group_Key = C1Pref.Concept_Group_Key
	-- Find other concept using same term
	JOIN	Concept C2		ON 	C2.Term_Key = T2.Term_Key
	-- Find synonym that is list preferred
	JOIN	VW_ConceptTerm C2Pref	ON 	C2Pref.Meaning_Key = C2.Meaning_Key
					AND	C2Pref.List_Preferred = 1
	-- Get group name
	JOIN	Concept_Group CG2	ON 	CG2.Concept_Group_Key = C2Pref.Concept_Group_Key
	WHERE	(@ConceptGroupKey IS NULL
		OR
		C1.Concept_Group_Key = @ConceptGroupKey)
	-- Only want different concepts sharing term.
	AND	C1Pref.Concept_Key <> C2Pref.Concept_Key

	ORDER BY T1.PlainText, CG1.Item_Name, C1Pref.PlainText, C2Pref.PlainText

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
GO


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
    $Revision: 4 $
    $Date: 2/02/07 15:49 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_PotentialSynonyms_Select_ForMerge] 
	@ConceptGroupKey	CHAR(16),
	@MaxRowCount		INT
AS
	SET NOCOUNT ON

	SET ROWCOUNT @MaxRowCount

	SELECT 	DISTINCT 
		CSource.Concept_Key 	AS SourceConceptKey,
		CG.Item_Name 		AS SourceGroup,
		TSource.Item_Name 	AS SourceConcept,
		TSource.PlainText,				-- There for the ORDER BY
		CPotSyn.Concept_Key 	AS SynonymConceptKey,
		CGPotSyn.Item_Name 	AS SynonymGroup,
		TPotSyn.Item_Name 	AS SynonymConcept,
		TPotentials.Item_Name 	AS SharedTerm,
		CSource.Custodian,
		CSource.Timestamp

	FROM 		Concept AS CSource
	INNER JOIN 	Concept AS CSynonyms 		ON CSynonyms.Meaning_Key = CSource.Meaning_Key
	INNER JOIN	Term AS TSource			ON TSource.Term_Key = CSource.Term_Key
	INNER JOIN 	Term AS TSynonyms 		ON TSynonyms.Term_Key = CSynonyms.Term_Key
	INNER JOIN 	Term AS TPotentials 		ON TPotentials.Item_Name = TSynonyms.Item_Name
							AND TPotentials.Language_Key = TSynonyms.Language_Key
	INNER JOIN 	Concept AS CPotentials 		ON CPotentials.Term_Key = TPotentials.Term_Key
	INNER JOIN 	Concept AS CPotSyn		ON CPotSyn.Meaning_Key = CPotentials.Meaning_Key
	INNER JOIN	Term AS TPotSyn			ON TPotSyn.Term_Key = CPotSyn.Term_Key
	LEFT  JOIN 	Concept AS CExclude		ON CExclude.Concept_Key = CPotentials.Concept_Key
							AND CExclude.Meaning_Key = CSource.Meaning_Key
	INNER JOIN	Concept_Group AS CG		ON CG.Concept_Group_Key = CSource.Concept_Group_Key
	INNER JOIN	Concept_Group AS CGPotSyn	ON CGPotSyn.Concept_Group_Key = CPotSyn.Concept_Group_Key

	WHERE 		(@ConceptGroupKey IS NULL 
			OR 
			CSource.Concept_Group_Key = @ConceptGroupKey)
	AND 		CExclude.Concept_Key IS NULL
	AND 		CSource.List_Preferred = 1
	AND		CPotSyn.List_Preferred = 1

	ORDER BY 	TSource.PlainText
	
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
GO
