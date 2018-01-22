/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_TermHTMLDetails_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_TermHTMLDetails_Select]
GO



/*===========================================================================*\
  Description:	Returns multiple recordsets suitable for populating the HTML
			details of a concept, excluding the name.  
			The following recordsets are returned:
				Designations
				Facts
				Sources
				Hyperlinks

  Parameters:	@Key	Collection key

  Created:	Dec 2003

  Last revision information:
    $Revision: 3 $
    $Date: 6/05/04 14:26 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_TermHTMLDetails_Select]
	@ConceptKey char(16)
AS

SET NOCOUNT ON

/*===========================================================================*\	
	Recordset for each designation
\*===========================================================================*/
	SELECT 
		CT.Item_Name, 
  	dbo.ufn_GetDateFromVagueDate(
					CD.From_Vague_Date_Start, 
					CD.From_Vague_Date_End, 
					CD.From_Vague_Date_Type) AS DateFrom,
		dbo.ufn_GetDateFromVagueDate(
					CD.To_Vague_Date_Start, 
					CD.To_Vague_Date_End, 
					CD.To_Vague_Date_Type) AS DateTo,
		MGeo.[Text] AS Geographic_Context,
		MC.[Text] AS Constraints
	FROM Concept_Designation CD
	INNER JOIN VW_ConceptTerm CT ON CT.Concept_Key=CD.Designation_Type_Concept_Key
	LEFT JOIN Metadata MGeo ON MGeo.Record_Key=CD.Concept_Designation_Key AND MGeo.Metadata_Type_Key='SYSTEM0000000001'
	LEFT JOIN Metadata MC ON MC.Record_Key=CD.Concept_Designation_Key AND MC.Metadata_Type_Key='SYSTEM0000000002'
	WHERE CD.Concept_Key=@ConceptKey
	

/*===========================================================================*\	
	Temporary table to hold the list of term version keys that we might need
	facts for.  These are all the related terms versions which at least have 
	some overlap with the current term version.
\*===========================================================================*/
	CREATE TABLE #TermVersionKeys (Term_Version_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS)

	INSERT INTO #TermVersionKeys (Term_Version_Key)
		SELECT Term_Version_Key FROM Concept WHERE Concept_Key=@ConceptKey
		
	WHILE @@RowCount>0
		INSERT INTO #TermVersionKeys (Term_Version_Key) 
		SELECT TVR.To_Term_Version_Key 
		FROM Term_Version_Relation TVR
		INNER JOIN #TermVersionKeys TVK ON TVK.Term_Version_Key=TVR.From_Term_Version_Key
		INNER JOIN Thesaurus_Relation_Type TRT on TRT.Thesaurus_Relation_Type_Key=TVR.Thesaurus_Relation_Type_Key
		INNER JOIN Semantic_Relation SR ON SR.Semantic_Relation_Key=TRT.Semantic_Relation_Key
				AND SR.Forward_Equivalence_Possible=1
		LEFT JOIN #TermVersionKeys TVK2 ON TVK2.Term_Version_Key=TVR.To_Term_Version_Key
		WHERE TVK2.Term_Version_Key IS NULL	

	WHILE @@RowCount>0
		INSERT INTO #TermVersionKeys (Term_Version_Key) 
		SELECT TVR.From_Term_Version_Key 
		FROM Term_Version_Relation TVR
		INNER JOIN #TermVersionKeys TVK ON TVK.Term_Version_Key=TVR.To_Term_Version_Key
		INNER JOIN Thesaurus_Relation_Type TRT on TRT.Thesaurus_Relation_Type_Key=TVR.Thesaurus_Relation_Type_Key
		INNER JOIN Semantic_Relation SR ON SR.Semantic_Relation_Key=TRT.Semantic_Relation_Key
				AND SR.Reverse_Equivalence_Possible=1
		LEFT JOIN #TermVersionKeys TVK2 ON TVK2.Term_Version_Key=TVR.From_Term_Version_Key
		WHERE TVK2.Term_Version_Key IS NULL


/*===========================================================================*\	
	Temporary table to hold the list of lineage concept keys that we need to 
	look at for inherited facts
\*===========================================================================*/
	CREATE TABLE #InheritedConcepts (Concept_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS)

	DECLARE @LineageID integer
	DECLARE @Lineage varchar(2000)
	DECLARE @CharPos integer
	DECLARE @ConceptGroupKey char(16)

	--First find all the parent lineages
  DECLARE Lineages_Cursor CURSOR LOCAL FORWARD_ONLY FOR
    SELECT Lineage_ID FROM Concept_Lineage WHERE Concept_Key=@ConceptKey

	OPEN Lineages_Cursor

  FETCH NEXT FROM Lineages_Cursor INTO @LineageID
  
  WHILE @@FETCH_STATUS=0
	BEGIN

	  --select the lineage and concept group
		SELECT @Lineage = CL.Lineage, @ConceptGroupKey = C.Concept_Group_Key
		FROM Concept_Lineage CL
		INNER JOIN Concept C on C.Concept_Key=CL.Concept_Key
		WHERE CL.Concept_Key=@ConceptKey
		AND CL.Lineage_ID=@LineageID
		
		SET @CharPos=1
		
		--Find each ancestor, start at top of tree and work down
		WHILE @CharPos<LEN(@Lineage)
		BEGIN
		  IF SUBSTRING(@Lineage, @CharPos, 1)='\'
			  INSERT INTO #InheritedConcepts
			    SELECT DISTINCT C.Concept_Key
					FROM Concept C
		      INNER JOIN Concept_Lineage CL ON CL.Concept_Key=C.Concept_Key
				  WHERE C.Concept_Group_Key=@ConceptGroupKey
			    AND CL.Lineage=Left(@Lineage, @CharPos-1)
		  SET @CharPos=@CharPos+1
		END

		FETCH NEXT FROM Lineages_Cursor INTO @LineageID

	END

	CLOSE Lineages_Cursor
	DEALLOCATE Lineages_Cursor

/*===========================================================================*\	
	Temporary table to hold the list of fact keys that are relevant
\*===========================================================================*/
	CREATE TABLE #Fact (Thesaurus_Fact_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS)
	
	INSERT INTO #Fact
			--Concept facts
			SELECT DISTINCT TF.Thesaurus_Fact_Key
			FROM Thesaurus_Fact TF
			WHERE TF.Concept_Key=@ConceptKey
			UNION
			--Meaning facts
			SELECT TF.Thesaurus_Fact_Key
			FROM Thesaurus_Fact TF
			INNER JOIN Concept C ON C.Meaning_Key=TF.Meaning_Key
			WHERE C.Concept_Key=@ConceptKey
			UNION
			--Term version facts
			SELECT TF.Thesaurus_Fact_Key
			FROM Thesaurus_Fact TF
			INNER JOIN Term_Version TV on TV.Term_Version_Key=TF.Term_Version_Key
			INNER JOIN Concept C ON C.Term_Version_Key=TV.Term_Version_Key
			WHERE C.Concept_Key=@ConceptKey
			UNION
			--Facts for related term versions
			SELECT TF.Thesaurus_Fact_Key
			FROM Thesaurus_Fact TF
			INNER JOIN #TermVersionKeys TVK ON TVK.Term_Version_Key=TF.Term_Version_Key
			WHERE TF.Related_Term_Versions=1
			--Inherited concept facts
			UNION
			SELECT DISTINCT TF.Thesaurus_Fact_Key
			FROM Thesaurus_Fact TF
			INNER JOIN #InheritedConcepts IC ON IC.Concept_Key=TF.Concept_Key
			WHERE TF.Inherited=1
			UNION
			--Inherited meaning facts
			SELECT DISTINCT TF.Thesaurus_Fact_Key
			FROM Thesaurus_Fact TF
			INNER JOIN Concept C ON C.Meaning_Key=TF.Meaning_Key
			INNER JOIN #InheritedConcepts IC ON IC.Concept_Key=C.Concept_Key			
			WHERE TF.Inherited=1

/*===========================================================================*\	
	Recordset for each fact, using a subquery to ensure a distinct list
\*===========================================================================*/
	SELECT Item_Name, Data  
	FROM Thesaurus_Fact TF
	INNER JOIN #Fact ON #Fact.Thesaurus_Fact_Key=TF.Thesaurus_Fact_Key

/*===========================================================================*\	
	Recordset for each source
\*===========================================================================*/
	SELECT 
		R.Source_Key,
		RA.Author + ' - ' +
				dbo.ufn_GetDateFromVagueDate(
						R.Year_Vague_Date_Start, 
						Year_Vague_Date_End, 
						Year_Vague_Date_Type) AS AuthorAndDate,
		CASE WHEN R.TITLE IS NULL THEN
			R.FULL_REFERENCE
		ELSE
			R.Title
		END AS SourceTitle
	FROM Reference R
	INNER JOIN VW_REFERENCE_AUTHORS RA ON RA.Source_Key=R.Source_Key
	INNER JOIN Source_Join SJ ON SJ.Source_Key=R.Source_Key
	INNER JOIN Concept C ON C.Concept_Key=@ConceptKey
	LEFT JOIN Term_Version TV ON TV.Term_Version_Key=C.Term_Version_Key
	LEFT JOIN Concept_Designation CD ON CD.Concept_Key=C.Concept_Key
	WHERE (SJ.Table_Name='Concept' AND SJ.Record_Key=@ConceptKey)
		OR (SJ.Table_Name='Term' AND SJ.Record_Key=C.Term_Key)
		OR (SJ.Table_Name='Term_Version' AND SJ.Record_Key=TV.Term_Version_Key)
		OR (SJ.Table_Name='Concept_Designation' AND SJ.Record_Key=CD.Concept_Designation_Key)
		OR (SJ.Table_Name='Thesaurus_Fact' AND SJ.Record_Key IN (SELECT Thesaurus_Fact_Key FROM #Fact))

	
/*===========================================================================*\	
	Recordset for each web link
\*===========================================================================*/
	SELECT 
		DH.Item_Name, 
		DH.Image_File, 
		CASE DH.Use_Concept_Key
			WHEN 1 THEN DH.URL + C.Concept_Key
			WHEN 0 THEN DH.URL + REPLACE(CT.Plaintext COLLATE SQL_Latin1_General_CP1_CI_AS, ' ', DH.Word_Separator)
		END AS Hyperlink
	FROM Domain_Hyperlink DH
	INNER JOIN Concept_Group CG ON CG.Local_Domain_Key=DH.Local_Domain_Key
	INNER JOIN Concept C ON C.Concept_Group_Key=CG.Concept_Group_Key
	INNER JOIN VW_ConceptTerm CT ON CT.Concept_Key=C.Concept_Key
	WHERE C.Concept_Key=@ConceptKey

	--Cleanup the temporary table
	DROP TABLE #Fact
	DROP TABLE #TermVersionKeys
	
	SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TermHTMLDetails_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_TermHTMLDetails_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_TermHTMLDetails_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_TermHTMLDetails_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_TermHTMLDetails_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_TermHTMLDetails_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_TermHTMLDetails_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_TermHTMLDetails_Select TO [Dev - JNCC SQL]
END

GO