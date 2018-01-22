/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptRelations_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptRelations_Select]
GO

/*===========================================================================*\
  Description:	Retrieves a list of all the related concepts for a concept.
	              Includes concept and meaning relations, synonyms and term versions.

  Parameters:	@ConceptKey	
 	            @IncludeSynonyms 

  Created:	September 2003

  Last revision information:
    $Revision: 9 $
    $Date: 3/08/11 14:55 $
    $Author: Simonlewis $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptRelations_Select]
	@ConceptKey char(16),
  @IncludeSynonyms bit
AS

SET NOCOUNT ON

--Create a temp table to store the output
DECLARE @Output TABLE (
  Relationship_Name varchar(100),
  Concept_Key char(16),
  Plaintext nvarchar(150),
  Item_Name nvarchar(150),
  Author_And_Date varchar(100),
  Concept_Group_Name varchar(100),
  Details text
)   

--Create a temp table to store the concept keys we are scanning
DECLARE @ConceptKeys TABLE (
  Concept_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS PRIMARY KEY,
	Ancestor BIT   -- is this a concept in the lineage above the selected concept?
)

--Find the current concept group.  The output only lists the concept group if different to the current item
DECLARE @ConceptGroupKey char(16)
SELECT @ConceptGroupKey=Concept_Group_Key FROM Concept WHERE Concept_Key=@ConceptKey

--Store the appropriate concept key
INSERT INTO @ConceptKeys (Concept_Key, Ancestor) 
VALUES (@ConceptKey, 0)

/*===========================================================================*\	
	Retrieve the list of lineage concept keys that we need to look at for 
	inherited relationships
\*===========================================================================*/
	DECLARE @LineageID integer
	DECLARE @Lineage varchar(2000)
	DECLARE @CharPos integer

	--First find all the parent lineages
  DECLARE Lineages_Cursor CURSOR LOCAL FORWARD_ONLY FOR
    SELECT Lineage_ID, Lineage FROM Concept_Lineage WHERE Concept_Key=@ConceptKey

	OPEN Lineages_Cursor

  FETCH NEXT FROM Lineages_Cursor INTO @LineageID, @Lineage
  
  WHILE @@FETCH_STATUS=0
	BEGIN
		
		SET @CharPos=1
		
		--Find each ancestor, start at top of tree and work down
		WHILE @CharPos<LEN(@Lineage)
		BEGIN
		  IF SUBSTRING(@Lineage, @CharPos, 1)='\'
			  INSERT INTO @ConceptKeys (Concept_Key, Ancestor)
			    SELECT DISTINCT C.Concept_Key, 1
					FROM Concept C
		      INNER JOIN Concept_Lineage CL ON CL.Concept_Key=C.Concept_Key
					LEFT JOIN @ConceptKeys CK ON CK.Concept_Key=C.Concept_Key
				  WHERE C.Concept_Group_Key=@ConceptGroupKey
			    AND CL.Lineage=Left(@Lineage, @CharPos-1)
					AND CK.Concept_Key IS NULL
		  SET @CharPos=@CharPos+1
		END

    FETCH NEXT FROM Lineages_Cursor INTO @LineageID, @Lineage

	END

	CLOSE Lineages_Cursor
	DEALLOCATE Lineages_Cursor

--Now get the synonyms we need to scan for
IF @IncludeSynonyms=1
  INSERT INTO @ConceptKeys (Concept_Key, Ancestor)
      SELECT C2.Concept_Key, CK1.Ancestor
      FROM @ConceptKeys CK1
			INNER JOIN Concept C1 ON C1.Concept_Key=CK1.Concept_Key
      INNER JOIN Concept C2 ON C2.Meaning_Key=C1.Meaning_Key
			-- Join to ensure no duplicates
			LEFT JOIN @ConceptKeys CK2 ON CK2.Concept_Key=C2.Concept_Key
			WHERE CK2.Concept_Key IS NULL

--Insert forward terms for concept relations.  The odd nesting of queries
--allows distinct to be used before the text field (comment) is obtained)
--Note that concept relations cannot be inherited.
INSERT INTO @Output (Relationship_Name, Concept_Key, Plaintext, 
      Item_Name, Author_And_Date, Concept_Group_Name, Details)
	SELECT 
		Data.Forward_Term, 
		Data.Concept_Key, 
		Data.Plaintext, 
		Data.Item_Name, 
		Data.Author_Copy, 
		Data.Concept_Group_Name,
		CR.Comment
	FROM (
		SELECT DISTINCT CR.Concept_Relation_Key, TRT.Forward_Term, C.Concept_Key, T.Plaintext, 
	      C.Published_Term AS	Item_Name, C.Author_Copy, 
	      CASE WHEN CG.Concept_Group_Key=@ConceptGroupKey THEN NULL ELSE CG.Item_Name END AS Concept_Group_Name
		FROM Concept_Relation CR
	    	INNER JOIN Concept C ON C.Concept_Key=CR.To_Concept_Key
	    	INNER JOIN Term T ON T.Term_Key=C.Term_Key
				INNER JOIN Thesaurus_Relation_Type TRT ON TRT.Thesaurus_Relation_Type_Key=CR.Thesaurus_Relation_Type_Key
			  INNER JOIN Concept_Group CG ON CG.Concept_Group_Key=C.Concept_Group_Key
				INNER JOIN @ConceptKeys CK ON CK.Concept_Key=CR.From_Concept_Key
						AND (CK.Ancestor=0 OR CR.Inherited=1)
	) Data INNER JOIN Concept_Relation CR ON CR.Concept_Relation_Key=Data.Concept_Relation_Key
  ORDER BY PlainText, Author_Copy


--Insert reverse terms for concept relations
INSERT INTO @Output (Relationship_Name, Concept_Key, Plaintext, 
      Item_Name, Author_And_Date, Concept_Group_Name, Details)
	SELECT 
		Data.Reverse_Term, 
		Data.Concept_Key, 
		Data.Plaintext, 
		Data.Item_Name, 
		Data.Author_Copy, 
		Data.Concept_Group_Name,
		CR.Comment
	FROM (
		SELECT DISTINCT CR.Concept_Relation_Key, TRT.Reverse_Term, C.Concept_Key, T.Plaintext, 
	      C.Published_Term	AS	Item_Name, C.Author_Copy, 
	      CASE WHEN CG.Concept_Group_Key=@ConceptGroupKey THEN NULL ELSE CG.Item_Name END AS Concept_Group_Name
		FROM Concept_Relation CR
	    	INNER JOIN Concept C ON C.Concept_Key=CR.From_Concept_Key
	    	INNER JOIN Term T ON T.Term_Key=C.Term_Key
				INNER JOIN Thesaurus_Relation_Type TRT ON TRT.Thesaurus_Relation_Type_Key=CR.Thesaurus_Relation_Type_Key
			  INNER JOIN Concept_Group CG ON CG.Concept_Group_Key=C.Concept_Group_Key
				INNER JOIN @ConceptKeys CK ON CK.Concept_Key=CR.To_Concept_Key
						AND (CK.Ancestor=0 OR CR.Inherited=1)
	) Data INNER JOIN Concept_Relation CR ON CR.Concept_Relation_Key=Data.Concept_Relation_Key
  ORDER BY PlainText, Author_Copy


--Insert forward terms for meaning relations
INSERT INTO @Output (Relationship_Name, Concept_Key, Plaintext, 
      Item_Name, Author_And_Date, Concept_Group_Name, Details)
	SELECT 
		Data.Forward_Term, 
		Data.Concept_Key, 
		Data.Plaintext, 
		Data.Item_Name, 
		Data.Author_Copy, 
		Data.Concept_Group_Name,
		MR.Comment
	FROM (
		SELECT DISTINCT MR.Meaning_Relation_Key, TRT.Forward_Term, C2.Concept_Key, T.Plaintext, 
	      C2.Published_Term	AS	Item_Name, C2.Author_Copy, 
	      CASE WHEN CG.Concept_Group_Key=@ConceptGroupKey THEN NULL ELSE CG.Item_Name END AS Concept_Group_Name
		FROM Concept C1
	      INNER JOIN Meaning_Relation MR ON MR.From_Meaning_Key=C1.Meaning_Key
				INNER JOIN Concept C2 ON C2.Meaning_Key=MR.To_Meaning_Key
					AND C2.List_Preferred=1
	    	INNER JOIN Term T ON T.Term_Key=C2.Term_Key
				INNER JOIN Thesaurus_Relation_Type TRT ON TRT.Thesaurus_Relation_Type_Key=MR.Thesaurus_Relation_Type_Key
			  INNER JOIN Concept_Group CG ON CG.Concept_Group_Key=C2.Concept_Group_Key
				INNER JOIN @ConceptKeys CK ON CK.Concept_Key=C1.Concept_Key
						AND (CK.Ancestor=0 OR MR.Inherited=1)
	) Data INNER JOIN Meaning_Relation MR ON MR.Meaning_Relation_Key=Data.Meaning_Relation_Key
  ORDER BY Plaintext, Author_Copy


--Insert reverse terms for meaning relations
INSERT INTO @Output (Relationship_Name, Concept_Key, Plaintext, 
      Item_Name, Author_And_Date, Concept_Group_Name, Details)
	SELECT 
		Data.Reverse_Term, 
		Data.Concept_Key, 
		Data.Plaintext, 
		Data.Item_Name, 
		Data.Author_Copy, 
		Data.Concept_Group_Name,
		MR.Comment
	FROM (
		SELECT DISTINCT MR.Meaning_Relation_Key, TRT.Reverse_Term, C2.Concept_Key, T.Plaintext, 
	      C2.Published_Term	AS	Item_Name, C2.Author_Copy, 
	      CASE WHEN CG.Concept_Group_Key=@ConceptGroupKey THEN NULL ELSE CG.Item_Name END AS Concept_Group_Name
		FROM Concept C1
	      INNER JOIN Meaning_Relation MR ON MR.To_Meaning_Key=C1.Meaning_Key
				INNER JOIN Concept C2 ON C2.Meaning_Key=MR.From_Meaning_Key
					AND C2.List_Preferred=1
	    	INNER JOIN Term T ON T.Term_Key=C2.Term_Key
				INNER JOIN Thesaurus_Relation_Type TRT ON TRT.Thesaurus_Relation_Type_Key=MR.Thesaurus_Relation_Type_Key
			  INNER JOIN Concept_Group CG ON CG.Concept_Group_Key=C2.Concept_Group_Key
				INNER JOIN @ConceptKeys CK ON CK.Concept_Key=C1.Concept_Key
						AND (CK.Ancestor=0 OR MR.Inherited=1)
	) Data INNER JOIN Meaning_Relation MR ON MR.Meaning_Relation_Key=Data.Meaning_Relation_Key
  ORDER BY Plaintext, Author_Copy


--Insert all known synonyms
INSERT INTO @Output (Relationship_Name, Concept_Key, Plaintext, 
      Item_Name, Author_And_Date, Concept_Group_Name)
	SELECT '#Synonyms', C2.Concept_Key, T.Plaintext, C2.Published_Term, C2.Author_Copy, 
      CASE WHEN CG.Concept_Group_Key=@ConceptGroupKey THEN NULL ELSE CG.Item_Name END 
	FROM Concept C1
	    INNER JOIN Concept C2 ON C2.Meaning_Key=C1.Meaning_Key
			INNER JOIN Term T ON T.Term_Key=C2.Term_Key
			INNER JOIN Concept_Group CG on CG.Concept_Group_Key=C2.Concept_Group_Key
  WHERE C1.Concept_Key=@ConceptKey
  ORDER BY T.Plaintext, C2.Author_Copy


--Table to hold all term versions
DECLARE @TermVersions TABLE (
		Concept_Key char(16) COLLATE SQL_Latin1_General_CP1_CI_AS PRIMARY KEY,
		Term_Version_Key char(16) COLLATE SQL_Latin1_General_CP1_CI_AS,
		Sort_Order INT
)

--Insert the selected concept into the version list
INSERT INTO @TermVersions(Concept_Key, Term_Version_Key, Sort_Order) 
		SELECT C.Concept_Key, C.Term_Version_Key, C.Sort_Code
		FROM Concept C
				INNER JOIN Term_Version TV ON TV.Term_Version_Key=C.Term_Version_Key
		WHERE C.Concept_Key=@ConceptKey

--Recursively find all the versions, first get the nearest set of versions
INSERT INTO @TermVersions(Concept_Key, Term_Version_Key, Sort_Order)
		SELECT TVR.From_Concept_Key, TVR.From_Term_Version_Key, TV.Sort_Order-1
		FROM @TermVersions TV
				INNER JOIN Term_Version_Relation TVR ON TVR.To_Term_Version_Key=TV.Term_Version_Key
				INNER JOIN Thesaurus_Relation_Type TRT ON TRT.Thesaurus_Relation_Type_Key=TVR.Thesaurus_Relation_Type_Key
				INNER JOIN Semantic_Relation SR ON SR.Semantic_Relation_Key=TRT.Semantic_Relation_Key
						AND SR.Reverse_Equivalence_Possible=1
 				--Join to exclude items already in the list
				LEFT JOIN @TermVersions TVExist ON TVExist.Term_Version_Key=TVR.From_Term_Version_Key
		WHERE TVExist.Concept_Key IS NULL
		UNION
		SELECT TVR.To_Concept_Key, TVR.To_Term_Version_Key, TV.Sort_Order+1
		FROM @TermVersions TV
				INNER JOIN Term_Version_Relation TVR ON TVR.From_Term_Version_Key=TV.Term_Version_Key
				INNER JOIN Thesaurus_Relation_Type TRT ON TRT.Thesaurus_Relation_Type_Key=TVR.Thesaurus_Relation_Type_Key
				INNER JOIN Semantic_Relation SR ON SR.Semantic_Relation_Key=TRT.Semantic_Relation_Key
						AND SR.Forward_Equivalence_Possible=1
 				--Join to exclude items already in the list
				LEFT JOIN @TermVersions TVExist ON TVExist.Term_Version_Key=TVR.To_Term_Version_Key
		WHERE TVExist.Concept_Key IS NULL

--Now recurse to find all other versions
WHILE @@ROWCOUNT<>0
	INSERT INTO @TermVersions(Concept_Key, Term_Version_Key, Sort_Order)
			SELECT TVR.From_Concept_Key, TVR.From_Term_Version_Key, TV.Sort_Order-1
			FROM @TermVersions TV
					INNER JOIN Term_Version_Relation TVR ON TVR.To_Term_Version_Key=TV.Term_Version_Key
					INNER JOIN Thesaurus_Relation_Type TRT ON TRT.Thesaurus_Relation_Type_Key=TVR.Thesaurus_Relation_Type_Key
					INNER JOIN Semantic_Relation SR ON SR.Semantic_Relation_Key=TRT.Semantic_Relation_Key
							AND SR.Reverse_Equivalence_Possible=1
	 				--Join to exclude items already in the list
					LEFT JOIN @TermVersions TVExist ON TVExist.Term_Version_Key=TVR.From_Term_Version_Key
			WHERE TVExist.Concept_Key IS NULL
			UNION
			SELECT TVR.To_Concept_Key, TVR.To_Term_Version_Key, TV.Sort_Order+1
			FROM @TermVersions TV
					INNER JOIN Term_Version_Relation TVR ON TVR.From_Term_Version_Key=TV.Term_Version_Key
					INNER JOIN Thesaurus_Relation_Type TRT ON TRT.Thesaurus_Relation_Type_Key=TVR.Thesaurus_Relation_Type_Key
					INNER JOIN Semantic_Relation SR ON SR.Semantic_Relation_Key=TRT.Semantic_Relation_Key
							AND SR.Forward_Equivalence_Possible=1
	 				--Join to exclude items already in the list
					LEFT JOIN @TermVersions TVExist ON TVExist.Term_Version_Key=TVR.To_Term_Version_Key
			WHERE TVExist.Concept_Key IS NULL

	
INSERT INTO @Output (Relationship_Name, Concept_Key, Plaintext, 
    Item_Name, Author_And_Date, Concept_Group_Name)
SELECT '#TermVersions', 
		C.Concept_Key,
		T.Plaintext,
		C.Published_Term,
		CASE WHEN TV.Version_Label IS NULL THEN 
			C.Author_Copy
		ELSE
			C.Author_Copy + ' (' + TV.Version_Label + ')'
		END,
    CASE WHEN CG.Concept_Group_Key=@ConceptGroupKey THEN NULL ELSE CG.Item_Name END 
FROM Concept C
		INNER JOIN Term T ON T.Term_Key=C.Term_Key
		INNER JOIN Concept_Group CG on CG.Concept_Group_Key=C.Concept_Group_Key
		INNER JOIN Term_Version TV on TV.Term_Version_Key=C.Term_Version_Key
		INNER JOIN @TermVersions TVLIST ON TVLIST.Concept_Key=C.Concept_Key
ORDER BY TVLIST.Sort_Order ASC


--Select the output
SELECT * FROM @Output

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptRelations_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptRelations_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptRelations_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptRelations_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptRelations_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ConceptRelations_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptRelations_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptRelations_Select TO [Dev - JNCC SQL]
END

GO