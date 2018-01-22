/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptToConceptRelations_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptToConceptRelations_Select]
GO

/*===========================================================================*\
  Description:	Returns the list of relationships of any type that exist 
			between a concept and a list of other concepts.  Both directions are scanned.

  Parameters:	@FromKey 		Concept_Key
							@ToKeys			Concept_Key\Concept_Key etc
							@DoAncestors - if 0, then inherited relationships not found.  If 1, 
							then only inherited relationships returned

  Created:	Dec 2003

  Last revision information:
    $Revision: 5 $
    $Date: 24/11/05 12:57 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptToConceptRelations_Select]
	@FromKey CHAR(16),
	@ToKeys VARCHAR(1600),  -- max 100 links scanned
	@IncludeInherited BIT

AS

SET NOCOUNT ON

DECLARE @CharPos INTEGER

DECLARE @FromConceptKeys TABLE (
  Concept_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS PRIMARY KEY,
	Ancestor BIT,   -- is this a concept in the lineage above the selected concept?
	Concept_Group_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS
)

DECLARE @ToConceptKeys TABLE (
  Concept_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS,
	Leaf_Concept_Key CHAR(16),
	Concept_Group_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS,
	Ancestor BIT,   -- is this a concept in the lineage above the selected concept?
	PRIMARY KEY (Concept_Key, Leaf_Concept_Key)
)

/*===========================================================================*\	
  Create temp tables to store the concept keys we are scanning from and to.
	This includes those in the hierarchy, because relationships can be 
	inherited
\*===========================================================================*/

--Find the current concept groups to aid when filtering lineage
DECLARE @FromConceptGroupKey char(16)
SELECT @FromConceptGroupKey=Concept_Group_Key FROM Concept WHERE Concept_Key=@FromKey

--Store the from concept key, we'll add the ancestors later
INSERT INTO @FromConceptKeys (Concept_Key, Ancestor, Concept_Group_Key) 
VALUES (@FromKey, 0, @FromConceptGroupKey)

-- and retrieve the To keys by parsing the \ separated list
SET @CharPos=1

WHILE @CharPos<LEN(@ToKeys)
BEGIN
  IF SUBSTRING(@ToKeys, @CharPos, 1)='\'
		INSERT INTO @ToConceptKeys (Concept_Key, Leaf_Concept_Key, Concept_Group_Key, Ancestor)
		  SELECT C.Concept_Key, C.Concept_Key, C.Concept_Group_Key, 0
			FROM Concept C
			LEFT JOIN @ToConceptKeys T ON T.Concept_Key=C.Concept_Key
			WHERE C.Concept_Key=SUBSTRING(@ToKeys, @CharPos-16, 16)
			AND T.Concept_Key IS NULL
  SET @CharPos=@CharPos+1
END

-- read the last item which has no \ after it
INSERT INTO @ToConceptKeys (Concept_Key, Leaf_Concept_Key, Concept_Group_Key, Ancestor)
  SELECT C.Concept_Key, C.Concept_Key, C.Concept_Group_Key, 0
	FROM Concept C
	LEFT JOIN @ToConceptKeys T ON T.Concept_Key=C.Concept_Key
	WHERE C.Concept_Key=RIGHT(@ToKeys, 16)
			AND T.Concept_Key IS NULL

/*===========================================================================*\	
	Retrieve the list of lineage concept keys that we need to look at for 
	inherited relationships, for both the From and the To ends.
	Note that Concept_Group_Key is included and any concept group is matched, 
	then the data is filtered afer.  This seems much faster than filtering
	out the concept group key at the start.
\*===========================================================================*/
IF @IncludeInherited=1 
BEGIN
  INSERT INTO @FromConceptKeys (Concept_Key, Ancestor, Concept_Group_Key) 
		SELECT DISTINCT CL2.Concept_Key, 1, C.Concept_Group_Key
		FROM @FromConceptKeys F
		INNER JOIN Concept_Lineage CL1 ON CL1.Concept_Key=F.Concept_Key
		INNER JOIN Concept_Lineage CL2 ON CL2.Lineage = LEFT(CL1.Lineage, LEN(CL2.Lineage))
		INNER JOIN Concept C ON C.Concept_Key=CL2.Concept_Key
		LEFT JOIN @FromConceptKeys F2 ON F2.Concept_Key=CL2.Concept_Key
		WHERE F2.Concept_Key IS NULL 

	INSERT INTO @ToConceptKeys (Concept_Key, Leaf_Concept_Key, Concept_Group_Key, Ancestor)
		SELECT DISTINCT C.Concept_Key, T.Concept_Key, C.Concept_Group_Key, 1
		FROM @ToConceptKeys T
		INNER JOIN Concept_Lineage CL1 ON CL1.Concept_Key=T.Concept_Key
		INNER JOIN Concept_Lineage CL2 ON CL2.Lineage = LEFT(CL1.Lineage, LEN(CL2.Lineage))
		INNER JOIN Concept C ON C.Concept_Key=CL2.Concept_Key
		LEFT JOIN @ToConceptKeys T2 ON T2.Concept_Key=CL2.Concept_Key
				AND T2.Leaf_Concept_Key=T.Concept_Key Collate SQL_Latin1_General_CP1_CI_AS
		WHERE T2.Concept_Key IS NULL 

		DELETE FROM @FromConceptKeys WHERE Concept_Group_Key<>@FromConceptGroupKey

END


SELECT DISTINCT
		'Concept' AS Type, 
		'Forward' AS Direction,
		T.Leaf_Concept_Key AS To_Concept_Key,
		TRT.Thesaurus_Relation_Type_Key, 
		TRT.Item_Name, 
		TRT.Forward_Term, 
		TRT.Reverse_Term
FROM Concept_Relation CR
INNER JOIN Thesaurus_Relation_Type TRT ON TRT.Thesaurus_Relation_Type_Key=CR.Thesaurus_Relation_Type_Key
INNER JOIN @FromConceptKeys F ON From_Concept_Key = F.Concept_Key
INNER JOIN @ToConceptKeys T ON To_Concept_Key=T.Concept_Key
WHERE @IncludeInherited=0 OR (CR.Inherited=1 AND (F.Ancestor=1 OR T.Ancestor=1))
UNION
SELECT 
		'Concept' AS Type, 
		'Reverse' AS Direction,
		T.Leaf_Concept_Key AS To_Concept_Key,
		TRT.Thesaurus_Relation_Type_Key, 
		TRT.Item_Name, 
		TRT.Forward_Term, 
		TRT.Reverse_Term
FROM Concept_Relation CR
INNER JOIN Thesaurus_Relation_Type TRT ON TRT.Thesaurus_Relation_Type_Key=CR.Thesaurus_Relation_Type_Key
INNER JOIN @FromConceptKeys F ON To_Concept_Key = F.Concept_Key
INNER JOIN @ToConceptKeys T ON From_Concept_Key=T.Concept_Key
WHERE @IncludeInherited=0 OR (CR.Inherited=1 AND (F.Ancestor=1 OR T.Ancestor=1))
UNION
SELECT 
		'Meaning' AS Type, 
		'Forward' AS Direction,
		T.Leaf_Concept_Key AS To_Concept_Key,
		TRT.Thesaurus_Relation_Type_Key, 
		TRT.Item_Name, 
		TRT.Forward_Term, 
		TRT.Reverse_Term
FROM Meaning_Relation MR
INNER JOIN Thesaurus_Relation_Type TRT ON TRT.Thesaurus_Relation_Type_Key=MR.Thesaurus_Relation_Type_Key
INNER JOIN Concept CFrom ON CFrom.Meaning_Key=MR.From_Meaning_Key
INNER JOIN Concept CTo ON CTo.Meaning_Key=MR.To_Meaning_Key
INNER JOIN @FromConceptKeys F ON CFrom.Concept_Key = F.Concept_Key
INNER JOIN @ToConceptKeys T ON CTo.Concept_Key=T.Concept_Key
WHERE @IncludeInherited=0 OR (MR.Inherited=1 AND (F.Ancestor=1 OR T.Ancestor=1))
UNION
SELECT 
		'Meaning' AS Type, 
		'Reverse' AS Direction,
		T.Leaf_Concept_Key AS To_Concept_Key,
		TRT.Thesaurus_Relation_Type_Key, 
		TRT.Item_Name, 
		TRT.Forward_Term, 
		TRT.Reverse_Term
FROM Meaning_Relation MR
INNER JOIN Thesaurus_Relation_Type TRT ON TRT.Thesaurus_Relation_Type_Key=MR.Thesaurus_Relation_Type_Key
INNER JOIN Concept CFrom ON CFrom.Meaning_Key=MR.From_Meaning_Key
INNER JOIN Concept CTo ON CTo.Meaning_Key=MR.To_Meaning_Key
INNER JOIN @FromConceptKeys F ON CTo.Concept_Key=F.Concept_Key
INNER JOIN @ToConceptKeys T ON CFrom.Concept_Key=T.Concept_Key
WHERE @IncludeInherited=0 OR (MR.Inherited=1 AND (F.Ancestor=1 OR T.Ancestor=1))
UNION
SELECT 
		'TermVersion' AS Type, 
		'Forward' AS Direction,
		T.Leaf_Concept_Key AS To_Concept_Key,
		TRT.Thesaurus_Relation_Type_Key, 
		TRT.Item_Name, 
		TRT.Forward_Term, 
		TRT.Reverse_Term
FROM Term_Version_Relation TVR
INNER JOIN Thesaurus_Relation_Type TRT ON TRT.Thesaurus_Relation_Type_Key=TVR.Thesaurus_Relation_Type_Key
INNER JOIN Concept CFrom ON CFrom.Term_Version_Key=TVR.From_Term_Version_Key
INNER JOIN Concept CTo ON CTo.Term_Version_Key=TVR.To_Term_Version_Key
INNER JOIN @ToConceptKeys T ON T.Concept_Key=CTo.Concept_Key
WHERE CFrom.Concept_Key=@FromKey
AND @IncludeInherited=0
UNION
SELECT 
		'TermVersion' AS Type, 
		'Reverse' AS Direction,
		T.Leaf_Concept_Key AS To_Concept_Key,
		TRT.Thesaurus_Relation_Type_Key, 
		TRT.Item_Name, 
		TRT.Forward_Term, 
		TRT.Reverse_Term
FROM Term_Version_Relation TVR
INNER JOIN Thesaurus_Relation_Type TRT ON TRT.Thesaurus_Relation_Type_Key=TVR.Thesaurus_Relation_Type_Key
INNER JOIN Concept CFrom ON CFrom.Term_Version_Key=TVR.From_Term_Version_Key
INNER JOIN Concept CTo ON CTo.Term_Version_Key=TVR.To_Term_Version_Key
INNER JOIN @ToConceptKeys T ON T.Concept_Key=CFrom.Concept_Key
WHERE CTo.Concept_Key=@FromKey
AND @IncludeInherited=0

SET NOCOUNT OFF

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptToConceptRelations_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptToConceptRelations_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptToConceptRelations_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptToConceptRelations_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptToConceptRelations_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ConceptToConceptRelations_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptToConceptRelations_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptToConceptRelations_Select TO [Dev - JNCC SQL]
END

GO

