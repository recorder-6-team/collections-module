/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
       FROM   SysObjects 
       WHERE  Id = Object_Id(N'[dbo].[usp_ConceptAncestors_Select]')
       AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptAncestors_Select]
GO

/*===========================================================================*\
  Description: Returns the ancestor hierarchy for a concept

  Parameters:	@ConceptKey - conecpt to find ancestors for

  Created:	August 2003
\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptAncestors_Select]
    @ConceptKey varchar(100),
    @LineageID integer,
    @HierarchyRelationTypeKey char(16)
AS
    DECLARE @Lineage VARCHAR(2000)
    DECLARE @CharPos INTEGER
    DECLARE @ConceptGroupKey CHAR(16)

    SET NOCOUNT ON

    --select the lineage and concept group
    SELECT @Lineage = CL.Lineage, @ConceptGroupKey = C.Concept_Group_Key
    FROM Concept_Lineage CL
    INNER JOIN Concept C on C.Concept_Key=CL.Concept_Key
    WHERE CL.Concept_Key=@ConceptKey
    AND CL.Lineage_ID=@LineageID

    --Create an output table
    CREATE TABLE #Output (
      Concept_Key CHAR(16),
      Item_Name NVARCHAR(500),
      HasChildren BIT,
      Concept_Rank_Key CHAR(16)
    )

    SET @CharPos=1

    --Find each ancestor, start at top of tree and work down
    WHILE @CharPos<LEN(@Lineage)
    BEGIN
      IF SUBSTRING(@Lineage, @CharPos, 1)='\'
          INSERT INTO #Output
            SELECT DISTINCT C.Concept_Key, C.Published_Term	AS	Item_Name,
            CASE WHEN CR.Concept_Relation_Key IS NULL THEN 0 ELSE 1 END,
                    C.Concept_Rank_Key
                FROM Concept C
                INNER JOIN Concept_Lineage CL ON CL.Concept_Key=C.Concept_Key
                LEFT JOIN Concept_Relation CR ON CR.From_Concept_Key=C.Concept_Key
              WHERE C.Concept_Group_Key=@ConceptGroupKey
            AND CL.Lineage=Left(@Lineage, @CharPos-1)
            AND CR.Thesaurus_Relation_Type_Key=@HierarchyRelationTypeKey
      SET @CharPos=@CharPos+1
    END

    SELECT * FROM #Output

    DROP TABLE #Output
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptAncestors_Select') AND SysStat & 0xf = 4)
BEGIN
        PRINT 'Setting up security on procedure usp_ConceptAncestors_Select'
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
            GRANT EXECUTE ON dbo.usp_ConceptAncestors_Select TO [R2k_AddOnly]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
        GRANT EXECUTE ON dbo.usp_ConceptAncestors_Select TO [R2k_Administrator]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
        GRANT EXECUTE ON dbo.usp_ConceptAncestors_Select TO [R2k_FullEdit]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
        GRANT EXECUTE ON dbo.usp_ConceptAncestors_Select TO [R2k_ReadOnly]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
        GRANT EXECUTE ON dbo.usp_ConceptAncestors_Select TO [R2k_RecordCardsOnly]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
            GRANT EXECUTE ON dbo.usp_ConceptAncestors_Select TO [Dev - JNCC SQL]
END

GO
