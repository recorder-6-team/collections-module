/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_CollectionUnitRelation_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_CollectionUnitRelation_Select]
GO

/*===========================================================================*\
  Description:	Returns data for the Collection Unit Relationship screen.

  Parameters:	@Key	Collection key

  Created:	October 2003

  Last revision information:
    $Revision: 6 $
    $Date: 6/05/04 14:26 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_CollectionUnitRelation_Select]
	@Key char(16),
	@ParentKey char(16)
AS

SET NOCOUNT ON

	SELECT TOP 1
	CASE CR.From_Collection_Unit_Key WHEN @ParentKey THEN -- Check which direction the relationship is in
		CR.To_Collection_Unit_Key
	ELSE
		CR.From_Collection_Unit_Key
	END AS Related_Unit_Key,
	CASE WHEN CRel.Collection_Unit_Key IS NULL THEN
		CASE WHEN SRel.Collection_Unit_Key IS NULL THEN
			CASE WHEN CTPreferred.PlainText IS NULL THEN 
				ITN.Preferred_Name
			ELSE	 
				+ CTPreferred.PlainText COLLATE SQL_Latin1_General_CP1_CI_AS
			END + 
			IsNull (' - ' + CN.Number, '')
		ELSE     
			IsNull (SRel.Item_Name + ' - ' + CN.Number, SRel.Item_Name)
		END		
	ELSE	
		IsNull (CRel.Item_Name + ' - ' + M.Number, CRel.Item_Name)
	END AS RelatedTo,
	CR.Thesaurus_Relation_Type_Key,
	CASE CR.From_Collection_Unit_Key WHEN @ParentKey THEN -- Check which direction the relationship is in
		TRT.Forward_Term
	ELSE 	
		TRT.Reverse_Term
	END AS RelationType,
	CR.Inferred_Type,
	CR.Vague_Date_Start,
	CR.Vague_Date_End,
	CR.Vague_Date_Type,
	CR.Author_Name_Key,
	dbo.ufn_GetFormattedName(CR.Author_Name_Key) AS AuthorName,
	CR.Comment,
	CR.Custodian,
	CR.[Timestamp]
	FROM Collection_Unit_Relation CR
	INNER JOIN Collection_Unit CURel ON (
			(CURel.Collection_Unit_Key=CR.To_Collection_Unit_Key AND CR.From_Collection_Unit_Key=@ParentKey) OR
			(CURel.Collection_Unit_Key=CR.From_Collection_Unit_Key AND CR.To_Collection_Unit_Key=@ParentKey))
	LEFT JOIN Collection CRel ON CRel.Collection_Unit_Key=CURel.Collection_Unit_Key
	LEFT JOIN (Movement_Collection_Unit MCU 
			INNER JOIN Movement_Direction MD ON MD.Movement_Direction_Key=MCU.Movement_Direction_Key
					AND MD.Outbound=0
			INNER JOIN Movement_Of_Ownership MO ON MO.Movement_Direction_Key = MD.Movement_Direction_Key
			INNER JOIN Movement_Of_Ownership_Exclusion MOE ON MOE.Movement_Of_Ownership_Key=MO.Movement_Of_Ownership_Key
				AND MOE.Movement_Of_Ownership_Exclusion_Key IS NULL
			INNER JOIN Movement M ON M.Movement_Key=MD.Movement_Key) ON MCU.Collection_Unit_Key=CRel.Collection_Unit_Key
	LEFT JOIN Store SRel ON SRel.Collection_Unit_Key=CURel.Collection_Unit_Key
	LEFT JOIN Specimen_Unit SURel ON SURel.Collection_Unit_Key=CURel.Collection_Unit_Key
	LEFT JOIN Determination D ON D.Determination_Key=SURel.Preferred_Determination_Key
	LEFT JOIN VW_ConceptTermPreferred CTPreferred ON CTPreferred.Concept_Key=D.Concept_Key
	LEFT JOIN Taxon_Determination TD ON TD.Taxon_Determination_Key=SURel.Preferred_Taxon_Determination_Key
	LEFT JOIN Index_Taxon_Name ITN ON ITN.Taxon_List_Item_Key=TD.Taxon_List_Item_Key
	LEFT JOIN Collection_Unit_Number CN ON CN.Collection_Unit_Key=CURel.Collection_Unit_Key
			AND CN.Preferred=1
	LEFT JOIN Concept C ON C.Concept_Key=CN.Type_Concept_Key
	 		AND C.Meaning_Key='SYSTEM0000000001'
	LEFT JOIN Thesaurus_Relation_Type TRT ON TRT.Thesaurus_Relation_Type_Key=CR.Thesaurus_Relation_Type_Key
	WHERE CR.Collection_Unit_Relation_Key=@Key
	AND MOE.Movement_Of_Ownership_Exclusion_Key IS NULL

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_CollectionUnitRelation_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_CollectionUnitRelation_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitRelation_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_CollectionUnitRelation_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_CollectionUnitRelation_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_CollectionUnitRelation_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_CollectionUnitRelation_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitRelation_Select TO [Dev - JNCC SQL]
END

GO