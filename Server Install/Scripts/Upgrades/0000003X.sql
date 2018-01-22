SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Determinations_Select_ForSpecimen]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Determinations_Select_ForSpecimen]
GO

CREATE PROCEDURE [dbo].[usp_Determinations_Select_ForSpecimen] 
@ParentKey CHAR(16)

AS

--  DESCRIPTION
--  Returns Determinations (Non-Recorder) for a specified Specimen
--
--  PARAMETERS
--  NAME			DESCRIPTION
--	@ParentKey 		Only the records associated with the parent key are returned
--
--
--  AUTHOR:     		Ben Collier, Dorset Software
--  CREATED:    		2003-08-26
--
SET NOCOUNT ON
SELECT 
	D.Determination_Key AS Item_Key, 
	CT.Item_Name AS Item_Name, 
	CR.Concept_Rank_Key, 
	Color_R, 
	Color_G, 
	Color_B,
	CG.Item_Name AS Hint 
FROM
	DETERMINATION D 
	INNER JOIN VW_ConceptTerm CT ON D.Concept_Key = CT.Concept_Key
	INNER JOIN	Concept_Group CG ON	CT.Concept_Group_Key = CG.Concept_Group_Key
	LEFT JOIN Concept_Rank CR ON CT.Concept_Rank_Key = CR.Concept_Rank_Key
WHERE D.Specimen_Collection_Unit_Key=@ParentKey

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Determinations_Select_ForSpecimen') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Determinations_Select_ForSpecimen'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Determinations_Select_ForSpecimen TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Determinations_Select_ForSpecimen TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Determinations_Select_ForSpecimen TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Determinations_Select_ForSpecimen TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Determinations_Select_ForSpecimen TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Determinations_Select_ForSpecimen TO [Dev - JNCC SQL]
END

GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_DeterminationsRecorder_Select_ForSpecimen]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_DeterminationsRecorder_Select_ForSpecimen]
GO

CREATE PROCEDURE [dbo].[usp_DeterminationsRecorder_Select_ForSpecimen] 
@ParentKey CHAR(16),
@ShowCommonNames BIT

AS

--  DESCRIPTION
--  Returns Determinations (Recorder) for a specified Specimen
--
--  PARAMETERS
--  NAME				DESCRIPTION
--	@ParentKey 			Only the records associated with the parent key are returned
--	@ShowCommonNames	Whether or not Common Names should be shown
--
--
--  AUTHOR:     		Ben Collier, Dorset Software
--  CREATED:    		2003-08-26
--
SET NOCOUNT ON
SELECT 
	TD.Taxon_Determination_Key AS Item_Key, 
	dbo.ufn_GetFormattedTaxonNameByParams(ITN.Actual_Name, ITN.Actual_Name_Italic, ITN.Common_Name, 
			ITN.Common_Name_Italic, ITN.Authority, @ShowCommonNames) AS Item_Name,
	CR.Concept_Rank_Key, Color_R, Color_G, Color_B,
	TL.Item_Name AS Hint
FROM 
SPECIMEN_UNIT SU
	INNER JOIN 
		(TAXON_DETERMINATION TD
		INNER JOIN
			INDEX_TAXON_NAME ITN
		ON TD.Taxon_List_Item_Key = ITN.Taxon_List_Item_Key
		INNER JOIN
			TAXON_LIST_Item TLI
		ON TD.Taxon_List_Item_Key = TLI.Taxon_List_Item_Key
		INNER JOIN
			TAXON_LIST_VERSION TLV
		ON TLI.Taxon_List_Version_Key = TLV.Taxon_List_Version_Key
		LEFT JOIN
			TAXON_LIST TL
		ON TLV.Taxon_List_Key = TL.Taxon_List_Key
		LEFT JOIN
			Concept C
		ON TD.Nomenclatural_Status_Concept_Key = C.Concept_Key
		LEFT JOIN
			CONCEPT_RANK CR
		ON C.Concept_Rank_Key = CR.Concept_Rank_Key)
	ON SU.Collection_Unit_Key = TD.Specimen_Collection_Unit_Key AND SU.Collection_Unit_Key = @ParentKey
ORDER BY Item_Name

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_DeterminationsRecorder_Select_ForSpecimen') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_DeterminationsRecorder_Select_ForSpecimen'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_DeterminationsRecorder_Select_ForSpecimen TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_DeterminationsRecorder_Select_ForSpecimen TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_DeterminationsRecorder_Select_ForSpecimen TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_DeterminationsRecorder_Select_ForSpecimen TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_DeterminationsRecorder_Select_ForSpecimen TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_DeterminationsRecorder_Select_ForSpecimen TO [Dev - JNCC SQL]
END

GO

