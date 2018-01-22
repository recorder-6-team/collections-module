/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_SpecimensCollected_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_SpecimensCollected_Select]
GO

/*===========================================================================*\
  Description:	Returns the list of all preferred determinations and the
		preferred number for the specimens linked to the currently
		selected collection. Returns the list of all determinations for 
		specimen top level node.

  Parameters:	@Key			Key of the event recorder person
		@CollectionUnitKey	Specimen or Collection Unit key
		@KeyIsSpecimen		Bit saying whether the key is a specimen

  Created:	October 2003

  Last revision information:
    $Revision: 9 $
    $Date: 13/03/13 8:38 $
    $Author: Alexanderpadley $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SpecimensCollected_Select]
	@Key char(16),
	@CollectionUnitKey char(16),
	@UserDomainMask int,
	@SessionID char(16),
	@ShowCommonNames bit,
	@KeyIsSpecimen bit
AS

SET NOCOUNT ON

IF @KeyIsSpecimen = 1
	SELECT DISTINCT
			SU.Collection_Unit_Key AS Item_Key,
			CASE WHEN SU.Life_Sciences = 0
			THEN CTP.Item_Name COLLATE SQL_Latin1_General_CP1_CI_AS
			ELSE dbo.ufn_GetFormattedTaxonNameByParams(
								Preferred_Name,
								0,
								Common_Name,
								0,
								NULL,
								@ShowCommonNames)
			END + CASE WHEN CUN.Number IS NULL THEN '' ELSE ' - ' + CUN.Number END AS Item_Name,
			CASE SU.Life_Sciences
				WHEN 1 THEN
					SUBSTRING(( 
						SELECT	C.Published_Term + ','
						FROM	Taxon_Determination D 
						INNER JOIN	Concept C
							ON	D.Nomenclatural_Status_Concept_Key = C.Concept_Key
						WHERE	D.Nomenclatural_Status_Concept_Key IS NOT NULL
							AND	D.Specimen_Collection_Unit_Key = SU.Collection_Unit_Key
						ORDER BY	D.Taxon_Determination_Key
							FOR XML PATH('')
					), 0, 250)
				ELSE
					SUBSTRING(( 
						SELECT	C.Published_Term + ','
						FROM	Determination D 
						INNER JOIN	Concept C
							ON	D.Nomenclatural_Status_Concept_Key = C.Concept_Key
						WHERE	D.Nomenclatural_Status_Concept_Key IS NOT NULL
							AND	D.Specimen_Collection_Unit_Key = SU.Collection_Unit_Key
						ORDER BY	D.Determination_Key
							FOR XML PATH('')
					), 0, 250) 
			END AS NomenclaturalStatus
	FROM		Individual AS I
	INNER JOIN	Survey_Event_Recorder AS SER ON SER.Name_Key = I.Name_Key
	INNER JOIN	Sample_Recorder AS SR ON SR.SE_Recorder_Key = SER.SE_Recorder_Key
	INNER JOIN	[Sample] AS S ON S.Sample_Key = SR.Sample_Key
	LEFT JOIN	Occurrence AS O ON O.Sample_Key = S.Sample_Key
	LEFT JOIN	Taxon_Occurrence AS XO ON XO.Sample_Key = S.Sample_Key
	INNER JOIN	Specimen_Field_Data AS SFD ON (SFD.Occurrence_Key = O.Occurrence_Key
							OR SFD.Taxon_Occurrence_Key = XO.Taxon_Occurrence_Key)
						AND SFD.Gathering_Event = 1
	INNER JOIN	Specimen_Unit AS SU ON SU.Collection_Unit_Key = SFD.Collection_Unit_Key
					AND SU.Collection_Unit_Key = @CollectionUnitKey
	INNER JOIN	Collection_Unit AS CU ON CU.Collection_Unit_Key = SU.Collection_Unit_Key 
					AND ((CU.Domain_Mask & @UserDomainMask > 0) 
						OR (CU.Entered_Session_ID = @SessionID)
						OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
					
	LEFT JOIN	(Determination AS D
	INNER JOIN	VW_ConceptTermPreferred AS CTP ON CTP.Concept_Key = D.Concept_Key)
			ON D.Determination_Key = SU.Preferred_Determination_Key
	LEFT JOIN 	(Taxon_Determination AS TD 
				INNER JOIN 	Index_Taxon_Name ITN ON ITN.Taxon_List_Item_Key = TD.Taxon_List_Item_Key)
			ON TD.Taxon_Determination_Key = SU.Preferred_Taxon_Determination_Key  
	LEFT JOIN 	Collection_Unit_Number CUN ON SU.Collection_Unit_key = CUN.Collection_Unit_Key 
					 AND CUN.Preferred = 1
	
	WHERE		I.Name_Key = @Key
	ORDER BY	Item_Name
ELSE
	SELECT DISTINCT
			SU.Collection_Unit_Key AS Item_Key,
			CASE WHEN SU.Life_Sciences = 0
			THEN CTP.PlainText COLLATE SQL_Latin1_General_CP1_CI_AS
			ELSE dbo.ufn_GetFormattedTaxonNameByParams(
								Preferred_Name,
								0,
								Common_Name,
								0,
								NULL,
								@ShowCommonNames)
			END + CASE WHEN CUN.Number IS NULL THEN '' ELSE ' - ' + CUN.Number END AS Item_Name,
			CASE SU.Life_Sciences
				WHEN 1 THEN
					SUBSTRING(( 
						SELECT	C.Published_Term + ','
						FROM	Taxon_Determination D 
						INNER JOIN	Concept C
							ON	D.Nomenclatural_Status_Concept_Key = C.Concept_Key
						WHERE	D.Nomenclatural_Status_Concept_Key IS NOT NULL
							AND	D.Specimen_Collection_Unit_Key = SU.Collection_Unit_Key
						ORDER BY	D.Taxon_Determination_Key
							FOR XML PATH('')
					), 0, 250)
				ELSE
					SUBSTRING(( 
						SELECT	C.Published_Term + ','
						FROM	Determination D 
						INNER JOIN	Concept C
							ON	D.Nomenclatural_Status_Concept_Key = C.Concept_Key
						WHERE	D.Nomenclatural_Status_Concept_Key IS NOT NULL
							AND	D.Specimen_Collection_Unit_Key = SU.Collection_Unit_Key
						ORDER BY	D.Determination_Key
							FOR XML PATH('')
					), 0, 250) 
			END AS NomenclaturalStatus
	FROM		Individual AS I
	INNER JOIN	Survey_Event_Recorder AS SER ON SER.Name_Key = I.Name_Key
	INNER JOIN	Sample_Recorder AS SR ON SR.SE_Recorder_Key = SER.SE_Recorder_Key
	INNER JOIN	[Sample] AS S ON S.Sample_Key = SR.Sample_Key
	LEFT JOIN	Occurrence AS O ON O.Sample_Key = S.Sample_Key
	LEFT JOIN	Taxon_Occurrence AS XO ON XO.Sample_Key = S.Sample_Key
	INNER JOIN	Specimen_Field_Data AS SFD ON (SFD.Occurrence_Key = O.Occurrence_Key
							OR SFD.Taxon_Occurrence_Key = XO.Taxon_Occurrence_Key)
						AND SFD.Gathering_Event = 1
	INNER JOIN	Specimen_Unit AS SU ON SU.Collection_Unit_Key = SFD.Collection_Unit_Key
					AND SU.Parent_Collection_Collection_Unit_Key = @CollectionUnitKey
	INNER JOIN	Collection_Unit AS CU ON CU.Collection_Unit_Key = SU.Collection_Unit_Key 
					AND ((CU.Domain_Mask & @UserDomainMask > 0) 
						OR (CU.Entered_Session_ID = @SessionID)
						OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
	LEFT JOIN	(Determination AS D
				INNER JOIN	VW_ConceptTermPreferred AS CTP ON CTP.Concept_Key = D.Concept_Key)
			ON D.Determination_Key = SU.Preferred_Determination_Key
	LEFT JOIN 	(Taxon_Determination AS TD 
				INNER JOIN 	Index_Taxon_Name ITN ON ITN.Taxon_List_Item_Key = TD.Taxon_List_Item_Key)
			ON TD.Taxon_Determination_Key = SU.Preferred_Taxon_Determination_Key  
	LEFT JOIN 	Collection_Unit_Number CUN ON SU.Collection_Unit_key = CUN.Collection_Unit_Key 
					 AND CUN.Preferred = 1
	
	WHERE		I.Name_Key = @Key
	ORDER BY	Item_Name
SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SpecimensCollected_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SpecimensCollected_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SpecimensCollected_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SpecimensCollected_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SpecimensCollected_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_SpecimensCollected_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SpecimensCollected_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SpecimensCollected_Select TO [Dev - JNCC SQL]
END

GO