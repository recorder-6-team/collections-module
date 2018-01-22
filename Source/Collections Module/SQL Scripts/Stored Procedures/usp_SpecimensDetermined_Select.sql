/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_SpecimensDetermined_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_SpecimensDetermined_Select]
GO

/*===========================================================================*\
  Description:	Returns the list of all preferred determinations and the
		preferred number for the specimens linked to the currently
		selected collection. Returns the list of all determinations for
		specimen top level node.

  Parameters:	@Key			Key of the determiner.
		@CollectionUnitKey	Key of the collection unit
		@KeyIsSpecimen		Bit saying whether the key is a specimen

  Created:	October 2003

  Last revision information:
    $Revision: 11 $
    $Date: 13/03/13 8:38 $
    $Author: Alexanderpadley $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SpecimensDetermined_Select]
	@Key char(16),
	@CollectionUnitKey char(16),
	@UserDomainMask int,
	@SessionID char(16),
	@ShowCommonNames bit,
	@KeyIsSpecimen bit
AS

SET NOCOUNT ON

IF @KeyIsSpecimen = 1
BEGIN
	SELECT DISTINCT
			SU.Collection_Unit_Key AS Item_Key, 
			CASE SU.Life_Sciences 
				WHEN 0 THEN 
					CTPref.Item_Name COLLATE SQL_Latin1_General_CP1_CI_AS
				ELSE dbo.ufn_GetFormattedTaxonNameByParams(
								ITN.Preferred_Name,
								0,
								ITN.Common_Name,
								0,
								NULL,
								@ShowCommonNames)
			END + 
			CASE 
				WHEN CUN.Number IS NULL THEN '' 
				ELSE ' - ' + CUN.Number 
			END AS Item_Name,
			CASE Life_Sciences
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
	FROM 		Specimen_Unit AS SU
	INNER JOIN	Collection_Unit AS CU ON CU.Collection_Unit_Key = SU.Collection_Unit_Key	
					AND ((CU.Domain_Mask & @UserDomainMask > 0) 
						OR (CU.Entered_Session_ID = @SessionID) 
						OR (CU.Changed_Session_ID = @SessionID) 
						OR (CU.Domain_Mask = 0))
	LEFT JOIN	Taxon_Determination AS TD ON TD.Taxon_Determination_Key = SU.Preferred_Taxon_Determination_Key
	LEFT JOIN	Determination AS D ON D.Determination_Key = SU.Preferred_Determination_Key
	INNER JOIN	Individual AS I ON (I.Name_Key = TD.Determiner
						OR I.Name_Key = D.Determiner_Name_Key)
					AND I.Name_Key = @Key
	LEFT JOIN 	VW_ConceptTermPreferred CTPref ON CTPref.Concept_Key=D.Concept_Key
	LEFT JOIN 	Index_Taxon_Name ITN ON ITN.Taxon_List_Item_Key=TD.Taxon_List_Item_Key
	LEFT JOIN 	Collection_Unit_Number CUN ON SU.Collection_Unit_key = CUN.Collection_Unit_Key 
			 			AND CUN.Preferred = 1
	WHERE 		SU.Collection_unit_key = @CollectionUnitKey
	ORDER BY 	Item_Name
END
ELSE
BEGIN
	SELECT DISTINCT
			SU.Collection_Unit_Key AS Item_Key, 
			CASE SU.Life_Sciences 
				WHEN 0 THEN 
					CTPref.PlainText COLLATE SQL_Latin1_General_CP1_CI_AS
				ELSE dbo.ufn_GetFormattedTaxonNameByParams(
								ITN.Preferred_Name,
								0,
								ITN.Common_Name,
								0,
								NULL,
								@ShowCommonNames)
			END + 
			CASE 
				WHEN CUN.Number IS NULL THEN '' 
				ELSE ' - ' + CUN.Number 
			END AS Item_Name,
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
	FROM 		Specimen_Unit AS SU
	INNER JOIN	Collection_Unit AS CU ON CU.Collection_Unit_Key = SU.Collection_Unit_Key	
					AND ((CU.Domain_Mask & @UserDomainMask > 0) 
						OR (CU.Entered_Session_ID = @SessionID) 
						OR (CU.Changed_Session_ID = @SessionID) 
						OR (CU.Domain_Mask = 0))
	LEFT JOIN	Taxon_Determination AS TD ON TD.Taxon_Determination_Key = SU.Preferred_Taxon_Determination_Key
	LEFT JOIN	Determination AS D ON D.Determination_Key = SU.Preferred_Determination_Key
	INNER JOIN	Individual AS I ON (I.Name_Key = TD.Determiner
						OR I.Name_Key = D.Determiner_Name_Key)
					AND I.Name_Key = @Key
	LEFT JOIN 	VW_ConceptTermPreferred CTPref ON CTPref.Concept_Key=D.Concept_Key
	LEFT JOIN 	Index_Taxon_Name ITN ON ITN.Taxon_List_Item_Key=TD.Taxon_List_Item_Key
	LEFT JOIN 	Collection_Unit_Number CUN ON SU.Collection_Unit_key = CUN.Collection_Unit_Key 
			 			AND CUN.Preferred = 1
	WHERE 		SU.Parent_Collection_Collection_unit_key = @CollectionUnitKey
	ORDER BY 	Item_Name
END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SpecimensDetermined_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SpecimensDetermined_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SpecimensDetermined_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SpecimensDetermined_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SpecimensDetermined_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_SpecimensDetermined_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SpecimensDetermined_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SpecimensDetermined_Select TO [Dev - JNCC SQL]
END

GO