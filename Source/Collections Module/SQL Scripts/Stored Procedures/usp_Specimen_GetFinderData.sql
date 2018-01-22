If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Specimen_GetFinderData]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Specimen_GetFinderData]
GO
    
/*===========================================================================*\
  Description:	Gets all the data needed to add dropped specimens to one of the
				specimen finder grids
  Parameters:	 

  Created:	October 2007

  Last revision information:
    $Revision: 7 $
    $Date: 4/08/11 14:40 $
    $Author: Simonlewis $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_Specimen_GetFinderData]
  @CollectionUnitKey  Char(16),
  @UserDomainMask Int,
  @ShowCommonNames Bit,
  @SessionID      Char(16)
 AS

-- Because of multiple preferred list synoynms - i.e. Amber and Ambre - different language but both preferred. Just select the first.
SELECT		TOP 1
			SU.Collection_Unit_Key					AS		Item_Key, 
			CASE 
				WHEN C.Concept_Key IS NOT NULL
				THEN C.Concept_Key 
				WHEN ITN.Taxon_List_Item_Key IS NOT NULL
				THEN ITN.Taxon_List_Item_Key 
			END										AS		Det_Item_Key, 
			CASE 
				WHEN SU.Life_Sciences = 0
				THEN CP.Published_Term 
				ELSE dbo.ufn_GetFormattedTaxonNameByParams(
						ITN.Actual_Name,
						ITN.Actual_Name_Italic,
						ITN.Common_Name, 
						ITN.Common_Name_Italic,
						ITN.Authority,
						@ShowCommonNames)
			END										AS		Item_Name, 
			SU.Life_Sciences, 
			dbo.ufn_GetPrefNumber(
					SU.Collection_Unit_Key)			AS		Number, 
			CU.Current_Location_Code, 
			SU.Specimen_Type_Concept_Key, 
			STCP.Published_Term						AS		Specimen_Type,
			C.List_Code, 
			DM.Item_Name							AS		Domain_Name 
FROM		SPECIMEN_UNIT							AS		SU 
INNER JOIN	COLLECTION_UNIT							AS		CU
ON			SU.Collection_Unit_Key					=		CU.Collection_Unit_Key 
AND			((CU.Domain_Mask & @UserDomainMask > 0)
OR			(CU.Domain_Mask = 0)
OR			(CU.Entered_Session_ID = @SessionID)
OR			(CU.Changed_Session_ID = @SessionID)) 
LEFT JOIN	COLLECTION_UNIT_NUMBER					AS		CUN
ON			SU.Collection_Unit_key					=		CUN.Collection_Unit_Key 
AND			CUN.Preferred							=		1 
LEFT JOIN	DETERMINATION							AS		D
ON			SU.Preferred_Determination_Key			=		D.Determination_Key 
LEFT JOIN	Concept									AS		C
ON			D.Concept_Key							=		C.Concept_Key 
LEFT JOIN	Concept									AS		CP
ON			CP.Meaning_Key							=		C.Meaning_Key
AND			CP.Concept_Group_Key					=		C.Concept_Group_Key
AND			CP.List_Preferred						=		1 
LEFT JOIN	TAXON_DETERMINATION						AS		TD
ON			SU.Preferred_Taxon_Determination_Key	=		TD.Taxon_Determination_Key 
LEFT JOIN	INDEX_TAXON_NAME						AS		ITN
ON			TD.Taxon_List_Item_Key					=		ITN.Taxon_List_Item_Key 
LEFT JOIN	Concept_Group							AS		CG
ON			C.Concept_Group_Key						=		CG.Concept_Group_Key 
LEFT JOIN	Local_Domain							AS		LD
ON			CG.Local_Domain_Key						=		LD.Local_Domain_Key 
LEFT JOIN	Domain									AS		DM
ON			LD.Domain_Key							=		DM.Domain_Key
INNER JOIN	Concept									AS		STC
ON			SU.Specimen_Type_Concept_Key			=		STC.Concept_Key 
LEFT JOIN	Concept									AS		STCP
ON			STCP.Meaning_Key						=		STC.Meaning_Key 
AND			STCP.Concept_Group_Key					=		STC.Concept_Group_Key
AND			STCP.List_Preferred						=		1 
WHERE		SU.Collection_Unit_Key					=		@CollectionUnitKey
ORDER BY	3 
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Specimen_GetFinderData') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Specimen_GetFinderData'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Specimen_GetFinderData TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimen_GetFinderData TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimen_GetFinderData TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimen_GetFinderData TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Specimen_GetFinderData TO [Dev - JNCC SQL]
END

GO