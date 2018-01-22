/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Determination_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Determination_Select]
GO

/*===========================================================================*\
  Description:	Returns data for the Determination General frame.

  Parameters:	@Key		Determination key
		@IsLifeScience 	If we want Life Sciences information, then
				we need to go to the Taxon tables, and hence,
				@IsLifeScience will be set to 1. If we want Earth
				Sciences information, we go to the Determination
				table and @IsLifeScience will be set to 0.

  Created:	October 2003

  Last revision information:
    $Revision: 6 $
    $Date: 26/08/11 14:56 $
    $Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Determination_Select]
	@Key char(16),
	@IsLifeScience bit,
	@IsSpecimenUnit bit = NULL
AS

SET NOCOUNT ON

	IF @IsLifeScience = 1 
		IF (@IsSpecimenUnit = NULL) OR (@IsSpecimenUnit = 0)	
			SELECT 		D.Taxon_List_Item_Key AS DetKey, 
					ITN.Preferred_Name AS Term,
					DM.Domain_Mask,
					D.Determination_Type_Key,
					D.Taxon_Occurrence_Key AS Occurrence_Key,
					D.Specimen_Collection_Unit_Key,
					DT.Short_Name AS Type,
					CTStat.Concept_Key AS Status_Concept_Key,
					CTStat.PlainText AS Status_Term,
					D.Confidence,
					D.Determiner AS Determiner_Name_Key,
					dbo.ufn_GetFormattedName(D.Determiner) AS Determiner,
					DR.Determiner_Role_Key,
					DR.Short_Name AS Determiner_Role,
					D.Vague_Date_Start,
					D.Vague_Date_End,
					D.Vague_Date_Type,
					D.Used_Specimen,
					D.Preferred,
					D.Method,
					D.Comment AS Notes,
					D.Inferred_Determiner,
					D.[Timestamp],
					D.Include_In_Label		
			FROM 		Taxon_Determination D
			INNER JOIN 	Index_Taxon_Name ITN ON ITN.Taxon_List_Item_Key = D.Taxon_List_Item_Key
			LEFT JOIN 	vw_ConceptTerm CTStat ON CTStat.Concept_Key = D.Nomenclatural_Status_Concept_Key
			INNER JOIN 	Determination_Type DT ON DT.Determination_Type_Key = D.Determination_Type_Key
			INNER JOIN 	Determiner_Role DR ON DR.Determiner_Role_Key = D.Determiner_Role_Key 
			LEFT JOIN 	Taxon_Dictionary_Concept_Mapping TDCM ON TDCM.Taxon_List_ITem_Key = D.Taxon_List_Item_Key
			LEFT JOIN 	Concept C ON C.Concept_Key = TDCM.Concept_Key
			LEFT JOIN 	Concept_Group CG ON CG.Concept_Group_Key = C.Concept_Group_Key
			LEFT JOIN 	Local_Domain LD ON LD.Local_Domain_Key = CG.Local_Domain_Key
			LEFT JOIN 	Domain DM ON DM.Domain_Key = LD.Domain_Key
			WHERE 		D.Taxon_Determination_Key = @Key
		ELSE
			SELECT 			
					SU.Collection_Unit_Key,		
					D.Taxon_List_Item_Key AS DetKey, 
					ITN.Preferred_Name AS Term,
					DM.Domain_Mask,
					D.Determination_Type_Key,
					D.Taxon_Occurrence_Key AS Occurrence_Key,
					D.Specimen_Collection_Unit_Key,
					DT.Short_Name AS Type,
					CTStat.Concept_Key AS Status_Concept_Key,
					CTStat.PlainText AS Status_Term,
					D.Confidence,
					D.Determiner AS Determiner_Name_Key,
					dbo.ufn_GetFormattedName(D.Determiner) AS Determiner,
					DR.Determiner_Role_Key,
					DR.Short_Name AS Determiner_Role,
					D.Vague_Date_Start,
					D.Vague_Date_End,
					D.Vague_Date_Type,
					D.Used_Specimen,
					CASE WHEN SU.Collection_Unit_Key IS NULL THEN 0
								ELSE 1
					END AS Preferred,
	
					D.Method,
					D.Comment AS Notes,
					D.Inferred_Determiner,
					D.[Timestamp],
					D.Include_In_Label			
			FROM 		Taxon_Determination D
			INNER JOIN 	Index_Taxon_Name ITN ON ITN.Taxon_List_Item_Key = D.Taxon_List_Item_Key
			LEFT JOIN 	vw_ConceptTerm CTStat ON CTStat.Concept_Key = D.Nomenclatural_Status_Concept_Key
			INNER JOIN 	Determination_Type DT ON DT.Determination_Type_Key = D.Determination_Type_Key
			INNER JOIN 	Determiner_Role DR ON DR.Determiner_Role_Key = D.Determiner_Role_Key 
			LEFT JOIN 	Taxon_Dictionary_Concept_Mapping TDCM ON TDCM.Taxon_List_ITem_Key = D.Taxon_List_Item_Key
			LEFT JOIN 	Concept C ON C.Concept_Key = TDCM.Concept_Key
			LEFT JOIN 	Concept_Group CG ON CG.Concept_Group_Key = C.Concept_Group_Key
			LEFT JOIN 	Local_Domain LD ON LD.Local_Domain_Key = CG.Local_Domain_Key
			LEFT JOIN 	Domain DM ON DM.Domain_Key = LD.Domain_Key
			LEFT JOIN	Specimen_Unit AS SU ON SU.Collection_Unit_Key = D.Specimen_Collection_Unit_Key
							AND SU.Preferred_Taxon_Determination_Key = @Key
			WHERE 		D.Taxon_Determination_Key = @Key
	ELSE
		IF (@IsSpecimenUnit = NULL) OR (@IsSpecimenUnit = 0)	
			SELECT 		CT.Concept_Key AS DetKey,
					CT.Item_Name AS Term,
					DM.Domain_Mask,
					D.Determination_Type_Key,
					D.Occurrence_Key,
					D.Specimen_Collection_Unit_Key,
					DT.Short_Name AS Type,
					CTStat.Concept_Key AS Status_Concept_Key,
					CTStat.PlainText AS Status_Term,
					D.Confidence,
					D.Determiner_Name_Key,
					dbo.ufn_GetFormattedName(D.Determiner_Name_Key ) AS Determiner,
					DR.Determiner_Role_Key,
					DR.Short_Name AS Determiner_Role,
					D.Vague_Date_Start,
					D.Vague_Date_End,
					D.Vague_Date_Type,
					D.Used_Specimen,
					D.Preferred,
					D.Method,
					D.Notes,
					D.Inferred_Determiner,
					D.[Timestamp],
					D.Include_In_Label
			FROM 		Determination D
			INNER JOIN 	vw_ConceptTerm CT ON CT.Concept_Key = D.Concept_Key
			LEFT JOIN 	vw_ConceptTerm CTStat ON CTStat.Concept_Key = D.Nomenclatural_Status_Concept_Key
			INNER JOIN 	Determination_Type DT ON DT.Determination_Type_Key = D.Determination_Type_Key
			INNER JOIN 	Determiner_Role DR ON DR.Determiner_Role_Key = D.Determiner_Role_Key 
			INNER JOIN 	Concept C ON C.Concept_Key = D.Concept_Key
			INNER JOIN 	Concept_Group CG ON CG.Concept_Group_Key = C.Concept_Group_Key
			INNER JOIN 	Local_Domain LD ON LD.Local_Domain_Key = CG.Local_Domain_Key
			INNER JOIN 	Domain DM ON DM.Domain_Key = LD.Domain_Key
			WHERE 		D.Determination_Key = @Key
		ELSE
			SELECT 		
					SU.Collection_Unit_Key,
					CT.Concept_Key AS DetKey,
					CT.Item_Name AS Term,
					DM.Domain_Mask,
					D.Determination_Type_Key,
					D.Occurrence_Key,
					D.Specimen_Collection_Unit_Key,
					DT.Short_Name AS Type,
					CTStat.Concept_Key AS Status_Concept_Key,
					CTStat.PlainText AS Status_Term,
					D.Confidence,
					D.Determiner_Name_Key,
					dbo.ufn_GetFormattedName(D.Determiner_Name_Key ) AS Determiner,
					DR.Determiner_Role_Key,
					DR.Short_Name AS Determiner_Role,
					D.Vague_Date_Start,
					D.Vague_Date_End,
					D.Vague_Date_Type,
					D.Used_Specimen,
					CASE WHEN SU.Collection_Unit_Key IS NULL THEN 0
									ELSE 1
					END AS Preferred,
					D.Method,
					D.Notes,
					D.Inferred_Determiner,
					D.[Timestamp],
					D.Include_In_Label			
			FROM 		Determination D
			INNER JOIN 	vw_ConceptTerm CT ON CT.Concept_Key = D.Concept_Key
			LEFT JOIN 	vw_ConceptTerm CTStat ON CTStat.Concept_Key = D.Nomenclatural_Status_Concept_Key
			INNER JOIN 	Determination_Type DT ON DT.Determination_Type_Key = D.Determination_Type_Key
			INNER JOIN 	Determiner_Role DR ON DR.Determiner_Role_Key = D.Determiner_Role_Key 
			INNER JOIN 	Concept C ON C.Concept_Key = D.Concept_Key
			INNER JOIN 	Concept_Group CG ON CG.Concept_Group_Key = C.Concept_Group_Key
			INNER JOIN 	Local_Domain LD ON LD.Local_Domain_Key = CG.Local_Domain_Key
			INNER JOIN 	Domain DM ON DM.Domain_Key = LD.Domain_Key
			LEFT JOIN	Specimen_Unit AS SU ON SU.Collection_Unit_Key = D.Specimen_Collection_Unit_Key
							AND SU.Preferred_Determination_Key = @Key
			WHERE 		D.Determination_Key = @Key
SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Determination_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Determination_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Determination_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Determination_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Determination_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Determination_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Determination_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Determination_Select TO [Dev - JNCC SQL]
END

GO
