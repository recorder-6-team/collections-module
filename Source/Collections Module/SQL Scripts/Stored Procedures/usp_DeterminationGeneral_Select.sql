/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_DeterminationGeneral_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_DeterminationGeneral_Select]
GO

/*===========================================================================*\
  Description:	Returns data for the DeterminationGeneral frame.

  Parameters:	@Key	Collection Unit key

  Created:	Setember 2003

  Last revision information:
    $Revision: 3 $
    $Date: 27/01/04 8:53 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_DeterminationGeneral_Select]
	@Key char(16),
	@IsTaxon BIT
AS

SET NOCOUNT ON

	IF @IsTaxon=1 
		SELECT 
			D.Taxon_List_Item_Key AS DetKey, 
			ITN.Actual_Name AS Term,
			DM.Domain_Mask,
			D.Determination_Type_Key,
			DT.Short_Name AS Type,
			CTStat.Concept_Key AS Status_Concept_Key,
			CTStat.PlainText AS Status_Term,
			D.Confidence,
			D.Determiner AS Determiner_Name_Key,
			dbo.ufn_GetFormattedName(D.Determiner) AS Determiner,
			DR.Determiner_Role_Key,
			DR.Short_Name as Determiner_Role,
			D.Vague_Date_Start,
			D.Vague_Date_End,
			D.Vague_Date_Type,
			D.Used_Specimen,
			D.Preferred,
			D.Method,
			D.Comment AS Notes,
			D.Timestamp
		
		FROM Taxon_Determination D
	
		INNER JOIN Index_Taxon_Name ITN ON ITN.Taxon_List_Item_Key = D.Taxon_List_Item_Key
		LEFT JOIN VW_ConceptTerm CTStat ON CTStat.Concept_Key=D.Nomenclatural_Status_Concept_Key
		INNER JOIN Determination_Type DT ON DT.Determination_Type_Key=D.Determination_Type_Key
		INNER JOIN Determiner_Role DR ON DR.Determiner_Role_Key = D.Determiner_Role_Key 
		LEFT JOIN Taxon_Dictionary_Concept_Mapping TDM ON TDM.Taxon_List_ITem_Key = D.Taxon_List_Item_Key
		LEFT JOIN Concept C ON C.Concept_Key=TDM.Concept_Key
		LEFT JOIN Concept_Group CG ON CG.Concept_Group_Key = C.Concept_Group_Key
		LEFT JOIN Local_Domain LD ON LD.Local_Domain_Key=CG.Local_Domain_Key
		LEFT JOIN Domain DM ON DM.Domain_Key=LD.Domain_Key
	
		WHERE D.Taxon_Determination_Key=@Key
	ELSE
		SELECT 
			CT.Concept_Key AS DetKey,
			CT.Plaintext AS Term,
			DM.Domain_Mask,
			D.Determination_Type_Key,
			DT.Short_Name AS Type,
			CTStat.Concept_Key AS Status_Concept_Key,
			CTStat.PlainText AS Status_Term,
			D.Confidence,
			D.Determiner_Name_Key,
			dbo.ufn_GetFormattedName(D.Determiner_Name_Key ) AS Determiner,
			DR.Determiner_Role_Key,
			DR.Short_Name as Determiner_Role,
			D.Vague_Date_Start,
			D.Vague_Date_End,
			D.Vague_Date_Type,
			D.Used_Specimen,
			D.Preferred,
			D.Method,
			D.Notes,
			D.Timestamp
	
		FROM Determination D
	
		INNER JOIN VW_ConceptTerm CT ON CT.Concept_Key=D.Concept_Key
		LEFT JOIN VW_ConceptTerm CTStat ON CTStat.Concept_Key=D.Nomenclatural_Status_Concept_Key
		INNER JOIN Determination_Type DT ON DT.Determination_Type_Key=D.Determination_Type_Key
		INNER JOIN Determiner_Role DR ON DR.Determiner_Role_Key = D.Determiner_Role_Key 
		INNER JOIN Concept C ON C.Concept_Key=D.Concept_Key
		INNER JOIN Concept_Group CG ON CG.Concept_Group_Key = C.Concept_Group_Key
		INNER JOIN Local_Domain LD ON LD.Local_Domain_Key=CG.Local_Domain_Key
		INNER JOIN Domain DM ON DM.Domain_Key=LD.Domain_Key
	
		WHERE D.Determination_Key=@Key

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_DeterminationGeneral_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_DeterminationGeneral_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_DeterminationGeneral_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_DeterminationGeneral_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_DeterminationGeneral_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_DeterminationGeneral_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_DeterminationGeneral_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_DeterminationGeneral_Select TO [Dev - JNCC SQL]
END

GO
