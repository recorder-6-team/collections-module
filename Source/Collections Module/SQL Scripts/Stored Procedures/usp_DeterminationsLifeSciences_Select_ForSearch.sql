IF Object_ID('dbo.usp_DeterminationsLifeSciences_Select_ForSearch') IS NOT NULL
	DROP PROCEDURE dbo.usp_DeterminationsLifeSciences_Select_ForSearch
GO

/*===========================================================================*\
  Description:
	Returns Concept_Key and DisplayTerm when search characters are entered.
	Uses domain mask if the taxon list item is mapped

  Parameters:
	@SearchText
	@UserDomainMask
	@SearchSize

  Created:	October 2003

  Last revision information:
	$Revision: 11 $
	$Date: 12/11/10 16:26 $
	$Author: Jamesbichard $

\*===========================================================================*/

CREATE PROCEDURE dbo.usp_DeterminationsLifeSciences_Select_ForSearch
	@SearchText 			VARCHAR(100),
	@UserDomainMask 		INT,
	@SearchSize 			INT = NULL,
	@PreferredSynonymsOnly	BIT = 0
AS
	SET NOCOUNT ON

    SET @SearchSize = ISNULL(@SearchSize, 0)

    SET ROWCOUNT @SearchSize

	SELECT		ITN.Taxon_List_Item_Key AS Item_Key, 
				dbo.ufn_GetFormattedSpeciesName(ITN.Taxon_List_Item_Key) + ' - ' + TL.Item_Name AS DisplayTerm,
				dbo.ufn_GetFormattedSpeciesName(ITN.Taxon_List_Item_Key) + ' - ' + TL.Item_Name AS SearchTerm,
				C.List_Preferred
	FROM		Index_Taxon_Name 					ITN 
	INNER JOIN 	Taxon_List_Version 					TLV 	ON TLV.Taxon_List_Version_Key 	= ITN.Taxon_List_Version_Key
	INNER JOIN 	Taxon_List 							TL 		ON TL.Taxon_List_Key			= TLV.Taxon_List_Key
	LEFT JOIN	Taxon_Dictionary_Concept_Mapping 	TDCM 	ON TDCM.Taxon_List_Item_Key 	= ITN.Taxon_List_Item_Key
	LEFT JOIN	Concept 							C 		ON C.Concept_Key 				= TDCM.Concept_Key
	LEFT JOIN 	Concept_Group 						CG 		ON CG.Concept_Group_Key			= C.Concept_Group_Key
	LEFT JOIN 	Local_Domain 						LD 		ON LD.Local_Domain_Key			= CG.Local_Domain_Key
	LEFT JOIN 	Domain 								D 		ON D.Domain_Key					= LD.Domain_Key
	WHERE 		((@PreferredSynonymsOnly = 0 AND ITN.Actual_Name LIKE @SearchText + '%')
				 OR
				 (ITN.Preferred_Name LIKE @SearchText + '%'))
	AND 		((((D.Domain_Mask & @UserDomainMask > 0) OR (D.Domain_Mask = 0)) AND D.Has_Occurrences = 1) OR D.Domain_Key IS NULL)
	ORDER BY DisplayTerm

    SET ROWCOUNT 0
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
PRINT 'Setting up security on procedure usp_DeterminationsLifeSciences_Select_ForSearch'
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
   	GRANT EXECUTE ON dbo.usp_DeterminationsLifeSciences_Select_ForSearch TO [R2k_AddOnly]
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
	GRANT EXECUTE ON dbo.usp_DeterminationsLifeSciences_Select_ForSearch TO [R2k_Administrator]
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
	GRANT EXECUTE ON dbo.usp_DeterminationsLifeSciences_Select_ForSearch TO [R2k_FullEdit]
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
	GRANT EXECUTE ON dbo.usp_DeterminationsLifeSciences_Select_ForSearch TO [R2k_ReadOnly]
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
	GRANT EXECUTE ON dbo.usp_DeterminationsLifeSciences_Select_ForSearch TO [R2k_RecordCardsOnly]
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
   	GRANT EXECUTE ON dbo.usp_DeterminationsLifeSciences_Select_ForSearch TO [Dev - JNCC SQL]
GO