IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_DeterminationsEarthSciences_Select_ForSearch') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_DeterminationsEarthSciences_Select_ForSearch]
GO

/*===========================================================================*\
  Description:	Returns Concept_Key and DisplayTerm when search characters are 
		entered.

  Parameters:	@SearchText
		@UserDomainMask

  Created:	October 2003

  Last revision information:
	$Revision: 10 $
	$Date: 26/08/11 14:44 $
	$Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_DeterminationsEarthSciences_Select_ForSearch] 
	@SearchText VARCHAR(100),
	@UserDomainMask INT
AS

	SET NOCOUNT ON

	SELECT 	DISTINCT
			VW.Concept_Key AS Item_Key,
			VW.Item_Name + ' - ' + CG.Item_Name AS DisplayTerm,
			VW.Item_Name + ' - ' + CG.Item_Name AS SearchTerm, 
			CG.Item_Name,
			VW.List_Preferred

	FROM		VW_ConceptTerm AS VW 
	INNER JOIN 	Concept_Group CG ON CG.Concept_Group_Key = VW.Concept_Group_Key
	INNER JOIN 	Local_Domain LD ON LD.Local_Domain_Key = CG.Local_Domain_Key
	INNER JOIN 	Domain D ON D.Domain_Key = LD.Domain_Key
			AND ((D.Domain_Mask & @UserDomainMask > 0) OR (D.Domain_Mask = 0))
			AND D.Has_Occurrences = 1
	-- Join to find out which concepts are mapped to taxa
	LEFT JOIN	Taxon_Dictionary_Concept_Mapping TDCM ON TDCM.Concept_Key = VW.Concept_Key
	LEFT JOIN	Search_Term ST ON ST.Concept_Key = VW.Concept_Key
	WHERE 		ST.PlainText LIKE @SearchText + '%'
	-- And filter out all concepts that are mapped to any taxon.
	AND		TDCM.Concept_Key IS NULL
	
	ORDER BY DisplayTerm
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_DeterminationsEarthSciences_Select_ForSearch') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_DeterminationsEarthSciences_Select_ForSearch'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_DeterminationsEarthSciences_Select_ForSearch TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_DeterminationsEarthSciences_Select_ForSearch TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_DeterminationsEarthSciences_Select_ForSearch TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_DeterminationsEarthSciences_Select_ForSearch TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_DeterminationsEarthSciences_Select_ForSearch TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_DeterminationsEarthSciences_Select_ForSearch TO [Dev - JNCC SQL]
END
GO