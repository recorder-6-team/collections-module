/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptGroups_Select_ForLocalDomain]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptGroups_Select_ForLocalDomain]
GO

/*===========================================================================*\
  Description:	Returns domain records.

  Parameters:	@LocalDomainKey	Subject area key 

  Created:	November 2003

  Last revision information:
    $Revision: 1 $
    $Date: 28/11/03 11:04 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptGroups_Select_ForLocalDomain]
	@ParentKey CHAR(16)
AS

SET NOCOUNT ON

	SELECT DISTINCT
		  	CG.Concept_Group_Key AS [Key], 
		  	CG.Item_Name, 
		  	CG.Local_Domain_Key,
			CASE WHEN CGV.Concept_Group_Version_Key IS NULL THEN 0 ELSE 1 END AS Has_Children
	FROM 	  	Concept_Group AS CG
	LEFT JOIN 	Concept_Group_Version CGV ON CGV.Concept_Group_Key = CG.Concept_Group_Key
	WHERE	  	Local_Domain_Key = @ParentKey
	ORDER BY  	Item_Name

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptGroups_Select_ForLocalDomain') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptGroups_Select_ForLocalDomain'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptGroups_Select_ForLocalDomain TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptGroups_Select_ForLocalDomain TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptGroups_Select_ForLocalDomain TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ConceptGroups_Select_ForLocalDomain TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptGroups_Select_ForLocalDomain TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptGroups_Select_ForLocalDomain TO [Dev - JNCC SQL]
END

GO