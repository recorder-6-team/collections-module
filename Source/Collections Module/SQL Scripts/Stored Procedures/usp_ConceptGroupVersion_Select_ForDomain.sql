/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptGroupVersion_Select_ForDomain]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptGroupVersion_Select_ForDomain]
GO

/*===========================================================================*\
  Description:	Retrieves a list of all versions of concept groups for a supplied domain.
		Includes the version in the caption and the date information.

  Parameters:	@Domain - key of the domain

  Created:	August 2003

  Last revision information:
    $Revision: 6 $
    $Date: 28/04/04 10:32 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptGroupVersion_Select_ForDomain]
	@Domain char(16)
AS

SELECT 		CGV.Concept_Group_Version_Key as Item_Key, 
		CG.Item_Name + ' - ' + CGV.Version as Item_Name, 
		CGV.From_Vague_Date_Start, 
		CGV.From_Vague_Date_End, 
		CGV.From_Vague_Date_Type,
		CGV.To_Vague_Date_Start, 
		CGV.To_Vague_Date_End, 
		CGV.To_Vague_Date_Type,
		IsNull(CG.Hierarchy_Relation_Type_Key, D.Default_Hierarchy_Relation_Type_Key) AS Hierarchy_Relation_Type_Key
FROM 		Local_Domain LD
INNER JOIN 	Concept_Group CG ON CG.Local_Domain_Key=LD.Local_Domain_Key
INNER JOIN 	Concept_Group_Version CGV on CGV.Concept_Group_Key=CG.Concept_Group_Key
INNER JOIN 	Domain AS D ON D.Domain_Key = LD.Domain_Key
WHERE 		LD.Domain_Key=@Domain
ORDER BY 	CGV.Sequence, CG.Item_Name

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptGroupVersion_Select_ForDomain') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptGroupVersion_Select_ForDomain'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptGroupVersion_Select_ForDomain TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptGroupVersion_Select_ForDomain TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptGroupVersion_Select_ForDomain TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ConceptGroupVersion_Select_ForDomain TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptGroupVersion_Select_ForDomain TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptGroupVersion_Select_ForDomain TO [Dev - JNCC SQL]
END

GO