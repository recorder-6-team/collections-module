/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_HierarchyRelationTypeKey_Get_ForConceptGroups]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_HierarchyRelationTypeKey_Get_ForConceptGroups]
GO

/*===========================================================================*\
  Description:	When adding new concept groups, the most frequently used 
		Hierarchy_Relation_Type_Key within the domain's existing 
		concept groups is selected and used as a default value.  
		This stored proc takes a local domain key and returns the most
		frequently used Hierarchy_Relation_Type_Key.

		If more than one key shares the value for most frequently
		used, only one of them will be selected (due to the 
		SELECT TOP 1). However, because of the order by clause
		it should always be the same one that is returned.

  Parameters:	@LocalDomainKey	

  Created:	November 2003

  Last revision information:
    $Revision: 1 $
    $Date: 28/11/03 11:03 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_HierarchyRelationTypeKey_Get_ForConceptGroups]
	@LocalDomainKey char(16)
AS

SET NOCOUNT ON

DECLARE @DomainKey char(16) 

	-- The local domain key is passed in. We need the domain key.
	SELECT 		@DomainKey = Domain_Key
	FROM 		Local_Domain
	WHERE 		Local_Domain_Key = @LocalDomainKey

	-- Now we have the domain key, we can get all the Heirarchy_Relation_Type_Keys for all
	-- the concept groups in the domain. We can then count them, order them and select
	-- the most popular key.
	SELECT TOP 1	Count (CG.Hierarchy_Relation_Type_Key) AS Frequency,
			CG.Hierarchy_Relation_Type_Key
	FROM		Domain D
	INNER JOIN	Local_Domain AS LD ON LD.Domain_Key = D.Domain_Key
	INNER JOIN	Concept_Group AS CG ON CG.Local_Domain_Key = LD.Local_Domain_Key
	WHERE 		D.Domain_Key = @DomainKey
	GROUP BY	CG.Hierarchy_Relation_Type_Key
	ORDER BY	Frequency DESC, CG.Hierarchy_Relation_Type_Key

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_HierarchyRelationTypeKey_Get_ForConceptGroups') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_HierarchyRelationTypeKey_Get_ForConceptGroups'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_HierarchyRelationTypeKey_Get_ForConceptGroups TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_HierarchyRelationTypeKey_Get_ForConceptGroups TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_HierarchyRelationTypeKey_Get_ForConceptGroups TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_HierarchyRelationTypeKey_Get_ForConceptGroups TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_HierarchyRelationTypeKey_Get_ForConceptGroups TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_HierarchyRelationTypeKey_Get_ForConceptGroups TO [Dev - JNCC SQL]
END

GO