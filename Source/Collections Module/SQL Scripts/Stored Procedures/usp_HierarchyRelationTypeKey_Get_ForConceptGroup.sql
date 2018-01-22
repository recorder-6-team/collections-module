/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_HierarchyRelationTypeKey_Get_ForConceptGroup]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_HierarchyRelationTypeKey_Get_ForConceptGroup]
GO

/*===========================================================================*\
  Description:	Gets the Hierarchy Relation Type Key table for a Concept Group.

  Parameters:	@Key
		@HierarchyRelationTypeKey OUTPUT

  Created:	January 2004

  Last revision information:
    $Revision: 1 $
    $Date: 8/01/04 11:30 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_HierarchyRelationTypeKey_Get_ForConceptGroup]
	@Key char(16),
	@HierarchyRelationTypeKey char(16) OUTPUT
AS
	SELECT 	@HierarchyRelationTypeKey = Hierarchy_Relation_Type_Key
	FROM	Concept_Group
	WHERE	Concept_Group_Key = @Key
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_HierarchyRelationTypeKey_Get_ForConceptGroup') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_HierarchyRelationTypeKey_Get_ForConceptGroup'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_HierarchyRelationTypeKey_Get_ForConceptGroup TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_HierarchyRelationTypeKey_Get_ForConceptGroup TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_HierarchyRelationTypeKey_Get_ForConceptGroup TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_HierarchyRelationTypeKey_Get_ForConceptGroup TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_HierarchyRelationTypeKey_Get_ForConceptGroup TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_HierarchyRelationTypeKey_Get_ForConceptGroup TO [Dev - JNCC SQL]
END

GO
