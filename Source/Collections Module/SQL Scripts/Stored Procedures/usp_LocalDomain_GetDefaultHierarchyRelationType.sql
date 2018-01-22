/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_LocalDomain_GetDefaultHierarchyRelationType]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_LocalDomain_GetDefaultHierarchyRelationType]
GO


/*===========================================================================*\
  Description:	Determine the default hierarchy relation type associated
				with a local domain (if any).

  Parameters:   @local_domain_key	Local domain key
		@hierarchy_relation_type_key
				[on exit] Hierarchy relation type key

  Created:	Feb 2004

  Last revision information:
	$Revision: 1 $
	$Date: 2/03/04 16:57 $
	$Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_LocalDomain_GetDefaultHierarchyRelationType]
	@local_domain_key CHAR(16),
	@hierarchy_relation_type_key CHAR(16) OUTPUT
AS
	SET NOCOUNT ON

	SELECT		@hierarchy_relation_type_key = d.Default_Hierarchy_Relation_Type_Key
	FROM		Local_Domain AS	ld
	INNER JOIN	Domain AS d
	ON		d.Domain_Key		=	ld.Domain_Key
	WHERE		ld.Local_Domain_Key	=	@local_domain_key

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_LocalDomain_GetDefaultHierarchyRelationType') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_LocalDomain_GetDefaultHierarchyRelationType'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_LocalDomain_GetDefaultHierarchyRelationType TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_LocalDomain_GetDefaultHierarchyRelationType TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_LocalDomain_GetDefaultHierarchyRelationType TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_LocalDomain_GetDefaultHierarchyRelationType TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_LocalDomain_GetDefaultHierarchyRelationType TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_LocalDomain_GetDefaultHierarchyRelationType TO [Dev - JNCC SQL]
END

GO