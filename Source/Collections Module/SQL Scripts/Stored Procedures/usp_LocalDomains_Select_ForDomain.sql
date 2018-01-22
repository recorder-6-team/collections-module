/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_LocalDomains_Select_ForDomain]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_LocalDomains_Select_ForDomain]
GO

/*===========================================================================*\
  Description:	Returns local domain records.

  Parameters:	@DomainKey	Domain key

  Created:	November 2003

  Last revision information:
    $Revision: 1 $
    $Date: 28/11/03 11:02 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_LocalDomains_Select_ForDomain]
	@ParentKey CHAR(16)
AS

SET NOCOUNT ON

	SELECT DISTINCT
		  	LD.Local_Domain_Key AS [Key], 
			LD.Item_Name + ' (' + L.Language_Key + ' - ' + L.Item_Name + ')' AS Item_Name, 
			LD.Domain_Key,
			CASE WHEN CG.Concept_Group_Key IS NULL THEN 0 ELSE 1 END AS Has_Children
	FROM 	  	Local_Domain AS LD
	INNER JOIN	Language AS L ON L.Language_Key = LD.Language_Key	
	LEFT JOIN 	Concept_Group CG ON CG.Local_Domain_Key = LD.Local_Domain_Key	
	WHERE	  	Domain_Key = @ParentKey
	ORDER BY	Item_Name

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_LocalDomains_Select_ForDomain') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_LocalDomains_Select_ForDomain'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_LocalDomains_Select_ForDomain TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_LocalDomains_Select_ForDomain TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_LocalDomains_Select_ForDomain TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_LocalDomains_Select_ForDomain TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_LocalDomains_Select_ForDomain TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_LocalDomains_Select_ForDomain TO [Dev - JNCC SQL]
END

GO