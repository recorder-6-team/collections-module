/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Domains_Select_ForSubjectArea]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Domains_Select_ForSubjectArea]
GO

/*===========================================================================*\
  Description:	Returns domain records.

  Parameters:	@SubjectAreaKey	Subject area key (optional)

  Created:	November 2003

  Last revision information:
    $Revision: 1 $
    $Date: 28/11/03 11:03 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Domains_Select_ForSubjectArea]
	@ParentKey CHAR(16)
AS

SET NOCOUNT ON

	SELECT DISTINCT
	 	  	D.Domain_Key AS [Key], 
		  	D.Item_Name, 
		  	D.Subject_Area_Key,
			CASE WHEN LD.Local_Domain_Key IS NULL THEN 0 ELSE 1 END AS Has_Children
	FROM 	  	Domain AS D
	LEFT JOIN	Local_Domain AS LD ON LD.Domain_Key = D.Domain_Key
	WHERE	  	D.Subject_Area_Key = @ParentKey
	ORDER BY  	D.Item_Name

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Domains_Select_ForSubjectArea') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Domains_Select_ForSubjectArea'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Domains_Select_ForSubjectArea TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Domains_Select_ForSubjectArea TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Domains_Select_ForSubjectArea TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Domains_Select_ForSubjectArea TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Domains_Select_ForSubjectArea TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Domains_Select_ForSubjectArea TO [Dev - JNCC SQL]
END

GO