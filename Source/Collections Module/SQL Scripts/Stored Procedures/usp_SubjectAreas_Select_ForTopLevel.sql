/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_SubjectAreas_Select_ForTopLevel]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_SubjectAreas_Select_ForTopLevel]
GO

/*===========================================================================*\
  Description:	Returns Subject Area records.

  Parameters:	None

  Created:	November 2003

  Last revision information:
    $Revision: 1 $
    $Date: 28/11/03 11:03 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SubjectAreas_Select_ForTopLevel]
AS

SET NOCOUNT ON

	SELECT DISTINCT
		  	SA.Subject_Area_Key AS [Key], 
		  	SA.Item_Name,
			CASE WHEN D.Domain_Key IS NULL THEN 0 ELSE 1 END AS Has_Children
	FROM	  	Subject_Area AS SA
	LEFT JOIN	Domain AS D ON D.Subject_Area_Key = SA.Subject_Area_Key
	ORDER BY  	SA.Item_Name

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SubjectAreas_Select_ForTopLevel') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SubjectAreas_Select_ForTopLevel'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SubjectAreas_Select_ForTopLevel TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SubjectAreas_Select_ForTopLevel TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SubjectAreas_Select_ForTopLevel TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_SubjectAreas_Select_ForTopLevel TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SubjectAreas_Select_ForTopLevel TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SubjectAreas_Select_ForTopLevel TO [Dev - JNCC SQL]
END

GO