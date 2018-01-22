/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_QETemplates_Select_Available]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_QETemplates_Select_Available]
GO

/*===========================================================================*\
  Description:	Returns the list of quick entry templates available.

  Created:	October 2003

  Last revision information:
    $Revision: 1 $
    $Date: 24/02/04 13:10 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_QETemplates_Select_Available]
@UserDomainMask int
AS

SET NOCOUNT ON

SELECT DISTINCT
	Q.QE_Template_Key, 
	Q.Item_Name, 
	Q.Template_Type, 
	Q.Subject_Area_Key, 
	Q.[timestamp]
FROM QE_Template Q 
INNER JOIN Domain D ON (D.Subject_Area_Key=Q.Subject_Area_Key
		AND D.Domain_Mask & @UserDomainMask > 0)
		OR Q.Subject_Area_Key IS NULL
ORDER BY Q.Item_Name

GO 

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_QETemplates_Select_Available') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_QETemplates_Select_Available'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_QETemplates_Select_Available TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_QETemplates_Select_Available TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_QETemplates_Select_Available TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_QETemplates_Select_Available TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_QETemplates_Select_Available TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_QETemplates_Select_Available TO [Dev - JNCC SQL]
END

GO