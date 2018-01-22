/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_DetailsReport_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_DetailsReport_Select]
GO

/*===========================================================================*\
  Description:	Returns information required from a Details_Report

  Parameters:	@Key	Details_Report key

  Created:	January 2004

  Last revision information:
    $Revision: 1 $
    $Date: 8/01/04 10:31 $
    $Author: Bencollier $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_DetailsReport_Select]
	@Key char(16)
AS

SET NOCOUNT ON

SELECT
	Item_Name AS Report_Title
FROM Details_Report 
WHERE Details_Report_Key=@Key

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_DetailsReport_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_DetailsReport_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_DetailsReport_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_DetailsReport_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_DetailsReport_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_DetailsReport_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_DetailsReport_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_DetailsReport_Select TO [Dev - JNCC SQL]
END

GO