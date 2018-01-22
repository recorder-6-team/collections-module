/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ListReportAndBlock_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ListReportAndBlock_Select]
GO

/*===========================================================================*\
  Description:	Returns information required from a List_Report and the 
		associated Report_Block record.

  Parameters:	@Key	List_Report key

  Created:	August 2003

  Last revision information:
    $Revision: 4 $
    $Date: 20/08/04 14:14 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ListReportAndBlock_Select]
	@Key char(16)
AS

SET NOCOUNT ON

SELECT
	LR.Item_Name AS Report_Title,
	RB.Header_File,
	RB.Row_File,
	RB.Footer_File,
	LR.Population_SQL,
	RB.Report_Block_Key
FROM List_Report LR
INNER JOIN Report_Block RB ON RB.Report_Block_Key=LR.Report_Block_Key
WHERE LR.List_Report_Key=@Key

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ListReportAndBlock_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ListReportAndBlock_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ListReportAndBlock_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ListReportAndBlock_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ListReportAndBlock_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ListReportAndBlock_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ListReportAndBlock_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ListReportAndBlock_Select TO [Dev - JNCC SQL]
END

GO