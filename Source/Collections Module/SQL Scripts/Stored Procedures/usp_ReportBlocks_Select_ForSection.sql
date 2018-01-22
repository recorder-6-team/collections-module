/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ReportBlocks_Select_ForSection]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ReportBlocks_Select_ForSection]
GO

/*===========================================================================*\
  Description:	Returns a list of the report block in a details report section.

  Parameters:	@Key - report section key

  Created:	Aug 2004

  Last revision information:
    $Revision: 3 $
    $Date: 5/10/04 13:26 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ReportBlocks_Select_ForSection]
@Key CHAR(16)
AS

SET NOCOUNT ON

SELECT 
		RB.Report_Block_Key,
		RBIS.Title,
		RB.Header_File,
		RB.Row_File,
		RB.Footer_File,
		RBIS.Population_SQL,
		RBIS.Population_SQL_Record_Count
FROM Report_Block_In_Section RBIS
INNER JOIN Report_Block RB
		ON RB.Report_Block_Key=RBIS.Report_Block_Key
WHERE RBIS.Report_Section_Key=@Key
ORDER BY RBIS.[Sequence]

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ReportBlocks_Select_ForSection') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ReportBlocks_Select_ForSection'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ReportBlocks_Select_ForSection TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ReportBlocks_Select_ForSection TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ReportBlocks_Select_ForSection TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ReportBlocks_Select_ForSection TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ReportBlocks_Select_ForSection TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ReportBlocks_Select_ForSection TO [Dev - JNCC SQL]
END

GO
