/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_AvailableReports_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_AvailableReports_Select]
GO

/*===========================================================================*\
  Description:	Returns the reports available for the supplied item and
    folders.  Report_Source column indicates which item or list the report is 
							appropriate to - 0=Item, 1=Top level list, 2=Folder list

  Parameters:	@SelectedItemKey - key of selected node
    					@SelectedItemTable - table of selected node
    					@TopLevelTable - table of the list of top level nodes
    					@TopLevelListCount - count of list of top level nodes
    					@FolderTable - table of list of nodes in current folder
    					@FolderListCount - count of list of nodes in current folder

  Created:	Nov 2003

  Last revision information:
    $Revision: 3 $
    $Date: 10/12/03 16:08 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_AvailableReports_Select]
	@SelectedItemKey CHAR(16),
	@SelectedItemTable VARCHAR(100),
	@TopLevelTable VARCHAR(100),
	@TopLevelListCount INT,
	@FolderTable VARCHAR(100),
	@FolderListCount INT
AS

SET NOCOUNT ON


SELECT 'Details' AS ReportType, Details_Report_Key AS Item_Key, Item_Name, 0 AS Report_Source
FROM Details_Report
WHERE Reported_Table=@SelectedItemTable
UNION
SELECT 'TopLevelList' AS ReportType, List_Report_Key AS Item_Key, Item_Name, 1 AS Report_Source
FROM List_Report
WHERE Reported_Table=@TopLevelTable
AND @TopLevelListCount>0
UNION
SELECT 'FolderList' AS ReportType, List_Report_Key AS Item_Key, Item_Name, 2 AS Report_Source
FROM List_Report
WHERE Reported_Table=@FolderTable
AND @FolderListCount>0


SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_AvailableReports_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_AvailableReports_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_AvailableReports_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_AvailableReports_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_AvailableReports_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_AvailableReports_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_AvailableReports_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_AvailableReports_Select TO [Dev - JNCC SQL]
END

GO
