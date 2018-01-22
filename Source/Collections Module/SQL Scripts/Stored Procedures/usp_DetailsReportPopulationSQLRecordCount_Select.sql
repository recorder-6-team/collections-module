If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_DetailsReportPopulationSQLRecordCount_Select]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_DetailsReportPopulationSQLRecordCount_Select]
GO

CREATE PROCEDURE [dbo].[usp_DetailsReportPopulationSQLRecordCount_Select] 
@ReportBlockInSectionKey CHAR(16)

AS

--  DESCRIPTION
--  Returns the Population_SQL_Record_Count for a given report
--
--  PARAMETERS
--	NAME				DESCRIPTION
--	@ReportKey 			Details_Report_Key for Details Report
--
--
--  AUTHOR:     		Ben Collier, Dorset Software
--  CREATED:    		2004-01-13
--
SET NOCOUNT ON

SELECT RBIS.Population_SQL_Record_Count
FROM
	Report_Block_In_Section RBIS
	INNER JOIN
		Report_Section RS
	ON RBIS.Report_Section_Key = RS.Report_Section_Key
WHERE RBIS.Report_Block_In_Section_Key = @ReportBlockInSectionKey


GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_DetailsReportPopulationSQLRecordCount_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_DetailsReportPopulationSQLRecordCount_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_DetailsReportPopulationSQLRecordCount_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_DetailsReportPopulationSQLRecordCount_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_DetailsReportPopulationSQLRecordCount_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_DetailsReportPopulationSQLRecordCount_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_DetailsReportPopulationSQLRecordCount_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_DetailsReportPopulationSQLRecordCount_Select TO [Dev - JNCC SQL]
END

GO
