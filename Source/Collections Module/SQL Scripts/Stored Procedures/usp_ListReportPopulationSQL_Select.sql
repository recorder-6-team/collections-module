If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_ListReportPopulationSQL_Select]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_ListReportPopulationSQL_Select]
GO

CREATE PROCEDURE [dbo].[usp_ListReportPopulationSQL_Select] 
@ReportKey CHAR(16)

AS

--  DESCRIPTION
--  Returns the PopulationSQL for a given report
--
--  PARAMETERS
--	NAME				DESCRIPTION
--	@ReportKey 			List_Report_Key for List Report
--
--
--  AUTHOR:     		Ben Collier, Dorset Software
--  CREATED:    		2004-01-07
--
SET NOCOUNT ON

SELECT Population_SQL
FROM
List_Report
WHERE List_Report_Key = @ReportKey


GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ListReportPopulationSQL_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ListReportPopulationSQL_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ListReportPopulationSQL_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ListReportPopulationSQL_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ListReportPopulationSQL_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ListReportPopulationSQL_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ListReportPopulationSQL_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ListReportPopulationSQL_Select TO [Dev - JNCC SQL]
END

GO
