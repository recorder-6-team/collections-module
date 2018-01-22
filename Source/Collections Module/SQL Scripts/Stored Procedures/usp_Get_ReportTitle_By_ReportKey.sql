If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Get_ReportTitle_By_ReportKey]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Get_ReportTitle_By_ReportKey]
GO

CREATE PROCEDURE [dbo].[usp_Get_ReportTitle_By_ReportKey] 
@ReportKey CHAR(16),
@IsListReport BIT,
@ReportTitle VARCHAR(100) OUTPUT

AS

--  DESCRIPTION
--  Returns the Title of a given report
--
--  PARAMETERS
--	NAME				DESCRIPTION
--	@ReportKey			Report Key
--
--
--  AUTHOR:				Ben Collier, Dorset Software
--  CREATED:			2004-01-07
--
SET NOCOUNT ON

IF @IsListReport = 1
	SELECT @ReportTitle = Item_Name 
	FROM List_Report
	WHERE List_Report_Key = @ReportKey
ELSE
	SELECT @ReportTitle = Item_Name 
	FROM Details_Report
	WHERE Details_Report_Key = @ReportKey

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Get_ReportTitle_By_ReportKey') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Get_ReportTitle_By_ReportKey'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Get_ReportTitle_By_ReportKey TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Get_ReportTitle_By_ReportKey TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Get_ReportTitle_By_ReportKey TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Get_ReportTitle_By_ReportKey TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Get_ReportTitle_By_ReportKey TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Get_ReportTitle_By_ReportKey TO [Dev - JNCC SQL]
END

GO