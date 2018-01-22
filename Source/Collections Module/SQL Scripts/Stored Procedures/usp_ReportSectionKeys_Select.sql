If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_ReportSectionKeys_Select]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_ReportSectionKeys_Select]
GO

CREATE PROCEDURE [dbo].[usp_ReportSectionKeys_Select] 
@ReportBlockInSectionKey CHAR(16)

AS

--  DESCRIPTION
--  Returns the Section_List_SQL for a specified section
--
--  PARAMETERS
--	NAME						DESCRIPTION
--	@ReportBlockInSectionKey	Specifies the section that contains the report blocks
--
--
--  AUTHOR:     				Ben Collier, Dorset Software
--  CREATED:    				2004-08-01
--
SET NOCOUNT ON

SELECT RS.Section_List_SQL
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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ReportSectionKeys_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ReportSectionKeys_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ReportSectionKeys_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ReportSectionKeys_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ReportSectionKeys_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ReportSectionKeys_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ReportSectionKeys_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ReportSectionKeys_Select TO [Dev - JNCC SQL]
END

GO
