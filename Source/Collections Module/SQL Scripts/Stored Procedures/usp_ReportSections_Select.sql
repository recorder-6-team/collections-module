/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ReportSections_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ReportSections_Select]
GO

/*===========================================================================*\
  Description:	Returns a list of the report sections in a details report,
			and the SQL used to populate the section repeats.

  Parameters:	@Key - details report key

  Created:	Aug 2004

  Last revision information:
    $Revision: 3 $
    $Date: 19/09/08 16:56 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ReportSections_Select]
	@Key CHAR(16)
AS
	SET NOCOUNT ON

	SELECT 	Report_Section_Key, 
			Section_List_SQL, 
			Item_Name_Macro,
			Section_Header
	FROM 	Report_Section
	WHERE 	Details_Report_Key = @Key
	ORDER BY [Sequence]

	SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ReportSections_Select') AND SysStat & 0xf = 4)
BEGIN
   	PRINT 'Setting up security on procedure usp_ReportSections_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
       	GRANT EXECUTE ON dbo.usp_ReportSections_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ReportSections_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ReportSections_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ReportSections_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ReportSections_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
       	GRANT EXECUTE ON dbo.usp_ReportSections_Select TO [Dev - JNCC SQL]
END
GO
