/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_References_Select_ForInternalSources]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_References_Select_ForInternalSources]
GO

/*===========================================================================*\
  Description:	Returns information for the 'Internal Documents' grid of
		the Sources tab page

  Parameters:	@Key	

  Created:	November 2003

  Last revision information:
    $Revision: 5 $
    $Date: 2/12/03 17:27 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_References_Select_ForInternalSources]
	@Key char(16),
	@TableName varchar(50)
AS

SET NOCOUNT ON

	SELECT 		SJ.Source_Join_Key AS Item_Key,
			R.Source_Key,
			R.Title AS Document,
			SJ.Original,
			R.Reference_Type AS Ref_Type,
			S.Custodian,
			SJ.Timestamp
	FROM		Source_Join AS SJ
	INNER JOIN	Reference AS R ON R.Source_Key = SJ.Source_Key
	INNER JOIN	Source AS S ON S.Source_Key = SJ.Source_Key AND S.Internal = 1
	WHERE		SJ.Record_Key = @Key
	AND 		SJ.Table_Name = @TableName
	-- Title is a [text] field, and must be converted before being used in ORDER BY clause.
	ORDER BY	Cast(R.Title AS varchar(100))

SET NOCOUNT OFF

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_References_Select_ForInternalSources') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_References_Select_ForInternalSources'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_References_Select_ForInternalSources TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_References_Select_ForInternalSources TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_References_Select_ForInternalSources TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_References_Select_ForInternalSources TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_References_Select_ForInternalSources TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_References_Select_ForInternalSources TO [Dev - JNCC SQL]
END
GO