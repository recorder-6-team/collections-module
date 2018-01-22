/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_References_Select_ForExternalSources]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_References_Select_ForExternalSources]
GO

/*===========================================================================*\
  Description:	Returns information for the 'External References' listbox of
		the Sources tab page

  Parameters:	@Key	

  Created:	November 2003

  Last revision information:
    $Revision: 4 $
    $Date: 2/12/03 17:27 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_References_Select_ForExternalSources]
	@Key char(16),
	@TableName varchar(50)
AS

SET NOCOUNT ON

	SELECT 		-- Source_Key is always unique in Source, Source_File AND Source_Join.
			-- So no need to also return Source_Join_Key. Source_Key is enough.
			SJ.Source_Key AS Item_Key,
			SF.[File_Name] AS Item_Name,
			S.Custodian,
			-- Timestamp is not required, external references for Sources tab can only be 
			-- added or deleted, not modified.
			NULL AS [Timestamp]  
	FROM		Source_Join AS SJ
	INNER JOIN	Source_File AS SF ON SF.Source_Key = SJ.Source_Key
	INNER JOIN	Source AS S ON S.Source_Key = SJ.Source_Key AND S.Internal = 0
	WHERE		SJ.Record_Key = @Key
	AND 		Table_Name = @TableName
	ORDER BY	Item_Name

SET NOCOUNT OFF

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_References_Select_ForExternalSources') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_References_Select_ForExternalSources'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_References_Select_ForExternalSources TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_References_Select_ForExternalSources TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_References_Select_ForExternalSources TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_References_Select_ForExternalSources TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_References_Select_ForExternalSources TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_References_Select_ForExternalSources TO [Dev - JNCC SQL]
END
GO