/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Reference_Select_ForInternalSources]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Reference_Select_ForInternalSources]
GO

/*===========================================================================*\
  Description:	This stored procedure takes in the source key and returns
		information for this record. Unlike 
		usp_References_Select_ForInternalSources,
		usp_Reference_Select_ForInternalSources only returns one
		record and takes the source_key, rather than the record_key.

  Parameters:	@Key	

  Created:	November 2003

  Last revision information:
    $Revision: 3 $
    $Date: 16/04/04 15:17 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Reference_Select_ForInternalSources]
	@Key char(16)
AS

SET NOCOUNT ON

	SELECT 	
			SJ.Source_Join_Key AS Item_Key,
			R.Source_Key,
			R.Full_Reference,
			R.Title AS Document,
			SJ.Original,			
			R.Reference_Type AS Ref_Type
	FROM		Reference AS R

			-- A left join is used because not all reference records
			-- will have a record in the Source_Join table referring
			-- to it.
	LEFT JOIN	Source_Join AS SJ ON SJ.Source_Key = R.Source_Key
	WHERE		R.Source_Key = @Key

SET NOCOUNT OFF

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Reference_Select_ForInternalSources') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Reference_Select_ForInternalSources'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Reference_Select_ForInternalSources TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Reference_Select_ForInternalSources TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Reference_Select_ForInternalSources TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Reference_Select_ForInternalSources TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Reference_Select_ForInternalSources TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Reference_Select_ForInternalSources TO [Dev - JNCC SQL]
END

GO