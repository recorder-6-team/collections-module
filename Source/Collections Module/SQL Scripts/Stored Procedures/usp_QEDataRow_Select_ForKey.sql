/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_QEDataRow_Select_ForKey]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_QEDataRow_Select_ForKey]
GO

/*===========================================================================*\
  Description:	Retrieves a single data row

  Parameters:	@Key - QE_Data_Row_key

  Created:	Jan 2004

  Last revision information:
    $Revision: 3 $
    $Date: 27/01/04 16:06 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_QEDataRow_Select_ForKey]
  @QEDataRowKey as int
 AS

SET NOCOUNT OFF

SELECT
	Validated,
	Processed,
	Timestamp 
FROM QE_Data_Row
WHERE QE_Data_Row_Key = @QEDataRowKey
	
GO 

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_QEDataRow_Select_ForKey') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_QEDataRow_Select_ForKey'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_QEDataRow_Select_ForKey TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_QEDataRow_Select_ForKey TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_QEDataRow_Select_ForKey TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_QEDataRow_Select_ForKey TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_QEDataRow_Select_ForKey TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_QEDataRow_Select_ForKey TO [Dev - JNCC SQL]
END

GO
