/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_QEDataRow_Select_ForSession]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_QEDataRow_Select_ForSession]
GO

/*===========================================================================*\
  Description: 	Returns all the QE_Data_Rows for a session

  Parameters:	@Key - QE_Data_Row_Key

  Created:	Jan 2004

  Last revision information:
    $Revision: 2 $
    $Date: 19/01/04 9:03 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_QEDataRow_Select_ForSession]
	@Key char(16)
AS

SELECT QE_Data_Row_Key, Validated, Timestamp, Processed 
FROM QE_Data_Row
WHERE QE_Session_Key = @Key
	AND General= 0
ORDER BY QE_Data_Row_Key

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_QEDataRow_Select_ForSession') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_QEDataRow_Select_ForSession'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_QEDataRow_Select_ForSession TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_QEDataRow_Select_ForSession TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_QEDataRow_Select_ForSession TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_QEDataRow_Select_ForSession TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_QEDataRow_Select_ForSession TO [Dev - JNCC SQL]
END

GO