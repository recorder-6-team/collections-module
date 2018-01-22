/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_QEGeneralDataRowKey_Get_ForSession]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_QEGeneralDataRowKey_Get_ForSession]
GO

/*===========================================================================*\
  Description:	Retrieves the General tab's row key for a session 

  Parameters:	@Key - QE_Session_Key
							@Output OUTPUT - QE_Data_Row_Key

  Created:	Jan 2004

  Last revision information:
    $Revision: 1 $
    $Date: 27/01/04 15:57 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_QEGeneralDataRowKey_Get_ForSession]
	@Key INT,
	@Output INT OUTPUT
AS
  
SELECT @Output=QE_Data_Row_Key
FROM QE_Data_Row
WHERE QE_Session_Key=@Key
AND General=1

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_QEDataItem_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_QEGeneralDataRowKey_Get_ForSession'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_QEGeneralDataRowKey_Get_ForSession TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_QEGeneralDataRowKey_Get_ForSession TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_QEGeneralDataRowKey_Get_ForSession TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_QEGeneralDataRowKey_Get_ForSession TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_QEGeneralDataRowKey_Get_ForSession TO [Dev - JNCC SQL]
END

GO
