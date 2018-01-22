/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_QEDataItem_Select_ForSession]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_QEDataItem_Select_ForSession]
GO

/*===========================================================================*\
  Description: 	Returns all the QE_Data_Items for a session.

  Parameters:	@Key - QE_Session_Key

  Created:	Jan 2004

  Last revision information:
    $Revision: 3 $
    $Date: 30/01/04 17:09 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_QEDataItem_Select_ForSession]
	@Key char(16)
AS

SELECT 
	DI.QE_Data_Item_Key,
	DR.QE_Data_Row_Key, 
	DI.QE_Template_Field_Key, 
	DI.Data_Value, 
	DI.Data_Display,
	DI.Timestamp
FROM QE_Data_Row DR
INNER JOIN QE_Data_Item DI ON DI.QE_Data_Row_Key=DR.QE_Data_Row_Key
WHERE DR.QE_Session_Key=@Key
AND DR.General= 0
ORDER BY DR.QE_Data_Row_Key

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_QEDataItem_Select_ForSession') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_QEDataItem_Select_ForSession'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_QEDataItem_Select_ForSession TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_QEDataItem_Select_ForSession TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_QEDataItem_Select_ForSession TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_QEDataItem_Select_ForSession TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_QEDataItem_Select_ForSession TO [Dev - JNCC SQL]
END

GO