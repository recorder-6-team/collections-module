/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_QEDataItem_Insert_ForMultiValues]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_QEDataItem_Insert_ForMultiValues]
GO

/*===========================================================================*\
  Description:	Inserts a record into the QE_Data_Item table

  Parameters:	@Key	Data Item key

  Created:	October 2007

  Last revision information:
    $Revision: 2 $
    $Date: 2/09/11 15:26 $
    $Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_QEDataItem_Insert_ForMultiValues]
	@DataItemKey INT OUTPUT,
	@DataRowKey INT,
	@TemplateFieldKey CHAR(16),
	@DataValue as VARCHAR(200),
	@DataDisplay as VARCHAR(200),
	@Position INT,
	@SessionID as CHAR(16)
AS
INSERT INTO QE_Data_Item (QE_Data_Row_Key, QE_Template_Field_Key,
	Data_Value, Data_Display, Position, Entered_Session_ID)
VALUES(@DataRowKey, @TemplateFieldKey, @DataValue, @DataDisplay, @Position, @SessionID)

SELECT @DataItemKey = SCOPE_IDENTITY()
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_QEDataItem_Insert_ForMultiValues') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_QEDataItem_Insert_ForMultiValues'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_QEDataItem_Insert_ForMultiValues TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_QEDataItem_Insert_ForMultiValues TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_QEDataItem_Insert_ForMultiValues TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_QEDataItem_Insert_ForMultiValues TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_QEDataItem_Insert_ForMultiValues TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_QEDataItem_Insert_ForMultiValues TO [Dev - JNCC SQL]
END

GO