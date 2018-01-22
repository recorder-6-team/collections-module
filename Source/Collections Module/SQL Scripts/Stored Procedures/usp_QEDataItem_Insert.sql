If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_QEDataItem_Insert]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_QEDataItem_Insert]
GO
    
/*===========================================================================*\
  Description:	

  Parameters:	

  Created:	August 2003

  Last revision information:
    $Revision: 6 $
    $Date: 18/02/15 19:33 $
    $Author: Simonwood $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_QEDataItem_Insert]
  @QEDataRowKey as int,
  @QETemplateFieldKey as char(16),
  @DataValue as varchar(200),
  @DataDisplay as Varchar(200),
  @SessionID as char(16)
 AS

SET NOCOUNT ON

IF NOT EXISTS(SELECT 1 FROM QE_Data_Item 
		WHERE QE_Data_Row_Key=@QEDataRowKey
		AND QE_Template_Field_Key=@QETemplateFieldKey)
BEGIN
	INSERT INTO QE_Data_Item (QE_Data_Row_Key, QE_Template_Field_Key,
			Data_Value, Data_Display, Entered_Session_ID)
		VALUES(@QEDataRowKey, @QETemplateFieldKey, @DataValue, @DataDisplay, @SessionID)

	SELECT QE_Data_Item_Key, Timestamp 
	FROM QE_Data_Item 
	WHERE	QE_Data_Item_Key = Scope_Identity()
END
ELSE 
BEGIN
  -- Item already exists so just update it
	DECLARE @QEDataItemKey CHAR(16)

  SELECT @QEDataItemKey=QE_Data_Item_Key
	FROM QE_Data_Item
	WHERE QE_Data_Row_Key=@QEDataRowKey
		AND QE_Template_Field_Key=@QETemplateFieldKey
	
	UPDATE QE_Data_Item
		SET Data_Value = @DataValue,
		Data_Display = @DataDisplay,
		Changed_Session_ID = @SessionID
	WHERE	 QE_Data_Item_Key = @QEDataItemKey
	
	SELECT QE_Data_Item_Key, Timestamp 
	FROM QE_Data_Item 
	WHERE	QE_Data_Item_Key = @QEDataItemKey
END

Update QE_Session
		set Changed_Session_ID = @SessionID
From QE_Data_Row QR
INNER JOIN QE_Session QS ON QS.QE_Session_Key = QR.QE_Session_Key
	where 
		QE_Data_Row_Key = @QEDataRowKey

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_QEDataItem_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_QEDataItem_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_QEDataItem_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_QEDataItem_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_QEDataItem_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_QEDataItem_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_QEDataItem_Insert TO [Dev - JNCC SQL]
END

GO