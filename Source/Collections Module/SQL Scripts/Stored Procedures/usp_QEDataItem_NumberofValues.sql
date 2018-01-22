/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_QEDataItem_NumberofValues]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_QEDataItem_NumberofValues]
GO

/*===========================================================================*\
  Description:	gets the number of values a multi-value field has

  Parameters:	@TemplateFieldKey	Data Item Template Field key
				@DataRowKey			Row Key of the row the item is on

  Created:	October 2007

  Last revision information:
    $Revision: 2 $
    $Date: 29/11/07 10:38 $
    $Author: Davidkelly $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_QEDataItem_NumberofValues]
	@TemplateFieldKey CHAR(16),
	@DataRowKey INT,
	@Count INT OUTPUT
AS
SELECT 
	@Count = count(*)
from
QE_Data_Item
where	QE_Template_Field_Key = @TemplateFieldKey
and		QE_Data_Row_Key = @DataRowKey

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_QEDataItem_NumberofValues') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_QEDataItem_NumberofValues'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_QEDataItem_NumberofValues TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_QEDataItem_NumberofValues TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_QEDataItem_NumberofValues TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_QEDataItem_NumberofValues TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_QEDataItem_NumberofValues TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_QEDataItem_NumberofValues TO [Dev - JNCC SQL]
END

GO