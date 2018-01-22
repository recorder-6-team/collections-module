SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

/*============================================================================*\
  Drop stored proc before re-creating.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_QEDataItem_DeleteForMultiValues') IS NOT NULL
	DROP PROCEDURE dbo.usp_QEDataItem_DeleteForMultiValues
GO

/*============================================================================*\	
  Description:

  Parameters:

  Created:		November 2009

  Last revision information:
	$Revision: 1 $
	$Date: 23/11/09 16:45 $
	$Author: Simonwood $		
\*============================================================================*/
CREATE PROCEDURE dbo.usp_QEDataItem_DeleteForMultiValues
	@DataRowKey							INT,
	@TemplateFieldKey					CHAR(16)
AS
	DELETE FROM	dbo.QE_Data_Item
	WHERE		QE_Data_Row_Key			=	@DataRowKey
	AND			QE_Template_Field_Key	=	@TemplateFieldKey
GO

/*============================================================================*\
  Grant permissions.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_QEDataItem_DeleteForMultiValues') IS NOT NULL
BEGIN
	PRINT 'Setting up security on procedure usp_QEDataItem_DeleteForMultiValues'
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_AddOnly')
			GRANT EXECUTE ON dbo.usp_QEDataItem_DeleteForMultiValues TO R2k_AddOnly
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_QEDataItem_DeleteForMultiValues TO R2k_Administrator
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_QEDataItem_DeleteForMultiValues TO R2k_FullEdit
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_QEDataItem_DeleteForMultiValues TO R2k_ReadOnly
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_QEDataItem_DeleteForMultiValues TO R2k_RecordCardsOnly
END
GO
