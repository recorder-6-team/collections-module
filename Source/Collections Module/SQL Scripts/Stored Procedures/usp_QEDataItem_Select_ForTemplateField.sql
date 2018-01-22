/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_QEDataItem_Select_ForTemplateField]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_QEDataItem_Select_ForTemplateField]
GO

/*===========================================================================*\
  Description:	Selects all the data from the QE_Data_Item table for
				the specified template field and data row.

  Parameters:	@TemplateFieldKey	Template Field key
				@DataRowKey			The data row key

  Created:	October 2007

  Last revision information:
    $Revision: 2 $
    $Date: 29/11/07 10:40 $
    $Author: Davidkelly $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_QEDataItem_Select_ForTemplateField]
	@TemplateFieldKey char(16),
	@DataRowKey int
AS
	SELECT 	QE_Data_Item_Key,
			QE_Data_Row_Key,
			QE_Template_Field_Key,
			Data_Value,
			Data_Display,
			Position,
			[Timestamp],
			NULL as Custodian
	FROM QE_Data_Item
	WHERE	QE_Template_Field_Key = @TemplateFieldKey
	AND		QE_Data_Row_Key = @DataRowKey
	ORDER BY Position
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_QEDataItem_Select_ForTemplateField') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_QEDataItem_Select_ForTemplateField'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_QEDataItem_Select_ForTemplateField TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_QEDataItem_Select_ForTemplateField TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_QEDataItem_Select_ForTemplateField TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_QEDataItem_Select_ForTemplateField TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_QEDataItem_Select_ForTemplateField TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_QEDataItem_Select_ForTemplateField TO [Dev - JNCC SQL]
END

GO