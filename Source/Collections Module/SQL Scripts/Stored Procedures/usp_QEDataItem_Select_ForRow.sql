SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF OBJECT_ID('dbo.usp_QEDataItem_Select_ForRow') IS NOT NULL
    DROP PROCEDURE dbo.usp_QEDataItem_Select_ForRow
GO

/*===========================================================================*\
  Description: 	Returns all the QE_Data_Items for a row.

  Parameters:	@Key					QE_Data_Row_Key

  Created:	Jan 2004

  Last revision information:
    $Revision: 3 $
    $Date: 12/04/11 11:21 $
    $Author: Andrewkemp $

\*===========================================================================*/
CREATE PROCEDURE dbo.usp_QEDataItem_Select_ForRow
	@Key		CHAR(16)
AS

	SELECT		QE_Data_Item_Key,
				QE_Template_Field_Key, 
				Data_Value, 
				Data_Display,
				Timestamp
	FROM		QE_Data_Item 
	WHERE		QE_Data_Row_Key			=	@Key
	ORDER BY	QE_Template_Field_Key,
				Position
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_QEDataItem_Select_ForRow') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_QEDataItem_Select_ForRow'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_QEDataItem_Select_ForRow TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_QEDataItem_Select_ForRow TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_QEDataItem_Select_ForRow TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_QEDataItem_Select_ForRow TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_QEDataItem_Select_ForRow TO [Dev - JNCC SQL]
END
GO