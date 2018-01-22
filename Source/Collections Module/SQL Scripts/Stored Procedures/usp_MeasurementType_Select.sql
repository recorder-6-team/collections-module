/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_MeasurementType_Select') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_MeasurementType_Select]
GO

/*===========================================================================*\
  Description: Returns a list of Measurement Types

  Parameters:  @DataTable

  Created:     November 2004

  Last revision information:
    $Revision: 1 $
    $Date: 17/11/04 14:35 $
    $Author: Ericsalmon $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_MeasurementType_Select]
	@DataTable VARCHAR(30)
AS
	SELECT		MT.Measurement_Type_Key AS Item_Key,
			MT.Short_Name
	FROM		Measurement_Type MT
	LEFT JOIN	Measurement_Type_Context MTC ON MTC.Measurement_Type_Key = MT.Measurement_Type_Key
	LEFT JOIN	Measurement_Context MC ON MC.Measurement_Context_Key = MTC.Measurement_Context_Key
	WHERE		MC.Data_Table = @DataTable
	OR		MC.Data_Table IS NULL
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_MeasurementType_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_MeasurementType_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_MeasurementType_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_MeasurementType_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_MeasurementType_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_MeasurementType_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_MeasurementType_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_MeasurementType_Select TO [Dev - JNCC SQL]
END
GO