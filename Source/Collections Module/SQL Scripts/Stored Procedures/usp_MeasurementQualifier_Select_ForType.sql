/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_MeasurementQualifier_Select_ForType') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_MeasurementQualifier_Select_ForType]
GO

/*===========================================================================*\
  Description: Returns a list of Measurement Qualifiers for the specified type

  Parameters:  @Type

  Created:     November 2004

  Last revision information:
    $Revision: 1 $
    $Date: 17/11/04 14:35 $
    $Author: Ericsalmon $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_MeasurementQualifier_Select_ForType]
	@Type CHAR(16)
AS
	SELECT	Measurement_Qualifier_Key AS Item_Key,
		Short_Name
	FROM 	Measurement_Qualifier
	WHERE 	Measurement_Type_Key = @Type
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_MeasurementQualifier_Select_ForType') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_MeasurementQualifier_Select_ForType'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_MeasurementQualifier_Select_ForType TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_MeasurementQualifier_Select_ForType TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_MeasurementQualifier_Select_ForType TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_MeasurementQualifier_Select_ForType TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_MeasurementQualifier_Select_ForType TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_MeasurementQualifier_Select_ForType TO [Dev - JNCC SQL]
END
GO