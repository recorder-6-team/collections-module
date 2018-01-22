IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_QETemplateField_Insert') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_QETemplateField_Insert]
GO
    
/*===========================================================================*\
  Description:	Inserts a record in QE_Template_Field table

  Parameters:	Table's fields.

  Created:	August 2003

  Last revision information:
    $Revision: 8 $
    $Date: 13/09/07 12:55 $
    $Author: Davidkelly $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_QETemplateField_Insert]
	@Key 				CHAR(16) OUTPUT,
	@QETemplateKey 			CHAR(16),
	@QEFieldKey 			CHAR(16) = NULL,
	@GeneralTab 			BIT,
	@SpecimenTab 			BIT,
	@ItemName 			VARCHAR(100),
	@DefaultValue 			VARCHAR(200),
	@DefaultDisplay 		VARCHAR(200),
	@SessionID 			VARCHAR(16),
	@IsCustom 			TINYINT,
	@MeasurementAppliesTo 		VARCHAR(50) = NULL,
	@MeasurementMethodConceptKey 	CHAR(16) = NULL,
	@MeasurementDuration 		VARCHAR(50) = NULL,
	@MeasurementAccuracy 		VARCHAR(50) = NULL,
	@MeasurementParameterConceptKey CHAR(16) = NULL,
	@MeasurementunitConceptKey 	CHAR(16) = NULL,
	@MeasurementIsSpecimen 		BIT = NULL,
	@TaxonMeasurementQualifierKey	CHAR(16) = NULL,
	@TaxonMeasurementUnitKey	CHAR(16) = NULL,
	@MeasurementIsTaxonData		BIT = NULL,
	@Sequence 			INT = 0,
	@NumberTypeConceptKey		CHAR(16) = NULL,
	@NumberPreferred			BIT = NULL,
	@Hidden				BIT,
	@Locked				BIT,
	@MetadataTypeKey	CHAR(16) = NULL
AS

SET NOCOUNT OFF

	EXEC spNextKey 'QE_Template_Field', @Key OUTPUT

	INSERT INTO QE_Template_Field (
		QE_Template_Field_Key, QE_Template_Key, QE_Field_Key, General_Tab, Specimen_Tab,
		Item_Name, Default_Value, Default_Display, Entered_Session_ID, System_Supplied_Data, Is_Custom,
		Measurement_Applies_To, Measurement_Method_Concept_Key, Measurement_Duration, Measurement_Accuracy,
		Measurement_Parameter_Concept_Key, Measurement_Unit_Concept_Key, Measurement_Is_Specimen,
		Measurement_Is_TaxonData, Taxon_Measurement_Qualifier_Key, Taxon_Measurement_Unit_Key, [Sequence],
		Number_Type_Concept_Key, Number_Preferred, Hidden, Locked, Metadata_Type_Key
	) VALUES (
		@Key, @QETemplateKey, @QEFieldKey, @GeneralTab, @SpecimenTab,
		@ItemName, @DefaultValue, @DefaultDisplay, @SessionID, 0, @IsCustom,
		@MeasurementAppliesTo, @MeasurementMethodConceptKey, @MeasurementDuration, @MeasurementAccuracy,
		@MeasurementParameterConceptKey, @MeasurementUnitConceptKey, @MeasurementIsSpecimen, 
		@MeasurementIsTaxonData, @TaxonMeasurementQualifierKey, @TaxonMeasurementUnitKey, @Sequence,
		@NumberTypeConceptKey, @NumberPreferred, @Hidden, @Locked, @MetadataTypeKey
	)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_QETemplateField_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_QETemplateField_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_QETemplateField_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_QETemplateField_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_QETemplateField_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_QETemplateField_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_QETemplateField_Insert TO [Dev - JNCC SQL]
END
GO