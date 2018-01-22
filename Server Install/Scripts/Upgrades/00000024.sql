IF Exists(SELECT * FROM SysColumns WHERE Name = 'Is_Measurement' AND Id = Object_Id('QE_Template_Field'))
	EXEC sp_Rename 'QE_Template_Field.Is_Measurement', 'Is_Custom', 'COLUMN'

ALTER TABLE QE_Template_Field
ALTER COLUMN Is_Custom TINYINT
GO

IF NOT Exists(SELECT * FROM SysColumns WHERE Name = 'Number_Type_Concept_Key' AND Id = Object_Id('QE_Template_Field'))
	ALTER TABLE QE_Template_Field
	ADD Number_Type_Concept_Key char(16) NULL
GO
IF NOT Exists(SELECT * FROM SysColumns WHERE Name = 'Number_Preferred' AND Id = Object_Id('QE_Template_Field'))
	ALTER TABLE QE_Template_Field
	ADD Number_Preferred bit NULL

GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_QETemplateField_Update]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_QETemplateField_Update]
GO
    
/*===========================================================================*\
  Description:	

  Parameters:	

  Created:	August 2003

  Last revision information:
    $Revision: 5 $
    $Date: 2/02/09 16:53 $
    $Author: Pauldavies $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_QETemplateField_Update]
 	@Key as char(16),
	@Timestamp as timestamp,
	@GeneralTab as bit,
	@SpecimenTab as bit,
	@ItemName as varchar(100),
	@DefaultValue as varchar(200),
	@DefaultDisplay as varchar(200),
	@SessionID as varchar(16),
	@Sequence int,
	@NumberTypeConceptKey as varchar(16),
	@NumberPreferred as bit
AS

Set Nocount off

update QE_Template_Field
	set 
	General_Tab = @GeneralTab,
	Specimen_Tab = @SpecimenTab,
	Item_Name = @ItemName,
	Default_Value = @DefaultValue,
	Default_Display = @DefaultDisplay,
	Changed_Session_ID= @SessionID,
	Sequence = @Sequence,
	Number_Type_Concept_Key = @NumberTypeConceptKey,
	Number_Preferred = @NumberPreferred
	where QE_Template_Field_Key= @Key and
	(timestamp = @Timestamp)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_QETemplateField_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_QETemplateField_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_QETemplateField_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_QETemplateField_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_QETemplateField_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_QETemplateField_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_QETemplateField_Update TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_QETemplateField_Select_ForTemplate') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_QETemplateField_Select_ForTemplate]
GO

/*===========================================================================*\
  Description:	Selects the fields for a template

  Parameters:	@QETemplateKey - QE_Template_Key
		@TemplateType - see template type field description

  Created:	Jan 2004

  Last revision information:
    $Revision: 5 $
    $Date: 2/02/09 16:53 $
    $Author: Pauldavies $

\*===========================================================================*/    
CREATE PROCEDURE [dbo].[usp_QETemplateField_Select_ForTemplate]
	@QETemplateKey 	char(16),
	@TemplateType 	tinyint
AS

SET NOCOUNT ON

	SELECT  	F.Item_Name AS 'Field', 
			General_Tab,
		 	Specimen_Tab,
			TF.Item_Name AS 'Alternative_Name', 
			Default_Value, 
			F.Default_Size,
			TF.QE_Template_Field_Key, 
			TF.Timestamp,
			Data_Type, 
			F.QE_Field_Key,
			F.Field_Lookup_Key,
			Default_Display,
			Is_Custom,
			NULL AS Measurement_Applies_To,			-- So we get columns named
			NULL AS Measurement_Method_Concept_Key,
			NULL AS Measurement_Duration,
			NULL AS Measurement_Accuracy,
			NULL AS Measurement_Parameter_Concept_Key,
			NULL AS Measurement_Unit_Concept_Key,
			NULL AS Measurement_Is_Specimen,
			NULL AS Measurement_Is_TaxonData,
			NULL AS Taxon_Measurement_Qualifier_Key,
			NULL AS Taxon_Measurement_Unit_Key,
			Field_Name,
			Table_Name,
			isnull(TF.Sequence, (select count(*) from QE_Template_Field)),
			NULL AS Number_Type_Concept_Key,
			NULL AS Number_Preferred
		
	FROM  		QE_Field F 
	LEFT JOIN 	QE_Template_Field TF ON F.QE_Field_Key = TF.QE_Field_Key AND @QETemplateKey = TF.QE_Template_Key AND Is_Custom = 0
	WHERE 		(Template_Type & @TemplateType) <> 0

	UNION -- Measurements for Thesaurus determinations

	SELECT 	CT.Plaintext + ' (' + D.Item_Name + ')' COLLATE SQL_Latin1_General_CP1_CI_AS, 
		General_Tab,
		Specimen_Tab, 
		F.Item_Name, 
		Default_Value,
		20, 
		QE_Template_Field_Key, 
		F.Timestamp,
		0, 
		NULL,
		'',
		Default_Display,
	 	Is_Custom,
		Measurement_Applies_To,
		Measurement_Method_Concept_Key,
		Measurement_Duration,
		Measurement_Accuracy,
		Measurement_Parameter_Concept_Key,
		Measurement_Unit_Concept_Key,
		Measurement_Is_Specimen,
		Measurement_Is_TaxonData,
		Taxon_Measurement_Qualifier_Key,
		Taxon_Measurement_Unit_Key,
		NULL,
		NULL,
		isnull(F.Sequence, (select count(*) from QE_Template_Field)),
		NULL AS Number_Type_Concept_Key,
		NULL AS Number_Preferred

	FROM	QE_Template_Field F
	JOIN	vw_ConceptTerm CT ON Concept_Key = F.Measurement_Parameter_Concept_Key
	JOIN 	Concept_Group CG ON CT.Concept_Group_Key = CG.Concept_Group_Key
	JOIN 	Local_Domain LD ON LD.Local_Domain_Key = CG.Local_Domain_Key
	JOIN 	Domain D ON D.Domain_Key = LD.Domain_Key AND (D.Has_Occurrences = 1 OR D.Domain_Key = 'SYSTEM00000000')
	WHERE 	Is_Custom = 1 AND QE_Template_Key = @QETemplateKey AND Measurement_Is_TaxonData = 0

	UNION -- Measurements for Taxon  determinations

	SELECT 	MT.Short_Name + ' of ' + MQ.Short_Name + ' (' + MU.Short_Name + ')' COLLATE SQL_Latin1_General_CP1_CI_AS, 
		General_Tab,
		Specimen_Tab, 
		F.Item_Name, 
		Default_Value,
		20, 
		QE_Template_Field_Key, 
		F.Timestamp,
		0, 
		NULL,
		'',
		Default_Display,
	 	Is_Custom,
		Measurement_Applies_To,
		NULL,	-- Irrelevant for Taxon
		NULL,	-- Irrelevant for Taxon
		Measurement_Accuracy,
		Measurement_Parameter_Concept_Key,
		Measurement_Unit_Concept_Key,
		Measurement_Is_Specimen,
		Measurement_Is_TaxonData,
		Taxon_Measurement_Qualifier_Key,
		Taxon_Measurement_Unit_Key,
		NULL,
		NULL,
		isnull(F.Sequence, (select count(*) from QE_Template_Field)),
		NULL AS Number_Type_Concept_Key,
		NULL AS Number_Preferred

	FROM	QE_Template_Field F
	JOIN	Measurement_Qualifier MQ ON MQ.Measurement_Qualifier_Key = Taxon_Measurement_Qualifier_Key
	JOIN	Measurement_Unit MU ON MU.Measurement_Unit_Key = Taxon_Measurement_Unit_Key 
	JOIN	Measurement_Type MT ON MT.Measurement_Type_Key = MU.Measurement_Type_Key
	WHERE 	Is_Custom = 1 AND QE_Template_Key = @QETemplateKey AND Measurement_Is_Specimen = 0 AND Measurement_Is_TaxonData = 1

	UNION --Numbers

	SELECT 	CT.Plaintext COLLATE SQL_Latin1_General_CP1_CI_AS, 
		General_Tab,
		Specimen_Tab, 
		F.Item_Name, 
		Default_Value,
		20, 
		QE_Template_Field_Key, 
		F.Timestamp,
		0, 
		NULL,
		'',
		Default_Display,
	 	Is_Custom,
		NULL,
		NULL,
		NULL,
		NULL,
		NULL,
		NULL,
		NULL,
		NULL,
		NULL,
		NULL,
		NULL,
		NULL,
		isnull(F.Sequence, (select count(*) from QE_Template_Field)),
		F.Number_Type_Concept_Key,
		F.Number_Preferred

	FROM	QE_Template_Field F
	JOIN	vw_ConceptTerm CT ON Concept_Key = F.Number_Type_Concept_Key
	WHERE 	Is_Custom = 2 AND QE_Template_Key = @QETemplateKey

	ORDER BY 26
GO 

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_QETemplateField_Select_ForTemplate') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_QETemplateField_Select_ForTemplate'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_QETemplateField_Select_ForTemplate TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_QETemplateField_Select_ForTemplate TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_QETemplateField_Select_ForTemplate TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_QETemplateField_Select_ForTemplate TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_QETemplateField_Select_ForTemplate TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_QETemplateField_Select_ForTemplate TO [Dev - JNCC SQL]
END
GO

IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_QETemplateField_Insert') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_QETemplateField_Insert]
GO
    
/*===========================================================================*\
  Description:	Inserts a record in QE_Template_Field table

  Parameters:	Table's fields.

  Created:	August 2003

  Last revision information:
    $Revision: 5 $
    $Date: 2/02/09 16:53 $
    $Author: Pauldavies $

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
	@NumberPreferred			BIT = NULL
AS

SET NOCOUNT OFF

	EXEC spNextKey 'QE_Template_Field', @Key OUTPUT

	INSERT INTO QE_Template_Field (
		QE_Template_Field_Key, QE_Template_Key, QE_Field_Key, General_Tab, Specimen_Tab,
		Item_Name, Default_Value, Default_Display, Entered_Session_ID, System_Supplied_Data, Is_Custom,
		Measurement_Applies_To, Measurement_Method_Concept_Key, Measurement_Duration, Measurement_Accuracy,
		Measurement_Parameter_Concept_Key, Measurement_Unit_Concept_Key, Measurement_Is_Specimen,
		Measurement_Is_TaxonData, Taxon_Measurement_Qualifier_Key, Taxon_Measurement_Unit_Key, [Sequence],
		Number_Type_Concept_Key, Number_Preferred
	) VALUES (
		@Key, @QETemplateKey, @QEFieldKey, @GeneralTab, @SpecimenTab,
		@ItemName, @DefaultValue, @DefaultDisplay, @SessionID, 0, @IsCustom,
		@MeasurementAppliesTo, @MeasurementMethodConceptKey, @MeasurementDuration, @MeasurementAccuracy,
		@MeasurementParameterConceptKey, @MeasurementUnitConceptKey, @MeasurementIsSpecimen, 
		@MeasurementIsTaxonData, @TaxonMeasurementQualifierKey, @TaxonMeasurementUnitKey, @Sequence,
		@NumberTypeConceptKey, @NumberPreferred
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