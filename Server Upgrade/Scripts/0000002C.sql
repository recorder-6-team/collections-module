SET QUOTED_IDENTIFIER ON
GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_ConceptKey_Get_ForTermKey]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_ConceptKey_Get_ForTermKey]
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

CREATE PROCEDURE [dbo].usp_ConceptKey_Get_ForTermKey 
	@Key char(16),
	@ConceptKey char(16) OUTPUT

AS

--  DESCRIPTION
--  Insert a record into Survey_Event_Geo_Area
--
--	PARAMETERS
--	NAME				DESCRIPTION
--	@Key				The Term_Key of the concept
--  @SurveyEventKey		The Concept_Key of the concept
--
--
--  AUTHOR:				David Kelly, Dorset Software
--  CREATED:			2007-09-07
--


	-- Required to be able to get number of changed records.
	SET NOCOUNT ON

	SELECT 	@ConceptKey = Concept_Key
	FROM
	Concept
	WHERE	Term_Key = @Key


GO
/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptKey_Get_ForTermKey') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptKey_Get_ForTermKey'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptKey_Get_ForTermKey TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptKey_Get_ForTermKey TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptKey_Get_ForTermKey TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ConceptKey_Get_ForTermKey TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptKey_Get_ForTermKey TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptKey_Get_ForTermKey TO [Dev - JNCC SQL]
END

GO



If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_MetadataType_Select]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_MetadataType_Select]
GO

/*===========================================================================*\
  Description:	Returns field data from Recorder for a specified Specimen

  Parameters:	
	@TableName	The name of the table

  Created:	September 2007

  Last revision information:
    $Revision: 3 $
    $Date: 2/02/09 16:55 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_MetadataType_Select] 
	@TableName VARCHAR(50)
AS
SELECT
	Metadata_Type_Key,
	Item_Name,
	Table_Name,
	Description,
	Custodian,
	[Timestamp]
FROM Metadata_Type
WHERE Table_Name = @TableName
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_MetadataType_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_MetadataType_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_MetadataType_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_MetadataType_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_MetadataType_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_MetadataType_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_MetadataType_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_MetadataType_Select TO [Dev - JNCC SQL]
END
GO

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
    $Revision: 3 $
    $Date: 2/02/09 16:55 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_QEDataItem_Insert_ForMultiValues]
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
  Description:	Inserts a record into the QE_Data_Item table

  Parameters:	@Key	Data Item key

  Created:	October 2007

  Last revision information:
    $Revision: 3 $
    $Date: 2/02/09 16:55 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_QEDataItem_NumberofValues]
	@Key CHAR(16),
	@Count INT OUTPUT
AS
SELECT 
	@Count = count(*)
from
QE_Data_Item
where QE_Template_Field_Key = @Key

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
				the specified template field.

  Parameters:	@Key	Template Field key

  Created:	October 2007

  Last revision information:
    $Revision: 3 $
    $Date: 2/02/09 16:55 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_QEDataItem_Select_ForTemplateField]
	@Key char(16)
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
	WHERE QE_Template_Field_Key = @Key
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

IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_QETemplateField_Insert') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_QETemplateField_Insert]
GO
    
/*===========================================================================*\
  Description:	Inserts a record in QE_Template_Field table

  Parameters:	Table's fields.

  Created:	August 2003

  Last revision information:
    $Revision: 3 $
    $Date: 2/02/09 16:55 $
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
    $Revision: 3 $
    $Date: 2/02/09 16:55 $
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
			NULL AS Number_Preferred,
			Hidden,
			Locked,
			NULL AS Metadata_Type_Key 
		
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
		NULL AS Number_Preferred,
		Hidden,
		Locked,
		NULL AS Metadata_Type_Key

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
		NULL AS Number_Preferred,
		Hidden,
		Locked,
		NULL AS Metadata_Type_Key

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
		F.Number_Preferred,
		Hidden,
		Locked,
		NULL AS Metadata_Type_Key

	FROM	QE_Template_Field F
	JOIN	vw_ConceptTerm CT ON Concept_Key = F.Number_Type_Concept_Key
	WHERE 	Is_Custom = 2 AND QE_Template_Key = @QETemplateKey

	UNION --Metadata

	SELECT 	MT.Table_Name + ' ' + MT.Item_Name COLLATE SQL_Latin1_General_CP1_CI_AS, 
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
		NULL AS Number_Type_Concept_Key,
		NULL AS Number_Preferred,
		Hidden,
		Locked,
		F.Metadata_Type_Key

	FROM	QE_Template_Field F
	JOIN	MetaData_Type MT ON MT.MetaData_Type_Key = F.Metadata_Type_Key
	WHERE 	Is_Custom = 3 AND QE_Template_Key = @QETemplateKey

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

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_QETemplateField_Update]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_QETemplateField_Update]
GO
    
/*===========================================================================*\
  Description:	

  Parameters:	

  Created:	August 2003

  Last revision information:
    $Revision: 3 $
    $Date: 2/02/09 16:55 $
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
	@NumberPreferred as bit,
	@Hidden as bit,
	@Locked as bit,
	@MetadataTypeKey as varchar(16)
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
	Number_Preferred = @NumberPreferred,
	Hidden = @Hidden,
	Locked = @Locked,
	Metadata_Type_Key = @MetadataTypeKey
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

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_SurveyEventGeoArea_Delete]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_SurveyEventGeoArea_Delete]
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

CREATE PROCEDURE [dbo].usp_SurveyEventGeoArea_Delete 
@SurveyEventGeoAreaKey CHAR(16)

AS

--  DESCRIPTION
--  Insert a record into Survey_Event_Geo_Area
--
--	PARAMETERS
--	NAME					DESCRIPTION
--	@SurveyEventGeoAreaKey	The Survey_Event_Geo_Area_Key of the record to be deleted
--
--
--  AUTHOR:				David Kelly, Dorset Software
--  CREATED:			2007-09-07
--

	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION

		DELETE	Survey_Event_Geo_Area
		WHERE	Survey_Event_Geo_Area_Key = @SurveyEventGeoAreaKey

		IF @@Error <> 0 GOTO RollbackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION

GO
/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SurveyEventGeoArea_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SurveyEventGeoArea_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SurveyEventGeoArea_Delete TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SurveyEventGeoArea_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SurveyEventGeoArea_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_SurveyEventGeoArea_Delete TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SurveyEventGeoArea_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SurveyEventGeoArea_Delete TO [Dev - JNCC SQL]
END

GO



If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_SurveyEventGeoArea_Insert]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_SurveyEventGeoArea_Insert]
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

CREATE PROCEDURE [dbo].usp_SurveyEventGeoArea_Insert 
@Key CHAR(16) OUTPUT,
@SurveyEventKey CHAR(16),
@ConceptKey CHAR(16),
@EnteredBy CHAR(16)

AS

--  DESCRIPTION
--  Insert a record into Survey_Event_Geo_Area
--
--	PARAMETERS
--	NAME				DESCRIPTION
--	@Key				The Survey_Event_Geo_Area_Key of the new record
--  @SurveyEventKey		The Survey_Event_Key of the new record
--	@Concept_Key		The Concept_Key of the new record
--	@EnteredBy			The user who enetered the record
--
--
--  AUTHOR:				David Kelly, Dorset Software
--  CREATED:			2007-09-07
--


	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	--find out whether this Survey Event Geo Area alreadly exists
	SET @Key = NULL
	SELECT	@Key = Survey_Event_Geo_Area_Key 
	FROM 	Survey_Event_Geo_Area
	WHERE	Concept_Key = @ConceptKey
	AND	Survey_Event_Key = @SurveyEventKey
	
	IF @Key IS NULL
	BEGIN
		BEGIN TRANSACTION
			EXECUTE spNextKey 'Survey_Event_Geo_Area', @Key OUTPUT

			INSERT INTO Survey_Event_GeoArea (
				Survey_Event_Geo_Area_Key,
				Survey_Event_Key,
				Concept_Key,
				Entered_By
			) VALUES (
				@Key,
				@SurveyEventKey,
				@ConceptKey,
				@EnteredBy
			)

			IF @@Error <> 0 GOTO RollBackAndExit

		COMMIT TRANSACTION
	END

	RETURN 0

RollBackAndExit:
	ROLLBACK TRANSACTION

GO
/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SurveyEventGeoArea_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SurveyEventGeoArea_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SurveyEventGeoArea_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SurveyEventGeoArea_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SurveyEventGeoArea_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_SurveyEventGeoArea_Insert TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SurveyEventGeoArea_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SurveyEventGeoArea_Insert TO [Dev - JNCC SQL]
END

GO



If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_SurveyEventGeoArea_Select]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_SurveyEventGeoArea_Select]
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

CREATE PROCEDURE [dbo].usp_SurveyEventGeoArea_Select 
@Key CHAR(16)

AS

--  DESCRIPTION
--  Returns all the Geo Areas associated with the survey event.
--
--	PARAMETERS
--	NAME				DESCRIPTION
--	@Key				They Survey_Event_Key to search on
--
--
--  AUTHOR:				David Kelly, Dorset Software
--  CREATED:			2007-09-07
--
SET NOCOUNT ON

SELECT
	T.Plaintext,
	T.Term_Key,
	SEGA.Survey_Event_Geo_Area_Key,
	SEGA.Survey_Event_Key,
	SEGA.Concept_Key,
	SE.Custodian,
	SEGA.[Timestamp]
FROM
	Term T
	JOIN Concept C ON C.Term_Key = T.Term_Key
	JOIN Concept C1 ON C1.Meaning_Key = C.Meaning_Key AND C.List_Preferred = 1
	JOIN Survey_Event_Geo_Area SEGA ON SEGA.Concept_Key = C1.Concept_Key
	JOIN Survey_Event SE ON SE.Survey_Event_Key = SEGA.Survey_Event_Key
WHERE
	SEGA.Survey_Event_Key = @Key
ORDER BY
	C1.Sort_Code, T.Plaintext

GO
/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SurveyEventGeoArea_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SurveyEventGeoArea_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SurveyEventGeoArea_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SurveyEventGeoArea_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SurveyEventGeoArea_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_SurveyEventGeoArea_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SurveyEventGeoArea_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SurveyEventGeoArea_Select TO [Dev - JNCC SQL]
END

GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_SurveyEventKey_Get_ForFieldData]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_SurveyEventKey_Get_ForFieldData]
GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

CREATE PROCEDURE [dbo].usp_SurveyEventKey_Get_ForFieldData 
	@Key char(16),
	@SurveyEventKey char(16) OUTPUT

AS

--  DESCRIPTION
--  Insert a record into Survey_Event_Geo_Area
--
--	PARAMETERS
--	NAME				DESCRIPTION
--	@Key				The Specimen_Field_Data_Key of the record
--  @SurveyEventKey		The Survey_Event_Key of the record
--
--
--  AUTHOR:				David Kelly, Dorset Software
--  CREATED:			2007-09-07
--


	-- Required to be able to get number of changed records.
	SET NOCOUNT ON

	SELECT 	@SurveyEventKey = S.Survey_Event_Key
	FROM
	Specimen_Field_Data SFD			
	LEFT JOIN 	Taxon_Occurrence XO ON XO.Taxon_Occurrence_Key=SFD.Taxon_Occurrence_Key
	LEFT JOIN 	Occurrence O ON O.Occurrence_Key=SFD.Occurrence_Key
	INNER JOIN 	[Sample] S ON S.Sample_Key=XO.Sample_Key OR S.Sample_Key=O.Sample_Key
	WHERE	SFD.Specimen_Field_Data_Key = @Key


GO
/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SurveyEventKey_Get_ForFieldData') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SurveyEventKey_Get_ForFieldData'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SurveyEventKey_Get_ForFieldData TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SurveyEventKey_Get_ForFieldData TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SurveyEventKey_Get_ForFieldData TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_SurveyEventKey_Get_ForFieldData TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SurveyEventKey_Get_ForFieldData TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SurveyEventKey_Get_ForFieldData TO [Dev - JNCC SQL]
END

GO



