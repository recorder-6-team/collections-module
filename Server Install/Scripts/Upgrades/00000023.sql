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
    $Revision: 2 $
    $Date: 2/02/09 16:52 $
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
			Is_Measurement,
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
			isnull(TF.Sequence, (select count(*) from QE_Template_Field))
		
	FROM  		QE_Field F 
	LEFT JOIN 	QE_Template_Field TF ON F.QE_Field_Key = TF.QE_Field_Key AND @QETemplateKey = TF.QE_Template_Key AND Is_Measurement = 0
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
	 	Is_Measurement,
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
		isnull(F.Sequence, (select count(*) from QE_Template_Field))
	FROM	QE_Template_Field F
	JOIN	vw_ConceptTerm CT ON Concept_Key = F.Measurement_Parameter_Concept_Key
	JOIN 	Concept_Group CG ON CT.Concept_Group_Key = CG.Concept_Group_Key
	JOIN 	Local_Domain LD ON LD.Local_Domain_Key = CG.Local_Domain_Key
	JOIN 	Domain D ON D.Domain_Key = LD.Domain_Key AND (D.Has_Occurrences = 1 OR D.Domain_Key = 'SYSTEM00000000')
	WHERE 	Is_Measurement = 1 AND QE_Template_Key = @QETemplateKey AND Measurement_Is_TaxonData = 0

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
	 	Is_Measurement,
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
		isnull(F.Sequence, (select count(*) from QE_Template_Field))
	FROM	QE_Template_Field F
	JOIN	Measurement_Qualifier MQ ON MQ.Measurement_Qualifier_Key = Taxon_Measurement_Qualifier_Key
	JOIN	Measurement_Unit MU ON MU.Measurement_Unit_Key = Taxon_Measurement_Unit_Key 
	JOIN	Measurement_Type MT ON MT.Measurement_Type_Key = MU.Measurement_Type_Key
	WHERE 	Is_Measurement = 1 AND QE_Template_Key = @QETemplateKey AND Measurement_Is_Specimen = 0 AND Measurement_Is_TaxonData = 1

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
    $Revision: 2 $
    $Date: 2/02/09 16:52 $
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
	@Sequence int
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
	Sequence = @Sequence
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