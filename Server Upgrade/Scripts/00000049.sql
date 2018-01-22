IF Object_ID('dbo.usp_QETemplates_Select_ForOccurrence') IS NOT NULL
	DROP PROCEDURE dbo.usp_QETemplates_Select_ForOccurrence
GO

/*===========================================================================*\
  Description:
	Selects all of the templates appropriate for the given occurrence.

  Parameters:
	@Item_Key - The key of the occurrence to search templates for.

  Created:	January 2011

  Last revision information:
	$Revision: 4 $
	$Date: 26/01/11 15:14 $
	$Author: Robertjohnson $

\*===========================================================================*/

CREATE PROCEDURE dbo.usp_QETemplates_Select_ForOccurrence(
	@Item_Key CHAR(16)
)
AS

	DECLARE @SpecimenType INT
	SET		@SpecimenType = 1

	SELECT DISTINCT		
				QET.QE_Template_Key,
				QET.Item_Name,
				QET.Template_Type,
				QET.Subject_Area_Key,
				QET."Timestamp"
	FROM		QE_Template			AS	QET
	LEFT JOIN	Domain				AS	Dom
		ON		QET.Subject_Area_Key = Dom.Subject_Area_Key
	LEFT JOIN	Local_Domain		AS	LD
		ON		Dom.Domain_Key		=	LD.Domain_Key
	LEFT JOIN	Concept_Group		AS	CG
		ON		LD.Local_Domain_Key =	CG.Local_Domain_Key
	LEFT JOIN	Concept				AS	C
		ON		CG.Concept_Group_Key =	C.Concept_Group_Key
	LEFT JOIN	Determination		AS	Det
		ON		C.Concept_Key		=	Det.Concept_Key
		AND		Det.Occurrence_Key	=	@Item_Key
	WHERE		QET.Template_Type	=	@SpecimenType
		AND		(
					QET.Subject_Area_Key	IS NULL
					OR
					Det.Determination_Key	IS NOT NULL
				)

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
PRINT 'Setting up security on procedure usp_QETemplates_Select_ForOccurrence'
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
   	GRANT EXECUTE ON dbo.usp_QETemplates_Select_ForOccurrence TO [R2k_AddOnly]
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
	GRANT EXECUTE ON dbo.usp_QETemplates_Select_ForOccurrence TO [R2k_Administrator]
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
	GRANT EXECUTE ON dbo.usp_QETemplates_Select_ForOccurrence TO [R2k_FullEdit]
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
	GRANT EXECUTE ON dbo.usp_QETemplates_Select_ForOccurrence TO [R2k_ReadOnly]
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
	GRANT EXECUTE ON dbo.usp_QETemplates_Select_ForOccurrence TO [R2k_RecordCardsOnly]
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
   	GRANT EXECUTE ON dbo.usp_QETemplates_Select_ForOccurrence TO [Dev - JNCC SQL]
GO




IF Object_ID('dbo.usp_QETemplates_Select_ForTaxonOccurrence') IS NOT NULL
	DROP PROCEDURE dbo.usp_QETemplates_Select_ForTaxonOccurrence
GO

/*===========================================================================*\
  Description:
	Selects all of the templates appropriate for the given taxon occurrence.

  Parameters:
	@Item_Key

  Created:	January 2011

  Last revision information:
	$Revision: 4 $
	$Date: 26/01/11 15:14 $
	$Author: Robertjohnson $

\*===========================================================================*/

CREATE PROCEDURE dbo.usp_QETemplates_Select_ForTaxonOccurrence(
	@Item_Key CHAR(16)
)
AS

	DECLARE @SpecimenType INT
	SET		@SpecimenType = 1

	SELECT DISTINCT		
				QET.QE_Template_Key,
				QET.Item_Name,
				QET.Template_Type,
				QET.Subject_Area_Key,
				QET."Timestamp"
	FROM		QE_Template		AS	QET
	LEFT JOIN	Domain			AS	D
		ON		QET.Subject_Area_Key = D.Subject_Area_Key
	LEFT JOIN	Local_Domain	AS	LD
		ON		D.Domain_Key	=	LD.Domain_Key
	LEFT JOIN	Concept_Group	AS	CG
		ON		LD.Local_Domain_Key = CG.Local_Domain_Key
	LEFT JOIN	Concept			AS	C
		ON		CG.Concept_Group_Key	=	C.Concept_Group_Key
	LEFT JOIN	Taxon_Dictionary_Concept_Mapping AS TDCM
		ON		C.Concept_Key			=	TDCM.Concept_Key
	LEFT JOIN	Taxon_Determination		AS	TD
		ON		TDCM.Taxon_List_Item_Key =	TD.Taxon_List_Item_Key
		AND		Taxon_Occurrence_Key	=	@Item_Key
		AND		TD.Preferred			=	1
	WHERE		QET.Template_Type		=	@SpecimenType
		AND		(
					QET.Subject_Area_Key IS NULL
					OR
					TD.Taxon_Determination_Key IS NOT NULL	
				)

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
PRINT 'Setting up security on procedure usp_QETemplates_Select_ForTaxonOccurrence'
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
   	GRANT EXECUTE ON dbo.usp_QETemplates_Select_ForTaxonOccurrence TO [R2k_AddOnly]
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
	GRANT EXECUTE ON dbo.usp_QETemplates_Select_ForTaxonOccurrence TO [R2k_Administrator]
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
	GRANT EXECUTE ON dbo.usp_QETemplates_Select_ForTaxonOccurrence TO [R2k_FullEdit]
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
	GRANT EXECUTE ON dbo.usp_QETemplates_Select_ForTaxonOccurrence TO [R2k_ReadOnly]
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
	GRANT EXECUTE ON dbo.usp_QETemplates_Select_ForTaxonOccurrence TO [R2k_RecordCardsOnly]
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
   	GRANT EXECUTE ON dbo.usp_QETemplates_Select_ForTaxonOccurrence TO [Dev - JNCC SQL]
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_QETemplateField_Select_ForTemplate') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_QETemplateField_Select_ForTemplate]
GO

/*===========================================================================*\
  Description:	Selects the fields for a template, with an optional input to
				specify a quick entry session

  Parameters:	@QETemplateKey - QE_Template_Key
				@TemplateType - see template type field description
				@QESessionKey - key for the QE session

  Created:	Jan 2004

  Last revision information:
    $Revision: 4 $
    $Date: 26/01/11 15:14 $
    $Author: Robertjohnson $

\*===========================================================================*/    
CREATE PROCEDURE [dbo].[usp_QETemplateField_Select_ForTemplate]
	@QETemplateKey 	char(16),
	@TemplateType 	tinyint,
	@QESessionKey	int = -1
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
			NULL AS Metadata_Type_Key, 
			Initialized_From_Occurrence,
			S.Occurrence_Key,
			S.Taxon_Occurrence_Key
		
	FROM  		QE_Field F 
	LEFT JOIN 	QE_Template_Field TF ON F.QE_Field_Key = TF.QE_Field_Key AND @QETemplateKey = TF.QE_Template_Key AND Is_Custom = 0
	LEFT JOIN		QE_Session S ON S.QE_Template_Key = TF.QE_Template_Key AND S.QE_Session_Key = @QESessionKey
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
		NULL AS Metadata_Type_Key, 
		(1 - Measurement_Is_Specimen) AS Initialized_From_Occurrence,
		S.Occurrence_Key,
		S.Taxon_Occurrence_Key
		
	FROM	QE_Template_Field F
	JOIN	vw_ConceptTerm CT ON Concept_Key = F.Measurement_Parameter_Concept_Key
	JOIN 	Concept_Group CG ON CT.Concept_Group_Key = CG.Concept_Group_Key
	JOIN 	Local_Domain LD ON LD.Local_Domain_Key = CG.Local_Domain_Key
	JOIN 	Domain D ON D.Domain_Key = LD.Domain_Key AND (D.Has_Occurrences = 1 OR D.Domain_Key = 'SYSTEM00000000')
	LEFT JOIN	QE_Session S ON S.QE_Template_Key = F.QE_Template_Key AND S.QE_Session_Key = @QESessionKey
	WHERE 	Is_Custom = 1 AND F.QE_Template_Key = @QETemplateKey AND Measurement_Is_TaxonData = 0

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
		NULL AS Metadata_Type_Key, 
		1 AS Initialized_From_Occurrence,
		S.Occurrence_Key,
		S.Taxon_Occurrence_Key

	FROM	QE_Template_Field F
	JOIN	Measurement_Qualifier MQ ON MQ.Measurement_Qualifier_Key = Taxon_Measurement_Qualifier_Key
	JOIN	Measurement_Unit MU ON MU.Measurement_Unit_Key = Taxon_Measurement_Unit_Key 
	JOIN	Measurement_Type MT ON MT.Measurement_Type_Key = MU.Measurement_Type_Key
	LEFT JOIN	QE_Session S ON S.QE_Template_Key = F.QE_Template_Key AND S.QE_Session_Key = @QESessionKey
	WHERE 	Is_Custom = 1 AND F.QE_Template_Key = @QETemplateKey AND Measurement_Is_Specimen = 0 AND Measurement_Is_TaxonData = 1

	UNION --Numbers

	SELECT 	CT.Plaintext COLLATE SQL_Latin1_General_CP1_CI_AS, 
		General_Tab,
		Specimen_Tab, 
		F.Item_Name, 
		Default_Value,
		20, 
		QE_Template_Field_Key, 
		F.Timestamp,
		CASE F.Number_Type_Concept_Key
			WHEN 'SYSTEM0000000001' THEN 16 ELSE 0 END, 
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
		NULL AS Metadata_Type_Key, 
		0 AS Initialized_From_Occurrence,
		NULL AS Occurrence_Key,
		NULL AS Taxon_Occurrence_Key

	FROM	QE_Template_Field F
	JOIN	vw_ConceptTerm CT ON Concept_Key = F.Number_Type_Concept_Key
	WHERE 	Is_Custom = 2 AND QE_Template_Key = @QETemplateKey
	AND		@TemplateType = 1

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
		F.Metadata_Type_Key, 
		0 AS Initialized_From_Occurrence,
		NULL AS Occurrence_Key,
		NULL AS Taxon_Occurrence_Key

	FROM	QE_Template_Field F
	JOIN	MetaData_Type MT ON MT.MetaData_Type_Key = F.Metadata_Type_Key
	WHERE 	Is_Custom = 3 AND QE_Template_Key = @QETemplateKey
	AND		@TemplateType = 1

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




IF Object_ID('dbo.usp_QEDataItem_Insert_ForOccurrence') IS NOT NULL
	DROP PROCEDURE dbo.usp_QEDataItem_Insert_ForOccurrence
GO

/*===========================================================================*\
  Description:
	Inserts quick entry data items data items using data from the
	occurrence of the given QE session.	

  Parameters:
	@QE_Session_Key - The quick entry session to which the data items are to
						be associated
	@SessionID

  Created:	January 2011

  Last revision information:
	$Revision: 4 $
	$Date: 26/01/11 15:14 $
	$Author: Robertjohnson $

\*===========================================================================*/

CREATE PROCEDURE dbo.usp_QEDataItem_Insert_ForOccurrence(
	@QE_Session_Key CHAR(16),
	@SessionID		CHAR(16)
)
AS

INSERT		QE_Data_Item (
			QE_Data_Row_Key,
			QE_Template_Field_Key,
			Data_Value,
			Data_Display,
			Entered_Session_ID,
			Position)

SELECT		DR.QE_Data_Row_Key,
			TF.QE_Template_Field_Key,
			
			CASE F.Item_Name
				WHEN 'Survey'	
				THEN Sy.Survey_Key
				WHEN 'Thesaurus Determination'
				THEN Dp.Concept_Key
				WHEN 'Gathering Site'
				THEN LN.Location_Key
				WHEN 'Gathering Method'
				THEN ST.Sample_Type_Key
				WHEN 'Gathering Occurrence Comment'
				-- For some reason, unlike with Data_Display below,
				-- SUBSTRING(dbo.ufn_RtfToPlaintext(O.Comment), 0, 200) in here
				-- causes a crash even when cast as TEXT, so just insert an empty string instead
				THEN CAST ('' AS TEXT)
				WHEN 'Gathering Location Name'
				THEN	CASE ISNULL(Sl.Location_Name, '')
							WHEN ''
							THEN SE.Location_Name
							ELSE Sl.Location_Name
						END
				WHEN 'Determiner'
				THEN I_TDa.Name_Key
				WHEN 'Determiner Role'
				THEN Da.Determiner_Role_Key
				WHEN 'Determination Date'
				THEN dbo.ufn_GetDateFromVagueDate(
							Dp.Vague_Date_Start,
							Dp.Vague_Date_End,
							Dp.Vague_Date_Type)
				WHEN 'Gathering Spatial Reference'
				THEN 
					CASE Sl.Spatial_Ref
						WHEN NULL
						THEN NULL
						ELSE '"' + Sl.Spatial_Ref + '",'
								+ ISNULL(Sl.Spatial_Ref_System, '') + ','
								+ CONVERT(VARCHAR(20), ISNULL(Sl.Lat, '')) + ','
								+ CONVERT(VARCHAR(20), ISNULL(Sl.Long, '')) + ','
								+ '"' + ISNULL(Sl.Spatial_Ref_qualifier, '') + '"'
					END
							
				WHEN 'Gathering Date'
				THEN dbo.ufn_GetDateFromVagueDate(
							Sl.Vague_Date_Start,
							Sl.Vague_Date_End,
							Sl.Vague_Date_Type)
				WHEN 'Field Collector'
				THEN I_SER.Name_Key
				WHEN 'Field Record Type'
				THEN CTRT.Concept_Key
				WHEN 'Determiners are Inferred'
				THEN CONVERT(VARCHAR(2), Dp.Inferred_Determiner)
				WHEN 'Geographic Areas'
				THEN CT.Concept_Key
				ELSE CASE
						WHEN TF.Is_Custom = 1
						THEN OD.Lower_Value
					 END	
			END	AS 'Data_Value',

			CASE F.Item_Name
				WHEN 'Survey'	
				THEN Sy.Item_Name
				WHEN 'Thesaurus Determination'
				THEN CT_Dp.Plaintext
				WHEN 'Gathering Site'
				THEN LN.Item_Name
				WHEN 'Gathering Method'
				THEN ST.Short_Name
				WHEN 'Gathering Occurrence Comment'
				THEN SUBSTRING(dbo.ufn_RtfToPlaintext(O.Comment), 0, 200)
				WHEN 'Gathering Location Name'
				THEN	CASE ISNULL(Sl.Location_Name, '')
							WHEN ''
							THEN SE.Location_Name
							ELSE Sl.Location_Name
						END
				WHEN 'Determiner'
				THEN dbo.ufn_GetFormattedName(I_TDa.Name_Key)
				WHEN 'Determiner Role'
				THEN DetRl.Short_Name
				WHEN 'Determination Date'
				THEN dbo.ufn_GetDateFromVagueDate(
							Dp.Vague_Date_Start,
							Dp.Vague_Date_End,
							Dp.Vague_Date_Type)
				WHEN 'Gathering Spatial Reference'
				THEN 
					CASE Sl.Spatial_Ref
						WHEN NULL
						THEN NULL
						ELSE Sl.Spatial_Ref
					END
				WHEN 'Gathering Date'
				THEN dbo.ufn_GetDateFromVagueDate(
							Sl.Vague_Date_Start,
							Sl.Vague_Date_End,
							Sl.Vague_Date_Type)
				WHEN 'Field Collector'
				THEN dbo.ufn_GetFormattedName(I_SER.Name_Key)
				WHEN 'Field Record Type'
				THEN CTRT.PlainText
				WHEN 'Determiners are Inferred'
				THEN 
					CASE Dp.Inferred_Determiner
						WHEN 0
						THEN 'No'
						WHEN 1
						THEN '!'
						WHEN 2
						THEN '?'
						WHEN 3
						THEN '!?'
					END
				WHEN 'Geographic Areas'
				THEN CT.Item_Name
				ELSE CASE
						WHEN TF.Is_Custom = 1
						THEN OD.Lower_Value
					 END	
			END	AS 'Data_Display',
			@SessionID,
			0 AS 'Position'

FROM		QE_Data_Row				AS	DR
INNER JOIN	QE_Session				AS	Sn
	ON		DR.QE_Session_Key		=	Sn.QE_Session_Key
INNER JOIN	Occurrence				AS	O
	ON		O.Occurrence_Key		=	Sn.Occurrence_Key
INNER JOIN	QE_Template_Field		AS	TF
	ON		TF.QE_Template_Key		=	Sn.QE_Template_Key	
	AND		(
				(TF.General_Tab		=	1
				AND DR.General		=	1)
			OR
				(TF.Specimen_Tab	=	1
				AND DR.General		=	0)
			)
LEFT JOIN	Occurrence_Data					AS	OD
	ON		O.Occurrence_Key				=	OD.Occurrence_Key
			AND	TF.QE_Field_Key				IS	NULL
			AND TF.Is_Custom				=	1
			AND TF.Measurement_Is_Specimen	=	0
			AND TF.Measurement_Is_TaxonData =	0
			AND TF.Measurement_Applies_To	=	OD.Applies_To
			AND	TF.Measurement_Parameter_Concept_Key			= OD.Parameter_Concept_Key
			AND	ISNULL(TF.Measurement_Method_Concept_Key, '')	= ISNULL(OD.Method_Concept_Key, '')
			AND	ISNULL(TF.Measurement_Duration, '')				= ISNULL(OD.Duration, '')
			AND ISNULL(TF.Measurement_Accuracy, '')				= ISNULL(OD.Accuracy, '')
			AND	ISNULL(TF.Measurement_Unit_Concept_Key, '')		= ISNULL(OD.Unit_Concept_Key, '')
LEFT JOIN	QE_Field			AS	F
	ON		TF.QE_Field_Key		=	F.QE_Field_Key
	AND		F.Initialized_From_Occurrence = 1

-- Survey
LEFT JOIN	"Sample"			AS	Sl
	ON		O.Sample_Key		=	Sl.Sample_Key
LEFT JOIN	Survey_Event		AS	SE
	ON		Sl.Survey_Event_Key	=	SE.Survey_Event_Key
LEFT JOIN	Survey				AS	Sy
	ON		SE.Survey_Key		=	Sy.Survey_Key

-- Determinations (preferred)
LEFT JOIN	Determination			AS	Dp
	ON		O.Occurrence_Key	=	Dp.Occurrence_Key
	AND		Dp.Preferred				=	1
LEFT JOIN	VW_ConceptTerm		AS	CT_Dp
	ON		Dp.Concept_Key		=	CT_Dp.Concept_Key

-- Taxon determinations (all)
LEFT JOIN	Determination			AS	Da
	ON		O.Occurrence_Key	=	Da.Occurrence_Key
	AND		(F.Item_Name = 'Determiner'
			OR F.Item_Name = 'Determiner Role')

-- Determiner
LEFT JOIN	Individual	AS	I_TDa
	ON		Da.Determiner_Name_Key = I_TDa.Name_Key

-- Determiner Role
LEFT JOIN	Determiner_Role			AS	DetRl
	ON		Da.Determiner_Role_Key =	DetRl.Determiner_Role_Key

-- Gathering site
LEFT JOIN	Location_Name		AS	LN
	ON		((Sl.Location_Key	IS NOT NULL
			AND Sl.Location_Key	= LN.Location_Key)

			OR

			(Sl.Location_Key	IS	NULL
			AND SE.Location_Key =	LN.Location_Key))
	AND		F.Item_Name			=	'Gathering Site'
	AND		LN.Preferred		=	1

-- Gathering method
LEFT JOIN	Sample_Type			AS	ST
	ON		F.Item_Name			=	'Gathering Method'
	AND		Sl.Sample_Type_Key	=	ST.Sample_Type_Key

-- Field Collector
LEFT JOIN	Survey_Event_Recorder	AS	SER
	ON		SE.Survey_Event_Key		=	SER.Survey_Event_Key
	AND		F.Item_Name				=	'Field Collector'
LEFT JOIN	Individual				AS	I_SER
	ON		SER.Name_Key			=	I_SER.Name_Key

-- Field Record Type
LEFT JOIN	VW_ConceptTerm	AS	CTRT
	ON		F.Item_Name				=	'Field Record Type'
	AND		O.Record_Type_Concept_Key	=	CTRT.Concept_Key

-- Geographic Areas
LEFT JOIN	Survey_Event_Geo_Area	AS	SEGA
	ON		SE.Survey_Event_Key		=	SEGA.Survey_Event_Key
	AND		F.Item_Name				=	'Geographic Areas'
LEFT JOIN	VW_ConceptTerm			AS	CT
	ON		SEGA.Concept_Key		=	CT.Concept_Key	


WHERE		Sn.QE_Session_Key	=	@QE_Session_Key
	AND		(F.QE_Field_Key	IS NOT NULL
			OR TF.Is_Custom	=	1)

-- Update each inserted data item such that items sharing
-- a row and field each have a different Position value. 
UPDATE	QE_Data_item
SET		Position = QEDI.Position
FROM	
(
	SELECT	DI.QE_Data_Item_Key,
			(SELECT		COUNT(*) 
			FROM		QE_Data_Item				AS	DI2
			WHERE		DI2.QE_Data_Row_Key			=	DI.QE_Data_Row_Key
				AND		DI2.QE_Template_Field_Key	=	DI.QE_Template_Field_Key
				AND		DI2.QE_Data_Item_Key		<	DI.QE_Data_Item_Key) AS 'Position'
	FROM		QE_Data_Item		AS	DI
	INNER JOIN	QE_Data_Row			AS	DR
		ON		DI.QE_Data_Row_Key	=	DR.QE_Data_Row_Key
	WHERE 		DR.QE_Session_Key	=	@QE_Session_Key
) AS QEDI
WHERE			QE_Data_item.QE_Data_Item_Key = QEDI.QE_Data_Item_Key

-- TODO: Possibly need another update section (or alteration to the update
--		above) to ensure that the position values of corresponding determiners
--		and determiner roles are equal.

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
PRINT 'Setting up security on procedure usp_QEDataItem_Insert_ForOccurrence'
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
   	GRANT EXECUTE ON dbo.usp_QEDataItem_Insert_ForOccurrence TO [R2k_AddOnly]
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
	GRANT EXECUTE ON dbo.usp_QEDataItem_Insert_ForOccurrence TO [R2k_Administrator]
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
	GRANT EXECUTE ON dbo.usp_QEDataItem_Insert_ForOccurrence TO [R2k_FullEdit]
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
	GRANT EXECUTE ON dbo.usp_QEDataItem_Insert_ForOccurrence TO [R2k_ReadOnly]
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
	GRANT EXECUTE ON dbo.usp_QEDataItem_Insert_ForOccurrence TO [R2k_RecordCardsOnly]
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
   	GRANT EXECUTE ON dbo.usp_QEDataItem_Insert_ForOccurrence TO [Dev - JNCC SQL]
GO





IF Object_ID('dbo.usp_QEDataItem_Insert_ForTaxonOccurrence') IS NOT NULL
	DROP PROCEDURE dbo.usp_QEDataItem_Insert_ForTaxonOccurrence
GO

/*===========================================================================*\
  Description:
	Inserts quick entry data items data items using data from the
	taxon occurrence of the given QE session.	

  Parameters:
	@QE_Session_Key - The quick entry session to which the data items are to
						be associated
	@SessionID

  Created:	January 2011

  Last revision information:
	$Revision: 4 $
	$Date: 26/01/11 15:14 $
	$Author: Robertjohnson $

\*===========================================================================*/

CREATE PROCEDURE dbo.usp_QEDataItem_Insert_ForTaxonOccurrence(
	@QE_Session_Key CHAR(16),
	@SessionID		CHAR(16)
)
AS

INSERT		QE_Data_Item (
			QE_Data_Row_Key,
			QE_Template_Field_Key,
			Data_Value,
			Data_Display,
			Entered_Session_ID,
			Position)

SELECT		DR.QE_Data_Row_Key,
			TF.QE_Template_Field_Key,
			
			CASE F.Item_Name
				WHEN 'Survey'	
				THEN Sy.Survey_Key
				WHEN 'Taxon Determination'
				THEN TDp.Taxon_List_Item_Key
				WHEN 'Gathering Site'
				THEN LN.Location_Key
				WHEN 'Gathering Method'
				THEN ST.Sample_Type_Key
				WHEN 'Gathering Occurrence Comment'
				-- For some reason, unlike with Data_Display below,
				-- SUBSTRING(dbo.ufn_RtfToPlaintext("TO".Comment), 0, 200) in here
				-- causes a crash even when cast as TEXT, so just insert an empty string instead
				THEN CAST ('' AS TEXT)
				WHEN 'Gathering Location Name'
				THEN	CASE ISNULL(Sl.Location_Name, '')
							WHEN ''
							THEN SE.Location_Name
							ELSE Sl.Location_Name
						END
				WHEN 'Determiner'
				THEN I_TDa.Name_Key
				WHEN 'Determiner Role'
				THEN TDa.Determiner_Role_Key
				WHEN 'Determination Date'
				THEN dbo.ufn_GetDateFromVagueDate(
							TDp.Vague_Date_Start,
							TDp.Vague_Date_End,
							TDp.Vague_Date_Type)
				WHEN 'Gathering Spatial Reference'
				THEN 
					CASE Sl.Spatial_Ref
						WHEN NULL
						THEN NULL
						ELSE '"' + Sl.Spatial_Ref + '",'
								+ ISNULL(Sl.Spatial_Ref_System, '') + ','
								+ CONVERT(VARCHAR(20), ISNULL(Sl.Lat, '')) + ','
								+ CONVERT(VARCHAR(20), ISNULL(Sl.Long, '')) + ','
								+ '"' + ISNULL(Sl.Spatial_Ref_qualifier, '') + '"'
					END
							
				WHEN 'Gathering Date'
				THEN dbo.ufn_GetDateFromVagueDate(
							Sl.Vague_Date_Start,
							Sl.Vague_Date_End,
							Sl.Vague_Date_Type)
				WHEN 'Field Collector'
				THEN I_SER.Name_Key
				WHEN 'Field Record Type'
				THEN RT.Record_Type_Key
				WHEN 'Determiners are Inferred'
				THEN CONVERT(VARCHAR(2), TDp.Inferred_Determiner)
				WHEN 'Geographic Areas'
				THEN CT.Concept_Key
				ELSE CASE
						WHEN TF.Is_Custom = 1
						THEN TOD.Data
					 END	
			END	AS 'Data_Value',

			CASE F.Item_Name
				WHEN 'Survey'	
				THEN Sy.Item_Name
				WHEN 'Taxon Determination'
				THEN T.Item_Name
				WHEN 'Gathering Site'
				THEN LN.Item_Name
				WHEN 'Gathering Method'
				THEN ST.Short_Name
				WHEN 'Gathering Occurrence Comment'
				THEN SUBSTRING(dbo.ufn_RtfToPlaintext("TO".Comment), 0, 200)
				WHEN 'Gathering Location Name'
				THEN	CASE ISNULL(Sl.Location_Name, '')
							WHEN ''
							THEN SE.Location_Name
							ELSE Sl.Location_Name
						END
				WHEN 'Determiner'
				THEN dbo.ufn_GetFormattedName(I_TDa.Name_Key)
				WHEN 'Determiner Role'
				THEN DetRl.Short_Name
				WHEN 'Determination Date'
				THEN dbo.ufn_GetDateFromVagueDate(
							TDp.Vague_Date_Start,
							TDp.Vague_Date_End,
							TDp.Vague_Date_Type)
				WHEN 'Gathering Spatial Reference'
				THEN 
					CASE Sl.Spatial_Ref
						WHEN NULL
						THEN NULL
						ELSE Sl.Spatial_Ref
					END
				WHEN 'Gathering Date'
				THEN dbo.ufn_GetDateFromVagueDate(
							Sl.Vague_Date_Start,
							Sl.Vague_Date_End,
							Sl.Vague_Date_Type)
				WHEN 'Field Collector'
				THEN dbo.ufn_GetFormattedName(I_SER.Name_Key)
				WHEN 'Field Record Type'
				THEN RT.Short_Name
				WHEN 'Determiners are Inferred'
				THEN 
					CASE TDp.Inferred_Determiner
						WHEN 0
						THEN 'No'
						WHEN 1
						THEN '!'
						WHEN 2
						THEN '?'
						WHEN 3
						THEN '!?'
					END
				WHEN 'Geographic Areas'
				THEN CT.Item_Name
				ELSE CASE
						WHEN TF.Is_Custom = 1
						THEN TOD.Data
					 END	
			END	AS 'Data_Display',
			@SessionID,
			0 AS 'Position'

FROM		QE_Data_Row				AS	DR
INNER JOIN	QE_Session				AS	Sn
	ON		DR.QE_Session_Key		=	Sn.QE_Session_Key
INNER JOIN	Taxon_Occurrence		AS	"TO"
	ON		"TO".Taxon_Occurrence_Key = Sn.Taxon_Occurrence_Key
INNER JOIN	QE_Template_Field		AS	TF
	ON		TF.QE_Template_Key		=	Sn.QE_Template_Key	
	AND		(
				(TF.General_Tab		=	1
				AND DR.General		=	1)
			OR
				(TF.Specimen_Tab	=	1
				AND DR.General		=	0)
			)
LEFT JOIN	Taxon_Occurrence_Data				AS	TOD
	ON		"TO".Taxon_Occurrence_Key			=	TOD.Taxon_Occurrence_Key
	AND		TF.QE_Field_Key						IS	NULL
	AND		TF.Is_Custom						=	1
	AND		TF.Measurement_Is_Specimen			=	0
	AND		TF.Measurement_Is_TaxonData			=	1
	AND		TF.Taxon_Measurement_Qualifier_Key	= TOD.Measurement_Qualifier_Key
	AND		TF.Taxon_Measurement_Unit_Key		= TOD.Measurement_Unit_Key
	AND		TF.Measurement_Accuracy				= TOD.Accuracy	

LEFT JOIN	QE_Field			AS	F
	ON		TF.QE_Field_Key		=	F.QE_Field_Key
	AND		F.Initialized_From_Occurrence = 1

-- Survey
LEFT JOIN	"Sample"			AS	Sl
	ON		"TO".Sample_Key		=	Sl.Sample_Key
LEFT JOIN	Survey_Event		AS	SE
	ON		Sl.Survey_Event_Key	=	SE.Survey_Event_Key
LEFT JOIN	Survey				AS	Sy
	ON		SE.Survey_Key		=	Sy.Survey_Key

-- Taxon Determinations (preferred)
LEFT JOIN	Taxon_Determination			AS	TDp
	ON		"TO".Taxon_Occurrence_Key	=	TDp.Taxon_Occurrence_Key
	AND		TDp.Preferred				=	1
LEFT JOIN	Taxon_List_Item				AS	TLI
	ON		TDp.Taxon_List_Item_Key		=	TLI.Taxon_List_Item_Key
LEFT JOIN	Taxon_Version				AS	TV 
	ON		TLI.Taxon_Version_Key		=	TV.Taxon_Version_Key
LEFT JOIN	Taxon						AS	T
	ON		TV.Taxon_Key				=	T.Taxon_Key

-- Taxon determinations (all)
LEFT JOIN	Taxon_Determination			AS	TDa
	ON		"TO".Taxon_Occurrence_Key	=	TDa.Taxon_Occurrence_Key
	AND		(F.Item_Name = 'Determiner'
			OR F.Item_Name = 'Determiner Role')

-- Determiner
LEFT JOIN	Individual		AS	I_TDa
	ON		TDa.Determiner	=	I_TDa.Name_Key

-- Determiner Role
LEFT JOIN	Determiner_Role			AS	DetRl
	ON		TDa.Determiner_Role_Key =	DetRl.Determiner_Role_Key

-- Gathering site
LEFT JOIN	Location_Name		AS	LN
	ON		((Sl.Location_Key	IS NOT NULL
			AND Sl.Location_Key	= LN.Location_Key)

			OR

			(Sl.Location_Key	IS NULL
			AND SE.Location_Key = LN.Location_Key))
	AND		F.Item_Name		=	'Gathering Site'
	AND		LN.Preferred	=	1

-- Gathering method
LEFT JOIN	Sample_Type			AS	ST
	ON		F.Item_Name			=	'Gathering Method'
	AND		Sl.Sample_Type_Key	=	ST.Sample_Type_Key

-- Field Collector
LEFT JOIN	Survey_Event_Recorder	AS	SER
	ON		SE.Survey_Event_Key		=	SER.Survey_Event_Key
	AND		F.Item_Name				=	'Field Collector'
LEFT JOIN	Individual				AS	I_SER
	ON		SER.Name_Key			=	I_SER.Name_Key

-- Field Record Type
LEFT JOIN	Record_Type				AS	RT
	ON		F.Item_Name				=	'Field Record Type'
	AND		"TO".Record_Type_Key	=	RT.Record_Type_Key

-- Geographic Areas
LEFT JOIN	Survey_Event_Geo_Area	AS	SEGA
	ON		SE.Survey_Event_Key		=	SEGA.Survey_Event_Key
	AND		F.Item_Name				=	'Geographic Areas'
LEFT JOIN	VW_ConceptTerm			AS	CT
	ON		SEGA.Concept_Key		=	CT.Concept_Key	


WHERE		Sn.QE_Session_Key	=	@QE_Session_Key
	AND		(F.QE_Field_Key	IS NOT NULL
			OR TF.Is_Custom	=	1)

-- Update each inserted data item such that items sharing
-- a row and field each have a different Position value. 
UPDATE	QE_Data_item
SET		Position = QEDI.Position
FROM	
(
	SELECT	DI.QE_Data_Item_Key,
			(SELECT		COUNT(*) 
			FROM		QE_Data_Item				AS	DI2
			WHERE		DI2.QE_Data_Row_Key			=	DI.QE_Data_Row_Key
				AND		DI2.QE_Template_Field_Key	=	DI.QE_Template_Field_Key
				AND		DI2.QE_Data_Item_Key		<	DI.QE_Data_Item_Key) AS 'Position'
	FROM		QE_Data_Item		AS	DI
	INNER JOIN	QE_Data_Row			AS	DR
		ON		DI.QE_Data_Row_Key	=	DR.QE_Data_Row_Key
	WHERE 		DR.QE_Session_Key	=	@QE_Session_Key
) AS QEDI
WHERE			QE_Data_item.QE_Data_Item_Key = QEDI.QE_Data_Item_Key

-- TODO: Possibly need another update section (or alteration to the update
--		above) to ensure that the position values of corresponding determiners
--		and determiner roles are equal.

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
PRINT 'Setting up security on procedure usp_QEDataItem_Insert_ForTaxonOccurrence'
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
   	GRANT EXECUTE ON dbo.usp_QEDataItem_Insert_ForTaxonOccurrence TO [R2k_AddOnly]
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
	GRANT EXECUTE ON dbo.usp_QEDataItem_Insert_ForTaxonOccurrence TO [R2k_Administrator]
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
	GRANT EXECUTE ON dbo.usp_QEDataItem_Insert_ForTaxonOccurrence TO [R2k_FullEdit]
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
	GRANT EXECUTE ON dbo.usp_QEDataItem_Insert_ForTaxonOccurrence TO [R2k_ReadOnly]
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
	GRANT EXECUTE ON dbo.usp_QEDataItem_Insert_ForTaxonOccurrence TO [R2k_RecordCardsOnly]
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
   	GRANT EXECUTE ON dbo.usp_QEDataItem_Insert_ForTaxonOccurrence TO [Dev - JNCC SQL]
GO