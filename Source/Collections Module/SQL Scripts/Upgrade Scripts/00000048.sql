SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

/*===========================================================================*\
  Description:	Adds the columns 'Occurrence_Key' and 'Taxon_Occurrence_Key'
				to the table QE_Session.
				
				Adds a new column 'Initialized_From_Occurrence' which indicates
				when a new quick entry session is created from an existing 
				occurrence.

				Adds the following stored procedures:
					usp_QETemplates_Select_ForOccurrence
					usp_QETemplates_Select_ForTaxonOccurrence
					usp_QEDataItem_Insert_ForOccurrence
					usp_QEDataItem_Insert_ForTaxonOccurrence

				Modifies usp_QESession_Insert.

  Created:	January 2011

  Last revision information:
    $Revision: 1 $
    $Date: 18/01/11 9:56 $
    $Author: Robertjohnson $

\*===========================================================================*/

ALTER TABLE QE_Session
ADD			Occurrence_Key			CHAR(16)	NULL,
			Taxon_Occurrence_Key	CHAR(16)	NULL

ALTER TABLE	QE_Field
ADD			Initialized_From_Occurrence	BIT NULL
GO

UPDATE	QE_Field
SET		Initialized_From_Occurrence = (
		SELECT	CASE
					WHEN Item_Name IN 
							('Survey',
							'Taxon Determination',
							'Thesaurus Determination',
							'Gathering Site',
							'Gathering Method',
							'Gathering Occurrence Comment',
							'Gathering Location Name',
							'Determiner',
							'Determiner Role',
							'Determination Date',
							'Gathering Spatial Reference',
							'Gathering Date',
							'Field Collector',
							'Field Record Type',
							'Determiners are Inferred',
							'Geographic Areas')
					THEN 1
					ELSE 0
				END)
GO

ALTER TABLE		QE_Field
ALTER COLUMN	Initialized_From_Occurrence	BIT NOT NULL
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
	$Revision: 1 $
	$Date: 18/01/11 9:56 $
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
				THEN O.Comment
				WHEN 'Gathering Location Name'
				THEN	CASE Sl.Location_Name
							WHEN NULL
							THEN SE.Location_Name
							ELSE Sl.Location_Name
						END
				WHEN 'Determiner'
				THEN I_TDa.Name_Key
				WHEN 'Determiner role'
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
				THEN RT.Record_Type_Key
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
				THEN dbo.ufn_RtfToPlaintext(O.Comment)
				WHEN 'Gathering Location Name'
				THEN	CASE Sl.Location_Name
							WHEN NULL
							THEN SE.Location_Name
							ELSE Sl.Location_Name
						END
				WHEN 'Determiner'
				THEN dbo.ufn_GetFormattedName(I_TDa.Name_Key)
				WHEN 'Determiner role'
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
				THEN RT.Short_Name
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
LEFT JOIN	Occurrence_Data			AS	OD
	ON		O.Occurrence_Key		=	OD.Occurrence_Key
INNER JOIN	QE_Template_Field		AS	TF
	ON		TF.QE_Template_Key		=	Sn.QE_Template_Key	
	AND		(
				(TF.General_Tab		=	1
				AND DR.General		=	1)
			OR
				(TF.Specimen_Tab	=	1
				AND DR.General		=	0)
			)
	AND		(TF.QE_Field_Key	IS NOT NULL
			
			OR

			TF.QE_Field_Key	IS NULL
			AND TF.Is_Custom	=	1
			AND TF.Measurement_Is_Specimen	= 0
			AND TF.Measurement_Is_TaxonData = 0
			AND TF.Measurement_Applies_To = OD.Applies_To
			AND	TF.Measurement_Method_Concept_Key = OD.Method_Concept_Key
			AND	TF.Measurement_Duration = OD.Duration
			AND TF.Measurement_Accuracy = OD.Accuracy
			AND	TF.Measurement_Parameter_Concept_Key = OD.Parameter_Concept_Key
			AND	TF.Measurement_Unit_Concept_Key = OD.Unit_Concept_Key)
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

			(Sl.Location_Key	IS NULL
			AND SE.Location_Key = LN.Location_Key))
	AND		LN.Preferred			=	1

-- Gathering method
LEFT JOIN	Sample_Type			AS	ST
	ON		Sl.Sample_Type_Key	=	ST.Sample_Type_Key

-- Field Collector
LEFT JOIN	Survey_Event_Recorder	AS	SER
	ON		SE.Survey_Event_Key		=	SER.Survey_Event_Key
	AND		F.Item_Name				=	'Field Collector'
LEFT JOIN	Individual				AS	I_SER
	ON		SER.Name_Key			=	I_SER.Name_Key

-- Field Record Type
LEFT JOIN	Record_Type	AS	RT
	ON		O.Record_Type_Concept_Key	=	RT.Record_Type_Key

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
	$Revision: 1 $
	$Date: 18/01/11 9:56 $
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
				THEN "TO".Comment
				WHEN 'Gathering Location Name'
				THEN	CASE Sl.Location_Name
							WHEN NULL
							THEN SE.Location_Name
							ELSE Sl.Location_Name
						END
				WHEN 'Determiner'
				THEN I_TDa.Name_Key
				WHEN 'Determiner role'
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
				THEN dbo.ufn_RtfToPlaintext("TO".Comment)
				WHEN 'Gathering Location Name'
				THEN	CASE Sl.Location_Name
							WHEN NULL
							THEN SE.Location_Name
							ELSE Sl.Location_Name
						END
				WHEN 'Determiner'
				THEN dbo.ufn_GetFormattedName(I_TDa.Name_Key)
				WHEN 'Determiner role'
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
LEFT JOIN	Taxon_Occurrence_Data	AS	TOD
	ON		"TO".Taxon_Occurrence_Key	=	TOD.Taxon_Occurrence_Key
INNER JOIN	QE_Template_Field		AS	TF
	ON		TF.QE_Template_Key		=	Sn.QE_Template_Key	
	AND		(
				(TF.General_Tab		=	1
				AND DR.General		=	1)
			OR
				(TF.Specimen_Tab	=	1
				AND DR.General		=	0)
			)
	AND		(TF.QE_Field_Key	IS NOT NULL
			
			OR

			TF.QE_Field_Key	IS NULL
			AND TF.Is_Custom	=	1
			AND TF.Measurement_Is_Specimen	= 0
			AND TF.Measurement_Is_TaxonData = 1
			AND TF.Taxon_Measurement_Qualifier_Key = TOD.Measurement_Qualifier_Key
			AND	TF.Taxon_Measurement_Unit_Key = TOD.Measurement_Unit_Key
			AND TF.Measurement_Accuracy = TOD.Accuracy)
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
LEFT JOIN	Individual	AS	I_TDa
	ON		TDa.Determiner = I_TDa.Name_Key

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
	AND		LN.Preferred			=	1

-- Gathering method
LEFT JOIN	Sample_Type			AS	ST
	ON		Sl.Sample_Type_Key	=	ST.Sample_Type_Key

-- Field Collector
LEFT JOIN	Survey_Event_Recorder	AS	SER
	ON		SE.Survey_Event_Key		=	SER.Survey_Event_Key
	AND		F.Item_Name				=	'Field Collector'
LEFT JOIN	Individual				AS	I_SER
	ON		SER.Name_Key			=	I_SER.Name_Key

-- Field Record Type
LEFT JOIN	Record_Type	AS	RT
	ON		"TO".Record_Type_Key	=	RT.Record_Type_Key

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
	$Revision: 1 $
	$Date: 18/01/11 9:56 $
	$Author: Robertjohnson $

\*===========================================================================*/

CREATE PROCEDURE dbo.usp_QETemplates_Select_ForOccurrence(
	@Item_Key CHAR(16)
)
AS

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
	WHERE		QET.Subject_Area_Key	IS NULL
		OR		Det.Determination_Key	IS NOT NULL

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
	$Revision: 1 $
	$Date: 18/01/11 9:56 $
	$Author: Robertjohnson $

\*===========================================================================*/

CREATE PROCEDURE dbo.usp_QETemplates_Select_ForTaxonOccurrence(
	@Item_Key CHAR(16)
)
AS

	DECLARE @SpecimenType INT
	SET		@SpecimenType = 0

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
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_QESession_Insert]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_QESession_Insert]
GO

/*===========================================================================*\
  Description:	Inserts a new quick entry session, including a row for the 
							general tab.  Display caption auto generated from the template
							name and the date.

  Parameters:	@Key OUTPUT - inserted key
							@QE_Template_Key - key of template
							@SessionID

  Created:	September 2003

  Last revision information:
    $Revision: 1 $
    $Date: 18/01/11 9:56 $
    $Author: Robertjohnson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_QESession_Insert]
	@Key					INT		OUTPUT,
	@QE_Template_Key		CHAR(16),
	@Occurrence_Key			CHAR(16) = NULL,
	@Taxon_Occurrence_Key	CHAR(16) = NULL,
	@SessionID				CHAR(16)
AS

DECLARE @Item_Name VARCHAR(50)

-- Read template name, truncate if necessary
SELECT @Item_Name = 
	CASE WHEN Len(Item_Name)>38 THEN
		Left(Item_Name, 36) + '..'
	ELSE
		Item_Name
	END
FROM QE_Template
WHERE QE_Template_Key=@QE_Template_Key

-- and append the date
SET @Item_Name = 
		@Item_Name + ' ' + 
		CONVERT(VARCHAR(11), GETDATE(), 106)

-- Ensure name is unique
DECLARE @Duplicates INT
SELECT @Duplicates = COUNT(*) FROM QE_Session 
WHERE Item_Name=@Item_Name
OR Item_Name LIKE @Item_Name + ' (%)'

IF @Duplicates>0
BEGIN
	WHILE EXISTS(SELECT 1 FROM QE_Session WHERE Item_Name = @Item_Name + ' (' + CONVERT(VARCHAR(8), @Duplicates+1) + ')')
		SET @Duplicates = @Duplicates + 1
	SET @Item_Name = @Item_Name + ' (' + CONVERT(VARCHAR(8), @Duplicates+1) + ')'  
END	
  
INSERT INTO QE_Session (
		QE_Template_Key,
		Item_Name,
		Entered_Session_ID,
		Occurrence_Key,
		Taxon_Occurrence_Key
)
VALUES (
	@QE_Template_Key,
	@Item_Name,
	@SessionID,
	@Occurrence_Key,
	@Taxon_Occurrence_Key
)

SET @Key=SCOPE_IDENTITY()  

INSERT INTO QE_Data_Row (
	QE_Session_Key, 
	General, 
	Validated, 
	Processed, 
	Entered_Session_ID)
VALUES (
  @Key, 
  1, 
  0, 
  0, 
  @SessionID)

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_QESession_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_QESession_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_QESession_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_QESession_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_QESession_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_QESession_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_QESession_Insert TO [Dev - JNCC SQL]
END

GO
