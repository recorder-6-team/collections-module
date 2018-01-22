SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

IF OBJECT_ID('dbo.usp_QEDataItem_Insert_ForSecondaryOccurrence') IS NOT NULL
	DROP PROCEDURE dbo.usp_QEDataItem_Insert_ForSecondaryOccurrence;
GO

/*===========================================================================*\
  Description:	Inserts @NumberOfRows of data into the QE_Data_Row table for
				the specified session.  Populates associated QE_Data_Item rows
				with data for the specified Occurrence.

  Parameters:	@QE_Session_Key
				@Occurrence_Key
				@NumberOfRows
				@SessionID

  Created:	June 2016

  Last revision information:
    $Revision: 1 $
    $Date: 21/06/16 12:48 $
    $Author: Christopherknight $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_QEDataItem_Insert_ForSecondaryOccurrence]
    @QE_Session_Key CHAR(16),
	@Occurrence_Key CHAR(16),
    @NumberOfRows INT,
	@SessionID CHAR(16)
 AS
BEGIN
SET NOCOUNT ON

DECLARE @DataRowIDs TABLE (QE_Data_Row_Key INT)
DECLARE @Occurrence_Temp TABLE (Occurrence_Key CHAR(16))

-- Insert @NumberOfRows rows into the QE_Data_Row table and get their ids.
INSERT INTO QE_Data_Row (QE_Session_Key, General, Validated, Processed, Entered_Session_ID)
OUTPUT inserted.QE_Data_Row_Key INTO @DataRowIDs
SELECT TOP (@NumberOfRows) 
	@QE_Session_Key, 
	0, 
	0, 
	0, 
	@SessionID 
FROM sys.all_columns

-- Insert relevant taxon occurrence for Cross Join to force prevention
-- of large data sets.
INSERT INTO @Occurrence_Temp
VALUES(@Occurrence_Key)

-- Insert N copies of the taxon occurence data
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
				THEN Dp.Determiner_Role_Key
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
INNER JOIN @DataRowIDs				AS	DRID
	ON DR.QE_Data_Row_Key = DRID.QE_Data_Row_Key
INNER JOIN	QE_Session				AS	Sn
	ON		DR.QE_Session_Key		=	Sn.QE_Session_Key
CROSS JOIN  @Occurrence_Temp	AS	OT
INNER JOIN  Occurrence		AS	O
	ON OT.Occurrence_Key = O.Occurrence_Key
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

-- Determiner
LEFT JOIN	Individual	AS	I_TDa
	ON		Dp.Determiner_Name_Key = I_TDa.Name_Key

-- Determiner Role
LEFT JOIN	Determiner_Role			AS	DetRl
	ON		Dp.Determiner_Role_Key =	DetRl.Determiner_Role_Key

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


END
GO
 
/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_QEDataItem_Insert_ForSecondaryOccurrence') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure dbo.usp_QEDataItem_Insert_ForSecondaryOccurrence'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_QEDataItem_Insert_ForSecondaryOccurrence TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_QEDataItem_Insert_ForSecondaryOccurrence TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_QEDataItem_Insert_ForSecondaryOccurrence TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_QEDataItem_Insert_ForSecondaryOccurrence TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_QEDataItem_Insert_ForSecondaryOccurrence TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_QEDataItem_Insert_ForSecondaryOccurrence TO [Dev - JNCC SQL]
END

GO