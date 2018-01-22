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
	$Revision: 3 $
	$Date: 21/06/16 13:25 $
	$Author: Christopherknight $

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
				THEN TDp.Determiner_Role_Key
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

-- Determiner
LEFT JOIN	Individual		AS	I_TDa
	ON		TDp.Determiner	=	I_TDa.Name_Key

-- Determiner Role
LEFT JOIN	Determiner_Role			AS	DetRl
	ON		TDp.Determiner_Role_Key =	DetRl.Determiner_Role_Key

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