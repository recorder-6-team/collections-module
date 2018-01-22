SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

/*===========================================================================*\
  Description:	
	For all templates that only contain 'Gathering Site' out of the fields 
	'Gathering Site', 'Gathering Location Name' and 'Gathering Spatial Reference', 
	adds the field 'Gathering Location Name' to the templates.
    
  Created:	September 2010

  Last revision information:
    $Revision: 1 $
    $Date: 13/09/10 11:48 $
    $Author: Robertjohnson $

\*===========================================================================*/

DECLARE	@GatheringLocationNameKey		CHAR(16),
		@GatheringSpatialReferenceKey	CHAR(16),
		@NewTemplateFieldKey			CHAR(16),
		@OldTemplateFieldKey			CHAR(16),
		@TemplateKey					CHAR(16),
		@NewSequenceNumber				INT

SELECT	@GatheringLocationNameKey = QE_Field_Key
FROM	QE_Field
WHERE	Item_Name = 'Gathering Location Name'

SELECT	@GatheringSpatialReferenceKey = QE_Field_Key
FROM	QE_Field
WHERE	Item_Name = 'Gathering Spatial Reference'

SELECT		@TemplateKey			=	T.QE_Template_Key,
			@OldTemplateFieldKey	=	TF1.QE_Template_Field_Key
FROM		QE_Template				AS	T
INNER JOIN	QE_Template_Field		AS	TF1
ON			T.QE_Template_Key		=	TF1.QE_Template_Key
INNER JOIN	QE_Field				AS	F
ON			TF1.QE_Field_Key		=	F.QE_Field_Key
	AND		F.Item_Name				=	'Gathering Site'
LEFT JOIN	QE_Template_Field		AS	TF2
ON			T.QE_Template_Key		=	TF2.QE_Template_Key
	AND		TF2.QE_Field_Key		=	@GatheringLocationNameKey
LEFT JOIN	QE_Template_Field		AS	TF3
ON			T.QE_Template_Key		=	TF3.QE_Template_Key
	AND		TF3.QE_Field_Key		=	@GatheringSpatialReferenceKey
WHERE		TF2.QE_Template_Field_Key	IS	NULL
	AND		TF3.QE_Template_Field_Key	IS	NULL

EXEC spNextKey 'QE_Template_Field', @NewTemplateFieldKey OUTPUT

SELECT TOP(1)	@NewSequenceNumber	= Sequence + 1
FROM			QE_Template_Field
WHERE			QE_Template_Key		= @TemplateKey
ORDER BY		Sequence DESC

INSERT INTO	QE_Template_Field (
			QE_Template_Field_Key,
			QE_Template_Key,
			QE_Field_Key,
			IS_Custom,
			General_Tab,
			Specimen_Tab,
			Measurement_Is_Specimen,
			Sequence,
			Entered_Session_ID,
			Changed_Session_ID,
			System_Supplied_Data,
			Measurement_Is_TaxonData,
			Number_Preferred)
SELECT		@NewTemplateFieldKey,
			QE_Template_Key,
			@GatheringLocationNameKey,
			0,
			General_Tab,
			Specimen_Tab,
			0,
			@NewSequenceNumber,
			Entered_Session_ID,
			Changed_Session_ID,
			System_Supplied_Data,
			0,
			0
FROM		QE_Template_Field
WHERE		QE_Template_Field_Key = @OldTemplateFieldKey
GO