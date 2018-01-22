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
	$Revision: 3 $
	$Date: 21/06/16 12:45 $
	$Author: Christopherknight $

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
    ORDER BY QET.Item_Name

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