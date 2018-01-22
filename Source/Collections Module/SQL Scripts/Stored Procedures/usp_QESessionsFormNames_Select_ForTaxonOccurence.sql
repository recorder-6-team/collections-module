SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

IF OBJECT_ID('dbo.usp_QESessionsFormNames_Select_ForTaxonOccurence') IS NOT NULL
	DROP PROCEDURE dbo.usp_QESessionsFormNames_Select_ForTaxonOccurence;
GO

/*===========================================================================*\
  Description:	Returns the list of unique form names associated with quick entry 
				sessions appropriate for taxon occurence data.

  Parameters:	@Item_Key

  Created:	June 2016

  Last revision information:
    $Revision: 1 $
    $Date: 21/06/16 12:40 $
    $Author: Christopherknight $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_QESessionsFormNames_Select_ForTaxonOccurence]
	@Item_Key CHAR(16)

 AS

SET NOCOUNT ON

	DECLARE @SpecimenType INT
	SET		@SpecimenType = 1

SELECT DISTINCT
		QET.QE_Template_Key,
		QET.Item_Name
FROM QE_Template QET
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
		AND		TD.Taxon_Occurrence_Key	=	@Item_Key
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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_QESessionsFormNames_Select_ForTaxonOccurence') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure dbo.usp_QESessionsFormNames_Select_ForTaxonOccurence'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_QESessionsFormNames_Select_ForTaxonOccurence TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_QESessionsFormNames_Select_ForTaxonOccurence TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_QESessionsFormNames_Select_ForTaxonOccurence TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_QESessionsFormNames_Select_ForTaxonOccurence TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_QESessionsFormNames_Select_ForTaxonOccurence TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_QESessionsFormNames_Select_ForTaxonOccurence TO [Dev - JNCC SQL]
END

GO