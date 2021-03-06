SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

IF OBJECT_ID('dbo.usp_QESessionsFormNames_Select_ForOccurence') IS NOT NULL
	DROP PROCEDURE dbo.usp_QESessionsFormNames_Select_ForOccurence;
GO

/*===========================================================================*\
  Description:	Returns the list of unique form names associated with quick entry 
				sessions appropriate for occurence data.

  Parameters:	@Item_Key

  Created:	June 2016

  Last revision information:
    $Revision: 1 $
    $Date: 21/06/16 12:40 $
    $Author: Christopherknight $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_QESessionsFormNames_Select_ForOccurence]
	@Item_Key CHAR(16)

 AS

SET NOCOUNT ON

	DECLARE @SpecimenType INT
	SET		@SpecimenType = 1

SELECT DISTINCT
		QET.QE_Template_Key,
		QET.Item_Name
FROM QE_Template QET
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
		ORDER BY QET.Item_Name

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_QESessionsFormNames_Select_ForOccurence') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_QESessionsFormNames_Select_ForOccurence'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_QESessionsFormNames_Select_ForOccurence TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_QESessionsFormNames_Select_ForOccurence TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_QESessionsFormNames_Select_ForOccurence TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_QESessionsFormNames_Select_ForOccurence TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_QESessionsFormNames_Select_ForOccurence TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_QESessionsFormNames_Select_ForOccurence TO [Dev - JNCC SQL]
END

GO