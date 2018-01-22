/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_SpecimenLabel_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_SpecimenLabel_Select]
GO

/*===========================================================================*\
  Description:	Returns a Specimen Label record.

  Parameters:	@Key	Specimen Label key

  Created:	Setember 2003

  Last revision information:
    $Revision: 6 $
    $Date: 18/11/09 13:15 $
    $Author: Simonlewis $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SpecimenLabel_Select]
	@Key char(16)
AS
	SET NOCOUNT ON
	SET ANSI_NULLS ON
	SET ANSI_PADDING ON
	SET ANSI_WARNINGS ON
	SET ARITHABORT ON
	SET CONCAT_NULL_YIELDS_NULL ON
	SET QUOTED_IDENTIFIER ON
	SET NO_BROWSETABLE OFF

	SELECT		S.Specimen_Label_Key,
				S.Collection_Unit_Key,
				S.Is_Inscription,
				S.Label_Position,
				S.Inscription,
				S.Translated,
				S.Translated_Language_Key,
				L.Language_Key + ' - ' + L.Item_Name		AS	Language,
				S.Comments,
				S.Author_Name_Key,
				dbo.ufn_GetFormattedName(S.Author_Name_Key)	AS	AuthorName,
				S.Inferred_Author,
				S.Confidence_Concept_Key,
				TConfidence.Item_Name						AS	Confidence_Name,
				S.Entered_Session_ID,
				S.Changed_Session_ID,
				S.Custodian,
				S.[Timestamp],
				S.Is_Current
	FROM		Specimen_Label								AS	S
	LEFT JOIN 	Language									AS	L
	ON			L.Language_Key								=	S.Translated_Language_Key 
	LEFT JOIN	VW_ConceptTerm								AS	TConfidence
	ON			TConfidence.Concept_Key						=	S.Confidence_Concept_Key
	WHERE 		Specimen_Label_Key							=	@Key

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SpecimenLabel_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SpecimenLabel_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SpecimenLabel_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SpecimenLabel_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SpecimenLabel_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_SpecimenLabel_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SpecimenLabel_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SpecimenLabel_Select TO [Dev - JNCC SQL]
END

GO
