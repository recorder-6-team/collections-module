/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptHistory_Select_ForConcept]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptHistory_Select_ForConcept]
GO

/*===========================================================================*\
  Description:	Returns Concept_History records for a given concept key.

  Parameters:	@Key	Concept_Key

  Created:	December 2003

  Last revision information:
    $Revision: 7 $
    $Date: 1/04/04 11:21 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptHistory_Select_ForConcept]
	@Key char(16)
AS

SET NOCOUNT ON

	SELECT
			CH.Concept_History_Key AS Item_Key,
			IsNull(
 				IsNull(CGV1.Version, '') + 
				IsNull(' (' + dbo.ufn_GetDateFromVagueDate(
						CH.From_Vague_Date_Start, CH.From_Vague_Date_End, CH.From_Vague_Date_Type) + ')', '') + ' -> ', '') 
			+ 
			IsNull(CGV2.Version, '') + 
			IsNull(' (' + dbo.ufn_GetDateFromVagueDate(
					CH.To_Vague_Date_Start, CH.To_Vague_Date_End, CH.To_Vague_Date_Type) + ') ', '')
			AS Item_Name
	FROM		Concept_History AS CH	
	LEFT JOIN	Concept_Group_Version AS CGV1 ON CGV1.Concept_Group_Version_Key = CH.Concept_Group_Version_From
	LEFT JOIN	Concept_Group_Version AS CGV2 ON CGV2.Concept_Group_Version_Key = CH.Concept_Group_Version_To
	WHERE		CH.Concept_Key = @Key
	ORDER BY	CH.From_Vague_Date_Start, CH.From_Vague_Date_End

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptHistory_Select_ForConcept') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptHistory_Select_ForConcept'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptHistory_Select_ForConcept TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptHistory_Select_ForConcept TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptHistory_Select_ForConcept TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ConceptHistory_Select_ForConcept TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptHistory_Select_ForConcept TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptHistory_Select_ForConcept TO [Dev - JNCC SQL]
END

GO