/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptGroupVersions_Select_ForConcept]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptGroupVersions_Select_ForConcept]
GO

/*===========================================================================*\
  Description:	Returns Concept Group Version records.

  Parameters:	@ConceptKey	Concept_Key

  Created:	December 2003

  Last revision information:
    $Revision: 2 $
    $Date: 23/03/04 11:10 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptGroupVersions_Select_ForConcept]
	@ConceptKey CHAR(16)
AS

SET NOCOUNT ON

	SELECT DISTINCT
			CGV.Concept_Group_Version_Key AS [Key],
		  	CGV.Version AS Item_Name,
			'('	+ dbo.ufn_GetDateFromVagueDate(CGV.From_Vague_Date_Start,
								CGV.From_Vague_Date_End,
								CGV.From_Vague_Date_Type) 
				+ IsNull((' - ' + dbo.ufn_GetDateFromVagueDate(CGV.To_Vague_Date_Start,
								CGV.To_Vague_Date_End,
								CGV.To_Vague_Date_Type)), '')
				+ ')'
			AS Dates,
			CGV.[Sequence]
	FROM 	  	Concept AS C
	INNER JOIN	Concept_Group_Version AS CGV ON CGV.Concept_Group_Key = C.Concept_Group_Key
	WHERE	  	C.Concept_Key = @ConceptKey
	ORDER BY  	Item_Name

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptGroupVersions_Select_ForConcept') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptGroupVersions_Select_ForConcept'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptGroupVersions_Select_ForConcept TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptGroupVersions_Select_ForConcept TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptGroupVersions_Select_ForConcept TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ConceptGroupVersions_Select_ForConcept TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptGroupVersions_Select_ForConcept TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptGroupVersions_Select_ForConcept TO [Dev - JNCC SQL]
END

GO