/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE  Id = Object_Id(N'[dbo].[usp_ConceptGroupVersions_Select_ForConceptGroup]') AND Type = 'P')
	DROP PROCEDURE [dbo].[usp_ConceptGroupVersions_Select_ForConceptGroup]
GO

/*===========================================================================*\
  Description:	Returns Concept Group Version records.

  Parameters:	@ConceptGroupKey	Concept Group key

  Created:	November 2003

  Last revision information:
    $Revision: 2 $
    $Date: 10/09/08 14:37 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptGroupVersions_Select_ForConceptGroup]
	@ParentKey CHAR(16)
AS

SET NOCOUNT ON

	SELECT DISTINCT
			Concept_Group_Version_Key AS [Key],
		  	Version 
				+ ' ('
				+ dbo.ufn_GetDateFromVagueDate(From_Vague_Date_Start,
								From_Vague_Date_End,
								From_Vague_Date_Type) 
				+ IsNull((' - ' + dbo.ufn_GetDateFromVagueDate(To_Vague_Date_Start,
								To_Vague_Date_End,
								To_Vague_Date_Type)), '')
				+ ')'
			AS Item_Name,
		  	Concept_Group_Key,
			0 AS Has_Children,
			"Sequence"
	FROM 	  	Concept_Group_Version
	WHERE	  	Concept_Group_Key = @ParentKey
	ORDER BY  	Item_Name

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptGroupVersions_Select_ForConceptGroup') AND SysStat & 0xf = 4)
BEGIN
   	PRINT 'Setting up security on procedure usp_ConceptGroupVersions_Select_ForConceptGroup'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
       	GRANT EXECUTE ON dbo.usp_ConceptGroupVersions_Select_ForConceptGroup TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptGroupVersions_Select_ForConceptGroup TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptGroupVersions_Select_ForConceptGroup TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ConceptGroupVersions_Select_ForConceptGroup TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptGroupVersions_Select_ForConceptGroup TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
       	GRANT EXECUTE ON dbo.usp_ConceptGroupVersions_Select_ForConceptGroup TO [Dev - JNCC SQL]
END
GO