/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptHistory_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptHistory_Select]
GO

/*===========================================================================*\
  Description:	Returns data from the Concept_History table

  Parameters:	@Key	Concept_History_Key

  Created:	December 2003

  Last revision information:
    $Revision: 4 $
    $Date: 1/04/04 11:21 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptHistory_Select]
	@Key char(16)
AS

SET NOCOUNT ON

	SELECT 	
			CH.From_Vague_Date_Start,
			CH.From_Vague_Date_End,
			CH.From_Vague_Date_Type,
			CH.Concept_Group_Version_From AS Concept_Group_Version_From_Key,
			FromCGV.Version + ' (' + 
				IsNull(dbo.ufn_GetDateFromVagueDate(FromCGV.From_Vague_Date_Start, FromCGV.From_Vague_Date_End, FromCGV.From_Vague_Date_Type), '') +
				IsNull(' - ' + dbo.ufn_GetDateFromVagueDate(FromCGV.To_Vague_Date_Start, FromCGV.To_Vague_Date_End, FromCGV.To_Vague_Date_Type), '') + ')' AS Concept_Group_Version_From_Name,
			CH.To_Vague_Date_Start,
			CH.To_Vague_Date_End,
			CH.To_Vague_Date_Type,
			CH.Concept_Group_Version_To AS Concept_Group_Version_To_Key,
			ToCGV.Version + ' (' + 
				IsNull(dbo.ufn_GetDateFromVagueDate(ToCGV.From_Vague_Date_Start, ToCGV.From_Vague_Date_End, ToCGV.From_Vague_Date_Type), '') +
				IsNull(' - ' + dbo.ufn_GetDateFromVagueDate(ToCGV.To_Vague_Date_Start, ToCGV.To_Vague_Date_End, ToCGV.To_Vague_Date_Type), '') + ')' AS Concept_Group_Version_To_Name,
			CH.[Timestamp]
	FROM		Concept_History AS CH
	LEFT JOIN	Concept_Group_Version AS FromCGV ON FromCGV.Concept_Group_Version_Key = CH.Concept_Group_Version_From
	LEFT JOIN	Concept_Group_Version AS ToCGV ON ToCGV.Concept_Group_Version_Key = CH.Concept_Group_Version_To
	WHERE 		Concept_History_Key = @Key

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptHistory_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptHistory_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptHistory_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptHistory_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptHistory_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ConceptHistory_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptHistory_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptHistory_Select TO [Dev - JNCC SQL]
END

GO