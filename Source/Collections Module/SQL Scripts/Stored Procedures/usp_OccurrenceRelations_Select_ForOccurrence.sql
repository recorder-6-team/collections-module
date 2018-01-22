/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_OccurrenceRelations_Select_ForOccurrence]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_OccurrenceRelations_Select_ForOccurrence]
GO

/*===========================================================================*\
  Description:	Returns Occurrence Relation records for a given Occurrence.

  Parameters:	@Key	Occurrence Key

  Created:	November 2003

  Last revision information:
    $Revision: 2 $
    $Date: 12/05/04 10:00 $
    $Author: Anthonysimpson $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_OccurrenceRelations_Select_ForOccurrence]
	@Key char(16)
AS
	
SET NOCOUNT ON

	SELECT 	  	R.Occurrence_Relation_Key AS Item_Key,
			R.To_Occurrence_Key AS Related_Occurrence_Key,
			CT.Item_Name AS Related_Occurrence_Name,
			TRT.Thesaurus_Relation_Type_Key,
			TRT.Item_Name AS Thesaurus_Relation_Type_Name,
			R.Comment,
			R.Custodian,
			R.[Timestamp]
	FROM		Occurrence_Relation R
	INNER JOIN	Thesaurus_Relation_Type TRT ON TRT.Thesaurus_Relation_Type_Key = R.Thesaurus_Relation_Type_Key
	LEFT JOIN	Determination D ON R.To_Occurrence_Key = D.Occurrence_Key AND D.Preferred = 1
	LEFT JOIN	vw_ConceptTermPreferred CT ON D.Concept_Key = CT.Concept_Key
	WHERE		R.From_Occurrence_Key = @Key

SET NOCOUNT OFF 
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_OccurrenceRelations_Select_ForOccurrence') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_OccurrenceRelations_Select_ForOccurrence'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_OccurrenceRelations_Select_ForOccurrence TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_OccurrenceRelations_Select_ForOccurrence TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_OccurrenceRelations_Select_ForOccurrence TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_OccurrenceRelations_Select_ForOccurrence TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_OccurrenceRelations_Select_ForOccurrence TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_OccurrenceRelations_Select_ForOccurrence TO [Dev - JNCC SQL]
END

GO