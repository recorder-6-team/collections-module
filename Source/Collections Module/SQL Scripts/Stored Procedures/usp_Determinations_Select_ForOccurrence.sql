/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Determinations_Select_ForOccurrence]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Determinations_Select_ForOccurrence]
GO

/*===========================================================================*\
  Description:	Returns determination records for a specified occurrence key.
		Gets the determinations through the direct relationship between
		Occurrence and Determination, and through the Specimen_Field_Data
		route.

  Parameters:	@OccurrenceKey

  Created:	October 2003

  Last revision information:
    $Revision: 1 $
    $Date: 19/11/03 9:45 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Determinations_Select_ForOccurrence]
	@OccurrenceKey char(16)
AS

SET NOCOUNT ON

	-- Use direct link from Occurrence -> Determination
	SELECT		D.Determination_Key AS [Item_Key], 
			CT.Item_Name
	FROM		vw_ConceptTerm CT
	INNER JOIN	Determination D ON D.Concept_Key = CT.Concept_Key
	WHERE		D.Occurrence_Key = @OccurrenceKey

	UNION

	-- Round trip from Occurrence -> Specimen_Field_Data -> Specimen_Unit -> Determination
	SELECT		D.Determination_Key AS [Item_Key], 
			CT.Item_Name
	FROM		Specimen_Field_Data SFD 
	INNER JOIN	Specimen_Unit SU ON SU.Collection_Unit_Key = SFD.Collection_Unit_Key
	INNER JOIN	Determination D ON D.Specimen_Collection_Unit_Key = SU.Collection_Unit_Key
	INNER JOIN	vw_ConceptTerm CT ON CT.Concept_Key = D.Concept_Key
	WHERE		SFD.Occurrence_Key = @OccurrenceKey

	-- And order properly
	ORDER BY	Item_Name

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Determinations_Select_ForOccurrence') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Determinations_Select_ForOccurrence'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Determinations_Select_ForOccurrence TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Determinations_Select_ForOccurrence TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Determinations_Select_ForOccurrence TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Determinations_Select_ForOccurrence TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Determinations_Select_ForOccurrence TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Determinations_Select_ForOccurrence TO [Dev - JNCC SQL]
END
GO