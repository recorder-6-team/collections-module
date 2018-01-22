/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_SpecimenFieldData_Select_ForOccurrence]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_SpecimenFieldData_Select_ForOccurrence]
GO

/*===========================================================================*\
  Description:	Returns a list of specimens linked to an occurrence.

  Parameters:	@OccurrenceKey

  Created:	November 2003

  Last revision information:
    $Revision: 1 $
    $Date: 6/02/07 16:36 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SpecimenFieldData_Select_ForOccurrence] 
	@OccurrenceKey 		CHAR(16),
	@IsTaxonOccurrence 	BIT,
	@UserDomainMask		INT,
	@SessionID		CHAR(16)
AS

SET NOCOUNT ON

	SELECT		SFD.Specimen_Field_Data_Key AS Item_Key,
			SFD.Collection_Unit_Key AS Specimen_Key,
		    	CASE 
				WHEN SU.Life_Sciences = 0 THEN CT.Item_Name COLLATE SQL_Latin1_General_CP1_CI_AS
				ELSE 
					CASE ITN.Actual_Name_Italic 
						WHEN 1 THEN '<i>' + ITN.Actual_Name + '</i>'
						ELSE ITN.Actual_Name 
					END
			END +
			CASE 
				WHEN CUN.Number IS NOT NULL THEN + ' - ' + CUN.Number 
				ELSE + '' 
			END AS Display_Text,
			SFD.Timestamp,
			SFD.Custodian

	FROM		Specimen_Field_Data SFD
	INNER JOIN	(Specimen_Unit SU 
			    	INNER JOIN	Collection_Unit CU 
					ON 	CU.Collection_Unit_Key = SU.Collection_Unit_Key
					AND 	((CU.Domain_Mask & @UserDomainMask > 0) 
						OR (CU.Entered_Session_ID = @SessionID) 
						OR (CU.Changed_Session_ID = @SessionID) 
						OR (CU.Domain_Mask = 0)))
			ON SU.Collection_Unit_Key = SFD.Collection_Unit_Key
	LEFT JOIN	(Collection_Unit_Number CUN 
				INNER JOIN	Concept C ON C.Concept_Key = CUN.Type_Concept_Key 
					AND 	C.Meaning_Key = 'SYSTEM0000000001')  -- Registration Number
			ON CUN.Collection_Unit_Key = SFD.Collection_Unit_Key 
			AND CUN.Preferred = 1
	LEFT JOIN	Determination D 
				INNER JOIN	vw_ConceptTerm CT ON CT.Concept_Key = D.Concept_Key
			ON D.Determination_Key = SU.Preferred_Determination_Key
	LEFT JOIN 	(Taxon_Determination TD
				INNER JOIN	Index_Taxon_Name ITN ON TD.Taxon_List_Item_Key = ITN.Taxon_List_Item_Key)
			ON SU.Preferred_Taxon_Determination_Key = TD.Taxon_Determination_Key
			OR (SU.Preferred_Taxon_Determination_Key IS NULL AND TD.Taxon_Occurrence_Key = @OccurrenceKey)

	WHERE		(@IsTaxonOccurrence = 0 AND SFD.Occurrence_Key = @OccurrenceKey)
	OR		(@IsTaxonOccurrence = 1 AND SFD.Taxon_Occurrence_Key = @OccurrenceKey)

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SpecimenFieldData_Select_ForOccurrence') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SpecimenFieldData_Select_ForOccurrence'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SpecimenFieldData_Select_ForOccurrence TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SpecimenFieldData_Select_ForOccurrence TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SpecimenFieldData_Select_ForOccurrence TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_SpecimenFieldData_Select_ForOccurrence TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SpecimenFieldData_Select_ForOccurrence TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SpecimenFieldData_Select_ForOccurrence TO [Dev - JNCC SQL]
END
GO
