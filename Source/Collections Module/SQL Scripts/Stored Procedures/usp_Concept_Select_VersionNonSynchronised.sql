/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'dbo.usp_Concept_Select_VersionNonSynchronised')
	   AND 	  Type = 'P')
    DROP PROCEDURE dbo.usp_Concept_Select_VersionNonSynchronised
GO

/*===========================================================================*\
  Description: Returns a list of concepts that are the top level for the 
    					 supplied concept group.

  Parameters:	@ConceptGroupKey


  Created:	May 2011

  Last revision information:
    $Revision: 1 $
    $Date: 26/05/11 15:23 $
    $Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].usp_Concept_Select_VersionNonSynchronised
	@ConceptGroupVersionKey char(16)
AS

SET ANSI_NULLS ON
SET ANSI_PADDING ON
SET ANSI_WARNINGS ON
SET ARITHABORT ON
SET CONCAT_NULL_YIELDS_NULL ON
SET QUOTED_IDENTIFIER ON
SET NO_BROWSETABLE OFF

	SELECT DISTINCT CT.Concept_Key, 
			CT.Item_Name, 
	  		CT.Concept_Rank_Key, 
			CT.Sort_Code,
			CT.PlainText  -- Required by the ORDER BY

	FROM 		VW_ConceptTerm CT
	LEFT JOIN	Taxon_Dictionary_Concept_Mapping CM 
			ON	CM.Concept_Key = CT.Concept_Key
	-- Filter all concepts from concept group
	INNER JOIN Concept_Group_Version CGV 
		ON CGV.Concept_Group_Key=CT.Concept_Group_Key
		AND CGV.Concept_Group_Version_Key=@ConceptGroupVersionKey
	-- Filter to all concepts where history puts them into the concept group version
	INNER JOIN (
		Concept_History CH
		LEFT JOIN Concept_Group_Version CGVFrom ON CGVFrom.Concept_Group_Version_Key=CH.Concept_Group_Version_From
		LEFT JOIN Concept_Group_Version CGVTo ON CGVTo.Concept_Group_Version_Key=CH.Concept_Group_Version_To
			) ON CH.Concept_Key=CT.Concept_Key
	WHERE 		(CH.Concept_Group_Version_From IS NULL OR CGVFrom.Sequence<=CGV.Sequence)
	AND			(CH.Concept_Group_Version_To IS NULL OR CGVTo.Sequence>=CGV.Sequence)
	AND			CM.Concept_Key IS NULL

	ORDER BY 	CT.Sort_Code, CT.PlainText  -- Use PlainText too, so list is alpha sorted when no Sort Codes.

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_Select_VersionNonSynchronised') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Concept_Select_VersionNonSynchronised'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Concept_Select_VersionNonSynchronised TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concept_Select_VersionNonSynchronised TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concept_Select_VersionNonSynchronised TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Concept_Select_VersionNonSynchronised TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Concept_Select_VersionNonSynchronised TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Concept_Select_VersionNonSynchronised TO [Dev - JNCC SQL]
END
GO
