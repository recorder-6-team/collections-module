/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'dbo.usp_Concept_Select_Unique')
	   AND 	  Type = 'P')
    DROP PROCEDURE dbo.usp_Concept_Select_Unique
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
CREATE PROCEDURE [dbo].usp_Concept_Select_Unique
	@ConceptGroupKey char(16)
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
	LEFT JOIN	(SELECT * FROM Concept WHERE Concept_Group_Key <> @ConceptGroupKey) AS Syn
			ON CT.Meaning_Key = Syn.Meaning_Key
	WHERE 		CT.List_Preferred = 1
	AND 		CT.Is_Current = 1
	AND 		CT.Concept_Group_Key = @ConceptGroupKey
	AND			Syn.Concept_Key IS NULL

	ORDER BY 	CT.Sort_Code, CT.PlainText  -- Use PlainText too, so list is alpha sorted when no Sort Codes.

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_Select_Unique') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Concept_Select_Unique'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Concept_Select_Unique TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concept_Select_Unique TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concept_Select_Unique TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Concept_Select_Unique TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Concept_Select_Unique TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Concept_Select_Unique TO [Dev - JNCC SQL]
END
GO
