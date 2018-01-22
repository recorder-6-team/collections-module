/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
       FROM   SysObjects 
       WHERE  Id = Object_Id(N'[dbo].[usp_Concept_Select_ForConceptGroupSearch]')
       AND    Type = 'P')
    DROP PROCEDURE [dbo].[usp_Concept_Select_ForConceptGroupSearch]
GO

/*===========================================================================*\
  Description:  Retrieves a list of concepts that match a search string, in a 
                                specified concept group.

  Parameters:   @ConceptGroup - key of the concept group
                            @SearchText - search text

  Created:  August 2003

  Last revision information:
    $Revision: 17 $
    $Date: 26/08/11 14:44 $
    $Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Concept_Select_ForConceptGroupSearch]
	@SearchKey char(16),
	@SearchText varchar(100),
	@SearchSize int = NULL
AS

SET ANSI_NULLS ON
SET ANSI_PADDING ON
SET ANSI_WARNINGS ON
SET ARITHABORT ON
SET CONCAT_NULL_YIELDS_NULL ON
SET QUOTED_IDENTIFIER ON
SET NO_BROWSETABLE OFF

    SET @SearchSize =   ISNULL(@SearchSize, 0)

    SET ROWCOUNT @SearchSize

	SELECT DISTINCT CT.Concept_Key as Item_Key,
	  CT.Item_Name AS DisplayTerm,
	  CT.Item_Name AS SearchTerm,
	  CT.Author_copy,
	  CT.Concept_Rank_Key
	FROM VW_ConceptTerm CT
	LEFT JOIN Search_Term ST on ST.Concept_Key = CT.Concept_Key
	WHERE CT.Concept_Group_Key = @SearchKey
	AND (ST.Plaintext like @SearchText + '%' 
	OR CT.Author_Copy like @SearchText + '%')
	AND CT.Is_Current = 1
	ORDER BY SearchTerm, Author_Copy

    SET ROWCOUNT 0    
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_Select_ForConceptGroupSearch') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_Concept_Select_ForConceptGroupSearch'
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        GRANT EXECUTE ON dbo.usp_Concept_Select_ForConceptGroupSearch TO [R2k_AddOnly]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
        GRANT EXECUTE ON dbo.usp_Concept_Select_ForConceptGroupSearch TO [R2k_Administrator]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
        GRANT EXECUTE ON dbo.usp_Concept_Select_ForConceptGroupSearch TO [R2k_FullEdit]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
        GRANT EXECUTE ON dbo.usp_Concept_Select_ForConceptGroupSearch TO [R2k_ReadOnly]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
        GRANT EXECUTE ON dbo.usp_Concept_Select_ForConceptGroupSearch TO [R2k_RecordCardsOnly]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_Concept_Select_ForConceptGroupSearch TO [Dev - JNCC SQL]
END

GO
