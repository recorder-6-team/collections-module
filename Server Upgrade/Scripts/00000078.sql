/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
       FROM   SysObjects 
       WHERE  Id = Object_Id(N'[dbo].[usp_Concept_Select_ForSubjectAreaSearch]')
       AND    Type = 'P')
    DROP PROCEDURE [dbo].[usp_Concept_Select_ForSubjectAreaSearch]
GO

/*===========================================================================*\
  Description:  Retrieves a list of concepts that match a search string, in a 
                                specified subject area.

  Parameters:   @SearchKey - key of the subject area
                            @SearchText - search text

  Created:  August 2003

  Last revision information:
    $Revision: 1 $
    $Date: 6/12/16 11:48 $
    $Author: Andrewkemp $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Concept_Select_ForSubjectAreaSearch]
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

	SELECT CT.Concept_Key as Item_Key,
	  CT.Item_Name AS DisplayTerm,
	  CT.Item_Name AS SearchTerm,
	  CT.Author_copy,
	  CT.Concept_Rank_Key
	FROM VW_ConceptTerm CT
	INNER JOIN Concept_Group CG ON CT.Concept_Group_Key = CG.Concept_Group_Key
	INNER JOIN Local_Domain LD ON CG.Local_Domain_Key = LD.Local_Domain_Key
	INNER JOIN Domain D ON LD.Domain_Key = D.Domain_Key
	LEFT JOIN Search_Term ST ON ST.Concept_Key = CT.Concept_Key
	WHERE D.Subject_Area_Key = @SearchKey
	AND (ST.Plaintext like @SearchText + '%'
	OR CT.Author_Copy like @SearchText + '%')
	AND CT.Is_Current = 1
	ORDER BY SearchTerm, CT.Author_Copy

    SET ROWCOUNT 0   
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_Select_ForSubjectAreaSearch') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_Concept_Select_ForSubjectAreaSearch'
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        GRANT EXECUTE ON dbo.usp_Concept_Select_ForSubjectAreaSearch TO [R2k_AddOnly]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
        GRANT EXECUTE ON dbo.usp_Concept_Select_ForSubjectAreaSearch TO [R2k_Administrator]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
        GRANT EXECUTE ON dbo.usp_Concept_Select_ForSubjectAreaSearch TO [R2k_FullEdit]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
        GRANT EXECUTE ON dbo.usp_Concept_Select_ForSubjectAreaSearch TO [R2k_ReadOnly]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
        GRANT EXECUTE ON dbo.usp_Concept_Select_ForSubjectAreaSearch TO [R2k_RecordCardsOnly]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_Concept_Select_ForSubjectAreaSearch TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Concept_Select_ForConceptGroupVersionSearch]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Concept_Select_ForConceptGroupVersionSearch]
GO

/*===========================================================================*\
  Description:	Retrieves a list of concepts that match a search string, in a 
 								specified concept group version.

  Parameters:	@SearchKey - key of the concept group version
							@SearchText - search text

  Created:	August 2003

  Last revision information:
    $Revision: 1 $
    $Date: 6/12/16 11:48 $
    $Author: Andrewkemp $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Concept_Select_ForConceptGroupVersionSearch]
	@SearchKey char(16),
  @SearchText varchar(100)
AS

SET ANSI_NULLS ON
SET ANSI_PADDING ON
SET ANSI_WARNINGS ON
SET ARITHABORT ON
SET CONCAT_NULL_YIELDS_NULL ON
SET QUOTED_IDENTIFIER ON
SET NO_BROWSETABLE OFF


SELECT DISTINCT CT.Concept_Key as Item_Key, 
  CT.Item_Name AS DisplayTerm,
  CT.Item_Name AS SearchTerm, 
  CT.Author_copy,
  CT.Concept_Rank_Key
FROM VW_ConceptTerm CT
  INNER JOIN Concept_Group_Version CGV on CGV.Concept_Group_Key=CT.Concept_Group_Key
      AND CGV.Concept_Group_Version_Key=@SearchKey
  LEFT JOIN Search_Term ST on ST.Concept_Key = CT.Concept_Key
  LEFT JOIN Concept_History CH on CH.Concept_Key=CT.Concept_Key
  LEFT JOIN Concept_Group_Version CGV1 ON CGV1.Concept_Group_Version_Key=CH.Concept_Group_Version_From
  LEFT JOIN Concept_Group_Version CGV2 ON CGV2.Concept_Group_Version_Key=CH.Concept_Group_Version_To
WHERE (ST.Plaintext like @SearchText + '%'
  OR CT.Author_Copy like @SearchText + '%')
  AND (CGV1.Concept_Group_Version_Key IS NULL OR CGV1.Sequence<=CGV.Sequence)
  AND (CGV2.Concept_Group_Version_Key IS NULL OR CGV2.Sequence>=CGV.Sequence)
ORDER BY SearchTerm, CT.Author_Copy
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_Select_ForConceptGroupVersionSearch') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Concept_Select_ForConceptGroupVersionSearch'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Concept_Select_ForConceptGroupVersionSearch TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForConceptGroupVersionSearch TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForConceptGroupVersionSearch TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForConceptGroupVersionSearch TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForConceptGroupVersionSearch TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Concept_Select_ForConceptGroupVersionSearch TO [Dev - JNCC SQL]
END

GO
