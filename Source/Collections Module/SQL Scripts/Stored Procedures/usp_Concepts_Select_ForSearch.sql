If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Concepts_Select_ForSearch]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Concepts_Select_ForSearch]
GO

/*===========================================================================*\
  Description: Search proc for concepts where Term_Version_Key is not null.

  Parameters:	@SearchText

  Created:	December 2003

  Last revision information:
    $Revision: 6 $
    $Date: 26/08/11 14:46 $
    $Author: Jamesbichard $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_Concepts_Select_ForSearch] 
	@SearchText varchar(100)
AS

SET NOCOUNT ON

	SELECT DISTINCT
		CT.Concept_Key AS Item_Key,
		CT.Item_Name + ' - ' + CG.Item_Name AS DisplayTerm,
		CT.Item_Name + ' - ' + CG.Item_Name COLLATE SQL_Latin1_General_CP1_CI_AS AS SearchTerm
	FROM
		VW_ConceptTerm CT
	INNER JOIN Concept_Group CG ON CG.Concept_Group_Key=CT.Concept_Group_Key
	LEFT JOIN Search_Term ST ON ST.Concept_Key = CT.Concept_Key
	WHERE ST.Plaintext LIKE @SearchText + '%'	
	ORDER BY DisplayTerm
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concepts_Select_ForSearch') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Concepts_Select_ForSearch'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Concepts_Select_ForSearch TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concepts_Select_ForSearch TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concepts_Select_ForSearch TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Concepts_Select_ForSearch TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Concepts_Select_ForSearch TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Concepts_Select_ForSearch TO [Dev - JNCC SQL]
END

GO
