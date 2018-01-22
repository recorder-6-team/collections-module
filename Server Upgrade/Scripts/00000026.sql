/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Parents_Select_ForConcept]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Parents_Select_ForConcept]
GO

/*===========================================================================*\
  Description:	Returns parent concepts for a given concept key.

  Parameters:	@Key	Concept_Key

  Created:	January 2004

  Last revision information:
    $Revision: 1 $
    $Date: 9/07/07 14:13 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Parents_Select_ForConcept]
	@Key char(16)
AS

SET NOCOUNT ON

	/*===================================================================*\
	  Get the Default_Hierarchy_Relation_Type_Key for the domain.
	\*===================================================================*/
	DECLARE @Hierarchy_Relation_Type_Key char(16)

	SELECT 		@Hierarchy_Relation_Type_Key = CG.Hierarchy_Relation_Type_Key
	FROM		Concept_Group AS CG
	INNER JOIN	Concept AS C ON C.Concept_Group_Key = CG.Concept_Group_Key
	WHERE		C.Concept_Key = @Key

	/*================*\
	  Get the parents.
	\*================*/
	SELECT DISTINCT	C.Concept_Key AS Item_Key,
			T.Item_Name + ' ' +
				IsNull
					(IsNull('- ' + TV.Version_Label + ' (' + TV.Author_And_Date + ')', 
						IsNull('- ' + TV.Version_Label, '- ' + TV.Author_And_Date)), '')
			AS Item_Name
	FROM Concept CChild
	INNER JOIN Concept CChildSyn ON CChildSyn.Meaning_Key=CChild.Meaning_Key
			AND CChildSyn.Concept_Group_Key=CChild.Concept_Group_Key
	INNER JOIN Concept_Relation AS CR ON CR.To_Concept_Key=CChildSyn.Concept_Key
	INNER JOIN	Concept AS C ON C.Concept_Key = CR.From_Concept_Key
	INNER JOIN	Term AS T ON T.Term_Key = C.Term_Key
	LEFT JOIN	Term_Version AS TV ON TV.Term_Version_Key = C.Term_Version_Key
	WHERE		CChild.Concept_Key = @Key
	AND		CR.Thesaurus_Relation_Type_Key = @Hierarchy_Relation_Type_Key

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Parents_Select_ForConcept') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Parents_Select_ForConcept'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Parents_Select_ForConcept TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Parents_Select_ForConcept TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Parents_Select_ForConcept TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Parents_Select_ForConcept TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Parents_Select_ForConcept TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Parents_Select_ForConcept TO [Dev - JNCC SQL]
END

GO