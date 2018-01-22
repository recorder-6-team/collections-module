/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptGroup_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptGroup_Select]
GO

/*===========================================================================*\
  Description:	Returns data from the Concept Group table.

  Parameters:	@Key	

  Created:	November 2003

  Last revision information:
    $Revision: 4 $
    $Date: 28/07/11 15:45 $
    $Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptGroup_Select]
	@Key char(16)
AS

SET NOCOUNT ON

	SELECT
			CG.Concept_Group_Key, 		
			CG.Local_Domain_Key,
			CG.Item_Name,
			CG.Authority,
			CG.Url,
			CG.Hierarchy_Relation_Type_Key,
			TRT.Item_Name AS Hierarchy_Relation_Type_Name,
			CG.Term_Generator_Key,
			TG.Item_Name as Term_Generator_Name,
			CG.Entered_Session_ID,
			CG.Changed_Session_ID,
			CG.System_Supplied_Data,
			CG.Custodian,
			CG.[Timestamp],
			CASE WHEN COUNT(CGQC.Checked_Date_Time) > 0 THEN 1
			ELSE 0 END AS HasHistory
	FROM		Concept_Group AS CG
	LEFT JOIN	Thesaurus_Relation_Type AS TRT ON TRT.Thesaurus_Relation_Type_Key = CG.Hierarchy_Relation_Type_Key
	LEFT JOIN	Concept_Group_Quality_Check AS CGQC ON CGQC.Concept_Group_Key = CG.Concept_Group_Key
	LEFT JOIN	Term_Generator TG ON TG.Term_Generator_Key = CG.Term_Generator_Key
	WHERE		CG.Concept_Group_Key = @Key 
	GROUP BY
			CG.Concept_Group_Key, 		
			CG.Local_Domain_Key,
			CG.Item_Name,
			CG.Authority,
			CG.Url,
			CG.Hierarchy_Relation_Type_Key,
			CG.Term_Generator_Key,
			TG.Item_Name,
			TRT.Item_Name,
			CG.Entered_Session_ID,
			CG.Changed_Session_ID,
			CG.System_Supplied_Data,
			CG.Custodian,
			CG.[Timestamp]

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptGroup_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptGroup_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptGroup_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptGroup_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptGroup_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ConceptGroup_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptGroup_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptGroup_Select TO [Dev - JNCC SQL]
END

GO