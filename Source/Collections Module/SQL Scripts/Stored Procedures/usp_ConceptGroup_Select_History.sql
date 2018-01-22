/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'dbo.usp_ConceptGroup_Select_History')
	   AND 	  Type = 'P')
    DROP PROCEDURE dbo.usp_ConceptGroup_Select_History
GO

/*===========================================================================*\
  Description:	Returns quality check history for a concept group.

  Parameters:	@Key	

  Created:	May 2011

  Last revision information:
    $Revision: 1 $
    $Date: 25/05/11 9:19 $
    $Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE dbo.usp_ConceptGroup_Select_History
	@Key char(16)
AS

SET NOCOUNT ON

	SELECT
		CGQC.Concept_Group_Quality_Check_Key as Item_Key, 	
		CGQC.Checked_Date_Time,
		dbo.ufn_GetFormattedName(CGQC.Checked_By_User) as Checked_By,
		CG.Custodian,
		CG.Timestamp
	FROM		Concept_Group AS CG
	LEFT JOIN	Concept_Group_Quality_Check CGQC
		ON		CGQC.Concept_Group_Key = CG.Concept_Group_Key
	WHERE		CG.Concept_Group_Key = @Key
	ORDER BY CGQC.Checked_Date_Time DESC

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptGroup_Select_History') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptGroup_Select_History'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptGroup_Select_History TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptGroup_Select_History TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptGroup_Select_History TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ConceptGroup_Select_History TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptGroup_Select_History TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptGroup_Select_History TO [Dev - JNCC SQL]
END

GO