/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'dbo.usp_ConceptGroup_Select_RecentHistory')
	   AND 	  Type = 'P')
    DROP PROCEDURE dbo.usp_ConceptGroup_Select_RecentHistory
GO

/*===========================================================================*\
  Description:	Returns the most recent quality check record for a concept
				group.

  Parameters:	@Key	

  Created:	May 2011

  Last revision information:
    $Revision: 1 $
    $Date: 25/05/11 9:19 $
    $Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE dbo.usp_ConceptGroup_Select_RecentHistory
	@Key char(16),
	@LastCheckedDetails varchar(150) OUTPUT
AS

SET NOCOUNT ON

	SELECT TOP 1		
			@LastCheckedDetails = 
			CASE
				WHEN CGQC.Concept_Group_Quality_Check_Key IS NULL THEN
					'This concept group has never been checked'
				ELSE
					CONVERT(VARCHAR(25), CGQC.Checked_Date_Time, 120) + ' by ' + 
					dbo.ufn_GetFormattedName(CGQC.Checked_By_User)
			END
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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptGroup_Select_RecentHistory') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptGroup_Select_RecentHistory'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptGroup_Select_RecentHistory TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptGroup_Select_RecentHistory TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptGroup_Select_RecentHistory TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ConceptGroup_Select_RecentHistory TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptGroup_Select_RecentHistory TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptGroup_Select_RecentHistory TO [Dev - JNCC SQL]
END

GO