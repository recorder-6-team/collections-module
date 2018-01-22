If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Concept_Select_ForGroupAndItemName]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Concept_Select_ForGroupAndItemName]
GO
    
/*===========================================================================*\
  Description:	Returns a concept key matching the plain text

  Parameters:	@Concept_Group_Key  Key of the Concept group whose members we're retrieving
		@Key#               Keys to match

  Created:	August 2003

  Last revision information:
    $Revision: 4 $
    $Date: 3/08/11 12:27 $
    $Author: Simonlewis $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_Concept_Select_ForGroupAndItemName]
	@Concept_Group_Key as Char(16),
	@PlainTExt as nVarchar(100)
	
 AS

SET NOCOUNT ON
SELECT C.Concept_Key FROM Concept C 
                       INNER JOIN Term T ON T.Term_Key = C.Term_Key 
                       WHERE (C.Concept_Group_Key = @Concept_Group_Key)
                       AND (C.List_Preferred = 1)
                       AND (C.Is_Current = 1)
		       and PlainText = @PlainText
                       ORDER BY C.Sort_Code, T.Plaintext
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_Select_ForGroupAndItemName') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Concept_Select_ForGroupAndItemName'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Concept_Select_ForGroupAndItemName TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForGroupAndItemName TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForGroupAndItemName TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForGroupAndItemName TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForGroupAndItemName TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Concept_Select_ForGroupAndItemName TO [Dev - JNCC SQL]
END
GO