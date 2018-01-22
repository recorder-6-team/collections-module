/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_RecordTypeConceptGroup_Select_ForDomain]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_RecordTypeConceptGroup_Select_ForDomain]
GO

/*===========================================================================*\
  Description:	Returns the contents of the concept group called Record Types
			in the domain supplied

  Parameters:	@Key	Domain key

  Created:	Mar 2004

  Last revision information:
    $Revision: 1 $
    $Date: 1/03/04 10:58 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_RecordTypeConceptGroup_Select_ForDomain]
	@Key char(16)
AS

SET NOCOUNT ON

SELECT CT.Concept_Key, CT.Plaintext
FROM Concept_Group CG
INNER JOIN Local_Domain LD ON LD.Local_Domain_Key=CG.Local_Domain_Key
INNER JOIN VW_ConceptTermPreferred CT ON CT.Concept_Group_Key=CG.Concept_Group_Key
WHERE CG.Item_Name = 'Record Types'
AND LD.Domain_Key=@Key

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_RecordTypeConceptGroup_Select_ForDomain') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_RecordTypeConceptGroup_Select_ForDomain'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_RecordTypeConceptGroup_Select_ForDomain TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_RecordTypeConceptGroup_Select_ForDomain TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_RecordTypeConceptGroup_Select_ForDomain TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_RecordTypeConceptGroup_Select_ForDomain TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_RecordTypeConceptGroup_Select_ForDomain TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_RecordTypeConceptGroup_Select_ForDomain TO [Dev - JNCC SQL]
END

GO