/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptCount_Get_ForDomain]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptCount_Get_ForDomain]
GO

/*===========================================================================*\
  Description:	Counts the number of Concepts in a specified domain.

  Parameters:	@Domain - key of the domain

  Created:	December 2003

  Last revision information:
    $Revision: 1 $
    $Date: 1/12/03 10:21 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptCount_Get_ForDomain]
	@Domain char(16),
	@ConceptCount int OUTPUT
AS

	SELECT 		@ConceptCount = Count(C.Concept_Key) 
	FROM 		Concept AS C
	INNER JOIN 	Concept_Group AS CG ON CG.Concept_Group_Key = C.Concept_Group_Key
	INNER JOIN 	Local_Domain AS LD ON LD.Local_Domain_Key = CG.Local_Domain_Key
	WHERE 		LD.Domain_Key = @Domain

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptCount_Get_ForDomain') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptCount_Get_ForDomain'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptCount_Get_ForDomain TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptCount_Get_ForDomain TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptCount_Get_ForDomain TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ConceptCount_Get_ForDomain TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptCount_Get_ForDomain TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptCount_Get_ForDomain TO [Dev - JNCC SQL]
END

GO