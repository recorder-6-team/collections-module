/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_FindFirstConceptGroupForSubject') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_FindFirstConceptGroupForSubject]
GO

/*===========================================================================*\
  Description:	Returns the key of the first concept group for a subject area.

  Parameters:	@SubjectAreaKey	subject area key
		@ConceptGroupKey	OUTPUT

  Created:	July 2008

  Last revision information:
    $Revision: 1 $
    $Date: 10/07/08 15:00 $
    $Author: Johndurman $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_FindFirstConceptGroupForSubject]
	@SubjectAreaKey char(16),
	@ConceptGroupKey char(16) OUTPUT
AS
	SET @ConceptGroupKey = (SELECT TOP 1 CG.Concept_Group_Key from Subject_Area SA
	INNER JOIN Domain D ON D.Subject_Area_Key = SA.Subject_Area_Key
	INNER JOIN Local_Domain LD ON LD.Domain_Key = D.Domain_Key
	INNER JOIN Concept_Group CG ON CG.Local_Domain_Key = LD.Local_Domain_Key
	WHERE SA.Subject_Area_Key = @SubjectAreaKey 
	ORDER BY SA.Item_Name, D.Item_Name, CG.Item_Name)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_FindFirstConceptGroupForSubject') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_FindFirstConceptGroupForSubject'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_FindFirstConceptGroupForSubject TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_FindFirstConceptGroupForSubject TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_FindFirstConceptGroupForSubject TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_FindFirstConceptGroupForSubject TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_FindFirstConceptGroupForSubject TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_FindFirstConceptGroupForSubject TO [Dev - JNCC SQL]
END
GO
