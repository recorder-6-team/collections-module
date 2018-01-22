/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptHasOccurrences_Get]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptHasOccurrences_Get]
GO

/*===========================================================================*\
  Description:	Returns whether or not a given Concept Key has occurrences

  Parameters:	@ConceptKey				-Concept_Key
				@HasOccurrences			-OUTPUT

  Created:	Jan 2004

  Last revision information:
    $Revision: 1 $
    $Date: 29/01/04 16:39 $
    $Author: Bencollier $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptHasOccurrences_Get]
	@ConceptKey CHAR(16),
	@HasOccurrences BIT OUTPUT
AS

SET NOCOUNT ON

IF EXISTS(
		SELECT * 
		FROM 
			Concept C
			INNER JOIN
				Concept_Group CG
			ON C.Concept_Group_Key = CG.Concept_Group_Key
				AND C.Concept_Key = @ConceptKey
			INNER JOIN
				Local_Domain LD
			ON CG.Local_Domain_Key = LD.Local_Domain_Key
			INNER JOIN
				Domain D
			ON LD.Domain_Key = D.Domain_Key
				AND D.Has_Occurrences = 1)
	SET @HasOccurrences = 1
ELSE
	SET @HasOccurrences = 0

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptHasOccurrences_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptHasOccurrences_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptHasOccurrences_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptHasOccurrences_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptHasOccurrences_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ConceptHasOccurrences_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptHasOccurrences_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptHasOccurrences_Get TO [Dev - JNCC SQL]
END

GO