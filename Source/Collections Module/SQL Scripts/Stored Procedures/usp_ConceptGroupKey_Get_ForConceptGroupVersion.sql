/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptGroupKey_Get_ForConceptGroupVersion]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptGroupKey_Get_ForConceptGroupVersion]
GO

/*===========================================================================*\
  Description:	Outputs the concept group key for a concept.

  Parameters:	@Key	ConceptGroupVersion key

  Created:	March 2004

  Last revision information:
    $Revision: 1 $
    $Date: 26/03/04 14:54 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptGroupKey_Get_ForConceptGroupVersion]
	@Key char(16),
	@ConceptGroupKey char(16) OUTPUT
AS
	SELECT 		@ConceptGroupKey = Concept_Group_Key
	FROM		Concept_Group_Version
	WHERE		Concept_Group_Version_Key = @Key
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptGroupKey_Get_ForConceptGroupVersion') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptGroupKey_Get_ForConceptGroupVersion'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptGroupKey_Get_ForConceptGroupVersion TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptGroupKey_Get_ForConceptGroupVersion TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptGroupKey_Get_ForConceptGroupVersion TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ConceptGroupKey_Get_ForConceptGroupVersion TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptGroupKey_Get_ForConceptGroupVersion TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptGroupKey_Get_ForConceptGroupVersion TO [Dev - JNCC SQL]
END

GO