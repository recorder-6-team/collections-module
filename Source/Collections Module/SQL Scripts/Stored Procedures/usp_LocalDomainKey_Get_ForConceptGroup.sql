/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_LocalDomainKey_Get_ForConceptGroup]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_LocalDomainKey_Get_ForConceptGroup]
GO

/*===========================================================================*\
  Description:	Outputs the concept group key for a concept.

  Parameters:	@Key	ConceptGroup key
  Return		@LocalDomainKey  

  Created:	January 2008


\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_LocalDomainKey_Get_ForConceptGroup]
	@Key char(16),
	@LocalDomainKey char(16) OUTPUT
AS
	SELECT 		@LocalDomainKey = Local_Domain_Key
	FROM		Concept_Group
	WHERE		Concept_Group_Key = @Key
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_LocalDomainKey_Get_ForConceptGroup') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_LocalDomainKey_Get_ForConceptGroup'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        GRANT EXECUTE ON [dbo].[usp_LocalDomainKey_Get_ForConceptGroup] TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON [dbo].[usp_LocalDomainKey_Get_ForConceptGroup] TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON [dbo].[usp_LocalDomainKey_Get_ForConceptGroup] TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON [dbo].[usp_LocalDomainKey_Get_ForConceptGroup] TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON [dbo].[usp_LocalDomainKey_Get_ForConceptGroup] TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON [dbo].[usp_LocalDomainKey_Get_ForConceptGroup] TO [Dev - JNCC SQL]
END

GO
