/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptGroupVersion_GetDomain]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_ConceptGroupVersion_GetDomain]
GO

/*===========================================================================*\
  Description:	Determine the domain associated with the specified concept
				group version.

  Parameters:	@Key		Concept group version key
							@domain_key				[on exit] Domain key

  Created:		Jan 2004

  Last revision information:
	$Revision: 1 $
	$Date: 10/02/04 16:05 $
	$Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptGroupVersion_GetDomain]
	@Key			CHAR(16),
	@Domain_Key					CHAR(16)	OUTPUT
AS
	SET NOCOUNT ON

	SELECT		@Domain_Key	=	L.Domain_Key
	FROM		Concept_Group CG
	INNER JOIN Concept_Group_Version CGV ON CGV.Concept_Group_Key=CG.Concept_Group_Key
	INNER JOIN	Local_Domain AS	L ON L.Local_Domain_Key	=	CG.Local_Domain_Key
	WHERE	CGV.Concept_Group_Version_Key	= @Key

	IF @@ROWCOUNT = 0
	BEGIN
		RAISERROR ('Concept group does not exist', 16, 1)
	END
GO


/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptGroupVersion_GetDomain') AND SysStat & 0xf = 4)
BEGIN
  PRINT 'Setting up security on procedure usp_ConceptGroupVersion_GetDomain'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptGroupVersion_GetDomain TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptGroupVersion_GetDomain TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptGroupVersion_GetDomain TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ConceptGroupVersion_GetDomain TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptGroupVersion_GetDomain TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptGroupVersion_GetDomain TO [Dev - JNCC SQL]
END
GO