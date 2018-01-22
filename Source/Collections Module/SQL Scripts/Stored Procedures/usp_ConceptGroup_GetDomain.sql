/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptGroup_GetDomain]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_ConceptGroup_GetDomain]
GO

/*===========================================================================*\
  Description:	Determine the domain associated with the specified concept
				group.

  Parameters:	@concept_group_key		Concept group key
				@domain_key				[on exit] Domain key

  Created:		Jan 2004

  Last revision information:
	$Revision: 2 $
	$Date: 10/02/04 16:38 $
	$Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptGroup_GetDomain]
	@Key			CHAR(16),
	@Domain_Key					CHAR(16)	OUTPUT
AS
	SET NOCOUNT ON

	SELECT @Domain_Key =	l.Domain_Key
	FROM Concept_Group AS	g
	INNER JOIN Local_Domain	AS	l
	ON l.Local_Domain_Key	=	g.Local_Domain_Key
	WHERE	g.Concept_Group_Key	=	@Key

	IF @@ROWCOUNT = 0
	BEGIN
		RAISERROR ('Concept group does not exist', 16, 1)
	END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptGroup_GetDomain') AND SysStat & 0xf = 4)
BEGIN
  PRINT 'Setting up security on procedure usp_ConceptGroup_GetDomain'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptGroup_GetDomain TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptGroup_GetDomain TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptGroup_GetDomain TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ConceptGroup_GetDomain TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptGroup_GetDomain TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptGroup_GetDomain TO [Dev - JNCC SQL]
END
GO