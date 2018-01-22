/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptGroup_GetLatestTimestamp]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_ConceptGroup_GetLatestTimestamp]
GO

/*===========================================================================*\
  Description:	Latest timestamp assigned to concepts in the specified group.

  Parameters:   @concept_group_key		Concept group key
				@timestamp				[on exit] Latest timestamp

  Created:		Jan 2004

  Last revision information:
	$Revision: 2 $
	$Date: 12/05/04 9:57 $
	$Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptGroup_GetLatestTimestamp]
	@concept_group_key	CHAR(16),
	@timestamp			TIMESTAMP	OUTPUT
AS
	SET NOCOUNT ON

	SELECT		@timestamp			=	MAX(Timestamp)
	FROM		Concept				AS	c
	WHERE		Concept_Group_Key	=	@concept_group_key
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptGroup_GetLatestTimestamp') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptGroup_GetLatestTimestamp'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptGroup_GetLatestTimestamp TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptGroup_GetLatestTimestamp TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptGroup_GetLatestTimestamp TO [Dev - JNCC SQL]
END
GO