/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptHistory_Insert_Imported]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_ConceptHistory_Insert_Imported]
GO

/*===========================================================================*\
  Description:	Add a concept history record corresponding to a concept
				imported from a taxon dictionary.

  Parameters:	@ConceptKey				Concept identifier
				@GroupVersionFrom		Initial concept group identifier
				@GroupVersionTo			Final concept group identifier (if any)

  Created:		Nov 2003

  Last revision information:
	$Revision: 5 $
	$Date: 12/05/04 9:57 $
	$Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptHistory_Insert_Imported]
	@ConceptKey			CHAR(16),
	@GroupVersionFrom	CHAR(16),
	@GroupVersionTo		CHAR(16)
AS
	SET NOCOUNT ON

	DECLARE		@concept_history_key	CHAR(16),
				@from_start				INT,
				@from_end				INT,
				@from_type				VARCHAR(2),
				@to_start				INT,
				@to_end					INT,
				@to_type				VARCHAR(2)

	SELECT		@from_start					=	From_Vague_Date_Start,
				@from_end					=	From_Vague_Date_End,
				@from_type					=	From_Vague_Date_Type
	FROM		Concept_Group_Version
	WHERE		Concept_Group_Version_Key	=   @GroupVersionFrom

	IF @GroupVersionTo IS NOT NULL
	BEGIN
		SELECT		@to_start					=	To_Vague_Date_Start,
					@to_end						=	To_Vague_Date_End,
					@to_type					=	To_Vague_Date_Type
		FROM		Concept_Group_Version
		WHERE		Concept_Group_Version_Key	=	@GroupVersionTo
	END

	/* write history record */
	EXECUTE		spNextKey	'Concept_History',
							@concept_history_key	OUTPUT
	IF @@ERROR <> 0 GOTO fail

	INSERT		Concept_History (
				Concept_History_Key,
				Concept_Key,
				Concept_Group_Version_From,
				Concept_Group_Version_To,
				From_Vague_Date_Start,
				From_Vague_Date_End,
				From_Vague_Date_Type,
				To_Vague_Date_Start,
				To_Vague_Date_End,
				To_Vague_Date_Type,
				Entered_Session_ID,
				Changed_Session_ID,
				System_Supplied_Data)
	SELECT      @concept_history_key,
				@ConceptKey,
				@GroupVersionFrom,
				@GroupVersionTo,
				@from_start,
				@from_end,
				@from_type,
				@to_start,
				@to_end,
				@to_type,
				c.Entered_Session_ID,
				c.Changed_Session_ID,
				c.System_Supplied_Data
	FROM		Concept							AS	c
	WHERE		c.Concept_Key					=	@ConceptKey				

	IF @@ERROR <> 0 GOTO fail
	RETURN

fail:
	RAISERROR ('usp_ConceptHistory_Insert_Imported failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptHistory_Insert_Imported') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptHistory_Insert_Imported'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptHistory_Insert_Imported TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptHistory_Insert_Imported TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptHistory_Insert_Imported TO [Dev - JNCC SQL]
END
GO