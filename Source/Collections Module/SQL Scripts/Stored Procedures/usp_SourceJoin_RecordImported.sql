/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_SourceJoin_RecordImported]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_SourceJoin_RecordImported]
GO

/*===========================================================================*\
  Description:	Make any changes required to Source_Join following import of
				a record from the taxon dictionary.

  Parameters:   @source_join_key		Source join key, or NULL if there is
										none.  Current value passed as input
										is updated on exit.
				@table_name				Table into which record was imported
				@record_key				Key of imported record
				@source_key				Source key (NULL if record should not
										be associated with a source)
				@entered_session_id		Session in which join was created (if
										applicable)
				@system_supplied_data	Is the join system supplied data?

  Created:		Jan 2004

  Last revision information:
	$Revision: 6 $
	$Date: 12/05/04 9:57 $
	$Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SourceJoin_RecordImported]
	@source_join_key		CHAR(16)		OUTPUT,
	@table_name				VARCHAR(50),
	@record_key				CHAR(16),
	@source_key				CHAR(16),
	@entered_session_id		CHAR(16),
	@system_supplied_data	BIT
AS
	SET NOCOUNT ON

	/* check whether source exists (current database contains random junk
	 * in TAXON_LIST_VERSION.SOURCE_KEY)
	 */
	IF @source_key IS NOT NULL
	BEGIN
		IF NOT EXISTS (	SELECT		1
						FROM		SOURCE
						WHERE		Source_Key			=	@source_key)
		BEGIN
			SET			@source_key			=	NULL
		END
	END

	IF @source_key IS NULL
	BEGIN
		/* delete current source join, if any */
		IF @source_join_key IS NOT NULL
		BEGIN
			DELETE		Source_Join
			WHERE		Source_Join_Key		=	@source_join_key

			IF @@ERROR <> 0 GOTO fail

			SET			@source_join_key	=	NULL
		END
	END
	ELSE IF @source_join_key IS NOT NULL
	BEGIN
		/* update current source join */
		UPDATE		Source_Join
		SET         Source_Key				=	@source_key,
					Entered_Session_ID		=	@entered_session_id,
					System_Supplied_Data	=	@system_supplied_data
		WHERE		Source_Join_Key			=	@source_join_key

		IF @@ERROR <> 0 GOTO fail
	END
	ELSE
	BEGIN
		SELECT		@source_join_key    =	Source_Join_Key
		FROM		Source_Join
		WHERE		Table_Name			=	@table_name
		AND			Record_Key			=	@record_key
		AND			Source_Key			=	@source_key

		IF @@ROWCOUNT = 0
		BEGIN
			/* create new source join */
			EXECUTE		spNextKey	'Source_Join',
									@source_join_key	OUTPUT
			IF @@ERROR <> 0 GOTO fail

			INSERT		Source_Join (
						Source_Join_Key,
						Table_Name,
						Record_Key,
						Source_Key,
						Entered_Session_ID,
						System_Supplied_Data)
			VALUES		(@source_join_key,
						@table_name,
						@record_key,
						@source_key,
						@entered_session_id,
						@system_supplied_data)

			IF @@ERROR <> 0 GOTO fail
		END
	END
	RETURN

fail:
	RAISERROR ('usp_SourceJoin_RecordImported failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SourceJoin_RecordImported') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SourceJoin_RecordImported'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SourceJoin_RecordImported TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SourceJoin_RecordImported TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SourceJoin_RecordImported TO [Dev - JNCC SQL]
END
GO