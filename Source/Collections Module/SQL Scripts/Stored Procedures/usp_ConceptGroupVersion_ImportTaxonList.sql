/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptGroupVersion_ImportTaxonList]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_ConceptGroupVersion_ImportTaxonList]
GO

/*===========================================================================*\
  Description:	Import concept group versions corresponding to the versions
				of a taxon list.

  Parameters:   @job_id					Job identifier

  Created:		Nov 2003

  Last revision information:
	$Revision: 9 $
	$Date: 22/06/05 12:10 $
	$Author: Andrewkemp $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptGroupVersion_ImportTaxonList]
	@job_id				INT
AS
	SET NOCOUNT ON

	DECLARE		@concept_group_key					CHAR(16),
				@taxon_list_key						CHAR(16),
				@taxon_list_version_key				CHAR(16),
				@version							INT,
				@vd_start							INT,
				@vd_end								INT,
				@vd_type							VARCHAR(2),
				@source_key							CHAR(16),
				@source_join_key					CHAR(16),
				@ins_user_key						CHAR(16),
				@ins_date							SMALLDATETIME,
				@ins_session_id						CHAR(16),
				@upd_user_key						CHAR(16),
				@upd_date							SMALLDATETIME,
				@upd_session_id						CHAR(16),
				@system								BIT,
				@concept_group_version_key			CHAR(16),
				@prior_concept_group_version_key	CHAR(16)

	/* determine parameters of job */
	SELECT      @concept_group_key						=	m.Concept_Group_Key,
				@taxon_list_key							=	m.Taxon_List_Key
	FROM		Import_Export_Job						AS	j
	INNER JOIN	Taxon_Dictionary_Concept_Group_Mapping	AS	m
	ON			m.Concept_Group_Key						=	j.Concept_Group_Key
	WHERE		j.Import_Export_Job_ID					=	@job_id

	IF @@ROWCOUNT = 0
	BEGIN
		RAISERROR ('Job does not exist or has not been configured', 16, 1)
		GOTO fail
	END

	EXECUTE		usp_Import_Export_Job_UpdateStatus	@job_id,
													'Importing concept group versions'
	IF @@ERROR <> 0 GOTO fail

	DECLARE		versions	CURSOR LOCAL FOR
	SELECT		TAXON_LIST_VERSION_KEY,
				VERSION,
				VAGUE_DATE_START,
				VAGUE_DATE_END,
				VAGUE_DATE_TYPE,
				SOURCE_KEY,
				ENTERED_BY,
				ENTRY_DATE,
				CHANGED_BY,
				CHANGED_DATE,
				SYSTEM_SUPPLIED_DATA
	FROM		TAXON_LIST_VERSION
	WHERE		TAXON_LIST_KEY			=	@taxon_list_key
	ORDER BY	VERSION

	OPEN		versions

	WHILE 1 = 1
	BEGIN
		FETCH		versions
		INTO		@taxon_list_version_key,
					@version,
					@vd_start,
					@vd_end,
					@vd_type,
					@source_key,
					@ins_user_key,
					@ins_date,
					@upd_user_key,
					@upd_date,
					@system

		IF @@FETCH_STATUS <> 0 BREAK

		/* clean up any vague date weirdness */
		SET			@vd_type	=	ISNULL(@vd_type, 'U')

		IF @vd_type = 'U'
		BEGIN
			SELECT		@vd_start	=	NULL,
						@vd_end		=	NULL
		END

		/* obtain session identifiers */
		EXECUTE		usp_Session_ForDate		@ins_user_key,
											@ins_date,
											@ins_session_id		OUTPUT
		IF @@ERROR <> 0 GOTO fail_from_cursor

		IF @upd_user_key IS NULL OR @upd_date IS NULL
		BEGIN
			SET			@upd_session_id		=	NULL
		END
		ELSE
		BEGIN
			EXECUTE		usp_Session_ForDate		@upd_user_key,
												@upd_date,
												@upd_session_id		OUTPUT
			IF @@ERROR <> 0 GOTO fail_from_cursor
		END

		BEGIN TRANSACTION

		SELECT		@concept_group_version_key						=	Concept_Group_Version_Key,
					@source_join_key								=	Source_Join_Key
		FROM		Taxon_Dictionary_Concept_Group_Version_Mapping
		WHERE		Taxon_List_Version_Key							=   @taxon_list_version_key

		IF @@ROWCOUNT > 0
		BEGIN
			/* update concept group version */
			UPDATE		Concept_Group_Version
			SET			Sequence					=	@version,
						From_Vague_Date_Start		=	@vd_start,
						From_Vague_Date_End			=	@vd_end,
						From_Vague_Date_Type		=	@vd_type,
						Entered_Session_ID			=	@ins_session_id,
						Changed_Session_ID			=	@upd_session_id,
						System_Supplied_Data		=	@system
			WHERE		Concept_Group_Version_Key	=	@concept_group_version_key

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END
		ELSE
		BEGIN
			/* create concept group version */
			EXECUTE		spNextKey	'Concept_Group_Version',
									@concept_group_version_key	OUTPUT

			IF @@ERROR <> 0 GOTO fail_from_cursor

			SET			@source_join_key			=	NULL

			INSERT		Concept_Group_Version
						(Concept_Group_Version_Key,
						Concept_Group_Key,
						Version,
						Sequence,
						From_Vague_Date_Start,
						From_Vague_Date_End,
						From_Vague_Date_Type,
						Acq_Vague_Date_Type,
						Entered_Session_ID,
						Changed_Session_ID,
						System_Supplied_Data)
			SELECT		@concept_group_version_key,
						@concept_group_key,
						LTRIM(STR(@version)),
						@version,
						@vd_start,
						@vd_end,
						@vd_type,
						'U',
						@ins_session_id,
						@upd_session_id,
						@system

			IF @@ERROR <> 0 GOTO fail_from_cursor

			/* record mapping */
			INSERT		Taxon_Dictionary_Concept_Group_Version_Mapping (
						Taxon_List_Version_Key,
						Concept_Group_Version_Key)
			VALUES		(@taxon_list_version_key,
						@concept_group_version_key)

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END
		
		/* set end date of prior version */
		IF @prior_concept_group_version_key IS NOT NULL
		BEGIN
			UPDATE		Concept_Group_Version
			SET			To_Vague_Date_Start			=	@vd_start,
						To_Vague_Date_End			=	@vd_end,
						To_Vague_Date_Type			=	@vd_type
			WHERE		Concept_Group_Version_Key	=	@prior_concept_group_version_key

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END

		/* make any changes required in Source_Join */
		IF @source_key IS NULL
		BEGIN
			UPDATE		Taxon_Dictionary_Concept_Group_Version_Mapping
			SET			Source_Join_Key									=	NULL
			WHERE		Concept_Group_Version_Key						=	@concept_group_version_key

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END

		EXECUTE		usp_SourceJoin_RecordImported	@source_join_key		OUTPUT,
													'Concept_Group_Version',
													@concept_group_version_key,
													@source_key,
													@ins_session_id,
													@system
		IF @@ERROR <> 0 GOTO fail_from_cursor

		IF @source_key IS NOT NULL
		BEGIN
			UPDATE		Taxon_Dictionary_Concept_Group_Version_Mapping
			SET			Source_Join_Key									=	@source_join_key
			WHERE		Concept_Group_Version_Key						=	@concept_group_version_key

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END

		/* update progress counter */
		EXECUTE		usp_Import_Export_Job_RecordProcessed	@job_id
		IF @@ERROR <> 0 GOTO fail_from_cursor

		COMMIT TRANSACTION

		SET		@prior_concept_group_version_key = @concept_group_version_key
	END

	CLOSE		versions
	DEALLOCATE	versions
	RETURN

fail_from_cursor:
	CLOSE		versions
	DEALLOCATE	versions

fail:
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_ConceptGroupVersion_ImportTaxonList failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptGroupVersion_ImportTaxonList') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptGroupVersion_ImportTaxonList'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptGroupVersion_ImportTaxonList TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptGroupVersion_ImportTaxonList TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptGroupVersion_ImportTaxonList TO [Dev - JNCC SQL]
END
GO