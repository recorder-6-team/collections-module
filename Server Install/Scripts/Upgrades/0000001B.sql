SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptDesignation_ImportTaxonList]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_ConceptDesignation_ImportTaxonList]
GO

/*===========================================================================*\
  Description:	Import concept designations corresponding to the taxon
				designations associated with the specified taxon list.

  Parameters:	@job_id					Job identifier

  Created:		Dec 2003

  Last revision information:
	$Revision: 2 $
	$Date: 2/02/09 16:46 $
	$Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptDesignation_ImportTaxonList]
	@job_id					INT
AS
	SET NOCOUNT ON

	DECLARE     @taxon_list_key					CHAR(16),
				@taxon_designation_key			CHAR(16),
				@concept_key					CHAR(16),
				@designation_type_concept_key	CHAR(16),
				@from_vague_date_start			INT,
				@to_vague_date_start			INT,
				@source_key						CHAR(16),
				@source_join_key				CHAR(16),
				@ins_user_key					CHAR(16),
				@ins_date						DATETIME,
				@ins_session_id					CHAR(16),
				@upd_user_key					CHAR(16),
				@upd_date						DATETIME,
				@upd_session_id					CHAR(16),
				@system							BIT,
				@concept_designation_key		CHAR(16)

	/* determine parameters of job */
	SELECT		@taxon_list_key							=	m.Taxon_List_Key
	FROM		Import_Export_Job						AS	j
	INNER JOIN	Taxon_Dictionary_Concept_Group_Mapping	AS	m
	ON			m.Concept_Group_Key						=	j.Concept_Group_Key
	WHERE		j.Import_Export_Job_ID					=	@job_id

	IF @@ROWCOUNT = 0
	BEGIN
		RAISERROR ('Job does not exist or has not been configured', 16, 1)
		RETURN
	END

	EXECUTE		usp_Import_Export_Job_UpdateStatus	@job_id,
													'Importing concept designations'
	IF @@ERROR <> 0 RETURN

	DECLARE		designations	CURSOR LOCAL FAST_FORWARD FOR
	SELECT		td.TAXON_DESIGNATION_KEY,
				tcm.Concept_Key,
				tdm.Concept_Designation_Type_Key,
				DATEDIFF(d, '18991230', td.DATE_FROM),
				DATEDIFF(d, '18991230', td.DATE_TO),
				td.SOURCE_KEY,
				td.ENTERED_BY,
				td.ENTRY_DATE,
				td.CHANGED_BY,
				td.CHANGED_DATE,
				td.SYSTEM_SUPPLIED_DATA
	FROM		TAXON_LIST_VERSION							AS	tlv
	INNER JOIN	TAXON_LIST_ITEM								AS	tli
	ON			tli.TAXON_LIST_VERSION_KEY					=	tlv.TAXON_LIST_VERSION_KEY
	INNER JOIN	Taxon_Dictionary_Concept_Mapping			AS	tcm
	ON			tcm.Taxon_List_Item_Key						=	tli.TAXON_LIST_ITEM_KEY
	INNER JOIN	TAXON_DESIGNATION							AS	td
	ON			td.TAXON_LIST_ITEM_KEY						=	tcm.Taxon_List_Item_Key
	INNER JOIN	Taxon_Dictionary_Designation_Type_Mapping	AS	tdm
	ON			tdm.Taxon_Designation_Type_Key				=	td.TAXON_DESIGNATION_TYPE_KEY
	WHERE		tlv.TAXON_LIST_KEY							=	@taxon_list_key

	OPEN		designations

	WHILE 1 = 1
	BEGIN
		FETCH		designations
		INTO		@taxon_designation_key,
					@concept_key,
					@designation_type_concept_key,
					@from_vague_date_start,
					@to_vague_date_start,
					@source_key,
					@ins_user_key,
					@ins_date,
					@upd_user_key,
					@upd_date,
					@system

		IF @@FETCH_STATUS <> 0 BREAK

		BEGIN TRANSACTION

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

		SELECT		@concept_designation_key						=	Concept_Designation_Key,
					@source_join_key								=	Source_Join_Key
		FROM		Taxon_Dictionary_Concept_Designation_Mapping
		WHERE		Taxon_Designation_Key							=	@taxon_designation_key

		IF @@ROWCOUNT > 0
		BEGIN
			/* update concept designation */
			UPDATE		Concept_Designation
			SET			Concept_Key						=	@concept_key,
						Designation_Type_Concept_Key	=	@designation_type_concept_key,
						From_Vague_Date_Start			=	@from_vague_date_start,
						From_Vague_Date_End				=	@from_vague_date_start,
						From_Vague_Date_Type			=	CASE WHEN @from_vague_date_start IS NULL
																THEN 'U'
																ELSE 'D'
															END,
						To_Vague_Date_Start				=	@to_vague_date_start,
						To_Vague_Date_End				=	@to_vague_date_start,
						To_Vague_Date_Type				=	CASE WHEN @to_vague_date_start IS NULL
																THEN NULL
																ELSE 'D'
															END,
						Entered_Session_ID				=	@ins_session_id,
						Changed_Session_ID				=	@upd_session_id,
						System_Supplied_Data			=	@system
			WHERE		Concept_Designation_Key			=	@concept_designation_key

			IF @@ERROR <> 0 GOTO fail_from_cursor												
		END
		ELSE
		BEGIN
			/* create concept designation */
			EXECUTE		spNextKey	'Concept_Designation',
									@concept_designation_key	OUTPUT
			IF @@ERROR <> 0 GOTO fail_from_cursor

			SET			@source_join_key				=	NULL

			INSERT		Concept_Designation (
						Concept_Designation_Key,
						Concept_Key,
						Designation_Type_Concept_Key,
						From_Vague_Date_Start,
						From_Vague_Date_End,
						From_Vague_Date_Type,
						To_Vague_Date_Start,
						To_Vague_Date_End,
						To_Vague_Date_Type,
						Entered_Session_ID,
						Changed_Session_ID,
						System_Supplied_Data)
			SELECT		@concept_designation_key,
						@concept_key,
						@designation_type_concept_key,
						@from_vague_date_start,
						@from_vague_date_start,
						CASE WHEN @from_vague_date_start IS NULL
							THEN 'U'
							ELSE 'D'
						END,
						@to_vague_date_start,
						@to_vague_date_start,
						CASE WHEN @to_vague_date_start IS NULL
							THEN NULL
							ELSE 'D'
						END,
						@ins_session_id,
						@upd_session_id,
						@system

			IF @@ERROR <> 0 GOTO fail_from_cursor

			/* record mapping */
			INSERT		Taxon_Dictionary_Concept_Designation_Mapping (
						Taxon_Designation_Key,
						Concept_Designation_Key)
			VALUES		(@taxon_designation_key,
						@concept_designation_key)

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END

		/* make any changes required in Source_Join */
		IF @source_key IS NULL
		BEGIN
			UPDATE		Taxon_Dictionary_Concept_Designation_Mapping
			SET			Source_Join_Key									=	NULL
			WHERE		Taxon_Designation_Key							=	@taxon_designation_key

			IF @@ERROR <> 0 GOTO fail_from_cursor			
		END

		EXECUTE		usp_SourceJoin_RecordImported	@source_join_key	OUTPUT,
													'Concept_Designation',
													@concept_designation_key,
													@source_key,
													@ins_session_id,
													@system
		IF @@ERROR <> 0 GOTO fail_from_cursor

		IF @source_key IS NOT NULL
		BEGIN
			UPDATE		Taxon_Dictionary_Concept_Designation_Mapping
			SET			Source_Join_Key									=	@source_join_key
			WHERE		Taxon_Designation_Key							=	@taxon_designation_key

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END

		/* update progress counter */
		EXECUTE		usp_Import_Export_Job_RecordProcessed	@job_id
		IF @@ERROR <> 0 GOTO fail_from_cursor

		COMMIT TRANSACTION
	END

	CLOSE		designations
	DEALLOCATE	designations
	RETURN

fail_from_cursor:
	CLOSE		designations
	DEALLOCATE	designations

fail:
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_ConceptDesignation_ImportTaxonList failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptDesignation_ImportTaxonList') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptDesignation_ImportTaxonList'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptDesignation_ImportTaxonList TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptDesignation_ImportTaxonList TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptDesignation_ImportTaxonList TO [Dev - JNCC SQL]
END
GO

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
	$Revision: 2 $
	$Date: 2/02/09 16:46 $
	$Author: Pauldavies $

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

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptRank_ImportTaxonList]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_ConceptRank_ImportTaxonList]
GO

/*===========================================================================*\
  Description:	Import concept ranks corresponding to taxon ranks from the
				specified taxon list.

  Parameters:   @job_id					Job identifier

  Created:		Nov 2003

  Last revision information:
	$Revision: 2 $
	$Date: 2/02/09 16:46 $
	$Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptRank_ImportTaxonList]
	@job_id				INT
AS
	SET NOCOUNT ON

	DECLARE     @taxon_list_key		CHAR(16),
				@domain_key			CHAR(16),
				@taxon_rank_key		CHAR(16),
				@item_name			VARCHAR(100),
				@sort_order			INT,
				@abbreviation		VARCHAR(10),
				@ins_user_key		CHAR(16),
				@ins_date			SMALLDATETIME,
				@ins_session_id		CHAR(16),
				@upd_user_key		CHAR(16),
				@upd_date			SMALLDATETIME,
				@upd_session_id		CHAR(16),
				@system				BIT,
				@concept_rank_key	CHAR(16)

	/* determine parameters of job */
	SELECT		@taxon_list_key							=	m.Taxon_List_Key,
				@domain_key								=	ld.Domain_Key
	FROM		Import_Export_Job						AS	j
	INNER JOIN	Taxon_Dictionary_Concept_Group_Mapping	AS	m
	ON			m.Concept_Group_Key						=	j.Concept_Group_Key
	INNER JOIN	Concept_Group							AS	g
	ON			g.Concept_Group_Key						=	m.Concept_Group_Key
	INNER JOIN	Local_Domain							AS	ld
	ON			ld.Local_Domain_Key						=	g.Local_Domain_Key
	WHERE		j.Import_Export_Job_ID					=	@job_id

	IF @@ROWCOUNT = 0
	BEGIN
		RAISERROR ('Job does not exist or has not been configured', 16, 1)
		RETURN
	END

	EXECUTE		usp_Import_Export_Job_UpdateStatus	@job_id,
													'Importing concept ranks'
	IF @@ERROR <> 0 RETURN

	DECLARE		ranks	CURSOR LOCAL FAST_FORWARD FOR
	SELECT DISTINCT
				tr.TAXON_RANK_KEY,
				ISNULL(tr.LONG_NAME, tr.SHORT_NAME),
				tr.SEQUENCE,
				tr.SHORT_NAME,
				tr.ENTERED_BY,
				tr.ENTRY_DATE,
				tr.CHANGED_BY,
				tr.CHANGED_DATE,
				tr.SYSTEM_SUPPLIED_DATA
	FROM		TAXON_LIST_VERSION			AS	tlv
	INNER JOIN	TAXON_LIST_ITEM				AS	tli
	ON			tli.TAXON_LIST_VERSION_KEY	=	tlv.TAXON_LIST_VERSION_KEY
	INNER JOIN	TAXON_RANK					AS	tr
	ON			tr.TAXON_RANK_KEY			=	tli.TAXON_RANK_KEY
	WHERE		tlv.TAXON_LIST_KEY			=	@taxon_list_key

	OPEN		ranks

	WHILE 1 = 1
	BEGIN
		FETCH		ranks
		INTO        @taxon_rank_key,
					@item_name,
					@sort_order,
					@abbreviation,		/* TODO: may clip! */
					@ins_user_key,
					@ins_date,
					@upd_user_key,
					@upd_date,
					@system

		IF @@FETCH_STATUS <> 0 BREAK

		BEGIN TRANSACTION

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

		SELECT		@concept_rank_key						=	Concept_Rank_Key
		FROM		Taxon_Dictionary_Concept_Rank_Mapping
		WHERE		Taxon_Rank_Key							=	@taxon_rank_key

		IF @@ROWCOUNT > 0
		BEGIN
			/* update concept rank */
			UPDATE		Concept_Rank
			SET			Domain_Key				=	@domain_key,
						Item_Name				=	@item_name,
						Sort_Order				=	@sort_order,
						Abbreviation			=	@abbreviation,
						Entered_Session_ID		=	@ins_session_id,
						Changed_Session_ID		=	@upd_session_id,
						System_Supplied_Data	=	@system
			WHERE		Concept_Rank_Key		=	@concept_rank_key

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END
		ELSE
		BEGIN
			/* create concept rank */
			EXECUTE		spNextKey	'Concept_Rank',
									@concept_rank_key		OUTPUT
			IF @@ERROR <> 0 GOTO fail_from_cursor

			INSERT		Concept_Rank (
						Concept_Rank_Key,
						Domain_Key,
						Item_Name,
						Sort_Order,
						Abbreviation,
						Color_R,
						Color_G,
						Color_B,
						Entered_Session_ID,
						Changed_Session_ID,
						System_Supplied_Data)
			VALUES		(@concept_rank_key,
						@domain_key,
						@item_name,
						@sort_order,
						@abbreviation,
						0,
						0,
						0,
						@ins_session_id,
						@upd_session_id,
						@system)

			IF @@ERROR <> 0 GOTO fail_from_cursor

			/* record mapping */
			INSERT		Taxon_Dictionary_Concept_Rank_Mapping (
						Taxon_Rank_Key,
						Concept_Rank_Key)
			VALUES		(@taxon_rank_key,
						@concept_rank_key)

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END

		EXECUTE		usp_Import_Export_Job_RecordProcessed	@job_id
		IF @@ERROR <> 0 GOTO fail_from_cursor

		COMMIT TRANSACTION
	END

	CLOSE		ranks
	DEALLOCATE	ranks
	RETURN

fail_from_cursor:
	CLOSE		ranks
	DEALLOCATE	ranks

fail:
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_ConceptRank_ImportTaxonList failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptRank_ImportTaxonList') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptRank_ImportTaxonList'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptRank_ImportTaxonList TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptRank_ImportTaxonList TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptRank_ImportTaxonList TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Concept_Delete]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Concept_Delete]
GO

/*===========================================================================*\
  Description:	Delete a record from the Concept tables. Also deletes records
		from other tables where necessary.

  Parameters:	@Key		Concept key.
				@Timestamp

  Created:	December 2003

  Last revision information:
    $Revision: 2 $
    $Date: 2/02/09 16:46 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Concept_Delete]
	@Key char(16),
	@Timestamp timestamp = NULL,
	@SyncTaxonDict bit = 0
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	DECLARE @MeaningKey char(16),
			@TermKey char(16),
			@TermVersionKey char(16),
			@ConceptsSharingMeaningKeyCount int,
			@ConceptsSharingTermKeyCount int,
			@ConceptsSharingTermVersionKeyCount int,
			@OriginalTimestamp timestamp

	-- Store the Meaning, Term and Term Version keys because the concept record
	-- needs to be deleted before these other records can be, due to referential
	-- integrity.
	SELECT	@MeaningKey = Meaning_Key,
			@TermKey = Term_Key,
			@TermVersionKey = Term_Version_Key,
			@OriginalTimestamp = [Timestamp]
	FROM 	Concept
	WHERE	Concept_Key = @Key

	-- Count the number of concepts that use this meaning key.
	SELECT 		@ConceptsSharingMeaningKeyCount = Count(C2.Concept_Key)
	FROM		Concept AS C1
	INNER JOIN	Concept AS C2 ON C2.Meaning_Key = C1.Meaning_Key
	WHERE		C1.Concept_Key = @Key

	-- Count the number of concepts that use the same term key as the concept we want to delete.
	SELECT 		@ConceptsSharingTermKeyCount = Count(DISTINCT C2.Concept_Key)
	FROM		Concept AS C1
	INNER JOIN	Concept AS C2 ON C2.Term_Key = C1.Term_Key
	WHERE		C1.Concept_Key = @Key

	-- Count the number of concepts that use the same term version key as the concept we want to delete.
	SELECT 		@ConceptsSharingTermVersionKeyCount = Count(DISTINCT C2.Concept_Key)
	FROM		Concept AS C1
	INNER JOIN	Concept AS C2 ON C2.Term_Version_Key = C1.Term_Version_Key
	WHERE		C1.Concept_Key = @Key


	BEGIN TRANSACTION
		/*============================================================*\
		  See if the user wants any associated taxon dictionary
		  records be deleted with the concept.
		\*============================================================*/
		IF @SyncTaxonDict = 1 
		BEGIN
			/*--------------------------------------------------------*\
			  Check that the Taxon_List_Item table exists before
			  attempting any of this deletion. In the future, the 
			  Thesaurus module could be installed without the Taxon
			  tables, so would go wrong if we tried to delete from
			  non-existant tables.			
			\*--------------------------------------------------------*/
			IF EXISTS (SELECT *
		   			FROM   SysObjects 
					WHERE  Id = Object_Id(N'[dbo].[Taxon_List_Item]')
					AND 	  Type = 'U')
			BEGIN
				-- Get the Taxon List Item Key for the current Concept
				DECLARE @TaxonListItemKey char(16)
	
				SELECT 	@TaxonListItemKey = Taxon_List_Item_Key
				FROM	Taxon_Dictionary_Concept_Mapping
				WHERE	Concept_Key = @Key

				/*--------------------------------------------------------*\
				  Delete the records related to the Taxon_List_Item table
				\*--------------------------------------------------------*/
				DELETE 	Taxon_Dictionary_Concept_Mapping
				WHERE	Taxon_List_Item_Key = @TaxonListItemKey
				AND	Concept_Key = @Key

				IF @@Error <> 0 GOTO RollbackAndExit

				DELETE	Taxon_Common_Name
				WHERE	Taxon_List_Item_Key = @TaxonListItemKey

				IF @@Error <> 0 GOTO RollbackAndExit

				DELETE	Index_Taxon_Synonym
				WHERE	Taxon_List_Item_Key = @TaxonListItemKey
				OR	Synonym_List_Item_Key = @TaxonListItemKey

				IF @@Error <> 0 GOTO RollbackAndExit

				DELETE	Export_Filter_Taxon
				WHERE	Taxon_List_Item_Key = @TaxonListItemKey

				IF @@Error <> 0 GOTO RollbackAndExit

				DELETE	Index_Taxon_Group
				WHERE	Taxon_List_Item_Key = @TaxonListItemKey
				OR	Contained_List_Item_Key = @TaxonListItemKey

				IF @@Error <> 0 GOTO RollbackAndExit

				DELETE	Taxon_Designation
				WHERE	Taxon_List_Item_Key = @TaxonListItemKey

				IF @@Error <> 0 GOTO RollbackAndExit

				DELETE	Taxon_User_Name
				WHERE	Taxon_List_Item_Key = @TaxonListItemKey

				IF @@Error <> 0 GOTO RollbackAndExit

				DELETE	Index_Taxon_Name
				WHERE	Taxon_List_Item_Key = @TaxonListItemKey

				IF @@Error <> 0 GOTO RollbackAndExit

				DELETE	Nameserver
				WHERE	Recommended_Taxon_List_Item_Key = @TaxonListItemKey

				IF @@Error <> 0 GOTO RollbackAndExit

				-- If one Concept shares the Term, attempt to delete the equivalent Taxon.
				IF @ConceptsSharingTermKeyCount = 1
				BEGIN
					DECLARE @TaxonKey char(16)

					-- Get the key of the equivalent Taxon
					SELECT 	@TaxonKey = Taxon_Key
					FROM	Taxon_Dictionary_Term_Mapping
					WHERE	Term_Key = @TermKey

							-- Only delete if there are no Taxon_Version records using the Taxon
					IF NOT EXISTS(SELECT 	*
									FROM 	Taxon_Version
									WHERE	Taxon_Key = @TaxonKey)
					BEGIN
						DELETE SF
						FROM Source_File SF
						INNER JOIN Taxon_Sources TS ON TS.Source_Key=SF.Source_Key
						WHERE TS.Taxon_Key=@TaxonKey
		
						DELETE Taxon_Sources
						WHERE Taxon_Key=@TaxonKey
					
						DELETE	Taxon
						WHERE	Taxon_Key = @TaxonKey
					END
				END

				/*-----------------------------------------------------------------*\
				  It is possible that this delete will fail. e.g. If the TLI record
				  is referred to in the Taxon_Determination table, or a row in 
				  the TLI table has its Parent set to the record we are attempting
				  to delete. This will cause it to go to the RollbackAndExit method,
				  where the user can be asked if they want to replace the concept
				  with another (4.2.17.18). Before deleting the TLI records, we
				  need to remove the Taxon_Dictionary_Meaning_Mapping records.
				\*-----------------------------------------------------------------*/ 
				DELETE	Taxon_Dictionary_Meaning_Mapping
				WHERE	Preferred_Name = @TaxonListItemKey

				IF @@Error <> 0 GOTO RollbackAndExit

				DELETE 	Taxon_List_Item
				WHERE	Taxon_List_Item_Key = @TaxonListItemKey

				IF @@Error <> 0 GOTO RollbackAndExit
			END
		END			
	
		/*====================================*\
		  Delete the records.
		\*====================================*/
		-- Delete the Concept_History record.
		DELETE	Concept_History
		WHERE	Concept_Key = @Key

		IF @@Error <> 0 GOTO RollbackAndExit

		/*-------------------------------------------------------*\
		  Delete the relation records which refer to the concept.
		\*-------------------------------------------------------*/
		DELETE	Concept_Relation
		WHERE	To_Concept_Key = @Key
		OR	From_Concept_Key = @Key

		IF @@Error <> 0 GOTO RollbackAndExit

		DELETE	Meaning_Relation
		WHERE	To_Concept_Key = @Key
		OR	From_Concept_Key = @Key

		IF @@Error <> 0 GOTO RollbackAndExit

		DELETE	Term_Version_Relation
		WHERE	To_Concept_Key = @Key
		OR	From_Concept_Key = @Key

		IF @@Error <> 0 GOTO RollbackAndExit

		/*-------------------------------------------------------*\
		  Delete the Enquiry_Concept records because otherwise
		  the deletion will fail because it says other records
		  link to the Concept. Enquiries cannot be viewed in the
		  Thesaurus Editor it would appear at a casual glance
		  that nothing is actually linked to the concept. 
		  So best to just delete the Enquiry_Concept join records.
		\*-------------------------------------------------------*/
		DELETE	Enquiry_Concept
		WHERE	Concept_Key = @Key

		IF @@Error <> 0 GOTO RollbackAndExit

		-- Delete the Concept_Lineage records.
		IF EXISTS (SELECT 1 FROM Concept WHERE Concept_Key = @Key)
		BEGIN
			EXECUTE		usp_ConceptLineage_DeleteConcept	@Key
			IF @@ERROR <> 0 GOTO RollbackAndExit
		END

		/*-------------------------------------------------------*\
			Delete the concept's designation records (and related)
		\*-------------------------------------------------------*/
		DELETE DM
		FROM Taxon_Dictionary_Concept_Designation_Mapping DM
		INNER JOIN Concept_Designation CD ON CD.Concept_Designation_Key=DM.Concept_Designation_Key
		WHERE CD.Concept_Key=@Key

		IF @@Error <> 0 GOTO RollbackAndExit		

		--Delete the source files
		DELETE SF
		FROM Source_File SF
		INNER JOIN Source_Join SJ
				ON SJ.Table_Name='Concept_Designation'
		INNER JOIN Concept_Designation CD
				ON CD.Concept_Designation_Key=SJ.Record_Key
				AND CD.Concept_Key=@Key
	
		IF @@Error <> 0 GOTO RollbackAndExit
	
		--Now delete the source joins
		DELETE SJ
		FROM Source_Join SJ
		INNER JOIN Concept_Designation CD
				ON CD.Concept_Designation_Key=SJ.Record_Key
				AND CD.Concept_Key=@Key
		WHERE SJ.Table_Name='Concept_Designation'

		IF @@Error <> 0 GOTO RollbackAndExit

		DELETE 
		FROM Concept_Designation
		WHERE Concept_Key=@Key

		/*-------------------------------------------------------*\
			 Delete the Concept record. Have to check timestamp passed into the proc
			 against the timestamp the Concept had before any of its related records
			 were deleted. This is because deleting the records above may cause
			 triggers to be fired. Deleting the record in Concept_History will fire
			 a trigger that updates the current Concept, causing its timestamp to 
			 change.
		\*-------------------------------------------------------*/

		--Delete the source files
		DELETE SF
		FROM Source_File SF
		INNER JOIN Source_Join SJ
				ON SJ.Table_Name='Concept' AND SJ.Record_Key=@Key
	
		IF @@Error <> 0 GOTO RollbackAndExit
	
		--Now delete the source joins
		DELETE SJ
		FROM Source_Join SJ
		WHERE SJ.Table_Name='Concept' AND SJ.Record_Key=@Key

		IF @@Error <> 0 GOTO RollbackAndExit

		DELETE	Concept
		WHERE	Concept_Key = @Key
		AND		((@Timestamp = @OriginalTimestamp) OR (@Timestamp IS NULL))

		IF @@Error <> 0 GOTO RollbackAndExit

		-- Delete the Meaning record if only one Concept uses that Meaning key.
		IF @ConceptsSharingMeaningKeyCount = 1 
			DELETE 	Meaning
			WHERE	Meaning_Key = @MeaningKey

		IF @@Error <> 0 GOTO RollbackAndExit

		-- Delete the Term Version record if only one Concept uses that Term Version key.
		IF @ConceptsSharingTermVersionKeyCount = 1
			DELETE	Term_Version
			WHERE	Term_Version_Key = @TermVersionKey

		IF @@Error <> 0 GOTO RollbackAndExit

		-- Delete the Term record if only one Concept uses that Term key.
		IF @ConceptsSharingTermKeyCount = 1
			IF NOT EXISTS(SELECT * FROM Term_Version WHERE Term_Key = @TermKey)	
				DELETE	Term
				WHERE	Term_Key = @TermKey

		IF @@Error <> 0 GOTO RollbackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
	RAISERROR ('usp_Concept_Delete failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Concept_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concept_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concept_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Concept_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Concept_Delete TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_Concept_ImportTaxonDesignationTypes]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_Concept_ImportTaxonDesignationTypes]
GO

/*===========================================================================*\
  Description:	Import concepts corresponding to the taxon designation types
				used in the specified taxon list.

  Parameters:	@job_id					Job identifier

  Created:		Dec 2003

  Last revision information:
	$Revision: 2 $
	$Date: 2/02/09 16:46 $
	$Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Concept_ImportTaxonDesignationTypes]
	@job_id					INT
AS
	SET NOCOUNT ON
	SET ANSI_NULLS ON

	DECLARE     @taxon_list_key					CHAR(16),
				@taxon_designation_type_key		CHAR(16),
				@item_name						NVARCHAR(300),
				@ins_user_key					CHAR(16),
				@ins_date						DATETIME,
				@ins_session_id					CHAR(16),
				@upd_user_key					CHAR(16),
				@upd_date						DATETIME,
				@upd_session_id					CHAR(16),
				@system							BIT,
				@concept_designation_type_key	CHAR(16),
				@term_key						CHAR(16),
				@meaning_key					CHAR(16),
				@concept_history_key			CHAR(16)

	/* determine parameters of job */
	SELECT		@taxon_list_key							=	m.Taxon_List_Key
	FROM		Import_Export_Job						AS	j
	INNER JOIN	Taxon_Dictionary_Concept_Group_Mapping	AS	m
	ON			m.Concept_Group_Key						=	j.Concept_Group_Key
	WHERE		j.Import_Export_Job_ID					=	@job_id

	IF @@ROWCOUNT = 0
	BEGIN
		RAISERROR ('Job does not exist or has not been configured', 16, 1)
		RETURN
	END

	EXECUTE		usp_Import_Export_Job_UpdateStatus	@job_id,
													'Importing designation types'
	IF @@ERROR <> 0 RETURN

	DECLARE		types	CURSOR LOCAL FAST_FORWARD FOR
	SELECT DISTINCT
				tdt.TAXON_DESIGNATION_TYPE_KEY,
				ISNULL(tdt.LONG_NAME,
					   tdt.SHORT_NAME),
				tdt.ENTERED_BY,
				tdt.ENTRY_DATE,
				tdt.CHANGED_BY,
				tdt.CHANGED_DATE,
				tdt.SYSTEM_SUPPLIED_DATA
	FROM		TAXON_LIST_VERSION				AS	tlv
	INNER JOIN	TAXON_LIST_ITEM					AS	tli
	ON			tli.TAXON_LIST_VERSION_KEY		=	tlv.TAXON_LIST_VERSION_KEY
	INNER JOIN	TAXON_DESIGNATION				AS	td
	ON			td.TAXON_LIST_ITEM_KEY			=	tli.TAXON_LIST_ITEM_KEY
	INNER JOIN	TAXON_DESIGNATION_TYPE			AS	tdt
	ON			tdt.TAXON_DESIGNATION_TYPE_KEY	=	td.TAXON_DESIGNATION_TYPE_KEY
	WHERE		tlv.TAXON_LIST_KEY				=	@taxon_list_key

	OPEN		types

	WHILE 1 = 1
	BEGIN
		FETCH		types
		INTO		@taxon_designation_type_key,
					@item_name,
					@ins_user_key,
					@ins_date,
					@upd_user_key,
					@upd_date,
					@system

		IF @@FETCH_STATUS <> 0 BREAK

		BEGIN TRANSACTION

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

		SELECT		@concept_designation_type_key				=	tdm.Concept_Designation_Type_Key,
					@term_key									=	c.Term_Key
		FROM		Taxon_Dictionary_Designation_Type_Mapping	AS	tdm
		INNER JOIN	Concept										AS	c
		ON			c.Concept_Key								=	tdm.Concept_Designation_Type_Key
		WHERE		tdm.Taxon_Designation_Type_Key				=	@taxon_designation_type_key

		IF @@ROWCOUNT > 0
		BEGIN
			/* designation type has previously been imported */
			IF NOT EXISTS (	SELECT		1
							FROM		Term
							WHERE		Term_Key		=	@term_key
							AND			Language_Key	=	'en'
							AND			Item_Name		=	@item_name )
			BEGIN
				/* term has changed */
				IF EXISTS (	SELECT		1
							FROM		Concept
							WHERE		Term_Key			=	@term_key
							AND			Concept_Group_Key	<>	'SYSTEM000000000T' )
				BEGIN
					/* term is linked outside this concept group; create
					 * a new term instead of updating the existing one */
					EXECUTE		spNextKey	'Term',
											@term_key	OUTPUT
					IF @@ERROR <> 0 GOTO fail_from_cursor

					INSERT		Term (
								Term_Key,
								Language_Key,
								Item_Name,
								Plaintext,
								Entered_Session_ID,
								Changed_Session_ID,
								System_Supplied_Data)
					VALUES		(@term_key,
								'en',
								@item_name,
								@item_name,
								@ins_session_id,
								@upd_session_id,
								@system)

					IF @@ERROR <> 0 GOTO fail_from_cursor
				END
				ELSE
				BEGIN
					/* term only linked within this concept group */
					DECLARE		@cur_term_key		CHAR(16)

					SELECT		@cur_term_key	=	Term_Key
					FROM		Term
					WHERE		Language_Key	=	'en'
					AND			Item_Name		=	@item_name

					IF @@ROWCOUNT = 0
					BEGIN
						/* term can simply be updated */
						UPDATE		Term
						SET			Language_Key	=	'en',
									Item_Name		=	@item_name
						WHERE		Term_Key		=	@term_key

						IF @@ERROR <> 0 GOTO fail_from_cursor
					END
					ELSE
					BEGIN
						/* term cannot be updated; there is an existing
						 * term with the same name which we will link to
						 * instead */
						DELETE		Term
						WHERE		Term_Key			=	@term_key

						IF @@ERROR <> 0 GOTO fail_from_cursor

						SET			@term_key			=	@cur_term_key
					END
				END
			END

			UPDATE		Concept
			SET			Term_Key				=	@term_key,
						Entered_Session_ID		=	@ins_session_id,
						Changed_Session_ID		=	@upd_session_id,
						System_Supplied_Data	=	@system
			WHERE		Concept_Key				=	@concept_designation_type_key

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END
		ELSE
		BEGIN
			/* find/create term */
			SELECT		@term_key		=	Term_Key
			FROM		Term
			WHERE		Language_Key	=	'en'
			AND			Item_Name		=	@item_name

			IF @@ROWCOUNT = 0
			BEGIN
				EXECUTE		spNextKey	'Term',
										@term_key	OUTPUT
				IF @@ERROR <> 0 GOTO fail_from_cursor

				INSERT		Term (
							Term_Key,
							Language_Key,
							Item_Name,
							Plaintext,
							Entered_Session_ID,
							Changed_Session_ID,
							System_Supplied_Data)
				VALUES		(@term_key,
							'en',
							@item_name,
							@item_name,
							@ins_session_id,
							@upd_session_id,
							@system)

				IF @@ERROR <> 0 GOTO fail_from_cursor
			END

			/* create Meaning */
			EXECUTE		spNextKey	'Meaning',
									@meaning_key	OUTPUT
			IF @@ERROR <> 0 GOTO fail_from_cursor

			INSERT		Meaning (
						Meaning_Key)
			VALUES		(@meaning_key)

			IF @@ERROR <> 0 GOTO fail_from_cursor

			/* create Concept */
			EXECUTE		spNextKey	'Concept',
									@concept_designation_type_key	OUTPUT
			IF @@ERROR <> 0 GOTO fail_from_cursor

			INSERT		Concept (
						Concept_Key,
						Term_Key,
						Concept_Group_Key,
						List_Preferred,
						Is_Current,
						Preferred,
						Name_Type_Concept_Key,
						Meaning_Key,
						Entered_Session_ID,
						Changed_Session_ID,
						System_Supplied_Data)
			VALUES 		(@concept_designation_type_key,
						@term_key,
						'SYSTEM000000000T', /* "Concept Designation Types" group */
						1,
						1,
						1,
						'SYSTEM0000000000', /* "Formal" -- meaningless, but
												we need a value here */
						@meaning_key,
						@ins_session_id,
						@upd_session_id,
						@system)

			IF @@ERROR <> 0 GOTO fail_from_cursor

			/* create concept history */
			EXECUTE		spNextKey	'Concept_History',
									@concept_history_key	OUTPUT
			IF @@ERROR <> 0 GOTO fail_from_cursor

			INSERT		Concept_History (
						Concept_History_Key,
						Concept_Key,
						Concept_Group_Version_From,
						Entered_Session_ID,
						Changed_Session_ID,
						System_Supplied_Data)
			VALUES		(@concept_history_key,
						@concept_designation_type_key,
						'SYSTEM000000000T', /* "Concept Designation Types" version */
						@ins_session_id,
						@upd_session_id,
						@system)

			IF @@ERROR <> 0 GOTO fail_from_cursor

			/* record taxon designation type mapping */
			INSERT		Taxon_Dictionary_Designation_Type_Mapping (
						Taxon_Designation_Type_Key,
						Concept_Designation_Type_Key)
			VALUES		(@taxon_designation_type_key,
						@concept_designation_type_key)

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END

		EXECUTE		usp_Import_Export_Job_RecordProcessed	@job_id
		IF @@ERROR <> 0 GOTO fail_from_cursor

		COMMIT TRANSACTION
	END

	CLOSE		types
	DEALLOCATE	types
	RETURN

fail_from_cursor:
	CLOSE		types
	DEALLOCATE	types

fail:
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_Concept_ImportTaxonDesignationTypes failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_ImportTaxonDesignationTypes') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Concept_ImportTaxonDesignationTypes'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concept_ImportTaxonDesignationTypes TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concept_ImportTaxonDesignationTypes TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Concept_ImportTaxonDesignationTypes TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_Concept_ImportTaxonList]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_Concept_ImportTaxonList]
GO

/*===========================================================================*\
  Description:	Import concepts corresponding to the contents of a taxon list.

  Parameters:	@job_id					Job identifier

  Created:		Nov 2003

  Last revision information:
	$Revision: 2 $
	$Date: 2/02/09 16:46 $
	$Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Concept_ImportTaxonList]
	@job_id				INT
AS
	SET NOCOUNT ON
	SET ANSI_NULLS ON

	DECLARE     @taxon_list_key				CHAR(16),
				@concept_group_key			CHAR(16),
				@taxon_list_item_key		CHAR(16),
				@taxon_version_key			CHAR(16),
				@term_key					CHAR(16),
				@term_version_key			CHAR(16),
				@list_preferred				BIT,
				@is_current					BIT,
				@is_preferred				BIT,
				@taxon_rank_key				CHAR(16),
				@rank_uses_italics			BIT,
				@concept_rank_key			CHAR(16),
				@name_type_concept_key		CHAR(16),
				@sort_code					INT,
				@ins_user_key				CHAR(16),
				@ins_date					SMALLDATETIME,
				@ins_session_id				CHAR(16),
				@upd_user_key				CHAR(16),
				@upd_date					SMALLDATETIME,
				@upd_session_id				CHAR(16),
				@system						BIT,
				@preferred_name				CHAR(16),
				@taxon_list_version_from	CHAR(16),
				@taxon_list_version_to		CHAR(16),	
				@concept_group_version_from	CHAR(16),
				@concept_group_version_to	CHAR(16),
				@meaning_key				CHAR(16),
				@concept_key				CHAR(16)

	/* determine parameters of job */
	SELECT		@taxon_list_key							=	m.Taxon_List_Key,
				@concept_group_key						=	m.Concept_Group_Key
	FROM		Import_Export_Job						AS	j
	INNER JOIN	Taxon_Dictionary_Concept_Group_Mapping	AS	m
	ON			m.Concept_Group_Key						=	j.Concept_Group_Key
	WHERE		j.Import_Export_Job_ID					=	@job_id

	IF @@ROWCOUNT = 0
	BEGIN
		RAISERROR ('Job does not exist or has not been configured', 16, 1)
		RETURN
	END

	EXECUTE		usp_Import_Export_Job_UpdateStatus	@job_id,
													'Importing concepts'
	IF @@ERROR <> 0 RETURN

	/* remove current lineage data */
	DELETE		l
	FROM		Concept					AS	c
	INNER JOIN	Concept_Lineage			AS	l
	ON			l.Concept_Key			=	c.Concept_Key
	WHERE		c.Concept_Group_Key		=	@concept_group_key

	IF @@ERROR <> 0 RETURN

	DECLARE		@items	TABLE (
				Taxon_List_Item_Key	CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS,
				Rank_Uses_Italic	BIT)

	INSERT		@items
	SELECT      tli.TAXON_LIST_ITEM_KEY,
				tr.LIST_FONT_ITALIC
	FROM        TAXON_LIST_VERSION				AS	tlv
	INNER JOIN	TAXON_LIST_ITEM					AS	tli
	ON			tli.TAXON_LIST_VERSION_KEY		=	tlv.TAXON_LIST_VERSION_KEY
	INNER JOIN	TAXON_RANK						AS	tr
	ON			tr.TAXON_RANK_KEY				=	tli.TAXON_RANK_KEY
	WHERE		tlv.TAXON_LIST_KEY				=	@taxon_list_key

	DECLARE		items		CURSOR FAST_FORWARD LOCAL FOR
	SELECT		tli.TAXON_LIST_ITEM_KEY,
				tli.TAXON_VERSION_KEY,
				CASE WHEN tli.TAXON_LIST_ITEM_KEY = tli.PREFERRED_NAME
					THEN 1	/* list preferred */
					ELSE 0
				END,
				CASE WHEN tli.TAXON_LIST_VERSION_TO IS NULL
					THEN 1	/* current */
					ELSE 0
				END,
				tli.TAXON_RANK_KEY,
				itm.Rank_Uses_Italic,
				tli.SORT_CODE,
				tli.ENTERED_BY,
				tli.ENTRY_DATE,
				tli.CHANGED_BY,
				tli.CHANGED_DATE,
				tli.SYSTEM_SUPPLIED_DATA,
				tli.PREFERRED_NAME,
				tli.TAXON_LIST_VERSION_KEY,
				tli.TAXON_LIST_VERSION_TO
	FROM		@items							AS	itm
	INNER JOIN	TAXON_LIST_ITEM					AS	tli
	ON			tli.TAXON_LIST_ITEM_KEY			=	itm.TAXON_LIST_ITEM_KEY

	OPEN        items

	WHILE 1 = 1
	BEGIN
		FETCH		items
		INTO		@taxon_list_item_key,
					@taxon_version_key,
					@list_preferred,
					@is_current,
					@taxon_rank_key,
					@rank_uses_italics,
					@sort_code,
					@ins_user_key,
					@ins_date,
					@upd_user_key,
					@upd_date,
					@system,
					@preferred_name,
					@taxon_list_version_from,
					@taxon_list_version_to

		IF @@FETCH_STATUS <> 0 BREAK

		BEGIN TRANSACTION

		/* perform mappings */
		SELECT		@term_version_key						=	Term_Version_Key
		FROM		Taxon_Dictionary_Term_Version_Mapping
		WHERE		Taxon_Version_Key						=	@taxon_version_key

		IF @@ROWCOUNT = 0 GOTO skip_item

		SELECT		@term_key								=	tm.Term_Key,
					@name_type_concept_key					=	ntm.Thesaurus_Name_Type_Key
		FROM		TAXON_VERSION							AS	tv
		INNER JOIN	TAXON									AS	tx
		ON			tx.TAXON_KEY							=	tv.TAXON_KEY
		INNER JOIN	Taxon_Dictionary_Term_Mapping			AS	tm
		ON			tm.Taxon_Key							=	tx.TAXON_KEY
		AND			tm.Italic_Font							=	CASE WHEN tx.Language = 'La'
																	 AND @rank_uses_italics = 1
																	THEN 1
																	ELSE 0
																END
		INNER JOIN	Taxon_Dictionary_Name_Type_Mapping		AS	ntm
		ON			ntm.Taxon_Name_Type_Key					=	tx.TAXON_NAME_TYPE_KEY
		WHERE		tv.TAXON_VERSION_KEY					=	@taxon_version_key

		IF @@ROWCOUNT = 0 GOTO skip_item

		SELECT		@concept_rank_key						=	Concept_Rank_Key
		FROM		Taxon_Dictionary_Concept_Rank_Mapping
		WHERE		Taxon_Rank_Key							=	@taxon_rank_key

		IF @@ROWCOUNT = 0 GOTO skip_item

		IF @list_preferred = 1
			SET			@is_preferred		=	1
		ELSE
		BEGIN
			SELECT		@is_preferred 		=	CASE WHEN TAXON_VERSION_KEY = @taxon_version_key
													THEN 1
													ELSE 0
												END
			FROM		TAXON_COMMON_NAME
			WHERE		TAXON_LIST_ITEM_KEY	=	@taxon_list_item_key
		END

		SELECT      @concept_group_version_from						=	Concept_Group_Version_Key
		FROM		Taxon_Dictionary_Concept_Group_Version_Mapping
		WHERE		Taxon_List_Version_Key							=   @taxon_list_version_from

		IF @@ROWCOUNT = 0 GOTO skip_item

		SELECT		@concept_group_version_to						=	Concept_Group_Version_Key
		FROM		Taxon_Dictionary_Concept_Group_Version_Mapping
		WHERE		Taxon_List_Version_Key							=	@taxon_list_version_to

		IF @@ROWCOUNT = 0
		BEGIN
			SET			@concept_group_version_to	=	NULL
		END

		/* obtain meaning key */
		SELECT		@meaning_key						=	Meaning_Key
		FROM        Taxon_Dictionary_Meaning_Mapping
		WHERE		Preferred_Name						=	@preferred_name

		IF @@ROWCOUNT = 0
		BEGIN
			/* look for meaning assigned to synonyms of @preferred_name from
			 * some other taxon list */
			SELECT		@meaning_key						=	tdm.Meaning_Key
			FROM		INDEX_TAXON_SYNONYM					AS	its
			INNER JOIN	Taxon_Dictionary_Meaning_Mapping	AS	tdm
			ON			tdm.Preferred_Name					=	its.SYNONYM_LIST_ITEM_KEY
			WHERE		its.TAXON_LIST_ITEM_KEY				=	@preferred_name

			IF @@ROWCOUNT = 0
			BEGIN
				/* create new meaning */
				EXECUTE		spNextKey	'Meaning',
										@meaning_key	OUTPUT
				IF @@ERROR <> 0 GOTO fail_from_cursor

				INSERT		Meaning (
							Meaning_Key)
				VALUES		(@meaning_key)

				IF @@ERROR <> 0 GOTO fail_from_cursor
			END

			INSERT		Taxon_Dictionary_Meaning_Mapping (
						Preferred_Name,
						Meaning_Key)
			VALUES		(@preferred_name,
						@meaning_key)

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END

		IF @meaning_key IS NOT NULL
				/* meaning not explicitly mapped to null,
				 * so we can import item */
		BEGIN
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

			SELECT      @concept_key						=	Concept_Key
			FROM		Taxon_Dictionary_Concept_Mapping
			WHERE		Taxon_List_Item_Key					=	@taxon_list_item_key

			IF @@ROWCOUNT > 0
			BEGIN
				DECLARE		@old_group_key			CHAR(16),
							@was_list_preferred		BIT
							
				/* update concept */
				UPDATE		Concept
				SET         @old_group_key				=	Concept_Group_Key,
							@was_list_preferred			=	List_Preferred,
							Term_Key					=	@term_key,
							Concept_Group_Key			=	@concept_group_key,
							Term_Version_Key			=	@term_version_key,
							List_Preferred				=	@list_preferred,
							Is_Current					=	@is_current,
							Preferred					=	@is_preferred,
							Concept_Rank_Key			=	@concept_rank_key,
							Name_Type_Concept_Key		=   @name_type_concept_key,
							Meaning_Key					=	@meaning_key,
							Sort_Code					=	@sort_code,
							Entered_Session_ID			=	@ins_session_id,
							Changed_Session_ID			=	@upd_session_id,
							System_Supplied_Data		=	@system
				WHERE		Concept_Key					=	@concept_key

				IF @@ERROR <> 0 GOTO fail_from_cursor

				/* re-create concept history */
				DELETE		Concept_History
				WHERE		Concept_Key					=	@concept_key

				IF @@ERROR <> 0 GOTO fail_from_cursor

				EXECUTE		usp_ConceptHistory_Insert_Imported	@concept_key,
																@concept_group_version_from,
																@concept_group_version_to
				IF @@ERROR <> 0 GOTO fail_from_cursor
			END
			ELSE
			BEGIN
				/* create concept */
				EXECUTE		spNextKey	'Concept',
										@concept_key	OUTPUT
				IF @@ERROR <> 0 GOTO fail_from_cursor

				INSERT		Concept
							(Concept_Key,
							Term_Key,
							Concept_Group_Key,
							Term_Version_Key,
							List_Preferred,
							Is_Current,
							Preferred,
							Concept_Rank_Key,
							Name_Type_Concept_Key,
							Meaning_Key,
							Sort_Code,
							Entered_Session_ID,
							Changed_Session_ID,
							System_Supplied_Data)
				VALUES		(@concept_key,
							@term_key,
							@concept_group_key,
							@term_version_key,
							@list_preferred,
							@is_current,
							@is_preferred,
							@concept_rank_key,
							@name_type_concept_key,
							@meaning_key,
							@sort_code,
							@ins_session_id,
							@upd_session_id,
							@system)

				IF @@ERROR <> 0 GOTO fail_from_cursor

				/* create concept history */
				EXECUTE		usp_ConceptHistory_Insert_Imported	@concept_key,
																@concept_group_version_from,
																@concept_group_version_to
				IF @@ERROR <> 0 GOTO fail_from_cursor

				/* record mapping */
				INSERT		Taxon_Dictionary_Concept_Mapping
							(Taxon_List_Item_Key,
							Concept_Key)
				VALUES		(@taxon_list_item_key,
							@concept_key)

				IF @@ERROR <> 0 GOTO fail_from_cursor
			END
		END
		
skip_item:
		EXECUTE		usp_Import_Export_Job_RecordProcessed	@job_id
		IF @@ERROR <> 0 GOTO fail_from_cursor

		COMMIT TRANSACTION
	END

	CLOSE		items
	DEALLOCATE	items
	RETURN

fail_from_cursor:
	CLOSE		items
	DEALLOCATE	items

fail:
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_Concept_ImportTaxonList failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_ImportTaxonList') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Concept_ImportTaxonList'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concept_ImportTaxonList TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concept_ImportTaxonList TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Concept_ImportTaxonList TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Domains_Select_ForOccurrences') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_Domains_Select_ForOccurrences]
GO

/*===========================================================================*\
  Description:	Returns a list of domain names and keys.

  Parameters:	
	@NameKey	Currently logged in user.

  Created:	October 2003

  Last revision information:
    $Revision: 2 $
    $Date: 2/02/09 16:46 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Domains_Select_ForOccurrences]
	@NameKey CHAR(16),
	@UserDomainMask INT
AS

SET NOCOUNT ON

	SELECT 	D.Domain_Key, D.Item_Name
	FROM	Domain D
	JOIN	User_Domain_Access UDA ON UDA.Domain_Key = D.Domain_Key AND UDA.Allow_Add = 1
	WHERE	D.Has_Occurrences = 1
	AND	UDA.Name_Key = @NameKey
	AND 	D.Domain_Mask & @UserDomainMask > 0
	ORDER BY D.Item_Name

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Domains_Select_ForOccurrences') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Domains_Select_ForOccurrences'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Domains_Select_ForOccurrences TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Domains_Select_ForOccurrences TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Domains_Select_ForOccurrences TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Domains_Select_ForOccurrences TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Domains_Select_ForOccurrences TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Domains_Select_ForOccurrences TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_LocationFullySpecified_Select]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_LocationFullySpecified_Select]
GO

/*===========================================================================*\
  Description:	Return a Location name with preferred name and spatial ref.

  Parameters:	@ItemKey
		@Output

  Created:	June 2004

  Last revision information:
    $Revision: 2 $
    $Date: 2/02/09 16:46 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_LocationFullySpecified_Select]
	@ItemKey char(16),
	@Output varchar(300) OUTPUT
AS
	SELECT	@Output = LN1.Item_Name + ' (' + LN2.Item_Name + ' - ' + L.Spatial_Ref + ')'
	FROM	Location L
	JOIN	Location_Name LN1 ON LN1.Location_Key = L.Location_Key
	JOIN	Location_Name LN2 ON LN2.Location_Key = L.Location_Key AND LN2.Preferred = 1
	WHERE	L.Location_Key = @ItemKey
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_LocationFullySpecified_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_LocationFullySpecified_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_LocationFullySpecified_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_LocationFullySpecified_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_LocationFullySpecified_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_LocationFullySpecified_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_LocationFullySpecified_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_LocationFullySpecified_Select TO [Dev - JNCC SQL]
END

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_TermVersion_ImportTaxonList]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_TermVersion_ImportTaxonList]
GO

/*===========================================================================*\
  Description:	Import term versions corresponding to items in a taxon list.

  Parameters:   @job_id					Job identifier

  Created:		Nov 2003

  Last revision information:
	$Revision: 2 $
	$Date: 2/02/09 16:46 $
	$Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_TermVersion_ImportTaxonList]
	@job_id				INT
AS
	SET NOCOUNT ON

	DECLARE     @taxon_list_key			CHAR(16),
				@taxon_version_key		CHAR(16),
				@term_version_key		CHAR(16),
				@term_key				CHAR(16),
				@version				VARCHAR(10),
				@author_and_date		VARCHAR(40),
				@source_key				CHAR(16),
				@source_join_key		CHAR(16),	
				@ins_user_key			CHAR(16),
				@ins_date				SMALLDATETIME,
				@ins_session_id			CHAR(16),
				@upd_user_key			CHAR(16),
				@upd_date				SMALLDATETIME,
				@upd_session_id			CHAR(16),
				@system					BIT

	/* determine parameters of job */
	SELECT		@taxon_list_key							=	m.Taxon_List_Key
	FROM		Import_Export_Job						AS	j
	INNER JOIN	Taxon_Dictionary_Concept_Group_Mapping	AS	m
	ON			m.Concept_Group_Key						=	j.Concept_Group_Key
	WHERE		j.Import_Export_Job_ID					=	@job_id

	IF @@ROWCOUNT = 0
	BEGIN
		RAISERROR ('Job does not exist or has not been configured', 16, 1)
		RETURN
	END

	EXECUTE		usp_Import_Export_Job_UpdateStatus	@job_id,
													'Importing term versions'
	IF @@ERROR <> 0 RETURN

	DECLARE		@versions	TABLE (
				Taxon_Version_Key	CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS,
				List_Font_Italic	BIT)

	INSERT		@versions
	SELECT      tli.TAXON_VERSION_KEY,
				tr.List_Font_Italic
	FROM        TAXON_LIST_VERSION				AS	tlv
	INNER JOIN	TAXON_LIST_ITEM					AS	tli
	ON			tli.TAXON_LIST_VERSION_KEY		=	tlv.TAXON_LIST_VERSION_KEY
	INNER JOIN	TAXON_RANK						AS	tr
	ON			tr.TAXON_RANK_KEY				=	tli.TAXON_RANK_KEY
	WHERE		tlv.TAXON_LIST_KEY				=	@taxon_list_key

	DECLARE		versions	CURSOR LOCAL FAST_FORWARD FOR
	SELECT DISTINCT
				tv.TAXON_VERSION_KEY,
				tm.Term_Key,
				tv.ATTRIBUTE,
				tx.AUTHORITY,
				tv.SOURCE_KEY,
				tv.ENTERED_BY,
				tv.ENTRY_DATE,
				tv.CHANGED_BY,
				tv.CHANGED_DATE,
				tv.SYSTEM_SUPPLIED_DATA
	FROM		@versions						AS	v0
	INNER JOIN	TAXON_VERSION					AS	tv
	ON			tv.TAXON_VERSION_KEY			=	v0.TAXON_VERSION_KEY
	INNER JOIN	TAXON							AS	tx
	ON			tx.TAXON_KEY					=	tv.TAXON_KEY
	INNER JOIN	Taxon_Dictionary_Term_Mapping	AS	tm
	ON			tm.Taxon_Key					=	tx.TAXON_KEY
	AND			tm.Italic_Font					=	CASE WHEN tx.LANGUAGE = 'La'
														 AND v0.LIST_FONT_ITALIC = 1
														THEN 1
														ELSE 0
													END

	OPEN		versions

	WHILE 1 = 1
	BEGIN
		FETCH		versions
		INTO		@taxon_version_key,
					@term_key,
					@version,
					@author_and_date,
					@source_key,
					@ins_user_key,
					@ins_date,
					@upd_user_key,
					@upd_date,
					@system

		IF @@FETCH_STATUS <> 0 BREAK

		BEGIN TRANSACTION

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

		/* look for existing mapping */
		SELECT		@term_version_key						=	Term_Version_Key,
					@source_join_key						=	Source_Join_Key
		FROM		Taxon_Dictionary_Term_Version_Mapping
		WHERE		Taxon_Version_Key						=	@taxon_version_key

		IF @@ROWCOUNT > 0
		BEGIN
			/* update term version */
			UPDATE		Term_Version
			SET			Version_Label			=	@version,
						Author_And_Date			=	@author_and_date,
						Changed_Session_ID		=	@upd_session_id,
						System_Supplied_Data	=	@system
			FROM		Term_Version
			WHERE		Term_Version_Key		=	@term_version_key

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END
		ELSE
		BEGIN
			/* create term version */
			EXECUTE		spNextKey	'Term_Version',
									@term_version_key	OUTPUT
			IF @@ERROR <> 0 GOTO fail_from_cursor

			SET			@source_join_key		=	NULL

			INSERT		Term_Version (
						Term_Version_Key,
						Term_Key,
						Version_Label,
						Author_And_Date,
						Entered_Session_ID,
						Changed_Session_ID,
						System_Supplied_Data)
			SELECT		@term_version_key,
						@term_key,
						@version,
						@author_and_date,
						@ins_session_id,
						@upd_session_id,
						@system

			IF @@ERROR <> 0 GOTO fail_from_cursor

			/* record mapping */
			INSERT		Taxon_Dictionary_Term_Version_Mapping (
						Taxon_Version_Key,
						Term_Version_Key)
			VALUES 		(@taxon_version_key,
						@term_version_key)

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END

		/* make any changes required in Source_Join */
		IF @source_key IS NULL
		BEGIN
			UPDATE		Taxon_Dictionary_Term_Version_Mapping
			SET			Source_Join_Key							=	NULL
			WHERE		Term_Version_Key						=	@term_version_key

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END

		EXECUTE		usp_SourceJoin_RecordImported	@source_join_key	OUTPUT,
													'Term_Version',
													@term_version_key,
													@source_key,
													@ins_session_id,
													@system
		IF @@ERROR <> 0 GOTO fail_from_cursor

		IF @source_key IS NOT NULL
		BEGIN
			UPDATE		Taxon_Dictionary_Term_Version_Mapping
			SET			Source_Join_Key							=	@source_join_key
			WHERE		Term_Version_Key						=	@term_version_key

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END

		/* update progress counter */
		EXECUTE		usp_Import_Export_Job_RecordProcessed	@job_id
		IF @@ERROR <> 0 GOTO fail_from_cursor

		COMMIT TRANSACTION
	END

	CLOSE		versions
	DEALLOCATE	versions
	RETURN

fail_from_cursor:
	CLOSE		versions
	DEALLOCATE	versions

fail:
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_TermVersion_ImportTaxonList failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TermVersion_ImportTaxonList') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_TermVersion_ImportTaxonList'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_TermVersion_ImportTaxonList TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_TermVersion_ImportTaxonList TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_TermVersion_ImportTaxonList TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_Term_ImportTaxonList]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_Term_ImportTaxonList]
GO

/*===========================================================================*\
  Description:	Import terms corresponding to items in a taxon list.

  Parameters:   @job_id					Job identifier

  Created:		Nov 2003

  Last revision information:
	$Revision: 2 $
	$Date: 2/02/09 16:46 $
	$Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Term_ImportTaxonList]
	@job_id				INT
AS
	SET NOCOUNT ON
	SET ANSI_NULLS ON

	DECLARE     @concept_group_key	CHAR(16),
				@taxon_list_key		CHAR(16),
				@taxon_key			CHAR(16),
				@term_key			CHAR(16),
				@item_name      	VARCHAR(60),
				@language			VARCHAR(2),
				@ins_user_key		CHAR(16),
				@ins_date			SMALLDATETIME,
				@ins_session_id		CHAR(16),
				@upd_user_key		CHAR(16),
				@upd_date			SMALLDATETIME,
				@upd_session_id		CHAR(16),
				@system				BIT,
				@italic				BIT,
				@plaintext			NVARCHAR(300),
				@create_term		BIT

	/* determine parameters of job */
	SELECT		@concept_group_key						=	m.Concept_Group_Key,
				@taxon_list_key							=	m.Taxon_List_Key
	FROM		Import_Export_Job						AS	j
	INNER JOIN	Taxon_Dictionary_Concept_Group_Mapping	AS	m
	ON			m.Concept_Group_Key						=	j.Concept_Group_Key
	WHERE		j.Import_Export_Job_ID					=	@job_id

	IF @@ROWCOUNT = 0
	BEGIN
		RAISERROR ('Job does not exist or has not been configured', 16, 1)
		RETURN
	END

	EXECUTE		usp_Import_Export_Job_UpdateStatus	@job_id,
													'Importing terms'
	IF @@ERROR <> 0 RETURN

	DECLARE		@versions	TABLE (
				Taxon_Version_Key	CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS,
				List_Font_Italic	BIT)

	INSERT		@versions
    SELECT      tli.TAXON_VERSION_KEY,
                tr.List_Font_Italic
	FROM        TAXON_LIST_VERSION				AS	tlv
	INNER JOIN	TAXON_LIST_ITEM					AS	tli
	ON			tli.TAXON_LIST_VERSION_KEY		=	tlv.TAXON_LIST_VERSION_KEY
	INNER JOIN	TAXON_RANK						AS	tr
	ON			tr.TAXON_RANK_KEY				=	tli.TAXON_RANK_KEY
	WHERE		tlv.TAXON_LIST_KEY				=	@taxon_list_key

	DECLARE		terms	CURSOR LOCAL FAST_FORWARD FOR
	SELECT DISTINCT
				t.TAXON_KEY,
				t.ITEM_NAME,
				t.LANGUAGE,
				t.ENTERED_BY,
				t.ENTRY_DATE,
				t.CHANGED_BY,
				t.CHANGED_DATE,
				t.SYSTEM_SUPPLIED_DATA,
				CASE WHEN t.LANGUAGE = 'La'
					 AND v0.LIST_FONT_ITALIC = 1
					THEN 1
					ELSE 0
				END
	FROM		@versions							AS	v0
	INNER JOIN	TAXON_VERSION						AS	tv
	ON			tv.TAXON_VERSION_KEY				=	v0.TAXON_VERSION_KEY
	INNER JOIN	TAXON								AS	t
	ON			t.TAXON_KEY							=	tv.TAXON_KEY

	OPEN		terms

	WHILE 1 = 1
	BEGIN
		FETCH		terms
		INTO		@taxon_key,
					@plaintext,
					@language,
					@ins_user_key,
					@ins_date,
					@upd_user_key,
					@upd_date,
					@system,
					@italic

		IF @@FETCH_STATUS <> 0 BREAK

		SET			@item_name		=	CASE WHEN @italic = 0
											THEN @plaintext
											ELSE '<i>' + @plaintext + '</i>'
										END

		BEGIN TRANSACTION										

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

		/* check for existing mapping */
		SELECT		@term_key						=	Term_Key
		FROM		Taxon_Dictionary_Term_Mapping
		WHERE		Taxon_Key						=	@taxon_key
		AND			Italic_Font						=	@italic

		SELECT		@create_term	=	CASE WHEN @@ROWCOUNT = 0
											THEN 1
											ELSE 0
										END

		IF @create_term = 0
		BEGIN
			IF NOT EXISTS (	SELECT		1
							FROM		Term
							WHERE		Term_Key				=	@term_key
							AND			Language_Key			=	@language
							AND			Item_Name				=	@item_name )
			BEGIN
				/* term has been modified */
				IF EXISTS (	SELECT		1
							FROM		Concept
							WHERE		Term_Key			=	@term_key
							AND			Concept_Group_Key	<>	@concept_group_key )
				BEGIN
					/* term is linked outside this concept group */
					SET			@create_term	=	1
				END
				ELSE
				BEGIN
					/* term linked only within this concept group */
					DECLARE		@new_term_key	CHAR(16)

					SELECT		@new_term_key	=	Term_Key
					FROM		Term
					WHERE		Language_Key	=	@language
					AND			Item_Name		=	@item_name
					AND			Term_Key		<>	@term_key

					IF @@ROWCOUNT = 0
					BEGIN
						/* update the current term */
						UPDATE		Term
						SET         Language_Key		=	@language,
									Item_Name			=	@item_name,
									Plaintext			=	@plaintext,
									Changed_Session_ID	=	@upd_session_id
						WHERE		Term_Key			=	@term_key

						IF @@ERROR <> 0 GOTO fail_from_cursor
					END
					ELSE
					BEGIN
						/* remove current term */
						UPDATE		Concept
						SET			Term_Key			=	@new_term_key,
									Term_Version_Key	=	NULL
						WHERE		Term_Key			=	@term_key

						IF @@ERROR <> 0 GOTO fail_from_cursor

						DELETE		Term_Version
						WHERE		Term_Key		=	@term_key

						IF @@ERROR <> 0 GOTO fail_from_cursor
						
						DELETE		Term
						WHERE		Term_Key		=	@term_key

						IF @@ERROR <> 0 GOTO fail_from_cursor

						/* link to the existing term that already
						 * has the new details */
						INSERT		Taxon_Dictionary_Term_Mapping (
									Taxon_Key,
									Italic_Font,
									Term_Key)
						VALUES		(@taxon_key,
									@italic,
									@new_term_key)

						IF @@ERROR <> 0 GOTO fail_from_cursor
					END
				END
			END /* term has been modified */
		END /* if @create_term = 0 */

		IF @create_term = 1
		BEGIN
			/* check for existing term that could be used */
			SELECT		@term_key		=	Term_Key
			FROM		Term
			WHERE		Language_Key	=	@language
			AND			Item_Name		=	@item_name

			IF @@ROWCOUNT > 0
			BEGIN
				/* map taxon onto the existing term */
				INSERT		Taxon_Dictionary_Term_Mapping (
							Taxon_Key,
							Italic_Font,
							Term_Key)
				VALUES		(@taxon_key,
							@italic,
							@term_key)

				IF @@ERROR <> 0 GOTO fail_from_cursor
			END
			ELSE
			BEGIN
				/* create term */
				EXECUTE		spNextKey	'Term',
										@term_key	OUTPUT
				IF @@ERROR <> 0 GOTO fail_from_cursor

				INSERT		Term (
							Term_Key,
							Language_Key,
							Item_Name,
							Plaintext,
							Entered_Session_ID,
							Changed_Session_ID,
							System_Supplied_Data)
				VALUES		(@term_key,
							@language,
							@item_name,
							@plaintext,
							@ins_session_id,
							@upd_session_id,
							@system)

				IF @@ERROR <> 0 GOTO fail_from_cursor

				/* record mapping */
				INSERT		Taxon_Dictionary_Term_Mapping
							(Taxon_Key,
							Italic_Font,
							Term_Key)
				VALUES		(@taxon_key,
							@italic,
							@term_key)

				IF @@ERROR <> 0 GOTO fail_from_cursor
			END
		END

		EXECUTE		usp_Import_Export_Job_RecordProcessed	@job_id
		IF @@ERROR <> 0 GOTO fail_from_cursor

		COMMIT TRANSACTION
	END

	CLOSE		terms
	DEALLOCATE	terms
	RETURN

fail_from_cursor:
	CLOSE		terms
	DEALLOCATE	terms

fail:
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_Term_ImportTaxonList failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Term_ImportTaxonList') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Term_ImportTaxonList'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Term_ImportTaxonList TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Term_ImportTaxonList TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Term_ImportTaxonList TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_ThesaurusFact_ImportTaxonList]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_ThesaurusFact_ImportTaxonList]
GO

/*===========================================================================*\
  Description:	Import thesaurus facts corresponding to the facts associated
  				with a taxon list.

  Parameters:	@job_id					Job identifier

  Created:		Dec 2003

  Last revision information:
	$Revision: 2 $
	$Date: 2/02/09 16:46 $
	$Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ThesaurusFact_ImportTaxonList]
	@job_id				INT
AS
	SET NOCOUNT ON

	DECLARE		@taxon_list_key			CHAR(16),
				@taxon_fact_key			CHAR(16),
				@type					VARCHAR(1),
				@meaning_key			CHAR(16),
				@ins_user_key			CHAR(16),
				@ins_date				SMALLDATETIME,
				@ins_session_id			CHAR(16),
				@upd_user_key			CHAR(16),
				@upd_date				SMALLDATETIME,
				@upd_session_id			CHAR(16),
				@system					BIT,
				@source_key				CHAR(16),
				@thesaurus_fact_key		CHAR(16),
				@fact_type_concept_key	CHAR(16),
				@source_join_key		CHAR(16)

	/* determine parameters of job */
	SELECT		@taxon_list_key							=	m.Taxon_List_Key
	FROM		Import_Export_Job						AS	j
	INNER JOIN	Taxon_Dictionary_Concept_Group_Mapping	AS	m
	ON			m.Concept_Group_Key						=	j.Concept_Group_Key
	WHERE		j.Import_Export_Job_ID					=	@job_id

	IF @@ROWCOUNT = 0
	BEGIN
		RAISERROR ('Job does not exist or has not been configured', 16, 1)
		RETURN
	END

	EXECUTE		usp_Import_Export_Job_UpdateStatus	@job_id,
													'Importing facts'
	IF @@ERROR <> 0 RETURN

	DECLARE		@versions	TABLE (
				Taxon_Version_Key	CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS)

	INSERT		@versions
	SELECT		tli.TAXON_VERSION_KEY
	FROM		TAXON_LIST_VERSION						AS	tlv
	INNER JOIN	TAXON_LIST_ITEM							AS	tli
	ON			tli.TAXON_LIST_VERSION_KEY				=	tlv.TAXON_LIST_VERSION_KEY
	WHERE		tlv.TAXON_LIST_KEY						=	@taxon_list_key

	DECLARE		facts	CURSOR LOCAL FAST_FORWARD FOR
	SELECT DISTINCT
				tf.TAXON_FACT_KEY,
				tf.TYPE,
				CASE tf.TYPE
					WHEN 'T' THEN 'SYSTEM00000002NO' /* HTML */
					WHEN 'S' THEN 'SYSTEM00000002NO' /* HTML */
					WHEN 'A' THEN 'SYSTEM00000002L9' /* AVI */
					WHEN 'W' THEN 'SYSTEM00000002L8' /* WAV */
					WHEN 'B' THEN 'SYSTEM00000000W0' /* Bitmap */
					WHEN 'J' THEN 'SYSTEM00000000VY' /* JPEG */
				END,
				c.Meaning_Key,
				tf.ENTERED_BY,
				tf.ENTRY_DATE,
				tf.CHANGED_BY,
				tf.CHANGED_DATE,
				tf.SYSTEM_SUPPLIED_DATA,
				tf.SOURCE_KEY
	FROM		@versions								AS	tli
	INNER JOIN	TAXON_FACT								AS	tf
	ON			tf.TAXON_VERSION_KEY					=	tli.TAXON_VERSION_KEY
	INNER JOIN	Taxon_Dictionary_Term_Version_Mapping	AS	m
	ON			m.Taxon_Version_Key						=	tf.TAXON_VERSION_KEY
	INNER JOIN	Concept									AS	c
	ON			c.Term_Version_Key						=	m.Term_Version_Key

	OPEN		facts

	WHILE 1 = 1
	BEGIN
		FETCH		facts
		INTO        @taxon_fact_key,
					@type,
					@fact_type_concept_key,
					@meaning_key,
					@ins_user_key,
					@ins_date,
					@upd_user_key,
					@upd_date,
					@system,
					@source_key

		IF @@FETCH_STATUS <> 0 BREAK

		BEGIN TRANSACTION

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

		SELECT		@thesaurus_fact_key						=	Thesaurus_Fact_Key,
					@source_join_key						=	Source_Join_Key
		FROM		Taxon_Dictionary_Thesaurus_Fact_Mapping
		WHERE		Taxon_Fact_Key							=	@taxon_fact_key

		IF @@ROWCOUNT > 0
		BEGIN
			/* update existing thesaurus fact */
			UPDATE		Thesaurus_Fact
			SET			Item_Name				=	tf.TITLE,
						Data
							=	CASE
									WHEN @type = 'T' OR @type = 'S' THEN tf.DATA
									WHEN CHARINDEX('href="', tf.DATA) > 0 THEN
										SUBSTRING(
											tf.DATA,
											CHARINDEX('href="', tf.DATA) + 6,
											CHARINDEX('"', tf.DATA, CHARINDEX('href="', tf.DATA) + 6)
											- (CHARINDEX('href="', tf.DATA) + 6))
									WHEN CHARINDEX('href=''', tf.DATA) > 0 THEN
										SUBSTRING(
											tf.DATA,
											CHARINDEX('href=''', tf.DATA) + 6,
											CHARINDEX('''', tf.DATA, CHARINDEX('href=''', tf.DATA) + 6)
										- (CHARINDEX('href=''', tf.DATA) + 6))
									ELSE SUBSTRING(
											tf.DATA,
											CHARINDEX('href=', tf.DATA) + 5,
											PATINDEX(
												'%[ >]%',
												SUBSTRING(
													tf.DATA,
													CHARINDEX('href=', tf.DATA) + 5,
													DATALENGTH(tf.DATA))) - 1)
								END,
						Meaning_Key				=	@meaning_key,
						Concept_Key				=	NULL,
						Term_Version_Key    	=	NULL,
						Inherited				=	0,
						Fact_Vague_Date_Start	=	tf.FACT_VAGUE_DATE_START,
						Fact_Vague_Date_End		=	tf.FACT_VAGUE_DATE_END,
						Fact_Vague_Date_Type	=	ISNULL(
														tf.FACT_VAGUE_DATE_TYPE,
														'U'),
						Fact_Type_Concept_Key	=	@fact_type_concept_key,
						Entered_Session_ID		=	@ins_session_id,
						Changed_Session_ID		=	@upd_session_id,
						System_Supplied_Data	=	@system
			FROM		TAXON_FACT			   	AS	tf,
						Thesaurus_Fact
			WHERE		tf.TAXON_FACT_KEY		=	@taxon_fact_key
			AND			Thesaurus_Fact_Key		=	@thesaurus_fact_key

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END
		ELSE
		BEGIN
			/* create thesaurus fact */
			EXECUTE		spNextKey	'Thesaurus_Fact',
									@thesaurus_fact_key		OUTPUT
			IF @@ERROR <> 0 GOTO fail_from_cursor

			SET			@source_join_key			=	NULL

			INSERT		Thesaurus_Fact (
						Thesaurus_Fact_Key,
						Item_Name,
						Data,
						Meaning_Key,
						Language_Key,
						Fact_Vague_Date_Start,
						Fact_Vague_Date_End,
						Fact_Vague_Date_Type,
						Fact_Type_Concept_Key,
						Related_Term_Versions,
						Entered_Session_ID,
						Changed_Session_ID,
						System_Supplied_Data)
			SELECT		@thesaurus_fact_key,
						tf.TITLE,
						CASE
							WHEN @type = 'T' OR @type = 'S' THEN tf.DATA
							WHEN CHARINDEX('href="', tf.DATA) > 0 THEN
								SUBSTRING(
									tf.DATA,
									CHARINDEX('href="', tf.DATA) + 6,
									CHARINDEX('"', tf.DATA, CHARINDEX('href="', tf.DATA) + 6)
									- (CHARINDEX('href="', tf.DATA) + 6))
							WHEN CHARINDEX('href=''', tf.DATA) > 0 THEN
								SUBSTRING(
									tf.DATA,
									CHARINDEX('href=''', tf.DATA) + 6,
									CHARINDEX('''', tf.DATA, CHARINDEX('href=''', tf.DATA) + 6)
								- (CHARINDEX('href=''', tf.DATA) + 6))
							ELSE SUBSTRING(
									tf.DATA,
									CHARINDEX('href=', tf.DATA) + 5,
									PATINDEX(
										'%[ >]%',
										SUBSTRING(
											tf.DATA,
											CHARINDEX('href=', tf.DATA) + 5,
											DATALENGTH(tf.DATA))) - 1)
						END,
						@meaning_key,
						'en',
						tf.FACT_VAGUE_DATE_START,
						tf.FACT_VAGUE_DATE_END,
						ISNULL(tf.FACT_VAGUE_DATE_TYPE, 'U'),
						@fact_type_concept_key,
						0,
						@ins_session_id,
						@upd_session_id,
						@system
			FROM		TAXON_FACT					AS	tf
			WHERE		tf.TAXON_FACT_KEY			=	@taxon_fact_key

			IF @@ERROR <> 0 GOTO fail_from_cursor

			/* record mapping */
			INSERT		Taxon_Dictionary_Thesaurus_Fact_Mapping (
						Taxon_Fact_Key,
						Thesaurus_Fact_Key)
			VALUES		(@taxon_fact_key,
						@thesaurus_fact_key)

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END

		/* make any changes required in Source_Join */
		IF @source_key IS NULL
		BEGIN
			UPDATE		Taxon_Dictionary_Thesaurus_Fact_Mapping
			SET			Source_Join_Key							=	NULL
			WHERE		Taxon_Fact_Key							=	@taxon_fact_key

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END

		EXECUTE		usp_SourceJoin_RecordImported	@source_join_key	OUTPUT,
													'Thesaurus_Fact',
													@thesaurus_fact_key,
													@source_key,
													@ins_session_id,
													@system
		IF @@ERROR <> 0 GOTO fail_from_cursor

		IF @source_key IS NOT NULL
		BEGIN
			UPDATE		Taxon_Dictionary_Thesaurus_Fact_Mapping
			SET			Source_Join_Key							=	@source_join_key
			WHERE		Taxon_Fact_Key							=	@taxon_fact_key

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END

		/* update progress counter */
		EXECUTE		usp_Import_Export_Job_RecordProcessed	@job_id
		IF @@ERROR <> 0 GOTO fail_from_cursor
		
		COMMIT TRANSACTION
	END

	CLOSE		facts
	DEALLOCATE	facts
	RETURN

fail_from_cursor:
	CLOSE		facts
	DEALLOCATE	facts

fail:
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_ThesaurusFact_ImportTaxonList failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ThesaurusFact_ImportTaxonList') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ThesaurusFact_ImportTaxonList'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ThesaurusFact_ImportTaxonList TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ThesaurusFact_ImportTaxonList TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ThesaurusFact_ImportTaxonList TO [Dev - JNCC SQL]
END
GO

