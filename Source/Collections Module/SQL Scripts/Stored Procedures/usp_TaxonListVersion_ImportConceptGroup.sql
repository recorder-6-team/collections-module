/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_TaxonListVersion_ImportConceptGroup]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_TaxonListVersion_ImportConceptGroup]
GO

/*===========================================================================*\
  Description:	Import taxon list versions corresponding to the versions
				of a concept group.

  Parameters:	@job_id					Job identifier

  Created:		Dec 2003

  Last revision information:
	$Revision: 7 $
	$Date: 17/08/11 10:41 $
	$Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_TaxonListVersion_ImportConceptGroup]
	@job_id					INT
AS
	SET NOCOUNT ON

	DECLARE		@concept_group_key				CHAR(16),
				@taxon_list_key					CHAR(16),
				@concept_group_version_key		CHAR(16),
				@version						INT,
				@authority						VARCHAR(50),
				@vague_date_start				INT,
				@vague_date_end					INT,
				@vague_date_type				VARCHAR(2),
				@entered_by						CHAR(16),
				@entry_date						SMALLDATETIME,
				@changed_by						CHAR(16),
				@changed_date					SMALLDATETIME,
				@system							BIT,
				@taxon_list_version_key			CHAR(16),
				@source_key						CHAR(16),
				@source_join_key				CHAR(16)

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
													'Exporting concept group versions'
	IF @@ERROR <> 0 GOTO fail

	DECLARE		versions	CURSOR LOCAL FAST_FORWARD FOR
	SELECT		cgv.Concept_Group_Version_Key,
				cgv.Sequence,
				CAST(cg.Authority AS VARCHAR(50)),
				cgv.From_Vague_Date_Start,
				cgv.From_Vague_Date_End,
				cgv.From_Vague_Date_Type,
				es.User_Name_Key,
				CONVERT(SMALLDATETIME,
						CONVERT(VARCHAR, es.Date_Time_Start, 112)),
				cs.User_Name_Key,
				CONVERT(SMALLDATETIME,
						CONVERT(VARCHAR, cs.Date_Time_End, 112)),
				cgv.System_Supplied_Data
	FROM		Concept_Group_Version		AS	cgv
	INNER JOIN	Concept_Group				AS	cg
	ON			cgv.Concept_Group_Key		=	cg.Concept_Group_Key
	INNER JOIN	Session						AS	es
	ON			es.Session_ID				=	cgv.Entered_Session_ID
	LEFT JOIN	Session						AS	cs
	ON			cs.Session_ID				=	cgv.Changed_Session_ID
	WHERE		cgv.Concept_Group_Key		=	@concept_group_key

	OPEN		versions

	WHILE 1 = 1
	BEGIN
		FETCH		versions
		INTO		@concept_group_version_key,
					@version,
					@authority,
					@vague_date_start,
					@vague_date_end,
					@vague_date_type,
					@entered_by,
					@entry_date,
					@changed_by,
					@changed_date,
					@system

		IF @@FETCH_STATUS <> 0 BREAK

		BEGIN TRANSACTION

		SELECT      @taxon_list_version_key							=	NULL,
					@source_join_key					   			=	NULL,
					@source_key										=	NULL

		SELECT		@taxon_list_version_key							=	m.Taxon_List_Version_Key,
					@source_key										=	j.Source_Key
		FROM		Taxon_Dictionary_Concept_Group_Version_Mapping	AS	m
		LEFT JOIN	Source_Join										AS	j
		ON			j.Source_Join_Key								=	m.Source_Join_Key
		WHERE		m.Concept_Group_Version_Key						=	@concept_group_version_key

		IF @source_key IS NULL
		BEGIN
			/* there is no existing mapping for the source join; pick an
			 * arbitrary join record (if there are any) and make this the
			 * mapped join.
			 */
			SELECT		@source_join_key	=	Source_Join_Key,
						@source_key			=	Source_Key
			FROM		Source_Join
			WHERE		Record_Key			=	@concept_group_version_key
			AND			Table_Name			=	'Concept_Group_Version'
			ORDER BY	Source_Join_Key
		END

		IF @taxon_list_version_key IS NOT NULL
		BEGIN
			/* update taxon list version */
			UPDATE		TAXON_LIST_VERSION
			SET			VERSION					=	@version,
						AUTHORITY				=	@authority,
						VAGUE_DATE_START		=	@vague_date_start,
						VAGUE_DATE_END			=	@vague_date_end,
						VAGUE_DATE_TYPE			=	@vague_date_type,
						SOURCE_KEY				=	@source_key,
						ENTERED_BY				=	@entered_by,
						ENTRY_DATE				=	@entry_date,
						CHANGED_BY				=	@changed_by,
						CHANGED_DATE			=	@changed_date,
						SYSTEM_SUPPLIED_DATA	=	@system
			WHERE		TAXON_LIST_VERSION_KEY	=	@taxon_list_version_key

			IF @@ERROR <> 0 GOTO fail_from_cursor

			IF @source_join_key IS NOT NULL
			BEGIN
				UPDATE		Taxon_Dictionary_Concept_Group_Version_Mapping
				SET			Source_Join_Key									=	@source_join_key
				WHERE		Taxon_List_Version_Key							=	@taxon_list_version_key

				IF @@ERROR <> 0 GOTO fail_from_cursor
			END
		END
		ELSE
		BEGIN
			/* create taxon list version */
			EXECUTE		spNextKey	'TAXON_LIST_VERSION',
									@taxon_list_version_key		OUTPUT
			IF @@ERROR <> 0 GOTO fail_from_cursor

			INSERT		TAXON_LIST_VERSION (
						TAXON_LIST_VERSION_KEY,
						TAXON_LIST_KEY,
						VERSION,
						AUTHORITY,
						VAGUE_DATE_START,
						VAGUE_DATE_END,
						VAGUE_DATE_TYPE,
						SOURCE_KEY,
						ENTERED_BY,
						ENTRY_DATE,
						CHANGED_BY,
						CHANGED_DATE,
						SYSTEM_SUPPLIED_DATA)
			VALUES		(@taxon_list_version_key,
						@taxon_list_key,
						@version,
						@authority,
						@vague_date_start,
						@vague_date_end,
						@vague_date_type,
						@source_key,
						@entered_by,
						@entry_date,
						@changed_by,
						@changed_date,
						@system)

			IF @@ERROR <> 0 GOTO fail_from_cursor

			/* record mapping */
			INSERT		Taxon_Dictionary_Concept_Group_Version_Mapping (
						Taxon_List_Version_Key,
						Concept_Group_Version_Key,
						Source_Join_Key)
			VALUES		(@taxon_list_version_key,
						@concept_group_version_key,
						@source_join_key)

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END

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
	RAISERROR ('usp_TaxonListVersion_ImportConceptGroup failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TaxonListVersion_ImportConceptGroup') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_TaxonListVersion_ImportConceptGroup'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_TaxonListVersion_ImportConceptGroup TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_TaxonListVersion_ImportConceptGroup TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_TaxonListVersion_ImportConceptGroup TO [Dev - JNCC SQL]
END
GO