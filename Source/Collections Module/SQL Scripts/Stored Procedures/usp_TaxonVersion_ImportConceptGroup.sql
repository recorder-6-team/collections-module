/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_TaxonVersion_ImportConceptGroup]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_TaxonVersion_ImportConceptGroup]
GO

/*===========================================================================*\
  Description:	Import taxon versions corresponding to items in a concept
				group.

  Parameters:	@job_id					Job identifier

  Created:		Dec 2003

  Last revision information:
	$Revision: 10 $
	$Date: 17/11/11 16:31 $
	$Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_TaxonVersion_ImportConceptGroup]
	@job_id					INT,
	@SessionID			CHAR(16)
AS
	SET NOCOUNT ON

 DECLARE     @concept_group_key			CHAR(16),
				@term_version_key			CHAR(16),
				@taxon_key					CHAR(16),
				@attribute					VARCHAR(65),
				@authority					VARCHAR(40),
				@entered_by					CHAR(16),
				@entry_date					SMALLDATETIME,
				@changed_by					CHAR(16),
				@changed_date				SMALLDATETIME,
				@system						BIT,
				@taxon_version_key			CHAR(16),
				@source_key					CHAR(16),
				@source_join_key			CHAR(16),
				@prior_term_version_key		CHAR(16),
				@concept_key	CHAR(16),
				@term_key		CHAR(16)


	/* determine parameters of job */
	SELECT      @concept_group_key			=	j.Concept_Group_Key
	FROM		Import_Export_Job			AS	j
	WHERE		j.Import_Export_Job_ID		=	@job_id

	IF @@ROWCOUNT = 0
	BEGIN
		RAISERROR ('Job does not exist or has not been configured', 16, 1)
		GOTO fail
	END

	EXECUTE		usp_Import_Export_Job_UpdateStatus	@job_id,
													'Exporting term versions'
	IF @@ERROR <> 0 GOTO fail
 

	DECLARE		versions	CURSOR LOCAL FAST_FORWARD FOR
	SELECT DISTINCT
				tv.Term_Version_Key,
				tm.Taxon_Key,
				tv.Version_Label,
				tv.Author_And_Date,
				es.User_Name_Key,
				CONVERT(SMALLDATETIME,
						CONVERT(VARCHAR, es.Date_Time_Start, 112)),
				cs.User_Name_Key,
				CONVERT(SMALLDATETIME,
						CONVERT(VARCHAR, cs.Date_Time_Start, 112)),
				ISNULL(tv.System_Supplied_Data, c.System_Supplied_Data),
				c.Concept_Key,
				tm.Term_key
	FROM		Concept							AS	c
	LEFT JOIN	Term_Version					AS	tv
	ON			tv.Term_Version_Key				=	c.Term_Version_Key
	INNER JOIN	Taxon_Dictionary_Term_Mapping	AS	tm
	ON			tm.Term_Key						=	c.Term_Key
	INNER JOIN	Session							AS	es
	ON			es.Session_ID					=	 ISNULL(tv.Entered_Session_ID, @SessionID)
	LEFT JOIN	Session							AS	cs
	ON			cs.Session_ID					=	tv.Changed_Session_ID
	WHERE		c.Concept_Group_Key				=	@concept_group_key
	ORDER BY	tv.Term_Version_Key

	SET			@prior_term_version_key			=	''

	OPEN		versions

	WHILE 1 = 1
	BEGIN
		FETCH		versions
		INTO		@term_version_key,
					@taxon_key,
					@attribute,
					@authority,
					@entered_by,
					@entry_date,
					@changed_by,
					@changed_date,
					@system,
					@concept_key,
					@term_key

		IF @@FETCH_STATUS <> 0 BREAK

		BEGIN TRANSACTION

		SELECT      @taxon_version_key						=	NULL,
					@source_join_key						=	NULL,
					@source_key								=	NULL

		SELECT		@taxon_version_key						=	m.Taxon_Version_Key,
					@source_key								=	j.Source_Key
		FROM		Taxon_Dictionary_Term_Version_Mapping	AS	m
		LEFT JOIN	Source_Join								AS	j
		ON			j.Source_Join_Key						=	m.Source_Join_Key
		WHERE		m.Term_Version_Key						=	@term_version_key

		IF @source_key IS NULL
		BEGIN
			/* there is no existing mapping for the source join; pick an
			 * arbitrary join record (if there are any) and make this the
			 * mapped join.
			 */
			SELECT		@source_join_key	=	Source_Join_Key,
						@source_key			=	Source_Key
			FROM		Source_Join
			WHERE		Record_Key			=	@term_version_key
			AND			Table_Name			=	'Term_Version'
			ORDER BY	Source_Join_Key
		END

		IF @taxon_version_key IS NOT NULL
		BEGIN
			/* update taxon version */
			UPDATE		TAXON_VERSION
			SET			TAXON_KEY				=	@taxon_key,
						ATTRIBUTE				=	@attribute,
						AUTHORITY				=	@authority,
						SOURCE_KEY				=	@source_key,
						ENTERED_BY				=	@entered_by,
						ENTRY_DATE				=	@entry_date,
						CHANGED_BY				=	@changed_by,
						CHANGED_DATE			=	@changed_date,
						SYSTEM_SUPPLIED_DATA   	=	@system
			WHERE		TAXON_VERSION_KEY		=	@taxon_version_key

			IF @@ERROR <> 0 GOTO fail_from_cursor

			IF @source_join_key IS NOT NULL
			BEGIN
				UPDATE		Taxon_Dictionary_Term_Version_Mapping
				SET			Source_Join_Key							=	@source_join_key
				WHERE		Taxon_Version_Key						=	@taxon_version_key

				IF @@ERROR <> 0 GOTO fail_from_cursor
			END
		END
		ELSE
		BEGIN
			/* create taxon version */
			EXECUTE		spNextKey		'TAXON_VERSION',
										@taxon_version_key	OUTPUT
			IF @@ERROR <> 0 GOTO fail_from_cursor

			INSERT		TAXON_VERSION (
						TAXON_VERSION_KEY,
						TAXON_KEY,
						ATTRIBUTE,
						AUTHORITY,
						UK_NATIVE,
						SOURCE_KEY,
						ENTERED_BY,
						ENTRY_DATE,
						CHANGED_BY,
						CHANGED_DATE,
						SYSTEM_SUPPLIED_DATA)
			VALUES		(@taxon_version_key,
						@taxon_key,
						@attribute,
						@authority,
						0,
						@source_key,
						@entered_by,
						@entry_date,
						@changed_by,
						@changed_date,
						@system)

			IF @@ERROR <> 0 GOTO fail_from_cursor

			/* record mapping */
			INSERT		Taxon_Dictionary_Term_Version_Mapping (
						Taxon_Version_Key,
						Term_Version_Key,
						Source_Join_Key)
			VALUES		(@taxon_version_key,
						@term_version_key,
						@source_join_key)

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END

		IF @term_version_key <> @prior_term_version_key
		BEGIN
			/* Use of @prior_term_version_key is a hack for the case where
			 * a single Term corresponds to multiple Taxon records; we don't
			 * increment the progress count until all the taxa have been
			 * considered.
			 */
			SET			@prior_term_version_key		=	@term_version_key

		END

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
	RAISERROR ('usp_TaxonVersion_ImportConceptGroup failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TaxonVersion_ImportConceptGroup') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_TaxonVersion_ImportConceptGroup'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_TaxonVersion_ImportConceptGroup TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_TaxonVersion_ImportConceptGroup TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_TaxonVersion_ImportConceptGroup TO [Dev - JNCC SQL]
END
GO