/*===========================================================================*\
  Description:	Changes made relating to CCN 141

  Created:		Apr 2011

  Last revision information:
	$ $
	$ $
	$ $

\*===========================================================================*/


/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'dbo.usp_TaxonFact_ImportConceptGroup')
	   AND    Type = 'P')
	DROP PROCEDURE dbo.usp_TaxonFact_ImportConceptGroup
GO

/*===========================================================================*\
  Description:	Import taxon facts corresponding to the facts associated with
				a concept group.

  Parameters:	@job_id					Job identifier

  Created:		Jan 2004

  Last revision information:
	$Revision: 1 $
	$Date: 9/05/11 11:33 $
	$Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE dbo.usp_TaxonFact_ImportConceptGroup
	@job_id				INT
AS
	SET NOCOUNT ON

	DECLARE		@concept_group_key		CHAR(16),
				@thesaurus_fact_key		CHAR(16),
				@taxon_fact_key			CHAR(16),
				@type					VARCHAR(1),
				@taxon_version_key		CHAR(16),
				@entered_by				CHAR(16),
				@entry_date				SMALLDATETIME,
				@changed_by				CHAR(16),
				@changed_date			SMALLDATETIME,
				@source_key				CHAR(16),
				@source_join_key		CHAR(16)

	/* determine parameters of job */
	SELECT		@concept_group_key			=	j.Concept_Group_Key
	FROM		Import_Export_Job			AS	j
	WHERE		j.Import_Export_Job_ID		=	@job_id

	IF @@ROWCOUNT = 0
	BEGIN
		RAISERROR ('Job does not exist or has not been configured', 16, 1)
		GOTO fail
	END

	EXECUTE		usp_Import_Export_Job_UpdateStatus	@job_id,
													'Exporting facts'
	IF @@ERROR <> 0 GOTO fail

	DECLARE		facts	CURSOR LOCAL FAST_FORWARD FOR
	SELECT DISTINCT
				f.Thesaurus_Fact_Key,
				CASE f.Fact_Type_Concept_Key
					WHEN 'SYSTEM00000002NO' THEN 'T' /* HTML */
					WHEN 'SYSTEM00000002L9' THEN 'A' /* AVI */
					WHEN 'SYSTEM00000002L8' THEN 'W' /* WAV */
					WHEN 'SYSTEM00000000W0' THEN 'B' /* Bitmap */
					WHEN 'SYSTEM00000000VY' THEN 'J' /* JPEG */
					ELSE 'T' /* Unknown types mapped to HTML */
				END,
				vm.Taxon_Version_Key,
				es.User_Name_Key,
				CONVERT(SMALLDATETIME,
						CONVERT(VARCHAR, es.Date_Time_Start, 112)),
				cs.User_Name_Key,
				CONVERT(SMALLDATETIME,
						CONVERT(VARCHAR, cs.Date_Time_Start, 112))
	FROM		Concept									AS	c
	INNER JOIN	Thesaurus_Fact							AS	f
	ON			f.Meaning_Key							=	c.Meaning_Key
	INNER JOIN	Taxon_Dictionary_Term_Version_Mapping	AS	vm
	ON			vm.Term_Version_Key						=	c.Term_Version_Key
	INNER JOIN	Session									AS	es
	ON			es.Session_ID							=	f.Entered_Session_ID
	LEFT JOIN	Session									AS	cs
	ON			cs.Session_ID							=	f.Changed_Session_ID
	WHERE		c.Concept_Group_Key						=	@concept_group_key
	AND		c.List_Preferred = 1

	OPEN		facts

	WHILE 1 = 1
	BEGIN
		FETCH		facts
		INTO        @thesaurus_fact_key,
					@type,
					@taxon_version_key,
					@entered_by,
					@entry_date,
					@changed_by,
					@changed_date

		IF @@FETCH_STATUS <> 0 BREAK

		BEGIN TRANSACTION

		SELECT		@taxon_fact_key							=	NULL,
					@source_join_key						=	NULL,
					@source_key								=	NULL

		SELECT		@taxon_fact_key							=	m.Taxon_Fact_Key,
					@source_key								=	j.Source_Key
		FROM		Taxon_Dictionary_Thesaurus_Fact_Mapping	AS	m
		LEFT JOIN	Source_Join								AS	j
		ON			j.Source_Join_Key						=	m.Source_Join_Key
		WHERE		m.Thesaurus_Fact_Key					=	@thesaurus_fact_key

		IF @source_key IS NULL
		BEGIN
			/* there is no existing mapping for the source join; pick an
			 * arbitrary join record (if there are any) and make this the
			 * mapped join.
			 */
			SELECT		@source_join_key	=	Source_Join_Key,
						@source_key			=	Source_Key
			FROM		Source_Join
			WHERE		Record_Key			=	@thesaurus_fact_key
			AND			Table_Name			=	'Thesaurus_Fact'
			ORDER BY	Source_Join_Key
		END

		IF @taxon_fact_key IS NOT NULL
		BEGIN
			/* update existing taxon fact */
			UPDATE		TAXON_FACT
			SET			TITLE						=	CAST(tf.Item_Name as VARCHAR(50)),
						TYPE						=	CASE WHEN TYPE = 'S' AND @type = 'T'
															THEN 'S'
															ELSE @type
														END,
						DATA						=	tf.Data,
						TAXON_VERSION_KEY			=	@taxon_version_key,
						FACT_VAGUE_DATE_START		=	tf.Fact_Vague_Date_Start,
						FACT_VAGUE_DATE_END			=	tf.Fact_Vague_Date_End,
						FACT_VAGUE_DATE_TYPE		=	tf.Fact_Vague_Date_Type,
						ENTERED_BY					=	@entered_by,
						ENTRY_DATE					=	@entry_date,
						CHANGED_BY					=	@changed_by,
						CHANGED_DATE				=	@changed_date,
						SYSTEM_SUPPLIED_DATA		=	tf.System_Supplied_Data,
						SOURCE_KEY					=	@source_key
			FROM		Thesaurus_Fact				AS	tf,
						TAXON_FACT
			WHERE       tf.Thesaurus_Fact_Key		=	@thesaurus_fact_key
			AND			TAXON_FACT_KEY				=	@taxon_fact_key

			IF @@ERROR <> 0 GOTO fail_from_cursor

			IF @source_join_key IS NOT NULL
			BEGIN
				UPDATE		Taxon_Dictionary_Thesaurus_Fact_Mapping
				SET			Source_Join_Key							=	@source_join_key
				WHERE		Taxon_Fact_Key							=	@taxon_fact_key

				IF @@ERROR <> 0 GOTO fail_from_cursor
			END
		END
		ELSE
		BEGIN
			/* create taxon fact */
			EXECUTE		spNextKey	'TAXON_FACT',
									@taxon_fact_key		OUTPUT
			IF @@ERROR <> 0 GOTO fail_from_cursor

			INSERT		TAXON_FACT (
						TAXON_FACT_KEY,
						TITLE,
						TYPE,
						DATA,
						TAXON_VERSION_KEY,
						FACT_VAGUE_DATE_START,
						FACT_VAGUE_DATE_END,
						FACT_VAGUE_DATE_TYPE,
						ENTERED_BY,
						ENTRY_DATE,
						CHANGED_BY,
						CHANGED_DATE,
						SYSTEM_SUPPLIED_DATA,
						SOURCE_KEY)
			SELECT		@taxon_fact_key,
						CAST(tf.Item_Name as VARCHAR(50)),
						@type,
						tf.Data,
						@taxon_version_key,
						tf.Fact_Vague_Date_Start,
						tf.Fact_Vague_Date_End,
						tf.Fact_Vague_Date_Type,
						@entered_by,
						@entry_date,
						@changed_by,
						@changed_date,
						tf.System_Supplied_Data,
						@source_key
			FROM        Thesaurus_Fact				AS	tf
			WHERE		tf.Thesaurus_Fact_Key		=	@thesaurus_fact_key

			IF @@ERROR <> 0 GOTO fail_from_cursor

			/* record mapping */
			INSERT		Taxon_Dictionary_Thesaurus_Fact_Mapping (
						Taxon_Fact_Key,
						Thesaurus_Fact_Key,
						Source_Join_Key)
			VALUES		(@taxon_fact_key,
						@thesaurus_fact_key,
						@source_join_key)

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END

		IF @type <> 'T'
		BEGIN
			/* convert "Data" to an HTML anchor; this couldn't be done on the
			 * INSERT/UPDATE above because the '+' operator is not defined
			 * for the "text" data type.
			 */
			DECLARE		@data_ptr		BINARY(16),
						@data_length	INT

			SELECT		@data_ptr		=	TEXTPTR(DATA),
						@data_length	=	DATALENGTH(DATA)
			FROM		TAXON_FACT
			WHERE		TAXON_FACT_KEY	=	@taxon_fact_key

			UPDATETEXT	TAXON_FACT.DATA @data_ptr @data_length 0 '"></a>'
			IF @@ERROR <> 0 GOTO fail_from_cursor

			UPDATETEXT	TAXON_FACT.DATA @data_ptr 0 0 '<a href="'
			IF @@ERROR <> 0 GOTO fail_from_cursor
		END

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
	RAISERROR ('usp_TaxonFact_ImportConceptGroup failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TaxonFact_ImportConceptGroup') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_TaxonFact_ImportConceptGroup'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_TaxonFact_ImportConceptGroup TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_TaxonFact_ImportConceptGroup TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_TaxonFact_ImportConceptGroup TO [Dev - JNCC SQL]
END
GO