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
	$Revision: 10 $
	$Date: 18/11/05 16:13 $
	$Author: Johnvanbreda $

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
					WHEN 'A' THEN 'SYSTEM00000002L9' /* AVI */
					WHEN 'W' THEN 'SYSTEM00000002L8' /* WAV */
					WHEN 'B' THEN 'SYSTEM00000000W0' /* Bitmap */
					WHEN 'J' THEN 'SYSTEM00000000VY' /* JPEG */
					ELSE 'SYSTEM00000002NO' /* HTML */
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
									WHEN @type NOT IN ('A', 'W', 'B', 'J') THEN tf.DATA
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
							WHEN @type NOT IN ('A', 'W', 'B', 'J') THEN tf.DATA
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
