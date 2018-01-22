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
	$Revision: 9 $
	$Date: 5/08/11 15:46 $
	$Author: Simonlewis $

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
							AND			Plaintext		=	dbo.ufn_RemoveHtmlMarkup(@item_name) )
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
								Plaintext,
								Entered_Session_ID,
								Changed_Session_ID,
								System_Supplied_Data)
					VALUES		(@term_key,
								'en',
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
					AND			Plaintext		=	dbo.ufn_RemoveHtmlMarkup(@item_name)

					IF @@ROWCOUNT = 0
					BEGIN
						/* term can simply be updated */
						UPDATE		Term
						SET			Language_Key	=	'en',
									Plaintext		=	dbo.ufn_RemoveHtmlMarkup(@item_name)
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
			AND			Plaintext		=	dbo.ufn_RemoveHtmlMarkup(@item_name)

			IF @@ROWCOUNT = 0
			BEGIN
				EXECUTE		spNextKey	'Term',
										@term_key	OUTPUT
				IF @@ERROR <> 0 GOTO fail_from_cursor

				INSERT		Term (
							Term_Key,
							Language_Key,
							Plaintext,
							Entered_Session_ID,
							Changed_Session_ID,
							System_Supplied_Data)
				VALUES		(@term_key,
							'en',
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
						System_Supplied_Data,
						Published_Term,
						Automatic_Published_Term)
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
						@system,
						@item_name,
						1)

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