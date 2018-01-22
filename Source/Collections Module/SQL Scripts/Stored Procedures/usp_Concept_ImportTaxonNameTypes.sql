/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_Concept_ImportTaxonNameTypes]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_Concept_ImportTaxonNameTypes]
GO

/*===========================================================================*\
  Description:	Import concepts corresponding to the taxon name types used in
				the specified taxon list.

  Parameters:   @job_id					Job identifier

  Created:		Nov 2003

  Last revision information:
	$Revision: 12 $
	$Date: 5/08/11 15:17 $
	$Author: Simonlewis $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Concept_ImportTaxonNameTypes]
	@job_id					INT
AS
	SET NOCOUNT ON
	SET ANSI_NULLS ON

	DECLARE		@taxon_list_key				CHAR(16),
				@taxon_name_type_key		CHAR(16),
				@item_name					VARCHAR(100),
				@author_and_date			VARCHAR(100),
				@ins_user_key				CHAR(16),
				@ins_date					SMALLDATETIME,
				@ins_session_id				CHAR(16),
				@system						BIT,
				@thesaurus_name_type_key	CHAR(16),
				@system_mapping				BIT,
				@term_key					CHAR(16),
				@term_version_key			CHAR(16),
				@meaning_key				CHAR(16),
				@concept_history_key		CHAR(16)

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
													'Importing name types'
	IF @@ERROR <> 0 RETURN

	DECLARE     @versions   TABLE ( Taxon_Version_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS)

	INSERT      @versions
	SELECT      tli.TAXON_VERSION_KEY
	FROM		TAXON_LIST_VERSION				AS	tlv
	INNER JOIN	TAXON_LIST_ITEM					AS	tli
	ON			tli.TAXON_LIST_VERSION_KEY		=	tlv.TAXON_LIST_VERSION_KEY
	WHERE		tlv.TAXON_LIST_KEY				=	@taxon_list_key

	DECLARE		name_types	CURSOR LOCAL FAST_FORWARD FOR
	SELECT DISTINCT
				tnt.TAXON_NAME_TYPE_KEY,
				tnt.SHORT_NAME,
				tnt.AUTHORITY,
				tnt.ENTERED_BY,
				tnt.ENTRY_DATE,
				tnt.SYSTEM_SUPPLIED_DATA
	FROM        @versions                       AS  v0
	INNER JOIN	TAXON_VERSION					AS	tv
	ON			tv.TAXON_VERSION_KEY			=	v0.TAXON_VERSION_KEY
	INNER JOIN	TAXON							AS	tx
	ON			tx.TAXON_KEY					=	tv.TAXON_KEY
	INNER JOIN	TAXON_NAME_TYPE					AS	tnt
	ON			tnt.TAXON_NAME_TYPE_KEY			=	tx.TAXON_NAME_TYPE_KEY

	OPEN        name_types

	WHILE 1 = 1
	BEGIN
		FETCH		name_types
		INTO		@taxon_name_type_key,
					@item_name,
					@author_and_date,
					@ins_user_key,
					@ins_date,
					@system
					
		IF @@FETCH_STATUS <> 0 BREAK

		BEGIN TRANSACTION

		SELECT		@thesaurus_name_type_key			=   Thesaurus_Name_Type_Key,
					@system_mapping						=	System_Supplied_Data
		FROM		Taxon_Dictionary_Name_Type_Mapping
		WHERE       Taxon_Name_Type_Key					=	@taxon_name_type_key

		IF @@ROWCOUNT > 0
		BEGIN
			/* name type has previously been imported */
			IF @system_mapping = 0
			BEGIN
				SELECT		@term_key			=	Term_Key,
							@term_version_key	=	Term_Version_Key
				FROM		Concept
				WHERE		Concept_Key			=	@thesaurus_name_type_key

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
								AND			Concept_Group_Key	<>	'SYSTEM000000000M' )
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
									System_Supplied_Data)
						VALUES		(@term_key,
									'en',
									@item_name,
									@ins_session_id,
									@system)

						IF @@ERROR <> 0 GOTO fail_from_cursor

						EXECUTE		spNextKey		'Term_Version',
													@term_version_key	OUTPUT
						IF @@ERROR <> 0 GOTO fail_from_cursor

						INSERT		Term_Version (
									Term_Version_Key,
									Term_Key,
									Author_And_Date,
									Entered_Session_ID,
									System_Supplied_Data)
						VALUES		(@term_version_key,
									@term_key,
									@author_and_date,
									@ins_session_id,
									@system)

						IF @@ERROR <> 0 GOTO fail_from_cursor									

						UPDATE		Concept
						SET			Term_Key		=	@term_key
						WHERE		Concept_Key		=	@thesaurus_name_type_key

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

							UPDATE		Term_Version
							SET			Author_And_Date		=	@author_and_date
							WHERE		Term_Version_Key	=	@term_version_key

							IF @@ERROR <> 0 GOTO fail_from_cursor
						END
						ELSE
						BEGIN
							/* term cannot be updated; there is an existing
							 * term with the same name which we will link to
							 * instead */
							EXECUTE		spNextKey	'Term_Version',
													@term_version_key	OUTPUT
							IF @@ERROR <> 0 GOTO fail_from_cursor

							INSERT		Term_Version (
										Term_Version_Key,
										Term_Key,
										Author_And_Date,
										Entered_Session_ID,
										System_Supplied_Data)
							VALUES		(@term_version_key,
										@cur_term_key,
										@author_and_date,
										@ins_session_id,
										@system)

							IF @@error <> 0 GOTO fail_from_cursor

							UPDATE		Concept
							SET			Term_Key			=	@cur_term_key,
										Term_Version_Key	=	@term_version_key
							WHERE		Term_Key			=	@term_key

							IF @@ERROR <> 0 GOTO fail_from_cursor

							DELETE		Term
							WHERE		Term_Key			=	@term_key

							IF @@ERROR <> 0 GOTO fail_from_cursor
						END
					END
				END
			END
		END
		ELSE
		BEGIN
			/* obtain session identifier */
			EXECUTE		usp_Session_ForDate		@ins_user_key,
												@ins_date,
												@ins_session_id		OUTPUT
			IF @@ERROR <> 0 GOTO fail_from_cursor

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
							System_Supplied_Data)
				VALUES		(@term_key,
							'en',
							@item_name,
							@ins_session_id,
							@system)

				IF @@ERROR <> 0 GOTO fail_from_cursor
			END

			EXECUTE		spNextKey	'Term_Version',
									@term_version_key	OUTPUT
			IF @@ERROR <> 0 GOTO fail_from_cursor

			INSERT		Term_Version (
						Term_Version_Key,
						Term_Key,
						Author_And_Date,
						Entered_Session_ID,
						System_Supplied_Data)
			VALUES		(@term_version_key,
						@term_key,
						@author_and_date,
						@ins_session_id,
						@system)

			IF @@ERROR <> 0 GOTO fail_from_cursor

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
									@thesaurus_name_type_key	OUTPUT
			IF @@ERROR <> 0 GOTO fail_from_cursor

			INSERT		Concept (
						Concept_Key,
						Term_Key,
						Concept_Group_Key,
						Term_Version_Key,
						List_Preferred,
						Is_Current,
						Preferred,
						Name_Type_Concept_Key,
						Meaning_Key,
						Entered_Session_ID,
						System_Supplied_Data,
						Published_Term,
						Automatic_Published_Term)
			VALUES 		(@thesaurus_name_type_key,
						@term_key,
						'SYSTEM000000000M', /* "Thesaurus Name Types" group */
						@term_version_key,
						1,
						1,
						1,
						'SYSTEM0000000000', /* "Formal" -- meaningless, but
												we need a value here */
						@meaning_key,
						@ins_session_id,
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
						System_Supplied_Data)
			VALUES		(@concept_history_key,
						@thesaurus_name_type_key,
						'SYSTEM000000000M', /* "Thesaurus Name Types" version */
						@ins_session_id,
						@system)

			IF @@ERROR <> 0 GOTO fail_from_cursor

			/* record taxon name type mapping */
			INSERT		Taxon_Dictionary_Name_Type_Mapping (
						Taxon_Name_Type_Key,
						Thesaurus_Name_Type_Key,
						System_Supplied_Data)
			VALUES		(@taxon_name_type_key,
						@thesaurus_name_type_key,
						0)

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END

		EXECUTE		usp_Import_Export_Job_RecordProcessed	@job_id
		IF @@ERROR <> 0 GOTO fail_from_cursor

		COMMIT TRANSACTION
	END

	CLOSE		name_types
	DEALLOCATE	name_types
	RETURN

fail_from_cursor:
	CLOSE		name_types
	DEALLOCATE	name_types

fail:
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_Concept_ImportTaxonNameTypes failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_ImportTaxonNameTypes') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Concept_ImportTaxonNameTypes'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concept_ImportTaxonNameTypes TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concept_ImportTaxonNameTypes TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Concept_ImportTaxonNameTypes TO [Dev - JNCC SQL]
END
GO