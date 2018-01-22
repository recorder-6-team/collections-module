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
	$Revision: 16 $
	$Date: 5/08/11 16:52 $
	$Author: Simonlewis $

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

		SET			@item_name		=	@plaintext

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
							AND			Plaintext				=	@plaintext )
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
					AND			Plaintext		=	@plaintext
					AND			Term_Key		<>	@term_key

					IF @@ROWCOUNT = 0
					BEGIN
						/* update the current term */
						UPDATE		Term
						SET         Language_Key		=	@language,
									Plaintext			=	@plaintext,
									Changed_Session_ID	=	@upd_session_id
						WHERE		Term_Key			=	@term_key

						IF @@ERROR <> 0 GOTO fail_from_cursor
					END
					ELSE
					BEGIN
						-- Create a placeholder term version for the concept
						-- whose term is to be deleted...
						
						DECLARE @TermVersionKey INT
						
						EXECUTE spNextKey 'Term_Version', @TermVersionKey OUTPUT
						
						INSERT INTO	dbo.Term_Version
									(
										Term_Version_Key,
										Term_Key,
										Version_Label,
										Author_And_Date,
										Entered_Session_ID,
										Changed_Session_ID,
										Custodian
									)
						VALUES		(
										@TermVersionKey,
										@term_key,
										NULL,
										NULL,
										@ins_session_id,
										@upd_session_id,
										NULL
									)
						
						/* remove current term */
						UPDATE		Concept
						SET			Term_Key			=	@new_term_key,
									Term_Version_Key	=	@TermVersionKey
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
			AND			Plaintext		=	@plaintext

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
							Plaintext,
							Entered_Session_ID,
							Changed_Session_ID,
							System_Supplied_Data)
				VALUES		(@term_key,
							@language,
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