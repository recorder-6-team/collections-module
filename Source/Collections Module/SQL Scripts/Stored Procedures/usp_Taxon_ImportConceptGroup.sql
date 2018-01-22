/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_Taxon_ImportConceptGroup]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_Taxon_ImportConceptGroup]
GO

/*===========================================================================*\
  Description:	Import taxa corresponding to terms in a concept group.

  Parameters:	@job_id					Job identifier

  Created:		Dec 2003

  Last revision information:
	$Revision: 9 $
	$Date: 17/11/11 17:09 $
	$Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Taxon_ImportConceptGroup]
	@job_id				CHAR(16)
AS
	SET NOCOUNT ON

	DECLARE     @concept_group_key		CHAR(16),
				@term_key				CHAR(16),
				@italic					BIT,
				@item_name				VARCHAR(60),
				@authority				VARCHAR(65),
				@language				VARCHAR(2),
				@taxon_name_type_key	CHAR(16),
				@entered_by				CHAR(16),
				@entry_date				SMALLDATETIME,
				@changed_by				CHAR(16),
				@changed_date			SMALLDATETIME,
				@system					BIT,
				@Taxon_Key				CHAR(16)

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
													'Exporting terms'
	IF @@ERROR <> 0 GOTO fail

	DECLARE		terms	CURSOR LOCAL FAST_FORWARD FOR
	SELECT DISTINCT
				t.Term_Key,
				CASE WHEN PATINDEX('%<i>%', C.Published_Term) <> 0
					THEN 1
					ELSE 0
				END,
				REPLACE(REPLACE(c.Published_Term, '<i>', ''), '</i>', '') COLLATE SQL_Latin1_General_CP1_CI_AS,
				tv.Author_And_Date,
				t.Language_Key,
				tnt.Taxon_Name_Type_Key,
				ISNULL(es.User_Name_Key, 'NBNSYS0000000004') AS User_Name_Key,
				ISNULL(CONVERT(SMALLDATETIME,
						CONVERT(VARCHAR, es.Date_Time_Start, 112)), GetDate()),
				cs.User_Name_Key,
				CONVERT(SMALLDATETIME,
						CONVERT(VARCHAR, cs.Date_Time_Start, 112)),
				t.System_Supplied_Data
	FROM		Concept								AS	c
	INNER JOIN	Term								AS	t
	ON			t.Term_Key							=	c.Term_Key
	LEFT JOIN	Term_Version						AS	tv
	ON			tv.Term_Version_Key					=	c.Term_Version_Key
	INNER JOIN	Taxon_Dictionary_Name_Type_Mapping	AS	tnt
	ON			tnt.Thesaurus_Name_Type_Key			=	c.Name_Type_Concept_Key
	LEFT JOIN	Session								AS	es
	ON			es.Session_ID						=	t.Entered_Session_ID
	LEFT JOIN	Session								AS	cs
	ON			cs.Session_ID						=	t.Changed_Session_ID
	WHERE		c.Concept_Group_Key					=	@concept_group_key

	OPEN		terms

	WHILE 1 = 1
	BEGIN
		FETCH		terms
		INTO		@term_key,
					@italic,
					@item_name,
					@authority,
					@language,
					@taxon_name_type_key,
					@entered_by,
					@entry_date,
					@changed_by,
					@changed_date,
					@system

		IF @@FETCH_STATUS <> 0 BREAK

		BEGIN TRANSACTION

		SELECT		@taxon_key						=	tdm.Taxon_Key
		FROM		Taxon_Dictionary_Term_Mapping	AS	tdm
		INNER JOIN	TAXON							AS	tx
		ON			tx.TAXON_KEY					=	tdm.Taxon_Key
		WHERE		tdm.Term_Key					=	@term_key
		AND			tx.TAXON_NAME_TYPE_KEY			=	@taxon_name_type_key

		IF @@ROWCOUNT > 0
		BEGIN
			/* update taxon */
			UPDATE		TAXON
			SET			ITEM_NAME					=	@item_name,
						AUTHORITY					=	@authority,
						LANGUAGE					=	@language,
						ENTERED_BY					=	@entered_by,
						ENTRY_DATE					=	@entry_date,
						CHANGED_BY					=	@changed_by,
						CHANGED_DATE				=	@changed_date,
						SYSTEM_SUPPLIED_DATA		=	@system
			WHERE		TAXON_KEY					=	@taxon_key

			UPDATE		Taxon_Dictionary_Term_Mapping
			SET			Italic_Font = @italic
			WHERE		Taxon_Key = @taxon_key AND Term_Key = @term_key

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END
		ELSE
		BEGIN
			/* create new taxon */
			EXECUTE		spNextKey	'TAXON',
									@taxon_key	OUTPUT
			IF @@ERROR <> 0 GOTO fail_from_cursor

			INSERT		TAXON (
						TAXON_KEY,
						ITEM_NAME,
						AUTHORITY,
						LANGUAGE,
						TAXON_NAME_TYPE_KEY,
						ENTERED_BY,
						ENTRY_DATE,
						CHANGED_BY,
						CHANGED_DATE,
						SYSTEM_SUPPLIED_DATA)
			VALUES		(@taxon_key,
						@item_name,
						@authority,
						@language,
						@taxon_name_type_key,
						@entered_by,
						@entry_date,
						@changed_by,
						@changed_date,
						@system)

			IF @@ERROR <> 0 GOTO fail_from_cursor

			/* record mapping */
			INSERT		Taxon_Dictionary_Term_Mapping (
						Taxon_Key,
						Italic_Font,
						Term_Key)
			VALUES		(@taxon_key,
						@italic,
						@term_key)

			IF @@ERROR <> 0 GOTO fail_from_cursor
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
	RAISERROR ('usp_Taxon_ImportConceptGroup failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Taxon_ImportConceptGroup') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Taxon_ImportConceptGroup'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Taxon_ImportConceptGroup TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Taxon_ImportConceptGroup TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Taxon_ImportConceptGroup TO [Dev - JNCC SQL]
END
GO