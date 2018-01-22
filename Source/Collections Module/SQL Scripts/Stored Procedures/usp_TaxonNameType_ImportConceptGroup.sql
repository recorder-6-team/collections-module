/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_TaxonNameType_ImportConceptGroup]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_TaxonNameType_ImportConceptGroup]
GO

/*===========================================================================*\
  Description:	Import taxon name types corresponding to the name type
				concepts used by the concepts in the specified concept group.

  Parameters:	@job_id					Job identifier

  Created:		Dec 2003

  Last revision information:
	$Revision: 4 $
	$Date: 12/05/04 9:57 $
	$Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_TaxonNameType_ImportConceptGroup]
	@job_id					INT
AS
	SET NOCOUNT ON

	DECLARE     @concept_group_key		CHAR(16),
				@name_type_concept_key	CHAR(16),
				@short_name				VARCHAR(20),
				@authority				VARCHAR(50),
				@entered_by				CHAR(16),
				@entry_date				SMALLDATETIME,
				@system					BIT,
				@taxon_name_type_key	CHAR(16),
				@system_mapping			BIT

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
													'Exporting thesaurus name types'
	IF @@ERROR <> 0 GOTO fail

	DECLARE		name_types	CURSOR LOCAL FAST_FORWARD FOR
	SELECT DISTINCT
				nt.Concept_Key,
				t.Plaintext,
				tv.Author_And_Date,
				es.User_Name_Key,
				CONVERT(SMALLDATETIME,
						CONVERT(VARCHAR, es.Date_Time_Start, 112)),
				nt.System_Supplied_Data
	FROM		Concept						AS	c
	INNER JOIN	Concept						AS	nt
	ON			nt.Concept_Key				=	c.Name_Type_Concept_Key
	INNER JOIN	Term						AS	t
	ON			t.Term_Key					=	nt.Term_Key
	LEFT JOIN	Term_Version				AS	tv
	ON			tv.Term_Version_Key			=	nt.Term_Version_Key
	INNER JOIN	Session						AS	es
	ON			es.Session_ID				=	nt.Entered_Session_ID
	WHERE		c.Concept_Group_Key			=	@concept_group_key

	OPEN		name_types

	WHILE 1 = 1
	BEGIN
		FETCH		name_types
		INTO        @name_type_concept_key,
					@short_name,
					@authority,
					@entered_by,
					@entry_date,
					@system

		IF @@FETCH_STATUS <> 0 BREAK

		BEGIN TRANSACTION

		SELECT		@taxon_name_type_key				=	Taxon_Name_Type_Key,
					@system_mapping						=	System_Supplied_Data
		FROM		Taxon_Dictionary_Name_Type_Mapping
		WHERE		Thesaurus_Name_Type_Key				=	@name_type_concept_key

		IF @@ROWCOUNT > 0
		BEGIN
			IF @system_mapping = 0
			BEGIN
				/* update name type */
				UPDATE		TAXON_NAME_TYPE
				SET			SHORT_NAME				=	@short_name,
							AUTHORITY				=	@authority,
							ENTERED_BY				=	@entered_by,
							ENTRY_DATE				=	@entry_date,
							SYSTEM_SUPPLIED_DATA	=	@system
				WHERE		TAXON_NAME_TYPE_KEY		=	@taxon_name_type_key

				IF @@ERROR <> 0 GOTO fail_from_cursor
			END
		END
		ELSE
		BEGIN
			/* create name type */
			EXECUTE		spNextKey	'TAXON_NAME_TYPE',
									@taxon_name_type_key	OUTPUT
			IF @@ERROR <> 0 GOTO fail_from_cursor

			INSERT		TAXON_NAME_TYPE (
						TAXON_NAME_TYPE_KEY,
						SHORT_NAME,
						AUTHORITY,
						ENTERED_BY,
						ENTRY_DATE,
						SYSTEM_SUPPLIED_DATA)
			VALUES		(@taxon_name_type_key,
						@short_name,
						@authority,
						@entered_by,
						@entry_date,
						@system)

			IF @@ERROR <> 0 GOTO fail_from_cursor

			/* record mapping */
			INSERT		Taxon_Dictionary_Name_Type_Mapping (
						Taxon_Name_Type_Key,
						Thesaurus_Name_Type_Key,
						System_Supplied_Data)
			VALUES		(@taxon_name_type_key,
						@name_type_concept_key,
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
	RAISERROR ('usp_TaxonNameType_ImportConceptGroup failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TaxonNameType_ImportConceptGroup') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_TaxonNameType_ImportConceptGroup'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_TaxonNameType_ImportConceptGroup TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_TaxonNameType_ImportConceptGroup TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_TaxonNameType_ImportConceptGroup TO [Dev - JNCC SQL]
END
GO