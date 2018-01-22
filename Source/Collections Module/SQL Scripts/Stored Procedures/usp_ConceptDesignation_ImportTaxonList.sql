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
	$Revision: 8 $
	$Date: 22/06/05 12:10 $
	$Author: Andrewkemp $

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