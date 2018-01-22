/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_TaxonDesignation_ImportConceptGroup]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_TaxonDesignation_ImportConceptGroup]
GO

/*===========================================================================*\
  Description:	Import taxon designations corresponding to the concept
				designations associated with the specified concept group.

  Parameters:	@job_id					Job identifier

  Created:		Dec 2003

  Last revision information:
	$Revision: 1 $
	$Date: 26/02/07 10:09 $
	$Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_TaxonDesignation_ImportConceptGroup]
	@job_id					INT
AS
	SET NOCOUNT ON

	DECLARE     @concept_group_key			CHAR(16),
				@concept_designation_key	CHAR(16),
				@taxon_designation_key		CHAR(16),
				@taxon_list_item_key		CHAR(16),
				@taxon_designation_type_key	CHAR(16),
				@date_from					DATETIME,
				@date_to					DATETIME,
				@entered_by					CHAR(16),
				@entry_date					SMALLDATETIME,
				@changed_by					CHAR(16),
				@changed_date				SMALLDATETIME,
				@system						BIT,
				@source_key					CHAR(16),
				@source_join_key			CHAR(16)

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
													'Exporting concept designations'
	IF @@ERROR <> 0 GOTO fail

	DECLARE		designations	CURSOR LOCAL FAST_FORWARD FOR
	SELECT		d.Concept_Designation_Key,
				cm.Taxon_List_Item_Key,
				tm.Taxon_Designation_Type_Key,
				CASE WHEN d.From_Vague_Date_Start >= -53688
					THEN DATEADD(day, d.From_Vague_Date_Start, '18991230')
							/* date that SQL server can represent */
					ELSE NULL
							/* null, or date that SQL server cannot represent */
				END,
				CASE WHEN d.To_Vague_Date_Start >= -53688
					THEN DATEADD(day, d.To_Vague_Date_Start, '18991230')
							/* date that SQL server can represent */
					ELSE NULL
							/* null, or date that SQL server cannot represent */
				END,
				es.User_Name_Key,
				CONVERT(SMALLDATETIME,
						CONVERT(VARCHAR, es.Date_Time_Start, 112)),
				cs.User_Name_Key,
				CONVERT(SMALLDATETIME,
						CONVERT(VARCHAR, cs.Date_Time_Start, 112)),
				d.System_Supplied_Data
	FROM        Concept										AS	c
	INNER JOIN	Concept_Designation							AS	d
	ON			d.Concept_Key								=	c.Concept_Key
	INNER JOIN	Taxon_Dictionary_Concept_Mapping			AS	cm
	ON			cm.Concept_Key								=	d.Concept_Key
	INNER JOIN	Taxon_Dictionary_Designation_Type_Mapping	AS	tm
	ON			tm.Concept_Designation_Type_Key				=	d.Designation_Type_Concept_Key
	INNER JOIN	Session										AS	es
	ON			es.Session_ID								=	d.Entered_Session_ID
	LEFT JOIN	Session										AS	cs
	ON			cs.Session_ID								=	d.Changed_Session_ID
	WHERE		c.Concept_Group_Key							=	@concept_group_key

	OPEN		designations

	WHILE 1 = 1
	BEGIN
		FETCH		designations
		INTO		@concept_designation_key,
					@taxon_list_item_key,
					@taxon_designation_type_key,
					@date_from,
					@date_to,
					@entered_by,
					@entry_date,
					@changed_by,
					@changed_date,
					@system

		IF @@FETCH_STATUS <> 0 BREAK

		BEGIN TRANSACTION

		SELECT		@taxon_designation_key							=	NULL,
					@source_join_key								=	NULL,
					@source_key										=	NULL

		SELECT		@taxon_designation_key							=	m.Taxon_Designation_Key,
					@source_key										=	j.Source_Key
		FROM		Taxon_Dictionary_Concept_Designation_Mapping	AS	m
		LEFT JOIN	Source_Join										AS	j
		ON			j.Source_Join_Key								=	m.Source_Join_Key
		WHERE		Concept_Designation_Key							=	@concept_designation_key

		IF @source_key IS NULL
		BEGIN
			/* there is no existing mapping for the source join; pick an
			 * arbitrary join record (if there are any) and make this the
			 * mapped join.
			 */
			SELECT		@source_join_key	=	Source_Join_Key,
						@source_key			=	Source_Key
			FROM		Source_Join
			WHERE		Record_Key			=	@concept_designation_key
			AND			Table_Name			=	'Concept_Designation'
			ORDER BY	Source_Join_Key
		END

		IF @taxon_designation_key IS NOT NULL
		BEGIN
			/* update taxon designation */
			UPDATE		TAXON_DESIGNATION
			SET			DATE_FROM					=	@date_from,
						DATE_TO						=	@date_to,
						TAXON_DESIGNATION_TYPE_KEY	=	@taxon_designation_type_key,
						TAXON_LIST_ITEM_KEY			=	@taxon_list_item_key,
						SOURCE_KEY					=	@source_key,
						ENTERED_BY					=	@entered_by,
						ENTRY_DATE					=	@entry_date,
						CHANGED_BY					=	@changed_by,
						CHANGED_DATE				=	@changed_date,
						SYSTEM_SUPPLIED_DATA		=	@system
			WHERE		TAXON_DESIGNATION_KEY		=	@taxon_designation_key

			IF @@ERROR <> 0 GOTO fail_from_cursor

			IF @source_join_key IS NOT NULL
			BEGIN
				UPDATE		Taxon_Dictionary_Concept_Designation_Mapping
				SET			Source_Join_Key									=	@source_join_key
				WHERE		Taxon_Designation_Key							=	@taxon_designation_key

				IF @@ERROR <> 0 GOTO fail_from_cursor				
			END
		END
		ELSE
		BEGIN
			/* create taxon designation */
			EXECUTE		spNextKey	'TAXON_DESIGNATION',
									@taxon_designation_key	OUTPUT
			IF @@ERROR <> 0 GOTO fail_from_cursor

			INSERT		TAXON_DESIGNATION (
						TAXON_DESIGNATION_KEY,
						DATE_FROM,
						DATE_TO,
						TAXON_DESIGNATION_TYPE_KEY,
						TAXON_LIST_ITEM_KEY,
						SOURCE_KEY,
						ENTERED_BY,
						ENTRY_DATE,
						CHANGED_BY,
						CHANGED_DATE,
						SYSTEM_SUPPLIED_DATA)
			VALUES		(@taxon_designation_key,
						@date_from,
						@date_to,
						@taxon_designation_type_key,
						@taxon_list_item_key,
						@source_key,
						@entered_by,
						@entry_date,
						@changed_by,
						@changed_date,
						@system)

			IF @@ERROR <> 0 GOTO fail_from_cursor

			/* record mapping */
			INSERT		Taxon_Dictionary_Concept_Designation_Mapping (
						Taxon_Designation_Key,
						Concept_Designation_Key,
						Source_Join_Key)
			VALUES		(@taxon_designation_key,
						@concept_designation_key,
						@source_join_key)

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END

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
	RAISERROR ('usp_TaxonDesignation_ImportConceptGroup failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TaxonDesignation_ImportConceptGroup') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_TaxonDesignation_ImportConceptGroup'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_TaxonDesignation_ImportConceptGroup TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_TaxonDesignation_ImportConceptGroup TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_TaxonDesignation_ImportConceptGroup TO [Dev - JNCC SQL]
END
GO