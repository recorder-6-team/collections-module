/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_TaxonDesignationType_ImportConceptGroup]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_TaxonDesignationType_ImportConceptGroup]
GO

/*===========================================================================*\
  Description:	Import taxon designation types corresponding to the types
  				used in the specified taxon list.

  Parameters:	@job_id					Job identifier

  Created:		Dec 2003

  Last revision information:
	$Revision: 3 $
	$Date: 4/08/11 10:29 $
	$Author: Simonlewis $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_TaxonDesignationType_ImportConceptGroup]
	@job_id					INT
AS
	SET NOCOUNT ON

	DECLARE     @concept_group_key				CHAR(16),
				@concept_key					CHAR(16),
				@taxon_designation_type_key		CHAR(16),
				@short_name						VARCHAR(40),
				@ins_user_key					CHAR(16),
				@ins_date						SMALLDATETIME,
				@upd_user_key					CHAR(16),
				@upd_date						SMALLDATETIME,
				@system							BIT

	/* determine parameters of job */
	SELECT		@concept_group_key				=	j.Concept_Group_Key
	FROM		Import_Export_Job				AS	j
	WHERE		j.Import_Export_Job_ID			=	@job_id

	IF @@ROWCOUNT = 0
	BEGIN
		RAISERROR ('Job does not exist or has not been configured', 16, 1)
		GOTO fail
	END

	EXECUTE		usp_Import_Export_Job_UpdateStatus	@job_id,
													'Importing designation types'
	IF @@ERROR <> 0 GOTO fail

	DECLARE		types	CURSOR LOCAL FAST_FORWARD FOR
	SELECT DISTINCT
				dt.Concept_Key,
				dt.Published_Term	AS	Item_Name,
				es.User_Name_Key,
				CONVERT(SMALLDATETIME,
						CONVERT(VARCHAR, es.Date_Time_Start, 112)),
				cs.User_Name_Key,
				CONVERT(SMALLDATETIME,
						CONVERT(VARCHAR, cs.Date_Time_Start, 112)),
				dt.System_Supplied_Data
	FROM		Concept							AS	c
	INNER JOIN	Concept_Designation				AS	d
	ON			d.Concept_Key					=	c.Concept_Key
	INNER JOIN	Concept							AS  dt
	ON			dt.Concept_Key					=	d.Designation_Type_Concept_Key
	INNER JOIN	Session							AS	es
	ON			es.Session_ID					=	dt.Entered_Session_ID
	LEFT JOIN	Session							AS	cs
	ON			cs.Session_ID					=	dt.Changed_Session_ID
	WHERE		c.Concept_Group_Key				=	@concept_group_key

	OPEN		types

	WHILE 1 = 1
	BEGIN
		FETCH		types
		INTO		@concept_key,
					@short_name,
					@ins_user_key,
					@ins_date,
					@upd_user_key,
					@upd_date,
					@system

		IF @@FETCH_STATUS <> 0 BREAK

		BEGIN TRANSACTION

		SELECT		@taxon_designation_type_key					=	Taxon_Designation_Type_Key
		FROM		Taxon_Dictionary_Designation_Type_Mapping
		WHERE		Concept_Designation_Type_Key				=	@concept_key

		IF @@ROWCOUNT > 0
		BEGIN
			/* update designation type */
			UPDATE		TAXON_DESIGNATION_TYPE
			SET			SHORT_NAME					=	@short_name,
						ENTERED_BY					=	@ins_user_key,
						ENTRY_DATE					=	@ins_date,
						CHANGED_BY					=	@upd_user_key,
						CHANGED_DATE				=	@upd_date,
						SYSTEM_SUPPLIED_DATA		=	@system
			WHERE		TAXON_DESIGNATION_TYPE_KEY	=	@taxon_designation_type_key

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END
		ELSE
		BEGIN
			/* create designation type */
			EXECUTE		spNextKey	'TAXON_DESIGNATION_TYPE',
									@taxon_designation_type_key		OUTPUT
			IF @@ERROR <> 0 GOTO fail_from_cursor

			INSERT		TAXON_DESIGNATION_TYPE (
						TAXON_DESIGNATION_TYPE_KEY,
						SHORT_NAME,
						ENTERED_BY,
						ENTRY_DATE,
						CHANGED_BY,
						CHANGED_DATE,
						SYSTEM_SUPPLIED_DATA)
			VALUES		(@taxon_designation_type_key,
						@short_name,
						@ins_user_key,
						@ins_date,
						@upd_user_key,
						@upd_date,
						@system)

			IF @@ERROR <> 0 GOTO fail_from_cursor

			/* record mapping */
			INSERT		Taxon_Dictionary_Designation_Type_Mapping (
						Taxon_Designation_Type_Key,
						Concept_Designation_Type_Key)
			VALUES		(@taxon_designation_type_key,
						@concept_key)

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
	RAISERROR ('usp_TaxonDesignationType_ImportConceptGroup failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TaxonDesignationType_ImportConceptGroup') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_TaxonDesignationType_ImportConceptGroup'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_TaxonDesignationType_ImportConceptGroup TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_TaxonDesignationType_ImportConceptGroup TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_TaxonDesignationType_ImportConceptGroup TO [Dev - JNCC SQL]
END
GO