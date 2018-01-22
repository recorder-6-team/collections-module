/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_SourceJoin_ImportTaxonSources]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_SourceJoin_ImportTaxonSources]
GO

/*===========================================================================*\
  Description:	Import term/source relationships corresponding to the
  				taxon/source relationships in a taxon list.

  Parameters:   @job_id					Job identifier

  Created:		Jan 2004

  Last revision information:
	$Revision: 4 $
	$Date: 12/05/04 9:57 $
	$Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SourceJoin_ImportTaxonSources]
	@job_id				INT
AS
	SET NOCOUNT ON

	DECLARE		@concept_group_key		CHAR(16),
				@source_link_key		CHAR(16),
				@term_key				CHAR(16),
				@source_key				CHAR(16),
				@original				BIT,
				@system_supplied_data	BIT,
				@source_join_key		CHAR(16)

	/* determine parameters of job */
	SELECT		@concept_group_key		=	Concept_Group_Key
	FROM		Import_Export_Job
	WHERE		Import_Export_Job_ID	=	@job_id

	IF @@ROWCOUNT = 0
	BEGIN
		RAISERROR ('Job does not exist or has not been configured', 16, 1)
		RETURN
	END

	EXECUTE		usp_Import_Export_Job_UpdateStatus	@job_id,
													'Importing term/source relationships'
	IF @@ERROR <> 0 RETURN

	DECLARE		sources		CURSOR LOCAL FAST_FORWARD FOR
	SELECT		ts.SOURCE_LINK_KEY,
				tm.Term_Key,
				ts.SOURCE_KEY,
				ts.ORIGINAL,
				tx.System_Supplied_Data
	FROM		Concept							AS	c
	INNER JOIN	Taxon_Dictionary_Term_Mapping	AS	tm
	ON			tm.Term_Key						=	c.Term_Key
	INNER JOIN	TAXON_SOURCES					AS	ts
	ON			ts.TAXON_KEY					=	tm.Taxon_Key
	INNER JOIN	TAXON							AS	tx
	ON			tx.TAXON_KEY					=	ts.TAXON_KEY
	WHERE		c.Concept_Group_Key				=	@concept_group_key

	OPEN		sources

	WHILE 1 = 1
	BEGIN
		FETCH		sources
		INTO		@source_link_key,
					@term_key,
					@source_key,
					@original,
					@system_supplied_data

		IF @@FETCH_STATUS <> 0 BREAK

		BEGIN TRANSACTION

		SELECT		@source_join_key						=	NULL

		SELECT		@source_join_key						=	Source_Join_Key
		FROM		Taxon_Dictionary_Term_Sources_Mapping
		WHERE		Source_Link_Key							=	@source_link_key

		IF @@ROWCOUNT = 0
		BEGIN
			SELECT		@source_join_key	=	Source_Join_Key
			FROM		Source_Join
			WHERE		Record_Key			=	@term_key
			AND			Table_Name			=	'Term'
			AND			Source_Key			=	@source_key
		END

		IF @source_join_key IS NOT NULL
		BEGIN
			/* update existing source join */
			UPDATE		Source_Join
			SET			Record_Key				=	@term_key,
						Source_Key				=	@source_key,
						Original				=	@original,
						System_Supplied_Data	=	@system_supplied_data
			WHERE		Source_Join_Key			=	@source_join_key

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END
		ELSE
		BEGIN
			/* create new source join */
			EXECUTE		spNextKey	'Source_Join',
									@source_join_key	OUTPUT
			IF @@ERROR <> 0 GOTO fail_from_cursor

			INSERT		Source_Join (
						Source_Join_Key,
						Table_Name,
						Record_Key,
						Source_Key,
						Original,
						Entered_Session_ID,
						System_Supplied_Data)
			VALUES 		(@source_join_key,
						'Term',
						@term_key,
						@source_key,
						@original,
						'SYSTEM0000000000',
						@system_supplied_data)

			IF @@ERROR <> 0 GOTO fail_from_cursor

			/* record mapping */
			INSERT		Taxon_Dictionary_Term_Sources_Mapping (
						Source_Link_Key,
						Source_Join_Key)
			VALUES		(@source_link_key,
						@source_join_key)

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END

		/* update progress counter */
		EXECUTE		usp_Import_Export_Job_RecordProcessed	@job_id
		IF @@ERROR <> 0 GOTO fail_from_cursor
		
		COMMIT TRANSACTION
	END

	CLOSE		sources
	DEALLOCATE	sources
	RETURN

fail_from_cursor:
	CLOSE		sources
	DEALLOCATE	sources

fail:
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_SourceJoin_ImportTaxonSources failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SourceJoin_ImportTaxonSources') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SourceJoin_ImportTaxonSources'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SourceJoin_ImportTaxonSources TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SourceJoin_ImportTaxonSources TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SourceJoin_ImportTaxonSources TO [Dev - JNCC SQL]
END
GO