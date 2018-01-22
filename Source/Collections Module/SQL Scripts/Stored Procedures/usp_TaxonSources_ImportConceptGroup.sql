/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_TaxonSources_ImportConceptGroup]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_TaxonSources_ImportConceptGroup]
GO

/*===========================================================================*\
  Description:	Import taxon/source relationships corresponding to the
  				term/source relationships in a concept group.

  Parameters:   @job_id					Job identifier

  Created:		Jan 2004

  Last revision information:
	$Revision: 2 $
	$Date: 12/05/04 9:57 $
	$Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_TaxonSources_ImportConceptGroup]
	@job_id				INT
AS
	SET NOCOUNT ON

	DECLARE		@concept_group_key		CHAR(16),
				@source_join_key		CHAR(16),
				@taxon_key				CHAR(16),
				@source_key				CHAR(16),
				@original				BIT,
				@source_link_key		CHAR(16)	

	/* determine parameters of job */
	SELECT		@concept_group_key		=	Concept_Group_Key
	FROM		Import_Export_Job
	WHERE		Import_Export_Job_ID	=	@job_id

	IF @@ROWCOUNT = 0
	BEGIN
		RAISERROR ('Job does not exist or has not been configured', 16, 1)
		GOTO fail
	END

	EXECUTE		usp_Import_Export_Job_UpdateStatus	@job_id,
													'Exporting term/source relationships'
	IF @@ERROR <> 0 GOTO fail

	DECLARE		sources		CURSOR LOCAL FAST_FORWARD FOR
	SELECT		j.Source_Join_Key,
				tm.Taxon_Key,
				j.Source_Key,
				j.Original
	FROM		Concept							AS	c
	INNER JOIN	Source_Join						AS	j
	ON			j.Record_Key					=	c.Term_Key
	AND			j.Table_Name					=	'Term'
	INNER JOIN	Taxon_Dictionary_Term_Mapping	AS	tm
	ON			tm.Term_Key						=	j.Record_Key
	WHERE		c.Concept_Group_Key				=	@concept_group_key

	OPEN		sources

	WHILE 1 = 1
	BEGIN
		FETCH		sources
		INTO		@source_join_key,
					@taxon_key,
					@source_key,
					@original

		IF @@FETCH_STATUS <> 0 BREAK

		BEGIN TRANSACTION

		SELECT		@source_link_key						=	Source_Link_Key
		FROM		Taxon_Dictionary_Term_Sources_Mapping
		WHERE		Source_Join_Key							=	@source_join_key

		IF @@ROWCOUNT > 0
		BEGIN
			/* update existing taxon source */
			UPDATE		TAXON_SOURCES
			SET			TAXON_KEY			=	@taxon_key,
						SOURCE_KEY			=	@source_key,
						ORIGINAL			=	@original
			WHERE		SOURCE_LINK_KEY		=	@source_link_key

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END
		ELSE
		BEGIN
			/* create a new taxon source */
			EXECUTE		spNextKey	'TAXON_SOURCES',
									@source_link_key	OUTPUT
			IF @@ERROR <> 0 GOTO fail_from_cursor

			INSERT		TAXON_SOURCES (
						SOURCE_LINK_KEY,
						TAXON_KEY,
						SOURCE_KEY,
						ORIGINAL)
			VALUES		(@source_link_key,
						@taxon_key,
						@source_key,
						@original)

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
	RAISERROR ('usp_TaxonSources_ImportConceptGroup failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TaxonSources_ImportConceptGroup') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_TaxonSources_ImportConceptGroup'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_TaxonSources_ImportConceptGroup TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_TaxonSources_ImportConceptGroup TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_TaxonSources_ImportConceptGroup TO [Dev - JNCC SQL]
END
GO