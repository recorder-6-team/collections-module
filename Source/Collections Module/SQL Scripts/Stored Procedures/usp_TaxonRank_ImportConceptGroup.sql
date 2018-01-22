/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_TaxonRank_ImportConceptGroup]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_TaxonRank_ImportConceptGroup]
GO

/*===========================================================================*\
  Description:	Import taxon ranks corresponding to concept ranks from the
				specified concept group.

  Parameters:	@job_id					Job identifier

  Created:		Dec 2003

  Last revision information:
	$Revision: 9 $
	$Date: 30/06/08 16:44 $
	$Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_TaxonRank_ImportConceptGroup]
	@job_id				INT
AS
	SET NOCOUNT ON

	DECLARE     @concept_group_key		CHAR(16),
				@concept_rank_key		CHAR(16),
				@sequence				SMALLINT,
				@short_name				VARCHAR(20),
				@long_name				VARCHAR(100),
				@entered_by				CHAR(16),
				@entry_date				SMALLDATETIME,
				@changed_by				CHAR(16),
				@changed_date			SMALLDATETIME,
				@system					BIT,
				@taxon_rank_key			CHAR(16),
				@DoUpdate				BIT

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
													'Exporting concept ranks'
	IF @@ERROR <> 0 GOTO fail

	DECLARE		ranks	CURSOR LOCAL FAST_FORWARD FOR
	SELECT DISTINCT
				cr.Concept_Rank_Key,
				cr.Sort_Order,
				cr.Abbreviation,
				cr.Item_Name,
				es.User_Name_Key,
				CONVERT(SMALLDATETIME,
						CONVERT(VARCHAR, es.Date_Time_Start, 112)),
				cs.User_Name_Key,
				CONVERT(SMALLDATETIME,
						CONVERT(VARCHAR, cs.Date_Time_Start, 112)),
				cr.System_Supplied_Data
	FROM		Concept				   			AS	c
	INNER JOIN	Concept_Rank					AS	cr
	ON			cr.Concept_Rank_Key				=	c.Concept_Rank_Key
	INNER JOIN	Session							AS	es
	ON			es.Session_ID					=	cr.Entered_Session_ID
	LEFT JOIN	Session							AS	cs
	ON			cs.Session_ID					=	cr.Changed_Session_ID
	WHERE		c.Concept_Group_Key				=	@concept_group_key

	OPEN		ranks

	WHILE 1 = 1
	BEGIN
		FETCH		ranks
		INTO		@concept_rank_key,
					@sequence,
					@short_name,
					@long_name,
					@entered_by,
					@entry_date,
					@changed_by,
					@changed_date,
					@system

		IF @@FETCH_STATUS <> 0 BREAK

		BEGIN TRANSACTION

		SELECT		@taxon_rank_key							=	Taxon_Rank_Key
		FROM		Taxon_Dictionary_Concept_Rank_Mapping
		WHERE		Concept_Rank_Key						=	@concept_rank_key

		IF @@ROWCOUNT = 0
		BEGIN
			/* No existing mapping, but search for a suitable taxon rank to map to */
			SELECT		@taxon_rank_key						=	TR.Taxon_Rank_Key
			FROM		Taxon_Rank TR
			WHERE		TR.Long_Name=@Long_Name
			AND 		(TR.Sequence=@Sequence or @Sequence IS NULL)
			
			IF @@ROWCOUNT = 0
				SET @DoUpdate=0
			ELSE BEGIN
				SET @DoUpdate=1
				/* If concept rank sequence is null, don't overwrite the good one in the
				dictionary.  Instead, copy the dictionary one back */
				if @Sequence IS NULL BEGIN
					SELECT @Sequence=Sequence
					FROM Taxon_Rank
					WHERE Taxon_Rank_Key=@taxon_rank_key

					UPDATE Concept_Rank
					SET Sort_Order=@Sequence
					WHERE Concept_Rank_Key=@concept_rank_key
				END	
			END

		END
		ELSE
			SET @DoUpdate=1

		IF @DoUpdate=1
		BEGIN
			/* update taxon rank */
			UPDATE		TAXON_RANK
			SET			SEQUENCE				=	@sequence,
						SHORT_NAME				=	@short_name,
						LONG_NAME				=	@long_name,
						ENTERED_BY				=	@entered_by,
						ENTRY_DATE				=	@entry_date,
						CHANGED_BY				=	@changed_by,
						CHANGED_DATE			=	@changed_date,
						SYSTEM_SUPPLIED_DATA	=	@system
			WHERE		TAXON_RANK_KEY			=	@taxon_rank_key

			/* ensure mapping exists */
			IF NOT EXISTS(SELECT 1 FROM Taxon_Dictionary_Concept_Rank_Mapping 
					WHERE Taxon_Rank_Key=@taxon_rank_key
					AND Concept_Rank_Key=@concept_rank_key)
				INSERT		Taxon_Dictionary_Concept_Rank_Mapping (
						Taxon_Rank_Key,
						Concept_Rank_Key)
				VALUES		(@taxon_rank_key,
						@concept_rank_key)
		   IF @@ERROR <> 0 GOTO fail_from_cursor
		END
		ELSE
		BEGIN
			/* create taxon rank */
			EXECUTE		spNextKey		'TAXON_RANK',
										@taxon_rank_key		OUTPUT
			IF @@ERROR <> 0 GOTO fail_from_cursor

			INSERT		TAXON_RANK (
						TAXON_RANK_KEY,
						SEQUENCE,
						SHORT_NAME,
						LONG_NAME,
						LIST_FONT_ITALIC,
						DISPLAY_IN_DETAILS,
						ENTERED_BY,
						ENTRY_DATE,
						CHANGED_BY,
						CHANGED_DATE,
						SYSTEM_SUPPLIED_DATA)
			VALUES		(@taxon_rank_key,
						@sequence,
						@short_name,
						@long_name,
						0,
						0,
						@entered_by,
						@entry_date,
						@changed_by,
						@changed_date,
						@system)

			IF @@ERROR <> 0 GOTO fail_from_cursor

			/* ensure mapping exists */
			IF NOT EXISTS(SELECT 1 FROM Taxon_Dictionary_Concept_Rank_Mapping WHERE Taxon_Rank_Key=@taxon_rank_key)
				INSERT		Taxon_Dictionary_Concept_Rank_Mapping (
						Taxon_Rank_Key,
						Concept_Rank_Key)
				VALUES		(@taxon_rank_key,
						@concept_rank_key)

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END

		EXECUTE		usp_Import_Export_Job_RecordProcessed	@job_id
		IF @@ERROR <> 0 GOTO fail

		COMMIT TRANSACTION
	END

	CLOSE		ranks
	DEALLOCATE	ranks
	RETURN

fail_from_cursor:
	CLOSE		ranks
	DEALLOCATE	ranks

fail:
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_TaxonRank_ImportConceptGroup failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TaxonRank_ImportConceptGroup') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_TaxonRank_ImportConceptGroup'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_TaxonRank_ImportConceptGroup TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_TaxonRank_ImportConceptGroup TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_TaxonRank_ImportConceptGroup TO [Dev - JNCC SQL]
END
GO