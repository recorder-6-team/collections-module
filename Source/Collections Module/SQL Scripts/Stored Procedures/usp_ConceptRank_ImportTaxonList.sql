/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptRank_ImportTaxonList]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_ConceptRank_ImportTaxonList]
GO

/*===========================================================================*\
  Description:	Import concept ranks corresponding to taxon ranks from the
				specified taxon list.

  Parameters:   @job_id					Job identifier

  Created:		Nov 2003

  Last revision information:
	$Revision: 10 $
	$Date: 1/07/08 9:25 $
	$Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptRank_ImportTaxonList]
	@job_id				INT
AS
	SET NOCOUNT ON

	DECLARE     @taxon_list_key		CHAR(16),
				@domain_key			CHAR(16),
				@taxon_rank_key		CHAR(16),
				@item_name			VARCHAR(100),
				@sort_order			INT,
				@abbreviation		VARCHAR(10),
				@ins_user_key		CHAR(16),
				@ins_date			SMALLDATETIME,
				@ins_session_id		CHAR(16),
				@upd_user_key		CHAR(16),
				@upd_date			SMALLDATETIME,
				@upd_session_id		CHAR(16),
				@system				BIT,
				@concept_rank_key	CHAR(16)

	/* determine parameters of job */
	SELECT		@taxon_list_key							=	m.Taxon_List_Key,
				@domain_key								=	ld.Domain_Key
	FROM		Import_Export_Job						AS	j
	INNER JOIN	Taxon_Dictionary_Concept_Group_Mapping	AS	m
	ON			m.Concept_Group_Key						=	j.Concept_Group_Key
	INNER JOIN	Concept_Group							AS	g
	ON			g.Concept_Group_Key						=	m.Concept_Group_Key
	INNER JOIN	Local_Domain							AS	ld
	ON			ld.Local_Domain_Key						=	g.Local_Domain_Key
	WHERE		j.Import_Export_Job_ID					=	@job_id

	IF @@ROWCOUNT = 0
	BEGIN
		RAISERROR ('Job does not exist or has not been configured', 16, 1)
		RETURN
	END

	EXECUTE		usp_Import_Export_Job_UpdateStatus	@job_id,
													'Importing concept ranks'
	IF @@ERROR <> 0 RETURN

	DECLARE		ranks	CURSOR LOCAL FAST_FORWARD FOR
	SELECT DISTINCT
				tr.TAXON_RANK_KEY,
				ISNULL(tr.LONG_NAME, tr.SHORT_NAME),
				tr.SEQUENCE,
				tr.SHORT_NAME,
				tr.ENTERED_BY,
				tr.ENTRY_DATE,
				tr.CHANGED_BY,
				tr.CHANGED_DATE,
				tr.SYSTEM_SUPPLIED_DATA
	FROM		TAXON_LIST_VERSION			AS	tlv
	INNER JOIN	TAXON_LIST_ITEM				AS	tli
	ON			tli.TAXON_LIST_VERSION_KEY	=	tlv.TAXON_LIST_VERSION_KEY
	INNER JOIN	TAXON_RANK					AS	tr
	ON			tr.TAXON_RANK_KEY			=	tli.TAXON_RANK_KEY
	WHERE		tlv.TAXON_LIST_KEY			=	@taxon_list_key

	OPEN		ranks

	WHILE 1 = 1
	BEGIN
		FETCH		ranks
		INTO        @taxon_rank_key,
					@item_name,
					@sort_order,
					@abbreviation,		/* TODO: may clip! */
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

		SELECT		@concept_rank_key						=	M.Concept_Rank_Key
		FROM		Taxon_Dictionary_Concept_Rank_Mapping M
		INNER JOIN	Concept_Rank CR 
					ON CR.Concept_Rank_Key					=	M.Concept_Rank_Key
					AND CR.Domain_Key						=	@Domain_Key
		WHERE		M.Taxon_Rank_Key						=	@taxon_rank_key

		IF @@ROWCOUNT > 0
		BEGIN
			/* update concept rank */
			UPDATE		Concept_Rank
			SET			Domain_Key				=	@domain_key,
						Item_Name				=	@item_name,
						Sort_Order				=	@sort_order,
						Abbreviation			=	@abbreviation,
						Entered_Session_ID		=	@ins_session_id,
						Changed_Session_ID		=	@upd_session_id,
						System_Supplied_Data	=	@system
			WHERE		Concept_Rank_Key		=	@concept_rank_key

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END
		ELSE
		BEGIN
			/* create concept rank */
			EXECUTE		spNextKey	'Concept_Rank',
									@concept_rank_key		OUTPUT
			IF @@ERROR <> 0 GOTO fail_from_cursor

			INSERT		Concept_Rank (
						Concept_Rank_Key,
						Domain_Key,
						Item_Name,
						Sort_Order,
						Abbreviation,
						Color_R,
						Color_G,
						Color_B,
						Entered_Session_ID,
						Changed_Session_ID,
						System_Supplied_Data)
			VALUES		(@concept_rank_key,
						@domain_key,
						@item_name,
						@sort_order,
						@abbreviation,
						0,
						0,
						0,
						@ins_session_id,
						@upd_session_id,
						@system)

			IF @@ERROR <> 0 GOTO fail_from_cursor

			/* record mapping */
			INSERT		Taxon_Dictionary_Concept_Rank_Mapping (
						Taxon_Rank_Key,
						Concept_Rank_Key)
			VALUES		(@taxon_rank_key,
						@concept_rank_key)

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END

		EXECUTE		usp_Import_Export_Job_RecordProcessed	@job_id
		IF @@ERROR <> 0 GOTO fail_from_cursor

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
	RAISERROR ('usp_ConceptRank_ImportTaxonList failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptRank_ImportTaxonList') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptRank_ImportTaxonList'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptRank_ImportTaxonList TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptRank_ImportTaxonList TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptRank_ImportTaxonList TO [Dev - JNCC SQL]
END
GO