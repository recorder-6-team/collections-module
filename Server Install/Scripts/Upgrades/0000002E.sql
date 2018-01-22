/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_TaxonListItem_ImportConceptGroup]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_TaxonListItem_ImportConceptGroup]
GO

/*===========================================================================*\
  Description:	Import taxon list items corresponding to the concepts in a
				concept group.

  Parameters:	@job_id					Job identifier

  Created:		Dec 2003

  Last revision information:
	$Revision: 1 $
	$Date: 16/11/07 15:47 $
	$Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_TaxonListItem_ImportConceptGroup]
	@job_id				INT
AS
	SET NOCOUNT ON

	DECLARE     @concept_group_key		CHAR(16),
				@concept_key			CHAR(16),
				@taxon_list_item_key	CHAR(16),
				@term_key				CHAR(16),
				@term_version_key		CHAR(16),
				@list_preferred			BIT,
				@meaning_key			CHAR(16),
				@taxon_key				CHAR(16),
				@taxon_version_key		CHAR(16),
				@preferred_name			CHAR(16),
				@sort_code				INT,
				@taxon_rank_key			CHAR(16),
				@ins_user_key			CHAR(16),
				@ins_date				SMALLDATETIME,
				@upd_user_key			CHAR(16),
				@upd_date				SMALLDATETIME,
				@system					BIT,
				@from_list_version_key	CHAR(16),
				@to_list_version_key	CHAR(16),
				@is_new					BIT

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
													'Exporting concepts'
	IF @@ERROR <> 0 GOTO fail

	DECLARE		concepts	CURSOR FAST_FORWARD LOCAL FOR
	SELECT		c.Concept_Key,
				c.Term_Key,
				c.Term_Version_Key,
				c.List_Preferred,
				c.Meaning_Key,
				c.Sort_Code,
				crm.Taxon_Rank_Key,
				es.User_Name_Key,
				CONVERT(SMALLDATETIME,
						CONVERT(VARCHAR, es.Date_Time_Start, 112)),
				cs.User_Name_Key,
				CONVERT(SMALLDATETIME,
						CONVERT(VARCHAR, cs.Date_Time_Start, 112)),
				c.System_Supplied_Data
	FROM		Concept									AS	c
	INNER JOIN	Taxon_Dictionary_Concept_Rank_Mapping	AS	crm
	ON			crm.Concept_Rank_Key					=	c.Concept_Rank_Key
	INNER JOIN	Session									AS	es
	ON			es.Session_ID							=	c.Entered_Session_ID
	LEFT JOIN	Session									AS	cs
	ON			cs.Session_ID							=	c.Changed_Session_ID
	WHERE		c.Concept_Group_Key						=	@concept_group_key
	ORDER BY	c.List_Preferred DESC	/* i.e. preferred names first */

	OPEN		concepts

	WHILE 1 = 1
	BEGIN
		FETCH		concepts
		INTO		@concept_key,
					@term_key,
					@term_version_key,
					@list_preferred,
					@meaning_key,
					@sort_code,
					@taxon_rank_key,
					@ins_user_key,
					@ins_date,
					@upd_user_key,
					@upd_date,
					@system

		IF @@FETCH_STATUS <> 0 BREAK

		BEGIN TRANSACTION

		SET ROWCOUNT 1

		SELECT      @from_list_version_key							=	gvm.Taxon_List_Version_Key
		FROM		Concept_History									AS	h
		INNER JOIN	Concept_Group_Version							AS	gv
		ON			gv.Concept_Group_Version_Key					=	h.Concept_Group_Version_From
		INNER JOIN	Taxon_Dictionary_Concept_Group_Version_Mapping	AS	gvm
		ON			gvm.Concept_Group_Version_Key					=	gv.Concept_Group_Version_Key
		WHERE		h.Concept_Key									=	@concept_key
		ORDER BY	gv.Sequence

		--If the from list version is null (because the thesaurus allows no history), 
		--then use the first version available	
		SET ROWCOUNT 1
		
		IF @from_list_version_key IS NULL
			SELECT @from_list_version_key = Taxon_List_Version_Key
			FROM Concept c
			INNER JOIN Taxon_Dictionary_Concept_Group_Mapping cgm ON cgm.Concept_Group_Key=c.Concept_Group_Key
			INNER JOIN Taxon_List_Version tlv on tlv.Taxon_list_Key=cgm.Taxon_List_Key
			WHERE c.Concept_Key=@concept_Key
			ORDER BY tlv.Version ASC

		/* we do the term and term version mappings inside the 'SET ROWCOUNT 1'
		 * because a single term may map onto multiple taxa; we need to choose
		 * exactly one, preferabyly one that is already correctly mapper
		 */
		SELECT		@Taxon_Key=tm.Taxon_Key
		FROM		Taxon_Dictionary_Term_Mapping tm
		LEFT JOIN Taxon_Version tv ON tv.Taxon_Key=tm.Taxon_Key
		LEFT JOIN Taxon_Dictionary_Term_Version_Mapping tvm 
			ON tvm.Taxon_Version_Key=tv.Taxon_Version_Key
			AND tvm.Term_Version_key=@term_version_key
		WHERE		tm.Term_Key						=	@term_key
		ORDER BY tvm.Term_Version_Key DESC

		IF @@ROWCOUNT = 0 GOTO skip_item

		SET ROWCOUNT 0

		SELECT		@to_list_version_key							=	gvm.Taxon_List_Version_Key
		FROM		Concept_History									AS	h
		INNER JOIN	Concept_Group_Version							AS	gv
		ON			gv.Concept_Group_Version_Key					=	h.Concept_Group_Version_To
		INNER JOIN	Taxon_Dictionary_Concept_Group_Version_Mapping	AS	gvm
		ON			gvm.Concept_Group_Version_Key					=	gv.Concept_Group_Version_Key
		WHERE		h.Concept_Key									=	@concept_key
		ORDER BY	gv.Sequence DESC

		IF @@ROWCOUNT = 0
		BEGIN
			SET			@to_list_version_key	=	NULL
		END

		IF @term_version_key IS NULL
		BEGIN
			SET			@taxon_version_key	=	NULL
		END
		ELSE
		BEGIN
			SELECT		@taxon_version_key						=	m.Taxon_Version_Key
			FROM		Taxon_Dictionary_Term_Version_Mapping	AS	m
			INNER JOIN	TAXON_VERSION							AS	tv
			ON			tv.TAXON_VERSION_KEY					=	m.Taxon_Version_Key
			WHERE		m.Term_Version_Key						=	@term_version_key
			AND			tv.TAXON_KEY							=	@taxon_key

			IF @@ROWCOUNT = 0 GOTO skip_item				
		END

		SET ROWCOUNT 0

		/* check for existing mapping */
		SELECT		@taxon_list_item_key				=	Taxon_List_Item_Key
		FROM		Taxon_Dictionary_Concept_Mapping
		WHERE		Concept_Key							=	@concept_key

		SET			@is_new		=	CASE WHEN @@ROWCOUNT = 0 THEN 1 ELSE 0 END

		IF @is_new = 1
		BEGIN
			EXECUTE		spNextKey	'TAXON_LIST_ITEM',
									@taxon_list_item_key	OUTPUT
			IF @@ERROR <> 0 GOTO fail_from_cursor
		END

		/* work out preferred name */
		IF @list_preferred = 1
		BEGIN
			SET			@preferred_name		=	@taxon_list_item_key
		END
		ELSE
		BEGIN
			SET			@preferred_name						=	NULL

			SET ROWCOUNT 1

			SELECT		@preferred_name						=	m.Taxon_List_Item_Key
			FROM		Concept								AS	c
			INNER JOIN 	Taxon_Dictionary_Concept_Mapping	AS	m
			ON			m.Concept_Key						=	c.Concept_Key
			WHERE		c.Concept_Group_Key					=	@concept_group_key
			AND			c.Meaning_Key						=	@meaning_key
			AND			c.List_Preferred					=	1

			SET ROWCOUNT 0
		END

		IF @preferred_name IS NOT NULL AND @taxon_version_key IS NULL
		BEGIN
			/* create a minimal taxon version */
			EXECUTE		spNextKey	'TAXON_VERSION',
									@taxon_version_key	OUTPUT
			IF @@ERROR <> 0 GOTO fail_from_cursor

			INSERT		TAXON_VERSION (
						TAXON_VERSION_KEY,
						TAXON_KEY,
						ENTERED_BY,
						ENTRY_DATE,
						SYSTEM_SUPPLIED_DATA)
			VALUES		(@taxon_version_key,
						@taxon_key,
						@ins_user_key,
						@ins_date,
						@system)

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END

		IF @is_new = 0
		BEGIN
			IF @preferred_name IS NULL
			BEGIN
				/* concept is not selectable; remove any list items */
				DELETE		Taxon_Dictionary_Concept_Mapping
				WHERE		TAXON_LIST_ITEM_KEY		=	@taxon_list_item_key
				
				DELETE		Taxon_Common_Name
				WHERE		TAXON_LIST_ITEM_KEY		=	@taxon_list_item_key

				DELETE		Index_Taxon_Name
				WHERE		TAXON_LIST_ITEM_KEY		=	@taxon_list_item_key

				DELETE		Index_Taxon_Synonym
				WHERE		TAXON_LIST_ITEM_KEY		=	@taxon_list_item_key
				OR			Synonym_List_Item_Key	=	@taxon_list_item_key

				DELETE		Index_Taxon_Group
				WHERE		TAXON_LIST_ITEM_KEY		=	@taxon_list_item_key
				OR			Contained_List_Item_Key	=	@taxon_list_item_key

				DELETE		TAXON_LIST_ITEM
				WHERE		TAXON_LIST_ITEM_KEY		=	@taxon_list_item_key

				IF @@ERROR <> 0 GOTO fail_from_cursor
			END
			ELSE
			BEGIN
				/* update taxon list item */
				UPDATE		TAXON_LIST_ITEM
				SET			TAXON_VERSION_KEY		=	@taxon_version_key,
							TAXON_LIST_VERSION_KEY	=	@from_list_version_key,
							TAXON_LIST_VERSION_TO	=	@to_list_version_key,
							PREFERRED_NAME			=	@preferred_name,
							SORT_CODE				=	@sort_code,
							TAXON_RANK_KEY			=	@taxon_rank_key,
							ENTERED_BY				=	@ins_user_key,
							ENTRY_DATE				=	@ins_date,
							CHANGED_BY				=	@upd_user_key,
							CHANGED_DATE			=	@upd_date,
							SYSTEM_SUPPLIED_DATA	=	@system
				WHERE		TAXON_LIST_ITEM_KEY		=	@taxon_list_item_key

				IF @@ERROR <> 0 GOTO fail_from_cursor
			END
		END
		ELSE
		BEGIN
			/* create taxon list item */
			INSERT		TAXON_LIST_ITEM (
						TAXON_LIST_ITEM_KEY,
						TAXON_VERSION_KEY,
						TAXON_LIST_VERSION_KEY,
						TAXON_LIST_VERSION_TO,
						PREFERRED_NAME,
						SORT_CODE,
						TAXON_RANK_KEY,
						ENTERED_BY,
						ENTRY_DATE,
						CHANGED_BY,
						CHANGED_DATE,
						SYSTEM_SUPPLIED_DATA)
			VALUES		(@taxon_list_item_key,
						@taxon_version_key,
						@from_list_version_key,
						@to_list_version_key,
						@preferred_name,
						@sort_code,
						@taxon_rank_key,
						@ins_user_key,
						@ins_date,
						@upd_user_key,
						@upd_date,
						@system)

			IF @@ERROR <> 0 GOTO fail_from_cursor

			/* record mapping */
			INSERT		Taxon_Dictionary_Concept_Mapping (
						Taxon_List_Item_Key,
						Concept_key)
			VALUES		(@taxon_list_item_key,
						@concept_key)

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END
		
skip_item:
		EXECUTE		usp_Import_Export_Job_RecordProcessed	@job_id
		IF @@ERROR <> 0 GOTO fail_from_cursor

		COMMIT TRANSACTION
	END

	CLOSE		concepts
	DEALLOCATE	concepts
	RETURN

fail_from_cursor:
	CLOSE		concepts
	DEALLOCATE	concepts

fail:
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_TaxonListItem_ImportConceptGroup failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TaxonListItem_ImportConceptGroup') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_TaxonListItem_ImportConceptGroup'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_TaxonListItem_ImportConceptGroup TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_TaxonListItem_ImportConceptGroup TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_TaxonListItem_ImportConceptGroup TO [Dev - JNCC SQL]
END
GO