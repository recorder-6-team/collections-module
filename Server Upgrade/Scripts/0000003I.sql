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
	$Date: 12/02/09 15:53 $
	$Author: Pauldavies $

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

				DELETE		Taxon_Dictionary_Meaning_Mapping
				WHERE		Preferred_Name			=	@taxon_list_item_key

				DELETE		NameServer
				WHERE		Recommended_Taxon_List_Item_Key	=	@taxon_list_item_key
				
				DELETE		Taxon_Common_Name
				WHERE		TAXON_LIST_ITEM_KEY		=	@taxon_list_item_key

				DELETE		Index_Taxon_Name
				WHERE		TAXON_LIST_ITEM_KEY		=	@taxon_list_item_key
						OR	Recommended_Taxon_List_Item_Key =	@taxon_list_item_key

				DELETE		Index_Taxon_Synonym
				WHERE		TAXON_LIST_ITEM_KEY		=	@taxon_list_item_key
				OR			Synonym_List_Item_Key	=	@taxon_list_item_key

				DELETE		Index_Taxon_Group
				WHERE		TAXON_LIST_ITEM_KEY		=	@taxon_list_item_key
				OR			Contained_List_Item_Key	=	@taxon_list_item_key
				
				DECLARE		@NewPreferred	CHAR(16)
				SET			@NewPreferred	=	NULL			

				SELECT		@NewPreferred			=	Taxon_List_Item_Key
				FROM		Taxon_List_Item
				WHERE		Taxon_List_Item_Key		<>	@taxon_list_item_key
					AND		Preferred_Name			=	@taxon_list_item_key

				IF @NewPreferred IS NOT NULL
					UPDATE	Taxon_List_Item
					SET		Preferred_Name	=	@NewPreferred
					WHERE	Taxon_List_Item_Key		<>	@taxon_list_item_key
					AND		Preferred_Name			=	@taxon_list_item_key

				UPDATE	Taxon_List_Item
				SET		Parent	=	NULL
				WHERE	Parent	=	@taxon_list_item_key

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
		ELSE IF @preferred_name IS NOT NULL
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
		
			INSERT		Taxon_Dictionary_Meaning_Mapping (
						Preferred_Name,
						Meaning_Key)
			VALUES		(@taxon_list_item_key,
						@meaning_key)

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

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_TaxonList_ImportConceptGroup]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_TaxonList_ImportConceptGroup]
GO

/*===========================================================================*\
  Description:	Import a concept group as a taxon list.

  Parameters:   @job_id					Job identifier
				@taxon_list_key			Taxon list key
				@concept_group_key		Concept group key

  Created:		Dec 2003

  Last revision information:
	$Revision: 1 $
	$Date: 12/02/09 15:53 $
	$Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_TaxonList_ImportConceptGroup]
	@job_id				INT,
	@Taxon_List_Key		CHAR(16),
	@concept_group_Key	CHAR(16),
	@SessionID	CHAR(16)
AS

SET NOCOUNT ON

	DECLARE		@existing_list_key					CHAR(16),
				@taxon_version_Wellformed			Char(1),
				@taxon_version_Incorrectformed		Char(1),
				@taxon_version_Unverified			Char(1),
				@taxon_version_status_recommended	Char(1),
				@taxon_version_status_synonym		Char(1),
				@taxon_type_scientific 				Char(1),
				@taxon_type_vernacular  			Char(1),
				@taxon_name_type_key_formal			CHAR(16),
				@record_count						INT
		
	/* determine default parameters */
	SET			@taxon_version_wellformed			=	'W'	
	SET			@taxon_version_incorrectformed		=	'I'
	SET			@taxon_version_unverified			=	'U'			
    SET			@taxon_version_status_recommended	=	'R'		
	SET			@taxon_version_status_synonym		=	'S'
	SET			@taxon_type_scientific				=	'S'
	SET			@taxon_type_vernacular				=	'V'
	SET			@taxon_name_type_key_formal			=	'MNHSYS0000000001'
	SELECT		@existing_list_key		=	Taxon_List_Key
	FROM		Taxon_Dictionary_Concept_Group_Mapping
	WHERE		Concept_Group_Key		=	@concept_group_Key

	IF @@ROWCOUNT = 0
	BEGIN
		/* record mapping */
		INSERT		Taxon_Dictionary_Concept_Group_Mapping (
					Taxon_List_Key,
					Concept_Group_Key)
		VALUES		(@Taxon_List_Key,
					@concept_group_Key)

		IF @@ERROR <> 0 RETURN
	END
	ELSE IF @existing_List_Key <> @Taxon_List_Key
	BEGIN
		RAISERROR (
			'Concept group has previously been imported into a different Taxon List',
			16,
			1)
		RETURN
	END
	/* Calculate size of job */
	SELECT		@record_count			=	COUNT(*)
	FROM		Concept_Group_Version
	WHERE		Concept_Group_Key		=	@concept_group_Key

	/* COUNT(column) generates a warning if NULLs are encountered. And this ends up coming out as
	   an error after the import. Not good. So turn it off to avoid that situation. */
	SET ANSI_WARNINGS OFF  ---VI 17676

	SET	@record_count = @record_count * 3 + (
		SELECT		COUNT(DISTINCT c.Name_Type_Concept_Key)
					+ COUNT(DISTINCT c.Term_Key)
					+ COUNT(DISTINCT c.Term_Version_Key)
					+ COUNT(DISTINCT j.Source_Join_Key)
					+ COUNT(DISTINCT c.Concept_Rank_Key)
					+ COUNT(DISTINCT c.Concept_Key)
					+ COUNT(DISTINCT d.Designation_Type_Concept_Key)
					+ COUNT(DISTINCT d.Concept_Designation_Key)
					+ COUNT(DISTINCT f.Thesaurus_Fact_Key + vm.Term_Version_Key)
		FROM		Concept									AS	c
		LEFT JOIN	Source_Join								AS	j	ON	j.Record_Key		=	c.Term_Key
																	AND	j.Table_Name		=	'Term'
		LEFT JOIN	Concept_Designation						AS	d   ON	d.Concept_Key		=	c.Concept_Key
		LEFT JOIN	Thesaurus_Fact							AS	f	ON	f.Meaning_Key		=	c.Meaning_Key
		LEFT JOIN	Taxon_Dictionary_Term_Version_Mapping	AS	vm	ON	vm.Term_Version_Key	=	c.Term_Version_Key
		WHERE		c.Concept_Group_Key						=	@concept_group_Key  )

	EXECUTE		usp_Import_Export_Job_Configure		@job_id,
													@concept_group_Key,
													@record_count
	IF @@ERROR <> 0 RETURN

	/* import Versions */
	EXECUTE		usp_TaxonListVersion_ImportConceptGroup		@job_id
	IF @@ERROR <> 0 RETURN

	/* import name types */
	EXECUTE		usp_TaxonNameType_ImportConceptGroup	@job_id
	IF @@ERROR <> 0 RETURN

	/* import taxa */
	EXECUTE		usp_Taxon_ImportConceptGroup	@job_id
	IF @@ERROR <> 0 RETURN

	/* import Taxon Versions */
	EXECUTE		usp_TaxonVersion_ImportConceptGroup		@job_id, @SessionID
	IF @@ERROR <> 0 RETURN

	/* import Taxon/source relationships */
	EXECUTE		usp_TaxonSources_ImportConceptGroup		@job_id
	IF @@ERROR <> 0 RETURN

	/* import Taxon ranks */
	EXECUTE		usp_TaxonRank_ImportConceptGroup	@job_id
	IF @@ERROR <> 0 RETURN

	/* import Taxon List Items */
	EXECUTE		usp_TaxonListItem_ImportConceptGroup	@job_id
	IF @@ERROR <> 0 RETURN

	/* import hierarchical relationships */
	EXECUTE		usp_TaxonListItem_ImportRelationships	@job_id
	IF @@ERROR <> 0 RETURN

	/* import designation types */
	EXECUTE		usp_TaxonDesignationType_ImportConceptGroup		@job_id
	IF @@ERROR <> 0 RETURN

	/* import designations */
	EXECUTE		usp_TaxonDesignation_ImportConceptGroup		@job_id
	IF @@ERROR <> 0 RETURN

	/* import facts */
	EXECUTE		usp_TaxonFact_ImportConceptGroup	@job_id
	IF @@ERROR <> 0 RETURN 

	EXECUTE		usp_Import_Export_Job_UpdateStatus	@job_id,
													'Updating Taxon Names Index...'
	/* Discard Index_Taxon_Name records for the concept group */
	DELETE ITN
	FROM Index_Taxon_Name ITN
	INNER JOIN Taxon_Dictionary_Concept_Mapping TDM ON TDM.Taxon_List_Item_Key=ITN.Taxon_List_Item_Key
	INNER JOIN Concept C1 ON C1.Concept_Key=TDM.Concept_Key
			AND C1.Concept_Group_Key=@concept_group_Key

	/* Discard NameServer records for the concept group */
	DELETE		NS
	FROM		NameServer NS
	INNER JOIN	Taxon_Dictionary_Term_Version_Mapping TDM
										ON TDM.Taxon_Version_Key		=	NS.INPUT_Taxon_Version_Key
	INNER JOIN	Term_Version		TV	ON TV.Term_Version_Key			=	TDM.Term_Version_Key
	INNER JOIN	Concept				C1	ON C1.Term_Key					=	TV.Term_Key
										AND	C1.Concept_Group_Key		=	@concept_group_Key

	/* Rebuild Index_Taxon_Name for the concept group */
	INSERT INTO Index_Taxon_Name (Taxon_List_Item_Key, Taxon_List_Version_Key,
	 Actual_Name, Actual_Name_Italic, Common_Name, Common_Name_Italic, 
	  Preferred_Name, Preferred_Name_Italic, Abbreviation, Authority, System_Supplied_Data )
	SELECT TLI.Taxon_List_Item_Key, TLI.Taxon_List_Version_Key, 
	  T.Item_Name, CASE WHEN TR3.List_Font_Italic = 1 AND T.Language = 'La' THEN 1 ELSE 0 END, 
	  T2.Item_Name, CASE WHEN TR3.List_Font_Italic = 1 AND T2.Language = 'La' THEN 1 ELSE 0 END, 
	  T3.Item_Name, CASE WHEN TR3.List_Font_Italic = 1 AND T3.Language = 'La' THEN 1 ELSE 0 END, 
	  T.Abbreviation, T.Authority, 1 
	FROM ((((((((Taxon_List_Item AS TLI 
	INNER JOIN Taxon_Dictionary_Concept_Mapping TDM ON TDM.Taxon_List_Item_Key=TLI.Taxon_List_Item_Key
	INNER JOIN Concept C1 ON C1.Concept_Key=TDM.Concept_Key
			AND C1.Concept_Group_Key=@concept_group_Key
	LEFT JOIN Taxon_Version AS TV ON TV.Taxon_Version_Key = TLI.Taxon_Version_Key) 
	LEFT JOIN Taxon AS T ON T.Taxon_Key = TV.Taxon_Key) 
	LEFT JOIN Taxon_Common_Name AS TCN ON TCN.Taxon_List_Item_Key = TLI.Taxon_List_Item_Key) 
	LEFT JOIN Taxon_Version AS TV2 ON TV2.Taxon_Version_Key = TCN.Taxon_Version_Key) 
	LEFT JOIN Taxon AS T2 ON T2.Taxon_Key = TV2.Taxon_Key) 
	LEFT JOIN Taxon_List_Item AS TLI3 ON TLI3.Taxon_List_Item_Key = TLI.Preferred_Name) 
	LEFT JOIN Taxon_Rank AS TR3 ON TR3.Taxon_Rank_Key = TLI3.Taxon_Rank_Key) 
	LEFT JOIN Taxon_Version AS TV3 ON TV3.Taxon_Version_Key = TLI3.Taxon_Version_Key) 
	LEFT JOIN Taxon AS T3 ON T3.Taxon_Key = TV3.Taxon_Key 
	WHERE TLI.Taxon_List_Version_To IS NULL
	
	CREATE TABLE #NameServer (
			Taxon_List_Item_Key				CHAR(16)	COLLATE database_default,
			Input_Taxon_Version_Key			CHAR(16)	COLLATE database_default,
			Taxon_Version_FORM				CHAR		COLLATE database_default,
			Taxon_Version_STATUS			CHAR		COLLATE database_default,
			Taxon_Type						CHAR		COLLATE database_default,
			Recommended_Taxon_Version_Key	CHAR(16)	COLLATE database_default,
			Recommended_Taxon_List_Item_Key	CHAR(16)	COLLATE database_default,
			Entered_By						CHAR(16)	COLLATE database_default,
			Entry_Date						DATETIME,
			Changed_By						CHAR(16)	COLLATE database_default,
			Changed_Date					DATETIME	)

	--	Populates the temporary table of potential new Nameserver objects.
	INSERT  INTO #NameServer (
			Taxon_List_Item_Key,	
			Input_Taxon_Version_Key,
			Taxon_Version_FORM,
			Taxon_Version_STATUS,
			Taxon_Type,
			Recommended_Taxon_Version_Key,
			Recommended_Taxon_List_Item_Key,
			Entered_By,
			Entry_Date,
			Changed_By,
			Changed_Date )
	SELECT DISTINCT TLI.Taxon_List_Item_Key,
			TLI.Taxon_Version_Key,
			CASE WHEN TV1.Validation_Level = 0 THEN @taxon_version_wellformed
				 WHEN TV1.Validation_Level = 3 THEN @taxon_version_incorrectformed 
				 ELSE @taxon_version_unverified 
			END,
			CASE WHEN TV1.Taxon_Version_Key is NULL THEN @taxon_version_unverified 
				 WHEN TV1.Taxon_Version_Key  = TLI.Taxon_Version_Key THEN @taxon_version_status_recommended 
				 ELSE @taxon_version_status_synonym  
			END,
			CASE WHEN TNT.TAXON_NAME_TYPE_KEY = @taxon_name_type_key_formal THEN @taxon_type_scientific 
				 ELSE @taxon_type_vernacular 
			END,  
			TV1.Taxon_Version_Key,
			TLI.PREFERRED_NAME,
			TLI.ENTERED_BY,
			TLI.ENTRY_DATE,
			TLI.CHANGED_BY,
			TLI.CHANGED_DATE		
	FROM		Taxon_List_Item		TLI 
	INNER JOIN	Taxon_Dictionary_Term_Version_Mapping	
									TDM		ON	TDM.Taxon_Version_Key		=	TLI.Taxon_Version_Key
	INNER JOIN	Term_Version		TV		ON	TV.Term_Version_Key			=	TDM.Term_Version_Key
	INNER JOIN	Concept				C1		ON	C1.Term_Key					=	TV.Term_Key
											AND	C1.Concept_Group_Key		=	@concept_group_Key
	LEFT JOIN	Taxon_List_Item		TLI1	ON	TLI1.Taxon_List_Item_Key	=	TLI.Preferred_Name
	LEFT JOIN	Taxon_Version		TV1		ON	TV1.Taxon_Version_Key		=	TLI1.Taxon_Version_Key 
	INNER JOIN	TAXON				TX		ON	TX.TAXON_KEY				=	TV1.TAXON_KEY
	INNER JOIN	TAXON_NAME_TYPE		TNT		ON	TNT.TAXON_NAME_TYPE_KEY		=	TX.TAXON_NAME_TYPE_KEY
	INNER JOIN	Taxon_List_Version	TLV		ON	TLV.Taxon_List_Version_Key	=	TLI.Taxon_List_Version_Key
											AND TLV.Taxon_List_Key			=	@Taxon_List_Key
	LEFT JOIN	NameServer			NS		ON	NS.Input_Taxon_Version_Key	=	TLI.Taxon_Version_Key
	WHERE		TLI.Taxon_List_Version_To	IS NULL
			AND	NS.Input_Taxon_Version_Key	IS NULL

	-- Removes any NameServer records relating to Taxon_List_Items with the same Input_Taxon_Version_Key
	-- as another Taxon_List_Item from a more recent version.
	DELETE		NS
	FROM		#NameServer			NS
	INNER JOIN	Taxon_List_Item		TLI
			ON	NS.Recommended_Taxon_List_Item_Key	=	TLI.Taxon_List_Item_Key
	INNER JOIN	Taxon_List_Version	TLV
			ON	TLI.Taxon_List_Version_Key			=	TLV.Taxon_List_Version_Key
	INNER JOIN	#NameServer			NS2
			ON	NS.Input_Taxon_Version_Key			=	NS2.Input_Taxon_Version_Key
			AND	NS.Recommended_Taxon_List_Item_Key	<>	NS2.Recommended_Taxon_List_Item_Key
	INNER JOIN	Taxon_List_Item		TLI2
			ON	NS2.Recommended_Taxon_List_Item_Key	=	TLI2.Taxon_List_Item_Key
	INNER JOIN	Taxon_List_Version	TLV2
			ON	TLI2.Taxon_List_Version_Key			=	TLV2.Taxon_List_Version_Key
			AND	TLV2.Version						>	TLV.Version

	-- Actually inserts the records into the nameserver. The join ensures that if there are still
	-- duplicate Input_Taxon_Version_Keys, a single one will be chosen so as not to conflict with
	-- the unique constant on the primary key.
	INSERT INTO	NameServer	(
				Input_Taxon_Version_Key,
				Taxon_Version_FORM,
				Taxon_Version_STATUS,
				Taxon_Type,
				Recommended_Taxon_Version_Key,
				Recommended_Taxon_List_Item_Key,
				Entered_By,
				Entry_Date,
				Changed_By,
				Changed_Date	)
	SELECT		NS.Input_Taxon_Version_Key,
				NS.Taxon_Version_FORM,
				NS.Taxon_Version_STATUS,
				NS.Taxon_Type,
				NS.Recommended_Taxon_Version_Key,
				NS.Recommended_Taxon_List_Item_Key,
				NS.Entered_By,
				NS.Entry_Date,
				NS.Changed_By,
				NS.Changed_Date	
	FROM		#NameServer									NS
	INNER JOIN	(	SELECT		Input_Taxon_Version_Key,
								MAX(Taxon_List_Item_Key) AS Taxon_List_Item_Key 
					FROM		#NameServer 
					GROUP BY	Input_Taxon_Version_Key	)	NS2
		ON		NS2.Taxon_List_Item_Key		=	NS.Taxon_List_Item_Key
		AND		NS2.Input_Taxon_Version_Key	=	NS.Input_Taxon_Version_Key
	
	DROP TABLE	#NameServer

	/* Update to include the Has_Children field */
	UPDATE ITN
	SET Has_Children=1
	FROM Index_Taxon_Name ITN
	INNER JOIN Taxon_List_Item TLIChild ON TLIChild.Parent=ITN.Taxon_List_Item_Key
	INNER JOIN Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key=ITN.Taxon_List_Version_Key
		AND TLV.Taxon_List_Key=@Taxon_List_Key

	EXEC usp_IndexTaxonName_ApplyNameServer_SingleList @Taxon_List_Key

	UPDATE Import_Export_Job
	SET Records_Processed = Records_Processed + @@ROWCOUNT
	WHERE Import_Export_Job_ID = @job_id

	EXECUTE		usp_Import_Export_Job_UpdateStatus	@job_id,
													'Exporting Taxon Common Names...'	

	/* Create a local table containing the Taxon common name data */
	DECLARE @TaxonCommonName TABLE (
		Taxon_List_Item_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS PRIMARY Key,
		Taxon_Version_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS
	)

	/* Get the Taxon_List_Item_Keys first, as there may be several Taxon_Version_Keys for some, 
	  and that would break the primary Key constraint. */
	INSERT INTO @TaxonCommonName
	SELECT DISTINCT TDM1.Taxon_List_Item_Key, NULL
	FROM 	Taxon_Dictionary_Concept_Mapping TDM1 
	JOIN 	Concept C1 
			ON C1.Concept_Key = TDM1.Concept_Key
			AND C1.Concept_Group_Key = @concept_group_Key

	/* Now get a Taxon_Version_Key for each Taxon_List_Item_Key found, it'll use just one, thus 
	  being ok with the primary Key constraint.  */
	UPDATE 	TCNTemp
	SET 	Taxon_Version_Key = TLI.Taxon_Version_Key
	FROM 	@TaxonCommonName TCNTemp
	INNER JOIN Taxon_Dictionary_Concept_Mapping TDM1 ON TDM1.Taxon_List_Item_Key = TCNTemp.Taxon_List_Item_Key
	INNER JOIN Concept C1 ON C1.Concept_Key=TDM1.Concept_Key
			AND C1.Concept_Group_Key=@concept_group_Key
	LEFT JOIN (
			Concept C2 
			INNER JOIN Term T ON T.Term_Key=C2.Term_Key
			INNER JOIN Language L ON L.Language_Key=T.Language_Key AND L.Priority=1
		) ON C2.Meaning_Key=C1.Meaning_Key
			AND C2.Preferred=1
			AND C2.Name_Type_Concept_Key='SYSTEM000000000L'
	LEFT JOIN Concept C3 ON C3.Meaning_Key=C1.Meaning_Key
		AND C3.List_Preferred=1
		AND C3.Concept_Group_Key=C1.Concept_Group_Key
	INNER JOIN Taxon_Dictionary_Concept_Mapping TDM2 ON TDM2.Concept_Key=ISNULL(C2.Concept_Key, C3.Concept_Key)
	INNER JOIN Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key=TDM2.Taxon_List_Item_Key

	UPDATE Import_Export_Job
	SET Records_Processed = Records_Processed + @@ROWCOUNT
	WHERE Import_Export_Job_ID = @job_id

	/* Update existing Taxon common name records that are out of date */
	UPDATE TCN
	SET Taxon_Version_Key=TCNTmp.Taxon_Version_Key
	FROM @TaxonCommonName TCNTmp
	INNER JOIN Taxon_Common_Name TCN ON TCN.Taxon_List_Item_Key=TCNTmp.Taxon_List_Item_Key
	WHERE TCN.Taxon_Version_Key=TCNTmp.Taxon_Version_Key

	/* For new Taxon_Common_Name records, if no common name in the Thesaurus then
	link to itself */
	UPDATE TCNTmp
	SET TCNTmp.Taxon_Version_Key=TLI.Taxon_Version_Key
	FROM @TaxonCommonName TCNTmp
	INNER JOIN Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key=TCNTmp.Taxon_List_Item_Key
	WHERE TCNTmp.Taxon_Version_Key IS NULL
		
	/* Insert any new required Taxon common name records */
	INSERT INTO Taxon_Common_Name
	SELECT DISTINCT TCNTmp.Taxon_List_Item_Key, TCNTmp.Taxon_Version_Key
	FROM @TaxonCommonName TCNTmp
	LEFT JOIN Taxon_Common_Name TCN ON TCN.Taxon_List_Item_Key=TCNTmp.Taxon_List_Item_Key
	WHERE TCN.Taxon_List_Item_Key IS NULL

	UPDATE Import_Export_Job
	SET Records_Processed = Records_Processed + @@ROWCOUNT
	WHERE Import_Export_Job_ID = @job_id

	SET ANSI_WARNINGS ON
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TaxonList_ImportConceptGroup') AND SysStat & 0xf = 4)
BEGIN
   	PRINT 'Setting up security on procedure usp_TaxonList_ImportConceptGroup'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_TaxonList_ImportConceptGroup TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_TaxonList_ImportConceptGroup TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
       	GRANT EXECUTE ON dbo.usp_TaxonList_ImportConceptGroup TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptGroup_ImportTaxonList]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_ConceptGroup_ImportTaxonList]
GO

/*===========================================================================*\
  Description:	Import a taxon list into the specified concept group.

  Parameters:   @job_id					Job identifier
				@taxon_list_key			Taxon list key
				@concept_group_key		Concept group key

  Created:		Nov 2003

  Last revision information:
	$Revision: 1 $
	$Date: 12/02/09 15:53 $
	$Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptGroup_ImportTaxonList]
	@job_id				INT,
	@taxon_list_key		CHAR(16),
	@concept_group_key	CHAR(16)
AS
	SET NOCOUNT ON
	SET ARITHABORT ON
	SET ANSI_WARNINGS OFF

	DECLARE		@existing_group_key		CHAR(16)

	SELECT		@existing_group_key						=	Concept_Group_Key
	FROM		Taxon_Dictionary_Concept_Group_Mapping
	WHERE		Taxon_List_Key							=	@taxon_list_key

	IF @@ROWCOUNT = 0
	BEGIN
		BEGIN TRANSACTION

		/* record mapping */
		INSERT		Taxon_Dictionary_Concept_Group_Mapping (
					Taxon_List_Key,
					Concept_Group_Key)
		VALUES		(@taxon_list_key,
					@concept_group_key)

		IF @@ERROR <> 0 GOTO fail

		COMMIT TRANSACTION
	END
	ELSE IF @existing_group_key <> @concept_group_key
	BEGIN
		RAISERROR (
			'Taxon list has previously been imported into a different group',
			16,
			1)
		RETURN
	END

	/* Calculate size of job */
	DECLARE		@record_count					INT

	DECLARE		@items	TABLE (
				Taxon_List_Item_Key			CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS,
				Taxon_List_Version_Key		CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS,
				Taxon_Rank_Key				CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS,
				Taxon_Version_Key			CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS,
				List_Preferred_Key			CHAR(16))

	INSERT		@items
	SELECT    	tli.TAXON_LIST_ITEM_KEY,
				tli.TAXON_LIST_VERSION_KEY,
				tli.TAXON_RANK_KEY,
				tli.TAXON_VERSION_KEY,
				CASE WHEN tli.TAXON_LIST_ITEM_KEY = tli.PREFERRED_NAME
					THEN tli.TAXON_LIST_ITEM_KEY
					ELSE NULL
				END
	FROM        TAXON_LIST_VERSION			AS	tlv
	INNER JOIN	TAXON_LIST_ITEM				AS	tli
	ON			tli.TAXON_LIST_VERSION_KEY	=	tlv.TAXON_LIST_VERSION_KEY
	WHERE		tlv.TAXON_LIST_KEY			=	@taxon_list_key

	SELECT		@record_count				=	2 * COUNT(DISTINCT tli.Taxon_List_Item_Key)
												+ COUNT(DISTINCT tli.Taxon_List_Version_Key)
												+ COUNT(DISTINCT tli.Taxon_Rank_Key)
												+ COUNT(DISTINCT tli.Taxon_Version_Key)
												+ COUNT(DISTINCT tv.TAXON_KEY)
												+ COUNT(DISTINCT ts.SOURCE_LINK_KEY)
												+ COUNT(DISTINCT tx.TAXON_NAME_TYPE_KEY)
												+ COUNT(DISTINCT td.TAXON_DESIGNATION_KEY)
												+ COUNT(DISTINCT td.TAXON_DESIGNATION_TYPE_KEY)
												+ COUNT(DISTINCT tf.TAXON_FACT_KEY)
												+ COUNT(DISTINCT tli.List_Preferred_Key)
	FROM        @items						AS	tli
	INNER JOIN	TAXON_VERSION				AS	tv
	ON			tv.TAXON_VERSION_KEY		=	tli.Taxon_Version_Key
	INNER JOIN	TAXON						AS	tx
	ON			tx.TAXON_KEY				=	tv.TAXON_KEY
	LEFT JOIN	TAXON_SOURCES				AS	ts
	ON			ts.TAXON_KEY				=	tx.TAXON_KEY
	LEFT JOIN	TAXON_DESIGNATION			AS	td
	ON			td.TAXON_LIST_ITEM_KEY		=	tli.Taxon_List_Item_Key
	LEFT JOIN	TAXON_FACT					AS	tf
	ON			tf.TAXON_VERSION_KEY		=	tli.Taxon_Version_Key

	SET ANSI_WARNINGS ON

	EXECUTE		usp_Import_Export_Job_Configure		@job_id,
													@concept_group_key,
													@record_count
	IF @@ERROR <> 0 RETURN

	/* import versions */
	EXECUTE		usp_ConceptGroupVersion_ImportTaxonList		@job_id
	IF @@ERROR <> 0 RETURN

	/* import terms */
	EXECUTE		usp_Term_ImportTaxonList	@job_id
	IF @@ERROR <> 0 RETURN

	/* import term versions */
	EXECUTE		usp_TermVersion_ImportTaxonList		@job_id
	IF @@ERROR <> 0 RETURN

	/* import concept ranks */
	EXECUTE		usp_ConceptRank_ImportTaxonList     @job_id
	IF @@ERROR <> 0 RETURN

	/* import name type concepts */
	EXECUTE		usp_Concept_ImportTaxonNameTypes    @job_id
	IF @@ERROR <> 0 RETURN

	/* import concepts */
	EXECUTE		usp_Concept_ImportTaxonList		@job_id
	IF @@ERROR <> 0 RETURN

	/* import concept relationships */
	EXECUTE		usp_ConceptRelation_ImportTaxonList		@job_id
	IF @@ERROR <> 0 RETURN

	/* (re-)create concept lineage */
	EXECUTE     usp_ConceptLineage_GenerateForGroup		@job_id
	IF @@ERROR <> 0 RETURN

	/* import term/source relationships */
	EXECUTE		usp_SourceJoin_ImportTaxonSources	@job_id
	IF @@ERROR <> 0 RETURN

	/* import designation types */
	EXECUTE		usp_Concept_ImportTaxonDesignationTypes		@job_id
	IF @@ERROR <> 0 RETURN

	/* import concept designations */
	EXECUTE		usp_ConceptDesignation_ImportTaxonList		@job_id
	IF @@ERROR <> 0 RETURN

	/* import thesaurus facts */
	EXECUTE		usp_ThesaurusFact_ImportTaxonList	@job_id
	IF @@ERROR <> 0 RETURN

	RETURN

fail:
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_ConceptGroup_ImportTaxonList failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptGroup_ImportTaxonList') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptGroup_ImportTaxonList'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptGroup_ImportTaxonList TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptGroup_ImportTaxonList TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptGroup_ImportTaxonList TO [Dev - JNCC SQL]
END
GO
