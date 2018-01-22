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
	$Date: 17/08/11 10:44 $
	$Author: Jamesbichard $

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
				@record_count						INT,
				@taxon_list_type_key				CHAR(16),
				@taxon_list_name					VARCHAR(100),
				@authority							VARCHAR(50),
				@user_name_key						CHAR(16),
				@taxon_list_version_key				CHAR(16)
		
	/* determine default parameters */
	SET			@taxon_version_wellformed			=	'W'	
	SET			@taxon_version_incorrectformed		=	'I'
	SET			@taxon_version_unverified			=	'U'			
    SET			@taxon_version_status_recommended	=	'R'		
	SET			@taxon_version_status_synonym		=	'S'
	SET			@taxon_type_scientific				=	'S'
	SET			@taxon_type_vernacular				=	'V'
	SET			@taxon_name_type_key_formal			=	'MNHSYS0000000001'

	SELECT @taxon_list_name = Item_Name, @authority = authority
	FROM Concept_Group
	WHERE Concept_Group_Key = @Concept_Group_Key

	--If @Taxon_List_Key is null, create a new taxon list with the same name
	--as the exported concept group
	IF @Taxon_List_Key IS NULL
	BEGIN
		EXEC dbo.spNextKey 'TAXON_LIST', @Taxon_List_Key OUTPUT
		EXEC dbo.spNextKey 'TAXON_LIST_VERSION', @taxon_list_version_key OUTPUT

		SELECT @taxon_list_type_key = Taxon_List_Type_Key
		FROM Taxon_List_Type
		WHERE Short_Name = 'Checklist'

		SELECT @user_name_key = User_Name_Key
		FROM Session
		WHERE Session_ID = @SessionID		

		INSERT INTO Taxon_List (
			taxon_list_key,
			item_name,
			taxon_list_type_key,
			entered_by,
			authority)
		VALUES (
			@taxon_list_key,
			@taxon_list_name,
			@taxon_list_type_key,
			@user_name_key,
			@authority)
	
		INSERT INTO Taxon_List_Version (
			taxon_list_version_key,
			taxon_list_key,
			entered_by)
		VALUES (
			@taxon_list_version_key,
			@taxon_list_key,
			@user_name_key)

		IF @@ERROR <> 0 RETURN
	END
	ELSE
	BEGIN
		UPDATE Taxon_List
		SET Authority = @Authority
		WHERE Taxon_List_Key = @taxon_list_key
	END

	SET ROWCOUNT 0

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

	SET ANSI_WARNINGS ON

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
	  Preferred_Name, Preferred_Name_Italic, Abbreviation, Authority, System_Supplied_Data,
	  Recommended_Taxon_List_Item_Key)
	SELECT TLI.Taxon_List_Item_Key, TLI.Taxon_List_Version_Key, 
	  T.Item_Name, CASE WHEN TR3.List_Font_Italic = 1 AND T.Language = 'La' THEN 1 ELSE 0 END, 
	  T2.Item_Name, CASE WHEN TR3.List_Font_Italic = 1 AND T2.Language = 'La' THEN 1 ELSE 0 END, 
	  T3.Item_Name, CASE WHEN TR3.List_Font_Italic = 1 AND T3.Language = 'La' THEN 1 ELSE 0 END, 
	  T.Abbreviation, T.Authority, 1, TDM2.Taxon_List_Item_Key 
	FROM ((((((((Taxon_List_Item AS TLI 
	INNER JOIN Taxon_Dictionary_Concept_Mapping TDM ON TDM.Taxon_List_Item_Key=TLI.Taxon_List_Item_Key
	INNER JOIN Concept C1 ON C1.Concept_Key=TDM.Concept_Key
			AND C1.Concept_Group_Key=@concept_group_Key
	LEFT JOIN Taxon_Dictionary_Concept_Mapping TDM2 ON TDM2.Concept_Key = C1.Meaning_Key
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

	--Get the taxon list item preferred synonyms for the taxon list items related 
	--to the export concept group. Use preferred lists to determine which synonym
	--is chosen.
	declare @bound int
	select @bound = Count(*) from taxon_list

	--Create a temporary table containing each taxon list item for the export group,
	--and for each item the list-preferred synonyms for each preferred list (as
	--recorded in Concept_Group_Preferred_Taxon_List). If there is no synonym in a
	--list, the original item is recorded, and the priority is set so that this will
	--be the lowest priority item.
	select distinct 
		tli.taxon_list_item_key,
		case
			when (syn.concept_key is not null and cmsyn.concept_key is not null)
				then cmsyn.taxon_list_item_key
			else tli.taxon_list_item_key
		end as synonym_taxon_list_item_key,
		case
			when (syn.concept_key is not null and cmsyn.concept_key is not null)
				then ptl.priority
			else @bound
		end as "priority"
	into #PreferredListSynonyms
	from taxon_list_item tli
	inner join taxon_dictionary_concept_mapping cm
		on cm.taxon_list_item_key = tli.taxon_list_item_key
	inner join concept c
		on c.concept_key = cm.concept_key
	left join (
		concept_group_preferred_taxon_list ptl
			inner join taxon_dictionary_concept_group_mapping cgm
				on cgm.taxon_list_key = ptl.taxon_list_key
			)
		on ptl.concept_group_key = c.concept_group_key 
	left join concept syn on syn.concept_group_key = cgm.concept_group_key
	and	syn.meaning_key = c.meaning_key
			and syn.list_preferred = 1
	left join taxon_dictionary_concept_mapping cmsyn
		on syn.concept_key = cmsyn.concept_key

	where c.concept_group_key = @concept_group_key

	--Create a new temporary table to store the overall preferred synonym for each
	--list item. This is the synonym in #PreferredListSynonyms with the lowest value
	--for priority. If there are more than one such synonyms, the maximum value of
	--the synonym key is arbitrarily chosen.
	select 
		ps.taxon_list_item_key,
		max(ps.synonym_taxon_list_item_key) as synonym_taxon_list_item_key,
		tli.taxon_version_key as synonym_taxon_version_key
	into #PreferredListPreferredSynonyms
	from #PreferredListSynonyms ps
	inner join (
			select taxon_list_item_key, min(priority) as priority
			from #PreferredListSynonyms
			group by taxon_list_item_key) as ps1
		on ps1.taxon_list_item_key = ps.taxon_list_item_key
		and ps1.priority = ps.priority
	inner join taxon_list_item tli 
		on tli.taxon_list_item_key = ps.synonym_taxon_list_item_key
	group by 
		ps.taxon_list_item_key, 
		ps.priority, 
		tli.taxon_version_key

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
			PS.Synonym_Taxon_Version_Key,
			PS.Synonym_Taxon_List_Item_Key,
			TLI.ENTERED_BY,
			TLI.ENTRY_DATE,
			TLI.CHANGED_BY,
			TLI.CHANGED_DATE		
	FROM		Taxon_List_Item		TLI 
	INNER JOIN	#PreferredListPreferredSynonyms PS
											ON	PS.Taxon_List_Item_Key		=	TLI.Taxon_List_Item_Key
	LEFT JOIN	Taxon_List_Item		TLI1	ON	TLI1.Taxon_List_Item_Key	=	TLI.Preferred_Name
	LEFT JOIN	Taxon_Version		TV1		ON	TV1.Taxon_Version_Key		=	TLI1.Taxon_Version_Key 
	INNER JOIN	TAXON				TX		ON	TX.TAXON_KEY				=	TV1.TAXON_KEY
	INNER JOIN	TAXON_NAME_TYPE		TNT		ON	TNT.TAXON_NAME_TYPE_KEY		=	TX.TAXON_NAME_TYPE_KEY
	INNER JOIN	Taxon_List_Version	TLV		ON	TLV.Taxon_List_Version_Key	=	TLI.Taxon_List_Version_Key
											AND TLV.Taxon_List_Key			=	@Taxon_List_Key
	LEFT JOIN	NameServer			NS		ON	NS.Input_Taxon_Version_Key	=	TLI.Taxon_Version_Key
	WHERE		TLI.Taxon_List_Version_To	IS NULL
			AND	NS.Input_Taxon_Version_Key	IS NULL

	drop table #PreferredListSynonyms
	drop table #PreferredListPreferredSynonyms

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
	   WHERE  Id = Object_Id(N'[dbo].[usp_TaxonListVersion_ImportConceptGroup]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_TaxonListVersion_ImportConceptGroup]
GO

/*===========================================================================*\
  Description:	Import taxon list versions corresponding to the versions
				of a concept group.

  Parameters:	@job_id					Job identifier

  Created:		Dec 2003

  Last revision information:
	$Revision: 1 $
	$Date: 17/08/11 10:44 $
	$Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_TaxonListVersion_ImportConceptGroup]
	@job_id					INT
AS
	SET NOCOUNT ON

	DECLARE		@concept_group_key				CHAR(16),
				@taxon_list_key					CHAR(16),
				@concept_group_version_key		CHAR(16),
				@version						INT,
				@authority						VARCHAR(50),
				@vague_date_start				INT,
				@vague_date_end					INT,
				@vague_date_type				VARCHAR(2),
				@entered_by						CHAR(16),
				@entry_date						SMALLDATETIME,
				@changed_by						CHAR(16),
				@changed_date					SMALLDATETIME,
				@system							BIT,
				@taxon_list_version_key			CHAR(16),
				@source_key						CHAR(16),
				@source_join_key				CHAR(16)

	/* determine parameters of job */
	SELECT      @concept_group_key						=	m.Concept_Group_Key,
				@taxon_list_key							=	m.Taxon_List_Key
	FROM		Import_Export_Job						AS	j
	INNER JOIN	Taxon_Dictionary_Concept_Group_Mapping	AS	m
	ON			m.Concept_Group_Key						=	j.Concept_Group_Key
	WHERE		j.Import_Export_Job_ID					=	@job_id

	IF @@ROWCOUNT = 0
	BEGIN
		RAISERROR ('Job does not exist or has not been configured', 16, 1)
		GOTO fail
	END

	EXECUTE		usp_Import_Export_Job_UpdateStatus	@job_id,
													'Exporting concept group versions'
	IF @@ERROR <> 0 GOTO fail

	DECLARE		versions	CURSOR LOCAL FAST_FORWARD FOR
	SELECT		cgv.Concept_Group_Version_Key,
				cgv.Sequence,
				CAST(cg.Authority AS VARCHAR(50)),
				cgv.From_Vague_Date_Start,
				cgv.From_Vague_Date_End,
				cgv.From_Vague_Date_Type,
				es.User_Name_Key,
				CONVERT(SMALLDATETIME,
						CONVERT(VARCHAR, es.Date_Time_Start, 112)),
				cs.User_Name_Key,
				CONVERT(SMALLDATETIME,
						CONVERT(VARCHAR, cs.Date_Time_End, 112)),
				cgv.System_Supplied_Data
	FROM		Concept_Group_Version		AS	cgv
	INNER JOIN	Concept_Group				AS	cg
	ON			cgv.Concept_Group_Key		=	cg.Concept_Group_Key
	INNER JOIN	Session						AS	es
	ON			es.Session_ID				=	cgv.Entered_Session_ID
	LEFT JOIN	Session						AS	cs
	ON			cs.Session_ID				=	cgv.Changed_Session_ID
	WHERE		cgv.Concept_Group_Key		=	@concept_group_key

	OPEN		versions

	WHILE 1 = 1
	BEGIN
		FETCH		versions
		INTO		@concept_group_version_key,
					@version,
					@authority,
					@vague_date_start,
					@vague_date_end,
					@vague_date_type,
					@entered_by,
					@entry_date,
					@changed_by,
					@changed_date,
					@system

		IF @@FETCH_STATUS <> 0 BREAK

		BEGIN TRANSACTION

		SELECT      @taxon_list_version_key							=	NULL,
					@source_join_key					   			=	NULL,
					@source_key										=	NULL

		SELECT		@taxon_list_version_key							=	m.Taxon_List_Version_Key,
					@source_key										=	j.Source_Key
		FROM		Taxon_Dictionary_Concept_Group_Version_Mapping	AS	m
		LEFT JOIN	Source_Join										AS	j
		ON			j.Source_Join_Key								=	m.Source_Join_Key
		WHERE		m.Concept_Group_Version_Key						=	@concept_group_version_key

		IF @source_key IS NULL
		BEGIN
			/* there is no existing mapping for the source join; pick an
			 * arbitrary join record (if there are any) and make this the
			 * mapped join.
			 */
			SELECT		@source_join_key	=	Source_Join_Key,
						@source_key			=	Source_Key
			FROM		Source_Join
			WHERE		Record_Key			=	@concept_group_version_key
			AND			Table_Name			=	'Concept_Group_Version'
			ORDER BY	Source_Join_Key
		END

		IF @taxon_list_version_key IS NOT NULL
		BEGIN
			/* update taxon list version */
			UPDATE		TAXON_LIST_VERSION
			SET			VERSION					=	@version,
						AUTHORITY				=	@authority,
						VAGUE_DATE_START		=	@vague_date_start,
						VAGUE_DATE_END			=	@vague_date_end,
						VAGUE_DATE_TYPE			=	@vague_date_type,
						SOURCE_KEY				=	@source_key,
						ENTERED_BY				=	@entered_by,
						ENTRY_DATE				=	@entry_date,
						CHANGED_BY				=	@changed_by,
						CHANGED_DATE			=	@changed_date,
						SYSTEM_SUPPLIED_DATA	=	@system
			WHERE		TAXON_LIST_VERSION_KEY	=	@taxon_list_version_key

			IF @@ERROR <> 0 GOTO fail_from_cursor

			IF @source_join_key IS NOT NULL
			BEGIN
				UPDATE		Taxon_Dictionary_Concept_Group_Version_Mapping
				SET			Source_Join_Key									=	@source_join_key
				WHERE		Taxon_List_Version_Key							=	@taxon_list_version_key

				IF @@ERROR <> 0 GOTO fail_from_cursor
			END
		END
		ELSE
		BEGIN
			/* create taxon list version */
			EXECUTE		spNextKey	'TAXON_LIST_VERSION',
									@taxon_list_version_key		OUTPUT
			IF @@ERROR <> 0 GOTO fail_from_cursor

			INSERT		TAXON_LIST_VERSION (
						TAXON_LIST_VERSION_KEY,
						TAXON_LIST_KEY,
						VERSION,
						AUTHORITY,
						VAGUE_DATE_START,
						VAGUE_DATE_END,
						VAGUE_DATE_TYPE,
						SOURCE_KEY,
						ENTERED_BY,
						ENTRY_DATE,
						CHANGED_BY,
						CHANGED_DATE,
						SYSTEM_SUPPLIED_DATA)
			VALUES		(@taxon_list_version_key,
						@taxon_list_key,
						@version,
						@authority,
						@vague_date_start,
						@vague_date_end,
						@vague_date_type,
						@source_key,
						@entered_by,
						@entry_date,
						@changed_by,
						@changed_date,
						@system)

			IF @@ERROR <> 0 GOTO fail_from_cursor

			/* record mapping */
			INSERT		Taxon_Dictionary_Concept_Group_Version_Mapping (
						Taxon_List_Version_Key,
						Concept_Group_Version_Key,
						Source_Join_Key)
			VALUES		(@taxon_list_version_key,
						@concept_group_version_key,
						@source_join_key)

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END

		EXECUTE		usp_Import_Export_Job_RecordProcessed	@job_id
		IF @@ERROR <> 0 GOTO fail_from_cursor
		
		COMMIT TRANSACTION
	END

	CLOSE		versions
	DEALLOCATE	versions
	RETURN

fail_from_cursor:
	CLOSE		versions
	DEALLOCATE	versions

fail:
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_TaxonListVersion_ImportConceptGroup failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TaxonListVersion_ImportConceptGroup') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_TaxonListVersion_ImportConceptGroup'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_TaxonListVersion_ImportConceptGroup TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_TaxonListVersion_ImportConceptGroup TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_TaxonListVersion_ImportConceptGroup TO [Dev - JNCC SQL]
END
GO

IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_DuplicateTerms_Merge]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
	DROP PROCEDURE [dbo].[usp_DuplicateTerms_Merge]
GO

/*===========================================================================*\
  Description:	
	Reassign records linked to @OldTermKey to @NewTermKey before deleting
	the @OldTermKey.

  Parameters:	
	@NewTermKey	Specify the key of the term to keep.
	@OldTermKey	Specify the key of the term to delete.

  Created:	
	January 2006

  Last revision information:
    $Revision: 1 $
    $Date: 17/08/11 10:44 $
    $Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_DuplicateTerms_Merge]
	@NewTermKey	CHAR(16),
	@OldTermKey	CHAR(16)
AS
	IF @NewTermKey <> @OldTermKey
	BEGIN
		SET NOCOUNT ON 

		-- Change over 
		UPDATE	Taxon_Dictionary_Term_Mapping
		SET	Term_Key = @NewTermKey
		WHERE	Term_Key = @OldTermKey

		UPDATE	Concept
		SET	Term_Key = @NewTermKey
		WHERE	Term_Key = @OldTermKey	

		/*========================================================*\
			Update term version records
		\*========================================================*/
		DECLARE @CurrentKey CHAR(16), @DuplicateKey CHAR(16), @UpdateTermKey BIT
		
		-- Update/delete term versions that use the old term key
		WHILE 7 = (6 + 1)
		BEGIN			
			-- Get the first term version which uses the old term key
			SELECT @CurrentKey = Term_Version_Key
			FROM Term_Version
			WHERE Term_Key = @OldTermKey

			-- If there are no more term versions using the old key, there is nothing to do
			IF @@RowCount = 0 BREAK

			-- Flags whether the current term version should be updated or deleted
			SET @UpdateTermKey = 0

			-- Check if we have already got a term version with the new term key which has
			-- the same authority and version label as the current term version. We also don't
			-- want the duplicate to be mapped to a taxon version - the only term versions we
			-- will remove are unmapped, and we don't want any linked concepts to suddenly get
			-- a term version which is mapped.		
			SELECT @DuplicateKey = tvduplicate.Term_Version_Key
			FROM Term_Version tvduplicate
			INNER JOIN Term_Version tvcurrent 
				ON ISNULL(tvduplicate.Version_Label, '') = ISNULL(tvcurrent.Version_Label, '')
				AND	ISNULL(tvduplicate.Author_And_Date, '') = ISNULL(tvcurrent.Author_And_Date, '')
			LEFT JOIN Taxon_Dictionary_Term_Version_Mapping tvm
				ON tvm.Term_Version_Key = tvduplicate.Term_Version_Key
			WHERE tvcurrent.Term_Version_Key = @CurrentKey AND tvduplicate.Term_Key = @NewTermKey
				AND tvm.Term_Version_Key IS NULL

			IF @@RowCount > 0
			BEGIN
				-- If we do, only keep the term version if it is already mapped to a taxon
				-- version (VI 24145). Otherwise remove the term version
				IF EXISTS (
					SELECT * FROM Taxon_Dictionary_Term_Version_Mapping
					WHERE Term_Version_Key = @CurrentKey)
				BEGIN
					SET @UpdateTermKey = 1
				END
			END
			ELSE
			BEGIN
				-- If the term version will not duplicate an existing term version when updated,
				-- update it.
				SET @UpdateTermKey = 1
			END
			
			-- Update term version to new term key/delete term version as required. In either
			-- case, the current term version will not be considered in the next iteration of the
			-- while loop since it will either no longer have the old term key or it will have
			-- been deleted.
			IF @UpdateTermKey = 1
			BEGIN
				UPDATE Term_Version
				SET Term_Key = @NewTermKey
				WHERE Term_Version_Key = @CurrentKey
			END
			ELSE
			BEGIN
				UPDATE Concept	
				SET Term_Version_Key = @DuplicateKey
				WHERE Term_Version_Key = @CurrentKey

				UPDATE Thesaurus_Fact	
				SET Term_Version_Key = @DuplicateKey
				WHERE Term_Version_Key = @CurrentKey

				DELETE FROM Term_Version
				WHERE Term_Version_Key = @CurrentKey
			END		
		END

		-- And finally remove the unnecessary leftover term.
		DELETE	Term
		WHERE	Term_Key = @OldTermKey

		SET NOCOUNT ON
	END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_DuplicateTerms_Merge') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_DuplicateTerms_Merge'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_DuplicateTerms_Merge TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_DuplicateTerms_Merge TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_DuplicateTerms_Merge TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_DuplicateTerms_Merge TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_DuplicateTerms_Merge TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_DuplicateTerms_Merge TO [Dev - JNCC SQL]
END
GO