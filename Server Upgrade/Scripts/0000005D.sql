/*===========================================================================*\
  Drop function before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].usp_Concept_GeneratePublishedTerm'))
    DROP PROCEDURE [dbo].usp_Concept_GeneratePublishedTerm
GO

/*===========================================================================*\
  Description:	A wrapper to call the function in the term generator
				table for generating published term

  Parameters:	 

  Created:	August 2011

  Last revision information:
    $Revision: 1 $
    $Date: 2/09/11 16:27 $
    $Author: Jamesbichard $

\*===========================================================================*/

CREATE PROCEDURE dbo.usp_Concept_GeneratePublishedTerm  
	@Plaintext VARCHAR(100),
	@AuthorAndDate VARCHAR(100),
	@Attributes VARCHAR(100),
	@RankKey CHAR(16),
	@ParentKey CHAR(16), 
	@TermGeneratorKey CHAR(16)
AS
BEGIN
	DECLARE @PublishedTermFunction NVARCHAR(257)

	SELECT @PublishedTermFunction = Published_Term_Function
	FROM Term_Generator
	WHERE Term_Generator_Key = @TermGeneratorKey

	
	DECLARE @sql NVARCHAR(4000)
	DECLARE @params NVARCHAR(500)

	SELECT @sql = '
		SELECT ' + @PublishedTermFunction + '(@Plaintext, @AuthorAndDate, @Attributes, @RankKey, @ParentKey)'
	
	SET @params = '@Plaintext NVARCHAR(150), ' +
				'@AuthorAndDate VARCHAR(100), @Attributes VARCHAR(100), ' +
				'@RankKey CHAR(16), @ParentKey CHAR(16)'

	EXEC sp_executesql @sql, @params, 
			@Plaintext = @Plaintext,
			@AuthorAndDate = @AuthorAndDate,
			@Attributes = @Attributes,
			@RankKey = @RankKey,
			@ParentKey = @ParentKey
END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_GeneratePublishedTerm') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Concept_GeneratePublishedTerm'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
			GRANT EXECUTE ON dbo.usp_Concept_GeneratePublishedTerm TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concept_GeneratePublishedTerm TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concept_GeneratePublishedTerm TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Concept_GeneratePublishedTerm TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Concept_GeneratePublishedTerm TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_UpdateAutomaticTerms') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].usp_Concept_UpdateAutomaticTerms
GO

/*===========================================================================*\
  Description:	Update published terms (if required) and search terms for a 
				given concept

  Parameters:	

  Created:	August 2011

  Last revision information:
    $Revision: 1 $
    $Date: 2/09/11 16:27 $
    $Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].usp_Concept_UpdateAutomaticTerms
	@ConceptKey 			CHAR(16),
	@AutomaticUpdate		BIT
AS
	DECLARE @TermGeneratorKey CHAR(16)
	SELECT @TermGeneratorKey = dbo.ufn_GetTermGenerator(@ConceptKey, 0)

	IF @AutomaticUpdate = 1
	BEGIN
		DECLARE @Plaintext VARCHAR(100),
				@ParentKey CHAR(16),
				@AuthorAndDate VARCHAR(100),
				@Attributes VARCHAR(100),
				@ConceptRankKey CHAR(16),
				@PublishedTerm NVARCHAR(450)

		SELECT @Plaintext = t.Plaintext,
				@AuthorAndDate = tv.Author_And_Date,
				@Attributes = tv.Version_Label,	
				@ConceptRankKey = c.Concept_Rank_Key
		FROM Concept c
		INNER JOIN Term t ON c.Term_Key = t.Term_Key
		LEFT JOIN Term_Version tv ON c.Term_Version_Key = tv.Term_Version_Key
		INNER JOIN Concept_Group cg ON c.Concept_Group_Key = cg.Concept_Group_Key
		WHERE c.Concept_Key = @ConceptKey
	
		CREATE TABLE #PublishedTerm (
			Published_Term NVARCHAR(450)
		)
		
		INSERT INTO #PublishedTerm
		EXEC usp_Concept_GeneratePublishedTerm 
			@Plaintext,
			@AuthorAndDate,
			@Attributes,	
			@ConceptRankKey,
			NULL,
			@TermGeneratorKey

		SELECT @PublishedTerm = Published_Term
		FROM #PublishedTerm
	
		DROP TABLE #PublishedTerm

		UPDATE Concept
		SET Published_Term = @PublishedTerm
		WHERE Concept_Key = @ConceptKey
	END

	EXEC usp_SearchTerm_DeleteOldTerms @ConceptKey
	EXEC usp_SearchTerm_Generate @ConceptKey, @TermGeneratorKey

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/

	PRINT 'Setting up security on procedure usp_Concept_UpdateAutomaticTerms'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_Concept_UpdateAutomaticTerms TO R2k_AddOnly
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concept_UpdateAutomaticTerms TO R2k_Administrator
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concept_UpdateAutomaticTerms TO R2k_FullEdit
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Concept_UpdateAutomaticTerms TO R2k_ReadOnly
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Concept_UpdateAutomaticTerms TO R2k_RecordCardsOnly
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
		GRANT EXECUTE ON dbo.usp_Concept_UpdateAutomaticTerms TO "Dev - JNCC SQL"

GO

SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_PostImport_Collections]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_PostImport_Collections]
GO

/*===========================================================================*\
  Description:	Recreate constraints that impede import of collections data

  Created:	Oct 2004

  Last revision information:
    $Revision: 1 $
    $Date: 2/09/11 16:27 $
    $Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_PostImport_Collections]
AS

IF NOT EXISTS (select * from dbo.sysobjects where id = object_id(N'[dbo].[FK_Specimen_Unit_Determination]') and OBJECTPROPERTY(id, N'IsForeignKey') = 1)
ALTER TABLE [dbo].[Specimen_Unit] ADD 
	CONSTRAINT [FK_Specimen_Unit_Determination] FOREIGN KEY 
	(
		[Preferred_Determination_Key]
	) REFERENCES [dbo].[Determination] (
		[Determination_Key]
	)

IF NOT EXISTS (select * from dbo.sysobjects where id = object_id(N'[dbo].[FK_Specimen_Unit_TAXON_DETERMINATION]') and OBJECTPROPERTY(id, N'IsForeignKey') = 1)
ALTER TABLE [dbo].[Specimen_Unit] ADD 
	CONSTRAINT [FK_Specimen_Unit_TAXON_DETERMINATION] FOREIGN KEY 
	(
		[Preferred_Taxon_Determination_Key]
	) REFERENCES [dbo].[TAXON_DETERMINATION] (
		[TAXON_DETERMINATION_KEY]
	)

/*===========================================================================*\
  Create indexes on VW_SpecimenDetsLife that we dropped pre-import
\*===========================================================================*/
SET ARITHABORT ON 
SET NUMERIC_ROUNDABORT OFF

CREATE UNIQUE CLUSTERED INDEX VW_SpecimenDetsLife
ON [dbo].[VW_SpecimenDetsLife] (Collection_Unit_Key, Taxon_Determination_Key)

CREATE INDEX IX_DeterminationKey ON [dbo].[VW_SpecimenDetsLife] (Taxon_List_Item_Key)

SET NUMERIC_ROUNDABORT OFF 
SET ARITHABORT  OFF 

GO


/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_PostImport_Collections') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_PostImport_Collections'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_PostImport_Collections TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_PostImport_Collections TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_PostImport_Collections TO [Dev - JNCC SQL]
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
	$Date: 2/09/11 16:27 $
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
	from (
		select
			taxon_list_item_key,
			max(synonym_taxon_list_item_key) as synonym_taxon_list_item_key,
			priority
		from #PreferredListSynonyms
		group by taxon_list_item_key, priority) as ps
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
	   WHERE  Id = Object_Id(N'[dbo].[usp_QEDataItem_Insert_ForMultiValues]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_QEDataItem_Insert_ForMultiValues]
GO

/*===========================================================================*\
  Description:	Inserts a record into the QE_Data_Item table

  Parameters:	@Key	Data Item key

  Created:	October 2007

  Last revision information:
    $Revision: 1 $
    $Date: 2/09/11 16:27 $
    $Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_QEDataItem_Insert_ForMultiValues]
	@DataItemKey INT OUTPUT,
	@DataRowKey INT,
	@TemplateFieldKey CHAR(16),
	@DataValue as VARCHAR(200),
	@DataDisplay as VARCHAR(200),
	@Position INT,
	@SessionID as CHAR(16)
AS
INSERT INTO QE_Data_Item (QE_Data_Row_Key, QE_Template_Field_Key,
	Data_Value, Data_Display, Position, Entered_Session_ID)
VALUES(@DataRowKey, @TemplateFieldKey, @DataValue, @DataDisplay, @Position, @SessionID)

SELECT @DataItemKey = SCOPE_IDENTITY()
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_QEDataItem_Insert_ForMultiValues') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_QEDataItem_Insert_ForMultiValues'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_QEDataItem_Insert_ForMultiValues TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_QEDataItem_Insert_ForMultiValues TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_QEDataItem_Insert_ForMultiValues TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_QEDataItem_Insert_ForMultiValues TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_QEDataItem_Insert_ForMultiValues TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_QEDataItem_Insert_ForMultiValues TO [Dev - JNCC SQL]
END

GO