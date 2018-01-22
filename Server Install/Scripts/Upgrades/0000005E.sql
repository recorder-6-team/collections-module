SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

/*============================================================================*\
	Drop stored proc before re-creating.
\*============================================================================*/
IF OBJECT_ID(N'dbo.usp_Search_Term_GenerateTermsUsingDefaultRule') IS NOT NULL
	DROP PROCEDURE dbo.usp_Search_Term_GenerateTermsUsingDefaultRule
GO

/*============================================================================*\
	Description:
		The default stored procedure used to generate the search terms for a
		concept.

	Parameters:
		@Plaintext:			The base term.
		@AuthorAndDate:		The author and date.
		@Attributes:		Additional attributes.
		@RankKey:			Identifies the rank of the concept.
		@ParentConceptKey:	Identifies the parent concept, if any.
		@PublishedTerm:		The published term.

	Created: July 2011

	Last revision information:
		$Revision: 3 $
		$Date: 9/12/11 15:12 $
		$Author: Simonwood $
\*============================================================================*/

CREATE PROCEDURE dbo.usp_Search_Term_GenerateTermsUsingDefaultRule
	@Plaintext NVARCHAR(150),
	@AuthorAndDate VARCHAR(100),
	@Attributes VARCHAR(100),
	@RankKey CHAR(16),
	@ParentConceptKey CHAR(16),
	@PublishedTerm NVARCHAR(256)
AS
	SET NOCOUNT ON
	
	DECLARE @SearchTerm TABLE
	(
		Search_Term NVARCHAR(450)
	)

	INSERT INTO	@SearchTerm
	SELECT		@Plaintext

	IF @PublishedTerm <> @Plaintext
	BEGIN
		INSERT INTO	@SearchTerm
		SELECT		dbo.ufn_RemoveHtmlMarkup(@PublishedTerm)
	END
	
	IF @AuthorAndDate IS NOT NULL AND LEN(@AuthorAndDate) > 0
	BEGIN
		INSERT INTO	@SearchTerm
		SELECT		@Plaintext + ' ' + @AuthorAndDate
	END

	IF @Attributes IS NOT NULL AND LEN(@Attributes) > 0
	BEGIN
		INSERT INTO	@SearchTerm
		SELECT		@Plaintext + ' ' + @Attributes
	END

	IF @AuthorAndDate IS NOT NULL AND LEN(@AuthorAndDate) > 0 
		AND @Attributes IS NOT NULL AND LEN(@Attributes) > 0
	BEGIN
		INSERT INTO	@SearchTerm
		SELECT		@Plaintext + ' ' + @AuthorAndDate + ' ' + @Attributes
	END

	SELECT DISTINCT
		Search_Term
	FROM @SearchTerm
GO

/*============================================================================*\
	Grant permissions.
\*============================================================================*/
PRINT 'Setting up security on procedure usp_Search_Term_GenerateTermsUsingDefaultRule'

IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
    GRANT EXECUTE ON dbo.usp_Search_Term_GenerateTermsUsingDefaultRule TO R2k_AddOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
	GRANT EXECUTE ON dbo.usp_Search_Term_GenerateTermsUsingDefaultRule TO R2k_Administrator
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
	GRANT EXECUTE ON dbo.usp_Search_Term_GenerateTermsUsingDefaultRule TO R2k_FullEdit
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
	GRANT EXECUTE ON dbo.usp_Search_Term_GenerateTermsUsingDefaultRule TO R2k_ReadOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
	GRANT EXECUTE ON dbo.usp_Search_Term_GenerateTermsUsingDefaultRule TO R2k_RecordCardsOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
    GRANT EXECUTE ON dbo.usp_Search_Term_GenerateTermsUsingDefaultRule TO "Dev - JNCC SQL"
GO
/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_TaxonVersion_ImportConceptGroup]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_TaxonVersion_ImportConceptGroup]
GO

/*===========================================================================*\
  Description:	Import taxon versions corresponding to items in a concept
				group.

  Parameters:	@job_id					Job identifier

  Created:		Dec 2003

  Last revision information:
	$Revision: 3 $
	$Date: 9/12/11 15:12 $
	$Author: Simonwood $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_TaxonVersion_ImportConceptGroup]
	@job_id					INT,
	@SessionID			CHAR(16)
AS
	SET NOCOUNT ON

 DECLARE     @concept_group_key			CHAR(16),
				@term_version_key			CHAR(16),
				@taxon_key					CHAR(16),
				@attribute					VARCHAR(65),
				@authority					VARCHAR(40),
				@entered_by					CHAR(16),
				@entry_date					SMALLDATETIME,
				@changed_by					CHAR(16),
				@changed_date				SMALLDATETIME,
				@system						BIT,
				@taxon_version_key			CHAR(16),
				@source_key					CHAR(16),
				@source_join_key			CHAR(16),
				@prior_term_version_key		CHAR(16),
				@concept_key	CHAR(16),
				@term_key		CHAR(16)


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
													'Exporting term versions'
	IF @@ERROR <> 0 GOTO fail
 

	DECLARE		versions	CURSOR LOCAL FAST_FORWARD FOR
	SELECT DISTINCT
				tv.Term_Version_Key,
				tm.Taxon_Key,
				tv.Version_Label,
				tv.Author_And_Date,
				es.User_Name_Key,
				CONVERT(SMALLDATETIME,
						CONVERT(VARCHAR, es.Date_Time_Start, 112)),
				cs.User_Name_Key,
				CONVERT(SMALLDATETIME,
						CONVERT(VARCHAR, cs.Date_Time_Start, 112)),
				ISNULL(tv.System_Supplied_Data, c.System_Supplied_Data),
				c.Concept_Key,
				tm.Term_key
	FROM		Concept							AS	c
	LEFT JOIN	Term_Version					AS	tv
	ON			tv.Term_Version_Key				=	c.Term_Version_Key
	INNER JOIN	Taxon_Dictionary_Term_Mapping	AS	tm
	ON			tm.Term_Key						=	c.Term_Key
	INNER JOIN	Session							AS	es
	ON			es.Session_ID					=	 ISNULL(tv.Entered_Session_ID, @SessionID)
	LEFT JOIN	Session							AS	cs
	ON			cs.Session_ID					=	tv.Changed_Session_ID
	WHERE		c.Concept_Group_Key				=	@concept_group_key
	ORDER BY	tv.Term_Version_Key

	SET			@prior_term_version_key			=	''

	OPEN		versions

	WHILE 1 = 1
	BEGIN
		FETCH		versions
		INTO		@term_version_key,
					@taxon_key,
					@attribute,
					@authority,
					@entered_by,
					@entry_date,
					@changed_by,
					@changed_date,
					@system,
					@concept_key,
					@term_key

		IF @@FETCH_STATUS <> 0 BREAK

		BEGIN TRANSACTION

		SELECT      @taxon_version_key						=	NULL,
					@source_join_key						=	NULL,
					@source_key								=	NULL

		SELECT		@taxon_version_key						=	m.Taxon_Version_Key,
					@source_key								=	j.Source_Key
		FROM		Taxon_Dictionary_Term_Version_Mapping	AS	m
		LEFT JOIN	Source_Join								AS	j
		ON			j.Source_Join_Key						=	m.Source_Join_Key
		WHERE		m.Term_Version_Key						=	@term_version_key

		IF @source_key IS NULL
		BEGIN
			/* there is no existing mapping for the source join; pick an
			 * arbitrary join record (if there are any) and make this the
			 * mapped join.
			 */
			SELECT		@source_join_key	=	Source_Join_Key,
						@source_key			=	Source_Key
			FROM		Source_Join
			WHERE		Record_Key			=	@term_version_key
			AND			Table_Name			=	'Term_Version'
			ORDER BY	Source_Join_Key
		END

		IF @taxon_version_key IS NOT NULL
		BEGIN
			/* update taxon version */
			UPDATE		TAXON_VERSION
			SET			TAXON_KEY				=	@taxon_key,
						ATTRIBUTE				=	@attribute,
						AUTHORITY				=	@authority,
						SOURCE_KEY				=	@source_key,
						ENTERED_BY				=	@entered_by,
						ENTRY_DATE				=	@entry_date,
						CHANGED_BY				=	@changed_by,
						CHANGED_DATE			=	@changed_date,
						SYSTEM_SUPPLIED_DATA   	=	@system
			WHERE		TAXON_VERSION_KEY		=	@taxon_version_key

			IF @@ERROR <> 0 GOTO fail_from_cursor

			IF @source_join_key IS NOT NULL
			BEGIN
				UPDATE		Taxon_Dictionary_Term_Version_Mapping
				SET			Source_Join_Key							=	@source_join_key
				WHERE		Taxon_Version_Key						=	@taxon_version_key

				IF @@ERROR <> 0 GOTO fail_from_cursor
			END
		END
		ELSE
		BEGIN
			/* create taxon version */
			EXECUTE		spNextKey		'TAXON_VERSION',
										@taxon_version_key	OUTPUT
			IF @@ERROR <> 0 GOTO fail_from_cursor

			INSERT		TAXON_VERSION (
						TAXON_VERSION_KEY,
						TAXON_KEY,
						ATTRIBUTE,
						AUTHORITY,
						UK_NATIVE,
						SOURCE_KEY,
						ENTERED_BY,
						ENTRY_DATE,
						CHANGED_BY,
						CHANGED_DATE,
						SYSTEM_SUPPLIED_DATA)
			VALUES		(@taxon_version_key,
						@taxon_key,
						@attribute,
						@authority,
						0,
						@source_key,
						@entered_by,
						@entry_date,
						@changed_by,
						@changed_date,
						@system)

			IF @@ERROR <> 0 GOTO fail_from_cursor

			/* record mapping */
			INSERT		Taxon_Dictionary_Term_Version_Mapping (
						Taxon_Version_Key,
						Term_Version_Key,
						Source_Join_Key)
			VALUES		(@taxon_version_key,
						@term_version_key,
						@source_join_key)

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END

		IF @term_version_key <> @prior_term_version_key
		BEGIN
			/* Use of @prior_term_version_key is a hack for the case where
			 * a single Term corresponds to multiple Taxon records; we don't
			 * increment the progress count until all the taxa have been
			 * considered.
			 */
			SET			@prior_term_version_key		=	@term_version_key

		END

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
	RAISERROR ('usp_TaxonVersion_ImportConceptGroup failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TaxonVersion_ImportConceptGroup') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_TaxonVersion_ImportConceptGroup'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_TaxonVersion_ImportConceptGroup TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_TaxonVersion_ImportConceptGroup TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_TaxonVersion_ImportConceptGroup TO [Dev - JNCC SQL]
END
GO
/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_Taxon_ImportConceptGroup]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_Taxon_ImportConceptGroup]
GO

/*===========================================================================*\
  Description:	Import taxa corresponding to terms in a concept group.

  Parameters:	@job_id					Job identifier

  Created:		Dec 2003

  Last revision information:
	$Revision: 3 $
	$Date: 9/12/11 15:12 $
	$Author: Simonwood $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Taxon_ImportConceptGroup]
	@job_id				CHAR(16)
AS
	SET NOCOUNT ON

	DECLARE     @concept_group_key		CHAR(16),
				@term_key				CHAR(16),
				@italic					BIT,
				@item_name				VARCHAR(60),
				@authority				VARCHAR(65),
				@language				VARCHAR(2),
				@taxon_name_type_key	CHAR(16),
				@entered_by				CHAR(16),
				@entry_date				SMALLDATETIME,
				@changed_by				CHAR(16),
				@changed_date			SMALLDATETIME,
				@system					BIT,
				@Taxon_Key				CHAR(16)

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
													'Exporting terms'
	IF @@ERROR <> 0 GOTO fail

	DECLARE		terms	CURSOR LOCAL FAST_FORWARD FOR
	SELECT DISTINCT
				t.Term_Key,
				CASE WHEN PATINDEX('%<i>%', C.Published_Term) <> 0
					THEN 1
					ELSE 0
				END,
				REPLACE(REPLACE(c.Published_Term, '<i>', ''), '</i>', '') COLLATE SQL_Latin1_General_CP1_CI_AS,
				tv.Author_And_Date,
				t.Language_Key,
				tnt.Taxon_Name_Type_Key,
				ISNULL(es.User_Name_Key, 'NBNSYS0000000004') AS User_Name_Key,
				ISNULL(CONVERT(SMALLDATETIME,
						CONVERT(VARCHAR, es.Date_Time_Start, 112)), GetDate()),
				cs.User_Name_Key,
				CONVERT(SMALLDATETIME,
						CONVERT(VARCHAR, cs.Date_Time_Start, 112)),
				t.System_Supplied_Data
	FROM		Concept								AS	c
	INNER JOIN	Term								AS	t
	ON			t.Term_Key							=	c.Term_Key
	LEFT JOIN	Term_Version						AS	tv
	ON			tv.Term_Version_Key					=	c.Term_Version_Key
	INNER JOIN	Taxon_Dictionary_Name_Type_Mapping	AS	tnt
	ON			tnt.Thesaurus_Name_Type_Key			=	c.Name_Type_Concept_Key
	LEFT JOIN	Session								AS	es
	ON			es.Session_ID						=	t.Entered_Session_ID
	LEFT JOIN	Session								AS	cs
	ON			cs.Session_ID						=	t.Changed_Session_ID
	WHERE		c.Concept_Group_Key					=	@concept_group_key

	OPEN		terms

	WHILE 1 = 1
	BEGIN
		FETCH		terms
		INTO		@term_key,
					@italic,
					@item_name,
					@authority,
					@language,
					@taxon_name_type_key,
					@entered_by,
					@entry_date,
					@changed_by,
					@changed_date,
					@system

		IF @@FETCH_STATUS <> 0 BREAK

		BEGIN TRANSACTION

		SELECT		@taxon_key						=	tdm.Taxon_Key
		FROM		Taxon_Dictionary_Term_Mapping	AS	tdm
		INNER JOIN	TAXON							AS	tx
		ON			tx.TAXON_KEY					=	tdm.Taxon_Key
		WHERE		tdm.Term_Key					=	@term_key
		AND			tx.TAXON_NAME_TYPE_KEY			=	@taxon_name_type_key

		IF @@ROWCOUNT > 0
		BEGIN
			/* update taxon */
			UPDATE		TAXON
			SET			ITEM_NAME					=	@item_name,
						AUTHORITY					=	@authority,
						LANGUAGE					=	@language,
						ENTERED_BY					=	@entered_by,
						ENTRY_DATE					=	@entry_date,
						CHANGED_BY					=	@changed_by,
						CHANGED_DATE				=	@changed_date,
						SYSTEM_SUPPLIED_DATA		=	@system
			WHERE		TAXON_KEY					=	@taxon_key

			UPDATE		Taxon_Dictionary_Term_Mapping
			SET			Italic_Font = @italic
			WHERE		Taxon_Key = @taxon_key AND Term_Key = @term_key

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END
		ELSE
		BEGIN
			/* create new taxon */
			EXECUTE		spNextKey	'TAXON',
									@taxon_key	OUTPUT
			IF @@ERROR <> 0 GOTO fail_from_cursor

			INSERT		TAXON (
						TAXON_KEY,
						ITEM_NAME,
						AUTHORITY,
						LANGUAGE,
						TAXON_NAME_TYPE_KEY,
						ENTERED_BY,
						ENTRY_DATE,
						CHANGED_BY,
						CHANGED_DATE,
						SYSTEM_SUPPLIED_DATA)
			VALUES		(@taxon_key,
						@item_name,
						@authority,
						@language,
						@taxon_name_type_key,
						@entered_by,
						@entry_date,
						@changed_by,
						@changed_date,
						@system)

			IF @@ERROR <> 0 GOTO fail_from_cursor

			/* record mapping */
			INSERT		Taxon_Dictionary_Term_Mapping (
						Taxon_Key,
						Italic_Font,
						Term_Key)
			VALUES		(@taxon_key,
						@italic,
						@term_key)

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END

		EXECUTE		usp_Import_Export_Job_RecordProcessed	@job_id
		IF @@ERROR <> 0 GOTO fail_from_cursor

		COMMIT TRANSACTION
	END

	CLOSE		terms
	DEALLOCATE	terms
	RETURN

fail_from_cursor:
	CLOSE		terms
	DEALLOCATE	terms

fail:
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_Taxon_ImportConceptGroup failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Taxon_ImportConceptGroup') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Taxon_ImportConceptGroup'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Taxon_ImportConceptGroup TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Taxon_ImportConceptGroup TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Taxon_ImportConceptGroup TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_PotentialSynonyms_Select_ForConcept]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_PotentialSynonyms_Select_ForConcept]
GO

/*===========================================================================*\
  Description:	Returns List Synonyms

  Parameters:	@Key	Concept_Key

  Created:	December 2003

  Last revision information:
    $Revision: 3 $
    $Date: 9/12/11 15:12 $
    $Author: Simonwood $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_PotentialSynonyms_Select_ForConcept]
	@Key char(16)
AS

SET NOCOUNT ON
SET ANSI_NULLS ON
SET ANSI_PADDING ON
SET ANSI_WARNINGS ON
SET CONCAT_NULL_YIELDS_NULL ON
SET QUOTED_IDENTIFIER ON
SET NO_BROWSETABLE OFF

	/*=============================*\
	  Actually get the results set.
	\*=============================*/
	SELECT DISTINCT
			CPotentials.Concept_Key AS Item_Key, 
			CT.Item_Name + ' (' + CG.Item_Name + ')' AS Item_Name
	FROM 		Concept AS CSource
	INNER JOIN 	Concept AS CSynonyms 		ON CSynonyms.Meaning_Key = CSource.Meaning_Key
	INNER JOIn	Term AS TSource			ON TSource.Term_Key = CSource.Term_Key
	INNER JOIN 	Term AS TSynonyms 		ON TSynonyms.Term_Key = CSynonyms.Term_Key
	INNER JOIN 	Term AS TPotentials 		ON TPotentials.Plaintext = TSynonyms.Plaintext
							AND TPotentials.Language_Key = TSource.Language_Key
	INNER JOIN 	Concept AS CPotentials 		ON CPotentials.Term_Key = TPotentials.Term_Key
	LEFT JOIN 	Concept AS CExclude		ON CExclude.Concept_Key = CPotentials.Concept_Key
							AND CExclude.Meaning_Key = CSource.Meaning_Key
	LEFT JOIN Homonym_Pair AS H	ON (H.Meaning_Key_1	= CPotentials.Meaning_Key
								AND	H.Meaning_Key_2	= CSource.Meaning_Key)
								OR (H.Meaning_Key_1 = CSource.Meaning_Key
								AND	H.Meaning_Key_2 = CPotentials.Meaning_Key)
	INNER JOIN 	VW_ConceptTerm AS CT 	ON CT.Concept_Key = CPotentials.Concept_Key
							AND CT.Concept_Key <> CSynonyms.Concept_Key
	LEFT JOIN	Term_Version AS TV 		ON TV.Term_Version_Key = CPotentials.Term_Version_Key
	INNER JOIN	Concept_Group AS CG 		ON CG.Concept_Group_Key = CPotentials.Concept_Group_Key
	WHERE 		CSource.Concept_Key = @Key
	AND 		CExclude.Concept_Key IS NULL
	AND			H.Meaning_Key_1 IS NULL	
SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_PotentialSynonyms_Select_ForConcept') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_PotentialSynonyms_Select_ForConcept'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_PotentialSynonyms_Select_ForConcept TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_PotentialSynonyms_Select_ForConcept TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_PotentialSynonyms_Select_ForConcept TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_PotentialSynonyms_Select_ForConcept TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_PotentialSynonyms_Select_ForConcept TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_PotentialSynonyms_Select_ForConcept TO [Dev - JNCC SQL]
END

GO

SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

/*============================================================================*\
	Drop function before re-creating.
\*============================================================================*/
IF OBJECT_ID(N'dbo.ufn_GetTermGenerator') IS NOT NULL
	DROP FUNCTION dbo.ufn_GetTermGenerator
GO

/*============================================================================*\
	Description:
		Loops through concept hierarchy, concept group, local domain and
		domain to get the term generator key for a concept.

	Created: July 2011

	Last revision information:
		$Revision: 3 $
		$Date: 9/12/11 15:12 $
		$Author: Simonwood $
\*============================================================================*/

CREATE FUNCTION dbo.ufn_GetTermGenerator(
	@Key CHAR(16),
	@IsConceptGroupKey BIT,
	@GetFromParent BIT
)
RETURNS CHAR(16)
AS
BEGIN
	DECLARE @TermGeneratorKey CHAR(16)
	DECLARE @ConceptGroupKey CHAR(16)
	DECLARE @LocalDomainKey CHAR(16)
	DECLARE @DomainKey CHAR(16)

	IF @IsConceptGroupKey = 0
	BEGIN
		--Check if concept has a defined published term rule
		SELECT 
			@TermGeneratorKey = Term_Generator_Key, 
			@ConceptGroupKey = Concept_Group_Key
		FROM Concept
		WHERE Concept_Key = @Key

		--If rule is undefined, look for the first term generator as one proceeds up
		--through the ancestors of the concept. Do this also if specified to get key
		--from parent - for cases term is being edited, and a term generator is stored
		--in DB but the user has selected to inherit rule in the UI.
		IF @TermGeneratorKey IS NULL OR @GetFromParent = 1
		BEGIN
			SELECT @TermGeneratorKey = (
				SELECT TOP 1 crelated.term_generator_key
				FROM concept c 
				LEFT JOIN concept_lineage cl on cl.concept_key = c.concept_key
				INNER JOIN (
					SELECT 
						cl1.lineage, 
						cl1.lineage_id, 
						c1.concept_group_key, 
						c1.concept_key,
						c1.automatic_published_term,
						c1.term_generator_key,	
						c1.published_term
					FROM concept_lineage cl1
					INNER JOIN concept c1 on c1.concept_key = cl1.concept_key 
				) as crelated ON cl.lineage LIKE crelated.lineage + '\%' 
						AND c.concept_group_key = crelated.concept_group_key
				WHERE c.concept_key = @Key and crelated.term_generator_key IS NOT NULL
				ORDER BY crelated.lineage DESC
			)
			
			--If we still don't have a term generator, start the search again using the concept
			--group key instead.
			IF @TermGeneratorKey IS NULL
			BEGIN
				RETURN dbo.ufn_GetTermGenerator(@ConceptGroupKey, 1, 0)
			END		
		END
	END
	ELSE
	BEGIN
		--Look for concept group rule
		SELECT @TermGeneratorKey = Term_Generator_Key,
				@LocalDomainKey = Local_Domain_Key
		FROM Concept_Group	
		WHERE Concept_Group_Key = @Key	
		
		--If rule is still undefined, look for local domain rule
		IF @TermGeneratorKey IS NULL
		BEGIN
			SELECT @TermGeneratorKey = Term_Generator_Key,
					@DomainKey = Domain_Key
			FROM Local_Domain
			WHERE Local_Domain_Key = @LocalDomainKey

			--If rule is still undefined, look for domain rule
			IF @TermGeneratorKey IS NULL
			BEGIN
				SELECT @TermGeneratorKey = Term_Generator_Key
				FROM Domain
				WHERE Domain_Key = @DomainKey					
			END
		END
	END

	--If term generator is still undefined, use the default
	IF @TermGeneratorKey IS NULL
	BEGIN
		SELECT @TermGeneratorKey = Term_Generator_Key
		FROM Term_Generator
		WHERE Item_Name = 'System default rule'
	END

	RETURN @TermGeneratorKey

END
GO

/*============================================================================*\
	Grant permissions.
\*============================================================================*/
PRINT 'Setting up security on function ufn_GetTermGenerator'

IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
    GRANT EXECUTE ON dbo.ufn_GetTermGenerator TO R2k_AddOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
	GRANT EXECUTE ON dbo.ufn_GetTermGenerator TO R2k_Administrator
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
	GRANT EXECUTE ON dbo.ufn_GetTermGenerator TO R2k_FullEdit
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
	GRANT EXECUTE ON dbo.ufn_GetTermGenerator TO R2k_ReadOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
	GRANT EXECUTE ON dbo.ufn_GetTermGenerator TO R2k_RecordCardsOnly
IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
    GRANT EXECUTE ON dbo.ufn_GetTermGenerator TO "Dev - JNCC SQL"
GO