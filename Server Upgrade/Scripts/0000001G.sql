SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptToConceptRelations_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptToConceptRelations_Select]
GO

/*===========================================================================*\
  Description:	Returns the list of relationships of any type that exist 
			between a concept and a list of other concepts.  Both directions are scanned.

  Parameters:	@FromKey 		Concept_Key
							@ToKeys			Concept_Key\Concept_Key etc
							@DoAncestors - if 0, then inherited relationships not found.  If 1, 
							then only inherited relationships returned

  Created:	Dec 2003

  Last revision information:
    $Revision: 1 $
    $Date: 29/11/05 10:18 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptToConceptRelations_Select]
	@FromKey CHAR(16),
	@ToKeys VARCHAR(1600),  -- max 100 links scanned
	@IncludeInherited BIT

AS

SET NOCOUNT ON

DECLARE @CharPos INTEGER

DECLARE @FromConceptKeys TABLE (
  Concept_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS PRIMARY KEY,
	Ancestor BIT,   -- is this a concept in the lineage above the selected concept?
	Concept_Group_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS
)

DECLARE @ToConceptKeys TABLE (
  Concept_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS,
	Leaf_Concept_Key CHAR(16),
	Concept_Group_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS,
	Ancestor BIT,   -- is this a concept in the lineage above the selected concept?
	PRIMARY KEY (Concept_Key, Leaf_Concept_Key)
)

/*===========================================================================*\	
  Create temp tables to store the concept keys we are scanning from and to.
	This includes those in the hierarchy, because relationships can be 
	inherited
\*===========================================================================*/

--Find the current concept groups to aid when filtering lineage
DECLARE @FromConceptGroupKey char(16)
SELECT @FromConceptGroupKey=Concept_Group_Key FROM Concept WHERE Concept_Key=@FromKey

--Store the from concept key, we'll add the ancestors later
INSERT INTO @FromConceptKeys (Concept_Key, Ancestor, Concept_Group_Key) 
VALUES (@FromKey, 0, @FromConceptGroupKey)

-- and retrieve the To keys by parsing the \ separated list
SET @CharPos=1

WHILE @CharPos<LEN(@ToKeys)
BEGIN
  IF SUBSTRING(@ToKeys, @CharPos, 1)='\'
		INSERT INTO @ToConceptKeys (Concept_Key, Leaf_Concept_Key, Concept_Group_Key, Ancestor)
		  SELECT C.Concept_Key, C.Concept_Key, C.Concept_Group_Key, 0
			FROM Concept C
			LEFT JOIN @ToConceptKeys T ON T.Concept_Key=C.Concept_Key
			WHERE C.Concept_Key=SUBSTRING(@ToKeys, @CharPos-16, 16)
			AND T.Concept_Key IS NULL
  SET @CharPos=@CharPos+1
END

-- read the last item which has no \ after it
INSERT INTO @ToConceptKeys (Concept_Key, Leaf_Concept_Key, Concept_Group_Key, Ancestor)
  SELECT C.Concept_Key, C.Concept_Key, C.Concept_Group_Key, 0
	FROM Concept C
	LEFT JOIN @ToConceptKeys T ON T.Concept_Key=C.Concept_Key
	WHERE C.Concept_Key=RIGHT(@ToKeys, 16)
			AND T.Concept_Key IS NULL

/*===========================================================================*\	
	Retrieve the list of lineage concept keys that we need to look at for 
	inherited relationships, for both the From and the To ends.
	Note that Concept_Group_Key is included and any concept group is matched, 
	then the data is filtered afer.  This seems much faster than filtering
	out the concept group key at the start.
\*===========================================================================*/
IF @IncludeInherited=1 
BEGIN
  INSERT INTO @FromConceptKeys (Concept_Key, Ancestor, Concept_Group_Key) 
		SELECT DISTINCT CL2.Concept_Key, 1, C.Concept_Group_Key
		FROM @FromConceptKeys F
		INNER JOIN Concept_Lineage CL1 ON CL1.Concept_Key=F.Concept_Key
		INNER JOIN Concept_Lineage CL2 ON CL2.Lineage = LEFT(CL1.Lineage, LEN(CL2.Lineage))
		INNER JOIN Concept C ON C.Concept_Key=CL2.Concept_Key
		LEFT JOIN @FromConceptKeys F2 ON F2.Concept_Key=CL2.Concept_Key
		WHERE F2.Concept_Key IS NULL 

	INSERT INTO @ToConceptKeys (Concept_Key, Leaf_Concept_Key, Concept_Group_Key, Ancestor)
		SELECT DISTINCT C.Concept_Key, T.Concept_Key, C.Concept_Group_Key, 1
		FROM @ToConceptKeys T
		INNER JOIN Concept_Lineage CL1 ON CL1.Concept_Key=T.Concept_Key
		INNER JOIN Concept_Lineage CL2 ON CL2.Lineage = LEFT(CL1.Lineage, LEN(CL2.Lineage))
		INNER JOIN Concept C ON C.Concept_Key=CL2.Concept_Key
		LEFT JOIN @ToConceptKeys T2 ON T2.Concept_Key=CL2.Concept_Key
				AND T2.Leaf_Concept_Key=T.Concept_Key Collate SQL_Latin1_General_CP1_CI_AS
		WHERE T2.Concept_Key IS NULL 

		DELETE FROM @FromConceptKeys WHERE Concept_Group_Key<>@FromConceptGroupKey

END


SELECT DISTINCT
		'Concept' AS Type, 
		'Forward' AS Direction,
		T.Leaf_Concept_Key AS To_Concept_Key,
		TRT.Thesaurus_Relation_Type_Key, 
		TRT.Item_Name, 
		TRT.Forward_Term, 
		TRT.Reverse_Term
FROM Concept_Relation CR
INNER JOIN Thesaurus_Relation_Type TRT ON TRT.Thesaurus_Relation_Type_Key=CR.Thesaurus_Relation_Type_Key
INNER JOIN @FromConceptKeys F ON From_Concept_Key = F.Concept_Key
INNER JOIN @ToConceptKeys T ON To_Concept_Key=T.Concept_Key
WHERE @IncludeInherited=0 OR (CR.Inherited=1 AND (F.Ancestor=1 OR T.Ancestor=1))
UNION
SELECT 
		'Concept' AS Type, 
		'Reverse' AS Direction,
		T.Leaf_Concept_Key AS To_Concept_Key,
		TRT.Thesaurus_Relation_Type_Key, 
		TRT.Item_Name, 
		TRT.Forward_Term, 
		TRT.Reverse_Term
FROM Concept_Relation CR
INNER JOIN Thesaurus_Relation_Type TRT ON TRT.Thesaurus_Relation_Type_Key=CR.Thesaurus_Relation_Type_Key
INNER JOIN @FromConceptKeys F ON To_Concept_Key = F.Concept_Key
INNER JOIN @ToConceptKeys T ON From_Concept_Key=T.Concept_Key
WHERE @IncludeInherited=0 OR (CR.Inherited=1 AND (F.Ancestor=1 OR T.Ancestor=1))
UNION
SELECT 
		'Meaning' AS Type, 
		'Forward' AS Direction,
		T.Leaf_Concept_Key AS To_Concept_Key,
		TRT.Thesaurus_Relation_Type_Key, 
		TRT.Item_Name, 
		TRT.Forward_Term, 
		TRT.Reverse_Term
FROM Meaning_Relation MR
INNER JOIN Thesaurus_Relation_Type TRT ON TRT.Thesaurus_Relation_Type_Key=MR.Thesaurus_Relation_Type_Key
INNER JOIN Concept CFrom ON CFrom.Meaning_Key=MR.From_Meaning_Key
INNER JOIN Concept CTo ON CTo.Meaning_Key=MR.To_Meaning_Key
INNER JOIN @FromConceptKeys F ON CFrom.Concept_Key = F.Concept_Key
INNER JOIN @ToConceptKeys T ON CTo.Concept_Key=T.Concept_Key
WHERE @IncludeInherited=0 OR (MR.Inherited=1 AND (F.Ancestor=1 OR T.Ancestor=1))
UNION
SELECT 
		'Meaning' AS Type, 
		'Reverse' AS Direction,
		T.Leaf_Concept_Key AS To_Concept_Key,
		TRT.Thesaurus_Relation_Type_Key, 
		TRT.Item_Name, 
		TRT.Forward_Term, 
		TRT.Reverse_Term
FROM Meaning_Relation MR
INNER JOIN Thesaurus_Relation_Type TRT ON TRT.Thesaurus_Relation_Type_Key=MR.Thesaurus_Relation_Type_Key
INNER JOIN Concept CFrom ON CFrom.Meaning_Key=MR.From_Meaning_Key
INNER JOIN Concept CTo ON CTo.Meaning_Key=MR.To_Meaning_Key
INNER JOIN @FromConceptKeys F ON CTo.Concept_Key=F.Concept_Key
INNER JOIN @ToConceptKeys T ON CFrom.Concept_Key=T.Concept_Key
WHERE @IncludeInherited=0 OR (MR.Inherited=1 AND (F.Ancestor=1 OR T.Ancestor=1))
UNION
SELECT 
		'TermVersion' AS Type, 
		'Forward' AS Direction,
		T.Leaf_Concept_Key AS To_Concept_Key,
		TRT.Thesaurus_Relation_Type_Key, 
		TRT.Item_Name, 
		TRT.Forward_Term, 
		TRT.Reverse_Term
FROM Term_Version_Relation TVR
INNER JOIN Thesaurus_Relation_Type TRT ON TRT.Thesaurus_Relation_Type_Key=TVR.Thesaurus_Relation_Type_Key
INNER JOIN Concept CFrom ON CFrom.Term_Version_Key=TVR.From_Term_Version_Key
INNER JOIN Concept CTo ON CTo.Term_Version_Key=TVR.To_Term_Version_Key
INNER JOIN @ToConceptKeys T ON T.Concept_Key=CTo.Concept_Key
WHERE CFrom.Concept_Key=@FromKey
AND @IncludeInherited=0
UNION
SELECT 
		'TermVersion' AS Type, 
		'Reverse' AS Direction,
		T.Leaf_Concept_Key AS To_Concept_Key,
		TRT.Thesaurus_Relation_Type_Key, 
		TRT.Item_Name, 
		TRT.Forward_Term, 
		TRT.Reverse_Term
FROM Term_Version_Relation TVR
INNER JOIN Thesaurus_Relation_Type TRT ON TRT.Thesaurus_Relation_Type_Key=TVR.Thesaurus_Relation_Type_Key
INNER JOIN Concept CFrom ON CFrom.Term_Version_Key=TVR.From_Term_Version_Key
INNER JOIN Concept CTo ON CTo.Term_Version_Key=TVR.To_Term_Version_Key
INNER JOIN @ToConceptKeys T ON T.Concept_Key=CFrom.Concept_Key
WHERE CTo.Concept_Key=@FromKey
AND @IncludeInherited=0

SET NOCOUNT OFF

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptToConceptRelations_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptToConceptRelations_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptToConceptRelations_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptToConceptRelations_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptToConceptRelations_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ConceptToConceptRelations_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptToConceptRelations_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptToConceptRelations_Select TO [Dev - JNCC SQL]
END

GO


/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_TaxonFact_ImportConceptGroup]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_TaxonFact_ImportConceptGroup]
GO

/*===========================================================================*\
  Description:	Import taxon facts corresponding to the facts associated with
				a concept group.

  Parameters:	@job_id					Job identifier

  Created:		Jan 2004

  Last revision information:
	$Revision: 1 $
	$Date: 29/11/05 10:18 $
	$Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_TaxonFact_ImportConceptGroup]
	@job_id				INT
AS
	SET NOCOUNT ON

	DECLARE		@concept_group_key		CHAR(16),
				@thesaurus_fact_key		CHAR(16),
				@taxon_fact_key			CHAR(16),
				@type					VARCHAR(1),
				@taxon_version_key		CHAR(16),
				@entered_by				CHAR(16),
				@entry_date				SMALLDATETIME,
				@changed_by				CHAR(16),
				@changed_date			SMALLDATETIME,
				@source_key				CHAR(16),
				@source_join_key		CHAR(16)

	/* determine parameters of job */
	SELECT		@concept_group_key			=	j.Concept_Group_Key
	FROM		Import_Export_Job			AS	j
	WHERE		j.Import_Export_Job_ID		=	@job_id

	IF @@ROWCOUNT = 0
	BEGIN
		RAISERROR ('Job does not exist or has not been configured', 16, 1)
		GOTO fail
	END

	EXECUTE		usp_Import_Export_Job_UpdateStatus	@job_id,
													'Exporting facts'
	IF @@ERROR <> 0 GOTO fail

	DECLARE		facts	CURSOR LOCAL FAST_FORWARD FOR
	SELECT DISTINCT
				f.Thesaurus_Fact_Key,
				CASE f.Fact_Type_Concept_Key
					WHEN 'SYSTEM00000002NO' THEN 'T' /* HTML */
					WHEN 'SYSTEM00000002L9' THEN 'A' /* AVI */
					WHEN 'SYSTEM00000002L8' THEN 'W' /* WAV */
					WHEN 'SYSTEM00000000W0' THEN 'B' /* Bitmap */
					WHEN 'SYSTEM00000000VY' THEN 'J' /* JPEG */
					ELSE 'T' /* Unknown types mapped to HTML */
				END,
				vm.Taxon_Version_Key,
				es.User_Name_Key,
				CONVERT(SMALLDATETIME,
						CONVERT(VARCHAR, es.Date_Time_Start, 112)),
				cs.User_Name_Key,
				CONVERT(SMALLDATETIME,
						CONVERT(VARCHAR, cs.Date_Time_Start, 112))
	FROM		Concept									AS	c
	INNER JOIN	Thesaurus_Fact							AS	f
	ON			f.Meaning_Key							=	c.Meaning_Key
	INNER JOIN	Taxon_Dictionary_Term_Version_Mapping	AS	vm
	ON			vm.Term_Version_Key						=	c.Term_Version_Key
	INNER JOIN	Session									AS	es
	ON			es.Session_ID							=	f.Entered_Session_ID
	LEFT JOIN	Session									AS	cs
	ON			cs.Session_ID							=	f.Changed_Session_ID
	WHERE		c.Concept_Group_Key						=	@concept_group_key
	AND		c.List_Preferred = 1

	OPEN		facts

	WHILE 1 = 1
	BEGIN
		FETCH		facts
		INTO        @thesaurus_fact_key,
					@type,
					@taxon_version_key,
					@entered_by,
					@entry_date,
					@changed_by,
					@changed_date

		IF @@FETCH_STATUS <> 0 BREAK

		BEGIN TRANSACTION

		SELECT		@taxon_fact_key							=	NULL,
					@source_join_key						=	NULL,
					@source_key								=	NULL

		SELECT		@taxon_fact_key							=	m.Taxon_Fact_Key,
					@source_key								=	j.Source_Key
		FROM		Taxon_Dictionary_Thesaurus_Fact_Mapping	AS	m
		LEFT JOIN	Source_Join								AS	j
		ON			j.Source_Join_Key						=	m.Source_Join_Key
		WHERE		m.Thesaurus_Fact_Key					=	@thesaurus_fact_key

		IF @source_key IS NULL
		BEGIN
			/* there is no existing mapping for the source join; pick an
			 * arbitrary join record (if there are any) and make this the
			 * mapped join.
			 */
			SELECT		@source_join_key	=	Source_Join_Key,
						@source_key			=	Source_Key
			FROM		Source_Join
			WHERE		Record_Key			=	@thesaurus_fact_key
			AND			Table_Name			=	'Thesaurus_Fact'
			ORDER BY	Source_Join_Key
		END

		IF @taxon_fact_key IS NOT NULL
		BEGIN
			/* update existing taxon fact */
			UPDATE		TAXON_FACT
			SET			TITLE						=	tf.Item_Name,
						TYPE						=	CASE WHEN TYPE = 'S' AND @type = 'T'
															THEN 'S'
															ELSE @type
														END,
						DATA						=	tf.Data,
						TAXON_VERSION_KEY			=	@taxon_version_key,
						FACT_VAGUE_DATE_START		=	tf.Fact_Vague_Date_Start,
						FACT_VAGUE_DATE_END			=	tf.Fact_Vague_Date_End,
						FACT_VAGUE_DATE_TYPE		=	tf.Fact_Vague_Date_Type,
						ENTERED_BY					=	@entered_by,
						ENTRY_DATE					=	@entry_date,
						CHANGED_BY					=	@changed_by,
						CHANGED_DATE				=	@changed_date,
						SYSTEM_SUPPLIED_DATA		=	tf.System_Supplied_Data,
						SOURCE_KEY					=	@source_key
			FROM		Thesaurus_Fact				AS	tf,
						TAXON_FACT
			WHERE       tf.Thesaurus_Fact_Key		=	@thesaurus_fact_key
			AND			TAXON_FACT_KEY				=	@taxon_fact_key

			IF @@ERROR <> 0 GOTO fail_from_cursor

			IF @source_join_key IS NOT NULL
			BEGIN
				UPDATE		Taxon_Dictionary_Thesaurus_Fact_Mapping
				SET			Source_Join_Key							=	@source_join_key
				WHERE		Taxon_Fact_Key							=	@taxon_fact_key

				IF @@ERROR <> 0 GOTO fail_from_cursor
			END
		END
		ELSE
		BEGIN
			/* create taxon fact */
			EXECUTE		spNextKey	'TAXON_FACT',
									@taxon_fact_key		OUTPUT
			IF @@ERROR <> 0 GOTO fail_from_cursor

			INSERT		TAXON_FACT (
						TAXON_FACT_KEY,
						TITLE,
						TYPE,
						DATA,
						TAXON_VERSION_KEY,
						FACT_VAGUE_DATE_START,
						FACT_VAGUE_DATE_END,
						FACT_VAGUE_DATE_TYPE,
						ENTERED_BY,
						ENTRY_DATE,
						CHANGED_BY,
						CHANGED_DATE,
						SYSTEM_SUPPLIED_DATA,
						SOURCE_KEY)
			SELECT		@taxon_fact_key,
						tf.Item_Name,
						@type,
						tf.Data,
						@taxon_version_key,
						tf.Fact_Vague_Date_Start,
						tf.Fact_Vague_Date_End,
						tf.Fact_Vague_Date_Type,
						@entered_by,
						@entry_date,
						@changed_by,
						@changed_date,
						tf.System_Supplied_Data,
						@source_key
			FROM        Thesaurus_Fact				AS	tf
			WHERE		tf.Thesaurus_Fact_Key		=	@thesaurus_fact_key

			IF @@ERROR <> 0 GOTO fail_from_cursor

			/* record mapping */
			INSERT		Taxon_Dictionary_Thesaurus_Fact_Mapping (
						Taxon_Fact_Key,
						Thesaurus_Fact_Key,
						Source_Join_Key)
			VALUES		(@taxon_fact_key,
						@thesaurus_fact_key,
						@source_join_key)

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END

		IF @type <> 'T'
		BEGIN
			/* convert "Data" to an HTML anchor; this couldn't be done on the
			 * INSERT/UPDATE above because the '+' operator is not defined
			 * for the "text" data type.
			 */
			DECLARE		@data_ptr		BINARY(16),
						@data_length	INT

			SELECT		@data_ptr		=	TEXTPTR(DATA),
						@data_length	=	DATALENGTH(DATA)
			FROM		TAXON_FACT
			WHERE		TAXON_FACT_KEY	=	@taxon_fact_key

			UPDATETEXT	TAXON_FACT.DATA @data_ptr @data_length 0 '"></a>'
			IF @@ERROR <> 0 GOTO fail_from_cursor

			UPDATETEXT	TAXON_FACT.DATA @data_ptr 0 0 '<a href="'
			IF @@ERROR <> 0 GOTO fail_from_cursor
		END

		EXECUTE		usp_Import_Export_Job_RecordProcessed	@job_id
		IF @@ERROR <> 0 GOTO fail_from_cursor

		COMMIT TRANSACTION
	END

	CLOSE		facts
	DEALLOCATE	facts
	RETURN

fail_from_cursor:
	CLOSE		facts
	DEALLOCATE	facts

fail:
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_TaxonFact_ImportConceptGroup failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TaxonFact_ImportConceptGroup') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_TaxonFact_ImportConceptGroup'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_TaxonFact_ImportConceptGroup TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_TaxonFact_ImportConceptGroup TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_TaxonFact_ImportConceptGroup TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_ThesaurusFact_ImportTaxonList]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_ThesaurusFact_ImportTaxonList]
GO

/*===========================================================================*\
  Description:	Import thesaurus facts corresponding to the facts associated
  				with a taxon list.

  Parameters:	@job_id					Job identifier

  Created:		Dec 2003

  Last revision information:
	$Revision: 1 $
	$Date: 29/11/05 10:18 $
	$Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ThesaurusFact_ImportTaxonList]
	@job_id				INT
AS
	SET NOCOUNT ON

	DECLARE		@taxon_list_key			CHAR(16),
				@taxon_fact_key			CHAR(16),
				@type					VARCHAR(1),
				@meaning_key			CHAR(16),
				@ins_user_key			CHAR(16),
				@ins_date				SMALLDATETIME,
				@ins_session_id			CHAR(16),
				@upd_user_key			CHAR(16),
				@upd_date				SMALLDATETIME,
				@upd_session_id			CHAR(16),
				@system					BIT,
				@source_key				CHAR(16),
				@thesaurus_fact_key		CHAR(16),
				@fact_type_concept_key	CHAR(16),
				@source_join_key		CHAR(16)

	/* determine parameters of job */
	SELECT		@taxon_list_key							=	m.Taxon_List_Key
	FROM		Import_Export_Job						AS	j
	INNER JOIN	Taxon_Dictionary_Concept_Group_Mapping	AS	m
	ON			m.Concept_Group_Key						=	j.Concept_Group_Key
	WHERE		j.Import_Export_Job_ID					=	@job_id

	IF @@ROWCOUNT = 0
	BEGIN
		RAISERROR ('Job does not exist or has not been configured', 16, 1)
		RETURN
	END

	EXECUTE		usp_Import_Export_Job_UpdateStatus	@job_id,
													'Importing facts'
	IF @@ERROR <> 0 RETURN

	DECLARE		@versions	TABLE (
				Taxon_Version_Key	CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS)

	INSERT		@versions
	SELECT		tli.TAXON_VERSION_KEY
	FROM		TAXON_LIST_VERSION						AS	tlv
	INNER JOIN	TAXON_LIST_ITEM							AS	tli
	ON			tli.TAXON_LIST_VERSION_KEY				=	tlv.TAXON_LIST_VERSION_KEY
	WHERE		tlv.TAXON_LIST_KEY						=	@taxon_list_key

	DECLARE		facts	CURSOR LOCAL FAST_FORWARD FOR
	SELECT DISTINCT
				tf.TAXON_FACT_KEY,
				tf.TYPE,
				CASE tf.TYPE
					WHEN 'A' THEN 'SYSTEM00000002L9' /* AVI */
					WHEN 'W' THEN 'SYSTEM00000002L8' /* WAV */
					WHEN 'B' THEN 'SYSTEM00000000W0' /* Bitmap */
					WHEN 'J' THEN 'SYSTEM00000000VY' /* JPEG */
					ELSE 'SYSTEM00000002NO' /* HTML */
				END,
				c.Meaning_Key,
				tf.ENTERED_BY,
				tf.ENTRY_DATE,
				tf.CHANGED_BY,
				tf.CHANGED_DATE,
				tf.SYSTEM_SUPPLIED_DATA,
				tf.SOURCE_KEY
	FROM		@versions								AS	tli
	INNER JOIN	TAXON_FACT								AS	tf
	ON			tf.TAXON_VERSION_KEY					=	tli.TAXON_VERSION_KEY
	INNER JOIN	Taxon_Dictionary_Term_Version_Mapping	AS	m
	ON			m.Taxon_Version_Key						=	tf.TAXON_VERSION_KEY
	INNER JOIN	Concept									AS	c
	ON			c.Term_Version_Key						=	m.Term_Version_Key

	OPEN		facts

	WHILE 1 = 1
	BEGIN
		FETCH		facts
		INTO        @taxon_fact_key,
					@type,
					@fact_type_concept_key,
					@meaning_key,
					@ins_user_key,
					@ins_date,
					@upd_user_key,
					@upd_date,
					@system,
					@source_key

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

		SELECT		@thesaurus_fact_key						=	Thesaurus_Fact_Key,
					@source_join_key						=	Source_Join_Key
		FROM		Taxon_Dictionary_Thesaurus_Fact_Mapping
		WHERE		Taxon_Fact_Key							=	@taxon_fact_key

		IF @@ROWCOUNT > 0
		BEGIN
			/* update existing thesaurus fact */
			UPDATE		Thesaurus_Fact
			SET			Item_Name				=	tf.TITLE,
						Data
							=	CASE
									WHEN @type NOT IN ('A', 'W', 'B', 'J') THEN tf.DATA
									WHEN CHARINDEX('href="', tf.DATA) > 0 THEN
										SUBSTRING(
											tf.DATA,
											CHARINDEX('href="', tf.DATA) + 6,
											CHARINDEX('"', tf.DATA, CHARINDEX('href="', tf.DATA) + 6)
											- (CHARINDEX('href="', tf.DATA) + 6))
									WHEN CHARINDEX('href=''', tf.DATA) > 0 THEN
										SUBSTRING(
											tf.DATA,
											CHARINDEX('href=''', tf.DATA) + 6,
											CHARINDEX('''', tf.DATA, CHARINDEX('href=''', tf.DATA) + 6)
										- (CHARINDEX('href=''', tf.DATA) + 6))
									ELSE SUBSTRING(
											tf.DATA,
											CHARINDEX('href=', tf.DATA) + 5,
											PATINDEX(
												'%[ >]%',
												SUBSTRING(
													tf.DATA,
													CHARINDEX('href=', tf.DATA) + 5,
													DATALENGTH(tf.DATA))) - 1)
								END,
						Meaning_Key				=	@meaning_key,
						Concept_Key				=	NULL,
						Term_Version_Key    	=	NULL,
						Inherited				=	0,
						Fact_Vague_Date_Start	=	tf.FACT_VAGUE_DATE_START,
						Fact_Vague_Date_End		=	tf.FACT_VAGUE_DATE_END,
						Fact_Vague_Date_Type	=	ISNULL(
														tf.FACT_VAGUE_DATE_TYPE,
														'U'),
						Fact_Type_Concept_Key	=	@fact_type_concept_key,
						Entered_Session_ID		=	@ins_session_id,
						Changed_Session_ID		=	@upd_session_id,
						System_Supplied_Data	=	@system
			FROM		TAXON_FACT			   	AS	tf,
						Thesaurus_Fact
			WHERE		tf.TAXON_FACT_KEY		=	@taxon_fact_key
			AND			Thesaurus_Fact_Key		=	@thesaurus_fact_key

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END
		ELSE
		BEGIN
			/* create thesaurus fact */
			EXECUTE		spNextKey	'Thesaurus_Fact',
									@thesaurus_fact_key		OUTPUT
			IF @@ERROR <> 0 GOTO fail_from_cursor

			SET			@source_join_key			=	NULL

			INSERT		Thesaurus_Fact (
						Thesaurus_Fact_Key,
						Item_Name,
						Data,
						Meaning_Key,
						Language_Key,
						Fact_Vague_Date_Start,
						Fact_Vague_Date_End,
						Fact_Vague_Date_Type,
						Fact_Type_Concept_Key,
						Related_Term_Versions,
						Entered_Session_ID,
						Changed_Session_ID,
						System_Supplied_Data)
			SELECT		@thesaurus_fact_key,
						tf.TITLE,
						CASE
							WHEN @type NOT IN ('A', 'W', 'B', 'J') THEN tf.DATA
							WHEN CHARINDEX('href="', tf.DATA) > 0 THEN
								SUBSTRING(
									tf.DATA,
									CHARINDEX('href="', tf.DATA) + 6,
									CHARINDEX('"', tf.DATA, CHARINDEX('href="', tf.DATA) + 6)
									- (CHARINDEX('href="', tf.DATA) + 6))
							WHEN CHARINDEX('href=''', tf.DATA) > 0 THEN
								SUBSTRING(
									tf.DATA,
									CHARINDEX('href=''', tf.DATA) + 6,
									CHARINDEX('''', tf.DATA, CHARINDEX('href=''', tf.DATA) + 6)
								- (CHARINDEX('href=''', tf.DATA) + 6))
							ELSE SUBSTRING(
									tf.DATA,
									CHARINDEX('href=', tf.DATA) + 5,
									PATINDEX(
										'%[ >]%',
										SUBSTRING(
											tf.DATA,
											CHARINDEX('href=', tf.DATA) + 5,
											DATALENGTH(tf.DATA))) - 1)
						END,
						@meaning_key,
						'en',
						tf.FACT_VAGUE_DATE_START,
						tf.FACT_VAGUE_DATE_END,
						ISNULL(tf.FACT_VAGUE_DATE_TYPE, 'U'),
						@fact_type_concept_key,
						0,
						@ins_session_id,
						@upd_session_id,
						@system
			FROM		TAXON_FACT					AS	tf
			WHERE		tf.TAXON_FACT_KEY			=	@taxon_fact_key

			IF @@ERROR <> 0 GOTO fail_from_cursor

			/* record mapping */
			INSERT		Taxon_Dictionary_Thesaurus_Fact_Mapping (
						Taxon_Fact_Key,
						Thesaurus_Fact_Key)
			VALUES		(@taxon_fact_key,
						@thesaurus_fact_key)

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END

		/* make any changes required in Source_Join */
		IF @source_key IS NULL
		BEGIN
			UPDATE		Taxon_Dictionary_Thesaurus_Fact_Mapping
			SET			Source_Join_Key							=	NULL
			WHERE		Taxon_Fact_Key							=	@taxon_fact_key

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END

		EXECUTE		usp_SourceJoin_RecordImported	@source_join_key	OUTPUT,
													'Thesaurus_Fact',
													@thesaurus_fact_key,
													@source_key,
													@ins_session_id,
													@system
		IF @@ERROR <> 0 GOTO fail_from_cursor

		IF @source_key IS NOT NULL
		BEGIN
			UPDATE		Taxon_Dictionary_Thesaurus_Fact_Mapping
			SET			Source_Join_Key							=	@source_join_key
			WHERE		Taxon_Fact_Key							=	@taxon_fact_key

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END

		/* update progress counter */
		EXECUTE		usp_Import_Export_Job_RecordProcessed	@job_id
		IF @@ERROR <> 0 GOTO fail_from_cursor
		
		COMMIT TRANSACTION
	END

	CLOSE		facts
	DEALLOCATE	facts
	RETURN

fail_from_cursor:
	CLOSE		facts
	DEALLOCATE	facts

fail:
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_ThesaurusFact_ImportTaxonList failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ThesaurusFact_ImportTaxonList') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ThesaurusFact_ImportTaxonList'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ThesaurusFact_ImportTaxonList TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ThesaurusFact_ImportTaxonList TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ThesaurusFact_ImportTaxonList TO [Dev - JNCC SQL]
END
GO

