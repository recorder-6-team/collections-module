/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_SeasonNames_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_SeasonNames_Select]
GO

/*===========================================================================*\
  Description:	Returns a list of season names according to preferred language.

  Parameters:	@Language	The ISO abbreviation of language in use on 
				client machine.

  Created:	March 2004

  Last revision information:
    $Revision: 2 $
    $Date: 23/02/07 12:01 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SeasonNames_Select]
	@Language varchar(4)
AS
 	SET NOCOUNT ON

	-- Get the list of seasons concepts
	SELECT Concept_Key, Concept_Group_Key, Sort_Code, Meaning_Key, 
		CAST('' AS VARCHAR(200)) AS Plaintext
	INTO #Seasons
	FROM Concept
	WHERE Concept_Group_Key = 'SYSTEM000000000R'	-- Season names
	AND List_Preferred=1
	
	-- Find the season terms in the preferred language
	UPDATE C1
	SET C1.Plaintext=T.PlainText
	FROM #Seasons C1
	JOIN 	Concept C2 ON C2.Meaning_Key = C1.Meaning_Key
	JOIN 	Term T ON T.Term_Key = C2.Term_Key
	JOIN 	Language L ON L.Language_Key = T.Language_Key
		AND	L.Language_Key = @Language
	WHERE 	C1.Concept_Group_Key = 'SYSTEM000000000R'	-- Season names
	
	-- Now find the season terms in English if preferred language not available
	UPDATE C1
	SET C1.Plaintext=T.PlainText
	FROM #Seasons C1
	JOIN 	Concept C2 ON C2.Meaning_Key = C1.Meaning_Key
	JOIN 	Term T ON T.Term_Key = C2.Term_Key
	JOIN 	Language L ON L.Language_Key = T.Language_Key
	WHERE 	C1.Concept_Group_Key = 'SYSTEM000000000R'	-- Season names
		AND	L.Language_Key = 'en'
		AND C1.Plaintext = ''
	
	SELECT Concept_Key, Plaintext 
	FROM #Seasons
	ORDER BY Sort_Code
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SeasonNames_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SeasonNames_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SeasonNames_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SeasonNames_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SeasonNames_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_SeasonNames_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SeasonNames_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SeasonNames_Select TO [Dev - JNCC SQL]
END
GO


/*===========================================================================*\
  Drop function before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_MonthToSeason]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	DROP FUNCTION ufn_MonthToSeason
GO

/*===========================================================================*\
  Description:	Converts a Month into a Season

  Parameters:	@Month		- Month number

  Created:	18 September 2003

  Last revision information:
    $Revision: 2 $
    $Date: 23/02/07 12:01 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE FUNCTION dbo.ufn_MonthToSeason(
  @Month INT
)
RETURNS varchar(150)

AS
BEGIN

	DECLARE @Output VARCHAR(6)
	DECLARE @Meaning_Key CHAR(16)

	IF @Month=1 OR @Month=2 OR @Month=12 
	  SET @Meaning_Key = 'SYSTEM000000008E'
	ELSE IF @Month=3 OR @Month=4 OR @Month=5
	  SET @Meaning_Key = 'SYSTEM000000008F'
	ELSE IF @Month=6 OR @Month=7 OR @Month=8
	  SET @Meaning_Key = 'SYSTEM000000008G'
	ELSE IF @Month=9 OR @Month=10 OR @Month=11
	  SET @Meaning_Key = 'SYSTEM000000008H'
	
	SELECT TOP 1 @Output = T.PlainText 
	FROM Concept C
	INNER JOIN Term T ON T.Term_Key=C.Term_Key
	INNER JOIN Language L ON l.Language_Key=T.Language_Key
	WHERE Meaning_Key = @Meaning_Key
	ORDER BY L.Priority ASC

	RETURN @Output

END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[ufn_MonthToSeason]')
	   AND    Type IN ('FN', 'IF', 'TF'))
	BEGIN
    	PRINT 'Setting up security on function ufn_MonthToSeason'
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
	        	GRANT EXECUTE ON dbo.ufn_MonthToSeason TO [R2k_AddOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
			GRANT EXECUTE ON dbo.ufn_MonthToSeason TO [R2k_Administrator]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
			GRANT EXECUTE ON dbo.ufn_MonthToSeason TO [R2k_FullEdit]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
			GRANT EXECUTE ON dbo.ufn_MonthToSeason TO [R2k_ReadOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
			GRANT EXECUTE ON dbo.ufn_MonthToSeason TO [R2k_RecordCardsOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
	        	GRANT EXECUTE ON dbo.ufn_MonthToSeason TO [Dev - JNCC SQL]
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
	$Revision: 2 $
	$Date: 23/02/07 12:01 $
	$Author: Johnvanbreda $

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
				CASE WHEN LEFT(t.Item_Name, 3) = '<i>'
					THEN 1
					ELSE 0
				END,
				t.Plaintext COLLATE SQL_Latin1_General_CP1_CI_AS,
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