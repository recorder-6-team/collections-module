SET ANSI_NULLS ON
SET QUOTED_IDENTIFIER ON
GO

SET NOCOUNT ON

/*============================================================================*/
--	Create the Term_Generator table...
/*============================================================================*/

IF NOT EXISTS
(
	SELECT		*
	FROM		sys.objects
	WHERE		"object_id"		=		OBJECT_ID(N'dbo.Term_Generator')
)
BEGIN
	CREATE TABLE dbo.Term_Generator
	(
		Term_Generator_Key CHAR(16) NOT NULL
		CONSTRAINT PK_Term_Generator PRIMARY KEY,

		Item_Name NVARCHAR(150) NOT NULL,

		Published_Term_Function NVARCHAR(257) NOT NULL,

		Search_Term_Procedure NVARCHAR(257) NOT NULL,

		CONSTRAINT UQ_Term_Generator_Item_Name
		UNIQUE (Item_Name)
	)

	--	Add the default term generator rule...
	INSERT INTO	dbo.Term_Generator
				(
					Term_Generator_Key,
					Item_Name,
					Published_Term_Function,
					Search_Term_Procedure
				)
	VALUES		(
					'DSS0000000000000',
					'System default rule',
					'dbo.ufn_GeneratePublishedTermUsingDefaultRule',
					'dbo.usp_Search_Term_GenerateTermsUsingDefaultRule'
				)
END

/*============================================================================*/
--	Changes to the Concept_Group table...
/*============================================================================*/

IF NOT EXISTS
(
	SELECT		*
	FROM		sys.columns
	WHERE		"object_id"		=		OBJECT_ID(N'dbo.Concept_Group')
	AND			"name"			=		'Term_Generator_Key'
)
BEGIN
	ALTER TABLE		dbo.Concept_Group
	ADD				Term_Generator_Key CHAR(16) NULL
	CONSTRAINT		FK_Concept_Group_Term_Generator FOREIGN KEY
	REFERENCES		dbo.Term_Generator (Term_Generator_Key)
END

/*============================================================================*/
--	Changes to the Domain table...
/*============================================================================*/

IF NOT EXISTS
(
	SELECT		*
	FROM		sys.columns
	WHERE		"object_id"		=		OBJECT_ID(N'dbo.Domain')
	AND			"name"			=		'Term_Generator_Key'
)
BEGIN
	ALTER TABLE		dbo.Domain
	ADD				Term_Generator_Key CHAR(16) NULL
	CONSTRAINT		FK_Domain_Term_Generator FOREIGN KEY
	REFERENCES		dbo.Term_Generator (Term_Generator_Key)
END

/*============================================================================*/
--	Changes to the Local_Domain table...
/*============================================================================*/

IF NOT EXISTS
(
	SELECT		*
	FROM		sys.columns
	WHERE		"object_id"		=		OBJECT_ID(N'dbo.Local_Domain')
	AND			"name"			=		'Term_Generator_Key'
)
BEGIN
	ALTER TABLE		dbo.Local_Domain
	ADD				Term_Generator_Key CHAR(16) NULL
	CONSTRAINT		FK_Local_Domain_Term_Generator FOREIGN KEY
	REFERENCES		dbo.Term_Generator (Term_Generator_Key)
END

/*============================================================================*/
--	Create the Search_Term table...
/*============================================================================*/

IF NOT EXISTS
(
	SELECT		*
	FROM		sys.objects
	WHERE		"object_id"		=		OBJECT_ID(N'dbo.Search_Term')
)
BEGIN
	CREATE TABLE dbo.Search_Term
	(
		Search_Term_Key CHAR(16) NOT NULL
		CONSTRAINT PK_Search_Term PRIMARY KEY,

		Concept_Key CHAR(16) NOT NULL
		CONSTRAINT FK_Search_Term_Concept FOREIGN KEY
		REFERENCES dbo.Concept (Concept_Key),

		Plaintext NVARCHAR(450) COLLATE SQL_Latin1_General_CP1_CI_AI NOT NULL,

		System_Generated BIT NOT NULL
	)

	CREATE NONCLUSTERED INDEX IX_Search_Term_Plaintext
	ON dbo.Search_Term (Plaintext)

	/*===========================================================================*\
	  Grant permissions.
	\*===========================================================================*/
	IF OBJECT_ID('dbo.Search_Term') IS NOT NULL
	BEGIN
    	PRINT 'Setting up security on table Search_Term'
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT SELECT ON dbo.Search_Term TO [R2k_AddOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		BEGIN
			GRANT SELECT ON dbo.Search_Term TO [R2k_Administrator]
			GRANT UPDATE ON dbo.Search_Term TO [R2k_Administrator]
			GRANT INSERT ON dbo.Search_Term TO [R2k_Administrator]
			GRANT DELETE ON dbo.Search_Term TO [R2k_Administrator]
		END
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		BEGIN
			GRANT SELECT ON dbo.Search_Term TO R2k_FullEdit
			GRANT UPDATE ON dbo.Search_Term TO R2k_FullEdit
			GRANT INSERT ON dbo.Search_Term TO R2k_FullEdit
			GRANT DELETE ON dbo.Search_Term TO R2k_FullEdit
		END
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
			GRANT SELECT ON dbo.Search_Term TO [R2k_ReadOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
			GRANT SELECT ON dbo.Search_Term TO [R2k_RecordCardsOnly]
	END
END

/*============================================================================*/
--	The Term_Version_Key field of the Concept table will be made non-nullable.
/*============================================================================*/

-- We need a new Term_Version key for each Term where:
-- a) The Term has at least one Concept with a NULL or invalid* Term_Version_Key
-- and
-- b) The Term does not already have a placeholder** Term_Version (NULL
-- Version_Label and Author_And_Date fields).
-- Note:
-- * Invalid Term_Version_Key values are possible (and exist) due to missing
-- FK_Concept_Term_Version constraint.
-- ** Existing placeholder Term_Version records will be used again, rather than
-- creating a new one and requiring a new key.

-- First we re-use existing placeholders where available (selecting the first of
-- possibly several existing placeholders per Term).
;
WITH CTE AS
(
	SELECT		C.Concept_Key,
				MIN(TV2.Term_Version_Key)	AS		Term_Version_Key
	FROM		dbo.Concept					AS		C
	LEFT JOIN	dbo.Term_Version			AS		TV
	ON			C.Term_Version_Key			=		TV.Term_Version_Key
	LEFT JOIN	dbo.Term_Version			AS		TV2
	ON			C.Term_Key					=		TV2.Term_Key
	AND			TV2.Version_Label			IS		NULL
	AND			TV2.Author_And_Date			IS		NULL

	WHERE		TV.Term_Version_Key			IS		NULL
	AND			TV2.Term_Version_Key		IS NOT	NULL

	GROUP BY	C.Concept_Key
)
UPDATE		C
SET			Term_Version_Key		=		CTE.Term_Version_Key
FROM		CTE
INNER JOIN	dbo.Concept				AS		C
ON			CTE.Concept_Key			=		C.Concept_Key

DECLARE @NewKeyCount INT

-- Work out how many new Term_Version keys we need.
SET @NewKeyCount =
(
	SELECT		COUNT(DISTINCT C.Term_Key)
	FROM		dbo.Concept					AS		C
	LEFT JOIN	dbo.Term_Version			AS		TV
	ON			C.Term_Version_Key			=		TV.Term_Version_Key
	WHERE		TV.Term_Version_Key			IS		NULL
)

-- Our new Term_Version keys are stored here.
DECLARE @Keys TABLE
(
	Key_Number INT IDENTITY(1, 1),
	"Key" CHAR(16)
)

DECLARE @NextKey CHAR(16)

DECLARE @Count INT
SET @Count = 0

WHILE @Count < @NewKeyCount
BEGIN
	EXEC dbo.spNextKey 'Term_Version', @NextKey OUTPUT

	INSERT INTO	@Keys
	SELECT		@NextKey
	
	SET @Count = @Count + 1
END

-- Create placeholder Term_Version records for Concepts that don't have them...
;
WITH CTE AS
(
	-- Concepts with the same Term share the same Key_Number, so we can make
	-- them share the same new placeholder Term_Version.
	SELECT		DENSE_RANK() OVER (ORDER BY C.Term_Key)	AS	Key_Number,
				C.Term_Key
	FROM		dbo.Concept					AS		C
	LEFT JOIN	dbo.Term_Version			AS		TV
	ON			C.Term_Version_Key			=		TV.Term_Version_Key
	WHERE		TV.Term_Version_Key			IS		NULL
)
INSERT INTO	dbo.Term_Version
			(
				Term_Version_Key,
				Term_Key,
				Version_Label,
				Author_And_Date,
				Entered_Session_ID,
				System_Supplied_Data
			)
SELECT		DISTINCT
			K."Key",
			C.Term_Key,
			NULL,
			NULL,
			'SYSTEM0000000000',
			1
FROM		CTE
INNER JOIN	@Keys				AS		K
ON			CTE.Key_Number		=		K.Key_Number
INNER JOIN	dbo.Concept			AS		C
ON			CTE.Term_Key		=		C.Term_Key

-- Now we can assign the placeholder Term_Version records to their Concepts.
;
WITH CTE AS
(
	SELECT		DENSE_RANK() OVER (ORDER BY C.Term_Key)	AS	Key_Number,
				C.Term_Key
	FROM		dbo.Concept					AS		C
	LEFT JOIN	dbo.Term_Version			AS		TV
	ON			C.Term_Version_Key			=		TV.Term_Version_Key
	WHERE		TV.Term_Version_Key			IS		NULL
)
UPDATE		C
SET			Term_Version_Key	=		K."Key"
FROM		CTE
INNER JOIN	@Keys				AS		K
ON			CTE.Key_Number		=		K.Key_Number
INNER JOIN	dbo.Concept			AS		C
ON			CTE.Term_Key		=		C.Term_Key

/*============================================================================*/
--	Changes to the Concept table...
/*============================================================================*/

-- We start by adding a constraint that should have been there already!
BEGIN TRY
	ALTER TABLE		dbo.Concept
	ADD CONSTRAINT	FK_Concept_Term_Version
	FOREIGN KEY		(Term_Version_Key)
	REFERENCES		dbo.Term_Version (Term_Version_Key)
END TRY
BEGIN CATCH
	-- Fail silently if the constraint is already there on client's database.
END CATCH

DROP INDEX IX_Term_Version_Key
ON dbo.Concept

ALTER TABLE		dbo.Concept
ALTER COLUMN	Term_Version_Key CHAR(16) NOT NULL

CREATE NONCLUSTERED INDEX IX_Term_Version_Key
		ON dbo.Concept (Term_Version_Key)
		ON "PRIMARY"

IF NOT EXISTS
(
	SELECT		*
	FROM		sys.columns
	WHERE		"object_id"		=		OBJECT_ID(N'dbo.Concept')
	AND			"name"			=		'Published_Term'
)
BEGIN
	ALTER TABLE		dbo.Concept
	ADD				Published_Term NVARCHAR(500) NULL
END

IF NOT EXISTS
(
	SELECT		*
	FROM		sys.columns
	WHERE		"object_id"		=		OBJECT_ID(N'dbo.Concept')
	AND			"name"			=		'Automatic_Published_Term'
)
BEGIN
	ALTER TABLE		dbo.Concept
	ADD				Automatic_Published_Term BIT NOT NULL
	CONSTRAINT		DF_Concept_Automatic_Published_Term DEFAULT(0)

	ALTER TABLE		dbo.Concept
	DROP CONSTRAINT	DF_Concept_Automatic_Published_Term
END

IF NOT EXISTS
(
	SELECT		*
	FROM		sys.columns
	WHERE		"object_id"		=		OBJECT_ID(N'dbo.Concept')
	AND			"name"			=		'Term_Generator_Key'
)
BEGIN
	ALTER TABLE		dbo.Concept
	ADD				Term_Generator_Key CHAR(16) NULL
	CONSTRAINT		FK_Concept_Term_Generator FOREIGN KEY
	REFERENCES		dbo.Term_Generator (Term_Generator_Key)
END

-- Now we try to apply a number of constraints that are missing from the dev
-- database and may be missing from the client's database. But we fail silently
-- and move on if any can't be applied - data cleanup is really another CCN...
BEGIN TRY
	ALTER TABLE		dbo.Concept
	ADD CONSTRAINT	FK_Concept_Concept_Group
	FOREIGN KEY		(Concept_Group_Key)
	REFERENCES		dbo.Concept_Group (Concept_Group_Key)
END TRY
BEGIN CATCH
END CATCH

BEGIN TRY
	ALTER TABLE		dbo.Concept
	ADD CONSTRAINT	FK_Concept_Concept_Name_Type
	FOREIGN KEY		(Name_Type_Concept_Key)
	REFERENCES		dbo.Concept (Concept_Key)
END TRY
BEGIN CATCH
END CATCH

BEGIN TRY
	ALTER TABLE		dbo.Concept
	ADD CONSTRAINT	FK_Concept_Term
	FOREIGN KEY		(Term_Key)
	REFERENCES		dbo.Term (Term_Key)
END TRY
BEGIN CATCH
END CATCH
	
BEGIN TRY
	ALTER TABLE		dbo.Concept
	ADD CONSTRAINT	FK_Concept_Meaning
	FOREIGN KEY		(Meaning_Key)
	REFERENCES		dbo.Meaning (Meaning_Key)
END TRY
BEGIN CATCH
END CATCH
	
BEGIN TRY
	ALTER TABLE		dbo.Concept
	ADD CONSTRAINT	FK_Concept_Concept_Rank
	FOREIGN KEY		(Concept_Rank_Key)
	REFERENCES		dbo.Concept_Rank (Concept_Rank_Key)
END TRY
BEGIN CATCH
END CATCH

-- Create tr_Concept_DisplayCaptionUpdates before next statement, which invokes
-- it...

/*===========================================================================*\
  Drop trigger before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[tr_Concept_DisplayCaptionUpdates]') 
	   AND    Type = 'TR')
    DROP TRIGGER [dbo].[tr_Concept_DisplayCaptionUpdates]
GO

CREATE TRIGGER [dbo].[tr_Concept_DisplayCaptionUpdates]
ON [dbo].[Concept]
AFTER UPDATE
AS 
	IF UPDATE(Term_Key) OR UPDATE(Published_Term)
	BEGIN
		UPDATE Conservation_Check 
		SET Search_Caption=
				dbo.ufn_GetDateFromVagueDate(CC.Vague_Date_Start, CC.Vague_Date_End, CC.Vague_Date_Type)
				+ ' - ' +
				T.Plaintext
				+ ' - ' +
				CC.Ref_Number,
			Display_Caption=
				dbo.ufn_GetDateFromVagueDate(CC.Vague_Date_Start, CC.Vague_Date_End, CC.Vague_Date_Type)
				+ ' - ' +
				C.Published_Term
				+ ' - ' +
				CC.Ref_Number
		FROM Conservation_Check CC
		INNER JOIN Concept C ON C.Concept_Key=CC.Type_Concept_Key
		INNER JOIN Term T ON T.Term_Key=C.Term_Key
		INNER JOIN Inserted I on I.Concept_Key=C.Concept_Key

		UPDATE Enquiry 
		SET Search_Caption=
				dbo.ufn_GetDateFromVagueDate(E.Vague_Date_Start, E.Vague_Date_End, E.Vague_Date_Type)
				+ ' - ' +
				T.Plaintext,
			Display_Caption=
				dbo.ufn_GetDateFromVagueDate(E.Vague_Date_Start, E.Vague_Date_End, E.Vague_Date_Type)
				+ ' - ' +
				C.Published_Term
		FROM Enquiry E
		INNER JOIN Concept C ON C.Concept_Key=E.Enquiry_Type_Concept_Key
		INNER JOIN Term T ON T.Term_Key=C.Term_Key
		INNER JOIN Inserted I on I.Concept_Key=C.Concept_Key

		UPDATE Valuation 
		SET Search_Caption=
				dbo.ufn_GetDateFromVagueDate(V.Vague_Date_Start, V.Vague_Date_End, V.Vague_Date_Type)
				+ ' - ' +
				T.Plaintext,
			Display_Caption=
				dbo.ufn_GetDateFromVagueDate(V.Vague_Date_Start, V.Vague_Date_End, V.Vague_Date_Type)
				+ ' - ' +
				C.Published_Term
		FROM Valuation V
		INNER JOIN Concept C ON C.Concept_Key=V.Type_Concept_Key
		INNER JOIN Term T ON T.Term_Key=C.Term_Key
		INNER JOIN Inserted I on I.Concept_Key=C.Concept_Key
	END
GO

-- For existing records, use the current term item name as the published term.
EXECUTE
(
	'UPDATE		C
	SET			Published_Term		=		T.Item_Name
	FROM		dbo.Concept			AS		C
	INNER JOIN	dbo.Term			AS		T
	ON			C.Term_Key			=		T.Term_Key
	WHERE		C.Published_Term	IS		NULL'
)

-- Now the field can be made non-nullable.
ALTER TABLE		dbo.Concept
ALTER COLUMN	Published_Term NVARCHAR(500) NOT NULL

/*============================================================================*/
--	Changes to the Term table...
/*============================================================================*/

-- Note: the Item_Name field is now obsolete, but is not removed here; it may be
-- needed for data migration or restoration purposes.

IF EXISTS
(
	SELECT		*
	FROM		sys.indexes
	WHERE		"object_id"		=		OBJECT_ID(N'dbo.Term')
	AND			"name"			=		N'IX_Item_Name'
)
BEGIN
	DROP INDEX IX_Item_Name ON dbo.Term
END

IF EXISTS
(
	SELECT		*
	FROM		sys.indexes
	WHERE		"object_id"		=		OBJECT_ID(N'dbo.Term')
	AND			"name"			=		N'IX_PlainText'
)
BEGIN
	DROP INDEX IX_PlainText ON dbo.Term
END

IF EXISTS
(
	SELECT		*
	FROM		sys.indexes
	WHERE		"object_id"		=		OBJECT_ID(N'dbo.Term')
	AND			"name"			=		N'IX_Term_Unique'
)
BEGIN
	DROP INDEX IX_Term_Unique ON dbo.Term
END

GO
-- Due to the fact that this index is missing from the development database and
-- duplicates exist (which may also be the case for live deployments), we need
-- to add clean-up code before trying to create it.
/*
CREATE UNIQUE NONCLUSTERED INDEX IX_Term_Unique
		ON dbo.Term (Plaintext, Language_Key)
		ON "PRIMARY"
*/

/*============================================================================*/
--	Changes to views...
/*============================================================================*/

ALTER VIEW dbo.VW_ConceptTerm
WITH SCHEMABINDING
AS
	SELECT		C.Concept_Key,
				C.Published_Term	AS		Item_Name,
				T.Plaintext,
				C.Author_Copy,
				C.Concept_Group_Key,
				C.Concept_Rank_Key,
				C.Sort_Code,
				C.Meaning_Key,
				List_Preferred,
				Is_Current
	FROM		dbo.Concept			AS		C
	INNER JOIN	dbo.Term			AS		T
	ON			T.Term_Key			=		C.Term_Key
GO

ALTER VIEW dbo.VW_ConceptTermCommon
WITH SCHEMABINDING
AS
	SELECT 		C1.Concept_Key,
				C2.Published_Term	AS		Item_Name,
				T.Plaintext,
				C2.Author_Copy,
				C1.Concept_Group_Key
	FROM 		dbo.Concept C1
	INNER JOIN 	dbo.Meaning			AS		M
	ON			M.Meaning_Key		=		C1.Concept_Key
	INNER JOIN 	dbo.Concept			AS		C2
	ON			C2.Meaning_Key		=		M.Meaning_Key
	INNER JOIN 	dbo.Term			AS		T
	ON			T.Term_Key			=		C2.Term_Key
	INNER JOIN 	dbo.Language		AS		L
	ON			L.Language_Key		=		T.Language_Key
	WHERE 		C2.Name_Type_Concept_Key	= 'SYSTEM000000000L'
	AND 		L.Priority			=		1
GO

ALTER VIEW dbo.VW_ConceptTermPreferred
AS
	SELECT		C.Concept_Key,
				C.Published_Term	AS		Item_Name,
				T.Plaintext,
				C.Author_Copy,
				C.Concept_Group_Key,
				C.Concept_Rank_Key,
				C.Sort_Code,
				CP.Concept_Key		AS		Preferred_Concept_Key
	FROM		dbo.Concept			AS		C
	INNER JOIN	dbo.Concept			AS		CP
	ON			CP.Meaning_Key		=		C.Meaning_Key
	AND			CP.List_Preferred	=		1
	AND			CP.Concept_Group_Key		= C.Concept_Group_Key
	INNER JOIN	dbo.Term			AS		T
	ON			T.Term_Key			=		CP.Term_Key
GO

-- Make the obsolete field nullable...
ALTER TABLE		dbo.Term
ALTER COLUMN	Item_Name NVARCHAR(300) NULL
GO

/*===========================================================================*\
  Drop trigger before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[tr_Term_DisplayCaptionUpdates]') 
	   AND    Type = 'TR')
    DROP TRIGGER [dbo].[tr_Term_DisplayCaptionUpdates]
GO

CREATE TRIGGER [dbo].[tr_Term_DisplayCaptionUpdates]
ON [dbo].[Term]
AFTER UPDATE
AS 
	IF UPDATE(Plaintext)
	BEGIN
		UPDATE Conservation_Check 
		SET Search_Caption=
				dbo.ufn_GetDateFromVagueDate(CC.Vague_Date_Start, CC.Vague_Date_End, CC.Vague_Date_Type)
				+ ' - ' +
				T.Plaintext
				+ ' - ' +
				CC.Ref_Number,
			Display_Caption=
				dbo.ufn_GetDateFromVagueDate(CC.Vague_Date_Start, CC.Vague_Date_End, CC.Vague_Date_Type)
				+ ' - ' +
				C.Published_Term
				+ ' - ' +
				CC.Ref_Number
		FROM Conservation_Check CC
		INNER JOIN Concept C ON C.Concept_Key=CC.Type_Concept_Key
		INNER JOIN Term T ON T.Term_Key=C.Term_Key
		INNER JOIN Inserted I on T.Term_Key=I.Term_Key

		UPDATE Enquiry 
		SET Search_Caption=
				dbo.ufn_GetDateFromVagueDate(E.Vague_Date_Start, E.Vague_Date_End, E.Vague_Date_Type)
				+ ' - ' +
				T.Plaintext,
			Display_Caption=
				dbo.ufn_GetDateFromVagueDate(E.Vague_Date_Start, E.Vague_Date_End, E.Vague_Date_Type)
				+ ' - ' +
				C.Published_Term
		FROM Enquiry E
		INNER JOIN Concept C ON C.Concept_Key=E.Enquiry_Type_Concept_Key
		INNER JOIN Term T ON T.Term_Key=C.Term_Key
		INNER JOIN Inserted I on T.Term_Key=I.Term_Key

		UPDATE Valuation 
		SET Search_Caption=
				dbo.ufn_GetDateFromVagueDate(V.Vague_Date_Start, V.Vague_Date_End, V.Vague_Date_Type)
				+ ' - ' +
				T.Plaintext,
			Display_Caption=
				dbo.ufn_GetDateFromVagueDate(V.Vague_Date_Start, V.Vague_Date_End, V.Vague_Date_Type)
				+ ' - ' +
				C.Published_Term
		FROM Valuation V
		INNER JOIN Concept C ON C.Concept_Key=V.Type_Concept_Key
		INNER JOIN Term T ON T.Term_Key=C.Term_Key
		INNER JOIN Inserted I on T.Term_Key=I.Term_Key

	END
GO

/*============================================================================*/
--	Pre-generate search terms...
/*============================================================================*/

DECLARE @SearchTerms TABLE
(
	-- This column will be used to pair each search term with its key. We only
	-- create the keys when we know how many are needed...
	Key_Number INT IDENTITY(1, 1),
	Concept_Key CHAR(16),
	Plaintext NVARCHAR(450)
)

-- Generate the search terms using the 5 variations provided by the default term
-- generator. UNION removes any duplicates resulting from NULL attributes...
INSERT INTO	@SearchTerms
			(
				Concept_Key,
				Plaintext
			)
SELECT		C.Concept_Key,
			T.Plaintext			AS			Search_Term_Plaintext
FROM		dbo.Concept			AS			C
INNER JOIN	dbo.Term			AS			T
ON			C.Term_Key			=			T.Term_key

UNION

SELECT		Concept_Key,
			dbo.ufn_RemoveHtmlMarkup(Published_Term)
								AS			Search_Term_Plaintext
FROM		dbo.Concept

UNION

SELECT		C.Concept_Key,
			RTRIM(T.Plaintext + ISNULL(' ' + TV.Author_And_Date, ''))
								COLLATE		SQL_Latin1_General_CP1_CI_AI
								AS			Search_Term_Plaintext
FROM		dbo.Concept			AS			C
INNER JOIN	dbo.Term			AS			T
ON			C.Term_Key			=			T.Term_Key
INNER JOIN	dbo.Term_Version	AS			TV
ON			C.Term_Version_Key	=			TV.Term_Version_Key

UNION

SELECT		C.Concept_Key,
			RTRIM(T.Plaintext + ISNULL(' ' + TV.Version_Label, ''))
								COLLATE		SQL_Latin1_General_CP1_CI_AI
								AS			Search_Term_Plaintext
FROM		dbo.Concept			AS			C
INNER JOIN	dbo.Term			AS			T
ON			C.Term_Key			=			T.Term_Key
INNER JOIN	dbo.Term_Version	AS			TV
ON			C.Term_Version_Key	=			TV.Term_Version_Key

UNION

SELECT		C.Concept_Key,
			RTRIM(T.Plaintext + ISNULL(' ' + TV.Author_And_Date, '') + ISNULL(' ' + TV.Version_Label, ''))
								COLLATE		SQL_Latin1_General_CP1_CI_AI
								AS			Search_Term_Plaintext
FROM		dbo.Concept			AS			C
INNER JOIN	dbo.Term			AS			T
ON			C.Term_Key			=			T.Term_Key
INNER JOIN	dbo.Term_Version	AS			TV
ON			C.Term_Version_Key	=			TV.Term_Version_Key


DECLARE @NewKeyCount INT

-- We will be generating up to 5 search terms per Concept if all search terms
-- are unique (far fewer in practice)...
SET @NewKeyCount =
(
	SELECT		COUNT(*)
	FROM		@SearchTerms
)

DECLARE		@SiteID						CHAR(8),
			@BaseKey					CHAR(8)
			
SELECT		@SiteID						=	Data
FROM		dbo.Setting
WHERE		Name						=	'SiteID'

-- Preallocate @NewKeyCount Search_Term keys.
UPDATE		dbo.LAST_KEY
SET			@BaseKey					=	LAST_KEY_TEXT,
			LAST_KEY_TEXT				=	RIGHT(
												'00000000'
												+ dbo.ufn_Base36Sum(
														LAST_KEY_TEXT,
														@NewKeyCount),
												8)
WHERE		TABLE_NAME					=	'Search_Term'

IF @@ROWCOUNT = 0
BEGIN
	-- No Search_Term keys have yet been generated.
	SET			@BaseKey					=	'00000000'
	
	INSERT INTO	dbo.LAST_KEY (
				TABLE_NAME,
				LAST_KEY_TEXT)
	SELECT		'Search_Term',
				RIGHT(
					'00000000'
					+ dbo.ufn_Base36Sum(@BaseKey, @NewKeyCount),
					8)
END												

-- Now we insert the search terms, matching each one to the new key we created
-- for it.
INSERT INTO		dbo.Search_Term
				(
					Search_Term_Key,
					Concept_Key,
					Plaintext,
					System_Generated
				)
SELECT			@SiteID
				+ RIGHT(
					'00000000'
					+ dbo.ufn_Base36Sum(
							@BaseKey,
							ST.Key_Number),
					8),
				ST.Concept_Key,
				ST.Plaintext,
				1
FROM			@SearchTerms			AS		ST
GO