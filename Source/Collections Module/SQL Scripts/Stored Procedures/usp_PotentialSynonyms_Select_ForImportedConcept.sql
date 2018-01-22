SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

/*============================================================================*\
  Drop stored proc before re-creating.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_PotentialSynonyms_Select_ForImportedConcept') IS NOT NULL
	DROP PROCEDURE dbo.usp_PotentialSynonyms_Select_ForImportedConcept
GO

/*============================================================================*\
  Description:  List-preferred potential synonyms of the specified concept.

  Parameters:   @concept_key            Concept key

  Created:      Jan 2004

  Last revision information:
	$Revision: 10 $
	$Date: 3/08/11 15:55 $
	$Author: Simonlewis $
\*============================================================================*/
CREATE PROCEDURE dbo.usp_PotentialSynonyms_Select_ForImportedConcept
	@concept_key        CHAR(16)
AS
	SET NOCOUNT ON

	DECLARE     @src_meaning_key    CHAR(16),
				@src_group_key      CHAR(16)

	SELECT      @src_meaning_key    =   Meaning_Key,
				@src_group_key      =   Concept_Group_Key
	FROM        Concept
	WHERE       Concept_Key         =   @concept_key

	/* work out all current synonyms of the concept */
	DECLARE     @current_synonyms   TABLE (
		Language_Key        CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS,
		Plaintext           NVARCHAR(300) COLLATE SQL_Latin1_General_CP1_CI_AI
		PRIMARY KEY (Language_Key, Plaintext))

	INSERT      @current_synonyms (
				Language_Key,
				Plaintext)
	SELECT DISTINCT
				t.Language_Key,
				t.Plaintext
	FROM        Concept             AS  c
	INNER JOIN  Term                AS  t
	ON          t.Term_Key          =   c.Term_Key
	WHERE       c.Meaning_Key       =   @src_meaning_key

	IF @@ERROR <> 0 RETURN        

	/* work out all list-preferred potential synonyms */
	DECLARE     @potential  TABLE (
			Concept_Key         CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS PRIMARY KEY,
			Concept_Group_Key   CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS,
			Meaning_Key         CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS,
			Term_Key            CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS,
			Author_Copy         VARCHAR(100) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
			Different_Group     BIT,
			Session_Start       DATETIME)

	INSERT      @potential (
				Concept_Key,
				Concept_Group_Key,
				Meaning_Key,
				Term_Key,
				Author_Copy,
				Different_Group,
				Session_Start)
	SELECT DISTINCT
				psyn.Concept_Key,
				psyn.Concept_Group_Key,
				psyn.Meaning_Key,
				psyn.Term_Key,
				psyn.Author_Copy,
				CASE psyn.Concept_Group_Key
					WHEN @src_group_key THEN 0
					ELSE 1
				END,
				s.Date_Time_Start
	FROM        @current_synonyms   AS  curr
	INNER JOIN  Term                AS  tpot WITH (INDEX (IX_Plaintext))
	ON          tpot.Plaintext      =   curr.Plaintext
	AND         tpot.Language_Key   =   curr.Language_Key
	INNER JOIN  Concept             AS  pot
	ON          pot.Term_Key        =   tpot.Term_Key
	INNER JOIN  Concept             AS  psyn
	ON          psyn.Meaning_Key    =   pot.Meaning_Key
	AND         psyn.List_Preferred =   1
	INNER JOIN  Session             AS  s
	ON          s.Session_ID        =   psyn.Entered_Session_ID
	WHERE       pot.Meaning_Key     <>  @src_meaning_key  /* not currently a synonym */

	IF @@ERROR <> 0 RETURN

	/* select most recently entered list-preferred concept for each
	 * potential synonym */
	SELECT      p.Concept_Key       AS  Item_Key,
				c.Published_Term	AS	Item_Name,
				g.Concept_Group_Key	AS	Group_Key,
				g.Item_Name         AS  Group_Name,
				g.Authority
	FROM        @potential          AS  p
	INNER JOIN	dbo.Concept			AS	c
	ON			p.Concept_Key		=	c.Concept_Key
	INNER JOIN  Concept_Group       AS  g
	ON          g.Concept_Group_Key =   p.Concept_Group_Key
	WHERE       p.Session_Start     =   (   SELECT      TOP 1 Session_Start
											FROM        @potential      AS  p2
											WHERE       p2.Meaning_Key  =   p.Meaning_Key
											ORDER BY    p2.Different_Group,
														p2.Session_Start DESC)
GO

/*============================================================================*\
  Grant permissions.
\*============================================================================*/
IF OBJECT_ID('dbo.usp_PotentialSynonyms_Select_ForImportedConcept') IS NOT NULL
BEGIN
	PRINT 'Setting up security on procedure usp_PotentialSynonyms_Select_ForImportedConcept'
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_AddOnly')
			GRANT EXECUTE ON dbo.usp_PotentialSynonyms_Select_ForImportedConcept TO R2k_AddOnly
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_PotentialSynonyms_Select_ForImportedConcept TO R2k_Administrator
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_PotentialSynonyms_Select_ForImportedConcept TO R2k_FullEdit
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_PotentialSynonyms_Select_ForImportedConcept TO R2k_ReadOnly
	IF EXISTS (SELECT * FROM sysusers WHERE name = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_PotentialSynonyms_Select_ForImportedConcept TO R2k_RecordCardsOnly
END
GO