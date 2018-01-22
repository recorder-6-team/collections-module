/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = Object_Id(N'[dbo].[usp_Concept_InsertSpreadsheetRow]') AND Type = 'P')
	DROP PROCEDURE [dbo].[usp_Concept_InsertSpreadsheetRow]
GO

/*===========================================================================*\
  Description:	Insert a Concept record, plus related records as required,
				based on data extracted from a spreadsheet.

  Parameters:   @SessionID							Session key
				@concept_group_key					Concept group key
				@author								Name of author
				@child_of							Parent concept key
				@citation_date						Citation date
                @rank           	    		    Rank (abbreviation or name)
				@fact_#?_title						Name of fact 		(*5)
				@fact_#?_type						Name of fact type 	(*5)
				@fact_#?_description				Fact data 			(*5)
				@designation_#?_status				Status of concept designation	(*2)
				@designation_#?_start_date_start	Start date of designation		(*2)
				@designation_#?_start_date_end
				@designation_#?_start_date_type
				@designation_#?_end_date_start		End date of designation			(*2)
				@designation_#?_end_date_end
				@designation_#?_end_date_type
				@list_code							Concept list code
				@name_type							Name of name type
				@sort_code							Concept sort code
				@synonym_of							Synonym concept key
				@language							Name of term language
				@language_key						Term language key
				@term_name							Term name
				@concept_key						[on exit] New concept key

  Created:		Jan 2004

  Last revision information:
	$Revision: 14 $
	$Date: 8/08/11 13:29 $
	$Author: Simonlewis $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Concept_InsertSpreadsheetRow]
	@SessionID							CHAR(16),
	@concept_group_key					CHAR(16),
	@author								VARCHAR(100),
	@child_of							CHAR(16),
	@citation_date						VARCHAR(100),
    @rank           				    VARCHAR(100),
	@fact_#1_title						VARCHAR(100),
	@fact_#1_type						NVARCHAR(150),
	@fact_#1_description				TEXT,
	@fact_#2_title						VARCHAR(100),
	@fact_#2_type						NVARCHAR(150),
	@fact_#2_description				TEXT,
	@fact_#3_title						VARCHAR(100),
	@fact_#3_type						NVARCHAR(150),
	@fact_#3_description				TEXT,
	@fact_#4_title						VARCHAR(100),
	@fact_#4_type						NVARCHAR(150),
	@fact_#4_description				TEXT,
	@fact_#5_title						VARCHAR(100),
	@fact_#5_type						NVARCHAR(150),
	@fact_#5_description				TEXT,
	@designation_#1_status				NVARCHAR(150),
	@designation_#1_start_date_start	INT,
	@designation_#1_start_date_end		INT,
	@designation_#1_start_date_type		VARCHAR(2),
	@designation_#1_end_date_start		INT,
	@designation_#1_end_date_end		INT,
	@designation_#1_end_date_type		VARCHAR(2),
	@designation_#2_status				NVARCHAR(150),
	@designation_#2_start_date_start	INT,
	@designation_#2_start_date_end		INT,
	@designation_#2_start_date_type		VARCHAR(2),
	@designation_#2_end_date_start		INT,
	@designation_#2_end_date_end		INT,
	@designation_#2_end_date_type		VARCHAR(2),
	@list_code							VARCHAR(50),
	@name_type							NVARCHAR(300),
	@sort_code							INT,
	@synonym_of							CHAR(16),
	@language							VARCHAR(50),
	@language_key						VARCHAR(4),
	@term_name							NVARCHAR(300),
	@concept_key						CHAR(16)	OUTPUT
AS
	SET NOCOUNT ON
	SET ARITHABORT ON

	/*--------------------------------------------------------------------------------*\ 
		Check parameters.
	\*--------------------------------------------------------------------------------*/
	IF @term_name IS NULL
	BEGIN
		RAISERROR ('Term name must be specified.', 16, 1)
		RETURN
	END

	IF NOT @language_key IS NULL
	BEGIN
		IF NOT EXISTS (	SELECT		1
						FROM		Language
						WHERE		Language_Key	=	@language_key )
		BEGIN
			RAISERROR ('Specified language does not exist.', 16, 1)
			RETURN
		END
	END
	ELSE
	BEGIN
		IF @language IS NULL
		BEGIN
			RAISERROR ('Language or Language Key must be specified.', 16, 1)
			RETURN
		END

		SELECT		@language_key	=	Language_Key
		FROM		Language
		WHERE		Item_Name		=	@language

		IF @@ROWCOUNT = 0
		BEGIN
			RAISERROR ('Specified language is not recognised.', 16, 1)
			RETURN
		END
	END

	DECLARE		@term_key			CHAR(16),
				@term_version_key	CHAR(16),
				@name_type_key		CHAR(16),
				@meaning_key		CHAR(16),
                @domain_key         CHAR(16),
                @rank_key           CHAR(16)

	BEGIN TRANSACTION

	/*--------------------------------------------------------------------------------*\ 
		Work out the term.
	\*--------------------------------------------------------------------------------*/
	SELECT		@term_key		=	Term_Key
	FROM		Term
	WHERE		Language_Key	=	@language_key
	AND			Plaintext		=	dbo.ufn_RemoveHtmlMarkup(LTRIM(RTRIM(@term_name)))

	IF @@ROWCOUNT = 0
	BEGIN
		EXECUTE		spNextKey	'Term',
								@term_key	OUTPUT
		IF @@ERROR <> 0 GOTO fail

		INSERT		Term (
					Term_Key,
					Language_Key,
					Plaintext,
					Entered_Session_ID)
		VALUES		(@term_key,
					@language_key,
					LTRIM(RTRIM(dbo.ufn_RemoveHtmlMarkup(@term_name))),
					@SessionID)
		IF @@ERROR <> 0 GOTO fail
	END

	/*--------------------------------------------------------------------------------*\ 
		Create term version.
	\*--------------------------------------------------------------------------------*/
	EXECUTE		spNextKey	'Term_Version',
							@term_version_key	OUTPUT
	IF @@ERROR <> 0 GOTO fail

	INSERT		Term_Version (
				Term_Version_Key,
				Term_Key,
				Author_And_Date,
				Entered_Session_ID)
	VALUES		(@term_version_key,
				@term_key,
				ISNULL(@author + ' ', '') + ISNULL(@citation_date, ''),
				@SessionID)
	IF @@ERROR <> 0 GOTO fail
	
	/*--------------------------------------------------------------------------------*\ 
		Work out the meaning.
	\*--------------------------------------------------------------------------------*/
	IF @synonym_of IS NOT NULL
	BEGIN
		SELECT		@meaning_key	=	Meaning_Key
		FROM		Concept
		WHERE		Concept_Key		=	@synonym_of

		IF @@ROWCOUNT = 0
		BEGIN
			RAISERROR ('Synonym does not exist.', 16, 1)
			GOTO fail
		END
	END
	ELSE
	BEGIN
		EXECUTE		spNextKey	'Meaning',
								@meaning_key	OUTPUT
		IF @@ERROR <> 0 GOTO fail

		INSERT		Meaning (
					Meaning_Key)
		VALUES		(@meaning_key)

		IF @@ERROR <> 0 GOTO fail
	END

	/*--------------------------------------------------------------------------------*\ 
		Work out the name type.
	\*--------------------------------------------------------------------------------*/
	IF @name_type IS NULL
	BEGIN
		SET			@name_type_key	=	'SYSTEM00000000AN' /* 'Unknown' */
	END
	ELSE
	BEGIN
		SELECT		@name_type_key		=	c.Concept_Key
		FROM		Concept				AS	c
		INNER JOIN	Term				AS	t
		ON			t.Term_Key			=	c.Term_Key
		WHERE		c.Concept_Group_Key	=	'SYSTEM000000000M'
		AND			t.Language_Key		=	'en'
		AND			t.Plaintext			=	dbo.ufn_RemoveHtmlMarkup(@name_type)

		
		IF @@ROWCOUNT = 0
		BEGIN
			EXECUTE		usp_Concept_Insert	@name_type_key	OUTPUT,
											'SYSTEM000000000M',
											@name_type,
											@name_type,
											'en',
											@SessionID,
											'SYSTEM00000000AN'
			IF @@ERROR <> 0 GOTO fail
		END
	END

	/*--------------------------------------------------------------------------------*\ 
		Work out the rank.
	\*--------------------------------------------------------------------------------*/
    IF @rank IS NOT NULL
    BEGIN
        EXECUTE     usp_ConceptGroup_GetDomain  @concept_group_key,
                                                @domain_key         OUTPUT
        IF @@ERROR <> 0 GOTO fail

        SELECT      @rank_key       =   Concept_Rank_Key
        FROM        Concept_Rank
        WHERE       Domain_Key      =   @domain_key
        AND         Abbreviation    =   @rank

        IF @rank_key IS NULL
        BEGIN
            SELECT      @rank_key       =   Concept_Rank_Key
            FROM        Concept_Rank
            WHERE       Domain_Key      =   @domain_key
            AND         Item_Name       =   @rank
        END

        IF @rank_key IS NULL
        BEGIN
            EXECUTE     spNextKey   'Concept_Rank',
                                    @rank_key       OUTPUT
            IF @@ERROR <> 0 GOTO fail

            INSERT      Concept_Rank (
                        Concept_Rank_Key,
                        Domain_Key,
                        Item_Name,
                        Abbreviation,
                        Color_R,
                        Color_G,
                        Color_B,
                        Entered_Session_ID)
            VALUES      (@rank_key,
                        @domain_key,
                        @rank,
                        @rank,
                        0,
                        0,
                        0,
                        @SessionID)
            IF @@ERROR <> 0 GOTO fail
        END
    END

	/*--------------------------------------------------------------------------------*\ 
		Create concept.
	\*--------------------------------------------------------------------------------*/
		
	EXECUTE		spNextKey	'Concept',
							@concept_key	OUTPUT
	IF @@ERROR <> 0 GOTO fail

	INSERT		Concept (
				Concept_Key,
				Term_Key,
				Concept_Group_Key,
				Term_Version_Key,
				List_Preferred,
				Preferred,
                Concept_Rank_Key,
				Name_Type_Concept_Key,
				Meaning_Key,
				Sort_Code,
				List_Code,
				Entered_Session_ID,
				Published_Term,
				Automatic_Published_Term)
	SELECT		@concept_key,
				@term_key,
				@concept_group_key,
				@term_version_key,
				CASE WHEN @synonym_of IS NULL THEN 1 ELSE 0 END,
				CASE
					WHEN @synonym_of IS NULL THEN 1
					WHEN EXISTS (	SELECT		1
									FROM		Concept			AS	c
									INNER JOIN	Term			AS	t
									ON			t.Term_Key		=	c.Term_Key
									WHERE		c.Meaning_Key	=	@meaning_key
									AND			t.Language_Key	=	@language_key)
						THEN 0
					ELSE 1
				END,
                @rank_key,
				@name_type_key,
				@meaning_key,
				@sort_code,
				@list_code,
				@SessionID,
				@term_name,
				1
	IF @@ERROR <> 0 GOTO fail

	/*--------------------------------------------------------------------------------*\ 
		Update the lineage.
	\*--------------------------------------------------------------------------------*/
	EXECUTE		usp_ConceptLineage_NewConcept	@concept_key
	IF @@ERROR <> 0 GOTO fail

	/*--------------------------------------------------------------------------------*\ 
		Create the parent-child relationship.
	\*--------------------------------------------------------------------------------*/
	IF @child_of IS NOT NULL
	BEGIN
		DECLARE		@relation_key		CHAR(16),
					@relation_type_key	CHAR(16)

		SELECT		@relation_type_key	=	Hierarchy_Relation_Type_Key
		FROM		Concept_Group
		WHERE		Concept_Group_Key	=	@concept_group_key

		EXECUTE		usp_ConceptRelation_Insert	@relation_key	OUTPUT,
												@child_of,
												@concept_key,
												@relation_type_key,
												@SessionID = @SessionID
		IF @@ERROR <> 0 GOTO fail
	END

	/*--------------------------------------------------------------------------------*\ 
		Create up to 5 facts.
	\*--------------------------------------------------------------------------------*/
	DECLARE	@Counter 			TINYINT,
			@fact_title			VARCHAR(100),
			@fact_type			NVARCHAR(150),
			@fact_description	VARCHAR(8000)
	DECLARE	@fact_type_key		CHAR(16),
			@fact_key			CHAR(16)
	SET		@Counter = 1

	WHILE @Counter <= 5 BEGIN
		-- Depending on counter, assign param values to holding vars
		IF @Counter = 1
			SELECT	@fact_title			= @fact_#1_title, 
					@fact_type			= @fact_#1_type,
					@fact_description	= @fact_#1_description
		ELSE
		IF @Counter = 2
			SELECT	@fact_title			= @fact_#2_title, 
					@fact_type			= @fact_#2_type,
					@fact_description	= @fact_#2_description
		ELSE
		IF @Counter = 3
			SELECT	@fact_title			= @fact_#3_title, 
					@fact_type			= @fact_#3_type,
					@fact_description	= @fact_#3_description
		ELSE
		IF @Counter = 4
			SELECT	@fact_title			= @fact_#4_title, 
					@fact_type			= @fact_#4_type,
					@fact_description	= @fact_#4_description
		ELSE
		IF @Counter = 5
			SELECT	@fact_title			= @fact_#5_title, 
					@fact_type			= @fact_#5_type,
					@fact_description	= @fact_#5_description

		-- Update now for next time round.
		SET @Counter = @Counter + 1

		-- Now proceed with the inserts
		IF @fact_description IS NOT NULL
		BEGIN
	
			IF @fact_type IS NULL
			BEGIN
				SET			@fact_type_key	=	'SYSTEM00000002NO' /* HTML */
			END
			ELSE
			BEGIN
				SELECT		@fact_type_key		=	c.Concept_Key
				FROM		Concept				AS	c
				INNER JOIN	Term				AS	t
				ON			t.Term_Key			=	c.Term_Key
				WHERE		c.Concept_Group_Key	=	'SYSTEM000000000L'
				AND			t.Language_Key		=	'en'
				AND			t.Plaintext			=	dbo.ufn_RemoveHtmlMarkup(@fact_type)
	
				IF @@ROWCOUNT = 0
				BEGIN
					EXECUTE		usp_Concept_Insert	@fact_type_key	OUTPUT,
													'SYSTEM000000000L',
													@fact_type,
													@fact_type,
													'en',
													@SessionID,
													'SYSTEM00000000AN'
					IF @@ERROR <> 0 GOTO fail
				END
			END
	
			EXECUTE		spNextKey	'Thesaurus_Fact',
									@fact_key			OUTPUT
			INSERT		Thesaurus_Fact (
						Thesaurus_Fact_Key,
						Item_Name,
						Data,
						Meaning_Key,
						Language_Key,
						Fact_Vague_Date_Type,
						Fact_Type_Concept_Key,
						Entered_Session_ID,
						System_Supplied_Data)
			VALUES		(@fact_key,
						ISNULL(@fact_title, 'Fact'),
						@fact_description,
						@meaning_key,
						'en',
						'U',
						@fact_type_key,
						@SessionID,
						0)
			IF @@ERROR <> 0 GOTO fail
		END
	END

	/*--------------------------------------------------------------------------------*\ 
		Create up to 2 designations.
	\*--------------------------------------------------------------------------------*/
	DECLARE @designation_Status				NVARCHAR(150),
			@designation_Start_Date_Start	INT,
			@designation_Start_Date_End		INT,
			@designation_Start_Date_Type	VARCHAR(2),
			@designation_End_Date_Start		INT,
			@designation_End_Date_End		INT,
			@designation_End_Date_Type		VARCHAR(2)
	DECLARE	@designation_key				CHAR(16),
			@designation_type_concept_key	CHAR(16),
			@designation_concept_group_key	CHAR(16),
			@designation_term_key			CHAR(16)
	SET		@Counter = 1

	WHILE @Counter <= 2 BEGIN
		SELECT	@designation_key				= NULL,
				@designation_type_concept_key	= NULL,
				@designation_concept_group_key	= NULL

		IF @Counter = 1
			SELECT	@designation_status 			= @designation_#1_status,
					@designation_start_date_start	= @designation_#1_start_date_start,
					@designation_start_date_end		= @designation_#1_start_date_end,
					@designation_start_date_type	= @designation_#1_start_date_type,
					@designation_end_date_start		= @designation_#1_end_date_start,
					@designation_end_date_end		= @designation_#1_end_date_end,
					@designation_end_date_type		= @designation_#1_end_date_type
		ELSE
			SELECT	@designation_status 			= @designation_#2_status,
					@designation_start_date_start	= @designation_#2_start_date_start,
					@designation_start_date_end		= @designation_#2_start_date_end,
					@designation_start_date_type	= @designation_#2_start_date_type,
					@designation_end_date_start		= @designation_#2_end_date_start,
					@designation_end_date_end		= @designation_#2_end_date_end,
					@designation_end_date_type		= @designation_#2_end_date_type

		IF @designation_status IS NOT NULL BEGIN
			-- Locate the Concept Designation Type concept group. Create new one if required.
			EXECUTE		usp_ConceptDesignationsAvailable_Get	@concept_group_key,
																@designation_concept_group_key OUTPUT
			IF @designation_concept_group_key IS NULL BEGIN
				EXECUTE	spNextKey 'Concept_Group', @designation_concept_group_key OUTPUT

				INSERT INTO Concept_Group (
							Concept_Group_Key,
							Local_Domain_Key,
							Item_Name,
							Entered_Session_Id)
				SELECT		@designation_concept_group_key,
							Local_Domain_Key,
							'Concept Designation Types',
							@SessionId
				FROM		Concept_Group
				WHERE		Concept_Group_Key = @concept_group_key

				IF @@ERROR <> 0 GOTO fail
			END

			-- Status has to be added as concept if not found.
			SELECT	@designation_type_concept_key = concept_Key
			FROM	vw_ConceptTerm
			WHERE	Item_Name 			= @designation_status
			AND		Concept_Group_Key 	= @designation_concept_group_key

			IF @designation_type_concept_key IS NULL BEGIN
				-- No history for this concept, so can't use usp_Concept_Insert directly.
				EXECUTE	usp_Term_Insert
						@Key				= @designation_term_key OUTPUT,
						@LanguageKey 		= 'en',
						@Plaintext 			= @designation_status,
						@SessionID 			= @SessionID,
						@SystemSuppliedData = 0
				IF @@ERROR <> 0 GOTO fail

				EXECUTE	usp_ConceptSimple_Insert 
						@Key 					= @designation_type_concept_key OUTPUT,
						@TermKey 				= @designation_term_key,
						@ConceptGroupKey 		= @designation_concept_group_key,
						@Preferred 				= 1,
						@NameTypeConceptKey		= 'SYSTEM0000000000', -- Formal name type
						@SessionID 				= @SessionID,
						@SystemSuppliedData		= 0,
						@AutomaticPublishedTerm	= 1
				IF @@ERROR <> 0 GOTO fail
			END

			-- Now add the concept designation record.
			EXECUTE		spNextKey	'Concept_Designation',
									@designation_key		OUTPUT
			IF @@ERROR <> 0 GOTO fail

			INSERT INTO Concept_Designation (
						Concept_Designation_Key,
						Concept_Key,
						Designation_Type_Concept_Key,
						From_Vague_Date_Start,
						From_Vague_Date_End,
						From_Vague_Date_Type,
						To_Vague_Date_Start,
						To_Vague_Date_End,
						To_Vague_Date_Type,
						Entered_Session_ID)
			VALUES 		(@designation_Key,
						@concept_Key,
						@designation_type_concept_key,
						@designation_start_date_start,
						@designation_start_date_end,
						@designation_start_date_type,
						@designation_end_date_start,
						@designation_end_date_end,
						@designation_end_date_type,
						@SessionID)

			IF @@ERROR <> 0 GOTO fail
		END

		SET @Counter = @Counter + 1
	END

	COMMIT TRANSACTION
	RETURN

fail:
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_Concept_InsertSpreadsheetRow failed', 16, 1)

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_InsertSpreadsheetRow') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_Concept_InsertSpreadsheetRow'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concept_InsertSpreadsheetRow TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concept_InsertSpreadsheetRow TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
       	GRANT EXECUTE ON dbo.usp_Concept_InsertSpreadsheetRow TO [Dev - JNCC SQL]
END
GO