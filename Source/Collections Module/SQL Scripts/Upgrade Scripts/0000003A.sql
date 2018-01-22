IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Concept_Select_AllChildConcepts]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
	DROP PROCEDURE [dbo].[usp_Concept_Select_AllChildConcepts]
GO

/*===========================================================================*\
  Description:	Returns All concepts  below a given concept.

  Parameters:
	@Key	Concept key

  Created:	August 2008

  Last revision information:
    $Revision: 4 $
    $Date: 19/09/08 11:57 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Concept_Select_AllChildConcepts] 
	@ParentKey	CHAR(16)
AS

	SET NOCOUNT ON

	DECLARE	@RelationKey CHAR(16)

	-- Get the hierarchy type from the concept group itself.
	SELECT	@RelationKey 	= 	Hierarchy_Relation_Type_Key
	FROM	Concept_Group	CG
	JOIN	Concept			C	ON	C.Concept_Group_Key = CG.Concept_Group_Key
	WHERE	Concept_Key		= 	@ParentKey

	CREATE TABLE #ConceptKeys (
		ConceptKey	CHAR(16),
		ItemName	NVARCHAR(300),
		SortCode	INT,
		HasChildren	BIT,
		Rank		CHAR(16)	
	)

	-- Get the ball rolling with the first level of chil concepts.
	INSERT INTO #ConceptKeys 
	EXECUTE usp_Concept_Select_ForParent @ParentKey, @RelationKey

	-- Repeat until no more concepts with child concepts.
	WHILE EXISTS(SELECT * FROM #ConceptKeys WHERE HasChildren = 1)
	BEGIN
		-- Get first concept with child concepts.
		SELECT 	@ParentKey 	= ConceptKey
		FROM	#ConceptKeys
		WHERE	HasChildren = 1
	
		-- Get the child concepts in.
		INSERT INTO #ConceptKeys
		EXECUTE usp_Concept_Select_ForParent @ParentKey, @RelationKey
	
		-- Update flag to indicate child concepts are done for this one.
		UPDATE 	#ConceptKeys
		SET		HasChildren = 0
		WHERE	ConceptKey 	= @ParentKey
	END

	SELECT * FROM #ConceptKeys

	DROP TABLE #ConceptKeys
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_Select_AllChildConcepts') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_Concept_Select_AllChildConcepts'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
       	GRANT EXECUTE ON dbo.usp_Concept_Select_AllChildConcepts TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concept_Select_AllChildConcepts TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concept_Select_AllChildConcepts TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Concept_Select_AllChildConcepts TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Concept_Select_AllChildConcepts TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
       	GRANT EXECUTE ON dbo.usp_Concept_Select_AllChildConcepts TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE  Id = Object_Id(N'[dbo].[usp_Specimen_Update_ForStore]') AND Type = 'P')
	DROP PROCEDURE [dbo].[usp_Specimen_Update_ForStore]
GO

/*===========================================================================*\
  Description:	Updates link between Specimen and its Store.

  Parameters:
	@ParentKey 				Store Key
	@ChildKey 				Specimen Key
	@UpdateUsualLocation	Flag to indicate whether usual location also gets updated.

  Created:	October 2003

  Last revision information:
    $Revision: 4 $
    $Date: 19/09/08 11:57 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Specimen_Update_ForStore] 
	@ParentKey 				CHAR(16),
	@ChildKey 				CHAR(16),
	@UpdateUsualLocation	BIT,
	@JoinKey 				CHAR(16) OUTPUT
AS
	SET NOCOUNT ON

	SET @JoinKey = @ChildKey

	DECLARE @ExistingContainerKey 	CHAR(16),
			@SpecimenMask 			INT

	-- Initialise variables.
	SELECT		@ExistingContainerKey = CU.Current_Container_Collection_Unit_Key
	FROM		Specimen_Unit SU
	INNER JOIN 	Collection_Unit CU ON CU.Collection_Unit_Key = SU.Collection_Unit_Key
	WHERE		SU.Collection_Unit_Key = @ChildKey

	-- Retrieve the mask of the preferred concept for specimen.
	EXECUTE	usp_Get_Concept_Domain_Mask_From_Specimen @ParentKey, @SpecimenMask OUTPUT

	BEGIN TRANSACTION

		UPDATE	Collection_Unit
		SET		Current_Container_Collection_Unit_Key = @ParentKey
		WHERE	Collection_Unit_Key = @ChildKey

		IF @UpdateUsualLocation = 1
			UPDATE	Collection_Unit
			SET		Usual_Container_Collection_Unit_Key = @ParentKey
			WHERE	Collection_Unit_Key = @ChildKey

		-- Update the "old" container mask, switch bits OFF.
		EXECUTE	usp_CollectionUnit_Update_DomainMask @ExistingContainerKey, @SpecimenMask, 0

		-- Update the "new" container mask, switch bits ON.
		EXECUTE	usp_CollectionUnit_Update_DomainMask @ParentKey, @SpecimenMask, 1

		IF @@Error <> 0 GOTO RollBackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit:
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Specimen_Update_ForStore') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_Specimen_Update_ForStore'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
       	GRANT EXECUTE ON dbo.usp_Specimen_Update_ForStore TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimen_Update_ForStore TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimen_Update_ForStore TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimen_Update_ForStore TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
       	GRANT EXECUTE ON dbo.usp_Specimen_Update_ForStore TO [Dev - JNCC SQL]
END
GO


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
	$Revision: 4 $
	$Date: 19/09/08 11:57 $
	$Author: Ericsalmon $

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
	AND			Item_Name		=	LTRIM(RTRIM(@term_name))

	IF @@ROWCOUNT = 0
	BEGIN
		EXECUTE		spNextKey	'Term',
								@term_key	OUTPUT
		IF @@ERROR <> 0 GOTO fail

		INSERT		Term (
					Term_Key,
					Language_Key,
					Item_Name,
					Plaintext,
					Entered_Session_ID)
		VALUES		(@term_key,
					@language_key,
					LTRIM(RTRIM(@term_name)),
					LTRIM(RTRIM(dbo.ufn_RemoveHtmlMarkup(@term_name))),
					@SessionID)
		IF @@ERROR <> 0 GOTO fail
	END

	/*--------------------------------------------------------------------------------*\ 
		Create term version.
	\*--------------------------------------------------------------------------------*/
	IF @author IS NOT NULL OR @citation_date IS NOT NULL
	BEGIN
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
	END

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
		AND			t.Item_Name			=	@name_type

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
				Entered_Session_ID)
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
				@SessionID
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
				AND			t.Item_Name			=	@fact_type
	
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
			@designation_concept_group_key	CHAR(16)
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
				EXECUTE	usp_Concept_Insert 
						@designation_type_concept_key OUTPUT,
						@designation_concept_group_key,
						@designation_status,
						@designation_status,
						'en',
						@SessionID,
						'SYSTEM0000000000', -- Formal name type
						0

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

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptDesignationsAvailable_Get]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptDesignationsAvailable_Get]
GO

/*===========================================================================*\
  Description:	Returns the Concept Group Key of a concept group called 
		'Concept Designation Types' in the current concept group's domain

  Parameters: 
	@Key 						Concept Group Key
	@DesignationTypesGroupKey 	OUTPUT

  Created:	April 2004

  Last revision information:
    $Revision: 4 $
    $Date: 19/09/08 11:57 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptDesignationsAvailable_Get]
	@Key 						CHAR(16),
	@DesignationTypesGroupKey 	CHAR(16) OUTPUT
AS
	SELECT 	@DesignationTypesGroupKey = CGD.Concept_Group_Key
	FROM 	Concept_Group 	CGD
	JOIN 	Local_Domain	LDD ON 	LDD.Local_Domain_Key	=	CGD.Local_Domain_Key
	JOIN 	Local_Domain 	LDC ON 	LDC.Domain_Key			=	LDD.Domain_Key
	JOIN 	Concept_Group	CGC ON 	CGC.Local_Domain_Key	=	LDC.Local_Domain_Key
								AND	CGC.Concept_Group_Key	=	@Key
	WHERE 	CGD.Item_Name		=	'Concept Designation Types'
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptDesignationsAvailable_Get') AND SysStat & 0xf = 4)
BEGIN
   	PRINT 'Setting up security on procedure usp_ConceptDesignationsAvailable_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptDesignationsAvailable_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptDesignationsAvailable_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
       	GRANT EXECUTE ON dbo.usp_ConceptDesignationsAvailable_Get TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE  Id = Object_Id(N'[dbo].[usp_ConceptGroupVersions_Select_ForConceptGroup]') AND Type = 'P')
	DROP PROCEDURE [dbo].[usp_ConceptGroupVersions_Select_ForConceptGroup]
GO

/*===========================================================================*\
  Description:	Returns Concept Group Version records.

  Parameters:	@ConceptGroupKey	Concept Group key

  Created:	November 2003

  Last revision information:
    $Revision: 4 $
    $Date: 19/09/08 11:57 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptGroupVersions_Select_ForConceptGroup]
	@ParentKey CHAR(16)
AS

SET NOCOUNT ON

	SELECT DISTINCT
			Concept_Group_Version_Key AS [Key],
		  	Version 
				+ ' ('
				+ dbo.ufn_GetDateFromVagueDate(From_Vague_Date_Start,
								From_Vague_Date_End,
								From_Vague_Date_Type) 
				+ IsNull((' - ' + dbo.ufn_GetDateFromVagueDate(To_Vague_Date_Start,
								To_Vague_Date_End,
								To_Vague_Date_Type)), '')
				+ ')'
			AS Item_Name,
		  	Concept_Group_Key,
			0 AS Has_Children,
			"Sequence"
	FROM 	  	Concept_Group_Version
	WHERE	  	Concept_Group_Key = @ParentKey
	ORDER BY  	Item_Name

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptGroupVersions_Select_ForConceptGroup') AND SysStat & 0xf = 4)
BEGIN
   	PRINT 'Setting up security on procedure usp_ConceptGroupVersions_Select_ForConceptGroup'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
       	GRANT EXECUTE ON dbo.usp_ConceptGroupVersions_Select_ForConceptGroup TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptGroupVersions_Select_ForConceptGroup TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptGroupVersions_Select_ForConceptGroup TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ConceptGroupVersions_Select_ForConceptGroup TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptGroupVersions_Select_ForConceptGroup TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
       	GRANT EXECUTE ON dbo.usp_ConceptGroupVersions_Select_ForConceptGroup TO [Dev - JNCC SQL]
END
GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Specimens_Select_ForCollection]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Specimens_Select_ForCollection]
GO

/*===========================================================================*\
  Description:	Returns Specimens data to the CollectionsBrowser for a given Collection.

  Parameters:
	@ParentKey 		When specified, only the records associated with the parent key are returned
	@UserDomainMask		User's Domain Mask restricting which records may be returned
	@SessionID 		User's SessionID
	@ShowCommonNames	Specifies whether or not Common Names should be shown
	@SortOrderIndex		Index determining Sort Order

  Created:	August 2003

  Last revision information:
    $Revision: 4 $
    $Date: 19/09/08 11:57 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Specimens_Select_ForCollection] 
	@UserDomainMask 	INT,
	@SessionID 			CHAR(16),
	@ParentKey 			CHAR(16),
	@ShowCommonNames 	BIT,
	@SortOrderIndex 	TINYINT
AS

	SET NOCOUNT ON
	-- Set of options for better use of vw_ConceptTerm.
	SET ANSI_NULLS ON
	SET ANSI_PADDING ON
	SET ANSI_WARNINGS ON
	SET ARITHABORT ON
	SET CONCAT_NULL_YIELDS_NULL ON
	SET QUOTED_IDENTIFIER ON
	SET NO_BROWSETABLE OFF

	-- Use temp table to build output, so silly data errors such as duplicate accessions
	-- don't duplicate in the list
	DECLARE @SpecimensSearch TABLE
	(
		Item_Key		CHAR(16) 		COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		Join_Key		CHAR(16) 		COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		Det_Item_Key	CHAR(16) 		COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		Det_Item_Name	NVARCHAR(150) 	COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		Item_Name		NVARCHAR(150) 	COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		Life_Sciences	BIT NULL,
		Number			VARCHAR(30) 	COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		Hint			NVARCHAR(150) 	COLLATE SQL_Latin1_General_CP1_CI_AS NULL 
	)

	INSERT INTO	@SpecimensSearch (Item_Key, Join_Key, Life_Sciences) 
	SELECT 	DISTINCT SU.Collection_Unit_Key, SU.Collection_Unit_Key, SU.Life_Sciences
	FROM 	Specimen_Unit 	SU
	JOIN 	Collection_Unit CU 	ON 	SU.Collection_Unit_Key = CU.Collection_Unit_Key
								AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
								OR 	(CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
	WHERE 	SU.Parent_Collection_Collection_Unit_Key = @ParentKey

	UPDATE		@SpecimensSearch
	SET			Number = CUN.Number
	FROM 		@SpecimensSearch 		SU
	LEFT JOIN 	Collection_Unit_Number 	CUN ON 	SU.Item_key 	= CUN.Collection_Unit_Key 
											AND CUN.Preferred	= 1

	UPDATE 	@SpecimensSearch
	SET		Det_Item_Key	= CPref.Concept_Key,
			Item_Name 		= TPref.Item_Name,
			Det_Item_Name	= TDet.Plaintext
	FROM 	@SpecimensSearch 		SU
	JOIN 	VW_SpecimenDetsEarth 	SDE 	ON 	SDE.Collection_Unit_Key			= SU.Item_Key
											AND SDE.Preferred_Determination_Key	= SDE.Determination_Key
	JOIN 	Concept 				C 		ON 	C.Concept_Key					= SDE.Concept_Key
	JOIN 	Term 					TDet 	ON 	TDet.Term_Key					= C.Term_Key
	JOIN 	Concept 				CPref 	ON 	CPref.Meaning_Key				= C.Meaning_Key
											AND CPref.List_Preferred			= 1
											AND CPref.Concept_Group_Key			= C.Concept_Group_Key
	JOIN 	Term 					TPref 	ON 	TPref.Term_Key					= CPref.Term_Key

	UPDATE 	@SpecimensSearch
	SET		Det_Item_Key	= SDL.Taxon_List_Item_Key,
			Item_Name 		= dbo.ufn_GetFormattedTaxonNameByParams(
				Preferred_Name,
				Preferred_Name_Italic,
				Common_Name,
				Common_Name_Italic,
				NULL,
				@ShowCommonNames),
			Det_Item_Name	= ITN.Actual_Name
	FROM 	@SpecimensSearch 	SU
	JOIN 	VW_SpecimenDetsLife SDL ON 	SDL.Collection_Unit_Key					= SU.Item_Key
									AND SDL.Preferred_Taxon_Determination_Key 	= SDL.Taxon_Determination_Key
	JOIN 	Index_Taxon_Name 	ITN	ON 	ITN.Taxon_List_Item_Key					= SDL.Taxon_List_Item_Key

	-- Select table and sort appropriately
	IF @SortOrderIndex = 0
		SELECT * FROM @SpecimensSearch
		ORDER BY Item_Name, Number
	ELSE 
	IF @SortOrderIndex = 1
		SELECT * FROM @SpecimensSearch
		ORDER BY Number, Item_Name

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Specimens_Select_ForCollection') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_Specimens_Select_ForCollection'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
       	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForCollection TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForCollection TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForCollection TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForCollection TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForCollection TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
       	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForCollection TO [Dev - JNCC SQL]
END
GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Specimens_Select_ForStore]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Specimens_Select_ForStore]
GO

/*===========================================================================*\
  Description:	Returns Specimens data to the CollectionsBrowser for a given Store

  Parameters:	
	@ParentKey 		When specified, only the records associated with the parent key are returned
	@UserDomainMask		User's Domain Mask restricting which records may be returned
	@SessionID 		User's SessionID
	@ShowCommonNames	Specifies whether or not Common Names should be shown
	@SortOrderIndex		Index determining Sort Order

  Created:	October 2003

  Last revision information:
    $Revision: 4 $
    $Date: 19/09/08 11:57 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Specimens_Select_ForStore] 
	@UserDomainMask 	INT,
	@SessionID 			CHAR(16),
	@ParentKey 			CHAR(16),
	@ShowCommonNames 	BIT,
	@SortOrderIndex 	TINYINT
AS
	SET NOCOUNT ON
	-- Set of options for better use of vw_ConceptTerm.
	SET ANSI_NULLS ON
	SET ANSI_PADDING ON
	SET ANSI_WARNINGS ON
	SET ARITHABORT ON
	SET CONCAT_NULL_YIELDS_NULL ON
	SET QUOTED_IDENTIFIER ON
	SET NO_BROWSETABLE OFF


	-- Use temp table to build output, so silly data errors such as duplicate accessions
	-- don't duplicate in the list
	DECLARE @SpecimensSearch TABLE
	(
		Item_Key		CHAR(16) 		COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		Join_Key		CHAR(16) 		COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		Det_Item_Key	CHAR(16) 		COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		Det_Item_Name	NVARCHAR(150) 	COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		Item_Name		NVARCHAR(150) 	COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		Life_Sciences	BIT NULL,
		Number			VARCHAR(30) 	COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		Hint			NVARCHAR(150) 	COLLATE SQL_Latin1_General_CP1_CI_AS NULL 
	)

	INSERT INTO @SpecimensSearch (Item_Key, Join_Key, Life_Sciences) 
	SELECT 	DISTINCT SU.Collection_Unit_Key, SU.Collection_Unit_Key, SU.Life_Sciences
	FROM 	Specimen_Unit 	SU
	JOIN 	Collection_Unit CU 	ON 	SU.Collection_Unit_Key = CU.Collection_Unit_Key 
								AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
								OR 	(CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
	WHERE 	CU.Current_Container_Collection_Unit_Key = @ParentKey



	UPDATE		@SpecimensSearch
	SET			Number = CUN.Number
	FROM 		@SpecimensSearch 		SU
	LEFT JOIN 	Collection_Unit_Number 	CUN ON 	SU.Item_key 	= CUN.Collection_Unit_Key 
											AND CUN.Preferred	= 1

	UPDATE 	@SpecimensSearch
	SET		Det_Item_Key	= CPref.Concept_Key,
			Item_Name 		= TPref.Item_Name,
			Det_Item_Name	= TDet.Plaintext
	FROM 	@SpecimensSearch 		SU
	JOIN 	VW_SpecimenDetsEarth 	SDE 	ON 	SDE.Collection_Unit_Key			= SU.Item_Key
											AND SDE.Preferred_Determination_Key	= SDE.Determination_Key
	JOIN 	Concept 				C 		ON 	C.Concept_Key					= SDE.Concept_Key
	JOIN 	Term 					TDet 	ON 	TDet.Term_Key					= C.Term_Key
	JOIN 	Concept 				CPref 	ON 	CPref.Meaning_Key				= C.Meaning_Key
											AND CPref.List_Preferred			= 1
											AND CPref.Concept_Group_Key			= C.Concept_Group_Key
	JOIN 	Term 					TPref 	ON 	TPref.Term_Key					= CPref.Term_Key

	UPDATE 	@SpecimensSearch
	SET		Det_Item_Key	= SDL.Taxon_List_Item_Key,
			Item_Name 		= dbo.ufn_GetFormattedTaxonNameByParams(
				Preferred_Name,
				Preferred_Name_Italic,
				Common_Name,
				Common_Name_Italic,
				NULL,
				@ShowCommonNames),
			Det_Item_Name	= ITN.Actual_Name
	FROM 	@SpecimensSearch 	SU
	JOIN 	VW_SpecimenDetsLife SDL ON 	SDL.Collection_Unit_Key					= SU.Item_Key
									AND SDL.Preferred_Taxon_Determination_Key 	= SDL.Taxon_Determination_Key
	JOIN 	Index_Taxon_Name 	ITN	ON 	ITN.Taxon_List_Item_Key					= SDL.Taxon_List_Item_Key

	-- Select table and sort appropriately
	IF @SortOrderIndex = 0
		SELECT * FROM @SpecimensSearch
		ORDER BY Item_Name, Number
	ELSE 
	IF @SortOrderIndex = 1
		SELECT * FROM @SpecimensSearch
		ORDER BY Number, Item_Name
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Specimens_Select_ForStore') AND SysStat & 0xf = 4)
BEGIN
   	PRINT 'Setting up security on procedure usp_Specimens_Select_ForStore'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
       	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForStore TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForStore TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForStore TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForStore TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForStore TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
       	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForStore TO [Dev - JNCC SQL]
END
GO