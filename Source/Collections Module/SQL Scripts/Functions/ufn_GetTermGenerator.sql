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
		$Revision: 7 $
		$Date: 15/09/11 11:02 $
		$Author: Jamesbichard $
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