/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ThesaurusFacts_Select_ForConcept]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ThesaurusFacts_Select_ForConcept]
GO

/*===========================================================================*\
  Description:	Returns data from the Thesaurus_Fact table

  Parameters:	@Key	Concept_Key

  Created:	December 2003

  Last revision information:
    $Revision: 2 $
    $Date: 17/12/03 12:19 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ThesaurusFacts_Select_ForConcept]
	@Key char(16)
AS

SET NOCOUNT ON

	-- Unfinished!!!!!!!!!!!!
	-- If 'Applies to: All related versions of this term' is selected,
	-- not all required records will be returned by this proc.

	DECLARE @ConceptKey char(16)
	DECLARE @MeaningKey char(16)
	DECLARE @TermVersionKey char(16)

	-- Get the Concept, Meaning and Term Version keys
	SELECT	@ConceptKey = Concept_Key,
		@MeaningKey = Meaning_Key,
		@TermVersionKey = Term_Version_Key
	FROM	Concept
	WHERE	Concept_Key = @Key
	
	-- Get all the correct Thesaurus_facts	
	SELECT 	
		Thesaurus_Fact_Key AS Item_Key,
		Item_Name
	FROM	Thesaurus_Fact
	WHERE	Concept_Key = @ConceptKey

	UNION

	SELECT 	
		Thesaurus_Fact_Key AS Item_Key,
		Item_Name
	FROM	Thesaurus_Fact
	WHERE	Meaning_Key = @MeaningKey

	UNION

	SELECT 	
		Thesaurus_Fact_Key AS Item_Key,
		Item_Name
	FROM	Thesaurus_Fact
	WHERE	Term_Version_Key = @TermVersionKey

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ThesaurusFacts_Select_ForConcept') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ThesaurusFacts_Select_ForConcept'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ThesaurusFacts_Select_ForConcept TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ThesaurusFacts_Select_ForConcept TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ThesaurusFacts_Select_ForConcept TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ThesaurusFacts_Select_ForConcept TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ThesaurusFacts_Select_ForConcept TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ThesaurusFacts_Select_ForConcept TO [Dev - JNCC SQL]
END

GO