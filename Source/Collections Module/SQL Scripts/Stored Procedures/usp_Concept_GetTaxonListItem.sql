/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_GetTaxonListItem') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_Concept_GetTaxonListItem]
GO

/*===========================================================================*\
  Description:	Determine the taxon (if any) associated with the
		specified concept.

  Parameters:	@ConceptKey
		@TaxonListItemKey [on exit] Taxon list item key, or NULL

  Created:	September 2004

  Last revision information:
	$Revision: 1 $
	$Date: 8/09/04 11:11 $
	$Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Concept_GetTaxonListItem]
	@ConceptKey char(16),
	@TaxonListItemKey char(16) OUTPUT
AS
	SET NOCOUNT ON

	SELECT	@TaxonListItemKey = Taxon_List_Item_Key
	FROM	Taxon_Dictionary_Concept_Mapping
	WHERE	Concept_Key = @ConceptKey

	IF @@ROWCOUNT = 0
		SET @TaxonListItemKey =	NULL
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_GetTaxonListItem') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Concept_GetTaxonListItem'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concept_GetTaxonListItem TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concept_GetTaxonListItem TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Concept_GetTaxonListItem TO [Dev - JNCC SQL]
END
GO