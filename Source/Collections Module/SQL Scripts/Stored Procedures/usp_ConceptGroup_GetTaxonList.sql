/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptGroup_GetTaxonList]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_ConceptGroup_GetTaxonList]
GO

/*===========================================================================*\
  Description:	Determine the taxon list (if any) associated with the
				specified concept group.

  Parameters:	@concept_group_key		Concept group key
				@taxon_list_key			[on exit] Taxon list key, or NULL

  Created:		Jan 2004

  Last revision information:
	$Revision: 2 $
	$Date: 12/05/04 9:57 $
	$Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptGroup_GetTaxonList]
	@concept_group_key			CHAR(16),
	@taxon_list_key				CHAR(16)	OUTPUT
AS
	SET NOCOUNT ON

	SELECT		@taxon_list_key							=	Taxon_List_Key
	FROM		Taxon_Dictionary_Concept_Group_Mapping
	WHERE		Concept_Group_Key						=	@concept_group_key

	IF @@ROWCOUNT = 0
	BEGIN
		SET			@taxon_list_key		=	NULL
	END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptGroup_GetTaxonList') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptGroup_GetTaxonList'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptGroup_GetTaxonList TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptGroup_GetTaxonList TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptGroup_GetTaxonList TO [Dev - JNCC SQL]
END
GO