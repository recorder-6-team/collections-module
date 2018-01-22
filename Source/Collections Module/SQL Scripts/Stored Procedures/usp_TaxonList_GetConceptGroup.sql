/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_TaxonList_GetConceptGroup]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_TaxonList_GetConceptGroup]
GO

/*===========================================================================*\
  Description:	Determine the concept group (if any) associated with the
				specified taxon list.

  Parameters:	@taxon_list_key			Taxon list key
				@concept_group_key		[on exit] Concept group key, or NULL

  Created:		Jan 2004

  Last revision information:
	$Revision: 2 $
	$Date: 12/05/04 9:57 $
	$Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_TaxonList_GetConceptGroup]
	@taxon_list_key				CHAR(16),
	@concept_group_key			CHAR(16)	OUTPUT
AS
	SET NOCOUNT ON

	SELECT		@concept_group_key						=	Concept_Group_Key
	FROM		Taxon_Dictionary_Concept_Group_Mapping
	WHERE		Taxon_List_Key							=	@taxon_list_key

	IF @@ROWCOUNT = 0
	BEGIN
		SET			@concept_group_key	=	NULL
	END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TaxonList_GetConceptGroup') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_TaxonList_GetConceptGroup'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_TaxonList_GetConceptGroup TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_TaxonList_GetConceptGroup TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_TaxonList_GetConceptGroup TO [Dev - JNCC SQL]
END
GO