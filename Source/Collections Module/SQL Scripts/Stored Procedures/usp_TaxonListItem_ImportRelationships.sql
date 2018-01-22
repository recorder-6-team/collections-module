/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_TaxonListItem_ImportRelationships]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_TaxonListItem_ImportRelationships]
GO

/*===========================================================================*\
  Description:	Import parent-child relationships between items from the
				specified concept group.

  Parameters:	@job_id					Job identifier

  Created:		Dec 2003

  Last revision information:
	$Revision: 2 $
	$Date: 12/05/04 9:57 $
	$Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_TaxonListItem_ImportRelationships]
	@job_id				INT
AS
	SET NOCOUNT ON

	DECLARE     @concept_group_key				CHAR(16),
				@thesaurus_relation_type_key	CHAR(16),
				@child_list_item_key			CHAR(16),
				@parent_list_item_key			CHAR(16)

	/* determine parameters of job */
	SELECT		@concept_group_key						=	g.Concept_Group_Key,
				@thesaurus_relation_type_key			=	g.Hierarchy_Relation_Type_Key
	FROM		Import_Export_Job						AS	j
	INNER JOIN	Concept_Group							AS	g
	ON			g.Concept_Group_Key						=	j.Concept_Group_Key
	WHERE		j.Import_Export_Job_ID					=	@job_id

	IF @@ROWCOUNT = 0
	BEGIN
		RAISERROR ('Job does not exist or has not been configured', 16, 1)
		GOTO fail
	END

	EXECUTE		usp_Import_Export_Job_UpdateStatus	@job_id,
													'Exporting concept relationships'
	IF @@ERROR <> 0 GOTO fail

	UPDATE		TAXON_LIST_ITEM
	SET			PARENT								=	pm.Taxon_List_Item_Key
	FROM		Concept								AS	c
	INNER JOIN	Taxon_Dictionary_Concept_Mapping	AS	cm
	ON			cm.Concept_Key						=	c.Concept_Key
	INNER JOIN	TAXON_LIST_ITEM
	ON			TAXON_LIST_ITEM.TAXON_LIST_ITEM_KEY	=	cm.Taxon_List_Item_Key
	LEFT JOIN	Concept_Relation					AS	r
	ON			r.To_Concept_Key					=	c.Concept_Key
	AND			r.Thesaurus_Relation_Type_Key		=	@thesaurus_relation_type_key
	LEFT JOIN	Taxon_Dictionary_Concept_Mapping	AS	pm
	ON			pm.Concept_Key						=	r.From_Concept_Key
	WHERE		c.Concept_Group_Key					=	@concept_group_key

	IF @@ERROR <> 0 GOTO fail
	RETURN

fail:
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_TaxonListItem_ImportRelationships failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TaxonListItem_ImportRelationships') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_TaxonListItem_ImportRelationships'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_TaxonListItem_ImportRelationships TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_TaxonListItem_ImportRelationships TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_TaxonListItem_ImportRelationships TO [Dev - JNCC SQL]
END
GO
