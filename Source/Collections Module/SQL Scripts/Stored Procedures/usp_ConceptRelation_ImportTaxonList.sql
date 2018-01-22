/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptRelation_ImportTaxonList]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_ConceptRelation_ImportTaxonList]
GO

/*===========================================================================*\
  Description:	Import relationships between concepts from the specified
				taxon list.

  Parameters:	@job_id					Job identifier

  Created:		Dec 2003

  Last revision information:
	$Revision: 7 $
	$Date: 12/05/04 9:57 $
	$Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptRelation_ImportTaxonList]
	@job_id				INT
AS
	SET NOCOUNT ON

	DECLARE     @concept_group_key				CHAR(16),
				@taxon_list_key					CHAR(16),
				@thesaurus_relation_type_key	CHAR(16),
				@parent_concept_key				CHAR(16),
				@child_concept_key				CHAR(16),
				@changed_session_id				CHAR(16),
				@system							BIT,
				@changed						BIT,
				@concept_relation_key			CHAR(16)

	/* determine parameters of job */
	SELECT      @concept_group_key						=	m.Concept_Group_Key,
				@taxon_list_key							=	m.Taxon_List_Key,
				@thesaurus_relation_type_key			=	g.Hierarchy_Relation_Type_Key
	FROM		Import_Export_Job						AS	j
	INNER JOIN	Taxon_Dictionary_Concept_Group_Mapping	AS	m
	ON			m.Concept_Group_Key						=	j.Concept_Group_Key
	INNER JOIN	Concept_Group							AS	g
	ON			g.Concept_Group_Key						=	m.Concept_Group_Key
	WHERE		j.Import_Export_Job_ID					=	@job_id

	IF @@ROWCOUNT = 0
	BEGIN
		RAISERROR ('Job does not exist or has not been configured', 16, 1)
		RETURN
	END

	EXECUTE		usp_Import_Export_Job_UpdateStatus	@job_id,
													'Importing concept relationships'
	IF @@ERROR <> 0 RETURN

	DECLARE		concepts	CURSOR LOCAL FAST_FORWARD FOR
	SELECT		p.Concept_Key,
				c.Concept_Key,
				ISNULL(c.Changed_Session_ID,
					   c.Entered_Session_ID),
				c.System_Supplied_Data
	FROM		TAXON_LIST_VERSION					AS	tlv
	INNER JOIN	TAXON_LIST_ITEM						AS	tli
	ON			tli.TAXON_LIST_VERSION_KEY			=	tlv.TAXON_LIST_VERSION_KEY
	INNER JOIN	Taxon_Dictionary_Concept_Mapping	AS	cm
	ON			cm.Taxon_List_Item_Key				=	tli.TAXON_LIST_ITEM_KEY
	INNER JOIN	Concept								AS	c
	ON			c.Concept_Key						=	cm.Concept_Key
	LEFT JOIN	Taxon_Dictionary_Concept_Mapping	AS	pm
	ON			pm.Taxon_List_Item_Key				=	tli.PARENT
	LEFT JOIN	Concept								AS	p
	ON			p.Concept_Key						=	pm.Concept_Key
	WHERE		tlv.TAXON_LIST_KEY					=	@taxon_list_key

	OPEN		concepts

	WHILE 1 = 1
	BEGIN
		FETCH		concepts
		INTO		@parent_concept_key,
					@child_concept_key,
					@changed_session_id,
					@system

		IF @@FETCH_STATUS <> 0 BREAK

		BEGIN TRANSACTION

		SELECT      @changed					=	CASE WHEN System_Supplied_Data = @system
														THEN 0
														ELSE 1
													END
		FROM		Concept_Relation
		WHERE		From_Concept_Key			=	@parent_concept_key
		AND			To_Concept_Key				=	@child_concept_key
		AND			Thesaurus_Relation_Type_Key	=   @thesaurus_relation_type_key

		IF @@ROWCOUNT > 0
		BEGIN
			IF @changed = 1
			BEGIN
				/* update existing relationship */
				UPDATE		Concept_Relation
				SET			Changed_Session_ID			=	@changed_session_id,
							System_Supplied_Data		=	@system
				WHERE		To_Concept_Key				=	@child_concept_key
				AND			Thesaurus_Relation_Type_Key	=	@thesaurus_relation_type_key

				IF @@ERROR <> 0 GOTO fail_from_cursor
			END
		END
		ELSE
		BEGIN
			/* remove any old relationships */
			DELETE		Concept_Relation
			WHERE		To_Concept_Key				=	@child_concept_key
			AND			Thesaurus_Relation_Type_Key	=	@thesaurus_relation_type_key

			IF @@ERROR <> 0 GOTO fail_from_cursor

			IF @parent_concept_key IS NOT NULL
			BEGIN
				/* create new relationship */
				EXECUTE     spNextKey	'Concept_Relation',
										@concept_relation_key	OUTPUT
				IF @@ERROR <> 0 GOTO fail_from_cursor

				INSERT		Concept_Relation (
							Concept_Relation_Key,
							From_Concept_Key,
							To_Concept_Key,
							Thesaurus_Relation_Type_Key,
							Entered_Session_ID,
							System_Supplied_Data)
				VALUES		(@concept_relation_key,
							@parent_concept_key,
							@child_concept_key,
							@thesaurus_relation_type_key,
							@changed_session_id,
							@system)

				IF @@ERROR <> 0 GOTO fail_from_cursor
			END
		END

		EXECUTE		usp_Import_Export_Job_RecordProcessed	@job_id
		IF @@ERROR <> 0 GOTO fail_from_cursor

		COMMIT TRANSACTION
	END

	CLOSE		concepts
	RETURN

fail_from_relations:
	CLOSE		relations
	
fail_from_cursor:
	CLOSE		concepts

fail:
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_ConceptRelation_ImportTaxonList failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptRelation_ImportTaxonList') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptRelation_ImportTaxonList'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptRelation_ImportTaxonList TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptRelation_ImportTaxonList TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptRelation_ImportTaxonList TO [Dev - JNCC SQL]
END
GO