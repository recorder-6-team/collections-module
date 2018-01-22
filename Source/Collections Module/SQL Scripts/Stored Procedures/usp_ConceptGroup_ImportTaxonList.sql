/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptGroup_ImportTaxonList]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_ConceptGroup_ImportTaxonList]
GO

/*===========================================================================*\
  Description:	Import a taxon list into the specified concept group.

  Parameters:   @job_id					Job identifier
				@taxon_list_key			Taxon list key
				@concept_group_key		Concept group key

  Created:		Nov 2003

  Last revision information:
	$Revision: 20 $
	$Date: 12/02/09 15:50 $
	$Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptGroup_ImportTaxonList]
	@job_id				INT,
	@taxon_list_key		CHAR(16),
	@concept_group_key	CHAR(16)
AS
	SET NOCOUNT ON
	SET ARITHABORT ON
	SET ANSI_WARNINGS OFF

	DECLARE		@existing_group_key		CHAR(16)

	SELECT		@existing_group_key						=	Concept_Group_Key
	FROM		Taxon_Dictionary_Concept_Group_Mapping
	WHERE		Taxon_List_Key							=	@taxon_list_key

	IF @@ROWCOUNT = 0
	BEGIN
		BEGIN TRANSACTION

		/* record mapping */
		INSERT		Taxon_Dictionary_Concept_Group_Mapping (
					Taxon_List_Key,
					Concept_Group_Key)
		VALUES		(@taxon_list_key,
					@concept_group_key)

		IF @@ERROR <> 0 GOTO fail

		COMMIT TRANSACTION
	END
	ELSE IF @existing_group_key <> @concept_group_key
	BEGIN
		RAISERROR (
			'Taxon list has previously been imported into a different group',
			16,
			1)
		RETURN
	END

	/* Calculate size of job */
	DECLARE		@record_count					INT

	DECLARE		@items	TABLE (
				Taxon_List_Item_Key			CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS,
				Taxon_List_Version_Key		CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS,
				Taxon_Rank_Key				CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS,
				Taxon_Version_Key			CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS,
				List_Preferred_Key			CHAR(16))

	INSERT		@items
	SELECT    	tli.TAXON_LIST_ITEM_KEY,
				tli.TAXON_LIST_VERSION_KEY,
				tli.TAXON_RANK_KEY,
				tli.TAXON_VERSION_KEY,
				CASE WHEN tli.TAXON_LIST_ITEM_KEY = tli.PREFERRED_NAME
					THEN tli.TAXON_LIST_ITEM_KEY
					ELSE NULL
				END
	FROM        TAXON_LIST_VERSION			AS	tlv
	INNER JOIN	TAXON_LIST_ITEM				AS	tli
	ON			tli.TAXON_LIST_VERSION_KEY	=	tlv.TAXON_LIST_VERSION_KEY
	WHERE		tlv.TAXON_LIST_KEY			=	@taxon_list_key

	SELECT		@record_count				=	2 * COUNT(DISTINCT tli.Taxon_List_Item_Key)
												+ COUNT(DISTINCT tli.Taxon_List_Version_Key)
												+ COUNT(DISTINCT tli.Taxon_Rank_Key)
												+ COUNT(DISTINCT tli.Taxon_Version_Key)
												+ COUNT(DISTINCT tv.TAXON_KEY)
												+ COUNT(DISTINCT ts.SOURCE_LINK_KEY)
												+ COUNT(DISTINCT tx.TAXON_NAME_TYPE_KEY)
												+ COUNT(DISTINCT td.TAXON_DESIGNATION_KEY)
												+ COUNT(DISTINCT td.TAXON_DESIGNATION_TYPE_KEY)
												+ COUNT(DISTINCT tf.TAXON_FACT_KEY)
												+ COUNT(DISTINCT tli.List_Preferred_Key)
	FROM        @items						AS	tli
	INNER JOIN	TAXON_VERSION				AS	tv
	ON			tv.TAXON_VERSION_KEY		=	tli.Taxon_Version_Key
	INNER JOIN	TAXON						AS	tx
	ON			tx.TAXON_KEY				=	tv.TAXON_KEY
	LEFT JOIN	TAXON_SOURCES				AS	ts
	ON			ts.TAXON_KEY				=	tx.TAXON_KEY
	LEFT JOIN	TAXON_DESIGNATION			AS	td
	ON			td.TAXON_LIST_ITEM_KEY		=	tli.Taxon_List_Item_Key
	LEFT JOIN	TAXON_FACT					AS	tf
	ON			tf.TAXON_VERSION_KEY		=	tli.Taxon_Version_Key

	SET ANSI_WARNINGS ON
	
	EXECUTE		usp_Import_Export_Job_Configure		@job_id,
													@concept_group_key,
													@record_count
	IF @@ERROR <> 0 RETURN

	/* import versions */
	EXECUTE		usp_ConceptGroupVersion_ImportTaxonList		@job_id
	IF @@ERROR <> 0 RETURN

	/* import terms */
	EXECUTE		usp_Term_ImportTaxonList	@job_id
	IF @@ERROR <> 0 RETURN

	/* import term versions */
	EXECUTE		usp_TermVersion_ImportTaxonList		@job_id
	IF @@ERROR <> 0 RETURN

	/* import concept ranks */
	EXECUTE		usp_ConceptRank_ImportTaxonList     @job_id
	IF @@ERROR <> 0 RETURN

	/* import name type concepts */
	EXECUTE		usp_Concept_ImportTaxonNameTypes    @job_id
	IF @@ERROR <> 0 RETURN

	/* import concepts */
	EXECUTE		usp_Concept_ImportTaxonList		@job_id
	IF @@ERROR <> 0 RETURN

	/* import concept relationships */
	EXECUTE		usp_ConceptRelation_ImportTaxonList		@job_id
	IF @@ERROR <> 0 RETURN

	/* (re-)create concept lineage */
	EXECUTE     usp_ConceptLineage_GenerateForGroup		@job_id
	IF @@ERROR <> 0 RETURN

	/* import term/source relationships */
	EXECUTE		usp_SourceJoin_ImportTaxonSources	@job_id
	IF @@ERROR <> 0 RETURN

	/* import designation types */
	EXECUTE		usp_Concept_ImportTaxonDesignationTypes		@job_id
	IF @@ERROR <> 0 RETURN

	/* import concept designations */
	EXECUTE		usp_ConceptDesignation_ImportTaxonList		@job_id
	IF @@ERROR <> 0 RETURN

	/* import thesaurus facts */
	EXECUTE		usp_ThesaurusFact_ImportTaxonList	@job_id
	IF @@ERROR <> 0 RETURN

	RETURN

fail:
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_ConceptGroup_ImportTaxonList failed', 16, 1)

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptGroup_ImportTaxonList') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptGroup_ImportTaxonList'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptGroup_ImportTaxonList TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptGroup_ImportTaxonList TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptGroup_ImportTaxonList TO [Dev - JNCC SQL]
END
GO
