/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_TaxonList_ImportConceptGroup]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_TaxonList_ImportConceptGroup]
GO

/*===========================================================================*\
  Description:	Import a concept group as a taxon list.

  Parameters:   @job_id					Job identifier
				@taxon_list_key			Taxon list key
				@concept_group_key		Concept group key

  Created:		Dec 2003

  Last revision information:
	$Revision: 1 $
	$Date: 6/03/07 16:14 $
	$Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_TaxonList_ImportConceptGroup]
	@job_id				INT,
	@taxon_list_key		CHAR(16),
	@concept_group_key	CHAR(16),
	@SessionID	CHAR(16)
AS
	SET NOCOUNT ON

	DECLARE		@existing_list_key		CHAR(16)

	SELECT		@existing_list_key						=	Taxon_List_Key
	FROM		Taxon_Dictionary_Concept_Group_Mapping
	WHERE		Concept_Group_Key						=	@concept_group_key

	IF @@ROWCOUNT = 0
	BEGIN
		/* record mapping */
		INSERT		Taxon_Dictionary_Concept_Group_Mapping (
					Taxon_List_Key,
					Concept_Group_Key)
		VALUES		(@taxon_list_key,
					@concept_group_key)

		IF @@ERROR <> 0 RETURN
	END
	ELSE IF @existing_list_key <> @taxon_list_key
	BEGIN
		RAISERROR (
			'Concept group has previously been imported into a different taxon list',
			16,
			1)
		RETURN
	END

	/* Calculate size of job */
	DECLARE		@record_count			INT

	SELECT		@record_count			=	COUNT(*)
	FROM		Concept_Group_Version
	WHERE		Concept_Group_Key		=	@concept_group_key

	/* COUNT(column) generates a warning if NULLs are encountered. And this ends up coming out as
	   an error after the import. Not good. So turn it off to avoid that situation. */
	SET ANSI_WARNINGS OFF

	SELECT		@record_count							=	@record_count * 3
															+ COUNT(DISTINCT c.Name_Type_Concept_Key)
															+ COUNT(DISTINCT c.Term_Key)
															+ COUNT(DISTINCT c.Term_Version_Key)
															+ COUNT(DISTINCT j.Source_Join_Key)
															+ COUNT(DISTINCT c.Concept_Rank_Key)
															+ COUNT(DISTINCT c.Concept_Key)
															+ COUNT(DISTINCT d.Designation_Type_Concept_Key)
															+ COUNT(DISTINCT d.Concept_Designation_Key)
															+ COUNT(DISTINCT f.Thesaurus_Fact_Key
																	+ vm.Term_Version_Key)
	FROM		Concept									AS	c
	LEFT JOIN	Source_Join								AS	j
	ON			j.Record_Key							=	c.Term_Key
	AND			j.Table_Name							=	'Term'
	LEFT JOIN	Concept_Designation						AS	d
	ON			d.Concept_Key							=	c.Concept_Key
	LEFT JOIN	Thesaurus_Fact							AS	f
	ON			f.Meaning_Key							=	c.Meaning_Key
	LEFT JOIN	Taxon_Dictionary_Term_Version_Mapping	AS	vm
	ON			vm.Term_Version_Key						=	c.Term_Version_Key
	WHERE		c.Concept_Group_Key						=	@concept_group_key
	AND 		j.Source_Join_Key IS NOT NULL

	EXECUTE		usp_Import_Export_Job_Configure		@job_id,
													@concept_group_key,
													@record_count
	IF @@ERROR <> 0 RETURN
	
	/* import versions */
	EXECUTE		usp_TaxonListVersion_ImportConceptGroup		@job_id
	IF @@ERROR <> 0 RETURN

	/* import name types */
	EXECUTE		usp_TaxonNameType_ImportConceptGroup	@job_id
	IF @@ERROR <> 0 RETURN

	/* import taxa */
	EXECUTE		usp_Taxon_ImportConceptGroup	@job_id
	IF @@ERROR <> 0 RETURN

	/* import taxon versions */
	EXECUTE		usp_TaxonVersion_ImportConceptGroup		@job_id, @SessionID
	IF @@ERROR <> 0 RETURN

	/* import taxon/source relationships */
	EXECUTE		usp_TaxonSources_ImportConceptGroup		@job_id
	IF @@ERROR <> 0 RETURN

	/* import taxon ranks */
	EXECUTE		usp_TaxonRank_ImportConceptGroup	@job_id
	IF @@ERROR <> 0 RETURN

	/* import taxon list items */
	EXECUTE		usp_TaxonListItem_ImportConceptGroup	@job_id
	IF @@ERROR <> 0 RETURN

	/* import hierarchical relationships */
	EXECUTE		usp_TaxonListItem_ImportRelationships	@job_id
	IF @@ERROR <> 0 RETURN

	/* import designation types */
	EXECUTE		usp_TaxonDesignationType_ImportConceptGroup		@job_id
	IF @@ERROR <> 0 RETURN

	/* import designations */
	EXECUTE		usp_TaxonDesignation_ImportConceptGroup		@job_id
	IF @@ERROR <> 0 RETURN

	/* import facts */
	EXECUTE		usp_TaxonFact_ImportConceptGroup	@job_id
	IF @@ERROR <> 0 RETURN 

	EXECUTE		usp_Import_Export_Job_UpdateStatus	@job_id,
													'Updating Taxon Names Index...'
	/* Discard Index_Taxon_Name records for the concept group */
	DELETE ITN
	FROM Index_Taxon_Name ITN
	INNER JOIN Taxon_Dictionary_Concept_Mapping TDM ON TDM.Taxon_List_Item_Key=ITN.Taxon_List_Item_Key
	INNER JOIN Concept C1 ON C1.Concept_Key=TDM.Concept_Key
			AND C1.Concept_Group_Key=@Concept_Group_Key
	
	/* Rebuild Index_Taxon_Name for the concept group */
	INSERT INTO Index_Taxon_Name (Taxon_List_Item_Key, Taxon_List_Version_Key,
	 Actual_Name, Actual_Name_Italic, Common_Name, Common_Name_Italic, 
	  Preferred_Name, Preferred_Name_Italic, Abbreviation, Authority, System_Supplied_Data )
	SELECT TLI.Taxon_List_Item_Key, TLI.Taxon_List_Version_Key, 
	  T.Item_Name, CASE WHEN TR3.List_Font_Italic = 1 AND T.Language = 'La' THEN 1 ELSE 0 END, 
	  T2.Item_Name, CASE WHEN TR3.List_Font_Italic = 1 AND T2.Language = 'La' THEN 1 ELSE 0 END, 
	  T3.Item_Name, CASE WHEN TR3.List_Font_Italic = 1 AND T3.Language = 'La' THEN 1 ELSE 0 END, 
	  T.Abbreviation, T.Authority, 1 
	FROM ((((((((Taxon_List_Item AS TLI 
	INNER JOIN Taxon_Dictionary_Concept_Mapping TDM ON TDM.Taxon_List_Item_Key=TLI.Taxon_List_Item_Key
	INNER JOIN Concept C1 ON C1.Concept_Key=TDM.Concept_Key
			AND C1.Concept_Group_Key=@Concept_Group_Key
	LEFT JOIN Taxon_version AS TV ON TV.Taxon_Version_Key = TLI.Taxon_Version_Key) 
	LEFT JOIN Taxon AS T ON T.Taxon_Key = TV.Taxon_Key) 
	LEFT JOIN Taxon_Common_Name AS TCN ON TCN.Taxon_List_Item_Key = TLI.Taxon_List_Item_Key) 
	LEFT JOIN Taxon_Version AS TV2 ON TV2.Taxon_Version_Key = TCN.Taxon_Version_Key) 
	LEFT JOIN Taxon AS T2 ON T2.Taxon_Key = TV2.Taxon_Key) 
	LEFT JOIN Taxon_List_Item AS TLI3 ON TLI3.Taxon_List_Item_Key = TLI.Preferred_Name) 
	LEFT JOIN Taxon_Rank AS TR3 ON TR3.Taxon_Rank_Key = TLI3.Taxon_Rank_Key) 
	LEFT JOIN Taxon_Version AS TV3 ON TV3.Taxon_Version_Key = TLI3.Taxon_Version_Key) 
	LEFT JOIN Taxon AS T3 ON T3.Taxon_Key = TV3.Taxon_Key 
	WHERE TLI.Taxon_List_Version_To IS NULL

	/* Update to include the Has_Children field */
	UPDATE ITN
	SET Has_Children=1
	FROM Index_Taxon_Name ITN
	INNER JOIN Taxon_List_Item TLIChild ON TLIChild.Parent=ITN.Taxon_List_Item_Key
	INNER JOIN Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key=ITN.Taxon_List_Version_Key
		AND TLV.Taxon_List_Key=@taxon_list_key

	EXEC usp_IndexTaxonName_ApplyNameServer_SingleList @taxon_list_key

	UPDATE Import_Export_Job
	SET Records_Processed = Records_Processed + @@ROWCOUNT
	WHERE Import_Export_Job_ID = @job_id
	
	EXECUTE		usp_Import_Export_Job_UpdateStatus	@job_id,
													'Exporting Taxon Common Names...'	

	/* Create a local table containing the taxon common name data */
	DECLARE @TaxonCommonName TABLE (
		Taxon_List_Item_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS PRIMARY KEY,
		Taxon_Version_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS
	)

	/* Get the Taxon_List_Item_Keys first, as there may be several Taxon_Version_Keys for some, 
	  and that would break the primary key constraint. */
	INSERT INTO @TaxonCommonName
	SELECT DISTINCT TDM1.Taxon_List_Item_Key, NULL
	FROM 	Taxon_Dictionary_Concept_Mapping TDM1 
	JOIN 	Concept C1 
			ON C1.Concept_Key = TDM1.Concept_Key
			AND C1.Concept_Group_Key = @Concept_Group_Key

	/* Now get a Taxon_Version_Key for each Taxon_List_Item_Key found, it'll use just one, thus 
	  being ok with the primary key constraint.  */
	UPDATE 	TCNTemp
	SET 	Taxon_Version_Key = TLI.Taxon_Version_Key
	FROM 	@TaxonCommonName TCNTemp
	INNER JOIN Taxon_Dictionary_Concept_Mapping TDM1 ON TDM1.Taxon_List_Item_Key = TCNTemp.Taxon_List_Item_Key
	INNER JOIN Concept C1 ON C1.Concept_Key=TDM1.Concept_Key
			AND C1.Concept_Group_Key=@Concept_Group_Key
	LEFT JOIN (
			Concept C2 
			INNER JOIN Term T ON T.Term_Key=C2.Term_Key
			INNER JOIN Language L ON L.Language_Key=T.Language_Key AND L.Priority=1
		) ON C2.Meaning_Key=C1.Meaning_Key
			AND C2.Preferred=1
			AND C2.Name_Type_Concept_Key='SYSTEM000000000L'
	LEFT JOIN Concept C3 ON C3.Meaning_Key=C1.Meaning_Key
		AND C3.List_Preferred=1
		AND C3.Concept_Group_Key=C1.Concept_Group_Key
	INNER JOIN Taxon_Dictionary_Concept_Mapping TDM2 ON TDM2.Concept_Key=ISNULL(C2.Concept_Key, C3.Concept_Key)
	INNER JOIN Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key=TDM2.Taxon_List_Item_Key

	UPDATE Import_Export_Job
	SET Records_Processed = Records_Processed + @@ROWCOUNT
	WHERE Import_Export_Job_ID = @job_id

	/* Update existing taxon common name records that are out of date */
	UPDATE TCN
	SET Taxon_Version_Key=TCNTmp.Taxon_Version_Key
	FROM @TaxonCommonName TCNTmp
	INNER JOIN Taxon_Common_Name TCN ON TCN.Taxon_List_Item_Key=TCNTmp.Taxon_List_Item_Key
	WHERE TCN.Taxon_Version_Key=TCNTmp.Taxon_Version_Key

	/* For new Taxon_Common_Name records, if no common name in the Thesaurus then
	link to itself */
	UPDATE TCNTmp
	SET TCNTmp.Taxon_Version_Key=TLI.Taxon_Version_Key
	FROM @TaxonCommonName TCNTmp
	INNER JOIN Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key=TCNTmp.Taxon_List_Item_Key
	WHERE TCNTmp.Taxon_Version_Key IS NULL
		
	/* Insert any new required taxon common name records */
	INSERT INTO Taxon_Common_Name
	SELECT DISTINCT TCNTmp.Taxon_List_Item_Key, TCNTmp.Taxon_Version_Key
	FROM @TaxonCommonName TCNTmp
	LEFT JOIN Taxon_Common_Name TCN ON TCN.Taxon_List_Item_Key=TCNTmp.Taxon_List_Item_Key
	WHERE TCN.Taxon_List_Item_Key IS NULL

	UPDATE Import_Export_Job
	SET Records_Processed = Records_Processed + @@ROWCOUNT
	WHERE Import_Export_Job_ID = @job_id
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TaxonList_ImportConceptGroup') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_TaxonList_ImportConceptGroup'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_TaxonList_ImportConceptGroup TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_TaxonList_ImportConceptGroup TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_TaxonList_ImportConceptGroup TO [Dev - JNCC SQL]
END
GO