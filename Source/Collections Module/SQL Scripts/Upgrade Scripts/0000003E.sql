/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TaxonListItem_EnableAllListVersions') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_TaxonListItem_EnableAllListVersions]
GO

/*===========================================================================*\
  Description:
	Enable data entry for all taxa across all list versions linked to the 
	given list.

  Parameters:	@taxon_list_key	Taxon List key

  Created:		Oct 2008

  Last revision information:
	$Revision: 4 $
	$Date: 29/01/09 16:54 $
	$Author: Simonwood $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_TaxonListItem_EnableAllListVersions]
	@taxon_list_key CHAR(16)
AS
	SET NOCOUNT ON

	DECLARE @Expired TABLE (
		Taxon_Version_Key	CHAR(16) COLLATE database_default,
		Version				INT
	)

	-- Get the latest version of expired items.
	INSERT INTO @Expired (Taxon_Version_Key, Version)
	SELECT	TLI1.Taxon_Version_Key, MAX(TLV1.Version)
	FROM	Taxon_List_Item		TLI1
	JOIN	Taxon_List_Version	TLV1	ON	TLV1.Taxon_List_Version_Key = TLI1.Taxon_List_Version_Key
										AND	TLV1.Taxon_List_Key			= @taxon_list_key
	WHERE	TLI1.Taxon_Version_Key NOT IN (
		SELECT	TLI2.Taxon_Version_Key
		FROM	Taxon_List_Item		TLI2
		JOIN	Taxon_List_Version	TLV2	ON	TLV2.Taxon_List_Version_Key = TLI2.Taxon_List_Version_Key
											AND	TLV2.Taxon_List_Key			= @taxon_list_key
		AND		TLI2.Taxon_List_Version_To 	IS 	NULL
	)
	GROUP BY TLI1.Taxon_Version_Key

	-- Now set the Taxon_List_Version_To to NULL to "re-enable" data entry for these.
	UPDATE	TLI1
	SET		Taxon_List_Version_To 		= 	null
	FROM	@Expired			E
	JOIN	Taxon_List_Item		TLI1	ON 	TLI1.Taxon_Version_Key		= E.Taxon_Version_Key
	JOIN	Taxon_List_Version	TLV1	ON	TLV1.Taxon_List_Version_Key = TLI1.Taxon_List_Version_Key
										AND	TLV1.Version				= E.Version
										AND	TLV1.Taxon_List_Key			= @taxon_list_key

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TaxonListItem_EnableAllListVersions') AND SysStat & 0xf = 4)
BEGIN
	PRINT 'Setting up security on procedure usp_TaxonListItem_EnableAllListVersions'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_TaxonListItem_EnableAllListVersions TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_TaxonListItem_EnableAllListVersions TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
		GRANT EXECUTE ON dbo.usp_TaxonListItem_EnableAllListVersions TO [Dev - JNCC SQL]
END
GO


If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_DeterminationsLifeSciences_Select_ForSearch]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
	DROP PROCEDURE [dbo].[usp_DeterminationsLifeSciences_Select_ForSearch]
GO

/*===========================================================================*\
  Description:
	Returns Concept_Key and DisplayTerm when search characters are entered.
	Uses domain mask if the taxon list item is mapped

  Parameters:
	@SearchText
	@UserDomainMask
	@SearchSize

  Created:	October 2003

  Last revision information:
	$Revision: 4 $
	$Date: 29/01/09 16:54 $
	$Author: Simonwood $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_DeterminationsLifeSciences_Select_ForSearch] 
	@SearchText 	VARCHAR(100),
	@UserDomainMask INT,
	@SearchSize 	INT = NULL
AS

	SET NOCOUNT ON

    SET @SearchSize = ISNULL(@SearchSize, 0)

    SET ROWCOUNT @SearchSize

	SELECT		ITN.Taxon_List_Item_Key AS Item_Key, 
				dbo.ufn_GetFormattedSpeciesName(ITN.Taxon_List_Item_Key) + ' - ' + TL.Item_Name AS DisplayTerm,
				dbo.ufn_GetFormattedSpeciesName(ITN.Taxon_List_Item_Key) + ' - ' + TL.Item_Name AS SearchTerm
	FROM		Index_Taxon_Name 					ITN 
	INNER JOIN 	Taxon_List_Version 					TLV 	ON TLV.Taxon_List_Version_Key 	= ITN.Taxon_List_Version_Key
	INNER JOIN 	Taxon_List 							TL 		ON TL.Taxon_List_Key			= TLV.Taxon_List_Key
	LEFT JOIN	Taxon_Dictionary_Concept_Mapping 	TDCM	ON TDCM.Taxon_List_Item_Key 	= ITN.Taxon_List_Item_Key
	LEFT JOIN	Concept 							C 		ON C.Concept_Key 				= TDCM.Concept_Key
	LEFT JOIN 	Concept_Group 						CG 		ON CG.Concept_Group_Key			= C.Concept_Group_Key
	LEFT JOIN 	Local_Domain 						LD 		ON LD.Local_Domain_Key			= CG.Local_Domain_Key
	LEFT JOIN 	Domain 								D 		ON D.Domain_Key					= LD.Domain_Key
	WHERE 		ITN.Actual_Name LIKE @SearchText + '%'
	AND 		((((D.Domain_Mask & @UserDomainMask > 0) OR (D.Domain_Mask = 0)) AND D.Has_Occurrences = 1) OR D.Domain_Key IS NULL)

	ORDER BY DisplayTerm

    SET ROWCOUNT 0
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_DeterminationsLifeSciences_Select_ForSearch') AND SysStat & 0xf = 4)
BEGIN
   	PRINT 'Setting up security on procedure usp_DeterminationsLifeSciences_Select_ForSearch'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
       	GRANT EXECUTE ON dbo.usp_DeterminationsLifeSciences_Select_ForSearch TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_DeterminationsLifeSciences_Select_ForSearch TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_DeterminationsLifeSciences_Select_ForSearch TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_DeterminationsLifeSciences_Select_ForSearch TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_DeterminationsLifeSciences_Select_ForSearch TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
       	GRANT EXECUTE ON dbo.usp_DeterminationsLifeSciences_Select_ForSearch TO [Dev - JNCC SQL]
END
GO

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
	$Revision: 4 $
	$Date: 29/01/09 16:54 $
	$Author: Simonwood $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_TaxonList_ImportConceptGroup]
	@job_id				INT,
	@Taxon_List_Key		CHAR(16),
	@concept_group_Key	CHAR(16),
	@SessionID	CHAR(16)
AS

SET NOCOUNT ON

	DECLARE		@existing_list_key					CHAR(16),
				@taxon_version_Wellformed			Char(1),
				@taxon_version_Incorrectformed		Char(1),
				@taxon_version_Unverified			Char(1),
				@taxon_version_status_recommended	Char(1),
				@taxon_version_status_synonym		Char(1),
				@taxon_type_scientific 				Char(1),
				@taxon_type_vernacular  			Char(1),
				@taxon_name_type_key_formal			CHAR(16),
				@record_count						INT
		
	/* determine default parameters */
	SET			@taxon_version_wellformed			=	'W'	
	SET			@taxon_version_incorrectformed		=	'I'
	SET			@taxon_version_unverified			=	'U'			
    SET			@taxon_version_status_recommended	=	'R'		
	SET			@taxon_version_status_synonym		=	'S'
	SET			@taxon_type_scientific				=	'S'
	SET			@taxon_type_vernacular				=	'V'
	SET			@taxon_name_type_key_formal			=	'MNHSYS0000000001'
	SELECT		@existing_list_key		=	Taxon_List_Key
	FROM		Taxon_Dictionary_Concept_Group_Mapping
	WHERE		Concept_Group_Key		=	@concept_group_Key

	IF @@ROWCOUNT = 0
	BEGIN
		/* record mapping */
		INSERT		Taxon_Dictionary_Concept_Group_Mapping (
					Taxon_List_Key,
					Concept_Group_Key)
		VALUES		(@Taxon_List_Key,
					@concept_group_Key)

		IF @@ERROR <> 0 RETURN
	END
	ELSE IF @existing_List_Key <> @Taxon_List_Key
	BEGIN
		RAISERROR (
			'Concept group has previously been imported into a different Taxon List',
			16,
			1)
		RETURN
	END
	/* Calculate size of job */
	SELECT		@record_count			=	COUNT(*)
	FROM		Concept_Group_Version
	WHERE		Concept_Group_Key		=	@concept_group_Key

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
	LEFT JOIN	Source_Join								AS	j	ON	j.Record_Key		=	c.Term_Key
																AND	j.Table_Name		=	'Term'
	LEFT JOIN	Concept_Designation						AS	d   ON	d.Concept_Key		=	c.Concept_Key
	LEFT JOIN	Thesaurus_Fact							AS	f	ON	f.Meaning_Key		=	c.Meaning_Key
	LEFT JOIN	Taxon_Dictionary_Term_Version_Mapping	AS	vm	ON	vm.Term_Version_Key	=	c.Term_Version_Key
	WHERE		c.Concept_Group_Key						=	@concept_group_Key
	AND 		j.Source_Join_Key IS NOT NULL

	EXECUTE		usp_Import_Export_Job_Configure		@job_id,
													@concept_group_Key,
													@record_count
	IF @@ERROR <> 0 RETURN

	/* import Versions */
	EXECUTE		usp_TaxonListVersion_ImportConceptGroup		@job_id
	IF @@ERROR <> 0 RETURN

	/* import name types */
	EXECUTE		usp_TaxonNameType_ImportConceptGroup	@job_id
	IF @@ERROR <> 0 RETURN

	/* import taxa */
	EXECUTE		usp_Taxon_ImportConceptGroup	@job_id
	IF @@ERROR <> 0 RETURN

	/* import Taxon Versions */
	EXECUTE		usp_TaxonVersion_ImportConceptGroup		@job_id, @SessionID
	IF @@ERROR <> 0 RETURN

	/* import Taxon/source relationships */
	EXECUTE		usp_TaxonSources_ImportConceptGroup		@job_id
	IF @@ERROR <> 0 RETURN

	/* import Taxon ranks */
	EXECUTE		usp_TaxonRank_ImportConceptGroup	@job_id
	IF @@ERROR <> 0 RETURN

	/* import Taxon List Items */
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
			AND C1.Concept_Group_Key=@concept_group_Key

	/* Discard NameServer records for the concept group */
	DELETE NS
	FROM NameServer NS
	INNER JOIN Taxon_Dictionary_Term_Version_Mapping TDM ON TDM.Taxon_Version_Key=NS.INPUT_Taxon_Version_Key
	INNER JOIN Term_Version	TV	ON TV.Term_Version_Key	 = TDM.Term_Version_Key
	INNER JOIN Concept		C1	ON C1.Term_Key			 = TV.Term_Key
								AND C1.Concept_Group_Key = @concept_group_Key

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
			AND C1.Concept_Group_Key=@concept_group_Key
	LEFT JOIN Taxon_Version AS TV ON TV.Taxon_Version_Key = TLI.Taxon_Version_Key) 
	LEFT JOIN Taxon AS T ON T.Taxon_Key = TV.Taxon_Key) 
	LEFT JOIN Taxon_Common_Name AS TCN ON TCN.Taxon_List_Item_Key = TLI.Taxon_List_Item_Key) 
	LEFT JOIN Taxon_Version AS TV2 ON TV2.Taxon_Version_Key = TCN.Taxon_Version_Key) 
	LEFT JOIN Taxon AS T2 ON T2.Taxon_Key = TV2.Taxon_Key) 
	LEFT JOIN Taxon_List_Item AS TLI3 ON TLI3.Taxon_List_Item_Key = TLI.Preferred_Name) 
	LEFT JOIN Taxon_Rank AS TR3 ON TR3.Taxon_Rank_Key = TLI3.Taxon_Rank_Key) 
	LEFT JOIN Taxon_Version AS TV3 ON TV3.Taxon_Version_Key = TLI3.Taxon_Version_Key) 
	LEFT JOIN Taxon AS T3 ON T3.Taxon_Key = TV3.Taxon_Key 
	WHERE TLI.Taxon_List_Version_To IS NULL

	/*Rebuild NameServer table */
	INSERT  INTO NameServer (	
			Input_Taxon_Version_Key,
			Taxon_Version_FORM,
			Taxon_Version_STATUS,
			Taxon_Type,
			Recommended_Taxon_Version_Key,
			Recommended_Taxon_List_Item_Key,
			Entered_By,
			Entry_Date,
			Changed_By,
			Changed_Date )
	SELECT  TLI.Taxon_Version_Key,
			CASE WHEN TV1.Validation_Level = 0 THEN @taxon_version_wellformed
				 WHEN TV1.Validation_Level = 3 THEN @taxon_version_incorrectformed 
				 ELSE @taxon_version_unverified 
			END,
			CASE WHEN TV1.Taxon_Version_Key is NULL THEN @taxon_version_unverified 
				 WHEN TV1.Taxon_Version_Key  = TLI.Taxon_Version_Key THEN @taxon_version_status_recommended 
				 ELSE @taxon_version_status_synonym  
			END,
			CASE WHEN TNT.TAXON_NAME_TYPE_KEY = @taxon_name_type_key_formal THEN @taxon_type_scientific 
				 ELSE @taxon_type_vernacular 
			END,  
			TV1.Taxon_Version_Key,
			TLI.PREFERRED_NAME,
			TLI.ENTERED_BY,
			TLI.ENTRY_DATE,
			TLI.CHANGED_BY,
			TLI.CHANGED_DATE		
	FROM	Taxon_List_Item TLI 
	INNER JOIN	Taxon_Dictionary_Term_Version_Mapping TDM ON TDM.Taxon_Version_Key	  =	TLI.Taxon_Version_Key
	INNER JOIN	Term_Version						  TV  ON TV.Term_Version_Key	  =	TDM.Term_Version_Key
	INNER JOIN	Concept								  C1  ON C1.Term_Key			  =	TV.Term_Key
														 AND C1.Concept_Group_Key	  =	@concept_group_Key
	LEFT JOIN	Taxon_List_Item						 TLI1 ON TLI1.Taxon_List_Item_Key = TLI.Preferred_Name
	LEFT JOIN	Taxon_Version					     TV1  ON TV1.Taxon_Version_Key	  = TLI1.Taxon_Version_Key 
	INNER JOIN	TAXON								  TX  ON TX.TAXON_KEY			  =	TV1.TAXON_KEY
	INNER JOIN	TAXON_NAME_TYPE						  TNT ON TNT.TAXON_NAME_TYPE_KEY  =	TX.TAXON_NAME_TYPE_KEY
	WHERE		TLI.Taxon_List_Version_To IS NULL


	/* Update to include the Has_Children field */
	UPDATE ITN
	SET Has_Children=1
	FROM Index_Taxon_Name ITN
	INNER JOIN Taxon_List_Item TLIChild ON TLIChild.Parent=ITN.Taxon_List_Item_Key
	INNER JOIN Taxon_List_Version TLV ON TLV.Taxon_List_Version_Key=ITN.Taxon_List_Version_Key
		AND TLV.Taxon_List_Key=@Taxon_List_Key

	EXEC usp_IndexTaxonName_ApplyNameServer_SingleList @Taxon_List_Key

	UPDATE Import_Export_Job
	SET Records_Processed = Records_Processed + @@ROWCOUNT
	WHERE Import_Export_Job_ID = @job_id

	EXECUTE		usp_Import_Export_Job_UpdateStatus	@job_id,
													'Exporting Taxon Common Names...'	

	/* Create a local table containing the Taxon common name data */
	DECLARE @TaxonCommonName TABLE (
		Taxon_List_Item_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS PRIMARY Key,
		Taxon_Version_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS
	)

	/* Get the Taxon_List_Item_Keys first, as there may be several Taxon_Version_Keys for some, 
	  and that would break the primary Key constraint. */
	INSERT INTO @TaxonCommonName
	SELECT DISTINCT TDM1.Taxon_List_Item_Key, NULL
	FROM 	Taxon_Dictionary_Concept_Mapping TDM1 
	JOIN 	Concept C1 
			ON C1.Concept_Key = TDM1.Concept_Key
			AND C1.Concept_Group_Key = @concept_group_Key

	/* Now get a Taxon_Version_Key for each Taxon_List_Item_Key found, it'll use just one, thus 
	  being ok with the primary Key constraint.  */
	UPDATE 	TCNTemp
	SET 	Taxon_Version_Key = TLI.Taxon_Version_Key
	FROM 	@TaxonCommonName TCNTemp
	INNER JOIN Taxon_Dictionary_Concept_Mapping TDM1 ON TDM1.Taxon_List_Item_Key = TCNTemp.Taxon_List_Item_Key
	INNER JOIN Concept C1 ON C1.Concept_Key=TDM1.Concept_Key
			AND C1.Concept_Group_Key=@concept_group_Key
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

	/* Update existing Taxon common name records that are out of date */
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
		
	/* Insert any new required Taxon common name records */
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