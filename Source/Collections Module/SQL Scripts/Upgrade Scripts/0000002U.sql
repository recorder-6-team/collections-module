/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_TaxonOccurrence_TaxonDetermination_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_TaxonOccurrence_TaxonDetermination_Update]
GO

/*===========================================================================*\
  Description:	

  Parameters:	@TaxonOccurrenceKey	-Taxon Occurrence Key
		@SpecimenUnitKey	-Specimen Unit Key

  Created:	Feb 2004

  Last revision information:
    $Revision: 1 $
    $Date: 21/12/07 9:57 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_TaxonOccurrence_TaxonDetermination_Update]
	@TaxonOccurrenceKey char(16),
	@SpecimenUnitKey char(16)
AS

	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF
	SET ANSI_NULLS ON

	BEGIN TRANSACTION	
		--Update existing Specimen Determinations to link to the new Occurrence
		UPDATE Taxon_Determination
			SET Taxon_Occurrence_Key = @TaxonOccurrenceKey,
				Preferred = 1
		FROM		Taxon_Determination AS TD
		INNER JOIN 	Specimen_Unit AS SU ON SU.Preferred_Taxon_Determination_Key = TD.Taxon_Determination_Key
		WHERE Specimen_Collection_Unit_Key = @SpecimenUnitKey

		IF @@Error <> 0 GOTO RollBackAndExit

		-- Update Validation flag in Taxon Occurrence
		DECLARE	@ValidationLevel int
		SELECT	@ValidationLevel = Validation_Level
		FROM	Taxon_Determination TD 
		JOIN	Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key = TD.Taxon_List_Item_Key
		JOIN	Taxon_Version TV ON TV.Taxon_Version_Key = TLI.Taxon_Version_Key
		WHERE	TD.Specimen_Collection_Unit_Key = @SpecimenUnitKey AND TD.Preferred = 1

		DECLARE	@CompetencyLevel int
		SELECT	@CompetencyLevel = DR.Validation_Competency
		FROM	Taxon_Determination TD 
		JOIN	Determiner_Role DR ON DR.Determiner_Role_Key = TD.Determiner_Role_Key
		WHERE	TD.Specimen_Collection_Unit_Key = @SpecimenUnitKey AND TD.Preferred = 1

		UPDATE	Taxon_Occurrence
		SET	Verified = 
				CASE 
					WHEN @ValidationLevel IS NULL THEN 0
					WHEN @ValidationLevel <= @CompetencyLevel THEN 2
					ELSE 1
				END
		WHERE	Taxon_Occurrence_Key = @TaxonOccurrenceKey

		IF @@Error <> 0 GOTO RollBackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TaxonOccurrence_TaxonDetermination_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_TaxonOccurrence_TaxonDetermination_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_TaxonOccurrence_TaxonDetermination_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_TaxonOccurrence_TaxonDetermination_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_TaxonOccurrence_TaxonDetermination_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_TaxonOccurrence_TaxonDetermination_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_TaxonOccurrence_TaxonDetermination_Update TO [Dev - JNCC SQL]
END

GO



/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Specimen_Select') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_Specimen_Select]
GO

/*===========================================================================*\
  Description:	Returns data for the Specimen General frame.

  Parameters:	@Key		Specimen Collection Unit key
		@IsLifeScience	Whether we should be using the Taxon_Determination
				or Taxon tables.

  Created:	Setember 2003

  Last revision information:
    $Revision: 1 $
    $Date: 21/12/07 9:57 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Specimen_Select]
	@Key char(16),
	@IsLifeScience bit
AS

SET NOCOUNT ON

	IF @IsLifeScience = 1
	BEGIN
		SELECT		CU.Collection_Unit_Key,
				ITN.Taxon_List_Item_Key AS Term_Key,
				CU.Current_Location_Code,
				CU.Usual_Location_Code,
				CU.Domain_Mask,
				SU.Dangerous,
				SU.Confidential,
				SU.Parent_Collection_Collection_Unit_Key AS Parent_Unit_Key,
				Coll.Item_Name AS ParentCollectionCollectionUnitName,
				CU.Current_Container_Collection_Unit_Key,
				CU.Usual_Container_Collection_Unit_Key,
				SCU.Item_Name + ISNULL(' - ' + CSC.Current_Location_Code, ISNULL(' - ' + CSC.Usual_Location_Code, '')) AS Current_Location_Name,
				SUS.Item_Name + ISNULL(' - ' + CSU.Current_Location_Code, ISNULL(' - ' + CSU.Usual_Location_Code, '')) AS Usual_Location_Name,
				CASE 	WHEN ITN.Preferred_Name_Italic = 1 THEN '<i>' + ITN.Preferred_Name + '</i>'
					ELSE ITN.Preferred_Name 
				END AS Item_Name,
				Specimen_Type_Concept_Key,
				CTType.PlainText AS Type,
				SU.Timestamp AS SUTimeStamp,
				CU.Timestamp AS CUTimeStamp

		FROM		Collection_Unit AS CU
		INNER JOIN	Specimen_Unit AS SU ON SU.Collection_Unit_Key = CU.Collection_Unit_Key	
		INNER JOIN	Collection AS Coll On Coll.Collection_Unit_Key = SU.Parent_Collection_Collection_Unit_Key
		LEFT JOIN	Taxon_Determination AS TD ON TD.Taxon_Determination_Key = SU.Preferred_Taxon_Determination_Key
		LEFT JOIN	Index_Taxon_Name AS ITN ON ITN.Taxon_List_Item_Key = TD.Taxon_List_Item_Key
	
		INNER JOIN 	VW_ConceptTerm AS CTType ON Specimen_Type_Concept_Key = CTType.Concept_Key

		LEFT JOIN 	Store SCU ON SCU.Collection_Unit_Key = CU.Current_Container_Collection_Unit_Key
		LEFT JOIN	Collection_Unit CSC ON CSC.Collection_Unit_Key = SCU.Collection_Unit_Key 
		LEFT JOIN 	Store SUS ON SUS.Collection_Unit_Key = CU.Usual_Container_Collection_Unit_Key
		LEFT JOIN	Collection_Unit CSU ON CSU.Collection_Unit_Key = SUS.Collection_Unit_Key
	
		WHERE SU.Collection_Unit_Key = @Key
	END
	ELSE
	BEGIN
		SELECT		CU.Collection_Unit_Key,
				C.Term_Key,
				CU.Current_Location_Code,
				CU.Usual_Location_Code,
				CU.Domain_Mask,
				SU.Dangerous,
				SU.Confidential,
				SU.Parent_Collection_Collection_Unit_Key AS Parent_Unit_Key,
				Coll.Item_Name AS ParentCollectionCollectionUnitName,
				CU.Current_Container_Collection_Unit_Key,
				CU.Usual_Container_Collection_Unit_Key,
				SCU.Item_Name + ISNULL(' - ' + CSC.Current_Location_Code, ISNULL(' - ' + CSC.Usual_Location_Code, '')) AS Current_Location_Name,
				SUS.Item_Name + ISNULL(' - ' + CSU.Current_Location_Code, ISNULL(' - ' + CSU.Usual_Location_Code, '')) AS Usual_Location_Name,
				T.Item_Name,
				Specimen_Type_Concept_Key,
				CTType.PlainText AS Type,
				SU.Timestamp AS SUTimeStamp,
				CU.Timestamp AS CUTimeStamp
	
		FROM		Collection_Unit AS CU
		INNER JOIN	Specimen_Unit AS SU ON SU.Collection_Unit_Key = CU.Collection_Unit_Key
		INNER JOIN	Collection AS Coll On Coll.Collection_Unit_Key = SU.Parent_Collection_Collection_Unit_Key
		LEFT JOIN	Determination AS D ON D.Determination_Key = SU.Preferred_Determination_Key
		LEFT JOIN	Concept AS C ON C.Concept_Key = D.Concept_Key
		LEFT JOIN	Term AS T ON T.Term_Key = C.Term_Key
	
		INNER JOIN 	VW_ConceptTerm AS CTType ON Specimen_Type_Concept_Key = CTType.Concept_Key
		LEFT JOIN 	Store SCU ON SCU.Collection_Unit_Key = CU.Current_Container_Collection_Unit_Key
		LEFT JOIN	Collection_Unit CSC ON CSC.Collection_Unit_Key = SCU.Collection_Unit_Key 
		LEFT JOIN 	Store SUS ON SUS.Collection_Unit_Key = CU.Usual_Container_Collection_Unit_Key
		LEFT JOIN	Collection_Unit CSU ON CSU.Collection_Unit_Key = SUS.Collection_Unit_Key
	
		WHERE SU.Collection_Unit_Key = @Key
	END

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Specimen_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Specimen_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Specimen_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimen_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimen_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Specimen_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimen_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Specimen_Select TO [Dev - JNCC SQL]
END
GO
