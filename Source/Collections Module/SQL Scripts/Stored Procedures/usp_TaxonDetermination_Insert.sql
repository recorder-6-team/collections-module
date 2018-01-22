/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TaxonDetermination_Insert') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_TaxonDetermination_Insert]
GO

/*===========================================================================*\
  Description:	Inserts a record in the Taxon Determination table.
		Ensures the Domain mask of the specimen is also updated.

  Parameters:	@Key 
		@DeterminedItemKey
		@OccurrenceKey  
		@SpecimenCollectionUnitKey  
		@DeterminationTypeKey  
		@NomenclaturalStatusConceptKey 
		@Confidence
		@DeterminerNameKey 
		@InferredDeterminer 
		@DeterminerRoleKey 
		@VagueDateStart
		@VagueDateEnd 
		@VagueDateType 
		@UsedSpecimen 
		@Preferred
		@Method
		@Notes 
		@SessionID
		@IncludeInLabel
		@IsForSpecimen		Indicates whether to update preferred 
					flag in Specimen_Unit or Determination.

  Created:	July 2003

  Last revision information:
    $Revision: 11 $
    $Date: 10/09/10 10:09 $
    $Author: Robertjohnson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_TaxonDetermination_Insert]
	@Key char(16) output, 
	@DeterminedItemKey char(16), 
	@OccurrenceKey char(16), 
	@SpecimenCollectionUnitKey char(16), 
	@DeterminationTypeKey char(16), 
	@NomenclaturalStatusConceptKey char(16),
	@Confidence tinyint,
	@DeterminerNameKey char(16), 
	@InferredDeterminer tinyint,
	@DeterminerRoleKey char(16), 
	@VagueDateStart int, 
	@VagueDateEnd int, 
	@VagueDateType varchar(2),
	@UsedSpecimen bit,
	@Preferred bit,
	@Method text,
	@Notes text,
	@SessionID char(16),
	@IncludeInLabel bit,
	@IsForSpecimen bit
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF
	SET XACT_ABORT ON
	SET ANSI_NULLS ON

	--Wrap everything in a transaction
	BEGIN TRANSACTION

		-- Get the user name key, as Taxon_Determination table doesn't have SessionID fields.
		DECLARE	@EnteredBy char(16),
			@PreferredForSpecimen bit

		SELECT	@EnteredBy = User_Name_Key FROM Session WHERE Session_ID = @SessionID
		SET	@PreferredForSpecimen = 0

		-- Get a new key first.
		EXECUTE spNextKey 'Taxon_Determination', @Key OUTPUT

		/*---------------------------------------------------------------------------------*\
		  Ensure only one preferred determination per occurrence.
		\*---------------------------------------------------------------------------------*/
		IF @IsForSpecimen = 1
		BEGIN
			IF @Preferred = 1 SET @PreferredForSpecimen = 1

			-- If there isn't a determination already, ensure the 'preferred' and
			-- 'include in label' flags are true.
			IF EXISTS ( SELECT * FROM Specimen_Unit 
							WHERE Collection_Unit_Key = @SpecimenCollectionUnitKey 
							AND Preferred_Taxon_Determination_Key IS NULL)
			BEGIN
				SET	@Preferred = 1
				SET @PreferredForSpecimen = 1
				SET	@IncludeInLabel = 1
			END

			-- Not used for Taxon_Occurrence, unless not already one present
			IF EXISTS(SELECT 1 FROM Taxon_Determination WHERE Taxon_Occurrence_Key=@OccurrenceKey AND Preferred=1)
				SET @Preferred = 0
		END ELSE BEGIN
			-- If there isn't a determination already, ensure the 'preferred' and
			-- 'include in label' flags are true.
			IF NOT EXISTS(SELECT * FROM Taxon_Determination WHERE Taxon_Occurrence_Key = @OccurrenceKey AND Preferred = 1)
			BEGIN
				SET @Preferred = 1
				SET	@IncludeInLabel = 1
			END
			ELSE
			-- If new determination is preferred, make sure previous preferred is turned off.
			IF @Preferred = 1
			BEGIN
				UPDATE	Taxon_Determination
				SET	Preferred = 0
				WHERE	Taxon_Occurrence_Key = @OccurrenceKey
				AND	Preferred = 1

				IF @@Error <> 0 GOTO RollbackAndExit
			END
		END

		/*---------------------------------------------------------------------------------*\
		  Do table insert.
		\*---------------------------------------------------------------------------------*/
		INSERT INTO Taxon_Determination (
			Taxon_Determination_Key, Taxon_List_Item_Key, Taxon_Occurrence_Key, 
			Vague_Date_Start, Vague_Date_End, Vague_Date_Type, Comment, Preferred, Determiner, 
			Determination_Type_Key, Determiner_Role_Key, Entered_By, Entry_Date, 
			Specimen_Collection_Unit_Key, Nomenclatural_Status_Concept_Key,
			Confidence, Used_Specimen, Method, Inferred_Determiner, Include_In_Label
		) VALUES (
			@Key, @DeterminedItemKey, @OccurrenceKey, 
			@VagueDateStart, @VagueDateEnd, @VagueDateType, @Notes, @Preferred, @DeterminerNameKey,
			@DeterminationTypeKey, @DeterminerRoleKey, @EnteredBy, GetDate(), 
			@SpecimenCollectionUnitKey, @NomenclaturalStatusConceptKey, 
			@Confidence, @UsedSpecimen, @Method, @InferredDeterminer, @IncludeInLabel
		)

		IF @@Error <> 0 GOTO RollbackAndExit

		IF @IsForSpecimen = 0 AND @Preferred = 1
		BEGIN
			-- Update validation flag of Taxon_Occurrence
			DECLARE	@ValidationLevel int
			SELECT	@ValidationLevel = Validation_Level
			FROM	Taxon_List_Item TLI 
			JOIN	Taxon_Version TV ON TV.Taxon_Version_Key = TLI.Taxon_Version_Key
			WHERE	TLI.Taxon_List_Item_Key = @DeterminedItemKey

			DECLARE	@CompetencyLevel int
			SELECT	@CompetencyLevel = DR.Validation_Competency
			FROM	Taxon_Determination TD 
			JOIN	Determiner_Role DR ON DR.Determiner_Role_Key = TD.Determiner_Role_Key
			WHERE	TD.Taxon_Determination_Key = @Key

			UPDATE	Taxon_Occurrence
			SET	Verified = 
					CASE 
						WHEN @ValidationLevel IS NULL THEN 0
						WHEN @ValidationLevel <= @CompetencyLevel THEN 2
						ELSE 1
					END
			WHERE	Taxon_Occurrence_Key = @OccurrenceKey
		END ELSE
		IF @PreferredForSpecimen = 1
		BEGIN
			UPDATE	Specimen_Unit
			SET	Preferred_Taxon_Determination_Key = @Key
			WHERE	Collection_Unit_Key = @SpecimenCollectionUnitKey

			IF @@Error <> 0 GOTO RollbackAndExit
		END

		/*-------------------------------------------------------------*\
		  Switch bit of new mask ON in Collection_Unit.
		\*-------------------------------------------------------------*/
		IF @IsForSpecimen = 1 AND @PreferredForSpecimen = 1
		BEGIN
			DECLARE @ConceptKey char(16),
				@ConceptMask int

			-- Get the right concept before getting the mask
			SELECT	@ConceptKey = Concept_Key
			FROM	Taxon_Dictionary_Concept_Mapping
			WHERE	Taxon_List_Item_Key = @DeterminedItemKey

			-- Retrieve the mask of the new concept.
			EXECUTE	usp_Get_Concept_Domain_Mask @ConceptKey, @ConceptMask OUTPUT
			-- And switch appropriate bit ON in Collection_Unit
			EXECUTE	usp_CollectionUnit_Update_DomainMask @SpecimenCollectionUnitKey, @ConceptMask, 1

			IF @@Error <> 0 GOTO RollbackAndExit
		END

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TaxonDetermination_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_TaxonDetermination_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_TaxonDetermination_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_TaxonDetermination_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_TaxonDetermination_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_TaxonDetermination_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_TaxonDetermination_Insert TO [Dev - JNCC SQL]
END
GO