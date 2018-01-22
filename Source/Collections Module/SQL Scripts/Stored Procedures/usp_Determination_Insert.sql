/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Determination_Insert') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_Determination_Insert]
GO

/*===========================================================================*\
  Description:	Inserts a record in the Determination table.
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
    $Revision: 12 $
    $Date: 10/09/10 10:09 $
    $Author: Robertjohnson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Determination_Insert]
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

	--Wrap everything in a transaction
	BEGIN TRANSACTION

		-- Get a new key.
		EXECUTE spNextKey 'Determination', @Key OUTPUT

		/*---------------------------------------------------------------------------------*\
		  Ensure only one preferred determination per occurrence.
		\*---------------------------------------------------------------------------------*/
		DECLARE	@PReferredForSpecimen bit
		SET	@PReferredForSpecimen = 0

		IF @IsForSpecimen = 1
		BEGIN
			IF @Preferred = 1
				SET @PreferredForSpecimen = 1

			-- Not used for Occurrence if for specimen, unless not already one present
			IF EXISTS(SELECT 1 FROM Determination WHERE Occurrence_Key = @OccurrenceKey AND Preferred = 1)
				SET @Preferred = 0
		END ELSE BEGIN
			-- If there isn't a determination already, ensure the 'preferred' and
			-- 'include in label' flags are true.
			IF NOT EXISTS(SELECT * FROM Determination WHERE Occurrence_Key = @OccurrenceKey AND Preferred = 1)
			BEGIN
				SET @Preferred = 1
				SET @IncludeInLabel = 1
			END
			ELSE
			-- If new determination is preferred, make sure previous preferred is turned off.
			IF @Preferred = 1
			BEGIN
				UPDATE	Determination
				SET	Preferred = 0
				WHERE	Occurrence_Key = @OccurrenceKey
				AND	Preferred = 1

				IF @@Error <> 0 GOTO RollbackAndExit
			END
		END

		-- Force determination to be specimen's preferred if its the only one
		IF EXISTS ( SELECT * FROM Specimen_Unit 
							WHERE Collection_Unit_Key = @SpecimenCollectionUnitKey 
							AND Preferred_Determination_Key IS NULL )
		BEGIN
			SET @PreferredForSpecimen = 1
			SET @Preferred = 1
			SET @IncludeInLabel = 1
		END
		/*-------------------------------------------------------------*\
		  Do the table insert.
		\*-------------------------------------------------------------*/
		INSERT INTO Determination (
			Determination_Key, Concept_Key, Occurrence_Key, Specimen_Collection_Unit_Key,
			Determination_Type_Key, Nomenclatural_Status_Concept_Key,
			Confidence, Determiner_Name_Key, Inferred_Determiner,
			Determiner_Role_Key, Vague_Date_Start, Vague_Date_End,
			Vague_Date_Type, Used_Specimen, Preferred, Method, Notes,
			Entered_Session_ID, Include_In_Label
		) VALUES (
			@Key, @DeterminedItemKey, @OccurrenceKey, @SpecimenCollectionUnitKey,
			@DeterminationTypeKey, @NomenclaturalStatusConceptKey, 
			@Confidence, @DeterminerNameKey, @InferredDeterminer,
			@DeterminerRoleKey, @VagueDateStart, @VagueDateEnd,
			@VagueDateType, @UsedSpecimen, @Preferred, @Method, @Notes, 
			@SessionID, @IncludeInLabel
		)

		IF @@Error <> 0 GOTO RollbackAndExit


		IF @PreferredForSpecimen = 1
		BEGIN
			UPDATE	Specimen_Unit
			SET	Preferred_Determination_Key = @Key
			WHERE	Collection_Unit_Key = @SpecimenCollectionUnitKey

			IF @@Error <> 0 GOTO RollbackAndExit
		END

		IF @IsForSpecimen = 1
		BEGIN
			DECLARE @ConceptMask int

			/*-------------------------------------------------------------*\
			  Switch bit of new mask ON in Collection_Unit.
			\*-------------------------------------------------------------*/
			-- Retrieve the mask of the new concept.
			EXECUTE	usp_Get_Concept_Domain_Mask @DeterminedItemKey, @ConceptMask OUTPUT
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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Determination_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Determination_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Determination_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Determination_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Determination_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Determination_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Determination_Insert TO [Dev - JNCC SQL]
END
GO