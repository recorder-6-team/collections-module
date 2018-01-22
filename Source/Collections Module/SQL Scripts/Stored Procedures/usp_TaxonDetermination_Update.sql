/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_TaxonDetermination_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_TaxonDetermination_Update]
GO

/*===========================================================================*\
  Description:	Updates a record in the Determination table.
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
		@Timestamp
		@IncludeInLabel
		@IsForSpecimen		Indicates whether to update preferred 
					flag in Specimen_Unit or Determination.

  Created:	July 2003

  Last revision information:
    $Revision: 10 $
    $Date: 6/09/10 17:17 $
    $Author: Robertjohnson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_TaxonDetermination_Update]
	@Key char(16),
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
	@Timestamp timestamp,
	@IncludeInLabel bit,
	@IsForSpecimen bit,
	@RecordsAffected int OUTPUT
AS
	DECLARE @SpecimenKey char(16),
		@CurrentConceptKey char(16),
		@CurrentConceptMask bigint,
		@CurrentPreferred bit,
		@ConceptKey char(16),
		@ConceptMask bigint,
		@Error int
SET NOCOUNT OFF

	-- Get correct concept for given @TaxonListItemKey
	SELECT	@ConceptKey = Concept_Key
	FROM	Taxon_Dictionary_Concept_Mapping
	WHERE	Taxon_List_Item_Key = @DeterminedItemKey

	-- Retrieve the mask of the new concept.
	EXECUTE	usp_Get_Concept_Domain_Mask @ConceptKey, @ConceptMask OUTPUT

	BEGIN TRANSACTION
		/*-------------------------------------------------------------*\
		  Determine if and where to update the preferred states.
		  And also if specimen mask needs to be update.

		  If @Preferred is 0, then we are updating non-preferred
		  determination and therefore, no need to reset the one that is
		  still preferred. This only ensures there is only 1 preferred
		  at any time for the specimen.

		  We only do this if the preferred determination has actually
		  changed determination. This is necessary to avoid a timestamp
		  error. For example - if @Preferred were 1, and there was no
		  checking to see if the preferred determination had changed,
		  the record's Preferred field and the record's Timestamp would
		  be altered. The main update would then fail because the
		  timestamp is different to the one passed into the procedure.
		\*-------------------------------------------------------------*/
		IF @IsForSpecimen = 1 AND @Preferred = 1 BEGIN
			DECLARE	@CurrentPrefDetKey char(16),
				@CurrentPrefTaxonDetKey char(16)

			-- Get existing preferred keys from Specimen_Unit table
			SELECT	@CurrentPrefDetKey = Preferred_Determination_Key,
				@CurrentPrefTaxonDetKey = Preferred_Taxon_Determination_Key
			FROM	Specimen_Unit
			WHERE	Collection_Unit_Key = @SpecimenCollectionUnitKey

			-- Changing to another preferred Taxon Determination
			IF @CurrentPrefTaxonDetKey <> @Key BEGIN
				-- Get existing concept's key
				SELECT		@CurrentConceptKey = TDM.Concept_Key
				FROM		Taxon_Determination TD
				INNER JOIN	Taxon_Dictionary_Concept_Mapping TDM ON TDM.Taxon_List_Item_Key = TD.Taxon_List_Item_Key
				WHERE		TD.Taxon_Determination_Key = @Key

				-- We're having a new preferred, so replace the old one.
				UPDATE	Specimen_Unit
				SET	Preferred_Taxon_Determination_Key = @Key
				WHERE	Collection_Unit_Key = @SpecimenCollectionUnitKey

				IF @@Error <> 0 GOTO RollbackAndExit

				-- Preferred state should NOT change in Taxon_Determination, so get existing value to
				-- override parameter value.
				SELECT	@Preferred = Preferred
				FROM	Taxon_Determination
				WHERE	Taxon_Determination_Key = @Key
			END

			-- Get existing concept's mask
			EXECUTE	usp_Get_Concept_Domain_Mask @CurrentConceptKey, @CurrentConceptMask OUTPUT

			-- Different mask, so switch current one OFF in Collection_Unit
			IF @CurrentConceptMask <> @ConceptMask BEGIN
				EXECUTE	usp_CollectionUnit_Update_DomainMask @SpecimenCollectionUnitKey, @CurrentConceptMask, 0
				IF @@Error <> 0 GOTO RollbackAndExit
			END
		END ELSE 
		-- Dealing with Taxon_Determination table only, for occurrences.
		-- Also means that Specimen_Collection_Unit_Key can be NULL, but that doesn't mean
		-- the value passed in is NULL though.
		IF @IsForSpecimen = 0 AND @Preferred = 1 BEGIN
			-- Not guaranteed there is an associated specimen key.
			SELECT		@CurrentConceptKey = TDM.Concept_Key,
					@SpecimenKey = TD.Specimen_Collection_Unit_Key
			FROM		Taxon_Determination TD
			INNER JOIN	Taxon_Dictionary_Concept_Mapping TDM ON TDM.Taxon_List_Item_Key = TD.Taxon_List_Item_Key
			WHERE		TD.Taxon_Occurrence_Key = @OccurrenceKey
			AND		TD.Preferred = 1

			-- We're having a new preferred, so switch the old one OFF
			UPDATE	Taxon_Determination
			SET	Preferred = 0
			WHERE	Taxon_Occurrence_Key = @OccurrenceKey
			AND	Preferred = 1

			IF @@Error <> 0 GOTO RollbackAndExit

			-- Get existing concept's mask.
			EXECUTE	usp_Get_Concept_Domain_Mask @CurrentConceptKey, @CurrentConceptMask OUTPUT

			-- New concept's mask different from current one. Refresh specimen mask is there is one.
			IF @SpecimenKey IS NOT NULL AND @CurrentConceptMask <> @ConceptMask BEGIN
				EXECUTE	usp_CollectionUnit_Update_DomainMask @SpecimenKey, @CurrentConceptMask, 0
				IF @@Error <> 0 GOTO RollbackAndExit
			END
		END

		/*-------------------------------------------------------------*\
		  Do the table update.
		\*-------------------------------------------------------------*/
		-- Get the user name key, as Taxon_Determination table doesn't have SessionID fields.
		DECLARE	@ChangedBy char(16)
		SELECT	@ChangedBy = User_Name_Key FROM Session WHERE Session_ID = @SessionID

		/*-------------------------------------------------------------------------------------*\
		  The main update statement has been split into two. The reason for this is because of 
		  incident 6174 which causes the following error: 
			"The query processor could not produce a query plan from the optimizer because 
			a query cannot update a text, ntext, or image column and a clustering key at 
			the same time."
		  Because we are doing two updates, use the Timestamp for the first update. If that one
		  fails, the next one won't run, and all is well.
		\*------------------------------------------------------------------------------------*/
		-- Update the text fields.
		UPDATE	Taxon_Determination
		SET	Comment = @Notes,
			Method = @Method
		WHERE	Taxon_Determination_Key = @Key
		AND		[Timestamp] = @Timestamp
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Taxon_Determination WHERE Taxon_Determination_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

		-- Do the main update
		UPDATE	Taxon_Determination
		SET	Taxon_List_Item_Key = @DeterminedItemKey,
			Taxon_Occurrence_Key = @OccurrenceKey,
			Vague_Date_Start = @VagueDateStart,
			Vague_Date_End = @VagueDateEnd,
			Vague_Date_Type = @VagueDateType,
			Preferred = @Preferred,
			Determiner = @DeterminerNameKey,
			Inferred_Determiner = @InferredDeterminer,	
			Determination_Type_Key = @DeterminationTypeKey,
			Determiner_Role_Key = @DeterminerRoleKey,
			Changed_By = @ChangedBy,
			Changed_Date = GetDate(),
			Specimen_Collection_Unit_Key = @SpecimenCollectionUnitKey,
			Nomenclatural_Status_Concept_Key = @NomenclaturalStatusConceptKey,
			Confidence = @Confidence,
			Used_Specimen = @UsedSpecimen,
			Include_In_Label = @IncludeInLabel
		WHERE	Taxon_Determination_Key = @Key

		SELECT	@RecordsAffected = @@ROWCOUNT,
			@Error = @@ERROR

		IF @Error <> 0 GOTO RollbackAndExit

		/*-------------------------------------------------------------*\
		  Switch bit of new mask ON in Collection_Unit.
		\*-------------------------------------------------------------*/
		IF @SpecimenCollectionUnitKey IS NOT NULL BEGIN
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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TaxonDetermination_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_TaxonDetermination_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_TaxonDetermination_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_TaxonDetermination_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_TaxonDetermination_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_TaxonDetermination_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_TaxonDetermination_Update TO [Dev - JNCC SQL]
END
GO