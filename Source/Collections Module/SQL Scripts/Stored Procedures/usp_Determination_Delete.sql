/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Determination_Delete]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Determination_Delete]
GO

/*===========================================================================*\
  Description:	Delete a record in the Determination table.
		Ensures the Domain mask of the specimen is also updated.

  Parameters:	@DeterminationKey
		@IsForSpecimen
		@Timestamp timestamp

  Created:	July 2003

  Last revision information:
    $Revision: 11 $
    $Date: 3/02/09 9:19 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Determination_Delete]
	@DeterminationKey char(16),
	@IsForSpecimen bit,
	@Timestamp timestamp
AS
	DECLARE @SpecimenKey char(16),
		@ConceptKey char(16),
		@ConceptMask bigint,
		@OccurrenceKey char(16),
		@WasPreferredForOccurrence bit,
		@WasPreferredForSpecimen bit

	SET @WasPreferredForSpecimen = 0
	SET @WasPreferredForOccurrence = 0

	BEGIN TRANSACTION
		/*-------------------------------------------------------------*\
		  Set some values to search for new preferred, if deleting 
		  current preferred.
		\*-------------------------------------------------------------*/
		SELECT	@ConceptKey = Concept_Key,
			@OccurrenceKey = Occurrence_Key,
			@WasPreferredForOccurrence = Preferred,
			@SpecimenKey = Specimen_Collection_Unit_Key
		FROM	Determination
		WHERE	Determination_Key = @DeterminationKey

		-- Check for Specimen.
		IF EXISTS(SELECT * FROM Specimen_Unit WHERE Preferred_Determination_Key = @DeterminationKey) BEGIN
			SELECT	@WasPreferredForSpecimen = 1,
				@SpecimenKey = Collection_Unit_Key
			FROM	Specimen_Unit
			WHERE	Preferred_Determination_Key = @DeterminationKey

			-- Need to clear the field because of referential integrity.
			UPDATE 	Specimen_Unit
			SET	Preferred_Determination_Key = NULL
			WHERE	Collection_Unit_Key = @SpecimenKey
			IF @@Error <> 0 GOTO RollbackAndExit
		END

		/*-------------------------------------------------------------*\
		  Delete either from Determination table, or just the link.
		\*-------------------------------------------------------------*/
		IF @OccurrenceKey IS NOT NULL AND @SpecimenKey IS NOT NULL 
			-- Determination linked to both a specimen AND an occurrence
			IF @IsForSpecimen = 1
				-- Deleting for specimen, clear the link and keep Occurrence key
				UPDATE	Determination
				SET	Specimen_Collection_Unit_Key = NULL
				WHERE	Determination_Key = @DeterminationKey
				AND		[Timestamp] = @Timestamp
			ELSE
				-- Deleting for occurrence, clear the link and keep Spceimen key
				UPDATE	Determination
				SET	Occurrence_Key = NULL
				WHERE	Determination_Key = @DeterminationKey
				AND		[Timestamp] = @Timestamp
		ELSE
			-- Only either specimen or occurrence key, safe to delete.
			DELETE	Determination
			WHERE	Determination_Key = @DeterminationKey
			AND		[Timestamp] = @Timestamp

		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Determination WHERE Determination_Key = @DeterminationKey)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

		/*-------------------------------------------------------------*\
		  See if preferred flags need to be updated, and where.
		\*-------------------------------------------------------------*/
		-- Retrieve the mask of the concept from deleted determination.
		EXECUTE	usp_Get_Concept_Domain_Mask @ConceptKey, @ConceptMask OUTPUT

		-- Need to look for a new preferred determination. Default to first one found.
		IF @WasPreferredForOccurrence = 1 OR @WasPreferredForSpecimen = 1 BEGIN
			DECLARE	@NewConceptKey char(16),
				@NewConceptMask bigint,
				@NewPreferredDeterminationKey char(16)

			-- Get new preferred first, if there are determinations left to choose from.
			IF EXISTS(SELECT * FROM Determination WHERE Occurrence_Key = @OccurrenceKey)
			BEGIN
				-- For occurrences, it all happens in Determination table.
				IF @WasPreferredForOccurrence = 1 BEGIN
					SELECT	TOP 1 @NewPreferredDeterminationKey = Determination_Key
					FROM	Determination
					WHERE	Occurrence_Key = @OccurrenceKey

					UPDATE 	Determination
					SET	Preferred = 1
					WHERE	Determination_Key = @NewPreferredDeterminationKey
				END

				-- For specimen, it happens in Specimen_Unit table.
				IF @WasPreferredForSpecimen = 1 BEGIN
					SELECT	TOP 1 @NewPreferredDeterminationKey = Determination_Key
					FROM	Determination
					WHERE	Specimen_Collection_Unit_Key = @SpecimenKey

					UPDATE 	Specimen_Unit
					SET	Preferred_Determination_Key = @NewPreferredDeterminationKey
					WHERE	Collection_Unit_Key = @SpecimenKey
				END
				IF @@Error <> 0 GOTO RollbackAndExit

				-- Get concept key of new preferred determination.
				SELECT	@NewConceptKey = Concept_Key
				FROM	Determination
				WHERE	Determination_Key = @NewPreferredDeterminationKey
			END

			-- Retrieve the mask of the concept from deleted determination.
			EXECUTE	usp_Get_Concept_Domain_Mask @NewConceptKey, @NewConceptMask OUTPUT

			-- Different mask, so switch old one OFF and new one ON.
			IF @SpecimenKey IS NOT NULL AND @ConceptMask <> @NewConceptMask 
			BEGIN
				EXECUTE	usp_CollectionUnit_Update_DomainMask @SpecimenKey, @ConceptMask, 0
				IF @@Error <> 0 GOTO RollbackAndExit

				EXECUTE	usp_CollectionUnit_Update_DomainMask @SpecimenKey, @NewConceptMask, 1
				IF @@Error <> 0 GOTO RollbackAndExit
			END
		END

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Determination_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Determination_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Determination_Delete TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Determination_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Determination_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Determination_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Determination_Delete TO [Dev - JNCC SQL]
END
GO