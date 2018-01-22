SET QUOTED_IDENTIFIER ON
SET ANSI_NULLS ON
GO

/*===========================================================================*\
  Description:	Adds a new column 'Include_In_Label' to Determination and
				Taxon_Determination, and updates the following stored procedures:
				
				usp_Determination_Select.sql
				usp_Determination_Update.sql
				usp_Determination_Insert.sql
				usp_TaxonDetermination_Insert.sql
				usp_TaxonDetermination_Update.sql

  Created:	Setember 2010

  Last revision information:
    $Revision: 4 $
    $Date: 17/09/10 10:07 $
    $Author: Robertjohnson $

\*===========================================================================*/

ALTER TABLE	Determination
ADD			Include_In_Label	BIT	NOT NULL
	CONSTRAINT	DF_Determination_Include_In_Label	DEFAULT 0

ALTER TABLE	Taxon_Determination
ADD			Include_In_Label	BIT	NOT NULL
	CONSTRAINT	DF_Taxon_Determination_Include_In_Label	DEFAULT 0
GO

UPDATE	Determination
SET		Include_In_Label = Preferred

UPDATE	Taxon_Determination
SET		Include_In_Label = Preferred
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Determination_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Determination_Select]
GO

/*===========================================================================*\
  Description:	Returns data for the Determination General frame.

  Parameters:	@Key		Determination key
		@IsLifeScience 	If we want Life Sciences information, then
				we need to go to the Taxon tables, and hence,
				@IsLifeScience will be set to 1. If we want Earth
				Sciences information, we go to the Determination
				table and @IsLifeScience will be set to 0.

  Created:	October 2003

  Last revision information:
    $Revision: 4 $
    $Date: 17/09/10 10:07 $
    $Author: Robertjohnson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Determination_Select]
	@Key char(16),
	@IsLifeScience bit,
	@IsSpecimenUnit bit = NULL
AS

SET NOCOUNT ON

	IF @IsLifeScience = 1 
		IF (@IsSpecimenUnit = NULL) OR (@IsSpecimenUnit = 0)	
			SELECT 		D.Taxon_List_Item_Key AS DetKey, 
					ITN.Preferred_Name AS Term,
					DM.Domain_Mask,
					D.Determination_Type_Key,
					D.Taxon_Occurrence_Key AS Occurrence_Key,
					D.Specimen_Collection_Unit_Key,
					DT.Short_Name AS Type,
					CTStat.Concept_Key AS Status_Concept_Key,
					CTStat.PlainText AS Status_Term,
					D.Confidence,
					D.Determiner AS Determiner_Name_Key,
					dbo.ufn_GetFormattedName(D.Determiner) AS Determiner,
					DR.Determiner_Role_Key,
					DR.Short_Name AS Determiner_Role,
					D.Vague_Date_Start,
					D.Vague_Date_End,
					D.Vague_Date_Type,
					D.Used_Specimen,
					D.Preferred,
					D.Method,
					D.Comment AS Notes,
					D.Inferred_Determiner,
					D.[Timestamp],
					D.Include_In_Label		
			FROM 		Taxon_Determination D
			INNER JOIN 	Index_Taxon_Name ITN ON ITN.Taxon_List_Item_Key = D.Taxon_List_Item_Key
			LEFT JOIN 	vw_ConceptTerm CTStat ON CTStat.Concept_Key = D.Nomenclatural_Status_Concept_Key
			INNER JOIN 	Determination_Type DT ON DT.Determination_Type_Key = D.Determination_Type_Key
			INNER JOIN 	Determiner_Role DR ON DR.Determiner_Role_Key = D.Determiner_Role_Key 
			LEFT JOIN 	Taxon_Dictionary_Concept_Mapping TDCM ON TDCM.Taxon_List_ITem_Key = D.Taxon_List_Item_Key
			LEFT JOIN 	Concept C ON C.Concept_Key = TDCM.Concept_Key
			LEFT JOIN 	Concept_Group CG ON CG.Concept_Group_Key = C.Concept_Group_Key
			LEFT JOIN 	Local_Domain LD ON LD.Local_Domain_Key = CG.Local_Domain_Key
			LEFT JOIN 	Domain DM ON DM.Domain_Key = LD.Domain_Key
			WHERE 		D.Taxon_Determination_Key = @Key
		ELSE
			SELECT 			
					SU.Collection_Unit_Key,		
					D.Taxon_List_Item_Key AS DetKey, 
					ITN.Preferred_Name AS Term,
					DM.Domain_Mask,
					D.Determination_Type_Key,
					D.Taxon_Occurrence_Key AS Occurrence_Key,
					D.Specimen_Collection_Unit_Key,
					DT.Short_Name AS Type,
					CTStat.Concept_Key AS Status_Concept_Key,
					CTStat.PlainText AS Status_Term,
					D.Confidence,
					D.Determiner AS Determiner_Name_Key,
					dbo.ufn_GetFormattedName(D.Determiner) AS Determiner,
					DR.Determiner_Role_Key,
					DR.Short_Name AS Determiner_Role,
					D.Vague_Date_Start,
					D.Vague_Date_End,
					D.Vague_Date_Type,
					D.Used_Specimen,
					CASE WHEN SU.Collection_Unit_Key IS NULL THEN 0
								ELSE 1
					END AS Preferred,
	
					D.Method,
					D.Comment AS Notes,
					D.Inferred_Determiner,
					D.[Timestamp],
					D.Include_In_Label			
			FROM 		Taxon_Determination D
			INNER JOIN 	Index_Taxon_Name ITN ON ITN.Taxon_List_Item_Key = D.Taxon_List_Item_Key
			LEFT JOIN 	vw_ConceptTerm CTStat ON CTStat.Concept_Key = D.Nomenclatural_Status_Concept_Key
			INNER JOIN 	Determination_Type DT ON DT.Determination_Type_Key = D.Determination_Type_Key
			INNER JOIN 	Determiner_Role DR ON DR.Determiner_Role_Key = D.Determiner_Role_Key 
			LEFT JOIN 	Taxon_Dictionary_Concept_Mapping TDCM ON TDCM.Taxon_List_ITem_Key = D.Taxon_List_Item_Key
			LEFT JOIN 	Concept C ON C.Concept_Key = TDCM.Concept_Key
			LEFT JOIN 	Concept_Group CG ON CG.Concept_Group_Key = C.Concept_Group_Key
			LEFT JOIN 	Local_Domain LD ON LD.Local_Domain_Key = CG.Local_Domain_Key
			LEFT JOIN 	Domain DM ON DM.Domain_Key = LD.Domain_Key
			LEFT JOIN	Specimen_Unit AS SU ON SU.Collection_Unit_Key = D.Specimen_Collection_Unit_Key
							AND SU.Preferred_Taxon_Determination_Key = @Key
			WHERE 		D.Taxon_Determination_Key = @Key
	ELSE
		IF (@IsSpecimenUnit = NULL) OR (@IsSpecimenUnit = 0)	
			SELECT 		CT.Concept_Key AS DetKey,
					CT.Plaintext AS Term,
					DM.Domain_Mask,
					D.Determination_Type_Key,
					D.Occurrence_Key,
					D.Specimen_Collection_Unit_Key,
					DT.Short_Name AS Type,
					CTStat.Concept_Key AS Status_Concept_Key,
					CTStat.PlainText AS Status_Term,
					D.Confidence,
					D.Determiner_Name_Key,
					dbo.ufn_GetFormattedName(D.Determiner_Name_Key ) AS Determiner,
					DR.Determiner_Role_Key,
					DR.Short_Name AS Determiner_Role,
					D.Vague_Date_Start,
					D.Vague_Date_End,
					D.Vague_Date_Type,
					D.Used_Specimen,
					D.Preferred,
					D.Method,
					D.Notes,
					D.Inferred_Determiner,
					D.[Timestamp],
					D.Include_In_Label
			FROM 		Determination D
			INNER JOIN 	vw_ConceptTerm CT ON CT.Concept_Key = D.Concept_Key
			LEFT JOIN 	vw_ConceptTerm CTStat ON CTStat.Concept_Key = D.Nomenclatural_Status_Concept_Key
			INNER JOIN 	Determination_Type DT ON DT.Determination_Type_Key = D.Determination_Type_Key
			INNER JOIN 	Determiner_Role DR ON DR.Determiner_Role_Key = D.Determiner_Role_Key 
			INNER JOIN 	Concept C ON C.Concept_Key = D.Concept_Key
			INNER JOIN 	Concept_Group CG ON CG.Concept_Group_Key = C.Concept_Group_Key
			INNER JOIN 	Local_Domain LD ON LD.Local_Domain_Key = CG.Local_Domain_Key
			INNER JOIN 	Domain DM ON DM.Domain_Key = LD.Domain_Key
			WHERE 		D.Determination_Key = @Key
		ELSE
			SELECT 		
					SU.Collection_Unit_Key,
					CT.Concept_Key AS DetKey,
					CT.Plaintext AS Term,
					DM.Domain_Mask,
					D.Determination_Type_Key,
					D.Occurrence_Key,
					D.Specimen_Collection_Unit_Key,
					DT.Short_Name AS Type,
					CTStat.Concept_Key AS Status_Concept_Key,
					CTStat.PlainText AS Status_Term,
					D.Confidence,
					D.Determiner_Name_Key,
					dbo.ufn_GetFormattedName(D.Determiner_Name_Key ) AS Determiner,
					DR.Determiner_Role_Key,
					DR.Short_Name AS Determiner_Role,
					D.Vague_Date_Start,
					D.Vague_Date_End,
					D.Vague_Date_Type,
					D.Used_Specimen,
					CASE WHEN SU.Collection_Unit_Key IS NULL THEN 0
									ELSE 1
					END AS Preferred,
					D.Method,
					D.Notes,
					D.Inferred_Determiner,
					D.[Timestamp],
					D.Include_In_Label			
			FROM 		Determination D
			INNER JOIN 	vw_ConceptTerm CT ON CT.Concept_Key = D.Concept_Key
			LEFT JOIN 	vw_ConceptTerm CTStat ON CTStat.Concept_Key = D.Nomenclatural_Status_Concept_Key
			INNER JOIN 	Determination_Type DT ON DT.Determination_Type_Key = D.Determination_Type_Key
			INNER JOIN 	Determiner_Role DR ON DR.Determiner_Role_Key = D.Determiner_Role_Key 
			INNER JOIN 	Concept C ON C.Concept_Key = D.Concept_Key
			INNER JOIN 	Concept_Group CG ON CG.Concept_Group_Key = C.Concept_Group_Key
			INNER JOIN 	Local_Domain LD ON LD.Local_Domain_Key = CG.Local_Domain_Key
			INNER JOIN 	Domain DM ON DM.Domain_Key = LD.Domain_Key
			LEFT JOIN	Specimen_Unit AS SU ON SU.Collection_Unit_Key = D.Specimen_Collection_Unit_Key
							AND SU.Preferred_Determination_Key = @Key
			WHERE 		D.Determination_Key = @Key
SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Determination_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Determination_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Determination_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Determination_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Determination_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Determination_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Determination_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Determination_Select TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Determination_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Determination_Update]
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
		@RecordsAffected	OUTPUT Can't rely on correct value to come out,
					so use a parameter instead.

  Created:	July 2003

  Last revision information:
    $Revision: 4 $
    $Date: 17/09/10 10:07 $
    $Author: Robertjohnson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Determination_Update]
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
		@CurrentConceptMask int,
		@CurrentPreferred bit,
		@ConceptMask int

	BEGIN TRANSACTION
		-- Retrieve the mask of the new concept.
		EXECUTE	usp_Get_Concept_Domain_Mask @DeterminedItemKey, @ConceptMask OUTPUT

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

			-- Changing to another preferred Determination
			IF (@CurrentPrefDetKey <> @Key) OR (@CurrentPrefDetKey IS NULL) BEGIN
				-- Get existing concept's key
				SELECT	@CurrentConceptKey = Concept_Key
				FROM	Determination
				WHERE	Determination_Key = @Key

				-- We're having a new preferred, so replace the old one.
				UPDATE	Specimen_Unit
				SET	Preferred_Determination_Key = @Key
				WHERE	Collection_Unit_Key = @SpecimenCollectionUnitKey

				IF @@Error <> 0 GOTO RollbackAndExit

				-- Preferred state should NOT change in Determination, so get existing value to
				-- override parameter value.
				SELECT	@Preferred = Preferred
				FROM	Determination
				WHERE	Determination_Key = @Key
			END

			-- Get existing concept's mask
			EXECUTE	usp_Get_Concept_Domain_Mask @CurrentConceptKey, @CurrentConceptMask OUTPUT

			-- Different mask, so switch current one OFF in Collection_Unit
			IF @CurrentConceptMask <> @ConceptMask BEGIN
				EXECUTE	usp_CollectionUnit_Update_DomainMask @SpecimenCollectionUnitKey, @CurrentConceptMask, 0
				IF @@Error <> 0 GOTO RollbackAndExit
			END
		END ELSE 
		-- Dealing with Determination/Taxon_Determination tables only, for occurrences.
		-- Also means that Specimen_Collection_Unit_Key can be NULL, but that doesn't mean
		-- the value passed in is NULL though.
		IF @IsForSpecimen = 0 AND @Preferred = 1 BEGIN
			-- Not guaranteed there is an associated specimen key for the preferred determination	.
			SELECT	@CurrentConceptKey = Concept_Key,
				@SpecimenKey = Specimen_Collection_Unit_Key
			FROM	Determination
			WHERE	Determination_Key IN (
					SELECT	D.Determination_Key
					FROM	Determination D 
					WHERE	D.Occurrence_Key = @OccurrenceKey
				UNION
					SELECT	D.Determination_Key
					FROM	Specimen_Field_Data SFD 
					JOIN	Specimen_Unit SU ON SU.Collection_Unit_Key = SFD.Collection_Unit_Key
					JOIN	Determination D ON D.Specimen_Collection_Unit_Key = SU.Collection_Unit_Key
					WHERE	SFD.Occurrence_Key = @OccurrenceKey
				)
			AND	Preferred = 1

			-- We're having a new preferred, so switch the old one OFF
			-- But only if not changing preferred, or updating WILL mess up timestamp flag!
			UPDATE	Determination
			SET	Preferred = 0
			WHERE	Determination_Key IN (
					SELECT	D.Determination_Key
					FROM	Determination D 
					WHERE	D.Occurrence_Key = @OccurrenceKey
				UNION
					SELECT	D.Determination_Key
					FROM	Specimen_Field_Data SFD 
					JOIN	Specimen_Unit SU ON SU.Collection_Unit_Key = SFD.Collection_Unit_Key
					JOIN	Determination D ON D.Specimen_Collection_Unit_Key = SU.Collection_Unit_Key
					WHERE	SFD.Occurrence_Key = @OccurrenceKey
				)
			AND	Preferred = 1
			AND	Determination_Key <> @Key 

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
		UPDATE	Determination
		SET	Concept_Key = @DeterminedItemKey,
			Occurrence_Key = @OccurrenceKey,
			Specimen_Collection_Unit_Key = @SpecimenCollectionUnitKey,
			Determination_Type_Key = @DeterminationTypeKey,
			Nomenclatural_Status_Concept_Key = @NomenclaturalStatusConceptKey,
			Confidence = @Confidence,
			Determiner_Name_Key = @DeterminerNameKey,
			Inferred_Determiner = @InferredDeterminer,
			Determiner_Role_Key = @DeterminerRoleKey,
			Vague_Date_Start = @VagueDateStart,
			Vague_Date_End = @VagueDateEnd,
			Vague_Date_Type = @VagueDateType,
			Used_Specimen = @UsedSpecimen,
			Preferred = @Preferred,
			Method = @Method,
			Notes = @Notes,
			Changed_Session_ID = @SessionID,
			Include_In_Label = @IncludeInLabel
		WHERE	Determination_Key = @Key
		AND		[Timestamp] = @Timestamp

		DECLARE @Error int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Determination WHERE Determination_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Determination_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Determination_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Determination_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Determination_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Determination_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Determination_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Determination_Update TO [Dev - JNCC SQL]
END
GO

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
    $Revision: 4 $
    $Date: 17/09/10 10:07 $
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
    $Revision: 4 $
    $Date: 17/09/10 10:07 $
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
    $Revision: 4 $
    $Date: 17/09/10 10:07 $
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