/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Specimen_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Specimen_Update]
GO

/*===========================================================================*\
  Description:	Updates a record in the Specimen table.
		Ensures the Domain mask of the specimen is also updated.

  Parameters:	@Key
		@ParentCollectionCollectionUnitKey
		@SpecimenTypeConceptKey
		@Confidential
		@Dangerous
		@PublishToWeb
		@LifeSciences 
		@SUTimestamp 
		@Checked 
		@CurrentContainerCollectionUnitKey
		@CurrentLocationCode
		@UsualContainerCollectionUnitKey 
		@UsualLocationCode
		@CUTimestamp 
		@SessionID 

  Created:	July 2003

  Last revision information:
    $Revision: 13 $
    $Date: 18/03/14 11:23 $
    $Author: Brynhorsfieldschonhut $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Specimen_Update]
	--for specimen_unit
	@Key char(16), 
	@ParentCollectionCollectionUnitKey char(16),
	@SpecimenTypeConceptKey char(16),
	@Confidential bit,
	@Dangerous bit,
	@PublishToWeb bit,
	@LifeSciences bit,
	@SUTimestamp timestamp,
	@Checked bit,
	--for Collection_Unit
	@CurrentContainerCollectionUnitKey char(16),
	@CurrentLocationCode varchar(30),
	@UsualContainerCollectionUnitKey char(16),
	@UsualLocationCode varchar(30),
	@CUTimestamp timestamp,
	-- Both
	@SessionID char(16),
	@InternalUse bit
AS
	DECLARE @ExistingCollectionKey char(16),
		@ExistingContainerKey char(16),
		@SpecimenMask int,
		@ContainerMask int,
		@CollectionMask int

	/*-------------------------------------------------------------*\
	  Initialise variables.
	\*-------------------------------------------------------------*/
	SELECT		@ExistingCollectionKey = S.Parent_Collection_Collection_Unit_Key, 
			@ExistingContainerKey = CU.Current_Container_Collection_Unit_Key
	FROM		Specimen_Unit S
	INNER JOIN 	Collection_Unit CU ON CU.Collection_Unit_Key = S.Collection_Unit_Key
	WHERE		S.Collection_Unit_Key = @Key

	-- Retrieve the mask of the preferred concept for specimen.
	EXECUTE	usp_Get_Concept_Domain_Mask_From_Specimen @Key, @SpecimenMask OUTPUT

	/*-------------------------------------------------------------*\
	  If container changing, might need to update container mask.
	\*-------------------------------------------------------------*/
	IF @ExistingContainerKey <> @CurrentContainerCollectionUnitKey
	BEGIN
		-- A container may also be a specimen with its own concept. So get it.
		-- But if it is a storage, don't.
		SET @ContainerMask = 0
		IF EXISTS(SELECT * FROM Specimen_Unit WHERE Collection_Unit_Key = @ExistingContainerKey)
			EXECUTE	usp_Get_Concept_Domain_Mask_From_Specimen @ExistingContainerKey, @ContainerMask OUTPUT

		-- Work out the bit(s) to turn OFF
		SELECT		@ContainerMask = ~ @ContainerMask & ~ Sum(DISTINCT DM.Domain_Mask) & @SpecimenMask
		FROM		Collection_Unit CU
		INNER JOIN	Determination D 	WITH (INDEX (IX_Determination_Specimen)) 
							ON D.Specimen_Collection_Unit_Key = CU.Collection_Unit_Key
		INNER JOIN	Concept C 		WITH (INDEX (PK_Concept)) -- non-clustered
							ON C.Concept_Key = D.Concept_Key
		INNER JOIN	Concept_Group CG 	ON CG.Concept_Group_Key = C.Concept_Group_Key
		INNER JOIN	Local_Domain LD 	ON LD.Local_Domain_Key = CG.Local_Domain_Key
		INNER JOIN	Domain DM 		ON DM.Domain_Key = LD.Domain_Key
		WHERE		D.Preferred = 1
		AND		CU.Current_Container_Collection_Unit_Key = @ExistingContainerKey
		AND		CU.Collection_Unit_Key <> @Key
	END

	/*-------------------------------------------------------------*\
	  If collection changing, might need to update collection mask.
	\*-------------------------------------------------------------*/
	IF @ExistingCollectionKey <> @ParentCollectionCollectionUnitKey
	BEGIN
		-- Work out the bit(s) to turn OFF
		SELECT		@CollectionMask = ~ Sum(DISTINCT DM.Domain_Mask) & @SpecimenMask
		FROM		Specimen_Unit S
		INNER JOIN	Determination D 	WITH (INDEX (IX_Determination_Specimen)) 
							ON D.Specimen_Collection_Unit_Key = S.Collection_Unit_Key
		INNER JOIN	Concept C 		WITH (INDEX (PK_Concept))  -- non-clustered
							ON C.Concept_Key = D.Concept_Key
		INNER JOIN	Concept_Group CG 	ON CG.Concept_Group_Key = C.Concept_Group_Key
		INNER JOIN	Local_Domain LD 	ON LD.Local_Domain_Key = CG.Local_Domain_Key
		INNER JOIN	Domain DM 		ON DM.Domain_Key = LD.Domain_Key
		WHERE		D.Preferred = 1
		AND		S.Parent_Collection_Collection_Unit_Key = @ExistingCollectionKey
		AND		S.Collection_Unit_Key <> @Key
	END

	/*-------------------------------------------------------------*\
	  Do the table updates first. Or the containers will still have
	  the specimen and its mask!
	\*-------------------------------------------------------------*/
	BEGIN TRANSACTION	

		UPDATE	Specimen_Unit
		SET	Parent_Collection_Collection_Unit_Key = @ParentCollectionCollectionUnitKey,
			Specimen_Type_Concept_Key = @SpecimenTypeConceptKey,
			Confidential = @Confidential,
			Dangerous = @Dangerous,
			Publish_To_Web = @PublishToWeb,
			Life_Sciences = @LifeSciences,
			Changed_Session_ID = @SessionID,
			Checked = @Checked,
			Internal_Use = @InternalUse 
		WHERE	Collection_Unit_Key = @Key
		AND		[Timestamp] = @SUTimestamp
	
		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Specimen_Unit WHERE Collection_Unit_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END
	
		UPDATE	Collection_Unit
		SET	Current_Container_Collection_Unit_Key = @CurrentContainerCollectionUnitKey,
			Usual_Container_Collection_Unit_Key = @UsualContainerCollectionUnitKey,
			Current_Location_Code = @CurrentLocationCode,
			Usual_Location_Code = @UsualLocationCode,
			Changed_Session_ID = @SessionID
		WHERE	Collection_Unit_Key = @Key
		AND		[Timestamp] = @CUTimestamp
	
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Collection_Unit WHERE Collection_Unit_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END
	
		/*-------------------------------------------------------------*\
		  Now switch the bits OFF, if necessary.
		\*-------------------------------------------------------------*/
		IF (@ExistingContainerKey <> @CurrentContainerCollectionUnitKey) OR (@ExistingCollectionKey <> @ParentCollectionCollectionUnitKey)
		BEGIN
			-- Loop through all 32 bits and check which ones need to be switched OFF or left alone.
			-- From 10000000 00000000 00000000 00000000 down to 00000000 00000000 00000000 00000001
			DECLARE	@BitMask bigint  -- Use BIGINT, no unsigned int in SQL Server!!!!!
			SET @BitMask = 0x80000000
	
			-- 1 / 2 = 0, use this as stop condition
			WHILE @BitMask <> 0
			BEGIN
				-- If the bit is found ON for container, switch it OFF
				IF (@ExistingContainerKey <> @CurrentContainerCollectionUnitKey) AND (@ContainerMask & @BitMask = @BitMask)
					-- Update the container mask
					EXECUTE	usp_CollectionUnit_Update_DomainMask @ExistingContainerKey, @BitMask, 0
				
	
				-- If the bit is found ON for collection, switch it OFF
				IF (@ExistingCollectionKey <> @ParentCollectionCollectionUnitKey) AND (@CollectionMask & @BitMask = @BitMask)
					-- Update the collection mask
					EXECUTE	usp_Collection_Update_DomainMask @ExistingCollectionKey, @BitMask, 0
	
				-- Set mask for next single bit in line
				SET @BitMask = @BitMask / 2
			END
		END
	
		/*-------------------------------------------------------------*\
		  And ON. Straight forward all bits in this case.
		\*-------------------------------------------------------------*/
		-- Update the *new* container mask
		EXECUTE	usp_CollectionUnit_Update_DomainMask @CurrentContainerCollectionUnitKey, @SpecimenMask, 1
		-- Update the *new* collection mask
		EXECUTE	usp_Collection_Update_DomainMask @ParentCollectionCollectionUnitKey, @SpecimenMask, 1

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Specimen_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Specimen_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Specimen_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimen_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimen_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimen_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Specimen_Update TO [Dev - JNCC SQL]
END
GO