/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Collection_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Collection_Update]
GO

/*===========================================================================*\
  Description:	Updates a record in the Collection table.
		Ensures the Domain mask of the collection is also updated.

  Parameters:	@Key
		@ParentCollectionKey
		@ItemName
		@AssemblerNameKey
		@Topic
		@RiskConceptKey
		@CollectionTimestamp		Timestamp of Collection record
		@CurrentContainerKey
		@CurrentLocationCode
		@UsualContainerKey
		@UsualLocationCode
		@CollectionUnitTimestamp	Timestamp of Collection_Unit record
		@SessionID

  Created:	July 2003

  Last revision information:
    $Revision: 10 $
    $Date: 3/02/09 11:04 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Collection_Update]
	-- Collection 
	@Key char(16),
	@ParentCollectionKey char(16),
	@ItemName varchar(150),
	@AssemblerNameKey char(16),
	@Topic varchar(200),
	@RiskConceptKey char(16),
	@CollectionTimestamp timestamp,
	-- Collection_Unit
	@CurrentContainerKey char(16),
	@CurrentLocationCode varchar(30),
	@UsualContainerKey char(16),
	@UsualLocationCode varchar(30),
	@CollectionUnitTimestamp timestamp,
	-- Both
	@SessionID char(16)
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	DECLARE @ExistingParentCollectionKey char(16),
		@ExistingContainerKey char(16),
		@CollectionMask int,
		@ContainerMask int,
		@ParentCollectionMask int

	/*-------------------------------------------------------------*\
	  Initialise variables.
	\*-------------------------------------------------------------*/
	SELECT		@ExistingParentCollectionKey = C.Parent_Collection_Collection_Unit_Key,
			@ExistingContainerKey = CU.Current_Container_Collection_Unit_Key,
			@CollectionMask = CU.Domain_Mask
	FROM		Collection C
	INNER JOIN	Collection_Unit CU ON CU.Collection_Unit_Key = C.Collection_Unit_Key
	WHERE		C.Collection_Unit_Key = @Key

	/*-------------------------------------------------------------*\
	  If container changing, might need to update container mask.
	\*-------------------------------------------------------------*/
	IF @ExistingContainerKey <> @CurrentContainerKey
	BEGIN
		-- Work out the bit(s) to turn OFF
		SELECT	@ContainerMask = ~ Sum(DISTINCT Domain_Mask) & @CollectionMask
		FROM	Collection_Unit
		WHERE	Current_Container_Collection_Unit_Key = @ExistingContainerKey
		AND	Collection_Unit_Key <> @Key
	END

	/*-------------------------------------------------------------*\
	  If collection changing, might need to update collection mask.
	\*-------------------------------------------------------------*/
	IF @ExistingParentCollectionKey <> @ParentCollectionKey
	BEGIN
		-- Work out the bit(s) to turn OFF
		SELECT		@ParentCollectionMask = ~ Sum(DISTINCT CU.Domain_Mask) & @CollectionMask
		FROM		Collection C
		INNER JOIN	Collection_Unit CU ON CU.Collection_Unit_Key = C.Collection_Unit_Key
		WHERE		C.Parent_Collection_Collection_Unit_Key = @ExistingParentCollectionKey
		AND		C.Collection_Unit_Key <> @Key
	END

	/*-------------------------------------------------------------*\
	  Do the table updates first. Or the containers will still have
	  the specimen and its mask!
	\*-------------------------------------------------------------*/
	BEGIN TRANSACTION

		UPDATE	Collection
		SET	Parent_Collection_Collection_Unit_Key = @ParentCollectionKey,
			Item_Name = @ItemName,
			Assembler_Name_Key = @AssemblerNameKey,
			Topic = @Topic,
			Risk_Concept_Key = @RiskConceptKey,
			Changed_Session_ID = @SessionID
		WHERE	Collection_Unit_Key = @Key
		AND		[Timestamp] = @CollectionTimestamp
		
		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM "Collection" WHERE Collection_Unit_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

		UPDATE	Collection_Unit
		SET	Current_Container_Collection_Unit_Key = @CurrentContainerKey,
			Current_Location_Code = @CurrentLocationCode,
			Usual_Container_Collection_Unit_Key = @UsualContainerKey,
			Usual_Location_Code = @UsualLocationCode,
			Changed_Session_ID = @SessionID
		WHERE	Collection_Unit_Key = @Key
		AND		[Timestamp] = @CollectionUnitTimestamp

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
		IF (@ExistingContainerKey <> @CurrentContainerKey) 
		OR (@ExistingParentCollectionKey <> @ParentCollectionKey)
		BEGIN
			-- Loop through all 32 bits and check which ones need to be switched OFF or left alone.
			-- From 10000000 00000000 00000000 00000000 down to 00000000 00000000 00000000 00000001
			DECLARE	@BitMask int
			SET @BitMask = 0x80000000
	
			-- 1 / 2 = 0, use this as stop condition
			WHILE @BitMask <> 0
			BEGIN
				-- If the bit is found ON for container, switch it OFF
				IF (@ExistingContainerKey <> @CurrentContainerKey) 
				AND (@ContainerMask & @BitMask = @BitMask)
					-- Update the container mask
					EXECUTE	usp_CollectionUnit_Update_DomainMask @ExistingContainerKey, @BitMask, 0
				
	
				-- If the bit is found ON for collection, switch it OFF
				IF (@ExistingParentCollectionKey <> @ParentCollectionKey) 
				AND (@ParentCollectionMask & @BitMask = @BitMask)
					-- Update the collection mask
					EXECUTE	usp_Collection_Update_DomainMask @ExistingParentCollectionKey, @BitMask, 0
	
				-- Set mask for next single bit in line
				SET @BitMask = @BitMask / 2
			END
		END
	
		/*-------------------------------------------------------------*\
		  And ON. Straight forward all bits in this case.
		\*-------------------------------------------------------------*/
		-- Update the *new* container mask
		EXECUTE	usp_CollectionUnit_Update_DomainMask @CurrentContainerKey, @CollectionMask, 1
		-- Update the *new* collection mask
		EXECUTE	usp_Collection_Update_DomainMask @ParentCollectionKey, @CollectionMask, 1

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Collection_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Collection_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Collection_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Collection_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Collection_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Collection_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Collection_Update TO [Dev - JNCC SQL]
END
GO