/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Collection_Delete]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Collection_Delete]
GO

/*===========================================================================*\
  Description:	Delete a record from the Collection table.
		Its mask should already have been dealt with and be 0. If not, 
		something wrong	probably happened somewhere else.

  Parameters:	@Key

  Created:	July 2003

  Last revision information:
    $Revision: 1 $
    $Date: 3/02/09 12:20 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Collection_Delete]
	@Key char(16),
	@Timestamp timestamp
AS
	BEGIN TRANSACTION

		-- Delete record from Collection table.
		DELETE	Collection
		WHERE	Collection_Unit_Key = @Key
	
		IF @@Error <> 0 GOTO RollbackAndExit

		DELETE	Specimen_Unit
		WHERE	Collection_Unit_Key = @Key

		IF @@Error <> 0 GOTO RollbackAndExit

		-- Delete record from Collection_Unit table.
		DELETE	Collection_Unit
		WHERE	Collection_Unit_Key = @Key
		AND		@Timestamp = [Timestamp]

		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Collection_Unit WHERE Collection_Unit_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

		IF @@Error <> 0 GOTO RollbackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Collection_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Collection_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Collection_Delete TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Collection_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Collection_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Collection_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Collection_Delete TO [Dev - JNCC SQL]
END
GO

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
    $Revision: 1 $
    $Date: 3/02/09 12:20 $
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

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_CollectionUnitData_Delete]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_CollectionUnitData_Delete]
GO

/*===========================================================================*\
  Description:	Deletes a record in the Collection Unit Data table

  Parameters:	@Key
		@Timestamp

  Created:	November 2003

  Last revision information:
    $Revision: 1 $
    $Date: 3/02/09 12:20 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_CollectionUnitData_Delete]
	@Key char(16),
	@Timestamp timestamp
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION

		DELETE	Collection_Unit_Data
		WHERE	Collection_Unit_Data_Key = @Key
		AND	[Timestamp] = @Timestamp

		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Collection_Unit_Data WHERE Collection_Unit_Data_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_CollectionUnitData_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_CollectionUnitData_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitData_Delete TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_CollectionUnitData_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_CollectionUnitData_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_CollectionUnitData_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitData_Delete TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_CollectionUnitData_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_CollectionUnitData_Update]
GO

/*===========================================================================*\
  Description:	Updates a record in the Collection_Unit_Data table.
		The Collection_Unit_Data table hold descriptor and measurement
		information.

  Parameters:	@Key
		@CollectionUnitKey
		@AppliesTo
		@MethodConceptKey
		@Duration
		@Accuracy
		@ParameterConceptKey
		@UnitConceptKey
		@Value
		@UpperValue
		@IsDescriptor
		@SessionID
		@Timestamp

  Created:	September 2003

  Last revision information:
    $Revision: 1 $
    $Date: 3/02/09 12:20 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_CollectionUnitData_Update]
	-- Required by both Measurements and Descriptors updates.
	@Key char(16),
	@IsDescriptor bit,
	@ParameterConceptKey char(16),
	@AppliesTo varchar(50),
	@Value varchar(50) = NULL,
	@SessionID char(16),
	@Timestamp timestamp,
	-- Only required for the Measurements update.
	@UpperValue varchar(50) = NULL,
	@CollectionUnitKey char(16) = NULL,
	@MethodConceptKey char(16) = NULL,
	@Duration varchar(50) = NULL,
	@Accuracy varchar(50) = NULL,
	@UnitConceptKey char(16) = NULL
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	/*----------------------------------------------------------------------------------*\
	  If we are updating measurement data, more information needs to changed than if
	  we are inserting descriptor data. Hence, there are two different update statements.
	\*----------------------------------------------------------------------------------*/
	BEGIN TRANSACTION

		IF @IsDescriptor = 1	
			-- Updating a descriptor.	
			UPDATE	Collection_Unit_Data
			SET	
				Applies_To = @AppliesTo,		
				Parameter_Concept_Key = @ParameterConceptKey,
				Lower_Value = IsNull(@Value, ' '),
				Is_Descriptor = @IsDescriptor,
				Changed_Session_ID = @SessionID
			WHERE	Collection_Unit_Data_Key	= @Key
			AND		[Timestamp]					= @Timestamp
		ELSE			
			-- Updating a measurement.
			UPDATE	Collection_Unit_Data
			SET	
				Applies_To = @AppliesTo,		
				Parameter_Concept_Key = @ParameterConceptKey,
				Lower_Value = IsNull(@Value, ' '),
				Is_Descriptor = @IsDescriptor,
				Changed_Session_ID = @SessionID,
				Collection_Unit_Key = @CollectionUnitKey,
				Method_Concept_Key = @MethodConceptKey,
				Duration = @Duration,
				Accuracy = @Accuracy,
				Unit_Concept_Key = @UnitConceptKey,
				Upper_Value = @UpperValue
			WHERE	Collection_Unit_Data_Key	= @Key
			AND		[Timestamp]					= @Timestamp

		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Collection_Unit_Data WHERE Collection_Unit_Data_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_CollectionUnitData_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_CollectionUnitData_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitData_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_CollectionUnitData_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_CollectionUnitData_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_CollectionUnitData_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitData_Update TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_CollectionUnitHistory_Delete]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_CollectionUnitHistory_Delete]
GO

/*===========================================================================*\
  Description:	Delete a record from the Collection_Unit_History table.

  Parameters:	@Key		Collection Unit History key.
		@Timestamp

  Created:	December 2003

  Last revision information:
    $Revision: 1 $
    $Date: 3/02/09 12:20 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_CollectionUnitHistory_Delete]
	@Key char(16),
	@Timestamp timestamp
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION

		DELETE	Collection_Unit_History
		WHERE	Collection_Unit_History_Key = @Key
		AND		[Timestamp] = @Timestamp
		
		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Collection_Unit_History WHERE Collection_Unit_History_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_CollectionUnitHistory_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_CollectionUnitHistory_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_CollectionUnitHistory_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_CollectionUnitHistory_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_CollectionUnitHistory_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitHistory_Delete TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_CollectionUnitHistory_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_CollectionUnitHistory_Update]
GO
 
/*===========================================================================*\
  Description:	Updates a record in the CollectionUnitHistory table.

  Parameters:	@Key 
		@CollectionUnitKey 
		@SourceNameKey 
		@FromVagueDateStart 
		@FromVagueDateEnd 
		@FromVagueDateType 
		@ToVagueDateStart 
		@ToVagueDateEnd 
		@ToVagueDateType
		@ItemName 
		@Comment 
		@SessionID 
		@Timestamp

  Created:	November 2003

  Last revision information:
    $Revision: 1 $
    $Date: 3/02/09 12:20 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_CollectionUnitHistory_Update]
	@Key char(16),
	@CollectionUnitKey char(16),
	@SourceNameKey char(16),
	@FromVagueDateStart int,
	@FromVagueDateEnd int,
	@FromVagueDateType varchar(2),
	@ToVagueDateStart int,
	@ToVagueDateEnd int,
	@ToVagueDateType varchar(2),
	@ItemName varchar(100),
	@Comment text,
	@SessionID char(16),
	@Timestamp timestamp
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION

		UPDATE 	Collection_Unit_History 
		SET	Collection_Unit_Key = @CollectionUnitKey,
			Source_Name_Key = @SourceNameKey,
			From_Vague_Date_Start = @FromVagueDateStart,
			From_Vague_Date_End = @FromVagueDateEnd,
			From_Vague_Date_Type = @FromVagueDateType,
			To_Vague_Date_Start = @ToVagueDateStart,
			To_Vague_Date_End = @ToVagueDateEnd,
			To_Vague_Date_Type = @ToVagueDateType,
			Item_Name = @ItemName,
			Comment = @Comment,
			Changed_Session_ID = @SessionID			
		
		WHERE	Collection_Unit_History_Key = @Key
		AND		[Timestamp] = @Timestamp

		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Collection_Unit_History WHERE Collection_Unit_History_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_CollectionUnitHistory_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_CollectionUnitHistory_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitHistory_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_CollectionUnitHistory_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_CollectionUnitHistory_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_CollectionUnitHistory_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitHistory_Update TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_CollectionUnitMaterial_Delete]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_CollectionUnitMaterial_Delete]
GO

/*===========================================================================*\
  Description:	Delete a record from the Collection_Unit_Material table.

  Parameters:	@Key		Collection Unit Material key.
		@Timestamp

  Created:	September 2003

  Last revision information:
    $Revision: 1 $
    $Date: 3/02/09 12:20 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_CollectionUnitMaterial_Delete]
	@Key char(16),
	@Timestamp timestamp
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION

		DELETE	Collection_Unit_Material
		WHERE	Collection_Unit_Material_Key = @Key
		AND		[Timestamp] = @Timestamp

		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Collection_Unit_Material WHERE Collection_Unit_Material_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_CollectionUnitMaterial_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_CollectionUnitMaterial_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitMaterial_Delete TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_CollectionUnitMaterial_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_CollectionUnitMaterial_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_CollectionUnitMaterial_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitMaterial_Delete TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_CollectionUnitMaterial_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_CollectionUnitMaterial_Update]
GO

/*===========================================================================*\
  Description:	Update a record in the Collection_Unit_Material table.

  Parameters:	@Key		Collection Unit Material key.
		@MaterialKey
		@Quantity
		@UnitKey
		@SessionID
		@Timestamp

  Created:	September 2003

  Last revision information:
    $Revision: 1 $
    $Date: 3/02/09 12:20 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_CollectionUnitMaterial_Update]
	@Key char(16),
	@MaterialKey char(16),
	@Quantity varchar(20),
	@UnitKey char(16),
	@SessionID char(16),
	@Timestamp timestamp
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION

		UPDATE	Collection_Unit_Material
		SET	Material_Concept_Key = @MaterialKey,
			Quantity = @Quantity,
			Unit_Concept_Key = @UnitKey,
			Changed_Session_ID = @SessionID
		WHERE	Collection_Unit_Material_Key = @Key
		AND		[Timestamp] = @Timestamp

		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Collection_Unit_Material WHERE Collection_Unit_Material_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_CollectionUnitMaterial_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_CollectionUnitMaterial_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitMaterial_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_CollectionUnitMaterial_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_CollectionUnitMaterial_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_CollectionUnitMaterial_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitMaterial_Update TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_CollectionUnitName_Delete]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_CollectionUnitName_Delete]
GO

/*===========================================================================*\
  Description:	Delete a record from the Collection_Unit_Name table.

  Parameters:	@Key
		@Timestamp

  Created:	December 2003

  Last revision information:
    $Revision: 1 $
    $Date: 3/02/09 12:20 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_CollectionUnitName_Delete]
	@Key char(16),
	@Timestamp timestamp
AS
	SET NOCOUNT OFF

	BEGIN TRANSACTION

		-- Delete record from Collection_Unit_Name table.
		DELETE	Collection_Unit_Name
		WHERE	Collection_Unit_Name_Key = @Key
		AND		[Timestamp] = @Timestamp
	
		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Collection_Unit_Name WHERE Collection_Unit_Name_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_CollectionUnitName_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_CollectionUnitName_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_CollectionUnitName_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_CollectionUnitName_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_CollectionUnitName_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitName_Delete TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_CollectionUnitName_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_CollectionUnitName_Update]
GO

/*===========================================================================*\
  Description:	Updates a record in the Collection_Unit_Name table.

  Parameters:	@Key
		@CollectionUnitKey
		@Name_Key
		@Relating_Type_Concept_Key
		@Vague_Date_Start
		@Vague_Date_End
		@Vague_Date_Type 
		@Comment
		@SessionID
		@Timestamp

  Created:	October 2003

  Last revision information:
    $Revision: 1 $
    $Date: 3/02/09 12:20 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_CollectionUnitName_Update]
	@Key char(16) OUTPUT,
	@CollectionUnitKey char(16),
	@NameKey char(16),
	@RelationTypeConceptKey char(16),
	@VagueDateStart int,
	@VagueDateEnd int,
	@VagueDateType varchar(2),
	@Comment text,
	@SessionID char(16),
	@Timestamp timestamp	
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION

		UPDATE 	Collection_Unit_Name
		SET	Collection_Unit_Key = @CollectionUnitKey,
			Name_Key = @NameKey,
			Relation_Type_Concept_Key = @RelationTypeConceptKey,
			Vague_Date_Start = @VagueDateStart,
			Vague_Date_End = @VagueDateEnd,
			Vague_Date_Type = @VagueDateType,
			Comment = @Comment,
			Changed_Session_ID = @SessionID
		WHERE	Collection_Unit_Name_Key = @Key
		AND		[Timestamp] = @Timestamp

		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Collection_Unit_Name WHERE Collection_Unit_Name_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_CollectionUnitName_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_CollectionUnitName_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitName_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_CollectionUnitName_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_CollectionUnitName_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_CollectionUnitName_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitName_Update TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_CollectionUnitNumber_Delete]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_CollectionUnitNumber_Delete]
GO

/*===========================================================================*\
  Description:	Delete a record from the Conservation_Unit_Number table.

  Parameters:	@Key
		@Timestamp

  Created:	December 2003

  Last revision information:
    $Revision: 1 $
    $Date: 3/02/09 12:20 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_CollectionUnitNumber_Delete]
	@Key char(16),
	@Timestamp timestamp
AS
	SET NOCOUNT OFF

	BEGIN TRANSACTION

		-- Delete record from Collection_Unit_Number table.
		DELETE	Collection_Unit_Number
		WHERE	Collection_Unit_Number_Key = @Key
		AND		[Timestamp] = @Timestamp
	
		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Collection_Unit_Number WHERE Collection_Unit_Number_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_CollectionUnitNumber_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_CollectionUnitNumber_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_CollectionUnitNumber_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_CollectionUnitNumber_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_CollectionUnitNumber_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitNumber_Delete TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_CollectionUnitNumber_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_CollectionUnitNumber_Update]
GO

/*===========================================================================*\
  Description:	Updates a record into the Collection Unit Number table

  Parameters:	@Key

		@Timestamp

  Created:	September 2003

  Last revision information:
    $Revision: 1 $
    $Date: 3/02/09 12:20 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_CollectionUnitNumber_Update]
	@Key char(16),
	@CollectionUnitKey char(16),
	@Number varchar(30),
	@TypeConceptKey char(16),
	@Preferred bit,
	@Notes text,
	@SessionID char(16),
	@Timestamp timestamp,
	@RecordsAffected int OUTPUT

AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	DECLARE @Error int
	/*---------------------------*\
	  Actually updates the table
	\*---------------------------*/
	BEGIN TRANSACTION

		IF @Preferred = 1
			UPDATE 	Collection_Unit_Number
			SET	Preferred = 0
			WHERE	Collection_Unit_Key = @CollectionUnitKey
			AND	Collection_Unit_Number_Key <> @Key -- So we don't get timestamp problems.
		
		UPDATE 	Collection_Unit_Number
		SET 	Collection_Unit_Key= @CollectionUnitKey,
			Number = @Number,
			Type_Concept_Key = @TypeConceptKey,
			Preferred = @Preferred,
			Notes = @Notes,
			Changed_Session_ID = @SessionID

		WHERE	Collection_Unit_Number_Key = @Key
		AND		[Timestamp] = @Timestamp
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Collection_Unit_Number WHERE Collection_Unit_Number_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_CollectionUnitNumber_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_CollectionUnitNumber_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitNumber_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_CollectionUnitNumber_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_CollectionUnitNumber_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_CollectionUnitNumber_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitNumber_Update TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_CollectionUnitProcess_Delete]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_CollectionUnitProcess_Delete]
GO

/*===========================================================================*\
  Description:	Delete a record from the Collection_Unit_Process table.

  Parameters:	@Key
		@Timestamp

  Created:	December 2003

  Last revision information:
    $Revision: 1 $
    $Date: 3/02/09 12:20 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_CollectionUnitProcess_Delete]
	@Key char(16),
	@Timestamp timestamp
AS
	SET NOCOUNT OFF

	BEGIN TRANSACTION

		-- Delete record from Collection_Unit_Process table.
		DELETE	Collection_Unit_Process
		WHERE	Collection_Unit_Process_Key = @Key
		AND		[Timestamp] = @Timestamp
	
		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Collection_Unit_Process WHERE Collection_Unit_Process_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_CollectionUnitProcess_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_CollectionUnitProcess_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_CollectionUnitProcess_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_CollectionUnitProcess_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_CollectionUnitProcess_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitProcess_Delete TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_CollectionUnitProcess_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_CollectionUnitProcess_Update]
GO

/*===========================================================================*\
  Description:	Updates a record in the Collection_Unit_Process table.

  Parameters:	@Key 
		@CollectionUnitKey 
		@ProcessConceptKey 
		@Description
		@NameKey
		@VagueDateStart
		@VagueDateEnd
		@VagueDateType
		@InferredProcess
		@InferredDescription
		@InferredPerson
		@InferredDate 
		@SessionID

  Created:	October 2003

  Last revision information:
    $Revision: 1 $
    $Date: 3/02/09 12:20 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_CollectionUnitProcess_Update]
	@Key char(16) OUTPUT,
	@CollectionUnitKey char(16),
	@ProcessConceptKey char(16),
	@Description text,
	@NameKey char(16),
	@VagueDateStart int,
	@VagueDateEnd int,
	@VagueDateType varchar(2),
	@InferredProcess tinyint,
	@InferredDescription tinyint,
	@InferredPerson tinyint,
	@InferredDate tinyint,
	@SessionID char(16),
	@Timestamp timestamp
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION

		UPDATE 	Collection_Unit_Process
		SET 	Collection_Unit_Key = @CollectionUnitKey,
			Process_Concept_Key = @ProcessConceptKey,
			Description = @Description,
			Name_Key = @NameKey,
			Vague_Date_Start = @VagueDateStart,
			Vague_Date_End = @VagueDateEnd,
			Vague_Date_Type = @VagueDateType,
			Inferred_Process = @InferredProcess,
			Inferred_Description = @InferredDescription,
			Inferred_Person = @InferredPerson,
			Inferred_Date = @InferredDate,
			Changed_Session_ID = @SessionID
		WHERE	Collection_Unit_Process_Key = @Key
		AND		[Timestamp] = @Timestamp

		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Collection_Unit_Process WHERE Collection_Unit_Process_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_CollectionUnitProcess_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_CollectionUnitProcess_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitProcess_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_CollectionUnitProcess_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_CollectionUnitProcess_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_CollectionUnitProcess_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitProcess_Update TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_CollectionUnitRelation_Delete]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_CollectionUnitRelation_Delete]
GO

/*===========================================================================*\
  Description:	Delete a record from the Collection_Unit_Relation table.

  Parameters:	@Key		Collection Unit Relation key.
		@Timestamp

  Created:	December 2003

  Last revision information:
    $Revision: 1 $
    $Date: 3/02/09 12:20 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_CollectionUnitRelation_Delete]
	@Key char(16),
	@Timestamp timestamp
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION

		DELETE	Collection_Unit_Relation
		WHERE	Collection_Unit_Relation_Key = @Key
		AND		[Timestamp] = @Timestamp

		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Collection_Unit_Relation WHERE Collection_Unit_Relation_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_CollectionUnitRelation_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_CollectionUnitRelation_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_CollectionUnitRelation_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_CollectionUnitRelation_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_CollectionUnitRelation_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitRelation_Delete TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_CollectionUnitRelation_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_CollectionUnitRelation_Update]
GO

/*===========================================================================*\
  Description:	Updates a record in the Collection Unit Relation table.

  Parameters:	@Key
		@FromCollectionUnitKey
		@ToCollectionUnitKey
		@ThesaurusRelationTypeKey
		@InferredType
		@VagueDateStart
		@VagueDateEnd
		@VagueDateType
		@AuthorNameKey 
		@Comment 
		@SessionID
		@Timestamp

  Created:	October 2003

  Last revision information:
    $Revision: 1 $
    $Date: 3/02/09 12:20 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_CollectionUnitRelation_Update]
	@Key char(16),
	@FromCollectionUnitKey char(16),
	@ToCollectionUnitKey char(16),
	@ThesaurusRelationTypeKey char(16),
	@InferredType tinyint,
	@VagueDateStart int,
	@VagueDateEnd int,
	@VagueDateType varchar(2),
	@AuthorNameKey char(16),
	@Comment text,
	@SessionID char(16),
	@Timestamp timestamp
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION

		UPDATE 	Collection_Unit_Relation
		SET	From_Collection_Unit_Key = @FromCollectionUnitKey,
			To_Collection_Unit_Key = @ToCollectionUnitKey,
			Thesaurus_Relation_Type_Key = @ThesaurusRelationTypeKey,
			Inferred_Type = @InferredType,
			Vague_Date_Start = @VagueDateStart,
			Vague_Date_End = @VagueDateEnd,
			Vague_Date_Type = @VagueDateType,
			Author_Name_Key = @AuthorNameKey,
			Comment = @Comment,
			Changed_Session_ID = @SessionID
		WHERE 	Collection_Unit_Relation_Key = @Key
		AND		[Timestamp] = @Timestamp

		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Collection_Unit_Relation WHERE Collection_Unit_Relation_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_CollectionUnitRelation_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_CollectionUnitRelation_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitRelation_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_CollectionUnitRelation_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_CollectionUnitRelation_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_CollectionUnitRelation_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitRelation_Update TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Concept_Delete]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Concept_Delete]
GO

/*===========================================================================*\
  Description:	Delete a record from the Concept tables. Also deletes records
		from other tables where necessary.  If @DeleteUnlinkedSynonyms
		is 1, then removes any non-list preferred concepts from the 
		same concept group.

  Parameters:	@Key		Concept key.
				@Timestamp

  Created:	December 2003

  Last revision information:
    $Revision: 1 $
    $Date: 3/02/09 12:20 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Concept_Delete]
	@Key char(16),
	@Timestamp timestamp = NULL,
	@SyncTaxonDict bit = 0,
	@DeleteUnlinkedSynonyms bit = 0
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	DECLARE @MeaningKey char(16),
			@TermKey char(16),
			@TermVersionKey char(16),
			@ConceptsSharingMeaningKeyCount int,
			@ConceptsSharingTermKeyCount int,
			@ConceptsSharingTermVersionKeyCount int,
			@OriginalTimestamp timestamp,
			@error				INT,
			@RecordsAffected	INT

	-- Store the Meaning, Term and Term Version keys because the concept record
	-- needs to be deleted before these other records can be, due to referential
	-- integrity.
	SELECT	@MeaningKey = Meaning_Key,
			@TermKey = Term_Key,
			@TermVersionKey = Term_Version_Key,
			@OriginalTimestamp = [Timestamp]
	FROM 	Concept
	WHERE	Concept_Key = @Key

	-- Count the number of concepts that use this meaning key.
	SELECT 		@ConceptsSharingMeaningKeyCount = Count(C2.Concept_Key)
	FROM		Concept AS C1
	INNER JOIN	Concept AS C2 ON C2.Meaning_Key = C1.Meaning_Key
	WHERE		C1.Concept_Key = @Key

	-- Count the number of concepts that use the same term key as the concept we want to delete.
	SELECT 		@ConceptsSharingTermKeyCount = Count(DISTINCT C2.Concept_Key)
	FROM		Concept AS C1
	INNER JOIN	Concept AS C2 ON C2.Term_Key = C1.Term_Key
	WHERE		C1.Concept_Key = @Key

	-- Count the number of concepts that use the same term version key as the concept we want to delete.
	SELECT 		@ConceptsSharingTermVersionKeyCount = Count(DISTINCT C2.Concept_Key)
	FROM		Concept AS C1
	INNER JOIN	Concept AS C2 ON C2.Term_Version_Key = C1.Term_Version_Key
	WHERE		C1.Concept_Key = @Key


	BEGIN TRANSACTION
		/*============================================================*\
		  See if the user wants any associated taxon dictionary
		  records be deleted with the concept.
		\*============================================================*/
		IF @SyncTaxonDict = 1 
		BEGIN
			/*--------------------------------------------------------*\
			  Check that the Taxon_List_Item table exists before
			  attempting any of this deletion. In the future, the 
			  Thesaurus module could be installed without the Taxon
			  tables, so would go wrong if we tried to delete from
			  non-existant tables.			
			\*--------------------------------------------------------*/
			IF EXISTS (SELECT *
		   			FROM   SysObjects 
					WHERE  Id = Object_Id(N'[dbo].[Taxon_List_Item]')
					AND 	  Type = 'U')
			BEGIN
				-- Get the Taxon List Item Key for the current Concept
				DECLARE @TaxonListItemKey char(16)
	
				SELECT 	@TaxonListItemKey = Taxon_List_Item_Key
				FROM	Taxon_Dictionary_Concept_Mapping
				WHERE	Concept_Key = @Key

				/*--------------------------------------------------------*\
				  Delete the records related to the Taxon_List_Item table
				\*--------------------------------------------------------*/
				DELETE 	Taxon_Dictionary_Concept_Mapping
				WHERE	Taxon_List_Item_Key = @TaxonListItemKey
				AND	Concept_Key = @Key

				IF @@Error <> 0 GOTO RollbackAndExit

				DELETE	Taxon_Common_Name
				WHERE	Taxon_List_Item_Key = @TaxonListItemKey

				IF @@Error <> 0 GOTO RollbackAndExit

				DELETE	Index_Taxon_Synonym
				WHERE	Taxon_List_Item_Key = @TaxonListItemKey
				OR	Synonym_List_Item_Key = @TaxonListItemKey

				IF @@Error <> 0 GOTO RollbackAndExit

				DELETE	Export_Filter_Taxon
				WHERE	Taxon_List_Item_Key = @TaxonListItemKey

				IF @@Error <> 0 GOTO RollbackAndExit

				DELETE	Index_Taxon_Group
				WHERE	Taxon_List_Item_Key = @TaxonListItemKey
				OR	Contained_List_Item_Key = @TaxonListItemKey

				IF @@Error <> 0 GOTO RollbackAndExit

				DELETE	Taxon_Designation
				WHERE	Taxon_List_Item_Key = @TaxonListItemKey

				IF @@Error <> 0 GOTO RollbackAndExit

				DELETE	Taxon_User_Name
				WHERE	Taxon_List_Item_Key = @TaxonListItemKey

				IF @@Error <> 0 GOTO RollbackAndExit

				DELETE	Index_Taxon_Name
				WHERE	Taxon_List_Item_Key = @TaxonListItemKey

				IF @@Error <> 0 GOTO RollbackAndExit

				DELETE	Nameserver
				WHERE	Recommended_Taxon_List_Item_Key = @TaxonListItemKey

				IF @@Error <> 0 GOTO RollbackAndExit

				-- If one Concept shares the Term, attempt to delete the equivalent Taxon.
				IF @ConceptsSharingTermKeyCount = 1
				BEGIN
					DECLARE @TaxonKey char(16)

					-- Get the key of the equivalent Taxon
					SELECT 	@TaxonKey = Taxon_Key
					FROM	Taxon_Dictionary_Term_Mapping
					WHERE	Term_Key = @TermKey

							-- Only delete if there are no Taxon_Version records using the Taxon
					IF NOT EXISTS(SELECT 	*
									FROM 	Taxon_Version
									WHERE	Taxon_Key = @TaxonKey)
					BEGIN
						DELETE SF
						FROM Source_File SF
						INNER JOIN Taxon_Sources TS ON TS.Source_Key=SF.Source_Key
						WHERE TS.Taxon_Key=@TaxonKey
		
						DELETE Taxon_Sources
						WHERE Taxon_Key=@TaxonKey
					
						DELETE	Taxon
						WHERE	Taxon_Key = @TaxonKey
					END
				END

				/*-----------------------------------------------------------------*\
				  It is possible that this delete will fail. e.g. If the TLI record
				  is referred to in the Taxon_Determination table, or a row in 
				  the TLI table has its Parent set to the record we are attempting
				  to delete. This will cause it to go to the RollbackAndExit method,
				  where the user can be asked if they want to replace the concept
				  with another (4.2.17.18). Before deleting the TLI records, we
				  need to remove the Taxon_Dictionary_Meaning_Mapping records.
				\*-----------------------------------------------------------------*/ 
				DELETE	Taxon_Dictionary_Meaning_Mapping
				WHERE	Preferred_Name = @TaxonListItemKey

				IF @@Error <> 0 GOTO RollbackAndExit

				DELETE 	Taxon_List_Item
				WHERE	Taxon_List_Item_Key = @TaxonListItemKey

				IF @@Error <> 0 GOTO RollbackAndExit
			END
		END			

		/*====================================*\
		  Delete the synonyms that are no longer
		  required.
		\*====================================*/
		IF @DeleteUnlinkedSynonyms=1
		BEGIN
			DECLARE @SynConceptKey CHAR(16)
			
			DECLARE csr CURSOR FOR
				SELECT CSyn.Concept_Key
				FROM Concept C
				INNER JOIN Concept CSyn 
					ON CSyn.Meaning_Key=C.Meaning_Key
					AND CSyn.Concept_Group_Key=C.Concept_Group_Key
					AND CSyn.List_Preferred=0
					AND C.Concept_Key=@Key
			
			OPEN csr
			WHILE (1=1)
			BEGIN
				FETCH NEXT FROM csr INTO @SynConceptKey

				IF @@FETCH_STATUS <> 0
					BREAK

				-- Recurse to remove synonym concepts
				EXEC usp_Concept_Delete @SynConceptKey
			END
			CLOSE csr
			DEALLOCATE csr
		END
	
		/*====================================*\
		  Delete the records.
		\*====================================*/
		-- Delete the Concept_History record.
		DELETE	Concept_History
		WHERE	Concept_Key = @Key

		IF @@Error <> 0 GOTO RollbackAndExit

		/*-------------------------------------------------------*\
		  Delete the relation records which refer to the concept.
		\*-------------------------------------------------------*/
		DELETE	Concept_Relation
		WHERE	To_Concept_Key = @Key
		OR	From_Concept_Key = @Key

		IF @@Error <> 0 GOTO RollbackAndExit

		DELETE	Meaning_Relation
		WHERE	To_Concept_Key = @Key
		OR	From_Concept_Key = @Key

		IF @@Error <> 0 GOTO RollbackAndExit

		DELETE	Term_Version_Relation
		WHERE	To_Concept_Key = @Key
		OR	From_Concept_Key = @Key

		IF @@Error <> 0 GOTO RollbackAndExit

		/*-------------------------------------------------------*\
		  Delete the Enquiry_Concept records because otherwise
		  the deletion will fail because it says other records
		  link to the Concept. Enquiries cannot be viewed in the
		  Thesaurus Editor it would appear at a casual glance
		  that nothing is actually linked to the concept. 
		  So best to just delete the Enquiry_Concept join records.
		\*-------------------------------------------------------*/
		IF EXISTS (SELECT * FROM dbo.sysobjects WHERE id = object_id(N'[dbo].[Enquiry_Concept]') 
					AND OBJECTPROPERTY(id, N'IsUserTable') = 1)
			DELETE	Enquiry_Concept
			WHERE	Concept_Key = @Key

		IF @@Error <> 0 GOTO RollbackAndExit

		-- Delete the Concept_Lineage records.
		IF EXISTS (SELECT 1 FROM Concept WHERE Concept_Key = @Key)
		BEGIN
			EXECUTE		usp_ConceptLineage_DeleteConcept	@Key
			IF @@ERROR <> 0 GOTO RollbackAndExit
		END

		/*-------------------------------------------------------*\
			Delete the concept's designation records (and related)
		\*-------------------------------------------------------*/
		IF EXISTS (SELECT * FROM dbo.sysobjects WHERE id = object_id(N'[dbo].[Taxon_Dictionary_Concept_Designation_Mapping]') 
					AND OBJECTPROPERTY(id, N'IsUserTable') = 1)
			DELETE DM
			FROM Taxon_Dictionary_Concept_Designation_Mapping DM
			INNER JOIN Concept_Designation CD ON CD.Concept_Designation_Key=DM.Concept_Designation_Key
			WHERE CD.Concept_Key=@Key

		IF @@Error <> 0 GOTO RollbackAndExit		

		IF EXISTS (SELECT * FROM dbo.sysobjects WHERE id = object_id(N'[dbo].[Source_Join]') 
					AND OBJECTPROPERTY(id, N'IsUserTable') = 1)
		BEGIN
			--Delete the source files
			DELETE SF
			FROM Source_File SF
			INNER JOIN Source_Join SJ
					ON SJ.Table_Name='Concept_Designation'
			INNER JOIN Concept_Designation CD
					ON CD.Concept_Designation_Key=SJ.Record_Key
					AND CD.Concept_Key=@Key
	
			IF @@Error <> 0 GOTO RollbackAndExit
		
			--Now delete the source joins
			DELETE SJ
			FROM Source_Join SJ
			INNER JOIN Concept_Designation CD
					ON CD.Concept_Designation_Key=SJ.Record_Key
					AND CD.Concept_Key=@Key
			WHERE SJ.Table_Name='Concept_Designation'

			IF @@Error <> 0 GOTO RollbackAndExit

			--Delete the source files for the main concept
			DELETE SF
			FROM Source_File SF
			INNER JOIN Source_Join SJ
					ON SJ.Table_Name='Concept' AND SJ.Record_Key=@Key

			IF @@Error <> 0 GOTO RollbackAndExit

			--Now delete the source joins
			DELETE SJ
			FROM Source_Join SJ
			WHERE SJ.Table_Name='Concept' AND SJ.Record_Key=@Key

			IF @@Error <> 0 GOTO RollbackAndExit
		END

		DELETE 
		FROM Concept_Designation
		WHERE Concept_Key=@Key

		/*-------------------------------------------------------*\
			 Delete the Concept record. Have to check timestamp passed into the proc
			 against the timestamp the Concept had before any of its related records
			 were deleted. This is because deleting the records above may cause
			 triggers to be fired. Deleting the record in Concept_History will fire
			 a trigger that updates the current Concept, causing its timestamp to 
			 change.
		\*-------------------------------------------------------*/

		DELETE	Concept
		WHERE	Concept_Key = @Key
		AND		(@Timestamp = @OriginalTimestamp OR @Timestamp IS NULL)

		-- VI 13430 - CCN178 - TSEQUAL and stored procs
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Concept WHERE Concept_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

		IF @RecordsAffected = 0 AND EXISTS (
			SELECT Concept_Key FROM Concept WHERE Concept_Key = @Key
		)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
		END

		-- Delete the Meaning record if only one Concept uses that Meaning key.
		IF @ConceptsSharingMeaningKeyCount = 1 
			DELETE 	Meaning
			WHERE	Meaning_Key = @MeaningKey

		IF @@Error <> 0 GOTO RollbackAndExit

		-- Delete the Term Version record if only one Concept uses that Term Version key.
		IF @ConceptsSharingTermVersionKeyCount = 1
			DELETE	Term_Version
			WHERE	Term_Version_Key = @TermVersionKey

		IF @@Error <> 0 GOTO RollbackAndExit

		-- Delete the Term record if only one Concept uses that Term key.
		IF @ConceptsSharingTermKeyCount = 1
			IF NOT EXISTS(SELECT * FROM Term_Version WHERE Term_Key = @TermKey)	
				DELETE	Term
				WHERE	Term_Key = @TermKey

		IF @@Error <> 0 GOTO RollbackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
	RAISERROR ('usp_Concept_Delete failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Concept_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concept_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concept_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Concept_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Concept_Delete TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptDesignation_Delete]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptDesignation_Delete]
GO

/*===========================================================================*\
  Description:	Delete a record from the Concept_Designation table.

  Parameters:	@Key		Concept Designation key.
		@Timestamp

  Created:	December 2003

  Last revision information:
    $Revision: 1 $
    $Date: 3/02/09 12:20 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptDesignation_Delete]
	@Key char(16),
	@Timestamp timestamp
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION

		DELETE	Concept_Designation
		WHERE	Concept_Designation_Key = @Key
		AND		[Timestamp] = @Timestamp

		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Concept_Designation WHERE Concept_Designation_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptDesignation_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptDesignation_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptDesignation_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptDesignation_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptDesignation_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptDesignation_Delete TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptDesignation_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptDesignation_Update]
GO

/*===========================================================================*\
  Description:	Updates a record in the Concept_Designation table

  Parameters:	@Key	Concept_Designation_Key

  Created:	December 2003

  Last revision information:
    $Revision: 1 $
    $Date: 3/02/09 12:20 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptDesignation_Update]
	@Key char(16),
	@ConceptKey char(16),
	@DesignationTypeConceptKey char(16),
	@FromVagueDateStart int,
	@FromVagueDateEnd int,
	@FromVagueDateType varchar(2) = NULL,
	@ToVagueDateStart int,
	@ToVagueDateEnd int,
	@ToVagueDateType varchar(2) = NULL,
	@SessionID char(16),
	@Timestamp timestamp
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	/*-------------------------------------------------------------*\
	  Wrap everything in one transaction.
	\*-------------------------------------------------------------*/
	BEGIN TRANSACTION

		UPDATE	Concept_Designation
		SET	
			Concept_Key = @ConceptKey,
			Designation_Type_Concept_Key = @DesignationTypeConceptKey,
			From_Vague_Date_Start = @FromVagueDateStart,
			From_Vague_Date_End = @FromVagueDateEnd,
			From_Vague_Date_Type = @FromVagueDateType,
			To_Vague_Date_Start = @ToVagueDateStart,
			To_Vague_Date_End = @ToVagueDateEnd,
			To_Vague_Date_Type = @ToVagueDateType,
			Changed_Session_ID = @SessionID			
		WHERE	Concept_Designation_Key = @Key
		AND		[Timestamp] = @Timestamp

		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Concept_Designation WHERE Concept_Designation_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

	/*-------------------------------------------------------------*\
	  All went well, so commit.
	\*-------------------------------------------------------------*/
	COMMIT TRANSACTION

RETURN 0

RollBackAndExit: 
	/*-------------------------------------------------------------*\
	  Cancel any changes, or left-overs might mess up some tables.
	\*-------------------------------------------------------------*/
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptDesignation_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptDesignation_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptDesignation_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptDesignation_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptDesignation_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptDesignation_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptDesignation_Update TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptGroup_Delete]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptGroup_Delete]
GO

/*===========================================================================*\
  Description:	Deletes a Concept_Group record.

  Parameters:	@Key	Concept_Group_key
		@Timestamp

  Created:	December 2003

  Last revision information:
    $Revision: 1 $
    $Date: 3/02/09 12:20 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptGroup_Delete]
	@Key char(16),
	@Timestamp timestamp = null,
	@SyncTaxonDict bit = 0
AS

SET NOCOUNT ON

	BEGIN TRANSACTION
		/*============================================================*\
		  See if the user wants any associated taxon dictionary
		  records be deleted with the Concept Group.
		\*============================================================*/
		IF @SyncTaxonDict = 1 
		BEGIN
			/*--------------------------------------------------------*\
			  Check that the Taxon_List table exists before
			  attempting any of this deletion. 
			\*--------------------------------------------------------*/
			IF EXISTS (SELECT *
		   			FROM   SysObjects 
					WHERE  Id = Object_Id(N'[dbo].[Taxon_List]')
					AND 	  Type = 'U')
			BEGIN
				DECLARE @TaxonListKey char(16)

				SELECT 	@TaxonListKey = Taxon_List_Key
				FROM	Taxon_Dictionary_Concept_Group_Mapping
				WHERE	Concept_Group_Key = @Key

				IF @@Error <> 0 GOTO RollbackAndExit

				DELETE	Taxon_List_Version
				WHERE	@TaxonListKey = Taxon_List_Key

				IF @@Error <> 0 GOTO RollbackAndExit

				DELETE	Taxon_Dictionary_Concept_Group_Mapping
				WHERE		Concept_Group_Key = @Key

				IF @@Error <> 0 GOTO RollbackAndExit

				DELETE	Taxon_List
				WHERE	Taxon_List_Key = @TaxonListKey

				IF @@Error <> 0 GOTO RollbackAndExit	
			END
		END
		ELSE
			DELETE	Taxon_Dictionary_Concept_Group_Mapping
			WHERE		Concept_Group_Key = @Key

		DELETE SF
		FROM Source_File SF
		INNER JOIN Source_Join SJ ON SJ.Source_Key=SF.Source_Key
		WHERE SJ.Table_Name='Concept_Group'
		AND SJ.Record_Key=@Key

		DELETE Source_Join
		WHERE Table_Name='Concept_Group'
		AND Record_Key=@Key
	
		DELETE 
		FROM 		Concept_Group
		WHERE		Concept_Group_Key = @Key
		AND			([Timestamp] = @Timestamp	OR (@Timestamp IS NULL))

		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Concept_Group WHERE Concept_Group_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptGroup_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptGroup_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptGroup_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptGroup_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptGroup_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptGroup_Delete TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptGroup_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptGroup_Update]
GO

/*===========================================================================*\
  Description:	Updates a record in the Concept_Group table.

  Parameters:	@Key 
		@URL
		@SessionID
		@ConceptGroupName 
		@Authority 
		@HierarchyRelationTypeKey 
		@LocalDomainKey
		@RecordsAffected
		@Timestamp 

  Created:	November 2003

  Last revision information:
	$Revision: 1 $
	$Date: 3/02/09 12:20 $
	$Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptGroup_Update]
	@Key char(16),
	@LocalDomainKey char(16),
	@ConceptGroupName varchar(100),
	@Authority varchar(100) = NULL,
	@URL varchar(255) = NULL,
	@HierarchyRelationTypeKey char(16) = NULL,
	@SessionID char(16),
	@Timestamp timestamp,
	@RecordsAffected int output
AS
	SET NOCOUNT OFF

	DECLARE	@hierarchy_changed BIT,
		@error INT	

	BEGIN TRANSACTION

		/* has hierarchical relation changed? */
		SELECT	@hierarchy_changed = 	CASE WHEN @HierarchyRelationTypeKey = Hierarchy_Relation_Type_Key
							THEN 0
							ELSE 1
						END
		FROM	Concept_Group
		WHERE	Concept_Group_Key = @Key

		UPDATE 	Concept_Group
		SET	Local_Domain_Key = @LocalDomainKey,
			Item_Name = @ConceptGroupName,
			Authority = @Authority,
			URL = @URL,
			Hierarchy_Relation_Type_Key = @HierarchyRelationTypeKey,
			Changed_Session_ID = @SessionID
		WHERE	Concept_Group_Key = @Key
		AND		[Timestamp] = @Timestamp

		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Concept_Group WHERE Concept_Group_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

		/* regenerate concept lineage if hierarchy changed */
		IF @hierarchy_changed = 1
		BEGIN
			EXECUTE	usp_ConceptLineage_GenerateForGroup @concept_group_key = @Key
			IF @@ERROR <> 0 GOTO RollbackAndExit
		END

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit:
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_ConceptGroup_Update failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptGroup_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptGroup_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptGroup_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptGroup_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptGroup_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptGroup_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptGroup_Update TO [Dev - JNCC SQL]
END

GO	

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptGroupVersion_Delete]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptGroupVersion_Delete]
GO

/*===========================================================================*\
  Description:	Deletes a Concept_Group_Version record.

  Parameters:	@Key	Concept_Group_Version key
		@Timestamp,
		@RecordsAffected

  Created:	November 2003

  Last revision information:
    $Revision: 1 $
    $Date: 3/02/09 12:20 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptGroupVersion_Delete]
	@Key char(16),
	@Timestamp timestamp = NULL,
	@SyncTaxonDict bit = 0,
	@RecordsAffected int = 1 OUTPUT
AS

SET NOCOUNT ON

	BEGIN TRANSACTION
		/*============================================================*\
		  See if the user wants any associated taxon dictionary
		  records be deleted with the Concept Group Version.
		\*============================================================*/
		IF @SyncTaxonDict = 1 
		BEGIN
			/*--------------------------------------------------------*\
			  Check that the Taxon_List_Version table exists before
			  attempting any of this deletion.
			\*--------------------------------------------------------*/
			IF EXISTS (SELECT *
		   			FROM   SysObjects 
					WHERE  Id = Object_Id(N'[dbo].[Taxon_List_Version]')
					AND 	  Type = 'U')
			BEGIN
				DECLARE		@Taxon_List_Version_Key char(16)

				SELECT		@Taxon_List_Version_Key = Taxon_List_Version_Key
				FROM		Taxon_Dictionary_Concept_Group_Version_Mapping
				WHERE		Concept_Group_Version_Key = @Key

				DELETE		Taxon_Dictionary_Concept_Group_Version_Mapping
				WHERE		Concept_Group_Version_Key = @Key

				/*-----------------------------------------------------------------*\
				  It is possible that this delete will fail. e.g. If the TLV record
				  is referred to in the Index_Taxon_Name or Taxon_List_Item tables, 
				  This will cause it to go to the RollbackAndExit method,
				  where the user can be asked if they want to replace the concept
				  with another (4.2.17.18)
				\*-----------------------------------------------------------------*/ 
				DELETE 		Taxon_List_Version
				WHERE		Taxon_List_Version_Key = @Taxon_List_Version_Key

				IF @@Error <> 0 GOTO RollbackAndExit
			END
		END

		DECLARE 
			@ConceptGroupKey CHAR(16),
			@Error INT

		SELECT @ConceptGroupKey=Concept_Group_Key
		FROM 	Concept_Group_Version
		WHERE	Concept_Group_Version_Key = @Key

		DELETE 
		FROM 	Concept_Group_Version
		WHERE	Concept_Group_Version_Key = @Key
		AND		([Timestamp] = @Timestamp	OR (@Timestamp IS NULL))

		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Concept_Group_Version WHERE Concept_Group_Version_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

		IF @RecordsAffected>0 
		BEGIN
			DECLARE @CurrentConceptKey CHAR(16)
			-- Update all the concepts to check if they are current
			DECLARE curConcepts CURSOR LOCAL FAST_FORWARD FOR
				SELECT	C.Concept_Key
				FROM	Concept AS C
				WHERE	Concept_Group_Key = @ConceptGroupKey
		
			OPEN curConcepts
		
			FETCH NEXT
			FROM	curConcepts
			INTO	@CurrentConceptKey
		
			WHILE @@Fetch_Status = 0
			BEGIN
				EXEC usp_Concept_UpdateIsCurrent @CurrentConceptKey, @ConceptGroupKey
				IF @@Error <> 0 GOTO RollbackAndExit
		
				FETCH NEXT
				FROM	curConcepts
				INTO	@CurrentConceptKey
			END
			CLOSE	curConcepts
			DEALLOCATE curConcepts
		END
	
	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptGroupVersion_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptGroupVersion_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptGroupVersion_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptGroupVersion_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptGroupVersion_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptGroupVersion_Delete TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptGroupVersion_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptGroupVersion_Update]
GO

/*===========================================================================*\
  Description:	Updates a record in the Concept_Group_Version table.

  Parameters:	@Key (output),	
		@ConceptGroupKey 
		@Version 
		@FromVagueDateStart (optional)
		@FromVagueDateEnd (optional)
		@FromVagueDateType (optional)
		@ToVagueDateStart (optional)
		@ToVagueDateEnd (optional)
		@ToVagueDateType (optional)
		@AcqVagueDateStart (optional)
		@AcqVagueDateEnd (optional)
		@AcqVagueDateType (optional)
		@URL (optional)
		@SessionID
		@SystemSuppliedData bit (optional),
		@Timestamp

  Created:	November 2003

  Last revision information:
    $Revision: 1 $
    $Date: 3/02/09 12:20 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptGroupVersion_Update]
	@Key char(16) OUTPUT,	
	@ConceptGroupKey char(16),
	@Version varchar(100),
	@FromVagueDateStart int = NULL,
	@FromVagueDateEnd int = NULL,
	@FromVagueDateType varchar(2) = NULL,
	@ToVagueDateStart int = NULL,
	@ToVagueDateEnd int = NULL,
	@ToVagueDateType varchar(2) = NULL,	
	@AcqVagueDateStart int = NULL,
	@AcqVagueDateEnd int = NULL,
	@AcqVagueDateType varchar(2) = NULL,
	@URL varchar(255) = NULL,
	@SessionID char(16),
	@Timestamp timestamp	
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	DECLARE @SequenceNumber int

	BEGIN TRANSACTION

		UPDATE 	Concept_Group_Version
		SET 	Concept_Group_Key = @ConceptGroupKey, 
			Version = @Version, 
			From_Vague_Date_Start = @FromVagueDateStart, 
			From_Vague_Date_End = @FromVagueDateEnd, 
			From_Vague_Date_Type = IsNull(@FromVagueDateType, 'U'), 
			To_Vague_Date_Start = @ToVagueDateStart, 
			To_Vague_Date_End = @ToVagueDateEnd, 
			To_Vague_Date_Type = @ToVagueDateType, 
			Acq_Vague_Date_Start = @AcqVagueDateStart, 
			Acq_Vague_Date_End = @AcqVagueDateEnd, 
			Acq_Vague_Date_Type = IsNull(@AcqVagueDateType, 'U'),
			URL = @URL, 
			Changed_Session_ID = @SessionID

		WHERE	Concept_Group_Version_Key = @Key
		AND		[Timestamp] = @Timestamp

		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Concept_Group_Version WHERE Concept_Group_Version_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptGroupVersion_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptGroupVersion_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptGroupVersion_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptGroupVersion_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptGroupVersion_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptGroupVersion_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptGroupVersion_Update TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptHistory_Delete]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptHistory_Delete]
GO

/*===========================================================================*\
  Description:	Delete a record from the Concept_History table.

  Parameters:	@Key
		@Timestamp,
		@RecordsAffected

  Created:	December 2003

  Last revision information:
    $Revision: 1 $
    $Date: 3/02/09 12:20 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptHistory_Delete]
	@Key char(16),
	@Timestamp timestamp,
	@RecordsAffected INT OUTPUT
AS
	SET NOCOUNT OFF

	BEGIN TRANSACTION

		DECLARE @ConceptGroupKey CHAR(16)
		DECLARE @ConceptKey CHAR(16)

		-- Find the concept group key, because we need this to check if the concept 
		-- remains current
		SELECT @ConceptGroupKey=CGV.Concept_Group_Key, @ConceptKey=CH.Concept_Key
		FROM Concept_History CH
		INNER JOIN Concept_Group_Version CGV ON CGV.Concept_Group_Version_Key=CH.Concept_Group_Version_To
		WHERE CH.Concept_History_Key=@Key

		-- Delete record from Concept_History table.
		DELETE	Concept_History
		WHERE	Concept_History_Key = @Key
		AND		[Timestamp] = @Timestamp

		DECLARE @Error int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Concept_History WHERE Concept_History_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END
		
		IF @RecordsAffected>0
		BEGIN
			--Ensure Concept's current status is set correctly.
			EXEC usp_Concept_UpdateIsCurrent @ConceptKey, @ConceptGroupKey
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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptHistory_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptHistory_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptHistory_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptHistory_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptHistory_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptHistory_Delete TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptHistory_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptHistory_Update]
GO

/*===========================================================================*\
  Description:	Updates a record in the Concept_History table

  Parameters:	@Key
		@ConceptKey 
		@ConceptGroupVersionFromKey
		@ConceptGroupVersionToKey 
		@FromVagueDateStart
		@FromVagueDateEnd 
		@FromVagueDateType
		@ToVagueDateStart 
		@ToVagueDateEnd
		@ToVagueDateType
		@SessionID 
		@Timestamp 

  Created:	December 2003

  Last revision information:
    $Revision: 1 $
    $Date: 3/02/09 12:20 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptHistory_Update]
	@Key char(16),
	@ConceptKey char(16),
	@ConceptGroupVersionFromKey char(16),
	@ConceptGroupVersionToKey char(16),
	@FromVagueDateStart int,
	@FromVagueDateEnd int,
	@FromVagueDateType varchar(2),
	@ToVagueDateStart int,
	@ToVagueDateEnd int,
	@ToVagueDateType varchar(2),
	@SessionID char(16),
	@Timestamp timestamp
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION
		
		UPDATE 	Concept_History
		SET 	Concept_Key = @ConceptKey,
				Concept_Group_Version_From = @ConceptGroupVersionFromKey,
				Concept_Group_Version_To = @ConceptGroupVersionToKey,
				From_Vague_Date_Start = @FromVagueDateStart,
				From_Vague_Date_End = @FromVagueDateEnd,
				From_Vague_Date_Type = @FromVagueDateType,
				To_Vague_Date_Start = @ToVagueDateStart,
				To_Vague_Date_End = @ToVagueDateEnd,
				To_Vague_Date_Type = @ToVagueDateType,
				Changed_Session_ID = @SessionID
		WHERE	Concept_History_Key = @Key
		AND		[Timestamp] = @Timestamp

		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Concept_History WHERE Concept_History_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

		--Ensure Concept's current status is set correctly.
		EXEC usp_Concept_UpdateIsCurrent @ConceptKey
		IF @@Error <> 0 GOTO RollbackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptHistory_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptHistory_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptHistory_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptHistory_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptHistory_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptHistory_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptHistory_Update TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptRank_Delete]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptRank_Delete]
GO

/*===========================================================================*\
  Description:	Delete a record from the Concept_Rank table.

  Parameters:	@Key
		@Timestamp

  Created:	January 2004

  Last revision information:
    $Revision: 1 $
    $Date: 3/02/09 12:20 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptRank_Delete]
	@Key char(16),
	@Timestamp timestamp
AS
	SET NOCOUNT OFF

	BEGIN TRANSACTION

		-- Delete record from Concept_Rank table.
		DELETE	Concept_Rank
		WHERE	Concept_Rank_Key = @Key
		AND		[Timestamp] = @Timestamp
	
		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Concept_Rank WHERE Concept_Rank_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptRank_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptRank_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptRank_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptRank_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptRank_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptRank_Delete TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptRank_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptRank_Update]
GO

/*===========================================================================*\
  Description:	Updates a record in the Concept_Rank table

  Parameters:	@Key 
		@DomainKey 
		@ItemName 
		@SortOrder 
		@Abbreviation 
		@ColorR
		@ColorG 
		@ColorB 
		@SessionID 
		@Timestamp 

  Created:	January 2004

  Last revision information:
    $Revision: 1 $
    $Date: 3/02/09 12:20 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptRank_Update]
	@Key char(16),
	@DomainKey char(16),
	@ItemName varchar(100),
	@SortOrder int = NULL,
	@Abbreviation varchar(10),
	@ColorR tinyint,
	@ColorG tinyint,
	@ColorB tinyint,
	@SessionID char(16),
	@Timestamp timestamp
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION
		
		UPDATE 	Concept_Rank
		SET 	Domain_Key = @DomainKey,
				Item_Name = @ItemName,
				Sort_Order = @SortOrder,
				Abbreviation = @Abbreviation,
				Color_R = @ColorR,
				Color_G = @ColorG,
				Color_B = @ColorB,
				Changed_Session_ID = @SessionID			
		WHERE	Concept_Rank_Key = @Key
		AND		[Timestamp] = @Timestamp

		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Concept_Rank WHERE Concept_Rank_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptRank_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptRank_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptRank_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptRank_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptRank_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptRank_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptRank_Update TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptRelation_Delete]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptRelation_Delete]
GO

/*===========================================================================*\
  Description:	Deletes a record from the ConceptRelation table.

  Parameters:	@Key	Concept_Relation_Key
		@Timestamp

  Created:	December 2003

  Last revision information: 
    $Revision: 1 $
    $Date: 3/02/09 12:20 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptRelation_Delete]
	@Key char(16),
	@Timestamp timestamp
AS

SET NOCOUNT ON

	/*-------------------------------------------------------------*\
	  Wrap everything in one transaction.
	\*-------------------------------------------------------------*/
	BEGIN TRANSACTION

		/*-------------------------------------------------------------*\
		  Remove Concept_Lineage records.
		\*-------------------------------------------------------------*/
		DECLARE		@from_concept_key	CHAR(16),
					@to_concept_key		CHAR(16),
					@relation_type_key	CHAR(16),
					@error				INT,
					@RecordsAffected	INT

		SELECT		@from_concept_key		=	From_Concept_Key,
					@to_concept_key			=	To_Concept_Key,
					@relation_type_key		=	Thesaurus_Relation_Type_Key
		FROM		Concept_Relation
		WHERE		Concept_Relation_Key	=	@Key

		IF @@ROWCOUNT > 0
		BEGIN
			EXECUTE		usp_ConceptLineage_RelationDeleted	@from_concept_key,
															@to_concept_key,
															@relation_type_key
			IF @@ERROR <> 0 GOTO RollbackAndExit
		END

		/*-------------------------------------------------------------*\
		  Update in Concept_Relation.
		\*-------------------------------------------------------------*/
		DELETE 	Concept_Relation
		WHERE	Concept_Relation_Key = @Key	
		AND	@Timestamp = Timestamp
		
		-- VI 13430 - CCN178 - TSEQUAL and stored procs
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Concept_Relation WHERE Concept_Relation_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

		IF @RecordsAffected = 0 AND EXISTS (
			SELECT Concept_Relation_Key FROM Concept_Relation WHERE Concept_Relation_Key = @Key
		)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
		END

	/*-------------------------------------------------------------*\
	  All went well, so commit.
	\*-------------------------------------------------------------*/
	COMMIT TRANSACTION

RETURN 0

RollBackAndExit: 
	/*-------------------------------------------------------------*\
	  Cancel any changes, or left-overs might mess up some tables.
	\*-------------------------------------------------------------*/
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_ConceptRelation_Delete failed', 16, 1)
GO	

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptRelation_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptRelation_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptRelation_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptRelation_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptRelation_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptRelation_Delete TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptRelation_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptRelation_Update]
GO

/*===========================================================================*\
  Description:	Updates a record in the Concept_Relation table.

  Parameters:	@Key
		@FromConceptKey
		@ToConceptKey
		@ThesaurusRelationTypeKey
		@Multiplicity
		@Inherited
		@Comment
		@SessionID
		@RecordsAffected
		@Timestamp

  Created:	December 2003

  Last revision information:
    $Revision: 1 $
    $Date: 3/02/09 12:20 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptRelation_Update]
	@Key char(16),
	@FromConceptKey char(16),
	@ToConceptKey char(16),
	@ThesaurusRelationTypeKey char(16),
	@Multiplicity float = NULL,
	@Inherited bit = NULL,
	@Comment text = NULL,
	@SessionID char(16),
	@RecordsAffected INT OUTPUT,
	@Timestamp timestamp
AS
	SET NOCOUNT ON

	/*-------------------------------------------------------------*\
	  Wrap everything in one transaction.
	\*-------------------------------------------------------------*/
	BEGIN TRANSACTION

		/*-------------------------------------------------------------*\
		  Update record in Concept_Relation.
		\*-------------------------------------------------------------*/
		DECLARE		@old_from_concept_key		CHAR(16),
					@old_to_concept_key			CHAR(16),
					@old_relation_type_key		CHAR(16),
					@error						INT

		UPDATE	Concept_Relation
		SET @old_from_concept_key = From_Concept_Key,
			From_Concept_Key = @FromConceptKey,
			@old_to_concept_key = To_Concept_Key,
			To_Concept_Key = @ToConceptKey,
			@old_relation_type_key = Thesaurus_Relation_Type_Key,
			Thesaurus_Relation_Type_Key = @ThesaurusRelationTypeKey,
			Multiplicity = @Multiplicity,
			Inherited = IsNull(@Inherited, 0),
			Comment = @Comment,
			Changed_Session_ID = @SessionID
		WHERE	Concept_Relation_Key = @Key
		AND		[Timestamp] = @Timestamp

		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Concept_Relation WHERE Concept_Relation_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

		/*-------------------------------------------------------------*\
		  Make corresponding changes in Concept_Lineage
		\*-------------------------------------------------------------*/
		EXECUTE		usp_ConceptLineage_UpdateRelation	@Key,
														@old_from_concept_key,
														@old_to_concept_key,
														@old_relation_type_key
		IF @@ERROR <> 0 GOTO RollbackAndExit

	/*-------------------------------------------------------------*\
	  All went well, so commit.
	\*-------------------------------------------------------------*/
	COMMIT TRANSACTION

RETURN 0

RollBackAndExit: 
	/*-------------------------------------------------------------*\
	  Cancel any changes, or left-overs might mess up some tables.
	\*-------------------------------------------------------------*/
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_ConceptRelation_Update failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptRelation_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptRelation_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptRelation_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptRelation_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptRelation_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptRelation_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptRelation_Update TO [Dev - JNCC SQL]
END

GO
			
/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConditionCheck_Delete]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConditionCheck_Delete]
GO

/*===========================================================================*\
  Description:	Delete a record from the Conservation_Check table.

  Parameters:	@Key
		@Timestamp

  Created:	December 2003

  Last revision information:
    $Revision: 1 $
    $Date: 3/02/09 12:20 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConditionCheck_Delete]
	@Key char(16),
	@Timestamp timestamp
AS
	SET NOCOUNT OFF

	BEGIN TRANSACTION

		DELETE	Collection_Unit_Check
		WHERE	Conservation_Check_Key = @Key

		DELETE	Movement_Conservation_Check
		WHERE	Conservation_Check_Key = @Key

		-- Delete record from Conservation_Check table.
		DELETE	Conservation_Check
		WHERE	Conservation_Check_Key = @Key
		AND		[Timestamp] = @Timestamp
		
		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Conservation_Check WHERE Conservation_Check_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConditionCheck_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConditionCheck_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConditionCheck_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConditionCheck_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConditionCheck_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConditionCheck_Delete TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConditionCheck_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConditionCheck_Update]
GO

/*===========================================================================*\
  Description:	Updates a record into the Conservation_Check table

  Parameters:	@Key
		@TypeConceptKey
		@RefNumber
		@VagueDateStart
		@VagueDateEnd
		@VagueDateType
		@CheckedByNameKey
		@ConditionConceptKey
		@AppliesToContainedSpecimens
		@Details
		@SessionID
		@Timestamp

  Created:	September 2003

  Last revision information:
    $Revision: 1 $
    $Date: 3/02/09 12:20 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConditionCheck_Update]
	@Key char(16),
	@TypeConceptKey char(16),
	@RefNumber varchar(20),
	@VagueDateStart int,
	@VagueDateEnd int,
	@VagueDateType varchar(2),
	@CheckedByNameKey char(16),
	@ConditionConceptKey char(16),
	@AppliesToContainedSpecimens bit,
	@Details text,
	@SessionID char(16),
	@Timestamp timestamp
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION
		
		UPDATE 	Conservation_Check
		SET 	Type_Concept_Key = @TypeConceptKey,
			Ref_Number = @RefNumber,
			Vague_Date_Start = @VagueDateStart,
			Vague_Date_End = @VagueDateEnd,
			Checked_By_Name_Key = @CheckedByNameKey,
			Condition_Concept_Key = @ConditionConceptKey,
			Applies_To_Contained_Specimens = @AppliesToContainedSpecimens,
			Details = @Details,
			Changed_Session_ID = @SessionID

		WHERE	Conservation_Check_Key = @Key
		AND		[Timestamp] = @Timestamp

		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Conservation_Check WHERE Conservation_Check_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConditionCheck_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConditionCheck_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConditionCheck_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConditionCheck_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConditionCheck_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConditionCheck_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConditionCheck_Update TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConservationJob_Delete]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConservationJob_Delete]
GO

/*===========================================================================*\
  Description:	Delete a record from the Conservation_Job table.

  Parameters:	@Key
		@Timestamp

  Created:	December 2003

  Last revision information:
    $Revision: 1 $
    $Date: 3/02/09 12:20 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConservationJob_Delete]
	@Key char(16),
	@Timestamp timestamp
AS
	SET NOCOUNT OFF

	BEGIN TRANSACTION

		-- Delete record from Conservation_Job table.
		DELETE	Conservation_Job
		WHERE	Conservation_Job_Key = @Key
		AND		[Timestamp] = @Timestamp
	
		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Conservation_Job WHERE Conservation_Job_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConservationJob_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConservationJob_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConservationJob_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConservationJob_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConservationJob_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConservationJob_Delete TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConservationJob_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConservationJob_Update]
GO

/*===========================================================================*\
  Description:	Updates a record into the Conservation Job table

  Parameters:	@Key

		@Timestamp

  Created:	September 2003

  Last revision information:
    $Revision: 1 $
    $Date: 3/02/09 12:20 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConservationJob_Update]
	@Key CHAR(16),
	@ItemName VARCHAR(100),
	@FromVagueDateStart INT,
	@FromVagueDateEnd INT,
	@FromVagueDateType VARCHAR(2) = NULL,
	@ToVagueDateStart INT,
	@ToVagueDateEnd INT,
	@ToVagueDateType VARCHAR(2),
	@Duration FLOAT,
	@DurationType CHAR(16),
	@Status TINYINT,
	@CostAmount MONEY,
	@Currency CHAR(16),
	@Details TEXT,
	@SessionID CHAR(16),
	@Timestamp timestamp,
	@RecordsAffected int output

AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION
	
		DECLARE	@Error int
		
		UPDATE 	Conservation_Job
		SET 	Item_Name = @ItemName,
			From_Vague_Date_Start = @FromVagueDateStart,
			From_Vague_Date_End = @FromVagueDateEnd,
			From_Vague_Date_Type = IsNull(@FromVagueDateType, 'U'),
			To_Vague_Date_Start = @ToVagueDateStart,
			To_Vague_Date_End = @ToVagueDateEnd,
			To_Vague_Date_Type = @ToVagueDateType,
			Duration = @Duration,
			Duration_Unit_Concept_Key = @DurationType,
			Status = @Status,
			Cost = @CostAmount,
			Currency_Concept_Key = @Currency,
			Details = @Details,
			Changed_Session_ID = @SessionID
		WHERE	Conservation_Job_Key = @Key
		AND		[Timestamp] = @Timestamp

		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Conservation_Job WHERE Conservation_Job_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConservationJob_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConservationJob_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConservationJob_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConservationJob_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConservationJob_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConservationJob_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConservationJob_Update TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConservationJobMaterial_Delete]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConservationJobMaterial_Delete]
GO

/*===========================================================================*\
  Description:	Delete a record from the Conservation_Job_Material table.

  Parameters:	@Key		Conservation_Job_Material_key.
		@Timestamp

  Created:	December 2003

  Last revision information:
    $Revision: 1 $
    $Date: 3/02/09 12:20 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConservationJobMaterial_Delete]
	@Key char(16),
	@Timestamp timestamp
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION

		DELETE	Conservation_Job_Material
		WHERE	Conservation_Job_Material_Key = @Key
		AND		[Timestamp] = @Timestamp

		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Conservation_Job_Material WHERE Conservation_Job_Material_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConservationJobMaterial_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConservationJobMaterial_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConservationJobMaterial_Delete TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConservationJobMaterial_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConservationJobMaterial_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConservationJobMaterial_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConservationJobMaterial_Delete TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConservationJobMaterial_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConservationJobMaterial_Update]
GO

/*===========================================================================*\
  Description:	Update a record in the Co table.

  Parameters:	@Key		Conservation_Job_Material key.
		@MaterialKey
		@Quantity
		@UnitKey
		@SessionID
		@Timestamp

  Created:	December 2003

  Last revision information:
    $Revision: 1 $
    $Date: 3/02/09 12:20 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConservationJobMaterial_Update]
	@Key char(16),
	@MaterialKey char(16),
	@Quantity varchar(20),
	@UnitKey char(16),
	@SessionID char(16),
	@Timestamp timestamp
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION

		UPDATE	Conservation_Job_Material
		SET	Material_Concept_Key = @MaterialKey,
			Quantity = @Quantity,
			Unit_Concept_Key = @UnitKey,
			Changed_Session_ID = @SessionID
		WHERE	Conservation_Job_Material_Key = @Key
		AND		[Timestamp] = @Timestamp

		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Conservation_Job_Material WHERE Conservation_Job_Material_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConservationJobMaterial_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConservationJobMaterial_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConservationJobMaterial_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConservationJobMaterial_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConservationJobMaterial_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConservationJobMaterial_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConservationJobMaterial_Update TO [Dev - JNCC SQL]
END
GO

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
    $Revision: 1 $
    $Date: 3/02/09 12:20 $
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
		@IsForSpecimen		Indicates whether to update preferred 
					flag in Specimen_Unit or Determination.
		@RecordsAffected	OUTPUT Can't rely on correct value to come out,
					so use a parameter instead.

  Created:	July 2003

  Last revision information:
    $Revision: 1 $
    $Date: 3/02/09 12:20 $
    $Author: Pauldavies $

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
			Changed_Session_ID = @SessionID
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
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Domain_Delete]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Domain_Delete]
GO

/*===========================================================================*\
  Description:	Deletes a Domain record.

  Parameters:	@Key	Domain_Key
				@Timestamp

  Created:	December 2003

  Last revision information:
    $Revision: 1 $
    $Date: 3/02/09 12:20 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Domain_Delete]
	@Key char(16),
	@Timestamp timestamp = NULL
AS

SET NOCOUNT ON

	BEGIN TRANSACTION
	
		DELETE 
		FROM 	Domain
		WHERE	Domain_Key = @Key
		AND		([Timestamp] = @Timestamp OR (@Timestamp IS NULL))

		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Domain WHERE Domain_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Domain_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Domain_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Domain_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Domain_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Domain_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Domain_Delete TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Domain_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Domain_Update]
GO

/*===========================================================================*\
  Description:	Updates a record in the Domain table

  Parameters:	@Key 
		@ItemName
		@SubjectAreaKey
		@HasOccurrences 
		@DefaultHierarchyRelationTypeKey 
		@DomainMask
		@SessionID
		@Timestamp

  Created:	November 2003

  Last revision information:
    $Revision: 1 $
    $Date: 3/02/09 12:20 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Domain_Update]
	@Key char(16),
	@ItemName varchar(100),
	@SubjectAreaKey char(16),
	@HasOccurrences bit,
	@DefaultHierarchyRelationTypeKey char(16),
	@DomainMask int,
	@SessionID char(16),
	@Timestamp timestamp
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION
		
		UPDATE 	Domain
		SET 	Item_Name = @ItemName, 
			Subject_Area_Key = @SubjectAreaKey,
			Has_Occurrences = @HasOccurrences,
			Default_Hierarchy_Relation_Type_Key = @DefaultHierarchyRelationTypeKey,
			Domain_Mask = @DomainMask,
			Changed_Session_ID = @SessionID
		WHERE	Domain_Key = @Key
		AND		[Timestamp] = @Timestamp

		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Domain WHERE Domain_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Domain_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Domain_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Domain_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Domain_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Domain_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Domain_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Domain_Update TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_DomainHyperlink_Delete]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_DomainHyperlink_Delete]
GO

/*===========================================================================*\
  Description:	Delete a record from the Domain_Hyperlink table.

  Parameters:	@Key
		@Timestamp

  Created:	January 2004

  Last revision information:
    $Revision: 1 $
    $Date: 3/02/09 12:20 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_DomainHyperlink_Delete]
	@Key char(16),
	@Timestamp timestamp
AS
	SET NOCOUNT OFF

	BEGIN TRANSACTION

		-- Delete record from Domain_Hyperlink table.
		DELETE	Domain_Hyperlink
		WHERE	Domain_Hyperlink_Key = @Key
		AND		[Timestamp] = @Timestamp
	
		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Domain_Hyperlink WHERE Domain_Hyperlink_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_DomainHyperlink_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_DomainHyperlink_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_DomainHyperlink_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_DomainHyperlink_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_DomainHyperlink_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_DomainHyperlink_Delete TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_DomainHyperlink_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_DomainHyperlink_Update]
GO

/*===========================================================================*\
  Description:	Updates a record in the Domain_Hyperlink table.

  Parameters:	@Key 
		@ItemName 
		@ImageFile 
		@URL 
		@UseConceptKey 
		@WordSeparator 
		@LocalDomainKey 
		@SessionID 
		@Timestamp 

  Created:	January 2004

  Last revision information:
    $Revision: 1 $
    $Date: 3/02/09 12:20 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_DomainHyperlink_Update]
	@Key char(16),
	@ItemName varchar(100), 
	@ImageFile varchar(255),
	@URL varchar(255),
	@UseConceptKey bit, 
	@WordSeparator varchar(5),
	@LocalDomainKey char(16),
	@SessionID char(16), 
	@Timestamp timestamp
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION
		
		UPDATE 	Domain_Hyperlink
		SET 	Item_Name = @ItemName,
			Image_File = @ImageFile,
			URL = @URL,
			Use_Concept_Key = @UseConceptKey,
			Word_Separator = @WordSeparator,
			Local_Domain_Key = @LocalDomainKey,
			Changed_Session_ID = @SessionID
		WHERE	Domain_Hyperlink_Key = @Key
		AND		[Timestamp] = @Timestamp

		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Domain_Hyperlink WHERE Domain_Hyperlink_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_DomainHyperlink_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_DomainHyperlink_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_DomainHyperlink_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_DomainHyperlink_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_DomainHyperlink_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_DomainHyperlink_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_DomainHyperlink_Update TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Enquiry_Delete]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Enquiry_Delete]
GO

/*===========================================================================*\
  Description:	Delete a record from the Enquiry table.

  Parameters:	@Key		Enquiry key.
		@Timestamp

  Created:	December 2003

  Last revision information:
    $Revision: 1 $
    $Date: 3/02/09 12:20 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Enquiry_Delete]
	@Key char(16),
	@Timestamp timestamp
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION

		DELETE	Enquiry
		WHERE	Enquiry_Key = @Key
		AND		[Timestamp] = @Timestamp

		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Enquiry WHERE Enquiry_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Enquiry_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Enquiry_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Enquiry_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Enquiry_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Enquiry_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Enquiry_Delete TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Enquiry_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Enquiry_Update]
GO

/*===========================================================================*\
  Description:	Updates a record in the Enquiry table

  Parameters:	@Key
		@EnquirerNameKey 
		@VagueEnquirer
		@EnquiryTypeConceptKey
		@EnquiryMethodConceptKey
		@VagueDateStart
		@VagueDateEnd
		@VagueDateType
		@MaterialLeft
		@ObservationPlanned
		@Description
		@AnsweredByNameKey
		@Answered
		@AnsweredVagueDateStart
		@AnsweredVagueDateEnd
		@AnsweredVagueDateType
		@SessionID
		@Timestamp

  Created:	October 2003

  Last revision information:
    $Revision: 1 $
    $Date: 3/02/09 12:20 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Enquiry_Update]
	@Key char(16) OUTPUT,
	@EnquirerNameKey char(16),
	@VagueEnquirer varchar(100),
	@EnquiryTypeConceptKey char(16),
	@EnquiryMethodConceptKey char(16),
	@VagueDateStart int,
	@VagueDateEnd int,
	@VagueDateType varchar(2),
	@MaterialLeft bit,
	@ObservationPlanned bit,
	@Description text,
	@AnsweredByNameKey char(16),
	@Answered bit,
	@AnsweredVagueDateStart int,
	@AnsweredVagueDateEnd int,
	@AnsweredVagueDateType varchar(2),
	@SessionID char(16),
	@Timestamp timestamp
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION

		/*-------------------------------------------------------------*\
		  Update in Enquiry.
		\*-------------------------------------------------------------*/
		UPDATE 	Enquiry
		SET	Enquirer_Name_Key = @EnquirerNameKey, 
			Vague_Enquirer = @VagueEnquirer, 
			Enquiry_Type_Concept_Key = @EnquiryTypeConceptKey,
			Enquiry_Method_Concept_Key = @EnquiryMethodConceptKey, 
			Vague_Date_Start = @VagueDateStart, 
			Vague_Date_End = @VagueDateEnd, 
			Vague_Date_Type = @VagueDateType,
			Material_Left = @MaterialLeft,
			Observation_Planned = @ObservationPlanned, 
			Description = @Description, 
			Answered_By_Name_Key = @AnsweredByNameKey,
			Answered = @Answered, 
			Answered_Vague_Date_Start = @AnsweredVagueDateStart, 
			Answered_Vague_Date_End = @AnsweredVagueDateEnd, 
			Answered_Vague_Date_Type = @AnsweredVagueDateType,
			Entered_Session_ID = @SessionID
		WHERE	Enquiry_Key = @Key
		AND		[Timestamp] = @Timestamp

		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Enquiry WHERE Enquiry_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Enquiry_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Enquiry_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Enquiry_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Enquiry_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Enquiry_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Enquiry_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Enquiry_Update TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_FieldData_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_FieldData_Update]
GO

/*===========================================================================*\
  Description:	Updates tables when information in the field data frame is
		changed.

  Parameters:	@Key 
		@SurveyEventKey 
	        @SurveyKey 
	        @LocationKey 
		@LocationName
		@SampleKey 
	        @SpatialRefQualifier 
	        @SpatialRef 
	        @GatheringMethod 
	        @VagueDateStart 
	        @VagueDateEnd 
	        @VagueDateType
		@CollectionUnitKey 
		@OccurrenceKey 
		@TaxonOccurrenceKey 
		@InferredSurvey 
		@InferredLocation 
		@InferredSpatialRef 
		@InferredSampleType 
		@InferredDate 
		@InferredCollectors 
		@GatheringEvent
		@SessionID 
		@Timestamp

  Created:	October 2003

  Last revision information:
    $Revision: 1 $
    $Date: 3/02/09 12:20 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_FieldData_Update]
	@Key char(16),
	@SurveyEventKey char(16) = NULL,
	@SurveyKey char(16),
	@LocationKey char(16),
	@LocationName varchar(100) = NULL,
	@SampleKey char(16),
	@SpatialRefQualifier varchar(20),
	@SpatialRef varchar(40),
	@GatheringMethod char(16),
	@VagueDateStart int,
	@VagueDateEnd int,
	@VagueDateType varchar(2) = NULL,
	@CollectionUnitKey char(16),
	@OccurrenceKey char(16),
	@TaxonOccurrenceKey char(16),
	@InferredSurvey tinyint,
	@InferredLocation tinyint,
	@InferredSpatialRef tinyint,
	@InferredSampleType tinyint,
	@InferredDate tinyint,
	@InferredCollectors tinyint,
	@GatheringEvent bit,
	@SessionID char(16),
	@Timestamp timestamp
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION
		
		UPDATE 	[Sample]
		SET 	Location_Key = @LocationKey,
			Vague_Date_Start = @VagueDateStart, 
			Vague_Date_End = @VagueDateEnd, 
			Vague_Date_Type = IsNull(@VagueDateType, 'U'),
			Spatial_Ref_Qualifier = @SpatialRefQualifier,
			Spatial_Ref = @SpatialRef,
			Sample_Type_Key = @GatheringMethod,
			Location_Name = @LocationName
		WHERE	Sample_Key = @SampleKey

		IF @@Error <> 0 GOTO RollbackAndExit

		-- Update the Survey Event table so that is points to the
		-- correct Survey record.

		IF @SurveyEventKey = NULL 
			SET @SurveyEventKey = (SELECT	Survey_Event_Key
						FROM	[Sample]
						WHERE	Sample_Key = @SampleKey)
		UPDATE	Survey_Event
		SET	Survey_Key = @SurveyKey
		WHERE	Survey_Event_Key = @SurveyEventKey

		IF @@Error <> 0 GOTO RollbackAndExit

		UPDATE 	Specimen_Field_Data
		SET	Gathering_Event = @GatheringEvent,
			Collection_Unit_Key = @CollectionUnitKey,
			Occurrence_Key = @OccurrenceKey,
			Taxon_Occurrence_Key = @TaxonOccurrenceKey,
			Inferred_Survey = @InferredSurvey,
			Inferred_Location = @InferredLocation,
			Inferred_Spatial_Ref = @InferredSpatialRef,
			Inferred_Sample_Type = @InferredSampleType,
			Inferred_Date = @InferredDate,
			Inferred_Collectors = @InferredCollectors			
		WHERE	Specimen_Field_Data_Key = @Key
		AND		[Timestamp] = @Timestamp
		
		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Specimen_Field_Data WHERE Specimen_Field_Data_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_FieldData_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_FieldData_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_FieldData_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_FieldData_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_FieldData_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_FieldData_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_FieldData_Update TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Funding_Delete]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Funding_Delete]
GO

/*===========================================================================*\
  Description:	Delete a record from either the Conservation_Job_Funding table
		or the Movement_Funding table.

  Parameters:	@Key
		@IsMovement
		@Timestamp

  Created:	December 2003

  Last revision information:
    $Revision: 1 $
    $Date: 3/02/09 12:20 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Funding_Delete]
	@Key char(16),
	@IsMovement bit,
	@Timestamp timestamp
AS
	SET NOCOUNT OFF

	BEGIN TRANSACTION
		
		DECLARE @Error int
		DECLARE @RecordsAffected int

		IF @IsMovement = 1 
		BEGIN
			-- Delete record from Movement_Funding table.
			DELETE	Movement_Funding
			WHERE	Movement_Funding_Key = @Key
			AND		[Timestamp] = @Timestamp

			SELECT @Error = @@Error, @RecordsAffected = @@Rowcount
		
			IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Movement_Funding WHERE Movement_Funding_Key = @Key)
			BEGIN
				RAISERROR('Record updated by another user', 16, 1)
				GOTO RollbackAndExit
			END
		END ELSE BEGIN
			-- Delete record from Conservation_Job_Funding table.
			DELETE	Conservation_Job_Funding
			WHERE	Conservation_Job_Funding_Key = @Key
			AND		[Timestamp] = @Timestamp

			SELECT @Error = @@Error, @RecordsAffected = @@Rowcount
		
			IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Conservation_Job_Funding WHERE Conservation_Job_Funding_Key = @Key)
			BEGIN
				RAISERROR('Record updated by another user', 16, 1)
				GOTO RollbackAndExit
			END
		END
	
		IF @Error <> 0 GOTO RollbackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Funding_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Funding_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Funding_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Funding_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Funding_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Funding_Delete TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Funding_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Funding_Update]
GO

/*===========================================================================*\
  Description:	Updates a record in the correct Funding table

  Parameters:	@Key 
		@ParentKey 
		@FundedByNameKey 
		@VagueDateStart 
		@VagueDateEnd 
		@VagueDateType 
		@Amount 
		@CurrencyConceptKey 
		@Details 
		@SessionID 
		@Timestamp 
		@IsMovement 

  Created:	September 2003

  Last revision information:
    $Revision: 1 $
    $Date: 3/02/09 12:20 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Funding_Update]
	@Key CHAR(16),
	@ParentKey CHAR(16),
	@FundedByNameKey CHAR(16),
	@VagueDateStart INT,
	@VagueDateEnd INT,
	@VagueDateType VARCHAR(2),
	@Amount MONEY,
	@CurrencyConceptKey CHAR(16),
	@Details TEXT,
	@SessionID CHAR(16),
	@Timestamp timestamp,
	@IsMovement BIT
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION
		
		DECLARE @Error int
		DECLARE @RecordsAffected int

		IF @IsMovement = 1
		BEGIN
			UPDATE 	Movement_Funding
			SET 	Movement_Key = @ParentKey,
				Funded_By_Name_Key = @FundedByNameKey,
				Vague_Date_Start = @VagueDateStart,
				Vague_Date_End = @VagueDateEnd,
				Vague_Date_Type = @VagueDateType,
				Amount = @Amount,
				Currency_Concept_Key = @CurrencyConceptKey,
				Details = @Details,
				Changed_Session_ID = @SessionID
			WHERE	Movement_Funding_Key = @Key
			AND		[Timestamp] = @Timestamp

			SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

			IF @Error <> 0 GOTO RollbackAndExit 
		
			IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Movement_Funding WHERE Movement_Funding_Key = @Key)
			BEGIN
				RAISERROR('Record updated by another user', 16, 1)
				GOTO RollbackAndExit
			END

		END ELSE BEGIN
			UPDATE 	Conservation_Job_Funding
			SET 	Conservation_Job_Key = @ParentKey,
				Funded_By_Name_Key = @FundedByNameKey,
				Vague_Date_Start = @VagueDateStart,
				Vague_Date_End = @VagueDateEnd,
				Vague_Date_Type = @VagueDateType,
				Amount = @Amount,
				Currency_Concept_Key = @CurrencyConceptKey,
				Details = @Details,
				Changed_Session_ID = @SessionID
			WHERE	Conservation_Job_Funding_Key = @Key
			AND		[Timestamp] = @Timestamp

			SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

			IF @Error <> 0 GOTO RollbackAndExit 
		
			IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Conservation_Job_Funding WHERE Conservation_Job_Funding_Key = @Key)
			BEGIN
				RAISERROR('Record updated by another user', 16, 1)
				GOTO RollbackAndExit
			END
		END

		IF @@Error <> 0 GOTO RollbackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO
			
/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Funding_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Funding_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Funding_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Funding_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Funding_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Funding_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Funding_Update TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_LocalDomain_Delete]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_LocalDomain_Delete]
GO

/*===========================================================================*\
  Description:	Deletes a Local_Domain record.

  Parameters:	@Key	Local_Domain_Key
				@Timestamp

  Created:	December 2003

  Last revision information:
    $Revision: 1 $
    $Date: 3/02/09 12:20 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_LocalDomain_Delete]
	@Key char(16),
	@Timestamp timestamp = NULL
AS

SET NOCOUNT ON

	BEGIN TRANSACTION
	
		DELETE 
		FROM 	Local_Domain
		WHERE	Local_Domain_Key = @Key
		AND		([Timestamp] = @Timestamp OR (@Timestamp IS NULL))

		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Local_Domain WHERE Local_Domain_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_LocalDomain_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_LocalDomain_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_LocalDomain_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_LocalDomain_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_LocalDomain_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_LocalDomain_Delete TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_LocalDomain_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_LocalDomain_Update]
GO

/*===========================================================================*\
  Description:	Updates a record in the Local Domain table

  Parameters:	@Key 
		@ItemName 
		@LanguageKey 
		@ConceptGroupLabel
		@SessionID 
		@Timestamp 

  Created:	November 2003

  Last revision information:
    $Revision: 1 $
    $Date: 3/02/09 12:20 $
    $Author: Pauldavies $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_LocalDomain_Update]
	@Key char(16),
	@ItemName varchar(100),
	@LanguageKey varchar(4),
	@ConceptGroupLabel varchar(50),
	@SessionID char(16),
	@Timestamp timestamp
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION
		
		UPDATE 	Local_Domain
		SET 	Item_Name = @ItemName, 
			Language_Key = @LanguageKey,
			Concept_Group_Label = @ConceptGroupLabel,
			Changed_Session_ID = @SessionID
		WHERE	Local_Domain_Key = @Key
		AND		[Timestamp] = @Timestamp

		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Local_Domain WHERE Local_Domain_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_LocalDomain_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_LocalDomain_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_LocalDomain_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_LocalDomain_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_LocalDomain_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_LocalDomain_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_LocalDomain_Update TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_LocationFeatureData_Delete') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_LocationFeatureData_Delete]
GO

/*===========================================================================*\
  Description:	Deletes a record in the Location Feature Data table

  Parameters:	@Key
		@Timestamp

  Created:	August 2004

  Last revision information:
    $Revision: 1 $
    $Date: 3/02/09 12:20 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_LocationFeatureData_Delete]
	@Key char(16),
	@Timestamp timestamp
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION

		DELETE	Location_Feature_Data
		WHERE	Location_Feature_Data_Key = @Key
		AND		[Timestamp] = @Timestamp

		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Location_Feature_Data WHERE Location_Feature_Data_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_LocationFeatureData_Delete') AND SysStat & 0xf = 4)
BEGIN
	PRINT 'Setting up security on procedure usp_LocationFeatureData_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_LocationFeatureData_Delete TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_LocationFeatureData_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_LocationFeatureData_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_LocationFeatureData_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
		GRANT EXECUTE ON dbo.usp_LocationFeatureData_Delete TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_LocationFeatureData_Update') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_LocationFeatureData_Update]
GO

/*===========================================================================*\
  Description:	Updates a record in the Location Feature Data table.
		The LocationFeature_Data table hold descriptor and measurement
		information.

  Parameters:  Fields of Location_Feature_Data

  Created:     August 2004

  Last revision information:
    $Revision: 1 $
    $Date: 3/02/09 12:20 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_LocationFeatureData_Update]
	-- Required by both Measurements and Descriptors updates.
	@Key char(16),
	@LocationFeatureKey char(16) = NULL,
	@IsDescriptor bit,
	@ParameterConceptKey char(16),
	@AppliesTo varchar(50),
	@Value varchar(50),	-- Used for Descriptors and as Lower_Value for Measurements
	@SessionID char(16),
	@Timestamp timestamp,
	-- Only required for the Measurements update.
	@UpperValue varchar(50) = NULL,
	@MethodConceptKey char(16) = NULL,
	@Duration varchar(50) = NULL,
	@Accuracy varchar(50) = NULL,
	@UnitConceptKey char(16) = NULL
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	/*----------------------------------------------------------------------------------*\
	  If we are updating measurement data, more information needs to changed than if
	  we are inserting descriptor data. Hence, there are two different update statements.
	\*----------------------------------------------------------------------------------*/
	BEGIN TRANSACTION

		IF @IsDescriptor = 1	
			-- Updating a descriptor.
			UPDATE	Location_Feature_Data
			SET	Applies_To = @AppliesTo,
				Parameter_Concept_Key = @ParameterConceptKey,
				Lower_Value = IsNull(@Value, ' '),
				Is_Descriptor = 1,
				Changed_Session_ID = @SessionID
			WHERE	Location_Feature_Data_Key = @Key
			AND		[Timestamp] = @Timestamp
	
		ELSE		
			-- Updating a measurement.
			UPDATE	Location_Feature_Data
			SET	Applies_To = @AppliesTo,
				Parameter_Concept_Key = @ParameterConceptKey,
				Lower_Value = IsNull(@Value, ' '),
				Is_Descriptor = 0,
				Changed_Session_ID = @SessionID,
				Location_Feature_Key = @LocationFeatureKey,
				Method_Concept_Key = @MethodConceptKey,
				Duration = @Duration,
				Accuracy = @Accuracy,
				Unit_Concept_Key = @UnitConceptKey,
				Upper_Value = @UpperValue
			WHERE	Location_Feature_Data_Key = @Key
			AND		[Timestamp] = @Timestamp

		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Location_Feature_Data WHERE Location_Feature_Data_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_LocationFeatureData_Update') AND SysStat & 0xf = 4)
BEGIN
	PRINT 'Setting up security on procedure usp_LocationFeatureData_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_LocationFeatureData_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_LocationFeatureData_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_LocationFeatureData_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_LocationFeatureData_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
		GRANT EXECUTE ON dbo.usp_LocationFeatureData_Update TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_MeaningRelation_Delete]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_MeaningRelation_Delete]
GO

/*===========================================================================*\
  Description:	Deletes a record from the MeaningRelation table.

  Parameters:	@Key		Meaning_Relation_Key
		@Timestamp

  Created:	December 2003

  Last revision information:
    $Revision: 1 $
    $Date: 3/02/09 12:20 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_MeaningRelation_Delete]
	@Key char(16),
	@Timestamp timestamp
AS

SET NOCOUNT ON

	/*-------------------------------------------------------------*\
	  Wrap everything in one transaction.
	\*-------------------------------------------------------------*/
	BEGIN TRANSACTION

		/*-------------------------------------------------------------*\
		  Update in Meaning_Relation.
		\*-------------------------------------------------------------*/
		DELETE 	Meaning_Relation
		WHERE	Meaning_Relation_Key = @Key	
		AND		[Timestamp] = @Timestamp
		
		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Meaning_Relation WHERE Meaning_Relation_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

	/*-------------------------------------------------------------*\
	  All went well, so commit.
	\*-------------------------------------------------------------*/
	COMMIT TRANSACTION

RETURN 0

RollBackAndExit: 
	/*-------------------------------------------------------------*\
	  Cancel any changes, or left-overs might mess up some tables.
	\*-------------------------------------------------------------*/
	ROLLBACK TRANSACTION

GO

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_MeaningRelation_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_MeaningRelation_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_MeaningRelation_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_MeaningRelation_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_MeaningRelation_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_MeaningRelation_Delete TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_MeaningRelation_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_MeaningRelation_Update]
GO

/*===========================================================================*\
  Description:	Updates a record in the Meaning_Relation table.

  Parameters:	@Key
		@FromKey
		@ToKey
		@ThesaurusRelationTypeKey
		@Multiplicity
		@Comment
		@SessionID
		@SystemSuppliedData

  Created:	December 2003

  Last revision information:
    $Revision: 1 $
    $Date: 3/02/09 12:20 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_MeaningRelation_Update]
	@Key char(16),
	@FromConceptKey char(16),
	@ToConceptKey char(16),
	@ThesaurusRelationTypeKey char(16),
	@Multiplicity float = NULL,
	@Inherited bit,
	@Comment text = NULL,
	@SessionID char(16),
	@Timestamp timestamp

AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	/*-------------------------------------------------------------*\
	  Wrap everything in one transaction.
	\*-------------------------------------------------------------*/
	BEGIN TRANSACTION

		DECLARE @FromMeaningKey char(16)
		DECLARE @ToMeaningKey char(16)

		SELECT 	@FromMeaningKey = Meaning_Key
		FROM	Concept
		WHERE	Concept_Key = @FromConceptKey 

		SELECT 	@ToMeaningKey = Meaning_Key
		FROM	Concept
		WHERE	Concept_Key = @ToConceptKey 

		IF @@Error <> 0 GOTO RollbackAndExit

		/*-------------------------------------------------------------*\
		  Update record in Meaning_Relation.
		\*-------------------------------------------------------------*/
		UPDATE	Meaning_Relation
		SET	From_Meaning_Key = @FromMeaningKey,
			To_Meaning_Key = @ToMeaningKey,
			From_Concept_Key = @FromConceptKey,
			To_Concept_Key = @ToConceptKey,
			Thesaurus_Relation_Type_Key = @ThesaurusRelationTypeKey,
			Multiplicity = @Multiplicity,
			Inherited = @Inherited,
			Comment = @Comment,
			Changed_Session_ID = @SessionID
		WHERE	Meaning_Relation_Key = @Key
		AND		[Timestamp] = @Timestamp
	
		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Meaning_Relation WHERE Meaning_Relation_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

	/*-------------------------------------------------------------*\
	  All went well, so commit.
	\*-------------------------------------------------------------*/
	COMMIT TRANSACTION

RETURN 0

RollBackAndExit: 
	/*-------------------------------------------------------------*\
	  Cancel any changes, or left-overs might mess up some tables.
	\*-------------------------------------------------------------*/
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_MeaningRelation_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_MeaningRelation_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_MeaningRelation_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_MeaningRelation_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_MeaningRelation_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_MeaningRelation_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_MeaningRelation_Update TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Metadata_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Metadata_Update]
GO

/*===========================================================================*\
  Description:	Updates a record in the Metadata table.

  Parameters:	@MetadataKey
		@Text
		@Timestamp
		@SessionID

  Created:	September 2003

  Last revision information:
    $Revision: 1 $
    $Date: 3/02/09 12:20 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Metadata_Update]
	@MetadataKey char(16),
	@Text text,
	@Timestamp timestamp,
	@SessionID char(16)
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	/*-------------------------------------------------------------*\
	  Do the table updates first. Or the containers will still have
	  the specimen and its mask!
	\*-------------------------------------------------------------*/
	BEGIN TRANSACTION

		UPDATE	Metadata
		SET	[Text] = @Text,
			Changed_Session_ID = @SessionID
		WHERE	Metadata_Key = @MetadataKey
		AND		[Timestamp] = @Timestamp

		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Metadata WHERE Metadata_Key = @MetadataKey)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Metadata_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Metadata_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Metadata_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Metadata_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Metadata_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Metadata_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Metadata_Update TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Movement_Delete]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Movement_Delete]
GO

/*===========================================================================*\
  Description:	Delete a record from the Movement_Of_Ownership table.

  Parameters:	@Key		Movement Of Ownership key.
		@Timestamp

  Created:	December 2003

  Last revision information:
    $Revision: 1 $
    $Date: 3/02/09 12:20 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Movement_Delete]
	@Key char(16),
	@Timestamp timestamp
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION

		DELETE		Movement_Of_Material_Exclusion 
		FROM		Movement_Of_Material_Exclusion AS MOME
		INNER JOIN	Movement_Of_Material AS MOM ON MOM.Movement_Of_Material_Key = MOME.Movement_Of_Material_Key
		INNER JOIN	Movement_Direction AS MD ON MD.Movement_Direction_Key = MOM.Movement_Direction_Key
		INNER JOIN	Movement AS M ON M.Movement_Key = MD.Movement_Key
		WHERE		M.Movement_Key = @Key

		IF @@Error <> 0 GOTO RollbackAndExit

		DELETE		Movement_Of_Material 
		FROM		Movement_Of_Material AS MOM
		INNER JOIN	Movement_Direction AS MD ON MD.Movement_Direction_Key = MOM.Movement_Direction_Key
		INNER JOIN	Movement AS M ON M.Movement_Key = MD.Movement_Key
		WHERE		M.Movement_Key = @Key

		IF @@Error <> 0 GOTO RollbackAndExit

		DELETE		Movement_Of_Ownership_Exclusion 
		FROM		Movement_Of_Ownership_Exclusion AS MOOE
		INNER JOIN	Movement_Of_Ownership AS MOO ON MOO.Movement_Of_Ownership_Key = MOOE.Movement_Of_Ownership_Key
		INNER JOIN	Movement_Direction AS MD ON MD.Movement_Direction_Key = MOO.Movement_Direction_Key
		INNER JOIN	Movement AS M ON M.Movement_Key = MD.Movement_Key
		WHERE		M.Movement_Key = @Key

		IF @@Error <> 0 GOTO RollbackAndExit

		DELETE		Movement_Of_Ownership 
		FROM		Movement_Of_Ownership AS MOO
		INNER JOIN	Movement_Direction AS MD ON MD.Movement_Direction_Key = MOO.Movement_Direction_Key
		INNER JOIN	Movement AS M ON M.Movement_Key = MD.Movement_Key
		WHERE		M.Movement_Key = @Key

		IF @@Error <> 0 GOTO RollbackAndExit

		DELETE		Movement_Collection_Unit
		FROM		Movement_Collection_Unit AS MCU
		INNER JOIN	Movement_Direction AS MD ON MD.Movement_Direction_Key = MCU.Movement_Direction_Key
		INNER JOIN	Movement AS M ON M.Movement_Key = MD.Movement_Key
		WHERE		M.Movement_Key = @Key

		IF @@Error <> 0 GOTO RollbackAndExit

		DELETE		Movement_Direction
		FROM		Movement_Direction
		WHERE		Movement_Key = @Key

		IF @@Error <> 0 GOTO RollbackAndExit

		DELETE		Movement_Funding
		FROM		Movement_Funding
		WHERE		Movement_Key = @Key

		IF @@Error <> 0 GOTO RollbackAndExit

		DELETE		Movement_Communication
		FROM		Movement_Communication
		WHERE		Movement_Key = @Key

		IF @@Error <> 0 GOTO RollbackAndExit

		DELETE		Movement_Valuation
		FROM		Movement_Valuation
		WHERE		Movement_Key = @Key

		IF @@Error <> 0 GOTO RollbackAndExit

		DELETE		Movement_Conservation_Check
		FROM		Movement_Conservation_Check
		WHERE		Movement_Key = @Key

		IF @@Error <> 0 GOTO RollbackAndExit

		DELETE		Movement_Enquiry
		FROM		Movement_Enquiry
		WHERE		Movement_Key = @Key

		IF @@Error <> 0 GOTO RollbackAndExit

		DELETE 		Movement
		WHERE		Movement_Key = @Key
		AND			[Timestamp] = @Timestamp

		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Movement WHERE Movement_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Movement_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Movement_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Movement_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Movement_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Movement_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Movement_Delete TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Movement_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Movement_Update]
GO

/*===========================================================================*\
  Description:	Updates a record in the Movement table.

  Parameters:	@Key
		@MovementType
		@OtherPartyNameKey
		@StaffResponsibleNameKey
		@ContactNameKey
		@ExpVagueDateStart
		@ExpVagueDateEnd
		@ExpVagueDateType
		@Number
		@Notes
		@SessionID
		@Timestamp 

  Created:	September 2003

  Last revision information:
    $Revision: 1 $
    $Date: 3/02/09 12:20 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Movement_Update]
	@Key char(16) OUTPUT,
	@MovementType tinyint, 
	@StaffResponsibleNameKey char(16),
	@OtherPartyNameKey char(16),
	@ContactNameKey char(16),
	@VagueDateStart int, 
	@VagueDateEnd int, 
	@VagueDateType varchar(2),
	@Number varchar(30),
	@Notes text,
	@SessionID char(16), 
	@Timestamp timestamp,
	@RecordsAffected int OUTPUT

AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	DECLARE @Error int

	BEGIN TRANSACTION
		
		UPDATE 	Movement
		SET 	Movement_Type = @MovementType, 
			Staff_Responsible_Name_Key = @StaffResponsibleNameKey,			
			Other_Party_Name_Key = @OtherPartyNameKey,
			Contact_Name_Key = @ContactNameKey,
			Exp_Vague_Date_Start = @VagueDateStart, 
			Exp_Vague_Date_End = @VagueDateEnd, 
			Exp_Vague_Date_Type = @VagueDateType,
			Number = @Number, 
			Notes = @Notes, 
			Changed_Session_ID = @SessionID

		WHERE	Movement_Key = @Key
		AND		[Timestamp] = @Timestamp

		SELECT	@RecordsAffected = @@RowCount,
			@Error = @@Error

		IF @Error <> 0 GOTO RollbackAndExit

		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Movement WHERE Movement_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Movement_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Movement_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Movement_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Movement_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Movement_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Movement_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Movement_Update TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_MovementCollectionUnit_Delete]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_MovementCollectionUnit_Delete]
GO

/*===========================================================================*\
  Description:	Delete a record from the Movement_Collection_Unit table.

  Parameters:	@Key		Movement_Collection_Unit key.

  Created:	December 2003

  Last revision information:
    $Revision: 1 $
    $Date: 3/02/09 12:20 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_MovementCollectionUnit_Delete]
	@Key char(16)
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION

		DELETE	Movement_Collection_Unit
		WHERE	Movement_Collection_Unit_Key = @Key

		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Movement_Collection_Unit WHERE Movement_Collection_Unit_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_MovementCollectionUnit_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_MovementCollectionUnit_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_MovementCollectionUnit_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_MovementCollectionUnit_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_MovementCollectionUnit_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_MovementCollectionUnit_Delete TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_MovementCommunication_Delete]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_MovementCommunication_Delete]
GO

/*===========================================================================*\
  Description:	Delete a record from the Movement_Communication table.

  Parameters:	@Key		Movement Communication key.
		@Timestamp

  Created:	November 2003

  Last revision information:
    $Revision: 1 $
    $Date: 3/02/09 12:20 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_MovementCommunication_Delete]
	@Key char(16),
	@Timestamp timestamp
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION

		DELETE	Movement_Communication
		WHERE	Movement_Communication_Key = @Key
		AND		[Timestamp] = @Timestamp

		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Movement_Communication WHERE Movement_Communication_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_MovementCommunication_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_MovementCommunication_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_MovementCommunication_Delete TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_MovementCommunication_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_MovementCommunication_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_MovementCommunication_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_MovementCommunication_Delete TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_MovementCommunication_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_MovementCommunication_Update]
GO

/*===========================================================================*\
  Description:	Updates a record in the Movement_Communication table.

  Parameters:	@Key
		@MovementKey
		@SenderNameKey
		@ReceiverNameKey
		@CommunicationTypeConceptKey
		@VagueDateStart
		@VagueDateEnd
		@VagueDateType
		@Content
		@FileRef
		@SessionID,
		@Timestamp

  Created:	September 2003

  Last revision information:
    $Revision: 1 $
    $Date: 3/02/09 12:20 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_MovementCommunication_Update]
	@Key char(16) OUTPUT,
	@MovementKey char(16),
	@SenderNameKey char(16),
	@ReceiverNameKey char(16),
	@CommunicationTypeConceptKey char(16),
	@VagueDateStart int, 
	@VagueDateEnd int, 
	@VagueDateType varchar(2),
	@Content text,
	@FileRef varchar(20),
	@SessionID char(16), 
	@Timestamp timestamp
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION
		
		UPDATE 	Movement_Communication
		SET 	Movement_Key = @MovementKey,
			Sender_Name_Key = @SenderNameKey,
			Receiver_Name_Key = @ReceiverNameKey,
			Communication_Type_Concept_Key = @CommunicationTypeConceptKey,
			Vague_Date_Start = @VagueDateStart,
			Vague_Date_End = @VagueDateEnd,
			Vague_Date_Type = @VagueDateType,
			Content = @Content,
			File_Ref = @FileRef,
			Changed_Session_ID = @SessionID

		WHERE	Movement_Communication_Key = @Key
		AND		[Timestamp] = @Timestamp

		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Movement_Communication WHERE Movement_Communication_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_MovementCommunication_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_MovementCommunication_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_MovementCommunication_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_MovementCommunication_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_MovementCommunication_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_MovementCommunication_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_MovementCommunication_Update TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_MovementDirection_Delete]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_MovementDirection_Delete]
GO

/*===========================================================================*\
  Description:	Delete a record from the Movement_Direction table.

  Parameters:	@Key		Movement Direction key.
		@Timestamp

  Created:	December 2003

  Last revision information:
    $Revision: 1 $
    $Date: 3/02/09 12:20 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_MovementDirection_Delete]
	@Key char(16),
	@Timestamp timestamp
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION

		DELETE	Movement_Direction
		WHERE	Movement_Direction_Key = @Key
		AND		[Timestamp] = @Timestamp

		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Movement_Direction WHERE Movement_Direction_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_MovementDirection_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_MovementDirection_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_MovementDirection_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_MovementDirection_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_MovementDirection_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_MovementDirection_Delete TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_MovementMaterialDetail_Delete]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_MovementMaterialDetail_Delete]
GO

/*===========================================================================*\
  Description:	Delete a record from the Movement_Of_Material table.

  Parameters:	@Key
		@Timestamp

  Created:	December 2003

  Last revision information:
    $Revision: 1 $
    $Date: 3/02/09 12:20 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_MovementMaterialDetail_Delete]
	@Key char(16),
	@Timestamp timestamp
AS
	SET NOCOUNT OFF

	BEGIN TRANSACTION

		-- Delete record from Movement_Of_Material table.
		DELETE	Movement_Of_Material
		WHERE	Movement_Of_Material_Key = @Key
		AND		[Timestamp] = @Timestamp
		
		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Movement_Of_Material WHERE Movement_Of_Material_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_MovementMaterialDetail_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_MovementMaterialDetail_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_MovementMaterialDetail_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_MovementMaterialDetail_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_MovementMaterialDetail_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_MovementMaterialDetail_Delete TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_MovementMaterialDetail_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_MovementMaterialDetail_Update]
GO

/*===========================================================================*\
  Description:	Updates records in the Movement_Of_Material table.

  Parameters:	@Key	Collection key

  Created:	September 2003

  Last revision information:
    $Revision: 1 $
    $Date: 3/02/09 12:20 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_MovementMaterialDetail_Update]
	@Key char(16), -- The key of the leafnode to be updated.
	@ContactNameKey char(16),
	@VagueDateStart int,
	@VagueDateEnd int,
	@VagueDateType varchar(2),
	@Completed bit,
	@ReceiverNameKey char(17),	-- Must be 17 because organisations will have a * on the end.
	@ValueAmount money,
	@CurrencyConceptKey char(16),
	@AcquisitionMethodConceptKey char(16),
	@Notes text,
	@SessionID char(16),
	@Timestamp timestamp
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	DECLARE @RefinedReceiverNameKey varchar(16)
	DECLARE @RefinedDepartmentKey varchar(16)
	DECLARE @MovementDirectionKey varchar(16)

	/*-------------------------------------------------------------*\
	  Need to get the MovementDirectionKey.
	\*-------------------------------------------------------------*/
	SET @MovementDirectionKey = (SELECT Movement_Direction_Key
					FROM Movement_Of_Material
					WHERE Movement_Of_Material_Key = @Key)

	/*-------------------------------------------------------------*\
	  If the @ReceiverNameKey doesn't have a '*' at the end of it,
	  then it is the key of and individual or an organisation.
	\*-------------------------------------------------------------*/
	IF CHARINDEX('*', @ReceiverNameKey)=0
	BEGIN
		SET @RefinedReceiverNameKey = @ReceiverNameKey
		SET @RefinedDepartmentKey = NULL
	END
	ELSE
 	BEGIN
		SET @RefinedReceiverNameKey =  (SELECT Name_Key 
			    			FROM Organisation_Department
			     			WHERE Organisation_Department_Key = 
							LEFT(@ReceiverNameKey, CHARINDEX('*', @ReceiverNameKey)-1))
		SET @RefinedDepartmentKey = LEFT(@ReceiverNameKey, CHARINDEX('*', @ReceiverNameKey)-1)
	END	

	BEGIN TRANSACTION

		UPDATE 	Movement_Of_Material
		SET	Movement_Direction_Key = @MovementDirectionKey,
			Contact_Name_Key = @ContactNameKey,
			Vague_Date_Start = @VagueDateStart,
			Vague_Date_End = @VagueDateEnd,
			Vague_Date_Type = @VagueDateType,
			Completed = @Completed,
			Receiver_Name_Key = @RefinedReceiverNameKey,
			Receiver_Organisation_Department_Key = @RefinedDepartmentKey,
			Value_Amount = @ValueAmount,
			Currency_Concept_Key = @CurrencyConceptKey,
			Acquisition_Method_Concept_Key = @AcquisitionMethodConceptKey,
			Notes = @Notes,
			Changed_Session_ID = @SessionID

		WHERE	Movement_Of_Material_Key = @Key
		AND		[Timestamp] = @Timestamp
		
		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Movement_Of_Material WHERE Movement_Of_Material_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_MovementMaterialDetail_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_MovementMaterialDetail_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_MovementMaterialDetail_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_MovementMaterialDetail_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_MovementMaterialDetail_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_MovementMaterialDetail_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_MovementMaterialDetail_Update TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_MovementOfOwnership_Delete]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_MovementOfOwnership_Delete]
GO

/*===========================================================================*\
  Description:	Delete a record from the Movement_Of_Ownership table.

  Parameters:	@Key		Movement Of Ownership key.
		@Timestamp

  Created:	December 2003

  Last revision information:
    $Revision: 1 $
    $Date: 3/02/09 12:20 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_MovementOfOwnership_Delete]
	@Key char(16),
	@Timestamp timestamp
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION

		DELETE	Movement_Of_Ownership 
		WHERE	Movement_Of_Ownership_Key = @Key
		AND		[Timestamp] = @Timestamp

		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Movement_Of_Ownership WHERE Movement_Of_Ownership_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_MovementOfOwnership_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_MovementOfOwnership_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_MovementOfOwnership_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_MovementOfOwnership_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_MovementOfOwnership_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_MovementOfOwnership_Delete TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_MovementOfOwnership_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_MovementOfOwnership_Update]
GO
/*===========================================================================*\
  Description: 	Updates a record in the Movement_Of_Ownership table.
  Parameters:	@Key
		@MovementDirectionKey
		@ContactNameKey
		@VagueDateStart
		@VagueDateEnd
		@VagueDateType
		@Notes
		@Completed
		@SessionID
		@Timestamp 

  Created:	October 2003

  Last revision information:
    $Revision: 1 $
    $Date: 3/02/09 12:20 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_MovementOfOwnership_Update]
	@Key char(16) OUTPUT,
	@ParentKey char(16),
	@ContactNameKey char(16),
	@VagueDateStart int,
	@VagueDateEnd int,
	@VagueDateType varchar(2),
	@Notes text,
	@Completed bit,
	@SessionID char(16),
	@Outbound bit,
	@Timestamp timestamp
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF
	
	DECLARE @MovementDirectionKey varchar(16)

	/*-------------------------------------------------------------*\
	  Need to get the MovementDirectionKey.
	\*-------------------------------------------------------------*/
	SELECT	@MovementDirectionKey = Movement_Direction_Key
	FROM	Movement_Direction AS MD
	WHERE 	MD.Movement_Key = @ParentKey
	AND 	MD.Outbound = @Outbound

	BEGIN TRANSACTION

		UPDATE 	Movement_Of_Ownership
		SET 	Movement_Direction_Key = @MovementDirectionKey,
			Contact_Name_Key = @ContactNameKey,
			Vague_Date_Start = @VagueDateStart,
			Vague_Date_End = @VagueDateEnd,
			Vague_Date_Type = @VagueDateType,
			Notes = @Notes,
			Completed = @Completed,
			Changed_Session_ID = @SessionID
		WHERE	Movement_Of_Ownership_Key = @Key 
		AND		[Timestamp] = @Timestamp

		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Movement_Of_Ownership WHERE Movement_Of_Ownership_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_MovementOfOwnership_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_MovementOfOwnership_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_MovementOfOwnership_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_MovementOfOwnership_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_MovementOfOwnership_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_MovementOfOwnership_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_MovementOfOwnership_Update TO [Dev - JNCC SQL]
END
GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Multimedia_Update]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Multimedia_Update]
GO

/*===========================================================================*\
  Description:	Updates Multimedia data in the Source_File table.

  Parameters:	@Key	
		@RecordKey
		@Filename
		@Title
		@Preferred
		@TableName
		@Timestamp
		@RecordsAffected (output)

  Created:	October 2003

  Last revision information:
    $Revision: 1 $
    $Date: 3/02/09 12:20 $
    $Author: Pauldavies $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_Multimedia_Update] 
	@Key char(16),
	@RecordKey char(16),
	@Filename varchar(255),
	@Title varchar(100),
	@Preferred bit,
	@TableName varchar(50),
	@Timestamp timestamp,
	@RecordsAffected int OUTPUT
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	DECLARE @Error int

	BEGIN TRANSACTION
		-- If this new item is to be set as Preferred, then all the others for that record key
		-- and table name should not be set as preferred.
		IF @Preferred = 1
		BEGIN
			UPDATE 		Source_File
			SET 		Preferred = 0
			FROM		Source_File AS SF
			INNER JOIN	Source_Join AS SJ ON SJ.Source_Key = SF.Source_Key
			WHERE		SJ.Record_Key = @RecordKey
			AND		SJ.Table_Name = @TableName
			AND		SJ.Source_Key <> @Key	-- We don't want to change the record
								-- we are updating, or the timestamp
								-- will change as well.
			IF @@Error <> 0 GOTO RollbackAndExit
		END
		ELSE BEGIN
			-- Ensure one other stays preferred
			IF NOT EXISTS(
							SELECT 1
							FROM		Source_File AS SF
								INNER JOIN	Source_Join AS SJ ON SJ.Source_Key = SF.Source_Key
								WHERE		SJ.Record_Key = @RecordKey
								AND		SJ.Table_Name = @TableName
								AND		Preferred = 1
								AND SF.Source_Key<>@Key)
			BEGIN
				UPDATE Source_File
					SET Preferred=1
					WHERE Source_Key IN (
						SELECT TOP 1 SF.Source_Key FROM Source_File SF
						INNER JOIN	Source_Join AS SJ ON SJ.Source_Key = SF.Source_Key
						WHERE		SJ.Record_Key = @RecordKey
						AND		SJ.Table_Name = @TableName
						AND SF.Source_Key<>@Key)
				IF @@Error <> 0 GOTO RollbackAndExit
			END
		END

		-- Force item to preferred if the only one
		IF @Preferred = 0
			IF NOT EXISTS(
				SELECT 1
				FROM		Source_File AS SF
					INNER JOIN	Source_Join AS SJ ON SJ.Source_Key = SF.Source_Key
					WHERE		SJ.Record_Key = @RecordKey
					AND		SJ.Table_Name = @TableName
					AND		Preferred = 1
					AND 	SF.Source_Key<>@Key)
				SET @Preferred=1

		/*---------------------------*\
		  Actually updates the table
		\*---------------------------*/
		UPDATE 	Source_File
		SET	[File_Name] = @FileName,
			Title = @Title,
			Preferred = @Preferred
		WHERE	Source_Key = @Key
		AND		[Timestamp] = @Timestamp

		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Source_File WHERE Source_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO
			
/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Multimedia_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Multimedia_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Multimedia_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Multimedia_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Multimedia_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Multimedia_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Multimedia_Update TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Occurrence_Delete]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Occurrence_Delete]
GO

/*===========================================================================*\
  Description:	Delete an Occurrence record.

  Parameters:	@Key

  Created:	December 2003

  Last revision information:
    $Revision: 1 $
    $Date: 3/02/09 12:20 $
    $Author: Pauldavies $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_Occurrence_Delete]
	@Key char(16),
	@Timestamp timestamp
AS
	SET NOCOUNT OFF

	BEGIN TRANSACTION

		DELETE	Occurrence_Relation
		WHERE	To_Occurrence_Key = @Key

		IF @@Error <> 0 GOTO RollBackAndExit

		DELETE	Occurrence
		WHERE	Occurrence_Key = @Key
		AND		[Timestamp] = @Timestamp

		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Occurrence WHERE Occurrence_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Occurrence_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Occurrence_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Occurrence_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Occurrence_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Occurrence_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Occurrence_Delete TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Occurrence_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Occurrence_Update]
GO

/*===========================================================================*\
  Description:	Updates an occurrence record.

  Parameters:	@Key
		@SurveyorsRef
		@RecordTypeKey
		@Comment
		@Confidential
		@Checked
		@CheckedBy
		@CheckedDate
		@TimeStamp
		@SessionID

  Created:	October 2003

  Last revision information:
    $Revision: 1 $
    $Date: 3/02/09 12:20 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Occurrence_Update]
	@Key char(16),
	@SurveyorsRef varchar(30),
	@RecordTypeKey char(16),
	@Comment text,
	@Confidential bit,
	@Checked bit,
	@CheckedBy char(16),
	@CheckedDate datetime,
	@TimeStamp timestamp,
	@SessionID char(16)
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION
	
		UPDATE	Occurrence
		SET	Surveyors_Ref = @SurveyorsRef,
			Record_Type_Concept_Key  = @RecordTypeKey,
			Comment = @Comment,
			Confidential = @Confidential,
			Checked = @Checked, 
			Checked_By = @CheckedBy,
			Checked_Date = @CheckedDate
		WHERE	Occurrence_Key = @Key
		AND		[Timestamp] = @Timestamp

		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Occurrence WHERE Occurrence_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Occurrence_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Occurrence_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Occurrence_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Occurrence_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Occurrence_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Occurrence_Update TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Occurrence_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Occurrence_Update TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_OccurrenceData_Delete]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_OccurrenceData_Delete]
GO

/*===========================================================================*\
  Description:	Deletes a record in the Occurrence Data table

  Parameters:	@Key
		@Timestamp

  Created:	November 2003

  Last revision information:
    $Revision: 1 $
    $Date: 3/02/09 12:20 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_OccurrenceData_Delete]
	@Key char(16),
	@Timestamp timestamp
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION

		DELETE	Occurrence_Data
		WHERE	Occurrence_Data_Key = @Key
		AND		[Timestamp] = @Timestamp

		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Occurrence_Data WHERE Occurrence_Data_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_OccurrenceData_Delete') AND SysStat & 0xf = 4)
BEGIN
	PRINT 'Setting up security on procedure usp_OccurrenceData_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_OccurrenceData_Delete TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_OccurrenceData_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_OccurrenceData_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_OccurrenceData_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
		GRANT EXECUTE ON dbo.usp_OccurrenceData_Delete TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_OccurrenceData_Update') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_OccurrenceData_Update]
GO

/*===========================================================================*\
  Description:	Updates a record in the Occurrence_Data table.
		The Occurrence_Data table hold descriptor and measurement
		information.

  Parameters:	@Key
		@CollectionUnitKey
		@AppliesTo
		@MethodConceptKey
		@Duration
		@Accuracy
		@ParameterConceptKey
		@UnitConceptKey
		@Value
		@UpperValue
		@IsDescriptor
		@SessionID
		@Timestamp

  Created:	November 2003

  Last revision information:
    $Revision: 1 $
    $Date: 3/02/09 12:20 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_OccurrenceData_Update]
	-- Required by both Measurements and Descriptors updates.
	@Key char(16),
	@IsDescriptor bit,
	@ParameterConceptKey char(16),
	@AppliesTo varchar(50),
	@Value varchar(50),	-- Used for Descriptors and as Lower_Value for Measurements
	@SessionID char(16),
	@Timestamp timestamp,

	-- Only required for the Measurements update.
	@UpperValue varchar(50) = NULL,
	@OccurrenceKey char(16) = NULL,
	@MethodConceptKey char(16) = NULL,
	@Duration varchar(50) = NULL,
	@Accuracy varchar(50) = NULL,
	@UnitConceptKey char(16) = NULL

AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	/*----------------------------------------------------------------------------------*\
	  If we are updating measurement data, more information needs to changed than if
	  we are inserting descriptor data. Hence, there are two different update statements.
	\*----------------------------------------------------------------------------------*/
	BEGIN TRANSACTION

		IF @IsDescriptor = 1	
			-- Updating a descriptor.
			UPDATE	Occurrence_Data
			SET	Applies_To = @AppliesTo,
				Parameter_Concept_Key = @ParameterConceptKey,
				Lower_Value = IsNull(@Value, ' '),
				Is_Descriptor = @IsDescriptor,
				Changed_Session_ID = @SessionID
			WHERE	Occurrence_Data_Key = @Key
			AND		[Timestamp] = @Timestamp
	
		ELSE		
			-- Updating a measurement.
			UPDATE	Occurrence_Data
			SET	Applies_To = @AppliesTo,
				Parameter_Concept_Key = @ParameterConceptKey,
				Lower_Value = IsNull(@Value, ' '),
				Is_Descriptor = @IsDescriptor,
				Changed_Session_ID = @SessionID,
				Occurrence_Key = @OccurrenceKey,
				Method_Concept_Key = @MethodConceptKey,
				Duration = @Duration,
				Accuracy = @Accuracy,
				Unit_Concept_Key = @UnitConceptKey,
				Upper_Value = @UpperValue
			WHERE	Occurrence_Data_Key = @Key
			AND		[Timestamp] = @Timestamp

		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Occurrence_Data WHERE Occurrence_Data_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_OccurrenceData_Update') AND SysStat & 0xf = 4)
BEGIN
	PRINT 'Setting up security on procedure usp_OccurrenceData_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_OccurrenceData_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_OccurrenceData_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_OccurrenceData_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_OccurrenceData_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
		GRANT EXECUTE ON dbo.usp_OccurrenceData_Update TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_OccurrenceRelation_Delete]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_OccurrenceRelation_Delete]
GO

/*===========================================================================*\
  Description:	Delete a record from the Occurrence_Relation table.

  Parameters:	@Key		Occurrence Relation key.
		@Timestamp

  Created:	November 2003

  Last revision information:
    $Revision: 1 $
    $Date: 3/02/09 12:20 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_OccurrenceRelation_Delete]
	@Key char(16),
	@Timestamp timestamp
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION

		DELETE	Occurrence_Relation
		WHERE	Occurrence_Relation_Key = @Key
		AND		[Timestamp] = @Timestamp

		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Occurrence_Relation WHERE Occurrence_Relation_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_OccurrenceRelation_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_OccurrenceRelation_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_OccurrenceRelation_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_OccurrenceRelation_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_OccurrenceRelation_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_OccurrenceRelation_Delete TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_OccurrenceRelation_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_OccurrenceRelation_Update]
GO

/*===========================================================================*\
  Description:	Update a record in the Occurrence_Relation table.

  Parameters:	@Key			Occurrence Relation key.
		@ToOccurrenceKey 
		@RelationTypeKey 
		@Comment
		@SessionID 
		@Timestamp 

  Created:	September 2003

  Last revision information:
    $Revision: 1 $
    $Date: 3/02/09 12:20 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_OccurrenceRelation_Update]
	@Key char(16),
	@ToOccurrenceKey char(16),
	@RelationTypeKey char(16),
	@Comment text,
	@SessionID char(16),
	@Timestamp timestamp
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION

		UPDATE	Occurrence_Relation
		SET	To_Occurrence_Key = @ToOccurrenceKey,
			Thesaurus_Relation_Type_Key = @RelationTypeKey,
			Comment = @Comment,
			Changed_Session_ID = @SessionID
		WHERE	Occurrence_Relation_Key = @Key
		AND		[Timestamp] = @Timestamp

		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Occurrence_Relation WHERE Occurrence_Relation_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_OccurrenceRelation_Update') AND SysStat & 0xf = 4)
BEGIN
	PRINT 'Setting up security on procedure usp_OccurrenceRelation_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_OccurrenceRelation_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_OccurrenceRelation_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_OccurrenceRelation_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_OccurrenceRelation_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
		GRANT EXECUTE ON dbo.usp_OccurrenceRelation_Update TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_QEDataItem_Delete]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_QEDataItem_Delete]
GO

/*===========================================================================*\
  Description:	Clears an item from a Quick Entry session

  Parameters:	@Key - QE_Data_Item key
							@Timestamp

  Created:	Jan 2004

  Last revision information:
    $Revision: 1 $
    $Date: 3/02/09 12:20 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_QEDataItem_Delete]
	@Key INT,
	@Timestamp TIMESTAMP
AS
  
		DELETE	QE_Data_Item
		WHERE	QE_Data_Item_Key = @Key
		AND		[Timestamp] = @Timestamp

		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		IF @@Rowcount = 0 AND EXISTS(SELECT 1 FROM QE_Data_Item WHERE QE_Data_Item_Key = @Key)
			RAISERROR('Record updated by another user', 16, 1)

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_QEDataItem_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_QEDataItem_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_QEDataItem_Delete TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_QEDataItem_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_QEDataItem_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_QEDataItem_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_QEDataItem_Delete TO [Dev - JNCC SQL]
END

GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_QEDataItem_Update]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_QEDataItem_Update]
GO
    
/*===========================================================================*\
  Description:	

  Parameters:	

  Created:	August 2003

  Last revision information:
    $Revision: 1 $
    $Date: 3/02/09 12:20 $
    $Author: Pauldavies $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_QEDataItem_Update]
  @QEDataItemKey as int,
  @DataValue as varchar(200),
  @DataDisplay as varchar(200),
  @Timestamp as timestamp,
  @SessionID as char(16)

 AS

	SET NOCOUNT OFF

	Update QE_Data_Item
		set Data_Value = @DataValue,
		Data_Display = @DataDisplay,
		Changed_Session_ID = @SessionID
	where 
		[Timestamp] = @Timestamp
		and (QE_Data_Item_Key = @QEDataItemKey)

	IF @@Rowcount = 0 AND EXISTS(SELECT 1 FROM QE_Data_Item WHERE QE_Data_Item_Key = @QEDataItemKey)
		RAISERROR('Record updated by another user', 16, 1)

GO 

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_QEDataItem_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_QEDataItem_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_QEDataItem_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_QEDataItem_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_QEDataItem_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_QEDataItem_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_QEDataItem_Update TO [Dev - JNCC SQL]
END

GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_QEDataRow_Delete]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_QEDataRow_Delete]
GO
    
/*===========================================================================*\
  Description:	

  Parameters:	

  Created:	August 2003

  Last revision information:
    $Revision: 1 $
    $Date: 3/02/09 12:20 $
    $Author: Pauldavies $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_QEDataRow_Delete]
  @QEDataRowKey as int,
  @Timestamp as timestamp
AS

	SET NOCOUNT ON

	SET Xact_abort ON
	BEGIN TRAN

		DELETE FROM QE_Data_Item 
		WHERE QE_Data_Row_key = @QEDataRowKey
		
		IF @@Error <> 0 GOTO RollbackAndExit 

		DELETE FROM QE_Data_Row
		WHERE	(QE_Data_Row_Key = @QEDataRowKey)
		AND		[Timestamp] = @Timestamp
		
		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM QE_Data_Row WHERE QE_Data_Row_Key = @QEDataRowKey)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

	COMMIT TRAN
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_QEDataRow_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_QEDataRow_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_QEDataRow_Delete TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_QEDataRow_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_QEDataRow_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_QEDataRow_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_QEDataRow_Delete TO [Dev - JNCC SQL]
END

GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_QEDataRow_Update]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_QEDataRow_Update]
GO
    
/*===========================================================================*\
  Description:	

  Parameters:	

  Created:	August 2003

  Last revision information:
    $Revision: 1 $
    $Date: 3/02/09 12:20 $
    $Author: Pauldavies $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_QEDataRow_Update]
  @QEDataRowKey as int,
  @Validated as bit,
  @Processed as bit,
  @Timestamp as timestamp

 AS

SET NOCOUNT OFF

Update QE_Data_Row
	set Validated = @Validated,
	Processed = @Processed
where	[Timestamp] = @Timestamp
AND		(QE_Data_Row_Key = @QEDataRowKey)

IF @@Rowcount = 0 AND EXISTS(SELECT 1 FROM QE_Data_Row WHERE QE_Data_Row_Key = @QEDataRowKey)
	RAISERROR('Record updated by another user', 16, 1)
	
GO 

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_QEDataRow_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_QEDataRow_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_QEDataRow_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_QEDataRow_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_QEDataRow_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_QEDataRow_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_QEDataRow_Update TO [Dev - JNCC SQL]
END

GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_QESession_Delete]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_QESession_Delete]
GO
    
/*===========================================================================*\
  Description:	

  Parameters:	

  Created:	August 2003

  Last revision information:
    $Revision: 1 $
    $Date: 3/02/09 12:20 $
    $Author: Pauldavies $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_QESession_Delete]
  @QESessionKey as int,
  @Timestamp as timestamp
AS

	SET NOCOUNT ON

	SET Xact_abort ON
	BEGIN TRAN

		DELETE FROM QE_Data_Item 
		WHERE QE_Data_Row_key IN 
		(SELECT QE_Data_Row_Key FROM QE_Data_Row 
		WHERE QE_Session_Key = @QESessionKey)

		IF @@Error <> 0 GOTO RollbackAndExit 

		DELETE FROM QE_Data_Row
		WHERE QE_Session_Key = @QESessionKey
	
		IF @@Error <> 0 GOTO RollbackAndExit 

		DELETE FROM QE_Session
		WHERE QE_Session_Key = @QESessionKey
		AND		[Timestamp] = @Timestamp

		DECLARE @Error int
		DECLARE @RecordsAffected int

		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM QE_Session WHERE QE_Session_Key = @QESessionKey)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

	COMMIT TRAN
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_QESession_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_QESession_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_QESession_Delete TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_QESession_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_QESession_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_QESession_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_QESession_Delete TO [Dev - JNCC SQL]
END

GO
If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_QESession_Update]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_QESession_Update]
GO
    
/*===========================================================================*\
  Description:	

  Parameters:	

  Created:	August 2003

  Last revision information:
    $Revision: 1 $
    $Date: 3/02/09 12:20 $
    $Author: Pauldavies $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_QESession_Update]
  @QESessionKey as int,
  @QESessionName as varchar(50),
  @Timestamp as timestamp,
  @SessionID as char(16)
AS

SET NOCOUNT OFF

Update QE_Session
	set Item_Name = @QESessionName,
	Changed_Session_ID = @SessionID
	where 
	QE_Session_Key = @QESessionKey 
	AND		[Timestamp] = @Timestamp

	IF @@Rowcount = 0 AND EXISTS(SELECT 1 FROM QE_Session WHERE QE_Session_Key = @QESessionKey)
		RAISERROR('Record updated by another user', 16, 1)

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_QESession_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_QESession_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_QESession_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_QESession_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_QESession_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_QESession_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_QESession_Update TO [Dev - JNCC SQL]
END

GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_QETemplate_Delete]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_QETemplate_Delete]
GO
    
/*===========================================================================*\
  Description:	Deletes a QETemplate. Fails if the template has been altered
		since it was first loaded

  Parameters:	@QE_Template_Key  Key of template
		@Timestamp        Previously retrieved timestamp

  Created:	August 2003

  Last revision information:
    $Revision: 1 $
    $Date: 3/02/09 12:20 $
    $Author: Pauldavies $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_QETemplate_Delete]
  @QETemplateKey  Char(16),
  @Timestamp      timestamp
AS

	SET NOCOUNT Off
	SET XACT_ABORT ON
	BEGIN TRAN
		DELETE FROM QE_Template_field WHERE QE_Template_Key = @QETemplateKEY

		IF @@Error <> 0 GOTO RollbackAndExit 

		DELETE FROM QE_Template 
		WHERE QE_Template_Key = @QETemplateKey
		AND ([Timestamp] = @Timestamp)
		
		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM QE_Template WHERE QE_Template_Key = @QETemplateKey)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END
	COMMIT TRAN
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION

GO 

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_QETemplate_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_QETemplate_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_QETemplate_Delete TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_QETemplate_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_QETemplate_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_QETemplate_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_QETemplate_Delete TO [Dev - JNCC SQL]
END

GO
 
If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_QETemplate_Update]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_QETemplate_Update]
GO
    
/*===========================================================================*\
  Description:	Updates a QETemplate

  Parameters:	@QE_Template_Key  Key of template
		@Timestamp        Previously retrieved timestamp
		@Item_Name        Name of the template
		@Template_Type    
		@Subject_Area_Key 

  Created:	August 2003

  Last revision information:
    $Revision: 1 $
    $Date: 3/02/09 12:20 $
    $Author: Pauldavies $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_QETemplate_Update]
  @Key  Char(16),
  @Timestamp      timestamp,
  @ItemName      varchar(100),
  @TemplateType  tinyint,
  @SessionID      Char(16),
  @SubjectAreaKey char(16)
 AS

SET NOCOUNT OFF

update QE_Template 
	set Item_Name = @ItemName,
	Template_Type = @TemplateType,
	Subject_Area_Key = @SubjectAreaKey,
	Changed_Session_ID = @SessionID
	where QE_Template_Key = @Key
	AND ([Timestamp] = @Timestamp)

IF @@Rowcount = 0 AND EXISTS(SELECT 1 FROM QE_Template WHERE QE_Template_Key = @Key)
	RAISERROR('Record updated by another user', 16, 1)

GO 

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_QETemplate_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_QETemplate_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_QETemplate_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_QETemplate_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_QETemplate_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_QETemplate_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_QETemplate_Update TO [Dev - JNCC SQL]
END

GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_QETemplateField_Delete]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_QETemplateField_Delete]
GO

/*===========================================================================*\
  Description:	Deletes a QETemplateField. Fails if the template has been altered
				since it was first loaded

  Parameters:	@QE_Template_Key  Key of template
				@Timestamp        Previously retrieved timestamp

  Created:	August 2003

  Last revision information:
    $Revision: 1 $
    $Date: 3/02/09 12:20 $
    $Author: Pauldavies $

\*===========================================================================*/
    
CREATE PROCEDURE [dbo].[usp_QETemplateField_Delete]
  @QETemplateFieldKey  Char(16),
  @Timestamp      timestamp
AS
	SET NOCOUNT ON
	SET XACT_ABORT ON

	BEGIN TRAN
		DELETE FROM QE_Data_Item
			WHERE QE_Template_Field_Key = @QETemplateFieldKey

		IF @@Error <> 0 GOTO RollbackAndExit 

		DELETE FROM QE_Template_Field
			WHERE QE_Template_Field_Key = @QETemplateFieldKey
			AND ([Timestamp] = @Timestamp)

		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM QE_Template_Field WHERE QE_Template_Field_Key = @QETemplateFieldKey)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

	COMMIT TRAN
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION

GO 

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_QETemplateField_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_QETemplateField_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_QETemplateField_Delete TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_QETemplateField_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_QETemplateField_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_QETemplateField_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_QETemplateField_Delete TO [Dev - JNCC SQL]
END

GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_QETemplateField_Update]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_QETemplateField_Update]
GO
    
/*===========================================================================*\
  Description:	

  Parameters:	

  Created:	August 2003

  Last revision information:
    $Revision: 1 $
    $Date: 3/02/09 12:20 $
    $Author: Pauldavies $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_QETemplateField_Update]
 	@Key as char(16),
	@Timestamp as timestamp,
	@GeneralTab as bit,
	@SpecimenTab as bit,
	@ItemName as varchar(100),
	@DefaultValue as varchar(200),
	@DefaultDisplay as varchar(200),
	@SessionID as varchar(16),
	@Sequence int,
	@NumberTypeConceptKey as varchar(16),
	@NumberPreferred as bit,
	@Hidden as bit,
	@Locked as bit,
	@MetadataTypeKey as varchar(16)
AS

Set Nocount off

update QE_Template_Field
	set 
	General_Tab = @GeneralTab,
	Specimen_Tab = @SpecimenTab,
	Item_Name = @ItemName,
	Default_Value = @DefaultValue,
	Default_Display = @DefaultDisplay,
	Changed_Session_ID= @SessionID,
	Sequence = @Sequence,
	Number_Type_Concept_Key = @NumberTypeConceptKey,
	Number_Preferred = @NumberPreferred,
	Hidden = @Hidden,
	Locked = @Locked,
	Metadata_Type_Key = @MetadataTypeKey
	where QE_Template_Field_Key= @Key 
	AND		[Timestamp] = @Timestamp

IF @@Rowcount = 0 AND EXISTS(SELECT 1 FROM QE_Template_Field WHERE QE_Template_Field_Key = @Key)
	RAISERROR('Record updated by another user', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_QETemplateField_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_QETemplateField_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_QETemplateField_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_QETemplateField_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_QETemplateField_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_QETemplateField_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_QETemplateField_Update TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_SemanticRelation_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_SemanticRelation_Update]
GO

/*===========================================================================*\
  Description:	Updates a record in the Semantic_Relation table

  Parameters:	@Key,
		@ItemName 
		@Unidirectional 
		@ForwardEquivalencePossible
		@ForwardEquivalenceDefinite
		@ReverseEquivalencePossible 
		@ReverseEquivalenceDefinite
		@ProportionalRelationship
		@Adjacent 
		@Description 
		@SessionID 
		@Timestamp

  Created:	December 2003

  Last revision information:
    $Revision: 1 $
    $Date: 3/02/09 12:20 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SemanticRelation_Update]
	@Key char(16),
	@ItemName varchar(100),
	@Unidirectional bit,
	@ForwardEquivalencePossible bit,
	@ForwardEquivalenceDefinite bit,
	@ReverseEquivalencePossible bit,
	@ReverseEquivalenceDefinite bit,
	@ProportionalRelationship bit = NULL,
	@ChronologicalOverlap int = NULL,
	@Adjacent bit,
	@Description text,
	@SessionID char(16),
	@Timestamp timestamp

AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	/*-------------------------------------------------------------*\
	  Wrap everything in one transaction.
	\*-------------------------------------------------------------*/
	BEGIN TRANSACTION

		/*-------------------------------------------------------------*\
		  Update in Semantic_Relation.
		\*-------------------------------------------------------------*/
		UPDATE 	Semantic_Relation 
		SET	Item_Name = @ItemName, 
			Unidirectional = @Unidirectional, 
			Forward_Equivalence_Possible = @ForwardEquivalencePossible,
			Forward_Equivalence_Definite = @ForwardEquivalenceDefinite,
			Reverse_Equivalence_Possible = @ReverseEquivalencePossible,
			Reverse_Equivalence_Definite = @ReverseEquivalenceDefinite,
			Proportional_Relationship = IsNull(@ProportionalRelationship, 0),
			Chronological_Overlap = @ChronologicalOverlap,
			Adjacent = @Adjacent,
			[Description] = @Description,
			Entered_Session_ID = @SessionID

		WHERE	Semantic_Relation_Key = @Key
		AND		[Timestamp] = @Timestamp

		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Semantic_Relation WHERE Semantic_Relation_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

	/*-------------------------------------------------------------*\
	  All went well, so commit.
	\*-------------------------------------------------------------*/
	COMMIT TRANSACTION

RETURN 0

RollBackAndExit: 
	/*-------------------------------------------------------------*\
	  Cancel any changes, or left-overs might mess up some tables.
	\*-------------------------------------------------------------*/
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SemanticRelation_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SemanticRelation_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SemanticRelation_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SemanticRelation_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SemanticRelation_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SemanticRelation_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SemanticRelation_Update TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_SourceJoin_Delete]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_SourceJoin_Delete]
GO

/*===========================================================================*\
  Description:	Delete a record from the Source_Join table.

  Parameters:	@Key		Collection Unit Material key.
		@Timestamp

  Created:	November 2003

  Last revision information:
    $Revision: 1 $
    $Date: 3/02/09 12:20 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SourceJoin_Delete]
	@Key char(16),
	@Timestamp timestamp
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION

		DELETE	Source_Join
		WHERE	Source_Join_Key = @Key
		AND		[Timestamp] = @Timestamp

		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Source_Join WHERE Source_Join_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SourceJoin_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SourceJoin_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SourceJoin_Delete TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SourceJoin_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SourceJoin_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SourceJoin_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SourceJoin_Delete TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_SourceJoin_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_SourceJoin_Update]
GO

/*===========================================================================*\
  Description:	Updates a record in the Source_Join table.

  Parameters:	@Key 		Source_Join_Key
		@Original 
		@Timestamp timestamp

  Created:	November 2003

  Last revision information:
    $Revision: 1 $
    $Date: 3/02/09 12:20 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SourceJoin_Update] 
	@Key CHAR(16),
	@Original bit,
	@Timestamp timestamp
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION

		UPDATE 	Source_Join 
		SET	Original = @Original
		WHERE	Source_Join_Key = @Key
		AND		[Timestamp] = @Timestamp

		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Source_Join WHERE Source_Join_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO
			
/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SourceJoin_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SourceJoin_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SourceJoin_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SourceJoin_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SourceJoin_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SourceJoin_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SourceJoin_Update TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Specimen_Delete') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_Specimen_Delete]
GO

/*===========================================================================*\
  Description:	Delete a record from the Specimen table.
		Ensures the Domain masks of the containing itmes are also updated.
		The assumpton is that a specimen being deleted is not a 
		container for any other specimens. They must have been either 
		deleted or moved to another container themselves.

  Parameters:	@SpecimenKey

  Created:	July 2003

  Last revision information:
    $Revision: 1 $
    $Date: 3/02/09 12:20 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Specimen_Delete]
	@SpecimenKey char(16),
	@Timestamp timestamp = NULL
AS
	DECLARE @ExistingCollectionKey char(16),
		@ExistingContainerKey char(16),
		@SpecimenMask int,
		@SUTimestamp timestamp

	/*-------------------------------------------------------------*\
	| Initialise variables.						|
	\*-------------------------------------------------------------*/
	SELECT		@ExistingCollectionKey = S.Parent_Collection_Collection_Unit_Key, 
			@ExistingContainerKey = CU.Current_Container_Collection_Unit_Key
	FROM		Specimen_Unit S
	INNER JOIN 	Collection_Unit CU ON CU.Collection_Unit_Key = S.Collection_Unit_Key
	WHERE		S.Collection_Unit_Key = @SpecimenKey

	-- Retrieve the mask of the preferred concept for specimen.
	EXECUTE	usp_Get_Concept_Domain_Mask_From_Specimen @SpecimenKey, @SpecimenMask OUTPUT

	/*-------------------------------------------------------------*\
	| Do the table delete first. Or the containers will still have	|
	| the specimen and its mask!					|
	\*-------------------------------------------------------------*/

	BEGIN TRANSACTION
		/*-------------------------------------------------------------*\
		| Before continuing, we need to save the timestamp, because the |
		| update that follows this, could update the timestamp itself	|
		| causing the final delete to fail.				|
		\*-------------------------------------------------------------*/
		SELECT	@SUTimestamp = [Timestamp]
		FROM	Specimen_Unit
		WHERE	Collection_Unit_Key = @SpecimenKey
		/*---------------------------------------------------------------------------*\
		  The Determination table has a relationship to the Specimen_Unit table and 
		  the Specimen_Unit has a relationship to the Determination table (similarly
		  for the Taxon_Determination table). Hence, the Preferred_Determination and
		  Preferred_Taxon_Determination fields in the Specimen_Unit table must be
		  both made NULL before they can be deleted.
		\*---------------------------------------------------------------------------*/
		UPDATE	Specimen_Unit
		SET	Preferred_Determination_Key = NULL,
			Preferred_Taxon_Determination_Key = NULL
		WHERE	Collection_Unit_Key = @SpecimenKey

		/*---------------------------------------------------------------------------*\
		  Specimen_Unit -> Determination -> Occurrence
		\*---------------------------------------------------------------------------*/
		-- Delete Determinations linked ONLY to Specimen being deleted.
		DELETE	Determination
		WHERE	Specimen_Collection_Unit_Key = @SpecimenKey
		AND	Occurrence_Key IS NULL

		-- Clear only reference to Specimen for Determinations also linked to Occurrence
		UPDATE	Determination
		SET	Specimen_Collection_Unit_Key = NULL
		WHERE	Specimen_Collection_Unit_Key = @SpecimenKey
		AND	Occurrence_Key IS NOT NULL

		IF @@Error <> 0 GOTO RollbackAndExit

		/*---------------------------------------------------------------------------*\
		  Specimen_Unit -> Taxon_Determination -> Taxon_Occurrence
		\*---------------------------------------------------------------------------*/
		-- Delete Taxon Determinations linked ONLY to Specimen being deleted.
		DELETE	Taxon_Determination
		WHERE	Specimen_Collection_Unit_Key = @SpecimenKey
		AND	Taxon_Occurrence_Key IS NULL

		-- Clear only reference to Specimen for Taxon Determinations also linked to Taxon Occurrence
		UPDATE 	Taxon_Determination
		SET	Specimen_Collection_Unit_Key = NULL
		WHERE	Specimen_Collection_Unit_Key = @SpecimenKey
		AND	Taxon_Occurrence_Key IS NOT NULL

		IF @@Error <> 0 GOTO RollbackAndExit

		/*-------------------------------------------------------------*\
		  Other tables.
		\*-------------------------------------------------------------*/
		DELETE	Specimen_Field_Data
		WHERE	Collection_Unit_Key = @SpecimenKey

		IF @@Error <> 0 GOTO RollbackAndExit

		DELETE	Specimen_Label		
		WHERE	Collection_Unit_Key = @SpecimenKey	

		IF @@Error <> 0 GOTO RollbackAndExit	

		DELETE	Specimen_Unit
		WHERE	Collection_Unit_Key = @SpecimenKey
		AND	(@Timestamp = @SUTimestamp OR (@Timestamp IS NULL))

		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Specimen_Unit WHERE Collection_Unit_Key = @SpecimenKey)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

		/*-------------------------------------------------------------*\
		 @Timestamp is null if proc called from a store related process. 
		 In that case , the Collection_Unit table will be handled by the 
		 store related process that called the proc, and not here.
		\*-------------------------------------------------------------*/
		IF @Timestamp IS NOT NULL
			-- If specimen is also a store, let the dedicated procedure deal with it
			IF EXISTS(SELECT * FROM Store WHERE Collection_Unit_Key = @SpecimenKey)
				EXECUTE	usp_Store_Delete @SpecimenKey
			ELSE
			-- Otherwise, just delete record from Collection_Unit table.
				DELETE	Collection_Unit
				WHERE	Collection_Unit_Key = @SpecimenKey

		IF @@Error <> 0 GOTO RollbackAndExit

		/*-------------------------------------------------------------*\
		  Now switch specimen bit OFF from container and collection.
		\*-------------------------------------------------------------*/
		-- Update the container mask
		EXECUTE	usp_CollectionUnit_Update_DomainMask @ExistingContainerKey, @SpecimenMask, 0
		-- Update the collection mask
		EXECUTE	usp_Collection_Update_DomainMask @ExistingCollectionKey, @SpecimenMask, 0

		IF @@Error <> 0 GOTO RollbackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Specimen_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Specimen_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Specimen_Delete TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimen_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimen_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimen_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Specimen_Delete TO [Dev - JNCC SQL]
END
GO

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
    $Revision: 1 $
    $Date: 3/02/09 12:20 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Specimen_Update]
	--for specimen_unit
	@Key char(16), 
	@ParentCollectionCollectionUnitKey char(16),
	@SpecimenTypeConceptKey char(16),
	@Confidential bit,
	@Dangerous bit,
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
	@SessionID char(16)
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
			Life_Sciences = @LifeSciences,
			Changed_Session_ID = @SessionID,
			Checked = @Checked 
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

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_SpecimenFieldData_Delete]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_SpecimenFieldData_Delete]
GO

/*===========================================================================*\
  Description:	Deletes a record from Specimen_Field_Data

  Parameters:	@Key

  Created:	November 2003

  Last revision information:
    $Revision: 1 $
    $Date: 3/02/09 12:20 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SpecimenFieldData_Delete]
	@Key char(16),
	@Timestamp timestamp
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION

		DELETE	Specimen_Field_Data
		WHERE	Specimen_Field_Data_Key = @Key
		AND		[Timestamp] = @Timestamp

		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Specimen_Field_Data WHERE Specimen_Field_Data_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SpecimenFieldData_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SpecimenFieldData_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SpecimenFieldData_Delete TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SpecimenFieldData_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SpecimenFieldData_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SpecimenFieldData_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SpecimenFieldData_Delete TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_SpecimenLabel_Delete]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_SpecimenLabel_Delete]
GO

/*===========================================================================*\
  Description:	Delete a record from the Specimen_Label table.

  Parameters:	@Key
		@Timestamp

  Created:	December 2003

  Last revision information:
    $Revision: 1 $
    $Date: 3/02/09 12:20 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SpecimenLabel_Delete]
	@Key char(16),
	@Timestamp timestamp
AS
	SET NOCOUNT OFF

	BEGIN TRANSACTION

		-- Delete record from Specimen_Label table.
		DELETE	Specimen_Label
		WHERE	Specimen_Label_Key = @Key
		AND		[Timestamp] = @Timestamp
		
		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Specimen_Label WHERE Specimen_Label_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SpecimenLabel_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SpecimenLabel_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SpecimenLabel_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SpecimenLabel_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SpecimenLabel_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SpecimenLabel_Delete TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_SpecimenLabel_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_SpecimenLabel_Update]
GO

/*===========================================================================*\
  Description:	Updates a record into the Specimen Label table

  Parameters:	@Key

		@Timestamp

  Created:	September 2003

  Last revision information:
    $Revision: 1 $
    $Date: 3/02/09 12:20 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SpecimenLabel_Update]
	@Key char(16),
	@CollectionUnitKey char(16),
	@IsInscription bit,
	@Position varchar(100),
	@Inscription ntext,
	@Translated text,
	@LanguageConceptKey varchar(4),
	@Comments text,
	@InferredAuthor tinyint,
	@AuthorNameKey char(16),
	@ConfidenceConceptKey char(16),
	@SessionID char(16),
	@Timestamp timestamp
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION
		
		UPDATE 	Specimen_Label
		SET 	Collection_Unit_Key = @CollectionUnitKey,
			Is_Inscription = @IsInscription,
			Label_Position = @Position,
			Inscription = @Inscription,
			Translated = @Translated,
			Translated_Language_Key = @LanguageConceptKey,
			Comments = @Comments,
			Inferred_Author = @InferredAuthor,
			Author_Name_Key = @AuthorNameKey,
			Confidence_Concept_Key = @ConfidenceConceptKey,
			Changed_Session_ID = @SessionID

		WHERE	Specimen_Label_Key = @Key
		AND		[Timestamp] = @Timestamp

		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Specimen_Label WHERE Specimen_Label_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SpecimenLabel_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SpecimenLabel_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SpecimenLabel_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SpecimenLabel_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SpecimenLabel_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SpecimenLabel_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SpecimenLabel_Update TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Store_Delete') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_Store_Delete]
GO

/*===========================================================================*\
  Description:	Delete a record from the Store table.
		If the store is also a specimen, the dedicated procedure to 
		delete a specimen is run, it will take care of properly updating
		all domain masks. If the store is just that, its mask should
		already have been dealt with and be 0. If not, something wrong
		probably happened.

  Parameters:	@StoreKey

  Created:	July 2003

  Last revision information:
    $Revision: 1 $
    $Date: 3/02/09 12:20 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Store_Delete]
	@StoreKey char(16),
	@Timestamp timestamp = NULL
AS
	BEGIN TRANSACTION
		-- Delete record from Store table.
		DELETE	Store
		WHERE	Collection_Unit_Key = @StoreKey
		AND	(	[Timestamp] = @Timestamp OR (@Timestamp IS NULL))

		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Store WHERE Collection_Unit_Key = @StoreKey)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

		-- Delete CollectionUnitRelation records that use this Collection_Unit_Key
		EXECUTE	usp_CollectionUnitRelations_Delete @StoreKey

		IF @@Error <> 0 GOTO RollBackAndExit

		/*-------------------------------------------------------------*\
		 @Timestamp is null if proc called from a specimen related process. 
		 In that case , the Collection_Unit table will be handled by the 
		 specimen related process that called the proc, and not here.
		\*-------------------------------------------------------------*/
		IF @Timestamp IS NOT NULL
			-- If store is also a specimen, let the dedicated procedure deal with it
			IF EXISTS(SELECT * FROM Specimen_Unit WHERE Collection_Unit_Key = @StoreKey)
				EXECUTE	usp_Specimen_Delete @StoreKey
			ELSE
			-- Otherwise, just delete record from Collection_Unit table.
				DELETE	Collection_Unit
				WHERE	Collection_Unit_Key = @StoreKey

		IF @@Error <> 0 GOTO RollBackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit:
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Store_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Store_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Store_Delete TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Store_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Store_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Store_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Store_Delete TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Store_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Store_Update]
GO

/*===========================================================================*\
  Description:	Updates a record in the Store table.
		Ensures the Domain masks of the store and its containers are
		also updated.

  Parameters:	@Key 
		@ItemName
		@CurrentContainerKey
		@UsualContainerKey
		@StoreTypeConceptKey
		@Comment 
		@SessionID 
		@CurrentLocationCode
		@UsualLocationCode 
		@Timestamp 

  Created:	July 2003

  Last revision information:
    $Revision: 1 $
    $Date: 3/02/09 12:20 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Store_Update]
	@Key char(16), 
	@ItemName varchar(100),
	@CurrentContainerKey char(16),
	@UsualContainerKey char(16),
	@StoreTypeConceptKey char(16),
	@Comment text,
	@SessionID char(16),
	@CurrentLocationCode varchar(30),
	@UsualLocationCode varchar(30),
	@Timestamp timestamp
AS
	DECLARE @ExistingContainerKey char(16),
		@StoreMask int,
		@ContainerMask int

	/*-------------------------------------------------------------*\
	  Initialise variables.
	\*-------------------------------------------------------------*/
	SELECT	@ExistingContainerKey = Current_Container_Collection_Unit_Key,
		@StoreMask = Domain_Mask
	FROM	Collection_Unit
	WHERE	Collection_Unit_Key = @Key

	/*-------------------------------------------------------------*\
	  If container changing, might need to update container mask.
	\*-------------------------------------------------------------*/
	IF @ExistingContainerKey <> @CurrentContainerKey
	BEGIN
		-- Work out the bit(s) to turn OFF
		SELECT	@ContainerMask = ~ Sum(DISTINCT Domain_Mask) & @StoreMask
		FROM	Collection_Unit CU
		WHERE	CU.Current_Container_Collection_Unit_Key = @ExistingContainerKey
		AND	CU.Collection_Unit_Key <> @Key
	END

	BEGIN TRANSACTION
		/*-------------------------------------------------------------*\
		  Do the table updates first. Or the containers will still have
		  the specimen and its mask!
		\*-------------------------------------------------------------*/
		UPDATE	Store
		SET	Item_Name = @ItemName,
			Store_Type_Concept_Key = @StoreTypeConceptKey,
			Comment = @Comment,
			Changed_Session_ID = @SessionID
		WHERE	Collection_Unit_Key = @Key
		AND		[Timestamp] = @Timestamp

		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Store WHERE Collection_Unit_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

		UPDATE	Collection_Unit
		SET	Current_Container_Collection_Unit_Key = @CurrentContainerKey,
			Usual_Container_Collection_Unit_Key = @UsualContainerKey,
			Usual_Location_Code = @UsualLocationCode,
			Current_Location_Code = @CurrentLocationCode,
			Changed_Session_ID = @SessionID
		WHERE	Collection_Unit_Key = @Key

		IF @@Error <> 0 GOTO RollbackAndExit

		/*-------------------------------------------------------------*\
		  Now switch the bits OFF, if necessary.
		\*-------------------------------------------------------------*/
		IF @ExistingContainerKey <> @CurrentContainerKey
		BEGIN
			-- Loop through all 32 bits and check which ones need to be switched OFF or left alone.
			-- From 10000000 00000000 00000000 00000000 down to 00000000 00000000 00000000 00000001
			DECLARE	@BitMask int
			SET @BitMask = 0x80000000

			-- 1 / 2 = 0, use this as stop condition
			WHILE @BitMask <> 0
			BEGIN
				-- If the bit is found ON for container, switch it OFF
				IF @ContainerMask & @BitMask = @BitMask
					-- Update the container mask
					EXECUTE	usp_CollectionUnit_Update_DomainMask @ExistingContainerKey, @BitMask, 0

				-- Set mask for next single bit in line
				SET @BitMask = @BitMask / 2
			END
		END

		/*-------------------------------------------------------------*\
		  And ON. Straight forward all bits in this case.
		\*-------------------------------------------------------------*/
		-- Update the *new* container mask
		EXECUTE	usp_CollectionUnit_Update_DomainMask @CurrentContainerKey, @StoreMask, 1

		IF @@Error <> 0 GOTO RollbackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit:
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Store_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Store_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Store_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Store_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Store_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Store_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Store_Update TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_StoreDiagram_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_StoreDiagram_Update]
GO

/*===========================================================================*\
  Description:	Updates a Diagram for a store

  Parameters:	@Key
		@XML
		@Timestamp

  Created:	Sept 2004

  Last revision information:
    $Revision: 1 $
    $Date: 3/02/09 12:20 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_StoreDiagram_Update]
	@Key char(16),
	@XML text,
	@Timestamp timestamp
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION

		UPDATE	Store
		SET	Diagram_XML = @Xml
		WHERE	Collection_Unit_Key = @Key
		AND		[Timestamp] = @Timestamp

		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Store WHERE Collection_Unit_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END	

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_StoreDiagram_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_StoreDiagram_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_StoreDiagram_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_StoreDiagram_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_StoreDiagram_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_StoreDiagram_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_StoreDiagram_Update TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_SubjectArea_Delete]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_SubjectArea_Delete]
GO

/*===========================================================================*\
  Description:	Deletes a Subject Area record.

  Parameters:	@Key	Subject_Area_Key
				@Timestamp

  Created:	December 2003

  Last revision information:
    $Revision: 1 $
    $Date: 3/02/09 12:20 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SubjectArea_Delete]
	@Key char(16),
	@Timestamp timestamp = NULL
AS

SET NOCOUNT ON

	BEGIN TRANSACTION

		DELETE 
		FROM 	Subject_Area
		WHERE	Subject_Area_Key = @Key
		AND		([Timestamp] = @Timestamp OR (@Timestamp IS NULL))

		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Subject_Area WHERE Subject_Area_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

	COMMIT TRANSACTION
	RETURN 0
	
RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SubjectArea_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SubjectArea_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SubjectArea_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SubjectArea_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SubjectArea_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SubjectArea_Delete TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_SubjectArea_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_SubjectArea_Update]
GO

/*===========================================================================*\
  Description:	Updates a record in the Subject_Area table.

  Parameters:	@Key
		@ItemName 
		@Comment 
		@SessionID 
		@Timestamp 

  Created:	November 2003

  Last revision information:
    $Revision: 1 $
    $Date: 3/02/09 12:20 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SubjectArea_Update]
	@Key char(16),
	@ItemName varchar(100),
	@Comment text,
	@SessionID char(16),
	@Timestamp timestamp
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION
		
		UPDATE 	Subject_Area
		SET 	Item_Name = @ItemName, 
			Comment = @Comment,
			Changed_Session_ID = @SessionID
		WHERE	Subject_Area_Key = @Key
		AND		[Timestamp] = @Timestamp

		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Subject_Area WHERE Subject_Area_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SubjectArea_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SubjectArea_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SubjectArea_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SubjectArea_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SubjectArea_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SubjectArea_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SubjectArea_Update TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Synonym_Delete]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Synonym_Delete]
GO

/*===========================================================================*\
  Description:	'Delete' a Synonym. This doesn't actually delete the concept
		that is the selected synonym, but gives the concept a new
		meaning key, so it is no longer a synonym.

  Parameters:	@Key
		@RecordsAffected  OUTPUT

  Created:	March 2004

  Last revision information:
    $Revision: 1 $
    $Date: 3/02/09 12:20 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Synonym_Delete]
	@Key char(16),
	@Timestamp timestamp,
	@RecordsAffected int OUTPUT
AS
SET NOCOUNT OFF
	
	BEGIN TRANSACTION
		DECLARE @MeaningKey char(16),
			@Error int

		/*-------------------------------------------------------------*\
		  Create a new Meaning Key
		\*-------------------------------------------------------------*/
		EXEC spNextKey 'Meaning', @MeaningKey OUTPUT
		IF @@ERROR <> 0 GOTO RollbackAndExit

		INSERT INTO Meaning (
			Meaning_Key
		) VALUES (
			@MeaningKey
		)
		IF @@Error <> 0 GOTO RollbackAndExit

		/*-------------------------------------------------------------*\
		  Update the concept record
		\*-------------------------------------------------------------*/
		UPDATE	Concept
		SET 	Meaning_Key = @MeaningKey,	
			List_Preferred = 1
		WHERE	Concept_Key = @Key
		AND	[Timestamp] = @Timestamp

		/*-------------------------------------------------------------*\
		  Get the number of records affected and see if any errors.
		\*-------------------------------------------------------------*/	
		SELECT	@RecordsAffected = @@RowCount,
			@Error = @@Error
		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Concept WHERE Concept_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit:
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_Synonym_Delete failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Synonym_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Synonym_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Synonym_Delete TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Synonym_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Synonym_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Synonym_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Synonym_Delete TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Task_Delete]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Task_Delete]
GO

/*===========================================================================*\
  Description:	Delete a record from the Conservation_Task table.

  Parameters:	@Key
		@Timestamp

  Created:	December 2003

  Last revision information:
    $Revision: 1 $
    $Date: 3/02/09 12:20 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Task_Delete]
	@Key char(16),
	@Timestamp timestamp
AS
	SET NOCOUNT OFF

	BEGIN TRANSACTION

		-- Delete all links between Collection units and this task.
		DELETE	Collection_Unit_Task
		WHERE	Conservation_Task_Key = @Key

		-- Delete all sources
		EXEC usp_References_Delete_ForSources 'Conservation_Task', @Key

		-- Delete record from Conservation_Task table.
		DELETE	Conservation_Task
		WHERE	Conservation_Task_Key = @Key
		AND		[Timestamp] = @Timestamp

		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount
	
		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Conservation_Task WHERE Conservation_Task_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Task_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Task_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Task_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Task_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Task_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Task_Delete TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Task_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Task_Update]
GO

/*===========================================================================*\
  Description:	Updates a record into the Conservation_Task table

  Parameters:	@Key
		@ConservationCheckKey 
		@ConservationJobKey
		@SetVagueDateStart
		@SetVagueDateEnd 
		@SetVagueDateType
		@Status
		@TypeConceptKey
		@Priority
		@Duration
		@DurationUnitConceptKey 
		@IdentifierNameKey
		@TaskAction
		@Comment
		@SessionID
		@Timestamp 

  Created:	September 2003

  Last revision information:
    $Revision: 1 $
    $Date: 3/02/09 12:20 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Task_Update]
	@Key char(16),
	@ConservationCheckKey char(16), 
	@ConservationJobKey char(16),
	@SetVagueDateStart int, 
	@SetVagueDateEnd int, 
	@SetVagueDateType varchar(2),
	@Status tinyint, 
	@TypeConceptKey char(16), 
	@Priority tinyint, 
	@Duration float, 
	@DurationUnitConceptKey char(16),
	@IdentifierNameKey char(16), 
	@TaskAction text, 
	@Comment text, 
	@SessionID char(16), 
	@Timestamp timestamp

AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION
		
		UPDATE 	Conservation_Task
		SET 	Conservation_Check_Key = @ConservationCheckKey, 
			Conservation_Job_Key = @ConservationJobKey,
			Set_Vague_Date_Start = @SetVagueDateStart, 
			Set_Vague_Date_End = @SetVagueDateEnd, 
			Set_Vague_Date_Type = @SetVagueDateType,
			Status = @Status, 
			Type_Concept_Key = @TypeConceptKey, 
			Priority = @Priority, 
			Duration = @Duration, 
			Duration_Unit_Concept_Key = @DurationUnitConceptKey,
			Identifier_Name_Key = @IdentifierNameKey, 
			Task_Action = @TaskAction, 
			Comment = @Comment, 
			Changed_Session_ID = @SessionID

		WHERE	Conservation_Task_Key = @Key
		AND		[Timestamp] = @Timestamp

		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Conservation_Task WHERE Conservation_Task_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Task_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Task_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Task_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Task_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Task_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Task_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Task_Update TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_TaxonDetermination_Delete]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_TaxonDetermination_Delete]
GO

/*===========================================================================*\
  Description:	Delete a record in the Taxon Determination table.
		Ensures the Domain mask of the specimen is also updated.

  Parameters:	@DeterminationKey
		@IsForSpecimen

  Created:	November 2003

  Last revision information:
    $Revision: 1 $
    $Date: 3/02/09 12:20 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_TaxonDetermination_Delete]
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
		-- Check for Specimen.
		IF EXISTS(SELECT * FROM Specimen_Unit WHERE Preferred_Taxon_Determination_Key = @DeterminationKey) BEGIN
			SELECT	@WasPreferredForSpecimen = 1,
				@SpecimenKey = Collection_Unit_Key
			FROM	Specimen_Unit
			WHERE	Preferred_Taxon_Determination_Key = @DeterminationKey

			-- Need to clear the field because of referential integrity.
			UPDATE 	Specimen_Unit
			SET	Preferred_Taxon_Determination_Key = NULL
			WHERE	Collection_Unit_Key = @SpecimenKey
			IF @@Error <> 0 GOTO RollbackAndExit
		END

		-- Check for Occurrence.
		SELECT		@ConceptKey = TDM.Concept_Key,
				@OccurrenceKey = TD.Taxon_Occurrence_Key,
				@WasPreferredForOccurrence = TD.Preferred,
				@SpecimenKey = TD.Specimen_Collection_Unit_Key
		FROM		Taxon_Determination TD
		INNER JOIN 	Taxon_Dictionary_Concept_Mapping TDM ON TDM.Taxon_List_Item_Key = TD.Taxon_List_Item_Key
		WHERE		TD.Taxon_Determination_Key = @DeterminationKey

		/*-------------------------------------------------------------*\
		  Delete either from Determination table, or just the link.
		\*-------------------------------------------------------------*/
		IF @OccurrenceKey IS NOT NULL AND @SpecimenKey IS NOT NULL 
			-- Determination linked to both a specimen AND an occurrence
			IF @IsForSpecimen = 1
				-- Deleting for specimen, clear the link and keep Occurrence key
				UPDATE	Taxon_Determination
				SET	Specimen_Collection_Unit_Key = NULL
				WHERE	Taxon_Determination_Key = @DeterminationKey
				AND		[Timestamp] = @Timestamp
			ELSE
				-- Deleting for occurrence, clear the link and keep Spceimen key
				UPDATE	Taxon_Determination
				SET	Taxon_Occurrence_Key = NULL
				WHERE	Taxon_Determination_Key = @DeterminationKey
				AND		[Timestamp] = @Timestamp
		ELSE
			-- Only either specimen or occurrence key, safe to delete.
			DELETE	Taxon_Determination
			WHERE	Taxon_Determination_Key = @DeterminationKey
			AND		[Timestamp] = @Timestamp

		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Taxon_Determination WHERE Taxon_Determination_Key = @DeterminationKey)
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
			IF EXISTS(SELECT * FROM Taxon_Determination WHERE Taxon_Occurrence_Key = @OccurrenceKey)
			BEGIN
				-- For occurrences, it all happens in Taxon_Determination table.
				IF @WasPreferredForOccurrence = 1 BEGIN
					SELECT	TOP 1 @NewPreferredDeterminationKey = Taxon_Determination_Key
					FROM	Taxon_Determination
					WHERE	Taxon_Occurrence_Key = @OccurrenceKey

					UPDATE 	Taxon_Determination
					SET	Preferred = 1
					WHERE	Taxon_Determination_Key = @NewPreferredDeterminationKey
					IF @@Error <> 0 GOTO RollbackAndExit
				END

				-- For specimen, it happens in Specimen_Unit table.
				IF @WasPreferredForSpecimen = 1 BEGIN
					SELECT	TOP 1 @NewPreferredDeterminationKey = Taxon_Determination_Key
					FROM	Taxon_Determination
					WHERE	Specimen_Collection_Unit_Key = @SpecimenKey

					UPDATE 	Specimen_Unit
					SET	Preferred_Taxon_Determination_Key = @NewPreferredDeterminationKey
					WHERE	Collection_Unit_Key = @SpecimenKey
					IF @@Error <> 0 GOTO RollbackAndExit
				END

				-- Get concept key of new preferred determination
				SELECT		@NewConceptKey = TDM.Concept_Key
				FROM		Taxon_Determination TD
				INNER JOIN	Taxon_Dictionary_Concept_Mapping TDM ON TDM.Taxon_List_Item_Key = TD.Taxon_List_Item_Key
				WHERE		Taxon_Determination_Key = @NewPreferredDeterminationKey
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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TaxonDetermination_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_TaxonDetermination_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_TaxonDetermination_Delete TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_TaxonDetermination_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_TaxonDetermination_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_TaxonDetermination_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_TaxonDetermination_Delete TO [Dev - JNCC SQL]
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
		@IsForSpecimen		Indicates whether to update preferred 
					flag in Specimen_Unit or Determination.

  Created:	July 2003

  Last revision information:
    $Revision: 1 $
    $Date: 3/02/09 12:20 $
    $Author: Pauldavies $

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
			Used_Specimen = @UsedSpecimen
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

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_TermVersion_Delete]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_TermVersion_Delete]
GO

/*===========================================================================*\
  Description:	Delete a record from the Term_Version table.

  Parameters:	@Key
		@Timestamp

  Created:	December 2003

  Last revision information:
    $Revision: 1 $
    $Date: 3/02/09 12:20 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_TermVersion_Delete]
	@Key char(16),
	@Timestamp timestamp = NULL,
	@UserID char(16) = NULL,
	@SyncTaxonDict bit = 0
AS
	SET NOCOUNT OFF

	BEGIN TRANSACTION

		/*============================================================*\
		  See if the user wants any associated taxon dictionary
		  records be deleted with the Term Version.
		\*============================================================*/
		IF @SyncTaxonDict = 1 
		BEGIN
			/*--------------------------------------------------------*\
			  Check that the Taxon_Version table exists before
			  attempting any of this deletion. 
			\*--------------------------------------------------------*/
			IF EXISTS (SELECT *
		   				FROM	SysObjects 
						WHERE	Id = Object_Id(N'[dbo].[Taxon_Version]')
						AND		Type = 'U')
			BEGIN
				DECLARE @TaxonVersionKey char(16)

				SELECT 	@TaxonVersionKey = Taxon_Version_Key
				FROM	Taxon_Dictionary_Term_Version_Mapping
				WHERE	Term_Version_Key = @Key

				IF @@Error <> 0 GOTO RollbackAndExit

				DELETE	Taxon_Dictionary_Term_Version_Mapping	
				WHERE	Taxon_Version_Key = @TaxonVersionKey
				AND		Term_Version_Key = @Key

				IF @@Error <> 0 GOTO RollbackAndExit

				DELETE	Taxon_Version_Key
				WHERE	Taxon_Version_Key_1 = @TaxonVersionKey
				OR		Taxon_Version_Key_2 = @TaxonVersionKey

				IF @@Error <> 0 GOTO RollbackAndExit

				DELETE	Taxon_Common_Name
				WHERE	Taxon_Version_Key = @TaxonVersionKey

				IF @@Error <> 0 GOTO RollbackAndExit

				DELETE	Taxon_Taxon_Association
				WHERE	Taxon_Version_Key_1 = @TaxonVersionKey
				OR 		Taxon_Version_Key_2 = @TaxonVersionKey

				IF @@Error <> 0 GOTO RollbackAndExit

				DELETE	Taxon_Biotope_Association
				WHERE	Taxon_Version_Key = @TaxonVersionKey

				IF @@Error <> 0 GOTO RollbackAndExit

				UPDATE	Taxon_Version
				SET		Attribute = NULL,
						Authority = NULL,
						Date_From = NULL,
						Date_To = NULL,
						Comment = NULL,
						Validation_Level = 0,
						Uk_Native = 0,
						Quality = NULL,
						Source_Key = NULL,
						Changed_By = @UserID,
						Changed_Date = GetDate()
				WHERE	Taxon_Version_Key = @TaxonVersionKey
			END
		END

		-- Delete record from Term_Version table.
		DELETE	Term_Version
		WHERE	Term_Version_Key = @Key
		AND		([Timestamp] = @Timestamp OR (@Timestamp IS NULL))
	
		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Term_Version WHERE Term_Version_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TermVersion_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_TermVersion_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_TermVersion_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_TermVersion_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_TermVersion_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_TermVersion_Delete TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_TermVersion_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_TermVersion_Update]
GO

/*===========================================================================*\
  Description:	Updates a record in the Term_Version table. However, if
				(@VersionLabel IS NULL AND @AuthorAndDate IS NULL) OR 
				(@VersionLabel = '' AND @AuthorAndDate = '') then
				the Term_Version record will be deleted. This method is used
				to delete Term Versions, rather than 'usp_TermVersion_Delete'
				because it is more intelligent using this method, than brute
				force deletion.

  Parameters:	@Key (Term_Version_Key)
				@ConceptKey 
				@VersionLabel
				@AuthorAndDate
				@SessionID 
				@Timestamp 
				@SyncTaxonDict
				@RecordsAffected OUTPUT

  Created:	December 2003

  Last revision information:
    $Revision: 1 $
    $Date: 3/02/09 12:20 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_TermVersion_Update]
	@Key char(16),
	@ConceptKey char(16),
	@VersionLabel varchar(100),
	@AuthorAndDate varchar(100),
	@SessionID char(16),
	@Timestamp timestamp = NULL,
	@SyncTaxonDict bit = 0,
	@RecordsAffected int =1 OUTPUT
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION

		DECLARE @TermKey char(16)
		DECLARE @TaxonVersionKey char(16)
		DECLARE @Error int

		SELECT 	@TermKey = Term_Key
		FROM	Concept
		WHERE	Concept_Key = @ConceptKey

		SELECT 	@TaxonVersionKey = Taxon_Version_Key
		FROM	Taxon_Dictionary_Term_Version_Mapping
		WHERE	Term_Version_Key = @Key


		IF @@Error <> 0 GOTO RollbackAndExit


		IF (@VersionLabel IS NULL AND @AuthorAndDate IS NULL) OR 
				(@VersionLabel = '' AND @AuthorAndDate = '') 
		BEGIN
			/*============================================================*\
			  See if the user wants any associated taxon dictionary
			  records be deleted with the Term Version.
			\*============================================================*/
			IF @SyncTaxonDict = 1 
			BEGIN
				/*--------------------------------------------------------*\
				  Check that the Taxon_Version table exists before
				  attempting any of this deletion. 
				\*--------------------------------------------------------*/
				IF EXISTS (SELECT *
			   				FROM	SysObjects 
							WHERE	Id = Object_Id(N'[dbo].[Taxon_Version]')
							AND		Type = 'U')
				BEGIN

					DELETE	Taxon_Dictionary_Term_Version_Mapping	
					WHERE	Taxon_Version_Key = @TaxonVersionKey
					AND		Term_Version_Key = @Key
	
					IF @@Error <> 0 GOTO RollbackAndExit
	
					DELETE	Taxon_Version_Relation
					WHERE	Taxon_Version_Key_1 = @TaxonVersionKey
					OR		Taxon_Version_Key_2 = @TaxonVersionKey
	
					IF @@Error <> 0 GOTO RollbackAndExit
	
					DELETE	Taxon_Common_Name
					WHERE	Taxon_Version_Key = @TaxonVersionKey
	
					IF @@Error <> 0 GOTO RollbackAndExit
	
					DELETE	Taxon_Taxon_Association
					WHERE	Taxon_Version_Key_1 = @TaxonVersionKey
					OR 		Taxon_Version_Key_2 = @TaxonVersionKey
	
					IF @@Error <> 0 GOTO RollbackAndExit
	
					DELETE	Taxon_Biotope_Association
					WHERE	Taxon_Version_Key = @TaxonVersionKey
	
					IF @@Error <> 0 GOTO RollbackAndExit

					UPDATE	Taxon_Version
					SET		Attribute = NULL,
							Authority = NULL,
							Date_From = NULL,
							Date_To = NULL,
							Comment = NULL,
							Validation_Level = 0,
							Uk_Native = 0,
							Quality = NULL,
							Source_Key = NULL
					WHERE	Taxon_Version_Key = @TaxonVersionKey

					IF @@Error <> 0 GOTO RollbackAndExit

					--Also need to keep the index up to date
					UPDATE ITN
					SET ITN.Authority=Null
					FROM Index_Taxon_Name ITN
					INNER JOIN Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key=ITN.Taxon_List_Item_Key
					WHERE TLI.Taxon_Version_Key=@TaxonVersionKey

					IF @@Error <> 0 GOTO RollbackAndExit

				END
			END

			-- When clearing a term version, remove the link to the record		
			UPDATE 	Concept
			SET 	Term_Version_Key = NULL
			WHERE 	Concept_Key = @ConceptKey			
			
			-- And remove the Term_Version record if not referred to anymore
			DELETE TV
			FROM Term_Version TV
			LEFT JOIN Concept C ON C.Term_Version_Key=TV.Term_Version_Key
			WHERE TV.Term_Version_Key=@Key
			AND C.Concept_Key IS NULL
			
			SELECT @RecordsAffected = 1
		END
		ELSE BEGIN
			UPDATE 	Term_Version
			SET 	Term_Key = @TermKey,
					Version_Label = @VersionLabel,
					Author_And_Date = @AuthorAndDate,
					Changed_Session_ID = @SessionID
			WHERE	Term_Version_Key = @Key
			AND		([Timestamp] = @Timestamp) OR (@Timestamp IS NULL)
	
			SELECT	@RecordsAffected = @@RowCount,
				@Error = @@Error
	
			IF @Error <> 0 GOTO RollbackAndExit 
	
			IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Term_Version WHERE Term_Version_Key = @Key)
			BEGIN
				RAISERROR('Record updated by another user', 16, 1)
				GOTO RollbackAndExit
			END
	
			/*-------------------------------------------------------------*\
			  Update Concept to point to new version
			\*-------------------------------------------------------------*/
			UPDATE 	Concept
			SET 	Term_Version_Key = @Key
			WHERE 	Concept_Key = @ConceptKey
	
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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TermVersion_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_TermVersion_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_TermVersion_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_TermVersion_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_TermVersion_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_TermVersion_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_TermVersion_Update TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_TermVersionRelation_Delete]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_TermVersionRelation_Delete]
GO

/*===========================================================================*\
  Description:	Deletes a record from the TermVersionRelation table.

  Parameters:	@Key		Term_Version_Relation_Key
		@Timestamp

  Created:	December 2003

  Last revision information:
    $Revision: 1 $
    $Date: 3/02/09 12:20 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_TermVersionRelation_Delete]
	@Key char(16),
	@Timestamp timestamp
AS

SET NOCOUNT ON

	/*-------------------------------------------------------------*\
	  Wrap everything in one transaction.
	\*-------------------------------------------------------------*/
	BEGIN TRANSACTION

		/*-------------------------------------------------------------*\
		  Update in Meaning_Relation.
		\*-------------------------------------------------------------*/
		DELETE 	Term_Version_Relation
		WHERE	Term_Version_Relation_Key = @Key	
		AND		[Timestamp] = @Timestamp
		
		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Term_Version_Relation WHERE Term_Version_Relation_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

	/*-------------------------------------------------------------*\
	  All went well, so commit.
	\*-------------------------------------------------------------*/
	COMMIT TRANSACTION

RETURN 0

RollBackAndExit: 
	/*-------------------------------------------------------------*\
	  Cancel any changes, or left-overs might mess up some tables.
	\*-------------------------------------------------------------*/
	ROLLBACK TRANSACTION

GO

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TermVersionRelation_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_TermVersionRelation_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_TermVersionRelation_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_TermVersionRelation_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_TermVersionRelation_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_TermVersionRelation_Delete TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_TermVersionRelation_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_TermVersionRelation_Update]
GO

/*===========================================================================*\
  Description:	Updates a record in the Term_Version_Relation table.

  Parameters:	@Key
		@FromConceptKey
		@ToConceptKey
		@ThesaurusRelationTypeKey
		@Multiplicity
		@Comment
		@SessionID
		@SystemSuppliedData

  Created:	December 2003

  Last revision information:
    $Revision: 1 $
    $Date: 3/02/09 12:20 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_TermVersionRelation_Update]
	@Key char(16),
	@FromConceptKey char(16),
	@ToConceptKey char(16),
	@ThesaurusRelationTypeKey char(16),
	@Multiplicity float = NULL,
	@Comment text = NULL,
	@SessionID char(16),
	@Timestamp timestamp

AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	/*-------------------------------------------------------------*\
	  Wrap everything in one transaction.
	\*-------------------------------------------------------------*/
	BEGIN TRANSACTION

		DECLARE @FromTermVersionKey char(16)
		DECLARE @ToTermVersionKey char(16)

		SELECT 	@FromTermVersionKey = Term_Version_Key
		FROM	Concept
		WHERE	Concept_Key = @FromConceptKey 

		SELECT 	@ToTermVersionKey = Term_Version_Key
		FROM	Concept
		WHERE	Concept_Key = @ToConceptKey 

		/*-------------------------------------------------------------*\
		  Update record in Term_Version_Relation.
		\*-------------------------------------------------------------*/
		UPDATE	Term_Version_Relation
		SET	From_Concept_Key = @FromConceptKey,
			To_Concept_Key = @ToConceptKey,
			From_Term_Version_Key = @FromTermVersionKey,
			To_Term_Version_Key = @ToTermVersionKey,
			Thesaurus_Relation_Type_Key = @ThesaurusRelationTypeKey,
			Multiplicity = @Multiplicity,
			Comment = @Comment,
			Changed_Session_ID = @SessionID
		WHERE	Term_Version_Relation_Key = @Key
		AND		[Timestamp] = @Timestamp	
	
		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Term_Version_Relation WHERE Term_Version_Relation_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

	/*-------------------------------------------------------------*\
	  All went well, so commit.
	\*-------------------------------------------------------------*/
	COMMIT TRANSACTION

RETURN 0

RollBackAndExit: 
	/*-------------------------------------------------------------*\
	  Cancel any changes, or left-overs might mess up some tables.
	\*-------------------------------------------------------------*/
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TermVersionRelation_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_TermVersionRelation_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_TermVersionRelation_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_TermVersionRelation_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_TermVersionRelation_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_TermVersionRelation_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_TermVersionRelation_Update TO [Dev - JNCC SQL]
END

GO
			
/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ThesaurusFact_Delete]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ThesaurusFact_Delete]
GO

/*===========================================================================*\
  Description:	Delete a record from the Thesaurus_Fact table.

  Parameters:	@Key		Thesaurus Fact key
		@Timestamp

  Created:	December 2003

  Last revision information:
    $Revision: 1 $
    $Date: 3/02/09 12:20 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ThesaurusFact_Delete]
	@Key char(16),
	@Timestamp timestamp = NULL,
	@SyncTaxonDict bit = 0
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION

		/*============================================================*\
		  See if the user wants any associated taxon dictionary
		  records be deleted with the concept.
		\*============================================================*/
		IF @SyncTaxonDict = 1 
		BEGIN
			/*--------------------------------------------------------*\
			  Check that the Taxon_Fact table exists before
			  attempting any of this deletion. 		
			\*--------------------------------------------------------*/
			IF EXISTS (SELECT *
		   			FROM   SysObjects 
					WHERE  Id = Object_Id(N'[dbo].[Taxon_Fact]')
					AND 	  Type = 'U')
			BEGIN
				DECLARE	@TaxonFactKey char(16)

				SELECT 	@TaxonFactKey = Taxon_Fact_Key
				FROM 	Taxon_Dictionary_Thesaurus_Fact_Mapping
				WHERE	Thesaurus_Fact_Key = @Key

				IF @@Error <> 0 GOTO RollbackAndExit

				DELETE	Taxon_Dictionary_Thesaurus_Fact_Mapping
				WHERE	Thesaurus_Fact_Key = @Key

				IF @@Error <> 0 GOTO RollbackAndExit

				DELETE	Taxon_Fact
				WHERE	Taxon_Fact_Key = @TaxonFactKey

				IF @@Error <> 0 GOTO RollbackAndExit
			END
		END	
		ELSE BEGIN
			DELETE	Taxon_Dictionary_Thesaurus_Fact_Mapping
			WHERE	Thesaurus_Fact_Key = @Key

			IF @@Error <> 0 GOTO RollbackAndExit
		END

		DELETE	Thesaurus_Fact
		WHERE	Thesaurus_Fact_Key = @Key
		AND		(@Timestamp = Timestamp OR @Timestamp IS NULL)

		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Thesaurus_Fact WHERE Thesaurus_Fact_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ThesaurusFact_Delete') AND SysStat & 0xf = 4)
BEGIN
   	PRINT 'Setting up security on procedure usp_ThesaurusFact_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ThesaurusFact_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ThesaurusFact_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ThesaurusFact_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
       	GRANT EXECUTE ON dbo.usp_ThesaurusFact_Delete TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ThesaurusFact_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ThesaurusFact_Update]
GO

/*===========================================================================*\
  Description:	Updates a record in the Thesaurus_Fact table

  Parameters:	@Key	Thesaurus_Fact_Key

  Created:	December 2003

  Last revision information:
    $Revision: 1 $
    $Date: 3/02/09 12:20 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ThesaurusFact_Update]
	@Key char(16),
	@ItemName varchar(100),
	@Data text,
	@MeaningKey char(16),
	@ConceptKey char(16),
	@TermVersionKey char(16),
	@RelatedTermVersions bit,
	@Inherited bit,
	@LanguageKey varchar(4),
	@FactTypeMeaningKey char(16),
	@FactTypeMeaningName varchar(100),
	@FactVagueDateStart int,
	@FactVagueDateEnd int,
	@FactVagueDateType varchar(2) = NULL,
	@SessionID char(16), 
	@SystemSuppliedData bit = NULL,
	@Timestamp timestamp
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	/*-------------------------------------------------------------*\
	  Wrap everything in one transaction.
	\*-------------------------------------------------------------*/
	BEGIN TRANSACTION

		DECLARE @FactTypeConceptKey char(16)

		-- The combo box stores the meaning key and the item name. This meaning key
		-- needs to be converted to a concept key. Because many concepts can
		-- share the same meaning key, we have to use the meaning key and the item name.
		SELECT 		@FactTypeConceptKey = Concept_Key
		FROM 		Concept AS C
		INNER JOIN 	Term AS T ON T.Term_Key = C.Term_Key
		WHERE 		C.Meaning_Key = @FactTypeMeaningKey
		AND 		T.Item_Name = @FactTypeMeaningName

		UPDATE	Thesaurus_Fact
		SET	Item_Name = @ItemName,
			Data = @Data,
			Meaning_Key = @MeaningKey,
			Concept_Key = @ConceptKey,
			Term_Version_Key = @TermVersionKey,
			Related_Term_Versions = @RelatedTermVersions,
			Inherited = @Inherited,
			Language_Key = @LanguageKey,
			Changed_Session_ID = @SessionID,
			Fact_Vague_Date_Start = @FactVagueDateStart,
			Fact_Vague_Date_End = @FactVagueDateEnd,
			Fact_Vague_Date_Type = IsNull(@FactVagueDateType, 'U'),
			Fact_Type_Concept_Key = @FactTypeConceptKey,
			System_Supplied_Data = IsNull(@SystemSuppliedData, 0)
		WHERE	Thesaurus_Fact_Key = @Key
		AND		@Timestamp = Timestamp

		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Thesaurus_Fact WHERE Thesaurus_Fact_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

	COMMIT TRANSACTION

	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ThesaurusFact_Update') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_ThesaurusFact_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
       	GRANT EXECUTE ON dbo.usp_ThesaurusFact_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ThesaurusFact_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ThesaurusFact_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ThesaurusFact_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
       	GRANT EXECUTE ON dbo.usp_ThesaurusFact_Update TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ThesaurusRelationType_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ThesaurusRelationType_Update]
GO

/*===========================================================================*\
  Description:	Updates a record in the Thesaurus_Relation_Type table.

  Parameters:	@Key	(Thesaurus_Relation_Type_Key)
		@SemanticRelationKey 
		@ItemName
		@ForwardTerm 
		@ReverseTerm 
		@SessionID 
		@Timestamp
		@Meaning (optional)
		@Concept (optional) 
		@TermVersion (optional)
		@Occurrence (optional) 
		@CollectionUnit (optional) 

  Created:	December 2003

  Last revision information:
    $Revision: 1 $
    $Date: 3/02/09 12:20 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ThesaurusRelationType_Update]
	@Key char(16),
	@SemanticRelationKey char(16),
	@ItemName varchar(100),
	@ForwardTerm varchar(100),
	@ReverseTerm varchar(100),
	@SessionID char(16),
	@Timestamp timestamp,
	@Meaning bit = NULL,
	@Concept bit = NULL,
	@TermVersion bit = NULL,
	@Occurrence bit = NULL,
	@CollectionUnit bit = NULL			
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION

		UPDATE	Thesaurus_Relation_Type
		SET	Semantic_Relation_Key = @SemanticRelationKey,
			Item_Name = @ItemName,
			Forward_Term = @ForwardTerm,
			Reverse_Term = @ReverseTerm,
			Changed_Session_ID = @SessionID
		WHERE	Thesaurus_Relation_Type_Key = @Key
		AND		[Timestamp] = @Timestamp

		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Thesaurus_Relation_Type WHERE Thesaurus_Relation_Type_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

		IF @Meaning = 1 EXEC usp_ThesaurusRelationTypeUsage_Insert @Key, 1, @SessionID
		ELSE EXEC usp_ThesaurusRelationTypeUsage_Delete @Key, 1
		IF @Concept = 1 EXEC usp_ThesaurusRelationTypeUsage_Insert @Key, 2, @SessionID
		ELSE EXEC usp_ThesaurusRelationTypeUsage_Delete @Key, 2
		IF @TermVersion = 1 EXEC usp_ThesaurusRelationTypeUsage_Insert @Key, 3, @SessionID
		ELSE EXEC usp_ThesaurusRelationTypeUsage_Delete @Key, 3
		IF @Occurrence = 1 EXEC usp_ThesaurusRelationTypeUsage_Insert @Key, 4, @SessionID
		ELSE EXEC usp_ThesaurusRelationTypeUsage_Delete @Key, 4
		IF @CollectionUnit = 1 EXEC usp_ThesaurusRelationTypeUsage_Insert @Key, 5, @SessionID
		ELSE EXEC usp_ThesaurusRelationTypeUsage_Delete @Key, 5

		IF @@Error <> 0 GOTO RollbackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ThesaurusRelationType_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ThesaurusRelationType_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ThesaurusRelationType_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ThesaurusRelationType_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ThesaurusRelationType_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ThesaurusRelationType_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ThesaurusRelationType_Update TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_UserDomainAccess_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_UserDomainAccess_Update]
GO

/*===========================================================================*\
  Description:	Updates a record in the User_Domain_Access table.

  Parameters:	@Key		User_Domain_Access_Key
		@Browse
		@QuickEntry
		@Add
		@Edit
		@Timestamp
		@SessionID

  Created:	December 2003

  Last revision information:
    $Revision: 1 $
    $Date: 3/02/09 12:20 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_UserDomainAccess_Update]
	@Key char(16),
	@Browse bit,
	@QuickEntry bit,
	@Add bit,
	@Edit bit,
	@Timestamp timestamp,
	@SessionID char(16)
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION

		UPDATE 	User_Domain_Access
		SET	Allow_Browse = @Browse,
			Allow_Quick_Entry = @QuickEntry,
			Allow_Add = @Add,
			Allow_Edit = @Edit,
			Changed_Session_ID = @SessionID
		WHERE	User_Domain_Access_Key = @Key
		AND		[Timestamp] = @Timestamp

		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM User_Domain_Access WHERE User_Domain_Access_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_UserDomainAccess_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_UserDomainAccess_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_UserDomainAccess_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_UserDomainAccess_Update TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Valuation_Delete]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Valuation_Delete]
GO

/*===========================================================================*\
  Description:	Delete a record from the Valuation table.

  Parameters:	@Key
		@Timestamp

  Created:	December 2003

  Last revision information:
    $Revision: 1 $
    $Date: 3/02/09 12:20 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Valuation_Delete]
	@Key char(16),
	@Timestamp timestamp
AS
	SET NOCOUNT OFF

	BEGIN TRANSACTION

		-- Delete record from Valuation table.
		DELETE	Valuation
		WHERE	Valuation_Key = @Key
		AND		[Timestamp] = @Timestamp
	
		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Valuation WHERE Valuation_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Valuation_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Valuation_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Valuation_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Valuation_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Valuation_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Valuation_Delete TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Valuation_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Valuation_Update]
GO

/*===========================================================================*\
  Description:	Updates a record in the Valuation table

  Parameters:	@Key
		@VagueDateStart
		@VagueDateEnd
		@VagueDateType
		@RefNumber
		@TypeConceptKey
		@ValuedByNameKey
		@ValueAmount
		@CurrencyConceptKey
		@ValidFromVagueDateStart
		@ValidFromVagueDateEnd 
		@ValidFromVagueDateType
		@ValidToVagueDateStart
		@ValidToVagueDateEnd 
		@ValidToVagueDateType
		@Description
		@SessionID 
		@Timestamp

  Created:	October 2003

  Last revision information:
    $Revision: 1 $
    $Date: 3/02/09 12:20 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Valuation_Update]
	@Key char(16) OUTPUT,
	@VagueDateStart int, 
	@VagueDateEnd int, 
	@VagueDateType varchar(2),
	@RefNumber varchar(30),
	@TypeConceptKey char(16),
	@ValuedByNameKey char(16),
	@ValueAmount money,
	@CurrencyConceptKey char(16),
	@ValidFromVagueDateStart int,
	@ValidFromVagueDateEnd int,
	@ValidFromVagueDateType varchar(2),
	@ValidToVagueDateStart int,
	@ValidToVagueDateEnd int,
	@ValidToVagueDateType varchar(2),
	@Description text,
	@SessionID char(16),
	@Timestamp timestamp

AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION

		UPDATE 	Valuation
		SET 	Vague_Date_Start = @VagueDateStart,
			Vague_Date_End = @VagueDateEnd,
			Vague_Date_Type = @VagueDateType,
			Ref_Number = @RefNumber,
			Type_Concept_Key = @TypeConceptKey,
			Valued_By_Name_Key = @ValuedByNameKey,
			Value_Amount = @ValueAmount,		
			Currency_Concept_Key = @CurrencyConceptKey,
			Valid_From_Vague_Date_Start = @ValidFromVagueDateStart, 
			Valid_From_Vague_Date_End = @ValidFromVagueDateEnd,
			Valid_From_Vague_Date_Type = @ValidFromVagueDateType, 
			Valid_To_Vague_Date_Start = @ValidToVagueDateStart,
			Valid_To_Vague_Date_End = @ValidToVagueDateEnd, 
			Valid_To_Vague_Date_Type = @ValidToVagueDateType,
			Description = @Description, 
			Entered_Session_ID = @SessionID
		WHERE	Valuation_Key = @Key
		AND		[Timestamp] = @Timestamp

		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Valuation WHERE Valuation_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Valuation_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Valuation_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Valuation_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Valuation_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Valuation_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Valuation_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Valuation_Update TO [Dev - JNCC SQL]
END
GO