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
    $Revision: 10 $
    $Date: 3/02/09 10:37 $
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