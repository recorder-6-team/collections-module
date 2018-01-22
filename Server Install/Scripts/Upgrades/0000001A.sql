SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_CollectionUnitStatus_Get') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_CollectionUnitStatus_Get]
GO

/*===========================================================================*\
  Description:	Returns the movement status for a collection unit record.

  Parameters:	@Key	Collection unit key
		@Status	OUTPUT

  Created:	August 2003

  Last revision information:
    $Revision: 1 $
    $Date: 24/03/05 10:52 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_CollectionUnitStatus_Get]
	@Key char(16),
	@Status varchar(200) OUTPUT
AS
	/*-------------------------------------------------------------*\
	  Get a new key.
	\*-------------------------------------------------------------*/
	-- If unknown, set result and exit.
	IF NOT (EXISTS(
		SELECT * FROM Movement_Collection_Unit MCU 
		INNER JOIN 	Movement_Of_Material MM ON MM.Movement_Direction_Key = MCU.Movement_Direction_Key
		LEFT JOIN 	Movement_Of_Material_Exclusion MME 
					ON MME.Movement_Of_Material_Key = MM.Movement_Of_Material_Key
					AND MME.Collection_Unit_Key=MCU.Collection_Unit_Key
		WHERE 	MCU.Collection_Unit_Key = @Key
		AND 	MME.Movement_Of_Material_Exclusion_Key IS NULL)
	OR EXISTS(
		SELECT * FROM Movement_Collection_Unit MCU 
		INNER JOIN 	Movement_Of_Ownership MO ON MO.Movement_Direction_Key = MCU.Movement_Direction_Key
		LEFT JOIN 	Movement_Of_Ownership_Exclusion MOE 
					ON MOE.Movement_Of_Ownership_Key = MO.Movement_Of_Ownership_Key
					AND MOE.Collection_Unit_Key=MCU.Collection_Unit_Key
		WHERE 	MCU.Collection_Unit_Key = @Key
		AND 	MOE.Movement_Of_Ownership_Exclusion_Key IS NULL))
	BEGIN
		SET @Status = 'Unknown'
		RETURN
	END

	/*-------------------------------------------------------------*\
	  Need to work out what it is.
	\*-------------------------------------------------------------*/
	-- Some variables.
	DECLARE	@HoldingOrganisationKey char(16),
		@OwnershipNameKey char(16),
		@OwnershipNameKeyIsHoldingOrg bit,
		@OwnershipMovementType int,
		@MaterialNameKey char(16),
		@MaterialNameKeyIsHoldingOrg bit,
		@MaterialMovementType int,
		@DepartmentName varchar(100)

	-- Get the organisation holding software install	
	SELECT	@HoldingOrganisationKey = Data FROM Setting WHERE [Name] = 'HoldingOrg'

	/*-------------------------------------------------------------*\
	  Get most recent movement of material.
	\*-------------------------------------------------------------*/
	SELECT	TOP 1	@MaterialNameKey = MM.Receiver_Name_Key, 
			@MaterialMovementType = Movement_Type, 
			@DepartmentName = OD.Item_Name
	FROM		Movement_Collection_Unit MCU
	INNER JOIN 	Movement_Direction MD ON MD.Movement_Direction_Key = MCU.Movement_Direction_Key
	INNER JOIN 	Movement M ON M.Movement_Key = MD.Movement_Key
	INNER JOIN 	Movement_Of_Material MM ON MM.Movement_Direction_Key = MCU.Movement_Direction_Key
	LEFT JOIN 	Movement_Of_Material_Exclusion MME 
				ON MME.Movement_Of_Material_Key = MM.Movement_Of_Material_Key
				AND MME.Collection_Unit_Key=MCU.Collection_Unit_Key
	LEFT JOIN	Organisation_Department OD ON OD.Organisation_Department_Key = MM.Receiver_Organisation_Department_Key
	WHERE		MCU.Collection_Unit_Key = @Key
	AND 		MME.Movement_Of_Material_Key IS NULL
	ORDER BY	MM.Vague_Date_Start DESC

	/*-------------------------------------------------------------*\
	  Lost and destroyed movements
	\*-------------------------------------------------------------*/
	IF @MaterialMovementType = 4
	BEGIN
		SET @Status='Destroyed'
		RETURN
	END
	IF @MaterialMovementType = 7
	BEGIN
		SET @Status='Lost'
		RETURN
	END

	/*-------------------------------------------------------------*\
	  Get most recent movement of ownership.
	\*-------------------------------------------------------------*/
	SELECT	TOP 1	@OwnershipNameKey = IsNull(MD2.Receiver_Name_Key, MD.Receiver_Name_Key),
			@OwnershipMovementType = M.Movement_Type
	FROM		Movement_Collection_Unit MCU
	INNER JOIN 	Movement_Direction MD ON MD.Movement_Direction_Key = MCU.Movement_Direction_Key
	INNER JOIN 	Movement M ON M.Movement_Key = MD.Movement_Key
			-- If the Movement is a loan in, the name key stored in the Movement_Direction
			-- record linked to the Movement_Collection_Unit record will be the receiver.
			-- The actual owner will be stored in the outbound Movement_Direction record
			-- associated with the movement, so do a left join on this.
	LEFT JOIN	(Movement_Direction MD2 JOIN Movement AS M2 ON M2.Movement_Key = MD2.Movement_key AND M2.Movement_Type = 2)
				ON MD2.Movement_Key = M.Movement_Key
				AND MD2.Outbound = 1
	INNER JOIN 	Movement_Of_Ownership MO ON MO.Movement_Direction_Key = MCU.Movement_Direction_Key
	LEFT JOIN 	Movement_Of_Ownership_Exclusion MOE 
				ON MOE.Movement_Of_Ownership_Key = MO.Movement_Of_Ownership_Key
				AND MOE.Collection_Unit_Key=MCU.Collection_Unit_Key
	WHERE		MCU.Collection_Unit_Key = @Key
	AND 		MOE.Movement_Of_Ownership_Key IS NULL
	ORDER BY	MO.Vague_Date_Start DESC

	/*-----------------------------------------------------------------------------*\
	  Now that we have the Name Key of the most recent Movement_Of_Onwership for 
	  this Collection_Unit, we want to see if this key refers to
	  the holding organisation, or if it refers to an individual who belongs
	  to the holding organisation
	\*-----------------------------------------------------------------------------*/
	IF @OwnershipNameKey = @HoldingOrganisationKey
		SET 	@OwnershipNameKeyIsHoldingOrg = 1
	ELSE IF EXISTS( SELECT 		I.Name_Key
			FROM		Individual AS I
			LEFT JOIN	Name_Relation AS NR1 ON NR1.Name_Key_1 = I.Name_Key
			LEFT JOIN	Name_Relation AS NR2 ON NR2.Name_Key_2 = I.Name_Key
			INNER JOIN	Organisation AS O ON (O.Name_Key = NR1.Name_Key_2 OR O.Name_Key = NR2.Name_Key_1)
			WHERE		I.Name_Key = @OwnershipNameKey
			AND 		O.Name_Key = @HoldingOrganisationKey)
		SET @OwnershipNameKeyIsHoldingOrg = 1
	-- If there is no @OwnershipNameKey assume that it belongs to the holding org.
	ELSE IF @OwnershipNameKey Is NULL
		SET @OwnershipNameKeyIsHoldingOrg = 1
	ELSE
		SET @OwnershipNameKeyIsHoldingOrg = 0

	/*==============================================================================*\
	  Find out if the item is currently in the Holding Organisation or not.
	\*==============================================================================*/	

	/*-----------------------------------------------------------------------------*\
	  Now that we have the Name Key for the most recent Movement of Material, we 
	  want to see if this key refers to the holding organisation, or if it refers 
	  to an individual who belongs to the holding organisation
	\*-----------------------------------------------------------------------------*/
	IF @MaterialNameKey = @HoldingOrganisationKey
		SET 	@MaterialNameKeyIsHoldingOrg = 1
	ELSE IF EXISTS( SELECT 		I.Name_Key
			FROM		Individual AS I
			LEFT JOIN	Name_Relation AS NR1 ON NR1.Name_Key_1 = I.Name_Key
			LEFT JOIN	Name_Relation AS NR2 ON NR2.Name_Key_2 = I.Name_Key
			INNER JOIN	Organisation AS O ON (O.Name_Key = NR1.Name_Key_2 OR O.Name_Key = NR2.Name_Key_1)
			WHERE		I.Name_Key = @MaterialNameKey
			AND 		O.Name_Key = @HoldingOrganisationKey)
		SET @MaterialNameKeyIsHoldingOrg = 1
	ELSE
		SET @MaterialNameKeyIsHoldingOrg = 0


	-- Formatted additional info after status. No need to repeat all over the place.
	DECLARE @ByOwner varchar(200), @ToOwner varchar(200), @FromOwner varchar(200)
	SET @ByOwner = IsNull(NullIf(' by '+ dbo.ufn_GetFormattedName(@OwnershipNameKey), ' by '), '')
	SET @ToOwner = IsNull(NullIf(' to '+ dbo.ufn_GetFormattedName(@OwnershipNameKey), ' to '), '')
	SET @FromOwner = IsNull(NullIf(' from '+ dbo.ufn_GetFormattedName(@OwnershipNameKey), ' from '), '')

	/*==============================================================================*\
	  Now work out what to display.
	\*==============================================================================*/		
	IF @OwnershipNameKeyIsHoldingOrg=1
	BEGIN
		IF @MaterialNameKeyIsHoldingOrg=1
		BEGIN
			IF @DepartmentName IS NOT NULL 
				SET @Status = 'In ' + @DepartmentName + ' department.'
			ELSE BEGIN
				SELECT	@DepartmentName = Full_Name
				FROM	Organisation
				WHERE	Name_Key = @HoldingOrganisationKey
	
				SET @Status = 'In ' + @DepartmentName + ' organisation.' 
			END 
		END ELSE
		IF @DepartmentName IS NOT NULL 
			SET @Status = 'In ' + @DepartmentName + ' department.'
		ELSE
			SET @Status = 'Owned' + @ByOwner
	END
	ELSE
	BEGIN
		IF @MaterialNameKeyIsHoldingOrg = 1
			SET @Status = 'On loan' + @FromOwner
		ELSE IF @MaterialMovementType = 8
			SET @Status = 'Sold' + @ToOwner
		ELSE IF @MaterialMovementType = 5
			SET @Status = 'Disposed' + @ToOwner		
		ELSE
			SET @Status = 'Owned' + @ByOwner
	END


GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_CollectionUnitStatus_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_CollectionUnitStatus_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitStatus_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_CollectionUnitStatus_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_CollectionUnitStatus_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_CollectionUnitStatus_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_CollectionUnitStatus_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitStatus_Get TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_CollectionUnit_OwnedByHoldingOrg_Get]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_CollectionUnit_OwnedByHoldingOrg_Get]
GO

/*===========================================================================*\
  Description:	See if a Collection Unit is currently owned by the Holding
		Organisation at a certain point in time

  Parameters:	@CollectionUnitKey  	
		@MovementKey			(Optional) Key will be provided if this
						proc is called because the user clicked
						'Link to existing'
		@NewMovementVagueDateEnd 	(Optional) Will be provided if this proc
						has been called from the Movement Frame.
		@OwnedByHoldingOrg		Output parameter than returns 1 if it does 
						belong to the Holding Organisation, 0 if it 
						doesn't.

  Created:	April 2004

  Last revision information:
    $Revision: 1 $
    $Date: 24/03/05 10:52 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_CollectionUnit_OwnedByHoldingOrg_Get] 
	@CollectionUnitKey char(16),
	@MovementKey char(16) = NULL,
	@NewMovementVagueDateEnd int = NULL,
	@OwnedByHoldingOrg bit OUTPUT
AS

SET NOCOUNT ON
	DECLARE @OwnershipNameKey char(16),
		@HoldingOrganisationKey char(16),
		@MaterialNameKey char(16),
		@MovementDirectionReceiverNameKey char(16),
		@MaterialMovementKey char(16),
		@OwnershipMovementKey char(16),
		@CurrentMovementVagueDateEnd int

	/*=======================================================================*\
	  If the most recent movement for an item saw it stop being owned by the
	  holding organisation, you shouldn't be able to add any movements that
	  happen after that date. However, you should be allowed to add movements
	  that occur prior to the movement that saw it leave. 

	  This is slightly flawed, in that if the most recent movement was a sale,
	  you could add another sale before this movement. Hence, the item would
	  have been sold twice. Retrospective altering of the data is not going
	  to be implemented in this version of the Collections Browser.
	\*=======================================================================*/
	/*-----------------------------------------------------------------------*\
	  This proc is called from the ValidateNewNode methods that are called when
	  the user clicks 'link to existing' and also from the Movement
	  frame. 

	  In the former case, we will have a Movement Key and can retrieve
	  the data using that. In the latter, we may not have a Movement Key, but
	  we will have the date because it is a field on the frame. 
	\*-----------------------------------------------------------------------*/
	IF @NewMovementVagueDateEnd IS NULL
		SELECT	@NewMovementVagueDateEnd = Exp_Vague_Date_End
		FROM	Movement
		WHERE	Movement_Key = @MovementKey

	/*----------------------------------------------------------------------*\
	  Get the date of the most recent movement. We will want to compare this
	  with the previously selected dates later on.
	\*----------------------------------------------------------------------*/
	SELECT TOP 1	@CurrentMovementVagueDateEnd = M.Exp_Vague_Date_End
	FROM		Movement_Collection_Unit AS MCU
	INNER JOIN	Movement_Direction AS MD ON MD.Movement_Direction_Key = MCU.Movement_Direction_Key
	INNER JOIN	Movement AS M ON M.Movement_Key = MD.Movement_Key
	WHERE		MCU.Collection_Unit_Key = @CollectionUnitKey
	ORDER BY	M.Exp_Vague_Date_Start DESC

	/*-------------------------------------------------------------*\
	  Get the organisation holding software install.
	\*-------------------------------------------------------------*/
	SELECT	@HoldingOrganisationKey = Data FROM Setting WHERE [Name] = 'HoldingOrg'

	/*-------------------------------------------------------------*\
	  Get most recent movement of ownership.
	\*-------------------------------------------------------------*/
	SELECT	TOP 1	@OwnershipNameKey = IsNull(MD2.Receiver_Name_Key, MD.Receiver_Name_Key),
			@OwnershipMovementKey = M.Movement_Key
	FROM		Movement_Collection_Unit MCU
	INNER JOIN 	Movement_Direction MD ON MD.Movement_Direction_Key = MCU.Movement_Direction_Key
	INNER JOIN 	Movement M ON M.Movement_Key = MD.Movement_Key
			/*------------------------------------------------------------------------------*\
			  If the Movement is a loan in, the name key stored in the Movement_Direction
			  record linked to the Movement_Collection_Unit record will be the receiver.
			  The actual owner will be stored in the outbound Movement_Direction record
			  associated with the movement, so do a left join on this.
			\*------------------------------------------------------------------------------*/
	LEFT JOIN	(Movement_Direction MD2 
				INNER JOIN Movement AS M2 ON M2.Movement_Key = MD2.Movement_key
								AND M2.Movement_Type = 2)
			ON MD2.Movement_Key = M.Movement_Key
			AND MD2.Outbound = 1
	LEFT JOIN 	Movement_Of_Ownership MO ON MO.Movement_Direction_Key = MCU.Movement_Direction_Key
	LEFT JOIN 	Movement_Of_Ownership_Exclusion MOE ON MOE.Movement_Of_Ownership_Key = MO.Movement_Of_Ownership_Key

	WHERE		MCU.Collection_Unit_Key = @CollectionUnitKey
	AND 		MOE.Movement_Of_Ownership_Key IS NULL

	ORDER BY	MO.Vague_Date_Start DESC

	/*-------------------------------------------------------------*\
	  Get most recent movement of material.
	\*-------------------------------------------------------------*/
	DECLARE @MaterialMovementExists BIT
	SET @MaterialMovementExists = 0		-- Will remain 0 if no record found in next SELECT.

	SELECT	TOP 1	@MaterialMovementExists = 1,
			@MaterialNameKey = MM.Receiver_Name_Key,
			@MovementDirectionReceiverNameKey = MD.Receiver_Name_Key,
			@MaterialMovementKey = M.Movement_Key 
	FROM		Movement_Collection_Unit MCU
	INNER JOIN 	Movement_Direction MD ON MD.Movement_Direction_Key = MCU.Movement_Direction_Key
	INNER JOIN 	Movement M ON M.Movement_Key = MD.Movement_Key
	INNER JOIN 	Movement_Of_Material MM ON MM.Movement_Direction_Key = MCU.Movement_Direction_Key
	LEFT JOIN 	Movement_Of_Material_Exclusion MME ON MME.Movement_Of_Material_Key = MM.Movement_Of_Material_Key
	LEFT JOIN	Organisation_Department OD ON OD.Organisation_Department_Key = MM.Receiver_Organisation_Department_Key

	WHERE		MCU.Collection_Unit_Key = @CollectionUnitKey
	AND 		MME.Movement_Of_Material_Key IS NULL

	ORDER BY	MM.Vague_Date_Start DESC

	/*==================================================================================*\
	  Now decide whether the item belongs to the holding organisation at the given
	  point in time.
	\*==================================================================================*/

	/*------------------------------------------------------------------------------*\
	  If the new current Movement is the most recent movement, then you should be
	  allowed to edit the date of the Movement freely. We need to check the 
	  Movement_Key against the key obtained from both the MOO and MOM, in case
	  it is null for one of them.
	\*------------------------------------------------------------------------------*/
	IF (@OwnershipMovementKey = @MovementKey) OR (@MaterialMovementKey = @MovementKey)
		SET @OwnedByHoldingOrg = 1

	/*------------------------------------------------------------------------------*\
	  If @CurrentMovementVagueDateEnd = 0, this means that there isn't a movement
	  currently associated with the Collection_Unit, so safe to assume that
	  it is owned by holding organisation
	\*------------------------------------------------------------------------------*/
	ELSE IF @CurrentMovementVagueDateEnd = 0
		SET @OwnedByHoldingOrg = 1

	/*------------------------------------------------------------------------------*\
	  If the new Movement is after the current movement we have to check that
	  the item has not been moved out of the holding organisation
	\*------------------------------------------------------------------------------*/
	ELSE IF @NewMovementVagueDateEnd >= @CurrentMovementVagueDateEnd
	BEGIN
		/*------------------------------------------------------------------------------*\
		  If there is a Movement Direction record but the Receiver Name Key is Null,
		  it will be a disposed / destroyed / lost movement and hence not owned by
		  the holding org.
		\*------------------------------------------------------------------------------*/
		IF (@MaterialMovementExists = 1) AND (@MovementDirectionReceiverNameKey IS NULL)
			SET @OwnedByHoldingOrg = 0

		/*------------------------------------------------------------------------------*\
		  Owner name key is the holding organisation
		\*------------------------------------------------------------------------------*/
		ELSE IF @OwnershipNameKey = @HoldingOrganisationKey
			SET @OwnedByHoldingOrg = 1

		/*------------------------------------------------------------------------------*\
		  See if the name key is an individual who belongs to the holding organisation.
		\*------------------------------------------------------------------------------*/
		ELSE IF EXISTS( SELECT 		I.Name_Key
				FROM		Individual AS I
				LEFT JOIN	Name_Relation AS NR1 ON NR1.Name_Key_1 = I.Name_Key
				LEFT JOIN	Name_Relation AS NR2 ON NR2.Name_Key_2 = I.Name_Key
				INNER JOIN	Organisation AS O ON (O.Name_Key = NR1.Name_Key_2
								  OR O.Name_Key = NR2.Name_Key_1)
				WHERE		I.Name_Key = @OwnershipNameKey
				AND 		O.Name_Key = @HoldingOrganisationKey)
			SET @OwnedByHoldingOrg = 1

		/*------------------------------------------------------------------------------*\
		  If there is no @OwnershipNameKey assume that it belongs to the holding org.
		\*------------------------------------------------------------------------------*/
		ELSE IF (@OwnershipNameKey Is NULL) 
			SET @OwnedByHoldingOrg = 1 

		/*------------------------------------------------------------------------------*\
		  Otherwise assume it doesn't belong to the holding organisation.
		\*------------------------------------------------------------------------------*/
		ELSE	SET @OwnedByHoldingOrg = 0
	END
	/*------------------------------------------------------------------------------*\
	  If the new movement is before the current movement then we can assume that
	  it is ok to add this movement
	\*------------------------------------------------------------------------------*/
	ELSE	
		SET @OwnedByHoldingOrg = 1
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_CollectionUnit_OwnedByHoldingOrg_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_CollectionUnit_OwnedByHoldingOrg_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_CollectionUnit_OwnedByHoldingOrg_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_CollectionUnit_OwnedByHoldingOrg_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_CollectionUnit_OwnedByHoldingOrg_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_CollectionUnit_OwnedByHoldingOrg_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_CollectionUnit_OwnedByHoldingOrg_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_CollectionUnit_OwnedByHoldingOrg_Get TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_CollectionUnit_Update_ForMovement]')
	   AND    Type = 'P')
    DROP PROCEDURE [dbo].[usp_CollectionUnit_Update_ForMovement]
GO

/*===========================================================================*\
  Description:	Inserts a record into the Movement_Collection_Unit join table 
		so that there is a relationship between the Movement and 
		Collection Unit tables.

  Parameters:	@ParentKey 	The key of the top level (parent) Movement node.
		@ChildKey	The key of the added (child) Collection Unit node. 
		@SessionID
		@JoinKey	Key for new record on join table.
		@IsAccessionOrExchange  Optional parameter
		@IsInbound	Optional parameter

  Created:	September 2003

  Last revision information:
    $Revision: 1 $
    $Date: 24/03/05 10:52 $
    $Author: Johnvanbreda $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_CollectionUnit_Update_ForMovement] 
	@ParentKey CHAR(16),
	@ChildKey CHAR(16), 
	@SessionID CHAR(16),
	@IsAccessionOrExchange BIT = NULL,
	@IsInbound BIT = NULL,
	@JoinKey CHAR(16) OUTPUT
AS
	SET NOCOUNT ON

	EXECUTE spNextKey 'Movement_Collection_Unit', @JoinKey OUTPUT

	DECLARE @DirKey CHAR(16)

	BEGIN TRANSACTION
		/*---------------------------------------------------------------------------*\
		  To link the new Movement node to the Collection Unit it was added in, 
		  we need to add a record to the Movement_Collection_Unit table. This table
		  requires a Movement_Direction_Key. Hence, we need to get the correct 
		  Movement_Direction_Key for the the Movement we are linking.
		  Also, if we are dealing with a loan in, we want the Movement_Collection_Unit
		  record to contain the Movement_Direction key for the inbound movement.
		\*---------------------------------------------------------------------------*/
		IF ISNULL(@IsInbound, 0) = 0
			SELECT	@DirKey = Movement_Direction_Key
			FROM 	Movement_Direction
			WHERE 	Movement_Key = @ParentKey
			AND 	Outbound = 1
		ELSE BEGIN
			SELECT	@DirKey = Movement_Direction_Key
			FROM	Movement_Direction
			WHERE	Movement_Key = @ParentKey
			AND	Outbound = 0

			/*---------------------------------------------------------------------------*\
			  The Accessions and Exchanges folder node is allowed to contain one node inbound.
			  As a result, any other accession or exchange movements associated with the 
			  collection unit should be deleted before the new record is inserted.
			\*---------------------------------------------------------------------------*/
			IF @IsAccessionOrExchange = 1
				DELETE	Movement_Collection_Unit
				FROM	Movement_Collection_Unit AS MCU
				JOIN	Movement_Direction AS MD ON MD.Movement_Direction_Key = MCU.Movement_Direction_Key
				JOIN	Movement AS M ON M.Movement_Key = MD.Movement_Key AND M.Movement_Type IN (0, 1)  -- Accession/Exchange
				WHERE	MCU.Collection_Unit_Key = @ChildKey
				AND	MD.Outbound = ISNULL(@IsInbound, 0)  -- Should be specified if dealing with Acc/Exch, but just in case.
		END

		/*---------------------------------------------------------------------------*\
		  Now that we have the Movement_Direction_Key, we can insert a record into
		  the Movement_Collection_Unit table.
		\*---------------------------------------------------------------------------*/
		INSERT INTO Movement_Collection_Unit (
			Movement_Collection_Unit_Key, Movement_Direction_Key, Collection_Unit_Key, Entered_Session_ID
		) VALUES (
			@JoinKey, @DirKey, @ChildKey, @SessionID
		)
	
		IF @@Error <> 0 GOTO RollbackAndExit

		DECLARE @First_Movement_Of_Material_Key CHAR(16)

		SELECT TOP 1 @First_Movement_Of_Material_Key = Movement_Of_Material_Key
		FROM 	Movement_Of_Material 
		WHERE 	Movement_Direction_Key = @DirKey
		ORDER BY [TimeStamp]

		/*---------------------------------------------------------------------------*\
		 If this is not the first movement of material, exclude this collection_unit from it
		\*---------------------------------------------------------------------------*/
		IF EXISTS(SELECT * FROM Movement_Of_Material 
				WHERE (Movement_Of_Material_Key <> @First_Movement_Of_Material_Key) 
				AND (Movement_Direction_Key = @DirKey))
		BEGIN
			DECLARE @Movement_Of_Material_Key CHAR(16)
			DECLARE @Movement_Of_Material_Exclusion_Key CHAR(16)
	
			DECLARE MOMEcsr CURSOR LOCAL FAST_FORWARD
			FOR 
			SELECT	Movement_Of_Material_Key
			FROM	Movement_Of_Material
			WHERE	Movement_Direction_Key = @DirKey
		
			OPEN MOMEcsr
			
			FETCH NEXT FROM MOMEcsr INTO @Movement_Of_Material_Key
			
			WHILE @@FETCH_STATUS = 0
			BEGIN
				EXECUTE spNextKey 'Movement_Of_Material_Exclusion', @Movement_Of_Material_Exclusion_Key OUTPUT				
				INSERT INTO Movement_Of_Material_Exclusion (
					Movement_Of_Material_Exclusion_Key, Movement_Of_Material_Key, Collection_Unit_Key, Entered_Session_ID
				) VALUES (
					@Movement_Of_Material_Exclusion_Key, @Movement_Of_Material_Key, @ChildKey, @SessionID
				)
	
				IF @@Error <> 0 GOTO RollbackAndExit
			
			
				FETCH NEXT FROM MOMEcsr INTO @Movement_Of_Material_Key
			END --End While
			
			CLOSE MOMEcsr
			DEALLOCATE MOMEcsr
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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_CollectionUnit_Update_ForMovement') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_CollectionUnit_Update_ForMovement'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_CollectionUnit_Update_ForMovement TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_CollectionUnit_Update_ForMovement TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_CollectionUnit_Update_ForMovement TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_CollectionUnit_Update_ForMovement TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_CollectionUnit_Update_ForMovement TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Collection_Select') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_Collection_Select]
GO

/*===========================================================================*\
  Description:	Returns a collection record.

  Parameters:	@Key	Collection key

  Created:	August 2003

  Last revision information:
    $Revision: 1 $
    $Date: 24/03/05 10:52 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Collection_Select]
	@Key char(16)
AS

SET NOCOUNT ON

	SELECT	CU.Collection_Unit_Key, 
		C.Item_Name,
		C.Parent_Collection_Collection_Unit_Key,
		C.Assembler_Name_Key, 
		dbo.ufn_GetFormattedName(C.Assembler_Name_Key) AS Assembler_Name,
		C.Topic, 
		CUN.Name_Key AS Owner_Name_Key,
		CU.Current_Container_Collection_Unit_Key,
		SC.Item_Name + ISNULL(' - ' + CSC.Current_Location_Code, ISNULL(' - ' + CSC.Usual_Location_Code, '')) AS Current_Location_Name,
		CU.Current_Location_Code,
		CU.Usual_Container_Collection_Unit_Key, 
		SU.Item_Name + ISNULL(' - ' + CSU.Current_Location_Code, ISNULL(' - ' + CSU.Usual_Location_Code, '')) AS Usual_Location_Name,
		CU.Usual_Location_Code,
		CU.Domain_Mask,
		C.Risk_Concept_Key,
		T.Item_Name AS Risk_Name,
		C.Timestamp AS Collection_Timestamp,
		CU.Timestamp AS Collection_Unit_Timestamp

	FROM		Collection C
	INNER JOIN 	Collection_Unit CU ON CU.Collection_Unit_Key = C.Collection_Unit_Key
	LEFT JOIN 	Store SC ON SC.Collection_Unit_Key = CU.Current_Container_Collection_Unit_Key
	LEFT JOIN	Collection_Unit CSC ON CSC.Collection_Unit_Key = SC.Collection_Unit_Key 

	LEFT JOIN 	Store SU ON SU.Collection_Unit_Key = CU.Usual_Container_Collection_Unit_Key
	LEFT JOIN	Collection_Unit CSU ON CSU.Collection_Unit_Key = SU.Collection_Unit_Key

	LEFT JOIN 	Collection_Unit_Name CUN ON CUN.Collection_Unit_Key = C.Collection_Unit_Key
						AND CUN.Relation_Type_Concept_Key = 'SYSTEM00000000I7' --Owner concept key
	LEFT JOIN 	Concept CP ON CP.Concept_Key = C.Risk_Concept_Key
	LEFT JOIN 	Term T ON T.Term_Key = CP.Term_Key
	
	WHERE	C.Collection_Unit_Key = @Key

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Collection_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Collection_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Collection_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Collection_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Collection_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Collection_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Collection_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Collection_Select TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Movement_Update_ForCollectionUnit]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Movement_Update_ForCollectionUnit]
GO

/*===========================================================================*\
  Description:	Inserts a record into the Movement_Collection_Unit join table 
		so that there is a relationship between the Movement_Direction 
		and Collection Unit tables.

  Parameters:	@ParentKey 	The key of the top level (parent) Collection Unit node.
		@ChildKey	The key of the Movement node. 
		@SessionID

  Created:	September 2003

  Last revision information:
    $Revision: 1 $
    $Date: 24/03/05 10:52 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Movement_Update_ForCollectionUnit] 
	@ParentKey 		CHAR(16),
	@ChildKey 		CHAR(16),
	@SessionID 		CHAR(16),
	@IsAccessionOrExchange 	BIT = NULL,
	@JoinKey 		CHAR(16) OUTPUT
AS
SET NOCOUNT ON

	EXECUTE spNextKey 'Movement_Collection_Unit', @JoinKey OUTPUT

	DECLARE @DirKey CHAR(16),
		@IsInbound BIT

	BEGIN TRANSACTION
		/*---------------------------------------------------------------------------*\
		  To link the new Movement node to the Collection Unit it was added in, 
		  we need to add a record to the Movement_Collection_Unit table. This table
		  requires a Movement_Direction_Key. Hence, we need to get the correct 
		  Movement_Direction_Key for the the Movement we are linking.
		\*---------------------------------------------------------------------------*/
		IF ISNULL(@IsAccessionOrExchange, 0) = 0 
		BEGIN
			SELECT	@DirKey = Movement_Direction_Key,
				@IsInbound = 0
			FROM 	Movement_Direction
			WHERE 	Movement_Key = @ChildKey
			AND 	Outbound = 1
		
			IF @DirKey IS NULL
				SELECT	@DirKey = Movement_Direction_Key,
					@IsInbound = 1
				FROM	Movement_Direction
				WHERE	Movement_Key = @ChildKey
				AND	Outbound = 0
		END ELSE
			/*---------------------------------------------------------------------------*\
			  The Accessions and Exchanges folder node is allowed to contain one inbound
			  and one outbound movement. As a result, any other accession or exchange 
			  movements associated with the collection unit should be deleted before the 
			  new record is inserted.
			\*---------------------------------------------------------------------------*/
			-- Get movement type, important for next bits.
			DECLARE	@MoveType INT
			SELECT	@MoveType = Movement_Type
			FROM	Movement 
			WHERE	Movement_Key = @ChildKey

			-- Dealing with Accession, that's an inbound movement, always. So find and remove other inbound 
			-- movement (there should be only one to find). The new one will replace it.
			IF @MoveType = 0
			BEGIN
				DELETE	Movement_Collection_Unit
				FROM	Movement_Collection_Unit AS MCU
				JOIN	Movement_Direction AS MD ON MD.Movement_Direction_Key = MCU.Movement_Direction_Key
				JOIN	Movement AS M ON M.Movement_Key = MD.Movement_Key AND M.Movement_Type IN (0, 1)	 -- Can have inbound Accessions (Type 0) AND Exchanges (Type 1)
				WHERE	MCU.Collection_Unit_Key = @ParentKey
				AND	MD.Outbound = 0
			
				-- Get the Direction Key to use in join table later.
				SELECT	@DirKey = Movement_Direction_Key
				FROM 	Movement_Direction
				WHERE 	Movement_Key = @ChildKey
				AND 	Outbound = 0
			END
			
			-- Dealing with Exchange. If the Collection_Unit has no inbound movement yet, this is going to be it.
			-- If there already is an inbound movement (accession/exchange), this one will be an outbound movement.
			-- And if there is already an outbound movement, find it and remove it. The new one will replace it.
			IF @MoveType = 1
			BEGIN
				-- If inbound movement exists, Exchange will be outbound only, so find and remove existing outbound, if any.
				IF EXISTS(	SELECT 	*
						FROM	Movement_Collection_Unit AS MCU
						JOIN	Movement_Direction AS MD ON MD.Movement_Direction_Key = MCU.Movement_Direction_Key
						WHERE	Collection_Unit_Key = @ParentKey 
						AND 	Outbound = 0)
				BEGIN
					-- Outbound it is.
					SELECT	@DirKey = Movement_Direction_Key
					FROM 	Movement_Direction
					WHERE 	Movement_Key = @ChildKey
					AND 	Outbound = 1
			
					-- Remove the existing outbound movement.
					DELETE	Movement_Collection_Unit
					FROM	Movement_Collection_Unit AS MCU
					JOIN	Movement_Direction AS MD ON MD.Movement_Direction_Key = MCU.Movement_Direction_Key
					JOIN	Movement AS M ON M.Movement_Key = MD.Movement_Key AND M.Movement_Type = 1  -- Can't have outbound Accessions (Type 0)
					WHERE	MCU.Collection_Unit_Key = @ParentKey
					AND	MD.Outbound = 1
				END ELSE
					-- Inbound it is.
					SELECT	@DirKey = Movement_Direction_Key
					FROM 	Movement_Direction
					WHERE 	Movement_Key = @ChildKey
					AND 	Outbound = 0
			END

		/*---------------------------------------------------------------------------*\
		  Now that we have the Movement_Direction_Key, we can insert a record into
		  the Movement_Collection_Unit table.
		\*---------------------------------------------------------------------------*/
		INSERT INTO Movement_Collection_Unit (
			Movement_Collection_Unit_Key, Movement_Direction_Key, Collection_Unit_Key, Entered_Session_ID
		) VALUES (
			@JoinKey, @DirKey, @ParentKey, @SessionID
		)
	
		IF @@Error <> 0 GOTO RollBackAndExit

	COMMIT TRANSACTION 
	RETURN 0

RollBackAndExit:
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Movement_Update_ForCollectionUnit') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Movement_Update_ForCollectionUnit'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Movement_Update_ForCollectionUnit TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Movement_Update_ForCollectionUnit TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Movement_Update_ForCollectionUnit TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Movement_Update_ForCollectionUnit TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Movement_Update_ForCollectionUnit TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_PostImport_Collections]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_PostImport_Collections]
GO

/*===========================================================================*\
  Description:	Recreate constraints that impede import of collections data

  Created:	Oct 2004

  Last revision information:
    $Revision: 1 $
    $Date: 24/03/05 10:52 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_PostImport_Collections]
AS

IF NOT EXISTS (select * from dbo.sysobjects where id = object_id(N'[dbo].[FK_Specimen_Unit_Determination]') and OBJECTPROPERTY(id, N'IsForeignKey') = 1)
ALTER TABLE [dbo].[Specimen_Unit] ADD 
	CONSTRAINT [FK_Specimen_Unit_Determination] FOREIGN KEY 
	(
		[Preferred_Determination_Key]
	) REFERENCES [dbo].[Determination] (
		[Determination_Key]
	)

IF NOT EXISTS (select * from dbo.sysobjects where id = object_id(N'[dbo].[FK_Specimen_Unit_TAXON_DETERMINATION]') and OBJECTPROPERTY(id, N'IsForeignKey') = 1)
ALTER TABLE [dbo].[Specimen_Unit] ADD 
	CONSTRAINT [FK_Specimen_Unit_TAXON_DETERMINATION] FOREIGN KEY 
	(
		[Preferred_Taxon_Determination_Key]
	) REFERENCES [dbo].[TAXON_DETERMINATION] (
		[TAXON_DETERMINATION_KEY]
	)
GO


/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_PostImport_Collections') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_PostImport_Collections'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_PostImport_Collections TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_PostImport_Collections TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_PostImport_Collections TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_SpecimenFieldData_Select_ForOccurrence]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_SpecimenFieldData_Select_ForOccurrence]
GO

/*===========================================================================*\
  Description:	Returns a list of specimens linked to an occurrence.

  Parameters:	@OccurrenceKey

  Created:	November 2003

  Last revision information:
    $Revision: 1 $
    $Date: 24/03/05 10:52 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SpecimenFieldData_Select_ForOccurrence] 
	@OccurrenceKey char(16),
	@IsTaxonOccurrence bit
AS

SET NOCOUNT ON

	SELECT		SFD.Specimen_Field_Data_Key AS Item_Key,
			SFD.Collection_Unit_Key AS Specimen_Key,
		    	CASE 
				WHEN SU.Life_Sciences = 0 THEN CT.Item_Name COLLATE SQL_Latin1_General_CP1_CI_AS
				ELSE 
					CASE ITN.Actual_Name_Italic 
						WHEN 1 THEN '<i>' + ITN.Actual_Name + '</i>'
						ELSE ITN.Actual_Name 
					END
			END +
			CASE 
				WHEN CUN.Number IS NOT NULL THEN + ' - ' + CUN.Number 
				ELSE + '' 
			END AS Display_Text,
			SFD.Timestamp,
			SFD.Custodian

	FROM		Specimen_Field_Data SFD
	INNER JOIN	Specimen_Unit SU ON SU.Collection_Unit_Key = SFD.Collection_Unit_Key
	LEFT JOIN	(Collection_Unit_Number CUN 
				INNER JOIN	Concept C ON C.Concept_Key = CUN.Type_Concept_Key 
						AND C.Meaning_Key = 'SYSTEM0000000001')  -- Registration Number
			ON CUN.Collection_Unit_Key = SFD.Collection_Unit_Key 
			AND CUN.Preferred = 1
	LEFT JOIN	Determination D 
				INNER JOIN	vw_ConceptTerm CT ON CT.Concept_Key = D.Concept_Key
			ON D.Determination_Key = SU.Preferred_Determination_Key
	LEFT JOIN 	(Taxon_Determination TD
				INNER JOIN	Index_Taxon_Name ITN ON TD.Taxon_List_Item_Key = ITN.Taxon_List_Item_Key)
			ON SU.Preferred_Taxon_Determination_Key = TD.Taxon_Determination_Key
			OR (SU.Preferred_Taxon_Determination_Key IS NULL AND TD.Taxon_Occurrence_Key = @OccurrenceKey)

	WHERE		(@IsTaxonOccurrence = 0 AND SFD.Occurrence_Key = @OccurrenceKey)
	OR		(@IsTaxonOccurrence = 1 AND SFD.Taxon_Occurrence_Key = @OccurrenceKey)

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SpecimenFieldData_Select_ForOccurrence') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SpecimenFieldData_Select_ForOccurrence'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SpecimenFieldData_Select_ForOccurrence TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SpecimenFieldData_Select_ForOccurrence TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SpecimenFieldData_Select_ForOccurrence TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_SpecimenFieldData_Select_ForOccurrence TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SpecimenFieldData_Select_ForOccurrence TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SpecimenFieldData_Select_ForOccurrence TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SpecimenLabel_Insert') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_SpecimenLabel_Insert]
GO
/*===========================================================================*\
  Description:	
  Parameters:	

  Created:	July 2003

  Last revision information:
    $Revision: 1 $
    $Date: 24/03/05 10:52 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SpecimenLabel_Insert]
	@Key char(16) OUTPUT,
	@CollectionUnitKey char(16),
	@IsInscription bit,
	@Position varchar(100) = NULL,		-- Because called through QuickEntry with no value.
	@Inscription nText,
	@Translated text,
	@LanguageConceptKey varchar(4),
	@InferredAuthor tinyint,
	@Comments text,
	@AuthorNameKey char(16) = NULL,
	@ConfidenceConceptKey char(16),
	@SessionID char(16)
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF
	
	EXECUTE spNextKey 'Specimen_Label', @Key OUTPUT

	BEGIN TRANSACTION	
		INSERT INTO Specimen_Label (
			Specimen_Label_Key, Collection_Unit_Key, Is_Inscription, Label_Position, Inscription,
			Translated, Translated_Language_Key, Inferred_Author, Comments, Author_Name_Key,
			Confidence_Concept_Key, Entered_Session_ID
		) VALUES (
			@Key, @CollectionUnitKey, @IsInscription, @Position, @Inscription, 
			@Translated, @LanguageConceptKey, IsNull(@InferredAuthor, 0), @Comments, @AuthorNameKey,
			@ConfidenceConceptKey, @SessionID
		)

		IF @@Error <> 0 GOTO RollBackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit:
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SpecimenLabel_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SpecimenLabel_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SpecimenLabel_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SpecimenLabel_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SpecimenLabel_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SpecimenLabel_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SpecimenLabel_Insert TO [Dev - JNCC SQL]
END
GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Specimens_Select_ForSearchByAnyDetermination]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Specimens_Select_ForSearchByAnyDetermination]
GO

/*===========================================================================*\
  Description:
	Returns Specimens based on the any determination.

  Parameters:
	@UserDomainMask		User's Domain Mask restricting which records may be returned
	@SessionID 		User's SessionID
	@ShowCommonNames	Specifies whether or not Common Names should be shown
	@SearchText		Text to be searched on
	@SortOrderIndex		Index determining Sort Order

  Created:
	October 2003

  Last revision information:
    $Revision: 1 $
    $Date: 24/03/05 10:52 $
    $Author: Johnvanbreda $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_Specimens_Select_ForSearchByAnyDetermination] 
	@UserDomainMask INT,
	@SessionID CHAR(16),
	@ShowCommonNames BIT,
	@SearchText VARCHAR(150),
	@SortOrderIndex TINYINT
AS
	SET NOCOUNT ON

	--Use temp table to build output, so silly data errors such as duplicate accessions
	-- don't duplicate in the list
	DECLARE @SpecimensSearch TABLE
	(
		[Item_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[Det_Item_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[Det_Item_Name] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[Item_Name] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[Life_Sciences] [bit] NULL,
		[Number] [varchar] (30) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[Hint] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL 
	)

	--Find all specimens with a determination match
	INSERT INTO @SpecimensSearch (Item_Key, Life_Sciences) 
	SELECT DISTINCT SU.Collection_Unit_Key, SU.Life_Sciences
	FROM 	SPECIMEN_UNIT SU
	JOIN 	Collection_Unit CU ON SU.Collection_Unit_Key = CU.Collection_Unit_Key 
		 	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
			OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
	JOIN 	VW_SpecimenDetsEarth SDE ON SU.Collection_Unit_Key = SDE.Collection_Unit_Key
	JOIN 	Concept C ON SDE.Concept_Key = C.Concept_Key
	JOIN 	Concept CSearch ON CSearch.Meaning_Key=C.Meaning_Key
	JOIN 	Term TSearch ON TSearch.Term_Key=CSearch.Term_Key
	WHERE 	(TSearch.Plaintext LIKE @SearchText + '%' AND SU.Life_Sciences=0) 

	INSERT INTO @SpecimensSearch (Item_Key, Life_Sciences) 
	SELECT DISTINCT SU.Collection_Unit_Key, SU.Life_Sciences
	FROM 	SPECIMEN_UNIT SU
	JOIN 	Collection_Unit CU ON SU.Collection_Unit_Key = CU.Collection_Unit_Key 
	 		AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
			OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
	JOIN 	VW_SpecimenDetsLife SDL ON SU.Collection_Unit_Key = SDL.Collection_Unit_Key
	JOIN 	Index_Taxon_Synonym ITS ON ITS.Taxon_List_Item_Key=SDL.Taxon_List_Item_Key
	JOIN 	INDEX_TAXON_NAME ITN	ON ITS.Synonym_List_Item_Key = ITN.Taxon_List_Item_Key
	WHERE 	(ITN.Actual_Name LIKE @SearchText + '%' AND SU.Life_Sciences=1)

	UPDATE @SpecimensSearch
	SET	Det_Item_Key=CPref.Concept_Key,
		Item_Name = TPref.Item_Name,
		Number=CUN.Number,
		Det_Item_Name=TDet.Plaintext,
		Hint=TDet.Plaintext
	FROM @SpecimensSearch SU
	LEFT JOIN Collection_Unit_Number CUN ON SU.Item_key = CUN.Collection_Unit_Key 
		AND CUN.Type_Concept_Key = 'SYSTEM0000000001' AND CUN.Preferred=1
	INNER JOIN VW_SpecimenDetsEarth SDE ON SDE.Collection_Unit_Key=SU.Item_Key
	INNER JOIN Concept C ON C.Concept_Key=SDE.Concept_Key
	INNER JOIN Term TDet ON TDet.Term_Key=C.Term_Key
	INNER JOIN Concept CPref ON CPref.Meaning_Key=C.Meaning_Key
		AND CPref.List_Preferred=1
		AND CPref.Concept_Group_Key=C.Concept_Group_Key
	INNER JOIN Term TPref ON TPref.Term_Key=CPref.Term_Key

	UPDATE @SpecimensSearch
	SET	Det_Item_Key=SDL.Taxon_List_Item_Key,
		Item_Name = dbo.ufn_GetFormattedTaxonNameByParams(
				Preferred_Name,
				Preferred_Name_Italic,
				Common_Name,
				Common_Name_Italic,
				null,
				@ShowCommonNames),
		Number=CUN.Number,
		Det_Item_Name=ITN.Actual_Name,
		Hint=ITN.Actual_Name
	FROM @SpecimensSearch SU
	LEFT JOIN Collection_Unit_Number CUN ON SU.Item_key = CUN.Collection_Unit_Key 
			AND CUN.Type_Concept_Key = 'SYSTEM0000000001' AND CUN.Preferred=1
	INNER JOIN VW_SpecimenDetsLife SDL ON SDL.Collection_Unit_Key=SU.Item_Key
			AND SDL.Preferred_Taxon_Determination_Key = SDL.Taxon_Determination_Key
	INNER JOIN Index_Taxon_Name ITN	ON ITN.Taxon_List_Item_Key=SDL.Taxon_List_Item_Key

	-- Select table and sort appropriately
	IF @SortOrderIndex = 0
		SELECT * from @SpecimensSearch
		ORDER BY Item_Name, Number
	ELSE IF @SortOrderIndex = 1
		SELECT * from @SpecimensSearch
		ORDER BY Number, Item_Name
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Specimens_Select_ForSearchByAnyDetermination') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Specimens_Select_ForSearchByAnyDetermination'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByAnyDetermination TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByAnyDetermination TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByAnyDetermination TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByAnyDetermination TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByAnyDetermination TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByAnyDetermination TO [Dev - JNCC SQL]
END
GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Specimens_Select_ForSearchByDescription]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Specimens_Select_ForSearchByDescription]
GO

/*===========================================================================*\
  Description:
	Returns Specimens data based on a search using the Description parameter.

  Parameters:
	@UserDomainMask		User's Domain Mask restricting which records may be returned
	@SessionID 		User's SessionID
	@ShowCommonNames	Specifies whether or not Common Names should be shown
	@SearchText		Text to be searched on
	@SortOrderIndex		Index determining Sort Order

  Created:
	October 2003

  Last revision information:
    $Revision: 1 $
    $Date: 24/03/05 10:52 $
    $Author: Johnvanbreda $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_Specimens_Select_ForSearchByDescription] 
	@UserDomainMask INT,
	@SessionID CHAR(16),
	@ShowCommonNames BIT,
	@SearchText VARCHAR(100),
	@SortOrderIndex TINYINT
AS
	SET NOCOUNT ON
	-- Set of options for better use of vw_ConceptTerm.
	SET ANSI_NULLS ON
	SET ANSI_PADDING ON
	SET ANSI_WARNINGS ON
	SET ARITHABORT ON
	SET CONCAT_NULL_YIELDS_NULL ON
	SET QUOTED_IDENTIFIER ON
	SET NO_BROWSETABLE OFF

	--Use temp table to build output, so silly data errors such as duplicate accessions
	-- don't duplicate in the list
	DECLARE @SpecimensSearch TABLE
	(
		[Item_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[Det_Item_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[Det_Item_Name] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[Item_Name] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[Life_Sciences] [bit] NULL,
		[Number] [varchar] (30) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[Hint] [text] COLLATE SQL_Latin1_General_CP1_CI_AS NULL 
	)

	INSERT INTO @SpecimensSearch (Item_Key, Life_Sciences, Hint) 
	SELECT 	SU.Collection_Unit_Key, SU.Life_Sciences, M.Text
	FROM 	Specimen_Unit SU
	JOIN 	Metadata M ON M.Record_Key=SU.Collection_Unit_Key
			AND M.Metadata_Type_Key='SYSTEM0000000006'
	JOIN 	Collection_Unit CU ON SU.Collection_Unit_Key = CU.Collection_Unit_Key 
	 		AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
			OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
	WHERE 	M.Text LIKE @SearchText + '%'

	UPDATE @SpecimensSearch
	SET	Det_Item_Key=CPref.Concept_Key,
		Item_Name = TPref.Item_Name,
		Number=CUN.Number,
		Det_Item_Name=TDet.Plaintext
	FROM @SpecimensSearch SU
	LEFT JOIN Collection_Unit_Number CUN ON SU.Item_key = CUN.Collection_Unit_Key 
		AND CUN.Type_Concept_Key = 'SYSTEM0000000001' AND CUN.Preferred=1
	INNER JOIN VW_SpecimenDetsEarth SDE ON SDE.Collection_Unit_Key=SU.Item_Key
	INNER JOIN Concept C ON C.Concept_Key=SDE.Concept_Key
	INNER JOIN Term TDet ON TDet.Term_Key=C.Term_Key
	INNER JOIN Concept CPref ON CPref.Meaning_Key=C.Meaning_Key
		AND CPref.List_Preferred=1
		AND CPref.Concept_Group_Key=C.Concept_Group_Key
	INNER JOIN Term TPref ON TPref.Term_Key=CPref.Term_Key


	UPDATE @SpecimensSearch
	SET	Det_Item_Key=SDL.Taxon_List_Item_Key,
		Item_Name = dbo.ufn_GetFormattedTaxonNameByParams(
				Preferred_Name,
				Preferred_Name_Italic,
				Common_Name,
				Common_Name_Italic,
				null,
				@ShowCommonNames),
		Number=CUN.Number,
		Det_Item_Name=ITN.Actual_Name
	FROM @SpecimensSearch SU
	LEFT JOIN Collection_Unit_Number CUN ON SU.Item_key = CUN.Collection_Unit_Key 
			AND CUN.Type_Concept_Key = 'SYSTEM0000000001' AND CUN.Preferred=1
	INNER JOIN VW_SpecimenDetsLife SDL ON SDL.Collection_Unit_Key=SU.Item_Key
			AND SDL.Preferred_Taxon_Determination_Key = SDL.Taxon_Determination_Key
	INNER JOIN Index_Taxon_Name ITN	ON ITN.Taxon_List_Item_Key=SDL.Taxon_List_Item_Key

	-- Select table and sort appropriately
	IF @SortOrderIndex = 0
		SELECT * from @SpecimensSearch
		ORDER BY Det_Item_Name, Number
	ELSE IF @SortOrderIndex = 1
		SELECT * from @SpecimensSearch
		ORDER BY Number, Item_Name
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Specimens_Select_ForSearchByDescription') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Specimens_Select_ForSearchByDescription'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByDescription TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByDescription TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByDescription TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByDescription TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByDescription TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByDescription TO [Dev - JNCC SQL]
END
GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Specimens_Select_ForSearchByDeterminationInGroup]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Specimens_Select_ForSearchByDeterminationInGroup]
GO

/*===========================================================================*\
  Description:
	Returns Specimens data based on the search parameter for Gathering Location.

  Parameters:
	@UserDomainMask		User's Domain Mask restricting which records may be returned
	@SessionID 		User's SessionID
	@ShowCommonNames	Specifies whether or not Common Names should be shown
	@SearchText		Text to be searched on
	@SortOrderIndex		Index determining Sort Order

  Created:
	October 2003

  Last revision information:
    $Revision: 1 $
    $Date: 24/03/05 10:52 $
    $Author: Johnvanbreda $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_Specimens_Select_ForSearchByDeterminationInGroup] 
	@UserDomainMask INT,
	@SessionID CHAR(16),
	@ShowCommonNames BIT,
	@SearchText VARCHAR(100),
	@SortOrderIndex TINYINT
AS
	SET NOCOUNT ON

	-- Set of options for better use of vw_ConceptTerm.
	SET ANSI_NULLS ON
	SET ANSI_PADDING ON
	SET ANSI_WARNINGS ON
	SET ARITHABORT ON
	SET CONCAT_NULL_YIELDS_NULL ON
	SET QUOTED_IDENTIFIER ON
	SET NO_BROWSETABLE OFF

	--Use temp table to build output, so silly data errors such as duplicate accessions
	-- don't duplicate in the list
	DECLARE @SpecimensSearch TABLE
	(
		[Item_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[Det_Item_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[Det_Item_Name] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[Item_Name] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[Life_Sciences] [bit] NULL,
		[Number] [varchar] (30) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[Hint] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL 
	)

	--Create a temp table to hold the meanings of the contents of the groups that match the search
	DECLARE @SearchLineage TABLE (
		Child_Meaning_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS,
		PlainText varchar(150) COLLATE SQL_Latin1_General_CP1_CI_AS
	)

	INSERT INTO 	@SearchLineage
	SELECT DISTINCT CChild.Meaning_Key, CSearch.PlainText
	FROM 		vw_ConceptTerm CSearch
	INNER JOIN 	Concept_Group CG ON CG.Concept_Group_Key = CSearch.Concept_Group_Key
	INNER JOIN 	Local_Domain LD ON LD.Local_Domain_Key = CG.Local_Domain_Key
	INNER JOIN 	Domain D ON D.Domain_Key = LD.Domain_Key AND D.Has_Occurrences = 1
	INNER JOIN 	Concept CSynSearch ON CSynSearch.Meaning_Key = CSearch.Meaning_Key
	INNER JOIN 	Concept_Lineage CL ON CL.Concept_Key = CSynSearch.Concept_Key
	INNER JOIN 	Concept_Lineage CLChild ON CLChild.Lineage LIKE CL.Lineage + '\%'
	INNER JOIN 	Concept CChild ON CChild.Concept_Key = CLChild.Concept_Key AND (CChild.Concept_Group_Key = CSynSearch.Concept_Group_Key)
	WHERE 		CSearch.Plaintext LIKE @SearchText + '%'

	INSERT INTO 	@SpecimensSearch (Item_Key, Life_Sciences, Hint) 
	SELECT DISTINCT SDE.Collection_Unit_Key, 0 AS Life_Sciences, SL.Plaintext
	FROM 		@SearchLineage SL
	INNER JOIN 	vw_ConceptTerm CChildSyn ON CChildSyn.Meaning_Key = SL.Child_Meaning_Key
	INNER JOIN 	vw_SpecimenDetsEarth SDE ON SDE.Concept_Key = CChildSyn.Concept_Key
	INNER JOIN 	Collection_Unit CU ON CU.Collection_Unit_Key = SDE.Collection_Unit_Key	
	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
	
	
	INSERT INTO 	@SpecimensSearch (Item_Key, Life_Sciences, Hint) 
	SELECT DISTINCT SDL.Collection_Unit_Key AS Item_Key, 1 AS Life_Sciences, ITNSearch.Actual_Name
	FROM 		Index_Taxon_Name ITNSearch
	INNER JOIN 	Index_Taxon_Synonym ITSSearch ON ITSSearch.Taxon_List_Item_Key = ITNSearch.Taxon_List_Item_Key
	INNER JOIN 	Index_Taxon_Group ITG ON ITG.Taxon_List_Item_Key = ITSSearch.Synonym_List_Item_Key
	INNER JOIN 	Index_Taxon_Synonym ITSSyn ON ITSSyn.Taxon_List_Item_Key = ITG.Contained_List_Item_Key
	INNER JOIN 	Index_Taxon_Name ITNSyn ON ITNSyn.Taxon_List_Item_Key = ITSSyn.Synonym_List_Item_Key
	INNER JOIN 	vw_SpecimenDetsLife SDL ON SDL.Taxon_List_Item_Key = ITNSyn.Taxon_List_Item_Key
	INNER JOIN 	Collection_Unit CU ON CU.Collection_Unit_Key = SDL.Collection_Unit_Key
		AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
	WHERE 		ITNSearch.Actual_Name LIKE @SearchText + '%'

	UPDATE 	@SpecimensSearch
	SET	Det_Item_Key=CPref.Concept_Key,
		Item_Name = TPref.Item_Name,
		Number=CUN.Number,
		Det_Item_Name=TDet.Plaintext 
	FROM 		@SpecimensSearch SU
	INNER JOIN 	Collection_Unit_Number CUN ON SU.Item_key = CUN.Collection_Unit_Key AND CUN.Type_Concept_Key = 'SYSTEM0000000001' AND CUN.Preferred = 1
	INNER JOIN 	VW_SpecimenDetsEarth SDE ON SDE.Collection_Unit_Key=SU.Item_Key
	INNER JOIN 	Concept C ON C.Concept_Key=SDE.Concept_Key
	INNER JOIN 	Term TDet ON TDet.Term_Key=C.Term_Key
	INNER JOIN 	Concept CPref ON CPref.Meaning_Key = C.Meaning_Key AND CPref.List_Preferred = 1 AND CPref.Concept_Group_Key = C.Concept_Group_Key
	INNER JOIN 	Term TPref ON TPref.Term_Key = CPref.Term_Key

	UPDATE 	@SpecimensSearch
	SET	Det_Item_Key=SDL.Taxon_List_Item_Key,
		Item_Name = dbo.ufn_GetFormattedTaxonNameByParams(
					Preferred_Name,
					Preferred_Name_Italic,
					Common_Name,
					Common_Name_Italic,
					NULL,
					@ShowCommonNames),
		Number=CUN.Number,
		Det_Item_Name=ITN.Actual_Name
	FROM 		@SpecimensSearch SU
	INNER JOIN 	Collection_Unit_Number CUN ON SU.Item_key = CUN.Collection_Unit_Key 
		AND CUN.Type_Concept_Key = 'SYSTEM0000000001' AND CUN.Preferred = 1
	INNER JOIN 	VW_SpecimenDetsLife SDL ON SU.Item_Key = SDL.Collection_Unit_Key
	INNER JOIN 	Index_Taxon_Name ITN	ON ITN.Taxon_List_Item_Key=SDL.Taxon_List_Item_Key

	-- Select table and sort appropriately
	IF @SortOrderIndex = 0
		SELECT * from @SpecimensSearch
		ORDER BY Det_Item_Name, Number
	ELSE IF @SortOrderIndex = 1
		SELECT * from @SpecimensSearch
		ORDER BY Number, Item_Name

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Specimens_Select_ForSearchByDeterminationInGroup') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Specimens_Select_ForSearchByDeterminationInGroup'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByDeterminationInGroup TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByDeterminationInGroup TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByDeterminationInGroup TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByDeterminationInGroup TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByDeterminationInGroup TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByDeterminationInGroup TO [Dev - JNCC SQL]
END
GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Specimens_Select_ForSearchByGatheringDate]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Specimens_Select_ForSearchByGatheringDate]
GO

/*===========================================================================*\
  Description:
	Returns Specimens data based on the search parameter for Gathering Date.

  Parameters:
	@UserDomainMask		User's Domain Mask restricting which records may be returned
	@SessionID 		User's SessionID
	@ShowCommonNames	Specifies whether or not Common Names should be shown
	@SearchText		Text to be searched on
	@SortOrderIndex		Index determining Sort Order

  Created:
	October 2003

  Last revision information:
    $Revision: 1 $
    $Date: 24/03/05 10:52 $
    $Author: Johnvanbreda $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_Specimens_Select_ForSearchByGatheringDate] 
	@UserDomainMask INT,
	@SessionID CHAR(16),
	@ShowCommonNames BIT,
	@SearchText VARCHAR(50),
	@SortOrderIndex TINYINT
AS
	SET NOCOUNT ON
	-- Set of options for better use of vw_ConceptTerm.
	SET ANSI_NULLS ON
	SET ANSI_PADDING ON
	SET ANSI_WARNINGS ON
	SET ARITHABORT ON
	SET CONCAT_NULL_YIELDS_NULL ON
	SET QUOTED_IDENTIFIER ON
	SET NO_BROWSETABLE OFF

	--Use temp table to build output, so silly data errors such as duplicate accessions
	-- don't duplicate in the list
	DECLARE @SpecimensSearch TABLE
	(
		[Item_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[Det_Item_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[Det_Item_Name] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[Item_Name] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[Life_Sciences] [bit] NULL,
		[Number] [varchar] (30) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[Hint] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL 
	)

	INSERT INTO @SpecimensSearch (Item_Key, Life_Sciences, Hint) 
	SELECT DISTINCT 
		SU.Collection_Unit_Key, 
		SU.Life_Sciences, 
		dbo.ufn_GetDateFromVagueDate(S.Vague_Date_Start, S.Vague_Date_End, S.Vague_Date_Type)
	FROM Specimen_Unit SU
	INNER JOIN Collection_Unit CU ON SU.Collection_Unit_Key = CU.Collection_Unit_Key 
	 	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
		OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
	INNER JOIN Specimen_Field_Data SFD ON SFD.Collection_Unit_Key = SU.Collection_Unit_Key
		AND SFD.Gathering_Event=1
	LEFT JOIN Occurrence O ON O.Occurrence_Key = SFD.Occurrence_Key
	LEFT JOIN Taxon_Occurrence XO ON XO.Taxon_Occurrence_Key=SFD.Taxon_Occurrence_Key
	INNER JOIN [Sample] S ON S.Sample_Key=O.Sample_Key OR S.Sample_Key = XO.Sample_Key
	WHERE dbo.ufn_CBWrapperForDoVagueDatesOverlap(@SearchText, S.Vague_Date_Start, S.Vague_Date_End) = 1


	UPDATE @SpecimensSearch
	SET	Det_Item_Key=CPref.Concept_Key,
		Item_Name = TPref.Item_Name,
		Number=CUN.Number,
		Det_Item_Name=TDet.Plaintext
	FROM @SpecimensSearch SU
	LEFT JOIN Collection_Unit_Number CUN ON SU.Item_key = CUN.Collection_Unit_Key 
		AND CUN.Type_Concept_Key = 'SYSTEM0000000001' AND CUN.Preferred=1
	INNER JOIN VW_SpecimenDetsEarth SDE ON SDE.Collection_Unit_Key=SU.Item_Key
	INNER JOIN Concept C ON C.Concept_Key=SDE.Concept_Key
	INNER JOIN Term TDet ON TDet.Term_Key=C.Term_Key
	INNER JOIN Concept CPref ON CPref.Meaning_Key=C.Meaning_Key
		AND CPref.List_Preferred=1
		AND CPref.Concept_Group_Key=C.Concept_Group_Key
	INNER JOIN Term TPref ON TPref.Term_Key=CPref.Term_Key


	UPDATE @SpecimensSearch
	SET	Det_Item_Key=SDL.Taxon_List_Item_Key,
		Item_Name = dbo.ufn_GetFormattedTaxonNameByParams(
				Preferred_Name,
				Preferred_Name_Italic,
				Common_Name,
				Common_Name_Italic,
				null,
				@ShowCommonNames),
		Number=CUN.Number,
		Det_Item_Name=ITN.Actual_Name,
		Hint=ITN.Actual_Name
	FROM @SpecimensSearch SU
	LEFT JOIN Collection_Unit_Number CUN ON SU.Item_key = CUN.Collection_Unit_Key 
			AND CUN.Type_Concept_Key = 'SYSTEM0000000001' AND CUN.Preferred=1
	INNER JOIN VW_SpecimenDetsLife SDL ON SDL.Collection_Unit_Key=SU.Item_Key
			AND SDL.Preferred_Taxon_Determination_Key = SDL.Taxon_Determination_Key
	INNER JOIN Index_Taxon_Name ITN	ON ITN.Taxon_List_Item_Key=SDL.Taxon_List_Item_Key

	-- Select table and sort appropriately
	IF @SortOrderIndex = 0
		SELECT * from @SpecimensSearch
		ORDER BY Det_Item_Name, Number
	ELSE IF @SortOrderIndex = 1
		SELECT * from @SpecimensSearch
		ORDER BY Number, Item_Name
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Specimens_Select_ForSearchByGatheringDate') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Specimens_Select_ForSearchByGatheringDate'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByGatheringDate TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByGatheringDate TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByGatheringDate TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByGatheringDate TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByGatheringDate TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByGatheringDate TO [Dev - JNCC SQL]
END
GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Specimens_Select_ForSearchByGatheringLocation]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Specimens_Select_ForSearchByGatheringLocation]
GO

/*===========================================================================*\
  Description:
	Returns Specimens data based on the search parameter for Gathering Location.

  Parameters:
	@UserDomainMask		User's Domain Mask restricting which records may be returned
	@SessionID 		User's SessionID
	@ShowCommonNames	Specifies whether or not Common Names should be shown
	@SearchText		Text to be searched on
	@SortOrderIndex		Index determining Sort Order

  Created:
	October 2003

  Last revision information:
    $Revision: 1 $
    $Date: 24/03/05 10:52 $
    $Author: Johnvanbreda $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_Specimens_Select_ForSearchByGatheringLocation] 
	@UserDomainMask INT,
	@SessionID CHAR(16),
	@ShowCommonNames BIT,
	@SearchText VARCHAR(100),
	@SortOrderIndex TINYINT
AS
	SET NOCOUNT ON
	-- Set of options for better use of vw_ConceptTerm.
	SET ANSI_NULLS ON
	SET ANSI_PADDING ON
	SET ANSI_WARNINGS ON
	SET ARITHABORT ON
	SET CONCAT_NULL_YIELDS_NULL ON
	SET QUOTED_IDENTIFIER ON
	SET NO_BROWSETABLE OFF

	--Use temp table to build output, so silly data errors such as duplicate accessions
	-- don't duplicate in the list
	DECLARE @SpecimensSearch TABLE
	(
		[Item_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[Det_Item_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[Det_Item_Name] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[Item_Name] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[Life_Sciences] [bit] NULL,
		[Number] [varchar] (30) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[Hint] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL 
	)

	INSERT INTO @SpecimensSearch (Item_Key, Life_Sciences, Hint) 
	SELECT 	DISTINCT SU.Collection_Unit_Key, SU.Life_Sciences, LN.Item_Name
	FROM 	Specimen_Unit SU
	INNER JOIN Collection_Unit CU ON SU.Collection_Unit_Key = CU.Collection_Unit_Key 
	 	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
		OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
	INNER JOIN Specimen_Field_Data SFD ON SFD.Collection_Unit_Key = SU.Collection_Unit_Key
		AND SFD.Gathering_Event=1
	LEFT JOIN Occurrence O ON O.Occurrence_Key = SFD.Occurrence_Key
	LEFT JOIN Taxon_Occurrence XO ON XO.Taxon_Occurrence_Key=SFD.Taxon_Occurrence_Key
	INNER JOIN [Sample] S ON S.Sample_Key=O.Sample_Key OR S.Sample_Key = XO.Sample_Key
	INNER JOIN [Survey_Event] SE ON SE.Survey_Event_Key=S.Survey_Event_Key
	INNER JOIN Location_Name LN ON LN.Location_Key=S.Location_Key OR LN.Location_KEY=SE.Location_Key
	WHERE LN.Item_Name LIKE @SearchText + '%'

	UPDATE @SpecimensSearch
	SET	Det_Item_Key=CPref.Concept_Key,
		Item_Name = TPref.Item_Name,
		Number=CUN.Number,
		Det_Item_Name=TDet.Plaintext,
		Hint=TDet.Plaintext
	FROM @SpecimensSearch SU
	LEFT JOIN Collection_Unit_Number CUN ON SU.Item_key = CUN.Collection_Unit_Key 
		AND CUN.Type_Concept_Key = 'SYSTEM0000000001' AND CUN.Preferred=1
	INNER JOIN VW_SpecimenDetsEarth SDE ON SDE.Collection_Unit_Key=SU.Item_Key
	INNER JOIN Concept C ON C.Concept_Key=SDE.Concept_Key
	INNER JOIN Term TDet ON TDet.Term_Key=C.Term_Key
	INNER JOIN Concept CPref ON CPref.Meaning_Key=C.Meaning_Key
		AND CPref.List_Preferred=1
		AND CPref.Concept_Group_Key=C.Concept_Group_Key
	INNER JOIN Term TPref ON TPref.Term_Key=CPref.Term_Key


	UPDATE @SpecimensSearch
	SET	Det_Item_Key=SDL.Taxon_List_Item_Key,
		Item_Name = dbo.ufn_GetFormattedTaxonNameByParams(
				Preferred_Name,
				Preferred_Name_Italic,
				Common_Name,
				Common_Name_Italic,
				NULL,
				@ShowCommonNames),
		Number=CUN.Number,
		Det_Item_Name=ITN.Actual_Name,
		Hint=ITN.Actual_Name
	FROM @SpecimensSearch SU
	LEFT JOIN Collection_Unit_Number CUN ON SU.Item_key = CUN.Collection_Unit_Key 
			AND CUN.Type_Concept_Key = 'SYSTEM0000000001' AND CUN.Preferred=1
	INNER JOIN VW_SpecimenDetsLife SDL ON SDL.Collection_Unit_Key=SU.Item_Key
			AND SDL.Preferred_Taxon_Determination_Key = SDL.Taxon_Determination_Key
	INNER JOIN Index_Taxon_Name ITN	ON ITN.Taxon_List_Item_Key=SDL.Taxon_List_Item_Key


-- Select table and sort appropriately
IF @SortOrderIndex = 0
	SELECT * from @SpecimensSearch
	ORDER BY Det_Item_Name, Number
ELSE IF @SortOrderIndex = 1
	SELECT * from @SpecimensSearch
	ORDER BY Number, Item_Name
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Specimens_Select_ForSearchByGatheringLocation') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Specimens_Select_ForSearchByGatheringLocation'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByGatheringLocation TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByGatheringLocation TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByGatheringLocation TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByGatheringLocation TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByGatheringLocation TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByGatheringLocation TO [Dev - JNCC SQL]
END
GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Specimens_Select_ForSearchByGeographicInformation]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Specimens_Select_ForSearchByGeographicInformation]
GO

/*===========================================================================*\
  Description:
	Returns Specimens data based on the search parameter for Geographic Information.

  Parameters:
	@UserDomainMask		User's Domain Mask restricting which records may be returned
	@SessionID 		User's SessionID
	@ShowCommonNames	Specifies whether or not Common Names should be shown
	@SearchText		Text to be searched on
	@SortOrderIndex		Index determining Sort Order

  Created:
	October 2003

  Last revision information:
    $Revision: 1 $
    $Date: 24/03/05 10:52 $
    $Author: Johnvanbreda $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_Specimens_Select_ForSearchByGeographicInformation] 
	@UserDomainMask INT,
	@SessionID CHAR(16),
	@ShowCommonNames BIT,
	@SearchText VARCHAR(100),
	@SortOrderIndex TINYINT
AS
	SET NOCOUNT ON
	-- Set of options for better use of vw_ConceptTerm.
	SET ANSI_NULLS ON
	SET ANSI_PADDING ON
	SET ANSI_WARNINGS ON
	SET ARITHABORT ON
	SET CONCAT_NULL_YIELDS_NULL ON
	SET QUOTED_IDENTIFIER ON
	SET NO_BROWSETABLE OFF

	--Use temp table to build output, so silly data errors such as duplicate accessions
	-- don't duplicate in the list
	DECLARE @SpecimensSearch TABLE
	(
		[Item_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[Det_Item_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[Det_Item_Name] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[Item_Name] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[Life_Sciences] [bit] NULL,
		[Number] [varchar] (30) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[Hint] [text] COLLATE SQL_Latin1_General_CP1_CI_AS NULL 
	)

	INSERT INTO @SpecimensSearch (Item_Key, Life_Sciences, Hint) 
	SELECT 	SU.Collection_Unit_Key, SU.Life_Sciences, M.Text
	FROM 	Specimen_Unit SU
	JOIN 	Metadata M ON M.Record_Key=SU.Collection_Unit_Key
			AND M.Metadata_Type_Key='SYSTEM0000000005'
	JOIN 	Collection_Unit CU ON SU.Collection_Unit_Key = CU.Collection_Unit_Key 
	 		AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
			OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
	WHERE 	M.Text LIKE @SearchText + '%'


	UPDATE 	@SpecimensSearch
	SET	Det_Item_Key=CPref.Concept_Key,
		Item_Name = TPref.Item_Name,
		Number=CUN.Number,
		Det_Item_Name=TDet.Plaintext
	FROM @SpecimensSearch SU
	LEFT JOIN Collection_Unit_Number CUN ON SU.Item_key = CUN.Collection_Unit_Key 
			AND CUN.Type_Concept_Key = 'SYSTEM0000000001' AND CUN.Preferred=1
	INNER JOIN VW_SpecimenDetsEarth SDE ON SDE.Collection_Unit_Key=SU.Item_Key
	INNER JOIN Concept C ON C.Concept_Key=SDE.Concept_Key
	INNER JOIN Term TDet ON TDet.Term_Key=C.Term_Key
	INNER JOIN Concept CPref ON CPref.Meaning_Key=C.Meaning_Key
			AND CPref.List_Preferred=1
			AND CPref.Concept_Group_Key=C.Concept_Group_Key
	INNER JOIN Term TPref ON TPref.Term_Key=CPref.Term_Key


	UPDATE @SpecimensSearch
	SET	Det_Item_Key = SDL.Taxon_List_Item_Key,
		Item_Name = dbo.ufn_GetFormattedTaxonNameByParams(
				Preferred_Name,
				Preferred_Name_Italic,
				Common_Name,
				Common_Name_Italic,
				NULL,
				@ShowCommonNames),
		Number = CUN.Number,
		Det_Item_Name = ITN.Actual_Name
	FROM 	@SpecimensSearch SU
	LEFT JOIN Collection_Unit_Number CUN ON SU.Item_key = CUN.Collection_Unit_Key 
			AND CUN.Type_Concept_Key = 'SYSTEM0000000001' AND CUN.Preferred = 1
	INNER JOIN VW_SpecimenDetsLife SDL ON SDL.Collection_Unit_Key = SU.Item_Key
			AND SDL.Preferred_Taxon_Determination_Key = SDL.Taxon_Determination_Key
	INNER JOIN Index_Taxon_Name ITN	ON ITN.Taxon_List_Item_Key = SDL.Taxon_List_Item_Key

	-- Select table and sort appropriately
	IF @SortOrderIndex = 0
		SELECT * FROM @SpecimensSearch
		ORDER BY Det_Item_Name, Number
	ELSE IF @SortOrderIndex = 1
		SELECT * FROM @SpecimensSearch
		ORDER BY Number, Item_Name
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Specimens_Select_ForSearchByGeographicInformation') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Specimens_Select_ForSearchByGeographicInformation'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByGeographicInformation TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByGeographicInformation TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByGeographicInformation TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByGeographicInformation TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByGeographicInformation TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByGeographicInformation TO [Dev - JNCC SQL]
END
GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Specimens_Select_ForSearchByPreferredAccNumber]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Specimens_Select_ForSearchByPreferredAccNumber]
GO

/*===========================================================================*\
  Description:
	Returns Specimens based on the Accession Number.

  Parameters:
	@UserDomainMask		User's Domain Mask restricting which records may be returned
	@SessionID 		User's SessionID
	@ShowCommonNames	Specifies whether or not Common Names should be shown
	@SearchText		Text to be searched on
	@SortOrderIndex		Index determining Sort Order

  Created:
	October 2003

  Last revision information:
    $Revision: 1 $
    $Date: 24/03/05 10:52 $
    $Author: Johnvanbreda $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_Specimens_Select_ForSearchByPreferredAccNumber] 
	@UserDomainMask INT,
	@SessionID CHAR(16),
	@ShowCommonNames BIT,
	@SearchText VARCHAR(30),
	@SortOrderIndex TINYINT
AS
	SET NOCOUNT ON
	-- Set of options for better use of vw_ConceptTerm.
	SET ANSI_NULLS ON
	SET ANSI_PADDING ON
	SET ANSI_WARNINGS ON
	SET ARITHABORT ON
	SET CONCAT_NULL_YIELDS_NULL ON
	SET QUOTED_IDENTIFIER ON
	SET NO_BROWSETABLE OFF

	--Use temp table to build output, so silly data errors such as duplicate accessions
	-- don't duplicate in the list
	DECLARE @SpecimensSearch TABLE
	(
		[Item_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[Det_Item_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[Det_Item_Name] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[Item_Name] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[Life_Sciences] [bit] NULL,
		[Number] [varchar] (30) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[Hint] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL 
	)

	INSERT INTO @SpecimensSearch (Item_Key, Life_Sciences, Hint) 
	SELECT DISTINCT SU.Collection_Unit_Key, SU.Life_Sciences, M.Number
	FROM SPECIMEN_UNIT SU
	INNER JOIN Collection_Unit CU ON SU.Collection_Unit_Key = CU.Collection_Unit_Key 
	 	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
		OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
	INNER JOIN Movement_Collection_Unit MCU ON MCU.Collection_Unit_Key = SU.Collection_Unit_Key
	INNER JOIN Movement_Direction MD ON MD.Movement_Direction_Key=MCU.Movement_Direction_Key
		AND MD.Outbound=0
	INNER JOIN Movement_Of_Ownership MOE ON MOE.Movement_Direction_Key=MD.Movement_Direction_Key
	LEFT JOIN Movement_Of_Ownership_Exclusion MOEE 
		ON MOEE.Movement_Of_Ownership_Key=MOE.Movement_Of_Ownership_Key
		AND SU.Collection_Unit_Key=MOEE.Collection_Unit_Key
	INNER JOIN Movement M ON M.Movement_Key=MD.Movement_Key
		AND M.Movement_Type IN (0,1)
	WHERE M.Number LIKE @SearchText + '%'
	AND MOEE.Movement_Of_Ownership_Exclusion_Key IS NULL


	UPDATE @SpecimensSearch
	SET	Det_Item_Key=CPref.Concept_Key,
		Item_Name = TPref.Item_Name,
		Number=CUN.Number,
		Det_Item_Name=TDet.Plaintext
	FROM @SpecimensSearch SU
	LEFT JOIN Collection_Unit_Number CUN ON SU.Item_key = CUN.Collection_Unit_Key 
		AND CUN.Type_Concept_Key = 'SYSTEM0000000001' AND CUN.Preferred=1
	INNER JOIN VW_SpecimenDetsEarth SDE ON SDE.Collection_Unit_Key=SU.Item_Key
	INNER JOIN Concept C ON C.Concept_Key=SDE.Concept_Key
	INNER JOIN Term TDet ON TDet.Term_Key=C.Term_Key
	INNER JOIN Concept CPref ON CPref.Meaning_Key=C.Meaning_Key
		AND CPref.List_Preferred=1
		AND CPref.Concept_Group_Key=C.Concept_Group_Key
	INNER JOIN Term TPref ON TPref.Term_Key=CPref.Term_Key

	UPDATE @SpecimensSearch
	SET	Det_Item_Key=SDL.Taxon_List_Item_Key,
		Item_Name = dbo.ufn_GetFormattedTaxonNameByParams(
				Preferred_Name,
				Preferred_Name_Italic,
				Common_Name,
				Common_Name_Italic,
				null,
				@ShowCommonNames),
		Number=CUN.Number,
		Det_Item_Name=ITN.Actual_Name
	FROM @SpecimensSearch SU
	LEFT JOIN Collection_Unit_Number CUN ON SU.Item_key = CUN.Collection_Unit_Key 
			AND CUN.Type_Concept_Key = 'SYSTEM0000000001' AND CUN.Preferred=1
	INNER JOIN VW_SpecimenDetsLife SDL ON SDL.Collection_Unit_Key=SU.Item_Key
			AND SDL.Preferred_Taxon_Determination_Key = SDL.Taxon_Determination_Key
	INNER JOIN Index_Taxon_Name ITN	ON ITN.Taxon_List_Item_Key=SDL.Taxon_List_Item_Key

	-- Select table and sort appropriately
	IF @SortOrderIndex = 0
		SELECT * from @SpecimensSearch
		ORDER BY Det_Item_Name, Number
	ELSE IF @SortOrderIndex = 1
		SELECT * from @SpecimensSearch
		ORDER BY Number, Item_Name
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Specimens_Select_ForSearchByPreferredAccNumber') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Specimens_Select_ForSearchByPreferredAccNumber'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByPreferredAccNumber TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByPreferredAccNumber TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByPreferredAccNumber TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByPreferredAccNumber TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByPreferredAccNumber TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByPreferredAccNumber TO [Dev - JNCC SQL]
END
GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Specimens_Select_ForSearchByPreferredDetermination]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Specimens_Select_ForSearchByPreferredDetermination]
GO

/*===========================================================================*\
  Description:
	Returns Specimens based on the the preferred determination.

  Parameters:
	@UserDomainMask		User's Domain Mask restricting which records may be returned
	@SessionID 		User's SessionID
	@ShowCommonNames	Specifies whether or not Common Names should be shown
	@SearchText		Text to be searched on
	@SortOrderIndex		Index determining Sort Order

  Created:
	October 2003

  Last revision information:
    $Revision: 1 $
    $Date: 24/03/05 10:52 $
    $Author: Johnvanbreda $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_Specimens_Select_ForSearchByPreferredDetermination] 
	@UserDomainMask INT,
	@SessionID CHAR(16),
	@ShowCommonNames BIT,
	@SearchText VARCHAR(150),
	@SortOrderIndex TINYINT
AS
	SET NOCOUNT ON
	-- Set of options for better use of vw_ConceptTerm.
	SET ANSI_NULLS ON
	SET ANSI_PADDING ON
	SET ANSI_WARNINGS ON
	SET ARITHABORT ON
	SET CONCAT_NULL_YIELDS_NULL ON
	SET QUOTED_IDENTIFIER ON
	SET NO_BROWSETABLE OFF

	--Use temp table to build output, so silly data errors such as duplicate accessions
	-- don't duplicate in the list
	DECLARE @SpecimensSearch TABLE
	(
		[Item_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[Det_Item_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[Det_Item_Name] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[Item_Name] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[Life_Sciences] [bit] NULL,
		[Number] [varchar] (30) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[Hint] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL 
	)

	--Find all specimens with a determination match
	INSERT INTO @SpecimensSearch (Item_Key, Life_Sciences) 
	SELECT DISTINCT SU.Collection_Unit_Key, SU.Life_Sciences
	FROM 	SPECIMEN_UNIT SU
	JOIN 	Collection_Unit CU ON SU.Collection_Unit_Key = CU.Collection_Unit_Key 
	 		AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
			OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
	JOIN 	VW_SpecimenDetsEarth SDE ON (SU.Collection_Unit_Key = SDE.Collection_Unit_Key)
			AND (SDE.Preferred_Determination_Key=SDE.Determination_Key)
	JOIN 	Concept C ON SDE.Concept_Key = C.Concept_Key
	JOIN 	Concept CSearch ON CSearch.Meaning_Key=C.Meaning_Key
	JOIN 	Term TSearch ON TSearch.Term_Key=CSearch.Term_Key
	WHERE (TSearch.Plaintext LIKE @SearchText + '%' AND SU.Life_Sciences=0) 

	INSERT INTO @SpecimensSearch (Item_Key, Life_Sciences) 
	SELECT DISTINCT SU.Collection_Unit_Key, SU.Life_Sciences
	FROM 	SPECIMEN_UNIT SU
	JOIN 	Collection_Unit CU ON SU.Collection_Unit_Key = CU.Collection_Unit_Key 
	 		AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
			OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
	JOIN 	VW_SpecimenDetsLife SDL ON SU.Collection_Unit_Key = SDL.Collection_Unit_Key
			AND (SDL.Preferred_Taxon_Determination_Key=SDL.Taxon_Determination_Key)
	JOIN 	Index_Taxon_Synonym ITS ON ITS.Taxon_List_Item_Key=SDL.Taxon_List_Item_Key
	JOIN 	INDEX_TAXON_NAME ITN	ON ITS.Synonym_List_Item_Key = ITN.Taxon_List_Item_Key
	WHERE 	(ITN.Actual_Name LIKE @SearchText + '%' AND SU.Life_Sciences=1)


	UPDATE @SpecimensSearch
	SET	Det_Item_Key=CPref.Concept_Key,
		Item_Name = TPref.Item_Name,
		Number=CUN.Number,
		Det_Item_Name=TDet.Plaintext,
		Hint=TDet.Plaintext
	FROM @SpecimensSearch SU
	LEFT JOIN Collection_Unit_Number CUN ON SU.Item_key = CUN.Collection_Unit_Key 
		AND CUN.Type_Concept_Key = 'SYSTEM0000000001' AND CUN.Preferred=1
	INNER JOIN VW_SpecimenDetsEarth SDE ON SDE.Collection_Unit_Key=SU.Item_Key
	INNER JOIN Concept C ON C.Concept_Key=SDE.Concept_Key
	INNER JOIN Term TDet ON TDet.Term_Key=C.Term_Key
	INNER JOIN Concept CPref ON CPref.Meaning_Key=C.Meaning_Key
		AND CPref.List_Preferred=1
		AND CPref.Concept_Group_Key=C.Concept_Group_Key
	INNER JOIN Term TPref ON TPref.Term_Key=CPref.Term_Key

	UPDATE @SpecimensSearch
	SET	Det_Item_Key=SDL.Taxon_List_Item_Key,
		Item_Name = dbo.ufn_GetFormattedTaxonNameByParams(
				Preferred_Name,
				Preferred_Name_Italic,
				Common_Name,
				Common_Name_Italic,
				null,
				@ShowCommonNames),
		Number=CUN.Number,
		Det_Item_Name=ITN.Actual_Name,
		Hint=ITN.Actual_Name
	FROM @SpecimensSearch SU
	LEFT JOIN Collection_Unit_Number CUN ON SU.Item_key = CUN.Collection_Unit_Key 
			AND CUN.Type_Concept_Key = 'SYSTEM0000000001' AND CUN.Preferred=1
	INNER JOIN VW_SpecimenDetsLife SDL ON SDL.Collection_Unit_Key=SU.Item_Key
			AND SDL.Preferred_Taxon_Determination_Key = SDL.Taxon_Determination_Key
	INNER JOIN Index_Taxon_Name ITN	ON ITN.Taxon_List_Item_Key=SDL.Taxon_List_Item_Key

	-- Select table and sort appropriately
	IF @SortOrderIndex = 0
		SELECT * from @SpecimensSearch
		ORDER BY Det_Item_Name, Number
	ELSE IF @SortOrderIndex = 1
		SELECT * from @SpecimensSearch
		ORDER BY Number, Det_Item_Name
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Specimens_Select_ForSearchByPreferredDetermination') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Specimens_Select_ForSearchByPreferredDetermination'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByPreferredDetermination TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByPreferredDetermination TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByPreferredDetermination TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByPreferredDetermination TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByPreferredDetermination TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByPreferredDetermination TO [Dev - JNCC SQL]
END
GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Specimens_Select_ForSearchByPreferredRegNumber]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Specimens_Select_ForSearchByPreferredRegNumber]
GO

/*===========================================================================*\
  Description:
	Returns Specimens data based on the search parameter for Preferred Reg Number.

  Parameters:
	@UserDomainMask		User's Domain Mask restricting which records may be returned
	@SessionID 		User's SessionID
	@ShowCommonNames	Specifies whether or not Common Names should be shown
	@SearchText		Text to be searched on
	@SortOrderIndex		Index determining Sort Order

  Created:
	October 2003

  Last revision information:
    $Revision: 1 $
    $Date: 24/03/05 10:52 $
    $Author: Johnvanbreda $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_Specimens_Select_ForSearchByPreferredRegNumber] 
	@UserDomainMask INT,
	@SessionID CHAR(16),
	@ShowCommonNames BIT,
	@SearchText VARCHAR(30),
	@SortOrderIndex TINYINT
AS
	SET NOCOUNT ON
	-- Set of options for better use of vw_ConceptTerm.
	SET ANSI_NULLS ON
	SET ANSI_PADDING ON
	SET ANSI_WARNINGS ON
	SET ARITHABORT ON
	SET CONCAT_NULL_YIELDS_NULL ON
	SET QUOTED_IDENTIFIER ON
	SET NO_BROWSETABLE OFF

	--Use temp table to build output, so silly data errors such as duplicate accessions
	-- don't duplicate in the list
	DECLARE @SpecimensSearch TABLE
	(
		[Item_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[Det_Item_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[Det_Item_Name] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[Item_Name] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[Life_Sciences] [bit] NULL,
		[Number] [varchar] (30) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[Hint] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL 
	)

	INSERT INTO @SpecimensSearch (Item_Key, Life_Sciences, Hint) 
	SELECT DISTINCT SU.Collection_Unit_Key, SU.Life_Sciences, CUN.Number
	FROM 	Specimen_Unit SU
	JOIN 	Collection_Unit CU ON SU.Collection_Unit_Key = CU.Collection_Unit_Key 
	   		AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
			OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
	JOIN 	Collection_Unit_Number CUN ON SU.Collection_Unit_key = CUN.Collection_Unit_Key 
			AND CUN.Type_Concept_Key = 'SYSTEM0000000001'  --REGISTRATION NUMBER
			AND CUN.Preferred = 1
			AND CUN.Number LIKE @SearchText + '%'

	UPDATE @SpecimensSearch
	SET	Det_Item_Key=CPref.Concept_Key,
		Item_Name = TPref.Item_Name,
		Number=CUN.Number,
		Det_Item_Name=TDet.Plaintext
	FROM @SpecimensSearch SU
	LEFT JOIN Collection_Unit_Number CUN ON SU.Item_key = CUN.Collection_Unit_Key 
		AND CUN.Type_Concept_Key = 'SYSTEM0000000001' AND CUN.Preferred=1
	INNER JOIN VW_SpecimenDetsEarth SDE ON SDE.Collection_Unit_Key=SU.Item_Key
	INNER JOIN Concept C ON C.Concept_Key=SDE.Concept_Key
	INNER JOIN Term TDet ON TDet.Term_Key=C.Term_Key
	INNER JOIN Concept CPref ON CPref.Meaning_Key=C.Meaning_Key
		AND CPref.List_Preferred=1
		AND CPref.Concept_Group_Key=C.Concept_Group_Key
	INNER JOIN Term TPref ON TPref.Term_Key=CPref.Term_Key

	UPDATE @SpecimensSearch
	SET	Det_Item_Key=SDL.Taxon_List_Item_Key,
		Item_Name = dbo.ufn_GetFormattedTaxonNameByParams(
				Preferred_Name,
				Preferred_Name_Italic,
				Common_Name,
				Common_Name_Italic,
				null,
				@ShowCommonNames),
		Number=CUN.Number,
		Det_Item_Name=ITN.Actual_Name
	FROM @SpecimensSearch SU
	LEFT JOIN Collection_Unit_Number CUN ON SU.Item_key = CUN.Collection_Unit_Key 
			AND CUN.Type_Concept_Key = 'SYSTEM0000000001' AND CUN.Preferred=1
	INNER JOIN VW_SpecimenDetsLife SDL ON SDL.Collection_Unit_Key=SU.Item_Key
			AND SDL.Preferred_Taxon_Determination_Key = SDL.Taxon_Determination_Key
	INNER JOIN Index_Taxon_Name ITN	ON ITN.Taxon_List_Item_Key=SDL.Taxon_List_Item_Key

	-- Select table and sort appropriately
	IF @SortOrderIndex = 0
		SELECT * from @SpecimensSearch
		ORDER BY Det_Item_Name, Number
	ELSE IF @SortOrderIndex = 1
		SELECT * from @SpecimensSearch
		ORDER BY Number, Item_Name
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Specimens_Select_ForSearchByPreferredRegNumber') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Specimens_Select_ForSearchByPreferredRegNumber'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByPreferredRegNumber TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByPreferredRegNumber TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByPreferredRegNumber TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByPreferredRegNumber TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByPreferredRegNumber TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByPreferredRegNumber TO [Dev - JNCC SQL]
END
GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Specimens_Select_ForSearchByType]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Specimens_Select_ForSearchByType]
GO

/*===========================================================================*\
  Description:
	Returns Specimens data based on the search parameter for Specimen Type.

  Parameters:
	@UserDomainMask		User's Domain Mask restricting which records may be returned
	@SessionID 		User's SessionID
	@ShowCommonNames	Specifies whether or not Common Names should be shown
	@SearchText		Text to be searched on
	@SortOrderIndex		Index determining Sort Order

  Created:
	October 2003

  Last revision information:
    $Revision: 1 $
    $Date: 24/03/05 10:52 $
    $Author: Johnvanbreda $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_Specimens_Select_ForSearchByType] 
	@UserDomainMask INT,
	@SessionID CHAR(16),
	@ShowCommonNames BIT,
	@SearchText VARCHAR(150),
	@SortOrderIndex TINYINT
AS
	SET NOCOUNT ON
	-- Set of options for better use of vw_ConceptTerm.
	SET ANSI_NULLS ON
	SET ANSI_PADDING ON
	SET ANSI_WARNINGS ON
	SET ARITHABORT ON
	SET CONCAT_NULL_YIELDS_NULL ON
	SET QUOTED_IDENTIFIER ON
	SET NO_BROWSETABLE OFF

	--Use temp table to build output, so silly data errors such as duplicate accessions
	-- don't duplicate in the list
	DECLARE @SpecimensSearch TABLE
	(
		[Item_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[Det_Item_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[Det_Item_Name] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[Item_Name] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[Life_Sciences] [bit] NULL,
		[Number] [varchar] (30) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[Hint] [nvarchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL 
	)

	INSERT INTO @SpecimensSearch (Item_Key, Life_Sciences, Hint) 
	SELECT DISTINCT SU.Collection_Unit_Key, SU.Life_Sciences, T.Plaintext
	FROM 	SPECIMEN_UNIT SU
	JOIN 	Collection_Unit CU ON SU.Collection_Unit_Key = CU.Collection_Unit_Key 
	 		AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
			OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
	JOIN 	Concept C ON C.Concept_Key = SU.Specimen_Type_Concept_Key
	JOIN 	Concept CSyn ON CSyn.Meaning_Key=C.Meaning_Key
	JOIN 	Term T ON T.Term_Key=CSyn.Term_Key
	WHERE 	T.Plaintext LIKE @Searchtext + '%'

	UPDATE @SpecimensSearch
	SET	Det_Item_Key=CPref.Concept_Key,
		Item_Name = TPref.Item_Name,
		Number=CUN.Number,
		Det_Item_Name=TDet.Plaintext
	FROM @SpecimensSearch SU
	LEFT JOIN Collection_Unit_Number CUN ON SU.Item_key = CUN.Collection_Unit_Key 
		AND CUN.Type_Concept_Key = 'SYSTEM0000000001' AND CUN.Preferred=1
	INNER JOIN VW_SpecimenDetsEarth SDE ON SDE.Collection_Unit_Key=SU.Item_Key
	INNER JOIN Concept C ON C.Concept_Key=SDE.Concept_Key
	INNER JOIN Term TDet ON TDet.Term_Key=C.Term_Key
	INNER JOIN Concept CPref ON CPref.Meaning_Key=C.Meaning_Key
		AND CPref.List_Preferred=1
		AND CPref.Concept_Group_Key=C.Concept_Group_Key
	INNER JOIN Term TPref ON TPref.Term_Key=CPref.Term_Key

	UPDATE @SpecimensSearch
	SET	Det_Item_Key=SDL.Taxon_List_Item_Key,
		Item_Name = dbo.ufn_GetFormattedTaxonNameByParams(
				Preferred_Name,
				Preferred_Name_Italic,
				Common_Name,
				Common_Name_Italic,
				null,
				@ShowCommonNames),
		Number=CUN.Number,
		Det_Item_Name=ITN.Actual_Name
	FROM @SpecimensSearch SU
	LEFT JOIN Collection_Unit_Number CUN ON SU.Item_key = CUN.Collection_Unit_Key 
			AND CUN.Type_Concept_Key = 'SYSTEM0000000001' AND CUN.Preferred=1
	INNER JOIN VW_SpecimenDetsLife SDL ON SDL.Collection_Unit_Key=SU.Item_Key
			AND SDL.Preferred_Taxon_Determination_Key = SDL.Taxon_Determination_Key
	INNER JOIN Index_Taxon_Name ITN	ON ITN.Taxon_List_Item_Key=SDL.Taxon_List_Item_Key

	-- Select table and sort appropriately
	IF @SortOrderIndex = 0
		SELECT * from @SpecimensSearch
		ORDER BY Det_Item_Name, Number
	ELSE IF @SortOrderIndex = 1
		SELECT * from @SpecimensSearch
		ORDER BY Number, Item_Name
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Specimens_Select_ForSearchByType') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Specimens_Select_ForSearchByType'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByType TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByType TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByType TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByType TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByType TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearchByType TO [Dev - JNCC SQL]
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
    $Date: 24/03/05 10:52 $
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
		INNER JOIN	Taxon_Determination AS TD ON TD.Taxon_Determination_Key = SU.Preferred_Taxon_Determination_Key
		INNER JOIN	Index_Taxon_Name AS ITN ON ITN.Taxon_List_Item_Key = TD.Taxon_List_Item_Key
	
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
		INNER JOIN	Determination AS D ON D.Determination_Key = SU.Preferred_Determination_Key
		INNER JOIN	Concept AS C ON C.Concept_Key = D.Concept_Key
		INNER JOIN	Term AS T ON T.Term_Key = C.Term_Key
	
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

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_StoragePlace_Select_ForCollectionUnit]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_StoragePlace_Select_ForCollectionUnit]
GO

/*===========================================================================*\
  Description:	Returns successive parent Stores for a specified Store

  Parameters:	@ParentKey 	Only the records associated with the parent key are returned

  Created:	August 2003

  Last revision information:
    $Revision: 1 $
    $Date: 24/03/05 10:52 $
    $Author: Johnvanbreda $
\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_StoragePlace_Select_ForCollectionUnit] 
	@UserDomainMask INT,
	@SessionID CHAR(16),
	@ParentKey CHAR(16)
AS

SET NOCOUNT ON

	DECLARE @StoragePlace TABLE
	(
		[Item_Index] [int] NOT NULL,
		[Collection_Unit_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
		[Join_Key][char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
		[Item_Name] [varchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
		[Number] [varchar] (30) NULL,
		[Type] [varchar] (100) NULL,
		[Code] [varchar] (30)
	)
	
	
	DECLARE @Collection_Unit_Key CHAR(16), 
		@Parent_Collection_Unit_Key CHAR(16), 
		@Item_Name VARCHAR(100), 
		@Number varchar(30), 
		@Item_Index INT,
		@Store_Code VARCHAR(30),
		@Store_Type varchar(100)

	--Obtain parent of input store
	SELECT	@Parent_Collection_Unit_Key = CU.Current_Container_Collection_Unit_Key
	FROM 	Collection_Unit CU
	WHERE	((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
		OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
	AND CU.Collection_Unit_Key = @ParentKey

	SET @Item_Index = 0

	--Obtain successive parents
	WHILE @Parent_Collection_Unit_Key IS NOT NULL
	BEGIN
		SELECT	@Collection_Unit_Key = S.Collection_Unit_Key, 
			@Item_Name = S.Item_Name + IsNull(' - ' + CU.Current_Location_Code, IsNull(' - ' + CU.Usual_Location_Code, '')),
			@Number = CUN.Number, 
			@Parent_Collection_Unit_Key = CU.Current_Container_Collection_Unit_Key,
			@Store_Code = CU.Current_Location_Code,
			@Store_Type = CT.Plaintext

		FROM 	Store S
		INNER JOIN	Collection_Unit CU
			ON S.Collection_Unit_Key = CU.Collection_Unit_Key
			AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
				OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
			AND S.Collection_Unit_Key = @Parent_Collection_Unit_Key

		LEFT JOIN 	Collection_Unit_Number CUN 
			ON S.Collection_Unit_Key = CUN.Collection_Unit_Key 
			AND CUN.Type_Concept_Key = 'SYSTEM0000000001'  --REGISTRATION NUMBER
			AND CUN.Preferred = 1
		LEFT JOIN	vw_ConceptTermPreferred AS CT ON CT.Concept_key = S.Store_Type_Concept_Key
	
		--Ensure we are not joining back to a previous store
		IF (@Item_Name IS NOT NULL) AND (@Collection_Unit_Key <> @ParentKey)
		AND NOT EXISTS(SELECT * FROM @StoragePlace WHERE Collection_Unit_Key = @Collection_Unit_Key)
		BEGIN
			INSERT @StoragePlace (Item_Index, Collection_Unit_Key, Join_Key, Item_Name, Number, Code, Type) 
			VALUES (@Item_Index, @Collection_Unit_Key, @Collection_Unit_Key, @Item_Name, @Number, @Store_Code, @Store_Type)

			SET @Item_Index = @Item_Index + 1
		END ELSE
			BREAK
	END

	--Return hierarchical list
	SELECT 	Collection_Unit_Key AS Item_Key, Join_Key, Item_Name, Number, Code, Type
	FROM 	@StoragePlace
	ORDER BY Item_Index DESC
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_StoragePlace_Select_ForCollectionUnit') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_StoragePlace_Select_ForCollectionUnit'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_StoragePlace_Select_ForCollectionUnit TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_StoragePlace_Select_ForCollectionUnit TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_StoragePlace_Select_ForCollectionUnit TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_StoragePlace_Select_ForCollectionUnit TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_StoragePlace_Select_ForCollectionUnit TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_StoragePlace_Select_ForCollectionUnit TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
       FROM   SysObjects
       WHERE  Id = Object_Id(N'[dbo].[usp_Store_PossibleChildStores_Select]')
       AND    Type = 'P')
    DROP PROCEDURE [dbo].[usp_Store_PossibleChildStores_Select]
GO

/*===========================================================================*\
  Description:  Select the list of stores that are currently in a store, or
					usually held in a store

  Parameters:   @Key	Store collection unit key

  Created:      Sept 2004

  Last revision information:
    $Revision: 1 $
    $Date: 24/03/05 10:52 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Store_PossibleChildStores_Select]
    @Key CHAR(16)
AS

	SELECT DISTINCT 
		S.Collection_Unit_Key, 
		S.Item_Name + ISNULL(' - ' + C.Current_Location_Code, ISNULL(' - ' + C.Usual_Location_Code, '')) AS Item_Name,
		CASE 
			WHEN C.Current_Container_Collection_Unit_Key=@Key THEN 1
			ELSE 0
		END AS IsCurrent
	FROM Store S
	INNER JOIN Collection_Unit C ON C.Collection_Unit_Key=S.Collection_Unit_Key
	LEFT JOIN Movement_Collection_Unit MCU ON MCU.Collection_Unit_Key=C.Collection_Unit_Key
	LEFT JOIN Movement_Direction MD ON MD.Movement_Direction_Key=MCU.Movement_Direction_Key
	LEFT JOIN Movement_Of_Material MOM ON MOM.Movement_Direction_Key=MD.Movement_Direction_Key
	LEFT JOIN Movement_Of_Material_Exclusion MOME 
			ON MOME.Movement_Of_Material_Key=MOM.Movement_Of_Material_Key
			AND MOME.Collection_Unit_Key=@Key
	LEFT JOIN Movement M 
			ON M.Movement_Key=MD.Movement_Key
			AND M.Movement_Type IN (4,5,7) -- destroyed, disposed, lost
	WHERE 	(C.Current_Container_Collection_Unit_Key=@Key
		OR C.Usual_Container_Collection_Unit_Key=@Key)
	AND 	(M.Movement_Key IS NULL
		OR MOME.Movement_Of_Material_Exclusion_Key IS NOT NULL)

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Store_PossibleChildStores_Select') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_Store_PossibleChildStores_Select'
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
        GRANT EXECUTE ON dbo.usp_Store_PossibleChildStores_Select TO [R2k_Administrator]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
        GRANT EXECUTE ON dbo.usp_Store_PossibleChildStores_Select TO [R2k_FullEdit]
 		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
        GRANT EXECUTE ON dbo.usp_Store_PossibleChildStores_Select TO [R2k_RecordCardsOnly]
 		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        GRANT EXECUTE ON dbo.usp_Store_PossibleChildStores_Select TO [R2k_AddOnly]
 		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
        GRANT EXECUTE ON dbo.usp_Store_PossibleChildStores_Select TO [R2k_ReadOnly]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_Store_PossibleChildStores_Select TO [Dev - JNCC SQL]
END

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Store_Select') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_Store_Select]
GO
 
/*===========================================================================*\
  Description:	Returns a store record.

  Parameters:	@Key	Collection_Unit key

  Created:	Oct 2003

  Last revision information:
    $Revision: 1 $
    $Date: 24/03/05 10:52 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Store_Select]
	@Key char(16)
AS

SET NOCOUNT ON

	SELECT		S.Collection_Unit_Key, -- required for async controls
			S.Item_Name,
			S.Store_Type_Concept_Key,
			CTS.PlainText AS TypeTerm,
			CASE WHEN SU.Collection_Unit_Key IS NULL THEN 0 ELSE 1 END AS IsSpecimen,
			CU.Current_Container_Collection_Unit_Key,
			SCurrent.Item_Name + ISNULL(' - ' + CSC.Current_Location_Code, ISNULL(' - ' + CSC.Usual_Location_Code, '')) AS Current_Container,
			CU.Current_Location_Code,
			CU.Usual_Container_Collection_Unit_Key,
			SUsual.Item_Name + ISNULL(' - ' + CSU.Current_Location_Code, ISNULL(' - ' + CSU.Usual_Location_Code, '')) AS Usual_Container,
			CU.Usual_Location_Code,
			S.Comment,
			CU.Custodian,
			S.[Timestamp] AS StoreTimestamp,
			CU.[Timestamp] AS UnitTimestamp

	FROM 		Store S
	INNER JOIN 	VW_ConceptTerm CTS ON CTS.Concept_Key = S.Store_Type_Concept_Key
	INNER JOIN 	Collection_Unit CU ON CU.Collection_Unit_Key = S.Collection_Unit_Key
	LEFT JOIN 	Specimen_Unit SU ON SU.Collection_Unit_Key = S.Collection_Unit_Key
	LEFT JOIN 	Store SCurrent ON SCurrent.Collection_Unit_Key = CU.Current_Container_Collection_Unit_Key
	LEFT JOIN	Collection_Unit CSC ON CSC.Collection_Unit_Key = SCurrent.Collection_Unit_Key 
	INNER JOIN 	Store SUsual ON SUsual.Collection_Unit_Key = CU.Usual_Container_Collection_Unit_Key
	INNER JOIN	Collection_Unit CSU ON CSU.Collection_Unit_Key = SUsual.Collection_Unit_Key 

	WHERE S.Collection_Unit_Key=@Key

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Store_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Store_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Store_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Store_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Store_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
	        GRANT EXECUTE ON dbo.usp_Store_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Store_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Store_Select TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_TaxonList_ImportConceptGroup]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_TaxonList_ImportConceptGroup]
GO

/*===========================================================================*\
  Description:	Import a concept group as a taxon list.

  Parameters:   @job_id					Job identifier
				@taxon_list_key			Taxon list key
				@concept_group_key		Concept group key

  Created:		Dec 2003

  Last revision information:
	$Revision: 1 $
	$Date: 24/03/05 10:52 $
	$Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_TaxonList_ImportConceptGroup]
	@job_id				INT,
	@taxon_list_key		CHAR(16),
	@concept_group_key	CHAR(16),
	@SessionID	CHAR(16)
AS
	SET NOCOUNT ON

	DECLARE		@existing_list_key		CHAR(16)

	SELECT		@existing_list_key						=	Taxon_List_Key
	FROM		Taxon_Dictionary_Concept_Group_Mapping
	WHERE		Concept_Group_Key						=	@concept_group_key

	IF @@ROWCOUNT = 0
	BEGIN
		/* record mapping */
		INSERT		Taxon_Dictionary_Concept_Group_Mapping (
					Taxon_List_Key,
					Concept_Group_Key)
		VALUES		(@taxon_list_key,
					@concept_group_key)

		IF @@ERROR <> 0 RETURN
	END
	ELSE IF @existing_list_key <> @taxon_list_key
	BEGIN
		RAISERROR (
			'Concept group has previously been imported into a different taxon list',
			16,
			1)
		RETURN
	END

	/* Calculate size of job */
	DECLARE		@record_count			INT

	SELECT		@record_count			=	COUNT(*)
	FROM		Concept_Group_Version
	WHERE		Concept_Group_Key		=	@concept_group_key

	SELECT		@record_count							=	@record_count * 3
															+ COUNT(DISTINCT c.Name_Type_Concept_Key)
															+ COUNT(DISTINCT c.Term_Key)
															+ COUNT(DISTINCT c.Term_Version_Key)
															+ COUNT(DISTINCT j.Source_Join_Key)
															+ COUNT(DISTINCT c.Concept_Rank_Key)
															+ COUNT(DISTINCT c.Concept_Key)
															+ COUNT(DISTINCT d.Designation_Type_Concept_Key)
															+ COUNT(DISTINCT d.Concept_Designation_Key)
															+ COUNT(DISTINCT f.Thesaurus_Fact_Key
																	+ vm.Term_Version_Key)
	FROM		Concept									AS	c
	LEFT JOIN	Source_Join								AS	j
	ON			j.Record_Key							=	c.Term_Key
	AND			j.Table_Name							=	'Term'
	LEFT JOIN	Concept_Designation						AS	d
	ON			d.Concept_Key							=	c.Concept_Key
	LEFT JOIN	Thesaurus_Fact							AS	f
	ON			f.Meaning_Key							=	c.Meaning_Key
	LEFT JOIN	Taxon_Dictionary_Term_Version_Mapping	AS	vm
	ON			vm.Term_Version_Key						=	c.Term_Version_Key
	WHERE		c.Concept_Group_Key						=	@concept_group_key
	AND j.Source_Join_Key IS NOT NULL

	EXECUTE		usp_Import_Export_Job_Configure		@job_id,
													@concept_group_key,
													@record_count
	IF @@ERROR <> 0 RETURN
	
	/* import versions */
	EXECUTE		usp_TaxonListVersion_ImportConceptGroup		@job_id
	IF @@ERROR <> 0 RETURN

	/* import name types */
	EXECUTE		usp_TaxonNameType_ImportConceptGroup	@job_id
	IF @@ERROR <> 0 RETURN

	/* import taxa */
	EXECUTE		usp_Taxon_ImportConceptGroup	@job_id
	IF @@ERROR <> 0 RETURN

	/* import taxon versions */
	EXECUTE		usp_TaxonVersion_ImportConceptGroup		@job_id, @SessionID
	IF @@ERROR <> 0 RETURN

	/* import taxon/source relationships */
	EXECUTE		usp_TaxonSources_ImportConceptGroup		@job_id
	IF @@ERROR <> 0 RETURN

	/* import taxon ranks */
	EXECUTE		usp_TaxonRank_ImportConceptGroup	@job_id
	IF @@ERROR <> 0 RETURN

	/* import taxon list items */
	EXECUTE		usp_TaxonListItem_ImportConceptGroup	@job_id
	IF @@ERROR <> 0 RETURN

	/* import hierarchical relationships */
	EXECUTE		usp_TaxonListItem_ImportRelationships	@job_id
	IF @@ERROR <> 0 RETURN

	/* import designation types */
	EXECUTE		usp_TaxonDesignationType_ImportConceptGroup		@job_id
	IF @@ERROR <> 0 RETURN

	/* import designations */
	EXECUTE		usp_TaxonDesignation_ImportConceptGroup		@job_id
	IF @@ERROR <> 0 RETURN

	/* import facts */
	EXECUTE		usp_TaxonFact_ImportConceptGroup	@job_id
	IF @@ERROR <> 0 RETURN 

	EXECUTE		usp_Import_Export_Job_UpdateStatus	@job_id,
													'Updating Taxon Names Index...'
	/* Discard Index_Taxon_Name records for the concept group */
	DELETE ITN
	FROM Index_Taxon_Name ITN
	INNER JOIN Taxon_Dictionary_Concept_Mapping TDM ON TDM.Taxon_List_Item_Key=ITN.Taxon_List_Item_Key
	INNER JOIN Concept C1 ON C1.Concept_Key=TDM.Concept_Key
			AND C1.Concept_Group_Key=@Concept_Group_Key
	
	/* Rebuild Index_Taxon_Name for the concept group */
	INSERT INTO Index_Taxon_Name (Taxon_List_Item_Key, Taxon_List_Version_Key,
	 Actual_Name, Actual_Name_Italic, Common_Name, Common_Name_Italic, 
	  Preferred_Name, Preferred_Name_Italic, Abbreviation, Authority, System_Supplied_Data )
	SELECT TLI.Taxon_List_Item_Key, TLI.Taxon_List_Version_Key, 
	  T.Item_Name, CASE WHEN TR3.List_Font_Italic = 1 AND T.Language = 'La' THEN 1 ELSE 0 END, 
	  T2.Item_Name, CASE WHEN TR3.List_Font_Italic = 1 AND T2.Language = 'La' THEN 1 ELSE 0 END, 
	  T3.Item_Name, CASE WHEN TR3.List_Font_Italic = 1 AND T3.Language = 'La' THEN 1 ELSE 0 END, 
	  T.Abbreviation, T.Authority, 1 
	FROM ((((((((Taxon_List_Item AS TLI 
	INNER JOIN Taxon_Dictionary_Concept_Mapping TDM ON TDM.Taxon_List_Item_Key=TLI.Taxon_List_Item_Key
	INNER JOIN Concept C1 ON C1.Concept_Key=TDM.Concept_Key
			AND C1.Concept_Group_Key=@Concept_Group_Key
	LEFT JOIN Taxon_version AS TV ON TV.Taxon_Version_Key = TLI.Taxon_Version_Key) 
	LEFT JOIN Taxon AS T ON T.Taxon_Key = TV.Taxon_Key) 
	LEFT JOIN Taxon_Common_Name AS TCN ON TCN.Taxon_List_Item_Key = TLI.Taxon_List_Item_Key) 
	LEFT JOIN Taxon_Version AS TV2 ON TV2.Taxon_Version_Key = TCN.Taxon_Version_Key) 
	LEFT JOIN Taxon AS T2 ON T2.Taxon_Key = TV2.Taxon_Key) 
	LEFT JOIN Taxon_List_Item AS TLI3 ON TLI3.Taxon_List_Item_Key = TLI.Preferred_Name) 
	LEFT JOIN Taxon_Rank AS TR3 ON TR3.Taxon_Rank_Key = TLI3.Taxon_Rank_Key) 
	LEFT JOIN Taxon_Version AS TV3 ON TV3.Taxon_Version_Key = TLI3.Taxon_Version_Key) 
	LEFT JOIN Taxon AS T3 ON T3.Taxon_Key = TV3.Taxon_Key 
	WHERE TLI.Taxon_List_Version_To IS NULL

	UPDATE Import_Export_Job
	SET Records_Processed = Records_Processed + @@ROWCOUNT
	WHERE Import_Export_Job_ID = @job_id
	
	EXECUTE		usp_Import_Export_Job_UpdateStatus	@job_id,
													'Exporting Taxon Common Names...'	

	/* Create a local table containing the taxon common name data */
	DECLARE @TaxonCommonName TABLE (
		Taxon_List_Item_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS PRIMARY KEY,
		Taxon_Version_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS
	)

	/* Get the Taxon_List_Item_Keys first, as there may be several Taxon_Version_Keys for some, 
	  and that would break the primary key constraint. */
	INSERT INTO @TaxonCommonName
	SELECT DISTINCT TDM1.Taxon_List_Item_Key, NULL
	FROM 	Taxon_Dictionary_Concept_Mapping TDM1 
	JOIN 	Concept C1 
			ON C1.Concept_Key = TDM1.Concept_Key
			AND C1.Concept_Group_Key = @Concept_Group_Key

	/* Now get a Taxon_Version_Key for each Taxon_List_Item_Key found, it'll use just one, thus 
	  being ok with the primary key constraint.  */
	UPDATE 	TCNTemp
	SET 	Taxon_Version_Key = TLI.Taxon_Version_Key
	FROM 	@TaxonCommonName TCNTemp
	INNER JOIN Taxon_Dictionary_Concept_Mapping TDM1 ON TDM1.Taxon_List_Item_Key = TCNTemp.Taxon_List_Item_Key
	INNER JOIN Concept C1 ON C1.Concept_Key=TDM1.Concept_Key
			AND C1.Concept_Group_Key=@Concept_Group_Key
	LEFT JOIN (
			Concept C2 
			INNER JOIN Term T ON T.Term_Key=C2.Term_Key
			INNER JOIN Language L ON L.Language_Key=T.Language_Key AND L.Priority=1
		) ON C2.Meaning_Key=C1.Meaning_Key
			AND C2.Preferred=1
			AND C2.Name_Type_Concept_Key='SYSTEM000000000L'
	LEFT JOIN Concept C3 ON C3.Meaning_Key=C1.Meaning_Key
		AND C3.List_Preferred=1
		AND C3.Concept_Group_Key=C1.Concept_Group_Key
	INNER JOIN Taxon_Dictionary_Concept_Mapping TDM2 ON TDM2.Concept_Key=ISNULL(C2.Concept_Key, C3.Concept_Key)
	INNER JOIN Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key=TDM2.Taxon_List_Item_Key

	UPDATE Import_Export_Job
	SET Records_Processed = Records_Processed + @@ROWCOUNT
	WHERE Import_Export_Job_ID = @job_id

	/* Update existing taxon common name records that are out of date */
	UPDATE TCN
	SET Taxon_Version_Key=TCNTmp.Taxon_Version_Key
	FROM @TaxonCommonName TCNTmp
	INNER JOIN Taxon_Common_Name TCN ON TCN.Taxon_List_Item_Key=TCNTmp.Taxon_List_Item_Key
	WHERE TCN.Taxon_Version_Key=TCNTmp.Taxon_Version_Key
		
	/* Insert any new required taxon common name records */
	INSERT INTO Taxon_Common_Name
	SELECT DISTINCT TCNTmp.Taxon_List_Item_Key, TCNTmp.Taxon_Version_Key
	FROM @TaxonCommonName TCNTmp
	LEFT JOIN Taxon_Common_Name TCN ON TCN.Taxon_List_Item_Key=TCNTmp.Taxon_List_Item_Key
	WHERE TCN.Taxon_List_Item_Key IS NULL

	UPDATE Import_Export_Job
	SET Records_Processed = Records_Processed + @@ROWCOUNT
	WHERE Import_Export_Job_ID = @job_id
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TaxonList_ImportConceptGroup') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_TaxonList_ImportConceptGroup'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_TaxonList_ImportConceptGroup TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_TaxonList_ImportConceptGroup TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_TaxonList_ImportConceptGroup TO [Dev - JNCC SQL]
END
GO

