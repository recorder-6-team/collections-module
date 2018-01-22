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
    $Date: 31/07/08 9:29 $
    $Author: Simonwood $

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
		GOTO CheckSpecimenPresence
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
		GOTO CheckSpecimenPresence
	END
	IF @MaterialMovementType = 7
	BEGIN
		SET @Status='Lost'
		GOTO CheckSpecimenPresence
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

CheckSpecimenPresence:
	IF EXISTS (SELECT * FROM Specimen_Unit WHERE Collection_Unit_Key = @Key)
	BEGIN
		IF NOT (@Status IN ('Lost', 'Destroyed') OR LEFT(@Status, 4) = 'Sold' OR LEFT(@Status, 8) = 'Disposed')
		BEGIN
			DECLARE @MeaningKey CHAR(16)

			SELECT @MeaningKey = C.Meaning_Key
			FROM Collection_Unit_Check CUC
			INNER JOIN Conservation_Check CC ON CUC.Conservation_Check_Key = CC.Conservation_Check_Key
			INNER JOIN Concept C ON C.Concept_Key = CC.Condition_Concept_Key
			WHERE CUC.Collection_Unit_Key = @Key 

			IF @MeaningKey IS NULL
			BEGIN
				IF @Status = 'Unknown'
					SET @Status = 'Presence Unknown'
				ELSE
					SET @Status = 'Presence Unknown, ' + @Status
			END
			ELSE IF @MeaningKey = 'SYSTEM00000021ZW'
			BEGIN
				IF @Status = 'Unknown'
					SET @Status = 'Not Found'
				ELSE
					SET @Status = 'Not Found, ' + @Status
			END
		END
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

