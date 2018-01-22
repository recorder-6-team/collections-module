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
    $Revision: 2 $
    $Date: 21/02/05 11:19 $
    $Author: Ericsalmon $

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
