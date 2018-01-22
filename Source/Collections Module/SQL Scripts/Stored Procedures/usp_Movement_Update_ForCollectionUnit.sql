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
    $Revision: 10 $
    $Date: 7/03/05 15:01 $
    $Author: Ericsalmon $

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