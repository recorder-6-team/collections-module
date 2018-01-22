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
    $Revision: 12 $
    $Date: 21/02/05 11:19 $
    $Author: Ericsalmon $

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