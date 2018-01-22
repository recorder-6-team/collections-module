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
    $Revision: 4 $
    $Date: 3/02/09 9:44 $
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