/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_MovementCollectionUnit_Insert]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_MovementCollectionUnit_Insert]
GO
/*===========================================================================*\
  Description:	
  Parameters:	

  Created:	July 2003

  Last revision information:
    $Revision: 3 $
    $Date: 5/02/04 10:35 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_MovementCollectionUnit_Insert]
	@Key char(16) output,
	@MovementDirectionKey char(16),
	@CollectionUnitKey char(16),
	@SessionID char(16)
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	/*-------------------------------------------------------------*\
	  Get a new key.
	\*-------------------------------------------------------------*/
	EXECUTE spNextKey 'Movement_Collection_Unit', @Key OUTPUT

	BEGIN TRANSACTION
	
	Insert into Movement_Collection_Unit
		(Movement_Collection_Unit_Key,
		Movement_Direction_Key,
		Collection_Unit_Key,
		Entered_Session_ID)
	Values
		(@Key,
		@MovementDirectionKey,
		@CollectionUnitKey,
		@SessionID)
		IF @@Error <> 0 GOTO RollBackAndExit

	COMMIT TRANSACTION 
	RETURN 0

RollBackAndExit:
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_MovementCollectionUnit_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_MovementCollectionUnit_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_MovementCollectionUnit_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_MovementCollectionUnit_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_MovementCollectionUnit_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_MovementCollectionUnit_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_MovementCollectionUnit_Insert TO [Dev - JNCC SQL]
END

GO