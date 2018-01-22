/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Store_Location_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Store_Location_Update]
GO

/*===========================================================================*\
  Description:	Updates a record in the Store table.
				Ensures the Domain masks of the store and its containers are
				also updated.

  Parameters:	@Key - Collection_Unit key of the store
				@CurrentContainerKey - Key of the new Current container
				@SessionID 
				@UpdateUsualContainer - Bit value used to decide whether to 
										update the Usual container key as well

  Created:	September 2004

  Last revision information:
    $Revision: 1 $
    $Date: 23/09/04 15:11 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Store_Location_Update]
	@Key char(16), 
	@CurrentContainerKey char(16),
	@SessionID char(16),
	@UpdateUsualContainer bit = 0
AS
	BEGIN TRANSACTION

		UPDATE	Collection_Unit
		SET		Current_Container_Collection_Unit_Key = @CurrentContainerKey,
				Changed_Session_ID = @SessionID
		WHERE	Collection_Unit_Key = @Key

		IF @UpdateUsualContainer = 1 
			UPDATE	Collection_Unit
			SET		Usual_Container_Collection_Unit_Key = @CurrentContainerKey
			WHERE	Collection_Unit_Key = @Key

		IF @@Error <> 0 GOTO RollbackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit:
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Store_Location_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Store_Location_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        GRANT EXECUTE ON dbo.usp_Store_Location_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Store_Location_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Store_Location_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Store_Location_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_Store_Location_Update TO [Dev - JNCC SQL]
END
GO