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
    $Revision: 9 $
    $Date: 3/02/09 10:58 $
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