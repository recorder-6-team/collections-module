/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_CollectionUnitMaterial_Delete]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_CollectionUnitMaterial_Delete]
GO

/*===========================================================================*\
  Description:	Delete a record from the Collection_Unit_Material table.

  Parameters:	@Key		Collection Unit Material key.
		@Timestamp

  Created:	September 2003

  Last revision information:
    $Revision: 7 $
    $Date: 2/02/09 17:21 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_CollectionUnitMaterial_Delete]
	@Key char(16),
	@Timestamp timestamp
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION

		DELETE	Collection_Unit_Material
		WHERE	Collection_Unit_Material_Key = @Key
		AND		[Timestamp] = @Timestamp

		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Collection_Unit_Material WHERE Collection_Unit_Material_Key = @Key)
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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_CollectionUnitMaterial_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_CollectionUnitMaterial_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitMaterial_Delete TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_CollectionUnitMaterial_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_CollectionUnitMaterial_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_CollectionUnitMaterial_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitMaterial_Delete TO [Dev - JNCC SQL]
END
GO