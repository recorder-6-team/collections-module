/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_CollectionUnitMaterial_Insert]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_CollectionUnitMaterial_Insert]
GO

/*===========================================================================*\
  Description:	Insert a record in the Collection_Unit_Material table.

  Parameters:	@Key			OUTPUT Collection unit material key
		@CollectionUnitKey
		@MaterialKey
		@Quantity
		@UnitKey
		@SessionID

  Created:	September 2003

  Last revision information:
    $Revision: 4 $
    $Date: 5/12/03 17:16 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_CollectionUnitMaterial_Insert]
	@Key char(16) OUTPUT,
	@CollectionUnitKey char(16),
	@MaterialKey char(16),
	@Quantity varchar(20),
	@UnitKey char(16),
	@SessionID char(16)
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	EXECUTE spNextKey 'Collection_Unit_Material', @Key OUTPUT

	BEGIN TRANSACTION

		INSERT INTO Collection_Unit_Material (
			Collection_Unit_Material_Key, Collection_Unit_Key, Material_Concept_Key,
			Quantity, Unit_Concept_Key, Entered_Session_ID
		) VALUES (
			@Key, @CollectionUnitKey, @MaterialKey, @Quantity, @UnitKey, @SessionID
		)

		IF @@Error <> 0 GOTO RollbackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_CollectionUnitMaterial_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_CollectionUnitMaterial_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitMaterial_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_CollectionUnitMaterial_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_CollectionUnitMaterial_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_CollectionUnitMaterial_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitMaterial_Insert TO [Dev - JNCC SQL]
END
GO
