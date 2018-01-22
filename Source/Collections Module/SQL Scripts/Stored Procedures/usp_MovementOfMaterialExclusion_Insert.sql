/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_MovementOfMaterialExclusion_Insert]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_MovementOfMaterialExclusion_Insert]
GO

/*===========================================================================*\
  Description:	Inserts a record into the Movement_Of_Material_Exclusion table.

  Parameters:	@Key	(New Movement_Of_Material_Exclusion_Key)
		@MovementOfMaterialKey
		@CollectionUnitKey
		@SessionID

  Created:	January 2004

  Last revision information:
    $Revision: 1 $
    $Date: 29/01/04 17:00 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_MovementOfMaterialExclusion_Insert]
	@Key char(16) OUTPUT, 
	@MovementOfMaterialKey char(16),
	@CollectionUnitKey char(16),
	@SessionID char(16)
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	EXECUTE spNextKey 'Movement_Of_Material_Exclusion', @Key OUTPUT

	BEGIN TRANSACTION
		/*-------------------------------------------------------------*\
		  Insert in Movement_Of_Material_Exclusion.
		\*-------------------------------------------------------------*/
		INSERT INTO Movement_Of_Material_Exclusion (
			Movement_Of_Material_Exclusion_Key,
			Movement_Of_Material_Key,
			Collection_Unit_Key,
			Entered_Session_ID
		) VALUES (
			@Key, 
			@MovementOfMaterialKey,
			@CollectionUnitKey,
			@SessionID
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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_MovementOfMaterialExclusion_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_MovementOfMaterialExclusion_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_MovementOfMaterialExclusion_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_MovementOfMaterialExclusion_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_MovementOfMaterialExclusion_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_MovementOfMaterialExclusion_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_MovementOfMaterialExclusion_Insert TO [Dev - JNCC SQL]
END
GO