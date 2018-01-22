/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_MovementOfMaterialExclusion_Delete]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_MovementOfMaterialExclusion_Delete]
GO

/*===========================================================================*\
  Description:	Delete a record from the Movement_Of_Material_Exclusion table.

  Parameters:	@MovementOfMaterialKey
		@CollectionUnitKey

  Created:	January 2004

  Last revision information:
    $Revision: 2 $
    $Date: 25/02/04 14:06 $
    $Author: Bencollier $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_MovementOfMaterialExclusion_Delete]
	@MovementOfMaterialKey char(16),
	@CollectionUnitKey char(16),
	@SessionID char(16)
AS
	SET NOCOUNT OFF

	BEGIN TRANSACTION

		--Exclude this collection_unit from all existing movements of material
		DECLARE @Movement_Of_Material_Key CHAR(16)
		DECLARE @Movement_Of_Material_Exclusion_Key CHAR(16)

		DECLARE MOMEcsr CURSOR LOCAL FAST_FORWARD
		FOR 
		SELECT MOM2.Movement_Of_Material_Key
		FROM Movement_Of_Material MOM1
		INNER JOIN Movement_Direction MD ON MOM1.Movement_Direction_Key = MD.Movement_Direction_Key
			AND MOM1.Movement_Of_Material_Key = @MovementOfMaterialKey
		INNER JOIN Movement_Of_Material MOM2 ON MD.Movement_Direction_Key = MOM2.Movement_Direction_Key
			AND MOM2.Movement_Of_Material_Key <> @MovementOfMaterialKey
	
		OPEN MOMEcsr
		
		FETCH NEXT FROM MOMEcsr INTO @Movement_Of_Material_Key
		
		WHILE @@FETCH_STATUS = 0
		BEGIN
		
			EXECUTE spNextKey 'Movement_Of_Material_Exclusion', @Movement_Of_Material_Exclusion_Key OUTPUT				
			INSERT INTO Movement_Of_Material_Exclusion
				(Movement_Of_Material_Exclusion_Key,
				Movement_Of_Material_Key,
				Collection_Unit_Key,
				Entered_Session_ID)
			VALUES
				(@Movement_Of_Material_Exclusion_Key,
				@Movement_Of_Material_Key,
				@CollectionUnitKey,
				@SessionID)

			IF @@Error <> 0 GOTO RollbackAndExit
		
		
		FETCH NEXT FROM MOMEcsr INTO @Movement_Of_Material_Key
		END --End While
		
		CLOSE MOMEcsr
		DEALLOCATE MOMEcsr

		IF @@Error <> 0 GOTO RollbackAndExit


		-- Delete record from Movement_Of_Material table for current collection unit.
		DELETE	Movement_Of_Material_Exclusion
		WHERE	Movement_Of_Material_Key = @MovementOfMaterialKey
		AND	Collection_Unit_Key = @CollectionUnitKey
		
		IF @@Error <> 0 GOTO RollbackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_MovementOfMaterialExclusion_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_MovementOfMaterialExclusion_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_MovementOfMaterialExclusion_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_MovementOfMaterialExclusion_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_MovementOfMaterialExclusion_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_MovementOfMaterialExclusion_Delete TO [Dev - JNCC SQL]
END
GO