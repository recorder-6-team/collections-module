/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConservationJobMaterial_Insert]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConservationJobMaterial_Insert]
GO

/*===========================================================================*\
  Description:	Insert a record in the Conservation_Job_Material table.

  Parameters:	@Key			OUTPUT Conservation_Job_Material_key
		@ConservationJobKey
		@MaterialKey
		@Quantity
		@UnitKey
		@SessionID

  Created:	December 2003

  Last revision information:
    $Revision: 1 $
    $Date: 8/01/04 11:32 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConservationJobMaterial_Insert]
	@Key char(16) OUTPUT,
	@ConservationJobKey char(16),
	@MaterialKey char(16),
	@Quantity varchar(20),
	@UnitKey char(16),
	@SessionID char(16)
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	EXECUTE spNextKey 'Conservation_Job_Material', @Key OUTPUT

	BEGIN TRANSACTION

		INSERT INTO Conservation_Job_Material (
			Conservation_Job_Material_Key, Conservation_Job_Key, Material_Concept_Key,
			Quantity, Unit_Concept_Key, Entered_Session_ID
		) VALUES (
			@Key, @ConservationJobKey, @MaterialKey, @Quantity, @UnitKey, @SessionID
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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConservationJobMaterial_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConservationJobMaterial_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConservationJobMaterial_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConservationJobMaterial_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConservationJobMaterial_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConservationJobMaterial_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConservationJobMaterial_Insert TO [Dev - JNCC SQL]
END
GO
