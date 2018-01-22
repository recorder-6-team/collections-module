/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConservationJobStaff_Insert]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConservationJobStaff_Insert]
GO

/*===========================================================================*\
  Description:	Inserts a record into the Conservation_Job_Staff table

  Parameters:	@Key 
		@ConservationJobKey 
		@NameKey 
		@SessionID 

  Created:	February 2004

  Last revision information:
    $Revision: 1 $
    $Date: 3/02/04 9:51 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConservationJobStaff_Insert]
	@Key char(16) OUTPUT,
	@ConservationJobKey char(16),
	@NameKey char(16),
	@SessionID char(16)
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	EXECUTE spNextKey 'Conservation_Job_Staff', @Key OUTPUT

	BEGIN TRANSACTION

		/*-------------------------------------------------------------*\
		  Insert in Conservation_Job_Staff.
		\*-------------------------------------------------------------*/
		INSERT INTO Conservation_Job_Staff (
			Conservation_Job_Staff_Key,
			Conservation_Job_Key,
			Name_Key,
			Entered_Session_ID			
		) VALUES (
			@Key,
			@ConservationJobKey,
			@NameKey,
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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConservationJobStaff_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConservationJobStaff_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConservationJobStaff_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConservationJobStaff_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConservationJobStaff_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConservationJobStaff_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConservationJobStaff_Insert TO [Dev - JNCC SQL]
END
GO