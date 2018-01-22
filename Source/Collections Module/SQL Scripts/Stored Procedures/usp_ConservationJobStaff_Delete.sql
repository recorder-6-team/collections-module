/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConservationJobStaff_Delete]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConservationJobStaff_Delete]
GO

/*===========================================================================*\
  Description:	Delete a record from the Conservation_Job_Staff table.

  Parameters:	@Key

  Created:	January 2004

  Last revision information:
    $Revision: 1 $
    $Date: 3/02/04 9:51 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConservationJobStaff_Delete]
	@Key char(16)
AS
	SET NOCOUNT OFF

	BEGIN TRANSACTION

		-- Delete record from Conservation_Job_Staff table.
		DELETE	Conservation_Job_Staff
		WHERE	Conservation_Job_Staff_Key = @Key
	
		IF @@Error <> 0 GOTO RollbackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConservationJobStaff_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConservationJobStaff_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConservationJobStaff_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConservationJobStaff_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConservationJobStaff_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConservationJobStaff_Delete TO [Dev - JNCC SQL]
END
GO