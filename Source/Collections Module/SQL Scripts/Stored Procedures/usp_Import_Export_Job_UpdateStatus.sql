/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_Import_Export_Job_UpdateStatus]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_Import_Export_Job_UpdateStatus]
GO

/*===========================================================================*\
  Description:	Update current status of an import/export process.

  Parameters:	@import_export_id		Job identifier
				@status_message			New status message

  Created:		Dec 2003

  Last revision information:
	$Revision: 2 $
	$Date: 12/05/04 9:57 $
	$Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Import_Export_Job_UpdateStatus]
	@import_export_id		INT,
	@status_message			VARCHAR(200)
AS
	SET NOCOUNT ON

	UPDATE		Import_Export_Job
	SET			Status_Message			=	@status_message
	WHERE		Import_Export_Job_ID	=	@import_export_id
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Import_Export_Job_UpdateStatus') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Import_Export_Job_UpdateStatus'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Import_Export_Job_UpdateStatus TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Import_Export_Job_UpdateStatus TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Import_Export_Job_UpdateStatus TO [Dev - JNCC SQL]
END
GO