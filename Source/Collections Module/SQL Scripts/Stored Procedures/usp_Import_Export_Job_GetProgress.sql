/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_Import_Export_Job_GetProgress]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_Import_Export_Job_GetProgress]
GO

/*===========================================================================*\
  Description:	Read current state of the specified import/export job.

  Parameters:	@job_id					Job identifier
				@status_message			[on exit] Describes current status
				@total_records			[on exit] Number of records in job
				@records_processed		[on exit] Number of records processed

  Created:		Dec 2003

  Last revision information:
	$Revision: 5 $
	$Date: 12/05/04 9:57 $
	$Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Import_Export_Job_GetProgress]
	@import_export_id		INT,
	@status_message			VARCHAR(200)	OUTPUT,
	@total_records			INT				OUTPUT,
	@records_processed		INT				OUTPUT
AS
	SET NOCOUNT ON

	SELECT		@status_message			=	Status_Message,
				@total_records			=	Total_Records,
				@records_processed		=	Records_Processed
	FROM		Import_Export_Job
	WHERE		Import_Export_Job_ID	=	@import_export_id

	IF @@ROWCOUNT = 0 RAISERROR ('Job does not exist or has been closed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Import_Export_Job_GetProgress') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Import_Export_Job_GetProgress'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Import_Export_Job_GetProgress TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Import_Export_Job_GetProgress TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Import_Export_Job_GetProgress TO [Dev - JNCC SQL]
END
GO