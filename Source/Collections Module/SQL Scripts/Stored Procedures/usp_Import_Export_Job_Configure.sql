/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_Import_Export_Job_Configure]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_Import_Export_Job_Configure]
GO

/*===========================================================================*\
  Description:	Set parameters of an import/export process.

  Parameters:	@import_export_id		Job identifier
				@concept_group_key		Concept group key
                @record_count			Total record count

  Created:		Dec 2003

  Last revision information:
	$Revision: 2 $
	$Date: 12/05/04 9:57 $
	$Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Import_Export_Job_Configure]
	@import_export_id		INT,
	@concept_group_key		CHAR(16),
	@record_count			INT
AS
	SET NOCOUNT ON

	UPDATE		Import_Export_Job
	SET			Total_Records			=	@record_count,
				Concept_Group_Key		=	@concept_group_key
	WHERE		Import_Export_Job_ID	=	@import_export_id
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Import_Export_Job_Configure') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Import_Export_Job_Configure'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Import_Export_Job_Configure TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Import_Export_Job_Configure TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Import_Export_Job_Configure TO [Dev - JNCC SQL]
END
GO