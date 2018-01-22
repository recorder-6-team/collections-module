/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_Import_Export_Job_Prepare]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_Import_Export_Job_Prepare]
GO

/*===========================================================================*\
  Description:	Prepare to intiate a taxon dictionary import/export process.

  Parameters:	@import_export_id		On exit, a job identifier, to be
										passed to
											usp_ConceptGroup_ImportTaxonList
										or
											usp_TaxonList_ImportConceptGroup.

										Can be used to look up current
										progress of the job in the table

											Import_Export_Progress.

  Created:		Dec 2003

  Last revision information:
	$Revision: 2 $
	$Date: 12/05/04 9:57 $
	$Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Import_Export_Job_Prepare]
	@import_export_id		INT		OUTPUT
AS
	SET NOCOUNT ON

	INSERT		Import_Export_Job (
				Status_Message,
				Total_Records,
				Records_Processed)
	VALUES		('Initialising',
				0,
				0)

	IF @@ERROR <> 0 RETURN

    SET			@import_export_id	=	SCOPE_IDENTITY()
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Import_Export_Job_Prepare') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Import_Export_Job_Prepare'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Import_Export_Job_Prepare TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Import_Export_Job_Prepare TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Import_Export_Job_Prepare TO [Dev - JNCC SQL]
END
GO