/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConservationJobStaff_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConservationJobStaff_Select]
GO

/*===========================================================================*\
  Description:	Returns the staff associated with a job.

  Parameters:	@Key	Conservation_Job_Key

  Created:	February 2004

  Last revision information:
    $Revision: 1 $
    $Date: 3/02/04 9:51 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConservationJobStaff_Select]
	@Key char(16)
AS

SET NOCOUNT ON

	SELECT 		CJS.Conservation_Job_Staff_Key AS [Key],
			CJS.Name_Key,
			dbo.ufn_GetFormattedName(CJS.Name_Key) AS [Name],
			CJ.Custodian,
			CJ.[Timestamp]
	FROM		Conservation_Job_Staff AS CJS
	INNER JOIN	Conservation_Job AS CJ ON CJ.Conservation_Job_Key = CJS.Conservation_Job_Key
	WHERE 		CJS.Conservation_Job_Key = @Key

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConservationJobStaff_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConservationJobStaff_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConservationJobStaff_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConservationJobStaff_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConservationJobStaff_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ConservationJobStaff_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConservationJobStaff_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConservationJobStaff_Select TO [Dev - JNCC SQL]
END

GO