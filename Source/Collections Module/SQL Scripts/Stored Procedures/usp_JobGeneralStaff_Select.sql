/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_JobGeneralStaff_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_JobGeneralStaff_Select]
GO

/*===========================================================================*\
  Description:	Returns a staff for JobGeneral frame.

  Parameters:	@Key

  Created:	Setember 2003

  Last revision information:
    $Revision: 2 $
    $Date: 12/11/03 14:48 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_JobGeneralStaff_Select]
	@Key char(16)
AS

SET NOCOUNT ON

	SELECT 
		CJS.Conservation_Job_Staff_Key,
		I.Name_Key, 
		dbo.ufn_GetFormattedName(I.Name_Key) AS ConservatorName
	FROM Conservation_Job_Staff CJS
	
	INNER JOIN Individual I ON I.Name_Key=CJS.Name_Key
	
	WHERE CJS.Conservation_Job_Key=@Key

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_JobGeneralStaff_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_JobGeneralStaff_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_JobGeneralStaff_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_JobGeneralStaff_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_JobGeneralStaff_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_JobGeneralStaff_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_JobGeneralStaff_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_JobGeneralStaff_Select TO [Dev - JNCC SQL]
END

GO
