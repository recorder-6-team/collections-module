/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Multimedia_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Multimedia_Select]
GO

/*===========================================================================*\
  Description:	Returns data for the Multimedia frame.

  Parameters:	@Key	Collection key

  Created:	September 2003

  Last revision information:
    $Revision: 2 $
    $Date: 12/11/03 16:39 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Multimedia_Select]
	@Key char(16)
AS

SET NOCOUNT ON

	SELECT 		SF.Title, SF.[File_Name], SF.Preferred, SF.[Timestamp], S.Custodian
	FROM 		Source_File AS SF
	INNER JOIN 	Source AS S ON S.Source_Key = SF.Source_Key
	WHERE 		SF.Source_Key = @Key

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Multimedia_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Multimedia_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Multimedia_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Multimedia_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Multimedia_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Multimedia_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Multimedia_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Multimedia_Select TO [Dev - JNCC SQL]
END

GO