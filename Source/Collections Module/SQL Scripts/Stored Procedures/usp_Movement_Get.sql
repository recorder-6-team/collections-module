/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Movement_Get]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Movement_Get]
GO

/*===========================================================================*\
  Description:	Returns a store's caption

  Parameters:	@Key 
				@Caption OUTPUT

  Created:	October 2003

  Last revision information:
    $Revision: 1 $
    $Date: 29/11/07 17:02 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Movement_Get] 
	@Key 		CHAR(16),
	@Caption 	VARCHAR(150) OUTPUT

AS
	SET NOCOUNT ON

	SELECT 	@Caption = Display_Caption
	FROM 	Movement
	WHERE 	Movement_Key = @Key


GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Movement_Get') AND SysStat & 0xf = 4)
BEGIN
   	PRINT 'Setting up security on procedure usp_Movement_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
       	GRANT EXECUTE ON dbo.usp_Movement_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Movement_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Movement_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Movement_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Movement_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
       	GRANT EXECUTE ON dbo.usp_Movement_Get TO [Dev - JNCC SQL]
END

GO