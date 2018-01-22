/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_MovementOfOwnership_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_MovementOfOwnership_Select]
GO
/*===========================================================================*\
  Description: 	Returns the details of the MovementOfOwnership table.
  Parameters:	@Key	The Movement_Of_Ownership key.	

  Created:	October 2003

  Last revision information:
    $Revision: 2 $
    $Date: 12/11/03 14:48 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_MovementOfOwnership_Select]
	@Key char(16)
AS
	SET NOCOUNT ON

	SELECT 	Movement_Direction_Key,
		Contact_Name_Key,
		dbo.ufn_GetFormattedName(Contact_Name_Key) AS Contact_Name,
		Vague_Date_Start,	
		Vague_Date_End,
		Vague_Date_Type,
		Notes,
		Completed,
		Custodian,
		Timestamp
	FROM	Movement_Of_Ownership
	WHERE	Movement_Of_Ownership_Key = @Key

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_MovementOfOwnership_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_MovementOfOwnership_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_MovementOfOwnership_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_MovementOfOwnership_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_MovementOfOwnership_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_MovementOfOwnership_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_MovementOfOwnership_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_MovementOfOwnership_Select TO [Dev - JNCC SQL]
END

GO
