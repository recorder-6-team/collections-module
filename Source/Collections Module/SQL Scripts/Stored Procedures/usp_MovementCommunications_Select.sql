/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_MovementCommunications_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_MovementCommunications_Select]
GO

/*===========================================================================*\
  Description:	Returns Movement_Communication records for the Movement
		Communications tab page.

  Parameters:	@Key	Collection key

  Created:	November 2003

  Last revision information:
    $Revision: 2 $
    $Date: 12/11/03 14:48 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_MovementCommunications_Select]
	@Key char(16)
AS

SET NOCOUNT ON
	SELECT 	
		MC.Movement_Communication_Key AS Item_Key,
		MC.Sender_Name_Key,
		dbo.ufn_GetFormattedName(MC.Sender_Name_Key) AS Sender_Name,
		MC.Receiver_Name_Key, 
		dbo.ufn_GetFormattedName(MC.Receiver_Name_Key) AS Receiver_Name,
		MC.Communication_Type_Concept_Key,
		TCommType.Item_Name AS Communication_Type_Item_Name,
		MC.Vague_Date_Start, 
		MC.Vague_Date_End, 
		MC.Vague_Date_Type, 
		MC.Content,
		MC.File_Ref,
		MC.Custodian,
		MC.Timestamp
	
	FROM 		Movement_Communication AS MC
	INNER JOIN 	VW_ConceptTerm AS TCommType ON MC.Communication_Type_Concept_Key = TCommType.Concept_Key
	
	WHERE MC.Movement_Key = @Key


SET NOCOUNT OFF

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_MovementCommunications_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_MovementCommunications_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_MovementCommunications_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_MovementCommunications_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_MovementCommunications_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_MovementCommunications_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_MovementCommunications_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_MovementCommunications_Select TO [Dev - JNCC SQL]
END

GO