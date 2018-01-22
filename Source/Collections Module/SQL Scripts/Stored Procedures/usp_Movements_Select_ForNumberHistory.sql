/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Movements_Select_ForNumberHistory]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Movements_Select_ForNumberHistory]
GO

/*===========================================================================*\
  Description:	Returns fields required for the NumberHistoryReadOnly frame.

  Parameters:	@Key	Accession Key

  Created:	Setember 2003

  Last revision information:
    $Revision: 1 $
    $Date: 12/11/03 16:41 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Movements_Select_ForNumberHistory]
	@Key char(16)
AS

SET NOCOUNT ON

	SELECT		dbo.ufn_GetFormattedName(M.Other_Party_Name_Key) AS OtherPartyName,
			dbo.ufn_GetFormattedName(M.Staff_Responsible_Name_Key) AS StaffResponsible,
			M.Number,
			MOO.Vague_Date_Start,
			MOO.Vague_Date_End,
			MOO.Vague_Date_Type,
			OD.Item_Name
	FROM		Movement AS M

	INNER JOIN	Movement_Direction AS MD ON M.Movement_Key = MD.Movement_Key
	LEFT JOIN	Movement_Of_Ownership AS MOO ON MD.Movement_Direction_Key = MOO.Movement_Direction_Key
	LEFT JOIN	Movement_Of_Material AS MOM ON MD.Movement_Direction_key = MOM.Movement_Direction_Key
	LEFT JOIN	Organisation_Department AS OD ON MOM.Receiver_Organisation_Department_Key = OD.Organisation_Department_Key

	WHERE		M.Movement_Key = @Key AND MD.Outbound = 0
		
SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Movements_Select_ForNumberHistory') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Movements_Select_ForNumberHistory'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Movements_Select_ForNumberHistory TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Movements_Select_ForNumberHistory TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Movements_Select_ForNumberHistory TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Movements_Select_ForNumberHistory TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Movements_Select_ForNumberHistory TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Movements_Select_ForNumberHistory TO [Dev - JNCC SQL]
END

GO