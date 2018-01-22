/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Movement_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Movement_Select]
GO

/*===========================================================================*\
  Description:	Returns data for the General tab of the Movement screen.

  Parameters:	@Key	Collection key

  Created:	September 2003

  Last revision information:
    $Revision: 5 $
    $Date: 18/03/14 9:27 $
    $Author: Brynhorsfieldschonhut $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Movement_Select]
	@Key char(16)
AS

SET NOCOUNT ON

	SELECT 		M.Movement_Type,
			M.Other_Party_Name_Key,
			IsNull(N.Organisation, 0) AS OtherPartyIsOrganisation,
			dbo.ufn_GetFormattedName(M.Other_Party_Name_Key) AS Other_Party_Name,
			M.Staff_Responsible_Name_Key, 
			dbo.ufn_GetFormattedName(M.Staff_Responsible_Name_Key) AS Staff_Responsible_Name,
			M.Contact_Name_Key,
			dbo.ufn_GetFormattedName(M.Contact_Name_Key) AS Contact_Name,
			M.Exp_Vague_Date_Start, 
			M.Exp_Vague_Date_End, 
			M.Exp_Vague_Date_Type, 
			M.Number,
			M.Notes,
			M.Display_Caption,
			M.Timestamp,
			LoanDate_Vague_Date_Start = CASE
				WHEN M.Movement_Type = 2 THEN MOMIN.Vague_Date_Start
				WHEN M.Movement_Type = 3 THEN MOMOUT.Vague_Date_Start
				ELSE NULL
			END,
			LoanDate_Vague_Date_End = CASE
				WHEN M.Movement_Type = 2 THEN MOMIN.Vague_Date_End
				WHEN M.Movement_Type = 3 THEN MOMOUT.Vague_Date_End
				ELSE NULL
			END,
			LoanDate_Vague_Date_Type = CASE
				WHEN M.Movement_Type = 2 THEN MOMIN.Vague_Date_Type
				WHEN M.Movement_Type = 3 THEN MOMOUT.Vague_Date_Type
				ELSE NULL
			END,
			LoanComplete = CASE
				WHEN M.Movement_Type = 2 THEN MOMIN.Completed
				WHEN M.Movement_Type = 3 THEN MOMOUT.Completed
				ELSE NULL
			END,
			ActualReturnDate_Vague_Date_Start = CASE
				WHEN M.Movement_Type = 2 THEN MOMOUT.Vague_Date_Start
				WHEN M.Movement_Type = 3 THEN MOMIN.Vague_Date_Start
				ELSE NULL
			END,
			ActualReturnDate_Vague_Date_End = CASE
				WHEN M.Movement_Type = 2 THEN MOMOUT.Vague_Date_End
				WHEN M.Movement_Type = 3 THEN MOMIN.Vague_Date_End
				ELSE NULL
			END,
			ActualReturnDate_Vague_Date_Type = CASE
				WHEN M.Movement_Type = 2 THEN MOMOUT.Vague_Date_Type
				WHEN M.Movement_Type = 3 THEN MOMIN.Vague_Date_Type
				ELSE NULL
			END,
			ReturnComplete = CASE
				WHEN M.Movement_Type = 2 THEN MOMOUT.Completed
				WHEN M.Movement_Type = 3 THEN MOMIN.Completed
				ELSE NULL
			END,			

			--Following fields are used to get async controls
			M.Movement_Key,
			0 AS CollectionIndex,
			1 AS SpecimenIndex,
			2 AS StoreIndex
	FROM 		Movement AS M
	LEFT JOIN	[Name] AS N ON N.Name_Key = M.Other_Party_Name_Key
	LEFT JOIN 	Movement_Direction 
		AS MDIN 
		ON M.Movement_Key = MDIN.Movement_Key AND MDIN.Outbound = 0
	LEFT JOIN	Movement_Of_Material 
		AS MOMIN 
		ON MDIN.Movement_Direction_Key = MOMIN.Movement_Direction_Key
	LEFT JOIN 	Movement_Direction 
		AS MDOUT 
		ON M.Movement_Key = MDOUT.Movement_Key AND MDOUT.Outbound = 1
	LEFT JOIN	Movement_Of_Material 
		AS MOMOUT 
		ON MDOUT.Movement_Direction_Key = MOMOUT.Movement_Direction_Key
	
	WHERE 	M.Movement_Key = @Key

SET NOCOUNT OFF

GO

 
/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Movement_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Movement_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Movement_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Movement_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Movement_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Movement_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Movement_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Movement_Select TO [Dev - JNCC SQL]
END

GO