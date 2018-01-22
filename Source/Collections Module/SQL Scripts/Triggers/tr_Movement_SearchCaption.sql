/*===========================================================================*\
  Drop trigger before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[tr_Movement_SearchCaption]') 
	   AND    Type = 'TR')
    DROP TRIGGER [dbo].[tr_Movement_SearchCaption]
GO

/*===========================================================================*\
  Description:	Update search caption on the Movement table.

  Type:		AFTER INSERT, UPDATE

  Created:	September 2003

  Last revision information:
    $Revision: 3 $
    $Date: 18/03/14 9:28 $
    $Author: Brynhorsfieldschonhut $

\*===========================================================================*/
CREATE TRIGGER [dbo].[tr_Movement_SearchCaption]
ON [dbo].[Movement]
AFTER INSERT, UPDATE
AS 

	IF UPDATE(Exp_Vague_Date_Start) 
			OR UPDATE(Exp_Vague_Date_End)
			OR UPDATE(Exp_Vague_Date_Type)
			OR UPDATE(Number)
			OR UPDATE(Other_Party_Name_Key)
	BEGIN
			UPDATE Movement 
			SET Search_Caption=
					dbo.ufn_GetMovementTypeName(I.Movement_Type) + ' (' + I.Number +  ') - ' +
					dbo.ufn_GetDateFromVagueDate(I.Exp_Vague_Date_Start, I.Exp_Vague_Date_End, I.Exp_Vague_Date_Type),
				Display_Caption=
					dbo.ufn_GetMovementTypeName(I.Movement_Type) + ' (' + I.Number +  ') - ' +
					dbo.ufn_GetDateFromVagueDate(I.Exp_Vague_Date_Start, I.Exp_Vague_Date_End, I.Exp_Vague_Date_Type)
			FROM Movement M
			INNER JOIN Inserted I on I.Movement_Key=M.Movement_Key
			WHERE I.Movement_Type IN (0, 1)

			UPDATE Movement
			SET Search_Caption=
					dbo.ufn_GetMovementTypeName(I.Movement_Type) + ' - ' + dbo.ufn_GetFormattedName(I.Other_Party_Name_Key) +  ' - ' +
					dbo.ufn_GetDateFromVagueDate(I.Exp_Vague_Date_Start, I.Exp_Vague_Date_End, I.Exp_Vague_Date_Type),
				Display_Caption=
					dbo.ufn_GetMovementTypeName(I.Movement_Type) + ' - ' + dbo.ufn_GetFormattedName(I.Other_Party_Name_Key) +  ' - ' +
					dbo.ufn_GetDateFromVagueDate(I.Exp_Vague_Date_Start, I.Exp_Vague_Date_End, I.Exp_Vague_Date_Type)
			FROM Movement M
			INNER JOIN Inserted I on I.Movement_Key=M.Movement_Key
			WHERE I.Movement_Type IN (2, 3)			 

			UPDATE Movement 
			SET Search_Caption=
					dbo.ufn_GetMovementTypeName(I.Movement_Type) + ' - ' +
					dbo.ufn_GetDateFromVagueDate(I.Exp_Vague_Date_Start, I.Exp_Vague_Date_End, I.Exp_Vague_Date_Type),
				Display_Caption=
					dbo.ufn_GetMovementTypeName(I.Movement_Type) + ' - ' +
					dbo.ufn_GetDateFromVagueDate(I.Exp_Vague_Date_Start, I.Exp_Vague_Date_End, I.Exp_Vague_Date_Type)
			FROM Movement M
			INNER JOIN Inserted I on I.Movement_Key=M.Movement_Key
			WHERE I.Movement_Type NOT IN (0, 1, 2, 3) 
	END

GO