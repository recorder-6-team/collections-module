/*===========================================================================*\
  Drop trigger before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[tr_Conservation_Task_SearchCaption]') 
	   AND    Type = 'TR')
    DROP TRIGGER [dbo].[tr_Conservation_Task_SearchCaption]
GO

/*===========================================================================*\
  Description:	Update search caption on the Conservation_Task table.

  Type:		AFTER INSERT, UPDATE

  Created:	September 2003

  Last revision information:
    $Revision: 2 $
    $Date: 30/01/04 17:08 $
    $Author: Bencollier $

\*===========================================================================*/
CREATE TRIGGER [dbo].[tr_Conservation_Task_SearchCaption]
ON [dbo].[Conservation_Task]
AFTER INSERT, UPDATE
AS 
	IF UPDATE(Set_Vague_Date_Start) 
			OR UPDATE(Set_Vague_Date_End)
			OR UPDATE(Set_Vague_Date_Type)
			OR UPDATE(Status)
		UPDATE Conservation_Task 
		SET Search_Caption=
				dbo.ufn_GetDateFromVagueDate(I.Set_Vague_Date_Start, I.Set_Vague_Date_End, I.Set_Vague_Date_Type)
				+ ' - ' + dbo.ufn_GetConservationStatus(I.Status),
			Display_Caption=
				dbo.ufn_GetDateFromVagueDate(I.Set_Vague_Date_Start, I.Set_Vague_Date_End, I.Set_Vague_Date_Type)
				+ ' - ' + dbo.ufn_GetConservationStatus(I.Status)
		FROM Conservation_Task CT
		INNER JOIN Inserted I on I.Conservation_Task_Key=CT.Conservation_Task_Key

GO