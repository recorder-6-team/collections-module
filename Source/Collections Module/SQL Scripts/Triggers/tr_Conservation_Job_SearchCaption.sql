/*===========================================================================*\
  Drop trigger before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[tr_Conservation_Job_SearchCaption]') 
	   AND    Type = 'TR')
    DROP TRIGGER [dbo].[tr_Conservation_Job_SearchCaption]
GO

/*===========================================================================*\
  Description:	Update search caption on the tr_Conservation_Job table.

  Type:		AFTER INSERT, UPDATE

  Created:	September 2003

  Last revision information:
    $Revision: 2 $
    $Date: 30/01/04 17:08 $
    $Author: Bencollier $

\*===========================================================================*/
CREATE TRIGGER [dbo].[tr_Conservation_Job_SearchCaption]
ON [dbo].[Conservation_Job]
AFTER INSERT, UPDATE
AS 
	IF UPDATE(From_Vague_Date_Start) 
			OR UPDATE(From_Vague_Date_End)
			OR UPDATE(From_Vague_Date_Type)
			OR UPDATE(Item_Name)
		UPDATE Conservation_Job 
		SET Search_Caption=
				dbo.ufn_GetDateFromVagueDate(I.From_Vague_Date_Start, I.From_Vague_Date_End, I.From_Vague_Date_Type)
				+ ' - ' + I.Item_Name,
			Display_Caption=
				dbo.ufn_GetDateFromVagueDate(I.From_Vague_Date_Start, I.From_Vague_Date_End, I.From_Vague_Date_Type)
				+ ' - ' + I.Item_Name
		FROM Conservation_Job CJ
		INNER JOIN Inserted I on I.Conservation_Job_Key=CJ.Conservation_Job_Key

GO