/*===========================================================================*\
  Drop trigger before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[tr_ConservationCheck_SearchCaption]') 
	   AND    Type = 'TR')
    DROP TRIGGER [dbo].[tr_ConservationCheck_SearchCaption]
GO

/*===========================================================================*\
  Description:	Update search caption on the Conservation Check table.

  Type:		AFTER INSERT, UPDATE

  Created:	September 2003

  Last revision information:
    $Revision: 2 $
    $Date: 30/01/04 17:08 $
    $Author: Bencollier $

\*===========================================================================*/
CREATE TRIGGER [dbo].[tr_ConservationCheck_SearchCaption]
ON [dbo].[Conservation_Check]
AFTER INSERT, UPDATE
AS 
	IF UPDATE(Vague_Date_Start) 
			OR UPDATE(Vague_Date_End)
			OR UPDATE(Vague_Date_Type)
			OR UPDATE(Type_Concept_Key)
			OR UPDATE(Ref_Number)
		UPDATE Conservation_Check 
		SET Search_Caption=
				dbo.ufn_GetDateFromVagueDate(I.Vague_Date_Start, I.Vague_Date_End, I.Vague_Date_Type)
				+ ' - ' +
				CT.Plaintext
				+ ' - ' +
				I.Ref_Number,
			Display_Caption=
				dbo.ufn_GetDateFromVagueDate(I.Vague_Date_Start, I.Vague_Date_End, I.Vague_Date_Type)
				+ ' - ' +
				CT.Item_Name
				+ ' - ' +
				I.Ref_Number
		FROM Conservation_Check CC
		INNER JOIN Inserted I on I.Conservation_Check_Key=CC.Conservation_Check_Key
		INNER JOIN VW_ConceptTerm CT ON CT.Concept_Key=I.Type_Concept_Key	

GO