/*===========================================================================*\
  Drop trigger before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[tr_Enquiry_SearchCaption]') 
	   AND    Type = 'TR')
    DROP TRIGGER [dbo].[tr_Enquiry_SearchCaption]
GO

/*===========================================================================*\
  Description:	Update search caption on the Enquiry table.

  Type:		AFTER INSERT, UPDATE

  Created:	September 2003

  Last revision information:
    $Revision: 2 $
    $Date: 30/01/04 17:08 $
    $Author: Bencollier $

\*===========================================================================*/
CREATE TRIGGER [dbo].[tr_Enquiry_SearchCaption]
ON [dbo].[Enquiry]
AFTER INSERT, UPDATE
AS 
	IF UPDATE(Vague_Date_Start) 
			OR UPDATE(Vague_Date_End)
			OR UPDATE(Vague_Date_Type)
			OR UPDATE(Enquiry_Type_Concept_Key)
		UPDATE Enquiry 
		SET Search_Caption=
				dbo.ufn_GetDateFromVagueDate(I.Vague_Date_Start, I.Vague_Date_End, I.Vague_Date_Type)
				+ ' - ' +
				CT.Plaintext,
			Display_Caption=
				dbo.ufn_GetDateFromVagueDate(I.Vague_Date_Start, I.Vague_Date_End, I.Vague_Date_Type)
				+ ' - ' +
				CT.Item_Name
		FROM Enquiry E
		INNER JOIN Inserted I on I.Enquiry_Key=E.Enquiry_Key
		INNER JOIN VW_ConceptTerm CT ON CT.Concept_Key=I.Enquiry_Type_Concept_Key	

GO