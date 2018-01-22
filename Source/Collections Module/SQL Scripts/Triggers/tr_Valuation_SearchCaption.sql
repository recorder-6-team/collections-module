/*===========================================================================*\
  Drop trigger before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[tr_Valuation_SearchCaption]') 
	   AND    Type = 'TR')
    DROP TRIGGER [dbo].[tr_Valuation_SearchCaption]
GO

/*===========================================================================*\
  Description:	Update search caption on the Valuation table.

  Type:		AFTER INSERT, UPDATE

  Created:	September 2003

  Last revision information:
    $Revision: 3 $
    $Date: 30/01/04 17:08 $
    $Author: Bencollier $

\*===========================================================================*/
CREATE TRIGGER [dbo].[tr_Valuation_SearchCaption]
ON [dbo].[Valuation]
AFTER INSERT, UPDATE
AS 
	IF UPDATE(Vague_Date_Start) 
			OR UPDATE(Vague_Date_End)
			OR UPDATE(Vague_Date_Type)
			OR UPDATE(Type_Concept_Key)
		UPDATE Valuation 
		SET Search_Caption=
				dbo.ufn_GetDateFromVagueDate(I.Vague_Date_Start, I.Vague_Date_End, I.Vague_Date_Type)
				+ ISNULL(' - ' + CT.Plaintext, ''),
			Display_Caption=
				dbo.ufn_GetDateFromVagueDate(I.Vague_Date_Start, I.Vague_Date_End, I.Vague_Date_Type)
				+ ISNULL(' - ' + CT.Item_Name, '')
		FROM Valuation V
		INNER JOIN Inserted I on I.Valuation_Key=V.Valuation_Key
		LEFT JOIN VW_ConceptTerm CT ON CT.Concept_Key=I.Type_Concept_Key	

GO