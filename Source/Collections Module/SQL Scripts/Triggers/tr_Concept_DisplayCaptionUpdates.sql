/*===========================================================================*\
  Drop trigger before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[tr_Concept_DisplayCaptionUpdates]') 
	   AND    Type = 'TR')
    DROP TRIGGER [dbo].[tr_Concept_DisplayCaptionUpdates]
GO

/*===========================================================================*\
  Description:	Update display caption on the following tables when an existing
				concept's term is modified:
				Conservation Check

  Type:		AFTER UPDATE

  Created:	September 2003

  Last revision information:
    $Revision: 3 $
    $Date: 3/08/11 10:10 $
    $Author: Simonlewis $

\*===========================================================================*/
CREATE TRIGGER [dbo].[tr_Concept_DisplayCaptionUpdates]
ON [dbo].[Concept]
AFTER UPDATE
AS 
	IF UPDATE(Term_Key) OR UPDATE(Published_Term)
	BEGIN
		UPDATE Conservation_Check 
		SET Search_Caption=
				dbo.ufn_GetDateFromVagueDate(CC.Vague_Date_Start, CC.Vague_Date_End, CC.Vague_Date_Type)
				+ ' - ' +
				T.Plaintext
				+ ' - ' +
				CC.Ref_Number,
			Display_Caption=
				dbo.ufn_GetDateFromVagueDate(CC.Vague_Date_Start, CC.Vague_Date_End, CC.Vague_Date_Type)
				+ ' - ' +
				C.Published_Term
				+ ' - ' +
				CC.Ref_Number
		FROM Conservation_Check CC
		INNER JOIN Concept C ON C.Concept_Key=CC.Type_Concept_Key
		INNER JOIN Term T ON T.Term_Key=C.Term_Key
		INNER JOIN Inserted I on I.Concept_Key=C.Concept_Key

		UPDATE Enquiry 
		SET Search_Caption=
				dbo.ufn_GetDateFromVagueDate(E.Vague_Date_Start, E.Vague_Date_End, E.Vague_Date_Type)
				+ ' - ' +
				T.Plaintext,
			Display_Caption=
				dbo.ufn_GetDateFromVagueDate(E.Vague_Date_Start, E.Vague_Date_End, E.Vague_Date_Type)
				+ ' - ' +
				C.Published_Term
		FROM Enquiry E
		INNER JOIN Concept C ON C.Concept_Key=E.Enquiry_Type_Concept_Key
		INNER JOIN Term T ON T.Term_Key=C.Term_Key
		INNER JOIN Inserted I on I.Concept_Key=C.Concept_Key

		UPDATE Valuation 
		SET Search_Caption=
				dbo.ufn_GetDateFromVagueDate(V.Vague_Date_Start, V.Vague_Date_End, V.Vague_Date_Type)
				+ ' - ' +
				T.Plaintext,
			Display_Caption=
				dbo.ufn_GetDateFromVagueDate(V.Vague_Date_Start, V.Vague_Date_End, V.Vague_Date_Type)
				+ ' - ' +
				C.Published_Term
		FROM Valuation V
		INNER JOIN Concept C ON C.Concept_Key=V.Type_Concept_Key
		INNER JOIN Term T ON T.Term_Key=C.Term_Key
		INNER JOIN Inserted I on I.Concept_Key=C.Concept_Key

	END

GO