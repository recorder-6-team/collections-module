/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Valuation_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Valuation_Select]
GO

/*===========================================================================*\
  Description:	Returns a valuation record.

  Parameters:	@Key	Valuation key

  Created:	Oct 2003

  Last revision information:
    $Revision: 2 $
    $Date: 12/11/03 16:40 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Valuation_Select]
	@Key char(16)
AS

SET NOCOUNT ON

SELECT
	V.Vague_Date_Start,
	V.Vague_Date_End,
	V.Vague_Date_Type,
	V.Ref_Number,
	V.Type_Concept_Key,
	CTT.Plaintext AS TypeTerm,
	V.Valued_By_Name_Key,
	dbo.ufn_GetFormattedName(V.Valued_By_Name_Key) AS ValuedBy,
	V.Value_Amount,
	V.Currency_Concept_Key,
	CTC.Plaintext AS CurrencyTerm,
	V.Valid_From_Vague_Date_Start,
	V.Valid_From_Vague_Date_End,
	V.Valid_From_Vague_Date_Type,
	V.Valid_To_Vague_Date_Start,
	V.Valid_To_Vague_Date_End,
	V.Valid_To_Vague_Date_Type,
	V.[Description],
	V.Custodian,
	V.[Timestamp]
FROM Valuation V
LEFT JOIN VW_ConceptTerm CTT ON CTT.Concept_Key=V.Type_Concept_Key
INNER JOIN VW_ConceptTerm CTC ON CTC.Concept_Key=V.Currency_Concept_Key
WHERE V.Valuation_Key=@Key

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Valuation_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Valuation_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Valuation_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Valuation_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Valuation_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Valuation_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Valuation_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Valuation_Select TO [Dev - JNCC SQL]
END

GO