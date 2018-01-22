/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Enquiry_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Enquiry_Select]
GO

/*===========================================================================*\
  Description:	Returns an enquiry record.

  Parameters:	@Key	Enquiry key

  Created:	August 2003

  Last revision information:
    $Revision: 4 $
    $Date: 6/02/04 10:13 $
    $Author: Bencollier $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Enquiry_Select]
	@Key char(16)
AS

SET ANSI_NULLS ON
SET ANSI_PADDING ON
SET ANSI_WARNINGS ON
SET ARITHABORT ON
SET CONCAT_NULL_YIELDS_NULL ON
SET QUOTED_IDENTIFIER ON
SET NO_BROWSETABLE OFF

SET NOCOUNT ON

SELECT 
		E.Enquirer_Name_Key,
		dbo.ufn_GetFormattedName(Enquirer_Name_Key) AS Linked_Name,
		E.Vague_Enquirer,
		E.Enquiry_Type_Concept_Key,
		CTT.Plaintext AS TypeTerm,
		E.Enquiry_Method_Concept_Key,
		CTM.Plaintext AS MethodTerm,
		E.Vague_Date_Start, 
		E.Vague_Date_End,
		E.Vague_Date_Type,
		E.Material_Left, 
		E.Observation_Planned,
		E.[Description], 
		E.Answered_By_Name_Key,
		E.Response,
		dbo.ufn_GetFormattedName(Answered_By_Name_Key) AS Answered_By,
		E.Answered,
		E.Answered_Vague_Date_Start,
		E.Answered_Vague_Date_End,
		E.Answered_Vague_Date_Type,
		E.Custodian,
		E.[Timestamp]		
FROM Enquiry E
INNER JOIN VW_ConceptTerm CTT ON CTT.Concept_Key=Enquiry_Type_Concept_Key
INNER JOIN VW_ConceptTerm CTM ON CTM.Concept_Key=Enquiry_Method_Concept_Key
WHERE E.Enquiry_Key=@Key

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Enquiry_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Enquiry_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Enquiry_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Enquiry_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Enquiry_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Enquiry_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Enquiry_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Enquiry_Select TO [Dev - JNCC SQL]
END

GO