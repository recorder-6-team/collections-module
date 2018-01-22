/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Funding_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Funding_Select]
GO

/*===========================================================================*\
  Description:	Returns data for the FundingGeneral frame.

  Parameters:	@Key	Collection Unit key

  Created:	Setember 2003

  Last revision information:
    $Revision: 2 $
    $Date: 12/11/03 14:48 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Funding_Select]
	@Key char(16),
	@IsMovement BIT
AS

SET NOCOUNT ON

	IF @IsMovement=1 
		SELECT
			F.Funded_By_Name_Key,
			dbo.ufn_GetFormattedName(F.Funded_By_Name_Key) AS Funded_By,
			F.Vague_Date_Start,
			F.Vague_Date_End,
			F.Vague_Date_Type,
			F.Amount,
			F.Currency_Concept_Key,
			CT.PlainText AS Currency,
			F.Details,
			F.Timestamp	
		FROM Movement_Funding F
	
		INNER JOIN VW_ConceptTerm CT ON CT.Concept_Key=F.Currency_Concept_Key
	
		WHERE F.Movement_Funding_Key=@Key
	ELSE
		SELECT
			F.Funded_By_Name_Key,
			dbo.ufn_GetFormattedName(F.Funded_By_Name_Key) AS Funded_By,
			F.Vague_Date_Start,
			F.Vague_Date_End,
			F.Vague_Date_Type,
			F.Amount,
			F.Currency_Concept_Key,
			CT.PlainText AS Currency,
			F.Details,
			F.Timestamp	
		FROM Conservation_Job_Funding F
	
		INNER JOIN VW_ConceptTerm CT ON CT.Concept_Key=F.Currency_Concept_Key
	
		WHERE F.Conservation_Job_Funding_Key=@Key

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Funding_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Funding_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Funding_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Funding_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Funding_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Funding_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Funding_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Funding_Select TO [Dev - JNCC SQL]
END

GO