/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConservationJob_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConservationJob_Select]
GO

/*===========================================================================*\
  Description:	Returns data for the JobGeneral frame.

  Parameters:	@Key	Collection Unit key

  Created:	Setember 2003

  Last revision information:
    $Revision: 3 $
    $Date: 12/11/03 13:47 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConservationJob_Select]
	@Key char(16)
AS

SET NOCOUNT ON

	SELECT
 		CJ.Conservation_Job_Key,
		CJ.Display_Caption,
		CJ.Item_Name,
		CJ.Job_Number,
		CJ.From_Vague_Date_Start,
		CJ.From_Vague_Date_End,
		CJ.From_Vague_Date_Type,
		CJ.To_Vague_Date_Start,
		CJ.To_Vague_Date_End,
		CJ.To_Vague_Date_Type,
		ROUND(CJ.Duration,2) AS Duration,
		CJ.Duration_Unit_Concept_Key,
		CTUnit.Plaintext AS TimeUnit,
		CJ.Status,
		CJ.Domain_Mask,
		CJ.Details,
		CJ.Cost,
		CJ.Currency_Concept_Key,
		CTCurr.Plaintext AS Currency,
		CJ.Timestamp

	FROM Conservation_Job CJ

	LEFT JOIN VW_ConceptTerm CTUnit on CTUnit.Concept_Key=CJ.Duration_Unit_Concept_Key
	LEFT JOIN VW_ConceptTerm CTCurr on CTCurr.Concept_Key=CJ.Currency_Concept_Key
	
	WHERE CJ.Conservation_Job_Key=@Key


SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConservationJob_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConservationJob_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConservationJob_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConservationJob_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConservationJob_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ConservationJob_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConservationJob_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConservationJob_Select TO [Dev - JNCC SQL]
END

GO