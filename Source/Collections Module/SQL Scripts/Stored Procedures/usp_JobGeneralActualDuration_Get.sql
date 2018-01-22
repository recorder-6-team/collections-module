/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_JobGeneralActualDuration_Get]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_JobGeneralActualDuration_Get]
GO

/*===========================================================================*\
  Description:	Returns the Actual Duration for the JobGeneral frame.

  Parameters: 	@Key	Conservation_Job_Key
	
  Created:	Setember 2003

  Last revision information:
    $Revision: 2 $
    $Date: 12/11/03 14:48 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_JobGeneralActualDuration_Get]

@Key CHAR(16),
@Duration VARCHAR(100) OUTPUT
	
AS

DECLARE @Seconds BIGINT
DECLARE @HighestMultiplierRelation CHAR(16)
DECLARE @DurationUnitKey CHAR(16)

SET NOCOUNT ON

	--Find the total number of seconds
	SELECT @Seconds=
 		SUM(CASE 
			WHEN MRTo.Meaning_Relation_Key IS NULL AND MRFrom.Meaning_Relation_Key IS NULL THEN
				CT.Duration -- recorded in seconds
			WHEN MRFrom.Meaning_Relation_Key IS NULL THEN
				CT.Duration * MRTo.Multiplicity
		ELSE
			CT.Duration / MRFrom.Multiplicity
		END)

	FROM Conservation_Task CT
	
	INNER JOIN Concept C on C.Concept_Key=CT.Duration_Unit_Concept_Key
	LEFT JOIN Meaning_Relation MRFrom ON MRFrom.From_Meaning_Key=C.Meaning_Key
		AND MRFrom.To_Meaning_Key='SYSTEM000000008Y'
	LEFT JOIN Meaning_Relation MRTo ON MRTo.To_Meaning_Key=C.Meaning_Key
		AND MRTo.From_Meaning_Key='SYSTEM000000008Y'
	
	WHERE CT.Conservation_Job_Key=@Key

	--Find the meaning key for the highest used unit
	SELECT TOP 1 
		@HighestMultiplierRelation=
		CASE WHEN MRFrom.Meaning_Relation_Key IS NULL THEN
			MRTo.Meaning_Relation_Key
		ELSE
			MRFrom.Meaning_Relation_Key
		END,
		@DurationUnitKey=CT.Duration_Unit_Concept_Key

	FROM Conservation_Task CT

	INNER JOIN Concept C on C.Concept_Key=CT.Duration_Unit_Concept_Key
	LEFT JOIN Meaning_Relation MRFrom ON MRFrom.From_Meaning_Key=C.Meaning_Key
		AND MRFrom.To_Meaning_Key='SYSTEM000000008Y'
	LEFT JOIN Meaning_Relation MRTo ON MRTo.To_Meaning_Key=C.Meaning_Key
		AND MRTo.From_Meaning_Key='SYSTEM000000008Y'

	WHERE CT.Conservation_Job_Key=@Key

	ORDER BY  	
		CASE WHEN MRFrom.Meaning_Relation_Key IS NULL THEN
		MRTo.Multiplicity
		ELSE
		1 / MRFrom.Multiplicity
		END DESC

	--Convert the total seconds back into the required unit.
	IF @HighestMultiplierRelation IS NULL
		--Result required in seconds
		SELECT CAST(@Seconds AS VARCHAR(100)) + ' ' + Plaintext AS Duration
	
		FROM VW_ConceptTerm WHERE Concept_Key='SYSTEM000000008Y'
		
	ELSE BEGIN
		--Convert to a different unit
		SELECT 
			@Duration = CAST(ROUND(
				CASE MR.From_Meaning_Key
					WHEN 'SYSTEM000000008Y' THEN @Seconds / MR.Multiplicity
				ELSE
					@Seconds * MR.Multiplicity
				END ,2)AS VARCHAR(100)) + ' ' + Plaintext
	
		FROM Meaning_Relation MR, VW_ConceptTerm CT
	
		WHERE MR.Meaning_Relation_Key=@HighestMultiplierRelation
			AND CT.Concept_Key=@DurationUnitKey
	END

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_JobGeneralActualDuration_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_JobGeneralActualDuration_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_JobGeneralActualDuration_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_JobGeneralActualDuration_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_JobGeneralActualDuration_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_JobGeneralActualDuration_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_JobGeneralActualDuration_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_JobGeneralActualDuration_Get TO [Dev - JNCC SQL]
END

GO