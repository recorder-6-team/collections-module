/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_CollectionUnitProcess_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_CollectionUnitProcess_Select]
GO

/*===========================================================================*\
  Description:	Returns a collection unit process record.

  Parameters:	@Key	Collection_Unit_Process key

  Created:	August 2003

  Last revision information:
    $Revision: 2 $
    $Date: 23/01/04 10:22 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_CollectionUnitProcess_Select]
	@Key char(16)
AS

SET NOCOUNT ON

SELECT
	CP.Process_Concept_Key,
	CTP.Plaintext AS ProcessTerm,
	CP.Inferred_Process,
	CP.[Description],
	CP.Inferred_Description,
	CP.Inferred_Person,
	CP.Name_Key,
	dbo.ufn_GetFormattedName(CP.Name_Key) AS Person,
	CP.Vague_Date_Start,
	CP.Vague_Date_End,
	CP.Vague_Date_Type,
	CP.Inferred_Date,
	CP.Custodian,
	CP.[Timestamp]
FROM Collection_Unit_Process CP
INNER JOIN VW_ConceptTerm CTP ON CTP.Concept_Key=CP.Process_Concept_Key
WHERE CP.Collection_Unit_Process_Key=@Key
SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_CollectionUnitProcess_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_CollectionUnitProcess_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitProcess_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_CollectionUnitProcess_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_CollectionUnitProcess_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_CollectionUnitProcess_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_CollectionUnitProcess_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitProcess_Select TO [Dev - JNCC SQL]
END

GO
