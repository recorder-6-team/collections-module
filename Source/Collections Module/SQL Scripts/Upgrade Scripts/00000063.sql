IF OBJECT_ID(N'usp_CollectionUnitStatus_Select') IS NOT NULL
	DROP PROCEDURE usp_CollectionUnitStatus_Select
GO

/*===========================================================================*\
  Description:	Returns the movement status for a collection unit record.

  Parameters:	@Key	Collection unit key

  Created:	August 2003

  Last revision information:
    $Revision: 2 $
    $Date: 13/11/12 10:19 $
    $Author: Alexanderpadley $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_CollectionUnitStatus_Select]
	@Key char(16)
AS

/*=========================================================
	Get the movement status.
=========================================================*/
DECLARE @MovementKey CHAR(16),
		@MovementStatus VARCHAR(200),
		@MovementType INT

SELECT TOP(1)	@MovementKey = M.Movement_Key,
				@MovementStatus = CASE M.Movement_Type
									WHEN 0 THEN 'Accessioned'
									WHEN 1 THEN	'Exchanged'
									WHEN 2 THEN	'Loaned In'
									WHEN 3 THEN	'Loaned Out'
									WHEN 4 THEN	'Destroyed'
									WHEN 5 THEN	'Disposed'
									WHEN 6 THEN	'Internal Transfer'
									WHEN 7 THEN 'Lost'
									WHEN 8 THEN	'Sold'
									WHEN 9 THEN	'Hosted Material'
								END,
				@MovementType = M.Movement_Type
--SELECT TOP(10) M.*
FROM 		Collection_Unit CU
INNER JOIN	Movement_Collection_Unit MCU 
	ON	CU.Collection_Unit_Key = MCU.Collection_Unit_Key
	AND CU.Collection_Unit_Key = @Key
INNER JOIN	Movement_Direction MD 
	ON	MCU.Movement_Direction_Key = MD.Movement_Direction_Key
INNER JOIN	Movement M 
	ON	MD.Movement_Key = M.Movement_Key
ORDER BY	CASE WHEN ISDATE(
					dbo.[ufn_GetDateFromVagueDate](M.Exp_Vague_Date_start, M.Exp_Vague_Date_End, M.Exp_Vague_Date_Type)) = 1
				THEN CAST(dbo.[ufn_GetDateFromVagueDate](M.Exp_Vague_Date_start, M.Exp_Vague_Date_End, M.Exp_Vague_Date_Type) AS DATETIME)
				ELSE NULL
			END DESC

DECLARE @DisplayLastJob TINYINT

IF (@MovementType IN (3,4,5,7,8))
BEGIN
	SET @DisplayLastJob = 0
END
ELSE
BEGIN
	SET @DisplayLastJob = 1
END

/*=========================================================
	Get the condition status.
=========================================================*/
DECLARE	@ConditionKey CHAR(16),
		@ConditionStatus VARCHAR(500)

-- This will affect the entire session. But Recorder shouldn't notice the difference.
SET DateFormat dmy
SELECT	TOP(1) 
		@ConditionKey = CC.Conservation_Check_Key,
		@ConditionStatus = C.Published_Term
FROM	Collection_Unit_Check CUC
INNER JOIN	Conservation_Check CC
	ON	CC.Conservation_Check_Key = CUC.Conservation_Check_Key
INNER JOIN	Concept C
	ON	CC.Condition_Concept_Key = C.Concept_Key
WHERE	CUC.Collection_Unit_Key = @Key
ORDER BY	CASE WHEN ISDATE(
					dbo.[ufn_GetDateFromVagueDate](Vague_Date_start, Vague_Date_End, Vague_Date_Type)) = 1
				THEN CAST(dbo.[ufn_GetDateFromVagueDate](Vague_Date_start, Vague_Date_End, Vague_Date_Type) AS DATETIME)
				ELSE NULL
			END DESC

/*=========================================================
	Get the conservation job title.
=========================================================*/
DECLARE	@ConservationJobKey CHAR(16),
		@ConservationJobTitle VARCHAR(500)

SELECT TOP(1)	@ConservationJobKey = CJ.Conservation_Job_Key, 
				@ConservationJobTitle = Item_Name
FROM		Collection_Unit CU
INNER JOIN	Collection_Unit_Check CUC 
	ON		CU.Collection_Unit_Key = CUC.Collection_Unit_Key 
	AND		CU.Collection_Unit_Key = @Key
INNER JOIN	Conservation_Check CC 
	ON		CUC.Conservation_Check_Key = CC.Conservation_Check_Key
INNER JOIN	Conservation_Task CT 
	ON		CC.Conservation_Check_Key = CT.Conservation_Check_Key
INNER JOIN	Conservation_Job CJ 
	ON		CT.Conservation_Job_Key = CJ.Conservation_Job_Key
WHERE		CJ.Status = 0 -- Pending status
ORDER BY	CASE WHEN ISDATE(
					dbo.[ufn_GetDateFromVagueDate](CJ.From_Vague_Date_Start, CJ.From_Vague_Date_End, CJ.From_Vague_Date_Type)) = 1
				THEN CAST(dbo.[ufn_GetDateFromVagueDate](CJ.From_Vague_Date_Start, CJ.From_Vague_Date_End, CJ.From_Vague_Date_Type) AS DATETIME)
				ELSE NULL
			END DESC

SELECT	@MovementKey AS MovementKey,
		@MovementStatus AS MovementStatus,
		@MovementType AS MovementType,
		@ConditionKey AS ConditionKey,
		@ConditionStatus AS ConditionStatus,
		@ConservationJobKey AS ConservationJobKey,
		@ConservationJobTitle AS ConservationJobTitle,
		@DisplayLastJob AS DisplayLastJob

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_CollectionUnitStatus_Select') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_CollectionUnitStatus_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitStatus_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_CollectionUnitStatus_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_CollectionUnitStatus_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_CollectionUnitStatus_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_CollectionUnitStatus_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_CollectionUnitStatus_Select TO [Dev - JNCC SQL]
END
GO