/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Task_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Task_Select]
GO

/*===========================================================================*\
  Description:	Returns data for the General tab of the Tasks Identified screen.

  Parameters:	@Key	Collection key

  Created:	September 2003

  Last revision information:
    $Revision: 5 $
    $Date: 18/11/03 15:09 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Task_Select]
	@Key char(16)
AS

SET NOCOUNT ON

	SELECT 	C.Conservation_Task_Key,
		C.Conservation_Check_Key,
		C.Conservation_Job_Key,
		C.Set_Vague_Date_Start, 
		C.Set_Vague_Date_End, 
		C.Set_Vague_Date_Type,
		C.Status,
		C.Type_Concept_Key,
		TType.Item_Name AS Type_Item_Name,
		C.Priority,
		C.Duration,
		TDuration.Item_Name AS Duration_Item_Name,
		C.Duration_Unit_Concept_Key,
		C.Identifier_Name_Key, 
			dbo.ufn_GetFormattedName(C.Identifier_Name_Key) AS Identifier_Name,
		C.Task_Action,
		C.Comment,
		C.[Timestamp]
	
	FROM 		Conservation_Task AS C
	INNER JOIN 	VW_ConceptTerm AS TType ON C.Type_Concept_Key = TType.Concept_Key
	LEFT JOIN 	VW_ConceptTerm AS TDuration ON C.Duration_Unit_Concept_Key = TDuration.Concept_Key
	
	WHERE C.Conservation_Task_Key = @Key

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Task_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Task_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Task_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Task_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Task_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Task_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Task_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Task_Select TO [Dev - JNCC SQL]
END

GO