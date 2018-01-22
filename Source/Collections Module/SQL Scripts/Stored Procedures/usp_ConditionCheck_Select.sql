/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConditionCheck_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConditionCheck_Select]
GO

/*===========================================================================*\
  Description:	Returns data for the General tab of the Condition Check screen.

  Parameters:	@Key	Collection key

  Created:	September 2003

  Last revision information:
    $Revision: 5 $
    $Date: 2/03/04 12:02 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConditionCheck_Select]
	@Key char(16)
AS

SET NOCOUNT ON

	/*-----------------------------------------------------------*\
	  Need to find out it ConditionCheck is associated with any
	  Collections or Stores, so we know whether to display the
	  AppliesToAllSpecimens check box (See TSD - 4.2.7.4).
	\*-----------------------------------------------------------*/
	DECLARE @HasStores bit,
		@HasCollections bit

	SELECT		@HasStores = CASE Count(*) WHEN 0 THEN 0 ELSE 1 END
	FROM		Collection_Unit_Check AS CUC
	INNER JOIN 	Store AS S ON S.Collection_Unit_Key = CUC.Collection_Unit_Key
	WHERE		CUC.Conservation_Check_Key = @Key

	SELECT		@HasCollections = CASE Count(*) WHEN 0 THEN 0 ELSE 1 END
	FROM		Collection_Unit_Check AS CUC
	INNER JOIN 	Collection AS C ON C.Collection_Unit_Key = CUC.Collection_Unit_Key
	WHERE		CUC.Conservation_Check_Key = @Key


	SELECT 	TType.Item_Name AS Check_Type_Name, 
		C.Type_Concept_Key,
		C.Ref_Number, 
		C.Vague_Date_Start, 
		C.Vague_Date_End, 
		C.Vague_Date_Type, 
		C.Checked_By_Name_Key, 
		dbo.ufn_GetFormattedName(C.Checked_By_Name_Key) AS Checked_By_Name,
		TCondition.Item_Name AS Condition_Name, 
		C.Condition_Concept_Key,
		C.Applies_To_Contained_Specimens, 
		C.Details,
		C.Domain_Mask,
		@HasCollections AS Has_Collections,
		@HasStores AS Has_Stores,
		C.[Timestamp]
	
	FROM 		Conservation_Check AS C
	INNER JOIN 	VW_ConceptTerm AS TType ON C.Type_Concept_Key = TType.Concept_Key
	INNER JOIN 	VW_ConceptTerm AS TCondition ON C.Condition_Concept_Key = TCondition.Concept_Key
	WHERE C.Conservation_Check_Key = @Key

SET NOCOUNT OFF

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConditionCheck_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConditionCheck_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConditionCheck_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConditionCheck_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConditionCheck_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ConditionCheck_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConditionCheck_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConditionCheck_Select TO [Dev - JNCC SQL]
END

GO