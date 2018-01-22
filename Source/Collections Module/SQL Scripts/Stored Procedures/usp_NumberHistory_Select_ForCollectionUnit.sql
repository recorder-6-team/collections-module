If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_NumberHistory_Select_ForCollectionUnit]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_NumberHistory_Select_ForCollectionUnit]
GO

/*===========================================================================*\
  Description:	Returns Numbering History for a specified Collection

  Parameters:	
	@ParentKey	When specified, only the records associated with the parent key are returned

  Created:	August 2003

  Last revision information:
    $Revision: 4 $
    $Date: 3/08/11 15:42 $
    $Author: Simonlewis $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_NumberHistory_Select_ForCollectionUnit] 
	@ParentKey CHAR(16)
AS

SET NOCOUNT ON

	SELECT 		CUN.Collection_Unit_Number_Key AS Item_Key, C.Published_Term AS Item_Name, CUN.Number, 
			CUN.Preferred AS xPreferred, 0 AS 'ISAccession', CUN.Collection_Unit_Number_Key AS Join_Key

	FROM 		Collection_Unit_Number CUN
	INNER JOIN 	Concept C ON CUN.Type_Concept_Key = C.Concept_Key AND (CUN.Collection_Unit_Key = @ParentKey)
	
	UNION

	SELECT 		M.Movement_Key AS Item_Key, C.Published_Term AS Item_Name, M.Number, 
			1 AS xPreferred, 1 AS 'ISAccession', MCU.Movement_Collection_Unit_Key AS Join_Key

	FROM 		Movement_Collection_Unit MCU
	INNER JOIN	Movement_Direction MD
		ON MCU.Movement_Direction_Key = MD.Movement_Direction_Key 
		AND (MCU.Collection_Unit_Key = @ParentKey)
		AND (MD.Outbound = 0)

	INNER JOIN	Movement M ON MD.Movement_Key = M.Movement_Key AND M.Movement_Type IN (0, 1)
	LEFT JOIN	Concept C ON C.Concept_Key = 'SYSTEM0000000006' --ACCESSION NUMBER
	
	ORDER BY 	xPreferred DESC, IsAccession, Item_Name
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_NumberHistory_Select_ForCollectionUnit') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_NumberHistory_Select_ForCollectionUnit'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_NumberHistory_Select_ForCollectionUnit TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_NumberHistory_Select_ForCollectionUnit TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_NumberHistory_Select_ForCollectionUnit TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_NumberHistory_Select_ForCollectionUnit TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_NumberHistory_Select_ForCollectionUnit TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_NumberHistory_Select_ForCollectionUnit TO [Dev - JNCC SQL]
END

GO