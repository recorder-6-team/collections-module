If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_JobsAppliedTo_Select_ForSpecimen]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_JobsAppliedTo_Select_ForSpecimen]
GO

/*===========================================================================*\
  Description:	Returns Jobs associated with a specified Specimen

  Parameters:
	@ParentKey 	Only the records associated with the parent key are returned
	@SortOrderIndex	Index determining Sort Order

  Created:	August 2003

  Last revision information:
    $Revision: 6 $
    $Date: 14/04/04 12:40 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_JobsAppliedTo_Select_ForSpecimen] 
	@ParentKey CHAR(16),
	@SortOrderIndex TINYINT
AS

SET NOCOUNT ON

IF @SortOrderIndex = 0
	SELECT DISTINCT	CJ.Conservation_Job_Key AS Item_Key, Item_Name, CJ.Display_Caption, CUC.Collection_Unit_Check_Key AS Join_Key

	FROM		Collection_Unit CU
	INNER JOIN	Collection_Unit_Check CUC ON CU.Collection_Unit_Key = CUC.Collection_Unit_Key AND CU.Collection_Unit_Key = @ParentKey
	INNER JOIN	Conservation_Check CC ON CUC.Conservation_Check_Key = CC.Conservation_Check_Key
	INNER JOIN	Conservation_Task CT ON CC.Conservation_Check_Key = CT.Conservation_Check_Key
	INNER JOIN	Conservation_Job CJ ON CT.Conservation_Job_Key = CJ.Conservation_Job_Key 

	ORDER BY 	CJ.Display_Caption
ELSE
	SELECT DISTINCT	CJ.Conservation_Job_Key AS Item_Key, Item_Name, CJ.Display_Caption, CUC.Collection_Unit_Check_Key AS Join_Key

	FROM		Collection_Unit CU
	INNER JOIN	Collection_Unit_Check CUC ON CU.Collection_Unit_Key = CUC.Collection_Unit_Key AND CU.Collection_Unit_Key = @ParentKey
	INNER JOIN	Conservation_Check CC ON CUC.Conservation_Check_Key = CC.Conservation_Check_Key
	INNER JOIN	Conservation_Task CT ON CC.Conservation_Check_Key = CT.Conservation_Check_Key
	INNER JOIN	Conservation_Job CJ ON CT.Conservation_Job_Key = CJ.Conservation_Job_Key 

	ORDER BY 	Item_Name
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_JobsAppliedTo_Select_ForSpecimen') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_JobsAppliedTo_Select_ForSpecimen'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_JobsAppliedTo_Select_ForSpecimen TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_JobsAppliedTo_Select_ForSpecimen TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_JobsAppliedTo_Select_ForSpecimen TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_JobsAppliedTo_Select_ForSpecimen TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_JobsAppliedTo_Select_ForSpecimen TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_JobsAppliedTo_Select_ForSpecimen TO [Dev - JNCC SQL]
END
GO