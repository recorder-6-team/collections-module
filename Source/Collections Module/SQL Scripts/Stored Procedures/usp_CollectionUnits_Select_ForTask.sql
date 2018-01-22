/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_CollectionUnits_Select_ForTask]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_CollectionUnits_Select_ForTask]
GO

/*===========================================================================*\
  Description: 	Selects the Collection Units associated with a task.
		The values for Collection_Unit_Type are as follows:
			0 as Collection, 
			1 as Specimen, 
			2 as Store

  Parameters:	@Key		Movement_Of_Material_Key
		@TaskKey

  Created:	January 2004

  Last revision information:
    $Revision: 2 $
    $Date: 29/03/04 14:09 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_CollectionUnits_Select_ForTask]
	@TaskKey CHAR(16),
	@UserDomainMask INT,
	@SessionID CHAR(16)
AS

SET ANSI_NULLS ON
SET ANSI_PADDING ON
SET ANSI_WARNINGS ON
SET ARITHABORT ON
SET CONCAT_NULL_YIELDS_NULL ON
SET QUOTED_IDENTIFIER ON
SET NO_BROWSETABLE OFF
	
	/*-----------------------------------------*\
	  Select the Collections.
	\*-----------------------------------------*/
	SELECT 		CUC.Collection_Unit_Key,
			C.Item_Name,
			0 AS Collection_Unit_Type,
			CASE WHEN CUT.Conservation_Task_Key IS NULL 	THEN 0
									ELSE 1
			END AS Included 
	FROM		Conservation_Task AS CT 
	INNER JOIN	Collection_Unit_Check AS CUC ON CUC.Conservation_Check_Key = CT.Conservation_Check_Key
	LEFT JOIN	Collection_Unit_Task AS CUT ON CUT.Conservation_Task_Key = CT.Conservation_Task_Key
							AND CUT.Collection_Unit_Key = CUC.Collection_Unit_Key
	INNER JOIN	Collection AS C ON C.Collection_Unit_Key = CUC.Collection_Unit_Key
  INNER JOIN Collection_Unit CU ON CU.Collection_Unit_Key = C.Collection_Unit_Key
        AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
				OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
	WHERE		CT.Conservation_Task_Key = @TaskKey

	UNION

	/*-----------------------------------------*\
	  Select the Specimens.
	\*-----------------------------------------*/
	SELECT 		CUC.Collection_Unit_Key,
			CASE WHEN SU.Life_Sciences = 0 
				THEN ISNULL(CTP.Item_Name, 'No Determination') 
				ELSE ISNULL(dbo.ufn_GetFormattedTaxonNameByParams(ITN.Actual_Name, ITN.Actual_Name_Italic, ITN.Common_Name, 
							ITN.Common_Name_Italic, ITN.Authority, 1), 'No Determination')
			END AS Item_Name, 
			1 AS Collection_Unit_Type,
			CASE WHEN CUT.Conservation_Task_Key IS NULL 	THEN 0
									ELSE 1
			END AS Included 
	FROM		Conservation_Task AS CT 
	INNER JOIN	Collection_Unit_Check AS CUC ON CUC.Conservation_Check_Key = CT.Conservation_Check_Key
	LEFT JOIN	Collection_Unit_Task AS CUT ON CUT.Conservation_Task_Key = CT.Conservation_Task_Key
							AND CUT.Collection_Unit_Key = CUC.Collection_Unit_Key
	INNER JOIN	Specimen_Unit AS SU ON SU.Collection_Unit_Key = CUC.Collection_Unit_Key
  INNER JOIN Collection_Unit CU ON CU.Collection_Unit_Key = SU.Collection_Unit_Key
        AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
				OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
	LEFT JOIN 	Determination D ON SU.Preferred_Determination_Key = D.Determination_Key
	LEFT JOIN 	VW_ConceptTermPreferred CTP ON D.Concept_Key = CTP.Concept_Key 
	LEFT JOIN 	Taxon_Determination TD ON SU.Preferred_Taxon_Determination_Key = TD.Taxon_Determination_Key
	LEFT JOIN 	Index_Taxon_Name ITN ON TD.Taxon_List_Item_Key = ITN.Taxon_List_Item_Key
	WHERE		CT.Conservation_Task_Key = @TaskKey

	UNION

	/*-----------------------*\
	  Select the Stores.
	\*-----------------------*/
	SELECT 		CUC.Collection_Unit_Key,
			S.Item_Name, 
			2 AS Collection_Unit_Type,
			CASE WHEN CUT.Conservation_Task_Key IS NULL 	THEN 0
									ELSE 1
			END AS Included 
	FROM		Conservation_Task AS CT 
	INNER JOIN	Collection_Unit_Check AS CUC ON CUC.Conservation_Check_Key = CT.Conservation_Check_Key
	LEFT JOIN	Collection_Unit_Task AS CUT ON CUT.Conservation_Task_Key = CT.Conservation_Task_Key
							AND CUT.Collection_Unit_Key = CUC.Collection_Unit_Key
	INNER JOIN	Store AS S ON S.Collection_Unit_Key = CUC.Collection_Unit_Key
  INNER JOIN Collection_Unit CU ON CU.Collection_Unit_Key = S.Collection_Unit_Key
        AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
				OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
	WHERE		CT.Conservation_Task_Key = @TaskKey


	ORDER BY Collection_Unit_Type, Item_Name
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_CollectionUnits_Select_ForTask') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_CollectionUnits_Select_ForTask'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_CollectionUnits_Select_ForTask TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_CollectionUnits_Select_ForTask TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_CollectionUnits_Select_ForTask TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_CollectionUnits_Select_ForTask TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_CollectionUnits_Select_ForTask TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_CollectionUnits_Select_ForTask TO [Dev - JNCC SQL]
END

GO
