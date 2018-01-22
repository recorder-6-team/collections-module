/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_CollectionUnits_Select_ForMovement]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_CollectionUnits_Select_ForMovement]
GO

/*===========================================================================*\
  Description: 	Selects the Collection Units associated with a movement.
		The values for Collection_Unit_Type are as follows:
			0 as Collection, 
			1 as Specimen, 
			2 as Store

  Parameters:	@Key		Movement_Of_Material_Key
		@MovementKey

  Created:	January 2004

  Last revision information:
    $Revision: 2 $
    $Date: 8/04/04 11:58 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_CollectionUnits_Select_ForMovement]
	@Key char(16),
	@MovementKey char(16),
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
	SELECT		MCU.Collection_Unit_Key, 
			C.Item_Name, 
			0 AS Collection_Unit_Type,
			CASE WHEN MOME.Movement_Of_Material_Exclusion_Key IS NULL THEN 1
										  ELSE 0
			END AS Included
	FROM		Movement_Collection_Unit AS MCU
	INNER JOIN	Movement_Direction AS MD ON MD.Movement_Direction_Key = MCU.Movement_Direction_Key
	LEFT JOIN	Movement_Of_Material_Exclusion AS MOME ON MOME.Collection_Unit_Key = MCU.Collection_Unit_Key
								AND MOME.Movement_Of_Material_Key = @Key
	INNER JOIN	Collection AS C ON C.Collection_Unit_Key = MCU.Collection_Unit_Key
	INNER JOIN	Collection_Unit AS CU ON CU.Collection_Unit_Key = C.Collection_Unit_Key
    	    			AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
				OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
	WHERE		MD.Movement_Key = @MovementKey

	UNION

	/*-----------------------------------------*\
	  Select the Specimens.
	\*-----------------------------------------*/
	SELECT		MCU.Collection_Unit_Key, 			
			CASE WHEN SU.Life_Sciences = 0 
				THEN ISNULL(CT.Item_Name, 'No Determination') 
				ELSE ISNULL(dbo.ufn_GetFormattedTaxonNameByParams(ITN.Actual_Name, ITN.Actual_Name_Italic, ITN.Common_Name, 
							ITN.Common_Name_Italic, ITN.Authority, 1), 'No Determination')
			END AS Item_Name, 
			1 AS Collection_Unit_Type,
			CASE WHEN MOME.Movement_Of_Material_Exclusion_Key IS NULL THEN 1
										  ELSE 0
			END AS Included
	FROM		Movement_Collection_Unit AS MCU
	INNER JOIN	Movement_Direction AS MD ON MD.Movement_Direction_Key = MCU.Movement_Direction_Key
	INNER JOIN	Specimen_Unit AS SU ON SU.Collection_Unit_Key = MCU.Collection_Unit_Key
	INNER JOIN	Collection_Unit AS CU ON CU.Collection_Unit_Key = SU.Collection_Unit_Key
    	    			AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
				OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
	LEFT JOIN	Movement_Of_Material_Exclusion AS MOME ON MOME.Collection_Unit_Key = MCU.Collection_Unit_Key
								AND MOME.Movement_Of_Material_Key = @Key
	LEFT JOIN 	Determination D ON SU.Preferred_Determination_Key = D.Determination_Key
	LEFT JOIN 	VW_ConceptTermPreferred CT ON D.Concept_Key = CT.Concept_Key 
	LEFT JOIN 	Taxon_Determination TD ON SU.Preferred_Taxon_Determination_Key = TD.Taxon_Determination_Key
	LEFT JOIN 	Index_Taxon_Name ITN ON TD.Taxon_List_Item_Key = ITN.Taxon_List_Item_Key
	WHERE		MD.Movement_Key = @MovementKey

	UNION

	/*-----------------------*\
	  Select the Stores.
	\*-----------------------*/
	SELECT		MCU.Collection_Unit_Key, 
			S.Item_Name, 
			2 AS Collection_Unit_Type,
			CASE WHEN MOME.Movement_Of_Material_Exclusion_Key IS NULL THEN 1
										  ELSE 0
			END AS Included
	FROM		Movement_Collection_Unit AS MCU
	INNER JOIN	Collection_Unit AS CU ON CU.Collection_Unit_Key = MCU.Collection_Unit_Key
    	    			AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
				OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
	INNER JOIN	Movement_Direction AS MD ON MD.Movement_Direction_Key = MCU.Movement_Direction_Key
	INNER JOIN	Store AS S ON S.Collection_Unit_Key = MCU.Collection_Unit_Key
	LEFT JOIN	Movement_Of_Material_Exclusion AS MOME ON MOME.Collection_Unit_Key = MCU.Collection_Unit_Key
								AND MOME.Movement_Of_Material_Key = @Key
	WHERE		MD.Movement_Key = @MovementKey


	ORDER BY Collection_Unit_Type, Item_Name
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_CollectionUnits_Select_ForMovement') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_CollectionUnits_Select_ForMovement'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_CollectionUnits_Select_ForMovement TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_CollectionUnits_Select_ForMovement TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_CollectionUnits_Select_ForMovement TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_CollectionUnits_Select_ForMovement TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_CollectionUnits_Select_ForMovement TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_CollectionUnits_Select_ForMovement TO [Dev - JNCC SQL]
END

GO
