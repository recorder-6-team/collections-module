/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Collection_Select') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_Collection_Select]
GO

/*===========================================================================*\
  Description:	Returns a collection record.

  Parameters:	@Key	Collection key

  Created:	August 2003

  Last revision information:
    $Revision: 7 $
    $Date: 3/08/11 11:36 $
    $Author: Simonlewis $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Collection_Select]
	@Key char(16)
AS

SET NOCOUNT ON

	SELECT	CU.Collection_Unit_Key, 
		C.Item_Name,
		C.Parent_Collection_Collection_Unit_Key,
		C.Assembler_Name_Key, 
		dbo.ufn_GetFormattedName(C.Assembler_Name_Key) AS Assembler_Name,
		C.Topic, 
		CUN.Name_Key AS Owner_Name_Key,
		CU.Current_Container_Collection_Unit_Key,
		SC.Item_Name + ISNULL(' - ' + CSC.Current_Location_Code, ISNULL(' - ' + CSC.Usual_Location_Code, '')) AS Current_Location_Name,
		CU.Current_Location_Code,
		CU.Usual_Container_Collection_Unit_Key, 
		SU.Item_Name + ISNULL(' - ' + CSU.Current_Location_Code, ISNULL(' - ' + CSU.Usual_Location_Code, '')) AS Usual_Location_Name,
		CU.Usual_Location_Code,
		CU.Domain_Mask,
		C.Risk_Concept_Key,
		CP.Published_Term AS Risk_Name,
		C.Timestamp AS Collection_Timestamp,
		CU.Timestamp AS Collection_Unit_Timestamp

	FROM		Collection C
	INNER JOIN 	Collection_Unit CU ON CU.Collection_Unit_Key = C.Collection_Unit_Key
	LEFT JOIN 	Store SC ON SC.Collection_Unit_Key = CU.Current_Container_Collection_Unit_Key
	LEFT JOIN	Collection_Unit CSC ON CSC.Collection_Unit_Key = SC.Collection_Unit_Key 

	LEFT JOIN 	Store SU ON SU.Collection_Unit_Key = CU.Usual_Container_Collection_Unit_Key
	LEFT JOIN	Collection_Unit CSU ON CSU.Collection_Unit_Key = SU.Collection_Unit_Key

	LEFT JOIN 	Collection_Unit_Name CUN ON CUN.Collection_Unit_Key = C.Collection_Unit_Key
						AND CUN.Relation_Type_Concept_Key = 'SYSTEM00000000I7' --Owner concept key
	LEFT JOIN 	Concept CP ON CP.Concept_Key = C.Risk_Concept_Key
	WHERE	C.Collection_Unit_Key = @Key

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Collection_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Collection_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Collection_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Collection_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Collection_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Collection_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Collection_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Collection_Select TO [Dev - JNCC SQL]
END
GO