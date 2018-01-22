/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
       FROM   SysObjects
       WHERE  Id = Object_Id(N'[dbo].[usp_Store_PossibleChildStores_Select]')
       AND    Type = 'P')
    DROP PROCEDURE [dbo].[usp_Store_PossibleChildStores_Select]
GO

/*===========================================================================*\
  Description:  Select the list of stores that are currently in a store, or
					usually held in a store

  Parameters:   @Key	Store collection unit key

  Created:      Sept 2004

  Last revision information:
    $Revision: 6 $
    $Date: 8/03/05 10:23 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Store_PossibleChildStores_Select]
    @Key CHAR(16)
AS

	SELECT DISTINCT 
		S.Collection_Unit_Key, 
		S.Item_Name + ISNULL(' - ' + C.Current_Location_Code, ISNULL(' - ' + C.Usual_Location_Code, '')) AS Item_Name,
		CASE 
			WHEN C.Current_Container_Collection_Unit_Key=@Key THEN 1
			ELSE 0
		END AS IsCurrent
	FROM Store S
	INNER JOIN Collection_Unit C ON C.Collection_Unit_Key=S.Collection_Unit_Key
	LEFT JOIN Movement_Collection_Unit MCU ON MCU.Collection_Unit_Key=C.Collection_Unit_Key
	LEFT JOIN Movement_Direction MD ON MD.Movement_Direction_Key=MCU.Movement_Direction_Key
	LEFT JOIN Movement_Of_Material MOM ON MOM.Movement_Direction_Key=MD.Movement_Direction_Key
	LEFT JOIN Movement_Of_Material_Exclusion MOME 
			ON MOME.Movement_Of_Material_Key=MOM.Movement_Of_Material_Key
			AND MOME.Collection_Unit_Key=@Key
	LEFT JOIN Movement M 
			ON M.Movement_Key=MD.Movement_Key
			AND M.Movement_Type IN (4,5,7) -- destroyed, disposed, lost
	WHERE 	(C.Current_Container_Collection_Unit_Key=@Key
		OR C.Usual_Container_Collection_Unit_Key=@Key)
	AND 	(M.Movement_Key IS NULL
		OR MOME.Movement_Of_Material_Exclusion_Key IS NOT NULL)

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Store_PossibleChildStores_Select') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_Store_PossibleChildStores_Select'
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
        GRANT EXECUTE ON dbo.usp_Store_PossibleChildStores_Select TO [R2k_Administrator]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
        GRANT EXECUTE ON dbo.usp_Store_PossibleChildStores_Select TO [R2k_FullEdit]
 		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
        GRANT EXECUTE ON dbo.usp_Store_PossibleChildStores_Select TO [R2k_RecordCardsOnly]
 		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        GRANT EXECUTE ON dbo.usp_Store_PossibleChildStores_Select TO [R2k_AddOnly]
 		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
        GRANT EXECUTE ON dbo.usp_Store_PossibleChildStores_Select TO [R2k_ReadOnly]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_Store_PossibleChildStores_Select TO [Dev - JNCC SQL]
END