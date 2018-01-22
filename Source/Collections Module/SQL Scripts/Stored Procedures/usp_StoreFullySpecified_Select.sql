/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_StoreFullySpecified_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_StoreFullySpecified_Select]
GO

/*===========================================================================*\
  Description:	Returns a fully specified store name, including code and parent

  Parameters:	@SearchText

  Created:	Sept 2004

  Last revision information:
    $Revision: 1 $
    $Date: 7/10/04 13:29 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_StoreFullySpecified_Select]
@ItemKey char(16),
@Output varchar(500) output
AS
	
SELECT
	@Output=S.Item_Name + 
		IsNull(' - ' + CU.Current_Location_Code, IsNull(' - ' + CU.Usual_Location_Code, '')) + 
		IsNull(', ' + dbo.ufn_GetTranslation_String_From_Value(0, 'IN') + ' ' + SP.Item_Name + 
		IsNull(' - ' + CUP.Current_Location_Code, IsNull(' - ' + CUP.Usual_Location_Code, '')), '')
FROM Store S
INNER JOIN Collection_Unit CU ON CU.Collection_Unit_Key=S.Collection_Unit_Key
LEFT JOIN Store SP ON SP.Collection_Unit_Key=CU.Current_Container_Collection_Unit_Key
LEFT JOIN Collection_Unit CUP ON CUP.Collection_Unit_Key=SP.Collection_Unit_Key
WHERE S.Collection_Unit_Key=@ItemKey


GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_StoreFullySpecified_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_StoreFullySpecified_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_StoreFullySpecified_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_StoreFullySpecified_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_StoreFullySpecified_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_StoreFullySpecified_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_StoreFullySpecified_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_StoreFullySpecified_Select TO [Dev - JNCC SQL]
END

GO