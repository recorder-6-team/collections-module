If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_History_Select_ForCollectionUnit]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_History_Select_ForCollectionUnit]
GO

CREATE PROCEDURE [dbo].[usp_History_Select_ForCollectionUnit] 
@ParentKey CHAR(16)

AS

--  DESCRIPTION
--  Returns History records for a specified Collection Unit
--
--	PARAMETERS
--	NAME				DESCRIPTION
--	@ParentKey 			Only records associated with the parent key are returned
--
--
--  AUTHOR:				Ben Collier, Dorset Software
--  CREATED:			2003-08-20
--
SET NOCOUNT ON

SELECT CUH.Collection_Unit_History_Key AS Item_Key, CUH.Item_Name, From_Vague_Date_Start, From_Vague_Date_End, From_Vague_Date_Type
FROM 
COLLECTION_UNIT CU
	INNER JOIN
		COLLECTION_UNIT_HISTORY CUH
   	ON CU.Collection_Unit_Key = CUH.Collection_Unit_Key AND CU.Collection_Unit_Key = @ParentKey
ORDER BY From_Vague_Date_Start DESC, From_Vague_Date_End DESC, From_Vague_Date_Type, Item_Name

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_History_Select_ForCollectionUnit') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_History_Select_ForCollectionUnit'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_History_Select_ForCollectionUnit TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_History_Select_ForCollectionUnit TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_History_Select_ForCollectionUnit TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_History_Select_ForCollectionUnit TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_History_Select_ForCollectionUnit TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_History_Select_ForCollectionUnit TO [Dev - JNCC SQL]
END

GO