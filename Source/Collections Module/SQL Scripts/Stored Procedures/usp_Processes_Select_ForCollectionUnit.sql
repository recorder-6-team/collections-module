If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Processes_Select_ForCollectionUnit]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Processes_Select_ForCollectionUnit]
GO

CREATE PROCEDURE [dbo].[usp_Processes_Select_ForCollectionUnit] 
@ParentKey CHAR(16)

AS
--  DESCRIPTION
--  Returns Processes data to the CollectionsBrowser for a given Collection Unit
--
--  PARAMETERS
--	NAME				DESCRIPTION
--	@ParentKey 			When specified, only the records associated with the parent key are returned
--
--
--  AUTHOR:     		Ben Collier, Dorset Software
--  CREATED:    		2003-08-25
--
SET NOCOUNT ON

SELECT DISTINCT Collection_Unit_Process_Key AS Item_Key, C.Published_Term AS Item_Name
FROM 
	Collection_Unit_Process CUP
	INNER JOIN 
		CONCEPT C
	ON CUP.Process_Concept_Key = C.Concept_Key AND CUP.Collection_Unit_Key = @ParentKey
ORDER BY Item_Name

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Processes_Select_ForCollectionUnit') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Processes_Select_ForCollectionUnit'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Processes_Select_ForCollectionUnit TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Processes_Select_ForCollectionUnit TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Processes_Select_ForCollectionUnit TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Processes_Select_ForCollectionUnit TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Processes_Select_ForCollectionUnit TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Processes_Select_ForCollectionUnit TO [Dev - JNCC SQL]
END

GO