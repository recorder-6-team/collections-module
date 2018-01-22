If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Measurements_Select_ForCollectionUnit]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Measurements_Select_ForCollectionUnit]
GO

CREATE PROCEDURE [dbo].[usp_Measurements_Select_ForCollectionUnit] 
@ParentKey CHAR(16)

AS
--  DESCRIPTION
--  Returns Measurements data to the CollectionsBrowser for a given Collection Unit
--
--  PARAMETERS
--	NAME				DESCRIPTION
--	@ParentKey 			When specified, only the records associated with the parent key are returned
--
--
--  AUTHOR:     		Ben Collier, Dorset Software
--  CREATED:    		2003-08-22
--
SET NOCOUNT ON

SELECT CUD.Collection_Unit_Data_Key As Item_Key, CUD.Applies_To As Item_Name, S.Date_Time_Start
FROM COLLECTION_UNIT_DATA CUD
INNER JOIN SESSION S ON CUD.Entered_Session_ID = S.Session_ID AND CUD.Collection_Unit_Key = @ParentKey
								AND CUD.Is_Descriptor = 0
ORDER BY CUD.Applies_To


GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Measurements_Select_ForCollectionUnit') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Measurements_Select_ForCollectionUnit'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Measurements_Select_ForCollectionUnit TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Measurements_Select_ForCollectionUnit TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Measurements_Select_ForCollectionUnit TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Measurements_Select_ForCollectionUnit TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Measurements_Select_ForCollectionUnit TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Measurements_Select_ForCollectionUnit TO [Dev - JNCC SQL]
END

GO