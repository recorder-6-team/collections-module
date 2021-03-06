If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_CollectionUnitRelation_DragDropKey_Get]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_CollectionUnitRelation_DragDropKey_Get]
GO

CREATE PROCEDURE [dbo].[usp_CollectionUnitRelation_DragDropKey_Get] 
@Key CHAR(16),
@ParentCollectionUnitKey CHAR(16),
@DragDropKey CHAR(16) OUTPUT

AS

--  DESCRIPTION
--  Returns the key used for drag and drop given a Collection_Unit_Relation_Key
--
--  PARAMETERS
--	NAME				DESCRIPTION
--	@Key				Collection_Unit_Relation_Key
--
--
--  AUTHOR:				Ben Collier, Dorset Software
--  CREATED:			2004-01-19
--
SET NOCOUNT ON
SELECT @DragDropKey = 
	CASE WHEN To_Collection_Unit_Key = @ParentCollectionUnitKey THEN From_Collection_Unit_Key
	ELSE To_Collection_Unit_Key
	END
FROM
	Collection_Unit_Relation
WHERE Collection_Unit_Relation_Key = @Key

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_CollectionUnitRelation_DragDropKey_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_CollectionUnitRelation_DragDropKey_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitRelation_DragDropKey_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_CollectionUnitRelation_DragDropKey_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_CollectionUnitRelation_DragDropKey_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_CollectionUnitRelation_DragDropKey_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_CollectionUnitRelation_DragDropKey_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitRelation_DragDropKey_Get TO [Dev - JNCC SQL]
END

GO