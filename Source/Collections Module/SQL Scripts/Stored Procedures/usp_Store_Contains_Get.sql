If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Store_Contains_Get]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Store_Contains_Get]
GO

CREATE PROCEDURE [dbo].[usp_Store_Contains_Get] 
@ContainerStoreKey CHAR(16),
@ContainedStoreKey CHAR(16),
@IsCurrentLocation BIT,
@Contains BIT OUTPUT
AS

--  DESCRIPTION
--  Returns successive parent Stores for a specified Store
--
--  PARAMETERS
--  NAME				DESCRIPTION
--	@ParentKey 			Only the records associated with the parent key are returned
--
--
--  AUTHOR:     		Ben Collier, Dorset Software
--  CREATED:    		2003-08-29
--
SET NOCOUNT ON


DECLARE @ContainedStoreContainers TABLE
(
	[Collection_Unit_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL
)


DECLARE @Parent_Collection_Unit_Key CHAR(16)

SET @Parent_Collection_Unit_Key = @ContainedStoreKey
SET @Contains = 0

--Obtain successive parents
WHILE @Parent_Collection_Unit_Key IS NOT NULL
BEGIN
	SELECT @Parent_Collection_Unit_Key = 
		CASE WHEN @IsCurrentLocation = 0 THEN CU.Usual_Container_Collection_Unit_Key 
		ELSE CU.Current_Container_Collection_Unit_Key END
	FROM STORE S 
	INNER JOIN COLLECTION_UNIT CU ON S.Collection_Unit_Key = CU.Collection_Unit_Key
			AND S.Collection_Unit_Key = @Parent_Collection_Unit_Key
	IF (@Parent_Collection_Unit_Key IS NULL) OR (@@RowCount = 0) OR (@Parent_Collection_Unit_Key = 'SYSTEM0000000000')
		--At top of storage hierarchy. No match found.
	BEGIN
		SET @Contains = 0
		BREAK
	END
	ELSE IF @Parent_Collection_Unit_Key = @ContainerStoreKey --Match found.
	BEGIN
		SET @Contains = 1
		BREAK
	END
	ELSE IF EXISTS(SELECT * FROM @ContainedStoreContainers WHERE Collection_Unit_Key = @Parent_Collection_Unit_Key)
	BEGIN --Recursive Store hierarchy found.
		SET @Contains = 1
		BREAK
	END
	ELSE --Log current Store 
		INSERT INTO @ContainedStoreContainers (Collection_Unit_Key) VALUES (@Parent_Collection_Unit_Key)

END


GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Store_Contains_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Store_Contains_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Store_Contains_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Store_Contains_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Store_Contains_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Store_Contains_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Store_Contains_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Store_Contains_Get TO [Dev - JNCC SQL]
END

GO