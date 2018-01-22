If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Collection_Contains_Get]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Collection_Contains_Get]
GO

CREATE PROCEDURE [dbo].[usp_Collection_Contains_Get] 
@ContainerCollectionKey CHAR(16),
@ContainedCollectionKey CHAR(16),
@Contains BIT OUTPUT
AS

--  DESCRIPTION
--  Checks to see if there is a hierarchical link between two specified collections
--
--  PARAMETERS
--  NAME					DESCRIPTION
--	@ContainerCollectionKey Container collection unit key
--	@ContainedCollectionKey Contained collection unit key
--  @Contains				Result
--
--
--  AUTHOR:     			Ben Collier, Dorset Software
--  CREATED:    			18/02/2004
--
SET NOCOUNT ON


DECLARE @ContainedCollectionContainers TABLE
(
	[Collection_Unit_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL
)


DECLARE @Parent_Collection_Unit_Key CHAR(16)

SET @Parent_Collection_Unit_Key = @ContainedCollectionKey
SET @Contains = 0

--Obtain successive parents
WHILE @Parent_Collection_Unit_Key IS NOT NULL
BEGIN
	SELECT @Parent_Collection_Unit_Key = C.Parent_Collection_Collection_Unit_Key
	FROM Collection C
	WHERE C.Collection_Unit_Key = @Parent_Collection_Unit_Key

	IF (@Parent_Collection_Unit_Key IS NULL) OR (@@RowCount = 0) --At top of storage hierarchy. No match found.
	BEGIN
		SET @Contains = 0
		BREAK
	END
	ELSE IF @Parent_Collection_Unit_Key = @ContainerCollectionKey --Match found.
	BEGIN
		SET @Contains = 1
		BREAK
	END
	ELSE IF EXISTS(SELECT * FROM @ContainedCollectionContainers WHERE Collection_Unit_Key = @Parent_Collection_Unit_Key)
	BEGIN --Recursive Store hierarchy found.
		SET @Contains = 1
		BREAK
	END
	ELSE --Log current Store
		INSERT INTO @ContainedCollectionContainers (Collection_Unit_Key) VALUES (@Parent_Collection_Unit_Key)

END


GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Collection_Contains_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Collection_Contains_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Collection_Contains_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Collection_Contains_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Collection_Contains_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Collection_Contains_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Collection_Contains_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Collection_Contains_Get TO [Dev - JNCC SQL]
END

GO