If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Multimedia_Select_ForAll]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Multimedia_Select_ForAll]
GO

CREATE PROCEDURE [dbo].[usp_Multimedia_Select_ForAll] 
@ParentKey CHAR(16),
@TableName VARCHAR(50)

AS
--  DESCRIPTION
--  Returns Multimedia data to the CollectionsBrowser for a given Table Name
--
--  PARAMETERS
--	NAME				DESCRIPTION
--	@ParentKey 			When specified, only the records associated with the parent key are returned
--  @TableName			Tells DB which table to return Multimedia data from
--
--
--  AUTHOR:     		Ben Collier, Dorset Software
--  CREATED:    		2003-08-27
--
SET NOCOUNT ON

SELECT SF.Source_Key AS Item_Key, SF.Title, SF.[File_Name]
FROM
SOURCE_JOIN SJ
	INNER JOIN
		SOURCE_FILE SF
	ON SJ.Source_key = SF.Source_Key 
		AND SJ.Record_Key = @ParentKey
		AND (UPPER(@TableName) = UPPER(Table_Name))
ORDER BY Item_Key

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Multimedia_Select_ForAll') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Multimedia_Select_ForAll'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Multimedia_Select_ForAll TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Multimedia_Select_ForAll TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Multimedia_Select_ForAll TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Multimedia_Select_ForAll TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Multimedia_Select_ForAll TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Multimedia_Select_ForAll TO [Dev - JNCC SQL]
END

GO