/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Multimedia_Insert]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Multimedia_Insert]
GO

/*===========================================================================*\
  Description:	Inserts Multimedia data into the Source_Join and Source_File 
		tables.

  Parameters:	@Key	
		@RecordKey
		@Filename
		@Title
		@Preferred
		@SessionID
		@TableName
		@RecordKey

  Created:	October 2003

  Last revision information:
    $Revision: 6 $
    $Date: 14/04/04 14:10 $
    $Author: Johnvanbreda $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_Multimedia_Insert] 
	@Key CHAR(16) OUTPUT,
	@RecordKey char(16),
	@Filename varchar(255),
	@Title varchar(100) = NULL,
	@Preferred bit,
	@SessionID char(16),
	@TableName varchar(50)
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	-- Force item to preferred if the only one
	IF @Preferred = 0
		IF NOT EXISTS(
			SELECT 1
			FROM		Source_File AS SF
				INNER JOIN	Source_Join AS SJ ON SJ.Source_Key = SF.Source_Key
				WHERE		SJ.Record_Key = @RecordKey
				AND		SJ.Table_Name = @TableName
				AND		Preferred = 1)
			SET @Preferred=1


	BEGIN TRANSACTION
		-- If this new item is to be set as Preferred, then all the others for that record key
		-- and table name should not be set as preferred.
		IF @Preferred = 1
			UPDATE 		Source_File
			SET 		Preferred = 0
			FROM		Source_File AS SF
			INNER JOIN	Source_Join AS SJ ON SJ.Source_Key = SF.Source_Key
			WHERE		SJ.Record_Key = @RecordKey
			AND		SJ.Table_Name = @TableName
			AND		Preferred = 1

		/*-------------------------------------------------------------*\
		  Insert in Source.
		\*-------------------------------------------------------------*/
		EXECUTE spNextKey 'Source', @Key OUTPUT		

		INSERT INTO Source (
			Source_Key, Internal
		) VALUES (
			@Key, 0
		)
		IF @@Error <> 0 GOTO RollbackAndExit

		/*-------------------------------------------------------------*\
		  Insert in Source_File.
		\*-------------------------------------------------------------*/
		INSERT INTO Source_File (
			Source_Key, [File_Name], Title, Preferred
		) VALUES (
			@Key, @Filename, @Title, @Preferred
		)
		IF @@Error <> 0 GOTO RollbackAndExit

		/*-------------------------------------------------------------*\
		  Generate new key and insert record into Source_Join.
		\*-------------------------------------------------------------*/
		DECLARE @SourceJoinKey char(16)
		EXECUTE spNextKey 'Source_Join', @SourceJoinKey OUTPUT
		
		INSERT INTO Source_Join (
			Source_Join_Key, Table_Name, Record_Key, Source_Key,
			Entered_Session_ID, System_Supplied_Data
		) VALUES (
			@SourceJoinKey, @TableName, @RecordKey, @Key,
			@SessionID, 0
		)
		IF @@Error <> 0 GOTO RollbackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO
			
/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Multimedia_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Multimedia_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Multimedia_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Multimedia_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Multimedia_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Multimedia_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Multimedia_Insert TO [Dev - JNCC SQL]
END
GO