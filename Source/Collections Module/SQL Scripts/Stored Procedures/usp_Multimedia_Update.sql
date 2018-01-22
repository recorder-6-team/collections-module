If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Multimedia_Update]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Multimedia_Update]
GO

/*===========================================================================*\
  Description:	Updates Multimedia data in the Source_File table.

  Parameters:	@Key	
		@RecordKey
		@Filename
		@Title
		@Preferred
		@TableName
		@Timestamp
		@RecordsAffected (output)

  Created:	October 2003

  Last revision information:
    $Revision: 9 $
    $Date: 3/02/09 9:54 $
    $Author: Pauldavies $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_Multimedia_Update] 
	@Key char(16),
	@RecordKey char(16),
	@Filename varchar(255),
	@Title varchar(100),
	@Preferred bit,
	@TableName varchar(50),
	@Timestamp timestamp,
	@RecordsAffected int OUTPUT
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	DECLARE @Error int

	BEGIN TRANSACTION
		-- If this new item is to be set as Preferred, then all the others for that record key
		-- and table name should not be set as preferred.
		IF @Preferred = 1
		BEGIN
			UPDATE 		Source_File
			SET 		Preferred = 0
			FROM		Source_File AS SF
			INNER JOIN	Source_Join AS SJ ON SJ.Source_Key = SF.Source_Key
			WHERE		SJ.Record_Key = @RecordKey
			AND		SJ.Table_Name = @TableName
			AND		SJ.Source_Key <> @Key	-- We don't want to change the record
								-- we are updating, or the timestamp
								-- will change as well.
			IF @@Error <> 0 GOTO RollbackAndExit
		END
		ELSE BEGIN
			-- Ensure one other stays preferred
			IF NOT EXISTS(
							SELECT 1
							FROM		Source_File AS SF
								INNER JOIN	Source_Join AS SJ ON SJ.Source_Key = SF.Source_Key
								WHERE		SJ.Record_Key = @RecordKey
								AND		SJ.Table_Name = @TableName
								AND		Preferred = 1
								AND SF.Source_Key<>@Key)
			BEGIN
				UPDATE Source_File
					SET Preferred=1
					WHERE Source_Key IN (
						SELECT TOP 1 SF.Source_Key FROM Source_File SF
						INNER JOIN	Source_Join AS SJ ON SJ.Source_Key = SF.Source_Key
						WHERE		SJ.Record_Key = @RecordKey
						AND		SJ.Table_Name = @TableName
						AND SF.Source_Key<>@Key)
				IF @@Error <> 0 GOTO RollbackAndExit
			END
		END

		-- Force item to preferred if the only one
		IF @Preferred = 0
			IF NOT EXISTS(
				SELECT 1
				FROM		Source_File AS SF
					INNER JOIN	Source_Join AS SJ ON SJ.Source_Key = SF.Source_Key
					WHERE		SJ.Record_Key = @RecordKey
					AND		SJ.Table_Name = @TableName
					AND		Preferred = 1
					AND 	SF.Source_Key<>@Key)
				SET @Preferred=1

		/*---------------------------*\
		  Actually updates the table
		\*---------------------------*/
		UPDATE 	Source_File
		SET	[File_Name] = @FileName,
			Title = @Title,
			Preferred = @Preferred
		WHERE	Source_Key = @Key
		AND		[Timestamp] = @Timestamp

		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Source_File WHERE Source_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO
			
/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Multimedia_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Multimedia_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Multimedia_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Multimedia_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Multimedia_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Multimedia_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Multimedia_Update TO [Dev - JNCC SQL]
END
GO