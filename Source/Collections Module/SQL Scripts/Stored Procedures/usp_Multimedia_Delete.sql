/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Multimedia_Delete]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Multimedia_Delete]
GO

/*===========================================================================*\
  Description:	Deletes records from the Source, Source_File and Source_Join
		tables given the Source_Key to delete.

  Parameters:	@Key		Source_Key
		@RecordKey	

  Created:	November 2003

  Last revision information:
    $Revision: 4 $
    $Date: 14/04/04 14:10 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Multimedia_Delete]
	@Key char(16),
	@RecordKey char(16)
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION
		DECLARE 
				@SourceJoinRecords int,
				@TableName VARCHAR(50)
		
		SELECT @TableName = Table_Name
		FROM Source_Join 
		WHERE Source_Key=@Key

		DELETE	Source_Join
		WHERE	Source_Key = @Key
		AND 	Record_Key = @RecordKey

		IF @@Error <> 0 GOTO RollbackAndExit

		/*---------------------------------------------------------*\
		  See if there are any other records that link to this
		  Sources record. If there are, we can't delete it. If there
		  aren't, it can go. Multiple Source_Join records pointing
		  to the same Source record can come about when duplicating
		  Valuations, for example.
		\*---------------------------------------------------------*/
	
		SELECT	@SourceJoinRecords = Count(Source_Join_Key)
		FROM	Source_Join 
		WHERE	Source_Key = @Key

		IF @SourceJoinRecords = 0 
		BEGIN
			DELETE 	Source_File
			WHERE	Source_Key = @Key

			IF @@Error <> 0 GOTO RollbackAndExit
	
			DELETE	Source
			WHERE 	Source_Key = @Key

			IF @@Error <> 0 GOTO RollbackAndExit
		END

		-- Ensure one other stays preferred
		IF NOT EXISTS(
						SELECT 1
						FROM		Source_File AS SF
							INNER JOIN	Source_Join AS SJ ON SJ.Source_Key = SF.Source_Key
							WHERE		SJ.Record_Key = @RecordKey
							AND		SJ.Table_Name = @TableName
							AND		Preferred = 1)
		BEGIN
			UPDATE Source_File
				SET Preferred=1
				WHERE Source_Key IN (
					SELECT TOP 1 SF.Source_Key FROM Source_File SF
					INNER JOIN	Source_Join AS SJ ON SJ.Source_Key = SF.Source_Key
					WHERE		SJ.Record_Key = @RecordKey
					AND		SJ.Table_Name = @TableName)
			IF @@Error <> 0 GOTO RollbackAndExit
		END


	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Multimedia_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Multimedia_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Multimedia_Delete TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Multimedia_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Multimedia_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Multimedia_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Multimedia_Delete TO [Dev - JNCC SQL]
END
GO