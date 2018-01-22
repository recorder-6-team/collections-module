/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_References_Delete_ForSources]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_References_Delete_ForSources]
GO

/*===========================================================================*\
  Description:	Deletes all records from the Source, Source_File and Source_Join
		tables given the Table_Name and Record_Key of the master record.

  Parameters:	@TableName
		@RecordKey

  Created:	December 2003

  Last revision information:
    $Revision: 4 $
    $Date: 6/05/04 14:26 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_References_Delete_ForSources]
	@TableName varchar(50),
	@RecordKey char(16)
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION
		/*-------------------------------------------------------------*\
		  Use temp table to store the keys of record to delete. Mind the 
		  collation!
		\*-------------------------------------------------------------*/
		DECLARE @Keys TABLE (
			Source_Join_Key char(16) COLLATE SQL_Latin1_General_CP1_CI_AS, 
			Source_Key char(16) COLLATE SQL_Latin1_General_CP1_CI_AS
		)

		INSERT @Keys
			SELECT 	Source_Join_Key, Source_Key
			FROM	Source_Join
			WHERE	Table_Name = @TableName
			AND	Record_Key = @RecordKey

		/*-------------------------------------------------------------*\
		  Delete from source tables. Order is important because of
		  referential integrity.
		\*-------------------------------------------------------------*/
		-- Source_Join first, external AND internal references
		DELETE 	Source_Join
		WHERE	Source_Join_Key IN (SELECT Source_Join_Key FROM @Keys)

		IF @@Error <> 0 GOTO RollbackAndExit

		-- Source_File, for external references
		DELETE	Source_File
		WHERE	Source_Key IN (SELECT Source_Key FROM @Keys)

		IF @@Error <> 0 GOTO RollbackAndExit

		-- Source table last
		DELETE	Source
		WHERE	Source_Key IN (SELECT Source_Key FROM @Keys)
		AND	Internal = 0

		IF @@Error <> 0 GOTO RollbackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_References_Delete_ForSources') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_References_Delete_ForSources'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_References_Delete_ForSources TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_References_Delete_ForSources TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_References_Delete_ForSources TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_References_Delete_ForSources TO [Dev - JNCC SQL]
END
GO