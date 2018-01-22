/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_SourceJoin_Insert]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_SourceJoin_Insert]
GO

/*===========================================================================*\
  Description:	Inserts a record into the Source_Join table.

  Parameters:	@Key 
		@TableName 
		@RecordKey 
		@SourceKey 
		@Original
		@SessionID 

  Created:	November 2003

  Last revision information:
    $Revision: 3 $
    $Date: 5/12/03 17:44 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SourceJoin_Insert] 
	@Key CHAR(16) OUTPUT,
	@TableName varchar(50),
	@RecordKey char(16),
	@SourceKey char(16),
	@Original bit,
	@SessionID char(16)
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION

		/*-------------------------------------------------------------*\
		  Insert into Source_Join.
		\*-------------------------------------------------------------*/
		EXECUTE spNextKey 'Source_Join', @Key OUTPUT
		
		INSERT INTO Source_Join (
			Source_Join_Key, Table_Name, Record_Key, Source_Key,
			Original, Entered_Session_ID, System_Supplied_Data
		) VALUES (
			@Key, @TableName, @RecordKey, @SourceKey,
			@Original, @SessionID, 0
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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SourceJoin_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SourceJoin_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SourceJoin_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SourceJoin_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SourceJoin_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SourceJoin_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SourceJoin_Insert TO [Dev - JNCC SQL]
END
GO