/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_CollectionUnitHistory_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_CollectionUnitHistory_Update]
GO
 
/*===========================================================================*\
  Description:	Updates a record into the CollectionUnitHistory table.

  Parameters:	@Key 
		@CollectionUnitKey 
		@SourceNameKey 
		@FromVagueDateStart 
		@FromVagueDateEnd 
		@FromVagueDateType 
		@ToVagueDateStart 
		@ToVagueDateEnd 
		@ToVagueDateType
		@ItemName 
		@Comment 
		@SessionID 
		@Timestamp

  Created:	November 2003

  Last revision information:
    $Revision: 5 $
    $Date: 2/02/09 17:20 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_CollectionUnitHistory_Update]
	@Key char(16),
	@CollectionUnitKey char(16),
	@SourceNameKey char(16),
	@FromVagueDateStart int,
	@FromVagueDateEnd int,
	@FromVagueDateType varchar(2),
	@ToVagueDateStart int,
	@ToVagueDateEnd int,
	@ToVagueDateType varchar(2),
	@ItemName varchar(100),
	@Comment text,
	@SessionID char(16),
	@Timestamp timestamp
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION

		UPDATE 	Collection_Unit_History 
		SET	Collection_Unit_Key = @CollectionUnitKey,
			Source_Name_Key = @SourceNameKey,
			From_Vague_Date_Start = @FromVagueDateStart,
			From_Vague_Date_End = @FromVagueDateEnd,
			From_Vague_Date_Type = @FromVagueDateType,
			To_Vague_Date_Start = @ToVagueDateStart,
			To_Vague_Date_End = @ToVagueDateEnd,
			To_Vague_Date_Type = @ToVagueDateType,
			Item_Name = @ItemName,
			Comment = @Comment,
			Changed_Session_ID = @SessionID			
		
		WHERE	Collection_Unit_History_Key = @Key
		AND		[Timestamp] = @Timestamp

		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Collection_Unit_History WHERE Collection_Unit_History_Key = @Key)
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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_CollectionUnitHistory_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_CollectionUnitHistory_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitHistory_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_CollectionUnitHistory_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_CollectionUnitHistory_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_CollectionUnitHistory_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitHistory_Update TO [Dev - JNCC SQL]
END
GO