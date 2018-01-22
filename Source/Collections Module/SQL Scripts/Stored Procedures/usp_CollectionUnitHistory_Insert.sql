/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_CollectionUnitHistory_Insert]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_CollectionUnitHistory_Insert]
GO
 
/*===========================================================================*\
  Description:	Inserts a record into the CollectionUnitHistory table.

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

  Created:	November 2003

  Last revision information:
    $Revision: 2 $
    $Date: 5/12/03 17:16 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_CollectionUnitHistory_Insert]
	@Key char(16) OUTPUT,
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
	@SessionID char(16)
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	EXECUTE spNextKey 'Collection_Unit_History', @Key OUTPUT

	BEGIN TRANSACTION

		INSERT INTO Collection_Unit_History (
			Collection_Unit_History_Key, 
			Collection_Unit_Key,
			Source_Name_Key,
			From_Vague_Date_Start,
			From_Vague_Date_End,
			From_Vague_Date_Type,
			To_Vague_Date_Start,
			To_Vague_Date_End,
			To_Vague_Date_Type,
			Item_Name,
			Comment,
			Entered_Session_ID			
		) VALUES (
			@Key,
			@CollectionUnitKey,
			@SourceNameKey,
			@FromVagueDateStart,
			@FromVagueDateEnd,
			@FromVagueDateType,
			@ToVagueDateStart,
			@ToVagueDateEnd,
			@ToVagueDateType,
			@ItemName,
			@Comment,
			@SessionID	
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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_CollectionUnitHistory_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_CollectionUnitHistory_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitHistory_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_CollectionUnitHistory_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_CollectionUnitHistory_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_CollectionUnitHistory_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitHistory_Insert TO [Dev - JNCC SQL]
END
GO