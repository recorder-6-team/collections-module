/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_DomainHyperlink_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_DomainHyperlink_Update]
GO

/*===========================================================================*\
  Description:	Updates a record in the Domain_Hyperlink table.

  Parameters:	@Key 
		@ItemName 
		@ImageFile 
		@URL 
		@UseConceptKey 
		@WordSeparator 
		@LocalDomainKey 
		@SessionID 
		@Timestamp 

  Created:	January 2004

  Last revision information:
    $Revision: 3 $
    $Date: 3/02/09 9:24 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_DomainHyperlink_Update]
	@Key char(16),
	@ItemName varchar(100), 
	@ImageFile varchar(255),
	@URL varchar(255),
	@UseConceptKey bit, 
	@WordSeparator varchar(5),
	@LocalDomainKey char(16),
	@SessionID char(16), 
	@Timestamp timestamp
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION
		
		UPDATE 	Domain_Hyperlink
		SET 	Item_Name = @ItemName,
			Image_File = @ImageFile,
			URL = @URL,
			Use_Concept_Key = @UseConceptKey,
			Word_Separator = @WordSeparator,
			Local_Domain_Key = @LocalDomainKey,
			Changed_Session_ID = @SessionID
		WHERE	Domain_Hyperlink_Key = @Key
		AND		[Timestamp] = @Timestamp

		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Domain_Hyperlink WHERE Domain_Hyperlink_Key = @Key)
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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_DomainHyperlink_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_DomainHyperlink_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_DomainHyperlink_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_DomainHyperlink_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_DomainHyperlink_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_DomainHyperlink_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_DomainHyperlink_Update TO [Dev - JNCC SQL]
END
GO