/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_DomainHyperlink_Insert]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_DomainHyperlink_Insert]
GO

/*===========================================================================*\
  Description:	Inserts a record into the Domain_Hyperlink table.

  Parameters:	@Key 
		@ItemName 
		@ImageFile 
		@URL 
		@UseConceptKey 
		@WordSeparator 
		@LocalDomainKey 
		@SessionID 
		@SystemSuppliedData 
		@Timestamp 
	
  Created:	January 2004

  Last revision information:
    $Revision: 1 $
    $Date: 29/01/04 17:02 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_DomainHyperlink_Insert]
	@Key char(16) OUTPUT,
	@ItemName varchar(100), 
	@ImageFile varchar(255) = NULL,
	@URL varchar(255),
	@UseConceptKey bit = NULL, 
	@WordSeparator varchar(5) = NULL,
	@LocalDomainKey char(16),
	@SessionID char(16),
	@SystemSuppliedData bit = NULL
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	EXECUTE spNextKey 'Domain_Hyperlink', @Key OUTPUT

	BEGIN TRANSACTION

		/*-------------------------------------------------------------*\
		  Insert in Domain_Hyperlink table.
		\*-------------------------------------------------------------*/
		INSERT INTO Domain_Hyperlink (
			Domain_Hyperlink_Key,
			Item_Name,
			Image_File,
			URL,
			Use_Concept_Key,
			Word_Separator,
			Local_Domain_Key,
			Entered_Session_ID,
			System_Supplied_Data
		) VALUES (
			@Key,
			@ItemName,
			@ImageFile,
			@URL,
			@UseConceptKey,
			@WordSeparator,
			@LocalDomainKey,
			@SessionID,
			IsNull(@SystemSuppliedData, 0)
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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_DomainHyperlink_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_DomainHyperlink_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_DomainHyperlink_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_DomainHyperlink_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_DomainHyperlink_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_DomainHyperlink_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_DomainHyperlink_Insert TO [Dev - JNCC SQL]
END
GO