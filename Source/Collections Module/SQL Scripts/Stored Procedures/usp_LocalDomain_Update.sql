/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_LocalDomain_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_LocalDomain_Update]
GO

/*===========================================================================*\
  Description:	Updates a record in the Local Domain table

  Parameters:	@Key 
		@ItemName 
		@LanguageKey 
		@ConceptGroupLabel
		@SessionID 
		@Timestamp 

  Created:	November 2003

  Last revision information:
    $Revision: 6 $
    $Date: 31/08/11 10:55 $
    $Author: Jamesbichard $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_LocalDomain_Update]
	@Key char(16),
	@ItemName varchar(100),
	@LanguageKey varchar(4),
	@ConceptGroupLabel varchar(50),
	@TermGeneratorKey char(16),
	@UpdateDescendents bit,
	@SessionID char(16),
	@Timestamp timestamp
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	IF @UpdateDescendents IS NULL SET @UpdateDescendents = 1

	IF @UpdateDescendents = 0
	BEGIN
		DISABLE TRIGGER tr_LocalDomain_PublishedTermFields ON Local_Domain
	END

	BEGIN TRANSACTION
		
		UPDATE 	Local_Domain
		SET 	Item_Name = @ItemName, 
			Language_Key = @LanguageKey,
			Concept_Group_Label = @ConceptGroupLabel,
			Changed_Session_ID = @SessionID,
			Term_Generator_Key = @TermGeneratorKey
		WHERE	Local_Domain_Key = @Key
		AND		[Timestamp] = @Timestamp

		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Local_Domain WHERE Local_Domain_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END;

		ENABLE TRIGGER tr_LocalDomain_PublishedTermFields ON Local_Domain
	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ENABLE TRIGGER tr_LocalDomain_PublishedTermFields ON Local_Domain
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_LocalDomain_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_LocalDomain_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_LocalDomain_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_LocalDomain_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_LocalDomain_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_LocalDomain_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_LocalDomain_Update TO [Dev - JNCC SQL]
END
GO