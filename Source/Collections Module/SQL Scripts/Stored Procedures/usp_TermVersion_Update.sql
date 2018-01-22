/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_TermVersion_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_TermVersion_Update]
GO

/*===========================================================================*\
  Description:	Updates a record in the Term_Version table. However, if
				(@VersionLabel IS NULL AND @AuthorAndDate IS NULL) OR 
				(@VersionLabel = '' AND @AuthorAndDate = '') then
				the Term_Version record will be deleted. This method is used
				to delete Term Versions, rather than 'usp_TermVersion_Delete'
				because it is more intelligent using this method, than brute
				force deletion.

  Parameters:	@Key (Term_Version_Key)
				@ConceptKey 
				@VersionLabel
				@AuthorAndDate
				@SessionID 
				@Timestamp 
				@SyncTaxonDict
				@RecordsAffected OUTPUT

  Created:	December 2003

  Last revision information:
    $Revision: 13 $
    $Date: 26/08/11 14:53 $
    $Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_TermVersion_Update]
	@Key char(16),
	@ConceptKey char(16),
	@TermKey char(16) = NULL,
	@VersionLabel varchar(100),
	@AuthorAndDate varchar(100),
	@SessionID char(16),
	@Timestamp timestamp = NULL,
	@SyncTaxonDict bit = 0,
	@RecordsAffected int =1 OUTPUT
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION

		DECLARE @Error int

		IF @TermKey IS NULL
		BEGIN
			SELECT 	@TermKey = Term_Key
			FROM	Concept
			WHERE	Concept_Key = @ConceptKey
		END

		IF @@Error <> 0 GOTO RollbackAndExit

		DECLARE @NewKey CHAR(16)
		SET @NewKey = NULL

		SELECT @NewKey = Term_Version_Key 
		FROM Term_Version
		WHERE (Version_Label = @VersionLabel OR (Version_Label IS NULL AND @VersionLabel IS NULL))
		AND (Author_And_Date = @AuthorAndDate OR (Author_And_Date IS NULL AND @AuthorAndDate IS NULL))
		AND Term_Key = @TermKey
		
		IF @NewKey IS NULL
		BEGIN
			SET @NewKey = @Key

			UPDATE 	Term_Version
			SET 	Term_Key = @TermKey,
					Version_Label = @VersionLabel,
					Author_And_Date = @AuthorAndDate,
					Changed_Session_ID = @SessionID
			WHERE	Term_Version_Key = @Key
			AND		([Timestamp] = @Timestamp OR @Timestamp IS NULL)

			SELECT	@RecordsAffected = @@RowCount,
				@Error = @@Error

			IF @Error <> 0 GOTO RollbackAndExit 

			IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Term_Version WHERE Term_Version_Key = @Key)
			BEGIN
				RAISERROR('Record updated by another user', 16, 1)
				GOTO RollbackAndExit
			END
		END		

		/*-------------------------------------------------------------*\
		  Update Concept to point to new version
		\*-------------------------------------------------------------*/
		UPDATE 	Concept
		SET 	Term_Version_Key = @NewKey
		WHERE 	Concept_Key = @ConceptKey

		SELECT	@RecordsAffected = @@RowCount,
			@Error = @@Error

		IF @Error <> 0 GOTO RollbackAndExit 

		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Term_Version WHERE Term_Version_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

		IF @NewKey <> @Key
		BEGIN
			DELETE FROM Term_Version WHERE Term_Version_Key = @Key
		END

		IF @@Error <> 0 GOTO RollbackAndExit	
		
	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TermVersion_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_TermVersion_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_TermVersion_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_TermVersion_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_TermVersion_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_TermVersion_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_TermVersion_Update TO [Dev - JNCC SQL]
END
GO