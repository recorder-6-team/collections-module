/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_TermVersion_Delete]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_TermVersion_Delete]
GO

/*===========================================================================*\
  Description:	Delete a record from the Term_Version table.

  Parameters:	@Key
		@Timestamp

  Created:	December 2003

  Last revision information:
    $Revision: 4 $
    $Date: 3/02/09 10:48 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_TermVersion_Delete]
	@Key char(16),
	@Timestamp timestamp = NULL,
	@UserID char(16) = NULL,
	@SyncTaxonDict bit = 0
AS
	SET NOCOUNT OFF

	BEGIN TRANSACTION

		/*============================================================*\
		  See if the user wants any associated taxon dictionary
		  records be deleted with the Term Version.
		\*============================================================*/
		IF @SyncTaxonDict = 1 
		BEGIN
			/*--------------------------------------------------------*\
			  Check that the Taxon_Version table exists before
			  attempting any of this deletion. 
			\*--------------------------------------------------------*/
			IF EXISTS (SELECT *
		   				FROM	SysObjects 
						WHERE	Id = Object_Id(N'[dbo].[Taxon_Version]')
						AND		Type = 'U')
			BEGIN
				DECLARE @TaxonVersionKey char(16)

				SELECT 	@TaxonVersionKey = Taxon_Version_Key
				FROM	Taxon_Dictionary_Term_Version_Mapping
				WHERE	Term_Version_Key = @Key

				IF @@Error <> 0 GOTO RollbackAndExit

				DELETE	Taxon_Dictionary_Term_Version_Mapping	
				WHERE	Taxon_Version_Key = @TaxonVersionKey
				AND		Term_Version_Key = @Key

				IF @@Error <> 0 GOTO RollbackAndExit

				DELETE	Taxon_Version_Key
				WHERE	Taxon_Version_Key_1 = @TaxonVersionKey
				OR		Taxon_Version_Key_2 = @TaxonVersionKey

				IF @@Error <> 0 GOTO RollbackAndExit

				DELETE	Taxon_Common_Name
				WHERE	Taxon_Version_Key = @TaxonVersionKey

				IF @@Error <> 0 GOTO RollbackAndExit

				DELETE	Taxon_Taxon_Association
				WHERE	Taxon_Version_Key_1 = @TaxonVersionKey
				OR 		Taxon_Version_Key_2 = @TaxonVersionKey

				IF @@Error <> 0 GOTO RollbackAndExit

				DELETE	Taxon_Biotope_Association
				WHERE	Taxon_Version_Key = @TaxonVersionKey

				IF @@Error <> 0 GOTO RollbackAndExit

				UPDATE	Taxon_Version
				SET		Attribute = NULL,
						Authority = NULL,
						Date_From = NULL,
						Date_To = NULL,
						Comment = NULL,
						Validation_Level = 0,
						Uk_Native = 0,
						Quality = NULL,
						Source_Key = NULL,
						Changed_By = @UserID,
						Changed_Date = GetDate()
				WHERE	Taxon_Version_Key = @TaxonVersionKey
			END
		END

		-- Delete record from Term_Version table.
		DELETE	Term_Version
		WHERE	Term_Version_Key = @Key
		AND		([Timestamp] = @Timestamp OR (@Timestamp IS NULL))
	
		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Term_Version WHERE Term_Version_Key = @Key)
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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TermVersion_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_TermVersion_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_TermVersion_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_TermVersion_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_TermVersion_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_TermVersion_Delete TO [Dev - JNCC SQL]
END
GO