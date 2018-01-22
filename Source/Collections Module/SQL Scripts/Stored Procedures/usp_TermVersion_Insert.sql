/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_TermVersion_Insert]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_TermVersion_Insert]
GO

/*===========================================================================*\
  Description:	Inserts a record into the Term_Version table

  Parameters:	@Key (Term_Version_Key)	OUTPUT
		@ConceptKey
		@VersionLabel
		@AuthorAndDate
		@SessionID
		@SyncTaxonDict
		@SystemSuppliedData
		
  Created:	December 2003

  Last revision information:
    $Revision: 8 $
    $Date: 26/08/11 14:53 $
    $Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_TermVersion_Insert]
	@Key char(16) OUTPUT,
	@ConceptKey char(16),
	@TermKey char(16) = null,
	@VersionLabel varchar(100),
	@AuthorAndDate varchar(100),
	@SessionID char(16),
	@SyncTaxonDict bit = 0,
	@SystemSuppliedData bit = NULL
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION

		IF @TermKey IS NULL
		BEGIN
			SELECT 	@TermKey = Term_Key
			FROM	Concept
			WHERE	Concept_Key = @ConceptKey
		END

		SELECT @Key = NULL

		-- Use existing term version if possible
		SELECT @Key = Term_Version_Key 
		FROM Term_Version
		WHERE (Version_Label = @VersionLabel OR (Version_Label IS NULL AND @VersionLabel IS NULL))
		AND (Author_And_Date = @AuthorAndDate OR (Author_And_Date IS NULL AND @AuthorAndDate IS NULL))
		AND Term_Key = @TermKey

		IF @Key IS NULL
		BEGIN
			EXECUTE spNextKey 'Term_Version', @Key OUTPUT
			
			INSERT INTO Term_Version (
				Term_Version_Key,
				Term_Key,
				Version_Label,
				Author_And_Date,
				Entered_Session_ID,
				System_Supplied_Data			
			) VALUES (
				@Key, 	
				@TermKey,
				@VersionLabel,
				@AuthorAndDate,
				@SessionID,
				IsNull(@SystemSuppliedData, 0)
			)
			IF @@Error <> 0 GOTO RollbackAndExit
		END

		/*-------------------------------------------------------------*\
		  Update Concept to point to new version
		\*-------------------------------------------------------------*/
		UPDATE Concept
		SET Term_Version_Key=@Key
		WHERE Concept_Key=@ConceptKey

		IF @@Error <> 0 GOTO RollbackAndExit

		/*-------------------------------------------------------------*\
			Keep mapping for version, so that deletion synching can work
		\*-------------------------------------------------------------*/
		IF (@SyncTaxonDict = 1) AND EXISTS (SELECT *	FROM	SysObjects 
						WHERE	Id = Object_Id(N'[dbo].[Taxon_Version]') AND Type = 'U')
		BEGIN
			DECLARE @TaxonVersionKey CHAR(16)
			SELECT @TaxonVersionKey=TLI.Taxon_Version_Key
			FROM Concept C
			INNER JOIN Taxon_Dictionary_Concept_Mapping CM ON CM.Concept_Key=C.Concept_Key
			INNER JOIN Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key=CM.Taxon_List_Item_Key

			IF NOT @TaxonVersionKey IS NULL
			BEGIN

				IF EXISTS(SELECT 1 FROM Taxon_Dictionary_Term_Version_Mapping WHERE Taxon_Version_Key=@TaxonVersionKey)
					UPDATE Taxon_Dictionary_Term_Version_Mapping 
					SET Term_Version_Key=@Key
					WHERE Taxon_Version_Key=@TaxonVersionKey
				ELSE
				INSERT INTO Taxon_Dictionary_Term_Version_Mapping VALUES(@TaxonVersionKey, @Key, NULL)

				IF @@Error <> 0 GOTO RollbackAndExit

			END

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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TermVersion_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_TermVersion_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_TermVersion_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_TermVersion_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_TermVersion_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_TermVersion_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_TermVersion_Insert TO [Dev - JNCC SQL]
END
GO