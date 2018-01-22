IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_DuplicateTerms_Merge]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
	DROP PROCEDURE [dbo].[usp_DuplicateTerms_Merge]
GO

/*===========================================================================*\
  Description:	
	Reassign records linked to @OldTermKey to @NewTermKey before deleting
	the @OldTermKey.

  Parameters:	
	@NewTermKey	Specify the key of the term to keep.
	@OldTermKey	Specify the key of the term to delete.

  Created:	
	January 2006

  Last revision information:
    $Revision: 3 $
    $Date: 26/08/11 15:22 $
    $Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_DuplicateTerms_Merge]
	@NewTermKey	CHAR(16),
	@OldTermKey	CHAR(16)
AS
	IF @NewTermKey <> @OldTermKey
	BEGIN
		SET NOCOUNT ON 

		-- Change over 
		UPDATE	Taxon_Dictionary_Term_Mapping
		SET	Term_Key = @NewTermKey
		WHERE	Term_Key = @OldTermKey

		UPDATE	Concept
		SET	Term_Key = @NewTermKey
		WHERE	Term_Key = @OldTermKey	

		/*========================================================*\
			Update term version records
		\*========================================================*/
		DECLARE @CurrentKey CHAR(16), @DuplicateKey CHAR(16), @UpdateTermKey BIT
		
		-- Update/delete term versions that use the old term key
		WHILE 1 = 1
		BEGIN			
			-- Get the first term version which uses the old term key
			SELECT @CurrentKey = Term_Version_Key
			FROM Term_Version
			WHERE Term_Key = @OldTermKey

			-- If there are no more term versions using the old key, there is nothing to do
			IF @@RowCount = 0 BREAK

			-- Flags whether the current term version should be updated or deleted
			SET @UpdateTermKey = 0

			-- Check if we have already got a term version with the new term key which has
			-- the same authority and version label as the current term version. 
			SELECT @DuplicateKey = tvduplicate.Term_Version_Key
			FROM Term_Version tvduplicate
			INNER JOIN Term_Version tvcurrent 
				ON ISNULL(tvduplicate.Version_Label, '') = ISNULL(tvcurrent.Version_Label, '')
				AND	ISNULL(tvduplicate.Author_And_Date, '') = ISNULL(tvcurrent.Author_And_Date, '')
			WHERE tvcurrent.Term_Version_Key = @CurrentKey AND tvduplicate.Term_Key = @NewTermKey

			IF @@RowCount > 0
			BEGIN
				-- If we do, only keep the term version if it is already mapped to a taxon
				-- version (VI 24145). Otherwise remove the term version
				IF EXISTS (
					SELECT * FROM Taxon_Dictionary_Term_Version_Mapping
					WHERE Term_Version_Key = @CurrentKey)
				BEGIN
					SET @UpdateTermKey = 1
				END
			END
			ELSE
			BEGIN
				-- If the term version will not duplicate an existing term version when updated,
				-- update it.
				SET @UpdateTermKey = 1
			END
			
			-- Update term version to new term key/delete term version as required. In either
			-- case, the current term version will not be considered in the next iteration of the
			-- while loop since it will either no longer have the old term key or it will have
			-- been deleted.
			IF @UpdateTermKey = 1
			BEGIN
				UPDATE Term_Version
				SET Term_Key = @NewTermKey
				WHERE Term_Version_Key = @CurrentKey
			END
			ELSE
			BEGIN
				UPDATE Concept	
				SET Term_Version_Key = @DuplicateKey
				WHERE Term_Version_Key = @CurrentKey

				UPDATE Thesaurus_Fact	
				SET Term_Version_Key = @DuplicateKey
				WHERE Term_Version_Key = @CurrentKey

				DELETE FROM Term_Version
				WHERE Term_Version_Key = @CurrentKey
			END		
		END

		-- And finally remove the unnecessary leftover term.
		DELETE	Term
		WHERE	Term_Key = @OldTermKey

		SET NOCOUNT ON
	END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_DuplicateTerms_Merge') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_DuplicateTerms_Merge'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_DuplicateTerms_Merge TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_DuplicateTerms_Merge TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_DuplicateTerms_Merge TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_DuplicateTerms_Merge TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_DuplicateTerms_Merge TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_DuplicateTerms_Merge TO [Dev - JNCC SQL]
END
