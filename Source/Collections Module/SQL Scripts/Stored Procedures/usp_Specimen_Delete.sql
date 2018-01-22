/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Specimen_Delete') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_Specimen_Delete]
GO

/*===========================================================================*\
  Description:	Delete a record from the Specimen table.
		Ensures the Domain masks of the containing itmes are also updated.
		The assumpton is that a specimen being deleted is not a 
		container for any other specimens. They must have been either 
		deleted or moved to another container themselves.

  Parameters:	@SpecimenKey

  Created:	July 2003

  Last revision information:
    $Revision: 13 $
    $Date: 20/03/14 9:34 $
    $Author: Christopherknight $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Specimen_Delete]
	@SpecimenKey char(16),
	@Timestamp timestamp = NULL
AS
	DECLARE @ExistingCollectionKey char(16),
		@ExistingContainerKey char(16),
		@SpecimenMask int,
		@SUTimestamp timestamp

	/*-------------------------------------------------------------*\
	| Initialise variables.						|
	\*-------------------------------------------------------------*/
	SELECT		@ExistingCollectionKey = S.Parent_Collection_Collection_Unit_Key, 
			@ExistingContainerKey = CU.Current_Container_Collection_Unit_Key
	FROM		Specimen_Unit S
	INNER JOIN 	Collection_Unit CU ON CU.Collection_Unit_Key = S.Collection_Unit_Key
	WHERE		S.Collection_Unit_Key = @SpecimenKey

	-- Retrieve the mask of the preferred concept for specimen.
	EXECUTE	usp_Get_Concept_Domain_Mask_From_Specimen @SpecimenKey, @SpecimenMask OUTPUT

	/*-------------------------------------------------------------*\
	| Do the table delete first. Or the containers will still have	|
	| the specimen and its mask!					|
	\*-------------------------------------------------------------*/

	BEGIN TRANSACTION
		/*-------------------------------------------------------------*\
		| Before continuing, we need to save the timestamp, because the |
		| update that follows this, could update the timestamp itself	|
		| causing the final delete to fail.				|
		\*-------------------------------------------------------------*/
		SELECT	@SUTimestamp = [Timestamp]
		FROM	Specimen_Unit
		WHERE	Collection_Unit_Key = @SpecimenKey
		/*---------------------------------------------------------------------------*\
		  The Determination table has a relationship to the Specimen_Unit table and 
		  the Specimen_Unit has a relationship to the Determination table (similarly
		  for the Taxon_Determination table). Hence, the Preferred_Determination and
		  Preferred_Taxon_Determination fields in the Specimen_Unit table must be
		  both made NULL before they can be deleted.
		\*---------------------------------------------------------------------------*/
		UPDATE	Specimen_Unit
		SET	Preferred_Determination_Key = NULL,
			Preferred_Taxon_Determination_Key = NULL
		WHERE	Collection_Unit_Key = @SpecimenKey

		/*---------------------------------------------------------------------------*\
          Delete Movement_Collections linked to Specimen being deleted.
		  Movement data for the specimen still exists - but allows specimen to
		  be deleted.
		\*---------------------------------------------------------------------------*/
		DELETE dbo.Movement_Collection_Unit
		WHERE Collection_Unit_Key = @SpecimenKey

		IF @@Error <> 0 GOTO RollbackAndExit

		-- Delete collection data associated with specimen.
		DELETE dbo.Collection_Unit_Check
		WHERE Collection_Unit_Key = @SpecimenKey
	
		IF @@Error <> 0 GOTO RollbackAndExit

		DELETE dbo.Collection_Unit_Data
		WHERE Collection_Unit_Key = @SpecimenKey
	
		IF @@Error <> 0 GOTO RollbackAndExit

		DELETE dbo.Collection_Unit_Enquiry
		WHERE Collection_Unit_Key = @SpecimenKey
	
		IF @@Error <> 0 GOTO RollbackAndExit

		DELETE dbo.Collection_Unit_Funding
		WHERE Collection_Unit_Key = @SpecimenKey
	
		IF @@Error <> 0 GOTO RollbackAndExit

		DELETE dbo.Collection_Unit_History
		WHERE Collection_Unit_Key = @SpecimenKey
	
		IF @@Error <> 0 GOTO RollbackAndExit

		DELETE dbo.Collection_Unit_Material
		WHERE Collection_Unit_Key = @SpecimenKey
	
		IF @@Error <> 0 GOTO RollbackAndExit

		DELETE dbo.Collection_Unit_Name
		WHERE Collection_Unit_Key = @SpecimenKey
	
		IF @@Error <> 0 GOTO RollbackAndExit

		DELETE dbo.Collection_Unit_Number
		WHERE Collection_Unit_Key = @SpecimenKey
	
		IF @@Error <> 0 GOTO RollbackAndExit

		DELETE dbo.Collection_Unit_Process
		WHERE Collection_Unit_Key = @SpecimenKey
	
		IF @@Error <> 0 GOTO RollbackAndExit

		DELETE dbo.Collection_Unit_Relation
		WHERE From_Collection_Unit_Key = @SpecimenKey 
		OR To_Collection_Unit_Key = @SpecimenKey
	
		IF @@Error <> 0 GOTO RollbackAndExit

		DELETE dbo.Collection_Unit_Task
		WHERE Collection_Unit_Key = @SpecimenKey
	
		IF @@Error <> 0 GOTO RollbackAndExit

		DELETE dbo.Collection_Unit_Valuation
		WHERE Collection_Unit_Key = @SpecimenKey
	
		IF @@Error <> 0 GOTO RollbackAndExit

		/*---------------------------------------------------------------------------*\
		  Specimen_Unit -> Determination -> Occurrence
		\*---------------------------------------------------------------------------*/
		-- Delete Determinations linked ONLY to Specimen being deleted.
		DELETE	Determination
		WHERE	Specimen_Collection_Unit_Key = @SpecimenKey
		AND	Occurrence_Key IS NULL

		-- Clear only reference to Specimen for Determinations also linked to Occurrence
		UPDATE	Determination
		SET	Specimen_Collection_Unit_Key = NULL
		WHERE	Specimen_Collection_Unit_Key = @SpecimenKey
		AND	Occurrence_Key IS NOT NULL

		IF @@Error <> 0 GOTO RollbackAndExit

		/*---------------------------------------------------------------------------*\
		  Specimen_Unit -> Taxon_Determination -> Taxon_Occurrence
		\*---------------------------------------------------------------------------*/
		-- Delete Taxon Determinations linked ONLY to Specimen being deleted.
		DELETE	Taxon_Determination
		WHERE	Specimen_Collection_Unit_Key = @SpecimenKey
		AND	Taxon_Occurrence_Key IS NULL

		-- Clear only reference to Specimen for Taxon Determinations also linked to Taxon Occurrence
		UPDATE 	Taxon_Determination
		SET	Specimen_Collection_Unit_Key = NULL
		WHERE	Specimen_Collection_Unit_Key = @SpecimenKey
		AND	Taxon_Occurrence_Key IS NOT NULL

		IF @@Error <> 0 GOTO RollbackAndExit

		/*-------------------------------------------------------------*\
		  Other tables.
		\*-------------------------------------------------------------*/
		DELETE	Specimen_Field_Data
		WHERE	Collection_Unit_Key = @SpecimenKey

		IF @@Error <> 0 GOTO RollbackAndExit

		DELETE	Specimen_Label		
		WHERE	Collection_Unit_Key = @SpecimenKey	

		IF @@Error <> 0 GOTO RollbackAndExit	

		DELETE	Specimen_Unit
		WHERE	Collection_Unit_Key = @SpecimenKey
		AND	(@Timestamp = @SUTimestamp OR (@Timestamp IS NULL))

		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Specimen_Unit WHERE Collection_Unit_Key = @SpecimenKey)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

		/*-------------------------------------------------------------*\
		 @Timestamp is null if proc called from a store related process. 
		 In that case , the Collection_Unit table will be handled by the 
		 store related process that called the proc, and not here.
		\*-------------------------------------------------------------*/
		IF @Timestamp IS NOT NULL
			-- If specimen is also a store, let the dedicated procedure deal with it
			IF EXISTS(SELECT * FROM Store WHERE Collection_Unit_Key = @SpecimenKey)
				EXECUTE	usp_Store_Delete @SpecimenKey
			ELSE
			-- Otherwise, just delete record from Collection_Unit table.
				DELETE	Collection_Unit
				WHERE	Collection_Unit_Key = @SpecimenKey

		IF @@Error <> 0 GOTO RollbackAndExit

		/*-------------------------------------------------------------*\
		  Now switch specimen bit OFF from container and collection.
		\*-------------------------------------------------------------*/
		-- Update the container mask
		EXECUTE	usp_CollectionUnit_Update_DomainMask @ExistingContainerKey, @SpecimenMask, 0
		-- Update the collection mask
		EXECUTE	usp_Collection_Update_DomainMask @ExistingCollectionKey, @SpecimenMask, 0

		IF @@Error <> 0 GOTO RollbackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Specimen_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Specimen_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Specimen_Delete TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimen_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimen_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimen_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Specimen_Delete TO [Dev - JNCC SQL]
END
GO