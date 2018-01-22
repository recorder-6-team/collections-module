/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Specimen_Insert]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Specimen_Insert]
GO

/*===========================================================================*\
  Description:	Inserts a record in the Specimen table and a preferred 
		determination in the Determination table.
		Ensures the Domain mask of the specimen is also updated.

  Parameters:	@Key 
		@ParentCollectionCollectionUnitKey 
		@SpecimenTypeConceptKey 
		@Confidential 
		@Dangerous
		@PublishToWeb
		@LifeSciences 
		@Checked 
		@CurrentContainerCollectionUnitKey 
		@CurrentLocationCode
		@UsualContainerCollectionUnitKey
		@UsualLocationCode 
		@SessionID

  Created:	July 2003

  Last revision information:
    $Revision: 12 $
    $Date: 8/05/14 13:44 $
    $Author: Brynhorsfieldschonhut $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Specimen_Insert]
	--for specimen_unit
	@Key char(16) OUTPUT,
	@ExistingCollectionUnitKey char(16) = NULL,
	@ParentCollectionCollectionUnitKey char(16),
	@SpecimenTypeConceptKey char(16),
	@Confidential bit,
	@Dangerous bit,
	@PublishToWeb bit = 0,
	@LifeSciences bit,
	@Checked bit,
	--for Collection_Unit
	@CurrentContainerCollectionUnitKey char(16),
	@CurrentLocationCode varchar(30),
	@UsualContainerCollectionUnitKey char(16),
	@UsualLocationCode varchar(30),
	-- Both
	@SessionID char(16),
	@InternalUse bit
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF
	SET XACT_ABORT ON
	
	BEGIN TRANSACTION
	
		/*-------------------------------------------------------------*\
		  Get the concept mask.
		\*-------------------------------------------------------------*/
		DECLARE @SpecimenMask int
		EXECUTE	usp_Get_Concept_Domain_Mask @SpecimenTypeConceptKey, @SpecimenMask OUTPUT

		IF @SpecimenMask IS NULL
			SET @SpecimenMask=0

		IF @ExistingCollectionUnitKey IS NULL BEGIN
			/*-------------------------------------------------------------*\
			  Get a new key.
			\*-------------------------------------------------------------*/
			EXECUTE spNextKey 'Collection_Unit', @Key OUTPUT
			/*-------------------------------------------------------------*\
			  Insert in Collection_Unit first.
			\*-------------------------------------------------------------*/
			INSERT INTO Collection_Unit (
				Collection_Unit_Key, 
				Current_Container_Collection_Unit_Key, 
				Current_Location_Code,
				Usual_Container_Collection_Unit_Key, 
				Usual_Location_Code, 
				Domain_Mask,
				Entered_Session_ID
			) VALUES (
				@Key, 
				@CurrentContainerCollectionUnitKey, 
				@CurrentLocationCode, 
				@UsualContainerCollectionUnitKey, 
				@UsualLocationCode, 
				@SpecimenMask, -- Domain_Mask is empty for new Collections
				@SessionID
			)
			IF @@Error <> 0 GOTO RollbackAndExit		
		END
		ELSE
			SET @Key = @ExistingCollectionUnitKey

		INSERT INTO Specimen_Unit (
			Collection_Unit_Key, 
			Parent_Collection_Collection_Unit_Key,
			Specimen_Type_Concept_Key,
			Confidential,
			Dangerous,
			Publish_To_Web, 
			Life_Sciences,
			Entered_Session_ID,
			Checked,
			Internal_Use)
		 VALUES (
			@Key, 
			@ParentCollectionCollectionUnitKey,
			@SpecimenTypeConceptKey,
			IsNull(@Confidential, 0),
			IsNull(@Dangerous, 0),
			IsNull(@PublishToWeb, 0),
			@LifeSciences,
			@SessionID,
			@Checked,
			@InternalUse 
		)
	
		IF @@Error <> 0 GOTO RollbackAndExit

		--The following line removed by Polly Shaw
		--EXECUTE usp_Determination_Insert @NewDeterminationKey, @NewSpecimenKey, @ConceptKey
	
		/*-------------------------------------------------------------*\
		  And switch bits ON.
		\*-------------------------------------------------------------*/
		-- Update the *new* container mask
		EXECUTE	usp_CollectionUnit_Update_DomainMask @CurrentContainerCollectionUnitKey, @SpecimenMask, 1
		-- Update the *new* collection mask
		EXECUTE	usp_Collection_Update_DomainMask @ParentCollectionCollectionUnitKey, @SpecimenMask, 1

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Specimen_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Specimen_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Specimen_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimen_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimen_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimen_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Specimen_Insert TO [Dev - JNCC SQL]
END
GO