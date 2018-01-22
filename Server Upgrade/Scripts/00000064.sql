/*===========================================================================*\
  Description:	Updates for CCN 157

  Created:	October 2012

  Last revision information:
    $Revision: 2 $
    $Date: 5/10/12 11:19 $
    $Author: Alexanderpadley $

\*===========================================================================*/

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Specimen_Select') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_Specimen_Select]
GO

/*===========================================================================*\
  Description:	Returns data for the Specimen General frame.

  Parameters:	@Key		Specimen Collection Unit key
		@IsLifeScience	Whether we should be using the Taxon_Determination
				or Taxon tables.

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Specimen_Select]
	@Key char(16),
	@IsLifeScience bit
AS

SET NOCOUNT ON

	IF @IsLifeScience = 1
	BEGIN
		SELECT		CU.Collection_Unit_Key,
				ITN.Taxon_List_Item_Key AS Term_Key,
				CU.Current_Location_Code,
				CU.Usual_Location_Code,
				CU.Domain_Mask,
				SU.Dangerous,
				SU.Confidential,
				SU.Parent_Collection_Collection_Unit_Key AS Parent_Unit_Key,
				Coll.Item_Name AS ParentCollectionCollectionUnitName,
				CU.Current_Container_Collection_Unit_Key,
				CU.Usual_Container_Collection_Unit_Key,
				SCU.Item_Name + ISNULL(' - ' + CSC.Current_Location_Code, ISNULL(' - ' + CSC.Usual_Location_Code, '')) AS Current_Location_Name,
				SUS.Item_Name + ISNULL(' - ' + CSU.Current_Location_Code, ISNULL(' - ' + CSU.Usual_Location_Code, '')) AS Usual_Location_Name,
				CASE 	WHEN ITN.Preferred_Name_Italic = 1 THEN '<i>' + ITN.Preferred_Name + '</i>'
					ELSE ITN.Preferred_Name 
				END AS Item_Name,
				Specimen_Type_Concept_Key,
				CTType.PlainText AS Type,
				SU.Timestamp AS SUTimeStamp,
				CU.Timestamp AS CUTimeStamp,
				SU.Checked,
				SU.Internal_Use

		FROM		Collection_Unit AS CU
		INNER JOIN	Specimen_Unit AS SU ON SU.Collection_Unit_Key = CU.Collection_Unit_Key	
		INNER JOIN	Collection AS Coll On Coll.Collection_Unit_Key = SU.Parent_Collection_Collection_Unit_Key
		LEFT JOIN	Taxon_Determination AS TD ON TD.Taxon_Determination_Key = SU.Preferred_Taxon_Determination_Key
		LEFT JOIN	Index_Taxon_Name AS ITN ON ITN.Taxon_List_Item_Key = TD.Taxon_List_Item_Key
	
		INNER JOIN 	VW_ConceptTerm AS CTType ON Specimen_Type_Concept_Key = CTType.Concept_Key

		LEFT JOIN 	Store SCU ON SCU.Collection_Unit_Key = CU.Current_Container_Collection_Unit_Key
		LEFT JOIN	Collection_Unit CSC ON CSC.Collection_Unit_Key = SCU.Collection_Unit_Key 
		LEFT JOIN 	Store SUS ON SUS.Collection_Unit_Key = CU.Usual_Container_Collection_Unit_Key
		LEFT JOIN	Collection_Unit CSU ON CSU.Collection_Unit_Key = SUS.Collection_Unit_Key
	
		WHERE SU.Collection_Unit_Key = @Key
	END
	ELSE
	BEGIN
		SELECT		CU.Collection_Unit_Key,
				C.Term_Key,
				CU.Current_Location_Code,
				CU.Usual_Location_Code,
				CU.Domain_Mask,
				SU.Dangerous,
				SU.Confidential,
				SU.Parent_Collection_Collection_Unit_Key AS Parent_Unit_Key,
				Coll.Item_Name AS ParentCollectionCollectionUnitName,
				CU.Current_Container_Collection_Unit_Key,
				CU.Usual_Container_Collection_Unit_Key,
				SCU.Item_Name + ISNULL(' - ' + CSC.Current_Location_Code, ISNULL(' - ' + CSC.Usual_Location_Code, '')) AS Current_Location_Name,
				SUS.Item_Name + ISNULL(' - ' + CSU.Current_Location_Code, ISNULL(' - ' + CSU.Usual_Location_Code, '')) AS Usual_Location_Name,
				C.Published_Term	AS	Item_Name,
				Specimen_Type_Concept_Key,
				CTType.PlainText AS Type,
				SU.Timestamp AS SUTimeStamp,
				CU.Timestamp AS CUTimeStamp,
				SU.Checked,
				SU.Internal_Use
	
		FROM		Collection_Unit AS CU
		INNER JOIN	Specimen_Unit AS SU ON SU.Collection_Unit_Key = CU.Collection_Unit_Key
		INNER JOIN	Collection AS Coll On Coll.Collection_Unit_Key = SU.Parent_Collection_Collection_Unit_Key
		LEFT JOIN	Determination AS D ON D.Determination_Key = SU.Preferred_Determination_Key
		LEFT JOIN	Concept AS C ON C.Concept_Key = D.Concept_Key
		
		INNER JOIN 	VW_ConceptTerm AS CTType ON Specimen_Type_Concept_Key = CTType.Concept_Key
		LEFT JOIN 	Store SCU ON SCU.Collection_Unit_Key = CU.Current_Container_Collection_Unit_Key
		LEFT JOIN	Collection_Unit CSC ON CSC.Collection_Unit_Key = SCU.Collection_Unit_Key 
		LEFT JOIN 	Store SUS ON SUS.Collection_Unit_Key = CU.Usual_Container_Collection_Unit_Key
		LEFT JOIN	Collection_Unit CSU ON CSU.Collection_Unit_Key = SUS.Collection_Unit_Key
	
		WHERE SU.Collection_Unit_Key = @Key
	END

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Specimen_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Specimen_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Specimen_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimen_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimen_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Specimen_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimen_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Specimen_Select TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Specimen_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Specimen_Update]
GO

/*===========================================================================*\
  Description:	Updates a record in the Specimen table.
		Ensures the Domain mask of the specimen is also updated.

  Parameters:	@Key
		@ParentCollectionCollectionUnitKey
		@SpecimenTypeConceptKey
		@Confidential
		@Dangerous
		@LifeSciences 
		@SUTimestamp 
		@Checked 
		@CurrentContainerCollectionUnitKey
		@CurrentLocationCode
		@UsualContainerCollectionUnitKey 
		@UsualLocationCode
		@CUTimestamp 
		@SessionID 

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Specimen_Update]
	--for specimen_unit
	@Key char(16), 
	@ParentCollectionCollectionUnitKey char(16),
	@SpecimenTypeConceptKey char(16),
	@Confidential bit,
	@Dangerous bit,
	@LifeSciences bit,
	@SUTimestamp timestamp,
	@Checked bit,
	--for Collection_Unit
	@CurrentContainerCollectionUnitKey char(16),
	@CurrentLocationCode varchar(30),
	@UsualContainerCollectionUnitKey char(16),
	@UsualLocationCode varchar(30),
	@CUTimestamp timestamp,
	-- Both
	@SessionID char(16),
	@InternalUse bit
AS
	DECLARE @ExistingCollectionKey char(16),
		@ExistingContainerKey char(16),
		@SpecimenMask int,
		@ContainerMask int,
		@CollectionMask int

	/*-------------------------------------------------------------*\
	  Initialise variables.
	\*-------------------------------------------------------------*/
	SELECT		@ExistingCollectionKey = S.Parent_Collection_Collection_Unit_Key, 
			@ExistingContainerKey = CU.Current_Container_Collection_Unit_Key
	FROM		Specimen_Unit S
	INNER JOIN 	Collection_Unit CU ON CU.Collection_Unit_Key = S.Collection_Unit_Key
	WHERE		S.Collection_Unit_Key = @Key

	-- Retrieve the mask of the preferred concept for specimen.
	EXECUTE	usp_Get_Concept_Domain_Mask_From_Specimen @Key, @SpecimenMask OUTPUT

	/*-------------------------------------------------------------*\
	  If container changing, might need to update container mask.
	\*-------------------------------------------------------------*/
	IF @ExistingContainerKey <> @CurrentContainerCollectionUnitKey
	BEGIN
		-- A container may also be a specimen with its own concept. So get it.
		-- But if it is a storage, don't.
		SET @ContainerMask = 0
		IF EXISTS(SELECT * FROM Specimen_Unit WHERE Collection_Unit_Key = @ExistingContainerKey)
			EXECUTE	usp_Get_Concept_Domain_Mask_From_Specimen @ExistingContainerKey, @ContainerMask OUTPUT

		-- Work out the bit(s) to turn OFF
		SELECT		@ContainerMask = ~ @ContainerMask & ~ Sum(DISTINCT DM.Domain_Mask) & @SpecimenMask
		FROM		Collection_Unit CU
		INNER JOIN	Determination D 	WITH (INDEX (IX_Determination_Specimen)) 
							ON D.Specimen_Collection_Unit_Key = CU.Collection_Unit_Key
		INNER JOIN	Concept C 		WITH (INDEX (PK_Concept)) -- non-clustered
							ON C.Concept_Key = D.Concept_Key
		INNER JOIN	Concept_Group CG 	ON CG.Concept_Group_Key = C.Concept_Group_Key
		INNER JOIN	Local_Domain LD 	ON LD.Local_Domain_Key = CG.Local_Domain_Key
		INNER JOIN	Domain DM 		ON DM.Domain_Key = LD.Domain_Key
		WHERE		D.Preferred = 1
		AND		CU.Current_Container_Collection_Unit_Key = @ExistingContainerKey
		AND		CU.Collection_Unit_Key <> @Key
	END

	/*-------------------------------------------------------------*\
	  If collection changing, might need to update collection mask.
	\*-------------------------------------------------------------*/
	IF @ExistingCollectionKey <> @ParentCollectionCollectionUnitKey
	BEGIN
		-- Work out the bit(s) to turn OFF
		SELECT		@CollectionMask = ~ Sum(DISTINCT DM.Domain_Mask) & @SpecimenMask
		FROM		Specimen_Unit S
		INNER JOIN	Determination D 	WITH (INDEX (IX_Determination_Specimen)) 
							ON D.Specimen_Collection_Unit_Key = S.Collection_Unit_Key
		INNER JOIN	Concept C 		WITH (INDEX (PK_Concept))  -- non-clustered
							ON C.Concept_Key = D.Concept_Key
		INNER JOIN	Concept_Group CG 	ON CG.Concept_Group_Key = C.Concept_Group_Key
		INNER JOIN	Local_Domain LD 	ON LD.Local_Domain_Key = CG.Local_Domain_Key
		INNER JOIN	Domain DM 		ON DM.Domain_Key = LD.Domain_Key
		WHERE		D.Preferred = 1
		AND		S.Parent_Collection_Collection_Unit_Key = @ExistingCollectionKey
		AND		S.Collection_Unit_Key <> @Key
	END

	/*-------------------------------------------------------------*\
	  Do the table updates first. Or the containers will still have
	  the specimen and its mask!
	\*-------------------------------------------------------------*/
	BEGIN TRANSACTION	

		UPDATE	Specimen_Unit
		SET	Parent_Collection_Collection_Unit_Key = @ParentCollectionCollectionUnitKey,
			Specimen_Type_Concept_Key = @SpecimenTypeConceptKey,
			Confidential = @Confidential,
			Dangerous = @Dangerous,
			Life_Sciences = @LifeSciences,
			Changed_Session_ID = @SessionID,
			Checked = @Checked,
			Internal_Use = @InternalUse 
		WHERE	Collection_Unit_Key = @Key
		AND		[Timestamp] = @SUTimestamp
	
		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Specimen_Unit WHERE Collection_Unit_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END
	
		UPDATE	Collection_Unit
		SET	Current_Container_Collection_Unit_Key = @CurrentContainerCollectionUnitKey,
			Usual_Container_Collection_Unit_Key = @UsualContainerCollectionUnitKey,
			Current_Location_Code = @CurrentLocationCode,
			Usual_Location_Code = @UsualLocationCode,
			Changed_Session_ID = @SessionID
		WHERE	Collection_Unit_Key = @Key
		AND		[Timestamp] = @CUTimestamp
	
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Collection_Unit WHERE Collection_Unit_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END
	
		/*-------------------------------------------------------------*\
		  Now switch the bits OFF, if necessary.
		\*-------------------------------------------------------------*/
		IF (@ExistingContainerKey <> @CurrentContainerCollectionUnitKey) OR (@ExistingCollectionKey <> @ParentCollectionCollectionUnitKey)
		BEGIN
			-- Loop through all 32 bits and check which ones need to be switched OFF or left alone.
			-- From 10000000 00000000 00000000 00000000 down to 00000000 00000000 00000000 00000001
			DECLARE	@BitMask bigint  -- Use BIGINT, no unsigned int in SQL Server!!!!!
			SET @BitMask = 0x80000000
	
			-- 1 / 2 = 0, use this as stop condition
			WHILE @BitMask <> 0
			BEGIN
				-- If the bit is found ON for container, switch it OFF
				IF (@ExistingContainerKey <> @CurrentContainerCollectionUnitKey) AND (@ContainerMask & @BitMask = @BitMask)
					-- Update the container mask
					EXECUTE	usp_CollectionUnit_Update_DomainMask @ExistingContainerKey, @BitMask, 0
				
	
				-- If the bit is found ON for collection, switch it OFF
				IF (@ExistingCollectionKey <> @ParentCollectionCollectionUnitKey) AND (@CollectionMask & @BitMask = @BitMask)
					-- Update the collection mask
					EXECUTE	usp_Collection_Update_DomainMask @ExistingCollectionKey, @BitMask, 0
	
				-- Set mask for next single bit in line
				SET @BitMask = @BitMask / 2
			END
		END
	
		/*-------------------------------------------------------------*\
		  And ON. Straight forward all bits in this case.
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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Specimen_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Specimen_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Specimen_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimen_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimen_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimen_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Specimen_Update TO [Dev - JNCC SQL]
END
GO

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
		@LifeSciences 
		@Checked 
		@CurrentContainerCollectionUnitKey 
		@CurrentLocationCode
		@UsualContainerCollectionUnitKey
		@UsualLocationCode 
		@SessionID

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Specimen_Insert]
	--for specimen_unit
	@Key char(16) OUTPUT,
	@ExistingCollectionUnitKey char(16) = NULL,
	@ParentCollectionCollectionUnitKey char(16),
	@SpecimenTypeConceptKey char(16),
	@Confidential bit,
	@Dangerous bit,
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