SET ANSI_NULLS ON
SET QUOTED_IDENTIFIER ON
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_AllSynonyms_Select_ForConcept]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_AllSynonyms_Select_ForConcept]
GO

/*===========================================================================*\
  Description:	Returns List Synonyms

  Parameters:	@Key	Concept_Key

  Created:	December 2003

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_AllSynonyms_Select_ForConcept]
	@Key char(16)
AS

SET NOCOUNT ON

	/*=============================*\
	  Get all known synonyms.
	\*=============================*/
	SELECT 		CAllKnownSynonyms.Concept_Key AS Item_Key,
			IsNull(T.Item_Name + ' ' + TV.Author_And_Date + ' (' + CG.Item_Name + ')', 
				T.Item_Name + ' (' + CG.Item_Name + ')') 
			AS Item_Name,
			CAllKnownSynonyms.Concept_Group_Key 
	FROM 		Concept AS CSource
	INNER JOIN	Concept AS CAllKnownSynonyms 	ON CAllKnownSynonyms.Meaning_Key = CSource.Meaning_Key 
							AND CAllKnownSynonyms.Concept_Key <> @Key
	INNER JOIN	Term AS T 			ON T.Term_Key = CAllKnownSynonyms.Term_Key
	LEFT JOIN	Term_Version AS TV 		ON TV.Term_Version_Key = CAllKnownSynonyms.Term_Version_Key
	INNER JOIN	Concept_Group AS CG 		ON CG.Concept_Group_Key = CAllKnownSynonyms.Concept_Group_Key
	WHERE 		CSource.Concept_key = @Key

	ORDER BY 	Item_Name

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_AllSynonyms_Select_ForConcept') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_AllSynonyms_Select_ForConcept'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_AllSynonyms_Select_ForConcept TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_AllSynonyms_Select_ForConcept TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_AllSynonyms_Select_ForConcept TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_AllSynonyms_Select_ForConcept TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_AllSynonyms_Select_ForConcept TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_AllSynonyms_Select_ForConcept TO [Dev - JNCC SQL]
END
GO

IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_CollectionList') AND SysStat & 0xf = 4)
DROP PROCEDURE [dbo].[usp_CollectionList]
GO
    
/*===========================================================================*\
  Description:	

  Parameters:	

  Created:	August 2003

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_CollectionList]
AS
	SET NOCOUNT ON
	SELECT	Item_Name, Collection_Unit_Key 
	FROM 	Collection
	ORDER BY Item_Name
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_CollectionList') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_CollectionList'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_CollectionList TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_CollectionList TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_CollectionList TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_CollectionList TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_CollectionList TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_CollectionList TO [Dev - JNCC SQL]
END
GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Collections_Select_ForStore]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Collections_Select_ForStore]
GO

/*===========================================================================*\
  Description:	Returns Collections for a specified Store.

  Parameters:	
	@ParentKey 	Only the records associated with the parent key are returned
	@SortOrderIndex	Index determining Sort Order
	@UserDomainMask	User's Domain Mask restricting which records may be returned
	@SessionID 	User's SessionID

  Created:	September 2003

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Collections_Select_ForStore] 
	@UserDomainMask INT,
	@SessionID CHAR(16),
	@SortOrderIndex TINYINT,
	@ParentKey CHAR(16)
AS

SET NOCOUNT ON

	DECLARE @Collection TABLE
	(
		[Collection_Unit_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
		[Item_Name] [varchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[Number] [varchar] (30) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
		[Collation_From_Vague_Date_Start] [int] NULL,
		[Collation_From_Vague_Date_End] [int] NULL
	)

	INSERT INTO 	@Collection (Collection_Unit_Key)
	SELECT DISTINCT C.Collection_Unit_Key
	FROM			Collection_Unit CU
	INNER JOIN 		Specimen_Unit SU ON CU.Collection_Unit_Key = SU.Collection_Unit_Key
						AND CU.Current_Container_Collection_Unit_Key = @ParentKey
	INNER JOIN 		Collection C ON SU.Parent_Collection_Collection_Unit_Key = C.Collection_Unit_Key
	INNER JOIN		Collection_Unit AS CU2 ON CU2.Collection_Unit_Key = C.Collection_Unit_Key
						AND ((CU2.Domain_Mask & @UserDomainMask > 0) OR (CU2.Entered_Session_ID = @SessionID) 
							OR (CU2.Changed_Session_ID = @SessionID) OR (CU2.Domain_Mask = 0))

	UPDATE	CList
	SET 	Item_Name = C.Item_Name,
		Number = M.Number,
		Collation_From_Vague_Date_Start = C.Collation_From_Vague_Date_Start,
		Collation_From_Vague_Date_End = C.Collation_From_Vague_Date_End

	FROM 		@Collection CList INNER JOIN Collection C ON CList.Collection_Unit_Key = C.Collection_Unit_Key
	LEFT JOIN	(Movement_Collection_Unit MCU
			INNER JOIN	Movement_Direction MD ON MCU.Movement_Direction_Key = MD.Movement_Direction_Key AND (MD.Outbound = 0)
			INNER JOIN	Movement M ON MD.Movement_Key = M.Movement_Key AND M.Movement_Type IN (0, 1)
			LEFT JOIN	(Concept CON
					INNER JOIN Term T ON CON.Term_Key = T.Term_Key
					)
				ON CON.Concept_Key = 'SYSTEM0000000006' --ACCESSION NUMBER
			)
		ON C.Collection_Unit_Key = MCU.Collection_Unit_Key

IF @SortOrderIndex = 0
	SELECT	Collection_Unit_Key AS Item_Key, Collection_Unit_Key AS Join_Key, Item_Name, Number
	FROM 	@Collection
	ORDER BY Collation_From_Vague_Date_Start DESC, Collation_From_Vague_Date_End DESC, Item_Name, Number
ELSE 
IF @SortOrderIndex = 1
	SELECT	Collection_Unit_Key AS Item_Key, Collection_Unit_Key AS Join_Key, Item_Name, Number
	FROM 	@Collection
	ORDER BY Item_Name, Number
ELSE 
IF @SortOrderIndex = 2
	SELECT	Collection_Unit_Key AS Item_Key, Collection_Unit_Key AS Join_Key, Item_Name, Number
	FROM 	@Collection
	ORDER BY Number
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Collections_Select_ForStore') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Collections_Select_ForStore'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Collections_Select_ForStore TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Collections_Select_ForStore TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Collections_Select_ForStore TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Collections_Select_ForStore TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Collections_Select_ForStore TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Collections_Select_ForStore TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_CollectionUnitStatus_Get') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_CollectionUnitStatus_Get]
GO

/*===========================================================================*\
  Description:	Returns the movement status for a collection unit record.

  Parameters:	@Key	Collection unit key
		@Status	OUTPUT

  Created:	August 2003

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_CollectionUnitStatus_Get]
	@Key char(16),
	@Status varchar(200) OUTPUT
AS
	/*-------------------------------------------------------------*\
	  Get a new key.
	\*-------------------------------------------------------------*/
	-- If unknown, set result and exit.
	IF NOT (EXISTS(
		SELECT * FROM Movement_Collection_Unit MCU 
		INNER JOIN 	Movement_Of_Material MM ON MM.Movement_Direction_Key = MCU.Movement_Direction_Key
		LEFT JOIN 	Movement_Of_Material_Exclusion MME ON MME.Movement_Of_Material_Key = MM.Movement_Of_Material_Key
			AND MME.Collection_Unit_Key=MCU.Collection_Unit_Key
		WHERE MCU.Collection_Unit_Key = @Key
		AND MME.Movement_Of_Material_Exclusion_Key IS NULL)
	OR EXISTS(
		SELECT * FROM Movement_Collection_Unit MCU 
		INNER JOIN 	Movement_Of_Ownership MO ON MO.Movement_Direction_Key = MCU.Movement_Direction_Key
		LEFT JOIN 	Movement_Of_Ownership_Exclusion MOE ON MOE.Movement_Of_Ownership_Key = MO.Movement_Of_Ownership_Key
			AND MOE.Collection_Unit_Key=MCU.Collection_Unit_Key
		WHERE MCU.Collection_Unit_Key = @Key
		AND MOE.Movement_Of_Ownership_Exclusion_Key IS NULL))
	BEGIN
		SET @Status = 'Unknown'
		RETURN
	END

	/*-------------------------------------------------------------*\
	  Need to work out what it is.
	\*-------------------------------------------------------------*/
	-- Some variables.
	DECLARE	@HoldingOrganisationKey char(16),
		@OwnershipNameKey char(16),
		@OwnershipNameKeyIsHoldingOrg bit,
		@OwnershipMovementType int,
		@MaterialNameKey char(16),
		@MaterialNameKeyIsHoldingOrg bit,
		@MaterialMovementType int,
		@DepartmentName varchar(100)

	-- Get the organisation holding software install	
	SELECT	@HoldingOrganisationKey = Data FROM Setting WHERE [Name] = 'HoldingOrg'

	/*-------------------------------------------------------------*\
	  Get most recent movement of material.
	\*-------------------------------------------------------------*/
	SELECT	TOP 1	@MaterialNameKey = MM.Receiver_Name_Key, 
			@MaterialMovementType = Movement_Type, 
			@DepartmentName = OD.Item_Name
	FROM		Movement_Collection_Unit MCU
		INNER JOIN 	Movement_Direction MD ON MD.Movement_Direction_Key = MCU.Movement_Direction_Key
		INNER JOIN 	Movement M ON M.Movement_Key = MD.Movement_Key
		INNER JOIN 	Movement_Of_Material MM ON MM.Movement_Direction_Key = MCU.Movement_Direction_Key
		LEFT JOIN 	Movement_Of_Material_Exclusion MME ON MME.Movement_Of_Material_Key = MM.Movement_Of_Material_Key
			AND MME.Collection_Unit_Key=MCU.Collection_Unit_Key
		LEFT JOIN	Organisation_Department OD ON OD.Organisation_Department_Key = MM.Receiver_Organisation_Department_Key
	WHERE		MCU.Collection_Unit_Key = @Key
		AND 		MME.Movement_Of_Material_Key IS NULL
	ORDER BY	MM.Vague_Date_Start DESC

	/*-------------------------------------------------------------*\
	  Lost and destroyed movements
	\*-------------------------------------------------------------*/
	IF @MaterialMovementType = 4
	BEGIN
		SET @Status='Destroyed'
		RETURN
	END
	IF @MaterialMovementType = 7
	BEGIN
		SET @Status='Lost'
		RETURN
	END

	/*-------------------------------------------------------------*\
	  Get most recent movement of ownership.
	\*-------------------------------------------------------------*/
	SELECT	TOP 1	@OwnershipNameKey = IsNull(MD2.Receiver_Name_Key, MD.Receiver_Name_Key),
			@OwnershipMovementType = M.Movement_Type
	FROM		Movement_Collection_Unit MCU
	INNER JOIN 	Movement_Direction MD ON MD.Movement_Direction_Key = MCU.Movement_Direction_Key
	INNER JOIN 	Movement M ON M.Movement_Key = MD.Movement_Key
			-- If the Movement is a loan in, the name key stored in the Movement_Direction
			-- record linked to the Movement_Collection_Unit record will be the receiver.
			-- The actual owner will be stored in the outbound Movement_Direction record
			-- associated with the movement, so do a left join on this.
	LEFT JOIN	(Movement_Direction MD2 
				INNER JOIN Movement AS M2 ON M2.Movement_Key = MD2.Movement_key
								AND M2.Movement_Type = 2)
			ON MD2.Movement_Key = M.Movement_Key
			AND MD2.Outbound = 1
	INNER JOIN 	Movement_Of_Ownership MO ON MO.Movement_Direction_Key = MCU.Movement_Direction_Key
	LEFT JOIN 	Movement_Of_Ownership_Exclusion MOE ON MOE.Movement_Of_Ownership_Key = MO.Movement_Of_Ownership_Key
			AND MOE.Collection_Unit_Key=MCU.Collection_Unit_Key
	WHERE		MCU.Collection_Unit_Key = @Key
	AND 		MOE.Movement_Of_Ownership_Key IS NULL
	ORDER BY	MO.Vague_Date_Start DESC

	/*-----------------------------------------------------------------------------*\
	  Now that we have the Name Key of the most recent Movement_Of_Onwership for 
	  this Collection_Unit, we want to see if this key refers to
	  the holding organisation, or if it refers to an individual who belongs
	  to the holding organisation
	\*-----------------------------------------------------------------------------*/
	IF 	@OwnershipNameKey = @HoldingOrganisationKey
		SET 	@OwnershipNameKeyIsHoldingOrg = 1
	ELSE IF EXISTS( SELECT 		I.Name_Key
			FROM		Individual AS I
			LEFT JOIN	Name_Relation AS NR1 ON NR1.Name_Key_1 = I.Name_Key
			LEFT JOIN	Name_Relation AS NR2 ON NR2.Name_Key_2 = I.Name_Key
			INNER JOIN	Organisation AS O ON (O.Name_Key = NR1.Name_Key_2
							  OR O.Name_Key = NR2.Name_Key_1)
			WHERE		I.Name_Key = @OwnershipNameKey
			AND 		O.Name_Key = @HoldingOrganisationKey)
		SET @OwnershipNameKeyIsHoldingOrg = 1
	-- If there is no @OwnershipNameKey assume that it belongs to the holding org.
	ELSE IF @OwnershipNameKey Is NULL
		SET @OwnershipNameKeyIsHoldingOrg = 1
	ELSE	SET @OwnershipNameKeyIsHoldingOrg = 0

	/*==============================================================================*\
	  Find out if the item is currently in the Holding Organisation or not.
	\*==============================================================================*/	

	/*-----------------------------------------------------------------------------*\
	  Now that we have the Name Key for the most recent Movement of Material, we 
	  want to see if this key refers to the holding organisation, or if it refers 
	  to an individual who belongs to the holding organisation
	\*-----------------------------------------------------------------------------*/
	IF 	@MaterialNameKey = @HoldingOrganisationKey
		SET 	@MaterialNameKeyIsHoldingOrg = 1
	ELSE IF EXISTS( SELECT 		I.Name_Key
			FROM		Individual AS I
			LEFT JOIN	Name_Relation AS NR1 ON NR1.Name_Key_1 = I.Name_Key
			LEFT JOIN	Name_Relation AS NR2 ON NR2.Name_Key_2 = I.Name_Key
			INNER JOIN	Organisation AS O ON (O.Name_Key = NR1.Name_Key_2
							  OR O.Name_Key = NR2.Name_Key_1)
			WHERE		I.Name_Key = @MaterialNameKey
			AND 		O.Name_Key = @HoldingOrganisationKey)
		SET @MaterialNameKeyIsHoldingOrg = 1
	ELSE	SET @MaterialNameKeyIsHoldingOrg = 0


	-- Formatted additional info after status. No need to repeat all over the place.
	DECLARE @ByOwner varchar(200), @ToOwner varchar(200), @FromOwner varchar(200)
	SET @ByOwner = IsNull(NullIf(' by '+ dbo.ufn_GetFormattedName(@OwnershipNameKey), ' by '), '')
	SET @ToOwner = IsNull(NullIf(' to '+ dbo.ufn_GetFormattedName(@OwnershipNameKey), ' to '), '')
	SET @FromOwner = IsNull(NullIf(' from '+ dbo.ufn_GetFormattedName(@OwnershipNameKey), ' from '), '')

	/*==============================================================================*\
	  Now work out what to display.
	\*==============================================================================*/		
	IF @OwnershipNameKeyIsHoldingOrg=1
	BEGIN
		IF @MaterialNameKeyIsHoldingOrg=1
		BEGIN
			IF @DepartmentName IS NOT NULL 
				SET @Status = 'In ' + @DepartmentName + ' department.'
			ELSE BEGIN
				SELECT	@DepartmentName = Full_Name
				FROM	Organisation
				WHERE	Name_Key = @HoldingOrganisationKey
	
				SET @Status = 'In ' + @DepartmentName + ' organisation.' 
			END 
		END
		ELSE
			SET @Status='On loan' + @ToOwner
	END
	ELSE
	BEGIN
		IF @MaterialNameKeyIsHoldingOrg=1
			SET @Status='On loan' + @FromOwner
		ELSE IF @MaterialMovementType=8
			SET @Status='Sold' + @ToOwner
		ELSE IF @MaterialMovementType=5
			SET @Status='Disposed' + @ToOwner		
		ELSE
			SET @Status='Owned' + @ByOwner
	END


GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_CollectionUnitStatus_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_CollectionUnitStatus_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitStatus_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_CollectionUnitStatus_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_CollectionUnitStatus_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_CollectionUnitStatus_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_CollectionUnitStatus_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitStatus_Get TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptGroupVersion_Delete]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptGroupVersion_Delete]
GO

/*===========================================================================*\
  Description:	Deletes a Concept_Group_Version record.

  Parameters:	@Key	Concept_Group_Version key
		@Timestamp,
		@RecordsAffected

  Created:	November 2003

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptGroupVersion_Delete]
	@Key char(16),
	@Timestamp timestamp = NULL,
	@SyncTaxonDict bit = 0,
	@RecordsAffected int = 1 OUTPUT
AS

SET NOCOUNT ON

	BEGIN TRANSACTION
		/*============================================================*\
		  See if the user wants any associated taxon dictionary
		  records be deleted with the Concept Group Version.
		\*============================================================*/
		IF @SyncTaxonDict = 1 
		BEGIN
			/*--------------------------------------------------------*\
			  Check that the Taxon_List_Version table exists before
			  attempting any of this deletion.
			\*--------------------------------------------------------*/
			IF EXISTS (SELECT *
		   			FROM   SysObjects 
					WHERE  Id = Object_Id(N'[dbo].[Taxon_List_Version]')
					AND 	  Type = 'U')
			BEGIN
				DECLARE		@Taxon_List_Version_Key char(16)

				SELECT		@Taxon_List_Version_Key = Taxon_List_Version_Key
				FROM		Taxon_Dictionary_Concept_Group_Version_Mapping
				WHERE		Concept_Group_Version_Key = @Key

				DELETE		Taxon_Dictionary_Concept_Group_Version_Mapping
				WHERE		Concept_Group_Version_Key = @Key

				/*-----------------------------------------------------------------*\
				  It is possible that this delete will fail. e.g. If the TLV record
				  is referred to in the Index_Taxon_Name or Taxon_List_Item tables, 
				  This will cause it to go to the RollbackAndExit method,
				  where the user can be asked if they want to replace the concept
				  with another (4.2.17.18)
				\*-----------------------------------------------------------------*/ 
				DELETE 		Taxon_List_Version
				WHERE		Taxon_List_Version_Key = @Taxon_List_Version_Key

				IF @@Error <> 0 GOTO RollbackAndExit
			END
		END

		DECLARE 
			@ConceptGroupKey CHAR(16),
			@Error INT

		SELECT @ConceptGroupKey=Concept_Group_Key
		FROM 	Concept_Group_Version
		WHERE	Concept_Group_Version_Key = @Key

		DELETE 
		FROM 	Concept_Group_Version
		WHERE	Concept_Group_Version_Key = @Key
		AND		((@Timestamp = Timestamp) OR (@Timestamp IS NULL))

		SELECT @RecordsAffected=@@ROWCOUNT, @Error = @@Error

		IF @Error <> 0 GOTO RollbackAndExit

		IF @RecordsAffected>0 
		BEGIN
			DECLARE @CurrentConceptKey CHAR(16)
			-- Update all the concepts to check if they are current
			DECLARE curConcepts CURSOR LOCAL FAST_FORWARD FOR
				SELECT	C.Concept_Key
				FROM	Concept AS C
				WHERE	Concept_Group_Key = @ConceptGroupKey
		
			OPEN curConcepts
		
			FETCH NEXT
			FROM	curConcepts
			INTO	@CurrentConceptKey
		
			WHILE @@Fetch_Status = 0
			BEGIN
				EXEC usp_Concept_UpdateIsCurrent @CurrentConceptKey, @ConceptGroupKey
				IF @@Error <> 0 GOTO RollbackAndExit
		
				FETCH NEXT
				FROM	curConcepts
				INTO	@CurrentConceptKey
			END
			CLOSE	curConcepts
			DEALLOCATE curConcepts
		END
	
	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptGroupVersion_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptGroupVersion_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptGroupVersion_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptGroupVersion_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptGroupVersion_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptGroupVersion_Delete TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptGroup_Delete]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptGroup_Delete]
GO

/*===========================================================================*\
  Description:	Deletes a Concept_Group record.

  Parameters:	@Key	Concept_Group_key
		@Timestamp

  Created:	December 2003

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptGroup_Delete]
	@Key char(16),
	@Timestamp timestamp = null,
	@SyncTaxonDict bit = 0
AS

SET NOCOUNT ON

	BEGIN TRANSACTION
		/*============================================================*\
		  See if the user wants any associated taxon dictionary
		  records be deleted with the Concept Group.
		\*============================================================*/
		IF @SyncTaxonDict = 1 
		BEGIN
			/*--------------------------------------------------------*\
			  Check that the Taxon_List table exists before
			  attempting any of this deletion. 
			\*--------------------------------------------------------*/
			IF EXISTS (SELECT *
		   			FROM   SysObjects 
					WHERE  Id = Object_Id(N'[dbo].[Taxon_List]')
					AND 	  Type = 'U')
			BEGIN
				DECLARE @TaxonListKey char(16)

				SELECT 	@TaxonListKey = Taxon_List_Key
				FROM	Taxon_Dictionary_Concept_Group_Mapping
				WHERE	Concept_Group_Key = @Key

				IF @@Error <> 0 GOTO RollbackAndExit

				DELETE	Taxon_List_Version
				WHERE	@TaxonListKey = Taxon_List_Key

				IF @@Error <> 0 GOTO RollbackAndExit

				DELETE	Taxon_Dictionary_Concept_Group_Mapping
				WHERE		Concept_Group_Key = @Key

				IF @@Error <> 0 GOTO RollbackAndExit

				DELETE	Taxon_List
				WHERE	Taxon_List_Key = @TaxonListKey

				IF @@Error <> 0 GOTO RollbackAndExit	
			END
		END
		ELSE
			DELETE	Taxon_Dictionary_Concept_Group_Mapping
			WHERE		Concept_Group_Key = @Key

		DELETE SF
		FROM Source_File SF
		INNER JOIN Source_Join SJ ON SJ.Source_Key=SF.Source_Key
		WHERE SJ.Table_Name='Concept_Group'
		AND SJ.Record_Key=@Key

		DELETE Source_Join
		WHERE Table_Name='Concept_Group'
		AND Record_Key=@Key
	
		DELETE 
		FROM 		Concept_Group
		WHERE		Concept_Group_Key = @Key
		AND			((@Timestamp = Timestamp) OR (@Timestamp IS NULL))

		IF @@Error <> 0 GOTO RollbackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptGroup_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptGroup_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptGroup_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptGroup_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptGroup_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptGroup_Delete TO [Dev - JNCC SQL]
END
GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_ConceptRanks_Select_ForSearch]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_ConceptRanks_Select_ForSearch]
GO

/*===========================================================================*\
  Description:	Search proc for Concept Ranks. If a duplicate is found,
				the Domain name is appended.

  Parameters:	@SearchText

  Created:		September 2004

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_ConceptRanks_Select_ForSearch] 
	@SearchText varchar(100)
AS

SET NOCOUNT ON

	/*----------------------------------------------------------*\
	  Declare the local variables, cursors and table variables.
	\*----------------------------------------------------------*/
	DECLARE @CurrentTitle varchar(113), 
			@CurrentConceptRankKey char(16),
			@CurrentDomainTitle varchar(100),
			@CurrentConceptRankName varchar(100)
	
	DECLARE	@NextTitle varchar(113),
			@NextConceptRankKey char(16),
			@NextDomainTitle varchar(100),
			@NextConceptRankName varchar(100)
	
	-- Select all of the Concept Ranks
	DECLARE curConceptRanks CURSOR LOCAL FAST_FORWARD FOR
		SELECT 		CR.Concept_Rank_Key, 
					CR.Abbreviation + ' - ' + CR.Item_Name AS Title,					
					D.Item_Name,
					CR.Item_Name AS Concept_Rank_Name
		FROM 		Concept_Rank AS CR
		INNER JOIN	Domain AS D ON D.Domain_Key = CR.Domain_Key
		ORDER BY	Title
	
	-- Create a table variable to store the result set in.
	DECLARE @tableResultSet TABLE(
		Concept_Rank_Key char(16),
		Title varchar(216),
		Concept_Rank_Name varchar(100)
	)
	
	DECLARE @MatchFound bit
	SET 	@MatchFound = 0
	
	OPEN curConceptRanks
	
	/*-------------------------------------------------*\
	  Set up the local variables with initial values.
	\*-------------------------------------------------*/
	FETCH NEXT 
	FROM	curConceptRanks
	INTO	@CurrentConceptRankKey, @CurrentTitle, @CurrentDomainTitle, @CurrentConceptRankName
	
	FETCH NEXT
	FROM	curConceptRanks
	INTO	@NextConceptRankKey, @NextTitle, @NextDomainTitle, @NextConceptRankName
	
	/*-------------------------------------------------*\
	  Start going through the cursor
	\*-------------------------------------------------*/
	WHILE @@Fetch_Status = 0
	BEGIN
		-- If a match has been found between the current and the next title, append the Domain name
		IF @CurrentTitle = @NextTitle
		BEGIN
			INSERT INTO @tableResultSet (Concept_Rank_Key, Title, Concept_Rank_Name) 
			VALUES (@CurrentConceptRankKey, @CurrentTitle + ' [' + @CurrentDomainTitle + ']', @CurrentConceptRankName)
	
			SET @MatchFound = 1
		END	
		-- If a match was found between the previous row and this row, add the Domain name
		ELSE IF @MatchFound = 1
		BEGIN
			INSERT INTO @tableResultSet (Concept_Rank_Key, Title, Concept_Rank_Name) 
			VALUES (@CurrentConceptRankKey, @CurrentTitle + ' [' + @CurrentDomainTitle + ']', @CurrentConceptRankName)
	
			SET @MatchFound = 0
		END
		-- If no match was found, don't bother adding the Domain name
		ELSE
			INSERT INTO @tableResultSet (Concept_Rank_Key, Title, Concept_Rank_Name) 
			VALUES (@CurrentConceptRankKey, @CurrentTitle, @CurrentConceptRankName)
		
		-- The next row's details have already been obtained, so put them into the local variables
		-- as the new current row
		SELECT 	@CurrentConceptRankKey = @NextConceptRankKey,
				@CurrentTitle = @NextTitle,
				@CurrentDomainTitle = @NextDomainTitle,
				@CurrentConceptRankName = @NextConceptRankName
		
		-- Get a new next row's details.
		FETCH NEXT
		FROM	curConceptRanks
		INTO	@NextConceptRankKey, @NextTitle, @NextDomainTitle, @NextConceptRankName
	END
	
	CLOSE 		curConceptRanks
	DEALLOCATE 	curConceptRanks
	
	/*-----------------------------------------------------------------------*\
	  Select the values from the temporary table that match the search text.
	\*-----------------------------------------------------------------------*/
	SELECT 	Concept_Rank_Key AS Item_Key, 
			Title AS DisplayTerm,
			Title COLLATE SQL_Latin1_General_CP1_CI_AS AS SearchTerm
	FROM 	@tableResultSet
	WHERE	Title LIKE @SearchText + '%'
	OR		Concept_Rank_Name LIKE @SearchText + '%'
	
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptRanks_Select_ForSearch') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptRanks_Select_ForSearch'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptRanks_Select_ForSearch TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptRanks_Select_ForSearch TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptRanks_Select_ForSearch TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ConceptRanks_Select_ForSearch TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptRanks_Select_ForSearch TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_ConceptRanks_Select_ForSearch TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptReplace_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptReplace_Update]
GO

/*===========================================================================*\
  Description:	Takes in two Concept keys - the key to be replaced and the
				key that will replace it. The tables to be updated have been
				chosen by using the Diagram tool in Enterprise manager, adding
				the Concept table, then choosing 'Add Related Tables'. All
				tables that have a foreign key to the Concept table will need
				to be updated.

  Parameters:	@Key					Concept Key to be replaced
				@NewConceptKey			Concept Key that will replace the old Concept Key
				@DeleteConceptRelations	If this flag is set, rather than update the
										Concept_Relation records, they are deleted
				@RecordsAffected		Output - this is necessary or the app will think
										the update has failed if the last update in the
										proc doesn't update any records.

  Created:		September 2004

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptReplace_Update]
	@Key char(16),
	@NewConceptKey char(16),
	@DeleteConceptRelations bit = 0,
	@RecordsAffected int = 1 output

AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION
		
		UPDATE 	Determination
		SET		Concept_Key = @NewConceptKey
		WHERE	Concept_Key = @Key 

		IF @@Error <> 0 GOTO RollbackAndExit

		UPDATE 	Determination
		SET		Nomenclatural_Status_Concept_Key = @NewConceptKey
		WHERE	Nomenclatural_Status_Concept_Key = @Key 

		IF @@Error <> 0 GOTO RollbackAndExit

		UPDATE	Occurrence
		SET		Record_Type_Concept_Key = @NewConceptKey
		WHERE	Record_Type_Concept_Key = @Key 

		IF @@Error <> 0 GOTO RollbackAndExit

		UPDATE	Occurrence_Data
		SET		Method_Concept_Key = @NewConceptKey
		WHERE	Method_Concept_Key = @Key 

		IF @@Error <> 0 GOTO RollbackAndExit

		UPDATE	Occurrence_Data
		SET		Parameter_Concept_Key = @NewConceptKey
		WHERE	Parameter_Concept_Key = @Key 

		IF @@Error <> 0 GOTO RollbackAndExit

		UPDATE	Occurrence_Data
		SET		Unit_Concept_Key = @NewConceptKey
		WHERE	Unit_Concept_Key = @Key 

		IF @@Error <> 0 GOTO RollbackAndExit

		UPDATE	Collection_Unit_Funding
		SET		Currency_Concept_Key = @NewConceptKey
		WHERE	Currency_Concept_Key = @Key 

		IF @@Error <> 0 GOTO RollbackAndExit

		UPDATE	Store
		SET		Store_Type_Concept_Key = @NewConceptKey
		WHERE	Store_Type_Concept_Key = @Key 

		IF @@Error <> 0 GOTO RollbackAndExit  

		UPDATE	Taxon_Dictionary_Name_Type_Mapping
		SET		Thesaurus_Name_Type_Key = @NewConceptKey
		WHERE	Thesaurus_Name_Type_Key = @Key 

		IF @@Error <> 0 GOTO RollbackAndExit   

		UPDATE	QE_Template_Field
		SET		Measurement_Method_Concept_Key = @NewConceptKey
		WHERE	Measurement_Method_Concept_Key = @Key 

		IF @@Error <> 0 GOTO RollbackAndExit

		UPDATE	QE_Template_Field
		SET		Measurement_Parameter_Concept_Key = @NewConceptKey
		WHERE	Measurement_Parameter_Concept_Key = @Key 

		IF @@Error <> 0 GOTO RollbackAndExit

		UPDATE	QE_Template_Field
		SET		Measurement_Unit_Concept_Key = @NewConceptKey
		WHERE	Measurement_Unit_Concept_Key = @Key 

		IF @@Error <> 0 GOTO RollbackAndExit

		UPDATE	Collection_Unit_Data
		SET 	Method_Concept_Key = @NewConceptKey
		WHERE	Method_Concept_Key = @Key 

		IF @@Error <> 0 GOTO RollbackAndExit

		UPDATE	Collection_Unit_Data
		SET 	Parameter_Concept_Key = @NewConceptKey
		WHERE	Parameter_Concept_Key = @Key 

		IF @@Error <> 0 GOTO RollbackAndExit

		UPDATE	Collection_Unit_Data
		SET 	Unit_Concept_Key = @NewConceptKey
		WHERE	Unit_Concept_Key = @Key 

		IF @@Error <> 0 GOTO RollbackAndExit

		IF NOT EXISTS (SELECT * FROM Taxon_Dictionary_Concept_Mapping WHERE	Concept_Key = @Key)
			UPDATE	Taxon_Dictionary_Concept_Mapping
			SET 	Concept_Key = @NewConceptKey
			WHERE	Concept_Key = @Key 

		IF @@Error <> 0 GOTO RollbackAndExit

		IF (@DeleteConceptRelations = 1)
			DELETE	Concept_Relation
			WHERE	(From_Concept_Key = @Key)
			OR		(To_Concept_Key = @Key)
		ELSE
			UPDATE	Concept_Relation
			SET 	From_Concept_Key = @NewConceptKey
			WHERE	From_Concept_Key = @Key 

		IF @@Error <> 0 GOTO RollbackAndExit

		UPDATE	Concept_Relation
		SET 	To_Concept_Key = @NewConceptKey
		WHERE	To_Concept_Key = @Key 

		IF @@Error <> 0 GOTO RollbackAndExit

		UPDATE	Concept_History
		SET		Concept_Key = @NewConceptKey
		WHERE	Concept_Key = @Key 

		IF @@Error <> 0 GOTO RollbackAndExit

		UPDATE	Conservation_Check
		SET		Type_Concept_Key = @NewConceptKey
		WHERE	Type_Concept_Key = @Key 

		IF @@Error <> 0 GOTO RollbackAndExit

		UPDATE	Conservation_Check
		SET		Condition_Concept_Key = @NewConceptKey
		WHERE	Condition_Concept_Key = @Key 

		IF @@Error <> 0 GOTO RollbackAndExit

		UPDATE	Conservation_Task
		SET		Type_Concept_Key = @NewConceptKey
		WHERE	Type_Concept_Key = @Key 

		IF @@Error <> 0 GOTO RollbackAndExit 

		UPDATE	Conservation_Task
		SET		Duration_Unit_Concept_Key = @NewConceptKey
		WHERE	Duration_Unit_Concept_Key = @Key 

		IF @@Error <> 0 GOTO RollbackAndExit 

		UPDATE	Conservation_Job
		SET		Duration_Unit_Concept_Key = @NewConceptKey
		WHERE	Duration_Unit_Concept_Key = @Key 

		IF @@Error <> 0 GOTO RollbackAndExit 	

		UPDATE	Conservation_Job
		SET		Currency_Concept_Key = @NewConceptKey
		WHERE	Currency_Concept_Key = @Key 

		IF @@Error <> 0 GOTO RollbackAndExit

		UPDATE	Conservation_Job_Material
		SET		Material_Concept_Key = @NewConceptKey
		WHERE	Material_Concept_Key = @Key 

		IF @@Error <> 0 GOTO RollbackAndExit  	

		UPDATE	Conservation_Job_Material
		SET		Unit_Concept_Key = @NewConceptKey
		WHERE	Unit_Concept_Key = @Key 

		IF @@Error <> 0 GOTO RollbackAndExit 

		UPDATE	Conservation_Job_Funding
		SET		Currency_Concept_Key = @NewConceptKey
		WHERE	Currency_Concept_Key = @Key 

		IF @@Error <> 0 GOTO RollbackAndExit 

		UPDATE	Movement_Communication
		SET		Communication_Type_Concept_Key = @NewConceptKey
		WHERE	Communication_Type_Concept_Key = @Key 

		IF @@Error <> 0 GOTO RollbackAndExit 		

		UPDATE	Collection_Unit_Process
		SET		Process_Concept_Key = @NewConceptKey
		WHERE	Process_Concept_Key = @Key 

		IF @@Error <> 0 GOTO RollbackAndExit 	

		UPDATE	Taxon_Dictionary_Designation_Type_Mapping
		SET		Concept_Designation_Type_Key = @NewConceptKey
		WHERE	Concept_Designation_Type_Key = @Key  

		IF @@Error <> 0 GOTO RollbackAndExit	

		UPDATE	Movement_Of_Material
		SET		Currency_Concept_Key = @NewConceptKey
		WHERE	Currency_Concept_Key = @Key 

		IF @@Error <> 0 GOTO RollbackAndExit 	

		UPDATE	Movement_Of_Material
		SET		Acquisition_Method_Concept_Key = @NewConceptKey
		WHERE	Acquisition_Method_Concept_Key = @Key 

		IF @@Error <> 0 GOTO RollbackAndExit 	

		UPDATE	Valuation
		SET		Type_Concept_Key = @NewConceptKey
		WHERE	Type_Concept_Key = @Key 

		IF @@Error <> 0 GOTO RollbackAndExit 

		UPDATE	Valuation
		SET		Currency_Concept_Key = @NewConceptKey
		WHERE	Currency_Concept_Key = @Key 

		IF @@Error <> 0 GOTO RollbackAndExit 

		UPDATE	Movement_Funding
		SET		Currency_Concept_Key = @NewConceptKey
		WHERE	Currency_Concept_Key = @Key 

		IF @@Error <> 0 GOTO RollbackAndExit 

		UPDATE	Enquiry
		SET		Enquiry_Type_Concept_Key = @NewConceptKey
		WHERE	Enquiry_Type_Concept_Key = @Key 

		IF @@Error <> 0 GOTO RollbackAndExit   

		UPDATE	Enquiry
		SET		Enquiry_Method_Concept_Key = @NewConceptKey
		WHERE	Enquiry_Method_Concept_Key = @Key 

		IF @@Error <> 0 GOTO RollbackAndExit  

		UPDATE	Enquiry_Concept
		SET		Concept_Key = @NewConceptKey
		WHERE	Concept_Key = @Key 

		IF @@Error <> 0 GOTO RollbackAndExit  

		UPDATE	Collection_Unit_Name
		SET		Relation_Type_Concept_Key = @NewConceptKey
		WHERE	Relation_Type_Concept_Key = @Key 

		IF @@Error <> 0 GOTO RollbackAndExit   

		UPDATE	Collection_Unit_Number
		SET		Type_Concept_Key = @NewConceptKey
		WHERE	Type_Concept_Key = @Key 

		IF @@Error <> 0 GOTO RollbackAndExit  		

		UPDATE	Collection_Unit_Material
		SET		Material_Concept_Key = @NewConceptKey
		WHERE	Material_Concept_Key = @Key 

		IF @@Error <> 0 GOTO RollbackAndExit  

		UPDATE	Collection_Unit_Material
		SET		Unit_Concept_Key = @NewConceptKey
		WHERE	Unit_Concept_Key = @Key 

		IF @@Error <> 0 GOTO RollbackAndExit 

		UPDATE	Concept_Designation
		SET		Concept_Key = @NewConceptKey
		WHERE	Concept_Key = @Key 

		IF @@Error <> 0 GOTO RollbackAndExit 

		UPDATE	Concept_Designation
		SET		Designation_Type_Concept_Key = @NewConceptKey
		WHERE	Designation_Type_Concept_Key = @Key 

		IF @@Error <> 0 GOTO RollbackAndExit 

		UPDATE	Specimen_Label
		SET		Confidence_Concept_Key = @NewConceptKey
		WHERE	Confidence_Concept_Key = @Key  

		IF @@Error <> 0 GOTO RollbackAndExit

		UPDATE	Collection
		SET		Risk_Concept_Key = @NewConceptKey
		WHERE	Risk_Concept_Key = @Key  

		IF @@Error <> 0 GOTO RollbackAndExit 

		UPDATE	Specimen_Unit 
		SET		Specimen_Type_Concept_Key = @NewConceptKey
		WHERE	Specimen_Type_Concept_Key = @Key 

		IF @@Error <> 0 GOTO RollbackAndExit  

		UPDATE 	Taxon_Determination
		SET		Nomenclatural_Status_Concept_Key = @NewConceptKey
		WHERE	Nomenclatural_Status_Concept_Key = @Key 

		IF @@Error <> 0 GOTO RollbackAndExit 

		UPDATE	Thesaurus_Fact
		SET		Concept_Key = @NewConceptKey
		WHERE	Concept_Key = @Key 
 
		IF @@Error <> 0 GOTO RollbackAndExit  

		SET @RecordsAffected = 1		

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	SET @RecordsAffected = 0		
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptReplace_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptReplace_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptReplace_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptReplace_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptReplace_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptReplace_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_ConceptReplace_Update TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptSimple_Update') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_ConceptSimple_Update]
GO

/*===========================================================================*\
  Description:	Updates a record in the concept table.

  Parameters:	@Key
		@TermKey 
		@ConceptGroupKey 
		@Preferred
		@ConceptRankKey 
		@NameTypeConceptKey
		@SortCode 
		@ListCode 
		@SessionID 
		@RecordsAffected 
		@Timestamp 

  Created:	December 2003

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptSimple_Update]
	@Key char(16),
	@TermKey char(16),
	@ConceptGroupKey char(16),
	@ListPreferred bit = NULL,
	@Preferred bit,
	@ConceptRankKey char(16),
	@NameTypeConceptKey char(16) = NULL,
	@SortCode int,
	@ListCode varchar(50),
	@SessionID char(16),
	@RecordsAffected int OUTPUT,
	@Timestamp timestamp
AS
SET NOCOUNT OFF
	
	BEGIN TRANSACTION
		/*-------------------*\
		  Update the Concept.
		\*-------------------*/
		DECLARE @old_concept_group_key CHAR(16),
			@old_list_preferred BIT,
			@OldConceptRankKey char(16),
			@error INT
		
		UPDATE	Concept
		SET 	@old_concept_group_key = Concept_Group_Key,
			@old_list_preferred = List_Preferred,
			@OldConceptRankKey = Concept_Rank_Key,
			List_Preferred = IsNull(@ListPreferred, List_Preferred),
			Concept_Group_Key = @ConceptGroupKey,
			Term_Key = @TermKey,
			Concept_Rank_Key = @ConceptRankKey,
			Preferred = @Preferred,
			Name_Type_Concept_Key = @NameTypeConceptKey,
			Sort_Code = @SortCode,
			List_Code = @ListCode,
			Changed_Session_ID = @SessionID			
		WHERE	Concept_Key = @Key
		AND	(@Timestamp = Timestamp)

		SELECT	@error = @@ERROR,
			@RecordsAffected = @@ROWCOUNT

		IF @error <> 0 GOTO RollbackAndExit

		/*----------------------------------------------------------------------------*\
		  Make corresponding changes in Concept_Lineage
		\*----------------------------------------------------------------------------*/
		EXECUTE	usp_ConceptLineage_ConceptUpdated	@Key,
								@old_concept_group_key,
								@old_list_preferred
		IF @@ERROR <> 0 GOTO RollbackAndExit
		
		/*----------------------------------------------------------------------------*\
		  If @Preferred = 1, then make sure the updated concept is the 
		  only Preferred synonym with the same language key and name type concept key.
		\*----------------------------------------------------------------------------*/
		IF @Preferred = 1 
			UPDATE		CSynonyms
			SET		Preferred = 0
			FROM 		Concept AS CSource
			INNER JOIN	Term 	AS TSource 	ON TSource.Term_Key = CSource.Term_Key
			INNER JOIN	Concept AS CSynonyms 	ON CSynonyms.Meaning_Key = CSource.Meaning_Key
								AND CSynonyms.Name_Type_Concept_Key = CSource.Name_Type_Concept_Key
								AND CSynonyms.Concept_Key <> CSource.Concept_Key
			INNER JOIN	Term 	AS TSynonyms 	ON TSynonyms.Term_Key = CSynonyms.Term_Key
								AND TSynonyms.Language_Key = TSource.Language_Key
			WHERE		CSource.Concept_Key = @Key

		IF @@Error <> 0 GOTO RollbackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit:
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_ConceptSimple_Update failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptSimple_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptSimple_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptSimple_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptSimple_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptSimple_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptSimple_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptSimple_Update TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Concept_Delete]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Concept_Delete]
GO

/*===========================================================================*\
  Description:	Delete a record from the Concept tables. Also deletes records
		from other tables where necessary.

  Parameters:	@Key		Concept key.
				@Timestamp

  Created:	December 2003

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Concept_Delete]
	@Key char(16),
	@Timestamp timestamp = NULL,
	@SyncTaxonDict bit = 0
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	DECLARE @MeaningKey char(16),
			@TermKey char(16),
			@TermVersionKey char(16),
			@ConceptsSharingMeaningKeyCount int,
			@ConceptsSharingTermKeyCount int,
			@ConceptsSharingTermVersionKeyCount int,
			@OriginalTimestamp timestamp

	-- Store the Meaning, Term and Term Version keys because the concept record
	-- needs to be deleted before these other records can be, due to referential
	-- integrity.
	SELECT	@MeaningKey = Meaning_Key,
			@TermKey = Term_Key,
			@TermVersionKey = Term_Version_Key,
			@OriginalTimestamp = [Timestamp]
	FROM 	Concept
	WHERE	Concept_Key = @Key

	-- Count the number of concepts that use this meaning key.
	SELECT 		@ConceptsSharingMeaningKeyCount = Count(C2.Concept_Key)
	FROM		Concept AS C1
	INNER JOIN	Concept AS C2 ON C2.Meaning_Key = C1.Meaning_Key
	WHERE		C1.Concept_Key = @Key

	-- Count the number of concepts that use the same term key as the concept we want to delete.
	SELECT 		@ConceptsSharingTermKeyCount = Count(DISTINCT C2.Concept_Key)
	FROM		Concept AS C1
	INNER JOIN	Concept AS C2 ON C2.Term_Key = C1.Term_Key
	WHERE		C1.Concept_Key = @Key

	-- Count the number of concepts that use the same term version key as the concept we want to delete.
	SELECT 		@ConceptsSharingTermVersionKeyCount = Count(DISTINCT C2.Concept_Key)
	FROM		Concept AS C1
	INNER JOIN	Concept AS C2 ON C2.Term_Version_Key = C1.Term_Version_Key
	WHERE		C1.Concept_Key = @Key


	BEGIN TRANSACTION
		/*============================================================*\
		  See if the user wants any associated taxon dictionary
		  records be deleted with the concept.
		\*============================================================*/
		IF @SyncTaxonDict = 1 
		BEGIN
			/*--------------------------------------------------------*\
			  Check that the Taxon_List_Item table exists before
			  attempting any of this deletion. In the future, the 
			  Thesaurus module could be installed without the Taxon
			  tables, so would go wrong if we tried to delete from
			  non-existant tables.			
			\*--------------------------------------------------------*/
			IF EXISTS (SELECT *
		   			FROM   SysObjects 
					WHERE  Id = Object_Id(N'[dbo].[Taxon_List_Item]')
					AND 	  Type = 'U')
			BEGIN
				-- Get the Taxon List Item Key for the current Concept
				DECLARE @TaxonListItemKey char(16)
	
				SELECT 	@TaxonListItemKey = Taxon_List_Item_Key
				FROM	Taxon_Dictionary_Concept_Mapping
				WHERE	Concept_Key = @Key

				/*--------------------------------------------------------*\
				  Delete the records related to the Taxon_List_Item table
				\*--------------------------------------------------------*/
				DELETE 	Taxon_Dictionary_Concept_Mapping
				WHERE	Taxon_List_Item_Key = @TaxonListItemKey
				AND	Concept_Key = @Key

				IF @@Error <> 0 GOTO RollbackAndExit

				DELETE	Taxon_Common_Name
				WHERE	Taxon_List_Item_Key = @TaxonListItemKey

				IF @@Error <> 0 GOTO RollbackAndExit

				DELETE	Index_Taxon_Synonym
				WHERE	Taxon_List_Item_Key = @TaxonListItemKey
				OR	Synonym_List_Item_Key = @TaxonListItemKey

				IF @@Error <> 0 GOTO RollbackAndExit

				DELETE	Export_Filter_Taxon
				WHERE	Taxon_List_Item_Key = @TaxonListItemKey

				IF @@Error <> 0 GOTO RollbackAndExit

				DELETE	Index_Taxon_Group
				WHERE	Taxon_List_Item_Key = @TaxonListItemKey
				OR	Contained_List_Item_Key = @TaxonListItemKey

				IF @@Error <> 0 GOTO RollbackAndExit

				DELETE	Taxon_Designation
				WHERE	Taxon_List_Item_Key = @TaxonListItemKey

				IF @@Error <> 0 GOTO RollbackAndExit

				DELETE	Taxon_User_Name
				WHERE	Taxon_List_Item_Key = @TaxonListItemKey

				IF @@Error <> 0 GOTO RollbackAndExit

				DELETE	Index_Taxon_Name
				WHERE	Taxon_List_Item_Key = @TaxonListItemKey

				IF @@Error <> 0 GOTO RollbackAndExit
		
				-- If one Concept shares the Term, attempt to delete the equivalent Taxon.
				IF @ConceptsSharingTermKeyCount = 1
				BEGIN
					DECLARE @TaxonKey char(16)
		
					-- Get the key of the equivalent Taxon
					SELECT 	@TaxonKey = Taxon_Key
					FROM	Taxon_Dictionary_Term_Mapping
					WHERE	Term_Key = @TermKey

							-- Only delete if there are no Taxon_Version records using the Taxon
					IF NOT EXISTS(SELECT 	* 
									FROM 	Taxon_Version
									WHERE	Taxon_Key = @TaxonKey)
					BEGIN
						DELETE SF
						FROM Source_File SF
						INNER JOIN Taxon_Sources TS ON TS.Source_Key=SF.Source_Key
						WHERE TS.Taxon_Key=@TaxonKey
		
						DELETE Taxon_Sources
						WHERE Taxon_Key=@TaxonKey
					
						DELETE	Taxon
						WHERE	Taxon_Key = @TaxonKey
					END
				END

				/*-----------------------------------------------------------------*\
				  It is possible that this delete will fail. e.g. If the TLI record
				  is referred to in the Taxon_Determination table, or a row in 
				  the TLI table has its Parent set to the record we are attempting
				  to delete. This will cause it to go to the RollbackAndExit method,
				  where the user can be asked if they want to replace the concept
				  with another (4.2.17.18). Before deleting the TLI records, we
				  need to remove the Taxon_Dictionary_Meaning_Mapping records.
				\*-----------------------------------------------------------------*/ 
				DELETE	Taxon_Dictionary_Meaning_Mapping
				WHERE	Preferred_Name = @TaxonListItemKey

				IF @@Error <> 0 GOTO RollbackAndExit

				DELETE 	Taxon_List_Item
				WHERE	Taxon_List_Item_Key = @TaxonListItemKey

				IF @@Error <> 0 GOTO RollbackAndExit
			END
		END			
	
		/*====================================*\
		  Delete the records.
		\*====================================*/
		-- Delete the Concept_History record.
		DELETE	Concept_History
		WHERE	Concept_Key = @Key

		IF @@Error <> 0 GOTO RollbackAndExit

		/*-------------------------------------------------------*\
		  Delete the relation records which refer to the concept.
		\*-------------------------------------------------------*/
		DELETE	Concept_Relation
		WHERE	To_Concept_Key = @Key
		OR	From_Concept_Key = @Key

		IF @@Error <> 0 GOTO RollbackAndExit

		DELETE	Meaning_Relation
		WHERE	To_Concept_Key = @Key
		OR	From_Concept_Key = @Key

		IF @@Error <> 0 GOTO RollbackAndExit

		DELETE	Term_Version_Relation
		WHERE	To_Concept_Key = @Key
		OR	From_Concept_Key = @Key

		IF @@Error <> 0 GOTO RollbackAndExit

		/*-------------------------------------------------------*\
		  Delete the Enquiry_Concept records because otherwise
		  the deletion will fail because it says other records
		  link to the Concept. Enquiries cannot be viewed in the
		  Thesaurus Editor it would appear at a casual glance
		  that nothing is actually linked to the concept. 
		  So best to just delete the Enquiry_Concept join records.
		\*-------------------------------------------------------*/
		DELETE	Enquiry_Concept
		WHERE	Concept_Key = @Key

		IF @@Error <> 0 GOTO RollbackAndExit

		-- Delete the Concept_Lineage records.
		IF EXISTS (SELECT 1 FROM Concept WHERE Concept_Key = @Key)
		BEGIN
			EXECUTE		usp_ConceptLineage_DeleteConcept	@Key
			IF @@ERROR <> 0 GOTO RollbackAndExit
		END

		/*-------------------------------------------------------*\
			Delete the concept's designation records (and related)
		\*-------------------------------------------------------*/
		DELETE DM
		FROM Taxon_Dictionary_Concept_Designation_Mapping DM
		INNER JOIN Concept_Designation CD ON CD.Concept_Designation_Key=DM.Concept_Designation_Key
		WHERE CD.Concept_Key=@Key

		IF @@Error <> 0 GOTO RollbackAndExit		

		--Delete the source files
		DELETE SF
		FROM Source_File SF
		INNER JOIN Source_Join SJ
				ON SJ.Table_Name='Concept_Designation'
		INNER JOIN Concept_Designation CD
				ON CD.Concept_Designation_Key=SJ.Record_Key
				AND CD.Concept_Key=@Key
	
		IF @@Error <> 0 GOTO RollbackAndExit
	
		--Now delete the source joins
		DELETE SJ
		FROM Source_Join SJ
		INNER JOIN Concept_Designation CD
				ON CD.Concept_Designation_Key=SJ.Record_Key
				AND CD.Concept_Key=@Key
		WHERE SJ.Table_Name='Concept_Designation'

		IF @@Error <> 0 GOTO RollbackAndExit

		DELETE 
		FROM Concept_Designation
		WHERE Concept_Key=@Key

		/*-------------------------------------------------------*\
			 Delete the Concept record. Have to check timestamp passed into the proc
			 against the timestamp the Concept had before any of its related records
			 were deleted. This is because deleting the records above may cause
			 triggers to be fired. Deleting the record in Concept_History will fire
			 a trigger that updates the current Concept, causing its timestamp to 
			 change.
		\*-------------------------------------------------------*/

		--Delete the source files
		DELETE SF
		FROM Source_File SF
		INNER JOIN Source_Join SJ
				ON SJ.Table_Name='Concept' AND SJ.Record_Key=@Key
	
		IF @@Error <> 0 GOTO RollbackAndExit
	
		--Now delete the source joins
		DELETE SJ
		FROM Source_Join SJ
		WHERE SJ.Table_Name='Concept' AND SJ.Record_Key=@Key

		IF @@Error <> 0 GOTO RollbackAndExit

		DELETE	Concept
		WHERE	Concept_Key = @Key
		AND		((@Timestamp = @OriginalTimestamp) OR (@Timestamp IS NULL))

		IF @@Error <> 0 GOTO RollbackAndExit

		-- Delete the Meaning record if only one Concept uses that Meaning key.
		IF @ConceptsSharingMeaningKeyCount = 1 
			DELETE 	Meaning
			WHERE	Meaning_Key = @MeaningKey

		IF @@Error <> 0 GOTO RollbackAndExit

		-- Delete the Term Version record if only one Concept uses that Term Version key.
		IF @ConceptsSharingTermVersionKeyCount = 1
			DELETE	Term_Version
			WHERE	Term_Version_Key = @TermVersionKey

		IF @@Error <> 0 GOTO RollbackAndExit

		-- Delete the Term record if only one Concept uses that Term key.
		IF @ConceptsSharingTermKeyCount = 1
			IF NOT EXISTS(SELECT * FROM Term_Version WHERE Term_Key = @TermKey)	
				DELETE	Term
				WHERE	Term_Key = @TermKey

		IF @@Error <> 0 GOTO RollbackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
	RAISERROR ('usp_Concept_Delete failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Concept_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concept_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concept_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Concept_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Concept_Delete TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_GetTaxonListItem') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_Concept_GetTaxonListItem]
GO

/*===========================================================================*\
  Description:	Determine the taxon (if any) associated with the
		specified concept.

  Parameters:	@ConceptKey
		@TaxonListItemKey [on exit] Taxon list item key, or NULL

  Created:	September 2004

  Last revision information:
	$Revision: 7 $
	$Date: 6/02/09 10:41 $
	$Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Concept_GetTaxonListItem]
	@ConceptKey char(16),
	@TaxonListItemKey char(16) OUTPUT
AS
	SET NOCOUNT ON

	SELECT	@TaxonListItemKey = Taxon_List_Item_Key
	FROM	Taxon_Dictionary_Concept_Mapping
	WHERE	Concept_Key = @ConceptKey

	IF @@ROWCOUNT = 0
		SET @TaxonListItemKey =	NULL
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_GetTaxonListItem') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Concept_GetTaxonListItem'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concept_GetTaxonListItem TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concept_GetTaxonListItem TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Concept_GetTaxonListItem TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_Concept_InsertSpreadsheetRow]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_Concept_InsertSpreadsheetRow]
GO

/*===========================================================================*\
  Description:	Insert a Concept record, plus related records as required,
				based on data extracted from a spreadsheet.

  Parameters:   @SessionID				Session key
				@concept_group_key		Concept group key
				@author					Name of author
				@child_of				Parent concept key
				@citation_date			Citation date
                @rank                   Rank (abbreviation or name)
				@fact_title				Name of fact
				@fact_type				Name of fact type
				@fact_description		Fact data
				@list_code				Concept list code
				@name_type				Name of name type
				@sort_code				Concept sort code
				@synonym_of				Synonym concept key
				@language				Name of term language
				@language_key			Term language key
				@term_name				Term name
				@concept_key			[on exit] New concept key

  Created:		Jan 2004

  Last revision information:
	$Revision: 7 $
	$Date: 6/02/09 10:41 $
	$Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Concept_InsertSpreadsheetRow]
	@SessionID			CHAR(16),
	@concept_group_key	CHAR(16),
	@author				VARCHAR(100),
	@child_of			CHAR(16),
	@citation_date		VARCHAR(100),
    @rank               VARCHAR(100),
	@fact_title			VARCHAR(100),
	@fact_type			NVARCHAR(300),
	@fact_description	TEXT,
	@list_code			VARCHAR(50),
	@name_type			NVARCHAR(300),
	@sort_code			INT,
	@synonym_of			CHAR(16),
	@language			VARCHAR(50),
	@language_key		VARCHAR(4),
	@term_name			NVARCHAR(300),
	@concept_key		CHAR(16)	OUTPUT
AS
	SET NOCOUNT ON

	/* check parameters */
	IF @term_name IS NULL
	BEGIN
		RAISERROR ('Term name must be specified.', 16, 1)
		RETURN
	END

	IF NOT @language_key IS NULL
	BEGIN
		IF NOT EXISTS (	SELECT		1
						FROM		Language
						WHERE		Language_Key	=	@language_key )
		BEGIN
			RAISERROR ('Specified language does not exist.', 16, 1)
			RETURN
		END
	END
	ELSE
	BEGIN
		IF @language IS NULL
		BEGIN
			RAISERROR ('Language or Language Key must be specified.', 16, 1)
			RETURN
		END

		SELECT		@language_key	=	Language_Key
		FROM		Language
		WHERE		Item_Name		=	@language

		IF @@ROWCOUNT = 0
		BEGIN
			RAISERROR ('Specified language is not recognised.', 16, 1)
			RETURN
		END
	END

	DECLARE		@term_key			CHAR(16),
				@term_version_key	CHAR(16),
				@name_type_key		CHAR(16),
				@meaning_key		CHAR(16),
                @domain_key         CHAR(16),
                @rank_key           CHAR(16)

	BEGIN TRANSACTION

	/* work out the term */
	SELECT		@term_key		=	Term_Key
	FROM		Term
	WHERE		Language_Key	=	@language_key
	AND			Item_Name		=	@term_name

	IF @@ROWCOUNT = 0
	BEGIN
		EXECUTE		spNextKey	'Term',
								@term_key	OUTPUT
		IF @@ERROR <> 0 GOTO fail

		INSERT		Term (
					Term_Key,
					Language_Key,
					Item_Name,
					Plaintext,
					Entered_Session_ID)
		VALUES		(@term_key,
					@language_key,
					@term_name,
					dbo.ufn_RemoveHtmlMarkup(@term_name),
					@SessionID)
		IF @@ERROR <> 0 GOTO fail
	END

	/* create term version */
	IF @author IS NOT NULL OR @citation_date IS NOT NULL
	BEGIN
		EXECUTE		spNextKey	'Term_Version',
								@term_version_key	OUTPUT
		IF @@ERROR <> 0 GOTO fail

		INSERT		Term_Version (
					Term_Version_Key,
					Term_Key,
					Author_And_Date,
					Entered_Session_ID)
		VALUES		(@term_version_key,
					@term_key,
					ISNULL(@author + ' ', '') + ISNULL(@citation_date, ''),
					@SessionID)
		IF @@ERROR <> 0 GOTO fail
	END

	/* work out the meaning */
	IF @synonym_of IS NOT NULL
	BEGIN
		SELECT		@meaning_key	=	Meaning_Key
		FROM		Concept
		WHERE		Concept_Key		=	@synonym_of

		IF @@ROWCOUNT = 0
		BEGIN
			RAISERROR ('Synonym does not exist.', 16, 1)
			GOTO fail
		END
	END
	ELSE
	BEGIN
		EXECUTE		spNextKey	'Meaning',
								@meaning_key	OUTPUT
		IF @@ERROR <> 0 GOTO fail

		INSERT		Meaning (
					Meaning_Key)
		VALUES		(@meaning_key)

		IF @@ERROR <> 0 GOTO fail
	END

	/* work out name type */
	IF @name_type IS NULL
	BEGIN
		SET			@name_type_key	=	'SYSTEM00000000AN' /* 'Unknown' */
	END
	ELSE
	BEGIN
		SELECT		@name_type_key		=	c.Concept_Key
		FROM		Concept				AS	c
		INNER JOIN	Term				AS	t
		ON			t.Term_Key			=	c.Term_Key
		WHERE		c.Concept_Group_Key	=	'SYSTEM000000000M'
		AND			t.Language_Key		=	'en'
		AND			t.Item_Name			=	@name_type

		IF @@ROWCOUNT = 0
		BEGIN
			EXECUTE		usp_Concept_Insert	@name_type_key	OUTPUT,
											'SYSTEM000000000M',
											@name_type,
											@name_type,
											'en',
											@SessionID,
											'SYSTEM00000000AN'
			IF @@ERROR <> 0 GOTO fail
		END
	END

    /* work out rank */
    IF @rank IS NOT NULL
    BEGIN
        EXECUTE     usp_ConceptGroup_GetDomain  @concept_group_key,
                                                @domain_key         OUTPUT
        IF @@ERROR <> 0 GOTO fail

        SELECT      @rank_key       =   Concept_Rank_Key
        FROM        Concept_Rank
        WHERE       Domain_Key      =   @domain_key
        AND         Abbreviation    =   @rank

        IF @rank_key IS NULL
        BEGIN
            SELECT      @rank_key       =   Concept_Rank_Key
            FROM        Concept_Rank
            WHERE       Domain_Key      =   @domain_key
            AND         Item_Name       =   @rank
        END

        IF @rank_key IS NULL
        BEGIN
            EXECUTE     spNextKey   'Concept_Rank',
                                    @rank_key       OUTPUT
            IF @@ERROR <> 0 GOTO fail

            INSERT      Concept_Rank (
                        Concept_Rank_Key,
                        Domain_Key,
                        Item_Name,
                        Abbreviation,
                        Color_R,
                        Color_G,
                        Color_B,
                        Entered_Session_ID)
            VALUES      (@rank_key,
                        @domain_key,
                        @rank,
                        @rank,
                        0,
                        0,
                        0,
                        @SessionID)
            IF @@ERROR <> 0 GOTO fail
        END
    END

	/* create concept */
	EXECUTE		spNextKey	'Concept',
							@concept_key	OUTPUT
	IF @@ERROR <> 0 GOTO fail

	INSERT		Concept (
				Concept_Key,
				Term_Key,
				Concept_Group_Key,
				Term_Version_Key,
				List_Preferred,
				Preferred,
                Concept_Rank_Key,
				Name_Type_Concept_Key,
				Meaning_Key,
				Sort_Code,
				List_Code,
				Entered_Session_ID)
	SELECT		@concept_key,
				@term_key,
				@concept_group_key,
				@term_version_key,
				CASE WHEN @synonym_of IS NULL THEN 1 ELSE 0 END,
				CASE
					WHEN @synonym_of IS NULL THEN 1
					WHEN EXISTS (	SELECT		1
									FROM		Concept			AS	c
									INNER JOIN	Term			AS	t
									ON			t.Term_Key		=	c.Term_Key
									WHERE		c.Meaning_Key	=	@meaning_key
									AND			t.Language_Key	=	@language_key)
						THEN 0
					ELSE 1
				END,
                @rank_key,
				@name_type_key,
				@meaning_key,
				@sort_code,
				@list_code,
				@SessionID
	IF @@ERROR <> 0 GOTO fail

	/* update lineage */
	EXECUTE		usp_ConceptLineage_NewConcept	@concept_key
	IF @@ERROR <> 0 GOTO fail

	/* create parent-child relationship */
	IF @child_of IS NOT NULL
	BEGIN
		DECLARE		@relation_key		CHAR(16),
					@relation_type_key	CHAR(16)

		SELECT		@relation_type_key	=	Hierarchy_Relation_Type_Key
		FROM		Concept_Group
		WHERE		Concept_Group_Key	=	@concept_group_key

		EXECUTE		usp_ConceptRelation_Insert	@relation_key	OUTPUT,
												@child_of,
												@concept_key,
												@relation_type_key,
												@SessionID = @SessionID
		IF @@ERROR <> 0 GOTO fail
	END

	/* create fact */
	IF @fact_description IS NOT NULL
	BEGIN
		DECLARE		@fact_type_key		CHAR(16),
					@fact_key			CHAR(16)

		IF @fact_type IS NULL
		BEGIN
			SET			@fact_type_key	=	'SYSTEM00000002NO' /* HTML */
		END
		ELSE
		BEGIN
			SELECT		@fact_type_key		=	c.Concept_Key
			FROM		Concept				AS	c
			INNER JOIN	Term				AS	t
			ON			t.Term_Key			=	c.Term_Key
			WHERE		c.Concept_Group_Key	=	'SYSTEM000000000L'
			AND			t.Language_Key		=	'en'
			AND			t.Item_Name			=	@fact_type

			IF @@ROWCOUNT = 0
			BEGIN
				EXECUTE		usp_Concept_Insert	@fact_type_key	OUTPUT,
												'SYSTEM000000000L',
												@fact_type,
												@fact_type,
												'en',
												@SessionID,
												'SYSTEM00000000AN'
				IF @@ERROR <> 0 GOTO fail
			END
		END

		EXECUTE		spNextKey	'Thesaurus_Fact',
								@fact_key			OUTPUT
		INSERT		Thesaurus_Fact (
					Thesaurus_Fact_Key,
					Item_Name,
					Data,
					Meaning_Key,
					Language_Key,
					Fact_Vague_Date_Type,
					Fact_Type_Concept_Key,
					Entered_Session_ID,
					System_Supplied_Data)
		VALUES		(@fact_key,
					ISNULL(@fact_title, 'Fact'),
					@fact_description,
					@meaning_key,
					'en',
					'U',
					@fact_type_key,
					@SessionID,
					0)
		IF @@ERROR <> 0 GOTO fail
	END

	COMMIT TRANSACTION
	RETURN

fail:
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_Concept_InsertSpreadsheetRow failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_InsertSpreadsheetRow') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Concept_InsertSpreadsheetRow'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concept_InsertSpreadsheetRow TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concept_InsertSpreadsheetRow TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Concept_InsertSpreadsheetRow TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_Paste') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_Concept_Paste]
GO

/*===========================================================================*\
  Description:	Pastes a concept from one position to another

  Parameters:	

  Created:	Aug 2004

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Concept_Paste]
	@ConceptKey CHAR(16),
	@DestConceptGroupKey CHAR(16),
	@DestParentConceptKey CHAR(16),
	@IsCut BIT,
	@SessionID CHAR(16),
	@SystemSuppliedData BIT=0
AS

BEGIN TRANSACTION

	/*-------------------------------------------------------------*\
		Prepare things for the operation
	\*-------------------------------------------------------------*/

	--Enforce a value in @SystemSuppliedData as the default value 
	--doesn't seem to work every time
	IF @SystemSuppliedData IS NULL
		SET @SystemSuppliedData=0

	DECLARE @SrcConceptGroupKey CHAR(16)
	DECLARE @Lineage VARCHAR(900)
	DECLARE @OldRelationTypeKey CHAR(16)
	DECLARE @NewRelationTypeKey CHAR(16)
	DECLARE @Key CHAR(16)
	DECLARE @DestConceptKey CHAR(16)


	SELECT 	@SrcConceptGroupKey = Concept_Group_Key
	FROM 	Concept
	WHERE 	Concept_Key = @ConceptKey
	IF @@Error <> 0 GOTO RollbackAndExit

	--Find the source concept group's hierarchy relationship
	SELECT 	@OldRelationTypeKey = CG.Hierarchy_Relation_Type_Key
	FROM 	Concept C
	JOIN 	Concept_Group CG ON CG.Concept_Group_Key = C.Concept_Group_Key
	WHERE 	C.Concept_Key = @ConceptKey
	IF @@Error <> 0 GOTO RollbackAndExit

	--Find the dest concept group's hierarchy relationship
	IF @DestParentConceptKey IS NULL 
		SET @NewRelationTypeKey=@OldRelationTypeKey
	ELSE
	BEGIN
		SELECT	@NewRelationTypeKey = CG.Hierarchy_Relation_Type_Key
		FROM 	Concept C
		JOIN 	Concept_Group CG ON CG.Concept_Group_Key = C.Concept_Group_Key
		WHERE 	C.Concept_Key = @DestParentConceptKey
	END

/*-------------------------------------------------------------*\
	Perform the cut or copy operation
\*-------------------------------------------------------------*/
IF @IsCut=1 
BEGIN
	SET @DestConceptKey=@ConceptKey

	--Prepare to delete subtree of lineage
	SELECT @Lineage=Lineage
	FROM Concept_Lineage
	WHERE Concept_Key=@ConceptKey
	IF @@Error <> 0 GOTO RollbackAndExit

	IF @DestParentConceptKey IS NULL
	BEGIN
		--Delete source's parent relationship(s)
		DECLARE @KeyToDel CHAR(16)
		DECLARE @Timestamp TIMESTAMP

		DECLARE csr CURSOR STATIC LOCAL FOR
			SELECT Concept_Relation_Key, Timestamp
			FROM Concept_Relation 
			WHERE To_Concept_Key=@ConceptKey
			AND Thesaurus_Relation_Type_Key=@OldRelationTypeKey

		OPEN csr
		
		WHILE 1=1
		BEGIN
			FETCH NEXT FROM csr INTO @KeyToDel, @Timestamp

			IF @@FETCH_STATUS<>0 
				BREAK
			
			EXEC usp_ConceptRelation_Delete @KeyToDel, @Timestamp
			IF @@Error <> 0 GOTO RollbackAndExit
		END
	END
	ELSE
	BEGIN
		--Update source's parent relationship to point to new parent key
		IF EXISTS(SELECT 1 FROM Concept_Relation 
					WHERE To_Concept_Key=@ConceptKey
					AND Thesaurus_Relation_Type_Key=@OldRelationTypeKey)
		BEGIN
			DECLARE @OldKey CHAR(16)
			SELECT @OldKey=From_Concept_Key, @Key=Concept_Relation_Key
			FROM Concept_Relation
			WHERE To_Concept_Key=@ConceptKey
				AND Thesaurus_Relation_Type_Key=@OldRelationTypeKey
			IF @@Error <> 0 GOTO RollbackAndExit

			UPDATE Concept_Relation 
			SET From_Concept_Key=@DestParentConceptKey,
				Changed_Session_ID=@SessionID,
				Thesaurus_Relation_Type_Key=@NewRelationTypeKey
			WHERE To_Concept_Key=@ConceptKey
				AND Thesaurus_Relation_Type_Key=@OldRelationTypeKey
			IF @@Error <> 0 GOTO RollbackAndExit

			EXECUTE		usp_ConceptLineage_UpdateRelation	
					@Key,
					@OldKey,
					@ConceptKey,
					@OldRelationTypeKey
			IF @@Error <> 0 GOTO RollbackAndExit
		END
		ELSE
		BEGIN
			EXECUTE spNextKey 'Concept_Relation', @Key OUTPUT	
		
			EXEC usp_ConceptRelation_Insert
				@Key,
				@DestParentConceptKey,
				@ConceptKey,
				@OldRelationTypeKey,
				NULL,
				NULL,
				NULL,
				@SessionID, 
				@SystemSuppliedData
			IF @@Error <> 0 GOTO RollbackAndExit
		END
	END

	IF @SrcConceptGroupKey<>@DestConceptGroupKey 
	BEGIN
		--Update concept group for source concepts to new group
		UPDATE CChild
		SET Concept_Group_Key = @DestConceptGroupKey
		FROM VW_ConceptChildren CC 
		INNER JOIN Concept CChild ON CChild.Concept_Key=CC.Child_Concept_Key
			AND CChild.Concept_Group_Key=@SrcConceptGroupKey
		WHERE CC.Parent_Concept_Key=@ConceptKey
		IF @@Error <> 0 GOTO RollbackAndExit
	END

	-- Actually delete the old lineage information	
	EXEC usp_ConceptLineage_DeleteSubtree @SrcConceptGroupKey, @Lineage
	IF @@Error <> 0 GOTO RollbackAndExit
END
ELSE
BEGIN
	--Whole branch being copied into a the concept group, so find all concepts and clone them
	DECLARE @ChildConceptKey CHAR(16)

	--Create a local table to hold key mappings
	DECLARE @ConceptMapping TABLE (
		Src_Concept_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS PRIMARY KEY,
		Dest_Concept_Key CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS
	)

	--Clone the source concepts, updating concept group key
	DECLARE csr CURSOR STATIC LOCAL FOR
		SELECT 	CChild.Concept_Key
		FROM 	VW_ConceptChildrenOnly CC 
		JOIN 	Concept CChild	ON CChild.Concept_Key = CC.Child_Concept_Key
					AND CChild.Concept_Group_Key = @SrcConceptGroupKey
		WHERE 	CC.Parent_Concept_Key = @ConceptKey
		-- Add the copied concept, instead of duplicating code to clone it separatly
		UNION
		SELECT	@ConceptKey

	OPEN csr
	FETCH NEXT FROM csr INTO @ChildConceptKey
	WHILE @@FETCH_STATUS=0
	BEGIN
		EXECUTE spNextKey 'Concept', @Key OUTPUT
		IF @@Error <> 0 GOTO RollBackAndExit
		
		-- When cloning the actual selected concept, remember the new concept key
		IF @ChildConceptKey = @ConceptKey 
			SET @DestConceptKey = @Key

		-- Rememer mappings so we can update relationships later
		INSERT INTO @ConceptMapping VALUES (@ChildConceptKey, @Key)
		IF @@Error <> 0 GOTO RollBackAndExit

		-- Clone the concept
		INSERT INTO Concept (
			Concept_Key, Term_Key, Concept_Group_Key, Term_Version_Key, List_Preferred, 
			Is_Current, Preferred, Concept_Rank_Key, Name_Type_Concept_Key, Meaning_Key,
			Author_Copy, Sort_Code, List_Code, Entered_Session_ID, System_Supplied_Data, Custodian
		)
		SELECT 	@Key, Term_Key, @DestConceptGroupKey, Term_Version_Key, List_Preferred, 
			Is_Current, Preferred, Concept_Rank_Key, Name_Type_Concept_Key, Meaning_Key,
			Author_Copy, Sort_Code, List_Code, @SessionID, @SystemSuppliedData, LEFT(@Key, 8)
		FROM 	Concept 
		WHERE 	Concept_Key = @ChildConceptKey
		IF @@Error <> 0 GOTO RollBackAndExit

		FETCH NEXT FROM csr INTO @ChildConceptKey
	END

	CLOSE csr
	DEALLOCATE csr

	/*-------------------------------------------------------------*\
		Clone the hierarchical relationships within the copied branch
			of concepts
	\*-------------------------------------------------------------*/
	DECLARE @SrcKey CHAR(16), @DestKey CHAR(16)

	--Declare a temp table with same structure as concept relation that 
	--we can populate with dummy primary keys, then update later
	SELECT TOP 0 * INTO #TempRel FROM Concept_Relation
	IF @@Error <> 0 GOTO RollbackAndExit

	DECLARE cmap CURSOR STATIC LOCAL FOR
		--Note we are cloning parent relationships within the branch, so 
		--exclude the top node
		SELECT * FROM @ConceptMapping WHERE Dest_Concept_Key<>@DestConceptKey
	
	OPEN cmap
	
	FETCH NEXT FROM cmap INTO @SrcKey, @DestKey
	WHILE @@FETCH_STATUS=0
	BEGIN
		INSERT INTO #TempRel (
			Concept_Relation_Key,
			From_Concept_Key,
			To_Concept_Key,
			Thesaurus_Relation_Type_Key,
			Multiplicity,
			Inherited,
			Comment,
			Entered_Session_ID,
			System_Supplied_Data,
			Custodian
			)
		SELECT 
			CR.Concept_Relation_Key, -- Will be replaced later
			ISNULL(CM.Dest_Concept_Key, CR.From_Concept_Key),
			@DestKey,
			Thesaurus_Relation_Type_Key,
			Multiplicity,
			Inherited,
			Comment,
			@SessionID,
			@SystemSuppliedData,
			Left(@DestKey, 8)
		FROM Concept_Relation CR
		LEFT JOIN @ConceptMapping CM ON CM.Src_Concept_Key=CR.From_Concept_Key
		WHERE CR.To_Concept_Key=@SrcKey
		AND CR.Thesaurus_Relation_Type_Key=@OldRelationTypeKey
		IF @@Error <> 0 GOTO RollbackAndExit

		FETCH NEXT FROM cmap INTO @SrcKey, @DestKey
	END
	
	CLOSE cmap
	DEALLOCATE cmap 

	--Now we have a table of concept relationships to insert, but we must update the keys first
	DECLARE crel CURSOR LOCAL FOR
		SELECT Concept_Relation_Key FROM #TempRel
	
	OPEN crel
	
	FETCH NEXT FROM crel INTO @SrcKey
	
	WHILE @@FETCH_STATUS=0
	BEGIN
		EXECUTE spNextKey 'Concept_Relation', @DestKey OUTPUT
		IF @@Error <> 0 GOTO RollbackAndExit
		
		UPDATE #TempRel
		SET Concept_Relation_Key=@DestKey
		WHERE CURRENT OF crel
		IF @@Error <> 0 GOTO RollbackAndExit

		FETCH NEXT FROM crel INTO @SrcKey		
	END

	CLOSE crel
	DEALLOCATE crel

	--Copy the relationships into the concept relation table
	INSERT INTO Concept_Relation (
			Concept_Relation_Key,
			From_Concept_Key,
			To_Concept_Key,
			Thesaurus_Relation_Type_Key,
			Multiplicity,
			Inherited,
			Comment,
			Entered_Session_ID,
			System_Supplied_Data,
			Custodian
		)
		SELECT 
			Concept_Relation_Key,
			From_Concept_Key,
			To_Concept_Key,
			Thesaurus_Relation_Type_Key,
			Multiplicity,
			Inherited,
			Comment,
			Entered_Session_ID,
			System_Supplied_Data,
			Custodian 
		FROM #TempRel
	IF @@Error <> 0 GOTO RollbackAndExit

	DROP TABLE #TempRel
END

	/*-------------------------------------------------------------*\
	 Join the copied branch of concepts to the destination concept.
	 This also fixes up the lineage.
	\*-------------------------------------------------------------*/
	IF (@DestParentConceptKey IS NOT NULL) AND ((@SrcConceptGroupKey<>@DestConceptGroupKey) OR (@IsCut=0))
	BEGIN
		EXECUTE spNextKey 'Concept_Relation', @Key OUTPUT	

		EXEC usp_ConceptRelation_Insert
			@Key,
			@DestParentConceptKey,
			@DestConceptKey,
			@OldRelationTypeKey,
			NULL,
			NULL,
			NULL,
			@SessionID, 
			@SystemSuppliedData
		IF @@Error <> 0 GOTO RollbackAndExit
	END

COMMIT TRANSACTION

RETURN

RollBackAndExit: 
	IF @@TranCount > 0 ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_Paste') AND SysStat & 0xf = 4)
BEGIN
	PRINT 'Setting up security on procedure usp_Concept_Paste'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concept_Paste TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concept_Paste TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
		GRANT EXECUTE ON dbo.usp_Concept_Paste TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Concept_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Concept_Select]
GO

/*===========================================================================*\
  Description:	Returns fields from the Concept table.

  Parameters:	@ConceptKey

  Created:	December 2003

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Concept_Select]
	@ConceptKey char(16)
AS
	SET NOCOUNT ON
	SET ANSI_NULLS ON
	SET ANSI_PADDING ON
	SET ANSI_WARNINGS ON
	SET CONCAT_NULL_YIELDS_NULL ON
	SET QUOTED_IDENTIFIER ON
	SET NO_BROWSETABLE OFF

	SELECT	
			C.Term_Key AS Item_Key,
			TL.Item_Name AS Item_Name,
			Lower(TL.Language_Key) AS Language_Key,
			L.Language_Key + ' - ' + L.Item_Name AS Language_Name,
			C.Concept_Group_Key,
			CG.Hierarchy_Relation_Type_Key,
			C.Term_Version_Key,	
			C.List_Preferred,
			C.Is_Current,
			C.Preferred,
			C.Concept_Rank_Key,
			CR.Item_Name AS Concept_Rank_Name,
			C.Name_Type_Concept_Key,
			CT.Item_Name AS Name_Type_Concept_Name,
			C.Meaning_Key,
			C.Author_Copy,
			C.Sort_Code,
			C.List_Code,
			CASE WHEN CRel.Concept_Relation_Key IS NULL THEN 0 ELSE 1 END AS HasChildren,
			C.System_Supplied_Data,
			C.Entered_Session_ID,
			C.[Timestamp]
	FROM 		Concept AS C
	INNER JOIN	Term AS TL ON TL.Term_Key = C.Term_Key
	INNER JOIN	Language AS L ON L.Language_Key = TL.Language_Key
	LEFT JOIN	Concept_Rank AS CR ON CR.Concept_Rank_Key = C.Concept_Rank_Key
	INNER JOIN	VW_ConceptTerm AS CT ON CT.Concept_Key = C.Name_Type_Concept_Key
	INNER JOIN	Concept_Group AS CG ON CG.Concept_Group_Key = C.Concept_Group_Key
	LEFT JOIN 	Concept_Relation CRel ON CRel.From_Concept_Key = C.Concept_Key
       				     	     AND CRel.Thesaurus_Relation_Type_Key = CG.Hierarchy_Relation_Type_Key
	WHERE		C.Concept_Key = @ConceptKey
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Concept_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Concept_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concept_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concept_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Concept_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Concept_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Concept_Select TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Concept_Select_ForTopLevel]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Concept_Select_ForTopLevel]
GO

/*===========================================================================*\
  Description: Returns a list of concepts that are the top level for the 
    					 supplied concept group.

  Parameters:	@ConceptGroupKey
		@HierarchyRelationTypeKey - relationship type used to populate
						hierarchy.

  Created:	August 2003

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Concept_Select_ForTopLevel]
	@ConceptGroupKey char(16),
  	@HierarchyRelationTypeKey char(16)
AS

SET ANSI_NULLS ON
SET ANSI_PADDING ON
SET ANSI_WARNINGS ON
SET ARITHABORT ON
SET CONCAT_NULL_YIELDS_NULL ON
SET QUOTED_IDENTIFIER ON
SET NO_BROWSETABLE OFF

	SELECT DISTINCT CT.Concept_Key, 
			CT.Item_Name, 
	  		CASE WHEN CR2.Concept_Relation_Key IS NULL THEN 0 ELSE 1 END AS HasChildren,
	  		CT.Concept_Rank_Key, 
			CT.Sort_Code,
			CT.PlainText  -- Required by the ORDER BY

	FROM 		VW_ConceptTerm CT
	LEFT JOIN 	Concept_Relation CR1 ON CR1.To_Concept_Key=CT.Concept_Key
	      				    AND CR1.Thesaurus_Relation_Type_Key=@HierarchyRelationTypeKey
			-- This is used to get the children. Join into Concept table to get only
			-- List_Preferred and Is_Current because these are the only one that will be 
			-- visible when expanded.
	LEFT JOIN 	(Concept_Relation CR2 
				INNER JOIN Concept AS C2 ON C2.Concept_Key = CR2.To_Concept_Key
							AND C2.List_Preferred = 1
							AND C2.Is_Current = 1) 
			ON CR2.From_Concept_Key=CT.Concept_Key
	       		AND CR2.Thesaurus_Relation_Type_Key=@HierarchyRelationTypeKey 
	WHERE 		CT.List_Preferred = 1
	AND 		CT.Is_Current = 1
	AND 		CT.Concept_Group_Key = @ConceptGroupKey
	AND 		CR1.From_Concept_Key IS NULL   -- i.e. No parents, therefore top level concept. 

	ORDER BY 	CT.Sort_Code, CT.PlainText  -- Use PlainText too, so list is alpha sorted when no Sort Codes.

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_Select_ForTopLevel') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Concept_Select_ForTopLevel'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Concept_Select_ForTopLevel TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForTopLevel TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForTopLevel TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForTopLevel TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Concept_Select_ForTopLevel TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Concept_Select_ForTopLevel TO [Dev - JNCC SQL]
END
GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Concept_Timestamp_Get]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Concept_Timestamp_Get]
GO

/*===========================================================================*\
  Description:  Get the timestamp for a Concept

  Parameters:   @Key - Concept Key
				@Timestamp - timestamp

  Created:		September 2004

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_Concept_Timestamp_Get] 
	@Key CHAR(16),
	@Timestamp TIMESTAMP OUTPUT
AS

	SELECT 	@Timestamp = [Timestamp]
	FROM	Concept
	WHERE	Concept_Key = @Key
		
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_Timestamp_Get') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_Concept_Timestamp_Get'
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
            GRANT EXECUTE ON dbo.usp_Concept_Timestamp_Get TO [R2k_AddOnly]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
        GRANT EXECUTE ON dbo.usp_Concept_Timestamp_Get TO [R2k_Administrator]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
        GRANT EXECUTE ON dbo.usp_Concept_Timestamp_Get TO [R2k_FullEdit]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
        GRANT EXECUTE ON dbo.usp_Concept_Timestamp_Get TO [R2k_ReadOnly]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
        GRANT EXECUTE ON dbo.usp_Concept_Timestamp_Get TO [R2k_RecordCardsOnly]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_Concept_Timestamp_Get TO [Dev - JNCC SQL]
END

GO

IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_DeterminationsEarthSciences_Select_ForSearch') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_DeterminationsEarthSciences_Select_ForSearch]
GO

/*===========================================================================*\
  Description:	Returns Concept_Key and DisplayTerm when search characters are 
		entered.

  Parameters:	@SearchText
		@UserDomainMask

  Created:	October 2003

  Last revision information:
	$Revision: 7 $
	$Date: 6/02/09 10:41 $
	$Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_DeterminationsEarthSciences_Select_ForSearch] 
	@SearchText VARCHAR(100),
	@UserDomainMask INT
AS

	SET NOCOUNT ON

	SELECT 		VW.Concept_Key AS Item_Key,
			VW.Item_Name + ' - ' + CG.Item_Name AS DisplayTerm,
			VW.Item_Name + ' - ' + CG.Item_Name AS SearchTerm, 
			CG.Item_Name

	FROM		VW_ConceptTerm AS VW 
	INNER JOIN 	Concept_Group CG ON CG.Concept_Group_Key = VW.Concept_Group_Key
	INNER JOIN 	Local_Domain LD ON LD.Local_Domain_Key = CG.Local_Domain_Key
	INNER JOIN 	Domain D ON D.Domain_Key = LD.Domain_Key
			AND ((D.Domain_Mask & @UserDomainMask > 0) OR (D.Domain_Mask = 0))
			AND D.Has_Occurrences = 1
	-- Join to find out which concepts are mapped to taxa
	LEFT JOIN	Taxon_Dictionary_Concept_Mapping TDCM ON TDCM.Concept_Key = VW.Concept_Key

	WHERE 		VW.PlainText LIKE @SearchText + '%'
	-- And filter out all concepts that are mapped to any taxon.
	AND		TDCM.Concept_Key IS NULL
	
	ORDER BY DisplayTerm
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_DeterminationsEarthSciences_Select_ForSearch') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_DeterminationsEarthSciences_Select_ForSearch'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_DeterminationsEarthSciences_Select_ForSearch TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_DeterminationsEarthSciences_Select_ForSearch TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_DeterminationsEarthSciences_Select_ForSearch TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_DeterminationsEarthSciences_Select_ForSearch TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_DeterminationsEarthSciences_Select_ForSearch TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_DeterminationsEarthSciences_Select_ForSearch TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Determination_Insert') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_Determination_Insert]
GO

/*===========================================================================*\
  Description:	Inserts a record in the Determination table.
		Ensures the Domain mask of the specimen is also updated.

  Parameters:	@Key 
		@DeterminedItemKey 
		@OccurrenceKey  
		@SpecimenCollectionUnitKey  
		@DeterminationTypeKey  
		@NomenclaturalStatusConceptKey 
		@Confidence
		@DeterminerNameKey 
		@InferredDeterminer 
		@DeterminerRoleKey 
		@VagueDateStart
		@VagueDateEnd 
		@VagueDateType 
		@UsedSpecimen 
		@Preferred
		@Method
		@Notes 
		@SessionID
		@IsForSpecimen		Indicates whether to update preferred 
					flag in Specimen_Unit or Determination.

  Created:	July 2003

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Determination_Insert]
	@Key char(16) output, 
	@DeterminedItemKey char(16), 
	@OccurrenceKey char(16), 
	@SpecimenCollectionUnitKey char(16), 
	@DeterminationTypeKey char(16), 
	@NomenclaturalStatusConceptKey char(16),
	@Confidence tinyint,
	@DeterminerNameKey char(16), 
	@InferredDeterminer tinyint,
	@DeterminerRoleKey char(16), 
	@VagueDateStart int, 
	@VagueDateEnd int, 
	@VagueDateType varchar(2),
	@UsedSpecimen bit,
	@Preferred bit,
	@Method text,
	@Notes text,
	@SessionID char(16),
	@IsForSpecimen bit
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF
	SET XACT_ABORT ON

	--Wrap everything in a transaction
	BEGIN TRANSACTION

		-- Get a new key.
		EXECUTE spNextKey 'Determination', @Key OUTPUT

		/*---------------------------------------------------------------------------------*\
		  Ensure only one preferred determination per occurrence.
		\*---------------------------------------------------------------------------------*/
		DECLARE	@PReferredForSpecimen bit
		SET	@PReferredForSpecimen = 0

		IF @IsForSpecimen = 1
		BEGIN
			-- Either Preferred passed in as true, or not preferred but no set in 
			-- Speciment Unit either.
			IF @Preferred = 1 
				SET @PreferredForSpecimen = 1

			-- Not used for Occurrence if for specimen, unless not already one present
			IF EXISTS(SELECT 1 FROM Determination WHERE Occurrence_Key = @OccurrenceKey AND Preferred = 1)
				SET @Preferred = 0
		END ELSE BEGIN
			-- If new determination not preferred, but there isn't one already, change the flag.
			IF @Preferred = 0 
			AND NOT EXISTS(SELECT * FROM Determination WHERE Occurrence_Key = @OccurrenceKey AND Preferred = 1)
				SET @Preferred = 1
			ELSE
			-- If new determination is preferred, make sure previous preferred is turned off.
			IF @Preferred = 1
			BEGIN
				UPDATE	Determination
				SET	Preferred = 0
				WHERE	Occurrence_Key = @OccurrenceKey
				AND	Preferred = 1

				IF @@Error <> 0 GOTO RollbackAndExit
			END
		END
		-- Force determination to be specimen's preferred if its the only one
		IF EXISTS ( SELECT * FROM Specimen_Unit 
							WHERE Collection_Unit_Key = @SpecimenCollectionUnitKey 
							AND Preferred_Determination_Key IS NULL )
			SET @PreferredForSpecimen = 1
		/*-------------------------------------------------------------*\
		  Do the table insert.
		\*-------------------------------------------------------------*/
		INSERT INTO Determination (
			Determination_Key, Concept_Key, Occurrence_Key, Specimen_Collection_Unit_Key,
			Determination_Type_Key, Nomenclatural_Status_Concept_Key,
			Confidence, Determiner_Name_Key, Inferred_Determiner,
			Determiner_Role_Key, Vague_Date_Start, Vague_Date_End,
			Vague_Date_Type, Used_Specimen, Preferred, Method, Notes,
			Entered_Session_ID
		) VALUES (
			@Key, @DeterminedItemKey, @OccurrenceKey, @SpecimenCollectionUnitKey,
			@DeterminationTypeKey, @NomenclaturalStatusConceptKey, 
			@Confidence, @DeterminerNameKey, @InferredDeterminer,
			@DeterminerRoleKey, @VagueDateStart, @VagueDateEnd,
			@VagueDateType, @UsedSpecimen, @Preferred, @Method, @Notes, 
			@SessionID
		)

		IF @@Error <> 0 GOTO RollbackAndExit


		IF @PreferredForSpecimen = 1
		BEGIN
			UPDATE	Specimen_Unit
			SET	Preferred_Determination_Key = @Key
			WHERE	Collection_Unit_Key = @SpecimenCollectionUnitKey

			IF @@Error <> 0 GOTO RollbackAndExit
		END

		IF @IsForSpecimen = 1
		BEGIN
			DECLARE @ConceptMask int

			/*-------------------------------------------------------------*\
			  Switch bit of new mask ON in Collection_Unit.
			\*-------------------------------------------------------------*/
			-- Retrieve the mask of the new concept.
			EXECUTE	usp_Get_Concept_Domain_Mask @DeterminedItemKey, @ConceptMask OUTPUT
			-- And switch appropriate bit ON in Collection_Unit
			EXECUTE	usp_CollectionUnit_Update_DomainMask @SpecimenCollectionUnitKey, @ConceptMask, 1

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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Determination_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Determination_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Determination_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Determination_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Determination_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Determination_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Determination_Insert TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_DomainMask_NextAvailable_Get]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_DomainMask_NextAvailable_Get]
GO

/*===========================================================================*\
  Description:	Goes through the possible Security Bits and returns the
				next available one. As soon as it gets to an available
				Security Bit, it quits the loop and the current
				Security Bit value is returned. If the value returned by this
				proc is 33, we can assume that all of the Domain Masks are
				in use.

  Parameters:	@SecurityBit OUTPUT

  Created:		September 2004

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_DomainMask_NextAvailable_Get]
	@SecurityBit int output
AS

SET NOCOUNT ON
	
	DECLARE @DomainMask INT
	
	SET 	@SecurityBit = 1
	
	WHILE @SecurityBit < 33
	BEGIN
		-- When @SecurityBit = 32 we get an overflow error when converting to the Domain Mask,
		-- so deal with that case later on
		IF @SecurityBit < 32
		BEGIN
			SET @DomainMask = Power(2, @SecurityBit - 1) 
			IF NOT EXISTS(SELECT *
							FROM Domain
							WHERE Domain_Mask = @DomainMask)
				BREAK
		END
		ELSE
		-- If @SecurityBit = 32
		BEGIN
			IF NOT EXISTS(SELECT *
							FROM Domain
							WHERE Domain_Mask = -2147483648)
				BREAK
		END		
			
		SET @SecurityBit = @SecurityBit + 1
	END

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_DomainMask_NextAvailable_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_DomainMask_NextAvailable_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_DomainMask_NextAvailable_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_DomainMask_NextAvailable_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_DomainMask_NextAvailable_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_DomainMask_NextAvailable_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_DomainMask_NextAvailable_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_DomainMask_NextAvailable_Get TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Domain_Delete]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Domain_Delete]
GO

/*===========================================================================*\
  Description:	Deletes a Domain record.

  Parameters:	@Key	Domain_Key
				@Timestamp

  Created:	December 2003

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Domain_Delete]
	@Key char(16),
	@Timestamp timestamp = NULL
AS

SET NOCOUNT ON

	BEGIN TRANSACTION
	
		DELETE 
		FROM 	Domain
		WHERE	Domain_Key = @Key
		AND		((@Timestamp = Timestamp) OR (@Timestamp IS NULL))

		IF @@Error <> 0 GOTO RollbackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Domain_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Domain_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Domain_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Domain_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Domain_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Domain_Delete TO [Dev - JNCC SQL]
END
GO

IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_EnquiryStats_Select') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_EnquiryStats_Select]
GO

/*===========================================================================*\
  Description:	Returns the number of enquiries against the departments they were made by.

  Parameters:	
	@VagueDateStart 	Start date to filter records by
	@VagueDateEnd		End date to filter records by

  Created:	January 2003

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_EnquiryStats_Select] 
	@VagueDateStart INT,
	@VagueDateEnd INT
AS
SET NOCOUNT ON

	IF @VagueDateStart = -1 SET @VagueDateStart = NULL
	IF @VagueDateEnd = -1 SET @VagueDateEnd = NULL

	/*---------------------------------------------*\
	  Get all enquiry types to build a total column
	\*---------------------------------------------*/
	DECLARE	@Sums varchar(2000),
		@Names varchar(1000)

	-- Default is to have at least a Total for all types.
	SET	@Sums = 'Cast(Count(E.Enquiry_Key) AS varchar(100))'
	SET	@Names = 'Total'

	DECLARE	@ETypeKey char(16), 
		@EType varchar(100)

	DECLARE curEnquiryType CURSOR FOR
		SELECT Concept_Key, PlainText FROM vw_ConceptTerm WHERE Concept_Group_Key = 'SYSTEM000000000A'
	OPEN curEnquiryType
	FETCH NEXT FROM curEnquiryType INTO @ETypeKey, @EType
	WHILE (@@Fetch_Status = 0) BEGIN
		-- Build up the SQL for the calculated value
		SET @Sums = @Sums + '+'',''+' +
			'Cast(Sum(CASE WHEN Enquiry_Type_Concept_Key = ''' + @ETypeKey + ''' THEN 1 ELSE 0 END) AS varchar(100))'
		-- Build up the column name out of all enquiry types.
		SET @Names = @Names + ',' + @EType

		FETCH NEXT FROM curEnquiryType INTO @ETypeKey, @EType
	END
	CLOSE curEnquiryType
	DEALLOCATE curEnquiryType

	/*---------------------------------------------*\
	  And run the query. Has to be a dynamic query.
	\*---------------------------------------------*/
	EXECUTE(
	'SELECT 	OD.Item_Name AS Department, ' + @Sums + ' AS [' + @Names + '] ' +
	'FROM		Organisation_Department OD ' +
	'LEFT JOIN 	(Individual I JOIN Enquiry E ON I.Name_Key = E.Answered_By_Name_Key) ' +
	'		ON OD.Organisation_Department_Key = I.Organisation_Department_Key ' +
	'LEFT JOIN	vw_ConceptTerm C ON C.Concept_Key = E.Enquiry_Type_Concept_Key ' +
	'GROUP BY OD.Item_Name ' +
	'ORDER BY OD.Item_Name'
	)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_EnquiryStats_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_EnquiryStats_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_EnquiryStats_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_EnquiryStats_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_EnquiryStats_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_EnquiryStats_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_EnquiryStats_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_EnquiryStats_Select TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ListSynonyms_Select_ForConcept]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ListSynonyms_Select_ForConcept]
GO

/*===========================================================================*\
  Description:	Returns List Synonyms

  Parameters:	@Key	Concept_Key

  Created:	December 2003

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ListSynonyms_Select_ForConcept]
	@Key char(16)
AS

SET NOCOUNT ON

	/*=============================*\
	  Get the list synonyms.
	\*=============================*/
	SELECT 		CListSynonyms.Concept_Key AS Item_Key,
			IsNull(T.Item_Name + ' ' + TV.Author_And_Date, T.Item_Name) AS Item_Name
	FROM 		Concept AS CSource
	INNER JOIN	Concept AS CListSynonyms 	ON CListSynonyms.Meaning_Key = CSource.Meaning_Key 
							AND CListSynonyms.Concept_Group_Key = CSource.Concept_Group_Key
							AND CListSynonyms.Concept_Key <> @Key
	INNER JOIN	Term AS T 			ON T.Term_Key = CListSynonyms.Term_Key
	LEFT JOIN	Term_Version AS TV 		ON TV.Term_Version_Key = CListSynonyms.Term_Version_Key
	WHERE 		CSource.Concept_key = @Key

	ORDER BY 	Item_Name

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ListSynonyms_Select_ForConcept') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ListSynonyms_Select_ForConcept'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ListSynonyms_Select_ForConcept TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ListSynonyms_Select_ForConcept TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ListSynonyms_Select_ForConcept TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ListSynonyms_Select_ForConcept TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ListSynonyms_Select_ForConcept TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ListSynonyms_Select_ForConcept TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_LocalDomain_Delete]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_LocalDomain_Delete]
GO

/*===========================================================================*\
  Description:	Deletes a Local_Domain record.

  Parameters:	@Key	Local_Domain_Key
				@Timestamp

  Created:	December 2003

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_LocalDomain_Delete]
	@Key char(16),
	@Timestamp timestamp = NULL
AS

SET NOCOUNT ON

	BEGIN TRANSACTION
	
		DELETE 
		FROM 	Local_Domain
		WHERE	Local_Domain_Key = @Key
		AND		((@Timestamp = Timestamp) OR (@Timestamp IS NULL))

		IF @@Error <> 0 GOTO RollbackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_LocalDomain_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_LocalDomain_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_LocalDomain_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_LocalDomain_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_LocalDomain_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_LocalDomain_Delete TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_MakeConceptsSynonyms_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_MakeConceptsSynonyms_Update]
GO

/*===========================================================================*\
  Description:	Take two Concept keys and make them synonyms.

  Parameters:	@ConceptKeySelected
		@ConceptKeyPasted

  Created:	December 2003

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_MakeConceptsSynonyms_Update]
	@ConceptKeySelected char(16),
	@ConceptKeyPasted char(16)
AS

SET NOCOUNT ON

	DECLARE @MeaningKeySelected char(16),
		@MeaningKeyPasted char(16),	
		@MeaningKeyChosen char(16),
		@MeaningKeyDiscarded char(16),
		@ConceptGroupKeySource char(16),
		@ConceptGroupKeyDest char(16),
		@SiteID char(8)

	BEGIN TRANSACTION

		/*=====================================================================*\
		  Get the meaning keys, and the Concept_Rank_Key of the target Concept.
		\*=====================================================================*/
		SELECT	@MeaningKeySelected = Meaning_Key
		FROM	Concept
		WHERE	Concept_Key = @ConceptKeySelected
	
		SELECT	@MeaningKeyPasted = Meaning_Key
		FROM	Concept
		WHERE	Concept_Key = @ConceptKeyPasted
	
		/*================================================================================*\
		  2 concepts that are in the custody of SYSTEM00 cannot be made synonyms of each 
		  other as the software expects both meaning keys to be present. 
		\*================================================================================*/
		IF NOT ((@ConceptKeySelected LIKE 'SYSTEM00%') AND (@ConceptKeyPasted LIKE 'SYSTEM00%'))
		BEGIN
			SELECT	@SiteID = Data
			FROM	Setting
			WHERE	[Name] = 'SiteID'

			/*==================================*\
			  Choose which meaning key to keep. 
			\*==================================*/	

			/*================================================================================*\
			  If 1 concept has a meaning key starting SYSTEM00, then both concepts are 
			  assigned to this key.
			\*================================================================================*/
			IF @MeaningKeySelected LIKE 'SYSTEM00%'
			BEGIN			
				SET @MeaningKeyChosen = @MeaningKeySelected
				SET @MeaningKeyDiscarded = @MeaningKeyPasted
			END
			ELSE IF @MeaningKeyPasted LIKE 'SYSTEM00%'
			BEGIN
				SET @MeaningKeyChosen = @MeaningKeyPasted
				SET @MeaningKeyDiscarded = @MeaningKeySelected
			END
	
			/*================================================================================*\
			  If neither concept has a meaning key starting SYSTEM00, but one of the meaning 
			  keys starts with the Site ID of the current site, then this is the meaning 
			  key adopted.
			\*================================================================================*/
			ELSE IF @MeaningKeySelected LIKE @SiteID + '%'
			BEGIN
				SET @MeaningKeyChosen = @MeaningKeySelected
				SET @MeaningKeyDiscarded = @MeaningKeyPasted
			END
			ELSE IF @MeaningKeyPasted LIKE @SiteID + '%'
			BEGIN
				SET @MeaningKeyChosen = @MeaningKeyPasted
				SET @MeaningKeyDiscarded = @MeaningKeySelected
			END
	
			/*================================================================================*\
			  If a single meaning key is still not selected by these rules, then the meaning 
			  key that belongs to the concept that will be list preferred after the synonymy 
			  is applied is selected.
			\*================================================================================*/
			
			-- Make sure usp_Concept_Update_ForPreferred is run before this stored proc.
			-- That way the preferred and list_preferred fields for the pasted concept
			-- will already have been updated.

			ELSE IF EXISTS (SELECT 	* 	
					FROM 	Concept
					WHERE 	Concept_Key = @ConceptKeySelected
					AND 	List_Preferred = 1)
	 		BEGIN
				SET @MeaningKeyChosen = @MeaningKeySelected
				SET @MeaningKeyDiscarded = @MeaningKeyPasted
			END
	
			ELSE IF EXISTS (SELECT 	* 	
					FROM 	Concept
					WHERE 	Concept_Key = @ConceptKeyPasted
					AND 	List_Preferred = 1)
	 		BEGIN
				SET @MeaningKeyChosen = @MeaningKeyPasted
				SET @MeaningKeyDiscarded = @MeaningKeySelected
			END
	
			/*================================================================================*\
			  If this still does not resolve a single meaning key, then the meaning key 
			  adopted is the one associated with the concept selected at the time of 
			  application of synonymy.
			\*================================================================================*/
			ELSE
			BEGIN
				SET @MeaningKeyChosen = @MeaningKeySelected
				SET @MeaningKeyDiscarded = @MeaningKeyPasted
			END
			
			/*===========================*\
			  Actually do the update.
			\*===========================*/		
			UPDATE	Concept
			SET	Meaning_Key = @MeaningKeyChosen
			WHERE	Meaning_Key = @MeaningKeyDiscarded

			/*=======================================================================*\
			  References to @MeaningKeyDiscarded need to be replaced with references
			  to @MeaningKeyChosen before the meaning record can be deleted.
			\*=======================================================================*/	
			UPDATE	Thesaurus_Fact
			SET	Meaning_Key = @MeaningKeyChosen
			WHERE	Meaning_Key = @MeaningKeyDiscarded

			UPDATE	Meaning_Relation
			SET	From_Meaning_Key = @MeaningKeyChosen
			WHERE	From_Meaning_Key = @MeaningKeyDiscarded

			UPDATE	Meaning_Relation
			SET	To_Meaning_Key = @MeaningKeyChosen
			WHERE	To_Meaning_Key = @MeaningKeyDiscarded

			UPDATE	Taxon_Dictionary_Meaning_Mapping
			SET	Meaning_Key = @MeaningKeyChosen
			WHERE	Meaning_Key = @MeaningKeyDiscarded

			/*========================================================================*\
			  Now all references to the old meaning key have gone, we can delete it 
			  (providing the two concepts aren't already synonyms. If that is the case, 
			  don't delete the meaing key.
			\*========================================================================*/
			IF @MeaningKeySelected <> @MeaningKeyPasted
				DELETE	Meaning
				WHERE	Meaning_Key = @MeaningKeyDiscarded
		
			/*===========================*\
			  Get the concept group keys.
			\*===========================*/
			SELECT	@ConceptGroupKeySource = Concept_Group_Key
			FROM	Concept
			WHERE	Concept_Key = @ConceptKeySelected
		
			SELECT	@ConceptGroupKeyDest = Concept_Group_Key
			FROM	Concept
			WHERE	Concept_Key = @ConceptKeyPasted
		
			/*===============================================================*\
			  If the source concept is in the same list as the dest concept, 
			  make it not list preferred.
			\*===============================================================*/
			IF @ConceptGroupKeySource = @ConceptGroupKeyDest
			BEGIN
				UPDATE	Concept
				SET	List_Preferred = 0
				WHERE	Concept_Key = @ConceptGroupKeySource
				AND	List_Preferred = 1
	
				DELETE	Concept_Lineage
				WHERE	Concept_Key = @ConceptGroupKeySource
			END
		END

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_MakeConceptsSynonyms_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_MakeConceptsSynonyms_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_MakeConceptsSynonyms_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_MakeConceptsSynonyms_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_MakeConceptsSynonyms_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_MakeConceptsSynonyms_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_MakeConceptsSynonyms_Update TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Measurements_Select_ForLocationFeature') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_Measurements_Select_ForLocationFeature]
GO

/*===========================================================================*\
  Description:	Returns measurement records for a specified Location Feature key.

  Parameters:	@LocationFeatureKey

  Created:	September 2004

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Measurements_Select_ForLocationFeature]
	@LocationFeatureKey char(16)
AS

SET NOCOUNT ON

	SELECT		LFD.Location_Feature_Data_Key AS [Item_Key], 
			-- Format 107 = "Mmm dd, yyyy".
			LFD.Applies_To + ' - ' + Convert(varchar, S.Date_Time_Start, 107) AS [Item_Name]
	FROM		Location_Feature_Data LFD
	INNER JOIN	Session S ON S.Session_ID = LFD.Entered_Session_ID
	WHERE		Location_Feature_Key = @LocationFeatureKey
	AND		Is_Descriptor = 0	-- Measurements only.
	ORDER BY 	Item_Name

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Measurements_Select_ForLocationFeature') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Measurements_Select_ForLocationFeature'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Measurements_Select_ForLocationFeature TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Measurements_Select_ForLocationFeature TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Measurements_Select_ForLocationFeature TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Measurements_Select_ForLocationFeature TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Measurements_Select_ForLocationFeature TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Measurements_Select_ForLocationFeature TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_PostImport_Collections]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_PostImport_Collections]
GO

/*===========================================================================*\
  Description:	Recreate constraints that impede import of collections data

  Created:	Oct 2004

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_PostImport_Collections]
AS

ALTER TABLE [dbo].[Specimen_Unit] ADD 
	CONSTRAINT [FK_Specimen_Unit_Determination] FOREIGN KEY 
	(
		[Preferred_Determination_Key]
	) REFERENCES [dbo].[Determination] (
		[Determination_Key]
	),
	CONSTRAINT [FK_Specimen_Unit_TAXON_DETERMINATION] FOREIGN KEY 
	(
		[Preferred_Taxon_Determination_Key]
	) REFERENCES [dbo].[TAXON_DETERMINATION] (
		[TAXON_DETERMINATION_KEY]
	)
GO

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_PostImport_Collections') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_PostImport_Collections'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_PostImport_Collections TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_PostImport_Collections TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_PostImport_Collections TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_PreImport_Collections]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_PreImport_Collections]
GO

/*===========================================================================*\
  Description:	Drop constraints that impede import of collections data

  Created:	Oct 2004

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_PreImport_Collections]
AS

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[FK_Specimen_Unit_Determination]') and OBJECTPROPERTY(id, N'IsForeignKey') = 1)
ALTER TABLE [dbo].[Specimen_Unit] DROP CONSTRAINT FK_Specimen_Unit_Determination
GO

if exists (select * from dbo.sysobjects where id = object_id(N'[dbo].[FK_Specimen_Unit_TAXON_DETERMINATION]') and OBJECTPROPERTY(id, N'IsForeignKey') = 1)
ALTER TABLE [dbo].[Specimen_Unit] DROP CONSTRAINT FK_Specimen_Unit_TAXON_DETERMINATION
GO

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_PreImport_Collections') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_PreImport_Collections'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_PreImport_Collections TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_PreImport_Collections TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_PreImport_Collections TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ReportBlocks_Select_ForSection]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ReportBlocks_Select_ForSection]
GO

/*===========================================================================*\
  Description:	Returns a list of the report block in a details report section.

  Parameters:	@Key - report section key

  Created:	Aug 2004

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ReportBlocks_Select_ForSection]
@Key CHAR(16)
AS

SET NOCOUNT ON

SELECT 
		RB.Report_Block_Key,
		RBIS.Title,
		RB.Header_File,
		RB.Row_File,
		RB.Footer_File,
		RBIS.Population_SQL,
		RBIS.Population_SQL_Record_Count
FROM Report_Block_In_Section RBIS
INNER JOIN Report_Block RB
		ON RB.Report_Block_Key=RBIS.Report_Block_Key
WHERE RBIS.Report_Section_Key=@Key
ORDER BY RBIS.[Sequence]

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ReportBlocks_Select_ForSection') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ReportBlocks_Select_ForSection'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ReportBlocks_Select_ForSection TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ReportBlocks_Select_ForSection TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ReportBlocks_Select_ForSection TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ReportBlocks_Select_ForSection TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ReportBlocks_Select_ForSection TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ReportBlocks_Select_ForSection TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SampleKey_Get') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_SampleKey_Get]
GO
/*===========================================================================*\
  Description:	
  Parameters:	

  Created:	July 2003

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SampleKey_Get]
	@Key char(16) OUTPUT,
	@VagueDateStart int,
	@VagueDateEnd int,
	@VagueDateType varchar(2),
	@SpatialRef varchar(40),
	@SpatialRefSystem varchar(4),
	@Lat float,
	@Long float,
	@SpatialRefQualifier varchar(20),
	@SampleTypeKey char(16),
	@LocationKey char(16),
	@SurveyKey char(16),
	@LocationName varchar(100)
AS
	/*----------------------------------------------------------------------------*\
	  Match exact list of Field Collectors if #TempFieldCollectors has been 
	  created by Collections Browser.
	\*----------------------------------------------------------------------------*/
	IF object_id('tempdb..#TempFieldCollectors') IS NOT NULL
	BEGIN
		-- Work out checksum of names to look for.
		DECLARE @ChkSm int
		SELECT @ChkSm = CheckSum_Agg(Checksum(Name_Key)) FROM #TempFieldCollectors

		-- Create a table variable to store the samples keys.
		DECLARE @Samples TABLE (Sample_Key char(16) COLLATE SQL_Latin1_General_CP1_CI_AS) 

		-- Select only the samples with ALL the names
		INSERT INTO @Samples (Sample_Key)
		SELECT DISTINCT S.Sample_Key
		FROM 	[Sample] S 
		JOIN 	Sample_Recorder SR ON SR.Sample_Key = S.Sample_Key 
		JOIN 	Survey_Event_Recorder SER ON SER.SE_Recorder_Key = SR.SE_Recorder_Key
		GROUP BY S.Sample_Key
		HAVING CheckSum_Agg(Checksum(SER.Name_Key)) = @ChkSm

		-- Now match on Spatial Ref, Sample Type, Date and Sample Recorders (through @Samples).
		SELECT 	@Key = S.Sample_Key
		FROM 	[Sample] AS S
		JOIN	@Samples AS SA ON S.Sample_Key = SA.Sample_Key
		JOIN 	Survey_Event AS SE ON SE.Survey_Event_Key = S.Survey_Event_Key
		WHERE	SE.Survey_Key = @SurveyKey
		AND	S.Sample_Type_Key = @SampleTypeKey
		AND	((S.Vague_Date_Start = @VagueDateStart AND S.Vague_Date_End = @VagueDateEnd AND S.Vague_Date_Type = @VagueDateType)
			OR 
			(S.Vague_Date_Type = 'U' AND (@VagueDateType='U' OR @VagueDateType IS NULL)))
		AND	IsNull(S.Location_Key, '') = IsNull(@LocationKey, '')
		AND	IsNull(S.Spatial_Ref, '') = IsNull(@SpatialRef, '')
		AND	IsNull(S.Spatial_Ref_System, '') = IsNull(@SpatialRefSystem, '')
		AND	IsNull(S.Location_Name, '') = IsNull(@LocationName, '')
	END
	ELSE
		/*---------------------------------------------------------------------*\
		  This matches on Spatial Ref, Sample Type and Date. I left this here  
		  in case it is required somewhere else and #TempFieldCollectors hasn't 
		  been created by the application.
		\*---------------------------------------------------------------------*/	
		SELECT 	@Key = Sample_Key
		FROM 	[Sample] AS S
		JOIN 	Survey_Event AS SE ON SE.Survey_Event_Key = S.Survey_Event_Key
		WHERE	SE.Survey_Key = @SurveyKey
		AND	S.Sample_Type_Key = @SampleTypeKey
		AND	((S.Vague_Date_Start = @VagueDateStart AND S.Vague_Date_End = @VagueDateEnd AND S.Vague_Date_Type = @VagueDateType)
			OR 
			(S.Vague_Date_Type = 'U' AND (@VagueDateType='U' OR @VagueDateType IS NULL)))
		AND	IsNull(S.Location_Key, '') = IsNull(@LocationKey, '')
		AND	IsNull(S.Spatial_Ref, '') = IsNull(@SpatialRef, '')
		AND	IsNull(S.Spatial_Ref_System, '') = IsNull(@SpatialRefSystem, '')
		AND	IsNull(S.Location_Name, '') = IsNull(@LocationName, '')
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SampleKey_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SampleKey_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SampleKey_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SampleKey_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SampleKey_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_SampleKey_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SampleKey_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SampleKey_Get TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_SampleRecorder_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_SampleRecorder_Update]
GO
/*===========================================================================*\
  Description:	Update a record in the Sample Recorder join table.
  Parameters:	@SampleKey char(16),
		@SERecorderKey char(16)

  Created:	August 2004

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SampleRecorder_Update]
	@SampleKey char(16),
	@SERecorderKey char(16)
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION

		UPDATE 	Sample_Recorder
		SET	Sample_Key = @SampleKey
		WHERE	SE_Recorder_Key = @SERecorderKey

		IF @@Error <> 0 GOTO RollBackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit:
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SampleRecorder_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SampleRecorder_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SampleRecorder_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SampleRecorder_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SampleRecorder_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SampleRecorder_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_SampleRecorder_Update TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Sample_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Sample_Update]
GO

/*===========================================================================*\
  Description:	Updates a record into the Sample table

  Parameters:	@Key
				@ConservationCheckKey 
				@ConservationJobKey
				@SetVagueDateStart
				@SetVagueDateEnd 
				@SetVagueDateType
				@Status
				@TypeConceptKey
				@Priority
				@Duration
				@DurationUnitConceptKey 
				@IdentifierNameKey
				@TaskAction
				@Comment
				@SessionID
				@Timestamp 

  Created:	August 2004

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Sample_Update]
	@Key char(16),
	@VagueDateStart int,
	@VagueDateEnd int,
	@VagueDateType varchar(2) = NULL,
	@SpatialRef varchar(40),
	@SpatialRefSystem varchar(4),
	@Lat float,
	@Long float,
	@SpatialRefQualifier varchar(20),
	@SampleTypeKey char(16) = NULL,
	@LocationKey char(16),
	@SurveyEventKey char(16),
	@Comment text,
	@ChangedBy char(16),
	@LocationName varchar(100)
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION
		
		UPDATE 	[Sample]
		SET 	Vague_Date_Start = @VagueDateStart, 
				Vague_Date_End = @VagueDateEnd, 
				Vague_Date_Type = @VagueDateType,
				Spatial_Ref = @SpatialRef, 
				Spatial_Ref_System = @SpatialRefSystem, 
				Spatial_Ref_Qualifier = @SpatialRefQualifier,
				Lat = @Lat, 
				Long = @Long, 
				Location_Key = @LocationKey,
				Sample_Type_Key = @SampleTypeKey,
				Survey_Event_Key= @SurveyEventKey,
				Comment = @Comment,
				Location_Name = @LocationName,
				Changed_By = @ChangedBy,
				Changed_Date = GetDate()
		WHERE	Sample_Key = @Key

		IF @@Error <> 0 GOTO RollbackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Sample_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Sample_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        GRANT EXECUTE ON dbo.usp_Sample_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Sample_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Sample_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Sample_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_Sample_Update TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SpecimensMailMerge_Select') AND SysStat & 0xf = 4)
    DROP PROCEDURE [dbo].[usp_SpecimensMailMerge_Select]
GO

/*===========================================================================*\
  Description:	Returns informatino about the specified specimens for a 
		Specimens Mail Merge Output report.

  Parameters:	Use #SpecimensMailMergeKeys as source.

  Created:	September 2004

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SpecimensMailMerge_Select]
AS

	SELECT		dbo.ufn_GetRegNumber(specimen.Collection_Unit_Key) AS RegNo, 
			CASE WHEN specimen.Life_Sciences = 0 THEN ISNULL(CT.Item_Name, 'No Determination') 
			ELSE ISNULL(dbo.ufn_GetFormattedTaxonNameByParams( 
				ITN.Actual_Name, ITN.Actual_Name_Italic, ITN.Common_Name, 
				ITN.Common_Name_Italic, ITN.Authority, 1), 'No Determination') 
			END AS Determination, 
			CASE WHEN specimen.Life_Sciences = 0 THEN ISNULL(CT.Item_Name, 'No Determination') 
			ELSE ISNULL(ITN.Actual_Name, 'No Determination') 
			END AS PlainDetermination, 
			dbo.ufn_GetFieldCollectors(specimen.Collection_Unit_Key) AS FieldCollector, 
			dbo.ufn_GetSpecimenGatheringSite(specimen.Collection_Unit_Key) AS GatheringSite, 
			dbo.ufn_GetDateFromVagueDate( 
			S.Vague_Date_Start, S.Vague_Date_End, S.Vague_Date_Type) AS GatheringDate 

	FROM  		Specimen_Unit AS specimen 
	LEFT JOIN 	Determination D ON D.Determination_Key = specimen.Preferred_Determination_Key 
	LEFT JOIN 	Occurrence O ON O.Occurrence_Key = D.Determination_Key 
	LEFT JOIN 	VW_ConceptTermPreferred CT ON CT.Concept_Key = D.Concept_Key 
	LEFT JOIN 	Taxon_Determination TD ON specimen.Preferred_Taxon_Determination_Key = TD.Taxon_Determination_Key 
	LEFT JOIN 	Taxon_Occurrence XO ON XO.Taxon_Occurrence_Key = TD.Taxon_Determination_Key 
	LEFT JOIN 	Index_Taxon_Name ITN ON ITN.Taxon_List_Item_Key = TD.Taxon_List_Item_Key 
	LEFT JOIN 	[Sample] S ON S.Sample_Key IN (XO.Sample_Key, O.Sample_Key) 

	WHERE		specimen.Collection_Unit_Key IN (SELECT	Specimen_Unit_Key FROM #TempSpecimenKeys)

	ORDER BY	PlainDetermination

	FOR XML AUTO
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SpecimensMailMerge_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SpecimensMailMerge_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SpecimensMailMerge_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SpecimensMailMerge_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SpecimensMailMerge_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_SpecimensMailMerge_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SpecimensMailMerge_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SpecimensMailMerge_Select TO [Dev - JNCC SQL]
END
GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Specimens_Select_ForJob]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Specimens_Select_ForJob]
GO

/*===========================================================================*\
  Description:	Returns Specimens associated with a specified Job.

  Parameters:
	@UserDomainMask		User's Domain Mask restricting which records may be returned
	@SessionID 		User's SessionID
	@ParentKey 		Only the records associated with the parent key are returned
	@ShowCommonNames	Specifies whether or not Common Names should be shown
	@SortOrderIndex		Index determining Sort Order

  Created:	August 2003

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Specimens_Select_ForJob] 
	@UserDomainMask INT,
	@SessionID CHAR(16),
	@ParentKey CHAR(16),
	@ShowCommonNames BIT,
	@SortOrderIndex TINYINT
AS

SET NOCOUNT ON
-- Set of options for better use of vw_ConceptTerm.
SET ANSI_NULLS ON
SET ANSI_PADDING ON
SET ANSI_WARNINGS ON
SET ARITHABORT ON
SET CONCAT_NULL_YIELDS_NULL ON
SET QUOTED_IDENTIFIER ON
SET NO_BROWSETABLE OFF

	DECLARE	@Result TABLE	(
		[Item_Key] [char] (16) COLLATE database_default NOT NULL,
		[Det_Item_Key] [char] (16) COLLATE database_default  NULL,
		[Item_Name] [varchar] (150) COLLATE database_default NULL,
		[PlainText] [varchar] (150) COLLATE database_default NULL,
		[Life_Sciences] BIT, 
		[Number] [varchar] (30) COLLATE database_default NULL,
		[Join_Key] [varchar] (16) COLLATE database_default NULL
	)

	INSERT INTO @Result (Item_Key, Det_Item_Key, Item_Name, PlainText, Life_Sciences, Number, Join_Key)
	SELECT 	DISTINCT SU.Collection_Unit_Key						COLLATE database_default,
			CASE 	
				WHEN SU.Life_Sciences=0 THEN CP.Concept_Key		COLLATE database_default 
				ELSE ITN.Taxon_List_Item_Key					COLLATE database_default
			END,
			CASE 	
				WHEN SU.Life_Sciences=0 THEN CP.Item_Name		COLLATE database_default
				ELSE dbo.ufn_GetFormattedTaxonNameByParams(ITN.Actual_Name, ITN.Actual_Name_Italic, ITN.Common_Name, 
					ITN.Common_Name_Italic, ITN.Authority, @ShowCommonNames) COLLATE database_default
	    	END,
			CASE 	
				WHEN SU.Life_Sciences=0 THEN CP.PlainText		COLLATE database_default
				WHEN @ShowCommonNames = 1 THEN ITN.Common_Name	COLLATE database_default
				ELSE ITN.Actual_Name							COLLATE database_default
	    	END,
			SU.Life_Sciences, Number, CUT.Collection_Unit_Task_Key COLLATE database_default

	FROM 		Conservation_Job CJ
	INNER JOIN	Conservation_Task CT ON CJ.Conservation_Job_Key = CT.Conservation_Job_Key AND CJ.Conservation_Job_Key = @ParentKey
	INNER JOIN	Collection_Unit_Task CUT ON CT.Conservation_Task_Key = CUT.Conservation_Task_Key
	INNER JOIN	Specimen_Unit SU ON CUT.Collection_Unit_Key = SU.Collection_Unit_Key
	INNER JOIN	Collection_Unit CU 
		ON SU.Collection_Unit_Key = CU.Collection_Unit_Key
		AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID)
			OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))

	LEFT JOIN 	Collection_Unit_Number CUN 
		ON SU.Collection_Unit_Key = CUN.Collection_Unit_Key
		AND CUN.Preferred = 1 AND CUN.Type_Concept_Key = 'SYSTEM0000000001'

	LEFT JOIN 	Determination D ON SU.Preferred_Determination_Key = D.Determination_Key
	LEFT JOIN 	VW_ConceptTermPreferred CP ON D.Concept_Key = CP.Concept_Key 
	LEFT JOIN 	Taxon_Determination TD ON SU.Preferred_Taxon_Determination_Key = TD.Taxon_Determination_Key
	LEFT JOIN 	Index_Taxon_Name ITN ON TD.Taxon_List_Item_Key = ITN.Taxon_List_Item_Key

	IF @SortOrderIndex = 0
		SELECT * FROM @Result ORDER BY PlainText, Number
	ELSE
		SELECT * FROM @Result ORDER BY Number, PlainText
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Specimens_Select_ForJob') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Specimens_Select_ForJob'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForJob TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForJob TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForJob TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForJob TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForJob TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForJob TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Specimens_Select_ForSearch]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Specimens_Select_ForSearch]
GO

/*===========================================================================*\
  Description:	Returns Collection_Unit_Key and DisplayTerm when search characters 
		are entered. The Specimen_Unit table does not have a Display_Caption 
		or Search_Caption field, so the caption must be constructed through 
		joins to other tables.

  Parameters:	@UserDomainMask		User's Domain Mask restricting which records may be returned.
		@SessionID 		User's SessionID.
		@SearchText 		Search text used to find collections.

  Created:	September 2003

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Specimens_Select_ForSearch] 
	@UserDomainMask int,
	@SessionID char(16),
	@ShowCommonNames BIT,
	@SearchText varchar(100)
AS

SET NOCOUNT ON


--Use temp table to build output, so silly data errors such as duplicate accessions
-- don't duplicate in the list
DECLARE @SpecimensSearch TABLE
(
	[Item_Key] [char] (16) COLLATE database_default NULL,
	[DisplayTerm] [nvarchar] (150) COLLATE database_default NULL,
	[SearchTerm] [nvarchar] (150) COLLATE database_default NULL,
	[Life_Sciences] [bit] NULL
)

--Find all specimens with a determination match
INSERT INTO 
	@SpecimensSearch (Item_Key, Life_Sciences, SearchTerm, DisplayTerm) 
SELECT DISTINCT 
	SU.Collection_Unit_Key					COLLATE database_default, 
	SU.Life_Sciences,
	CASE Su.Life_Sciences 
		WHEN 0 THEN TSearch.Plaintext		COLLATE database_default
		ELSE ITN.Actual_Name				COLLATE database_default
	END AS SearchTerm,
	CASE Su.Life_Sciences 
		WHEN 0 THEN TSearch.Item_Name		COLLATE database_default
		ELSE CASE ITN.Actual_Name_Italic
			WHEN 1 THEN '<i>' + ITN.Actual_Name + '</i>' COLLATE database_default
			ELSE ITN.Actual_Name			COLLATE database_default
		END
	END AS DisplayTerm
	
FROM SPECIMEN_UNIT SU
INNER JOIN Collection_Unit CU ON SU.Collection_Unit_Key = CU.Collection_Unit_Key 
 	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
	OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
LEFT JOIN VW_SpecimenDetsEarth SDE ON SU.Collection_Unit_Key = SDE.Collection_Unit_Key
LEFT JOIN Concept C ON SDE.Concept_Key = C.Concept_Key
LEFT JOIN Concept CSearch ON CSearch.Meaning_Key=C.Meaning_Key
LEFT JOIN Term TSearch ON TSearch.Term_Key=CSearch.Term_Key
LEFT JOIN VW_SpecimenDetsLife SDL ON SU.Collection_Unit_Key = SDL.Collection_Unit_Key
LEFT JOIN Index_Taxon_Synonym ITS ON ITS.Taxon_List_Item_Key=SDL.Taxon_List_Item_Key
LEFT JOIN INDEX_TAXON_NAME ITN	ON ITS.Synonym_List_Item_Key = ITN.Taxon_List_Item_Key
WHERE 
	(TSearch.Plaintext LIKE @SearchText + '%' AND SU.Life_Sciences=0) 
	OR 
	(ITN.Actual_Name LIKE @SearchText + '%' AND SU.Life_Sciences=1)

-- Update the number in case there are 2 registrations for a specimen, so we don't duplicate
-- the rows in the output results.
UPDATE @SpecimensSearch
SET 
		SearchTerm = SearchTerm + ' - ' + CUN.Number,
		DisplayTerm = DisplayTerm + ' - ' + CUN.Number
FROM @SpecimensSearch SU
INNER JOIN Collection_Unit_Number CUN ON SU.Item_key = CUN.Collection_Unit_Key 
		AND CUN.Type_Concept_Key = 'SYSTEM0000000001' AND CUN.Preferred=1
INNER JOIN Concept C ON C.Concept_Key=CUN.Type_Concept_Key
	AND C.Meaning_Key='SYSTEM0000000001'

-- Select table and sort appropriately
SELECT * from @SpecimensSearch
ORDER BY SearchTerm

GO




/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Specimens_Select_ForSearch') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Specimens_Select_ForSearch'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearch TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearch TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearch TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearch TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearch TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForSearch TO [Dev - JNCC SQL]
END
GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Specimens_Select_ForValuation]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Specimens_Select_ForValuation]
GO

/*===========================================================================*\
  Description:	Returns Specimens data for a given Valuations.

  Parameters:
	@ParentKey 		Only the records associated with the parent key are returned
	@UserID			Name_Key of current user
	@ShowCommonNames	Specifies whether or not Common Names should be shown
	@SortOrderIndex		Index determining Sort Order

  Created:	August 2003

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Specimens_Select_ForValuation] 
	@UserDomainMask INT,
	@SessionID CHAR(16),
	@ParentKey CHAR(16),
	@UserID CHAR(16),
	@ShowCommonNames BIT,
	@SortOrderIndex TINYINT
AS

SET NOCOUNT ON
-- Set of options for better use of vw_ConceptTerm.
SET ANSI_NULLS ON
SET ANSI_PADDING ON
SET ANSI_WARNINGS ON
SET ARITHABORT ON
SET CONCAT_NULL_YIELDS_NULL ON
SET QUOTED_IDENTIFIER ON
SET NO_BROWSETABLE OFF

	SELECT 		DISTINCT
			SU.Collection_Unit_Key AS Item_Key, 
			CUV.Collection_Unit_Valuation_Key AS Join_Key,
			CASE 	WHEN SU.Life_Sciences=0 THEN CT.Concept_Key 
				ELSE ITN.Taxon_List_Item_Key 
			END AS Det_Item_Key,
			CASE 	WHEN SU.Life_Sciences=0 THEN CT.Item_Name 
				ELSE dbo.ufn_GetFormattedTaxonNameByParams(ITN.Actual_Name, ITN.Actual_Name_Italic, ITN.Common_Name, 
					ITN.Common_Name_Italic, ITN.Authority, @ShowCommonNames) 
			END AS Item_Name,
			SU.Life_Sciences, 
			CUN.Number,
			CASE @SortOrderIndex WHEN 0 THEN Item_Name ELSE CUN.Number END AS Sort1, 
			CASE @SortOrderIndex WHEN 0 THEN CUN.Number ELSE Item_Name END AS Sort2

	FROM 		Collection_Unit_Valuation CUV
	INNER JOIN	[User] U ON U.Name_Key = @UserID 
	INNER JOIN	Specimen_Unit SU ON CUV.Collection_Unit_Key = SU.Collection_Unit_Key AND CUV.Valuation_Key = @ParentKey
	INNER JOIN	Collection_Unit CU 
		ON SU.Collection_Unit_Key = CU.Collection_Unit_Key
		AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
			OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))

	LEFT JOIN 	Collection_Unit_Number CUN 
		ON SU.Collection_Unit_Key = CUN.Collection_Unit_Key
		AND CUN.Preferred = 1 AND CUN.Type_Concept_Key = 'SYSTEM0000000001'

	LEFT JOIN 	Determination D ON SU.Preferred_Determination_Key = D.Determination_Key
	LEFT JOIN 	VW_ConceptTermPreferred CT ON D.Concept_Key = CT.Concept_Key 
	LEFT JOIN 	Taxon_Determination TD 	ON SU.Preferred_Taxon_Determination_Key = TD.Taxon_Determination_Key
	LEFT JOIN 	Index_Taxon_Name ITN 	ON TD.Taxon_List_Item_Key = ITN.Taxon_List_Item_Key

	WHERE U.Allow_Finance = 1

	ORDER BY
		-- 0: Item_Name, CUN.Number
		-- 1: CUN.Number, Item_Name
		Sort1, Sort2
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Specimens_Select_ForValuation') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Specimens_Select_ForValuation'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForValuation TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForValuation TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForValuation TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForValuation TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Specimens_Select_ForValuation TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Specimens_Select_ForValuation TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_StoreDiagram_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_StoreDiagram_Update]
GO

/*===========================================================================*\
  Description:	Updates a Diagram for a store

  Parameters:	@Key
		@XML
		@Timestamp

  Created:	Sept 2004

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_StoreDiagram_Update]
	@Key char(16),
	@XML text,
	@Timestamp timestamp
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION

		UPDATE	Store
		SET	Diagram_XML = @Xml
		WHERE	Collection_Unit_Key = @Key
		AND	(@Timestamp = Timestamp)

		IF @@Error <> 0 GOTO RollbackAndExit	

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_StoreDiagram_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_StoreDiagram_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_StoreDiagram_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_StoreDiagram_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_StoreDiagram_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_StoreDiagram_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_StoreDiagram_Update TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_StoreFullySpecified_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_StoreFullySpecified_Select]
GO

/*===========================================================================*\
  Description:	Returns a fully specified store name, including code and parent

  Parameters:	@SearchText

  Created:	Sept 2004

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_StoreFullySpecified_Select]
@ItemKey char(16),
@Output varchar(500) output
AS
	
SELECT
	@Output=S.Item_Name + 
		IsNull(' - ' + CU.Current_Location_Code, IsNull(' - ' + CU.Usual_Location_Code, '')) + 
		IsNull(', ' + dbo.ufn_GetTranslation_String_From_Value(0, 'IN') + ' ' + SP.Item_Name + 
		IsNull(' - ' + CUP.Current_Location_Code, IsNull(' - ' + CUP.Usual_Location_Code, '')), '')
FROM Store S
INNER JOIN Collection_Unit CU ON CU.Collection_Unit_Key=S.Collection_Unit_Key
LEFT JOIN Store SP ON SP.Collection_Unit_Key=CU.Current_Container_Collection_Unit_Key
LEFT JOIN Collection_Unit CUP ON CUP.Collection_Unit_Key=SP.Collection_Unit_Key
WHERE S.Collection_Unit_Key=@ItemKey


GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_StoreFullySpecified_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_StoreFullySpecified_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_StoreFullySpecified_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_StoreFullySpecified_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_StoreFullySpecified_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_StoreFullySpecified_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_StoreFullySpecified_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_StoreFullySpecified_Select TO [Dev - JNCC SQL]
END

GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_StoreHierarchy_Child_Select_ForStore]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_StoreHierarchy_Child_Select_ForStore]
GO

/*===========================================================================*\
  Description:	Returns successive child Stores for a specified Store

  Parameters:	
	@ParentKey 	Only the records associated with the parent key are returned
	@UserDomainMask	User's Domain Mask restricting which records may be returned
	@SessionID 	User's SessionID

  Created:	February 2004

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_StoreHierarchy_Child_Select_ForStore] 
	@UserDomainMask INT,
	@SessionID CHAR(16),
	@SortOrderIndex TINYINT,
	@ParentKey CHAR(16)
AS

SET NOCOUNT ON

	DECLARE @StoreHierarchy TABLE
	(
		[Collection_Unit_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
		[Item_Name] [varchar] (150) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
		[Number] [varchar] (30) NULL,
		[Bottom_Level] [bit] NOT NULL
	)

	--Obtain first generation children
	INSERT INTO @StoreHierarchy (Collection_Unit_Key, Item_Name, Number, Bottom_Level) 
	SELECT 		S.Collection_Unit_Key, 
				S.Item_Name + IsNull(' - ' + CU.Current_Location_Code, IsNull(' - ' + CU.Usual_Location_Code, '')) AS Item_Name, 
				CUN.Number, 0
	FROM 		Store S
	INNER JOIN 	Collection_Unit CU ON S.Collection_Unit_Key = CU.Collection_Unit_Key
				AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
					OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
				AND CU.Current_Container_Collection_Unit_Key = @ParentKey

	LEFT JOIN 	Collection_Unit_Number CUN ON S.Collection_Unit_Key = CUN.Collection_Unit_Key 
				AND CUN.Type_Concept_Key = 'SYSTEM0000000001'  --REGISTRATION NUMBER
				AND CUN.Preferred = 1

	UPDATE		SH
	SET			SH.Bottom_Level = CASE WHEN S.Collection_Unit_Key IS NULL THEN 1 ELSE 0 END
	FROM		@StoreHierarchy SH
	LEFT JOIN 	Collection_Unit CU ON SH.Collection_Unit_Key = CU.Current_Container_Collection_Unit_Key
					AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
					OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
	-- Need to do a join onto Store because other Collection_Units could have their current container
	-- as this store. However, we are only interested in the Stores at the moment.
	LEFT JOIN Store as S on S.Collection_Unit_Key = CU.Collection_Unit_Key

	--Return hierarchical list
	IF @SortOrderIndex = 0
		SELECT	Collection_Unit_Key AS Item_Key, Collection_Unit_Key AS Join_Key, Item_Name, Number, Bottom_Level
		FROM	@StoreHierarchy
		ORDER BY Item_Name, Number
	ELSE 
		SELECT	Collection_Unit_Key AS Item_Key, Collection_Unit_Key AS Join_Key, Item_Name, Number, Bottom_Level
		FROM	@StoreHierarchy
		ORDER BY Number, Item_Name
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_StoreHierarchy_Child_Select_ForStore') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_StoreHierarchy_Child_Select_ForStore'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_StoreHierarchy_Child_Select_ForStore TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_StoreHierarchy_Child_Select_ForStore TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_StoreHierarchy_Child_Select_ForStore TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_StoreHierarchy_Child_Select_ForStore TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_StoreHierarchy_Child_Select_ForStore TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_StoreHierarchy_Child_Select_ForStore TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_StoreHierarchy_Select_ForSearchByName]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_StoreHierarchy_Select_ForSearchByName]
GO

/*===========================================================================*\
  Description:	Returns top level Store Hierarchy nodes with matching Store
				names. To qualify as a top level node, they have to have no 
				current location.

  Parameters:	@UserDomainMask		User's Domain Mask restricting which 
									records may be returned
				@SessionID 			User's SessionID
				@SortOrderIndex		Index determining Sort Order
				@SearchText			Text to match on

  Created:		September 2004

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_StoreHierarchy_Select_ForSearchByName] 
@UserDomainMask INT,
@SessionID CHAR(16),
@SortOrderIndex TINYINT,
@SearchText VARCHAR(100)

AS

	SET NOCOUNT ON
	
	-- Create a table variable to put the initial results into. We use the results in this
	-- table to work out whether there are any stores contained within it.
	DECLARE @StoreHierarchy TABLE
	(
		[Collection_Unit_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
		[Item_Name] [varchar] (150),
		[Number] [varchar] (30) NULL,
		[Bottom_Level] [bit] NOT NULL,
		[Current_Location_Code] [varchar] (30) NULL
	)
	
	-- Insert the initials results into the table variable.
	INSERT INTO @StoreHierarchy (Collection_Unit_Key, Item_Name, Number, Bottom_Level, Current_Location_Code) 
	SELECT 		S.Collection_Unit_Key AS Item_Key, 
				S.Item_Name + IsNull(' - ' + CU.Current_Location_Code, IsNull(' - ' + CU.Usual_Location_Code, '')) AS Item_Name,  
				Number,
				0,
				CU.Current_Location_Code
	FROM 		Store AS S
	INNER JOIN	Collection_Unit AS CU ON CU.Collection_Unit_Key = S.Collection_Unit_Key
	    			AND ((CU.Domain_Mask & @UserDomainMask > 0) 
						OR (CU.Entered_Session_ID = @SessionID) 
						OR (CU.Changed_Session_ID = @SessionID) 
						OR (CU.Domain_Mask = 0))
					AND S.Item_Name LIKE @SearchText + '%'
	LEFT JOIN	Collection_Unit_Number AS CUN ON CUN.Collection_Unit_Key = S.Collection_Unit_Key 
					AND CUN.Type_Concept_Key = 'SYSTEM0000000001'  --REGISTRATION NUMBER
					AND CUN.Preferred = 1
	WHERE		CU.Current_Container_Collection_Unit_Key IS NULL
	
	-- Work out whether the top level node is also the bottom level node.
	UPDATE		SH
	SET			SH.Bottom_Level = CASE WHEN S.Collection_Unit_Key IS NULL THEN 1 ELSE 0 END
	FROM		@StoreHierarchy AS SH
	LEFT JOIN 	Collection_Unit CU ON CU.Current_Container_Collection_Unit_Key = SH.Collection_Unit_Key 
					AND ((CU.Domain_Mask & @UserDomainMask > 0) 
						OR (CU.Entered_Session_ID = @SessionID) 
						OR (CU.Changed_Session_ID = @SessionID) 
						OR (CU.Domain_Mask = 0))
				-- Need to do a join onto Store because other Collection_Units could have their current container
				-- as this store. However, we are only interested in the Stores at the moment.
	LEFT JOIN 	Store AS S ON S.Collection_Unit_Key = CU.Collection_Unit_Key
	
	-- Select the results for the result set.
	SELECT		Collection_Unit_Key AS Item_Key, 
				Collection_Unit_Key AS Join_Key, 
				Item_Name, 
				Number, 
				Bottom_Level
	FROM		@StoreHierarchy
	ORDER BY 	CASE @SortOrderIndex WHEN 0 THEN Item_Name 
									 WHEN 1 THEN Number
									 WHEN 2 THEN Current_Location_Code
				END,
				CASE @SortOrderIndex WHEN 0 THEN Number
									 WHEN 1 THEN Item_Name
									 WHEN 2 THEN Item_Name
				END,
				CASE @SortOrderIndex WHEN 2 THEN Number
				END

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_StoreHierarchy_Select_ForSearchByName') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_StoreHierarchy_Select_ForSearchByName'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_StoreHierarchy_Select_ForSearchByName TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_StoreHierarchy_Select_ForSearchByName TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_StoreHierarchy_Select_ForSearchByName TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_StoreHierarchy_Select_ForSearchByName TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_StoreHierarchy_Select_ForSearchByName TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_StoreHierarchy_Select_ForSearchByName TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_StoreHierarchy_Select_ForSearchByRegistrationNumber]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_StoreHierarchy_Select_ForSearchByRegistrationNumber]
GO

/*===========================================================================*\
  Description:	Returns top level Store Hierarchy nodes with matching Store
				names. To qualify as a top level node, they have to have no 
				current location.

  Parameters:	@UserDomainMask		User's Domain Mask restricting which 
									records may be returned
				@SessionID 			User's SessionID
				@SortOrderIndex		Index determining Sort Order
				@SearchText			Registration number to match on

  Created:		September 2004

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_StoreHierarchy_Select_ForSearchByRegistrationNumber] 
@UserDomainMask INT,
@SessionID CHAR(16),
@SortOrderIndex TINYINT,
@SearchText VARCHAR(30)

AS

	SET NOCOUNT ON
	
	-- Create a table variable to put the initial results into. We use the results in this
	-- table to work out whether there are any stores contained within it.
	DECLARE @StoreHierarchy TABLE
	(
		[Collection_Unit_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
		[Item_Name] [varchar] (150),
		[Number] [varchar] (30) NULL,
		[Bottom_Level] [bit] NOT NULL,
		[Current_Location_Code] [varchar] (30) NULL
	)
	
	-- Insert the initials results into the table variable.
	INSERT INTO @StoreHierarchy (Collection_Unit_Key, Item_Name, Number, Bottom_Level, Current_Location_Code) 
	SELECT 		S.Collection_Unit_Key AS Item_Key, 
				S.Item_Name + IsNull(' - ' + CU.Current_Location_Code, IsNull(' - ' + CU.Usual_Location_Code, '')) AS Item_Name, 
				Number,
				0,
				CU.Current_Location_Code
	FROM 		Store AS S
	INNER JOIN	Collection_Unit AS CU ON CU.Collection_Unit_Key = S.Collection_Unit_Key
	    			AND ((CU.Domain_Mask & @UserDomainMask > 0) 
						OR (CU.Entered_Session_ID = @SessionID) 
						OR (CU.Changed_Session_ID = @SessionID) 
						OR (CU.Domain_Mask = 0))
	LEFT JOIN	Collection_Unit_Number AS CUN ON CUN.Collection_Unit_Key = S.Collection_Unit_Key 
					AND CUN.Type_Concept_Key = 'SYSTEM0000000001'  --REGISTRATION NUMBER
					AND CUN.Preferred = 1
	WHERE		CU.Current_Container_Collection_Unit_Key IS NULL
	AND			CUN.Number LIKE @SearchText + '%'
	
	-- Work out whether the top level node is also the bottom level node.
	UPDATE		SH
	SET			SH.Bottom_Level = CASE WHEN S.Collection_Unit_Key IS NULL THEN 1 ELSE 0 END
	FROM		@StoreHierarchy AS SH
	LEFT JOIN 	Collection_Unit CU ON CU.Current_Container_Collection_Unit_Key = SH.Collection_Unit_Key 
					AND ((CU.Domain_Mask & @UserDomainMask > 0) 
						OR (CU.Entered_Session_ID = @SessionID) 
						OR (CU.Changed_Session_ID = @SessionID) 
						OR (CU.Domain_Mask = 0))
				-- Need to do a join onto Store because other Collection_Units could have their current container
				-- as this store. However, we are only interested in the Stores at the moment.
	LEFT JOIN 	Store AS S ON S.Collection_Unit_Key = CU.Collection_Unit_Key
	
	-- Select the results for the result set.
	SELECT		Collection_Unit_Key AS Item_Key, 
				Collection_Unit_Key AS Join_Key, 
				Item_Name, 
				Number, 
				Bottom_Level
	FROM		@StoreHierarchy
	ORDER BY 	CASE @SortOrderIndex WHEN 0 THEN Item_Name 
									 WHEN 1 THEN Number
									 WHEN 2 THEN Current_Location_Code
				END,
				CASE @SortOrderIndex WHEN 0 THEN Number
									 WHEN 1 THEN Item_Name
									 WHEN 2 THEN Item_Name
				END,
				CASE @SortOrderIndex WHEN 2 THEN Number
				END

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_StoreHierarchy_Select_ForSearchByRegistrationNumber') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_StoreHierarchy_Select_ForSearchByRegistrationNumber'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_StoreHierarchy_Select_ForSearchByRegistrationNumber TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_StoreHierarchy_Select_ForSearchByRegistrationNumber TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_StoreHierarchy_Select_ForSearchByRegistrationNumber TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_StoreHierarchy_Select_ForSearchByRegistrationNumber TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_StoreHierarchy_Select_ForSearchByRegistrationNumber TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_StoreHierarchy_Select_ForSearchByRegistrationNumber TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_StoreHierarchy_Select_ForSearchByStoreType]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_StoreHierarchy_Select_ForSearchByStoreType]
GO

/*===========================================================================*\
  Description:	Returns top level Store Hierarchy nodes with matching Store
				names. To qualify as a top level node, they have to have no 
				current location.

  Parameters:	@UserDomainMask		User's Domain Mask restricting which 
									records may be returned
				@SessionID 			User's SessionID
				@SortOrderIndex		Index determining Sort Order
				@SearchText			Store type to match on

  Created:		September 2004

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_StoreHierarchy_Select_ForSearchByStoreType] 
@UserDomainMask INT,
@SessionID CHAR(16),
@SortOrderIndex TINYINT,
@SearchText VARCHAR(100)

AS

	SET NOCOUNT ON
	
	-- Create a table variable to put the initial results into. We use the results in this
	-- table to work out whether there are any stores contained within it.
	DECLARE @StoreHierarchy TABLE
	(
		[Collection_Unit_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
		[Item_Name] [varchar] (150),
		[Number] [varchar] (30) NULL,
		[Bottom_Level] [bit] NOT NULL,
		[Current_Location_Code] [varchar] (30) NULL
	)
	
	-- Insert the initials results into the table variable.
	INSERT INTO @StoreHierarchy (Collection_Unit_Key, Item_Name, Number, Bottom_Level, Current_Location_Code) 
	SELECT 		S.Collection_Unit_Key AS Item_Key, 
				S.Item_Name + IsNull(' - ' + CU.Current_Location_Code, IsNull(' - ' + CU.Usual_Location_Code, '')) AS Item_Name, 
				Number,
				0,
				CU.Current_Location_Code
	FROM 		Store AS S
	INNER JOIN	Collection_Unit AS CU ON CU.Collection_Unit_Key = S.Collection_Unit_Key
	    			AND ((CU.Domain_Mask & @UserDomainMask > 0) 
						OR (CU.Entered_Session_ID = @SessionID) 
						OR (CU.Changed_Session_ID = @SessionID) 
						OR (CU.Domain_Mask = 0))
	LEFT JOIN	Collection_Unit_Number AS CUN ON CUN.Collection_Unit_Key = S.Collection_Unit_Key 
					AND CUN.Type_Concept_Key = 'SYSTEM0000000001'  --REGISTRATION NUMBER
					AND CUN.Preferred = 1
					AND Number LIKE @SearchText + '%'
	INNER JOIN	VW_ConceptTerm AS CT ON CT.Concept_Key = S.Store_Type_Concept_Key 
					AND CT.PlainText LIKE @SearchText + '%'
	WHERE		CU.Current_Container_Collection_Unit_Key IS NULL
	
	-- Work out whether the top level node is also the bottom level node.
	UPDATE		SH
	SET			SH.Bottom_Level = CASE WHEN S.Collection_Unit_Key IS NULL THEN 1 ELSE 0 END
	FROM		@StoreHierarchy AS SH
	LEFT JOIN 	Collection_Unit CU ON CU.Current_Container_Collection_Unit_Key = SH.Collection_Unit_Key 
					AND ((CU.Domain_Mask & @UserDomainMask > 0) 
						OR (CU.Entered_Session_ID = @SessionID) 
						OR (CU.Changed_Session_ID = @SessionID) 
						OR (CU.Domain_Mask = 0))
				-- Need to do a join onto Store because other Collection_Units could have their current container
				-- as this store. However, we are only interested in the Stores at the moment.
	LEFT JOIN 	Store AS S ON S.Collection_Unit_Key = CU.Collection_Unit_Key
	
	-- Select the results for the result set.
	SELECT		Collection_Unit_Key AS Item_Key, 
				Collection_Unit_Key AS Join_Key, 
				Item_Name, 
				Number, 
				Bottom_Level
	FROM		@StoreHierarchy
	ORDER BY 	CASE @SortOrderIndex WHEN 0 THEN Item_Name 
									 WHEN 1 THEN Number
									 WHEN 2 THEN Current_Location_Code
				END,
				CASE @SortOrderIndex WHEN 0 THEN Number
									 WHEN 1 THEN Item_Name
									 WHEN 2 THEN Item_Name
				END,
				CASE @SortOrderIndex WHEN 2 THEN Number
				END

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_StoreHierarchy_Select_ForSearchByStoreType') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_StoreHierarchy_Select_ForSearchByStoreType'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_StoreHierarchy_Select_ForSearchByStoreType TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_StoreHierarchy_Select_ForSearchByStoreType TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_StoreHierarchy_Select_ForSearchByStoreType TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_StoreHierarchy_Select_ForSearchByStoreType TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_StoreHierarchy_Select_ForSearchByStoreType TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_StoreHierarchy_Select_ForSearchByStoreType TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_StoreHierarchy_Select_ForSearchByUsualLocationCode]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_StoreHierarchy_Select_ForSearchByUsualLocationCode]
GO

/*===========================================================================*\
  Description:	Returns top level Store Hierarchy nodes with matching Store
				names. To qualify as a top level node, they have to have no 
				current location.

  Parameters:	@UserDomainMask		User's Domain Mask restricting which 
									records may be returned
				@SessionID 			User's SessionID
				@SortOrderIndex		Index determining Sort Order
				@SearchText			Usual location code to match on

  Created:		September 2004

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_StoreHierarchy_Select_ForSearchByUsualLocationCode] 
@UserDomainMask INT,
@SessionID CHAR(16),
@SortOrderIndex TINYINT,
@SearchText VARCHAR(30)

AS

	SET NOCOUNT ON
	
	-- Create a table variable to put the initial results into. We use the results in this
	-- table to work out whether there are any stores contained within it.
	DECLARE @StoreHierarchy TABLE
	(
		[Collection_Unit_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
		[Item_Name] [varchar] (150), 
		[Number] [varchar] (30) NULL,
		[Bottom_Level] [bit] NOT NULL,
		[Current_Location_Code] [varchar] (30) NULL
	)
	
	-- Insert the initials results into the table variable.
	INSERT INTO @StoreHierarchy (Collection_Unit_Key, Item_Name, Number, Bottom_Level, Current_Location_Code) 
	SELECT 		S.Collection_Unit_Key AS Item_Key, 
				S.Item_Name + IsNull(' - ' + CU.Current_Location_Code, IsNull(' - ' + CU.Usual_Location_Code, '')) AS Item_Name,  
				Number,
				0,
				CU.Current_Location_Code
	FROM 		Store AS S
	INNER JOIN	Collection_Unit AS CU ON CU.Collection_Unit_Key = S.Collection_Unit_Key
	    			AND ((CU.Domain_Mask & @UserDomainMask > 0) 
						OR (CU.Entered_Session_ID = @SessionID) 
						OR (CU.Changed_Session_ID = @SessionID) 
						OR (CU.Domain_Mask = 0))
					AND CU.Usual_Location_Code LIKE @SearchText + '%'
	LEFT JOIN	Collection_Unit_Number AS CUN ON CUN.Collection_Unit_Key = S.Collection_Unit_Key 
					AND CUN.Type_Concept_Key = 'SYSTEM0000000001'  --REGISTRATION NUMBER
					AND CUN.Preferred = 1
	WHERE		CU.Current_Container_Collection_Unit_Key IS NULL
	
	-- Work out whether the top level node is also the bottom level node.
	UPDATE		SH
	SET			SH.Bottom_Level = CASE WHEN S.Collection_Unit_Key IS NULL THEN 1 ELSE 0 END
	FROM		@StoreHierarchy AS SH
	LEFT JOIN 	Collection_Unit CU ON CU.Current_Container_Collection_Unit_Key = SH.Collection_Unit_Key 
					AND ((CU.Domain_Mask & @UserDomainMask > 0) 
						OR (CU.Entered_Session_ID = @SessionID) 
						OR (CU.Changed_Session_ID = @SessionID) 
						OR (CU.Domain_Mask = 0))
				-- Need to do a join onto Store because other Collection_Units could have their current container
				-- as this store. However, we are only interested in the Stores at the moment.
	LEFT JOIN 	Store AS S ON S.Collection_Unit_Key = CU.Collection_Unit_Key
	
	-- Select the results for the result set.
	SELECT		Collection_Unit_Key AS Item_Key, 
				Collection_Unit_Key AS Join_Key, 
				Item_Name, 
				Number, 
				Bottom_Level
	FROM		@StoreHierarchy
	ORDER BY 	CASE @SortOrderIndex WHEN 0 THEN Item_Name 
									 WHEN 1 THEN Number
									 WHEN 2 THEN Current_Location_Code
				END,
				CASE @SortOrderIndex WHEN 0 THEN Number
									 WHEN 1 THEN Item_Name
									 WHEN 2 THEN Item_Name
				END,
				CASE @SortOrderIndex WHEN 2 THEN Number
				END

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_StoreHierarchy_Select_ForSearchByUsualLocationCode') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_StoreHierarchy_Select_ForSearchByUsualLocationCode'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_StoreHierarchy_Select_ForSearchByUsualLocationCode TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_StoreHierarchy_Select_ForSearchByUsualLocationCode TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_StoreHierarchy_Select_ForSearchByUsualLocationCode TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_StoreHierarchy_Select_ForSearchByUsualLocationCode TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_StoreHierarchy_Select_ForSearchByUsualLocationCode TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_StoreHierarchy_Select_ForSearchByUsualLocationCode TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_StoreHierarchy_Select_ForTopLevel]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_StoreHierarchy_Select_ForTopLevel]
GO

/*===========================================================================*\
  Description:	Returns top level Store Hierarchy nodes. To qualify as a top
				level node, they have to have no current location.

  Parameters:	@Key 				Optional Key. When specified, only the 
									single top level record is returned with 
									that key
				@UserDomainMask		User's Domain Mask restricting which 
									records may be returned
				@SessionID 			User's SessionID
				@SortOrderIndex		Index determining Sort Order

  Created:	September 2004

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_StoreHierarchy_Select_ForTopLevel] 
@UserDomainMask INT,
@SessionID CHAR(16),
@SortOrderIndex TINYINT,
@Key CHAR(16) = NULL

AS

	SET NOCOUNT ON
	
	-- Create  a table to hold the items we are looking for
	DECLARE @Search TABLE (ItemKey CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS PRIMARY KEY)
	
	IF @Key IS NOT NULL
		INSERT INTO @Search VALUES (@Key)
	ELSE IF object_id('tempdb..#TempFilter') is not null
		INSERT INTO @Search SELECT DISTINCT ItemKey FROM #TempFilter
	ELSE
		INSERT INTO @Search SELECT Collection_Unit_Key FROM Store
	
	-- Create a table variable to put the initial results into. We use the results in this
	-- table to work out whether there are any stores contained within it.
	DECLARE @StoreHierarchy TABLE
	(
		[Collection_Unit_Key] [char] (16) COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
		[Item_Name] [varchar] (150),-- COLLATE SQL_Latin1_General_CP1_CI_AS NOT NULL,
		[Number] [varchar] (30) NULL,
		[Bottom_Level] [bit] NOT NULL,
		[Current_Location_Code] [varchar] (30) NULL
	)
	
	-- Insert the initials results into the table variable.
	INSERT INTO @StoreHierarchy (Collection_Unit_Key, Item_Name, Number, Bottom_Level, Current_Location_Code) 
	SELECT 		S.Collection_Unit_Key AS Item_Key, 
				S.Item_Name + IsNull(' - ' + CU.Current_Location_Code, IsNull(' - ' + CU.Usual_Location_Code, '')) AS Item_Name, 
				Number,
				0,
				CU.Current_Location_Code
	FROM 		Store AS S
	INNER JOIN	Collection_Unit AS CU ON CU.Collection_Unit_Key = S.Collection_Unit_Key
	    			AND ((CU.Domain_Mask & @UserDomainMask > 0) 
						OR (CU.Entered_Session_ID = @SessionID) 
						OR (CU.Changed_Session_ID = @SessionID) 
						OR (CU.Domain_Mask = 0))
	LEFT JOIN	Collection_Unit_Number AS CUN ON CUN.Collection_Unit_Key = S.Collection_Unit_Key 
					AND CUN.Type_Concept_Key = 'SYSTEM0000000001'  --REGISTRATION NUMBER
					AND CUN.Preferred = 1
	INNER JOIN	@Search AS SR ON SR.ItemKey = CU.Collection_Unit_Key
	WHERE		CU.Current_Container_Collection_Unit_Key IS NULL
	
	-- Work out whether the top level node is also the bottom level node.
	UPDATE		SH
	SET			SH.Bottom_Level = CASE WHEN S.Collection_Unit_Key IS NULL THEN 1 ELSE 0 END
	FROM		@StoreHierarchy AS SH
	LEFT JOIN 	Collection_Unit CU ON CU.Current_Container_Collection_Unit_Key = SH.Collection_Unit_Key 
					AND ((CU.Domain_Mask & @UserDomainMask > 0) 
						OR (CU.Entered_Session_ID = @SessionID) 
						OR (CU.Changed_Session_ID = @SessionID) 
						OR (CU.Domain_Mask = 0))
				-- Need to do a join onto Store because other Collection_Units could have their current container
				-- as this store. However, we are only interested in the Stores at the moment.
	LEFT JOIN 	Store AS S ON S.Collection_Unit_Key = CU.Collection_Unit_Key
	
	-- Select the results for the result set.
	SELECT		Collection_Unit_Key AS Item_Key, 
				Collection_Unit_Key AS Join_Key, 
				Item_Name, 
				Number, 
				Bottom_Level
	FROM		@StoreHierarchy
	ORDER BY 	CASE @SortOrderIndex WHEN 0 THEN Item_Name 
									 WHEN 1 THEN Number
									 WHEN 2 THEN Current_Location_Code
				END,
				CASE @SortOrderIndex WHEN 0 THEN Number
									 WHEN 1 THEN Item_Name
									 WHEN 2 THEN Item_Name
				END,
				CASE @SortOrderIndex WHEN 2 THEN Number
				END

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_StoreHierarchy_Select_ForTopLevel') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_StoreHierarchy_Select_ForTopLevel'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_StoreHierarchy_Select_ForTopLevel TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_StoreHierarchy_Select_ForTopLevel TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_StoreHierarchy_Select_ForTopLevel TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_StoreHierarchy_Select_ForTopLevel TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_StoreHierarchy_Select_ForTopLevel TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_StoreHierarchy_Select_ForTopLevel TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_StoreIsBottomLevel_Get]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_StoreIsBottomLevel_Get]
GO

/*===========================================================================*\
  Description:	Takes a store key and returns whether it is a bottom level 
				store or not. To be a bottom level store, it has to not 
				contain any other stores currently (i.e. no other stores have
				this store as their current location).

  Parameters:	@Key
				@IsBottomLevel Output

  Created:		September 2004

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_StoreIsBottomLevel_Get]
	@Key char(16),
	@IsBottomLevel bit OUTPUT 
AS
	IF EXISTS	(SELECT 	*
				FROM 		Store AS S
				INNER JOIN	Collection_Unit AS CU ON CU.Collection_Unit_Key = S.Collection_Unit_Key
				WHERE		CU.Current_Container_Collection_Unit_Key = @Key)
		SET @IsBottomLevel = 0
	ELSE
		SET @IsBottomLevel = 1
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_StoreIsBottomLevel_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_StoreIsBottomLevel_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_StoreIsBottomLevel_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_StoreIsBottomLevel_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_StoreIsBottomLevel_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_StoreIsBottomLevel_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_StoreIsBottomLevel_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
       	GRANT EXECUTE ON dbo.usp_StoreIsBottomLevel_Get TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_StoreName_Get]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_StoreName_Get]
GO

/*===========================================================================*\
  Description:	Returns the store name for the given store key.

  Parameters:	@StoreKey
		@StoreName	Output

  Created:	August 2003

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_StoreName_Get]
	@StoreKey char(16),
	@StoreName varchar(100) OUTPUT 
AS
	SELECT		@StoreName = S.Item_Name + IsNull(' - ' + CU.Current_Location_Code, IsNull(' - ' + CU.Usual_Location_Code, ''))
	FROM		Store AS S
	INNER JOIN 	Collection_Unit AS CU ON CU.Collection_Unit_Key = S.Collection_Unit_Key
	WHERE		S.Collection_Unit_Key = @StoreKey

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_StoreName_Get') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_StoreName_Get'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_StoreName_Get TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_StoreName_Get TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_StoreName_Get TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_StoreName_Get TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_StoreName_Get TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_StoreName_Get TO [Dev - JNCC SQL]
END

GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Stores_Select_ForCollection]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Stores_Select_ForCollection]
GO

/*===========================================================================*\
  Description:	Returns Stores data to the CollectionsBrowser for a given Collection.

  Parameters:
	@ParentKey 	When specified, only the records associated with the parent key are returned
	@UserDomainMask	User's Domain Mask restricting which records may be returned
	@SessionID 	User's SessionID
	@SortOrderIndex	Index determining Sort Order

  Created:	August 2003

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Stores_Select_ForCollection] 
	@UserDomainMask INT,
	@SessionID CHAR(16),
	@SortOrderIndex TINYINT,
	@ParentKey CHAR(16)
AS

SET NOCOUNT ON

IF @SortOrderIndex = 0
	SELECT DISTINCT S.Collection_Unit_Key AS Item_Key, 
				S.Item_Name + IsNull(' - ' + CU2.Current_Location_Code, IsNull(' - ' + CU2.Usual_Location_Code, '')) AS Item_Name,  
				Number, 
				S.Collection_Unit_Key AS Join_Key
	FROM		Specimen_Unit SU
	INNER JOIN	Collection_Unit CU ON SU.Collection_Unit_Key = CU.Collection_Unit_Key 
									AND Parent_Collection_Collection_Unit_Key = @ParentKey
	INNER JOIN	Store S ON CU.Current_Container_Collection_Unit_Key = S.Collection_Unit_Key
	INNER JOIN	Collection_Unit AS CU2 ON CU2.Collection_Unit_Key = S.Collection_Unit_Key
									AND ((CU2.Domain_Mask & @UserDomainMask > 0) OR (CU2.Entered_Session_ID = @SessionID) 
										OR (CU2.Changed_Session_ID = @SessionID) OR (CU2.Domain_Mask = 0))
	LEFT JOIN 	Collection_Unit_Number CUN 
		ON S.Collection_Unit_Key = CUN.Collection_Unit_Key 
		AND CUN.Type_Concept_Key = 'SYSTEM0000000001'  --REGISTRATION NUMBER
		AND CUN.Preferred = 1

	ORDER BY S.Item_Name, Number
ELSE 
IF @SortOrderIndex = 1
	SELECT DISTINCT S.Collection_Unit_Key AS Item_Key,
				S.Item_Name + IsNull(' - ' + CU.Current_Location_Code, IsNull(' - ' + CU.Usual_Location_Code, '')) AS Item_Name,  
				Number, 
				S.Collection_Unit_Key AS Join_Key
	FROM		Specimen_Unit SU
	INNER JOIN	Collection_Unit CU ON SU.Collection_Unit_Key = CU.Collection_Unit_Key 
									AND Parent_Collection_Collection_Unit_Key = @ParentKey
	INNER JOIN	Store S ON CU.Current_Container_Collection_Unit_Key = S.Collection_Unit_Key
	INNER JOIN	Collection_Unit AS CU2 ON CU2.Collection_Unit_Key = S.Collection_Unit_Key
									AND ((CU2.Domain_Mask & @UserDomainMask > 0) OR (CU2.Entered_Session_ID = @SessionID) 
										OR (CU2.Changed_Session_ID = @SessionID) OR (CU2.Domain_Mask = 0))
	LEFT JOIN 	Collection_Unit_Number CUN 
		ON S.Collection_Unit_Key = CUN.Collection_Unit_Key 
		AND CUN.Type_Concept_Key = 'SYSTEM0000000001'  --REGISTRATION NUMBER
		AND CUN.Preferred = 1

	ORDER BY Number, S.Item_Name
ELSE 
IF @SortOrderIndex = 2
	SELECT DISTINCT S.Collection_Unit_Key AS Item_Key, 
				S.Item_Name + IsNull(' - ' + CU.Current_Location_Code, IsNull(' - ' + CU.Usual_Location_Code, '')) AS Item_Name,  
				Number, 
				S.Collection_Unit_Key AS Join_Key, 
				CU.Current_Location_Code
	FROM		Specimen_Unit SU
	INNER JOIN	Collection_Unit CU ON SU.Collection_Unit_Key = CU.Collection_Unit_Key 
									AND Parent_Collection_Collection_Unit_Key = @ParentKey
	INNER JOIN	Store S ON CU.Current_Container_Collection_Unit_Key = S.Collection_Unit_Key
	INNER JOIN	Collection_Unit AS CU2 ON CU2.Collection_Unit_Key = S.Collection_Unit_Key
									AND ((CU2.Domain_Mask & @UserDomainMask > 0) OR (CU2.Entered_Session_ID = @SessionID) 
										OR (CU2.Changed_Session_ID = @SessionID) OR (CU2.Domain_Mask = 0))
	LEFT JOIN 	Collection_Unit_Number CUN 
		ON S.Collection_Unit_Key = CUN.Collection_Unit_Key 
		AND CUN.Type_Concept_Key = 'SYSTEM0000000001'  --REGISTRATION NUMBER
		AND CUN.Preferred = 1

	ORDER BY CU.Current_Location_Code, S.Item_Name, Number
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Stores_Select_ForCollection') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Stores_Select_ForCollection'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Stores_Select_ForCollection TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Stores_Select_ForCollection TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Stores_Select_ForCollection TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Stores_Select_ForCollection TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Stores_Select_ForCollection TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Stores_Select_ForCollection TO [Dev - JNCC SQL]
END
GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Stores_Select_ForConditionCheck]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Stores_Select_ForConditionCheck]
GO

/*===========================================================================*\
  Description:	Returns Stores associated with a specified Condition Check

  Parameters:	
	@ParentKey 	When specified, only the records associated with the parent key are returned
	@SortOrderIndex	Index determining Sort Order
	@UserDomainMask	User's Domain Mask restricting which records may be returned
	@SessionID 	User's SessionID

  Created:	August 2003

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Stores_Select_ForConditionCheck] 
	@UserDomainMask INT,
	@SessionID CHAR(16),
	@SortOrderIndex TINYINT,
	@ParentKey CHAR(16)
AS

SET NOCOUNT ON

	SELECT 		S.Collection_Unit_Key AS Item_Key,  
			S.Item_Name + IsNull(' - ' + CU.Current_Location_Code, IsNull(' - ' + CU.Usual_Location_Code, '')) AS Item_Name, 
			CUN.Number, CUC.Collection_Unit_Check_Key AS Join_Key

	FROM 		Collection_Unit_Check CUC
	INNER JOIN 	Store S	ON CUC.Collection_Unit_Key = S.Collection_Unit_Key AND CUC.Conservation_Check_Key = @ParentKey
	INNER JOIN	Collection_Unit CU 
		ON S.Collection_Unit_Key = CU.Collection_Unit_Key
	        AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
			OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))

	LEFT JOIN 	Collection_Unit_Number CUN 
		ON S.Collection_Unit_Key = CUN.Collection_Unit_Key 
		AND CUN.Type_Concept_Key = 'SYSTEM0000000001'  --REGISTRATION NUMBER
		AND CUN.Preferred = 1

	ORDER BY 
		-- 0: S.Item_Name, CUN.Number 
		-- 1: CUN.Number, S.Item_Name
		-- 2: Current_Location_Code, S.Item_Name, CUN.Number 
		CASE @SortOrderIndex WHEN 1 THEN CUN.Number WHEN 2 THEN Current_Location_Code ELSE NULL END, 
		S.Item_Name,
		CASE @SortOrderIndex WHEN 1 THEN NULL ELSE CUN.Number END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Stores_Select_ForConditionCheck') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Stores_Select_ForConditionCheck'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Stores_Select_ForConditionCheck TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Stores_Select_ForConditionCheck TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Stores_Select_ForConditionCheck TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Stores_Select_ForConditionCheck TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Stores_Select_ForConditionCheck TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Stores_Select_ForConditionCheck TO [Dev - JNCC SQL]
END
GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Stores_Select_ForEnquiry]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Stores_Select_ForEnquiry]
GO

/*===========================================================================*\
  Description:	Returns Stores for a specified Enquiry.

  Parameters:	
	@ParentKey 	When specified, only the records associated with the parent key are returned
	@UserDomainMask	User's Domain Mask restricting which records may be returned
	@SessionID 	User's SessionID
	@SortOrderIndex	Index determining Sort Order

  Created:	August 2003

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_Stores_Select_ForEnquiry] 
	@UserDomainMask INT,
	@SessionID CHAR(16),
	@SortOrderIndex TINYINT,
	@ParentKey CHAR(16)
AS

SET NOCOUNT ON

	SELECT 		S.Collection_Unit_Key AS Item_Key,  
			S.Item_Name + IsNull(' - ' + CU.Current_Location_Code, IsNull(' - ' + CU.Usual_Location_Code, '')) AS Item_Name, 
			CUN.Number, CUE.Collection_Unit_Enquiry_Key AS Join_Key

	FROM 		Collection_Unit_Enquiry CUE
	INNER JOIN	Store S	ON CUE.Collection_Unit_Key = S.Collection_Unit_Key AND CUE.Enquiry_Key = @ParentKey
	INNER JOIN	Collection_Unit CU 
		ON S.Collection_Unit_Key = CU.Collection_Unit_Key
	        AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID)
			OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))

	LEFT JOIN 	Collection_Unit_Number CUN 
		ON S.Collection_Unit_Key = CUN.Collection_Unit_Key 
		AND CUN.Type_Concept_Key = 'SYSTEM0000000001'  --REGISTRATION NUMBER
		AND CUN.Preferred = 1

	ORDER BY 
		-- 0: S.Item_Name, CUN.Number 
		-- 1: CUN.Number, S.Item_Name
		-- 2: Current_Location_Code, S.Item_Name, CUN.Number 
		CASE @SortOrderIndex WHEN 1 THEN CUN.Number WHEN 2 THEN Current_Location_Code ELSE NULL END, 
		S.Item_Name,
		CASE @SortOrderIndex WHEN 1 THEN NULL ELSE CUN.Number END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Stores_Select_ForEnquiry') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Stores_Select_ForEnquiry'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Stores_Select_ForEnquiry TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Stores_Select_ForEnquiry TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Stores_Select_ForEnquiry TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Stores_Select_ForEnquiry TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Stores_Select_ForEnquiry TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Stores_Select_ForEnquiry TO [Dev - JNCC SQL]
END
GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Stores_Select_ForJob]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Stores_Select_ForJob]
GO

/*===========================================================================*\
  Description:	Returns Stores associated with a specified Job.

  Parameters:	
	@UserDomainMask	User's Domain Mask restricting which records may be returned
	@SessionID 	User's SessionID
	@SortOrderIndex	Index determining Sort Order
	@ParentKey 	Only the records associated with the parent key are returned

  Created:	August 2003

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Stores_Select_ForJob] 
	@UserDomainMask INT,
	@SessionID CHAR(16),
	@SortOrderIndex TINYINT,
	@ParentKey CHAR(16)
AS
SET NOCOUNT ON

	SELECT 		S.Collection_Unit_Key AS Item_Key,  
			S.Item_Name + IsNull(' - ' + CU.Current_Location_Code, IsNull(' - ' + CU.Usual_Location_Code, '')) AS Item_Name, 
			Number, CUT.Collection_Unit_Task_Key AS Join_Key

	FROM 		Conservation_Job CJ
	INNER JOIN	Conservation_Task CT ON CJ.Conservation_Job_Key = CT.Conservation_Job_Key AND CJ.Conservation_Job_Key = @ParentKey
	INNER JOIN	Collection_Unit_Task CUT ON CT.Conservation_Task_Key = CUT.Conservation_Task_Key
	INNER JOIN	Store S ON CUT.Collection_Unit_Key = S.Collection_Unit_Key
	INNER JOIN    	Collection_Unit CU 
		ON S.Collection_Unit_Key = CU.Collection_Unit_Key
	       	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID)
			OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))

	LEFT JOIN 	Collection_Unit_Number CUN 
		ON S.Collection_Unit_Key = CUN.Collection_Unit_Key 
		AND CUN.Type_Concept_Key = 'SYSTEM0000000001'  --REGISTRATION NUMBER
		AND CUN.Preferred = 1

	ORDER BY 
		-- 0: S.Item_Name, Number 
		-- 1: Number, S.Item_Name
		-- 2: Current_Location_Code, S.Item_Name, CUN.Number 
		CASE @SortOrderIndex WHEN 1 THEN Number WHEN 2 THEN Current_Location_Code ELSE NULL END, 
		S.Item_Name,
		CASE @SortOrderIndex WHEN 1 THEN NULL ELSE Number END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Stores_Select_ForJob') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Stores_Select_ForJob'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Stores_Select_ForJob TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Stores_Select_ForJob TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Stores_Select_ForJob TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Stores_Select_ForJob TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Stores_Select_ForJob TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Stores_Select_ForJob TO [Dev - JNCC SQL]
END
GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Stores_Select_ForMovement]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Stores_Select_ForMovement]
GO

/*===========================================================================*\
  Description:	Returns Stores for a specified Movement.

  Parameters:	
	@ParentKey 	When specified, only the records associated with the parent key are returned
	@UserDomainMask	User's Domain Mask restricting which records may be returned
	@SessionID 	User's SessionID
	@SortOrderIndex	Index determining Sort Order

  Created:	August 2003

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Stores_Select_ForMovement] 
	@UserDomainMask INT,
	@SessionID CHAR(16),
	@SortOrderIndex TINYINT,
	@ParentKey CHAR(16)
AS

SET NOCOUNT ON

	SELECT 		S.Collection_Unit_Key AS Item_Key, 	
				S.Item_Name + IsNull(' - ' + CU.Current_Location_Code, IsNull(' - ' + CU.Usual_Location_Code, '')) AS Item_Name, 
				CUN.Number, MCU.Movement_Collection_Unit_Key AS Join_Key

	FROM		Movement M
	INNER JOIN 	Movement_Direction MD ON M.Movement_Key = MD.Movement_Key AND (M.Movement_Key = @ParentKey)
	INNER JOIN 	Movement_Collection_Unit MCU ON MD.Movement_Direction_Key = MCU.Movement_Direction_Key
	INNER JOIN 	Store S	ON MCU.Collection_Unit_Key = S.Collection_Unit_Key
	INNER JOIN	Collection_UNIT CU 
		ON S.Collection_Unit_Key = CU.Collection_Unit_Key
	        AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
			OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))

	LEFT JOIN 	Collection_Unit_Number CUN 
		ON S.Collection_Unit_Key = CUN.Collection_Unit_Key 
		AND CUN.Type_Concept_Key = 'SYSTEM0000000001'  --REGISTRATION NUMBER
		AND CUN.Preferred = 1

	ORDER BY 
		-- 0: S.Item_Name, CUN.Number 
		-- 1: CUN.Number, S.Item_Name
		-- 2: Current_Location_Code, S.Item_Name, CUN.Number 
		CASE @SortOrderIndex WHEN 1 THEN CUN.Number WHEN 2 THEN Current_Location_Code ELSE NULL END, 
		S.Item_Name,
		CASE @SortOrderIndex WHEN 1 THEN NULL ELSE CUN.Number END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Stores_Select_ForMovement') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Stores_Select_ForMovement'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Stores_Select_ForMovement TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Stores_Select_ForMovement TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Stores_Select_ForMovement TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Stores_Select_ForMovement TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Stores_Select_ForMovement TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Stores_Select_ForMovement TO [Dev - JNCC SQL]
END
GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Stores_Select_ForMovementIn]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Stores_Select_ForMovementIn]
GO

/*===========================================================================*\
  Description:	Returns Stores for a specified Movement.

  Parameters:	
	@ParentKey 	When specified, only the records associated with the parent key are returned
	@UserDomainMask	User's Domain Mask restricting which records may be returned
	@SessionID 	User's SessionID
	@SortOrderIndex	Index determining Sort Order

  Created:	August 2003

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Stores_Select_ForMovementIn] 
	@UserDomainMask INT,
	@SessionID CHAR(16),
	@SortOrderIndex TINYINT,
	@ParentKey CHAR(16)
AS

SET NOCOUNT ON

	SELECT 		S.Collection_Unit_Key AS Item_Key, 	
				S.Item_Name + IsNull(' - ' + CU.Current_Location_Code, IsNull(' - ' + CU.Usual_Location_Code, '')) AS Item_Name, 
				CUN.Number, MCU.Movement_Collection_Unit_Key AS Join_Key

	FROM		Movement M
	INNER JOIN 	Movement_Direction MD ON M.Movement_Key = MD.Movement_Key AND (M.Movement_Key = @ParentKey)
	INNER JOIN 	Movement_Collection_Unit MCU ON MD.Movement_Direction_Key = MCU.Movement_Direction_Key AND (MD.Outbound = 0)
	INNER JOIN 	Store S	ON MCU.Collection_Unit_Key = S.Collection_Unit_Key
	INNER JOIN	Collection_UNIT CU 
		ON S.Collection_Unit_Key = CU.Collection_Unit_Key
	        AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID)
			OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))

	LEFT JOIN 	Collection_Unit_Number CUN 
		ON S.Collection_Unit_Key = CUN.Collection_Unit_Key 
		AND CUN.Type_Concept_Key = 'SYSTEM0000000001'  --REGISTRATION NUMBER
		AND CUN.Preferred = 1

	ORDER BY 
		-- 0: S.Item_Name, CUN.Number 
		-- 1: CUN.Number, S.Item_Name
		-- 2: Current_Location_Code, S.Item_Name, CUN.Number 
		CASE @SortOrderIndex WHEN 1 THEN CUN.Number WHEN 2 THEN Current_Location_Code ELSE NULL END, 
		S.Item_Name,
		CASE @SortOrderIndex WHEN 1 THEN NULL ELSE CUN.Number END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Stores_Select_ForMovementIn') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Stores_Select_ForMovementIn'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Stores_Select_ForMovementIn TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Stores_Select_ForMovementIn TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Stores_Select_ForMovementIn TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Stores_Select_ForMovementIn TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Stores_Select_ForMovementIn TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Stores_Select_ForMovementIn TO [Dev - JNCC SQL]
END
GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Stores_Select_ForMovementOut]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Stores_Select_ForMovementOut]
GO

/*===========================================================================*\
  Description:	Returns Stores for a specified Movement.

  Parameters:	
	@ParentKey 	When specified, only the records associated with the parent key are returned
	@UserDomainMask	User's Domain Mask restricting which records may be returned
	@SessionID 	User's SessionID
	@SortOrderIndex	Index determining Sort Order

  Created:	August 2003

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Stores_Select_ForMovementOut] 
	@UserDomainMask INT,
	@SessionID CHAR(16),
	@SortOrderIndex TINYINT,
	@ParentKey CHAR(16)
AS

SET NOCOUNT ON

	SELECT 		S.Collection_Unit_Key AS Item_Key, 	
				S.Item_Name + IsNull(' - ' + CU.Current_Location_Code, IsNull(' - ' + CU.Usual_Location_Code, '')) AS Item_Name, 
				CUN.Number, MCU.Movement_Collection_Unit_Key AS Join_Key

	FROM		Movement M
	INNER JOIN 	Movement_Direction MD ON M.Movement_Key = MD.Movement_Key AND (M.Movement_Key = @ParentKey)
	INNER JOIN 	Movement_Collection_Unit MCU ON MD.Movement_Direction_Key = MCU.Movement_Direction_Key AND (MD.Outbound = 1)
	INNER JOIN 	Store S	ON MCU.Collection_Unit_Key = S.Collection_Unit_Key
	INNER JOIN	Collection_Unit CU 
		ON S.Collection_Unit_Key = CU.Collection_Unit_Key
	        AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
			OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))

	LEFT JOIN 	Collection_Unit_Number CUN 
		ON S.Collection_Unit_Key = CUN.Collection_Unit_Key 
		AND CUN.Type_Concept_Key = 'SYSTEM0000000001'  --REGISTRATION NUMBER
		AND CUN.Preferred = 1

	ORDER BY 
		-- 0: S.Item_Name, CUN.Number 
		-- 1: CUN.Number, S.Item_Name
		-- 2: Current_Location_Code, S.Item_Name, CUN.Number 
		CASE @SortOrderIndex WHEN 1 THEN CUN.Number WHEN 2 THEN Current_Location_Code ELSE NULL END, 
		S.Item_Name,
		CASE @SortOrderIndex WHEN 1 THEN NULL ELSE CUN.Number END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Stores_Select_ForMovementOut') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Stores_Select_ForMovementOut'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Stores_Select_ForMovementOut TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Stores_Select_ForMovementOut TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Stores_Select_ForMovementOut TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Stores_Select_ForMovementOut TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Stores_Select_ForMovementOut TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Stores_Select_ForMovementOut TO [Dev - JNCC SQL]
END
GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Stores_Select_ForSearchByName]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Stores_Select_ForSearchByName]
GO

CREATE PROCEDURE [dbo].[usp_Stores_Select_ForSearchByName] 
@UserDomainMask INT,
@SessionID CHAR(16),
@SortOrderIndex TINYINT,
@SearchText VARCHAR(100)

AS

--  DESCRIPTION
--  Returns Stores data based on the search parameter for Store Name
--
--  PARAMETERS
--	NAME				DESCRIPTION
--	@SearchText			Text to be used for search
--	@UserDomainMask		User's Domain Mask restricting which records may be returned
--	@SessionID 			User's SessionID
--	@SortOrderIndex		Index determining Sort Order
--
--
--  AUTHOR:				Ben Collier, Dorset Software
--  CREATED:			2003-09-15
--
SET NOCOUNT ON

IF @SortOrderIndex = 0
	SELECT S.Collection_Unit_Key AS Item_Key, 	
				S.Item_Name + IsNull(' - ' + CU.Current_Location_Code, IsNull(' - ' + CU.Usual_Location_Code, '')) AS Item_Name, 
				Number, S.Item_Name AS Hint
	FROM 
	STORE S
	    INNER JOIN
	   	    COLLECTION_UNIT CU 
	    ON S.Collection_Unit_Key = CU.Collection_Unit_Key
	       	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
				OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
			AND S.Item_Name LIKE @SearchText + '%'
		LEFT JOIN 
			COLLECTION_UNIT_NUMBER CUN 
		ON S.Collection_Unit_Key = CUN.Collection_Unit_Key 
			AND CUN.Type_Concept_Key = 'SYSTEM0000000001'  --REGISTRATION NUMBER
			AND CUN.Preferred = 1
	ORDER BY S.Item_Name, Number
ELSE IF @SortOrderIndex = 1
	SELECT S.Collection_Unit_Key AS Item_Key, S.Item_Name, Number, Number AS Hint
	FROM 
	STORE S
	    INNER JOIN
	   	    COLLECTION_UNIT CU 
	    ON S.Collection_Unit_Key = CU.Collection_Unit_Key
	       	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
				OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
			AND S.Item_Name LIKE @SearchText + '%'
		LEFT JOIN 
			COLLECTION_UNIT_NUMBER CUN 
		ON S.Collection_Unit_Key = CUN.Collection_Unit_Key 
			AND CUN.Type_Concept_Key = 'SYSTEM0000000001'  --REGISTRATION NUMBER
			AND CUN.Preferred = 1
	ORDER BY Number, S.Item_Name
ELSE IF @SortOrderIndex = 2
	SELECT S.Collection_Unit_Key AS Item_Key, S.Item_Name, Number, Current_Location_Code AS Hint
	FROM 
	STORE S
	    INNER JOIN
	   	    COLLECTION_UNIT CU 
	    ON S.Collection_Unit_Key = CU.Collection_Unit_Key
	       	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
				OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
			AND S.Item_Name LIKE @SearchText + '%'
		LEFT JOIN 
			COLLECTION_UNIT_NUMBER CUN 
		ON S.Collection_Unit_Key = CUN.Collection_Unit_Key 
			AND CUN.Type_Concept_Key = 'SYSTEM0000000001'  --REGISTRATION NUMBER
			AND CUN.Preferred = 1
	ORDER BY Current_Location_Code, S.Item_Name, Number

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Stores_Select_ForSearchByName') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Stores_Select_ForSearchByName'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Stores_Select_ForSearchByName TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Stores_Select_ForSearchByName TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Stores_Select_ForSearchByName TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Stores_Select_ForSearchByName TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Stores_Select_ForSearchByName TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Stores_Select_ForSearchByName TO [Dev - JNCC SQL]
END

GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Stores_Select_ForSearchByRegistrationNumber]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Stores_Select_ForSearchByRegistrationNumber]
GO

CREATE PROCEDURE [dbo].[usp_Stores_Select_ForSearchByRegistrationNumber] 
@UserDomainMask INT,
@SessionID CHAR(16),
@SortOrderIndex TINYINT,
@SearchText VARCHAR(30)

AS

--  DESCRIPTION
--  Returns Stores data based on the search parameter for Store Registration Number
--
--  PARAMETERS
--	NAME				DESCRIPTION
--	@SearchText			Text to be used for search
--	@UserDomainMask		User's Domain Mask restricting which records may be returned
--	@SessionID 			User's SessionID
--	@SortOrderIndex		Index determining Sort Order
--
--
--  AUTHOR:				Ben Collier, Dorset Software
--  CREATED:			2003-09-15
--
SET NOCOUNT ON

IF @SortOrderIndex = 0
	SELECT S.Collection_Unit_Key AS Item_Key, 	
				S.Item_Name + IsNull(' - ' + CU.Current_Location_Code, IsNull(' - ' + CU.Usual_Location_Code, '')) AS Item_Name, 
				Number, S.Item_Name AS Hint
	FROM 
	STORE S
	    INNER JOIN
	   	    COLLECTION_UNIT CU 
	    ON S.Collection_Unit_Key = CU.Collection_Unit_Key
	       	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
				OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
		INNER JOIN 
			COLLECTION_UNIT_NUMBER CUN 
		ON S.Collection_Unit_Key = CUN.Collection_Unit_Key 
			AND CUN.Type_Concept_Key = 'SYSTEM0000000001'  --REGISTRATION NUMBER
			AND CUN.Preferred = 1
			AND Number LIKE @SearchText + '%'
	ORDER BY S.Item_Name, Number
ELSE IF @SortOrderIndex = 1
	SELECT S.Collection_Unit_Key AS Item_Key, 	
				S.Item_Name + IsNull(' - ' + CU.Current_Location_Code, IsNull(' - ' + CU.Usual_Location_Code, '')) AS Item_Name, 
				Number, Number AS Hint
	FROM 
	STORE S
	    INNER JOIN
	   	    COLLECTION_UNIT CU 
	    ON S.Collection_Unit_Key = CU.Collection_Unit_Key
	       	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
				OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
		INNER JOIN 
			COLLECTION_UNIT_NUMBER CUN 
		ON S.Collection_Unit_Key = CUN.Collection_Unit_Key 
			AND CUN.Type_Concept_Key = 'SYSTEM0000000001'  --REGISTRATION NUMBER
			AND CUN.Preferred = 1
			AND Number LIKE @SearchText + '%'
	ORDER BY Number, S.Item_Name
ELSE IF @SortOrderIndex = 2
	SELECT S.Collection_Unit_Key AS Item_Key, 	
				S.Item_Name + IsNull(' - ' + CU.Current_Location_Code, IsNull(' - ' + CU.Usual_Location_Code, '')) AS Item_Name, 
				Number, Current_Location_Code AS Hint
	FROM 
	STORE S
	    INNER JOIN
	   	    COLLECTION_UNIT CU 
	    ON S.Collection_Unit_Key = CU.Collection_Unit_Key
	       	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
				OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
		INNER JOIN 
			COLLECTION_UNIT_NUMBER CUN 
		ON S.Collection_Unit_Key = CUN.Collection_Unit_Key 
			AND CUN.Type_Concept_Key = 'SYSTEM0000000001'  --REGISTRATION NUMBER
			AND CUN.Preferred = 1
			AND Number LIKE @SearchText + '%'
	ORDER BY Current_Location_Code, S.Item_Name, Number

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Stores_Select_ForSearchByRegistrationNumber') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Stores_Select_ForSearchByRegistrationNumber'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Stores_Select_ForSearchByRegistrationNumber TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Stores_Select_ForSearchByRegistrationNumber TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Stores_Select_ForSearchByRegistrationNumber TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Stores_Select_ForSearchByRegistrationNumber TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Stores_Select_ForSearchByRegistrationNumber TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Stores_Select_ForSearchByRegistrationNumber TO [Dev - JNCC SQL]
END

GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Stores_Select_ForSearchByStoreType]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Stores_Select_ForSearchByStoreType]
GO

CREATE PROCEDURE [dbo].[usp_Stores_Select_ForSearchByStoreType] 
@UserDomainMask INT,
@SessionID CHAR(16),
@SortOrderIndex TINYINT,
@SearchText VARCHAR(100)

AS

--  DESCRIPTION
--  Returns Stores data based on the search parameter for Store Type
--
--  PARAMETERS
--	NAME				DESCRIPTION
--	@SearchText			Text to be used for search
--	@UserDomainMask		User's Domain Mask restricting which records may be returned
--	@SessionID 			User's SessionID
--	@SortOrderIndex		Index determining Sort Order
--
--
--  AUTHOR:				Ben Collier, Dorset Software
--  CREATED:			05/02/2004
--
SET NOCOUNT ON

IF @SortOrderIndex = 0
	SELECT S.Collection_Unit_Key AS Item_Key, 	
				S.Item_Name + IsNull(' - ' + CU.Current_Location_Code, IsNull(' - ' + CU.Usual_Location_Code, '')) AS Item_Name, 
				Number, S.Item_Name AS Hint
	FROM 
	STORE S
	    INNER JOIN
	   	    COLLECTION_UNIT CU 
	    ON S.Collection_Unit_Key = CU.Collection_Unit_Key
	       	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
				OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
		LEFT JOIN 
			COLLECTION_UNIT_NUMBER CUN 
		ON S.Collection_Unit_Key = CUN.Collection_Unit_Key 
			AND CUN.Type_Concept_Key = 'SYSTEM0000000001'  --REGISTRATION NUMBER
			AND CUN.Preferred = 1
		INNER JOIN
			VW_ConceptTerm CT
		ON S.Store_Type_Concept_Key = CT.Concept_Key
			AND CT.PlainText LIKE @SearchText + '%'
	ORDER BY S.Item_Name, Number
ELSE IF @SortOrderIndex = 1
	SELECT S.Collection_Unit_Key AS Item_Key, 	
				S.Item_Name + IsNull(' - ' + CU.Current_Location_Code, IsNull(' - ' + CU.Usual_Location_Code, '')) AS Item_Name, 
				Number, S.Item_Name AS Hint
	FROM 
	STORE S
	    INNER JOIN
	   	    COLLECTION_UNIT CU 
	    ON S.Collection_Unit_Key = CU.Collection_Unit_Key
	       	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
				OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
		LEFT JOIN 
			COLLECTION_UNIT_NUMBER CUN 
		ON S.Collection_Unit_Key = CUN.Collection_Unit_Key 
			AND CUN.Type_Concept_Key = 'SYSTEM0000000001'  --REGISTRATION NUMBER
			AND CUN.Preferred = 1
		INNER JOIN
			VW_ConceptTerm CT
		ON S.Store_Type_Concept_Key = CT.Concept_Key
			AND CT.PlainText LIKE @SearchText + '%'
	ORDER BY Number, S.Item_Name
ELSE IF @SortOrderIndex = 2
	SELECT S.Collection_Unit_Key AS Item_Key, 	
				S.Item_Name + IsNull(' - ' + CU.Current_Location_Code, IsNull(' - ' + CU.Usual_Location_Code, '')) AS Item_Name, 
				Number, S.Item_Name AS Hint
	FROM 
	STORE S
	    INNER JOIN
	   	    COLLECTION_UNIT CU 
	    ON S.Collection_Unit_Key = CU.Collection_Unit_Key
	       	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
				OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
		LEFT JOIN 
			COLLECTION_UNIT_NUMBER CUN 
		ON S.Collection_Unit_Key = CUN.Collection_Unit_Key 
			AND CUN.Type_Concept_Key = 'SYSTEM0000000001'  --REGISTRATION NUMBER
			AND CUN.Preferred = 1
		INNER JOIN
			VW_ConceptTerm CT
		ON S.Store_Type_Concept_Key = CT.Concept_Key
			AND CT.PlainText LIKE @SearchText + '%'
	ORDER BY Current_Location_Code, S.Item_Name, Number

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Stores_Select_ForSearchByStoreType') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Stores_Select_ForSearchByStoreType'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Stores_Select_ForSearchByStoreType TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Stores_Select_ForSearchByStoreType TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Stores_Select_ForSearchByStoreType TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Stores_Select_ForSearchByStoreType TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Stores_Select_ForSearchByStoreType TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Stores_Select_ForSearchByStoreType TO [Dev - JNCC SQL]
END

GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Stores_Select_ForSearchByUsualLocationCode]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Stores_Select_ForSearchByUsualLocationCode]
GO

CREATE PROCEDURE [dbo].[usp_Stores_Select_ForSearchByUsualLocationCode] 
@UserDomainMask INT,
@SessionID CHAR(16),
@SortOrderIndex TINYINT,
@SearchText VARCHAR(30)

AS

--  DESCRIPTION
--  Returns Stores data based on the search parameter for Usual Location
--
--  PARAMETERS
--	NAME				DESCRIPTION
--	@SearchText			Text to be used for search
--	@UserDomainMask		User's Domain Mask restricting which records may be returned
--	@SessionID 			User's SessionID
--	@SortOrderIndex		Index determining Sort Order
--
--
--  AUTHOR:				Ben Collier, Dorset Software
--  CREATED:			2003-09-15
--
SET NOCOUNT ON

IF @SortOrderIndex = 0
	SELECT S.Collection_Unit_Key AS Item_Key, 	
				S.Item_Name + IsNull(' - ' + CU.Current_Location_Code, IsNull(' - ' + CU.Usual_Location_Code, '')) AS Item_Name, 
				Number, S.Item_Name AS Hint
	FROM 
	STORE S
	    INNER JOIN
	   	    COLLECTION_UNIT CU 
	    ON S.Collection_Unit_Key = CU.Collection_Unit_Key
	       	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
				OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
			AND CU.Usual_Location_Code LIKE @SearchText + '%'
		LEFT JOIN 
			COLLECTION_UNIT_NUMBER CUN 
		ON S.Collection_Unit_Key = CUN.Collection_Unit_Key 
			AND CUN.Type_Concept_Key = 'SYSTEM0000000001'  --REGISTRATION NUMBER
			AND CUN.Preferred = 1
	ORDER BY S.Item_Name, Number
ELSE IF @SortOrderIndex = 1
	SELECT S.Collection_Unit_Key AS Item_Key, 	
				S.Item_Name + IsNull(' - ' + CU.Current_Location_Code, IsNull(' - ' + CU.Usual_Location_Code, '')) AS Item_Name, 
				Number, Number AS Hint
	FROM 
	STORE S
	    INNER JOIN
	   	    COLLECTION_UNIT CU 
	    ON S.Collection_Unit_Key = CU.Collection_Unit_Key
	       	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
				OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
			AND CU.Usual_Location_Code LIKE @SearchText + '%'
		LEFT JOIN 
			COLLECTION_UNIT_NUMBER CUN 
		ON S.Collection_Unit_Key = CUN.Collection_Unit_Key 
			AND CUN.Type_Concept_Key = 'SYSTEM0000000001'  --REGISTRATION NUMBER
			AND CUN.Preferred = 1
	ORDER BY Number, S.Item_Name
ELSE IF @SortOrderIndex = 2
	SELECT S.Collection_Unit_Key AS Item_Key, S.Item_Name, 	
				S.Item_Name + IsNull(' - ' + CU.Current_Location_Code, IsNull(' - ' + CU.Usual_Location_Code, '')) AS Item_Name, 
				CU.Current_Location_Code AS Hint
	FROM 
	STORE S
	    INNER JOIN
	   	    COLLECTION_UNIT CU 
	    ON S.Collection_Unit_Key = CU.Collection_Unit_Key
	       	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
				OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
			AND CU.Usual_Location_Code LIKE @SearchText + '%'
		LEFT JOIN 
			COLLECTION_UNIT_NUMBER CUN 
		ON S.Collection_Unit_Key = CUN.Collection_Unit_Key 
			AND CUN.Type_Concept_Key = 'SYSTEM0000000001'  --REGISTRATION NUMBER
			AND CUN.Preferred = 1
	ORDER BY Current_Location_Code, S.Item_Name, Number

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Stores_Select_ForSearchByUsualLocationCode') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Stores_Select_ForSearchByUsualLocationCode'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Stores_Select_ForSearchByUsualLocationCode TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Stores_Select_ForSearchByUsualLocationCode TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Stores_Select_ForSearchByUsualLocationCode TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Stores_Select_ForSearchByUsualLocationCode TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Stores_Select_ForSearchByUsualLocationCode TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Stores_Select_ForSearchByUsualLocationCode TO [Dev - JNCC SQL]
END

GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Stores_Select_ForTopLevel]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Stores_Select_ForTopLevel]
GO

CREATE PROCEDURE [dbo].[usp_Stores_Select_ForTopLevel] 
@UserDomainMask INT,
@SessionID CHAR(16),
@SortOrderIndex TINYINT,
@Key CHAR(16) = NULL

AS

--  DESCRIPTION
--  Returns top level Stores data to the CollectionsBrowser
--
--  PARAMETERS
--  NAME				DESCRIPTION
--	@Key 				Optional Key. When specified, only the single top level record is returned with that key
--	@UserDomainMask		User's Domain Mask restricting which records may be returned
--	@SessionID 			User's SessionID
--	@SortOrderIndex		Index determining Sort Order
--
--
--  AUTHOR:     		Ben Collier, Dorset Software
--  CREATED:		    2003-08-15
--
SET NOCOUNT ON

-- Create  a table to hold the items we are looking for
DECLARE @Search TABLE (ItemKey CHAR(16) COLLATE SQL_Latin1_General_CP1_CI_AS PRIMARY KEY)

IF @Key IS NOT NULL
	INSERT INTO @Search VALUES (@Key)
ELSE IF object_id('tempdb..#TempFilter') is not null
	INSERT INTO @Search SELECT DISTINCT ItemKey FROM #TempFilter
ELSE
	INSERT INTO @Search SELECT Collection_Unit_Key FROM Store

IF @SortOrderIndex = 0
BEGIN
		SELECT S.Collection_Unit_Key AS Item_Key, 	
				S.Item_Name + IsNull(' - ' + CU.Current_Location_Code, IsNull(' - ' + CU.Usual_Location_Code, '')) AS Item_Name, 
				Number
		FROM 
		STORE S
		    INNER JOIN
	    	    COLLECTION_UNIT CU 
		    ON S.Collection_Unit_Key = CU.Collection_Unit_Key
	        	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
					OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
			LEFT JOIN 
				COLLECTION_UNIT_NUMBER CUN 
			ON S.Collection_Unit_Key = CUN.Collection_Unit_Key 
				AND CUN.Type_Concept_Key = 'SYSTEM0000000001'  --REGISTRATION NUMBER
				AND CUN.Preferred = 1
		INNER JOIN @Search SR ON SR.ItemKey=CU.Collection_Unit_Key
		ORDER BY S.Item_Name, Number
END
ELSE IF @SortOrderIndex = 1
BEGIN
		SELECT S.Collection_Unit_Key AS Item_Key, 	
				S.Item_Name + IsNull(' - ' + CU.Current_Location_Code, IsNull(' - ' + CU.Usual_Location_Code, '')) AS Item_Name, 
				Number
		FROM 
		STORE S
		    INNER JOIN
	    	    COLLECTION_UNIT CU 
		    ON S.Collection_Unit_Key = CU.Collection_Unit_Key
	        	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
					OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
			LEFT JOIN 
				COLLECTION_UNIT_NUMBER CUN 
			ON S.Collection_Unit_Key = CUN.Collection_Unit_Key 
				AND CUN.Type_Concept_Key = 'SYSTEM0000000001'  --REGISTRATION NUMBER
				AND CUN.Preferred = 1
		INNER JOIN @Search SR ON SR.ItemKey=CU.Collection_Unit_Key
		ORDER BY Number, S.Item_Name
END
ELSE IF @SortOrderIndex = 2
BEGIN
		SELECT S.Collection_Unit_Key AS Item_Key, 	
				S.Item_Name + IsNull(' - ' + CU.Current_Location_Code, IsNull(' - ' + CU.Usual_Location_Code, '')) AS Item_Name, 
				Number, Current_Location_Code AS Hint
		FROM 
		STORE S
		    INNER JOIN
	    	    COLLECTION_UNIT CU 
		    ON S.Collection_Unit_Key = CU.Collection_Unit_Key
	        	AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
					OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
			LEFT JOIN 
				COLLECTION_UNIT_NUMBER CUN 
			ON S.Collection_Unit_Key = CUN.Collection_Unit_Key 
				AND CUN.Type_Concept_Key = 'SYSTEM0000000001'  --REGISTRATION NUMBER
				AND CUN.Preferred = 1
		INNER JOIN @Search SR ON SR.ItemKey=CU.Collection_Unit_Key
		ORDER BY Current_Location_Code, S.Item_Name, Number
END

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Stores_Select_ForTopLevel') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Stores_Select_ForTopLevel'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Stores_Select_ForTopLevel TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Stores_Select_ForTopLevel TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Stores_Select_ForTopLevel TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Stores_Select_ForTopLevel TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Stores_Select_ForTopLevel TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Stores_Select_ForTopLevel TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Store_LayoutXML_Select]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Store_LayoutXML_Select]
GO

/*===========================================================================*\
  Description:	Returns XML for a store's diagram

  Parameters:	@Key	Store key

  Created:	Sept 2004

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Store_LayoutXML_Select]
	@Key char(16)
AS

SELECT Diagram_XML, TimeStamp
FROM Store
WHERE Collection_Unit_Key=@Key

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Store_LayoutXML_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Store_LayoutXML_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Store_LayoutXML_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Store_LayoutXML_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Store_LayoutXML_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Store_LayoutXML_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Store_LayoutXML_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Store_LayoutXML_Select TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Store_Location_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Store_Location_Update]
GO

/*===========================================================================*\
  Description:	Updates a record in the Store table.
				Ensures the Domain masks of the store and its containers are
				also updated.

  Parameters:	@Key - Collection_Unit key of the store
				@CurrentContainerKey - Key of the new Current container
				@SessionID 
				@UpdateUsualContainer - Bit value used to decide whether to 
										update the Usual container key as well

  Created:	September 2004

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Store_Location_Update]
	@Key char(16), 
	@CurrentContainerKey char(16),
	@SessionID char(16),
	@UpdateUsualContainer bit = 0
AS
	BEGIN TRANSACTION

		UPDATE	Collection_Unit
		SET		Current_Container_Collection_Unit_Key = @CurrentContainerKey,
				Changed_Session_ID = @SessionID
		WHERE	Collection_Unit_Key = @Key

		IF @UpdateUsualContainer = 1 
			UPDATE	Collection_Unit
			SET		Usual_Container_Collection_Unit_Key = @CurrentContainerKey
			WHERE	Collection_Unit_Key = @Key

		IF @@Error <> 0 GOTO RollbackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit:
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Store_Location_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Store_Location_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        GRANT EXECUTE ON dbo.usp_Store_Location_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Store_Location_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Store_Location_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Store_Location_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_Store_Location_Update TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
       FROM   SysObjects
       WHERE  Id = Object_Id(N'[dbo].[usp_Store_PossibleChildStores_Select]')
       AND    Type = 'P')
    DROP PROCEDURE [dbo].[usp_Store_PossibleChildStores_Select]
GO

/*===========================================================================*\
  Description:  Select the list of stores that are currently in a store, or
					usually held in a store

  Parameters:   @Key	Store collection unit key

  Created:      Sept 2004

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Store_PossibleChildStores_Select]
    @Key CHAR(16)
AS

SELECT DISTINCT 
		S.Collection_Unit_Key, 
		S.Item_Name + ISNULL(' - ' + C.Current_Location_Code, '') AS Item_Name,
		CASE 
			WHEN C.Current_Container_Collection_Unit_Key=@Key THEN 1
			ELSE 0
		END AS IsCurrent
FROM Store S
INNER JOIN Collection_Unit C ON C.Collection_Unit_Key=S.Collection_Unit_Key
LEFT JOIN Movement_Collection_Unit MCU ON MCU.Collection_Unit_Key=C.Collection_Unit_Key
LEFT JOIN Movement_Direction MD ON MD.Movement_Direction_Key=MCU.Movement_Direction_Key
LEFT JOIN Movement_Of_Material MOM ON MOM.Movement_Direction_Key=MD.Movement_Direction_Key
LEFT JOIN Movement_Of_Material_Exclusion MOME 
		ON MOME.Movement_Of_Material_Key=MOM.Movement_Of_Material_Key
		AND MOME.Collection_Unit_Key=@Key
LEFT JOIN Movement M 
		ON M.Movement_Key=MD.Movement_Key
		AND M.Movement_Type IN (4,5,7) -- destroyed, disposed, lost
WHERE 
		(C.Current_Container_Collection_Unit_Key=@Key
		OR C.Usual_Container_Collection_Unit_Key=@Key)
		AND (M.Movement_Key IS NULL
		OR MOME.Movement_Of_Material_Exclusion_Key IS NOT NULL)

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Store_PossibleChildStores_Select') AND SysStat & 0xf = 4)
BEGIN
    PRINT 'Setting up security on procedure usp_Store_PossibleChildStores_Select'
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
        GRANT EXECUTE ON dbo.usp_Store_PossibleChildStores_Select TO [R2k_Administrator]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
        GRANT EXECUTE ON dbo.usp_Store_PossibleChildStores_Select TO [R2k_FullEdit]
 		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
        GRANT EXECUTE ON dbo.usp_Store_PossibleChildStores_Select TO [R2k_RecordCardsOnly]
 		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        GRANT EXECUTE ON dbo.usp_Store_PossibleChildStores_Select TO [R2k_AddOnly]
 		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
        GRANT EXECUTE ON dbo.usp_Store_PossibleChildStores_Select TO [R2k_ReadOnly]
    IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_Store_PossibleChildStores_Select TO [Dev - JNCC SQL]
END

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Store_Select_ForNameSearch]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Store_Select_ForNameSearch]
GO

/*===========================================================================*\
  Description:	Returns a list of stores matching a search string.

  Parameters:	@SearchText

  Created:	August 2003

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Store_Select_ForNameSearch]
	@SearchText varchar(100),
	@UserDomainMask INT,
	@SessionID CHAR(16)
AS
	SELECT		S.Collection_Unit_Key AS Item_Key, 
				S.Item_Name + IsNull(' - ' + CU.Current_Location_Code, IsNull(' - ' + CU.Usual_Location_Code, '')) AS DisplayTerm, 
				S.Item_Name + IsNull(' - ' + CU.Current_Location_Code, IsNull(' - ' + CU.Usual_Location_Code, '')) AS SearchTerm
	FROM		Store S
	INNER JOIN 	COLLECTION_UNIT CU ON S.Collection_Unit_Key = CU.Collection_Unit_Key
     				AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Entered_Session_ID = @SessionID) 
						OR (CU.Changed_Session_ID = @SessionID) OR (CU.Domain_Mask = 0))
	WHERE		Item_Name LIKE @SearchText + '%'
	ORDER BY 	Item_Name
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Store_Select_ForNameSearch') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Store_Select_ForNameSearch'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Store_Select_ForNameSearch TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Store_Select_ForNameSearch TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Store_Select_ForNameSearch TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Store_Select_ForNameSearch TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Store_Select_ForNameSearch TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Store_Select_ForNameSearch TO [Dev - JNCC SQL]
END

GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_SubjectArea_Delete]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_SubjectArea_Delete]
GO

/*===========================================================================*\
  Description:	Deletes a Subject Area record.

  Parameters:	@Key	Subject_Area_Key
				@Timestamp

  Created:	December 2003

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SubjectArea_Delete]
	@Key char(16),
	@Timestamp timestamp = NULL
AS

SET NOCOUNT ON

	BEGIN TRANSACTION

		DELETE 
		FROM 	Subject_Area
		WHERE	Subject_Area_Key = @Key
		AND		((@Timestamp = Timestamp) OR (@Timestamp IS NULL))

		IF @@Error <> 0 GOTO RollbackAndExit

	COMMIT TRANSACTION
	RETURN 0
	
RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SubjectArea_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SubjectArea_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SubjectArea_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SubjectArea_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SubjectArea_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SubjectArea_Delete TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_SurveyEvent_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_SurveyEvent_Update]
GO

/*===========================================================================*\
  Description:	Updates a record into the Survey_Event table

  Parameters:	@Key char(16),
				@VagueDateStart int,
				@VagueDateEnd int,
				@VagueDateType varchar(2) = NULL,
				@SpatialRef varchar(40),
				@SpatialRefSystem varchar(4),
				@Lat float,
				@Long float,
				@SpatialRefQualifier varchar(20),
				@LocationKey char(16),
				@SurveyKey char(16),
				@Comment text,
				@ChangedBy char(16),
				@LocationName varchar(100)

  Created:		August 2004

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SurveyEvent_Update]
	@Key char(16),
	@VagueDateStart int,
	@VagueDateEnd int,
	@VagueDateType varchar(2) = NULL,
	@SpatialRef varchar(40),
	@SpatialRefSystem varchar(4),
	@Lat float,
	@Long float,
	@SpatialRefQualifier varchar(20),
	@LocationKey char(16),
	@SurveyKey char(16),
	@Comment text,
	@ChangedBy char(16),
	@LocationName varchar(100)
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION
		
		UPDATE 	Survey_Event
		SET 	Vague_Date_Start = @VagueDateStart, 
				Vague_Date_End = @VagueDateEnd, 
				Vague_Date_Type = @VagueDateType,
				Spatial_Ref = @SpatialRef, 
				Spatial_Ref_System = @SpatialRefSystem, 
				Spatial_Ref_Qualifier = @SpatialRefQualifier,
				Lat = @Lat, 
				Long = @Long, 
				Location_Key = @LocationKey,
				Survey_Key= @SurveyKey,
				Comment = @Comment,
				Location_Name = @LocationName,
				Changed_By = @ChangedBy,
				Changed_Date = GetDate()
		WHERE	Survey_Event_Key = @Key

		IF @@Error <> 0 GOTO RollbackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SurveyEvent_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SurveyEvent_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SurveyEvent_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SurveyEvent_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SurveyEvent_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SurveyEvent_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        GRANT EXECUTE ON dbo.usp_SurveyEvent_Update TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TaxonDetermination_Insert') AND SysStat & 0xf = 4)
	DROP PROCEDURE [dbo].[usp_TaxonDetermination_Insert]
GO

/*===========================================================================*\
  Description:	Inserts a record in the Taxon Determination table.
		Ensures the Domain mask of the specimen is also updated.

  Parameters:	@Key 
		@DeterminedItemKey
		@OccurrenceKey  
		@SpecimenCollectionUnitKey  
		@DeterminationTypeKey  
		@NomenclaturalStatusConceptKey 
		@Confidence
		@DeterminerNameKey 
		@InferredDeterminer 
		@DeterminerRoleKey 
		@VagueDateStart
		@VagueDateEnd 
		@VagueDateType 
		@UsedSpecimen 
		@Preferred
		@Method
		@Notes 
		@SessionID
		@IsForSpecimen		Indicates whether to update preferred 
					flag in Specimen_Unit or Determination.

  Created:	July 2003

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_TaxonDetermination_Insert]
	@Key char(16) output, 
	@DeterminedItemKey char(16), 
	@OccurrenceKey char(16), 
	@SpecimenCollectionUnitKey char(16), 
	@DeterminationTypeKey char(16), 
	@NomenclaturalStatusConceptKey char(16),
	@Confidence tinyint,
	@DeterminerNameKey char(16), 
	@InferredDeterminer tinyint,
	@DeterminerRoleKey char(16), 
	@VagueDateStart int, 
	@VagueDateEnd int, 
	@VagueDateType varchar(2),
	@UsedSpecimen bit,
	@Preferred bit,
	@Method text,
	@Notes text,
	@SessionID char(16),
	@IsForSpecimen bit
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF
	SET XACT_ABORT ON

	--Wrap everything in a transaction
	BEGIN TRANSACTION

		-- Get the user name key, as Taxon_Determination table doesn't have SessionID fields.
		DECLARE	@EnteredBy char(16),
			@PreferredForSpecimen bit

		SELECT	@EnteredBy = User_Name_Key FROM Session WHERE Session_ID = @SessionID
		SET	@PreferredForSpecimen = 0

		-- Get a new key first.
		EXECUTE spNextKey 'Taxon_Determination', @Key OUTPUT

		/*---------------------------------------------------------------------------------*\
		  Ensure only one preferred determination per occurrence.
		\*---------------------------------------------------------------------------------*/
		IF @IsForSpecimen = 1
		BEGIN
			-- Either Preferred passed in as true, or not preferred but not set in 
			-- Speciment Unit either.
			IF @Preferred = 1
			OR (@Preferred = 0 AND EXISTS ( SELECT * FROM Specimen_Unit 
							WHERE Collection_Unit_Key = @SpecimenCollectionUnitKey 
							AND Preferred_Taxon_Determination_Key IS NULL))
				SET @PreferredForSpecimen = 1

			-- Not used for Taxon_Occurrence, unless not already one present
			IF EXISTS(SELECT 1 FROM Taxon_Determination WHERE Taxon_Occurrence_Key=@OccurrenceKey AND Preferred=1)
				SET @Preferred = 0
		END ELSE BEGIN
			-- If new determination not preferred, but there isn't one already, change the flag.
			IF @Preferred = 0 
			AND NOT EXISTS(SELECT * FROM Taxon_Determination WHERE Taxon_Occurrence_Key = @OccurrenceKey AND Preferred = 1)
				SET @Preferred = 1
			ELSE
			-- If new determination is preferred, make sure previous preferred is turned off.
			IF @Preferred = 1
			BEGIN
				UPDATE	Taxon_Determination
				SET	Preferred = 0
				WHERE	Taxon_Occurrence_Key = @OccurrenceKey
				AND	Preferred = 1

				IF @@Error <> 0 GOTO RollbackAndExit
			END
		END

		/*---------------------------------------------------------------------------------*\
		  Do table insert.
		\*---------------------------------------------------------------------------------*/
		INSERT INTO Taxon_Determination (
			Taxon_Determination_Key, Taxon_List_Item_Key, Taxon_Occurrence_Key, 
			Vague_Date_Start, Vague_Date_End, Vague_Date_Type, Comment, Preferred, Determiner, 
			Determination_Type_Key, Determiner_Role_Key, Entered_By, Entry_Date, 
			Specimen_Collection_Unit_Key, Nomenclatural_Status_Concept_Key,
			Confidence, Used_Specimen, Method, Inferred_Determiner
		) VALUES (
			@Key, @DeterminedItemKey, @OccurrenceKey, 
			@VagueDateStart, @VagueDateEnd, @VagueDateType, @Notes, @Preferred, @DeterminerNameKey,
			@DeterminationTypeKey, @DeterminerRoleKey, @EnteredBy, GetDate(), 
			@SpecimenCollectionUnitKey, @NomenclaturalStatusConceptKey, 
			@Confidence, @UsedSpecimen, @Method, @InferredDeterminer
		)

		IF @@Error <> 0 GOTO RollbackAndExit

		IF @IsForSpecimen = 0 AND @Preferred = 1
		BEGIN
			-- Update validation flag of Taxon_Occurrence
			DECLARE	@ValidationLevel int
			SELECT	@ValidationLevel = Validation_Level
			FROM	Taxon_List_Item TLI 
			JOIN	Taxon_Version TV ON TV.Taxon_Version_Key = TLI.Taxon_Version_Key
			WHERE	TLI.Taxon_List_Item_Key = @DeterminedItemKey

			DECLARE	@CompetencyLevel int
			SELECT	@CompetencyLevel = DR.Validation_Competency
			FROM	Taxon_Determination TD 
			JOIN	Determiner_Role DR ON DR.Determiner_Role_Key = TD.Determiner_Role_Key
			WHERE	TD.Taxon_Determination_Key = @Key

			UPDATE	Taxon_Occurrence
			SET	Verified = 
					CASE 
						WHEN @ValidationLevel IS NULL THEN 0
						WHEN @ValidationLevel <= @CompetencyLevel THEN 2
						ELSE 1
					END
			WHERE	Taxon_Occurrence_Key = @OccurrenceKey
		END ELSE
		IF @PreferredForSpecimen = 1
		BEGIN
			UPDATE	Specimen_Unit
			SET	Preferred_Taxon_Determination_Key = @Key
			WHERE	Collection_Unit_Key = @SpecimenCollectionUnitKey

			IF @@Error <> 0 GOTO RollbackAndExit
		END

		/*-------------------------------------------------------------*\
		  Switch bit of new mask ON in Collection_Unit.
		\*-------------------------------------------------------------*/
		IF @IsForSpecimen = 1 AND @PreferredForSpecimen = 1
		BEGIN
			DECLARE @ConceptKey char(16),
				@ConceptMask int

			-- Get the right concept before getting the mask
			SELECT	@ConceptKey = Concept_Key
			FROM	Taxon_Dictionary_Concept_Mapping
			WHERE	Taxon_List_Item_Key = @DeterminedItemKey

			-- Retrieve the mask of the new concept.
			EXECUTE	usp_Get_Concept_Domain_Mask @ConceptKey, @ConceptMask OUTPUT
			-- And switch appropriate bit ON in Collection_Unit
			EXECUTE	usp_CollectionUnit_Update_DomainMask @SpecimenCollectionUnitKey, @ConceptMask, 1

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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TaxonDetermination_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_TaxonDetermination_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_TaxonDetermination_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_TaxonDetermination_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_TaxonDetermination_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_TaxonDetermination_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_TaxonDetermination_Insert TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_TaxonListItem_ImportConceptGroup]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_TaxonListItem_ImportConceptGroup]
GO

/*===========================================================================*\
  Description:	Import taxon list items corresponding to the concepts in a
				concept group.

  Parameters:	@job_id					Job identifier

  Created:		Dec 2003

  Last revision information:
	$Revision: 7 $
	$Date: 6/02/09 10:41 $
	$Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_TaxonListItem_ImportConceptGroup]
	@job_id				INT
AS
	SET NOCOUNT ON

	DECLARE     @concept_group_key		CHAR(16),
				@concept_key			CHAR(16),
				@taxon_list_item_key	CHAR(16),
				@term_key				CHAR(16),
				@term_version_key		CHAR(16),
				@list_preferred			BIT,
				@meaning_key			CHAR(16),
				@taxon_key				CHAR(16),
				@taxon_version_key		CHAR(16),
				@preferred_name			CHAR(16),
				@sort_code				INT,
				@taxon_rank_key			CHAR(16),
				@ins_user_key			CHAR(16),
				@ins_date				SMALLDATETIME,
				@upd_user_key			CHAR(16),
				@upd_date				SMALLDATETIME,
				@system					BIT,
				@from_list_version_key	CHAR(16),
				@to_list_version_key	CHAR(16),
				@is_new					BIT

	/* determine parameters of job */
	SELECT      @concept_group_key			=	j.Concept_Group_Key
	FROM		Import_Export_Job			AS	j
	WHERE		j.Import_Export_Job_ID		=	@job_id

	IF @@ROWCOUNT = 0
	BEGIN
		RAISERROR ('Job does not exist or has not been configured', 16, 1)
		GOTO fail
	END

	EXECUTE		usp_Import_Export_Job_UpdateStatus	@job_id,
													'Exporting concepts'
	IF @@ERROR <> 0 GOTO fail

	DECLARE		concepts	CURSOR FAST_FORWARD LOCAL FOR
	SELECT		c.Concept_Key,
				c.Term_Key,
				c.Term_Version_Key,
				c.List_Preferred,
				c.Meaning_Key,
				c.Sort_Code,
				crm.Taxon_Rank_Key,
				es.User_Name_Key,
				CONVERT(SMALLDATETIME,
						CONVERT(VARCHAR, es.Date_Time_Start, 112)),
				cs.User_Name_Key,
				CONVERT(SMALLDATETIME,
						CONVERT(VARCHAR, cs.Date_Time_Start, 112)),
				c.System_Supplied_Data
	FROM		Concept									AS	c
	INNER JOIN	Taxon_Dictionary_Concept_Rank_Mapping	AS	crm
	ON			crm.Concept_Rank_Key					=	c.Concept_Rank_Key
	INNER JOIN	Session									AS	es
	ON			es.Session_ID							=	c.Entered_Session_ID
	LEFT JOIN	Session									AS	cs
	ON			cs.Session_ID							=	c.Changed_Session_ID
	WHERE		c.Concept_Group_Key						=	@concept_group_key
	ORDER BY	c.List_Preferred DESC	/* i.e. preferred names first */

	OPEN		concepts

	WHILE 1 = 1
	BEGIN
		FETCH		concepts
		INTO		@concept_key,
					@term_key,
					@term_version_key,
					@list_preferred,
					@meaning_key,
					@sort_code,
					@taxon_rank_key,
					@ins_user_key,
					@ins_date,
					@upd_user_key,
					@upd_date,
					@system

		IF @@FETCH_STATUS <> 0 BREAK

		BEGIN TRANSACTION

		SET ROWCOUNT 1

		SELECT      @from_list_version_key							=	gvm.Taxon_List_Version_Key
		FROM		Concept_History									AS	h
		INNER JOIN	Concept_Group_Version							AS	gv
		ON			gv.Concept_Group_Version_Key					=	h.Concept_Group_Version_From
		INNER JOIN	Taxon_Dictionary_Concept_Group_Version_Mapping	AS	gvm
		ON			gvm.Concept_Group_Version_Key					=	gv.Concept_Group_Version_Key
		WHERE		h.Concept_Key									=	@concept_key
		ORDER BY	gv.Sequence

		--If the from list version is null (because the thesaurus allows no history), 
		--then use the first version available	
		SET ROWCOUNT 1
		
		IF @from_list_version_key IS NULL
			SELECT @from_list_version_key = Taxon_List_Version_Key
			FROM Concept c
			INNER JOIN Taxon_Dictionary_Concept_Group_Mapping cgm ON cgm.Concept_Group_Key=c.Concept_Group_Key
			INNER JOIN Taxon_List_Version tlv on tlv.Taxon_list_Key=cgm.Taxon_List_Key
			WHERE c.Concept_Key=@concept_Key
			ORDER BY tlv.Version ASC

		SET ROWCOUNT 0

		SELECT		@to_list_version_key							=	gvm.Taxon_List_Version_Key
		FROM		Concept_History									AS	h
		INNER JOIN	Concept_Group_Version							AS	gv
		ON			gv.Concept_Group_Version_Key					=	h.Concept_Group_Version_To
		INNER JOIN	Taxon_Dictionary_Concept_Group_Version_Mapping	AS	gvm
		ON			gvm.Concept_Group_Version_Key					=	gv.Concept_Group_Version_Key
		WHERE		h.Concept_Key									=	@concept_key
		ORDER BY	gv.Sequence DESC

		IF @@ROWCOUNT = 0
		BEGIN
			SET			@to_list_version_key	=	NULL
		END

		/* we do the term and term version mappings inside the 'SET ROWCOUNT 1'
		 * because a single term may map onto multiple taxa; we need to choose
		 * exactly one, but any one will do
		 */
		SELECT		@taxon_key						=	Taxon_Key
		FROM		Taxon_Dictionary_Term_Mapping
		WHERE		Term_Key						=	@term_key

		IF @@ROWCOUNT = 0 GOTO skip_item

		IF @term_version_key IS NULL
		BEGIN
			SET			@taxon_version_key	=	NULL
		END
		ELSE
		BEGIN
			SELECT		@taxon_version_key						=	m.Taxon_Version_Key
			FROM		Taxon_Dictionary_Term_Version_Mapping	AS	m
			INNER JOIN	TAXON_VERSION							AS	tv
			ON			tv.TAXON_VERSION_KEY					=	m.Taxon_Version_Key
			WHERE		m.Term_Version_Key						=	@term_version_key
			AND			tv.TAXON_KEY							=	@taxon_key

			IF @@ROWCOUNT = 0 GOTO skip_item				
		END

		SET ROWCOUNT 0

		/* check for existing mapping */
		SELECT		@taxon_list_item_key				=	Taxon_List_Item_Key
		FROM		Taxon_Dictionary_Concept_Mapping
		WHERE		Concept_Key							=	@concept_key

		SET			@is_new		=	CASE WHEN @@ROWCOUNT = 0 THEN 1 ELSE 0 END

		IF @is_new = 1
		BEGIN
			EXECUTE		spNextKey	'TAXON_LIST_ITEM',
									@taxon_list_item_key	OUTPUT
			IF @@ERROR <> 0 GOTO fail_from_cursor
		END

		/* work out preferred name */
		IF @list_preferred = 1
		BEGIN
			SET			@preferred_name		=	@taxon_list_item_key
		END
		ELSE
		BEGIN
			SET			@preferred_name						=	NULL

			SET ROWCOUNT 1

			SELECT		@preferred_name						=	m.Taxon_List_Item_Key
			FROM		Concept								AS	c
			INNER JOIN 	Taxon_Dictionary_Concept_Mapping	AS	m
			ON			m.Concept_Key						=	c.Concept_Key
			WHERE		c.Concept_Group_Key					=	@concept_group_key
			AND			c.Meaning_Key						=	@meaning_key
			AND			c.List_Preferred					=	1

			SET ROWCOUNT 0
		END

		IF @preferred_name IS NOT NULL AND @taxon_version_key IS NULL
		BEGIN
			/* create a minimal taxon version */
			EXECUTE		spNextKey	'TAXON_VERSION',
									@taxon_version_key	OUTPUT
			IF @@ERROR <> 0 GOTO fail_from_cursor

			INSERT		TAXON_VERSION (
						TAXON_VERSION_KEY,
						TAXON_KEY,
						ENTERED_BY,
						ENTRY_DATE,
						SYSTEM_SUPPLIED_DATA)
			VALUES		(@taxon_version_key,
						@taxon_key,
						@ins_user_key,
						@ins_date,
						@system)

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END

		IF @is_new = 0
		BEGIN
			IF @preferred_name IS NULL
			BEGIN
				/* concept is not selectable; remove any list items */
				DELETE		TAXON_LIST_ITEM
				WHERE		TAXON_LIST_ITEM_KEY		=	@taxon_list_item_key

				IF @@ERROR <> 0 GOTO fail_from_cursor
			END
			ELSE
			BEGIN
				/* update taxon list item */
				UPDATE		TAXON_LIST_ITEM
				SET			TAXON_VERSION_KEY		=	@taxon_version_key,
							TAXON_LIST_VERSION_KEY	=	@from_list_version_key,
							TAXON_LIST_VERSION_TO	=	@to_list_version_key,
							PREFERRED_NAME			=	@preferred_name,
							SORT_CODE				=	@sort_code,
							TAXON_RANK_KEY			=	@taxon_rank_key,
							ENTERED_BY				=	@ins_user_key,
							ENTRY_DATE				=	@ins_date,
							CHANGED_BY				=	@upd_user_key,
							CHANGED_DATE			=	@upd_date,
							SYSTEM_SUPPLIED_DATA	=	@system
				WHERE		TAXON_LIST_ITEM_KEY		=	@taxon_list_item_key

				IF @@ERROR <> 0 GOTO fail_from_cursor
			END
		END
		ELSE
		BEGIN
			/* create taxon list item */
			INSERT		TAXON_LIST_ITEM (
						TAXON_LIST_ITEM_KEY,
						TAXON_VERSION_KEY,
						TAXON_LIST_VERSION_KEY,
						TAXON_LIST_VERSION_TO,
						PREFERRED_NAME,
						SORT_CODE,
						TAXON_RANK_KEY,
						ENTERED_BY,
						ENTRY_DATE,
						CHANGED_BY,
						CHANGED_DATE,
						SYSTEM_SUPPLIED_DATA)
			VALUES		(@taxon_list_item_key,
						@taxon_version_key,
						@from_list_version_key,
						@to_list_version_key,
						@preferred_name,
						@sort_code,
						@taxon_rank_key,
						@ins_user_key,
						@ins_date,
						@upd_user_key,
						@upd_date,
						@system)

			IF @@ERROR <> 0 GOTO fail_from_cursor

			/* record mapping */
			INSERT		Taxon_Dictionary_Concept_Mapping (
						Taxon_List_Item_Key,
						Concept_key)
			VALUES		(@taxon_list_item_key,
						@concept_key)

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END
		
skip_item:
		EXECUTE		usp_Import_Export_Job_RecordProcessed	@job_id
		IF @@ERROR <> 0 GOTO fail_from_cursor

		COMMIT TRANSACTION
	END

	CLOSE		concepts
	DEALLOCATE	concepts
	RETURN

fail_from_cursor:
	CLOSE		concepts
	DEALLOCATE	concepts

fail:
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_TaxonListItem_ImportConceptGroup failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TaxonListItem_ImportConceptGroup') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_TaxonListItem_ImportConceptGroup'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_TaxonListItem_ImportConceptGroup TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_TaxonListItem_ImportConceptGroup TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_TaxonListItem_ImportConceptGroup TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects
	   WHERE  Id = Object_Id(N'[dbo].[usp_TaxonVersion_ImportConceptGroup]')
	   AND    Type = 'P')
	DROP PROCEDURE [dbo].[usp_TaxonVersion_ImportConceptGroup]
GO

/*===========================================================================*\
  Description:	Import taxon versions corresponding to items in a concept
				group.

  Parameters:	@job_id					Job identifier

  Created:		Dec 2003

  Last revision information:
	$Revision: 7 $
	$Date: 6/02/09 10:41 $
	$Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_TaxonVersion_ImportConceptGroup]
	@job_id					INT,
	@SessionID			CHAR(16)
AS
	SET NOCOUNT ON

 DECLARE     @concept_group_key			CHAR(16),
				@term_version_key			CHAR(16),
				@taxon_key					CHAR(16),
				@attribute					VARCHAR(10),
				@entered_by					CHAR(16),
				@entry_date					SMALLDATETIME,
				@changed_by					CHAR(16),
				@changed_date				SMALLDATETIME,
				@system						BIT,
				@taxon_version_key			CHAR(16),
				@source_key					CHAR(16),
				@source_join_key			CHAR(16),
				@prior_term_version_key		CHAR(16),
				@concept_key	CHAR(16),
				@term_key		CHAR(16)


	/* determine parameters of job */
	SELECT      @concept_group_key			=	j.Concept_Group_Key
	FROM		Import_Export_Job			AS	j
	WHERE		j.Import_Export_Job_ID		=	@job_id

	IF @@ROWCOUNT = 0
	BEGIN
		RAISERROR ('Job does not exist or has not been configured', 16, 1)
		GOTO fail
	END

	EXECUTE		usp_Import_Export_Job_UpdateStatus	@job_id,
													'Exporting term versions'
	IF @@ERROR <> 0 GOTO fail
 

	DECLARE		versions	CURSOR LOCAL FAST_FORWARD FOR
	SELECT DISTINCT
				tv.Term_Version_Key,
				tm.Taxon_Key,
				tv.Version_Label,
				es.User_Name_Key,
				CONVERT(SMALLDATETIME,
						CONVERT(VARCHAR, es.Date_Time_Start, 112)),
				cs.User_Name_Key,
				CONVERT(SMALLDATETIME,
						CONVERT(VARCHAR, cs.Date_Time_Start, 112)),
				ISNULL(tv.System_Supplied_Data, c.System_Supplied_Data),
				c.Concept_Key,
				tm.Term_key
	FROM		Concept							AS	c
	LEFT JOIN	Term_Version					AS	tv
	ON			tv.Term_Version_Key				=	c.Term_Version_Key
	INNER JOIN	Taxon_Dictionary_Term_Mapping	AS	tm
	ON			tm.Term_Key						=	c.Term_Key
	INNER JOIN	Session							AS	es
	ON			es.Session_ID					=	 ISNULL(tv.Entered_Session_ID, @SessionID)
	LEFT JOIN	Session							AS	cs
	ON			cs.Session_ID					=	tv.Changed_Session_ID
	WHERE		c.Concept_Group_Key				=	@concept_group_key
	ORDER BY	tv.Term_Version_Key

	SET			@prior_term_version_key			=	''

	OPEN		versions

	WHILE 1 = 1
	BEGIN
		FETCH		versions
		INTO		@term_version_key,
					@taxon_key,
					@attribute,
					@entered_by,
					@entry_date,
					@changed_by,
					@changed_date,
					@system,
					@concept_key,
					@term_key

		IF @@FETCH_STATUS <> 0 BREAK

		BEGIN TRANSACTION

		SELECT      @taxon_version_key						=	NULL,
					@source_join_key						=	NULL,
					@source_key								=	NULL

		SELECT		@taxon_version_key						=	m.Taxon_Version_Key,
					@source_key								=	j.Source_Key
		FROM		Taxon_Dictionary_Term_Version_Mapping	AS	m
		LEFT JOIN	Source_Join								AS	j
		ON			j.Source_Join_Key						=	m.Source_Join_Key
		WHERE		m.Term_Version_Key						=	@term_version_key

		IF @source_key IS NULL
		BEGIN
			/* there is no existing mapping for the source join; pick an
			 * arbitrary join record (if there are any) and make this the
			 * mapped join.
			 */
			SELECT		@source_join_key	=	Source_Join_Key,
						@source_key			=	Source_Key
			FROM		Source_Join
			WHERE		Record_Key			=	@term_version_key
			AND			Table_Name			=	'Term_Version'
			ORDER BY	Source_Join_Key
		END

		IF @taxon_version_key IS NOT NULL
		BEGIN
			/* update taxon version */
			UPDATE		TAXON_VERSION
			SET			TAXON_KEY				=	@taxon_key,
						ATTRIBUTE				=	@attribute,
						SOURCE_KEY				=	@source_key,
						ENTERED_BY				=	@entered_by,
						ENTRY_DATE				=	@entry_date,
						CHANGED_BY				=	@changed_by,
						CHANGED_DATE			=	@changed_date,
						SYSTEM_SUPPLIED_DATA   	=	@system
			WHERE		TAXON_VERSION_KEY		=	@taxon_version_key

			IF @@ERROR <> 0 GOTO fail_from_cursor

			IF @source_join_key IS NOT NULL
			BEGIN
				UPDATE		Taxon_Dictionary_Term_Version_Mapping
				SET			Source_Join_Key							=	@source_join_key
				WHERE		Taxon_Version_Key						=	@taxon_version_key

				IF @@ERROR <> 0 GOTO fail_from_cursor
			END
		END
		ELSE
		BEGIN
			/* create taxon version */
			EXECUTE		spNextKey		'TAXON_VERSION',
										@taxon_version_key	OUTPUT
			IF @@ERROR <> 0 GOTO fail_from_cursor

			INSERT		TAXON_VERSION (
						TAXON_VERSION_KEY,
						TAXON_KEY,
						ATTRIBUTE,
						UK_NATIVE,
						SOURCE_KEY,
						ENTERED_BY,
						ENTRY_DATE,
						CHANGED_BY,
						CHANGED_DATE,
						SYSTEM_SUPPLIED_DATA)
			VALUES		(@taxon_version_key,
						@taxon_key,
						@attribute,
						0,
						@source_key,
						@entered_by,
						@entry_date,
						@changed_by,
						@changed_date,
						@system)

			IF @@ERROR <> 0 GOTO fail_from_cursor

			/* If no term version, then create one for synchronisation reasons */
			IF @Term_Version_Key IS NULL
			BEGIN
				EXECUTE		spNextKey		'TERM_VERSION',
										@term_version_key	OUTPUT

				INSERT INTO Term_Version (
						Term_Version_Key,
						Term_Key,
						Entered_Session_ID,
						System_Supplied_Data)
				VALUES (
						@Term_Version_Key,
						@Term_Key,
						@SessionId,
						@system)
				
				UPDATE Concept 
				SET Term_Version_Key=@Term_Version_Key
				WHERE Concept_Key=@Concept_Key

			END

			/* record mapping */
			INSERT		Taxon_Dictionary_Term_Version_Mapping (
						Taxon_Version_Key,
						Term_Version_Key,
						Source_Join_Key)
			VALUES		(@taxon_version_key,
						@term_version_key,
						@source_join_key)

			IF @@ERROR <> 0 GOTO fail_from_cursor
		END

		IF @term_version_key <> @prior_term_version_key
		BEGIN
			/* Use of @prior_term_version_key is a hack for the case where
			 * a single Term corresponds to multiple Taxon records; we don't
			 * increment the progress count until all the taxa have been
			 * considered.
			 */
			SET			@prior_term_version_key		=	@term_version_key

		END

		COMMIT TRANSACTION
	END

	CLOSE		versions
	DEALLOCATE	versions
	RETURN

fail_from_cursor:
	CLOSE		versions
	DEALLOCATE	versions

fail:
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_TaxonVersion_ImportConceptGroup failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TaxonVersion_ImportConceptGroup') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_TaxonVersion_ImportConceptGroup'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_TaxonVersion_ImportConceptGroup TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_TaxonVersion_ImportConceptGroup TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_TaxonVersion_ImportConceptGroup TO [Dev - JNCC SQL]
END
GO

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
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
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
		AND		((@Timestamp = Timestamp) OR (@Timestamp IS NULL))
	
		IF @@Error <> 0 GOTO RollbackAndExit

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
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_TermVersion_Insert]
	@Key char(16) OUTPUT,
	@ConceptKey char(16),
	@VersionLabel varchar(100),
	@AuthorAndDate varchar(100),
	@SessionID char(16),
	@SyncTaxonDict bit = 0,
	@SystemSuppliedData bit = NULL
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION

		DECLARE @TermKey char(16)

		SELECT 	@TermKey = Term_Key
		FROM	Concept
		WHERE	Concept_Key = @ConceptKey

		/*-------------------------------------------------------------*\
		  Insert in Term_Version, only if there are details.
			Otherwise, ensure concept is not linked to a term version
		\*-------------------------------------------------------------*/
		IF (@VersionLabel IS NULL AND @AuthorAndDate IS NULL) OR 
				(@VersionLabel = '' AND @AuthorAndDate = '') BEGIN

			UPDATE Concept
			SET Term_Version_Key=NULL
			WHERE Concept_Key=@ConceptKey
			IF @@Error <> 0 GOTO RollbackAndExit

			SET @Key=NULL
		
		END
		ELSE BEGIN
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
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_TermVersion_Update]
	@Key char(16),
	@ConceptKey char(16),
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

		DECLARE @TermKey char(16)
		DECLARE @TaxonVersionKey char(16)
		DECLARE @Error int

		SELECT 	@TermKey = Term_Key
		FROM	Concept
		WHERE	Concept_Key = @ConceptKey

		SELECT 	@TaxonVersionKey = Taxon_Version_Key
		FROM	Taxon_Dictionary_Term_Version_Mapping
		WHERE	Term_Version_Key = @Key


		IF @@Error <> 0 GOTO RollbackAndExit


		IF (@VersionLabel IS NULL AND @AuthorAndDate IS NULL) OR 
				(@VersionLabel = '' AND @AuthorAndDate = '') 
		BEGIN
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

					DELETE	Taxon_Dictionary_Term_Version_Mapping	
					WHERE	Taxon_Version_Key = @TaxonVersionKey
					AND		Term_Version_Key = @Key
	
					IF @@Error <> 0 GOTO RollbackAndExit
	
					DELETE	Taxon_Version_Relation
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
							Source_Key = NULL
					WHERE	Taxon_Version_Key = @TaxonVersionKey

					IF @@Error <> 0 GOTO RollbackAndExit

					--Also need to keep the index up to date
					UPDATE ITN
					SET ITN.Authority=Null
					FROM Index_Taxon_Name ITN
					INNER JOIN Taxon_List_Item TLI ON TLI.Taxon_List_Item_Key=ITN.Taxon_List_Item_Key
					WHERE TLI.Taxon_Version_Key=@TaxonVersionKey

					IF @@Error <> 0 GOTO RollbackAndExit

				END
			END

			-- When clearing a term version, remove the link to the record		
			UPDATE 	Concept
			SET 	Term_Version_Key = NULL
			WHERE 	Concept_Key = @ConceptKey			
			
			-- And remove the Term_Version record if not referred to anymore
			DELETE TV
			FROM Term_Version TV
			LEFT JOIN Concept C ON C.Term_Version_Key=TV.Term_Version_Key
			WHERE TV.Term_Version_Key=@Key
			AND C.Concept_Key IS NULL
			
			SELECT @RecordsAffected = 1
		END
		ELSE BEGIN
			UPDATE 	Term_Version
			SET 	Term_Key = @TermKey,
					Version_Label = @VersionLabel,
					Author_And_Date = @AuthorAndDate,
					Changed_Session_ID = @SessionID
			WHERE	Term_Version_Key = @Key
			AND		((@Timestamp = Timestamp)) OR (@Timestamp IS NULL)
	
			SELECT	@RecordsAffected = @@RowCount,
				@Error = @@Error
	
			IF @Error <> 0 GOTO RollbackAndExit
	
			/*-------------------------------------------------------------*\
			  Update Concept to point to new version
			\*-------------------------------------------------------------*/
			UPDATE 	Concept
			SET 	Term_Version_Key = @Key
			WHERE 	Concept_Key = @ConceptKey
	
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

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ThesaurusFact_Delete]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ThesaurusFact_Delete]
GO

/*===========================================================================*\
  Description:	Delete a record from the Thesaurus_Fact table.

  Parameters:	@Key		Thesaurus Fact key
		@Timestamp

  Created:	December 2003

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ThesaurusFact_Delete]
	@Key char(16),
	@Timestamp timestamp = NULL,
	@SyncTaxonDict bit = 0
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION

		/*============================================================*\
		  See if the user wants any associated taxon dictionary
		  records be deleted with the concept.
		\*============================================================*/
		IF @SyncTaxonDict = 1 
		BEGIN
			/*--------------------------------------------------------*\
			  Check that the Taxon_Fact table exists before
			  attempting any of this deletion. 		
			\*--------------------------------------------------------*/
			IF EXISTS (SELECT *
		   			FROM   SysObjects 
					WHERE  Id = Object_Id(N'[dbo].[Taxon_Fact]')
					AND 	  Type = 'U')
			BEGIN
				DECLARE	@TaxonFactKey char(16)

				SELECT 	@TaxonFactKey = Taxon_Fact_Key
				FROM 	Taxon_Dictionary_Thesaurus_Fact_Mapping
				WHERE	Thesaurus_Fact_Key = @Key

				IF @@Error <> 0 GOTO RollbackAndExit

				DELETE	Taxon_Dictionary_Thesaurus_Fact_Mapping
				WHERE	Thesaurus_Fact_Key = @Key

				IF @@Error <> 0 GOTO RollbackAndExit

				DELETE	Taxon_Fact
				WHERE	Taxon_Fact_Key = @TaxonFactKey

				IF @@Error <> 0 GOTO RollbackAndExit

			END
		END	
		ELSE BEGIN
			DELETE	Taxon_Dictionary_Thesaurus_Fact_Mapping
			WHERE	Thesaurus_Fact_Key = @Key

			IF @@Error <> 0 GOTO RollbackAndExit
		END

		DELETE	Thesaurus_Fact
		WHERE	Thesaurus_Fact_Key = @Key
		AND		((@Timestamp = Timestamp) OR (@Timestamp IS NULL))

		IF @@Error <> 0 GOTO RollbackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ThesaurusFact_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ThesaurusFact_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ThesaurusFact_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ThesaurusFact_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ThesaurusFact_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ThesaurusFact_Delete TO [Dev - JNCC SQL]
END
GO

/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ThesaurusFact_Insert]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ThesaurusFact_Insert]
GO

/*===========================================================================*\
  Description:	Inserts a record into the Thesaurus_Fact table

  Parameters:	@Key	Thesaurus_Fact_Key

  Created:	December 2003

  Last revision information:
    $Revision: 7 $
    $Date: 6/02/09 10:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ThesaurusFact_Insert]
	@Key char(16) OUTPUT,
	@ItemName varchar(100),
	@Data text,
	@MeaningKey char(16),
	@ConceptKey char(16),
	@TermVersionKey char(16),
	@RelatedTermVersions bit,
	@Inherited bit,
	@LanguageKey varchar(4),
	@FactTypeMeaningKey char(16),
	@FactTypeMeaningName varchar(100),
	@FactVagueDateStart int,
	@FactVagueDateEnd int,
	@FactVagueDateType varchar(2) = NULL,
	@SessionID char(16),
	@SystemSuppliedData bit = NULL
	
AS


	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	/*-------------------------------------------------------------*\
	  Get a new key.
	\*-------------------------------------------------------------*/
	EXECUTE spNextKey 'Thesaurus_Fact', @Key OUTPUT

	DECLARE @FactTypeConceptKey char(16)

	/*-------------------------------------------------------------*\
	  There is no transaction in this proc, as the Delphi code
		wraps the whole lot in a transaction.
	\*-------------------------------------------------------------*/

	-- The combo box stores the meaning key and the item name. This meaning key
	-- needs to be converted to a concept key. Because many concepts can
	-- share the same meaning key, we have to use the meaning key and the item name.
	SELECT 		@FactTypeConceptKey = Concept_Key
	FROM 		Concept AS C
	INNER JOIN 	Term AS T ON T.Term_Key = C.Term_Key
	WHERE 		C.Meaning_Key = @FactTypeMeaningKey
	AND 		T.Item_Name = @FactTypeMeaningName

	/*-------------------------------------------------------------*\
	  Insert in Thesaurus_Fact.
	\*-------------------------------------------------------------*/
	INSERT INTO Thesaurus_Fact (
		Thesaurus_Fact_Key,
		Item_Name,
		Data,
		Meaning_Key,
		Concept_Key,
		Term_Version_Key,
		Related_Term_Versions,
		Inherited,
		Language_Key,
		Fact_Vague_Date_Start,
		Fact_Vague_Date_End,
		Fact_Vague_Date_Type,
		Fact_Type_Concept_Key,
		Entered_Session_ID,
		System_Supplied_Data
	) VALUES (
		@Key,
		@ItemName,
		@Data,
		@MeaningKey,
		@ConceptKey,
		@TermVersionKey,
		@RelatedTermVersions,
		@Inherited,
		@LanguageKey,
		@FactVagueDateStart,
		@FactVagueDateEnd,
		IsNull(@FactVagueDateType, 'U'),
		@FactTypeConceptKey,
		@SessionID,
		IsNull(@SystemSuppliedData, 0)
	)

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ThesaurusFact_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ThesaurusFact_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ThesaurusFact_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ThesaurusFact_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ThesaurusFact_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ThesaurusFact_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ThesaurusFact_Insert TO [Dev - JNCC SQL]
END

GO

If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_ReportBlockOrders_Select]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_ReportBlockOrders_Select]
GO

CREATE PROCEDURE [dbo].[usp_ReportBlockOrders_Select] 
@ReportBlockKey CHAR(16)

AS

--  DESCRIPTION
--  Returns the allowed ordering that can be applied to a Report Block
--
--  PARAMETERS
--	NAME				DESCRIPTION
--	@ReportBlockKey		Report Block which ordering is to be found
--
--
--  AUTHOR:     		Ben Collier, Dorset Software
--  CREATED:    		2004-05-01
--
SET NOCOUNT ON

SELECT Report_Block_Order_Key, Item_Name, Order_Clause_SQL
FROM
	Report_Block_Order
WHERE Report_Block_Key = @ReportBlockKey
ORDER BY Item_Name

GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ReportBlockOrders_Select') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ReportBlockOrders_Select'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ReportBlockOrders_Select TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ReportBlockOrders_Select TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ReportBlockOrders_Select TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_ReportBlockOrders_Select TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ReportBlockOrders_Select TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ReportBlockOrders_Select TO [Dev - JNCC SQL]
END

GO
