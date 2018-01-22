/*===========================================================================*\
  Description:	Renames the 'Measurement Parameters and Descriptor Parameters'
		Concept Group into 'Measurment Parameters'. Creates a new
		Concept Group called 'Descriptor Parameters'. Then duplicates
		all of the Concepts from the original CG to the new one.

  Created:	July 2004

  Last revision information:
    $Revision: 5 $
    $Date: 24/09/08 15:59 $
    $Author: Simonwood $

\*===========================================================================*/
SET NOCOUNT OFF
-- Set of options for better use of vw_ConceptTerm.
SET ANSI_NULLS ON
SET ANSI_PADDING ON
SET ANSI_WARNINGS ON
SET ARITHABORT ON
SET CONCAT_NULL_YIELDS_NULL ON
SET QUOTED_IDENTIFIER ON
SET NO_BROWSETABLE OFF

GO

BEGIN TRANSACTION

	/*===========================================================================*\
	  Change the name of the currently existing Concept Group.
	\*===========================================================================*/
	UPDATE 	Concept_Group
	SET	Item_Name = 'Measurement Parameters'
	WHERE	Concept_Group_Key = 'SYSTEM000000000E'

	IF @@Error <> 0 GOTO RollbackAndExit

	/*===========================================================================*\
	  Create the new Concept Group by duplicating the details of the current one.
	\*===========================================================================*/
	DECLARE @NewConceptGroupKey char(16)
	SET @NewConceptGroupKey = 'SYSTEM000000000W'

	IF NOT EXISTS(SELECT * FROM Concept_Group WHERE Concept_Group_Key = @NewConceptGroupKey)
		INSERT INTO Concept_Group (
			Concept_Group_Key, 
			Local_Domain_Key, 
			Item_Name, 
			Authority,
			URL,
			Hierarchy_Relation_Type_Key, 
			Entered_Session_ID, 
			System_Supplied_Data
		) 
		SELECT @NewConceptGroupKey,
			Local_Domain_Key,
			'Descriptor Parameters',
			Authority,
			URL,
			Hierarchy_Relation_Type_Key,
			Entered_Session_ID,
			System_Supplied_Data
		FROM 	Concept_Group
		WHERE	Concept_Group_Key = 'SYSTEM000000000E'

	IF @@Error <> 0 GOTO RollbackAndExit	

	/*===========================================================================*\
	  Duplicate the Concept Group Version records for the new Concept Group.
	\*===========================================================================*/
	DECLARE @OriginalCGVKey char(16),
		@NewCGVKey char(16)

	DECLARE curCGV CURSOR LOCAL FAST_FORWARD FOR
		SELECT	Concept_Group_Version_Key
		FROM	Concept_Group_Version
		WHERE	Concept_Group_Key = 'SYSTEM000000000E'

	OPEN curCGV

	FETCH NEXT
	FROM	curCGV
	INTO	@OriginalCGVKey

	WHILE @@Fetch_Status = 0
	BEGIN
		EXECUTE spNextKey 'Concept_Group_Version', @NewCGVKey OUTPUT
	
		INSERT INTO Concept_Group_Version (
			Concept_Group_Version_Key, 
			Concept_Group_Key, 
			Version,
			[Sequence], 
			From_Vague_Date_Start,
			From_Vague_Date_End,
			From_Vague_Date_Type,
			To_Vague_Date_Start,
			To_Vague_Date_End,
			To_Vague_Date_Type,	
			Acq_Vague_Date_Start,
			Acq_Vague_Date_End,
			Acq_Vague_Date_Type,	
			URL,
			Entered_Session_ID,
			System_Supplied_Data
		) SELECT @NewCGVKey,
			@NewConceptGroupKey,
			Version,
			[Sequence],
			From_Vague_Date_Start,
			From_Vague_Date_End,
			From_Vague_Date_Type,
			To_Vague_Date_Start,
			To_Vague_Date_End,
			To_Vague_Date_Type,	
			Acq_Vague_Date_Start,
			Acq_Vague_Date_End,
			Acq_Vague_Date_Type,	
			URL,
			Entered_Session_ID,
			System_Supplied_Data
		FROM 	Concept_Group_Version
		WHERE	Concept_Group_Version_Key = @OriginalCGVKey	

		IF @@Error <> 0 GOTO RollbackAndExit	

		FETCH NEXT
		FROM	curCGV
		INTO	@OriginalCGVKey	
	END

	CLOSE curCGV
	DEALLOCATE curCGV

	/*========================================================================================*\
	  We now want to duplicate all of the Concepts from the current to the new Concept Groups.
	  Whilst doing this, we also want to duplicate the Concept_History records and create
	  the Concept Lineage for each Concept.
	\*========================================================================================*/
	DECLARE @OriginalConceptKey char(16),
		@NewConceptKey char(16),
		@OriginalConceptHistoryKey char(16),
		@NewConceptHistoryKey char(16)

	DECLARE curConcept CURSOR LOCAL FAST_FORWARD FOR
		SELECT	Concept_Key
		FROM	Concept
		WHERE	Concept_Group_Key = 'SYSTEM000000000E'

	OPEN curConcept

	FETCH NEXT
	FROM	curConcept
	INTO	@OriginalConceptKey

	WHILE @@Fetch_Status = 0
	BEGIN
		/*---------------------*\
		  Insert the Concept.
		\*---------------------*/
		EXECUTE spNextKey 'Concept', @NewConceptKey OUTPUT
	
		INSERT INTO Concept (
			Concept_Key, 
			Term_Key, 
			Concept_Group_Key, 
			List_Preferred, 
			Is_Current, 
			Preferred, 
			Concept_Rank_Key, 
			Name_Type_Concept_Key, 
			Meaning_Key, 
			Author_Copy, 
			Sort_Code, 
			List_Code, 
			Entered_Session_ID, 
			System_Supplied_Data
		) SELECT
			@NewConceptKey, 
			Term_Key, 
			@NewConceptGroupKey, 
			List_Preferred, 
			Is_Current, 
			Preferred, 
			Concept_Rank_Key, 
			Name_Type_Concept_Key, 
			Meaning_Key, 
			Author_Copy, 
			Sort_Code, 
			List_Code, 
			Entered_Session_ID, 
			System_Supplied_Data
		FROM	Concept
		WHERE	Concept_Key = @OriginalConceptKey
		IF @@Error <> 0 GOTO RollbackAndExit

		/*-------------------------------------------------------------*\
		  Create Concept_Lineage for new Concept
		\*-------------------------------------------------------------*/
		EXECUTE	usp_ConceptLineage_NewConcept	@NewConceptKey
		IF @@ERROR <> 0 GOTO RollbackAndExit

		/*---------------------------------------*\
		  Get the next Concept from the cursor.		
		\*---------------------------------------*/
		FETCH NEXT
		FROM	curConcept
		INTO	@OriginalConceptKey	
	END

	CLOSE curConcept
	DEALLOCATE curConcept

COMMIT TRANSACTION
RETURN

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

