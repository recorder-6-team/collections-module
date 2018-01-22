/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptGroup_Insert]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptGroup_Insert]
GO

/*===========================================================================*\
  Description:	Adds a Concept Group by adding records to the Concept Group
		and Concept Group Version tables.

  Parameters:	@Key 
		@URL
		@SessionID
		@ConceptGroupName 
		@Authority 
		@HierarchyRelationTypeKey 
		@LocalDomainKey 
		@FromVagueDateStart 
		@FromVagueDateEnd
		@FromVagueDateType 
		@ToVagueDateStart 
		@ToVagueDateEnd 
		@ToVagueDateType
		@AcqVagueDateStart 
		@AcqVagueDateEnd 
		@AcqVagueDateType
		@Version

  Created:	November 2003

  Last revision information:
    $Revision: 7 $
    $Date: 28/07/11 15:45 $
    $Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptGroup_Insert]
	@Key char(16) OUTPUT,	-- New Concept_Group_Key
	-- For both tables
	@URL varchar(255) = NULL,
	@SessionID char(16),
	
	-- For Concept_Group table
	@ConceptGroupName varchar(100),
	@Authority varchar(100) = NULL,
	@HierarchyRelationTypeKey char(16) = NULL,
	@LocalDomainKey char(16),
	@TermGeneratorKey char(16),
	
	-- For Concept_Group_Version table
	@FromVagueDateStart int = NULL,
	@FromVagueDateEnd int = NULL,
	@FromVagueDateType varchar(2) = NULL,
	@ToVagueDateStart int = NULL,
	@ToVagueDateEnd int = NULL,
	@ToVagueDateType varchar(2) = NULL,	
	@AcqVagueDateStart int = NULL,
	@AcqVagueDateEnd int = NULL,
	@AcqVagueDateType varchar(2) = NULL,
	@Version varchar(100) = NULL
	
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION

		/*-------------------------------------------------------------*\
		  Insert in Concept_Group.
		\*-------------------------------------------------------------*/
		EXECUTE spNextKey 'Concept_Group', @Key OUTPUT		

		INSERT INTO Concept_Group (
			Concept_Group_Key, 
			Local_Domain_Key, 
			Item_Name, 
			Authority,
			URL,
			Hierarchy_Relation_Type_Key, 
			Term_Generator_Key,
			Entered_Session_ID, 
			System_Supplied_Data
		) VALUES (
			@Key, 
			@LocalDomainKey, 
			@ConceptGroupName, 
			@Authority, 
			@URL,
			@HierarchyRelationTypeKey, 
			@TermGeneratorKey,
			@SessionID, 
			0
		)
		IF @@Error <> 0 GOTO RollbackAndExit

		-- Only if @Version contains a string do we want to add a Concept_Group_Version record here.
		-- A Concept_Group record needs to have at least on Concept_Group_Version record. However,
		-- if no value for @Version is supplied, it is likely that the Concept_Group_Version record
		-- is being added elsewhere because the key of the new record is required.
		IF @Version IS NOT NULL
		BEGIN
			/*-------------------------------------------------------------*\
			  Insert in Concept_Group_Version.
			\*-------------------------------------------------------------*/
			DECLARE @ConceptGroupVersionKey char(16) 
			EXECUTE spNextKey 'Concept_Group_Version', @ConceptGroupVersionKey OUTPUT		
			
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
			) VALUES (
				@ConceptGroupVersionKey,
				@Key,
				@Version,
				1,
				@FromVagueDateStart,
				@FromVagueDateEnd,
				IsNull(@FromVagueDateType, 'U'),
				@ToVagueDateStart,
				@ToVagueDateEnd,
				IsNull(@ToVagueDateType, 'U'),	
				@AcqVagueDateStart,
				@AcqVagueDateEnd,
				IsNull(@AcqVagueDateType, 'U'),
				@URL,
				@SessionID,
				0
			)
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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptGroup_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptGroup_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptGroup_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptGroup_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptGroup_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptGroup_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptGroup_Insert TO [Dev - JNCC SQL]
END
GO	