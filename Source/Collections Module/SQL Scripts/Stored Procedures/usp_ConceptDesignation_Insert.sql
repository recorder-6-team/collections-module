/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptDesignation_Insert]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptDesignation_Insert]
GO

/*===========================================================================*\
  Description:	Inserts a record into the Concept_Designation table

  Parameters:	

  Created:	December 2003

  Last revision information:
    $Revision: 2 $
    $Date: 5/03/04 16:29 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptDesignation_Insert]
	@Key char(16) OUTPUT,
	@ConceptKey char(16),
	@DesignationTypeConceptKey char(16),
	@FromVagueDateStart int,
	@FromVagueDateEnd int,
	@FromVagueDateType varchar(2) = NULL,
	@ToVagueDateStart int,
	@ToVagueDateEnd int,
	@ToVagueDateType varchar(2) = NULL,
	@SessionID char(16),
	@SystemSuppliedData bit = NULL
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	/*-------------------------------------------------------------*\
	  Get a new key.
	\*-------------------------------------------------------------*/
	EXECUTE spNextKey 'Concept_Designation', @Key OUTPUT

	/*-------------------------------------------------------------*\
	  Wrap everything in one transaction.
	\*-------------------------------------------------------------*/
	BEGIN TRANSACTION

		/*-------------------------------------------------------------*\
		  Insert in Concept_Designation.
		\*-------------------------------------------------------------*/
		INSERT INTO Concept_Designation (
			Concept_Designation_Key,
			Concept_Key,
			Designation_Type_Concept_Key,
			From_Vague_Date_Start,
			From_Vague_Date_End,
			From_Vague_Date_Type,
			To_Vague_Date_Start,
			To_Vague_Date_End,
			To_Vague_Date_Type,
			Entered_Session_ID,
			System_Supplied_Data			
		) VALUES (
			@Key,
			@ConceptKey,
			@DesignationTypeConceptKey,
			@FromVagueDateStart,
			@FromVagueDateEnd,
			IsNull(@FromVagueDateType, 'U'),
			@ToVagueDateStart,
			@ToVagueDateEnd,
			@ToVagueDateType,
			@SessionID,
			IsNull(@SystemSuppliedData, 0)
		)
		IF @@Error <> 0 GOTO RollbackAndExit

	/*-------------------------------------------------------------*\
	  All went well, so commit.
	\*-------------------------------------------------------------*/
	COMMIT TRANSACTION

RETURN 0

RollBackAndExit: 
	/*-------------------------------------------------------------*\
	  Cancel any changes, or left-overs might mess up some tables.
	\*-------------------------------------------------------------*/
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptDesignation_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptDesignation_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ConceptDesignation_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptDesignation_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptDesignation_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptDesignation_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptDesignation_Insert TO [Dev - JNCC SQL]
END

GO