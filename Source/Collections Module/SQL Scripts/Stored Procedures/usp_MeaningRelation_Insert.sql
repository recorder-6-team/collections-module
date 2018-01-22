/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_MeaningRelation_Insert]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_MeaningRelation_Insert]
GO

/*===========================================================================*\
  Description:	Inserts a record into the Meaning_Relation table.

  Parameters:	@Key 	OUTPUT
		@FromConceptKey
		@ToConceptKey
		@FromMeaningKey
		@ToMeaningKey
		@ThesaurusRelationTypeKey
		@Multiplicity
		@Inherited
		@Comment
		@SessionID
		@SystemSuppliedData

  Created:	December 2003

  Last revision information:
    $Revision: 3 $
    $Date: 19/12/03 17:25 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_MeaningRelation_Insert]
	@Key char(16) OUTPUT,
	@FromConceptKey char(16),
	@ToConceptKey char(16),
	@ThesaurusRelationTypeKey char(16),
	@Multiplicity float = NULL,
	@Inherited bit,
	@Comment text = NULL,
	@SessionID char(16),
	@SystemSuppliedData bit = NULL

AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	/*-------------------------------------------------------------*\
	  Get a new key.
	\*-------------------------------------------------------------*/
	EXECUTE spNextKey 'Meaning_Relation', @Key OUTPUT

	/*-------------------------------------------------------------*\
	  Wrap everything in one transaction.
	\*-------------------------------------------------------------*/
	BEGIN TRANSACTION

		DECLARE @FromMeaningKey char(16)
		DECLARE @ToMeaningKey char(16)

		SELECT 	@FromMeaningKey = Meaning_Key
		FROM	Concept
		WHERE	Concept_Key = @FromConceptKey 

		SELECT 	@ToMeaningKey = Meaning_Key
		FROM	Concept
		WHERE	Concept_Key = @ToConceptKey 

		/*-------------------------------------------------------------*\
		  Insert in Meaning_Relation.
		\*-------------------------------------------------------------*/
		INSERT INTO Meaning_Relation (
			Meaning_Relation_Key,
			From_Meaning_Key,
			To_Meaning_Key,
			From_Concept_Key,
			To_Concept_Key,
			Thesaurus_Relation_Type_Key,
			Inherited,
			Multiplicity,
			Comment,
			Entered_Session_ID,
			System_Supplied_Data
		) VALUES (
			@Key,
			@FromMeaningKey,
			@ToMeaningKey,
			@FromConceptKey,
			@ToConceptKey,
			@ThesaurusRelationTypeKey,
			@Inherited,
			@Multiplicity,
			@Comment,
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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_MeaningRelation_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_MeaningRelation_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_MeaningRelation_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_MeaningRelation_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_MeaningRelation_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_MeaningRelation_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_MeaningRelation_Insert TO [Dev - JNCC SQL]
END

GO
			