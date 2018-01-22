/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_MeaningRelation_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_MeaningRelation_Update]
GO

/*===========================================================================*\
  Description:	Updates a record in the Meaning_Relation table.

  Parameters:	@Key
		@FromKey
		@ToKey
		@ThesaurusRelationTypeKey
		@Multiplicity
		@Comment
		@SessionID
		@SystemSuppliedData

  Created:	December 2003

  Last revision information:
    $Revision: 4 $
    $Date: 3/02/09 9:41 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_MeaningRelation_Update]
	@Key char(16),
	@FromConceptKey char(16),
	@ToConceptKey char(16),
	@ThesaurusRelationTypeKey char(16),
	@Multiplicity float = NULL,
	@Inherited bit,
	@Comment text = NULL,
	@SessionID char(16),
	@Timestamp timestamp

AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

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

		IF @@Error <> 0 GOTO RollbackAndExit

		/*-------------------------------------------------------------*\
		  Update record in Meaning_Relation.
		\*-------------------------------------------------------------*/
		UPDATE	Meaning_Relation
		SET	From_Meaning_Key = @FromMeaningKey,
			To_Meaning_Key = @ToMeaningKey,
			From_Concept_Key = @FromConceptKey,
			To_Concept_Key = @ToConceptKey,
			Thesaurus_Relation_Type_Key = @ThesaurusRelationTypeKey,
			Multiplicity = @Multiplicity,
			Inherited = @Inherited,
			Comment = @Comment,
			Changed_Session_ID = @SessionID
		WHERE	Meaning_Relation_Key = @Key
		AND		[Timestamp] = @Timestamp
	
		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Meaning_Relation WHERE Meaning_Relation_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_MeaningRelation_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_MeaningRelation_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_MeaningRelation_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_MeaningRelation_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_MeaningRelation_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_MeaningRelation_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_MeaningRelation_Update TO [Dev - JNCC SQL]
END

GO
			