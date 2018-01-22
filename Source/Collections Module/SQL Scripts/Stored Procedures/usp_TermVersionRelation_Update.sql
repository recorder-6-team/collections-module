/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_TermVersionRelation_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_TermVersionRelation_Update]
GO

/*===========================================================================*\
  Description:	Updates a record in the Term_Version_Relation table.

  Parameters:	@Key
		@FromConceptKey
		@ToConceptKey
		@ThesaurusRelationTypeKey
		@Multiplicity
		@Comment
		@SessionID
		@SystemSuppliedData

  Created:	December 2003

  Last revision information:
    $Revision: 4 $
    $Date: 3/02/09 10:51 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_TermVersionRelation_Update]
	@Key char(16),
	@FromConceptKey char(16),
	@ToConceptKey char(16),
	@ThesaurusRelationTypeKey char(16),
	@Multiplicity float = NULL,
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

		DECLARE @FromTermVersionKey char(16)
		DECLARE @ToTermVersionKey char(16)

		SELECT 	@FromTermVersionKey = Term_Version_Key
		FROM	Concept
		WHERE	Concept_Key = @FromConceptKey 

		SELECT 	@ToTermVersionKey = Term_Version_Key
		FROM	Concept
		WHERE	Concept_Key = @ToConceptKey 

		/*-------------------------------------------------------------*\
		  Update record in Term_Version_Relation.
		\*-------------------------------------------------------------*/
		UPDATE	Term_Version_Relation
		SET	From_Concept_Key = @FromConceptKey,
			To_Concept_Key = @ToConceptKey,
			From_Term_Version_Key = @FromTermVersionKey,
			To_Term_Version_Key = @ToTermVersionKey,
			Thesaurus_Relation_Type_Key = @ThesaurusRelationTypeKey,
			Multiplicity = @Multiplicity,
			Comment = @Comment,
			Changed_Session_ID = @SessionID
		WHERE	Term_Version_Relation_Key = @Key
		AND		[Timestamp] = @Timestamp	
	
		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Term_Version_Relation WHERE Term_Version_Relation_Key = @Key)
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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TermVersionRelation_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_TermVersionRelation_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_TermVersionRelation_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_TermVersionRelation_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_TermVersionRelation_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_TermVersionRelation_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_TermVersionRelation_Update TO [Dev - JNCC SQL]
END

GO
			