/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_CollectionUnitRelation_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_CollectionUnitRelation_Update]
GO

/*===========================================================================*\
  Description:	Updates a record in the Collection Unit Relation table.

  Parameters:	@Key
		@FromCollectionUnitKey
		@ToCollectionUnitKey
		@ThesaurusRelationTypeKey
		@InferredType
		@VagueDateStart
		@VagueDateEnd
		@VagueDateType
		@AuthorNameKey 
		@Comment 
		@SessionID
		@Timestamp

  Created:	October 2003

  Last revision information:
    $Revision: 5 $
    $Date: 2/02/09 17:33 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_CollectionUnitRelation_Update]
	@Key char(16),
	@FromCollectionUnitKey char(16),
	@ToCollectionUnitKey char(16),
	@ThesaurusRelationTypeKey char(16),
	@InferredType tinyint,
	@VagueDateStart int,
	@VagueDateEnd int,
	@VagueDateType varchar(2),
	@AuthorNameKey char(16),
	@Comment text,
	@SessionID char(16),
	@Timestamp timestamp
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION

		UPDATE 	Collection_Unit_Relation
		SET	From_Collection_Unit_Key = @FromCollectionUnitKey,
			To_Collection_Unit_Key = @ToCollectionUnitKey,
			Thesaurus_Relation_Type_Key = @ThesaurusRelationTypeKey,
			Inferred_Type = @InferredType,
			Vague_Date_Start = @VagueDateStart,
			Vague_Date_End = @VagueDateEnd,
			Vague_Date_Type = @VagueDateType,
			Author_Name_Key = @AuthorNameKey,
			Comment = @Comment,
			Changed_Session_ID = @SessionID
		WHERE 	Collection_Unit_Relation_Key = @Key
		AND		[Timestamp] = @Timestamp

		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Collection_Unit_Relation WHERE Collection_Unit_Relation_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_CollectionUnitRelation_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_CollectionUnitRelation_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitRelation_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_CollectionUnitRelation_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_CollectionUnitRelation_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_CollectionUnitRelation_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitRelation_Update TO [Dev - JNCC SQL]
END
GO