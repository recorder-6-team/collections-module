/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_CollectionUnitRelation_Insert]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_CollectionUnitRelation_Insert]
GO

/*===========================================================================*\
  Description:	Inserts a record into the Collection Unit Relation table.

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

  Created:	October 2003

  Last revision information:
    $Revision: 3 $
    $Date: 5/12/03 17:25 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_CollectionUnitRelation_Insert]
	@Key char(16) OUTPUT,
	@FromCollectionUnitKey char(16),
	@ToCollectionUnitKey char(16),
	@ThesaurusRelationTypeKey char(16),
	@InferredType tinyint,
	@VagueDateStart int,
	@VagueDateEnd int,
	@VagueDateType varchar(2),
	@AuthorNameKey char(16),
	@Comment text,
	@SessionID char(16)
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	EXECUTE spNextKey 'Collection_Unit_Relation', @Key OUTPUT

	BEGIN TRANSACTION
		INSERT INTO Collection_Unit_Relation (
			Collection_Unit_Relation_Key, From_Collection_Unit_Key,
			To_Collection_Unit_Key, Thesaurus_Relation_Type_Key, Inferred_Type,
			Vague_Date_Start, Vague_Date_End, Vague_Date_Type, Author_Name_Key, 
			Comment, Entered_Session_ID
		) VALUES (
			@Key, @FromCollectionUnitKey,
			@ToCollectionUnitKey, @ThesaurusRelationTypeKey, @InferredType,
			@VagueDateStart, @VagueDateEnd, @VagueDateType, @AuthorNameKey, 
			@Comment, @SessionID
		)
		IF @@Error <> 0 GOTO RollbackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_CollectionUnitRelation_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_CollectionUnitRelation_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitRelation_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_CollectionUnitRelation_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_CollectionUnitRelation_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_CollectionUnitRelation_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitRelation_Insert TO [Dev - JNCC SQL]
END
GO	