/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_OccurrenceRelation_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_OccurrenceRelation_Update]
GO

/*===========================================================================*\
  Description:	Update a record in the Occurrence_Relation table.

  Parameters:	@Key			Occurrence Relation key.
		@ToOccurrenceKey 
		@RelationTypeKey 
		@Comment
		@SessionID 
		@Timestamp 

  Created:	September 2003

  Last revision information:
    $Revision: 4 $
    $Date: 3/02/09 9:59 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_OccurrenceRelation_Update]
	@Key char(16),
	@ToOccurrenceKey char(16),
	@RelationTypeKey char(16),
	@Comment text,
	@SessionID char(16),
	@Timestamp timestamp
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION

		UPDATE	Occurrence_Relation
		SET	To_Occurrence_Key = @ToOccurrenceKey,
			Thesaurus_Relation_Type_Key = @RelationTypeKey,
			Comment = @Comment,
			Changed_Session_ID = @SessionID
		WHERE	Occurrence_Relation_Key = @Key
		AND		[Timestamp] = @Timestamp

		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Occurrence_Relation WHERE Occurrence_Relation_Key = @Key)
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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_OccurrenceRelation_Update') AND SysStat & 0xf = 4)
BEGIN
	PRINT 'Setting up security on procedure usp_OccurrenceRelation_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_OccurrenceRelation_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_OccurrenceRelation_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_OccurrenceRelation_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_OccurrenceRelation_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
		GRANT EXECUTE ON dbo.usp_OccurrenceRelation_Update TO [Dev - JNCC SQL]
END
GO