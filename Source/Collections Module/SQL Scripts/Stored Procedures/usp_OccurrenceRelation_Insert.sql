/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_OccurrenceRelation_Insert]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_OccurrenceRelation_Insert]
GO

/*===========================================================================*\
  Description:	Insert a record in the Occurrence_Relation table.

  Parameters:	@Key			OUTPUT Occurrence Relation key
		@FromOccurrenceKey
		@ToOccurrenceKey
		@RelationTypeKey
		@Comment
		@SessionID

  Created:	November 2003

  Last revision information:
    $Revision: 2 $
    $Date: 5/12/03 17:40 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_OccurrenceRelation_Insert]
	@Key char(16) OUTPUT,
	@FromOccurrenceKey char(16),
	@ToOccurrenceKey char(16),
	@RelationTypeKey char(16),
	@Comment text,
	@SessionID char(16)
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	EXECUTE spNextKey 'Occurrence_Relation', @Key OUTPUT

	BEGIN TRANSACTION

		INSERT INTO Occurrence_Relation (
			Occurrence_Relation_Key, From_Occurrence_Key, To_Occurrence_Key,
			Thesaurus_Relation_Type_Key, Comment, Entered_Session_ID
		) VALUES (
			@Key, @FromOccurrenceKey, @ToOccurrenceKey, @RelationTypeKey, @Comment, @SessionID
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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_OccurrenceRelation_Insert') AND SysStat & 0xf = 4)
BEGIN
	PRINT 'Setting up security on procedure usp_OccurrenceRelation_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
		GRANT EXECUTE ON dbo.usp_OccurrenceRelation_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_OccurrenceRelation_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_OccurrenceRelation_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_OccurrenceRelation_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
		GRANT EXECUTE ON dbo.usp_OccurrenceRelation_Insert TO [Dev - JNCC SQL]
END
GO
