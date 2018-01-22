/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_CollectionUnitName_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_CollectionUnitName_Update]
GO

/*===========================================================================*\
  Description:	Updates a record in the Collection_Unit_Name table.

  Parameters:	@Key
		@CollectionUnitKey
		@Name_Key
		@Relating_Type_Concept_Key
		@Vague_Date_Start
		@Vague_Date_End
		@Vague_Date_Type 
		@Comment
		@SessionID
		@Timestamp

  Created:	October 2003

  Last revision information:
    $Revision: 5 $
    $Date: 2/02/09 17:24 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_CollectionUnitName_Update]
	@Key char(16) OUTPUT,
	@CollectionUnitKey char(16),
	@NameKey char(16),
	@RelationTypeConceptKey char(16),
	@VagueDateStart int,
	@VagueDateEnd int,
	@VagueDateType varchar(2),
	@Comment text,
	@SessionID char(16),
	@Timestamp timestamp	
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION

		UPDATE 	Collection_Unit_Name
		SET	Collection_Unit_Key = @CollectionUnitKey,
			Name_Key = @NameKey,
			Relation_Type_Concept_Key = @RelationTypeConceptKey,
			Vague_Date_Start = @VagueDateStart,
			Vague_Date_End = @VagueDateEnd,
			Vague_Date_Type = @VagueDateType,
			Comment = @Comment,
			Changed_Session_ID = @SessionID
		WHERE	Collection_Unit_Name_Key = @Key
		AND		[Timestamp] = @Timestamp

		DECLARE @Error int
		DECLARE @RecordsAffected int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Collection_Unit_Name WHERE Collection_Unit_Name_Key = @Key)
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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_CollectionUnitName_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_CollectionUnitName_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitName_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_CollectionUnitName_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_CollectionUnitName_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_CollectionUnitName_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitName_Update TO [Dev - JNCC SQL]
END
GO