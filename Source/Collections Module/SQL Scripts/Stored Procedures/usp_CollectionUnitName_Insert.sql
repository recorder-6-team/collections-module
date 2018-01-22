/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_CollectionUnitName_Insert]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_CollectionUnitName_Insert]
GO

/*===========================================================================*\
  Description:	Inserts a record into the Collection_Unit_Name table

  Parameters:	@Key
		@CollectionUnitKey
		@Name_Key
		@Relating_Type_Concept_Key
		@Vague_Date_Start
		@Vague_Date_End
		@Vague_Date_Type 
		@Comment
		@SessionID

  Created:	October 2003

  Last revision information:
    $Revision: 3 $
    $Date: 5/12/03 17:16 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_CollectionUnitName_Insert]
	@Key char(16) OUTPUT,
	@CollectionUnitKey char(16),
	@NameKey char(16),
	@RelationTypeConceptKey char(16),
	@VagueDateStart int,
	@VagueDateEnd int,
	@VagueDateType varchar(2),
	@Comment text,
	@SessionID char(16)	
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	EXECUTE spNextKey 'Collection_Unit_Name', @Key OUTPUT

	BEGIN TRANSACTION

		INSERT INTO Collection_Unit_Name (
			Collection_Unit_Name_Key, Collection_Unit_Key, Name_Key,
			Relation_Type_Concept_Key, Vague_Date_Start, Vague_Date_End,
			Vague_Date_Type, Comment, Entered_Session_ID
		) VALUES (
			@Key, @CollectionUnitKey, @NameKey,
			@RelationTypeConceptKey, @VagueDateStart, @VagueDateEnd,
			@VagueDateType, @Comment, @SessionID
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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_CollectionUnitName_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_CollectionUnitName_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitName_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_CollectionUnitName_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_CollectionUnitName_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_CollectionUnitName_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_CollectionUnitName_Insert TO [Dev - JNCC SQL]
END
GO