/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ThesaurusRelationTypeUsage_Insert]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ThesaurusRelationTypeUsage_Insert]
GO

/*===========================================================================*\
  Description:	Inserts a record into the Thesaurus_Relation_Type_Usage table.

  Parameters:	@Key		Thesaurus Relation Type Key
		@RelationUsage
		@SessionID
		@SystemSuppliedData (optional)

  Created:	December 2003

  Last revision information:
    $Revision: 1 $
    $Date: 8/12/03 12:04 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ThesaurusRelationTypeUsage_Insert]
	@ThesaurusRelationTypeKey char(16),
	@RelationUsage tinyint,
	@SessionID char(16),
	@SystemSuppliedData bit = NULL

AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	DECLARE @LocalTranCount int

	-- This stored proc is called from another stored proc. Hence, the trancount is
	-- not going to be 0 when it gets here. Therefore, it should be stored locally at 
	-- the beginning and checked against @@TranCount when leaving the proc.
	SET @LocalTranCount = @@TranCount


	IF NOT EXISTS(	SELECT  Thesaurus_Relation_Type_Usage_Key
			FROM 	Thesaurus_Relation_Type_Usage
			WHERE 	Thesaurus_Relation_Type_Key = @ThesaurusRelationTypeKey
			AND 	Relation_Usage = @RelationUsage) 
	BEGIN
		DECLARE @Key char(16)
	
		/*-------------------------------------------------------------*\
		  Get a new key.
		\*-------------------------------------------------------------*/
		EXECUTE spNextKey 'Thesaurus_Relation_Type_Usage', @Key OUTPUT
	
		/*-------------------------------------------------------------*\
		  Wrap everything in one transaction.
		\*-------------------------------------------------------------*/
		BEGIN TRANSACTION
	
			/*-------------------------------------------------------------*\
			  Inserts a record into the Thesaurus_Relation_Type table.
			\*-------------------------------------------------------------*/
			INSERT INTO Thesaurus_Relation_Type_Usage (
				Thesaurus_Relation_Type_Usage_Key,
				Thesaurus_Relation_Type_Key,
				Relation_Usage,
				Entered_Session_ID,
				System_Supplied_Data
			) VALUES (
				@Key,
				@ThesaurusRelationTypeKey,
				@RelationUsage,
				@SessionID,
				ISNULL(@SystemSuppliedData, 0)
			)
			IF @@Error <> 0 GOTO RollbackAndExit
	
		/*-------------------------------------------------------------*\
		  All went well, so commit.
		\*-------------------------------------------------------------*/
		COMMIT TRANSACTION

	END


RollBackAndExit: 
	/*-------------------------------------------------------------*\
	  Cancel any changes, or left-overs might mess up some tables.
	\*-------------------------------------------------------------*/
	IF @LocalTranCount <> @@TranCount ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ThesaurusRelationTypeUsage_Insert') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ThesaurusRelationTypeUsage_Insert'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ThesaurusRelationTypeUsage_Insert TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ThesaurusRelationTypeUsage_Insert TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ThesaurusRelationTypeUsage_Insert TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ThesaurusRelationTypeUsage_Insert TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ThesaurusRelationTypeUsage_Insert TO [Dev - JNCC SQL]
END

GO