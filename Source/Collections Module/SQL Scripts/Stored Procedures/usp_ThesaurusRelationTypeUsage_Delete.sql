/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ThesaurusRelationTypeUsage_Delete]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ThesaurusRelationTypeUsage_Delete]
GO

/*===========================================================================*\
  Description:	Deletes a record from the Thesaurus_Relation_Type_Usage table.

  Parameters:	@Key		Thesaurus Relation Type Key
		@RelationUsage

  Created:	December 2003

  Last revision information:
    $Revision: 1 $
    $Date: 8/12/03 12:04 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ThesaurusRelationTypeUsage_Delete]
	@Key char(16),
	@RelationUsage tinyint
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	IF EXISTS(	SELECT  Thesaurus_Relation_Type_Usage_Key
			FROM 	Thesaurus_Relation_Type_Usage
			WHERE 	Thesaurus_Relation_Type_Key = @Key
			AND 	Relation_Usage = @RelationUsage) 
	BEGIN

		DECLARE @LocalTranCount int
	
		-- This stored proc is called from another stored proc. Hence, the trancount is
		-- not going to be 0 when it gets here. Therefore, it should be stored locally at 
		-- the beginning and checked against @@TranCount when leaving the proc.
		SET @LocalTranCount = @@TranCount

		BEGIN TRANSACTION
	
			/*-------------------------------------------------------------*\
			  Deletes a record from the Thesaurus_Relation_Type_Usage table.
			\*-------------------------------------------------------------*/
	
			DELETE	Thesaurus_Relation_Type_Usage
			WHERE	Thesaurus_Relation_Type_Key = @Key
			AND	Relation_Usage = @RelationUsage
	
			IF @@Error <> 0 GOTO RollbackAndExit
	
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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ThesaurusRelationTypeUsage_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ThesaurusRelationTypeUsage_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ThesaurusRelationTypeUsage_Delete TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ThesaurusRelationTypeUsage_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ThesaurusRelationTypeUsage_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ThesaurusRelationTypeUsage_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ThesaurusRelationTypeUsage_Delete TO [Dev - JNCC SQL]
END

GO