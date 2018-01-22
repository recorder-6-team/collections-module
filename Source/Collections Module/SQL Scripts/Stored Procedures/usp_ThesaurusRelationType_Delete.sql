/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ThesaurusRelationType_Delete]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ThesaurusRelationType_Delete]
GO

/*===========================================================================*\
  Description:	Deletes a record from the Thesaurus_Relation_Type table and 
		the associated records in the Thesaurus_Relation_Type_Usage table.

  Parameters:	@Key		Thesaurus Relation Type Key

  Created:	December 2003

  Last revision information:
    $Revision: 1 $
    $Date: 8/12/03 12:04 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ThesaurusRelationType_Delete]
	@Key char(16)
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION

		/*-------------------------------------------------------------*\
		  Deletes a record from the Thesaurus_Relation_Type table.
		\*-------------------------------------------------------------*/

		DELETE 	Thesaurus_Relation_Type_Usage
		WHERE	Thesaurus_Relation_Type_Key = @Key	
		
		IF @@Error <> 0 GOTO RollbackAndExit

		DELETE	Thesaurus_Relation_Type
		WHERE	Thesaurus_Relation_Type_Key = @Key

		IF @@Error <> 0 GOTO RollbackAndExit

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
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ThesaurusRelationType_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ThesaurusRelationType_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_ThesaurusRelationType_Delete TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ThesaurusRelationType_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ThesaurusRelationType_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ThesaurusRelationType_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ThesaurusRelationType_Delete TO [Dev - JNCC SQL]
END

GO