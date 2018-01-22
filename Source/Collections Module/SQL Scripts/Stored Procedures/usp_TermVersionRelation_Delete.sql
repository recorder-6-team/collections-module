/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_TermVersionRelation_Delete]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_TermVersionRelation_Delete]
GO

/*===========================================================================*\
  Description:	Deletes a record from the TermVersionRelation table.

  Parameters:	@Key		Term_Version_Relation_Key
		@Timestamp

  Created:	December 2003

  Last revision information:
    $Revision: 4 $
    $Date: 3/02/09 10:50 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_TermVersionRelation_Delete]
	@Key char(16),
	@Timestamp timestamp
AS

SET NOCOUNT ON

	/*-------------------------------------------------------------*\
	  Wrap everything in one transaction.
	\*-------------------------------------------------------------*/
	BEGIN TRANSACTION

		/*-------------------------------------------------------------*\
		  Update in Meaning_Relation.
		\*-------------------------------------------------------------*/
		DELETE 	Term_Version_Relation
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

SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_TermVersionRelation_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_TermVersionRelation_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_TermVersionRelation_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_TermVersionRelation_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_TermVersionRelation_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_TermVersionRelation_Delete TO [Dev - JNCC SQL]
END

GO