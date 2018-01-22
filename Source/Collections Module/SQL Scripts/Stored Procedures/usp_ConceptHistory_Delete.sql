/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_ConceptHistory_Delete]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_ConceptHistory_Delete]
GO

/*===========================================================================*\
  Description:	Delete a record from the Concept_History table.

  Parameters:	@Key
		@Timestamp,
		@RecordsAffected

  Created:	December 2003

  Last revision information:
    $Revision: 4 $
    $Date: 2/02/09 17:53 $
    $Author: Pauldavies $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_ConceptHistory_Delete]
	@Key char(16),
	@Timestamp timestamp,
	@RecordsAffected INT OUTPUT
AS
	SET NOCOUNT OFF

	BEGIN TRANSACTION

		DECLARE @ConceptGroupKey CHAR(16)
		DECLARE @ConceptKey CHAR(16)

		-- Find the concept group key, because we need this to check if the concept 
		-- remains current
		SELECT @ConceptGroupKey=CGV.Concept_Group_Key, @ConceptKey=CH.Concept_Key
		FROM Concept_History CH
		INNER JOIN Concept_Group_Version CGV ON CGV.Concept_Group_Version_Key=CH.Concept_Group_Version_To
		WHERE CH.Concept_History_Key=@Key

		-- Delete record from Concept_History table.
		DELETE	Concept_History
		WHERE	Concept_History_Key = @Key
		AND		[Timestamp] = @Timestamp

		DECLARE @Error int
		
		SELECT @Error = @@Error, @RecordsAffected = @@Rowcount

		IF @Error <> 0 GOTO RollbackAndExit 
	
		IF @RecordsAffected = 0 AND EXISTS(SELECT 1 FROM Concept_History WHERE Concept_History_Key = @Key)
		BEGIN
			RAISERROR('Record updated by another user', 16, 1)
			GOTO RollbackAndExit
		END
		
		IF @RecordsAffected>0
		BEGIN
			--Ensure Concept's current status is set correctly.
			EXEC usp_Concept_UpdateIsCurrent @ConceptKey, @ConceptGroupKey
			IF @@Error <> 0 GOTO RollbackAndExit
		END

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_ConceptHistory_Delete') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_ConceptHistory_Delete'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_ConceptHistory_Delete TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_ConceptHistory_Delete TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_ConceptHistory_Delete TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_ConceptHistory_Delete TO [Dev - JNCC SQL]
END
GO