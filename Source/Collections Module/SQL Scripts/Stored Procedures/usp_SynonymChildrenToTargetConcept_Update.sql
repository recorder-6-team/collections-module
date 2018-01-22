/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_SynonymChildrenToTargetConcept_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_SynonymChildrenToTargetConcept_Update]
GO

/*===========================================================================*\
  Description:	Make the children of the node on the clipboard become 
		children of the target concept.

  Parameters:	@TargetConceptKey
		@SynonymConceptKey 
		@RecordsAffected (output)

  Created:	February 2004

  Last revision information:
    $Revision: 2 $
    $Date: 23/02/04 10:57 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_SynonymChildrenToTargetConcept_Update]
	@TargetConceptKey char(16),
	@SynonymConceptKey char(16),
	@RecordsAffected int output
AS
	-- Required to be able to get number of changed records.
	SET NOCOUNT OFF

	BEGIN TRANSACTION

		DECLARE	@Error int
		
		UPDATE	Concept_Relation
		SET	From_Concept_Key = @TargetConceptKey
		WHERE 	From_Concept_Key = @SynonymConceptKey

		SELECT	@RecordsAffected = @@RowCount,
			@Error = @@Error

		IF @Error <> 0 GOTO RollbackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_SynonymChildrenToTargetConcept_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_SynonymChildrenToTargetConcept_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_SynonymChildrenToTargetConcept_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_SynonymChildrenToTargetConcept_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_SynonymChildrenToTargetConcept_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_SynonymChildrenToTargetConcept_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_SynonymChildrenToTargetConcept_Update TO [Dev - JNCC SQL]
END
GO