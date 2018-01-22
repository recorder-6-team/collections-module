/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Synonym_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Synonym_Update]
GO

/*===========================================================================*\
  Description:	Proc. for altering the state of synonyms. For instance, a 
		'Potential Synonym' can be made into a 'Known Synonym' or
		a 'List Synonym', and a 'Known Synonym' can be made into 
		a 'List Synonym'.

		@SynonymType input parameter is used to determine what fields
		should be updated:
		
		0) Change Meaning_Key and Concept_Group_Key i.e. 'Potential 
			Synonym' to 'List Synonym'
	 	1) Change Meaning_Key, i.e. 'Potential Synonym' to 'Known Synonym'
	 	2) Change Concept_Group_Key, i.e. 'Known Synonym' to 'List Synonym'

  Parameters:	@Key
		@MeaningKey 
		@ConceptGroupKey
		@RecordsAffected  OUTPUT

  Created:	February 2004

  Last revision information:
    $Revision: 1 $
    $Date: 26/02/04 17:23 $
    $Author: Anthonysimpson $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Synonym_Update]
	@TargetConceptKey char(16),
	@DraggedConceptKey char(16),
	@SynonymType int,
	@RecordsAffected int OUTPUT
	
AS
SET NOCOUNT OFF
	DECLARE @TargetMeaningKey char(16),
		@TargetConceptGroupKey char(16),
		@Error int

	-- Get the Meaning_Key and Concept_Group_Key of the target concept.
	SELECT	@TargetMeaningKey = Meaning_Key,
		@TargetConceptGroupKey = Concept_Group_Key
	FROM	Concept
	WHERE	Concept_Key = @TargetConceptKey
	
	BEGIN TRANSACTION
		IF @SynonymType = 0
			UPDATE	Concept
			SET	Meaning_Key = @TargetMeaningKey,
				Concept_Group_Key = @TargetConceptGroupKey
			WHERE	Concept_Key = @DraggedConceptKey
		ELSE IF @SynonymType = 1
			UPDATE	Concept
			SET	Meaning_Key = @TargetMeaningKey
			WHERE	Concept_Key = @DraggedConceptKey
		ELSE IF @SynonymType = 2
			UPDATE	Concept
			SET	Concept_Group_Key = @TargetConceptGroupKey
			WHERE	Concept_Key = @DraggedConceptKey	

		SELECT	@RecordsAffected = @@RowCount,
			@Error = @@Error
		IF @Error <> 0 GOTO RollbackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit:
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_Synonym_Update failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Synonym_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Synonym_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Synonym_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Synonym_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Synonym_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Synonym_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Synonym_Update TO [Dev - JNCC SQL]
END
GO