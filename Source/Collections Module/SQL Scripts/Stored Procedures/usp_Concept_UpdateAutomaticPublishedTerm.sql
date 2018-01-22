/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].usp_Concept_UpdateAutomaticPublishedTerm')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].usp_Concept_UpdateAutomaticPublishedTerm
GO

/*===========================================================================*\
  Description:	Returns all concepts which belong to the domain/local domain/
				concept group specified, or descend from the concept specified

  Parameters:	

  Created:	August 2011

  Last revision information:
    $Revision: 2 $
    $Date: 26/08/11 14:51 $
    $Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].usp_Concept_UpdateAutomaticPublishedTerm
	@ConceptKey char(16),
	@AutomaticPublishedTerm bit,
	@RecordsAffected int = 1 OUTPUT
AS
	SET NOCOUNT OFF
	
	BEGIN TRANSACTION
		DECLARE @Error INT

		UPDATE Concept
		SET Automatic_Published_Term = @AutomaticPublishedTerm
		WHERE Concept_Key = @ConceptKey

		SELECT @RecordsAffected = @@ROWCOUNT, @Error = @@Error

		IF @Error <> 0 GOTO RollbackAndExit

	COMMIT TRANSACTION
	RETURN 0

RollbackAndExit:
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_UpdateAutomaticPublishedTerm') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Concept_UpdateAutomaticPublishedTerm'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Concept_UpdateAutomaticPublishedTerm TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concept_UpdateAutomaticPublishedTerm TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concept_UpdateAutomaticPublishedTerm TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Concept_UpdateAutomaticPublishedTerm TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Concept_UpdateAutomaticPublishedTerm TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Concept_UpdateAutomaticPublishedTerm TO [Dev - JNCC SQL]
END

GO
