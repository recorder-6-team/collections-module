/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Concept_Update_MakeListPreferred]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Concept_Update_MakeListPreferred]
GO

/*===========================================================================*\
  Description:	Ensures that there is only one synonym in the list that is
		list preferred.  Moves the hierarchy relations over to the new 
		list preferred item.

  Parameters:   @RecordsAffected
				@Key

  Created:	January 2004

  Last revision information:
    $Revision: 3 $
    $Date: 18/05/06 10:55 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Concept_Update_MakeListPreferred]
	@RecordsAffected int OUTPUT,
	@Key char(16),
	@HierarchyRelationTypeKey char(16)
AS
	SET NOCOUNT ON

	/*-------------------------------------------------------------*\
	  Wrap everything in one transaction.
	\*-------------------------------------------------------------*/
	BEGIN TRANSACTION
		/*--------------------------------------------------------------------*\
		  Set the desired Concept to be list preferred.
		\*--------------------------------------------------------------------*/
		DECLARE		@error					INT,
					@concept_group_key		CHAR(16),
					@old_list_preferred		BIT

		UPDATE		Concept
		SET			@concept_group_key	=	Concept_Group_Key,
					@old_list_preferred	=	List_Preferred,
					List_Preferred		=	1
		WHERE		Concept_Key			=	@Key

		SELECT		@error				=	@@ERROR,
					@RecordsAffected	=	@@ROWCOUNT

		IF @error <> 0 GOTO RollbackAndExit


		/*--------------------------------------------------------------------*\
		  Move the hierarchy concept relations for the old list preferred item over
		\*--------------------------------------------------------------------*/
		UPDATE CR
		SET From_Concept_Key=@Key
		FROM Concept_Relation CR
		INNER JOIN	Concept AS C1 ON C1.Concept_Key=CR.From_Concept_Key
		INNER JOIN	Concept AS C2 	ON C2.Meaning_Key = C1.Meaning_Key
							AND C2.Concept_Group_Key = C1.Concept_Group_Key
		WHERE C2.Concept_Key=@Key
		AND CR.Thesaurus_Relation_Type_Key = @HierarchyRelationTypeKey

		IF @error <> 0 GOTO RollbackAndExit


		UPDATE CR
		SET To_Concept_Key=@Key
		FROM Concept_Relation CR
		INNER JOIN	Concept AS C1 ON C1.Concept_Key=CR.To_Concept_Key
		INNER JOIN	Concept AS C2 	ON C2.Meaning_Key = C1.Meaning_Key
							AND C2.Concept_Group_Key = C1.Concept_Group_Key
		WHERE C2.Concept_Key=@Key
		AND CR.Thesaurus_Relation_Type_Key = @HierarchyRelationTypeKey

		IF @error <> 0 GOTO RollbackAndExit


		/*--------------------------------------------------------------------*\
		  Make all the other synonyms in the concept group List_Preferred = 0
		\*--------------------------------------------------------------------*/
		UPDATE		CSynonyms
		SET		List_Preferred = 0
		FROM 		Concept AS CSource
		INNER JOIN	Concept AS CSynonyms 	ON CSynonyms.Meaning_Key = CSource.Meaning_Key
							AND CSynonyms.Concept_Group_Key = CSource.Concept_Group_Key
							AND CSynonyms.Concept_Key <> CSource.Concept_Key
		WHERE		CSource.Concept_Key = @Key

		SELECT		@error				=	@@ERROR,
					@RecordsAffected	=	@RecordsAffected + @@ROWCOUNT

		IF @error <> 0 GOTO RollbackAndExit


		/*--------------------------------------------------------------------*\
		  Make corresponding changes in Concept_Lineage.
		\*--------------------------------------------------------------------*/
		IF @old_list_preferred = 0
		BEGIN
			EXECUTE		usp_ConceptLineage_ConceptUpdated	@Key,
															@concept_group_key,
															@old_list_preferred
			IF @@error <> 0 GOTO RollbackAndExit
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
	IF @@TRANCOUNT > 0 ROLLBACK TRANSACTION
	RAISERROR ('usp_Concept_Update_MakeListPreferred failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_Update_MakeListPreferred') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Concept_Update_MakeListPreferred'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Concept_Update_MakeListPreferred TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concept_Update_MakeListPreferred TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concept_Update_MakeListPreferred TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Concept_Update_MakeListPreferred TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Concept_Update_MakeListPreferred TO [Dev - JNCC SQL]
END

GO
			