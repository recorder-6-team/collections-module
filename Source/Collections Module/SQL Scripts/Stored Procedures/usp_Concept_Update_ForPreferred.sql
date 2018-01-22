/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Concept_Update_ForPreferred]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Concept_Update_ForPreferred]
GO

/*===========================================================================*\
  Description:	When the user clicks 'paste as synonym' (from the popup menu 
		over the treeview of the Thesaurus Navigator in the 
		Concept Organiser), the pasted concept’s List_Preferred flag 
		is set to 0. The Preferred flag is also set to 0 
		if the concept selected in the treeview already has 
		a preferred item in its list of synonyms which matches the 
		pasted concept’s Concept_Name_Type_Key and term language.  

  Parameters:	@PastedConceptKey
				@SelectedConceptKey
				@RecordsAffected

  Created:	January 2004

  Last revision information:
    $Revision: 2 $
    $Date: 12/01/04 10:36 $
    $Author: Andrewkemp $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Concept_Update_ForPreferred]
	@PastedConceptKey char(16),
	@SelectedConceptKey char(16),
	@RecordsAffected INT OUTPUT
AS
	SET NOCOUNT ON

	/*-------------------------------------------------------------*\
	  Wrap everything in one transaction.
	\*-------------------------------------------------------------*/
	BEGIN TRANSACTION
	
		DECLARE	@PastedConceptNameTypeKey char(16)
		DECLARE	@PastedLanguage varchar(4)
	
		/*------------------------------------------------------------------*\
		  Get the Concept_Name_Type_Key and Language for the pasted concept.
		\*------------------------------------------------------------------*/		
		SELECT		@PastedConceptNameTypeKey = C.Name_Type_Concept_Key,
				@PastedLanguage = T.Language_Key
		FROM		Concept AS C
		INNER JOIN	Term AS T ON T.Term_Key = C.Term_Key
		WHERE		C.Concept_Key = @PastedConceptKey

		/*------------------------------------------------------------------*\
		  See if the pasted concept should be preferred.
		\*------------------------------------------------------------------*/		
		IF EXISTS	(SELECT 	C2.Concept_Key
				FROM		Concept AS C1
				INNER JOIN	Concept AS C2 	ON C2.Meaning_Key = C1.Meaning_Key 
								AND C2.Name_Type_Concept_Key = @PastedConceptNameTypeKey
								AND C2.Preferred = 1
				INNER JOIN	Term AS T2	ON T2.Term_key = C2.Term_Key AND T2.Language_Key = @PastedLanguage
				WHERE		C1.Concept_Key = @SelectedConceptKey)
			UPDATE		Concept
			SET		Preferred = 0
			WHERE		Concept_Key = @PastedConceptKey
		ELSE
			UPDATE		Concept
			SET		Preferred = 1
			WHERE		Concept_Key = @PastedConceptKey

		IF @@Error <> 0 GOTO RollbackAndExit

		/*------------------------------------------------------------------*\
		  Update the pasted concept's list preferred field.
		\*------------------------------------------------------------------*/
		DECLARE		@error					INT,
					@concept_group_key		CHAR(16),
					@old_list_preferred		BIT
					
		UPDATE		Concept
		SET         @concept_group_key		=	Concept_Group_Key,
					@old_list_preferred		=	List_Preferred,
					List_Preferred			=	0
		WHERE		Concept_Key 			=	@PastedConceptKey

		SELECT		@error					=	@@ERROR,
					@RecordsAffected		=	@@ROWCOUNT

		IF @error <> 0 GOTO RollbackAndExit

		/*--------------------------------------------------------------------*\
		  Make corresponding changes in Concept_Lineage.
		\*--------------------------------------------------------------------*/
		IF @RecordsAffected > 0
		BEGIN
			EXECUTE		usp_ConceptLineage_ConceptUpdated	@PastedConceptKey,
															@concept_group_key,
															@old_list_preferred
			IF @@ERROR <> 0 GOTO RollbackAndExit
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
	RAISERROR ('usp_Concept_Update_ForPreferred failed', 16, 1)
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_Update_ForPreferred') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Concept_Update_ForPreferred'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Concept_Update_ForPreferred TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concept_Update_ForPreferred TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concept_Update_ForPreferred TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Concept_Update_ForPreferred TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Concept_Update_ForPreferred TO [Dev - JNCC SQL]
END

GO
			