/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_MakeConceptsSynonyms_Update]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_MakeConceptsSynonyms_Update]
GO

/*===========================================================================*\
  Description:	Take two Concept keys and make them synonyms.

  Parameters:	@ConceptKeySelected
		@ConceptKeyPasted

  Created:	December 2003

  Last revision information:
    $Revision: 7 $
    $Date: 15/06/11 9:53 $
    $Author: Jamesbichard $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_MakeConceptsSynonyms_Update]
	@ConceptKeySelected char(16),
	@ConceptKeyPasted char(16)
AS

SET NOCOUNT ON

	DECLARE @MeaningKeySelected char(16),
		@MeaningKeyPasted char(16),	
		@MeaningKeyChosen char(16),
		@MeaningKeyDiscarded char(16),
		@ConceptGroupKeySource char(16),
		@ConceptGroupKeyDest char(16),
		@HomonymKey char(16),
		@SiteID char(8)

	BEGIN TRANSACTION

		/*=====================================================================*\
		  Get the meaning keys, and the Concept_Rank_Key of the target Concept.
		\*=====================================================================*/
		SELECT	@MeaningKeySelected = Meaning_Key
		FROM	Concept
		WHERE	Concept_Key = @ConceptKeySelected
	
		SELECT	@MeaningKeyPasted = Meaning_Key
		FROM	Concept
		WHERE	Concept_Key = @ConceptKeyPasted
	
		/*================================================================================*\
		  2 concepts that are in the custody of SYSTEM00 cannot be made synonyms of each 
		  other as the software expects both meaning keys to be present. 
		\*================================================================================*/
		IF NOT ((@ConceptKeySelected LIKE 'SYSTEM00%') AND (@ConceptKeyPasted LIKE 'SYSTEM00%'))
		BEGIN
			SELECT	@SiteID = Data
			FROM	Setting
			WHERE	[Name] = 'SiteID'

			/*==================================*\
			  Choose which meaning key to keep. 
			\*==================================*/	

			/*================================================================================*\
			  If 1 concept has a meaning key starting SYSTEM00, then both concepts are 
			  assigned to this key.
			\*================================================================================*/
			IF @MeaningKeySelected LIKE 'SYSTEM00%'
			BEGIN			
				SET @MeaningKeyChosen = @MeaningKeySelected
				SET @MeaningKeyDiscarded = @MeaningKeyPasted
			END
			ELSE IF @MeaningKeyPasted LIKE 'SYSTEM00%'
			BEGIN
				SET @MeaningKeyChosen = @MeaningKeyPasted
				SET @MeaningKeyDiscarded = @MeaningKeySelected
			END
	
			/*================================================================================*\
			  If neither concept has a meaning key starting SYSTEM00, but one of the meaning 
			  keys starts with the Site ID of the current site, then this is the meaning 
			  key adopted.
			\*================================================================================*/
			ELSE IF @MeaningKeySelected LIKE @SiteID + '%'
			BEGIN
				SET @MeaningKeyChosen = @MeaningKeySelected
				SET @MeaningKeyDiscarded = @MeaningKeyPasted
			END
			ELSE IF @MeaningKeyPasted LIKE @SiteID + '%'
			BEGIN
				SET @MeaningKeyChosen = @MeaningKeyPasted
				SET @MeaningKeyDiscarded = @MeaningKeySelected
			END
	
			/*================================================================================*\
			  If a single meaning key is still not selected by these rules, then the meaning 
			  key that belongs to the concept that will be list preferred after the synonymy 
			  is applied is selected.
			\*================================================================================*/
			
			-- Make sure usp_Concept_Update_ForPreferred is run before this stored proc.
			-- That way the preferred and list_preferred fields for the pasted concept
			-- will already have been updated.

			ELSE IF EXISTS (SELECT 	* 	
					FROM 	Concept
					WHERE 	Concept_Key = @ConceptKeySelected
					AND 	List_Preferred = 1)
	 		BEGIN
				SET @MeaningKeyChosen = @MeaningKeySelected
				SET @MeaningKeyDiscarded = @MeaningKeyPasted
			END
	
			ELSE IF EXISTS (SELECT 	* 	
					FROM 	Concept
					WHERE 	Concept_Key = @ConceptKeyPasted
					AND 	List_Preferred = 1)
	 		BEGIN
				SET @MeaningKeyChosen = @MeaningKeyPasted
				SET @MeaningKeyDiscarded = @MeaningKeySelected
			END
	
			/*================================================================================*\
			  If this still does not resolve a single meaning key, then the meaning key 
			  adopted is the one associated with the concept selected at the time of 
			  application of synonymy.
			\*================================================================================*/
			ELSE
			BEGIN
				SET @MeaningKeyChosen = @MeaningKeySelected
				SET @MeaningKeyDiscarded = @MeaningKeyPasted
			END
			
			/*===========================*\
			  Actually do the update.
			\*===========================*/		
			UPDATE	Concept
			SET	Meaning_Key = @MeaningKeyChosen
			WHERE	Meaning_Key = @MeaningKeyDiscarded

			/*=======================================================================*\
			  References to @MeaningKeyDiscarded need to be replaced with references
			  to @MeaningKeyChosen before the meaning record can be deleted.
			\*=======================================================================*/	
			UPDATE	Thesaurus_Fact
			SET	Meaning_Key = @MeaningKeyChosen
			WHERE	Meaning_Key = @MeaningKeyDiscarded

			UPDATE	Meaning_Relation
			SET	From_Meaning_Key = @MeaningKeyChosen
			WHERE	From_Meaning_Key = @MeaningKeyDiscarded

			UPDATE	Meaning_Relation
			SET	To_Meaning_Key = @MeaningKeyChosen
			WHERE	To_Meaning_Key = @MeaningKeyDiscarded

			UPDATE	Taxon_Dictionary_Meaning_Mapping
			SET	Meaning_Key = @MeaningKeyChosen
			WHERE	Meaning_Key = @MeaningKeyDiscarded

			

 
			/*========================================================================*\
			  Now all references to the old meaning key have gone, we can delete it 
			  (providing the two concepts aren't already synonyms. If that is the case, 
			  don't delete the meaing key.
			\*========================================================================*/
			IF @MeaningKeySelected <> @MeaningKeyPasted
			BEGIN
				--If @MeaningKeyChosen and @MeaningKeyDiscarded are homonyms, the homonym
				--record must be removed.
				IF EXISTS (SELECT * FROM Homonym_Pair
							WHERE (Meaning_Key_1 = @MeaningKeyChosen 
								AND Meaning_Key_2 = @MeaningKeyDiscarded)
							OR (Meaning_Key_2 = @MeaningKeyChosen 
								AND Meaning_Key_1 = @MeaningKeyDiscarded))
				BEGIN
					DELETE FROM Homonym_Pair
					WHERE (Meaning_Key_1 = @MeaningKeyChosen 
						AND Meaning_Key_2 = @MeaningKeyDiscarded)
					OR (Meaning_Key_2 = @MeaningKeyChosen 
						AND Meaning_Key_1 = @MeaningKeyDiscarded)
				END

				--Store all homonym meaning keys of the discarded meaning key in a cursor
				DECLARE partnerKey CURSOR LOCAL FAST_FORWARD FOR 
				SELECT
					CASE
						WHEN Meaning_Key_1 = @MeaningKeyDiscarded THEN Meaning_Key_2
						ELSE Meaning_Key_1
					END as Homonym_Meaning_Key
				FROM Homonym_Pair
				WHERE Meaning_Key_1 = @MeaningKeyDiscarded OR Meaning_Key_2 = @MeaningKeyDiscarded
				

				OPEN partnerKey

				WHILE 1 = 1
				BEGIN
					FETCH		partnerKey
					INTO		@HomonymKey

					IF @@FETCH_STATUS <> 0 BREAK

					DELETE FROM Homonym_Pair
						WHERE (Meaning_Key_1 = @MeaningKeyDiscarded AND Meaning_Key_2 = @HomonymKey)
						OR (Meaning_Key_2 = @MeaningKeyDiscarded AND Meaning_Key_1 = @HomonymKey)

					--Have to check if @HomonymPair is already a homonym of 
					--@MeaningKeyChosen before inserting homonym pair.
					IF NOT EXISTS (
						SELECT * FROM Homonym_Pair
						WHERE (Meaning_Key_1 = @MeaningKeyChosen AND Meaning_Key_2 = @HomonymKey)
						OR (Meaning_Key_2 = @MeaningKeyChosen AND Meaning_Key_1 = @HomonymKey)
					)
					BEGIN
						EXEC dbo.usp_HomonymPair_Insert @MeaningKeyChosen, @HomonymKey
					END
				END

				CLOSE partnerKey
				DEALLOCATE partnerKey
		
				DELETE	Meaning
				WHERE	Meaning_Key = @MeaningKeyDiscarded
			END
		
			/*===========================*\
			  Get the concept group keys.
			\*===========================*/
			SELECT	@ConceptGroupKeySource = Concept_Group_Key
			FROM	Concept
			WHERE	Concept_Key = @ConceptKeySelected
		
			SELECT	@ConceptGroupKeyDest = Concept_Group_Key
			FROM	Concept
			WHERE	Concept_Key = @ConceptKeyPasted
		
			/*===============================================================*\
			  If the source concept is in the same list as the dest concept, 
			  make it not list preferred.
			\*===============================================================*/
			IF @ConceptGroupKeySource = @ConceptGroupKeyDest
			BEGIN
				UPDATE	Concept
				SET	List_Preferred = 0
				WHERE	Concept_Key = @ConceptGroupKeySource
				AND	List_Preferred = 1
	
				DELETE	Concept_Lineage
				WHERE	Concept_Key = @ConceptGroupKeySource
			END
		END

	COMMIT TRANSACTION
	RETURN 0

RollBackAndExit: 
	ROLLBACK TRANSACTION
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_MakeConceptsSynonyms_Update') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_MakeConceptsSynonyms_Update'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_MakeConceptsSynonyms_Update TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_MakeConceptsSynonyms_Update TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_MakeConceptsSynonyms_Update TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_MakeConceptsSynonyms_Update TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_MakeConceptsSynonyms_Update TO [Dev - JNCC SQL]
END
GO
