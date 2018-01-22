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
    $Revision: 1 $
    $Date: 15/06/11 9:55 $
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

/*===========================================================================*\
  Drop function before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'dbo.ufn_GetConceptAncestorPath')
	   AND    Type IN ('FN', 'IF', 'TF'))
	DROP FUNCTION dbo.ufn_GetConceptAncestorPath
GO

/*===========================================================================*\
  Description:	Returns the hierarchy of a specified concept

  Parameters:	@Concept_Key		The key of the concept for which the ancestor
									hierarchy is return

  Created:	May 2011

  Last revision information:
    $Revision: 1 $
    $Date: 15/06/11 9:55 $
    $Author: Jamesbichard $

\*===========================================================================*/
CREATE FUNCTION dbo.ufn_GetConceptAncestorPath (
	@ItemName varchar(100),
	@Concept_Key char(16)
)
RETURNS varchar(5000)
AS
BEGIN
	DECLARE @lineageId INT,
			@HierarchyPath varchar(5000),
			@CurrentLineage varchar(1000),
			@current_ancestor varchar(200)

	SET @HierarchyPath = ''
	
	DECLARE conceptLineageId CURSOR LOCAL FAST_FORWARD FOR
	SELECT lineage_id 
	FROM concept_lineage 
	WHERE concept_key = @Concept_Key

	OPEN conceptLineageId

	WHILE 1 = 1
	BEGIN

		FETCH conceptLineageId
		INTO @lineageId

		IF @@FETCH_STATUS <> 0 BREAK

		SET @CurrentLineage = ''

		DECLARE		ancestors	CURSOR LOCAL FAST_FORWARD FOR
		select t.item_name 
		from concept c 
		left join concept_lineage cl on cl.concept_key = c.concept_key
		inner join (
			select cl1.lineage, cl1.lineage_id, c.concept_group_key, c.term_key
			from concept_lineage cl1
			inner join concept c on c.concept_key = cl1.concept_key ) as crelated
			on cl.lineage LIKE crelated.lineage + '\%' 
				and c.concept_group_key = crelated.concept_group_key
		inner join term t on t.term_key = crelated.term_key
		where c.concept_key = @Concept_Key and cl.lineage_id = @lineageId
		order by crelated.lineage

		OPEN		ancestors

		WHILE 1 = 1
		BEGIN
			FETCH		ancestors
			INTO		@current_ancestor

			IF @@FETCH_STATUS <> 0 BREAK
	
			IF LEN(@CurrentLineage) = 0
			BEGIN
				SET @CurrentLineage = @ItemName + ': '
			END

			SET @CurrentLineage = @CurrentLineage + @current_ancestor + ' - '
		END
	
		CLOSE		ancestors
		DEALLOCATE	ancestors

		IF LEN(@CurrentLineage) > 0 
		BEGIN
			SELECT @CurrentLineage = SUBSTRING(@CurrentLineage, 0, LEN(@CurrentLineage) - 1)								 
		END ELSE
		BEGIN
			SELECT @CurrentLineage = @ItemName	
		END

		SET @HierarchyPath = @HierarchyPath + @CurrentLineage + '**'
	END
	
	CLOSE conceptLineageId
	DEALLOCATE conceptLineageId

	IF LEN(@HierarchyPath) > 0
	BEGIN
		SET @HierarchyPath = SUBSTRING(@HierarchyPath, 0, LEN(@HierarchyPath) - 1)
	END
	ELSE
	BEGIN
		SET @HierarchyPath = @ItemName
	END

	RETURN @HierarchyPath
END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * 
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'dbo.ufn_GetConceptAncestorPath')
	   AND    Type IN ('FN', 'IF', 'TF'))
	BEGIN
    	PRINT 'Setting up security on function dbo.ufn_GetConceptAncestorPath'
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
	        	GRANT EXECUTE ON dbo.ufn_GetConceptAncestorPath TO [R2k_AddOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
			GRANT EXECUTE ON dbo.ufn_GetConceptAncestorPath TO [R2k_Administrator]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
			GRANT EXECUTE ON dbo.ufn_GetConceptAncestorPath TO [R2k_FullEdit]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
			GRANT EXECUTE ON dbo.ufn_GetConceptAncestorPath TO [R2k_ReadOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
			GRANT EXECUTE ON dbo.ufn_GetConceptAncestorPath TO [R2k_RecordCardsOnly]
		IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
	        	GRANT EXECUTE ON dbo.ufn_GetConceptAncestorPath TO [Dev - JNCC SQL]
	END
GO