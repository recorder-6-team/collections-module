/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].usp_Concept_UpdateDescendentTerms')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].usp_Concept_UpdateDescendentTerms
GO


/*===========================================================================*\
  Description:	Update published and search terms for descendants of a concept

  Parameters:	

  Created:	August 2011

  Last revision information:
    $Revision: 4 $
    $Date: 17/08/11 9:13 $
    $Author: Jamesbichard $

\*===========================================================================*/

CREATE PROCEDURE dbo.usp_Concept_UpdateDescendentTerms
	@ConceptKey char(16) = null,
	@ConceptGroupKey char(16) = null,
	@LocalDomainKey char(16) = null,
	@DomainKey char(16) = null
AS
	SET NOCOUNT ON

	DECLARE @AutomaticPublishedTerm BIT

	IF @ConceptKey IS NOT NULL
	BEGIN
		-- get concept descendents from the concept lineage table
		DECLARE descendents CURSOR FAST_FORWARD LOCAL FOR
		SELECT crelated.concept_key 
		FROM concept c 
		LEFT JOIN concept_lineage cl on cl.concept_key = c.concept_key
		INNER JOIN (
			SELECT 
				cl1.lineage, 
				cl1.lineage_id, 
				c1.concept_group_key, 
				c1.concept_key
			FROM concept_lineage cl1
			INNER JOIN concept c1 on c1.concept_key = cl1.concept_key ) as crelated
			ON crelated.lineage LIKE cl.lineage + '\%' 
				AND c.concept_group_key = crelated.concept_group_key
		WHERE c.concept_key = @ConceptKey
		ORDER BY crelated.lineage

		OPEN descendents

		WHILE 1 = 1
		BEGIN
			FETCH descendents
			INTO @ConceptKey

			IF @@FETCH_STATUS <> 0 BREAK

			SELECT @AutomaticPublishedTerm = Automatic_Published_Term
			FROM Concept
			WHERE Concept_Key = @ConceptKey

			EXEC usp_Concept_UpdateAutomaticTerms @ConceptKey, @AutomaticPublishedTerm
		END

		CLOSE descendents	
		DEALLOCATE descendents
	END
	ELSE
	BEGIN
		DECLARE descendents CURSOR FAST_FORWARD LOCAL FOR
		SELECT 
			c.Concept_Key
		FROM Concept c
		INNER JOIN Concept_Group cg ON cg.Concept_Group_Key = c.Concept_Group_Key
		INNER JOIN Local_Domain ld ON ld.Local_Domain_Key = cg.Local_Domain_Key
		INNER JOIN Domain d ON d.Domain_Key = ld.Domain_Key	
		WHERE (cg.Concept_Group_Key = @ConceptGroupKey OR @ConceptGroupKey IS NULL)
		AND (ld.Local_Domain_Key = @LocalDomainKey OR @LocalDomainKey IS NULL)
		AND (d.Domain_Key = @DomainKey OR @DomainKey IS NULL)

		OPEN descendents

		WHILE 1 = 1
		BEGIN
			FETCH descendents
			INTO @ConceptKey

			IF @@FETCH_STATUS <> 0 BREAK

			SELECT @AutomaticPublishedTerm = Automatic_Published_Term
			FROM Concept
			WHERE Concept_Key = @ConceptKey

			EXEC usp_Concept_UpdateAutomaticTerms @ConceptKey, @AutomaticPublishedTerm
		END
		
		CLOSE descendents	
		DEALLOCATE descendents
	END

	
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Concept_UpdateDescendentTerms') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Concept_UpdateDescendentTerms'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
			GRANT EXECUTE ON dbo.usp_Concept_UpdateDescendentTerms TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Concept_UpdateDescendentTerms TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Concept_UpdateDescendentTerms TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Concept_UpdateDescendentTerms TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Concept_UpdateDescendentTerms TO [Dev - JNCC SQL]
END
GO