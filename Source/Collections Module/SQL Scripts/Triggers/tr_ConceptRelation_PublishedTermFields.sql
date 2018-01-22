/*===========================================================================*\
  Drop trigger before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT Name 
	   FROM   SysObjects 
	   WHERE  Name = N'tr_ConceptRelation_PublishedTermFields' 
	   AND 	  Type = 'TR')
    DROP TRIGGER [dbo].tr_ConceptRelation_PublishedTermFields
GO

/*===========================================================================*\
  Description:	Updates published term and search terms for a concept and descendants
				when its parent is changed

  Created:	Aug 2011

  Last revision information:
    $Revision: 3 $
    $Date: 9/08/11 13:07 $
    $Author: Jamesbichard $

\*===========================================================================*/
CREATE TRIGGER [dbo].tr_ConceptRelation_PublishedTermFields ON [dbo].Concept_Relation
FOR UPDATE, INSERT

AS
	IF UPDATE (To_Concept_Key) OR UPDATE(From_Concept_Key)
	BEGIN
		DECLARE	@AutomaticUpdate BIT,
				@ConceptKey CHAR(16)

		SELECT @ConceptKey = c.Concept_Key, @AutomaticUpdate = c.Automatic_Published_Term
		FROM Inserted
		INNER JOIN Concept c on c.Concept_Key = Inserted.To_Concept_Key

		EXEC usp_Concept_UpdateAutomaticTerms @ConceptKey, @AutomaticUpdate

		DECLARE @HierarchyRelationTypeKey CHAR(16)
		SELECT @HierarchyRelationTypeKey = cg.Hierarchy_Relation_Type_Key
		FROM Concept c
		INNER JOIN Concept_Group cg ON cg.Concept_Group_Key = c.Concept_Group_Key
		WHERE c.Concept_Key = @ConceptKey
	
		IF @HierarchyRelationTypeKey IS NOT NULL
		BEGIN	
			EXEC usp_Concept_UpdateDescendentTerms @ConceptKey = @ConceptKey	
		END

		IF @@ERROR <>0
			RAISERROR('Error updating published term and search terms',16,1)		
	END
GO