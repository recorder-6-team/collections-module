/*===========================================================================*\
  Drop trigger before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT Name 
	   FROM   SysObjects 
	   WHERE  Name = N'tr_Concept_PublishedTermFields' 
	   AND 	  Type = 'TR')
    DROP TRIGGER [dbo].tr_Concept_PublishedTermFields
GO

/*===========================================================================*\
  Description:	Updates published and search terms for a concept and its
				descendants

  Created:	Aug 2011

  Last revision information:
    $Revision: 3 $
    $Date: 9/08/11 13:07 $
    $Author: Jamesbichard $

\*===========================================================================*/
CREATE TRIGGER [dbo].tr_Concept_PublishedTermFields ON [dbo].[Concept] 
FOR UPDATE, INSERT

AS
	IF UPDATE (Term_Version_Key) OR UPDATE (Term_Generator_Key)
		OR UPDATE (Term_Key) OR UPDATE (Concept_Rank_Key)
	BEGIN
		DECLARE @AutomaticUpdate BIT,
				@ConceptKey CHAR(16)

		SELECT @ConceptKey = Concept_Key, @AutomaticUpdate = Automatic_Published_Term
		FROM Inserted 
	
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
			RAISERROR('Error updating Concept table',16,1)
		
	END
GO