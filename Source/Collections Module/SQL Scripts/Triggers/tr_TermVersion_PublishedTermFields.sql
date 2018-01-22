/*===========================================================================*\
  Drop trigger before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT Name 
	   FROM   SysObjects 
	   WHERE  Name = N'tr_TermVersion_PublishedTermFields' 
	   AND 	  Type = 'TR')
    DROP TRIGGER [dbo].tr_TermVersion_PublishedTermFields
GO

/*===========================================================================*\
  Description:	Updates published term and search terms for a concept and descendants
				when plaintext is changed

  Created:	Aug 2011

  Last revision information:
    $Revision: 3 $
    $Date: 9/08/11 13:07 $
    $Author: Jamesbichard $

\*===========================================================================*/
CREATE TRIGGER [dbo].tr_TermVersion_PublishedTermFields ON [dbo].Term_Version
FOR UPDATE, INSERT

AS
	IF UPDATE (Author_And_Date) OR UPDATE(Version_Label)
	BEGIN
		DECLARE	@AutomaticUpdate BIT,
				@ConceptKey CHAR(16)

		DECLARE concepts CURSOR FAST_FORWARD LOCAL FOR
		SELECT c.Concept_Key, c.Automatic_Published_Term
		FROM Inserted 
		INNER JOIN Concept c on c.Term_Version_Key = Inserted.Term_Version_Key

		OPEN concepts

		WHILE 1 = 1
		BEGIN
			FETCH concepts
			INTO @ConceptKey, @AutomaticUpdate

			IF @@FETCH_STATUS <> 0 BREAK

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
		END

		CLOSE concepts
		DEALLOCATE concepts

		IF @@ERROR <>0
			RAISERROR('Error updating published term and search terms',16,1)		
	END
GO