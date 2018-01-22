/*===========================================================================*\
  Drop trigger before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT Name 
	   FROM   SysObjects 
	   WHERE  Name = N'tr_ConceptGroup_PublishedTermFields' 
	   AND 	  Type = 'TR')
    DROP TRIGGER [dbo].tr_ConceptGroup_PublishedTermFields
GO

/*===========================================================================*\
  Description:	Updates published term and search terms for a concept
				when plaintext is changed

  Created:	Nov 2003

  Last revision information:
    $Revision: 2 $
    $Date: 9/08/11 14:21 $
    $Author: Jamesbichard $

\*===========================================================================*/
CREATE TRIGGER [dbo].tr_ConceptGroup_PublishedTermFields ON [dbo].Concept_Group
FOR UPDATE, INSERT

AS
	IF UPDATE (Term_Generator_Key)
	BEGIN
		DECLARE @ConceptGroupKey CHAR(16)

		SELECT @ConceptGroupKey = Inserted.Concept_Group_Key
		FROM Inserted 

		EXEC usp_Concept_UpdateDescendentTerms @ConceptGroupKey = @ConceptGroupKey
		

		IF @@ERROR <>0
			RAISERROR('Error updating published term and search terms',16,1)		
	END
GO
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
    $Revision: 2 $
    $Date: 9/08/11 14:21 $
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
    $Revision: 2 $
    $Date: 9/08/11 14:21 $
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
/*===========================================================================*\
  Drop trigger before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT Name 
	   FROM   SysObjects 
	   WHERE  Name = N'tr_Domain_PublishedTermFields' 
	   AND 	  Type = 'TR')
    DROP TRIGGER [dbo].tr_Domain_PublishedTermFields
GO

/*===========================================================================*\
  Description:	Updates published term and search terms for a concept
				when plaintext is changed

  Created:	Nov 2003

  Last revision information:
    $Revision: 2 $
    $Date: 9/08/11 14:21 $
    $Author: Jamesbichard $

\*===========================================================================*/
CREATE TRIGGER [dbo].tr_Domain_PublishedTermFields ON [dbo].Domain
FOR UPDATE, INSERT

AS
	IF UPDATE (Term_Generator_Key)
	BEGIN
		DECLARE @DomainKey CHAR(16)

		SELECT @DomainKey = Inserted.Domain_Key
		FROM Inserted 

		EXEC usp_Concept_UpdateDescendentTerms @DomainKey = @DomainKey

		IF @@ERROR <>0
			RAISERROR('Error updating published term and search terms',16,1)		
	END
GO
/*===========================================================================*\
  Drop trigger before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT Name 
	   FROM   SysObjects 
	   WHERE  Name = N'tr_LocalDomain_PublishedTermFields' 
	   AND 	  Type = 'TR')
    DROP TRIGGER [dbo].tr_LocalDomain_PublishedTermFields
GO

/*===========================================================================*\
  Description:	Updates published term and search terms for a concept
				when plaintext is changed

  Created:	Nov 2003

  Last revision information:
    $Revision: 2 $
    $Date: 9/08/11 14:21 $
    $Author: Jamesbichard $

\*===========================================================================*/
CREATE TRIGGER [dbo].tr_LocalDomain_PublishedTermFields ON [dbo].Local_Domain
FOR UPDATE, INSERT

AS
	IF UPDATE (Term_Generator_Key)
	BEGIN
		DECLARE @LocalDomainKey CHAR(16)

		SELECT @LocalDomainKey = Inserted.Local_Domain_Key
		FROM Inserted 

		EXEC usp_Concept_UpdateDescendentTerms @LocalDomainKey = @LocalDomainKey
		

		IF @@ERROR <>0
			RAISERROR('Error updating published term and search terms',16,1)		
	END
GO
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
    $Revision: 2 $
    $Date: 9/08/11 14:21 $
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
/*===========================================================================*\
  Drop trigger before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT Name 
	   FROM   SysObjects 
	   WHERE  Name = N'tr_Term_PublishedTermFields' 
	   AND 	  Type = 'TR')
    DROP TRIGGER [dbo].tr_Term_PublishedTermFields
GO

/*===========================================================================*\
  Description:	Updates published and search terms for a concept and its
				descendants

  Created:	Aug 2011

  Last revision information:
    $Revision: 2 $
    $Date: 9/08/11 14:21 $
    $Author: Jamesbichard $

\*===========================================================================*/
CREATE TRIGGER [dbo].tr_Term_PublishedTermFields ON [dbo].Term 
FOR UPDATE, INSERT

AS
	IF UPDATE (Plaintext)
	BEGIN
		DECLARE @AutomaticUpdate BIT,
				@ConceptKey CHAR(16)

		DECLARE concepts CURSOR FAST_FORWARD LOCAL FOR
		SELECT c.Concept_Key, c.Automatic_Published_Term
		FROM Inserted 
		INNER JOIN Concept c on c.Term_Key = Inserted.Term_Key

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
