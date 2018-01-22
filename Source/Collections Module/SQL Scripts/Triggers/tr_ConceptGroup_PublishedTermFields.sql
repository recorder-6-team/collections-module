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
    $Revision: 3 $
    $Date: 9/08/11 13:07 $
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