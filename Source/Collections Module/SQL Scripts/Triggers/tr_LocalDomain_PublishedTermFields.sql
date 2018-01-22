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
    $Revision: 3 $
    $Date: 9/08/11 13:07 $
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