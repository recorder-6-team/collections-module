/*===========================================================================*\
  Drop trigger before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT Name 
	   FROM   SysObjects 
	   WHERE  Name = N'tr_Concept_AuthorCopy' 
	   AND 	  Type = 'TR')
    DROP TRIGGER [dbo].[tr_Concept_AuthorCopy]
GO

/*===========================================================================*\
  Description:	This trigger updates the Author_Copy field in the Concept 
		table when the Author_And_Date field in the Term_Version 
		table is updated

  Created:	Nov 2003

  Last revision information:
    $Revision: 3 $
    $Date: 21/11/03 16:11 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE TRIGGER [dbo].[tr_Concept_AuthorCopy] ON [dbo].[Concept] 
FOR UPDATE, INSERT

AS
	IF UPDATE (Term_Version_Key)
	BEGIN
		UPDATE Concept
		SET Concept.Author_Copy=Term_Version.Author_And_Date
		FROM Concept C 
		INNER JOIN Inserted I ON C.Concept_Key=I.Concept_Key
		LEFT JOIN Term_Version on Term_Version.Term_Version_Key=C.Term_Version_Key
	
		IF @@ERROR <>0
			RAISERROR('Error updating Author_Copy in Concept table',16,1)
	END
GO