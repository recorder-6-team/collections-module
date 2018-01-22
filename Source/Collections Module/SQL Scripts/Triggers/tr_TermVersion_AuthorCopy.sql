/*===========================================================================*\
  Drop trigger before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT Name 
	   FROM   SysObjects 
	   WHERE  Name = N'tr_TermVersion_AuthorCopy' 
	   AND 	  Type = 'TR')
    DROP TRIGGER [dbo].[tr_TermVersion_AuthorCopy]
GO

/*===========================================================================*\
  Description:	This trigger updates the Author_Copy field in the Concept 
		table when the Author_And_Date field in the Term_Version 
		table is updated

  Created:	Nov 2003

  Last revision information:
    $Revision: 5 $
    $Date: 21/11/03 16:11 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE TRIGGER [dbo].[tr_TermVersion_AuthorCopy] ON [dbo].[Term_Version] 
FOR UPDATE, INSERT

AS
	IF UPDATE (Author_And_Date)
	BEGIN
		UPDATE Concept
		SET Concept.Author_Copy=I.Author_And_Date
		FROM Concept INNER JOIN Inserted I
		ON Concept.Term_Version_Key=I.Term_Version_Key
	
		IF @@ERROR <>0
			RAISERROR('Error updating Author_Copy in Concept table',16,1)
	END
GO