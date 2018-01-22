/*===========================================================================*\
  Drop trigger before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[tr_CollectionUnitCheck_ForDomainMask]') 
	   AND    Type = 'TR')
    DROP TRIGGER [dbo].[tr_CollectionUnitCheck_ForDomainMask]
GO

/*===========================================================================*\
  Description:	Update Domain_Mask of all checks linked to changed records in 
		Collection_Unit_Check.

  Type:		AFTER INSERT, UPDATE and DELETE

  Created:	September 2003

  Last revision information:
    $Revision: 3 $
    $Date: 21/01/04 9:04 $
    $Author: Johnvanbreda $

\*===========================================================================*/
CREATE TRIGGER [dbo].[tr_CollectionUnitCheck_ForDomainMask]
ON [dbo].[Collection_Unit_Check]
AFTER INSERT, UPDATE, DELETE
AS 
	DECLARE	@CheckKey char(16)

	/*-------------------------------------------------------------*\
	  Only the Check key is needed to get to the jobs needing 
	  updating. But we need to through all keys, hence the cursor.
	\*-------------------------------------------------------------*/
	DECLARE curChecks CURSOR LOCAL FAST_FORWARD FOR
		SELECT	DISTINCT Conservation_Check_Key
		FROM	Deleted
		WHERE	Conservation_Check_Key NOT IN (SELECT Conservation_Check_Key FROM Inserted)
		UNION
		SELECT	DISTINCT Conservation_Check_Key
		FROM 	Inserted	
	
	/*-------------------------------------------------------------*\
	  Go and do it now.
	\*-------------------------------------------------------------*/
	OPEN curChecks
	FETCH NEXT FROM curChecks INTO @CheckKey
	WHILE @@Fetch_Status = 0
	BEGIN
		EXECUTE	usp_ConservationCheck_Update_DomainMask NULL, @CheckKey
	
		FETCH NEXT FROM curChecks INTO @CheckKey
	END
	
	-- Cleanup
	CLOSE curChecks
	DEALLOCATE curChecks
GO
