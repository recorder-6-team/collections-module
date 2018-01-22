If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Enquiries_Select_ForMovement]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Enquiries_Select_ForMovement]
GO

/*===========================================================================*\
  Description:	Returns Enquiries data to the CollectionsBrowser for a given Movement.

  Parameters:	
	@ParentKey 	When specified, only the records associated with the parent key are returned
	@UserDomainMask	User's Domain Mask restricting which records may be returned
	@SessionID 	User's SessionID
	@SortOrderIndex	Index determining Sort Order

  Created:	October 2003

  Last revision information:
    $Revision: 5 $
    $Date: 13/04/04 12:24 $
    $Author: Ericsalmon $

\*===========================================================================*/

CREATE PROCEDURE [dbo].[usp_Enquiries_Select_ForMovement] 
	@ParentKey CHAR(16),
	@UserDomainMask INT,
	@SessionID CHAR(16),
	@SortOrderIndex TINYINT
AS

SET NOCOUNT ON

IF @SortOrderIndex = 0
	SELECT DISTINCT	ME.Enquiry_Key AS Item_Key, ME.Movement_Enquiry_Key AS Join_Key,
			E.Display_Caption, T.PlainText, Vague_Date_Start, Vague_Date_End, Vague_Date_Type

	FROM		Movement M
	INNER JOIN	Movement_Enquiry ME ON M.Movement_Key = ME.Movement_Key
	INNER JOIN 	Enquiry E ON ME.Enquiry_Key = E.Enquiry_Key AND ME.Movement_Key = @ParentKey
	INNER JOIN 	Concept C ON E.Enquiry_Type_Concept_Key = C.Concept_Key
	INNER JOIN 	Term T ON C.Term_Key = T.Term_Key
	INNER JOIN	Movement_Direction MD ON ME.Movement_Key = MD.Movement_Key 
	LEFT JOIN	(Movement_Collection_Unit MCU
			INNER JOIN Collection_Unit CU
				ON MCU.Collection_Unit_Key = CU.Collection_Unit_Key
				AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Domain_Mask = 0))
			)
		ON MD.Movement_Direction_Key = MCU.Movement_Direction_Key

	WHERE 	(CU.Collection_Unit_Key IS NOT NULL) 
	OR 	((MCU.Collection_Unit_Key IS NULL) AND ((M.Entered_Session_ID = @SessionID) OR (M.Changed_Session_ID = @SessionID)))

	ORDER BY Vague_Date_Start DESC, Vague_Date_End DESC, T.PlainText, Vague_Date_Type
ELSE
	SELECT DISTINCT	ME.Enquiry_Key AS Item_Key, ME.Movement_Enquiry_Key AS Join_Key,
			E.Display_Caption, T.PlainText, Vague_Date_Start, Vague_Date_End, Vague_Date_Type

	FROM		Movement M
	INNER JOIN	Movement_Enquiry ME ON M.Movement_Key = ME.Movement_Key
	INNER JOIN 	Enquiry E ON ME.Enquiry_Key = E.Enquiry_Key AND ME.Movement_Key = @ParentKey
	INNER JOIN 	Concept C ON E.Enquiry_Type_Concept_Key = C.Concept_Key
	INNER JOIN 	Term T ON C.Term_Key = T.Term_Key
	INNER JOIN	Movement_Direction MD ON ME.Movement_Key = MD.Movement_Key 
	LEFT JOIN	(Movement_Collection_Unit MCU
			INNER JOIN Collection_Unit CU
				ON MCU.Collection_Unit_Key = CU.Collection_Unit_Key
				AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Domain_Mask = 0))
			)
		ON MD.Movement_Direction_Key = MCU.Movement_Direction_Key

	WHERE 	(CU.Collection_Unit_Key IS NOT NULL) 
	OR 	((MCU.Collection_Unit_Key IS NULL) AND ((M.Entered_Session_ID = @SessionID) OR (M.Changed_Session_ID = @SessionID)))

	ORDER BY  T.PlainText
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Enquiries_Select_ForMovement') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Enquiries_Select_ForMovement'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Enquiries_Select_ForMovement TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Enquiries_Select_ForMovement TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Enquiries_Select_ForMovement TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Enquiries_Select_ForMovement TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Enquiries_Select_ForMovement TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Enquiries_Select_ForMovement TO [Dev - JNCC SQL]
END
GO
