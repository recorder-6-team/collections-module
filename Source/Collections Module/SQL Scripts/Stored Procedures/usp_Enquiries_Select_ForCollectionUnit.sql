If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Enquiries_Select_ForCollectionUnit]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Enquiries_Select_ForCollectionUnit]
GO

/*===========================================================================*\
  Description:	Returns Enquiries for a specified Collection.

  Parameters:
	@ParentKey 	When specified, only the records associated with the parent key are returned
	@SortOrderIndex	Index determining Sort Order

  Created:	August 2003

  Last revision information:
    $Revision: 8 $
    $Date: 8/04/04 18:15 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Enquiries_Select_ForCollectionUnit] 
	@ParentKey CHAR(16),
	@SortOrderIndex TINYINT
AS

SET NOCOUNT ON

	SELECT 		CUE.Enquiry_Key AS Item_Key, E.Display_Caption, CUE.Collection_Unit_Enquiry_Key AS Join_Key

	FROM 		Collection_Unit_Enquiry CUE
	INNER JOIN 	Enquiry E ON CUE.Enquiry_Key = E.Enquiry_Key AND CUE.Collection_Unit_Key = @ParentKey
	INNER JOIN 	Concept C ON E.Enquiry_Type_Concept_Key = C.Concept_Key
	INNER JOIN 	Term T ON C.Term_Key = T.Term_Key

	ORDER BY 
		-- 0: Vague_Date_Start DESC, Vague_Date_End DESC, T.PlainText, Vague_Date_Type
		-- 1: T.PlainText 
		CASE @SortOrderIndex WHEN 0 THEN Vague_Date_Start ELSE NULL END DESC,
		CASE @SortOrderIndex WHEN 0 THEN Vague_Date_End ELSE NULL END DESC,
		T.PlainText,
		CASE @SortOrderIndex WHEN 0 THEN Vague_Date_Type ELSE NULL END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Enquiries_Select_ForCollectionUnit') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Enquiries_Select_ForCollectionUnit'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Enquiries_Select_ForCollectionUnit TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Enquiries_Select_ForCollectionUnit TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Enquiries_Select_ForCollectionUnit TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Enquiries_Select_ForCollectionUnit TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Enquiries_Select_ForCollectionUnit TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Enquiries_Select_ForCollectionUnit TO [Dev - JNCC SQL]
END
GO
