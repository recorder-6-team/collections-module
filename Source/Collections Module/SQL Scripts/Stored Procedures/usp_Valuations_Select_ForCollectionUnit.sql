If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Valuations_Select_ForCollectionUnit]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Valuations_Select_ForCollectionUnit]
GO

/*===========================================================================*\
  Description:	Returns Valuations data to the CollectionsBrowser for a given Collection Unit.

  Parameters:
	@ParentKey 	Only the records associated with the parent key are returned
	@UserID		Name_Key of current user
	@SortOrderIndex	Index determining Sort Order

  Created:	August 2003

  Last revision information:
    $Revision: 6 $
    $Date: 8/04/04 18:16 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Valuations_Select_ForCollectionUnit] 
	@ParentKey CHAR(16),
	@UserID CHAR(16),
	@SortOrderIndex TINYINT
AS

SET NOCOUNT ON

	SELECT 		V.Valuation_Key AS Item_Key, V.Display_Caption, CUV.Collection_Unit_Valuation_Key AS Join_Key

	FROM 		Collection_Unit_Valuation CUV
	INNER JOIN	Valuation V ON CUV.Valuation_Key = V.Valuation_Key AND CUV.Collection_Unit_Key = @ParentKey
	INNER JOIN	[User] U ON U.Name_Key = @UserID
	LEFT JOIN	VW_ConceptTerm CT ON CT.Concept_Key = V.Type_Concept_Key

	WHERE U.Allow_Finance = 1

	ORDER BY 
		-- 0: V.Vague_Date_Start DESC, V.Vague_Date_End DESC, V.Vague_Date_Type, CT.Item_Name
		-- 1: CT.Item_Name, V.Vague_Date_Start DESC, V.Vague_Date_End DESC, V.Vague_Date_Type
		CASE @SortOrderIndex WHEN 1 THEN CT.Item_Name ELSE NULL END,
		V.Vague_Date_Start DESC, V.Vague_Date_End DESC, V.Vague_Date_Type,
		CASE @SortOrderIndex WHEN 0 THEN CT.Item_Name ELSE NULL END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Valuations_Select_ForCollectionUnit') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Valuations_Select_ForCollectionUnit'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Valuations_Select_ForCollectionUnit TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Valuations_Select_ForCollectionUnit TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Valuations_Select_ForCollectionUnit TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Valuations_Select_ForCollectionUnit TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Valuations_Select_ForCollectionUnit TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Valuations_Select_ForCollectionUnit TO [Dev - JNCC SQL]
END
GO