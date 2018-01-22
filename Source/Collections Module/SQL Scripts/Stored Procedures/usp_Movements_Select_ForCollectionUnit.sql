If EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID(N'[dbo].[usp_Movements_Select_ForCollectionUnit]') AND OBJECTPROPERTY(Id, N'IsProcedure') = 1)
DROP PROCEDURE [dbo].[usp_Movements_Select_ForCollectionUnit]
GO

/*===========================================================================*\
  Description:	Returns Movements data to the CollectionsBrowser for a given Collection Unit.

  Parameters:
	@ParentKey 	When specified, only the records associated with the parent key are returned
	@UserDomainMask	User's Domain Mask restricting which records may be returned
	@SessionID 	User's SessionID
	@SortOrderIndex	Index determining Sort Order

  Created:	August 2003

  Last revision information:
    $Revision: 15 $
    $Date: 8/04/04 18:16 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Movements_Select_ForCollectionUnit] 
	@UserDomainMask BIGINT,
	@SessionID CHAR(16),
	@ParentKey CHAR(16),
	@MovementGroupType TINYINT,
	@SortOrderIndex TINYINT
AS

SET NOCOUNT ON

	DECLARE	@MinType int,
		@MaxType int

	IF @MovementGroupType  = 0 BEGIN
		SET @MinType = 0
		SET @MaxType = 1
	END ELSE
	IF @MovementGroupType = 1 BEGIN
		SET @MinType = 2
		SET @MaxType = 3
	END ELSE
	IF @MovementGroupType = 2 BEGIN
		SET @MinType = 4
		SET @MaxType = 9
	END

	SELECT 		M.Movement_Key AS Item_Key, M.Movement_Type, Number, 
			Exp_Vague_Date_Start, Exp_Vague_Date_End, Exp_Vague_Date_Type, 
			M.Display_Caption, MCU.Movement_Collection_Unit_Key AS Join_Key

	FROM 		Collection_Unit CU
	INNER JOIN	Movement_Collection_Unit MCU 
		ON CU.Collection_Unit_Key = MCU.Collection_Unit_Key
		AND (CU.Collection_Unit_Key = @ParentKey)
		AND ((CU.Domain_Mask & @UserDomainMask > 0) OR (CU.Domain_Mask = 0))

	INNER JOIN	Movement_Direction MD ON MCU.Movement_Direction_Key = MD.Movement_Direction_Key
	INNER JOIN	Movement M ON MD.Movement_Key = M.Movement_Key AND (M.Movement_Type BETWEEN @MinType AND @MaxType)

	ORDER BY 	
		-- 0: M.Exp_Vague_Date_Start DESC, M.Exp_Vague_Date_End DESC, M.Movement_Type, M.Number, M.Exp_Vague_Date_Type
		-- 1: M.Number
		-- 2: M.Movement_Type, M.Number
		CASE @SortOrderIndex WHEN 0 THEN M.Exp_Vague_Date_Start ELSE NULL END DESC,
		CASE @SortOrderIndex WHEN 0 THEN M.Exp_Vague_Date_End ELSE NULL END DESC, 
		CASE WHEN @SortOrderIndex IN (0, 2) THEN M.Movement_Type ELSE NULL END,
		M.Number,
		CASE @SortOrderIndex WHEN 0 THEN M.Exp_Vague_Date_Type ELSE NULL END
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Movements_Select_ForCollectionUnit') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Movements_Select_ForCollectionUnit'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Movements_Select_ForCollectionUnit TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Movements_Select_ForCollectionUnit TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Movements_Select_ForCollectionUnit TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Movements_Select_ForCollectionUnit TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Movements_Select_ForCollectionUnit TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Movements_Select_ForCollectionUnit TO [Dev - JNCC SQL]
END
GO