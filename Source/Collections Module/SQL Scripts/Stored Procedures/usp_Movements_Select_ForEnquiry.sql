/*===========================================================================*\
  Drop stored proc before re-creating.
\*===========================================================================*/
IF EXISTS (SELECT *
	   FROM   SysObjects 
	   WHERE  Id = Object_Id(N'[dbo].[usp_Movements_Select_ForEnquiry]')
	   AND 	  Type = 'P')
    DROP PROCEDURE [dbo].[usp_Movements_Select_ForEnquiry]
GO

/*===========================================================================*\
  Description:	Returns Movements for the Enquiry node.

  Parameters:	@SessionID 
		@ParentKey 	Enquiry key
		@MovementGroupType 
		@SortOrderIndex 

  Created:	January 2004

  Last revision information:
    $Revision: 2 $
    $Date: 8/04/04 18:16 $
    $Author: Ericsalmon $

\*===========================================================================*/
CREATE PROCEDURE [dbo].[usp_Movements_Select_ForEnquiry]
	@SessionID char(16),
	@ParentKey char(16),
	@MovementGroupType tinyint,
	@SortOrderIndex tinyint
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

	SELECT 		M.Movement_Key AS Item_Key, M.Movement_Type, M.Number, 
			Exp_Vague_Date_Start, Exp_Vague_Date_End, Exp_Vague_Date_Type, 
			M.Display_Caption, ME.Movement_Enquiry_Key AS Join_Key

	FROM		Movement_Enquiry AS ME 
	INNER JOIN	Movement AS M ON M.Movement_Key = ME.Movement_Key AND (M.Movement_Type BETWEEN @MinType AND @MaxType)
	WHERE		ME.Enquiry_Key = @ParentKey		
	ORDER BY 	
		-- 0: M.Exp_Vague_Date_Start DESC, M.Exp_Vague_Date_End DESC, M.Movement_Type, M.Number, M.Exp_Vague_Date_Type
		-- 1: M.Number
		-- 2: M.Movement_Type, M.Number
		CASE @SortOrderIndex WHEN 0 THEN M.Exp_Vague_Date_Start ELSE NULL END DESC,
		CASE @SortOrderIndex WHEN 0 THEN M.Exp_Vague_Date_End ELSE NULL END DESC, 
		CASE WHEN @SortOrderIndex IN (0, 2) THEN M.Movement_Type ELSE NULL END,
		M.Number,
		CASE @SortOrderIndex WHEN 0 THEN M.Exp_Vague_Date_Type ELSE NULL END
SET NOCOUNT OFF
GO

/*===========================================================================*\
  Grant permissions.
\*===========================================================================*/
IF EXISTS (SELECT * FROM SysObjects WHERE Id = OBJECT_ID('dbo.usp_Movements_Select_ForEnquiry') AND SysStat & 0xf = 4)
BEGIN
    	PRINT 'Setting up security on procedure usp_Movements_Select_ForEnquiry'
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_AddOnly')
        	GRANT EXECUTE ON dbo.usp_Movements_Select_ForEnquiry TO [R2k_AddOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_Administrator')
		GRANT EXECUTE ON dbo.usp_Movements_Select_ForEnquiry TO [R2k_Administrator]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_FullEdit')
		GRANT EXECUTE ON dbo.usp_Movements_Select_ForEnquiry TO [R2k_FullEdit]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_ReadOnly')
		GRANT EXECUTE ON dbo.usp_Movements_Select_ForEnquiry TO [R2k_ReadOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'R2k_RecordCardsOnly')
		GRANT EXECUTE ON dbo.usp_Movements_Select_ForEnquiry TO [R2k_RecordCardsOnly]
	IF EXISTS (SELECT * FROM SYSUSERS WHERE NAME = 'Dev - JNCC SQL')
        	GRANT EXECUTE ON dbo.usp_Movements_Select_ForEnquiry TO [Dev - JNCC SQL]
END
GO